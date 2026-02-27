/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
 *
 * Licensed under the  terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License.
 *
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */
/**
 * AST analyzer specified to search for TVM specific issues.
 */

#include <liblangutil/ErrorReporter.h>
#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/analysis/TVMAnalyzer.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;

bool BaseAnalyzer::analyze(SourceUnit const& _sourceUnit) {
	_sourceUnit.accept(*this);
	return !m_errorReporter.hasErrors();
}

bool TVMAnalyzer::visit(RevertStatement const& _revertStatement) {
	m_errorReporter
		.typeError(1594_error, _revertStatement.location(), "Revert statements are not supported currently.");
	return true;
}

bool TVMAnalyzer::visit(MemberAccess const& _node) {
	auto funType = to<FunctionType>(_node.annotation().type);
	if (funType) {
		if (funType->hasBoundFirstArgument()) {
			auto printError = [&] {
				m_errorReporter.fatalTypeError(
					5939_error,
					_node.location(),
					"Function references are not supported if the function is called as arg1.fun(arg2, ..., argn)."
				);
			};

			if (m_functionCall.empty()) {
				printError();
			} else {
				Expression const& expr = m_functionCall.back()->expression();
				Expression const* targetExpr{};
				if (auto opt = dynamic_cast<FunctionCallOptions const*>(&expr)) {
					targetExpr = &opt->expression();
				} else {
					targetExpr = &m_functionCall.back()->expression();
				}
				if (*targetExpr != _node) {
					printError();
				}
			}
		}
	}
	return true;
}

bool TVMAnalyzer::visit(FunctionDefinition const& _function) {
	if (_function.isImplemented())
		m_currentFunction = &_function;
	else
		solAssert(!m_currentFunction, "");

	return true;
}

bool TVMAnalyzer::visit(Return const& _return) {
	if (m_currentFunction && m_currentFunction->isResponsible()) {
		auto hasName = [&](std::string const& name) -> bool {
			auto b = _return.names().begin();
			auto e = _return.names().end();
			return std::find_if(b, e, [&](ASTPointer<ASTString> const& x) { return name == *x; }) != e;
		};
		if (!hasName("value") || !hasName("bounce") || !hasName("flag")) {
			m_errorReporter.fatalDeclarationError(
				5133_error,
				_return.location(),
				std::string{} +
					"`value`, `bounce` and `flag` options must be explicitly defined for `responsible` functions.\n" +
					"Hint: use `{value: 0, bounce: false, flag: 64}`."
			);
		}
	}

	return true;
}

bool TVMAnalyzer::visit(FunctionCall const& _functionCall) {
	m_functionCall.emplace_back(&_functionCall);
	return true;
}

void TVMAnalyzer::endVisit(FunctionDefinition const&) { m_currentFunction = nullptr; }

void TVMAnalyzer::endVisit(FunctionCall const&) {
	solAssert(!m_functionCall.empty(), "");
	m_functionCall.pop_back();
}

void TVMAnalyzer::endVisit(PragmaDirective const& _pragma) {
	if (!_pragma.literals().empty() && _pragma.literals().at(0) == "copyleft") {
		std::vector<ASTPointer<Expression>> params = _pragma.parameter();

		auto checkConstInteger =
			[&](Expression const& _e, SourceLocation const& loc, std::string const& msg, bigint const& max_val) {
				if (_e.annotation().type->category() != Type::Category::RationalNumber) {
					m_errorReporter.syntaxError(5514_error, loc, msg);
					return;
				}
				auto number = dynamic_cast<RationalNumberType const*>(_e.annotation().type);
				solAssert(number, "");
				if (number->isFractional()) {
					m_errorReporter.syntaxError(8784_error, loc, msg);
					return;
				}
				bigint val = number->value2();
				if (val < 0 || val >= max_val) {
					m_errorReporter.syntaxError(5971_error, loc, msg);
				}
			};

		// check type
		checkConstInteger(
			*params.at(0),
			params.at(0)->location(),
			"Expected constant integer type (uint8).",
			bigint(1) << 8
		);

		// check address
		checkConstInteger(
			*params.at(1),
			params.at(1)->location(),
			"Expected constant integer type (uint256).",
			bigint(1) << 256
		);
	}
}

bool TVMAnalyzerFlag128::visit(ContractDefinition const& contract) {
	switch (contract.contractKind()) {
	case ContractKind::Contract:
		return true;
	case ContractKind::Interface:
	case ContractKind::Library:
		return false;
	}

	solUnimplemented("");
}

bool TVMAnalyzerFlag128::visit(IfStatement const& _function) {
	auto const prev128 = m_functionCallWith128Flag;
	_function.trueStatement().accept(*this);
	if (_function.falseStatement()) {
		auto const new128 = m_functionCallWith128Flag;
		m_functionCallWith128Flag = prev128;
		_function.falseStatement()->accept(*this);
		if (m_functionCallWith128Flag == nullptr) {
			m_functionCallWith128Flag = new128;
		}
	}
	return false;
}

bool TVMAnalyzerFlag128::visit(FunctionDefinition const& _function) {
	m_function = &_function;
	m_functionCallWith128Flag = nullptr;
	return true;
}

void TVMAnalyzerFlag128::endVisit(FunctionDefinition const&) {
	m_function = nullptr;
	m_functionCallWith128Flag = nullptr;
}

bool TVMAnalyzerFlag128::visit(FunctionCall const& _functionCall) {
	std::optional<bigint> flagValue;
	bool isSendMsg = false;

	auto funcType = to<FunctionType>(_functionCall.expression().annotation().type);
	if (funcType && funcType->kind() == FunctionType::Kind::Selfdestruct) {
		isSendMsg = true; // selfdestruct
		flagValue = 128;
	}

	if (auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression())) {
		auto memberAccess = to<MemberAccess>(&functionOptions->expression());
		auto newExpr = to<NewExpression>(&functionOptions->expression());
		if (memberAccess || newExpr) {
			isSendMsg = true; // this.f{value: 0}();
			auto const& names = functionOptions->names();
			for (uint32_t i = 0; i < names.size(); ++i) {
				if (*names[i] == "flag") {
					ASTPointer<Expression const> flagExpr = functionOptions->options().at(i);
					flagValue = ExprUtils::constValue(*flagExpr);
				}
			}
		}
	} else {
		Expression const* currentExpression = &_functionCall.expression();
		auto memberAccess = to<MemberAccess>(currentExpression);
		if (memberAccess) {
			auto functionDefinition = getRemoteFunctionDefinition(memberAccess);
			if (functionDefinition != nullptr) {
				isSendMsg = true; // this.f();
			} else {
				auto category = getType(&memberAccess->expression())->category();
				if (isIn(category, Type::Category::Address, Type::Category::AddressStd)) {
					if (memberAccess->memberName() == "transfer") {
						isSendMsg = true; // addr.transfer(...)
						auto args = _functionCall.arguments();
						int argumentQty = static_cast<int>(args.size());
						if (!_functionCall.names().empty() || argumentQty == 0) {
							for (int arg = 0; arg < argumentQty; ++arg) {
								if (*_functionCall.names().at(arg) == "flag") {
									flagValue = ExprUtils::constValue(*args.at(arg));
								}
							}
						} else {
							if (args.size() >= 3) {
								flagValue = ExprUtils::constValue(*args.at(2));
							}
						}
					}
				}
			}
		}
	}

	if (!isSendMsg) {
		return true;
	}

	if (m_functionCallWith128Flag != nullptr) {
		m_errorReporter.typeError(
			2526_error,
			_functionCall.location(),
			SecondarySourceLocation().append("Sending all balance:", m_functionCallWith128Flag->location()),
			"External function call will fail on action phase because all balance was already send."
		);
	}

	if (flagValue.has_value() && 128 <= flagValue && flagValue <= 128 + 3) {
		m_functionCallWith128Flag = &_functionCall;

		if (m_function && !m_function->returnParameters().empty() && m_function->functionIsExternallyVisible()) {
			if (m_functionCallWith128Flag != nullptr) {
				m_errorReporter.typeError(
					3337_error,
					m_function->location(),
					SecondarySourceLocation().append("Sending all balance:", m_functionCallWith128Flag->location()),
					"Public function (that returns some parameters) will emit the event. It will fail on action phase "
					"because all balance was already send."
				);
			}
		}
	}

	return false;
}

bool TVMAnalyzerFlag128::visit(EmitStatement const& _emit) {
	if (m_functionCallWith128Flag != nullptr) {
		m_errorReporter.typeError(
			8901_error,
			_emit.location(),
			SecondarySourceLocation().append("Sending all balance:", m_functionCallWith128Flag->location()),
			"Emitting the event will fail on action phase because all balance was already send."
		);
	}
	return true;
}

bool ExtraFlagAnalyzer::visit(FunctionCall const& _functionCall) {
	std::optional<bigint> extraFlags;
	std::optional<bool> bounce;
	SourceLocation const* locExtraFlags = nullptr;
	SourceLocation const* locBounce = nullptr;

	if (auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression())) {
		auto memberAccess = to<MemberAccess>(&functionOptions->expression());
		auto newExpr = to<NewExpression>(&functionOptions->expression());
		if (memberAccess || newExpr) {
			// this.f{value: 0}();
			auto const& names = functionOptions->names();
			for (uint32_t i = 0; i < names.size(); ++i) {
				if (*names[i] == "extra_flags") {
					ASTPointer<Expression const> flagExpr = functionOptions->options().at(i);
					extraFlags = ExprUtils::constValue(*flagExpr);
					locExtraFlags = &flagExpr->location();
				}
				if (*names[i] == "bounce") {
					ASTPointer<Expression const> flagExpr = functionOptions->options().at(i);
					bounce = ExprUtils::constBool(*flagExpr);
					locBounce = &flagExpr->location();
				}
			}
		}
	} else {
		Expression const* currentExpression = &_functionCall.expression();
		auto memberAccess = to<MemberAccess>(currentExpression);
		if (memberAccess) {
			auto functionDefinition = getRemoteFunctionDefinition(memberAccess);
			if (functionDefinition != nullptr) {
				// this.f();
			} else {
				Type const* expressionType = _functionCall.expression().annotation().type;
				switch (expressionType->category()) {
				case Type::Category::Function: {
					auto functionType = to<FunctionType>(expressionType);
					switch (functionType->kind()) {
					case FunctionType::Kind::AddressTransfer:
					case FunctionType::Kind::ABIEncodeIntMsg: {
						// addr.transfer(...)
						auto args = _functionCall.arguments();
						int argumentQty = static_cast<int>(args.size());
						if (!_functionCall.names().empty() || argumentQty == 0) {
							for (int arg = 0; arg < argumentQty; ++arg) {
								std::string name = *_functionCall.names().at(arg);
								if (name == "extra_flags") {
									extraFlags = ExprUtils::constValue(*args.at(arg));
									locExtraFlags = &args.at(arg)->location();
								}
								if (name == "bounce") {
									bounce = ExprUtils::constBool(*args.at(arg));
									locBounce = &args.at(arg)->location();
								}
							}
						}
						break;
					}
					default:
						break;
					}
					break;
				}
				default:
					break;
				}
			}
		}
	}

	if (extraFlags.has_value()) {
		if ((*extraFlags & 0x3) == 2)
			m_errorReporter.typeError(
				8797_error,
				*locExtraFlags,
				"\"(extra_flags & 0x3)\" cannot be equal to 2. Possible values: 1 or 3."
				" Note: if the second bit in \"extra_flags\" is set, then the first bit must be set too."
			);

		if ((!bounce.has_value() || !bounce.value()) && (extraFlags.value() & 0x3) != 0) {
			std::string errorMsg = "\"(extra_flags & 0x3)\" must be equal to zero because \"bounce\" flag is not set.";
			if (bounce.has_value())
				m_errorReporter.typeError(
					9272_error,
					*locExtraFlags,
					SecondarySourceLocation().append("Bounce flag is here:", *locBounce),
					errorMsg

				);
			else
				m_errorReporter.typeError(9186_error, *locExtraFlags, errorMsg);
		}
	}

	return true;
}

bool ExtMsgAnalyzer::visit(FunctionDefinition const& _function) {
	if (_function.isExternalMsg()) {
		m_function = &_function;
		return true;
	}
	return false;
}

void ExtMsgAnalyzer::endVisit(FunctionDefinition const&) { m_function = nullptr; }

bool ExtMsgAnalyzer::visit(MemberAccess const& _memberAccess) {
	if (m_function != nullptr) {
		Type const* exprType = _memberAccess.expression().annotation().type;
		if (auto magicType = to<MagicType>(exprType)) {
			ASTString const& member = _memberAccess.memberName();

			switch (magicType->kind()) {
			case MagicType::Kind::Message:
				if (isIn(member, "sender", "currencies", "createdAt")) {
					m_errorReporter.typeError(
						1897_error,
						_memberAccess.location(),
						SecondarySourceLocation().append("The declaration is here:", m_function->location()),
						"\"msg." + member + "\" cannot be used in the function that marked as \"externalMsg\""
					);
				}
				break;
			default:
				break;
			}
		}
	}
	return true;
}

bool TVMAnalyzerPackUnpack::visit(FunctionCall const& _functionCall) {
	auto funcType = to<FunctionType>(_functionCall.expression().annotation().type);
	if (funcType && funcType->kind() == FunctionType::Kind::TVMUnpackData) {
		return false;
	}

	return true;
}

bool TVMAnalyzerPackUnpack::visit(Identifier const& _node) {
	auto varDecl = to<VariableDeclaration>(_node.annotation().referencedDeclaration);
	if (varDecl && varDecl->isUnpacked()) {
		m_errorReporter.typeError(
			7813_error,
			_node.location(),
			SecondarySourceLocation().append("Declaration of unpacked variable:", varDecl->location()),
			"Using unpacked variable. Use tvm.unpackData(...) to get the variable."
		);
	}
	return true;
}

bool TVMAnalyzerPackUnpack::visit(VariableDeclaration const& _node) {
	if (_node.isUnpacked() && _node.value() != nullptr) {
		m_errorReporter.typeError(8382_error, _node.location(), "Unpacked variable can not be initialized here.");
	}
	return true;
}

ContactsUsageScanner::ContactsUsageScanner(ContractDefinition const& cd) {
	for (ContractDefinition const* base: cd.annotation().linearizedBaseContracts) {
		base->accept(*this);
	}
}

bool ContactsUsageScanner::visit(FunctionCall const& _functionCall) {
	Expression const& expr = _functionCall.expression();
	Type const* exprType = getType(&expr);
	auto funType = to<FunctionType>(exprType);
	if (funType) {
		if (funType->hasDeclaration() &&
			isIn(funType->kind(), FunctionType::Kind::Internal, FunctionType::Kind::DelegateCall)) {
			// Library functions aren't part of contract definition, that's why we use lazy visiting
			Declaration const& decl = funType->declaration();
			if (!m_usedFunctions.contains(&decl)) {
				m_usedFunctions.insert(&decl);
				decl.accept(*this);
			}
		}
		switch (funType->kind()) {
		case FunctionType::Kind::MsgPubkey:
			m_hasMsgPubkey = true;
			break;
		default:
			break;
		}
	}

	return true;
}

bool ContactsUsageScanner::visit(MemberAccess const& _node) {
	auto const& expr = _node.expression();
	auto type = expr.annotation().type;

	if (type->category() == Type::Category::Magic) {
		auto identifier = to<Identifier>(&_node.expression());
		if (identifier) {
			if (identifier->name() == "msg" && _node.memberName() == "sender") {
				m_hasMsgSender = true;
			}
		}
	}
	return true;
}

bool ContactsUsageScanner::visit(FunctionDefinition const& fd) {
	if (fd.isResponsible())
		m_hasResponsibleFunction = true;
	return true;
}

CFAnalyzer::CFAnalyzer(Statement const& node) {
	node.accept(*this);
	solAssert(m_loopDepth == 0, "");
	m_alwaysReturns = doesAlways<Return>(&node);
	m_alwaysContinue = doesAlways<Continue>(&node);
	m_alwaysBreak = doesAlways<Break>(&node);
}

bool CFAnalyzer::startLoop() {
	m_loopDepth++;
	return true;
}

bool CFAnalyzer::visit(ForEachStatement const&) { return startLoop(); }

bool CFAnalyzer::visit(WhileStatement const&) { return startLoop(); }

bool CFAnalyzer::visit(ForStatement const&) { return startLoop(); }

void CFAnalyzer::endVisit(ForEachStatement const&) { m_loopDepth--; }

void CFAnalyzer::endVisit(WhileStatement const&) { m_loopDepth--; }

void CFAnalyzer::endVisit(ForStatement const&) { m_loopDepth--; }

void CFAnalyzer::endVisit(Return const&) { m_canReturn = true; }

void CFAnalyzer::endVisit(Break const&) {
	if (m_loopDepth == 0)
		m_canBreak = true;
}

void CFAnalyzer::endVisit(Continue const&) {
	if (m_loopDepth == 0)
		m_canContinue = true;
}
