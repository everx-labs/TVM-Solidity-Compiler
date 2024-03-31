/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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

#include "TVMAnalyzer.hpp"
#include <liblangutil/ErrorReporter.h>
#include <libsolidity/codegen/TVMConstants.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace std;

TVMAnalyzer::TVMAnalyzer(ErrorReporter &_errorReporter):
	m_errorReporter(_errorReporter)
{
}

bool TVMAnalyzer::analyze(const SourceUnit &_sourceUnit)
{
	_sourceUnit.accept(*this);
	return !m_errorReporter.hasErrors();
}

bool TVMAnalyzer::visit(MemberAccess const& _node) {
	auto funType = to<FunctionType>(_node.annotation().type);
	if (funType) {
		if (funType->hasBoundFirstArgument()) {
			auto printError = [&]{
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

bool TVMAnalyzer::visit(ContractDefinition const& contract) {
	std::map<std::string, EventDefinition const*> used;
	for (EventDefinition const* event : contract.definedInterfaceEvents()) {
		if (used.count(event->name())) {
			m_errorReporter.declarationError(
				2018_error,
				event->location(),
				SecondarySourceLocation().append("Another declaration is here:", used.at(event->name())->location()),
				"Event overriding is not supported."
			);
		} else {
			used[event->name()] = event;
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
			return std::find_if(b, e, [&](ASTPointer<ASTString> const& x){
				return name == *x;
			}) != e;
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

void TVMAnalyzer::endVisit(FunctionDefinition const&) {
	m_currentFunction = nullptr;
}

void TVMAnalyzer::endVisit(FunctionCall const&) {
	solAssert(!m_functionCall.empty(), "");
	m_functionCall.pop_back();
}

void TVMAnalyzer::endVisit(PragmaDirective const& _pragma) {
	if (!_pragma.literals().empty() && _pragma.literals().at(0) == "copyleft") {
		std::vector<ASTPointer<Expression>> params = _pragma.parameter();

		auto checkConstInteger = [&](Expression const& _e, SourceLocation const& loc, const std::string& msg, const bigint& max_val){
			if (_e.annotation().type->category() != Type::Category::RationalNumber) {
				m_errorReporter.syntaxError(5514_error, loc, msg);
				return;
			}
			auto number = dynamic_cast<RationalNumberType const *>(_e.annotation().type);
			solAssert(number, "");
			if (number->isFractional()) {
				m_errorReporter.syntaxError(8784_error, loc, msg);
				return;
			}
			bigint val = number->value2();
			if (val < 0 || val >= max_val) {
				m_errorReporter.syntaxError(5971_error, loc, msg);
				return;
			}
		};

		// check type
		checkConstInteger(*params.at(0), params.at(0)->location(),
						 "Expected constant integer type (uint8).", bigint(1) << 8);

		// check address
		checkConstInteger(*params.at(1), params.at(1)->location(),
						 "Expected constant integer type (uint256).", bigint(1) << 256);
	}
}

ContactsUsageScanner::ContactsUsageScanner(const ContractDefinition &cd) {
	for (ContractDefinition const* base : cd.annotation().linearizedBaseContracts) {
		base->accept(*this);
	}
}

bool ContactsUsageScanner::visit(FunctionCall const& _functionCall) {
	Expression const& expr = _functionCall.expression();
	Type const* exprType = getType(&expr);
	auto funType = to<FunctionType>(exprType);
	if (funType) {
		if (funType->hasDeclaration() &&
			isIn(funType->kind(), FunctionType::Kind::Internal, FunctionType::Kind::DelegateCall))
		{
			// Library functions aren't part of contract definition, that's why we use lazy visiting
			Declaration const& decl = funType->declaration();
			if (!m_usedFunctions.count(&decl)) {
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

	if (_functionCall.isAwait()) {
		m_hasAwaitCall = true;

		Type const* expressionType = getType(&_functionCall.expression());
		auto functionType = to<FunctionType>(expressionType);
		auto fd = to<FunctionDefinition>(&functionType->declaration());
		solAssert(fd, "");
		m_awaitFunctions.insert(fd);
	}
	return true;
}

bool ContactsUsageScanner::visit(const MemberAccess &_node) {
	if (getType(&_node.expression())->category() == Type::Category::Magic) {
		auto identifier = to<Identifier>(&_node.expression());
		if (identifier) {
			if (identifier->name() == "msg" && _node.memberName() == "sender") {
				m_hasMsgSender = true;
			}
		}
	}
	return true;
}

bool ContactsUsageScanner::visit(const FunctionDefinition &fd) {
	if (fd.isResponsible())
		m_hasResponsibleFunction = true;
	return true;
}

bool withPrelocatedRetValues(const FunctionDefinition *f) {
	LocationReturn locationReturn = ::notNeedsPushContWhenInlining(f->body());
	if (!f->returnParameters().empty() && isIn(locationReturn, LocationReturn::noReturn, LocationReturn::Anywhere)) {
		return true;
	}

	for (const ASTPointer<VariableDeclaration> &retArg : f->returnParameters()) {
		if (!retArg->name().empty()) {
			return true;
		}
	}
	return !f->modifiers().empty();
}

LocationReturn notNeedsPushContWhenInlining(const Block &_block) {

	ast_vec<Statement> statements = _block.statements();

	CFAnalyzer bodyScanner{_block};
	if (!bodyScanner.canReturn()) {
		return LocationReturn::noReturn;
	}

	for (std::vector<int>::size_type i = 0; i + 1 < statements.size(); ++i) {
		CFAnalyzer scanner{*statements[i].get()};
		if (scanner.canReturn()) {
			return LocationReturn::Anywhere;
		}
	}
	bool isLastStatementReturn = to<Return>(statements.back().get()) != nullptr;
	return isLastStatementReturn ? LocationReturn::Last : LocationReturn::Anywhere;
}

CFAnalyzer::CFAnalyzer(Statement const &node) {
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

bool CFAnalyzer::visit(ForEachStatement const &) {
	return startLoop();
}

bool CFAnalyzer::visit(WhileStatement const &) {
	return startLoop();
}

bool CFAnalyzer::visit(ForStatement const &) {
	return startLoop();
}

void CFAnalyzer::endVisit(ForEachStatement const &) {
	m_loopDepth--;
}

void CFAnalyzer::endVisit(WhileStatement const &) {
	m_loopDepth--;
}

void CFAnalyzer::endVisit(ForStatement const &) {
	m_loopDepth--;
}

void CFAnalyzer::endVisit(Return const &) {
	m_canReturn = true;
}

void CFAnalyzer::endVisit(Break const &) {
	if (m_loopDepth == 0)
		m_canBreak = true;
}

void CFAnalyzer::endVisit(Continue const &) {
	if (m_loopDepth == 0)
		m_canContinue = true;
}
