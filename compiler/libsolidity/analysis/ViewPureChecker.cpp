/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0

#include <libsolidity/analysis/ViewPureChecker.h>
#include <libsolidity/ast/ExperimentalFeatures.h>
#include <liblangutil/ErrorReporter.h>

#include <functional>
#include <utility>
#include <variant>

using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;

bool ViewPureChecker::check()
{
	for (auto const& source: m_ast)
		source->accept(*this);

	return !m_errors;
}

bool ViewPureChecker::visit(ImportDirective const&)
{
	return false;
}

bool ViewPureChecker::visit(FunctionDefinition const& _funDef)
{
	solAssert(!m_currentFunction, "");
	m_currentFunction = &_funDef;
	m_bestMutabilityAndLocation = {StateMutability::Pure, _funDef.location()};
	if (ContractDefinition const* contr = _funDef.annotation().contract) {
		if (contr->isLibrary() && _funDef.stateMutability() != StateMutability::NonPayable) {
			m_errorReporter.warning(3818_error, _funDef.location(),
									"Library functions must have default mutability. Delete keyword view or pure.");
		}
	}
	if (_funDef.isFree() && !_funDef.isInlineAssembly() && _funDef.stateMutability() != StateMutability::Pure) {
		m_errorReporter.warning(4029_error, _funDef.location(),
								"Free function must have pure mutability.");
	}
	return true;
}

void ViewPureChecker::endVisit(FunctionDefinition const& _funDef)
{
	solAssert(m_currentFunction == &_funDef, "");
	for (ASTPointer<ModifierInvocation> const& mod : m_currentFunction->modifiers()) {
		auto modDef = dynamic_cast<ModifierDefinition const*>(mod->name().annotation().referencedDeclaration);
		if (modDef) {
			MutabilityAndLocation mutAndLoc = m_inferredMutability.at(modDef);
			reportMutability(mutAndLoc.mutability, mutAndLoc.location, m_currentFunction->location());
		}
	}

	ContractDefinition const* contract = _funDef.annotation().contract;
	if (
		m_bestMutabilityAndLocation.mutability < _funDef.stateMutability() &&
		_funDef.isImplemented() &&
		!_funDef.body().statements().empty() &&
		!_funDef.isConstructor() &&
		!_funDef.overrides() &&
		!(contract && contract->isLibrary()) &&
		!_funDef.isFree() &&
		!_funDef.isFallback() &&
		!_funDef.isReceive() &&
		!_funDef.virtualSemantics() &&
		!_funDef.isInlineAssembly()
	)
		m_errorReporter.warning(
			2018_error,
			_funDef.location(),
			"Function state mutability can be restricted to " + stateMutabilityToString(m_bestMutabilityAndLocation.mutability)
		);
	m_currentFunction = nullptr;
}

bool ViewPureChecker::visit(ModifierDefinition const& _modifier)
{
	solAssert(m_currentFunction == nullptr, "");
	m_bestMutabilityAndLocation = {StateMutability::Pure, _modifier.location()};
	return true;
}

void ViewPureChecker::endVisit(ModifierDefinition const& _modifierDef)
{
	solAssert(m_currentFunction == nullptr, "");
	m_inferredMutability[&_modifierDef] = std::move(m_bestMutabilityAndLocation);
}

void ViewPureChecker::endVisit(Identifier const& _identifier)
{
	Declaration const* declaration = _identifier.annotation().referencedDeclaration;
	solAssert(declaration, "");

	StateMutability mutability = StateMutability::Pure;

	bool writes = _identifier.annotation().willBeWrittenTo;
	if (VariableDeclaration const* varDecl = dynamic_cast<VariableDeclaration const*>(declaration))
	{
		if (varDecl->immutable())
		{
			// Immutables that are assigned literals are pure.
			if (!(varDecl->value() && varDecl->value()->annotation().type->category() == Type::Category::RationalNumber))
				mutability = StateMutability::View;
		}
		else if (varDecl->isStateVariable() && !varDecl->isConstant())
			mutability = writes ? StateMutability::NonPayable : StateMutability::View;
	}
	else if (MagicVariableDeclaration const* magicVar = dynamic_cast<MagicVariableDeclaration const*>(declaration))
	{
		switch (magicVar->type()->category())
		{
		case Type::Category::Contract:
			solAssert(_identifier.name() == "this", "");
			break;
		case Type::Category::Integer:
			solAssert(_identifier.name() == "now", "");
			mutability = StateMutability::Pure;
			break;
		default:
			break;
		}
	}

	reportMutability(mutability, _identifier.location());
}

void ViewPureChecker::reportMutability(
	StateMutability _mutability,
	SourceLocation const& _location,
	std::optional<SourceLocation> const& funcDecl
)
{
	if (_mutability > m_bestMutabilityAndLocation.mutability)
		m_bestMutabilityAndLocation = MutabilityAndLocation{_mutability, _location};
	if (!m_currentFunction || _mutability <= m_currentFunction->stateMutability())
		return;

	// Check for payable here, because any occurrence of `msg.value`
	// will set mutability to payable.
	if (_mutability == StateMutability::View)
	{
		auto errText = "Function declared as pure, but this expression (potentially) reads from the "
				 "environment or state and thus requires \"view\".";
		if (funcDecl) {
			m_errorReporter.typeError(
				6669_error,
				_location,
				SecondarySourceLocation().append("Function declaration is here", *funcDecl),
				errText
			);
		} else {
			m_errorReporter.typeError(7706_error, _location, errText);
		}
		m_errors = true;
	}
	else if (_mutability == StateMutability::NonPayable)
	{
		auto errText = "Function declared as " +
					   stateMutabilityToString(m_currentFunction->stateMutability()) +
					   ", but this expression (potentially) modifies the state and thus "
					   "requires the default.";
		if (funcDecl) {
			m_errorReporter.typeError(
				5861_error,
				_location,
				SecondarySourceLocation().append("Function declaration is here", *funcDecl),
				errText
			);
		} else {
			m_errorReporter.typeError(9527_error, _location, errText);
		}
		m_errors = true;
	}
	else
		solAssert(false, "");

	solAssert(
		m_currentFunction->stateMutability() == StateMutability::View ||
		m_currentFunction->stateMutability() == StateMutability::Pure ||
		m_currentFunction->stateMutability() == StateMutability::NonPayable,
		""
	);
}

ViewPureChecker::MutabilityAndLocation const& ViewPureChecker::modifierMutability(
	ModifierDefinition const& _modifier
)
{
	if (!m_inferredMutability.count(&_modifier))
	{
		MutabilityAndLocation bestMutabilityAndLocation{};
		FunctionDefinition const* currentFunction = nullptr;
		std::swap(bestMutabilityAndLocation, m_bestMutabilityAndLocation);
		std::swap(currentFunction, m_currentFunction);

		_modifier.accept(*this);

		std::swap(bestMutabilityAndLocation, m_bestMutabilityAndLocation);
		std::swap(currentFunction, m_currentFunction);
	}
	return m_inferredMutability.at(&_modifier);
}

void ViewPureChecker::reportFunctionCallMutability(StateMutability _mutability, langutil::SourceLocation const& _location)
{
	reportMutability(_mutability, _location);
}

void ViewPureChecker::endVisit(BinaryOperation const& _binaryOperation)
{
	if (*_binaryOperation.annotation().userDefinedFunction != nullptr)
		reportFunctionCallMutability((*_binaryOperation.annotation().userDefinedFunction)->stateMutability(), _binaryOperation.location());
}

void ViewPureChecker::endVisit(UnaryOperation const& _unaryOperation)
{
	if (*_unaryOperation.annotation().userDefinedFunction != nullptr)
		reportFunctionCallMutability((*_unaryOperation.annotation().userDefinedFunction)->stateMutability(), _unaryOperation.location());
}

void ViewPureChecker::endVisit(FunctionCall const& _functionCall)
{
	if (*_functionCall.annotation().kind != FunctionCallKind::FunctionCall)
		return;

	StateMutability mutability;
	auto ma = dynamic_cast<MemberAccess const *>(&_functionCall.expression());
	auto opt = dynamic_cast<FunctionCallOptions const *>(&_functionCall.expression());
	if ((ma == nullptr) && opt) {
		ma = dynamic_cast<MemberAccess const *>(&opt->expression());
	}
	if (opt && dynamic_cast<NewExpression const *>(&opt->expression())) {
		mutability = StateMutability::Pure; // deploy via new
	} else {
		if (ma && ma->expression().annotation().type->category() == Type::Category::Contract) {
			mutability = StateMutability::Pure; // call function of another contract
		} else {
			bool isLibCall{};
			if (ma) {
				if (auto libFunction = dynamic_cast<FunctionDefinition const *>(ma->annotation().referencedDeclaration)) {
					DeclarationAnnotation const &da = libFunction->annotation();
					if (da.contract->isLibrary()) {
						isLibCall = true;
						auto t = ma->expression().annotation().type;
						if (t->category() == Type::Category::TypeType) {
							// uint z = MyLib.sum(a, b);
							mutability = StateMutability::Pure;
						} else {
							// TODO: check mutability more detail
							// a.sum(b) - we can not modify a in function a
							mutability = isStateVariable(*ma) ? StateMutability::NonPayable : StateMutability::Pure;
						}
					}
				}
			}
			if (!isLibCall) {
				bool isFree{};
				auto ident = dynamic_cast<Identifier const*>(&_functionCall.expression());
				if (ident) {
					auto funcDef = dynamic_cast<FunctionDefinition const*>(ident->annotation().referencedDeclaration);
					isFree = funcDef && funcDef->isFree() && !funcDef->isInlineAssembly();
				}
				if (isFree)
					mutability = StateMutability::Pure;
				else
					mutability = dynamic_cast<FunctionType const &>(*_functionCall.expression().annotation().type).stateMutability();
			}
		}
	}
	reportMutability(mutability, _functionCall.location());
}

bool ViewPureChecker::visit(MemberAccess const& _memberAccess)
{
	// Catch the special case of `this.f.selector` which is a pure expression.
	ASTString const& member = _memberAccess.memberName();
	if (
		_memberAccess.expression().annotation().type->category() == Type::Category::Function &&
		member == "selector"
	)
		if (auto const* expr = dynamic_cast<MemberAccess const*>(&_memberAccess.expression()))
			if (auto const* exprInt = dynamic_cast<Identifier const*>(&expr->expression()))
				if (exprInt->name() == "this")
					// Do not continue visiting.
					return false;
	return true;
}

void ViewPureChecker::endVisit(MemberAccess const& _memberAccess)
{
	StateMutability mutability = StateMutability::Pure;
	bool writes = _memberAccess.annotation().willBeWrittenTo;
	const bool isStateVar = isStateVariable(_memberAccess);

	ASTString const& member = _memberAccess.memberName();
	switch (_memberAccess.expression().annotation().type->category())
	{
	case Type::Category::Optional:
		if (member == "set" || member == "reset")
			if (isStateVar)
				mutability = StateMutability::NonPayable;
		if (member == "get" || member == "hasValue")
			if (isStateVar)
				mutability = StateMutability::View;
		break;
	case Type::Category::Mapping:
		if (member == "delMin" ||
			member == "delMax" ||
			member == "replace" ||
			member == "add" ||
			member == "getSet" ||
			member == "getAdd" ||
			member == "getDel" ||
			member == "getReplace") {
			if (isStateVar)
				mutability = StateMutability::NonPayable;
		}
		break;
	case Type::Category::Address:
		if (member == "balance" || member == "currencies")
			mutability = StateMutability::Pure;
		break;
	case Type::Category::Magic:
	{
		using MagicMember = std::pair<MagicType::Kind, std::string>;
		std::set<MagicMember> static const pureMembers{
			{MagicType::Kind::ABI, "codeSalt"},
			{MagicType::Kind::ABI, "decode"},
			{MagicType::Kind::ABI, "decodeData"},
			{MagicType::Kind::ABI, "encode"},
			{MagicType::Kind::ABI, "encodeBody"},
			{MagicType::Kind::ABI, "encodeCall"},
			{MagicType::Kind::ABI, "encodeData"},
			{MagicType::Kind::ABI, "encodeExtMsg"},
			{MagicType::Kind::ABI, "encodeIntMsg"},
			{MagicType::Kind::ABI, "encodeOldDataInit"},
			{MagicType::Kind::ABI, "encodePacked"},
			{MagicType::Kind::ABI, "encodeStateInit"},
			{MagicType::Kind::ABI, "encodeWithSelector"},
			{MagicType::Kind::ABI, "encodeWithSignature"},
			{MagicType::Kind::ABI, "functionId"},
			{MagicType::Kind::ABI, "setCodeSalt"},
			{MagicType::Kind::ABI, "stateInitHash"},
			{MagicType::Kind::Block, "blockhash"},
			{MagicType::Kind::Block, "logicaltime"},
			{MagicType::Kind::Block, "timestamp"},
			{MagicType::Kind::Gosh, "applyBinPatch"},
			{MagicType::Kind::Gosh, "applyBinPatchQ"},
			{MagicType::Kind::Gosh, "applyPatch"},
			{MagicType::Kind::Gosh, "applyPatchQ"},
			{MagicType::Kind::Gosh, "applyZipBinPatch"},
			{MagicType::Kind::Gosh, "applyZipBinPatchQ"},
			{MagicType::Kind::Gosh, "applyZipPatch"},
			{MagicType::Kind::Gosh, "applyZipPatchQ"},
			{MagicType::Kind::Gosh, "diff"},
			{MagicType::Kind::Gosh, "unzip"},
			{MagicType::Kind::Gosh, "zip"},
			{MagicType::Kind::Gosh, "zipDiff"},
			{MagicType::Kind::Math, "abs"},
			{MagicType::Kind::Math, "divc"},
			{MagicType::Kind::Math, "divmod"},
			{MagicType::Kind::Math, "divr"},
			{MagicType::Kind::Math, "max"},
			{MagicType::Kind::Math, "min"},
			{MagicType::Kind::Math, "minmax"},
			{MagicType::Kind::Math, "modpow2"},
			{MagicType::Kind::Math, "muldiv"},
			{MagicType::Kind::Math, "muldivc"},
			{MagicType::Kind::Math, "muldivmod"},
			{MagicType::Kind::Math, "muldivr"},
			{MagicType::Kind::Math, "sign"},
			{MagicType::Kind::Message, "body"},
			{MagicType::Kind::Message, "createdAt"},
			{MagicType::Kind::Message, "currencies"},
			{MagicType::Kind::Message, "data"},
			{MagicType::Kind::Message, "forwardFee"},
			{MagicType::Kind::Message, "hasStateInit"},
			{MagicType::Kind::Message, "importFee"},
			{MagicType::Kind::Message, "isExternal"},
			{MagicType::Kind::Message, "isInternal"},
			{MagicType::Kind::Message, "isTickTock"},
			{MagicType::Kind::Message, "pubkey"},
			{MagicType::Kind::Message, "sender"},
			{MagicType::Kind::Message, "sig"},
			{MagicType::Kind::Message, "value"},
			{MagicType::Kind::MetaType, "creationCode"},
			{MagicType::Kind::MetaType, "interfaceId"},
			{MagicType::Kind::MetaType, "max"},
			{MagicType::Kind::MetaType, "min"},
			{MagicType::Kind::MetaType, "name"},
			{MagicType::Kind::MetaType, "runtimeCode"},
			{MagicType::Kind::Rnd, "getSeed"},
			{MagicType::Kind::Rnd, "next"},
			{MagicType::Kind::Rnd, "setSeed"},
			{MagicType::Kind::Rnd, "shuffle"},
			{MagicType::Kind::TVM, "accept"},
			{MagicType::Kind::TVM, "bindump"},
			{MagicType::Kind::TVM, "buildDataInit"},
			{MagicType::Kind::TVM, "buildExtMsg"},
			{MagicType::Kind::TVM, "buildIntMsg"},
			{MagicType::Kind::TVM, "buildStateInit"},
			{MagicType::Kind::TVM, "buyGas"},
			{MagicType::Kind::TVM, "checkSign"},
			{MagicType::Kind::TVM, "code"},
			{MagicType::Kind::TVM, "codeSalt"},
			{MagicType::Kind::TVM, "configParam"},
			{MagicType::Kind::TVM, "encodeBody"},
			{MagicType::Kind::TVM, "exit"},
			{MagicType::Kind::TVM, "exit1"},
			{MagicType::Kind::TVM, "functionId"},
			{MagicType::Kind::TVM, "hash"},
			{MagicType::Kind::TVM, "hexdump"},
			{MagicType::Kind::TVM, "initCodeHash"},
			{MagicType::Kind::TVM, "log"},
			{MagicType::Kind::TVM, "rawConfigParam"},
			{MagicType::Kind::TVM, "rawReserve"},
			{MagicType::Kind::TVM, "sendrawmsg"},
			{MagicType::Kind::TVM, "setCodeSalt"},
			{MagicType::Kind::TVM, "setCurrentCode"},
			{MagicType::Kind::TVM, "setGasLimit"},
			{MagicType::Kind::TVM, "setcode"},
			{MagicType::Kind::TVM, "stateInitHash"},
			{MagicType::Kind::Transaction, "logicaltime"},
			{MagicType::Kind::Transaction, "storageFee"},
			{MagicType::Kind::Transaction, "timestamp"},
		};
		std::set<MagicMember> static const nonpayableMembers{
			{MagicType::Kind::TVM, "commit"},
			{MagicType::Kind::TVM, "rawCommit"},
			{MagicType::Kind::TVM, "setData"},
			{MagicType::Kind::TVM, "resetStorage"}
		};

		auto const& type = dynamic_cast<MagicType const&>(*_memberAccess.expression().annotation().type);
		MagicMember magicMember(type.kind(), member);

		if (!pureMembers.count(magicMember))
			mutability = StateMutability::View;
		if (nonpayableMembers.count(magicMember))
			mutability = StateMutability::NonPayable;

		break;
	}
	case Type::Category::Struct:
	{
		if (isStateVar)
			mutability = writes ? StateMutability::NonPayable : StateMutability::View;
		break;
	}
	case Type::Category::Array:
	{
		if (member == "length")
			if (isStateVar)
				mutability = StateMutability::View;

		if (member == "pop" || member == "push" || member == "append")
			if (isStateVar)
				mutability = StateMutability::NonPayable;
		break;
	}
	default:
	{
		if (VariableDeclaration const* varDecl = dynamic_cast<VariableDeclaration const*>(
			_memberAccess.annotation().referencedDeclaration
		))
			if (varDecl->isStateVariable() && !varDecl->isConstant())
				mutability = writes ? StateMutability::NonPayable : StateMutability::View;
		break;
	}
	}
	reportMutability(mutability, _memberAccess.location());
}

void ViewPureChecker::endVisit(IndexAccess const& _indexAccess)
{
	if (!_indexAccess.indexExpression())
		solAssert(_indexAccess.annotation().type->category() == Type::Category::TypeType, "");
	else
	{
		bool writes = _indexAccess.annotation().willBeWrittenTo;
		if (isStateVariable(_indexAccess))
			reportMutability(writes ? StateMutability::NonPayable : StateMutability::View, _indexAccess.location());
	}
}

void ViewPureChecker::endVisit(IndexRangeAccess const& _indexRangeAccess)
{
	bool writes = _indexRangeAccess.annotation().willBeWrittenTo;
	if (isStateVariable(_indexRangeAccess))
		reportMutability(writes ? StateMutability::NonPayable : StateMutability::View, _indexRangeAccess.location());
}

bool ViewPureChecker::isStateVariable(Expression const& expression) const {
	if (auto m = dynamic_cast<MemberAccess const *>(&expression)) {
		return isStateVariable(m->expression());
	}
	if (auto index = dynamic_cast<IndexAccess const *>(&expression)) {
		return isStateVariable(index->baseExpression());
	}
	if (auto indef = dynamic_cast<Identifier const*>(&expression)) {
		auto varDecl = dynamic_cast<VariableDeclaration const *>(indef->annotation().referencedDeclaration);
		return varDecl != nullptr && varDecl->isStateVariable();
	}
	return false;
}

void ViewPureChecker::endVisit(ModifierInvocation const& _modifier)
{
	if (ModifierDefinition const* mod = dynamic_cast<decltype(mod)>(_modifier.name().annotation().referencedDeclaration))
	{
		MutabilityAndLocation const& mutAndLocation = modifierMutability(*mod);
		reportMutability(mutAndLocation.mutability, _modifier.location(), mutAndLocation.location);
	}
	else
		solAssert(dynamic_cast<ContractDefinition const*>(_modifier.name().annotation().referencedDeclaration), "");
}
