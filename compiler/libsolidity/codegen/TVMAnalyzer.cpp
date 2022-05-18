/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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
 * @author TON Labs <connect@tonlabs.io>
 * @date 2019
 * AST analyzer specified to search for TVM specific issues.
 */

#include "TVMAnalyzer.hpp"
#include <liblangutil/ErrorReporter.h>
#include <libsolidity/codegen/TVMConstants.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace std;

TVMAnalyzer::TVMAnalyzer(ErrorReporter &_errorReporter, bool _structWarning):
	m_errorReporter(_errorReporter), m_structWarning(_structWarning)
{
}

bool TVMAnalyzer::analyze(const SourceUnit &_sourceUnit)
{
	_sourceUnit.accept(*this);
	return Error::containsOnlyWarnings(m_errorReporter.errors());
}

bool TVMAnalyzer::visit(ContractDefinition const& contract) {
	std::map<std::string, EventDefinition const*> used;
	for (EventDefinition const* event : contract.interfaceEvents()) {
		if (used.count(event->name())) {
			m_errorReporter.declarationError(
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

bool TVMAnalyzer::visit(Assignment const& _variable) {
	std::vector<const Identifier *> lefts;
	if (auto ident = dynamic_cast<const Identifier *>(&_variable.leftHandSide()))
		lefts.push_back(ident);
	if (auto tuple = dynamic_cast<const TupleExpression *>(&_variable.leftHandSide()))
		for (const auto& tup: tuple->components())
			if (auto ident = dynamic_cast<const Identifier *>(tup.get()))
				lefts.push_back(ident);

	for (auto ident: lefts) {
		if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
			solAssert(!var->name().empty(), "");
			if (dynamic_cast<const StructType *>(var->type()) && var->isLocalVariable())
				m_structLoads[std::make_pair(var->id(), var)] = &_variable;
		}
	}

	std::vector<const MemberAccess *> lefts2;
	if (auto ident = dynamic_cast<const MemberAccess *>(&_variable.leftHandSide()))
		lefts2.push_back(ident);
	if (auto tuple = dynamic_cast<const TupleExpression *>(&_variable.leftHandSide()))
		for (const auto& tup: tuple->components())
			if (auto ident = dynamic_cast<const MemberAccess *>(tup.get()))
				lefts2.push_back(ident);

	for (auto member: lefts2) {
		if (auto ident = dynamic_cast<const Identifier *>(&member->expression()))
			if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
				solAssert(!var->name().empty(), "");
				if (auto iter = m_structLoads.find(std::make_pair(var->id(), var));
					 iter != m_structLoads.end())
					 m_changedStructs.emplace(make_pair(make_pair(var->id(), var), &_variable));
			}
	}

	std::vector<const Identifier *> rights;
	if (auto ident = dynamic_cast<const Identifier *>(&_variable.rightHandSide()))
		rights.push_back(ident);
	if (auto tuple = dynamic_cast<const TupleExpression *>(&_variable.rightHandSide()))
		for (const auto& tup: tuple->components())
			if (auto ident = dynamic_cast<const Identifier *>(tup.get()))
				rights.push_back(ident);

	for (auto ident: rights) {
		if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
			solAssert(!var->name().empty(), "");
			if (dynamic_cast<const StructType *>(var->type())) {
				auto range = m_changedStructs.equal_range(make_pair(var->id(), var));
				m_changedStructs.erase(range.first, range.second);
			}
		}
	}

	return true;
}

bool TVMAnalyzer::visit(UnaryOperation const& _node) {
	auto op = _node.getOperator();
	if (op == Token::Inc || op == Token::Dec) {
		if (auto member = dynamic_cast<const MemberAccess*>(&_node.subExpression())) {
			if (auto ident = dynamic_cast<const Identifier*>(&member->expression())) {
				if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
					solAssert(!var->name().empty(), "");
					if (dynamic_cast<const StructType *>(var->type())) {
						if (auto iter = m_structLoads.find(std::make_pair(var->id(), var));
							 iter != m_structLoads.end()) {
							 m_changedStructs.emplace(make_pair(make_pair(var->id(), var), &_node));
						}
					}
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

	for (const auto& ret: _function.returnParameters())
			m_declaredReturns.emplace_back(ret->id(), ret.get());
		return true;
}

bool TVMAnalyzer::visit(Return const& _return) {
	if (m_currentFunction && _return.expression()) {
		std::vector<const Identifier *> rets;
		if (auto ident = dynamic_cast<const Identifier*>(_return.expression()))
			rets.push_back(ident);

		if (auto tuple = dynamic_cast<const TupleExpression*>(_return.expression()))
			for (const auto& tup: tuple->components())
				if (auto ident = dynamic_cast<const Identifier*>(tup.get()))
					rets.push_back(ident);

		for (auto ident: rets) {
			if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
				solAssert(!var->name().empty(), "");
				if (dynamic_cast<const StructType *>(var->type())) {
					auto range = m_changedStructs.equal_range(make_pair(var->id(), var));
					m_changedStructs.erase(range.first, range.second);
				}
			}
		}
	}

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
	for (const ASTPointer<const Expression>& argument: _functionCall.arguments()) {
		if (auto ident = dynamic_cast<const Identifier*>(argument.get())) {
			if (auto var = dynamic_cast<VariableDeclaration const*>(ident->annotation().referencedDeclaration)) {
				solAssert(!var->name().empty(), "");
				if (dynamic_cast<const StructType *>(var->type())) {
					auto range = m_changedStructs.equal_range(make_pair(var->id(), var));
					m_changedStructs.erase(range.first, range.second);
				}
			}
		}
	}
	return true;
}

bool TVMAnalyzer::visit(VariableDeclaration const& _variable) {
	if (dynamic_cast<const StructType *>(_variable.type()) && _variable.isLocalVariable() && !_variable.name().empty())
		m_structLoads[std::make_pair(_variable.id(), &_variable)] = &_variable;
	return true;
}

void TVMAnalyzer::endVisit(FunctionDefinition const&) {
	if (m_currentFunction && !m_currentFunction->body().statements().empty())
		if (m_structWarning)
			for (auto const& var: m_changedStructs)
				if (!var.first.second->attribute() || *var.first.second->attribute() != TvmConst::Attributes::MuteStructWarning())
					if (find(m_declaredReturns.begin(), m_declaredReturns.end(), var.first) == m_declaredReturns.end())
						m_errorReporter.warning(var.second->location(), "Struct variable was modified but was not used or saved.\nUse attribute [[maybe_unsaved]] to suspend this warning.");

	m_declaredReturns.clear();
	m_structLoads.clear();
	m_changedStructs.clear();
	m_currentFunction = nullptr;
}

void TVMAnalyzer::endVisit(PragmaDirective const& _pragma) {
	if (_pragma.literals().size() >= 1 && _pragma.literals().at(0) == "copyleft") {
		std::vector<ASTPointer<Expression>> params = _pragma.parameter();

		auto checkConstIneger = [&](Expression const& _e, SourceLocation const& loc, const std::string& msg, bigint max_val){
			if (_e.annotation().type->category() != Type::Category::RationalNumber) {
				m_errorReporter.syntaxError(loc, msg);
				return;
			}
			auto number = dynamic_cast<RationalNumberType const *>(_e.annotation().type);
			solAssert(number, "");
			if (number->isFractional()) {
				m_errorReporter.syntaxError(loc, msg);
				return;
			}
			bigint val = number->value();
			if (val < 0 || val >= max_val) {
				m_errorReporter.syntaxError(loc, msg);
				return;
			}
		};

		// check type
		checkConstIneger(*params.at(0), params.at(0)->location(),
						 "Expected constant integer type (uint8).", bigint(1) << 8);

		// check address
		checkConstIneger(*params.at(1), params.at(1)->location(),
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
			case FunctionType::Kind::TVMCode:
				m_hasTvmCode = true;
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
