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

bool TVMAnalyzer::visit(Assignment const& _variable) {
	std::vector<const Identifier *> lefts;
	if (auto ident = dynamic_cast<const Identifier *>(&_variable.leftHandSide()))
		lefts.push_back(ident);
	if (auto tuple = dynamic_cast<const TupleExpression *>(&_variable.leftHandSide()))
		for (auto tup: tuple->components())
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
		for (auto tup: tuple->components())
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
		for (auto tup: tuple->components())
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

	for (auto ret: _function.returnParameters())
			m_declaredReturns.push_back(make_pair(ret->id(), ret.get()));
		return true;
}

bool TVMAnalyzer::visit(Return const& _return) {
	if (m_currentFunction && _return.expression()) {
		std::vector<const Identifier *> rets;
		if (auto ident = dynamic_cast<const Identifier*>(_return.expression()))
			rets.push_back(ident);

		if (auto tuple = dynamic_cast<const TupleExpression*>(_return.expression()))
			for (auto tup: tuple->components())
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

	return true;
}

bool TVMAnalyzer::visit(FunctionCall const& _functionCall) {
	for (ASTPointer<const Expression> argument: _functionCall.arguments()) {
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
	if (dynamic_cast<const StructType *>(_variable.type()) && _variable.isLocalVariable() && _variable.name().size())
		m_structLoads[std::make_pair(_variable.id(), &_variable)] = &_variable;
	return true;
}

void TVMAnalyzer::endVisit(FunctionDefinition const&) {
	if (m_currentFunction && !m_currentFunction->body().statements().empty())
		if (m_structWarning)
			for (auto const& var: m_changedStructs)
				if (!var.first.second->attribute() || *var.first.second->attribute() != TvmConst::Attributes::MuteStructWarning)
					if (find(m_declaredReturns.begin(), m_declaredReturns.end(), var.first) == m_declaredReturns.end())
						m_errorReporter.warning(var.second->location(), "Struct variable was modified but was not used or saved.\nUse attribute [[maybe_unsaved]] to suspend this warning.");

	m_declaredReturns.clear();
	m_structLoads.clear();
	m_changedStructs.clear();
	m_currentFunction = nullptr;
}

