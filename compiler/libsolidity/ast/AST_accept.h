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
/**
 * @author Christian <c@ethdev.com>
 * @date 2014
 * Implementation of the accept functions of AST nodes, included by AST.cpp to not clutter that
 * file with these mechanical implementations.
 */

#pragma once

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>

namespace solidity::frontend
{

void SourceUnit::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_nodes, _visitor);
	_visitor.endVisit(*this);
}

void SourceUnit::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_nodes, _visitor);
	_visitor.endVisit(*this);
}

void PragmaDirective::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_parameter, _visitor);
	_visitor.endVisit(*this);
}

void PragmaDirective::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_parameter, _visitor);
	_visitor.endVisit(*this);
}

void ImportDirective::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		for (SymbolAlias const& symbolAlias: symbolAliases())
			if (symbolAlias.symbol)
				symbolAlias.symbol->accept(_visitor);
	_visitor.endVisit(*this);
}

void ImportDirective::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		for (SymbolAlias const& symbolAlias: symbolAliases())
			if (symbolAlias.symbol)
				symbolAlias.symbol->accept(_visitor);
	_visitor.endVisit(*this);
}

void StructuredDocumentation::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void StructuredDocumentation::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void ContractDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		listAccept(m_baseContracts, _visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void ContractDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		listAccept(m_baseContracts, _visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void IdentifierPath::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void IdentifierPath::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void InheritanceSpecifier::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_baseName->accept(_visitor);
		if (m_arguments)
			listAccept(*m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void InheritanceSpecifier::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_baseName->accept(_visitor);
		if (m_arguments)
			listAccept(*m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void EnumDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_members, _visitor);
	_visitor.endVisit(*this);
}

void EnumDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_members, _visitor);
	_visitor.endVisit(*this);
}

void EnumValue::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void EnumValue::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void UserDefinedValueTypeDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_underlyingType)
			m_underlyingType->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void UserDefinedValueTypeDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_underlyingType)
			m_underlyingType->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void UsingForDirective::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		listAccept(m_functionsOrLibrary, _visitor);
		if (m_typeName)
			m_typeName->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void UsingForDirective::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		listAccept(m_functionsOrLibrary, _visitor);
		if (m_typeName)
			m_typeName->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void StructDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_members, _visitor);
	_visitor.endVisit(*this);
}

void StructDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_members, _visitor);
	_visitor.endVisit(*this);
}

void ParameterList::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_parameters, _visitor);
	_visitor.endVisit(*this);
}

void ParameterList::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_parameters, _visitor);
	_visitor.endVisit(*this);
}

void OverrideSpecifier::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_overrides, _visitor);
	_visitor.endVisit(*this);
}

void OverrideSpecifier::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_overrides, _visitor);
	_visitor.endVisit(*this);
}

void FunctionDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		m_parameters->accept(_visitor);
		if (m_returnParameters)
			m_returnParameters->accept(_visitor);
		if (m_experimentalReturnExpression)
			m_experimentalReturnExpression->accept(_visitor);
		listAccept(m_functionModifiers, _visitor);
		if (m_body)
			m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		m_parameters->accept(_visitor);
		if (m_returnParameters)
			m_returnParameters->accept(_visitor);
		if (m_experimentalReturnExpression)
			m_experimentalReturnExpression->accept(_visitor);
		listAccept(m_functionModifiers, _visitor);
		if (m_body)
			m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void VariableDeclaration::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_typeName)
			m_typeName->accept(_visitor);
		if (m_typeExpression)
			m_typeExpression->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		if (m_value)
			m_value->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void VariableDeclaration::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_typeName)
			m_typeName->accept(_visitor);
		if (m_typeExpression)
			m_typeExpression->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		if (m_value)
			m_value->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ModifierDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		if (m_body)
			m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ModifierDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
		if (m_overrides)
			m_overrides->accept(_visitor);
		if (m_body)
			m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ModifierInvocation::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_modifierName->accept(_visitor);
		if (m_arguments)
			listAccept(*m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void ModifierInvocation::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_modifierName->accept(_visitor);
		if (m_arguments)
			listAccept(*m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void EventDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void EventDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ErrorDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ErrorDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_documentation)
			m_documentation->accept(_visitor);
		m_parameters->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ElementaryTypeName::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void ElementaryTypeName::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void UserDefinedTypeName::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		this->pathNode().accept(_visitor);
	_visitor.endVisit(*this);
}

void UserDefinedTypeName::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		this->pathNode().accept(_visitor);
	_visitor.endVisit(*this);
}

void FunctionTypeName::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_parameterTypes->accept(_visitor);
		m_returnTypes->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionTypeName::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_parameterTypes->accept(_visitor);
		m_returnTypes->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Mapping::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_keyType->accept(_visitor);
		m_valueType->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Mapping::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_keyType->accept(_visitor);
		m_valueType->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Optional::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		for (ASTPointer<TypeName>& t : m_types)
			t->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Optional::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		for (const ASTPointer<TypeName>& t : m_types)
			t->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TvmVector::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TvmVector::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TvmStack::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TvmStack::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ArrayTypeName::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_baseType->accept(_visitor);
		if (m_length)
			m_length->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ArrayTypeName::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_baseType->accept(_visitor);
		if (m_length)
			m_length->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void FreeInlineAssembly::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void FreeInlineAssembly::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Block::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		listAccept(m_statements, _visitor);
	_visitor.endVisit(*this);
}

void Block::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		listAccept(m_statements, _visitor);
	_visitor.endVisit(*this);
}

void PlaceholderStatement::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void PlaceholderStatement::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void IfStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_trueBody->accept(_visitor);
		if (m_falseBody)
			m_falseBody->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void IfStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_trueBody->accept(_visitor);
		if (m_falseBody)
			m_falseBody->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TryCatchClause::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_parameters)
			m_parameters->accept(_visitor);
		m_block->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TryCatchClause::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_parameters)
			m_parameters->accept(_visitor);
		m_block->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TryStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
		m_clause->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TryStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
		m_clause->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void WhileStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void WhileStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ForStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_initExpression)
			m_initExpression->accept(_visitor);
		if (m_condExpression)
			m_condExpression->accept(_visitor);
		if (m_loopExpression)
			m_loopExpression->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ForStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_initExpression)
			m_initExpression->accept(_visitor);
		if (m_condExpression)
			m_condExpression->accept(_visitor);
		if (m_loopExpression)
			m_loopExpression->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ForEachStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_rangeDeclaration->accept(_visitor);
		m_rangeExpression->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void ForEachStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_rangeDeclaration->accept(_visitor);
		m_rangeExpression->accept(_visitor);
		m_body->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Continue::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Continue::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Break::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Break::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Return::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this)) {
		for (const ASTPointer<Expression>& opt : m_options)
			opt->accept(_visitor);
		if (m_expression)
			m_expression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Return::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this)) {
		for (const ASTPointer<Expression>& opt : m_options)
			opt->accept(_visitor);
		if (m_expression)
			m_expression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Throw::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Throw::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void RevertStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		m_errorCall->accept(_visitor);
	_visitor.endVisit(*this);
}

void RevertStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		m_errorCall->accept(_visitor);
	_visitor.endVisit(*this);
}

void EmitStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this)) {
		m_eventCall->accept(_visitor);
		for (const ASTPointer<Expression>& opt : m_options) {
			opt->accept(_visitor);
		}
	}
	_visitor.endVisit(*this);
}

void EmitStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this)) {
		m_eventCall->accept(_visitor);
		for (const ASTPointer<Expression>& opt : m_options) {
			opt->accept(_visitor);
		}
	}
	_visitor.endVisit(*this);
}

void ExpressionStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		if (m_expression)
			m_expression->accept(_visitor);
	_visitor.endVisit(*this);
}

void ExpressionStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		if (m_expression)
			m_expression->accept(_visitor);
	_visitor.endVisit(*this);
}

void VariableDeclarationStatement::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		for (ASTPointer<VariableDeclaration> const& var: m_variables)
			if (var)
				var->accept(_visitor);
		if (m_initialValue)
			m_initialValue->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void VariableDeclarationStatement::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		for (ASTPointer<VariableDeclaration> const& var: m_variables)
			if (var)
				var->accept(_visitor);
		if (m_initialValue)
			m_initialValue->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Conditional::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_trueExpression->accept(_visitor);
		m_falseExpression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Conditional::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		m_trueExpression->accept(_visitor);
		m_falseExpression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Assignment::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_leftHandSide->accept(_visitor);
		m_rightHandSide->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Assignment::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_leftHandSide->accept(_visitor);
		m_rightHandSide->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TupleExpression::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		for (auto const& component: m_components)
			if (component)
				component->accept(_visitor);
	_visitor.endVisit(*this);
}

void TupleExpression::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		for (auto const& component: m_components)
			if (component)
				component->accept(_visitor);
	_visitor.endVisit(*this);
}

void UnaryOperation::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		m_subExpression->accept(_visitor);
	_visitor.endVisit(*this);
}

void UnaryOperation::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		m_subExpression->accept(_visitor);
	_visitor.endVisit(*this);
}

void BinaryOperation::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_left->accept(_visitor);
		m_right->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void BinaryOperation::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_left->accept(_visitor);
		m_right->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionCall::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_expression->accept(_visitor);
		listAccept(m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionCall::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_expression->accept(_visitor);
		listAccept(m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void InitializerList::accept(ASTVisitor &_visitor) {
	if (_visitor.visit(*this))
	{
		listAccept(m_options, _visitor);
	}
	_visitor.endVisit(*this);
}

void InitializerList::accept(ASTConstVisitor &_visitor) const {
	if (_visitor.visit(*this))
	{
		listAccept(m_options, _visitor);
	}
	_visitor.endVisit(*this);
}

void CallList::accept(ASTVisitor &_visitor) {
	if (_visitor.visit(*this))
	{
		m_function->accept(_visitor);
		listAccept(m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void CallList::accept(ASTConstVisitor &_visitor) const {
	if (_visitor.visit(*this))
	{
		m_function->accept(_visitor);
		listAccept(m_arguments, _visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionCallOptions::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_expression->accept(_visitor);
		listAccept(m_options, _visitor);
	}
	_visitor.endVisit(*this);
}

void FunctionCallOptions::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_expression->accept(_visitor);
		listAccept(m_options, _visitor);
	}
	_visitor.endVisit(*this);
}

void NewExpression::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		m_typeName->accept(_visitor);
	_visitor.endVisit(*this);
}

void NewExpression::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		m_typeName->accept(_visitor);
	_visitor.endVisit(*this);
}

void MemberAccess::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
		m_expression->accept(_visitor);
	_visitor.endVisit(*this);
}

void MemberAccess::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
		m_expression->accept(_visitor);
	_visitor.endVisit(*this);
}

void IndexAccess::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_base->accept(_visitor);
		if (m_index)
			m_index->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void IndexAccess::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_base->accept(_visitor);
		if (m_index)
			m_index->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void IndexRangeAccess::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_base->accept(_visitor);
		if (m_start)
			m_start->accept(_visitor);
		if (m_end)
			m_end->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void IndexRangeAccess::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_base->accept(_visitor);
		if (m_start)
			m_start->accept(_visitor);
		if (m_end)
			m_end->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Identifier::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Identifier::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void ElementaryTypeNameExpression::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void ElementaryTypeNameExpression::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void MappingNameExpression::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void MappingNameExpression::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void OptionalNameExpression::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void OptionalNameExpression::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_type->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Literal::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Literal::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

/// Experimental Solidity nodes
/// @{
void TypeClassDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_typeVariable->accept(_visitor);
		if (m_documentation)
			m_documentation->accept(_visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void TypeClassDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_typeVariable->accept(_visitor);
		if (m_documentation)
			m_documentation->accept(_visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void TypeClassInstantiation::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		m_typeConstructor->accept(_visitor);
		if (m_argumentSorts)
			m_argumentSorts->accept(_visitor);
		m_class->accept(_visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void TypeClassInstantiation::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		m_typeConstructor->accept(_visitor);
		if (m_argumentSorts)
			m_argumentSorts->accept(_visitor);
		m_class->accept(_visitor);
		listAccept(m_subNodes, _visitor);
	}
	_visitor.endVisit(*this);
}

void TypeDefinition::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (m_arguments)
			m_arguments->accept(_visitor);
		if (m_typeExpression)
			m_typeExpression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TypeDefinition::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (m_arguments)
			m_arguments->accept(_visitor);
		if (m_typeExpression)
			m_typeExpression->accept(_visitor);
	}
	_visitor.endVisit(*this);
}


void TypeClassName::accept(ASTVisitor& _visitor)
{
	if (_visitor.visit(*this))
	{
		if (auto* path = std::get_if<ASTPointer<IdentifierPath>>(&m_name))
			(*path)->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void TypeClassName::accept(ASTConstVisitor& _visitor) const
{
	if (_visitor.visit(*this))
	{
		if (auto* path = std::get_if<ASTPointer<IdentifierPath>>(&m_name))
			(*path)->accept(_visitor);
	}
	_visitor.endVisit(*this);
}

void Builtin::accept(ASTVisitor& _visitor)
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}

void Builtin::accept(ASTConstVisitor& _visitor) const
{
	_visitor.visit(*this);
	_visitor.endVisit(*this);
}
/// @}

}
