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
/**
 * @author Christian <c@ethdev.com>
 * @date 2015
 * Component that resolves type names to types and annotates the AST accordingly.
 */

#include <libsolidity/analysis/ReferencesResolver.h>
#include <libsolidity/analysis/NameAndTypeResolver.h>
#include <libsolidity/analysis/ConstantEvaluator.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/Exceptions.h>

#include <libsolutil/StringUtils.h>

#include <boost/algorithm/string.hpp>
#include <boost/range/adaptor/transformed.hpp>

using namespace std;
using namespace solidity::langutil;

namespace solidity::frontend
{

bool ReferencesResolver::resolve(ASTNode const& _root)
{
	_root.accept(*this);
	return !m_errorOccurred;
}

bool ReferencesResolver::visit(Block const& _block)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_block);
	return true;
}

void ReferencesResolver::endVisit(Block const& _block)
{
	if (!m_resolveInsideCode)
		return;

	m_resolver.setScope(_block.scope());
}

bool ReferencesResolver::visit(ForStatement const& _for)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_for);
	return true;
}

void ReferencesResolver::endVisit(ForStatement const& _for)
{
	if (!m_resolveInsideCode)
		return;
	m_resolver.setScope(_for.scope());
}

void ReferencesResolver::endVisit(VariableDeclarationStatement const& _varDeclStatement)
{
	if (!m_resolveInsideCode)
		return;
	for (auto const& var: _varDeclStatement.declarations())
		if (var)
			m_resolver.activateVariable(var->name());
}

bool ReferencesResolver::visit(Identifier const& _identifier)
{
	auto declarations = m_resolver.nameFromCurrentScope(_identifier.name());
	if (declarations.empty())
	{
		string suggestions = m_resolver.similarNameSuggestions(_identifier.name());
		string errorMessage = "Undeclared identifier.";
		if (!suggestions.empty())
		{
			if ("\"" + _identifier.name() + "\"" == suggestions)
				errorMessage += " " + std::move(suggestions) + " is not (or not yet) visible at this point.";
			else
				errorMessage += " Did you mean " + std::move(suggestions) + "?";
		}
		declarationError(_identifier.location(), errorMessage);
	}
	else if (declarations.size() == 1)
		_identifier.annotation().referencedDeclaration = declarations.front();
	else
		_identifier.annotation().overloadedDeclarations =
			m_resolver.cleanedDeclarations(_identifier, declarations);
	return false;
}

bool ReferencesResolver::visit(ElementaryTypeName const& _typeName)
{
	if (!_typeName.annotation().type)
		_typeName.annotation().type = TypeProvider::fromElementaryTypeName(_typeName.typeName());
	return true;
}

bool ReferencesResolver::visit(FunctionDefinition const& _functionDefinition)
{
	m_returnParameters.push_back(_functionDefinition.returnParameterList().get());
	return true;
}

void ReferencesResolver::endVisit(FunctionDefinition const&)
{
	solAssert(!m_returnParameters.empty(), "");
	m_returnParameters.pop_back();
}

bool ReferencesResolver::visit(ModifierDefinition const&)
{
	m_returnParameters.push_back(nullptr);
	return true;
}

void ReferencesResolver::endVisit(ModifierDefinition const&)
{
	solAssert(!m_returnParameters.empty(), "");
	m_returnParameters.pop_back();
}

void ReferencesResolver::endVisit(UserDefinedTypeName const& _typeName)
{
	Declaration const* declaration = m_resolver.pathFromCurrentScope(_typeName.namePath());
	if (!declaration)
	{
		fatalDeclarationError(_typeName.location(), "Identifier not found or not unique.");
		return;
	}

	_typeName.annotation().referencedDeclaration = declaration;

	if (StructDefinition const* structDef = dynamic_cast<StructDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::structType(*structDef);
	else if (EnumDefinition const* enumDef = dynamic_cast<EnumDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::enumType(*enumDef);
	else if (ContractDefinition const* contract = dynamic_cast<ContractDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::contract(*contract);
	else
	{
		_typeName.annotation().type = TypeProvider::emptyTuple();
		typeError(_typeName.location(), "Name has to refer to a struct, enum or contract.");
	}
}

void ReferencesResolver::endVisit(FunctionTypeName const& _typeName)
{
	switch (_typeName.visibility())
	{
	case Visibility::Internal:
	case Visibility::External:
		break;
	default:
		fatalTypeError(_typeName.location(), "Invalid visibility, can only be \"external\" or \"internal\".");
		return;
	}

	if (_typeName.visibility() == Visibility::External)
		for (auto const& t: _typeName.parameterTypes() + _typeName.returnParameterTypes())
		{
			solAssert(t->annotation().type, "Type not set for parameter.");
			if (!t->annotation().type->interfaceType(false).get())
			{
				fatalTypeError(t->location(), "Internal type cannot be used for external function type.");
				return;
			}
		}

	_typeName.annotation().type = TypeProvider::function(_typeName);
}

void ReferencesResolver::endVisit(Mapping const& _typeName)
{
	TypePointer keyType = _typeName.keyType().annotation().type;
	TypePointer valueType = _typeName.valueType().annotation().type;
	// Convert key type to memory.
	keyType = TypeProvider::withLocationIfReference(keyType);
	valueType = TypeProvider::withLocationIfReference(valueType);
	_typeName.annotation().type = TypeProvider::mapping(keyType, valueType);
}

void ReferencesResolver::endVisit(Optional const& _typeName)
{
	std::vector<ASTPointer<TypeName>> const& comp = _typeName.maybeTypes();
	std::vector<Type const*> types;
	for (const ASTPointer<TypeName>& c : comp) {
		types.emplace_back(c->annotation().type);
	}

	if (comp.size() == 1) {
		_typeName.annotation().type = TypeProvider::optional(types.at(0));
	} else {
		_typeName.annotation().type = TypeProvider::optional(TypeProvider::tuple(types));
	}
}

void ReferencesResolver::endVisit(const ElementaryTypeName &_typeName) {
	if (_typeName.typeName().token() == Token::ExtraCurrencyCollection) {
		_typeName.annotation().type = TypeProvider::extraCurrencyCollection();
	} else {
		ASTConstVisitor::endVisit(_typeName);
	}
}

void ReferencesResolver::endVisit(ArrayTypeName const& _typeName)
{
	TypePointer baseType = _typeName.baseType().annotation().type;
	if (!baseType)
	{
		solAssert(!m_errorReporter.errors().empty(), "");
		return;
	}
	if (baseType->storageBytes() == 0)
		fatalTypeError(_typeName.baseType().location(), "Illegal base type of storage size zero for array.");
	if (Expression const* length = _typeName.length())
	{
		TypePointer& lengthTypeGeneric = length->annotation().type;
		if (!lengthTypeGeneric)
			lengthTypeGeneric = ConstantEvaluator(m_errorReporter).evaluate(*length);
		RationalNumberType const* lengthType = dynamic_cast<RationalNumberType const*>(lengthTypeGeneric);
		if (!lengthType || !lengthType->mobileType())
			fatalTypeError(length->location(), "Invalid array length, expected integer literal or constant expression.");
		else if (lengthType->isZero())
			fatalTypeError(length->location(), "Array with zero length specified.");
		else if (lengthType->isFractional())
			fatalTypeError(length->location(), "Array with fractional length specified.");
		else if (lengthType->isNegative())
			fatalTypeError(length->location(), "Array with negative length specified.");
		else
			_typeName.annotation().type = TypeProvider::array(baseType, lengthType->literalValue(nullptr));
	}
	else
		_typeName.annotation().type = TypeProvider::array(baseType);

}

bool ReferencesResolver::visit(InlineAssembly const& /*_inlineAssembly*/)
{
	return false;
}

bool ReferencesResolver::visit(Return const& _return)
{
	solAssert(!m_returnParameters.empty(), "");
	_return.annotation().functionReturnParameters = m_returnParameters.back();
	return true;
}

void ReferencesResolver::endVisit(VariableDeclaration const& _variable)
{
	if (_variable.annotation().type)
		return;

	if (_variable.isConstant() && !_variable.isStateVariable())
		m_errorReporter.declarationError(_variable.location(), "The \"constant\" keyword can only be used for state variables.");

	if (!_variable.typeName())
	{
		// This can still happen in very unusual cases where a developer uses constructs, such as
		// `var a;`, however, such code will have generated errors already.
		// However, we cannot blindingly solAssert() for that here, as the TypeChecker (which is
		// invoking ReferencesResolver) is generating it, so the error is most likely(!) generated
		// after this step.
		return;
	}


	TypePointer type = _variable.typeName()->annotation().type;
	if (auto ref = dynamic_cast<ReferenceType const*>(type))
	{
		bool isPointer = !_variable.isStateVariable();
		type = TypeProvider::withLocation(ref, isPointer);
	}

	_variable.annotation().type = type;
}

void ReferencesResolver::typeError(SourceLocation const& _location, string const& _description)
{
	m_errorOccurred = true;
	m_errorReporter.typeError(_location, _description);
}

void ReferencesResolver::fatalTypeError(SourceLocation const& _location, string const& _description)
{
	m_errorOccurred = true;
	m_errorReporter.fatalTypeError(_location, _description);
}

void ReferencesResolver::declarationError(SourceLocation const& _location, string const& _description)
{
	m_errorOccurred = true;
	m_errorReporter.declarationError(_location, _description);
}

void ReferencesResolver::declarationError(SourceLocation const& _location, SecondarySourceLocation const& _ssl, string const& _description)
{
	m_errorOccurred = true;
	m_errorReporter.declarationError(_location, _ssl, _description);
}

void ReferencesResolver::fatalDeclarationError(SourceLocation const& _location, string const& _description)
{
	m_errorOccurred = true;
	m_errorReporter.fatalDeclarationError(_location, _description);
}


}
