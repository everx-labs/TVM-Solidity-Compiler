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

#include <libsolidity/analysis/DeclarationTypeChecker.h>

#include <libsolidity/analysis/ConstantEvaluator.h>

#include <libsolidity/ast/TypeProvider.h>

#include <liblangutil/ErrorReporter.h>

#include <libsolutil/Algorithms.h>
#include <libsolutil/Visitor.h>

#include <range/v3/view/transform.hpp>

using namespace solidity::langutil;
using namespace solidity::frontend;

bool DeclarationTypeChecker::visit(ElementaryTypeName const& _typeName)
{
	if (_typeName.annotation().type)
		return false;

	_typeName.annotation().type = TypeProvider::fromElementaryTypeName(_typeName.typeName());
	return true;
}

bool DeclarationTypeChecker::visit(EnumDefinition const& _enum)
{
	if (_enum.members().size() > 256)
		m_errorReporter.declarationError(
			1611_error,
			_enum.location(),
			"Enum with more than 256 members is not allowed."
		);

	return false;
}

bool DeclarationTypeChecker::visit(StructDefinition const& _struct)
{
	if (_struct.annotation().recursive.has_value())
	{
		if (!m_currentStructsSeen.empty() && *_struct.annotation().recursive)
			m_recursiveStructSeen = true;
		return false;
	}

	if (m_currentStructsSeen.count(&_struct))
	{
		_struct.annotation().recursive = true;
		m_recursiveStructSeen = true;
		return false;
	}

	bool previousRecursiveStructSeen = m_recursiveStructSeen;
	bool hasRecursiveChild = false;

	m_currentStructsSeen.insert(&_struct);

	for (auto const& member: _struct.members())
	{
		m_recursiveStructSeen = false;
		member->accept(*this);
		solAssert(member->annotation().type, "");
		if (m_recursiveStructSeen)
			hasRecursiveChild = true;
	}

	if (!_struct.annotation().recursive.has_value())
		_struct.annotation().recursive = hasRecursiveChild;
	m_recursiveStructSeen = previousRecursiveStructSeen || *_struct.annotation().recursive;
	m_currentStructsSeen.erase(&_struct);
	if (m_currentStructsSeen.empty())
		m_recursiveStructSeen = false;

	// Check direct recursion, fatal error if detected.
	auto visitor = [&](StructDefinition const& _struct, auto& _cycleDetector, size_t _depth)
	{
		if (_depth >= 256)
			m_errorReporter.fatalDeclarationError(
				5651_error,
				_struct.location(),
				"Struct definition exhausts cyclic dependency validator."
			);

		for (ASTPointer<VariableDeclaration> const& member: _struct.members())
		{
			Type const* memberType = member->annotation().type;

			if (auto arrayType = dynamic_cast<ArrayType const*>(memberType))
				memberType = arrayType->finalBaseType(true);

			if (auto structType = dynamic_cast<StructType const*>(memberType))
				if (_cycleDetector.run(structType->structDefinition()))
					return;
		}
	};
	if (util::CycleDetector<StructDefinition>(visitor).run(_struct))
		m_errorReporter.fatalTypeError(2046_error, _struct.location(), "Recursive struct definition.");

	return false;
}

void DeclarationTypeChecker::endVisit(UserDefinedValueTypeDefinition const& _userDefined)
{
	TypeName const* typeName = _userDefined.underlyingType();
	solAssert(typeName, "");
	if (!dynamic_cast<ElementaryTypeName const*>(typeName))
		m_errorReporter.fatalTypeError(
			8657_error,
			typeName->location(),
			"The underlying type for a user defined value type has to be an elementary value type."
		);

	Type const* type = typeName->annotation().type;
	solAssert(type, "");
	solAssert(!dynamic_cast<UserDefinedValueType const*>(type), "");
	if (!type->isValueType())
		m_errorReporter.typeError(
			8129_error,
			_userDefined.location(),
			"The underlying type of the user defined value type \"" +
			_userDefined.name() +
			"\" is not a value type."
		);
}

void DeclarationTypeChecker::endVisit(UserDefinedTypeName const& _typeName)
{
	if (_typeName.annotation().type)
		return;

	Declaration const* declaration = _typeName.pathNode().annotation().referencedDeclaration;
	solAssert(declaration, "");

	if (StructDefinition const* structDef = dynamic_cast<StructDefinition const*>(declaration))
	{
		if (!m_insideFunctionType && !m_currentStructsSeen.empty())
			structDef->accept(*this);
		_typeName.annotation().type = TypeProvider::structType(*structDef);
	}
	else if (EnumDefinition const* enumDef = dynamic_cast<EnumDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::enumType(*enumDef);
	else if (ContractDefinition const* contract = dynamic_cast<ContractDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::contract(*contract);
	else if (auto userDefinedValueType = dynamic_cast<UserDefinedValueTypeDefinition const*>(declaration))
		_typeName.annotation().type = TypeProvider::userDefinedValueType(*userDefinedValueType);
	else
	{
		_typeName.annotation().type = TypeProvider::emptyTuple();
		m_errorReporter.fatalTypeError(
			5172_error,
			_typeName.location(),
			"Name has to refer to a user-defined type."
		);
	}
}

void DeclarationTypeChecker::endVisit(IdentifierPath const& _path)
{
	Declaration const* declaration = _path.annotation().referencedDeclaration;
	solAssert(declaration, "");

	if (ContractDefinition const* contract = dynamic_cast<ContractDefinition const*>(declaration))
		if (contract->isLibrary())
			m_errorReporter.typeError(1130_error, _path.location(), "Invalid use of a library name.");
}

bool DeclarationTypeChecker::visit(FunctionTypeName const& _typeName)
{
	if (_typeName.annotation().type)
		return false;

	bool previousInsideFunctionType = m_insideFunctionType;
	m_insideFunctionType = true;
	_typeName.parameterTypeList()->accept(*this);
	_typeName.returnParameterTypeList()->accept(*this);
	m_insideFunctionType = previousInsideFunctionType;

	switch (_typeName.visibility())
	{
		case Visibility::Internal:
		case Visibility::External:
			break;
		default:
			m_errorReporter.fatalTypeError(
				6012_error,
				_typeName.location(),
				"Invalid visibility, can only be \"external\" or \"internal\"."
			);
			return false;
	}

	_typeName.annotation().type = TypeProvider::function(_typeName);
	return false;
}

void DeclarationTypeChecker::endVisit(Mapping const& _mapping)
{
	if (_mapping.annotation().type)
		return;

	// key type is checked in TVMTypeChecker::visit(const Mapping &_mapping)

	Type const* keyType = _mapping.keyType().annotation().type;
	ASTString keyName = _mapping.keyName();

	Type const* valueType = _mapping.valueType().annotation().type;
	ASTString valueName = _mapping.valueName();

	_mapping.annotation().type = TypeProvider::mapping(keyType, keyName, valueType, valueName);

	// Check if parameter names are conflicting.
	if (!keyName.empty())
	{
		auto childMappingType = dynamic_cast<MappingType const*>(valueType);
		ASTString currentValueName = valueName;
		bool loop = true;
		while (loop)
		{
			bool isError = false;
			// Value type is a mapping.
			if (childMappingType)
			{
				// Compare top mapping's key name with child mapping's key name.
				ASTString childKeyName = childMappingType->keyName();
				if (keyName == childKeyName)
					isError = true;

				auto valueType = childMappingType->valueType();
				currentValueName = childMappingType->valueName();
				childMappingType = dynamic_cast<MappingType const*>(valueType);
			}
			else
			{
				// Compare top mapping's key name with the value name.
				if (keyName == currentValueName)
					isError = true;

				loop = false; // We arrived at the end of mapping recursion.
			}

			// Report error.
			if (isError)
			{
				m_errorReporter.declarationError(
					1809_error,
					_mapping.location(),
					"Conflicting parameter name \"" + keyName + "\" in mapping."
				);
			}
		}
	}
}

void DeclarationTypeChecker::endVisit(Optional const& _optional)
{
	if (_optional.annotation().type)
		return;

	std::vector<ASTPointer<TypeName>> const& comp = _optional.maybeTypes();
	std::vector<Type const*> types;
	for (const ASTPointer<TypeName>& c : comp) {
		types.emplace_back(c->annotation().type);
	}

	if (comp.size() == 1) {
		_optional.annotation().type = TypeProvider::optional(types.at(0));
	} else {
		_optional.annotation().type = TypeProvider::optional(TypeProvider::tuple(types));
	}
}

void DeclarationTypeChecker::endVisit(TvmVector const& _tvmVector)
{
	if (_tvmVector.annotation().type)
		return;

	TypeName const& type = _tvmVector.type();
	_tvmVector.annotation().type = TypeProvider::tvmVector(type.annotation().type);
}

void DeclarationTypeChecker::endVisit(TvmStack const& _tvmStack)
{
	if (_tvmStack.annotation().type)
		return;

	TypeName const& type = _tvmStack.type();
	_tvmStack.annotation().type = TypeProvider::tvmStack(type.annotation().type);
}

void DeclarationTypeChecker::endVisit(ArrayTypeName const& _typeName)
{
	if (_typeName.annotation().type)
		return;

	Type const* baseType = _typeName.baseType().annotation().type;
	if (!baseType)
	{
		solAssert(!m_errorReporter.errors().empty(), "");
		return;
	}

	if (Expression const* length = _typeName.length())
	{
		std::optional<rational> lengthValue;
		if (length->annotation().type && length->annotation().type->category() == Type::Category::RationalNumber)
			lengthValue = dynamic_cast<RationalNumberType const&>(*length->annotation().type).value();
		else if (std::optional<ConstantEvaluator::TypedRational> value = ConstantEvaluator::evaluate(m_errorReporter, *length))
			lengthValue = value->value;

		if (!lengthValue)
			m_errorReporter.typeError(
				5462_error,
				length->location(),
				"Invalid array length, expected integer literal or constant expression."
			);
		else if (*lengthValue == 0)
			m_errorReporter.typeError(1406_error, length->location(), "Array with zero length specified.");
		else if (lengthValue->denominator() != 1)
			m_errorReporter.typeError(3208_error, length->location(), "Array with fractional length specified.");
		else if (*lengthValue < 0)
			m_errorReporter.typeError(3658_error, length->location(), "Array with negative length specified.");
		else if (lengthValue > TypeProvider::uint256()->max())
			m_errorReporter.typeError(
				1847_error,
				length->location(),
				"Array length too large, maximum is 2**256 - 1."
			);

		_typeName.annotation().type = TypeProvider::array(
			baseType,
			lengthValue ? u256(lengthValue->numerator()) : u256(0)
		);
	}
	else
		_typeName.annotation().type = TypeProvider::array(baseType);
}

void DeclarationTypeChecker::endVisit(VariableDeclaration const& _variable)
{
	if (_variable.annotation().type)
		return;

	if (_variable.isFileLevelVariable() && !_variable.isConstant())
		m_errorReporter.declarationError(
			8342_error,
			_variable.location(),
			"Only constant variables are allowed at file level."
		);
	if (_variable.isConstant() && (!_variable.isStateVariable() && !_variable.isFileLevelVariable()))
		m_errorReporter.declarationError(
			1788_error,
			_variable.location(),
			"The \"constant\" keyword can only be used for state variables or variables at file level."
		);
	if (_variable.immutable() && !_variable.isStateVariable())
		m_errorReporter.declarationError(
			8297_error,
			_variable.location(),
			"The \"immutable\" keyword can only be used for state variables."
		);

	Type const* type = _variable.typeName().annotation().type;
	_variable.annotation().type = type;
}

bool DeclarationTypeChecker::visit(UsingForDirective const& _usingFor)
{
	if (_usingFor.usesBraces())
	{
		for (ASTPointer<IdentifierPath> const& function: _usingFor.functionsOrLibrary())
			if (auto functionDefinition = dynamic_cast<FunctionDefinition const*>(function->annotation().referencedDeclaration))
			{
				if (functionDefinition->isInlineAssembly())
					m_errorReporter.typeError(
						1167_error,
						function->location(),
						"Only file-level functions (not assembly) and library functions can be attached to a type in a \"using\" statement."
					);
				if (!functionDefinition->isFree() && !(
					dynamic_cast<ContractDefinition const*>(functionDefinition->scope()) &&
					dynamic_cast<ContractDefinition const*>(functionDefinition->scope())->isLibrary()
				))
					m_errorReporter.typeError(
						4167_error,
						function->location(),
						"Only file-level functions and library functions can be attached to a type in a \"using\" statement"
					);
			}
			else
				m_errorReporter.fatalTypeError(8187_error, function->location(), "Expected function name." );
	}
	else
	{
		ContractDefinition const* library = dynamic_cast<ContractDefinition const*>(
			_usingFor.functionsOrLibrary().front()->annotation().referencedDeclaration
		);
		if (!library || !library->isLibrary())
			m_errorReporter.fatalTypeError(
				4357_error,
				_usingFor.functionsOrLibrary().front()->location(),
				"Library name expected. If you want to attach a function, use '{...}'."
			);
	}

	// We do not visit _usingFor.functions() because it will lead to an error since
	// library names cannot be mentioned stand-alone.

	if (_usingFor.typeName())
		_usingFor.typeName()->accept(*this);

	return false;
}

bool DeclarationTypeChecker::visit(InheritanceSpecifier const& _inheritanceSpecifier)
{
	auto const* contract = dynamic_cast<ContractDefinition const*>(_inheritanceSpecifier.name().annotation().referencedDeclaration);
	solAssert(contract, "");
	if (contract->isLibrary())
	{
		m_errorReporter.typeError(
			2571_error,
			_inheritanceSpecifier.name().location(),
			"Libraries cannot be inherited from."
		);
		return false;
	}
	return true;
}

bool DeclarationTypeChecker::check(ASTNode const& _node)
{
	auto watcher = m_errorReporter.errorWatcher();
	_node.accept(*this);
	return watcher.ok();
}
