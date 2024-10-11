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

#pragma once

#include <libsolidity/ast/Types.h>

#include <array>
#include <map>
#include <memory>
#include <optional>
#include <utility>

namespace solidity::frontend
{

/**
 * API for accessing the Solidity Type System.
 *
 * This is the Solidity Compiler's type provider. Use it to request for types. The caller does
 * <b>not</b> own the types.
 *
 * It is not recommended to explicitly instantiate types unless you really know what and why
 * you are doing it.
 */
class TypeProvider
{
public:
	TypeProvider() = default;
	TypeProvider(TypeProvider&&) = default;
	TypeProvider(TypeProvider const&) = delete;
	TypeProvider& operator=(TypeProvider&&) = default;
	TypeProvider& operator=(TypeProvider const&) = delete;
	~TypeProvider() = default;

	/// Resets state of this TypeProvider to initial state, wiping all mutable types.
	/// This invalidates all dangling pointers to types provided by this TypeProvider.
	static void reset();

	/// @name Factory functions
	/// Factory functions that convert an AST @ref TypeName to a Type.
	static Type const* fromElementaryTypeName(ElementaryTypeNameToken const& _type);

	/// Converts a given elementary type name with optional data location
	/// suffix " storage", " calldata" or " memory" to a type pointer. If suffix not given, defaults to " storage".
	static Type const* fromElementaryTypeName(std::string const& _name);

	/// @returns boolean type.
	static BoolType const* boolean() noexcept { return &m_boolean; }
	static NullType const* nullType() noexcept { return &m_nullType; }
	static EmptyMapType const* emptyMapType() noexcept { return &m_emptyMapType; }
	static TvmCellType const* tvmcell() noexcept { return &m_tvmcell; }
	static TvmSliceType const* tvmslice() noexcept { return &m_tvmslice; }
	static TvmBuilderType const* tvmbuilder() noexcept { return &m_tvmbuilder; }
	static StringBuilderType const* stringBuilder() noexcept { return &m_stringBuilder; }
	static FixedBytesType const* byte() { return fixedBytes(1); }
	static FixedBytesType const* fixedBytes(unsigned m) { return m_bytesM.at(m - 1).get(); }
	static ArrayType const* bytesStorage();
	static ArrayType const* bytesMemory();
	static ArrayType const* bytesCalldata();
	static ArrayType const* stringStorage();
	static Variant const* variant();
	static ArrayType const* stringMemory();

	/// Constructor for a byte array ("bytes") and string.
	static ArrayType const* array(bool _isString = false);

	/// Constructor for a dynamically sized array type ("type[]")
	static ArrayType const* array(Type const* _baseType);

	/// Constructor for a fixed-size array type ("type[20]")
	static ArrayType const* array(Type const* _baseType, u256 const& _length);

	static ArraySliceType const* arraySlice(ArrayType const& _arrayType);

	static AddressType const* address() noexcept { return &m_address; }
	static InitializerListType const* initializerList() noexcept { return &m_initializerList; }
	static CallListType const* callList() noexcept { return &m_callList; }

	static IntegerType const* integer(unsigned _bits, IntegerType::Modifier _modifier)
	{
		if (_modifier == IntegerType::Modifier::Unsigned)
			return m_uintM.at(_bits - 1).get();
		else
			return m_intM.at(_bits - 1).get();
	}

	static NanType const* qIntegerNAN()
	{
		return m_qintNAN.get();
	}

	static QIntegerType const* qInteger(unsigned _bits, IntegerType::Modifier _modifier)
	{
		if (_modifier == IntegerType::Modifier::Unsigned)
			return m_quintM.at(_bits - 1).get();
		else
			return m_qintM.at(_bits - 1).get();
	}

	static QBoolType const* qBool()
	{
		return m_qbool.get();
	}

	static IntegerType const* uint(unsigned _bits) { return integer(_bits, IntegerType::Modifier::Unsigned); }
	static IntegerType const* int_(unsigned _bits) { return integer(_bits, IntegerType::Modifier::Signed); }

	static IntegerType const* uint256() { return uint(256); }
	static IntegerType const* uint64() { return uint(64); }
	static IntegerType const* uint32() { return uint(32); }
	static IntegerType const* uint128() { return uint(128); }
	static IntegerType const* int257() { return integer(257, IntegerType::Modifier::Signed); }

	static VarIntegerType const* coins();
	static VarIntegerType const* varinteger(unsigned m, IntegerType::Modifier _modifier);

	static FixedPointType const* fixedPoint(unsigned m, unsigned n, FixedPointType::Modifier _modifier);

	static StringLiteralType const* stringLiteral(std::string const& literal);

	/// @param members the member types the tuple type must contain. This is passed by value on purspose.
	/// @returns a tuple type with the given members.
	static TupleType const* tuple(std::vector<Type const*> members);

	static TupleType const* emptyTuple() noexcept { return &m_emptyTuple; }

	/// @returns the internally-facing or externally-facing type of a function or the type of a function declaration.
	static FunctionType const* function(FunctionDefinition const& _function, FunctionType::Kind _kind = FunctionType::Kind::Declaration);

	/// @returns the accessor function type of a state variable.
	static FunctionType const* function(VariableDeclaration const& _varDecl);

	/// @returns the function type of an event.
	static FunctionType const* function(EventDefinition const& _event);

	static FunctionType const* function(ErrorDefinition const& _error);

	/// @returns the type of a function type name.
	static FunctionType const* function(FunctionTypeName const& _typeName);

	/// @returns the function type to be used for a plain type (not derived from a declaration).
	static FunctionType const* function(
		strings const& _parameterTypes,
		strings const& _returnParameterTypes,
		FunctionType::Kind _kind = FunctionType::Kind::Internal,
		StateMutability _stateMutability = StateMutability::NonPayable,
		FunctionType::Options _options = {}
	);

	/// @returns a highly customized FunctionType, use with care.
	static FunctionType const* function(TypePointers const& _parameterTypes,
		TypePointers const& _returnParameterTypes,
		strings _parameterNames = strings{},
		strings _returnParameterNames = strings{},
		FunctionType::Kind _kind = FunctionType::Kind::Internal,
		StateMutability _stateMutability = StateMutability::NonPayable,
		Declaration const* _declaration = nullptr,
		FunctionType::Options _options = {}
	);

	/// Auto-detect the proper type for a literal. @returns an empty pointer if the literal does
	/// not fit any type.
	static Type const* forLiteral(Literal const& _literal);
	static RationalNumberType const* rationalNumber(Literal const& _literal);

	static RationalNumberType const* rationalNumber(
		rational const& _value,
		Type const* _compatibleBytesType = nullptr
	);

	static ContractType const* contract(ContractDefinition const& _contract, bool _isSuper = false);

	static InaccessibleDynamicType const* inaccessibleDynamic() noexcept { return &m_inaccessibleDynamic; }

	/// @returns the type of an enum instance for given definition, there is one distinct type per enum definition.
	static EnumType const* enumType(EnumDefinition const& _enum);

	/// @returns special type for imported modules. These mainly give access to their scope via members.
	static ModuleType const* module(SourceUnit const& _source);

	static TypeType const* typeType(Type const* _actualType);

	static StructType const* structType(StructDefinition const& _struct);

	static ModifierType const* modifier(ModifierDefinition const& _modifierDef);

	static MagicType const* magic(MagicType::Kind _kind);

	static MagicType const* meta(Type const* _type);

	static MappingType const* mapping(Type const* _keyType, ASTString _keyName, Type const* _valueType, ASTString _valueName);

	static MappingType const* extraCurrencyCollection();

	static OptionalType const* optional(Type const* _type);

	static TvmVectorType const* tvmVector(Type const* _type);

	static TvmStackType const* tvmStack(Type const* _type);

	static UserDefinedValueType const* userDefinedValueType(UserDefinedValueTypeDefinition const& _definition);

private:
	/// Global TypeProvider instance.
	static TypeProvider& instance()
	{
		static TypeProvider _provider;
		return _provider;
	}

	template <typename T, typename... Args>
	static inline T const* createAndGet(Args&& ... _args);

	static BoolType const m_boolean;
	static NullType const m_nullType;
	static EmptyMapType const m_emptyMapType;
	static TvmCellType const m_tvmcell;
	static TvmSliceType const m_tvmslice;
	static TvmBuilderType const m_tvmbuilder;
	static StringBuilderType const m_stringBuilder;

	static InaccessibleDynamicType const m_inaccessibleDynamic;

	/// These are lazy-initialized because they depend on `byte` being available.
	static std::unique_ptr<ArrayType> m_bytesStorage;
	static std::unique_ptr<ArrayType> m_bytesMemory;
	static std::unique_ptr<ArrayType> m_bytesCalldata;
	static std::unique_ptr<ArrayType> m_stringStorage;
    static std::unique_ptr<Variant> m_variant;
	static std::unique_ptr<ArrayType> m_stringMemory;

	static TupleType const m_emptyTuple;
	static AddressType const m_address;
	static InitializerListType const m_initializerList;
	static CallListType const m_callList;
	static std::array<std::unique_ptr<IntegerType>, 257> const m_intM;
	static std::array<std::unique_ptr<IntegerType>, 256> const m_uintM;
	static std::unique_ptr<NanType> const m_qintNAN;
	static std::array<std::unique_ptr<QIntegerType>, 257> const m_qintM;
	static std::array<std::unique_ptr<QIntegerType>, 256> const m_quintM;
	static std::unique_ptr<QBoolType> const m_qbool;
	static std::array<std::unique_ptr<FixedBytesType>, 32> const m_bytesM;
	static std::array<std::unique_ptr<MagicType>, 9> const m_magics;        ///< MagicType's except MetaType

	std::map<std::pair<unsigned, IntegerType::Modifier>, std::unique_ptr<VarIntegerType>> m_varinterger{};
	std::map<std::pair<unsigned, unsigned>, std::unique_ptr<FixedPointType>> m_ufixedMxN{};
	std::map<std::pair<unsigned, unsigned>, std::unique_ptr<FixedPointType>> m_fixedMxN{};
	std::map<std::string, std::unique_ptr<StringLiteralType>> m_stringLiteralTypes{};
	std::vector<std::unique_ptr<Type>> m_generalTypes{};
};

}
