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

#include <libsolidity/codegen/TVMConstants.hpp>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace solidity;
using namespace solidity::frontend;
using namespace solidity::util;

BoolType const TypeProvider::m_boolean{};
NullType const TypeProvider::m_nullType{};
EmptyMapType const TypeProvider::m_emptyMapType{};
TvmCellType const TypeProvider::m_tvmcell{};
TvmSliceType const TypeProvider::m_tvmslice{};
TvmBuilderType const TypeProvider::m_tvmbuilder{};
StringBuilderType const TypeProvider::m_stringBuilder{};

InaccessibleDynamicType const TypeProvider::m_inaccessibleDynamic{};

/// The string and bytes unique_ptrs are initialized when they are first used because
/// they rely on `byte` being available which we cannot guarantee in the static init context.
std::unique_ptr<ArrayType> TypeProvider::m_bytesStorage;
std::unique_ptr<ArrayType> TypeProvider::m_bytesMemory;
std::unique_ptr<ArrayType> TypeProvider::m_bytesCalldata;
std::unique_ptr<ArrayType> TypeProvider::m_stringStorage;
std::unique_ptr<ArrayType> TypeProvider::m_stringMemory;
std::unique_ptr<Variant> TypeProvider::m_variant;

TupleType const TypeProvider::m_emptyTuple{};
AddressType const TypeProvider::m_address{};
AddressStdType const TypeProvider::m_addressStd{};
InitializerListType const TypeProvider::m_initializerList{};
CallListType const TypeProvider::m_callList{};

std::array<std::unique_ptr<IntegerType>, 257> const TypeProvider::m_intM = []{
	std::array<std::unique_ptr<IntegerType>, 257> arr;
	for (int i = 0; i < 257; ++i) {
		arr[i] = std::make_unique<IntegerType>(i + 1, IntegerType::Modifier::Signed);
	}
	return arr;
}();

std::array<std::unique_ptr<IntegerType>, 256> const TypeProvider::m_uintM = [] {
	std::array<std::unique_ptr<IntegerType>, 256> arr{};
	for (int i = 0; i < 256; ++i) {
		arr[i] = std::make_unique<IntegerType>(i + 1, IntegerType::Modifier::Unsigned);
	}
	return arr;
}();

std::unique_ptr<NanType> const TypeProvider::m_qintNAN = std::make_unique<NanType>();

std::array<std::unique_ptr<QIntegerType>, 257> const TypeProvider::m_qintM = [] {
	std::array<std::unique_ptr<QIntegerType>, 257> arr{};
	for (int i = 0; i < 257; ++i) {
		arr[i] = std::make_unique<QIntegerType>(i + 1, IntegerType::Modifier::Signed);
	}
	return arr;
}();

std::array<std::unique_ptr<QIntegerType>, 256> const TypeProvider::m_quintM = [] {
	std::array<std::unique_ptr<QIntegerType>, 256> arr{};
	for (int i = 0; i < 256; ++i) {
		arr[i] = std::make_unique<QIntegerType>(i + 1, IntegerType::Modifier::Unsigned);
	}
	return arr;
}();

std::unique_ptr<QBoolType> const TypeProvider::m_qbool = std::make_unique<QBoolType>();

std::array<std::unique_ptr<FixedBytesType>, 32> const TypeProvider::m_bytesM{{
	{std::make_unique<FixedBytesType>(1)},
	{std::make_unique<FixedBytesType>(2)},
	{std::make_unique<FixedBytesType>(3)},
	{std::make_unique<FixedBytesType>(4)},
	{std::make_unique<FixedBytesType>(5)},
	{std::make_unique<FixedBytesType>(6)},
	{std::make_unique<FixedBytesType>(7)},
	{std::make_unique<FixedBytesType>(8)},
	{std::make_unique<FixedBytesType>(9)},
	{std::make_unique<FixedBytesType>(10)},
	{std::make_unique<FixedBytesType>(11)},
	{std::make_unique<FixedBytesType>(12)},
	{std::make_unique<FixedBytesType>(13)},
	{std::make_unique<FixedBytesType>(14)},
	{std::make_unique<FixedBytesType>(15)},
	{std::make_unique<FixedBytesType>(16)},
	{std::make_unique<FixedBytesType>(17)},
	{std::make_unique<FixedBytesType>(18)},
	{std::make_unique<FixedBytesType>(19)},
	{std::make_unique<FixedBytesType>(20)},
	{std::make_unique<FixedBytesType>(21)},
	{std::make_unique<FixedBytesType>(22)},
	{std::make_unique<FixedBytesType>(23)},
	{std::make_unique<FixedBytesType>(24)},
	{std::make_unique<FixedBytesType>(25)},
	{std::make_unique<FixedBytesType>(26)},
	{std::make_unique<FixedBytesType>(27)},
	{std::make_unique<FixedBytesType>(28)},
	{std::make_unique<FixedBytesType>(29)},
	{std::make_unique<FixedBytesType>(30)},
	{std::make_unique<FixedBytesType>(31)},
	{std::make_unique<FixedBytesType>(32)}
}};

std::array<std::unique_ptr<MagicType>, 11> const TypeProvider::m_magics{{
	{std::make_unique<MagicType>(MagicType::Kind::Block)},
	{std::make_unique<MagicType>(MagicType::Kind::Message)},
	{std::make_unique<MagicType>(MagicType::Kind::Transaction)},
	{std::make_unique<MagicType>(MagicType::Kind::ABI)},
	{std::make_unique<MagicType>(MagicType::Kind::TVM)},
	{std::make_unique<MagicType>(MagicType::Kind::Math)},
	{std::make_unique<MagicType>(MagicType::Kind::Rnd)},
	{std::make_unique<MagicType>(MagicType::Kind::Gosh)},
	{std::make_unique<MagicType>(MagicType::Kind::BLS)},
	{std::make_unique<MagicType>(MagicType::Kind::RIST255)},
	{std::make_unique<MagicType>(MagicType::Kind::Error)}
	// MetaType is stored separately
}};

inline void clearCache(Type const& type)
{
	type.clearCache();
}

template <typename T>
inline void clearCache(std::unique_ptr<T> const& type)
{
	// Some lazy-initialized types might not exist yet.
	if (type)
		type->clearCache();
}

template <typename Container>
inline void clearCaches(Container& container)
{
	for (auto const& e: container)
		clearCache(e);
}

void TypeProvider::reset()
{
	clearCache(m_boolean);
	clearCache(m_inaccessibleDynamic);
	clearCache(m_bytesStorage);
	clearCache(m_bytesMemory);
	clearCache(m_bytesCalldata);
	clearCache(m_stringStorage);
	clearCache(m_variant);
	clearCache(m_stringMemory);
	clearCache(m_emptyTuple);
	clearCache(m_address);
	clearCaches(instance().m_intM);
	clearCaches(instance().m_uintM);
	clearCaches(instance().m_bytesM);
	clearCaches(instance().m_magics);

	instance().m_generalTypes.clear();
	instance().m_stringLiteralTypes.clear();
	instance().m_ufixedMxN.clear();
	instance().m_fixedMxN.clear();
	instance().m_varinteger.clear();
}

template <typename T, typename... Args>
inline T const* TypeProvider::createAndGet(Args&& ... _args)
{
	instance().m_generalTypes.emplace_back(std::make_unique<T>(std::forward<Args>(_args)...));
	return static_cast<T const*>(instance().m_generalTypes.back().get());
}

Type const* TypeProvider::fromElementaryTypeName(ElementaryTypeNameToken const& _type)
{
	solAssert(
		TokenTraits::isElementaryTypeName(_type.token()),
		"Expected an elementary type name but got " + _type.toString()
	);

	unsigned const m = _type.firstNumber();
	unsigned const n = _type.secondNumber();

	switch (_type.token())
	{
	case Token::VarUint:
	case Token::Varuint:
		return varinteger(32, IntegerType::Modifier::Unsigned);
	case Token::VarUintM:
	case Token::VaruintM:
		return varinteger(m, IntegerType::Modifier::Unsigned);
	case Token::coins:
		return coins();
	case Token::VarInt:
	case Token::Varint:
		return varinteger(32, IntegerType::Modifier::Signed);
	case Token::VarIntM:
	case Token::VarintM:
		return varinteger(m, IntegerType::Modifier::Signed);
	case Token::IntM:
		return integer(m, IntegerType::Modifier::Signed);
	case Token::UIntM:
		return integer(m, IntegerType::Modifier::Unsigned);
	case Token::QIntM:
		return qInteger(m, IntegerType::Modifier::Signed);
	case Token::QUIntM:
		return qInteger(m, IntegerType::Modifier::Unsigned);
	case Token::Byte:
		return byte();
	case Token::BytesM:
		return fixedBytes(m);
	case Token::FixedMxN:
		return fixedPoint(m, n, FixedPointType::Modifier::Signed);
	case Token::UFixedMxN:
		return fixedPoint(m, n, FixedPointType::Modifier::Unsigned);
	case Token::Int:
		return integer(257, IntegerType::Modifier::Signed);
	case Token::UInt:
		return integer(256, IntegerType::Modifier::Unsigned);
	case Token::QInt:
		return qInteger(257, IntegerType::Modifier::Signed);
	case Token::QUInt:
		return qInteger(256, IntegerType::Modifier::Unsigned);
	case Token::QBool:
		return qBool();
	case Token::Fixed:
		return fixedPoint(128, 18, FixedPointType::Modifier::Signed);
	case Token::UFixed:
		return fixedPoint(128, 18, FixedPointType::Modifier::Unsigned);
	case Token::Address:
		return address();
	case Token::AddressStd:
		return addressStd();
	case Token::Bool:
		return boolean();
	case Token::TvmCell:
		return tvmcell();
	case Token::TvmSlice:
		return tvmslice();
	case Token::TvmBuilder:
		return tvmbuilder();
	case Token::StringBuilder:
		return stringBuilder();
	case Token::Bytes:
		return bytesStorage();
	case Token::String:
		return stringStorage();
    case Token::Variant:
        return variant();
	default:
		solAssert(
			false,
			"Unable to convert elementary typename " + _type.toString() + " to type."
		);
	}
}

Type const* TypeProvider::fromElementaryTypeName(std::string const& _name)
{
	std::vector<std::string> nameParts;
	boost::split(nameParts, _name, boost::is_any_of(" "));
	solAssert(nameParts.size() == 1 || nameParts.size() == 2, "Cannot parse elementary type: " + _name);

	Token token;
	unsigned short firstNum, secondNum;
	std::tie(token, firstNum, secondNum) = TokenTraits::fromIdentifierOrKeyword(nameParts[0]);

	auto t = fromElementaryTypeName(ElementaryTypeNameToken(token, firstNum, secondNum));
	if (auto* ref = dynamic_cast<CompositeType const*>(t))
	{
		return ref;
	}
	else if (t->category() == Type::Category::Address)
	{
		if (nameParts.size() == 2)
		{
			if (nameParts[1] == "payable")
				return address();
			else
				solAssert(false, "Invalid state mutability for address type: " + nameParts[1]);
		}
		return address();
	}
	else
	{
		solAssert(nameParts.size() == 1, "Storage location suffix only allowed for reference types");
		return t;
	}
}

ArrayType const* TypeProvider::bytesStorage()
{
	if (!m_bytesStorage)
		m_bytesStorage = std::make_unique<ArrayType>(false);
	return m_bytesStorage.get();
}

ArrayType const* TypeProvider::bytesMemory()
{
	if (!m_bytesMemory)
		m_bytesMemory = std::make_unique<ArrayType>(false);
	return m_bytesMemory.get();
}

ArrayType const* TypeProvider::bytesCalldata()
{
	if (!m_bytesCalldata)
		m_bytesCalldata = std::make_unique<ArrayType>(false);
	return m_bytesCalldata.get();
}

ArrayType const* TypeProvider::stringStorage()
{
	if (!m_stringStorage)
		m_stringStorage = std::make_unique<ArrayType>(true);
	return m_stringStorage.get();
}

Variant const* TypeProvider::variant()
{
    if (!m_variant)
        m_variant = std::make_unique<Variant>();
    return m_variant.get();
}

ArrayType const* TypeProvider::stringMemory()
{
	if (!m_stringMemory)
		m_stringMemory = std::make_unique<ArrayType>(true);
	return m_stringMemory.get();
}

Type const* TypeProvider::forLiteral(Literal const& _literal)
{
	switch (_literal.token())
	{
	case Token::TrueLiteral:
	case Token::FalseLiteral:
		return boolean();
	case Token::NullLiteral:
		return nullType();
	case Token::EmptyMap:
		return emptyMapType();
	case Token::TVMNaN:
		return qIntegerNAN();
	case Token::Number:
		return rationalNumber(_literal);
	case Token::StringLiteral:
	case Token::UnicodeStringLiteral:
	case Token::HexStringLiteral:
		return stringLiteral(_literal.value());
	default:
		return nullptr;
	}
}

RationalNumberType const* TypeProvider::rationalNumber(Literal const& _literal)
{
	solAssert(_literal.token() == Token::Number, "");
	std::tuple<bool, rational> validLiteral = RationalNumberType::isValidLiteral(_literal);
	if (std::get<0>(validLiteral))
	{
		Type const* compatibleBytesType = nullptr;
		if (_literal.isHexNumber())
		{
			size_t const digitCount = _literal.valueWithoutUnderscores().length() - 2;
			if (digitCount % 2 == 0 && (digitCount / 2) <= 32)
				compatibleBytesType = fixedBytes(static_cast<unsigned>(digitCount / 2));
		}

		return rationalNumber(std::get<1>(validLiteral), compatibleBytesType);
	}
	return nullptr;
}

StringLiteralType const* TypeProvider::stringLiteral(std::string const& literal)
{
	auto i = instance().m_stringLiteralTypes.find(literal);
	if (i != instance().m_stringLiteralTypes.end())
		return i->second.get();
	else
		return instance().m_stringLiteralTypes.emplace(literal, std::make_unique<StringLiteralType>(literal)).first->second.get();
}

VarIntegerType const* TypeProvider::varinteger(unsigned m, IntegerType::Modifier _modifier) {
	auto& map = instance().m_varinteger;
	auto i = map.find(std::make_pair(m, _modifier));
	if (i != map.end())
		return i->second.get();

	return map.emplace(
			std::make_pair(m, _modifier),
			std::make_unique<VarIntegerType>(m, _modifier)
	).first->second.get();
}

VarIntegerType const* TypeProvider::coins() {
	return TypeProvider::varinteger(16, IntegerType::Modifier::Unsigned);
}

FixedPointType const* TypeProvider::fixedPoint(unsigned m, unsigned n, FixedPointType::Modifier _modifier)
{
	auto& map = _modifier == FixedPointType::Modifier::Unsigned ? instance().m_ufixedMxN : instance().m_fixedMxN;

	auto i = map.find(std::make_pair(m, n));
	if (i != map.end())
		return i->second.get();

	return map.emplace(
		std::make_pair(m, n),
		std::make_unique<FixedPointType>(m, n, _modifier)
	).first->second.get();
}

TupleType const* TypeProvider::tuple(std::vector<Type const*> members)
{
	if (members.empty())
		return &m_emptyTuple;

	return createAndGet<TupleType>(std::move(members));
}

FunctionType const* TypeProvider::function(FunctionDefinition const& _function, FunctionType::Kind _kind)
{
	return createAndGet<FunctionType>(_function, _kind);
}

FunctionType const* TypeProvider::function(VariableDeclaration const& _varDecl)
{
	return createAndGet<FunctionType>(_varDecl);
}

FunctionType const* TypeProvider::function(EventDefinition const& _def)
{
	return createAndGet<FunctionType>(_def);
}

FunctionType const* TypeProvider::function(ErrorDefinition const& _def)
{
	return createAndGet<FunctionType>(_def);
}

FunctionType const* TypeProvider::function(FunctionTypeName const& _typeName)
{
	return createAndGet<FunctionType>(_typeName);
}

FunctionType const* TypeProvider::function(
	strings const& _parameterTypes,
	strings const& _returnParameterTypes,
	FunctionType::Kind _kind,
	StateMutability _stateMutability,
	FunctionType::Options _options
)
{
	// Can only use this constructor for "arbitraryParameters".
	solAssert(!_options.valueSet && !_options.gasSet && !_options.saltSet && !_options.hasBoundFirstArgument);
	return createAndGet<FunctionType>(
		_parameterTypes,
		_returnParameterTypes,
		_kind,
		_stateMutability,
		std::move(_options)
	);
}

FunctionType const* TypeProvider::function(
	TypePointers const& _parameterTypes,
	TypePointers const& _returnParameterTypes,
	strings _parameterNames,
	strings _returnParameterNames,
	FunctionType::Kind _kind,
	StateMutability _stateMutability,
	Declaration const* _declaration,
	FunctionType::Options _options
)
{
	return createAndGet<FunctionType>(
		_parameterTypes,
		_returnParameterTypes,
		_parameterNames,
		_returnParameterNames,
		_kind,
		_stateMutability,
		_declaration,
		std::move(_options)
	);
}

RationalNumberType const* TypeProvider::rationalNumber(rational const& _value, Type const* _compatibleBytesType)
{
	return createAndGet<RationalNumberType>(_value, _compatibleBytesType);
}

ArrayType const* TypeProvider::array(bool _isString)
{
	return _isString ? stringStorage() : bytesStorage();
}

ArrayType const* TypeProvider::array(Type const* _baseType)
{
	return createAndGet<ArrayType>(_baseType);
}

ArrayType const* TypeProvider::array(Type const* _baseType, u256 const& _length)
{
	return createAndGet<ArrayType>(_baseType, _length);
}

ArraySliceType const* TypeProvider::arraySlice(ArrayType const& _arrayType)
{
	return createAndGet<ArraySliceType>(_arrayType);
}

ContractType const* TypeProvider::contract(ContractDefinition const& _contractDef, bool _isSuper)
{
	return createAndGet<ContractType>(_contractDef, _isSuper);
}

EnumType const* TypeProvider::enumType(EnumDefinition const& _enumDef)
{
	return createAndGet<EnumType>(_enumDef);
}

ModuleType const* TypeProvider::module(SourceUnit const& _source)
{
	return createAndGet<ModuleType>(_source);
}

TypeType const* TypeProvider::typeType(Type const* _actualType)
{
	return createAndGet<TypeType>(_actualType);
}

StructType const* TypeProvider::structType(StructDefinition const& _struct)
{
	return createAndGet<StructType>(_struct);
}

ModifierType const* TypeProvider::modifier(ModifierDefinition const& _def)
{
	return createAndGet<ModifierType>(_def);
}

MagicType const* TypeProvider::magic(MagicType::Kind _kind)
{
	solAssert(_kind != MagicType::Kind::MetaType, "MetaType is handled separately");
	return m_magics.at(static_cast<size_t>(_kind)).get();
}

MagicType const* TypeProvider::meta(Type const* _type)
{
	solAssert(
		_type && (
			_type->category() == Type::Category::Contract ||
			_type->category() == Type::Category::Integer ||
			_type->category() == Type::Category::VarInteger ||
			_type->category() == Type::Category::Enum
		),
		"Only enum, contracts or integer types supported for now."
	);
	return createAndGet<MagicType>(_type);
}

MappingType const* TypeProvider::mapping(Type const* _keyType, ASTString _keyName, Type const* _valueType, ASTString _valueName)
{
	return createAndGet<MappingType>(_keyType, _keyName, _valueType, _valueName);
}

MappingType const *TypeProvider::extraCurrencyCollection()
{
	auto keyType = TypeProvider::uint(32);
	auto valueType = TypeProvider::varinteger(32, IntegerType::Modifier::Unsigned);
	return createAndGet<MappingType>(keyType, "", valueType, "");
}

OptionalType const* TypeProvider::optional(Type const* _type)
{
	return createAndGet<OptionalType>(_type);
}

TvmVectorType const* TypeProvider::tvmVector(Type const* _type)
{
	return createAndGet<TvmVectorType>(_type);
}

TvmStackType const* TypeProvider::tvmStack(Type const* _type)
{
	return createAndGet<TvmStackType>(_type);
}

UserDefinedValueType const* TypeProvider::userDefinedValueType(UserDefinedValueTypeDefinition const& _definition)
{
	return createAndGet<UserDefinedValueType>(_definition);
}

IntegerType const* TypeProvider::extraFlags() { return uint(TvmConst::EXTRA_FLAG_SIZE); }
