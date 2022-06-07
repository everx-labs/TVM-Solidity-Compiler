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
 * @date 2014
 * Solidity data types
 */

#include <libsolidity/ast/Types.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>

#include <libsolutil/Algorithms.h>
#include <libsolutil/CommonData.h>
#include <libsolutil/CommonIO.h>
#include <libsolutil/Keccak256.h>
#include <libsolutil/UTF8.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/sliced.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/copy.hpp>

#include <limits>

using namespace std;
using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;

namespace
{

bool isStringOrStrLiteral(Type const &type) {
	if (dynamic_cast<StringLiteralType const*>(&type))
		return true;
	auto array = dynamic_cast<ArrayType const*>(&type);
	return array && array->isString();
}

/// Check whether (_base ** _exp) fits into 4096 bits.
bool fitsPrecisionExp(bigint const& _base, bigint const& _exp)
{
	if (_base == 0)
		return true;

	solAssert(_base > 0, "");

	size_t const bitsMax = 4096;

	unsigned mostSignificantBaseBit = boost::multiprecision::msb(_base);
	if (mostSignificantBaseBit == 0) // _base == 1
		return true;
	if (mostSignificantBaseBit > bitsMax) // _base >= 2 ^ 4096
		return false;

	bigint bitsNeeded = _exp * (mostSignificantBaseBit + 1);

	return bitsNeeded <= bitsMax;
}

/// Checks whether _mantissa * (X ** _exp) fits into 4096 bits,
/// where X is given indirectly via _log2OfBase = log2(X).
bool fitsPrecisionBaseX(
	bigint const& _mantissa,
	double _log2OfBase,
	uint32_t _exp
)
{
	if (_mantissa == 0)
		return true;

	solAssert(_mantissa > 0, "");

	size_t const bitsMax = 4096;

	unsigned mostSignificantMantissaBit = boost::multiprecision::msb(_mantissa);
	if (mostSignificantMantissaBit > bitsMax) // _mantissa >= 2 ^ 4096
		return false;

	bigint bitsNeeded = mostSignificantMantissaBit + bigint(floor(double(_exp) * _log2OfBase)) + 1;
	return bitsNeeded <= bitsMax;
}

/// Checks whether _mantissa * (10 ** _expBase10) fits into 4096 bits.
bool fitsPrecisionBase10(bigint const& _mantissa, uint32_t _expBase10)
{
	double const log2Of10AwayFromZero = 3.3219280948873624;
	return fitsPrecisionBaseX(_mantissa, log2Of10AwayFromZero, _expBase10);
}

/// Checks whether _mantissa * (2 ** _expBase10) fits into 4096 bits.
bool fitsPrecisionBase2(bigint const& _mantissa, uint32_t _expBase2)
{
	return fitsPrecisionBaseX(_mantissa, 1.0, _expBase2);
}

/// Checks whether _value fits into IntegerType _type.
BoolResult fitsIntegerType(bigint const& _value, IntegerType const& _type)
{
	if (_value < 0 && !_type.isSigned())
		return BoolResult::err("Cannot implicitly convert signed literal to unsigned type.");

	if (_type.minValue() > _value || _value > _type.maxValue())
		return BoolResult::err("Literal is too large to fit in " + _type.toString(false) + ".");

	return true;
}

/// Checks whether _value fits into _bits bits when having 1 bit as the sign bit
/// if _signed is true.
bool fitsIntoBits(bigint const& _value, unsigned _bits, bool _signed)
{
	return fitsIntegerType(_value, *TypeProvider::integer(
		_bits,
		_signed ? IntegerType::Modifier::Signed : IntegerType::Modifier::Unsigned
	));
}

util::Result<TypePointers> transformParametersToExternal(TypePointers const& _parameters, bool _inLibrary)
{
	TypePointers transformed;

	for (auto const& type: _parameters)
	{
		if (TypePointer ext = type->interfaceType(_inLibrary).get())
			transformed.push_back(ext);
		else
			return util::Result<TypePointers>::err("Parameter should have external type.");
	}

	return transformed;
}

}

BoolResult Type::isImplicitlyConvertibleTo(Type const& _other) const {
	if (*this == _other) {
		return true;
	}
	if (auto optOther = dynamic_cast<OptionalType const*>(&_other))
		if (isImplicitlyConvertibleTo(*optOther->valueType()))
			return true;
	return false;
}

void Type::clearCache() const
{
	m_members.clear();
}

void StorageOffsets::computeOffsets(TypePointers const& _types)
{
	bigint slotOffset = 0;
	unsigned byteOffset = 0;
	map<size_t, pair<u256, unsigned>> offsets;
	for (size_t i = 0; i < _types.size(); ++i)
	{
		Type const* type = _types[i];
		if (!type->canBeStored())
			continue;
		if (byteOffset + type->storageBytes() > 32)
		{
			// would overflow, go to next slot
			++slotOffset;
			byteOffset = 0;
		}
		if (slotOffset >= bigint(1) << 256)
			BOOST_THROW_EXCEPTION(Error(Error::Type::TypeError) << util::errinfo_comment("Object too large for storage."));
		offsets[i] = make_pair(u256(slotOffset), byteOffset);
		solAssert(type->storageSize() >= 1, "Invalid storage size.");
		if (type->storageSize() == 1 && byteOffset + type->storageBytes() <= 32)
			byteOffset += type->storageBytes();
		else
		{
			slotOffset += type->storageSize();
			byteOffset = 0;
		}
	}
	if (byteOffset > 0)
		++slotOffset;
	if (slotOffset >= bigint(1) << 256)
		BOOST_THROW_EXCEPTION(Error(Error::Type::TypeError) << util::errinfo_comment("Object too large for storage."));
	m_storageSize = u256(slotOffset);
	swap(m_offsets, offsets);
}

pair<u256, unsigned> const* StorageOffsets::offset(size_t _index) const
{
	if (m_offsets.count(_index))
		return &m_offsets.at(_index);
	else
		return nullptr;
}

void MemberList::combine(MemberList const & _other)
{
	m_memberTypes += _other.m_memberTypes;
}

pair<u256, unsigned> const* MemberList::memberStorageOffset(string const& _name) const
{
	if (!m_storageOffsets)
	{
		TypePointers memberTypes;
		memberTypes.reserve(m_memberTypes.size());
		for (auto const& member: m_memberTypes)
			memberTypes.push_back(member.type);
		m_storageOffsets = std::make_unique<StorageOffsets>();
		m_storageOffsets->computeOffsets(memberTypes);
	}
	for (size_t index = 0; index < m_memberTypes.size(); ++index)
		if (m_memberTypes[index].name == _name)
			return m_storageOffsets->offset(index);
	return nullptr;
}

u256 const& MemberList::storageSize() const
{
	// trigger lazy computation
	memberStorageOffset("");
	return m_storageOffsets->storageSize();
}

/// Helper functions for type identifier
namespace
{

string parenthesizeIdentifier(string const& _internal)
{
	return "(" + _internal + ")";
}

template <class Range>
string identifierList(Range const&& _list)
{
	return parenthesizeIdentifier(boost::algorithm::join(_list, ","));
}

string richIdentifier(Type const* _type)
{
	return _type ? _type->richIdentifier() : "";
}

string identifierList(vector<TypePointer> const& _list)
{
	return identifierList(_list | boost::adaptors::transformed(richIdentifier));
}

string identifierList(Type const* _type)
{
	return parenthesizeIdentifier(richIdentifier(_type));
}

string identifierList(Type const* _type1, Type const* _type2)
{
	TypePointers list;
	list.push_back(_type1);
	list.push_back(_type2);
	return identifierList(list);
}

string parenthesizeUserIdentifier(string const& _internal)
{
	return parenthesizeIdentifier(_internal);
}

}

string Type::escapeIdentifier(string const& _identifier)
{
	string ret = _identifier;
	// FIXME: should be _$$$_
	boost::algorithm::replace_all(ret, "$", "$$$");
	boost::algorithm::replace_all(ret, ",", "_$_");
	boost::algorithm::replace_all(ret, "(", "$_");
	boost::algorithm::replace_all(ret, ")", "_$");
	return ret;
}

string Type::identifier() const
{
	string ret = escapeIdentifier(richIdentifier());
	solAssert(ret.find_first_of("0123456789") != 0, "Identifier cannot start with a number.");
	solAssert(
		ret.find_first_not_of("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMONPQRSTUVWXYZ_$") == string::npos,
		"Identifier contains invalid characters."
	);
	return ret;
}

TypePointer Type::commonType(Type const* _a, Type const* _b)
{
	if (!_a || !_b)
		return nullptr;
	else if (_a->mobileType() && _b->isImplicitlyConvertibleTo(*_a->mobileType()))
		return _a->mobileType();
	else if (_b->mobileType() && _a->isImplicitlyConvertibleTo(*_b->mobileType()))
		return _b->mobileType();
	else
		return nullptr;
}

MemberList const& Type::members(ContractDefinition const* _currentScope) const
{
	if (!m_members[_currentScope])
	{
		MemberList::MemberMap members = nativeMembers(_currentScope);
		if (_currentScope)
			members += boundFunctions(*this, *_currentScope);
		m_members[_currentScope] = make_unique<MemberList>(move(members));
	}
	return *m_members[_currentScope];
}

TypePointer Type::fullEncodingType(bool _inLibraryCall, bool _encoderV2, bool) const
{
	TypePointer encodingType = mobileType();
	if (encodingType)
		encodingType = encodingType->interfaceType(_inLibraryCall);
	if (encodingType)
		encodingType = encodingType->encodingType();
	// Structs are fine in the following circumstances:
	// - ABIv2 or,
	// - storage struct for a library
	TypePointer baseType = encodingType;
	while (auto const* arrayType = dynamic_cast<ArrayType const*>(baseType))
		baseType = arrayType->baseType();
	if (dynamic_cast<StructType const*>(baseType))
		if (!_encoderV2)
			return nullptr;
	return encodingType;
}

MemberList::MemberMap Type::boundFunctions(Type const& _type, ContractDefinition const& _scope)
{
	// Normalise data location of type.
	TypePointer type = TypeProvider::withLocationIfReference(&_type);
	set<Declaration const*> seenFunctions;
	MemberList::MemberMap members;
	for (ContractDefinition const* contract: _scope.annotation().linearizedBaseContracts)
		for (UsingForDirective const* ufd: contract->usingForDirectives())
		{
			if (ufd->typeName() && *type != *TypeProvider::withLocationIfReference(
				ufd->typeName()->annotation().type
			))
				continue;
			auto const& library = dynamic_cast<ContractDefinition const&>(
				*ufd->libraryName().annotation().referencedDeclaration
			);
			for (FunctionDefinition const* function: library.definedFunctions())
			{
				if (!function->isVisibleAsLibraryMember() || seenFunctions.count(function))
					continue;
				seenFunctions.insert(function);
				if (function->parameters().empty())
					continue;
				FunctionTypePointer fun = FunctionType(*function, FunctionType::Kind::External).asCallableFunction(true, true);
				if (_type.isImplicitlyConvertibleTo(*fun->selfType()))
					members.emplace_back(function->name(), fun, function);
			}
		}
	return members;
}

string AddressType::richIdentifier() const
{
	return "t_address";
}

BoolResult AddressType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	return _convertTo.category() == category();
}

BoolResult AddressType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (_convertTo.category() == category())
		return true;
	else if (dynamic_cast<ContractType const*>(&_convertTo))
		return true;
	return isImplicitlyConvertibleTo(_convertTo);
}

string AddressType::toString(bool) const
{
	return "address";
}

string AddressType::canonicalName() const
{
	return "address";
}

u256 AddressType::literalValue(Literal const* _literal) const
{
	solAssert(_literal, "");
	solAssert(_literal->value().substr(0, 2) == "0x", "");
	return u256(_literal->valueWithoutUnderscores());
}

TypeResult AddressType::unaryOperatorResult(Token _operator) const
{
	return _operator == Token::Delete ? TypeProvider::emptyTuple() : nullptr;
}


TypeResult AddressType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (!TokenTraits::isCompareOp(_operator))
		return TypeResult::err("Arithmetic operations on addresses are not supported. Convert to integer first before using them.");

	return Type::commonType(this, _other);
}

bool AddressType::operator==(Type const& _other) const
{
	return _other.category() == category();
}

MemberList::MemberMap AddressType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap members = {
		{"balance", TypeProvider::uint(128)},
		{"currencies", TypeProvider::extraCurrencyCollection()},
		{"wid", TypeProvider::integer(8, IntegerType::Modifier::Signed)},
		{"value", TypeProvider::uint256()},
		{"isStdZero", TypeProvider::function(strings(), strings{"bool"}, FunctionType::Kind::AddressIsZero, false, StateMutability::Pure)},
		{"isNone", TypeProvider::function(strings(), strings{"bool"}, FunctionType::Kind::AddressIsZero, false, StateMutability::Pure)},
		{"isExternZero", TypeProvider::function(strings(), strings{"bool"}, FunctionType::Kind::AddressIsZero, false, StateMutability::Pure)}
	};
	members.emplace_back("unpack", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Signed), TypeProvider::uint256()},
			strings{},
			strings{string(), string()},
			FunctionType::Kind::AddressUnpack,
			false, StateMutability::Pure
	));
	members.emplace_back("getType", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(8)},
			strings{},
			strings{string()},
			FunctionType::Kind::AddressType,
			false, StateMutability::Pure
	));
	members.emplace_back("isStdAddrWithoutAnyCast", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::boolean()},
			strings{},
			strings{string()},
			FunctionType::Kind::AddressIsStdAddrWithoutAnyCast,
			false, StateMutability::Pure
	));
	members.emplace_back("transfer", TypeProvider::function(
			{
				TypeProvider::uint(128),
				TypeProvider::boolean(),
				TypeProvider::uint(16),
				TypeProvider::tvmcell(),
				TypeProvider::extraCurrencyCollection(),
				TypeProvider::tvmcell(),
		 	},
			{},
			{"value", "bounce", "flag", "body", "currencies", "stateInit"},
			{},
			FunctionType::Kind::AddressTransfer,
			true,
			StateMutability::Pure
	));
	return members;
}

namespace
{

bool isValidShiftAndAmountType(Token _operator, Type const& _shiftAmountType)
{
	// Disable >>> here.
	if (_operator == Token::SHR)
		return false;
	else if (IntegerType const* otherInt = dynamic_cast<decltype(otherInt)>(&_shiftAmountType))
		return true;
	else if (RationalNumberType const* otherRat = dynamic_cast<decltype(otherRat)>(&_shiftAmountType))
		return !otherRat->isFractional() && otherRat->integerType() && !otherRat->integerType()->isSigned();
	else
		return false;
}

}

IntegerType::IntegerType(unsigned _bits, IntegerType::Modifier _modifier):
	m_bits(_bits), m_modifier(_modifier)
{
	if (_bits == 257 && _modifier == IntegerType::Modifier::Signed) {

	} else {
		solAssert(
				m_bits > 0 && m_bits <= 256,
				"Invalid bit number for integer type: " + util::toString(m_bits)
		);
	}
}

string IntegerType::richIdentifier() const
{
	return "t_" + string(isSigned() ? "" : "u") + "int" + to_string(numBits());
}

BoolResult IntegerType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() == Category::VarInteger){
		IntegerType const& convertTo = dynamic_cast<VarInteger const&>(_convertTo).asIntegerType();
		if (convertTo.m_bits < m_bits)
			return false;
		else if (isSigned())
			return convertTo.isSigned();
		else
			return !convertTo.isSigned() || convertTo.m_bits > m_bits;
	}
	else if (_convertTo.category() == category())
	{
		IntegerType const& convertTo = dynamic_cast<IntegerType const&>(_convertTo);
		if (convertTo.m_bits < m_bits)
			return false;
		else if (isSigned())
			return convertTo.isSigned();
		else
			return !convertTo.isSigned() || convertTo.m_bits > m_bits;
	}
	else if (_convertTo.category() == Category::FixedPoint)
	{
		FixedPointType const& convertTo = dynamic_cast<FixedPointType const&>(_convertTo);
		return maxValue() <= convertTo.maxIntegerValue() && minValue() >= convertTo.minIntegerValue();
	}
	else
		return false;
}

BoolResult IntegerType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	return _convertTo.category() == category() ||
		_convertTo.category() == Category::VarInteger ||
		_convertTo.category() == Category::Address ||
		_convertTo.category() == Category::Contract ||
		_convertTo.category() == Category::Enum ||
		(_convertTo.category() == Category::FixedBytes && numBits() == dynamic_cast<FixedBytesType const&>(_convertTo).numBytes() * 8) ||
		_convertTo.category() == Category::FixedPoint;
}

TypeResult IntegerType::unaryOperatorResult(Token _operator) const
{
	// "delete" is ok for all integer types
	if (_operator == Token::Delete)
		return TypeResult{TypeProvider::emptyTuple()};
	// we allow -, ++ and --
	else if (_operator == Token::Sub || _operator == Token::Inc ||
		_operator == Token::Dec || _operator == Token::BitNot)
		return TypeResult{this};
	else
		return TypeResult::err("");
}

bool IntegerType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	IntegerType const& other = dynamic_cast<IntegerType const&>(_other);
	return other.m_bits == m_bits && other.m_modifier == m_modifier;
}

string IntegerType::toString(bool) const
{
	string prefix = isSigned() ? "int" : "uint";
	return prefix + util::toString(m_bits);
}

bigint IntegerType::minValue() const
{
	if (isSigned())
		return -(bigint(1) << (m_bits - 1));
	else
		return bigint(0);
}

bigint IntegerType::maxValue() const
{
	if (isSigned())
		return (bigint(1) << (m_bits - 1)) - 1;
	else
		return (bigint(1) << m_bits) - 1;
}

TypeResult IntegerType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (
		_other->category() != Category::RationalNumber &&
		_other->category() != Category::FixedPoint &&
		_other->category() != category()
	)
		return nullptr;
	if (TokenTraits::isShiftOp(_operator))
	{
		// Shifts are not symmetric with respect to the type
		if (isValidShiftAndAmountType(_operator, *_other))
			return this;
		else
			return nullptr;
	}
	else if (Token::Exp == _operator)
	{
		if (auto otherIntType = dynamic_cast<IntegerType const*>(_other))
		{
			if (otherIntType->isSigned())
				return TypeResult::err("Exponentiation power is not allowed to be a signed integer type.");
		}
		else if (dynamic_cast<FixedPointType const*>(_other))
			return nullptr;
		return this;
	}

	auto commonType = Type::commonType(this, _other); //might be an integer or fixed point
	if (!commonType)
		return nullptr;

	// All integer types can be compared
	if (TokenTraits::isCompareOp(_operator))
		return commonType;
	if (TokenTraits::isBooleanOp(_operator))
		return nullptr;
	return commonType;
}

FixedPointType::FixedPointType(unsigned _totalBits, unsigned _fractionalDigits, FixedPointType::Modifier _modifier):
	m_totalBits(_totalBits), m_fractionalDigits(_fractionalDigits), m_modifier(_modifier)
{
	solAssert(
		8 <= m_totalBits && m_totalBits <= 256 && m_totalBits % 8 == 0 && m_fractionalDigits <= 80,
		"Invalid bit number(s) for fixed type: " +
		util::toString(_totalBits) + "x" + util::toString(_fractionalDigits)
	);
}

string FixedPointType::richIdentifier() const
{
	return "t_" + string(isSigned() ? "" : "u") + "fixed" + to_string(m_totalBits) + "x" + to_string(m_fractionalDigits);
}

BoolResult FixedPointType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() == category())
	{
		FixedPointType const& convertTo = dynamic_cast<FixedPointType const&>(_convertTo);
		if (convertTo.fractionalDigits() < m_fractionalDigits)
			return BoolResult::err("Too many fractional digits.");
		if (convertTo.numBits() < m_totalBits)
			return false;
		else
			return convertTo.maxIntegerValue() >= maxIntegerValue() && convertTo.minIntegerValue() <= minIntegerValue();
	}
	return false;
}

BoolResult FixedPointType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	return _convertTo.category() == category() ||
			_convertTo.category() == Category::Integer ||
			_convertTo.category() == Category::VarInteger;
}

TypeResult FixedPointType::unaryOperatorResult(Token _operator) const
{
	switch (_operator)
	{
	case Token::Delete:
		// "delete" is ok for all fixed types
		return TypeResult{TypeProvider::emptyTuple()};
	case Token::Add:
	case Token::Sub:
	case Token::Inc:
	case Token::Dec:
		// for fixed, we allow +, -, ++ and --
		return this;
	default:
		return nullptr;
	}
}

bool FixedPointType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	FixedPointType const& other = dynamic_cast<FixedPointType const&>(_other);
	return other.m_totalBits == m_totalBits && other.m_fractionalDigits == m_fractionalDigits && other.m_modifier == m_modifier;
}

string FixedPointType::toString(bool) const
{
	string prefix = isSigned() ? "fixed" : "ufixed";
	return prefix + util::toString(m_totalBits) + "x" + util::toString(m_fractionalDigits);
}

bigint FixedPointType::maxIntegerValue() const
{
	bigint maxValue = (bigint(1) << (m_totalBits - (isSigned() ? 1 : 0))) - 1;
	return maxValue / boost::multiprecision::pow(bigint(10), m_fractionalDigits);
}

bigint FixedPointType::minIntegerValue() const
{
	if (isSigned())
	{
		bigint minValue = -(bigint(1) << (m_totalBits - (isSigned() ? 1 : 0)));
		return minValue / boost::multiprecision::pow(bigint(10), m_fractionalDigits);
	}
	else
		return bigint(0);
}

TypeResult FixedPointType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	auto commonType = Type::commonType(this, _other);

	if (!commonType)
		return nullptr;

	// All fixed types can be compared
	if (TokenTraits::isCompareOp(_operator))
		return commonType;
	if (TokenTraits::isBitOp(_operator) || TokenTraits::isBooleanOp(_operator) || _operator == Token::Exp)
		return nullptr;
	return commonType;
}

IntegerType const* FixedPointType::asIntegerType() const
{
	return TypeProvider::integer(numBits(), isSigned() ? IntegerType::Modifier::Signed : IntegerType::Modifier::Unsigned);
}

tuple<bool, rational> RationalNumberType::parseRational(string const& _value)
{
	rational value;
	try
	{
		auto radixPoint = find(_value.begin(), _value.end(), '.');

		if (radixPoint != _value.end())
		{
			if (
				!all_of(radixPoint + 1, _value.end(), ::isdigit) ||
				!all_of(_value.begin(), radixPoint, ::isdigit)
			)
				return make_tuple(false, rational(0));

			// Only decimal notation allowed here, leading zeros would switch to octal.
			auto fractionalBegin = find_if_not(
				radixPoint + 1,
				_value.end(),
				[](char const& a) { return a == '0'; }
			);

			rational numerator;
			rational denominator(1);

			denominator = bigint(string(fractionalBegin, _value.end()));
			denominator /= boost::multiprecision::pow(
				bigint(10),
				distance(radixPoint + 1, _value.end())
			);
			numerator = bigint(string(_value.begin(), radixPoint));
			value = numerator + denominator;
		}
		else
			value = bigint(_value);
		return make_tuple(true, value);
	}
	catch (...)
	{
		return make_tuple(false, rational(0));
	}
}

tuple<bool, rational> RationalNumberType::isValidLiteral(Literal const& _literal)
{
	rational value;
	try
	{
		ASTString valueString = _literal.valueWithoutUnderscores();

		auto expPoint = find(valueString.begin(), valueString.end(), 'e');
		if (expPoint == valueString.end())
			expPoint = find(valueString.begin(), valueString.end(), 'E');

		if (boost::starts_with(valueString, "0x"))
		{
			// process as hex
			value = bigint(valueString);
		}
		else if (expPoint != valueString.end())
		{
			// Parse mantissa and exponent. Checks numeric limit.
			tuple<bool, rational> mantissa = parseRational(string(valueString.begin(), expPoint));

			if (!get<0>(mantissa))
				return make_tuple(false, rational(0));
			value = get<1>(mantissa);

			// 0E... is always zero.
			if (value == 0)
				return make_tuple(true, rational(0));

			bigint exp = bigint(string(expPoint + 1, valueString.end()));

			if (exp > numeric_limits<int32_t>::max() || exp < numeric_limits<int32_t>::min())
				return make_tuple(false, rational(0));

			uint32_t expAbs = bigint(abs(exp)).convert_to<uint32_t>();

			if (exp < 0)
			{
				if (!fitsPrecisionBase10(abs(value.denominator()), expAbs))
					return make_tuple(false, rational(0));
				value /= boost::multiprecision::pow(
					bigint(10),
					expAbs
				);
			}
			else if (exp > 0)
			{
				if (!fitsPrecisionBase10(abs(value.numerator()), expAbs))
					return make_tuple(false, rational(0));
				value *= boost::multiprecision::pow(
					bigint(10),
					expAbs
				);
			}
		}
		else
		{
			// parse as rational number
			tuple<bool, rational> tmp = parseRational(valueString);
			if (!get<0>(tmp))
				return tmp;
			value = get<1>(tmp);
		}
	}
	catch (...)
	{
		return make_tuple(false, rational(0));
	}
	switch (_literal.subDenomination())
	{
		case Literal::SubDenomination::None:
		case Literal::SubDenomination::Nano:
		case Literal::SubDenomination::Nanoton:
		case Literal::SubDenomination::Nanoever:
		case Literal::SubDenomination::NTon:
		case Literal::SubDenomination::Second:
			break;
		case Literal::SubDenomination::Micro:
		case Literal::SubDenomination::Microton:
		case Literal::SubDenomination::Microever:
			value *= bigint("1000");
			break;
		case Literal::SubDenomination::Milli:
		case Literal::SubDenomination::Milliton:
		case Literal::SubDenomination::Milliever:
			value *= bigint("1000000");
			break;
		case Literal::SubDenomination::Ton:
		case Literal::SubDenomination::Ever:
		case Literal::SubDenomination::SmallTon:
		case Literal::SubDenomination::SmallEver:
			value *= bigint("1000000000");
			break;
		case Literal::SubDenomination::Kiloton:
		case Literal::SubDenomination::Kiloever:
		case Literal::SubDenomination::KTon:
		case Literal::SubDenomination::KEver:
			value *= bigint("1000000000000");
			break;
		case Literal::SubDenomination::Megaton:
		case Literal::SubDenomination::Megaever:
		case Literal::SubDenomination::MTon:
		case Literal::SubDenomination::MEver:
			value *= bigint("1000000000000000");
			break;
		case Literal::SubDenomination::Gigaton:
		case Literal::SubDenomination::Gigaever:
		case Literal::SubDenomination::GTon:
		case Literal::SubDenomination::GEver:
			value *= bigint("1000000000000000000");
			break;
		case Literal::SubDenomination::Minute:
			value *= bigint("60");
			break;
		case Literal::SubDenomination::Hour:
			value *= bigint("3600");
			break;
		case Literal::SubDenomination::Day:
			value *= bigint("86400");
			break;
		case Literal::SubDenomination::Week:
			value *= bigint("604800");
			break;
		case Literal::SubDenomination::Year:
			value *= bigint("31536000");
			break;
	}


	return make_tuple(true, value);
}

BoolResult RationalNumberType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	switch (_convertTo.category())
	{
	case Category::VarInteger:
	{
		if (isFractional())
			return false;
		IntegerType const& targetType = dynamic_cast<VarInteger const&>(_convertTo).asIntegerType();
		return fitsIntegerType(m_value.numerator(), targetType);
	}
	case Category::Integer:
	{
		if (isFractional())
			return false;
		IntegerType const& targetType = dynamic_cast<IntegerType const&>(_convertTo);
		return fitsIntegerType(m_value.numerator(), targetType);
	}
	case Category::FixedPoint:
	{
		FixedPointType const& targetType = dynamic_cast<FixedPointType const&>(_convertTo);
		// Store a negative number into an unsigned.
		if (isNegative() && !targetType.isSigned())
			return false;
		if (!isFractional())
			return (targetType.minIntegerValue() <= m_value) && (m_value <= targetType.maxIntegerValue());
		rational value = m_value * pow(bigint(10), targetType.fractionalDigits());
		// Need explicit conversion since truncation will occur.
		if (value.denominator() != 1)
			return false;
		return fitsIntoBits(value.numerator(), targetType.numBits(), targetType.isSigned());
	}
	case Category::FixedBytes:
		return (m_value == rational(0)) || (m_compatibleBytesType && *m_compatibleBytesType == _convertTo);
	default:
		return false;
	}
}

BoolResult RationalNumberType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (isImplicitlyConvertibleTo(_convertTo))
		return true;
	else if (_convertTo.category() != Category::FixedBytes)
	{
		TypePointer mobType = mobileType();
		return (mobType && mobType->isExplicitlyConvertibleTo(_convertTo));
	}
	else
		return false;
}

TypeResult RationalNumberType::unaryOperatorResult(Token _operator) const
{
	rational value;
	switch (_operator)
	{
	case Token::BitNot:
		if (isFractional())
			return nullptr;
		value = ~m_value.numerator();
		break;
	case Token::Add:
		value = +(m_value);
		break;
	case Token::Sub:
		value = -(m_value);
		break;
	case Token::After:
		return this;
	default:
		return nullptr;
	}
	return TypeResult{TypeProvider::rationalNumber(value)};
}

TypeResult RationalNumberType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (_other->category() == Category::Integer || _other->category() == Category::FixedPoint)
	{
		auto commonType = Type::commonType(this, _other);
		if (!commonType)
			return nullptr;
		return commonType->binaryOperatorResult(_operator, _other);
	}
	else if (_other->category() != category())
		return nullptr;

	RationalNumberType const& other = dynamic_cast<RationalNumberType const&>(*_other);
	if (TokenTraits::isCompareOp(_operator))
	{
		// Since we do not have a "BoolConstantType", we have to do the actual comparison
		// at runtime and convert to mobile typse first. Such a comparison is not a very common
		// use-case and will be optimized away.
		TypePointer thisMobile = mobileType();
		TypePointer otherMobile = other.mobileType();
		if (!thisMobile || !otherMobile)
			return nullptr;
		return thisMobile->binaryOperatorResult(_operator, otherMobile);
	}
	else
	{
		rational value;
		bool fractional = isFractional() || other.isFractional();
		switch (_operator)
		{
		//bit operations will only be enabled for integers and fixed types that resemble integers
		case Token::BitOr:
			if (fractional)
				return nullptr;
			value = m_value.numerator() | other.m_value.numerator();
			break;
		case Token::BitXor:
			if (fractional)
				return nullptr;
			value = m_value.numerator() ^ other.m_value.numerator();
			break;
		case Token::BitAnd:
			if (fractional)
				return nullptr;
			value = m_value.numerator() & other.m_value.numerator();
			break;
		case Token::Add:
			value = m_value + other.m_value;
			break;
		case Token::Sub:
			value = m_value - other.m_value;
			break;
		case Token::Mul:
			value = m_value * other.m_value;
			break;
		case Token::Div:
			if (other.m_value == rational(0))
				return nullptr;
			else
				value = m_value / other.m_value;
			break;
		case Token::Mod:
			if (other.m_value == rational(0))
				return nullptr;
			else if (fractional)
			{
				rational tempValue = m_value / other.m_value;
				value = m_value - (tempValue.numerator() / tempValue.denominator()) * other.m_value;
			}
			else
				value = m_value.numerator() % other.m_value.numerator();
			break;
		case Token::Exp:
		{
			if (other.isFractional())
				return nullptr;
			solAssert(other.m_value.denominator() == 1, "");
			bigint const& exp = other.m_value.numerator();

			// x ** 0 = 1
			// for 0, 1 and -1 the size of the exponent doesn't have to be restricted
			if (exp == 0)
				value = 1;
			else if (m_value.numerator() == 0 || m_value == 1)
				value = m_value;
			else if (m_value == -1)
			{
				bigint isOdd = abs(exp) & bigint(1);
				value = 1 - 2 * isOdd.convert_to<int>();
			}
			else
			{
				if (abs(exp) > numeric_limits<uint32_t>::max())
					return nullptr; // This will need too much memory to represent.

				uint32_t absExp = bigint(abs(exp)).convert_to<uint32_t>();

				if (!fitsPrecisionExp(abs(m_value.numerator()), absExp) || !fitsPrecisionExp(abs(m_value.denominator()), absExp))
					return TypeResult::err("Precision of rational constants is limited to 4096 bits.");

				static auto const optimizedPow = [](bigint const& _base, uint32_t _exponent) -> bigint {
					if (_base == 1)
						return 1;
					else if (_base == -1)
						return 1 - 2 * int(_exponent & 1);
					else
						return boost::multiprecision::pow(_base, _exponent);
				};

				bigint numerator = optimizedPow(m_value.numerator(), absExp);
				bigint denominator = optimizedPow(m_value.denominator(), absExp);

				if (exp >= 0)
					value = makeRational(numerator, denominator);
				else
					// invert
					value = makeRational(denominator, numerator);
			}
			break;
		}
		case Token::SHL:
		{
			if (fractional)
				return nullptr;
			else if (other.m_value < 0)
				return nullptr;
			else if (other.m_value > numeric_limits<uint32_t>::max())
				return nullptr;
			if (m_value.numerator() == 0)
				value = 0;
			else
			{
				uint32_t exponent = other.m_value.numerator().convert_to<uint32_t>();
				if (!fitsPrecisionBase2(abs(m_value.numerator()), exponent))
					return nullptr;
				value = m_value.numerator() * boost::multiprecision::pow(bigint(2), exponent);
			}
			break;
		}
		// NOTE: we're using >> (SAR) to denote right shifting. The type of the LValue
		//       determines the resulting type and the type of shift (SAR or SHR).
		case Token::SAR:
		{
			if (fractional)
				return nullptr;
			else if (other.m_value < 0)
				return nullptr;
			else if (other.m_value > numeric_limits<uint32_t>::max())
				return nullptr;
			if (m_value.numerator() == 0)
				value = 0;
			else
			{
				uint32_t exponent = other.m_value.numerator().convert_to<uint32_t>();
				if (exponent > boost::multiprecision::msb(boost::multiprecision::abs(m_value.numerator())))
					value = m_value.numerator() < 0 ? -1 : 0;
				else
				{
					if (m_value.numerator() < 0)
						// Add 1 to the negative value before dividing to get a result that is strictly too large,
						// then subtract 1 afterwards to round towards negative infinity.
						// This is the same algorithm as used in ExpressionCompiler::appendShiftOperatorCode(...).
						// To see this note that for negative x, xor(x,all_ones) = (-x-1) and
						// therefore xor(div(xor(x,all_ones), exp(2, shift_amount)), all_ones) is
						// -(-x - 1) / 2^shift_amount - 1, which is the same as
						// (x + 1) / 2^shift_amount - 1.
						value = rational((m_value.numerator() + 1) / boost::multiprecision::pow(bigint(2), exponent) - bigint(1), 1);
					else
						value = rational(m_value.numerator() / boost::multiprecision::pow(bigint(2), exponent), 1);
				}
			}
			break;
		}
		default:
			return nullptr;
		}

		// verify that numerator and denominator fit into 4096 bit after every operation
		if (value.numerator() != 0 && max(boost::multiprecision::msb(abs(value.numerator())), boost::multiprecision::msb(abs(value.denominator()))) > 4096)
			return TypeResult::err("Precision of rational constants is limited to 4096 bits.");

		return TypeResult{TypeProvider::rationalNumber(value)};
	}
}

string RationalNumberType::richIdentifier() const
{
	// rational seemingly will put the sign always on the numerator,
	// but let just make it deterministic here.
	bigint numerator = abs(m_value.numerator());
	bigint denominator = abs(m_value.denominator());
	if (m_value < 0)
		return "t_rational_minus_" + numerator.str() + "_by_" + denominator.str();
	else
		return "t_rational_" + numerator.str() + "_by_" + denominator.str();
}

bool RationalNumberType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	RationalNumberType const& other = dynamic_cast<RationalNumberType const&>(_other);
	return m_value == other.m_value;
}

string RationalNumberType::bigintToReadableString(bigint const& _num)
{
	string str = _num.str();
	if (str.size() > 32)
	{
		int omitted = str.size() - 8;
		str = str.substr(0, 4) + "...(" + to_string(omitted) + " digits omitted)..." + str.substr(str.size() - 4, 4);
	}
	return str;
}

string RationalNumberType::toString(bool) const
{
	if (!isFractional())
		return "int_const " + bigintToReadableString(m_value.numerator());

	string numerator = bigintToReadableString(m_value.numerator());
	string denominator = bigintToReadableString(m_value.denominator());
	return "rational_const " + numerator + " / " + denominator;
}

u256 RationalNumberType::literalValue(Literal const*) const
{
	// We ignore the literal and hope that the type was correctly determined to represent
	// its value.

	u256 value;
	bigint shiftedValue;

	if (!isFractional())
		shiftedValue = m_value.numerator();
	else
	{
		auto fixed = fixedPointType();
		solAssert(fixed, "Rational number cannot be represented as fixed point type.");
		int fractionalDigits = fixed->fractionalDigits();
		shiftedValue = m_value.numerator() * boost::multiprecision::pow(bigint(10), fractionalDigits) / m_value.denominator();
	}

	// we ignore the literal and hope that the type was correctly determined
	solAssert(shiftedValue <= u256(-1), "Number constant too large.");
	solAssert(shiftedValue >= -(bigint(1) << 255), "Number constant too small.");

	if (m_value >= rational(0))
		value = u256(shiftedValue);
	else
		value = s2u(s256(shiftedValue));
	return value;
}

bigint RationalNumberType::value() const {
	if (!isFractional())
		return m_value.numerator();

	bigint shiftedValue;
	auto fixed = fixedPointType();
	solAssert(fixed, "Rational number cannot be represented as fixed point type.");
	int fractionalDigits = fixed->fractionalDigits();
	shiftedValue = m_value.numerator() * boost::multiprecision::pow(bigint(10), fractionalDigits) / m_value.denominator();

	// we ignore the literal and hope that the type was correctly determined
	solAssert(shiftedValue <= u256(-1), "Number constant too large.");
	solAssert(shiftedValue >= -(bigint(1) << 255), "Number constant too small.");
	return shiftedValue;
}

TypePointer RationalNumberType::mobileType() const
{
	if (!isFractional())
		return integerType();
	else
		return fixedPointType();
}

IntegerType const* RationalNumberType::integerType() const
{
	solAssert(!isFractional(), "integerType() called for fractional number.");
	bigint value = m_value.numerator();
	bool negative = (value < 0);
	if (negative) // convert to positive number of same bit requirements
		value = ((0 - value) - 1) << 1;
	if (value > u256(-1))
		return nullptr;
	else
		return TypeProvider::integer(
			max(util::bytesRequired(value), 1u) * 8,
			negative ? IntegerType::Modifier::Signed : IntegerType::Modifier::Unsigned
		);
}

FixedPointType const* RationalNumberType::fixedPointType() const
{
	bool negative = (m_value < 0);
	unsigned fractionalDigits = 0;
	rational value = abs(m_value); // We care about the sign later.
	rational maxValue = negative ?
		rational(bigint(1) << 255, 1):
		rational((bigint(1) << 256) - 1, 1);

	while (value * 10 <= maxValue && value.denominator() != 1 && fractionalDigits < 80)
	{
		value *= 10;
		fractionalDigits++;
	}

	if (value > maxValue)
		return nullptr;

	// This means we round towards zero for positive and negative values.
	bigint v = value.numerator() / value.denominator();

	if (negative && v != 0)
		// modify value to satisfy bit requirements for negative numbers:
		// add one bit for sign and decrement because negative numbers can be larger
		v = (v - 1) << 1;

	if (v > u256(-1))
		return nullptr;

	unsigned totalBits = max(util::bytesRequired(v), 1u) * 8;
	solAssert(totalBits <= 256, "");

	return TypeProvider::fixedPoint(
		totalBits, fractionalDigits,
		negative ? FixedPointType::Modifier::Signed : FixedPointType::Modifier::Unsigned
	);
}

StringLiteralType::StringLiteralType(Literal const& _literal):
	m_value(_literal.value())
{
}

StringLiteralType::StringLiteralType(string const& _value):
	m_value{_value}
{
}

BoolResult StringLiteralType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (auto fixedBytes = dynamic_cast<FixedBytesType const*>(&_convertTo))
		return size_t(fixedBytes->numBytes()) >= m_value.size();
	if (auto arrayType = dynamic_cast<ArrayType const*>(&_convertTo))
		return
			arrayType->isByteArray() && !(arrayType->isString() && !isValidUTF8());
	else
		return false;
}

string StringLiteralType::richIdentifier() const
{
	// Since we have to return a valid identifier and the string itself may contain
	// anything, we hash it.
	return "t_stringliteral_" + util::toHex(util::keccak256(m_value).asBytes());
}

bool StringLiteralType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	return m_value == dynamic_cast<StringLiteralType const&>(_other).m_value;
}

std::string StringLiteralType::toString(bool) const
{
	size_t invalidSequence;

	if (!util::validateUTF8(m_value, invalidSequence))
		return "literal_string (contains invalid UTF-8 sequence at position " + util::toString(invalidSequence) + ")";

	return "literal_string \"" + m_value + "\"";
}

TypePointer StringLiteralType::mobileType() const
{
	return TypeProvider::stringMemory();
}

bool StringLiteralType::isValidUTF8() const
{
	return util::validateUTF8(m_value);
}

TypeResult StringLiteralType::binaryOperatorResult(Token _operator, Type const *_other) const {
	if (_operator == Token::Add &&
		(_other->category() == Type::Category::FixedBytes || isStringOrStrLiteral(*_other))
	) {
		// We don't return Type::commonType() because Type::commonType(bytesN, string_literal) == bytesN
		return TypeProvider::array(true);
	}
	if (TokenTraits::isCompareOp(_operator))
		return Type::commonType(this, _other);
	return nullptr;
}

FixedBytesType::FixedBytesType(unsigned _bytes): m_bytes(_bytes)
{
	solAssert(
		m_bytes > 0 && m_bytes <= 32,
		"Invalid byte number for fixed bytes type: " + util::toString(m_bytes)
	);
}

BoolResult FixedBytesType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() != category())
		return false;
	FixedBytesType const& convertTo = dynamic_cast<FixedBytesType const&>(_convertTo);
	return convertTo.m_bytes >= m_bytes;
}

BoolResult FixedBytesType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (auto arr = dynamic_cast<ArrayType const*>(&_convertTo);
		arr && arr->isByteArray()
	) {
		return true;
	}
	return (_convertTo.category() == Category::Integer && numBytes() * 8 == dynamic_cast<IntegerType const&>(_convertTo).numBits()) ||
		_convertTo.category() == Category::FixedPoint ||
		_convertTo.category() == category();
}

TypeResult FixedBytesType::unaryOperatorResult(Token _operator) const
{
	// "delete" and "~" is okay for FixedBytesType
	if (_operator == Token::Delete)
		return TypeResult{TypeProvider::emptyTuple()};
	else if (_operator == Token::BitNot)
		return this;

	return nullptr;
}

TypeResult FixedBytesType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (_operator == Token::Add && isStringOrStrLiteral(*_other))
	{
		return TypeProvider::array(true);
	}

	if (TokenTraits::isShiftOp(_operator))
	{
		if (isValidShiftAndAmountType(_operator, *_other))
			return this;
		else
			return nullptr;
	}

	auto commonType = dynamic_cast<FixedBytesType const*>(Type::commonType(this, _other));
	if (!commonType)
		return nullptr;

	// FixedBytes can be compared and have bitwise operators applied to them
	if (TokenTraits::isCompareOp(_operator) || TokenTraits::isBitOp(_operator))
		return TypeResult(commonType);

	return nullptr;
}

MemberList::MemberMap FixedBytesType::nativeMembers(ContractDefinition const*) const
{
	return MemberList::MemberMap{MemberList::Member{"length", TypeProvider::uint(8)}};
}

string FixedBytesType::richIdentifier() const
{
	return "t_bytes" + to_string(m_bytes);
}

bool FixedBytesType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	FixedBytesType const& other = dynamic_cast<FixedBytesType const&>(_other);
	return other.m_bytes == m_bytes;
}

u256 BoolType::literalValue(Literal const* _literal) const
{
	solAssert(_literal, "");
	if (_literal->token() == Token::TrueLiteral)
		return u256(1);
	else if (_literal->token() == Token::FalseLiteral)
		return u256(0);
	else
		solAssert(false, "Bool type constructed from non-boolean literal.");
}

TypeResult BoolType::unaryOperatorResult(Token _operator) const
{
	if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	else if (_operator == Token::Not)
		return this;
	else
		return nullptr;
}

TypeResult BoolType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (category() != _other->category())
		return nullptr;
	if (_operator == Token::Equal || _operator == Token::NotEqual || _operator == Token::And || _operator == Token::Or)
		return _other;
	else
		return nullptr;
}

Type const* ContractType::encodingType() const
{
	if (isSuper())
		return nullptr;

	return TypeProvider::address();
}

BoolResult ContractType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (m_super)
		return false;

	if (*this == _convertTo)
		return true;
	if (_convertTo.category() == Category::Contract)
	{
		auto const& bases = contractDefinition().annotation().linearizedBaseContracts;
		if (m_super && bases.size() <= 1)
			return false;
		return find(
			m_super ? ++bases.begin() : bases.begin(), bases.end(),
			&dynamic_cast<ContractType const&>(_convertTo).contractDefinition()
		) != bases.end();
	}
	if (_convertTo.category() == Category::Address)
		return true;
	return false;
}

BoolResult ContractType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (m_super)
		return false;

	if (dynamic_cast<AddressType const*>(&_convertTo))
		return true;

	return isImplicitlyConvertibleTo(_convertTo);
}

TypeResult ContractType::unaryOperatorResult(Token _operator) const
{
	if (isSuper())
		return nullptr;
	else if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	else
		return nullptr;
}

Type const* ReferenceType::withLocation(bool _isPointer) const
{
	return TypeProvider::withLocation(this, _isPointer);
}

TypeResult ReferenceType::unaryOperatorResult(Token _operator) const
{
	if (_operator != Token::Delete)
		return nullptr;
	return TypeProvider::emptyTuple();
}

TypePointer ReferenceType::copyForLocationIfReference(Type const* _type) const
{
	return TypeProvider::withLocationIfReference(_type);
}

string ReferenceType::identifierLocationSuffix() const
{
	string id;
	if (isPointer())
		id += "_ptr";
	return id;
}

ArrayType::ArrayType(bool _isString):
	ReferenceType(),
	m_arrayKind(_isString ? ArrayKind::String : ArrayKind::Bytes),
	m_baseType{TypeProvider::byte()}
{
}

TypeResult ArrayType::binaryOperatorResult(Token _operator, const Type *_other) const
{
	if (isString()) {
		if (_operator == Token::Add &&
			(_other->category() == Type::Category::FixedBytes || isStringOrStrLiteral(*_other))
		) {
			return TypeProvider::array(true);
		}
		if (TokenTraits::isCompareOp(_operator)) {
			return Type::commonType(this, _other);
		}
	}
	if (isByteArray() && _operator == Token::Equal)
		return Type::commonType(this, _other);
	return nullptr;
}

void ArrayType::clearCache() const
{
	Type::clearCache();

	m_interfaceType.reset();
	m_interfaceType_library.reset();
}

BoolResult ArrayType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() != category())
		return false;
	auto& convertTo = dynamic_cast<ArrayType const&>(_convertTo);
	if (isByteArray() ^ convertTo.isByteArray())
		return false;
	Type const* t0 = baseType();
	Type const* t1 = convertTo.baseType();
	while (dynamic_cast<ArrayType const*>(t0) && dynamic_cast<ArrayType const*>(t1)) {
		t0 = dynamic_cast<ArrayType const*>(t0)->baseType();
		t1 = dynamic_cast<ArrayType const*>(t1)->baseType();
	}
	return *t0 == *t1;
}

BoolResult ArrayType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (isImplicitlyConvertibleTo(_convertTo))
		return true;

	if (isByteArray() && (_convertTo.category() == Category::FixedBytes))
		return true;

	// allow conversion bytes <-> string
	if (_convertTo.category() != category())
		return false;
	auto& convertTo = dynamic_cast<ArrayType const&>(_convertTo);
	if (!isByteArray() || !convertTo.isByteArray())
		return false;
	return true;
}

string ArrayType::richIdentifier() const
{
	string id;
	if (isString())
		id = "t_string";
	else if (isByteArray())
		id = "t_bytes";
	else
	{
		id = "t_array";
		id += identifierList(baseType());
		if (isDynamicallySized())
			id += "dyn";
		else
			id += length().str();
	}
	id += identifierLocationSuffix();

	return id;
}

bool ArrayType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	ArrayType const& other = dynamic_cast<ArrayType const&>(_other);
	if (
		!ReferenceType::operator==(other) ||
		other.isByteArray() != isByteArray() ||
		other.isString() != isString() ||
		other.isDynamicallySized() != isDynamicallySized()
	)
		return false;
	if (*other.baseType() != *baseType())
		return false;
	return isDynamicallySized() || length() == other.length();
}

bool ArrayType::validForCalldata() const
{
	if (auto arrayBaseType = dynamic_cast<ArrayType const*>(baseType()))
		if (!arrayBaseType->validForCalldata())
			return false;
	return isDynamicallySized() || unlimitedStaticCalldataSize(true) <= numeric_limits<unsigned>::max();
}

bigint ArrayType::unlimitedStaticCalldataSize(bool _padded) const
{
	solAssert(!isDynamicallySized(), "");
	bigint size = bigint(length()) * calldataStride();
	if (_padded)
		size = ((size + 31) / 32) * 32;
	return size;
}

unsigned ArrayType::calldataEncodedSize(bool _padded) const
{
	solAssert(!isDynamicallyEncoded(), "");
	bigint size = unlimitedStaticCalldataSize(_padded);
	solAssert(size <= numeric_limits<unsigned>::max(), "Array size does not fit unsigned.");
	return unsigned(size);
}

unsigned ArrayType::calldataEncodedTailSize() const
{
	solAssert(isDynamicallyEncoded(), "");
	if (isDynamicallySized())
		// We do not know the dynamic length itself, but at least the uint256 containing the
		// length must still be present.
		return 32;
	bigint size = unlimitedStaticCalldataSize(false);
	solAssert(size <= numeric_limits<unsigned>::max(), "Array size does not fit unsigned.");
	return unsigned(size);
}

bool ArrayType::isDynamicallyEncoded() const
{
	return isDynamicallySized() || baseType()->isDynamicallyEncoded();
}

u256 ArrayType::storageSize() const
{
	if (isDynamicallySized())
		return 1;

	bigint size;
	unsigned baseBytes = baseType()->storageBytes();
	if (baseBytes == 0)
		size = 1;
	else if (baseBytes < 32)
	{
		unsigned itemsPerSlot = 32 / baseBytes;
		size = (bigint(length()) + (itemsPerSlot - 1)) / itemsPerSlot;
	}
	else
		size = bigint(length()) * baseType()->storageSize();
	if (size >= bigint(1) << 256)
		BOOST_THROW_EXCEPTION(Error(Error::Type::TypeError) << util::errinfo_comment("Array too large for storage."));
	return max<u256>(1, u256(size));
}

unsigned ArrayType::sizeOnStack() const
{
	return 1;
}

string ArrayType::toString(bool _short) const
{
	string ret;
	if (isString())
		ret = "string";
	else if (isByteArray())
		ret = "bytes";
	else
	{
		ret = baseType()->toString(_short) + "[";
		if (!isDynamicallySized())
			ret += length().str();
		ret += "]";
	}
	return ret;
}

string ArrayType::canonicalName() const
{
	string ret;
	if (isString())
		ret = "string";
	else if (isByteArray())
		ret = "bytes";
	else
	{
		ret = baseType()->canonicalName() + "[";
		if (!isDynamicallySized())
			ret += length().str();
		ret += "]";
	}
	return ret;
}

string ArrayType::signatureInExternalFunction(bool _structsByName) const
{
	if (isByteArray())
		return canonicalName();
	else
	{
		solAssert(baseType(), "");
		return
			baseType()->signatureInExternalFunction(_structsByName) +
			"[" +
			(isDynamicallySized() ? "" : length().str()) +
			"]";
	}
}

MemberList::MemberMap ArrayType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap members;

	members.emplace_back("empty", TypeProvider::function(
			{},
			{TypeProvider::boolean()},
			{},
			{{}},
			FunctionType::Kind::ArrayEmpty,
			false,
			StateMutability::Pure
	));

	if (isByteArray())
	{
		members.emplace_back("toSlice", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmslice()},
			strings{},
			strings{{}},
			FunctionType::Kind::ByteToSlice,
			false, StateMutability::Pure
		));
		members.emplace_back("dataSize", TypeProvider::function(
				{TypeProvider::uint256()},
				{TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()},
				{{}},
				{{}, {}, {}},
				FunctionType::Kind::TVMDataSize,
				{}, StateMutability::Pure
		));
		members.emplace_back("dataSizeQ", TypeProvider::function(
			{TypeProvider::uint256()},
			{TypeProvider::optional(TypeProvider::tuple({TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()}))},
			{{}},
			{{}},
			FunctionType::Kind::TVMDataSize,
			{}, StateMutability::Pure
		));
		members.emplace_back("append", TypeProvider::function(
				TypePointers{TypeProvider::bytesMemory()},
				TypePointers{},
				strings{string("tail")},
				strings{},
				FunctionType::Kind::StringMethod,
				false, StateMutability::Pure
		));
	}

	if (!isString())
	{
		members.emplace_back("length", TypeProvider::uint256());
		members.emplace_back("push", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			isByteArray() ? FunctionType::Kind::ByteArrayPush : FunctionType::Kind::ArrayPush,
			false, StateMutability::Pure
		));
		members.emplace_back("push", TypeProvider::function(
			TypePointers{baseType()},
			TypePointers{},
			strings{string()},
			strings{},
			isByteArray() ? FunctionType::Kind::ByteArrayPush : FunctionType::Kind::ArrayPush,
			false, StateMutability::Pure
		));
		members.emplace_back("pop", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::ArrayPop,
			false, StateMutability::Pure
		));
	} else {
		members.emplace_back("substr", TypeProvider::function(
			TypePointers{TypeProvider::uint256()},
			TypePointers{TypeProvider::stringMemory()},
			strings{string("from")},
			strings{string("substr")},
			FunctionType::Kind::StringSubstr,
			false,
			StateMutability::Pure
		));
		members.emplace_back("substr", TypeProvider::function(
			TypePointers{TypeProvider::uint256(), TypeProvider::uint256()},
			TypePointers{TypeProvider::stringMemory()},
			strings{string("from"), "to"},
			strings{string("substr")},
			FunctionType::Kind::StringSubstr,
			false,
			StateMutability::Pure
		));
		members.emplace_back("byteLength", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(32)},
			strings{},
			strings{string("byteLength")},
			FunctionType::Kind::StringMethod,
			false, StateMutability::Pure
		));
		for (const std::string name : {"find", "findLast"}) {
			members.emplace_back(name, TypeProvider::function(
				TypePointers{TypeProvider::fixedBytes(1)},
				TypePointers{TypeProvider::optional(TypeProvider::uint(32))},
				strings{string("symbol")},
				strings{string("pos")},
				FunctionType::Kind::StringMethod,
				false, StateMutability::Pure
			));
		}
		members.emplace_back("find", TypeProvider::function(
			TypePointers{TypeProvider::stringMemory()},
			TypePointers{TypeProvider::optional(TypeProvider::uint(32))},
			strings{string("substr")},
			strings{string("pos")},
			FunctionType::Kind::StringMethod,
			false, StateMutability::Pure
		));
	}
	return members;
}

static void appendMapMethods(MemberList::MemberMap& members, Type const* keyType, Type const* valueType, Type const* realKeyType) {
	members.emplace_back("at", TypeProvider::function(
		TypePointers{keyType},
		TypePointers{valueType},
		{{}},
		{{}},
		FunctionType::Kind::MappingAt,
		false, StateMutability::Pure
	));


	for (const std::string name : {"min", "max"}) {
		members.emplace_back(name, TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MappingGetMinMax,
				false, StateMutability::Pure
		));
	}
	for (const std::string name : {"delMin", "delMax"}) {
		members.emplace_back(name, TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MappingDelMinOrMax,
				false, StateMutability::Pure
		));
	}
	for (const std::string name : {"next", "prev", "nextOrEq", "prevOrEq"}) {
		members.emplace_back(name, TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MappingGetNextKey,
				true, StateMutability::Pure
		));
	}
	members.emplace_back("keys", TypeProvider::function(
			{},
			{TypeProvider::array(realKeyType)},
			{},
			{{}},
			FunctionType::Kind::MappingKeys,
			false, StateMutability::Pure
	));
	members.emplace_back("values", TypeProvider::function(
			{},
			{TypeProvider::array(valueType)},
			{},
			{{}},
			FunctionType::Kind::MappingValues,
			false, StateMutability::Pure
	));
	members.emplace_back("fetch", TypeProvider::function(
			TypePointers{keyType},
			TypePointers{TypeProvider::optional(valueType)},
			strings{string{}},
			strings{string{}},
			FunctionType::Kind::MappingFetch,
			false, StateMutability::Pure
	));
	members.emplace_back("exists", TypeProvider::function(
			TypePointers{keyType},
			TypePointers{TypeProvider::boolean()},
			strings{string()},
			strings{string()},
			FunctionType::Kind::MappingExists,
			false, StateMutability::Pure
	));
	members.emplace_back("empty", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::boolean()},
			strings{},
			strings{string()},
			FunctionType::Kind::MappingEmpty,
			false, StateMutability::Pure
	));
	for (const std::string name : {"replace", "add"}) {
		members.emplace_back(name, TypeProvider::function(
				TypePointers{keyType, valueType},
				TypePointers{TypeProvider::boolean()},
				strings{string(), string()},
				strings{string()},
				FunctionType::Kind::MappingReplaceOrAdd,
				false, StateMutability::Pure
		));
	}
	for (const std::string name : {"getSet", "getAdd", "getReplace"}) {
		members.emplace_back(name, TypeProvider::function(
				TypePointers{keyType, valueType},
				TypePointers{TypeProvider::optional(valueType)},
				strings{string(), string()},
				strings{string()},
				FunctionType::Kind::MappingGetSet,
				false, StateMutability::Pure
		));
	}
}

MemberList::MemberMap MappingType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap members;
	appendMapMethods(members, keyType(), valueType(), realKeyType());
	return members;
}

TypePointer ArrayType::encodingType() const
{
	return TypeProvider::withLocation(this, true);
}

TypePointer ArrayType::decodingType() const
{
	return this;
}

u256 ArrayType::memoryDataSize() const
{
	solAssert(!isDynamicallySized(), "");
	solAssert(!isByteArray(), "");
	bigint size = bigint(m_length) * m_baseType->memoryHeadSize();
	solAssert(size <= numeric_limits<u256>::max(), "Array size does not fit u256.");
	return u256(size);
}

std::unique_ptr<ReferenceType> ArrayType::copyForLocation(bool _isPointer) const
{
	auto copy = make_unique<ArrayType>();
	copy->m_isPointer = _isPointer;
	copy->m_arrayKind = m_arrayKind;
	copy->m_baseType = copy->copyForLocationIfReference(m_baseType);
	copy->m_hasDynamicLength = m_hasDynamicLength;
	copy->m_length = m_length;
	return copy;
}

BoolResult ArraySliceType::isImplicitlyConvertibleTo(Type const& _other) const
{
	if (Type::isImplicitlyConvertibleTo(_other)) {
		return true;
	}

	if (m_arrayType.isDynamicallySized() && m_arrayType == _other)
		return true;
	return (*this) == _other;
}

string ArraySliceType::richIdentifier() const
{
	return m_arrayType.richIdentifier() + "_slice";
}

bool ArraySliceType::operator==(Type const& _other) const
{
	if (auto const* other = dynamic_cast<ArraySliceType const*>(&_other))
		return m_arrayType == other->m_arrayType;
	return false;
}

string ArraySliceType::toString(bool _short) const
{
	return m_arrayType.toString(_short) + " slice";
}

string ContractType::richIdentifier() const
{
	return (m_super ? "t_super" : "t_contract") + parenthesizeUserIdentifier(m_contract.name()) + to_string(m_contract.id());
}

bool ContractType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	ContractType const& other = dynamic_cast<ContractType const&>(_other);
	return other.m_contract == m_contract && other.m_super == m_super;
}

string ContractType::toString(bool) const
{
	return
		string(m_contract.isLibrary() ? "library " : "contract ") +
		string(m_super ? "super " : "") +
		m_contract.name();
}

string ContractType::canonicalName() const
{
	return m_contract.annotation().canonicalName;
}

MemberList::MemberMap ContractType::nativeMembers(ContractDefinition const* _contract) const
{
	MemberList::MemberMap members;
	solAssert(_contract, "");
	if (m_super)
	{
		// add the most derived of all functions which are visible in derived contracts
		auto bases = m_contract.annotation().linearizedBaseContracts;
		solAssert(bases.size() >= 1, "linearizedBaseContracts should at least contain the most derived contract.");
		// `sliced(1, ...)` ignores the most derived contract, which should not be searchable from `super`.
		for (ContractDefinition const* base: bases | boost::adaptors::sliced(1, bases.size()))
			for (FunctionDefinition const* function: base->definedFunctions())
			{
				if (!function->isVisibleInDerivedContracts() || !function->isImplemented())
					continue;

				auto functionType = TypeProvider::function(*function, FunctionType::Kind::Internal);
				bool functionWithEqualArgumentsFound = false;
				for (auto const& member: members)
				{
					if (member.name != function->name())
						continue;
					auto memberType = dynamic_cast<FunctionType const*>(member.type);
					solAssert(!!memberType, "Override changes type.");
					if (!memberType->hasEqualParameterTypes(*functionType))
						continue;
					functionWithEqualArgumentsFound = true;
					break;
				}
				if (!functionWithEqualArgumentsFound)
					members.emplace_back(function->name(), functionType, function);
			}
	}
	else if (!m_contract.isLibrary())
	{
		for (auto const& it: m_contract.interfaceFunctions())
			members.emplace_back(
				it.second->declaration().name(),
				it.second->asCallableFunction(m_contract.isLibrary()),
				&it.second->declaration()
			);
	}
	return members;
}

FunctionType const* ContractType::newExpressionType() const
{
	if (!m_constructorType)
		m_constructorType = FunctionType::newExpressionType(m_contract);
	return m_constructorType;
}

vector<tuple<VariableDeclaration const*, u256, unsigned>> ContractType::stateVariables() const
{
	vector<VariableDeclaration const*> variables;
	for (ContractDefinition const* contract: boost::adaptors::reverse(m_contract.annotation().linearizedBaseContracts))
		for (VariableDeclaration const* variable: contract->stateVariables())
			if (!variable->isConstant())
				variables.push_back(variable);
	TypePointers types;
	for (auto variable: variables)
		types.push_back(variable->annotation().type);
	StorageOffsets offsets;
	offsets.computeOffsets(types);

	vector<tuple<VariableDeclaration const*, u256, unsigned>> variablesAndOffsets;
	for (size_t index = 0; index < variables.size(); ++index)
		if (auto const* offset = offsets.offset(index))
			variablesAndOffsets.emplace_back(variables[index], offset->first, offset->second);
	return variablesAndOffsets;
}

void StructType::clearCache() const
{
	Type::clearCache();

	m_interfaceType.reset();
	m_interfaceType_library.reset();
}

Type const* StructType::encodingType() const
{
	return this;
}

BoolResult StructType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() != category())
		return false;
	auto& convertTo = dynamic_cast<StructType const&>(_convertTo);
	return this->m_struct == convertTo.m_struct;
}

string StructType::richIdentifier() const
{
	return "t_struct" + parenthesizeUserIdentifier(m_struct.name()) + to_string(m_struct.id()) + identifierLocationSuffix();
}

bool StructType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	StructType const& other = dynamic_cast<StructType const&>(_other);
	return other.m_struct == m_struct;
}


unsigned StructType::calldataEncodedSize(bool) const
{
	solAssert(!isDynamicallyEncoded(), "");

	unsigned size = 0;
	for (auto const& member: members(nullptr))
	{
		solAssert(member.type->canLiveOutsideStorage(), "");
		// Struct members are always padded.
		size += member.type->calldataEncodedSize();
	}
	return size;
}


unsigned StructType::calldataEncodedTailSize() const
{
	solAssert(isDynamicallyEncoded(), "");

	unsigned size = 0;
	for (auto const& member: members(nullptr))
	{
		solAssert(member.type->canLiveOutsideStorage(), "");
		// Struct members are always padded.
		size += member.type->calldataHeadSize();
	}
	return size;
}

unsigned StructType::calldataOffsetOfMember(std::string const& _member) const
{
	unsigned offset = 0;
	for (auto const& member: members(nullptr))
	{
		solAssert(member.type->canLiveOutsideStorage(), "");
		if (member.name == _member)
			return offset;
		// Struct members are always padded.
		offset += member.type->calldataHeadSize();
	}
	solAssert(false, "Struct member not found.");
}

bool StructType::isDynamicallyEncoded() const
{
	if (recursive())
		return true;
	solAssert(interfaceType(false).get(), "");
	for (auto t: memoryMemberTypes())
	{
		solAssert(t, "Parameter should have external type.");
		t = t->interfaceType(false);
		if (t->isDynamicallyEncoded())
			return true;
	}
	return false;
}

u256 StructType::memoryDataSize() const
{
	u256 size;
	for (auto const& t: memoryMemberTypes())
		size += t->memoryHeadSize();
	return size;
}

u256 StructType::storageSize() const
{
	return max<u256>(1, members(nullptr).storageSize());
}

string StructType::toString(bool ) const
{
	string ret = "struct " + m_struct.annotation().canonicalName;;
	return ret;
}

MemberList::MemberMap StructType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap members;
	TypePointers types;
	for (ASTPointer<VariableDeclaration> const& variable: m_struct.members())
	{
		TypePointer type = variable->annotation().type;
		types.push_back(type);
		solAssert(type, "");
		members.emplace_back(
			variable->name(),
			copyForLocationIfReference(type),
			variable.get()
		);
	}
	members.emplace_back("unpack", TypeProvider::function(
			TypePointers{},
			types,
			strings{},
			strings{types.size()},
			FunctionType::Kind::StructUnpack,
			false, StateMutability::Pure
	));
	return members;
}

std::unique_ptr<ReferenceType> StructType::copyForLocation(bool _isPointer) const
{
	auto copy = make_unique<StructType>(m_struct);
	copy->m_isPointer = _isPointer;
	return copy;
}

string StructType::signatureInExternalFunction(bool _structsByName) const
{
	if (_structsByName)
		return canonicalName();
	else
	{
		TypePointers memberTypes = memoryMemberTypes();
		auto memberTypeStrings = memberTypes | boost::adaptors::transformed([&](TypePointer _t) -> string
		{
			solAssert(_t, "Parameter should have external type.");
			auto t = _t->interfaceType(_structsByName);
			solAssert(t.get(), "");
			return t.get()->signatureInExternalFunction(_structsByName);
		});
		return "(" + boost::algorithm::join(memberTypeStrings, ",") + ")";
	}
}

string StructType::canonicalName() const
{
	return m_struct.annotation().canonicalName;
}

FunctionTypePointer StructType::constructorType() const
{
	TypePointers paramTypes;
	strings paramNames;
	for (auto const& member: members(nullptr))
	{
		if (!member.type->canLiveOutsideStorage())
			continue;
		paramNames.push_back(member.name);
		paramTypes.push_back(TypeProvider::withLocationIfReference(member.type));
	}
	return TypeProvider::function(
		paramTypes,
		TypePointers{TypeProvider::withLocation(this, false)},
		paramNames,
		strings(1, ""),
		FunctionType::Kind::Internal
	);
}

pair<u256, unsigned> const& StructType::storageOffsetsOfMember(string const& _name) const
{
	auto const* offsets = members(nullptr).memberStorageOffset(_name);
	solAssert(offsets, "Storage offset of non-existing member requested.");
	return *offsets;
}

u256 StructType::memoryOffsetOfMember(string const& _name) const
{
	u256 offset;
	for (auto const& member: members(nullptr))
		if (member.name == _name)
			return offset;
		else
			offset += member.type->memoryHeadSize();
	solAssert(false, "Member not found in struct.");
	return 0;
}

TypePointers StructType::memoryMemberTypes() const
{
	TypePointers types;
	for (ASTPointer<VariableDeclaration> const& variable: m_struct.members())
		if (variable->annotation().type->canLiveOutsideStorage())
			types.push_back(variable->annotation().type);
	return types;
}

set<string> StructType::membersMissingInMemory() const
{
	set<string> missing;
	for (ASTPointer<VariableDeclaration> const& variable: m_struct.members())
		if (!variable->annotation().type->canLiveOutsideStorage())
			missing.insert(variable->name());
	return missing;
}

TypePointer EnumType::encodingType() const
{
	return TypeProvider::uint(8 * storageBytes());
}

TypeResult EnumType::unaryOperatorResult(Token _operator) const
{
	return _operator == Token::Delete ? TypeProvider::emptyTuple() : nullptr;
}

string EnumType::richIdentifier() const
{
	return "t_enum" + parenthesizeUserIdentifier(m_enum.name()) + to_string(m_enum.id());
}

bool EnumType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	EnumType const& other = dynamic_cast<EnumType const&>(_other);
	return other.m_enum == m_enum;
}

unsigned EnumType::storageBytes() const
{
	size_t elements = numberOfMembers();
	if (elements <= 1)
		return 1;
	else
		return util::bytesRequired(elements - 1);
}

string EnumType::toString(bool) const
{
	return string("enum ") + m_enum.annotation().canonicalName;
}

string EnumType::canonicalName() const
{
	return m_enum.annotation().canonicalName;
}

size_t EnumType::numberOfMembers() const
{
	return m_enum.members().size();
}

BoolResult EnumType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	return _convertTo == *this || _convertTo.category() == Category::Integer;
}

unsigned EnumType::memberValue(ASTString const& _member) const
{
	unsigned index = 0;
	for (ASTPointer<EnumValue> const& decl: m_enum.members())
	{
		if (decl->name() == _member)
			return index;
		++index;
	}
	solAssert(false, "Requested unknown enum value " + _member);
}

BoolResult TupleType::isImplicitlyConvertibleTo(Type const& _other) const
{
	if (Type::isImplicitlyConvertibleTo(_other)) {
		return true;
	}

	if (auto tupleType = dynamic_cast<TupleType const*>(&_other))
	{
		TypePointers const& targets = tupleType->components();
		if (targets.empty())
			return components().empty();
		if (components().size() != targets.size())
			return false;
		for (size_t i = 0; i < targets.size(); ++i)
			if (!components()[i] && targets[i])
				return false;
			else if (components()[i] && targets[i] && !components()[i]->isImplicitlyConvertibleTo(*targets[i]))
				return false;
		return true;
	}
	else
		return false;
}

string TupleType::richIdentifier() const
{
	return "t_tuple" + identifierList(components());
}

bool TupleType::operator==(Type const& _other) const
{
	if (auto tupleOther = dynamic_cast<TupleType const*>(&_other)) {
		if (components().size() == tupleOther->components().size()) {
			bool ok = true;
			for (size_t i = 0; i < components().size(); ++i) {
				if (tupleOther->components().at(i) != nullptr)
					ok &= *components().at(i) == *tupleOther->components().at(i);
			}
			return ok;
		}
	}
	return false;
}

string TupleType::toString(bool _short) const
{
	if (components().empty())
		return "tuple()";
	string str = "tuple(";
	for (auto const& t: components())
		str += (t ? t->toString(_short) : "") + ",";
	str.pop_back();
	return str + ")";
}

u256 TupleType::storageSize() const
{
	solAssert(false, "Storage size of non-storable tuple type requested.");
}

unsigned TupleType::sizeOnStack() const
{
	unsigned size = 0;
	for (auto const& t: components())
		size += t ? t->sizeOnStack() : 0;
	return size;
}

TypePointer TupleType::mobileType() const
{
	TypePointers mobiles;
	for (auto const& c: components())
	{
		if (c)
		{
			auto mt = c->mobileType();
			if (!mt)
				return nullptr;
			mobiles.push_back(mt);
		}
		else
			mobiles.push_back(nullptr);
	}
	return TypeProvider::tuple(move(mobiles));
}

TypePointer TupleType::closestTemporaryType(Type const* _targetType) const
{
	solAssert(!!_targetType, "");
	TypePointers const& targetComponents = dynamic_cast<TupleType const&>(*_targetType).components();
	solAssert(components().size() == targetComponents.size(), "");
	TypePointers tempComponents(targetComponents.size());
	for (size_t i = 0; i < targetComponents.size(); ++i)
	{
		if (components()[i] && targetComponents[i])
		{
			tempComponents[i] = components()[i]->closestTemporaryType(targetComponents[i]);
			solAssert(tempComponents[i], "");
		}
	}
	return TypeProvider::tuple(move(tempComponents));
}

FunctionType::FunctionType(FunctionDefinition const& _function, Kind _kind):
	m_kind(_kind),
	m_stateMutability(_function.stateMutability()),
	m_declaration(&_function)
{
	solAssert(
		_kind == Kind::Internal || _kind == Kind::External || _kind == Kind::Declaration,
		"Only internal or external function types or function declaration types can be created from function definitions."
	);
	if (_kind == Kind::Internal && m_stateMutability == StateMutability::NonPayable)
		m_stateMutability = StateMutability::NonPayable;

	for (ASTPointer<VariableDeclaration> const& var: _function.parameters())
	{
		m_parameterNames.push_back(var->name());
		m_parameterTypes.push_back(var->annotation().type);
	}
	for (ASTPointer<VariableDeclaration> const& var: _function.returnParameters())
	{
		m_returnParameterNames.push_back(var->name());
		m_returnParameterTypes.push_back(var->annotation().type);
	}

	solAssert(
		m_parameterNames.size() == m_parameterTypes.size(),
		"Parameter names list must match parameter types list!"
	);

	solAssert(
		m_returnParameterNames.size() == m_returnParameterTypes.size(),
		"Return parameter names list must match return parameter types list!"
	);
}

FunctionType::FunctionType(VariableDeclaration const& _varDecl):
	m_kind(Kind::External),
	m_stateMutability(StateMutability::View),
	m_declaration(&_varDecl)
{
	auto returnType = _varDecl.annotation().type;

	while (true)
	{
		if (auto mappingType = dynamic_cast<MappingType const*>(returnType))
		{
			m_parameterTypes.push_back(mappingType->keyType());
			m_parameterNames.emplace_back("");
			returnType = mappingType->valueType();
		}
		else if (auto arrayType = dynamic_cast<ArrayType const*>(returnType))
		{
			if (arrayType->isByteArray())
				// Return byte arrays as whole.
				break;
			returnType = arrayType->baseType();
			m_parameterNames.emplace_back("");
			m_parameterTypes.push_back(TypeProvider::uint256());
		}
		else
			break;
	}

	if (auto structType = dynamic_cast<StructType const*>(returnType))
	{
		for (auto const& member: structType->members(nullptr))
		{
			solAssert(member.type, "");
			if (member.type->category() != Category::Mapping)
			{
				if (auto arrayType = dynamic_cast<ArrayType const*>(member.type))
					if (!arrayType->isByteArray())
						continue;
				m_returnParameterTypes.push_back(TypeProvider::withLocationIfReference(
					member.type
				));
				m_returnParameterNames.push_back(member.name);
			}
		}
	}
	else
	{
		m_returnParameterTypes.push_back(TypeProvider::withLocationIfReference(
			returnType
		));
		m_returnParameterNames.emplace_back("");
	}

	solAssert(
			m_parameterNames.size() == m_parameterTypes.size(),
			"Parameter names list must match parameter types list!"
			);
	solAssert(
			m_returnParameterNames.size() == m_returnParameterTypes.size(),
			"Return parameter names list must match return parameter types list!"
			);
}

FunctionType::FunctionType(EventDefinition const& _event):
	m_kind(Kind::Event),
	m_stateMutability(StateMutability::Pure),
	m_declaration(&_event)
{
	for (ASTPointer<VariableDeclaration> const& var: _event.parameters())
	{
		m_parameterNames.push_back(var->name());
		m_parameterTypes.push_back(var->annotation().type);
	}

	solAssert(
			m_parameterNames.size() == m_parameterTypes.size(),
			"Parameter names list must match parameter types list!"
			);
	solAssert(
			m_returnParameterNames.size() == m_returnParameterTypes.size(),
			"Return parameter names list must match return parameter types list!"
			);
}

FunctionType::FunctionType(FunctionTypeName const& _typeName):
	m_parameterNames(_typeName.parameterTypes().size(), ""),
	m_returnParameterNames(_typeName.returnParameterTypes().size(), ""),
	m_kind(_typeName.visibility() == Visibility::External ? Kind::External : Kind::Internal),
	m_stateMutability(_typeName.stateMutability())
{
	for (auto const& t: _typeName.parameterTypes())
	{
		solAssert(t->annotation().type, "Type not set for parameter.");
		if (m_kind == Kind::External)
			solAssert(
				t->annotation().type->interfaceType(false).get(),
				"Internal type used as parameter for external function."
			);
		m_parameterTypes.push_back(t->annotation().type);
	}
	for (auto const& t: _typeName.returnParameterTypes())
	{
		solAssert(t->annotation().type, "Type not set for return parameter.");
		if (m_kind == Kind::External)
			solAssert(
				t->annotation().type->interfaceType(false).get(),
				"Internal type used as return parameter for external function."
			);
		m_returnParameterTypes.push_back(t->annotation().type);
	}

	solAssert(
			m_parameterNames.size() == m_parameterTypes.size(),
			"Parameter names list must match parameter types list!"
			);
	solAssert(
			m_returnParameterNames.size() == m_returnParameterTypes.size(),
			"Return parameter names list must match return parameter types list!"
			);
}

FunctionTypePointer FunctionType::newExpressionType(ContractDefinition const& _contract)
{
	FunctionDefinition const* constructor = _contract.constructor();
	TypePointers parameters;
	strings parameterNames;
	StateMutability stateMutability = StateMutability::NonPayable;

	solAssert(!_contract.isInterface(), "");

	if (constructor)
	{
		for (ASTPointer<VariableDeclaration> const& var: constructor->parameters())
		{
			parameterNames.push_back(var->name());
			parameters.push_back(var->annotation().type);
		}
	}

	return TypeProvider::function(
		parameters,
		TypePointers{TypeProvider::contract(_contract)},
		parameterNames,
		strings{""},
		Kind::Creation,
		false,
		stateMutability
	);
}

vector<string> FunctionType::parameterNames() const
{
	if (!bound())
		return m_parameterNames;
	return vector<string>(m_parameterNames.cbegin() + 1, m_parameterNames.cend());
}

TypePointers FunctionType::returnParameterTypesWithoutDynamicTypes() const
{
	TypePointers returnParameterTypes = m_returnParameterTypes;

	if (
		m_kind == Kind::External ||
		m_kind == Kind::DelegateCall ||
		m_kind == Kind::BareCall ||
		m_kind == Kind::BareCallCode ||
		m_kind == Kind::BareDelegateCall ||
		m_kind == Kind::BareStaticCall
	)
		for (auto& param: returnParameterTypes)
			if (param->isDynamicallySized())
				param = TypeProvider::inaccessibleDynamic();

	return returnParameterTypes;
}

TypePointers FunctionType::parameterTypes() const
{
	if (!bound())
		return m_parameterTypes;
	return TypePointers(m_parameterTypes.cbegin() + 1, m_parameterTypes.cend());
}

string FunctionType::richIdentifier() const
{
	string id = "t_function_";
	switch (m_kind)
	{
	case Kind::StructUnpack: id += "structunpack"; break;

	case Kind::OptionalGet: id += "optionalmethod"; break;
	case Kind::OptionalHasValue: id += "optionalhasvalue"; break;
	case Kind::OptionalReset: id += "optionalreset"; break;
	case Kind::OptionalSet: id += "optionalmethod"; break;

	case Kind::StringMethod: id += "stringmethod"; break;
	case Kind::StringSubstr: id += "stringsubstr"; break;

	case Kind::DecodeFunctionParams: id += "tvmslicedecodefunctionparams"; break;
	case Kind::TVMSliceCompare: id += "tvmslicecompare"; break;
	case Kind::TVMSliceDataSize: id += "tvmslicedatasize"; break;
	case Kind::TVMSliceDecode: id += "tvmslicedecode"; break;
	case Kind::TVMSliceDecodeQ: id += "tvmslicedecodeq"; break;
	case Kind::TVMSliceDecodeStateVars: id += "tvmslicedecodestatevars"; break;
	case Kind::TVMSliceEmpty: id += "tvmsliceempty"; break;
	case Kind::TVMSliceHas: id += "tvmslicehasxxx"; break;
	case Kind::TVMSliceSize: id += "tvmslicesize"; break;
	case Kind::TVMSliceSkip: id += "tvmsliceskip"; break;

	case Kind::TVMCellDepth: id += "tvmcelldepth"; break;
	case Kind::TVMCellToSlice: id += "tvmcelltoslice"; break;
	case Kind::TVMDataSize: id += "tvmdatasize"; break;
	case Kind::TVMDataSizeQ: id += "tvmdatasizeq"; break;

	case Kind::Format: id += "format"; break;
	case Kind::Stoi: id += "stoi"; break;
	case Kind::LogTVM: id += "logtvm"; break;
	case Kind::TVMAccept: id += "tvmaccept"; break;
	case Kind::TVMBuildExtMsg: id += "tvmbuildextmsg"; break;
	case Kind::TVMBuildIntMsg: id += "tvmbuildintmsg"; break;
	case Kind::TVMBuildStateInit: id += "tvmbuildstateinit"; break;
	case Kind::TVMBuildDataInit: id += "tvmbuilddatainit"; break;

	case Kind::TVMBuilderMethods: id += "tvmbuildermethods"; break;
	case Kind::TVMBuilderStore: id += "tvmbuilderstore"; break;

	case Kind::TVMTuplePush: id += "tvmtuplepush"; break;
	case Kind::TVMTuplePop: id += "tvmtuplepop"; break;
	case Kind::TVMTupleLength: id += "tvmtuplelength"; break;
	case Kind::TVMTupleEmpty: id += "tvmtupleempty"; break;

	case Kind::TVMChecksign: id += "tvmchecksign"; break;
	case Kind::TVMCode: id += "tvmcode"; break;
	case Kind::TVMCodeSalt: id += "tvmcodesalt"; break;
	case Kind::TVMCommit: id += "tvmcommit"; break;
	case Kind::TVMConfigParam: id += "tvmconfigparam"; break;
	case Kind::TVMDeploy: id += "tvmdeploy"; break;
	case Kind::TVMDump: id += "tvmxxxdump"; break;
	case Kind::TVMEncodeBody: id += "tvmencodebody"; break;
	case Kind::TVMExit1: id += "tvmexit1"; break;
	case Kind::TVMExit: id += "tvmexit"; break;
	case Kind::TVMFunctionId: id += "tvmfunctionid"; break;
	case Kind::TVMHash: id += "tvmhash"; break;
	case Kind::TVMInitCodeHash: id += "tvminitcodehash"; break;
	case Kind::TVMLoadRef: id += "tvmloadref"; break;
	case Kind::TVMLoadSlice: id += "tvmloadslice"; break;
	case Kind::TVMPubkey: id += "tvmpubkey"; break;
	case Kind::TVMRawConfigParam: id += "tvmrawconfigparam"; break;
	case Kind::TVMReplayProtInterval: id += "tvmreplayprotinterval"; break;
	case Kind::TVMReplayProtTime: id += "tvmreplayprottime"; break;
	case Kind::TVMResetStorage: id += "tvmresetstorage"; break;
	case Kind::TVMSendMsg: id += "tvmsendmsg"; break;
	case Kind::TVMSetcode: id += "tvmsetcode"; break;
	case Kind::TVMSetCodeSalt: id += "tvmsetcodesalt"; break;
	case Kind::TVMSetGasLimit: id += "tvmsetgaslimit"; break;
	case Kind::TVMSetPubkey: id += "tvmsetpubkey"; break;
	case Kind::TVMSetReplayProtTime: id += "tvmsetreplayprottime"; break;


	case Kind::AddressTransfer: id += "tvmtransfer"; break;

	case Kind::TXtimestamp: id += "txtimestamp"; break;

	case Kind::ExtraCurrencyCollectionMethods: id += "extracurrencycollectionmethods"; break;
	case Kind::MsgPubkey: id += "msgpubkey"; break;
	case Kind::AddressIsZero: id += "addressiszero"; break;
	case Kind::AddressUnpack: id += "addressunpack"; break;
	case Kind::AddressType: id += "addresstype"; break;
	case Kind::AddressIsStdAddrWithoutAnyCast: id += "addressisstdaddrwithoutanycast"; break;
	case Kind::AddressMakeAddrExtern: id += "addressmakeaddrextern"; break;
	case Kind::AddressMakeAddrNone: id += "addressmakeaddrnone"; break;
	case Kind::AddressMakeAddrStd: id += "addressmakeaddrstd"; break;

	case Kind::MathAbs: id += "mathabs"; break;
	case Kind::MathDivC: id += "divc"; break;
	case Kind::MathDivR: id += "divr"; break;
	case Kind::MathMin: id += "mathmin"; break;
	case Kind::MathMax: id += "mathmax"; break;
	case Kind::MathMinMax: id += "mathminmax"; break;
	case Kind::MathModpow2: id += "mathmodpow2"; break;
	case Kind::MathMulDiv: id += "mathmuldiv"; break;
	case Kind::MathMulDivMod: id += "mathmuldivmod"; break;
	case Kind::MathDivMod: id += "mathdivmod"; break;
	case Kind::MathSign: id += "mathsign"; break;

	case Kind::MappingAt: id += "mappingat"; break;
	case Kind::MappingDelMinOrMax: id += "mapdelmin"; break;
	case Kind::MappingEmpty: id += "mapempty"; break;
	case Kind::MappingExists: id += "mapexists"; break;
	case Kind::MappingFetch: id += "mapfetch"; break;
	case Kind::MappingGetMinMax: id += "mapgetminmax"; break;
	case Kind::MappingGetNextKey: id += "mapgetnext"; break;
	case Kind::MappingGetPrevKey: id += "mapgetprev"; break;
	case Kind::MappingGetSet: id += "mappingsetget"; break;
	case Kind::MappingKeys: id += "mappingkeys"; break;
	case Kind::MappingReplaceOrAdd: id += "mappingreplaceoradd"; break;
	case Kind::MappingValues: id += "mappingvalues"; break;

	case Kind::Declaration: id += "declaration"; break;
	case Kind::Internal: id += "internal"; break;
	case Kind::External: id += "external"; break;
	case Kind::DelegateCall: id += "delegatecall"; break;
	case Kind::BareCall: id += "barecall"; break;
	case Kind::BareCallCode: id += "barecallcode"; break;
	case Kind::BareDelegateCall: id += "baredelegatecall"; break;
	case Kind::BareStaticCall: id += "barestaticcall"; break;
	case Kind::Creation: id += "creation"; break;
	case Kind::Send: id += "send"; break;
	case Kind::Transfer: id += "transfer"; break;
	case Kind::KECCAK256: id += "keccak256"; break;
	case Kind::Selfdestruct: id += "selfdestruct"; break;
	case Kind::Revert: id += "revert"; break;
	case Kind::ECRecover: id += "ecrecover"; break;
	case Kind::SHA256: id += "sha256"; break;
	case Kind::RIPEMD160: id += "ripemd160"; break;
	case Kind::Log0: id += "log0"; break;
	case Kind::Log1: id += "log1"; break;
	case Kind::Log2: id += "log2"; break;
	case Kind::Log3: id += "log3"; break;
	case Kind::Log4: id += "log4"; break;
	case Kind::GasLeft: id += "gasleft"; break;
	case Kind::Event: id += "event"; break;
	case Kind::SetGas: id += "setgas"; break;
	case Kind::SetValue: id += "setvalue"; break;
	case Kind::SetFlag: id += "setflag"; break;
	case Kind::BlockHash: id += "blockhash"; break;
	case Kind::AddMod: id += "addmod"; break;
	case Kind::MulMod: id += "mulmod"; break;

	case Kind::BitSize: id += "bitsize"; break;
	case Kind::GasToValue: id += "gastovalue"; break;
	case Kind::UBitSize: id += "ubitsize"; break;
	case Kind::ValueToGas: id += "valuetogas"; break;

	case Kind::ArrayEmpty: id += "arrayempty"; break;
	case Kind::ArrayPush: id += "arraypush"; break;
	case Kind::ArrayPop: id += "arraypop"; break;
	case Kind::ByteArrayPush: id += "bytearraypush"; break;
	case Kind::ByteToSlice: id += "bytetoslice"; break;

	case Kind::ObjectCreation: id += "objectcreation"; break;
	case Kind::Assert: id += "assert"; break;
	case Kind::Require: id += "require"; break;
	case Kind::ABIEncode: id += "abiencode"; break;
	case Kind::ABIEncodePacked: id += "abiencodepacked"; break;
	case Kind::ABIEncodeWithSelector: id += "abiencodewithselector"; break;
	case Kind::ABIEncodeWithSignature: id += "abiencodewithsignature"; break;
	case Kind::ABIDecode: id += "abidecode"; break;
	case Kind::MetaType: id += "metatype"; break;

	case Kind::RndGetSeed: id += "rndgetseed"; break;
	case Kind::RndNext: id += "rndnext"; break;
	case Kind::RndSetSeed: id += "rndsetseed"; break;
	case Kind::RndShuffle: id += "rndshuffle"; break;

	case Kind::GoshApplyPatch: id += "goshapplypatch"; break;
	case Kind::GoshApplyPatchQ: id += "goshapplypatchq"; break;
	case Kind::GoshApplyZipPatch: id += "goshapplyzippatch"; break;
	case Kind::GoshApplyZipPatchQ: id += "goshapplyzippatchq"; break;
	case Kind::GoshDiff: id += "goshdiff"; break;
	case Kind::GoshUnzip: id += "goshunzip"; break;
	case Kind::GoshZip: id += "goshzip"; break;
	case Kind::GoshZipDiff: id += "goshzipdiff"; break;
	}
	id += "_" + stateMutabilityToString(m_stateMutability);
	id += identifierList(m_parameterTypes) + "returns" + identifierList(m_returnParameterTypes);

	if (bound())
		id += "bound_to" + identifierList(selfType());
	return id;
}

bool FunctionType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	FunctionType const& other = dynamic_cast<FunctionType const&>(_other);
	if (!equalExcludingStateMutability(other))
		return false;
	if (m_stateMutability != other.stateMutability())
		return false;
	return true;
}

BoolResult FunctionType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (_convertTo.category() == category())
	{
		auto const& convertToType = dynamic_cast<FunctionType const&>(_convertTo);
		return (m_kind == FunctionType::Kind::Declaration) == (convertToType.kind() == FunctionType::Kind::Declaration);
	}
	return false;
}

BoolResult FunctionType::isImplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (Type::isImplicitlyConvertibleTo(_convertTo)) {
		return true;
	}

	if (_convertTo.category() != category())
		return false;

	FunctionType const& convertTo = dynamic_cast<FunctionType const&>(_convertTo);

	if (!equalExcludingStateMutability(convertTo))
		return false;

	// e.g. pure should be convertible to view, but not the other way around.
	if (m_stateMutability > convertTo.stateMutability())
		return false;

	return true;
}

TypeResult FunctionType::unaryOperatorResult(Token _operator) const
{
	if (_operator == Token::Delete)
		return TypeResult(TypeProvider::emptyTuple());
	return nullptr;
}

TypeResult FunctionType::binaryOperatorResult(Token _operator, Type const* _other) const
{
	if (_other->category() != category() || !(_operator == Token::Equal || _operator == Token::NotEqual))
		return nullptr;
	FunctionType const& other = dynamic_cast<FunctionType const&>(*_other);
	if (kind() == Kind::Internal && other.kind() == Kind::Internal && sizeOnStack() == 1 && other.sizeOnStack() == 1)
		return commonType(this, _other);
	return nullptr;
}

string FunctionType::canonicalName() const
{
	return "function";
}

string FunctionType::toString(bool _short) const
{
	string name = "function ";
	if (m_kind == Kind::Declaration)
	{
		auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(m_declaration);
		solAssert(functionDefinition, "");
		auto const* contract = dynamic_cast<ContractDefinition const*>(functionDefinition->scope());
		solAssert(contract, "");
		name += contract->annotation().canonicalName;
		name += '.';
		name += functionDefinition->name();
	}
	name += '(';
	for (auto it = m_parameterTypes.begin(); it != m_parameterTypes.end(); ++it)
		name += (*it)->toString(_short) + (it + 1 == m_parameterTypes.end() ? "" : ",");
	name += ")";
	if (m_stateMutability != StateMutability::NonPayable)
		name += " " + stateMutabilityToString(m_stateMutability);
	if (m_kind == Kind::External)
		name += " external";
	if (!m_returnParameterTypes.empty())
	{
		name += " returns (";
		for (auto it = m_returnParameterTypes.begin(); it != m_returnParameterTypes.end(); ++it)
			name += (*it)->toString(_short) + (it + 1 == m_returnParameterTypes.end() ? "" : ",");
		name += ")";
	}
	return name;
}

unsigned FunctionType::calldataEncodedSize(bool _padded) const
{
	unsigned size = storageBytes();
	if (_padded)
		size = ((size + 31) / 32) * 32;
	return size;
}

u256 FunctionType::storageSize() const
{
	if (m_kind == Kind::External || m_kind == Kind::Internal)
		return 1;
	else
		solAssert(false, "Storage size of non-storable function type requested.");
}

bool FunctionType::leftAligned() const
{
	if (m_kind == Kind::External)
		return true;
	else
		solAssert(false, "Alignment property of non-exportable function type requested.");
}

unsigned FunctionType::storageBytes() const
{
	if (m_kind == Kind::External)
		return 20 + 4;
	else if (m_kind == Kind::Internal)
		return 8; // it should really not be possible to create larger programs
	else
		solAssert(false, "Storage size of non-storable function type requested.");
}

unsigned FunctionType::sizeOnStack() const
{
	Kind kind = m_kind;
	if (m_kind == Kind::SetGas || m_kind == Kind::SetValue)
	{
		solAssert(m_returnParameterTypes.size() == 1, "");
		kind = dynamic_cast<FunctionType const&>(*m_returnParameterTypes.front()).m_kind;
	}

	unsigned size = 0;

	switch (kind)
	{
	case Kind::External:
	case Kind::DelegateCall:
		size = 2;
		break;
	case Kind::BareCall:
	case Kind::BareCallCode:
	case Kind::BareDelegateCall:
	case Kind::BareStaticCall:
	case Kind::Internal:
	case Kind::ArrayPush:
	case Kind::ArrayPop:
	case Kind::ByteArrayPush:
	case Kind::Transfer:
	case Kind::Send:
		size = 1;
		break;
	default:
		break;
	}

	if (bound())
		size += m_parameterTypes.front()->sizeOnStack();
	return size;
}

FunctionTypePointer FunctionType::interfaceFunctionType() const
{
	// Note that m_declaration might also be a state variable!
	solAssert(m_declaration, "Declaration needed to determine interface function type.");
	bool isLibraryFunction = kind() != Kind::Event && dynamic_cast<ContractDefinition const&>(*m_declaration->scope()).isLibrary();

	util::Result<TypePointers> paramTypes =
		transformParametersToExternal(m_parameterTypes, isLibraryFunction);

	if (!paramTypes.message().empty())
		return FunctionTypePointer();

	util::Result<TypePointers> retParamTypes =
		transformParametersToExternal(m_returnParameterTypes, isLibraryFunction);

	if (!retParamTypes.message().empty())
		return FunctionTypePointer();

	auto variable = dynamic_cast<VariableDeclaration const*>(m_declaration);
	if (variable && retParamTypes.get().empty())
		return FunctionTypePointer();

	return TypeProvider::function(
		paramTypes,
		retParamTypes,
		m_parameterNames,
		m_returnParameterNames,
		m_kind,
		m_arbitraryParameters,
		m_stateMutability,
		m_declaration
	);
}

MemberList::MemberMap FunctionType::nativeMembers(ContractDefinition const* _scope) const
{
	switch (m_kind)
	{
	case Kind::Declaration:
		if (declaration().isPartOfExternalInterface())
			return {{"selector", TypeProvider::fixedBytes(4)}};
		else
			return MemberList::MemberMap();
	case Kind::Internal:
		if (
			auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(m_declaration);
			functionDefinition &&
			_scope &&
			functionDefinition->annotation().contract &&
			_scope != functionDefinition->annotation().contract &&
			functionDefinition->isPartOfExternalInterface()
		)
		{
			solAssert(_scope->derivesFrom(*functionDefinition->annotation().contract), "");
			return {{"selector", TypeProvider::fixedBytes(4)}};
		}
		else
			return MemberList::MemberMap();
	case Kind::External:
	case Kind::Creation:
	case Kind::BareCall:
	case Kind::BareCallCode:
	case Kind::BareDelegateCall:
	case Kind::BareStaticCall:
	{
		MemberList::MemberMap members;
		if (m_kind == Kind::External)
		{
			members.emplace_back("selector", TypeProvider::fixedBytes(4));
			members.emplace_back("address", TypeProvider::address());
		}
		return members;
	}
	case Kind::DelegateCall:
	{
		auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(m_declaration);
		solAssert(functionDefinition, "");
		solAssert(functionDefinition->visibility() != Visibility::Private, "");
		if (functionDefinition->visibility() != Visibility::Internal)
		{
			auto const* contract = dynamic_cast<ContractDefinition const*>(m_declaration->scope());
			solAssert(contract, "");
			solAssert(contract->isLibrary(), "");
			return {{"selector", TypeProvider::fixedBytes(4)}};
		}
		return {};
	}
	default:
		return MemberList::MemberMap();
	}
}

TypePointer FunctionType::encodingType() const
{
	// Only external functions can be encoded, internal functions cannot leave code boundaries.
	if (m_kind == Kind::External)
		return this;
	else
		return nullptr;
}

TypeResult FunctionType::interfaceType(bool /*_inLibrary*/) const
{
	return this;
}

bool FunctionType::canTakeArguments(
	FuncCallArguments const& _arguments,
	Type const* _selfType
) const
{
	solAssert(!bound() || _selfType, "");
	if (bound() && !_selfType->isImplicitlyConvertibleTo(*selfType()))
		return false;
	TypePointers paramTypes = parameterTypes();
	std::vector<std::string> const paramNames = parameterNames();

	if (takesArbitraryParameters())
		return true;
	else if (_arguments.numArguments() != paramTypes.size())
		return false;
	else if (!_arguments.hasNamedArguments())
		return equal(
			_arguments.types.cbegin(),
			_arguments.types.cend(),
			paramTypes.cbegin(),
			[](Type const* argumentType, Type const* parameterType)
			{
				return argumentType->isImplicitlyConvertibleTo(*parameterType);
			}
		);
	else if (paramNames.size() != _arguments.numNames())
		return false;
	else
	{
		solAssert(_arguments.numArguments() == _arguments.numNames(), "Expected equal sized type & name vectors");

		size_t matchedNames = 0;

		for (auto const& argName: _arguments.names)
			for (size_t i = 0; i < paramNames.size(); i++)
				if (*argName == paramNames[i])
				{
					matchedNames++;
					if (!_arguments.types[i]->isImplicitlyConvertibleTo(*paramTypes[i]))
						return false;
				}

		if (matchedNames == _arguments.numNames())
			return true;

		return false;
	}
}

bool FunctionType::hasEqualParameterTypes(FunctionType const& _other) const
{
	if (m_parameterTypes.size() != _other.m_parameterTypes.size())
		return false;
	return equal(
		m_parameterTypes.cbegin(),
		m_parameterTypes.cend(),
		_other.m_parameterTypes.cbegin(),
		[](Type const* _a, Type const* _b) -> bool { return *_a == *_b; }
	);
}

bool FunctionType::hasEqualReturnTypes(FunctionType const& _other) const
{
	if (m_returnParameterTypes.size() != _other.m_returnParameterTypes.size())
		return false;
	return equal(
		m_returnParameterTypes.cbegin(),
		m_returnParameterTypes.cend(),
		_other.m_returnParameterTypes.cbegin(),
		[](Type const* _a, Type const* _b) -> bool { return *_a == *_b; }
	);
}

bool FunctionType::equalExcludingStateMutability(FunctionType const& _other) const
{
	if (m_kind != _other.m_kind)
		return false;

	if (!hasEqualParameterTypes(_other) || !hasEqualReturnTypes(_other))
		return false;

	if (bound() != _other.bound())
		return false;

	solAssert(!bound() || *selfType() == *_other.selfType(), "");

	return true;
}

bool FunctionType::isBareCall() const
{
	switch (m_kind)
	{
	case Kind::BareCall:
	case Kind::BareCallCode:
	case Kind::BareDelegateCall:
	case Kind::BareStaticCall:
	case Kind::ECRecover:
	case Kind::SHA256:
	case Kind::RIPEMD160:
		return true;
	default:
		return false;
	}
}

string FunctionType::externalSignature() const
{
	solAssert(m_declaration != nullptr, "External signature of function needs declaration");
	solAssert(!m_declaration->name().empty(), "Fallback function has no signature.");
	switch (kind())
	{
	case Kind::Internal:
	case Kind::External:
	case Kind::DelegateCall:
	case Kind::Event:
	case Kind::Declaration:
		break;
	default:
		solAssert(false, "Invalid function type for requesting external signature.");
	}

	// "inLibrary" is only relevant if this is not an event.
	bool const inLibrary = kind() != Kind::Event && dynamic_cast<ContractDefinition const&>(*m_declaration->scope()).isLibrary();

	auto extParams = transformParametersToExternal(m_parameterTypes, inLibrary);

	solAssert(extParams.message().empty(), extParams.message());

	auto typeStrings = extParams.get() | boost::adaptors::transformed([&](TypePointer _t) -> string
	{
		string typeName = _t->signatureInExternalFunction(inLibrary);
		return typeName;
	});
	return m_declaration->name() + "(" + boost::algorithm::join(typeStrings, ",") + ")";
}

u256 FunctionType::externalIdentifier() const
{
	return util::FixedHash<4>::Arith(util::FixedHash<4>(util::keccak256(externalSignature())));
}

string FunctionType::externalIdentifierHex() const
{
	return util::FixedHash<4>(util::keccak256(externalSignature())).hex();
}

bool FunctionType::isPure() const
{
	// TODO: replace this with m_stateMutability == StateMutability::Pure once
	//       the callgraph analyzer is in place
	return
		m_kind == Kind::KECCAK256 ||
		m_kind == Kind::ECRecover ||
		m_kind == Kind::SHA256 ||
		m_kind == Kind::RIPEMD160 ||
		m_kind == Kind::AddMod ||
		m_kind == Kind::MulMod ||
		m_kind == Kind::ObjectCreation ||
		m_kind == Kind::ABIEncode ||
		m_kind == Kind::ABIEncodePacked ||
		m_kind == Kind::ABIEncodeWithSelector ||
		m_kind == Kind::ABIEncodeWithSignature ||
		m_kind == Kind::ABIDecode ||
		m_kind == Kind::MetaType ||

		m_kind == Kind::AddressMakeAddrStd ||
		m_kind == Kind::AddressMakeAddrNone;
}

TypePointers FunctionType::parseElementaryTypeVector(strings const& _types)
{
	TypePointers pointers;
	pointers.reserve(_types.size());
	for (string const& type: _types)
		pointers.push_back(TypeProvider::fromElementaryTypeName(type));
	return pointers;
}

FunctionTypePointer FunctionType::asCallableFunction(bool _inLibrary, bool _bound) const
{
	if (_bound)
		solAssert(!m_parameterTypes.empty(), "");

	TypePointers parameterTypes;
	for (auto const& t: m_parameterTypes)
	{
		auto refType = dynamic_cast<ReferenceType const*>(t);
		if (refType)
			parameterTypes.push_back(TypeProvider::withLocation(refType, true));
		else
			parameterTypes.push_back(t);
	}

	Kind kind = m_kind;
	if (_inLibrary)
	{
		solAssert(!!m_declaration, "Declaration has to be available.");
		if (!m_declaration->isPublic())
			kind = Kind::Internal; // will be inlined
		else
			kind = Kind::DelegateCall;
	}

	return TypeProvider::function(
		parameterTypes,
		m_returnParameterTypes,
		m_parameterNames,
		m_returnParameterNames,
		kind,
		m_arbitraryParameters,
		m_stateMutability,
		m_declaration,
		_bound
	);
}

Type const* FunctionType::selfType() const
{
	solAssert(bound(), "Function is not bound.");
	solAssert(m_parameterTypes.size() > 0, "Function has no self type.");
	return m_parameterTypes.at(0);
}

ASTPointer<StructuredDocumentation> FunctionType::documentation() const
{
	auto function = dynamic_cast<StructurallyDocumented const*>(m_declaration);
	if (function)
		return function->documentation();

	return ASTPointer<StructuredDocumentation>();
}

bool FunctionType::padArguments() const
{
	// No padding only for hash functions, low-level calls and the packed encoding function.
	switch (m_kind)
	{
	case Kind::BareCall:
	case Kind::BareCallCode:
	case Kind::BareDelegateCall:
	case Kind::BareStaticCall:
	case Kind::SHA256:
	case Kind::RIPEMD160:
	case Kind::KECCAK256:
	case Kind::ABIEncodePacked:
		return false;
	default:
		return true;
	}
	return true;
}

Type const* MappingType::encodingType() const
{
	return TypeProvider::uint(256);
}

Type const* MappingType::realKeyType() const
{
	auto strOrBytesType = dynamic_cast<ArrayType const*>(m_keyType);
	if ((strOrBytesType != nullptr && strOrBytesType->isByteArray()) ||
		m_keyType->category() == Type::Category::TvmCell
	) {
		return TypeProvider::uint256();
	}

	return m_keyType;
}

BoolResult MappingType::isImplicitlyConvertibleTo(Type const& _other) const
{
	if (Type::isImplicitlyConvertibleTo(_other)) {
		return true;
	}

	if (_other.category() != category())
		return false;
	auto map = dynamic_cast<MappingType const*>(&_other);
	return *keyType() == *map->keyType() && *valueType() == *map->valueType();
}

BoolResult OptionalType::isImplicitlyConvertibleTo(Type const& _other) const
{
	if (Type::isImplicitlyConvertibleTo(_other))
		return true;

	if (auto optOther = dynamic_cast<OptionalType const*>(&_other)) {
		if (isImplicitlyConvertibleTo(*optOther->valueType()))
			return true;
		if (valueType()->isImplicitlyConvertibleTo(*optOther->valueType()))
			return true;
	}
	bool r = *this == _other;
	return r;
}

string MappingType::richIdentifier() const
{
	return "t_mapping" + identifierList(m_keyType, m_valueType);
}

string OptionalType::richIdentifier() const
{
	return "t_optional_" + m_type->richIdentifier();
}


bool MappingType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	MappingType const& other = dynamic_cast<MappingType const&>(_other);
	return *other.m_keyType == *m_keyType && *other.m_valueType == *m_valueType;
}

bool OptionalType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	OptionalType const& other = dynamic_cast<OptionalType const&>(_other);
	return *other.m_type == *m_type;
}

string MappingType::toString(bool _short) const
{
	return "mapping(" + keyType()->toString(_short) + " => " + valueType()->toString(_short) + ")";
}

string OptionalType::toString(bool _short) const
{
	return "optional(" + valueType()->toString(_short) + ")";
}

string MappingType::canonicalName() const
{
	return "mapping(" + keyType()->canonicalName() + " => " + valueType()->canonicalName() + ")";
}

string OptionalType::canonicalName() const
{
	return "optional(" + valueType()->canonicalName() + ")";
}

TypeResult MappingType::unaryOperatorResult(Token _operator) const {
	return _operator == Token::Delete ? TypeProvider::tuple(std::vector<Type const*>()) : TypePointer();
}

TypeResult OptionalType::unaryOperatorResult(Token _operator) const {
	return _operator == Token::Delete ? TypeProvider::tuple(std::vector<Type const*>()) : TypePointer();
}

BoolResult NullType::isImplicitlyConvertibleTo(Type const& _other) const {
	auto opt = dynamic_cast<OptionalType const*>(&_other);
	return opt;
}

std::string NullType::richIdentifier() const {
	return "null";
}

bool NullType::operator==(Type const& _other) const {
	return _other.category() == category();
}

std::string NullType::toString(bool /*_short*/) const {
	return "null";
}

std::string NullType::canonicalName() const {
	return "null";
}

BoolResult EmptyMapType::isImplicitlyConvertibleTo(Type const& _other) const {
	auto map  = dynamic_cast<MappingType const*>(&_other);
	return map;
}

std::string EmptyMapType::richIdentifier() const {
	return "emptyMap";
}

bool EmptyMapType::operator==(Type const& _other) const {
	return _other.category() == category();
}

std::string EmptyMapType::toString(bool /*_short*/) const {
	return "emptyMap";
}

std::string EmptyMapType::canonicalName() const {
	return "emptyMap";
}

string TypeType::richIdentifier() const
{
	return "t_type" + identifierList(actualType());
}

bool TypeType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	TypeType const& other = dynamic_cast<TypeType const&>(_other);
	return *actualType() == *other.actualType();
}

u256 TypeType::storageSize() const
{
	solAssert(false, "Storage size of non-storable type type requested.");
}

unsigned TypeType::sizeOnStack() const
{
	if (auto contractType = dynamic_cast<ContractType const*>(m_actualType))
		if (contractType->contractDefinition().isLibrary())
			return 1;
	return 0;
}

MemberList::MemberMap TypeType::nativeMembers(ContractDefinition const* _currentScope) const
{
	MemberList::MemberMap members;
	if (m_actualType->category() == Category::Contract)
	{
		ContractDefinition const& contract = dynamic_cast<ContractType const&>(*m_actualType).contractDefinition();
		bool inDerivingScope = _currentScope && _currentScope->derivesFrom(contract);

		for (auto const* declaration: contract.declarations())
		{
			if (dynamic_cast<ModifierDefinition const*>(declaration))
				continue;

			if (!contract.isLibrary() && inDerivingScope && declaration->isVisibleInDerivedContracts())
				members.emplace_back(declaration->name(), declaration->type(), declaration);
			else if (
				(contract.isLibrary() && declaration->isVisibleAsLibraryMember()) ||
				declaration->isVisibleViaContractTypeAccess()
			)
				members.emplace_back(declaration->name(), declaration->typeViaContractName(), declaration);
		}
	}
	else if (m_actualType->category() == Category::Enum)
	{
		EnumDefinition const& enumDef = dynamic_cast<EnumType const&>(*m_actualType).enumDefinition();
		auto enumType = TypeProvider::enumType(enumDef);
		for (ASTPointer<EnumValue> const& enumValue: enumDef.members())
			members.emplace_back(enumValue->name(), enumType);
	} else if (m_actualType->category() == Category::Address) {
		members.emplace_back("makeAddrExtern", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::uint256()},
				TypePointers{TypeProvider::address()},
				strings{string(), string()},
				strings{string()},
				FunctionType::Kind::AddressMakeAddrExtern,
				false, StateMutability::Pure
		));
		members.emplace_back("makeAddrNone", TypeProvider::function(
				TypePointers{},
				TypePointers{TypeProvider::address()},
				strings{},
				strings{string()},
				FunctionType::Kind::AddressMakeAddrNone,
				false, StateMutability::Pure
		));
		members.emplace_back("makeAddrStd", TypeProvider::function(
				TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Signed), TypeProvider::uint256()},
				TypePointers{TypeProvider::address()},
				strings{string(), string()},
				strings{string()},
				FunctionType::Kind::AddressMakeAddrStd,
				false, StateMutability::Pure
		));
	}
	return members;
}

BoolResult TypeType::isExplicitlyConvertibleTo(Type const& _convertTo) const
{
	if (dynamic_cast<AddressType const*>(&_convertTo))
		if (auto const* contractType = dynamic_cast<ContractType const*>(m_actualType))
			return contractType->contractDefinition().isLibrary();
	return isImplicitlyConvertibleTo(_convertTo);
}

ModifierType::ModifierType(ModifierDefinition const& _modifier)
{
	TypePointers params;
	params.reserve(_modifier.parameters().size());
	for (ASTPointer<VariableDeclaration> const& var: _modifier.parameters())
		params.push_back(var->annotation().type);
	swap(params, m_parameterTypes);
}

u256 ModifierType::storageSize() const
{
	solAssert(false, "Storage size of non-storable type type requested.");
}

string ModifierType::richIdentifier() const
{
	return "t_modifier" + identifierList(m_parameterTypes);
}

bool ModifierType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	ModifierType const& other = dynamic_cast<ModifierType const&>(_other);

	if (m_parameterTypes.size() != other.m_parameterTypes.size())
		return false;
	auto typeCompare = [](Type const* _a, Type const* _b) -> bool { return *_a == *_b; };

	if (!equal(
		m_parameterTypes.cbegin(),
		m_parameterTypes.cend(),
		other.m_parameterTypes.cbegin(),
		typeCompare
	))
		return false;
	return true;
}

string ModifierType::toString(bool _short) const
{
	string name = "modifier (";
	for (auto it = m_parameterTypes.begin(); it != m_parameterTypes.end(); ++it)
		name += (*it)->toString(_short) + (it + 1 == m_parameterTypes.end() ? "" : ",");
	return name + ")";
}

string ModuleType::richIdentifier() const
{
	return "t_module_" + to_string(m_sourceUnit.id());
}

bool ModuleType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	return &m_sourceUnit == &dynamic_cast<ModuleType const&>(_other).m_sourceUnit;
}

MemberList::MemberMap ModuleType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap symbols;
	for (auto const& symbolName: m_sourceUnit.annotation().exportedSymbols)
		for (Declaration const* symbol: symbolName.second)
			symbols.emplace_back(symbolName.first, symbol->type(), symbol);
	return symbols;
}

string ModuleType::toString(bool) const
{
	return string("module \"") + m_sourceUnit.annotation().path + string("\"");
}

string MagicType::richIdentifier() const
{
	switch (m_kind)
	{
	case Kind::Block:
		return "t_magic_block";
	case Kind::Message:
		return "t_magic_message";
	case Kind::TVM:
		return "t_magic_tvm";
	case Kind::Transaction:
		return "t_magic_transaction";
	case Kind::ABI:
		return "t_magic_abi";
	case Kind::MetaType:
		solAssert(m_typeArgument, "");
		return "t_magic_meta_type_" + m_typeArgument->richIdentifier();
	case Kind::Math:
		return "t_magic_math";
	case Kind::Rnd:
		return "t_magic_rnd";
	case Kind::Gosh:
		return "t_magic_gosh";
	}
	return "";
}

bool MagicType::operator==(Type const& _other) const
{
	if (_other.category() != category())
		return false;
	MagicType const& other = dynamic_cast<MagicType const&>(_other);
	return other.m_kind == m_kind;
}

MemberList::MemberMap MagicType::nativeMembers(ContractDefinition const*) const
{
	switch (m_kind)
	{
	case Kind::Block:
		return MemberList::MemberMap({
			{"coinbase", TypeProvider::address()},
			{"timestamp", TypeProvider::uint(64)},
			{"blockhash", TypeProvider::function(strings{"uint"}, strings{"bytes32"}, FunctionType::Kind::BlockHash, false, StateMutability::View)},
			{"difficulty", TypeProvider::uint256()},
			{"number", TypeProvider::uint256()},
			{"gaslimit", TypeProvider::uint256()}
		});
	case Kind::Message:
		return MemberList::MemberMap({
			{"sender", TypeProvider::address()},
			{"pubkey", TypeProvider::function(strings(), strings{"uint"}, FunctionType::Kind::MsgPubkey, false, StateMutability::Pure)},
			{"createdAt", TypeProvider::uint(32)},
			{"hasStateInit", TypeProvider::boolean()},
			{"gas", TypeProvider::uint256()},
			{"value", TypeProvider::uint(128)},
			{"data", TypeProvider::tvmslice()},
			{"sig", TypeProvider::fixedBytes(4)},
			{"currencies", TypeProvider::extraCurrencyCollection()},
			{"isExternal", TypeProvider::boolean()},
			{"isInternal", TypeProvider::boolean()},
			{"isTickTock", TypeProvider::boolean()},
		});
	case Kind::TVM: {
		MemberList::MemberMap members = {
			{"code", TypeProvider::function({}, {TypeProvider::tvmcell()}, {}, {{}}, FunctionType::Kind::TVMCode, false, StateMutability::Pure)},
			{"codeSalt", TypeProvider::function({TypeProvider::tvmcell()}, {TypeProvider::optional(TypeProvider::tvmcell())}, {{}}, {{}}, FunctionType::Kind::TVMCodeSalt, false, StateMutability::Pure)},
			{"setCodeSalt", TypeProvider::function({TypeProvider::tvmcell(), TypeProvider::tvmcell()}, {TypeProvider::tvmcell()}, {{}, {}}, {{}}, FunctionType::Kind::TVMSetCodeSalt, false, StateMutability::Pure)},
			{"pubkey", TypeProvider::function(strings(), strings{"uint"}, FunctionType::Kind::TVMPubkey, false, StateMutability::Pure)},
			{"setPubkey", TypeProvider::function({"uint"}, {}, FunctionType::Kind::TVMSetPubkey, false, StateMutability::NonPayable)},
			{"accept", TypeProvider::function(strings(), strings(), FunctionType::Kind::TVMAccept, false, StateMutability::Pure)},
			{"commit", TypeProvider::function(strings(), strings(), FunctionType::Kind::TVMCommit, false, StateMutability::NonPayable)},
			{"rawCommit", TypeProvider::function(strings(), strings(), FunctionType::Kind::TVMCommit, false, StateMutability::NonPayable)},
			{"getData", TypeProvider::function({}, {TypeProvider::tvmcell()}, {}, {{}}, FunctionType::Kind::TVMCommit, false, StateMutability::Pure)},
			{"setData", TypeProvider::function({TypeProvider::tvmcell()}, {}, {{}}, {}, FunctionType::Kind::TVMCommit, false, StateMutability::NonPayable)},
			{"resetStorage", TypeProvider::function(strings(), strings(), FunctionType::Kind::TVMResetStorage, false, StateMutability::NonPayable)},
			{"log", TypeProvider::function(strings{"string"}, strings{}, FunctionType::Kind::LogTVM, false, StateMutability::Pure)},
			{"exit", TypeProvider::function(strings{}, strings{}, FunctionType::Kind::TVMExit, false, StateMutability::Pure)},
			{"exit1", TypeProvider::function(strings{}, strings{}, FunctionType::Kind::TVMExit1, false, StateMutability::Pure)},
			{"setGasLimit", TypeProvider::function({"uint"}, {}, FunctionType::Kind::TVMSetGasLimit, false, StateMutability::Pure)},
			{"initCodeHash", TypeProvider::function({}, {"uint256"}, FunctionType::Kind::TVMInitCodeHash, false, StateMutability::Pure)},

			// for stdlib
			{"replayProtTime", TypeProvider::function({}, {"uint64"}, FunctionType::Kind::TVMReplayProtTime, false, StateMutability::Pure)},
			{"setReplayProtTime", TypeProvider::function({"uint64"}, {}, FunctionType::Kind::TVMSetReplayProtTime, false, StateMutability::Pure)},
			{"replayProtInterval", TypeProvider::function({}, {"uint64"}, FunctionType::Kind::TVMReplayProtInterval, false, StateMutability::Pure)},
		};
		members.emplace_back("rawReserve", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::extraCurrencyCollection(),  TypeProvider::uint256()},
				TypePointers{},
				strings{string{}, string{}, string{}},
				strings{},
				FunctionType::Kind::TVMSetcode,
				false, StateMutability::Pure
		));
		members.emplace_back("rawReserve", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::uint256()},
				TypePointers{},
				strings{string{}, string{}},
				strings{},
				FunctionType::Kind::TVMSetcode,
				false, StateMutability::Pure
		));
		members.emplace_back("setcode", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell()},
				TypePointers{},
				strings{string()},
				strings{},
				FunctionType::Kind::TVMSetcode,
				false, StateMutability::Pure
		));
		members.emplace_back("setCurrentCode", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell()},
				TypePointers{},
				strings{string()},
				strings{},
				FunctionType::Kind::TVMSetcode,
				false, StateMutability::Pure
		));
		members.emplace_back("bindump", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::TVMDump,
				true, StateMutability::Pure
		));
		members.emplace_back("hexdump", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::TVMDump,
				true, StateMutability::Pure
		));
		members.emplace_back("hash", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell()},
				TypePointers{TypeProvider::uint256()},
				strings{string()},
				strings{string()},
				FunctionType::Kind::TVMHash,
				false, StateMutability::Pure
		));
		members.emplace_back("hash", TypeProvider::function(
				{TypeProvider::tvmslice()},
				{TypeProvider::uint256()},
				{{}},
				{{}},
				FunctionType::Kind::TVMHash,
				false, StateMutability::Pure
		));
		members.emplace_back("hash", TypeProvider::function(
				TypePointers{TypeProvider::array(true)},
				TypePointers{TypeProvider::uint256()},
				strings{string()},
				strings{string()},
				FunctionType::Kind::TVMHash,
				false, StateMutability::Pure
		));
		members.emplace_back("checkSign", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()},
				TypePointers{TypeProvider::boolean()},
				strings{string(), string(), string(), string()},
				strings{string()},
				FunctionType::Kind::TVMChecksign,
				false, StateMutability::Pure
		));
		members.emplace_back("checkSign", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::tvmslice(), TypeProvider::uint256()},
				TypePointers{TypeProvider::boolean()},
				strings{string(), string(), string()},
				strings{string()},
				FunctionType::Kind::TVMChecksign,
				false, StateMutability::Pure
		));
		members.emplace_back("checkSign", TypeProvider::function(
				TypePointers{TypeProvider::tvmslice(), TypeProvider::tvmslice(), TypeProvider::uint256()},
				TypePointers{TypeProvider::boolean()},
				strings{string(), string(), string()},
				strings{string()},
				FunctionType::Kind::TVMChecksign,
				false, StateMutability::Pure
		));
		members.emplace_back("sendrawmsg", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell(), TypeProvider::uint(8)},
				TypePointers{},
				strings{string(), string()},
				strings{},
				FunctionType::Kind::TVMSendMsg,
				false, StateMutability::Pure
		));

		members.emplace_back("configParam", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::TVMConfigParam,
				true, StateMutability::Pure
		));

		members.emplace_back("rawConfigParam", TypeProvider::function(
				TypePointers{TypeProvider::integer(32, IntegerType::Modifier::Signed)},
				TypePointers{TypeProvider::tvmcell(), TypeProvider::boolean()},
				strings{string()},
				strings{string(), string()},
				FunctionType::Kind::TVMRawConfigParam,
				false, StateMutability::Pure
		));

		members.emplace_back("buildExtMsg", TypeProvider::function(
				TypePointers{TypeProvider::address(),
							 TypeProvider::callList(),
							 TypeProvider::uint(32),
							 TypeProvider::uint(8),
							 TypeProvider::uint(32),
							 TypeProvider::optional(TypeProvider::uint(32)),
							 TypeProvider::uint(64),
							 TypeProvider::uint(32),
							 TypeProvider::optional(TypeProvider::uint256()),
							 TypeProvider::boolean(),
							 TypeProvider::tvmcell()},
				TypePointers{TypeProvider::tvmcell()},
				strings{string("dest"),			// mandatory
						string("call"),			// mandatory
						string("callbackId"),	// mandatory
						string("abiVer"),		// must be deleted
						string("onErrorId"),	    // mandatory
						string("signBoxHandle"),	// can be omitted
						string("time"),			// can be omitted
						string("expire"),		// can be omitted
						string("pubkey"),		// can be omitted
						string("sign"),			// can be omitted
						string("stateInit")},	// can be omitted
				strings{string()},
				FunctionType::Kind::TVMBuildExtMsg,
				true, StateMutability::Pure
		));


		members.emplace_back("buildIntMsg", TypeProvider::function(
			{
				TypeProvider::address(),
				TypeProvider::uint(128),
				TypeProvider::extraCurrencyCollection(),
				TypeProvider::boolean(),
				TypeProvider::callList(),
				TypeProvider::tvmcell(),
			},
			{TypeProvider::tvmcell()},
			{
				"dest", // mandatory
				"value", // mandatory
				"currencies", // can be omitted
				"bounce", // can be omitted
				"call", // mandatory
				"stateInit", // can be omitted
			},
			{{}},
			FunctionType::Kind::TVMBuildIntMsg,
			true, StateMutability::Pure
		));

		members.emplace_back("buildStateInit", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell(),
							 TypeProvider::tvmcell(),
							 TypeProvider::uint(8),
							 TypeProvider::initializerList(),
							 TypeProvider::uint256(),
							 //TypeProvider::contract(...) it's commented because we should set the concrete contract
							 // but it can be any contract
							 },
				TypePointers{TypeProvider::tvmcell()},
				strings{string("code"),	// mandatory
						string("data"),	// conflicts with pubkey and varInit
						string("splitDepth"),	// can be omitted
						string("varInit"),	// conflicts with data
						string("pubkey"),	// conflicts with data
						//string("contr")
					},
				strings{string()},
				FunctionType::Kind::TVMBuildStateInit,
				true, StateMutability::Pure
		));

		members.emplace_back("buildDataInit", TypeProvider::function(
			{
				TypeProvider::uint256(),
				TypeProvider::initializerList(),
				//TypeProvider::contract(...) it's commented because we should set the concrete contract
			},
			{TypeProvider::tvmcell()},
			{"pubkey", "varInit"},
			{{}},
			FunctionType::Kind::TVMBuildDataInit,
			true, StateMutability::Pure
		));

		members.emplace_back("insertPubkey", TypeProvider::function(
				TypePointers{TypeProvider::tvmcell(), TypeProvider::uint256()},
				TypePointers{TypeProvider::tvmcell()},
				strings{string(), string()},
				strings{string()},
				FunctionType::Kind::TVMDeploy,
				false, StateMutability::Pure
		));

		members.emplace_back("stateInitHash", TypeProvider::function(
				TypePointers{TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint(16), TypeProvider::uint(16)},
				TypePointers{TypeProvider::uint256()},
				strings{string(), string(), string(), string()},
				strings{string()},
				FunctionType::Kind::TVMDeploy,
				false, StateMutability::Pure
		));

		members.emplace_back("functionId", TypeProvider::function(
				TypePointers{},
				TypePointers{TypeProvider::uint(32)},
				strings{},
				strings{string()},
				FunctionType::Kind::TVMFunctionId,
				true, StateMutability::Pure
		));

		members.emplace_back("encodeBody", TypeProvider::function(
				TypePointers{},
				TypePointers{TypeProvider::tvmcell()},
				strings{},
				strings{string()},
				FunctionType::Kind::TVMEncodeBody,
				true, StateMutability::Pure
		));

		return members;
	}
	case Kind::Rnd: {
		MemberList::MemberMap members = {
			{
				"next",
				TypeProvider::function({}, {}, {}, {}, FunctionType::Kind::RndNext, true, StateMutability::Pure)
			},
			{
				"setSeed",
				TypeProvider::function({TypeProvider::uint256()}, {}, {{}}, {}, FunctionType::Kind::RndSetSeed, false, StateMutability::Pure)
			},
			{
				"getSeed",
				TypeProvider::function({}, {TypeProvider::uint256()}, {}, {{}}, FunctionType::Kind::RndGetSeed, false, StateMutability::Pure)
			},
			{
				"shuffle",
				TypeProvider::function({}, {}, {}, {}, FunctionType::Kind::RndShuffle, false, StateMutability::Pure)
			},
			{
				"shuffle",
				TypeProvider::function({TypeProvider::uint256()}, {}, {{}}, {}, FunctionType::Kind::RndShuffle, false, StateMutability::Pure)
			}
		};
		return members;
	}
	case Kind::Math: {
		MemberList::MemberMap members = {
			{
				"divc",
				TypeProvider::function(
					{}, {}, {}, {}, FunctionType::Kind::MathDivC, false, StateMutability::Pure
				)
			},
			{
				"divr",
				TypeProvider::function(
						{}, {}, {}, {}, FunctionType::Kind::MathDivR, false, StateMutability::Pure
				)
			}
		};
		members.emplace_back("max", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathMax,
				true, StateMutability::Pure
		));
		members.emplace_back("min", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathMin,
				true, StateMutability::Pure
		));
		members.emplace_back("minmax", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathMinMax,
				true, StateMutability::Pure
		));
		for(const std::string code : {"muldiv", "muldivr", "muldivc"}) {
			members.emplace_back(code, TypeProvider::function(
					TypePointers{},
					TypePointers{},
					strings{},
					strings{},
					FunctionType::Kind::MathMulDiv,
					true, StateMutability::Pure
			));
		}
		members.emplace_back("muldivmod", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathMulDivMod,
				true, StateMutability::Pure
		));
		members.emplace_back("divmod", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathDivMod,
				true, StateMutability::Pure
		));
		members.emplace_back("abs", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathAbs,
				true, StateMutability::Pure
		));
		members.emplace_back("modpow2", TypeProvider::function(
				TypePointers{},
				TypePointers{},
				strings{},
				strings{},
				FunctionType::Kind::MathModpow2,
				true, StateMutability::Pure
		));
		members.emplace_back("sign", TypeProvider::function(
				TypePointers{TypeProvider::integer(256, IntegerType::Modifier::Signed)},
				TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Signed)},
				strings{string("value")},
				strings{string("sign")},
				FunctionType::Kind::MathSign,
				false, StateMutability::Pure
		));
		return members;
	}
	case Kind::Transaction:
		return MemberList::MemberMap({
			{"gasprice", TypeProvider::uint256()},
			{"origin", TypeProvider::address()},
			{"storageFee", TypeProvider::uint(64)},
			{"timestamp", TypeProvider::uint(64)},
		});
	case Kind::ABI:
		return MemberList::MemberMap({
			{"encode", TypeProvider::function(
				TypePointers{},
				TypePointers{TypeProvider::tvmcell()},
				strings{},
				strings{{}},
				FunctionType::Kind::ABIEncode,
				true,
				StateMutability::Pure
			)},
			{"encodePacked", TypeProvider::function(
				TypePointers{},
				TypePointers{TypeProvider::array()},
				strings{},
				strings{1, ""},
				FunctionType::Kind::ABIEncodePacked,
				true,
				StateMutability::Pure
			)},
			{"encodeWithSelector", TypeProvider::function(
				TypePointers{TypeProvider::fixedBytes(4)},
				TypePointers{TypeProvider::array()},
				strings{1, ""},
				strings{1, ""},
				FunctionType::Kind::ABIEncodeWithSelector,
				true,
				StateMutability::Pure
			)},
			{"encodeWithSignature", TypeProvider::function(
				TypePointers{TypeProvider::array(true)},
				TypePointers{TypeProvider::array()},
				strings{1, ""},
				strings{1, ""},
				FunctionType::Kind::ABIEncodeWithSignature,
				true,
				StateMutability::Pure
			)},
			{"decode", TypeProvider::function(
				TypePointers(),
				TypePointers(),
				strings{},
				strings{},
				FunctionType::Kind::ABIDecode,
				true,
				StateMutability::Pure
			)}
		});
	case Kind::Gosh: {
		MemberList::MemberMap members;
		for (auto const&[name, type] : std::vector<std::tuple<string, FunctionType::Kind>>{
				{"diff", FunctionType::Kind::GoshDiff},
				{"applyPatch", FunctionType::Kind::GoshApplyPatch},
		}) {
			members.push_back({ name,
				TypeProvider::function(
					{TypeProvider::stringMemory(), TypeProvider::stringMemory()},
					{TypeProvider::stringMemory()},
					{{}, {}},
					{{}},
					type,
					true,
					StateMutability::Pure
		  	)});
		}

		for (auto const&[name, type] : std::vector<std::tuple<string, FunctionType::Kind>>{
				{"zipDiff", FunctionType::Kind::GoshZipDiff},
				{"applyZipPatch", FunctionType::Kind::GoshApplyZipPatch},
		}) {
			members.push_back({ name,
				TypeProvider::function(
					{TypeProvider::tvmcell(), TypeProvider::tvmcell()},
					{TypeProvider::tvmcell()},
					{{}, {}},
					{{}},
					type,
					true,
					StateMutability::Pure
		  	)});
		}

		members.push_back({ "applyPatchQ",
			TypeProvider::function(
				{TypeProvider::stringMemory(), TypeProvider::stringMemory()},
				{TypeProvider::optional(TypeProvider::stringMemory())},
				{{}, {}},
				{{}},
				FunctionType::Kind::GoshApplyZipPatchQ,
				true,
				StateMutability::Pure
		)});

		members.push_back({ "applyZipPatchQ",
			TypeProvider::function(
				{TypeProvider::tvmcell(), TypeProvider::tvmcell()},
				{TypeProvider::optional(TypeProvider::tvmcell())},
				{{}, {}},
				{{}},
				FunctionType::Kind::GoshApplyZipPatchQ,
				true,
				StateMutability::Pure
		)});

		members.push_back({
			"zip",
			TypeProvider::function(
				{TypeProvider::stringMemory()},
				{TypeProvider::tvmcell()},
				{{}},
				{{}},
				FunctionType::Kind::GoshZip,
				true,
				StateMutability::Pure
		)});
		members.push_back({
			"unzip",
			TypeProvider::function(
				  {TypeProvider::tvmcell()},
				  {TypeProvider::stringMemory()},
				  {{}},
				  {{}},
				  FunctionType::Kind::GoshUnzip,
				  true,
				  StateMutability::Pure
		)});

		return members;
	}
	case Kind::MetaType:
	{
		solAssert(
			m_typeArgument && m_typeArgument->category() == Type::Category::Contract,
			"Only contracts supported for now"
		);
		ContractDefinition const& contract = dynamic_cast<ContractType const&>(*m_typeArgument).contractDefinition();
		if (contract.canBeDeployed())
			return MemberList::MemberMap({
				{"creationCode", TypeProvider::array()},
				{"runtimeCode", TypeProvider::array()},
				{"name", TypeProvider::stringMemory()},
			});
		else
			return {};
	}
	}
	solAssert(false, "Unknown kind of magic.");
	return {};
}

string MagicType::toString(bool _short) const
{
	switch (m_kind)
	{
	case Kind::Block:
		return "block";
	case Kind::Message:
		return "msg";
	case Kind::TVM:
		return "tvm";
	case Kind::Transaction:
		return "tx";
	case Kind::ABI:
		return "abi";
	case Kind::MetaType:
		solAssert(m_typeArgument, "");
		return "type(" + m_typeArgument->toString(_short) + ")";
	case Kind::Math:
		return "math";
	case Kind::Rnd:
		return "rnd";
	case Kind::Gosh:
		return "gosh";
	}
	solAssert(false, "Unknown kind of magic.");
	return {};
}

TypePointer MagicType::typeArgument() const
{
	solAssert(m_kind == Kind::MetaType, "");
	solAssert(m_typeArgument, "");
	return m_typeArgument;
}

TypePointer InaccessibleDynamicType::decodingType() const
{
	return TypeProvider::uint(256);
}

MemberList::MemberMap OptionalType::nativeMembers(ContractDefinition const*) const
{
	MemberList::MemberMap members;
	members.emplace_back("hasValue", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::boolean()},
			strings{},
			strings{string()},
			FunctionType::Kind::OptionalHasValue,
			false,
			StateMutability::Pure
	));
	members.emplace_back("get", TypeProvider::function(
			TypePointers{},
			TypePointers{valueType()},
			strings{},
			strings{string()},
			FunctionType::Kind::OptionalGet,
			false,
			StateMutability::Pure
	));
	members.emplace_back("set", TypeProvider::function(
			TypePointers{valueType()},
			TypePointers{},
			strings{string()},
			strings{},
			FunctionType::Kind::OptionalSet,
			false,
			StateMutability::Pure
	));
	if (auto tuple = dynamic_cast<TupleType const*>(valueType())) {
		TypePointers input;
		strings names;
		for (const TypePointer& comp : tuple->components()) {
			input.push_back(comp);
			names.push_back("");
		}
		members.emplace_back("set", TypeProvider::function(
				input,
				{},
				names,
				{},
				FunctionType::Kind::OptionalSet,
				false,
				StateMutability::Pure
		));
	}
    members.emplace_back("reset", TypeProvider::function(
            TypePointers{},
            TypePointers{},
            strings{},
            strings{},
            FunctionType::Kind::OptionalReset,
            false,
            StateMutability::Pure
    ));
	return members;
}

TypeResult TvmSliceType::unaryOperatorResult(Token _operator) const  {
	if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	return nullptr;
}

MemberList::MemberMap TvmSliceType::nativeMembers(ContractDefinition const *) const {
	MemberList::MemberMap members = {
		{
			"dataSize",
			TypeProvider::function(
				{TypeProvider::uint256()},
				{TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()},
				{{}},
				{{}, {}, {}},
				FunctionType::Kind::TVMSliceDataSize,
				true,
				StateMutability::Pure
			)
		},
		{
			"dataSizeQ",
			TypeProvider::function(
				{TypeProvider::uint256()},
				{TypeProvider::optional(TypeProvider::tuple({TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()}))},
				{{}},
				{{}},
				FunctionType::Kind::TVMSliceDataSize,
				true,
				StateMutability::Pure
			)
		}
	};

	members.emplace_back("decode", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::TVMSliceDecode,
			true,
			StateMutability::Pure
	));
	members.emplace_back("decodeQ", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::TVMSliceDecodeQ,
			true,
			StateMutability::Pure
	));

	members.emplace_back("decodeFunctionParams", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::DecodeFunctionParams,
			true,
			StateMutability::Pure
	));

	members.emplace_back("decodeStateVars", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::TVMSliceDecodeStateVars,
			true,
			StateMutability::Pure
	));

	members.emplace_back("loadUnsigned", TypeProvider::function(
			TypePointers{TypeProvider::uint(16)},
			TypePointers{TypeProvider::uint(8)},
			strings{string()},
			strings{string()},
			FunctionType::Kind::TVMLoadRef,
			false,
			StateMutability::Pure
	));

	members.emplace_back("loadSigned", TypeProvider::function(
			TypePointers{TypeProvider::uint(16)},
			TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Signed)},
			strings{string()},
			strings{string()},
			FunctionType::Kind::TVMLoadRef,
			false,
			StateMutability::Pure
	));

	members.emplace_back("hasNBits", TypeProvider::function(
			TypePointers{TypeProvider::uint(16)},
			TypePointers{TypeProvider::boolean()},
			strings{string()},
			strings{string()},
			FunctionType::Kind::TVMSliceHas,
			false,
			StateMutability::Pure
	));

	members.emplace_back("hasNRefs", TypeProvider::function(
			TypePointers{TypeProvider::uint(8)},
			TypePointers{TypeProvider::boolean()},
			strings{string()},
			strings{string()},
			FunctionType::Kind::TVMSliceHas,
			false,
			StateMutability::Pure
	));

	members.emplace_back("hasNBitsAndRefs", TypeProvider::function(
			TypePointers{TypeProvider::uint(16), TypeProvider::uint(8)},
			TypePointers{TypeProvider::boolean()},
			strings{string(), string()},
			strings{string()},
			FunctionType::Kind::TVMSliceHas,
			false,
			StateMutability::Pure
	));

	members.emplace_back("loadTons", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(128)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMLoadRef,
			false,
			StateMutability::Pure
	));

	members.emplace_back("loadSlice", TypeProvider::function(
			{TypeProvider::uint256()},
			{TypeProvider::tvmslice()},
			{{}},
			{{}},
			FunctionType::Kind::TVMLoadSlice,
			false,
			StateMutability::Pure
	));

	members.emplace_back("loadSlice", TypeProvider::function(
			{TypeProvider::uint256(), TypeProvider::uint256()},
			{TypeProvider::tvmslice()},
			{{},{}},
			{{}},
			FunctionType::Kind::TVMLoadSlice,
			false,
			StateMutability::Pure
	));

	members.emplace_back("skip", TypeProvider::function(
			strings{"uint"},
			strings{},
			FunctionType::Kind::TVMSliceSkip,
			false,
			StateMutability::Pure
	));

	members.emplace_back("skip", TypeProvider::function(
			strings{"uint", "uint"},
			strings{},
			FunctionType::Kind::TVMSliceSkip,
			false,
			StateMutability::Pure
	));

	members.emplace_back("size", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16), TypeProvider::uint(8)},
			strings{},
			strings{string(), string()},
			FunctionType::Kind::TVMSliceSize,
			false, StateMutability::Pure
	));

	members.emplace_back("empty", TypeProvider::function(
			{},
			{TypeProvider::boolean()},
			{},
			{{}},
			FunctionType::Kind::TVMSliceEmpty,
			false, StateMutability::Pure
	));

	members.emplace_back("bits", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMSliceSize,
			false, StateMutability::Pure
	));

	members.emplace_back("refs", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(8)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMSliceSize,
			false, StateMutability::Pure
	));

	members.emplace_back("depth", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMSliceSize,
			false, StateMutability::Pure
	));

	members.emplace_back("loadRef", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmcell()},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMLoadRef,
			false, StateMutability::Pure
	));

	members.emplace_back("loadRefAsSlice", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmslice()},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMLoadRef,
			false, StateMutability::Pure
	));

	members.emplace_back("compare", TypeProvider::function(
			TypePointers{TypeProvider::tvmslice()},
			TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Signed)},
			strings{string()},
			strings{string()},
			FunctionType::Kind::TVMSliceCompare,
			false, StateMutability::Pure
	));

	return members;
}

TypeResult TvmCellType::unaryOperatorResult(Token _operator) const {
	if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	return nullptr;
}

MemberList::MemberMap TvmCellType::nativeMembers(const ContractDefinition *) const
{
	MemberList::MemberMap members;

	members.emplace_back("depth", TypeProvider::function(
		TypePointers{},
		TypePointers{TypeProvider::uint(16)},
		strings{},
		strings{string()},
		FunctionType::Kind::TVMCellDepth,
		false, StateMutability::Pure
	));

	members.emplace_back("toSlice", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmslice()},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMCellToSlice,
			false, StateMutability::Pure
	));

	members.emplace_back("dataSize", TypeProvider::function(
		{TypeProvider::uint256()},
		{TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()},
		{{}},
		{{}, {}, {}},
		FunctionType::Kind::TVMDataSize,
		{}, StateMutability::Pure
	));

	members.emplace_back("dataSizeQ", TypeProvider::function(
			{TypeProvider::uint256()},
			{TypeProvider::optional(TypeProvider::tuple({TypeProvider::uint256(), TypeProvider::uint256(), TypeProvider::uint256()}))},
			{{}},
			{{}},
			FunctionType::Kind::TVMDataSize,
			{}, StateMutability::Pure
	));


	return members;
}

TypeResult TvmVectorType::unaryOperatorResult(Token _operator) const {
	if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	return nullptr;
}

MemberList::MemberMap TvmVectorType::nativeMembers(const ContractDefinition *) const
{
	MemberList::MemberMap members;

	members.emplace_back("push", TypeProvider::function(
			TypePointers{valueType()},
			TypePointers{},
			strings{string()},
			strings{},
			FunctionType::Kind::TVMTuplePush,
			false, StateMutability::Pure
	));

	members.emplace_back("length", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(8)},
			strings{},
			strings{string("length")},
			FunctionType::Kind::TVMTupleLength,
			false, StateMutability::Pure
	));

	members.emplace_back("pop", TypeProvider::function(
			TypePointers{},
			TypePointers{valueType()},
			strings{},
			strings{string("last")},
			FunctionType::Kind::TVMTuplePop,
			false, StateMutability::Pure
	));

	members.emplace_back("empty", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::boolean()},
			strings{},
			strings{string("is_empty")},
			FunctionType::Kind::TVMTupleLength,
			false, StateMutability::Pure
	));

	return members;
}

std::string TvmVectorType::toString(bool _short) const {
	return "vector(" + valueType()->toString(_short) + ")";
}

std::string TvmVectorType::richIdentifier() const {
	return "t_vector_" + valueType()->richIdentifier();
}

TypeResult TvmBuilderType::unaryOperatorResult(Token _operator) const {
	if (_operator == Token::Delete)
		return TypeProvider::emptyTuple();
	return nullptr;
}

TypeResult TvmCellType::binaryOperatorResult(Token _operator, const Type *_other) const {
	if (_other->category() != category() ||
		(_operator != Token::Equal && _operator != Token::NotEqual))
		return nullptr;
	return _other;
}

MemberList::MemberMap TvmBuilderType::nativeMembers(const ContractDefinition *) const
{
	MemberList::MemberMap members;

    members.emplace_back("depth", TypeProvider::function(
            TypePointers{},
            TypePointers{TypeProvider::uint(16)},
            strings{},
            strings{string()},
            FunctionType::Kind::TVMBuilderMethods,
            false, StateMutability::Pure
    ));

	members.emplace_back("bits", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("refs", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(8)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("size", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16), TypeProvider::uint(8)},
			strings{},
			strings{string(), string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("remBits", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("remRefs", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(8)},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("remBitsAndRefs", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::uint(16), TypeProvider::uint(8)},
			strings{},
			strings{string(), string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("toCell", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmcell()},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("toSlice", TypeProvider::function(
			TypePointers{},
			TypePointers{TypeProvider::tvmslice()},
			strings{},
			strings{string()},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("storeRef", TypeProvider::function(
			TypePointers{TypeProvider::tvmbuilder()},
			TypePointers{},
			strings{string()},
			strings{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("storeRef", TypeProvider::function(
			{TypeProvider::tvmcell()},
			{},
			{{}},
			{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("storeRef", TypeProvider::function(
			{TypeProvider::tvmslice()},
			{},
			{{}},
			{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	for (const std::string func : {"storeOnes", "storeZeros"}) {
		members.emplace_back(func, TypeProvider::function(
				{TypeProvider::uint256()},
				{},
				{{}},
				{},
				FunctionType::Kind::TVMBuilderMethods,
				false, StateMutability::Pure
		));
	}

	members.emplace_back("store", TypeProvider::function(
			TypePointers{},
			TypePointers{},
			strings{},
			strings{},
			FunctionType::Kind::TVMBuilderStore,
			true, StateMutability::Pure
	));

	members.emplace_back("storeSigned", TypeProvider::function(
			TypePointers{TypeProvider::integer(256, IntegerType::Modifier::Signed), TypeProvider::uint(16)},
			TypePointers{},
			strings{string(), string()},
			strings{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	members.emplace_back("storeUnsigned", TypeProvider::function(
			TypePointers{TypeProvider::uint(256), TypeProvider::uint(16)},
			TypePointers{},
			strings{string(), string()},
			strings{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));
	members.emplace_back("storeTons", TypeProvider::function(
			TypePointers{TypeProvider::uint(128)},
			TypePointers{},
			strings{string()},
			strings{},
			FunctionType::Kind::TVMBuilderMethods,
			false, StateMutability::Pure
	));

	return members;
}

BoolResult VarInteger::isImplicitlyConvertibleTo(Type const& _convertTo) const {
	return m_int.isImplicitlyConvertibleTo(_convertTo);
}

BoolResult VarInteger::isExplicitlyConvertibleTo(Type const& _convertTo) const {
	return m_int.isExplicitlyConvertibleTo(_convertTo);
}

TypeResult VarInteger::binaryOperatorResult(Token _operator, Type const* _other) const {
	return m_int.binaryOperatorResult(_operator, _other);
}

std::string VarInteger::toString(bool) const {
	return std::string{} + "var" + (m_int.isSigned()? "Int" : "Uint") + std::to_string(m_n);
}

int VarInteger::maxBitSizeInCell() const {
	if (m_n == 16) {
		return 4 + (15 * 8);
	}
	if (m_n == 32) {
		return 5 + (31 * 8);
	}
	solUnimplemented("");
}

IntegerType const* ExtraCurrencyCollectionType::keyType() const {
	return TypeProvider::uint(32);
}

IntegerType const* ExtraCurrencyCollectionType::valueType() const {
	return TypeProvider::uint(256);
}

VarInteger const* ExtraCurrencyCollectionType::realValueType() const {
	return TypeProvider::varInteger(32, IntegerType::Modifier::Unsigned);
}

MemberList::MemberMap ExtraCurrencyCollectionType::nativeMembers(ContractDefinition const *) const {
	MemberList::MemberMap members;
	appendMapMethods(members, keyType(), valueType(), keyType());
	return members;
}

TypeResult ExtraCurrencyCollectionType::unaryOperatorResult(Token _operator) const {
	return _operator == Token::Delete ? TypeProvider::tuple(std::vector<Type const*>()) : TypePointer();
}
