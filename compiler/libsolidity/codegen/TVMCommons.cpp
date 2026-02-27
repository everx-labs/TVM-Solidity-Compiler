/*
 * Copyright (C) 2019-2026 EverX. All Rights Reserved.
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
 * Common TVM codegen routines, in particular, types, data structures, scope, stack manipulations, etc.
 */

#include <boost/algorithm/string/trim.hpp>

#include <libsolidity/ast/TypeProvider.h>

#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>

using namespace solidity::langutil;
using namespace solidity::util;

namespace solidity::frontend {

std::string eventName(EventDefinition const* _event) {
	ContractDefinition const* contract = _event->annotation().contract;
	if (contract == nullptr)
		return _event->name();
	if (contract->isLibrary())
		return contract->name() + "#" + _event->name();
	return _event->name();
}

void cast_error(ASTNode const& node, std::string const& error_message) {
	GlobalParams::g_errorReporter->fatalParserError(9768_error, node.location(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

void fatal_error(std::string const& error_message) {
	GlobalParams::g_errorReporter->error(5711_error, Error::Type::TypeError, SourceLocation(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

FunctionDefinition const* getSuperFunction(
	ContractDefinition const* currentContract,
	ContractDefinition const* mainContract,
	std::string const& hexName
) {
	FunctionDefinition const* prev = nullptr;
	for (auto c: getContractsChain(mainContract)) {
		if (c == currentContract)
			break;
		for (FunctionDefinition const* f: c->definedFunctions()) {
			if (f->isOrdinary() && f->externalIdentifierHex() == hexName) {
				prev = f;
			}
		}
	}
	return prev;
}

Type const* getType(VariableDeclaration const* var) { return var->annotation().type; }

bool isAddressOrAddressStdOrContractType(Type const* type) {
	return to<AddressType>(type) || to<AddressStdType>(type) || to<ContractType>(type);
}

bool isUsualArray(Type const* type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && !arrayType->isByteArrayOrString();
}

bool isByteArrayOrString(Type const* type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && arrayType->isByteArrayOrString();
}

bool isString(Type const* type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isString());
}

bool isSlice(Type const* type) { return to<TvmSliceType>(type) != nullptr; }

bool isSmallOptional(OptionalType const* type) {
	ABITypeSize size{type->valueType()};
	return 1 + size.maxBits <= 1023 && size.maxRefs <= 3;
}

bool optValueAsTuple(Type const* optValueType) {
	return isIn(optValueType->category(), Type::Category::Mapping, Type::Category::Optional);
}

int optTypeQty(Type const* type) {
	auto optValueType = to<OptionalType>(type);
	return optValueType ? 1 + optTypeQty(optValueType->valueType()) : 0;
}

int bitsForEnum(size_t val_count) {
	int bytes = 0;
	val_count--;
	while (true) {
		val_count >>= 8;
		++bytes;
		if (val_count == 0) {
			break;
		}
	}
	return 8 * bytes;
}

Type const* getType(Expression const* expr) { return expr->annotation().type; }

bool isIntegralType(Type const* type) { return TypeInfo(type).isNumeric; }

bool isStringOrStringLiteralOrBytes(Type const* type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isByteArrayOrString());
}

std::string typeToDictChar(Type const* keyType) {
	TypeInfo ti(keyType);
	if (ti.isNumeric) {
		return ti.isSigned ? "I" : "U";
	} else if (isStringOrStringLiteralOrBytes(keyType) || keyType->category() == Type::Category::TvmCell) {
		return "U";
	}
	return ""; // dict key is slice
}

int dictKeyLength(Type const* key) {
	if (isIn(key->category(), Type::Category::Address, Type::Category::AddressStd, Type::Category::Contract)) {
		return AddressInfo::stdAddrWithoutAnyCastLength();
	}

	TypeInfo ti{key};
	if (ti.isNumeric) {
		return ti.numBits;
	}

	if (isStringOrStringLiteralOrBytes(key) || key->category() == Type::Category::TvmCell) {
		return 256; // hash of tree of cells
	}

	auto structType = to<StructType>(key);
	if (structType) {
		int bitLength = 0;
		StructDefinition const& structDefinition = structType->structDefinition();
		for (auto const& member: structDefinition.members()) {
			TypeInfo ti2{member->type()};
			solAssert(ti2.isNumeric, "");
			bitLength += ti2.numBits;
		}
		return bitLength;
	}
	solUnimplemented("");
}

IntegerType getKeyTypeOfC4() { return IntegerType(TvmConst::C4::KeyLength); }

IntegerType const& getArrayKeyType() {
	return *TypeProvider::integer(TvmConst::ArrayKeyLength, IntegerType::Modifier::Unsigned);
}

std::tuple<Type const*, Type const*> dictKeyValue(Type const* type) {
	auto mapType = to<MappingType>(type);
	solAssert(mapType, "");
	return {mapType->keyType(), mapType->valueType()};
}

std::tuple<Type const*, Type const*> realDictKeyValue(Type const* type) {
	Type const* keyType{};
	Type const* valueType{};
	if (auto mapType = to<MappingType>(type)) {
		keyType = mapType->keyType();
		valueType = mapType->valueType();
	} else {
		solUnimplemented("");
	}
	return {keyType, valueType};
}

std::vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract) {
	std::vector<FunctionDefinition const*> result;
	auto contracts = contract->annotation().linearizedBaseContracts;
	std::ranges::reverse(contracts);
	return contracts;
}

std::vector<VariableDeclaration const*> stateVariables(ContractDefinition const* _contract, StateVarType stateVarType) {
	std::vector<VariableDeclaration const*> variableDeclarations;
	std::vector<ContractDefinition const*> mainChain = getContractsChain(_contract);
	for (ContractDefinition const* contract: mainChain)
		for (VariableDeclaration const* variable: contract->stateVariables())
			if (!variable->isConstant()) {
				bool isTransient = variable->isTransient();
				bool isUnpacked = variable->isUnpacked();
				bool ok = false;
				switch (stateVarType) {
				case StateVarType::Usual:
					ok = !isTransient && !isUnpacked;
					break;
				case StateVarType::Transient:
					ok = isTransient;
					break;
				case StateVarType::Unpacked:
					ok = isUnpacked;
					break;
				}
				if (ok) {
					variableDeclarations.push_back(variable);
				}
			}
	return variableDeclarations;
}

bool isSuper(Expression const* expr) {
	if (auto identifier = to<Identifier>(expr)) {
		return identifier->name() == "super";
	}
	return false;
}

bool isAddressThis(FunctionCall const* funCall) {
	if (!funCall)
		return false;
	auto arguments = funCall->arguments();
	if (auto etn = to<ElementaryTypeNameExpression>(&funCall->expression())) {
		if (etn->type().typeName().token() == Token::Address) {
			solAssert(!arguments.empty(), "");
			if (auto arg0 = to<Identifier>(arguments[0].get())) {
				if (arg0->name() == "this")
					return true;
			}
		}
	}
	return false;
}

CallableDeclaration const* getFunctionDeclarationOrConstructor(Expression const* expr, bool quiet) {
	auto f = to<FunctionType>(expr->annotation().type);
	if (f) {
		return to<CallableDeclaration>(&f->declaration());
	}
	auto tt = dynamic_cast<TypeType const*>(expr->annotation().type);
	if (quiet && !tt) {
		return nullptr;
	}
	solAssert(tt, "");
	auto contractType = dynamic_cast<ContractType const*>(tt->actualType());
	if (quiet && !contractType) {
		return nullptr;
	}
	solAssert(contractType, "");
	return contractType->contractDefinition().constructor(); // null if no constructor
}

bool isEmptyFunction(FunctionDefinition const* f) {
	return f == nullptr || (f->modifiers().empty() && f->body().statements().empty());
}

std::vector<VariableDeclaration const*> convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<VariableDeclaration const*> ret;
	ret.reserve(arr.size());
	for (auto const& v: arr)
		ret.emplace_back(v.get());
	return ret;
}

std::vector<Type const*> getTypesFromVarDecls(std::vector<VariableDeclaration const*> const& arr) {
	std::vector<Type const*> ret;
	ret.reserve(arr.size());
	for (auto const& v: arr)
		ret.emplace_back(v->type());
	return ret;
}

std::vector<Type const*> getTypesFromVarDecls(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<Type const*> ret;
	ret.reserve(arr.size());
	for (auto const& v: arr)
		ret.emplace_back(v->type());
	return ret;
}

std::pair<std::vector<Type const*>, std::vector<std::string>> getTupleTypes(TupleType const* tuple) {
	std::vector<std::string> names;
	std::vector<Type const*> types;
	int i = 0;
	for (Type const* comp: tuple->components()) {
		types.emplace_back(comp);
		names.emplace_back(std::to_string(i));

		++i;
	}
	return {types, names};
}

DictValueType toDictValueType(Type::Category const& category) {
	switch (category) {
	case Type::Category::Address:
	case Type::Category::AddressStd:
		return DictValueType::Address;
	case Type::Category::Array:
		return DictValueType::Array;
	case Type::Category::Bool:
		return DictValueType::Bool;
	case Type::Category::Contract:
		return DictValueType::Contract;
	case Type::Category::Enum:
		return DictValueType::Enum;
	case Type::Category::FixedBytes:
		return DictValueType::FixedBytes;
	case Type::Category::Integer:
		return DictValueType::Integer;
	case Type::Category::Mapping:
		return DictValueType::Mapping;
	case Type::Category::Optional:
		return DictValueType::Optional;
	case Type::Category::Struct:
		return DictValueType::Struct;
	case Type::Category::TvmCell:
		return DictValueType::TvmCell;
	case Type::Category::TvmSlice:
		return DictValueType::TvmSlice;
	case Type::Category::VarInteger:
		return DictValueType::VarInteger;
	case Type::Category::Function:
		return DictValueType::Function;
	case Type::Category::FixedPoint:
		return DictValueType::FixedPoint;
	default:
		solUnimplemented("");
	}
}

std::set<CallableDeclaration const*> getAllBaseFunctions(CallableDeclaration const* f) {
	std::set<CallableDeclaration const*> res;
	for (CallableDeclaration const* base: f->annotation().baseFunctions) {
		res.insert(base);
		std::set<CallableDeclaration const*> cur = getAllBaseFunctions(base);
		res.insert(cur.begin(), cur.end());
	}
	return res;
}

ABITypeSize::ABITypeSize(Type const* _type) {
	if (_type->category() == Type::Category::AddressStd) {
		maxBits = AddressInfo::stdAddrWithAnyCastLength();
		maxRefs = 0;
	} else if (isAddressOrAddressStdOrContractType(_type)) {
		maxBits = AddressInfo::maxBitLength();
		maxRefs = 0;
	} else if (auto varint = to<VarIntegerType>(_type)) {
		maxBits = varint->maxBitSizeInCell();
		maxRefs = 0;
	} else if (isIntegralType(_type)) {
		TypeInfo ti{_type};
		solAssert(ti.isNumeric, "");
		maxBits = ti.numBits;
		maxRefs = 0;
	} else if (auto arrayType = to<ArrayType>(_type)) {
		if (arrayType->isByteArrayOrString()) {
			maxBits = 0;
			maxRefs = 1;
		} else {
			maxBits = 32 + 1;
			maxRefs = 1;
		}
	} else if (to<TvmCellType>(_type)) {
		maxBits = 0;
		maxRefs = 1;
	} else if (auto opt = to<OptionalType>(_type)) {
		if (isSmallOptional(opt)) {
			ABITypeSize size{opt->valueType()};
			maxBits = 1 + size.maxBits;
			maxRefs = size.maxRefs;
		} else {
			maxBits = 1;
			maxRefs = 1;
		}
	} else if (auto st = to<StructType>(_type)) {
		maxBits = 0;
		maxRefs = 0;
		for (auto const& t: st->structDefinition().members()) {
			ABITypeSize size{t->type()};
			maxBits += size.maxBits;
			maxRefs += size.maxRefs;
		}
	} else if (auto tup = to<TupleType>(_type)) {
		maxBits = 0;
		maxRefs = 0;
		for (auto t: tup->components()) {
			ABITypeSize size{t};
			maxBits += size.maxBits;
			maxRefs += size.maxRefs;
		}
	} else if (to<MappingType>(_type)) {
		maxBits = 1;
		maxRefs = 1;
	} else if (to<FunctionType>(_type)) {
		maxBits = 32;
		maxRefs = 0;
	} else if (to<TvmSliceType>(_type) || to<TvmBuilderType>(_type)) {
		maxBits = 1023;
		maxRefs = 3;
	} else if (auto userDefType = to<UserDefinedValueType>(_type)) {
		*this = ABITypeSize{&userDefType->underlyingType()};
	} else {
		solUnimplemented("Undefined type: " + _type->toString());
	}

	fixedSize = to<IntegerType>(_type) ||
				to<QIntegerType>(_type) ||
				to<BoolType>(_type) ||
				to<QBoolType>(_type) ||
				to<EnumType>(_type) ||
				to<TvmCellType>(_type) ||
				to<FunctionType>(_type) ||
				to<FixedBytesType>(_type) ||
				to<FixedPointType>(_type);

	fixedRefs = fixedSize || to<AddressType>(_type) || to<ContractType>(_type);

	solAssert(maxBits != -1);
	solAssert(maxRefs != -1);
}

int strToInt(std::string const& str) {
	std::string const& trimed = boost::algorithm::trim_copy(str);
	return boost::lexical_cast<int>(trimed);
}

int qtyWithoutLoc(
	std::vector<Pointer<TvmAstNode>>::const_iterator beg,
	std::vector<Pointer<TvmAstNode>>::const_iterator end
) {
	int qty = 0;
	for (auto it = beg; it != end; ++it) {
		if (!convertToLoc(it->get())) {
			++qty;
		}
	}
	return qty;
}

int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> const& arr) { return qtyWithoutLoc(arr.begin(), arr.end()); }

void trimLoc(std::vector<Pointer<TvmAstNode>>& arr) {
	while (!arr.empty() && convertToLoc(arr.back().get())) {
		arr.pop_back();
	}
}

std::optional<std::string> StrUtils::toBitString(bigint value, int bitlen, bool isSign) {
	if (bitlen == 0) {
		if (value == 0)
			return "";
		return {};
	}
	bigint const one = 1;
	if (isSign) {
		bigint p2 = one << (bitlen - 1);
		if (!(-p2 <= value && value <= p2 - 1))
			return {};
	} else {
		bigint p2 = one << bitlen;
		if (!(0 <= value && value < p2))
			return {};
	}
	if (value < 0) {
		value = pow(bigint(2), bitlen) + value;
		solAssert(value > 0, "");
	}
	std::string s;
	for (int i = 0; i < bitlen; ++i) {
		s += value % 2 == 0 ? "0" : "1";
		value /= 2;
	}
	solAssert(value == 0, "");
	std::reverse(s.rbegin(), s.rbegin() + bitlen);
	return s;
}

std::string StrUtils::binaryStringToSlice(std::string const& _s) {
	std::string s = _s;
	bool haveCompletionTag = false;
	if (s.size() % 4 != 0) {
		haveCompletionTag = true;
		s += "1";
		s += std::string((4 - s.size() % 4) % 4, '0');
	}
	std::string ans;
	for (int i = 0; i < static_cast<int>(s.length()); i += 4) {
		int x = stoi(s.substr(i, 4), nullptr, 2);
		std::stringstream sstream;
		sstream << std::hex << x;
		ans += sstream.str();
	}
	if (haveCompletionTag) {
		ans += "_";
	}
	return ans;
}

std::string StrUtils::toBitString(std::string const& slice) {
	std::string bitString;
	if (slice.at(0) == 'x') {
		for (std::size_t i = 1; i < slice.size(); ++i) {
			if (slice.at(i) == '_') {
				while (!bitString.empty() && *bitString.rbegin() == '0') // trim last zeroes
					bitString.pop_back();
				if (!bitString.empty()) // trim last one
					bitString.pop_back();
				solAssert(i + 1 == slice.size(), "");
			} else {
				size_t pos{};
				auto sss = slice.substr(i, 1);
				int value = std::stoi(sss, &pos, 16);
				solAssert(pos == 1, "");
				bitString += StrUtils::toBitString(value, 4, false).value();
			}
		}
	} else if (isIn(slice, "0", "1")) {
		bitString = slice;
	} else {
		solUnimplemented("");
	}
	return bitString;
}

std::optional<std::string> StrUtils::unitSlices(std::string const& sliceA, std::string const& sliceB) {
	return unitBitStringToHex(toBitString(sliceA), toBitString(sliceB));
}

std::optional<std::string> StrUtils::unitBitStringToHex(std::string const& bitStringA, std::string const& bitStringB) {
	std::string const& bitString = bitStringA + bitStringB;
	if (bitString.length() > TvmConst::CellBitLength) {
		// TODO implement
		return std::nullopt;
	}
	return {"x" + StrUtils::binaryStringToSlice(bitString)};
}

std::string StrUtils::tonsToBinaryString(u256 const& value) { return tonsToBinaryString(bigint(value)); }

std::string StrUtils::tonsToBinaryString(bigint value) {
	std::string s;
	int len = 256;
	for (int i = 0; i < 256; ++i) {
		if (value == 0) {
			len = i;
			break;
		}
		s += value % 2 == 0 ? "0" : "1";
		value /= 2;
	}
	solAssert(len <= 120, "coins value must fit into 120 bit");
	while (len % 8 != 0) {
		s += "0";
		len++;
	}
	std::reverse(s.rbegin(), s.rbegin() + len);
	len = len / 8;
	std::string res;
	for (int i = 0; i < 4; ++i) {
		res += len % 2 == 0 ? "0" : "1";
		len /= 2;
	}
	std::reverse(res.rbegin(), res.rbegin() + 4);
	return res + s;
}

std::string StrUtils::boolToBinaryString(bool value) { return value ? "1" : "0"; }

std::string StrUtils::literalToSliceAddress(bigint const& value) {
	// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt;
	std::string s;
	s += "10";
	s += "0";
	s += std::string(8, '0');
	s += StrUtils::toBitString(value, 256, false).value();
	return s;
}

bigint StrUtils::toBigint(std::string const& binStr) {
	bigint res;
	for (char ch: binStr) {
		res <<= 1;
		if (ch == '1')
			++res;
	}
	return res;
}

std::optional<bigint> StrUtils::toNegBigint(std::string const& binStr) {
	if (binStr.at(0) != '1')
		return {};
	bigint res = StrUtils::toBigint(binStr) - (bigint(1) << binStr.length());
	return res;
}

std::string StrUtils::toBinString(bigint num) {
	solAssert(num >= 0, "");
	std::string res;
	while (num != 0) {
		res += num % 2 == 0 ? "0" : "1";
		num /= 2;
	}
	std::ranges::reverse(res);
	return res;
}

// e.g.: hello -> 68656c6c6f
std::string StrUtils::stringToHex(std::string const& str) {
	std::string slice;
	for (char index: str) {
		std::stringstream ss;
		ss << std::hex << std::setfill('0') << std::setw(2) << (static_cast<unsigned>(index) & 0xFFu);
		slice += ss.str();
	}
	return slice;
}

std::string StrUtils::intToHex(uint32_t id) {
	std::ostringstream oss;
	oss << std::hex << std::setfill('0') << std::setw(8) << id;
	return oss.str();
}

std::optional<bigint> ExprUtils::constValue(Expression const& _e) {
	// TODO see ConstantEvaluator ?
	if (*_e.annotation().isPure) {
		if (auto memberAccess = to<MemberAccess>(&_e)) {
			if (auto variable =
					dynamic_cast<VariableDeclaration const*>(memberAccess->annotation().referencedDeclaration)) {
				return constValue(*variable->value());
			}
		}

		if (auto ident = to<Identifier>(&_e)) {
			IdentifierAnnotation& identifierAnnotation = ident->annotation();
			auto const* variable = to<VariableDeclaration>(identifierAnnotation.referencedDeclaration);
			if (variable) {
				return constValue(*variable->value());
			}
		}
	}

	if (_e.annotation().type->category() == Type::Category::RationalNumber) {
		auto number = dynamic_cast<RationalNumberType const*>(_e.annotation().type);
		solAssert(number, "");
		bigint val = number->value2();
		return val;
	}

	return {};
}

std::optional<bool> ExprUtils::constBool(Expression const& _e) {
	auto l = to<Literal>(&_e);
	if (l != nullptr && isIn(l->token(), Token::TrueLiteral, Token::FalseLiteral)) {
		return l->token() == Token::TrueLiteral;
	}
	return {};
}

std::map<bigint, int> const& MathConsts::power2Exp() {
	static std::map<bigint, int> power2Exp;
	if (power2Exp.empty()) {
		bigint p2 = 1;
		for (int p = 0; p <= 256; ++p) {
			power2Exp[p2] = p;
			p2 *= 2;
		}
	}
	return power2Exp;
}

std::map<bigint, int> const& MathConsts::power2DecExp() {
	static std::map<bigint, int> power2DecExp;
	if (power2DecExp.empty()) {
		bigint p2 = 1;
		for (int p = 0; p <= 256; ++p) {
			power2DecExp[p2 - 1] = p;
			p2 *= 2;
		}
	}
	return power2DecExp;
}

std::map<bigint, int> const& MathConsts::power2NegExp() {
	static std::map<bigint, int> power2NegExp;
	if (power2NegExp.empty()) {
		bigint p2 = 1;
		for (int p = 0; p <= 256; ++p) {
			power2NegExp[-p2] = p;
			p2 *= 2;
		}
	}
	return power2NegExp;
}

std::map<int, bigint> const& MathConsts::power10() {
	static std::map<int, bigint> power10;
	if (power10.empty()) {
		bigint p10 = 1;
		for (int i = 0; i <= 80; ++i) {
			power10[i] = p10;
			p10 *= 10;
		}
	}
	return power10;
}

namespace {
std::pair<bool, int> getSignAndBits(Type const* type) {
	bool isSigned = false;
	int numBits = 0;
	if (auto intResult = to<IntegerType>(type)) {
		isSigned = intResult->isSigned();
		numBits = intResult->numBits();
	} else if (auto qintResult = to<QIntegerType>(type)) {
		isSigned = qintResult->asIntegerType()->isSigned();
		numBits = qintResult->asIntegerType()->numBits();
	} else if (auto varintResult = to<VarIntegerType>(type)) {
		isSigned = varintResult->asIntegerType().isSigned();
		numBits = varintResult->asIntegerType().numBits();
	} else if (auto fixedResult = to<FixedPointType>(type)) {
		isSigned = fixedResult->isSigned();
		numBits = fixedResult->numBits();
	} else
		solUnimplemented(type->toString());
	return {isSigned, numBits};
}
}

bool isFitUselessUnary(Type const* common, Token op) {
	auto const [isSigned, numBits] = getSignAndBits(common);
	return (!isSigned && numBits == 256 && op == Token::Inc);
}

bool isFitUseless(Type const* left, Type const* right, Type const* common, Token op) {
	bool const isLeftSigned = getSignAndBits(left->mobileType()).first;
	bool const isRightSigned = getSignAndBits(right->mobileType()).first;
	auto const [isSigned, numBits] = getSignAndBits(common);
	return (!isSigned && numBits == 256 && isIn(op, Token::Add, Token::Exp, Token::Mul, Token::SHL)) ||
		   (
			   // we should throw an overflow exception if case of type(SignType).min / -1,
			   // e.g. -128/-1 does not fit into int8
			   (!isLeftSigned || !isRightSigned) && op == Token::Div
		   );
}

bool isInRange257(bigint const& value) {
	bigint maxUint256 = bigint(1) << 256;
	return -maxUint256 <= value && value < maxUint256;
}

// { width: 16, poly: 0x1021, init: 0x0000, refin: false, refout: false,
// xorout: 0x0000, check: 0x31c3, residue: 0x0000 };
/*
  Name  : CRC-16 CCITT
  Poly  : 0x1021    x^16 + x^12 + x^5 + 1
  Init  : 0x0000
  Revert: false
  XorOut: 0x0000
  MaxLen: 4095 байт (32767 бит)
*/
namespace {
unsigned short crc16(char const* pcBlock, unsigned short len) {
	unsigned short crc = 0;

	while (len--) {
		crc ^= static_cast<unsigned char>(*pcBlock++) << 8;

		for (unsigned char i = 0; i < 8; i++)
			crc = crc & 0x8000 ? (crc << 1) ^ 0x1021 : crc << 1;
	}
	return crc;
}
}


unsigned short crc16(std::string const& str) { return crc16(str.c_str(), str.size()); }

std::vector<ArithmeticOperation> tonCombinedArithmeticOperations() {
	static std::vector<ArithmeticOperation> answer;
	if (answer.empty()) {
		for (auto const& oper: std::vector<ArithmeticOperation>{
				 {"mulAddDivMod", 4, 2, false},
				 {"addDivMod", 3, 2, false},
				 {"addRShiftMod", 3, 2, true},
				 {"lShiftAddDivMod", 4, 2, true},
			 }) {
			for (auto const& [pref, type]: std::vector<std::tuple<std::string, Type const*>>{
					 {"", TypeProvider::int257()},
					 {"q", TypeProvider::qInteger(257, IntegerType::Modifier::Signed)},
				 }) {
				auto name = oper.name;
				if (pref == "q") {
					name = toUpper(oper.name.at(0)) + oper.name.substr(1);
				}
				for (auto const& suf: std::vector<std::string>{"", "R", "C"}) {
					answer.emplace_back(ArithmeticOperation{pref + name + suf, oper.take, oper.ret, oper.withRShift});
				}
			}
		}

		for (auto const& oper: std::vector<ArithmeticOperation>{
				 {"mulAddRShift", 4, 2, true},
			 }) {
			for (auto const& [pref, type]: std::vector<std::tuple<std::string, Type const*>>{
					 {"", TypeProvider::int257()},
					 {"q", TypeProvider::qInteger(257, IntegerType::Modifier::Signed)},
				 }) {
				auto name = oper.name;
				if (pref == "q") {
					name = toUpper(oper.name.at(0)) + oper.name.substr(1);
				}
				for (auto const& suf: std::vector<std::string>{"Mod", "RMod", "CMod"}) {
					answer.emplace_back(ArithmeticOperation{pref + name + suf, oper.take, oper.ret, oper.withRShift});
				}
			}
		}
	}
	return answer;
}

FunctionDefinition const* getRemoteFunctionDefinition(MemberAccess const* memberAccess) {
	auto expr = &memberAccess->expression();
	if (isSuper(expr))
		return nullptr;
	auto ctype = to<ContractType>(getType(expr));
	if (!ctype)
		return nullptr;
	Declaration const* decl = memberAccess->annotation().referencedDeclaration;
	auto f = to<FunctionDefinition>(decl);
	if (!f) {
		cast_error(*memberAccess, "Unsupported remote function call.");
	}
	return f;
}

} // end namespace solidity::frontend
