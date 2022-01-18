/*
 * Copyright 2018-2021 TON DEV SOLUTIONS LTD.
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
 * Common TVM codegen routines, in particular, types, data structures, scope, stack manipulations, etc.
 */

#include <boost/algorithm/string/trim.hpp>

#include "TVM.h"
#include "TVMCommons.hpp"
#include "TVMConstants.hpp"

using namespace std;
using namespace solidity::langutil;
using namespace solidity::util;

namespace solidity::frontend {

std::string functionName(FunctionDefinition const *_function) {
	if (_function->isConstructor()) {
		return _function->annotation().contract->name();
	}

	if (_function->isReceive()) {
		return "receive";
	}
	if (_function->isFallback()) {
		return "fallback";
	}
	if (_function->isOnBounce()) {
		return "onBounce";
	}
	return _function->name();
}

void cast_error(const ASTNode &node, const string &error_message) {
	GlobalParams::g_errorReporter->fatalParserError(node.location(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

void fatal_error(const string &error_message) {
	GlobalParams::g_errorReporter->error(Error::Type::TypeError, SourceLocation(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

const ContractDefinition *
getSuperContract(const ContractDefinition *currentContract, const ContractDefinition *mainContract, const string &fname) {
	ContractDefinition const* prev = nullptr;
	for (auto c : getContractsChain(mainContract)) {
		if (c == currentContract)
			break;
		if (getFunction(c, fname))
			prev = c;
	}
	return prev;
}

const Type *getType(const VariableDeclaration *var) {
	return var->annotation().type;
}

bool isAddressOrContractType(const Type *type) {
	return to<AddressType>(type) || to<ContractType>(type);
}

bool isUsualArray(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && !arrayType->isByteArray();
}

bool isByteArrayOrString(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && arrayType->isByteArray();
}

bool isString(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isString());
}

bool isSlice(const Type * type) {
	return to<TvmSliceType>(type) != nullptr;
}

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

const Type *getType(const Expression *expr) {
	return expr->annotation().type;
}

bool isIntegralType(const Type *type) {
	return TypeInfo(type).isNumeric;
}

bool isStringOrStringLiteralOrBytes(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isByteArray());
}

bool isRefType(Type const* t) {
	return (t->category() == Type::Category::Array && to<ArrayType>(t)->isByteArray()) ||
			t->category() == Type::Category::TvmCell;
}

std::string typeToDictChar(Type const *keyType) {
	TypeInfo ti(keyType);
	if (ti.isNumeric) {
		return ti.isSigned? "I" : "U";
	} else if (isStringOrStringLiteralOrBytes(keyType) || keyType->category() == Type::Category::TvmCell) {
		return "U";
	}
	return ""; // dict key is slice
}

int lengthOfDictKey(Type const *key) {
	if (isIn(key->category(), Type::Category::Address, Type::Category::Contract)) {
		return AddressInfo::stdAddrWithoutAnyCastLength();
	}

	TypeInfo ti{key};
	if (ti.isNumeric) {
		return ti.numBits;
	}

	if (isStringOrStringLiteralOrBytes(key) || key->category() == Type::Category::TvmCell){
		return 256; // hash of tree of cells
	}

    auto structType = to<StructType>(key);
	if (structType) {
        int bitLength = 0;
        StructDefinition const &structDefinition = structType->structDefinition();
        for (const auto &member : structDefinition.members()) {
            TypeInfo ti2{member->type()};
            solAssert(ti2.isNumeric, "");
            bitLength += ti2.numBits;
        }
        return bitLength;
    }
	solUnimplemented("");
}

IntegerType getKeyTypeOfC4() {
	return IntegerType(TvmConst::C4::KeyLength);
}

IntegerType getKeyTypeOfArray() {
	return IntegerType(TvmConst::ArrayKeyLength);
}

std::tuple<Type const*, Type const*>
dictKeyValue(Type const* type) {
	Type const* keyType{};
	Type const* valueType{};
	if (auto mapType = to<MappingType>(type)) {
		keyType = mapType->keyType();
		valueType = mapType->valueType();
	} else if (auto ccType = to<ExtraCurrencyCollectionType>(type)) {
		keyType = ccType->keyType();
		valueType = ccType->realValueType();
	} else {
		solUnimplemented("");
	}
	return {keyType, valueType};
}

vector<ContractDefinition const *> getContractsChain(ContractDefinition const *contract) {
	vector<FunctionDefinition const*> result;
	auto contracts = contract->annotation().linearizedBaseContracts;
	std::reverse(contracts.begin(), contracts.end());
	return contracts;
}

vector<std::pair<FunctionDefinition const *, ContractDefinition const *>>
getContractFunctionPairs(ContractDefinition const *contract) {
	vector<pair<FunctionDefinition const*, ContractDefinition const*>> result;
	for (ContractDefinition const* c : getContractsChain(contract)) {
		for (const auto f : c->definedFunctions())
			result.emplace_back(f, c);
	}
	return result;
}

const FunctionDefinition *getFunction(ContractDefinition const *contract, const string &functionName) {
	const FunctionDefinition* result = nullptr;
	for (const auto f : contract->definedFunctions()) {
		if (f->name() == functionName)
			return f;
	}
	return result;
}

bool isSuper(Expression const *expr) {
	if (auto identifier = to<Identifier>(expr)) {
		return identifier->name() == "super";
	}
	return false;
}

bool isMacro(const std::string &functionName) {
	return boost::ends_with(functionName, "_macro");
}

bool isAddressThis(const FunctionCall *funCall) {
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

vector<FunctionDefinition const *> getContractFunctions(ContractDefinition const *contract, const string &funcName) {
	vector<FunctionDefinition const*> result;
	for (auto &[functionDefinition, contractDefinition] : getContractFunctionPairs(contract)) {
		(void)contractDefinition;	// suppress unused variable error
		if (functionDefinition->isConstructor()) {
			continue;
		}
		if (functionName(functionDefinition) == funcName)
			result.push_back(functionDefinition);
	}
	return result;
}

CallableDeclaration const* getFunctionDeclarationOrConstructor(Expression const* expr, bool quiet) {
	auto f = to<FunctionType>(expr->annotation().type);
	if (f) {
		return to<CallableDeclaration>(&f->declaration());
	}
	auto tt = dynamic_cast<const TypeType*>(expr->annotation().type);
	if (quiet && !tt) {
		return nullptr;
	}
	solAssert(tt, "");
	auto contractType = dynamic_cast<const ContractType*>(tt->actualType());
	if (quiet && !contractType) {
		return nullptr;
	}
	solAssert(contractType, "");
	return contractType->contractDefinition().constructor(); // null if no constructor
}

bool isEmptyFunction(FunctionDefinition const* f) {
	return f == nullptr || (f->modifiers().empty() && f->body().statements().empty());
}




std::vector<VariableDeclaration const*>
convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<VariableDeclaration const*>  ret;
	for (const auto& v : arr)
		ret.emplace_back(v.get());
	return ret;
}

std::vector<Type const*>
getTypesFromVarDecls(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<Type const*>  ret;
	for (const auto& v : arr)
		ret.emplace_back(v->type());
	return ret;
}

std::pair<
	std::vector<Type const*>,
	std::vector<std::string>
>
getTupleTypes(TupleType const* tuple) {
	std::vector<std::string> names;
	std::vector<Type const*> types;
	int i = 0;
	for (const TypePointer& comp : tuple->components()) {
		types.emplace_back(comp);
		names.emplace_back(to_string(i));

		++i;
	}
	return {types, names};
}

DictValueType toDictValueType(const Type::Category& category) {
	switch (category) {
		case Type::Category::Address:
			return DictValueType::Address;
		case Type::Category::Array:
			return DictValueType::Array;
		case Type::Category::Bool:
			return DictValueType::Bool;
		case Type::Category::Contract:
			return DictValueType::Contract;
		case Type::Category::Enum:
			return DictValueType::Enum;
		case Type::Category::ExtraCurrencyCollection:
			return DictValueType::ExtraCurrencyCollection;
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

std::string stringToBytes(const std::string& str) {
	std::string slice;
	for (char index : str) {
		std::stringstream ss;
		ss << std::hex << std::setfill('0') << std::setw(2)
			<< (static_cast<unsigned>(index) & 0xFFu);
		slice += ss.str();
	}
	return slice;
}

std::set<CallableDeclaration const*> getAllBaseFunctions(CallableDeclaration const* f) {
	std::set<CallableDeclaration const*> res;
	for (CallableDeclaration const* base : f->annotation().baseFunctions) {
		res.insert(base);
		std::set<CallableDeclaration const*> cur = getAllBaseFunctions(base);
		res.insert(cur.begin(), cur.end());
	}
	return res;
}


ABITypeSize::ABITypeSize(const Type *type) {
	if (isAddressOrContractType(type)){
		maxBits = AddressInfo::maxBitLength();
		maxRefs = 0;
	} else if (isIntegralType(type)) {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		maxBits = ti.numBits;
		maxRefs = 0;
	} else if (auto varInt = to<VarInteger>(type)) {
		maxBits = varInt->maxBitSizeInCell();
		maxRefs = 0;
	} else if (auto arrayType = to<ArrayType>(type)) {
		if (arrayType->isByteArray()) {
			maxBits = 0;
			maxRefs = 1;
		} else {
			maxBits = 32 + 1;
			maxRefs = 1;
		}
	} else if (to<TvmCellType>(type)) {
		maxBits = 0;
		maxRefs = 1;
	} else if (auto opt = to<OptionalType>(type)) {
		if (isSmallOptional(opt)) {
			ABITypeSize size{opt->valueType()};
			maxBits = 1 + size.maxBits;
			maxRefs = size.maxRefs;
		} else {
			maxBits = 1;
			maxRefs = 1;
		}
	} else if (auto st = to<StructType>(type)) {
		maxBits = 0;
		maxRefs = 0;
		for (auto t : st->structDefinition().members()) {
			ABITypeSize size{t->type()};
			maxBits += size.maxBits;
			maxRefs += size.maxRefs;
		}
	} else if (auto tup = to<TupleType>(type)) {
		maxBits = 0;
		maxRefs = 0;
		for (auto t : tup->components()) {
			ABITypeSize size{t};
			maxBits += size.maxBits;
			maxRefs += size.maxRefs;
		}
	} else if (to<MappingType>(type)) {
		maxBits = 1;
		maxRefs = 1;
	} else if (to<FunctionType>(type)) {
		maxBits = 32;
		maxRefs = 0;
	} else {
		solUnimplemented("Undefined type: " + type->toString());
	}
}

bool isLoc(Pointer<TvmAstNode> const& node) {
	auto c = dynamic_pointer_cast<Loc>(node);
	return c != nullptr;
}

vector<string> split (const string &s, char sep) {
	vector<string> result;
	stringstream ss (s);
	string item;

	while (getline (ss, item, sep)) {
		result.push_back (item);
	}

	return result;
}

int strToInt(const std::string& str) {
	const std::string& trimed = boost::algorithm::trim_copy(str);
	return boost::lexical_cast<int>(trimed);
}

int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>>::const_iterator beg,
				  std::vector<Pointer<TvmAstNode>>::const_iterator end) {
	int qty = 0;
	for (auto it = beg; it != end; ++it) {
		if (!to<Loc>((*it).get())){
			++qty;
		}
	}
	return qty;
}

int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> const& arr) {
	return qtyWithoutLoc(arr.begin(), arr.end());
}

std::string StrUtils::toBitString(bigint value, int bitlen) {
	solAssert(value >= 0, "");
	std::string s;
	for (int i = 0; i < bitlen; ++i) {
		s += value % 2 == 0? "0" : "1";
		value /= 2;
	}
	std::reverse(s.rbegin(), s.rbegin() + bitlen);
	return s;
}

std::string StrUtils::binaryStringToSlice(const std::string &_s) {
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

std::string StrUtils::toBitString(const std::string& slice) {
	std::string bitString;
	if (slice.at(0) == 'x') {
		for (std::size_t i = 1; i < slice.size(); ++i) {
			if (i + 2 == slice.size() && slice[i + 1] == '_') {
				size_t pos{};
				int value = std::stoi(slice.substr(i, 1), &pos, 16);
				solAssert(pos == 1, "");
				int bitLen = 4;
				while (true) {
					bool isOne = value % 2 == 1;
					--bitLen;
					value /= 2;
					if (isOne) {
						break;
					}
				}
				bitString += StrUtils::toBitString(value, bitLen);
				break;
			}
			size_t pos{};
			auto sss = slice.substr(i, 1);
			int value = std::stoi(sss, &pos, 16);
			solAssert(pos == 1, "");
			bitString += StrUtils::toBitString(value, 4);
		}
	} else {
		if (isIn(slice, "0", "1")) {
			return slice;
		}
		solUnimplemented("");
	}
	return bitString;
}

std::optional<std::string> StrUtils::unitSlices(const std::string& sliceA, const std::string& sliceB) {
	return unitBitStringToHex(toBitString(sliceA), toBitString(sliceB));
}

std::optional<std::string> StrUtils::unitBitStringToHex(const std::string& bitStringA, const std::string& bitStringB) {
	const std::string& bitString = bitStringA + bitStringB;
	if (bitString.length() > TvmConst::CellBitLength)
		return std::nullopt;
	return {"x" + StrUtils::binaryStringToSlice(bitString)};
}

std::string StrUtils::tonsToBinaryString(const u256& value) {
	return tonsToBinaryString(bigint(value));
}

std::string StrUtils::tonsToBinaryString(bigint value) {
	std::string s;
	int len = 256;
	for (int i = 0; i < 256; ++i) {
		if (value == 0) {
			len = i;
			break;
		}
		s += value % 2 == 0? "0" : "1";
		value /= 2;
	}
	solAssert(len < 120, "Ton value should fit 120 bit");
	while (len % 8 != 0) {
		s += "0";
		len++;
	}
	std::reverse(s.rbegin(), s.rbegin() + len);
	len = len/8;
	std::string res;
	for (int i = 0; i < 4; ++i) {
		res += len % 2 == 0? "0" : "1";
		len /= 2;
	}
	std::reverse(res.rbegin(), res.rbegin() + 4);
	return res + s;
}

std::string StrUtils::boolToBinaryString(bool value) {
	return value ? "1" : "0";
}

std::string StrUtils::literalToSliceAddress(Literal const* literal) {
	Type const *type = literal->annotation().type;
	u256 value = type->literalValue(literal);
	// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt;
	std::string s;
	s += "10";
	s += "0";
	s += std::string(8, '0');
	s += StrUtils::toBitString(value);
	return s;
}

} // end namespace solidity::frontend
