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
 * Common TVM codegen routines, in particular, types, data structures, scope, stack manipulations, etc.
 */

#pragma once

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/SourceReferenceFormatter.h>

#include <functional>
#include <regex>
#include <typeinfo>
#include <json/json.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <utility>

#include "TvmAst.hpp"

#define DBG(x) cout << (x) << endl;

namespace solidity::frontend {

template <typename T> using ast_vec = std::vector<ASTPointer<T>>;

template <typename T1, typename T2>
T1 const* to(T2 const* ptr) { return dynamic_cast<T1 const*>(ptr); }

template<typename T, typename... Args>
constexpr bool isIn(T v, Args... args) {
	return (... || (v == (args)));
}

constexpr uint64_t str2int(const char* str, int i = 0) {
	return !str[i] ? 5381 : (str2int(str, i+1) * 33) ^ str[i];
}

std::string functionName(FunctionDefinition const* _function);

bool isAddressOrContractType(const Type* type);
bool isUsualArray(const Type* type);
bool isByteArrayOrString(const Type* type);
bool isString(const Type* type);
bool isSlice(const Type* type);
bool isSmallOptional(OptionalType const* type);
bool optValueAsTuple(Type const* optValueType);
int optTypeQty(Type const* type);

struct AddressInfo {

	static int stdAddrWithoutAnyCastLength() {
		// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt
		return 2 + 1 + 8 + 256;
	}

	static int minBitLength() {
		// addr_none$00 = MsgAddressExt;
		return 2;
	}

	static int maxBitLength() {
		// anycast_info$_ depth:(#<= 30) { depth >= 1 }
		// rewrite_pfx:(bits depth) = Anycast;

		// addr_var$11 anycast:(Maybe Anycast) addr_len:(## 9)
		// workchain_id:int32 address:(bits addr_len) = MsgAddressInt;

		// 2 +  // 11
		// 1 + 5 + 30 + // anycast
		// 9 + // addr_len
		// 32 + // workchain_id:int32
		// 512 // address
		return 591;
	}
};

struct VarUIntegerInfo {
	static int maxTonBitLength() {
		return 4 + 15 * 8;
		// var_uint$_ {n:#} len:(#< n) value:(uint (len * 8)) = VarUInteger n;
		// nanograms$_ amount:(VarUInteger 16) = Grams;
	}
};

int bitsForEnum(size_t val_count);

struct TypeInfo {
	bool isNumeric{};
	bool isSigned{};
	int numBits{};
	Type::Category category{};

	explicit TypeInfo(const Type* type) {
		if (auto* integerType = to<IntegerType>(type)) {
			isNumeric = true;
			isSigned = integerType->isSigned();
			numBits = integerType->numBits();
		} else if (to<BoolType>(type)) {
			isNumeric = true;
			isSigned = true;
			numBits = 1;
		} else if (auto* fixedBytesType = to<FixedBytesType>(type)) {
			isNumeric = true;
			isSigned = false;
			numBits = 8 * static_cast<int>(fixedBytesType->numBytes());
		} else if (auto enumType = to<EnumType>(type)) {
			isNumeric = true;
			isSigned = false;
			numBits = bitsForEnum(enumType->numberOfMembers());
		} else if (auto* fp = to<FixedPointType>(type)) {
			isNumeric = true;
			isSigned = fp->isSigned();
			numBits = fp->numBits();
		}
		category = type->category();
	}
};

const Type* getType(const Expression* expr);

const Type* getType(const VariableDeclaration* var);

bool isIntegralType(const Type* type);

bool isStringOrStringLiteralOrBytes(const Type* type);

bool isRefType(const Type* type);

std::string typeToDictChar(Type const* keyType);

int lengthOfDictKey(Type const* key);

IntegerType getKeyTypeOfC4();

IntegerType getKeyTypeOfArray();

std::tuple<Type const*, Type const*>
dictKeyValue(Type const* type);

std::vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract);

std::vector<std::pair<FunctionDefinition const*, ContractDefinition const*>>
getContractFunctionPairs(ContractDefinition const* contract);

const FunctionDefinition* getFunction(ContractDefinition const* contract, const std::string& functionName);

bool isSuper(Expression const* expr);

bool isMacro(const std::string& functionName);

bool isAddressThis(const FunctionCall* funCall);

// List of all function but constructors with a given name
std::vector<FunctionDefinition const*> getContractFunctions(ContractDefinition const* contract, const std::string& funcName);

const ContractDefinition* getSuperContract(const ContractDefinition* currentContract,
										   const ContractDefinition* mainContract,
										   const std::string& fname);

[[noreturn]]
void cast_error(const ASTNode& node, const std::string& error_message);

[[noreturn]]
void fatal_error(const std::string &error_message);

class PragmaDirectiveHelper {
public:
	explicit PragmaDirectiveHelper(std::vector<PragmaDirective const *> const& _pragmaDirectives) :
			pragmaDirectives{_pragmaDirectives} {
	}

	bool havePubkey() const {
		return std::get<0>(haveHeader("pubkey"));
	}

	bool haveTime() const {
		return std::get<0>(haveHeader("time"));
	}

	bool haveExpire() const {
		return std::get<0>(haveHeader("expire"));
	}

	std::tuple<bool, PragmaDirective const *> haveHeader(const std::string& str) const {
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 2 &&
				pd->literals()[0] == "AbiHeader" &&
				pd->literals()[1] == str) {
				return {true, pd};
			}
		}
		return {false, nullptr};
	}

	bool haveIgnoreIntOverflow() const {
		return std::any_of(pragmaDirectives.begin(), pragmaDirectives.end(), [](const auto& pd){
			return pd->literals().size() == 1 && pd->literals()[0] == "ignoreIntOverflow";
		});
	}

	ASTPointer<Expression> haveMsgValue() const {
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 1 &&
				pd->literals()[0] == "msgValue") {
				return pd->parameter();
			}
		}
		return nullptr;
	}
private:
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};


struct ABITypeSize {
	int maxBits = -1;
	int maxRefs = -1;

	explicit ABITypeSize(Type const* type);
};

inline std::pair<std::vector<Type const*>, std::vector<ASTNode const*>>
getParams(const std::vector<VariableDeclaration const*>& params, size_t offset = 0) {
	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (auto it = params.begin() + offset; it != params.end(); it++) {
		types.push_back(getType(*it));
		nodes.push_back(*it);
	}
	return std::make_pair(types, nodes);
}


template<typename T>
std::pair<std::vector<Type const*>, std::vector<ASTNode const*>>
getParams(const ast_vec<T>& params, size_t offset = 0) {
	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (auto it = params.begin() + offset; it != params.end(); it++) {
		types.push_back(getType(it->get()));
		nodes.push_back(it->get());
	}
	return std::make_pair(types, nodes);
}

CallableDeclaration const * getFunctionDeclarationOrConstructor(Expression const* expr, bool quiet = false);

bool isEmptyFunction(FunctionDefinition const* f);

// TODO delete
enum class LocationReturn {
	noReturn,
	Last,
	Anywhere
};

struct ControlFlowInfo {
	int stackSize {-1};
	bool doAnalyzeFlag {false};
	bool isLoop {false};
};

LocationReturn notNeedsPushContWhenInlining(Block const& _block);

std::vector<VariableDeclaration const*>
convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr);

std::vector<Type const*>
getTypesFromVarDecls(std::vector<ASTPointer<VariableDeclaration>> const& arr);

std::pair<
	std::vector<Type const*>,
	std::vector<std::string>
>
getTupleTypes(TupleType const* tuple);

enum class DataType {
	Builder,
	Cell,
	Slice
};

enum class DictValueType {
	Address,
	Array,
	Bool,
	Contract,
	Enum,
	ExtraCurrencyCollection,
	FixedBytes,
	FixedPoint,
	Function,
	Integer,
	Mapping,
	Optional,
	Struct,
	TvmCell,
	TvmSlice,
	VarInteger
};

enum class GetDictOperation {
	GetFromMapping,
	GetSetFromMapping,
	GetAddFromMapping,
	GetReplaceFromMapping,
	GetFromArray,
	Fetch,
	Exist
};

enum class SetDictOperation { Set, Replace, Add };

struct LValueInfo {
	std::vector<Expression const*> expressions;
	bool doesntNeedToCollect = false;
};

DictValueType toDictValueType(const Type::Category& category);
std::string stringToBytes(const std::string& str);
std::set<CallableDeclaration const*> getAllBaseFunctions(CallableDeclaration const* f);
bool isLoc(Pointer<TvmAstNode> const& node);
std::vector<std::string> split(const std::string &s, char sep = '\n');
int strToInt(const std::string& str);
int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>>::const_iterator beg,
				  std::vector<Pointer<TvmAstNode>>::const_iterator end);
int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> const& arr);

namespace StrUtils {
	std::string toBitString(bigint value, int bitlen = 256);
	std::string binaryStringToSlice(const std::string & s);
	std::string toBitString(const std::string& slice);
	std::optional<std::string> unitSlices(const std::string& sliceA, const std::string& sliceB);
	std::optional<std::string> unitBitStringToHex(const std::string& bitStringA, const std::string& bitStringB);
	std::string tonsToBinaryString(const u256& value);
	std::string tonsToBinaryString(bigint value);
	std::string boolToBinaryString(bool value);
	std::string literalToSliceAddress(Literal const* literal);
}

} // end solidity::frontend
