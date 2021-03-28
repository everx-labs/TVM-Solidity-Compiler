/*
 * Copyright 2018-2019 TON DEV SOLUTIONS LTD.
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

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;

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

bool ends_with(const string& str, const string& suffix);

std::string functionName(FunctionDefinition const* _function);


template <typename T>
static bool doesAlways(const Statement* st) {
	auto rec = [] (const Statement* s) {
		return doesAlways<T>(s);
	};
	if (to<T>(st))
		return true;

	if (
		to<Assignment>(st) ||
		to<Break>(st) ||
		to<Continue>(st) ||
		to<EmitStatement>(st) ||
		to<ExpressionStatement>(st) ||
		to<ForEachStatement>(st) ||
		to<ForStatement>(st) ||
		to<PlaceholderStatement>(st) ||
		to<Return>(st) ||
		to<VariableDeclarationStatement>(st) ||
		to<WhileStatement>(st)
	)
		return false;
	if (auto block = to<Block>(st)) {
		return std::any_of(block->statements().begin(), block->statements().end(), [&](const auto& s){
			return rec(s.get());
		});
	}
	if (auto ifStatement = to<IfStatement>(st)) {
		if (!ifStatement->falseStatement())
			return false;
		return rec(&ifStatement->trueStatement()) && rec(ifStatement->falseStatement());
	}
	solUnimplemented( string("Unsupported statement type: ") + typeid(*st).name());
}

bool isAddressOrContractType(const Type* type);

bool isUsualArray(const Type* type);

bool isByteArrayOrString(const Type* type);

bool isString(const Type* type);

bool isSlice(const Type* type);

struct AddressInfo {

	static int stdAddrLength() {
		// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt
		return 2 + 1 + 8 + 256;
	}

	static int minBitLength() {
		// addr_none$00 = MsgAddressExt;
		return 2;
	}

	static int maxBitLength() {
		// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt
		// anycast_info$_ depth:(#<= 30) { depth >= 1 } rewrite_pfx:(bits depth) = Anycast;
		return 2 + 1 + (5 + 30) + 8 + 256;
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

bool isTvmIntrinsic(const string& name);

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

string storeIntegralOrAddress(const Type* type, bool reverse);

vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract);

vector<std::pair<FunctionDefinition const*, ContractDefinition const*>>
getContractFunctionPairs(ContractDefinition const* contract);

const FunctionDefinition* getFunction(ContractDefinition const* contract, const string& functionName);

bool isSuper(Expression const* expr);

bool isMacro(const std::string& functionName);

bool isAddressThis(const FunctionCall* funCall);

// List of all function but constructors with a given name
vector<FunctionDefinition const*> getContractFunctions(ContractDefinition const* contract, const string& funcName);

const ContractDefinition* getSuperContract(const ContractDefinition* currentContract,
										   const ContractDefinition* mainContract,
										   const string& fname);

[[noreturn]]
void cast_error(const ASTNode& node, const string& error_message);

void cast_warning(const ASTNode& node, const string& error_message);

[[noreturn]]
void fatal_error(const string &error_message);

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

	int abiVersion() const {
		return std::get<0>(haveHeader("v1"))? 1 : 2;
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
	int minBits = -1;
	int maxBits = -1;
	int minRefs = -1;
	int maxRefs = -1;

	explicit ABITypeSize(Type const* type, ASTNode const* node = nullptr) {
		if (isAddressOrContractType(type)){
			minBits = AddressInfo::minBitLength();
			maxBits = AddressInfo::maxBitLength();
			minRefs = 0;
			maxRefs = 0;
		} else if (isIntegralType(type)) {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			minBits = ti.numBits;
			maxBits = ti.numBits;
			minRefs = 0;
			maxRefs = 0;
		} else if (auto arrayType = to<ArrayType>(type)) {
			if (arrayType->isByteArray()) {
				minBits = 0;
				maxBits = 0;
				minRefs = 1;
				maxRefs = 1;
			} else {
				minBits = 32 + 1;
				maxBits = 32 + 1;
				minRefs = 0;
				maxRefs = 1;
			}
		} else if (to<TvmCellType>(type)) {
			minBits = 0;
			maxBits = 0;
			minRefs = 1;
			maxRefs = 1;
		} else if (to<MappingType>(type)) {
			minBits = 1;
			maxBits = 1;
			minRefs = 0;
			maxRefs = 1;
		} else {
			if (node)
				cast_error(*node, "Undefined type");
			else {
				solUnimplemented("Undefined type");
			}
		}
	}
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

CallableDeclaration const * getFunctionDeclarationOrConstructor(Expression const* expr);

bool isEmptyFunction(FunctionDefinition const* f);

enum class LocationReturn {
	noReturn,
	Last,
	Anywhere
};

struct ControlFlowInfo {
	int stackSize {-1};
	bool isLoop {false};
	bool useJmp {false};
};

struct ContInfo {
	static constexpr int CONTINUE_FLAG = 1;
	static constexpr int BREAK_FLAG = 2;
	static constexpr int RETURN_FLAG = 4;

	bool canReturn{};
	bool canBreak{};
	bool canContinue{};
	bool alwaysReturns{};
	bool alwaysBreak{};
	bool alwaysContinue{};

	bool doThatAlways() const {
		return alwaysReturns || alwaysBreak || alwaysContinue;
	}

	bool mayDoThat() const {
		return canReturn || canBreak || canContinue;
	}
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

DictValueType toDictValueType(const Type::Category& category);

int integerLog2(int value);

std::string stringToBytes(const std::string& str);

} // end solidity::frontend
