/*
 * Copyright (C) 2019-2023 EverX. All Rights Reserved.
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

namespace solidity::frontend {

template <typename T> using ast_vec = std::vector<ASTPointer<T>>;

template <typename T1, typename T2>
T1 const* to(T2 const* ptr) { return dynamic_cast<T1 const*>(ptr); }

template<typename T, typename... Args>
constexpr bool isIn(T v, Args... args) {
	return (... || (v == (args)));
}

constexpr uint64_t str2int(const char* str, int i = 0) {
	return !str[i] ? 5381 : (str2int(str, i+1) * 33) ^ uint64_t(str[i]);
}

std::string functionName(FunctionDefinition const* _function);
std::string eventName(EventDefinition const* _event);

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
			numBits = int(integerType->numBits());
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
			numBits = int(fp->numBits());
		}
		category = type->category();
	}
};

const Type* getType(const Expression* expr);

const Type* getType(const VariableDeclaration* var);

bool isIntegralType(const Type* type);

bool isStringOrStringLiteralOrBytes(const Type* type);

std::string typeToDictChar(Type const* keyType);

int dictKeyLength(Type const* key);

IntegerType getKeyTypeOfC4();

IntegerType const& getArrayKeyType();

std::tuple<Type const*, Type const*>
dictKeyValue(Type const* type);

std::tuple<Type const*, Type const*>
realDictKeyValue(Type const* type);


std::vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract);
std::vector<VariableDeclaration const *> stateVariables(ContractDefinition const* contract, bool _withNoStorage);

std::vector<std::pair<FunctionDefinition const*, ContractDefinition const*>>
getContractFunctionPairs(ContractDefinition const* contract);

bool isSuper(Expression const* expr);
bool isAddressThis(const FunctionCall* funCall);

// List of all function but constructors with a given name
std::vector<FunctionDefinition const*> getContractFunctions(ContractDefinition const* contract, const std::string& funcName);

FunctionDefinition const* getSuperFunction(
	const ContractDefinition* currentContract,
	const ContractDefinition* mainContract,
	const std::string& hexName
);

[[noreturn]]
void cast_error(const ASTNode& node, const std::string& error_message);

[[noreturn]]
void fatal_error(const std::string &error_message);

class PragmaDirectiveHelper {
public:
	explicit PragmaDirectiveHelper(std::vector<PragmaDirective const *> const& _pragmaDirectives) :
			pragmaDirectives{_pragmaDirectives} {
	}

	bool hasTime()   const { return !std::get<0>(hasHeader("notime")); }
	bool hasPubkey() const { return std::get<0>(hasHeader("pubkey")); }
	bool hasExpire() const { return std::get<0>(hasHeader("expire")); }

	std::tuple<bool, PragmaDirective const *> hasHeader(const std::string& str) const {
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 2 &&
				pd->literals()[0] == "AbiHeader" &&
				pd->literals()[1] == str) {
				return {true, pd};
			}
		}
		return {false, nullptr};
	}

	bool hasIgnoreIntOverflow() const {
		return std::any_of(pragmaDirectives.begin(), pragmaDirectives.end(), [](const auto& pd){
			return pd->literals().size() == 1 && pd->literals()[0] == "ignoreIntOverflow";
		});
	}

	std::optional<std::vector<ASTPointer<Expression>>> hasMsgValue() const {
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 1 &&
				pd->literals()[0] == "msgValue") {
				return pd->parameter();
			}
		}
		return {};
	}

	std::optional<std::vector<ASTPointer<Expression>>> hasCopyleft() const {
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 1 &&
				pd->literals()[0] == "copyleft") {
				return pd->parameter();
			}
		}
		return {};
	}

	bool hasUpgradeFunc() const {
		return std::any_of(pragmaDirectives.begin(), pragmaDirectives.end(), [](PragmaDirective const *pd){
			return pd->literals().size() == 2 && pd->literals()[0] == "upgrade" && pd->literals()[1] == "func";
		});
	}

	bool hasUpgradeOldSol() const {
		// TODO test that it's impossible to use func and oldsol at same time
		return std::any_of(pragmaDirectives.begin(), pragmaDirectives.end(), [](PragmaDirective const *pd){
			return pd->literals().size() == 2 && pd->literals()[0] == "upgrade" && pd->literals()[1] == "oldsol";
		});
	}

private:
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};


struct ABITypeSize {
	int maxBits = -1;
	int maxRefs = -1;

	explicit ABITypeSize(Type const* _type);
	void init(Type const* _type);
};

inline std::pair<std::vector<Type const*>, std::vector<ASTNode const*>>
getParams(const std::vector<VariableDeclaration const*>& params, std::vector<ASTNode const*>::difference_type offset = 0) {
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

class ControlFlowInfo {
public:
	ControlFlowInfo() = default;
	ControlFlowInfo(int stackSize, bool hasAnalyzeFlag, bool isLoop) : m_stackSize(stackSize),
																	   m_hasAnalyzeFlag(hasAnalyzeFlag), m_isLoop(isLoop) {}

	int stackSize() const {
		return m_stackSize;
	}

	bool hasAnalyzeFlag() const {
		return m_hasAnalyzeFlag;
	}

	bool isLoop() const {
		return m_isLoop;
	}

private:
	int m_stackSize {-1};
	bool m_hasAnalyzeFlag {false};
	bool m_isLoop {false};
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
	GetDelFromMapping,
	GetReplaceFromMapping,
	GetFromArray,
	Fetch,
	Exist
};

enum class SetDictOperation { Set, Replace, Add };

struct LValueInfo {
	std::vector<Expression const*> expressions;
	int stackSizeDiff = 0;
};

DictValueType toDictValueType(const Type::Category& category);
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
	std::string literalToSliceAddress(bigint const& value);
	bigint toBigint(const std::string& binStr);
	std::string toBinString(bigint num);
	std::string stringToHex(const std::string& str);
}

namespace ExprUtils {
	std::optional<bigint> constValue(Expression const &_e);
	std::optional<bool> constBool(Expression const &_e);
}

namespace MathConsts {
	std::map<bigint, int> const& power2Exp();
	std::map<bigint, int> const& power2DecExp();
	std::map<bigint, int> const& power2NegExp();
	std::map<int, bigint> const& power10();
}

} // end solidity::frontend
