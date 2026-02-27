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

#pragma once

#include <liblangutil/SourceReferenceFormatter.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>

#include <regex>
#include <utility>

#include <libsolidity/codegen/TvmAst.hpp>

namespace solidity::frontend {

std::string eventName(EventDefinition const* _event);

bool isAddressOrAddressStdOrContractType(Type const* type);
bool isUsualArray(Type const* type);
bool isByteArrayOrString(Type const* type);
bool isString(Type const* type);
bool isSlice(Type const* type);
bool isSmallOptional(OptionalType const* type);
bool optValueAsTuple(Type const* optValueType);
int optTypeQty(Type const* type);

struct AddressInfo {
	static constexpr int stdAddrWithoutAnyCastLength() {
		// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt
		return 2 + 1 + 8 + 256;
	}

	static constexpr int stdAddrWithAnyCastLength() {
		// addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt
		return 2 + (1 + 5 + 30) + 8 + 256;
	}

	static constexpr int externalAddressLength() {
		// addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
		return 2 + 9 + 511;
	}

	static constexpr int maxBitLength() {
		// anycast_info$_ depth:(#<= 30) { depth >= 1 }
		// rewrite_pfx:(bits depth) = Anycast;

		// addr_var$11 anycast:(Maybe Anycast) addr_len:(## 9)
		// workchain_id:int32 address:(bits addr_len) = MsgAddressInt;

		// 2 +  // 11
		// 1 + 5 + 30 + // anycast
		// 9 + // addr_len
		// 32 + // workchain_id:int32
		// 512 // address // TODO, actually 2**9 - 1 == 511
		return 591;
	}
};

struct VarUIntegerInfo {
	static int maxTonBitLength() { return TypeProvider::coins()->maxBitSizeInCell(); }
};

int bitsForEnum(size_t val_count);

struct TypeInfo {
	bool isNumeric{};
	bool isSigned{};
	int numBits{};
	Type::Category category{};
	bool isQuiet{};

	explicit TypeInfo(Type const* type) {
		category = type->category();
		isNumeric = true;
		if (auto* integerType = to<IntegerType>(type)) {
			isSigned = integerType->isSigned();
			numBits = static_cast<int>(integerType->numBits());
		} else if (auto* qintegerType = to<QIntegerType>(type)) {
			isSigned = qintegerType->asIntegerType()->isSigned();
			numBits = static_cast<int>(qintegerType->asIntegerType()->numBits());
			isQuiet = true;
		} else if (auto* varint = to<VarIntegerType>(type)) {
			isSigned = varint->asIntegerType().isSigned();
			numBits = static_cast<int>(varint->asIntegerType().numBits());
		} else if (to<BoolType>(type)) {
			isSigned = true;
			numBits = 1;
		} else if (to<QBoolType>(type)) {
			isSigned = true;
			numBits = 1;
			isQuiet = true;
		} else if (auto* fixedBytesType = to<FixedBytesType>(type)) {
			isSigned = false;
			numBits = 8 * static_cast<int>(fixedBytesType->numBytes());
		} else if (auto enumType = to<EnumType>(type)) {
			isSigned = false;
			numBits = bitsForEnum(enumType->numberOfMembers());
		} else if (auto* fp = to<FixedPointType>(type)) {
			isSigned = fp->isSigned();
			numBits = static_cast<int>(fp->numBits());
		} else
			isNumeric = false;
	}
};

Type const* getType(Expression const* expr);

Type const* getType(VariableDeclaration const* var);

bool isIntegralType(Type const* type);

bool isStringOrStringLiteralOrBytes(Type const* type);

std::string typeToDictChar(Type const* keyType);

int dictKeyLength(Type const* key);

IntegerType getKeyTypeOfC4();

IntegerType const& getArrayKeyType();

std::tuple<Type const*, Type const*> dictKeyValue(Type const* type);

std::tuple<Type const*, Type const*> realDictKeyValue(Type const* type);

std::vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract);

enum class StateVarType {
	Usual,
	Transient,
	Unpacked,
};

std::vector<VariableDeclaration const*> stateVariables(ContractDefinition const* _contract, StateVarType stateVarType);

bool isSuper(Expression const* expr);
bool isAddressThis(FunctionCall const* funCall);

FunctionDefinition const* getSuperFunction(
	ContractDefinition const* currentContract,
	ContractDefinition const* mainContract,
	std::string const& hexName
);

[[noreturn]]
void cast_error(ASTNode const& node, std::string const& error_message);

[[noreturn]]
void fatal_error(std::string const& error_message);

class PragmaDirectiveHelper {
public:
	explicit PragmaDirectiveHelper(std::vector<PragmaDirective const*> const& _pragmaDirectives):
		pragmaDirectives{_pragmaDirectives} {}

	bool hasIgnoreIntOverflow() const {
		return std::ranges::any_of(pragmaDirectives, [](auto const& pd) {
			return pd->literals().size() == 1 && pd->literals()[0] == "ignoreIntOverflow";
		});
	}

	std::optional<std::vector<ASTPointer<Expression>>> hasCopyleft() const {
		for (PragmaDirective const* pd: pragmaDirectives) {
			if (pd->literals().size() == 1 && pd->literals()[0] == "copyleft") {
				return pd->parameter();
			}
		}
		return {};
	}

	bool hasUpgradeOldSol() const {
		return std::ranges::any_of(pragmaDirectives, [](PragmaDirective const* pd) {
			return pd->literals().size() == 2 && pd->literals()[0] == "upgrade" && pd->literals()[1] == "oldsol";
		});
	}

private:
	std::vector<PragmaDirective const*> const& pragmaDirectives;
};

class ABITypeSize {
public:
	explicit ABITypeSize(Type const* _type);

	bool fixedSize = false;
	bool fixedRefs = false;
	int maxBits = -1;
	int maxRefs = -1;
};

inline std::pair<std::vector<Type const*>, std::vector<ASTNode const*>> getParams(
	std::vector<VariableDeclaration const*> const& params,
	std::vector<ASTNode const*>::difference_type offset = 0
) {
	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (auto it = params.begin() + offset; it != params.end(); ++it) {
		types.push_back(getType(*it));
		nodes.push_back(*it);
	}
	return std::make_pair(types, nodes);
}


template <typename T>
std::pair<std::vector<Type const*>, std::vector<ASTNode const*>>
getParams(ast_vec<T> const& params, size_t offset = 0) {
	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (auto it = params.begin() + offset; it != params.end(); ++it) {
		types.push_back(getType(it->get()));
		nodes.push_back(it->get());
	}
	return std::make_pair(types, nodes);
}

CallableDeclaration const* getFunctionDeclarationOrConstructor(Expression const* expr, bool quiet = false);

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
	ControlFlowInfo(int stackSize, bool hasAnalyzeFlag, bool isLoop):
		m_stackSize(stackSize),
		m_hasAnalyzeFlag(hasAnalyzeFlag),
		m_isLoop(isLoop) {}

	int stackSize() const { return m_stackSize; }

	bool hasAnalyzeFlag() const { return m_hasAnalyzeFlag; }

	bool isLoop() const { return m_isLoop; }

private:
	int m_stackSize{-1};
	bool m_hasAnalyzeFlag{false};
	bool m_isLoop{false};
};

std::vector<VariableDeclaration const*> convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr);

std::vector<Type const*> getTypesFromVarDecls(std::vector<VariableDeclaration const*> const& arr);

std::vector<Type const*> getTypesFromVarDecls(std::vector<ASTPointer<VariableDeclaration>> const& arr);

std::pair<std::vector<Type const*>, std::vector<std::string>> getTupleTypes(TupleType const* tuple);

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

enum class SetDictOperation {
	Set,
	Replace,
	Add
};

struct LValueInfo {
	std::vector<Expression const*> expressions;
	int stackSizeDiff = 0;
};

DictValueType toDictValueType(Type::Category const& category);
std::set<CallableDeclaration const*> getAllBaseFunctions(CallableDeclaration const* f);
int strToInt(std::string const& str);
int qtyWithoutLoc(
	std::vector<Pointer<TvmAstNode>>::const_iterator beg,
	std::vector<Pointer<TvmAstNode>>::const_iterator end
);
int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> const& arr);
void trimLoc(std::vector<Pointer<TvmAstNode>>& arr);

namespace StrUtils {
std::optional<std::string> toBitString(bigint value, int bitlen, bool isSign);
std::string binaryStringToSlice(std::string const& _s);
std::string toBitString(std::string const& slice);
std::optional<std::string> unitSlices(std::string const& sliceA, std::string const& sliceB);
std::optional<std::string> unitBitStringToHex(std::string const& bitStringA, std::string const& bitStringB);
std::string tonsToBinaryString(u256 const& value);
std::string tonsToBinaryString(bigint value);
std::string boolToBinaryString(bool value);
std::string literalToSliceAddress(bigint const& value);
bigint toBigint(std::string const& binStr);
std::optional<bigint> toNegBigint(std::string const& binStr);
std::string toBinString(bigint num);
std::string stringToHex(std::string const& str);
std::string intToHex(uint32_t id);
}

namespace ExprUtils {
std::optional<bigint> constValue(Expression const& _e);
std::optional<bool> constBool(Expression const& _e);
}

namespace MathConsts {
std::map<bigint, int> const& power2Exp();
std::map<bigint, int> const& power2DecExp();
std::map<bigint, int> const& power2NegExp();
std::map<int, bigint> const& power10();
}

bool isFitUselessUnary(Type const* common, Token op);
bool isFitUseless(Type const* left, Type const* right, Type const* common, Token op);
bool isInRange257(bigint const& value);

unsigned short crc16(std::string const& str);

struct ArithmeticOperation {
	std::string name;
	size_t take;
	size_t ret;
	bool withRShift;
};
std::vector<ArithmeticOperation> tonCombinedArithmeticOperations();


FunctionDefinition const* getRemoteFunctionDefinition(MemberAccess const* memberAccess);

} // end solidity::frontend
