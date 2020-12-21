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

#define DBG(x) cout << x << endl;

namespace solidity::frontend {

template <typename T>	using string_map = std::map<std::string, T>;
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

template <typename T, typename TT>
T get_from_map(const string_map<T>& map, const string& key, TT defValue) {
	if (map.count(key) > 0)
		return map.at(key);
	return defValue;
}

bool ends_with(const string& str, const string& suffix);

std::string functionName(FunctionDefinition const* _function);

class ContactsUsageScanner: public ASTConstVisitor
{
public:
	explicit ContactsUsageScanner(ContractDefinition const& cd) {
		for (ContractDefinition const* base : cd.annotation().linearizedBaseContracts) {
			base->accept(*this);
		}
	}

	bool visit(FunctionCall const& _functionCall) override {
		auto ma = to<MemberAccess>(&_functionCall.expression());
		if (ma && ma->memberName() == "pubkey" && ma->expression().annotation().type->category() == Type::Category::Magic) {
			auto expr = to<Identifier>(&ma->expression());
			if (expr && expr->name() == "msg") {
				haveMsgPubkey = true;
			}
		}
		return true;
	}

	bool visit(MemberAccess const &_node) override {
		if (_node.expression().annotation().type->category() == Type::Category::Magic) {
			auto identifier = to<Identifier>(&_node.expression());
			if (identifier && identifier->name() == "msg" && _node.memberName() == "sender") {
				haveMsgSender = true;
			}
		}
		return true;
	}

	bool haveMsgPubkey{};
	bool haveMsgSender{};
};


class FunctionUsageScanner: public ASTConstVisitor
{
public:
	explicit FunctionUsageScanner(const ASTNode& node) {
		node.accept(*this);
	}

	bool visit(FunctionCall const& _functionCall) override {
		auto identifier = to<Identifier>(&_functionCall.expression());
		if (identifier) {
			auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
			if (functionDefinition && !functionDefinition->isInline()) {
				havePrivateFunctionCall = true;
			}
		}

		return true;
	}

	bool havePrivateFunctionCall{};
};

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
		} else if (auto* enumType = to<EnumType>(type)) {
			isNumeric = true;
			isSigned = false;
			numBits = bitsForEnum(enumType->numberOfMembers());
		}
		category = type->category();
	}
};

bool isTvmIntrinsic(const string& name);

bool isFunctionForInlining(FunctionDefinition const* f);

const Type* getType(const Expression* expr);

const Type* getType(const VariableDeclaration* var);

bool isIntegralType(const Type* type);

bool isStringOrStringLiteralOrBytes(const Type* type);

bool isRefType(const Type* type);

std::string typeToDictChar(Type const* keyType);

int lengthOfDictKey(Type const* key);

IntegerType getKeyTypeOfC4();

IntegerType getKeyTypeOfArray();

string storeIntegralOrAddress(const Type* type, bool reverse);

vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract);

vector<std::pair<FunctionDefinition const*, ContractDefinition const*>>
getContractFunctionPairs(ContractDefinition const* contract);

const FunctionDefinition* getFunction(ContractDefinition const* contract, const string& functionName);

bool isSuper(Expression const* expr);

bool isMacro(const std::string& functionName);

bool isAddressThis(const FunctionCall* fcall);

// List of all function but constructors with a given name
vector<FunctionDefinition const*> getContractFunctions(ContractDefinition const* contract, const string& funcName);

// List of all contract but constructor and  TvmIntrinsic functions including derived
vector<FunctionDefinition const*> getContractFunctions(ContractDefinition const* contract);

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
		for (PragmaDirective const *pd : pragmaDirectives) {
			if (pd->literals().size() == 1 &&
				 pd->literals()[0] == "ignoreIntOverflow") {
				return true;
			}
		}
		return false;
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
				solAssert(false, "Undefined type");
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

std::vector<VariableDeclaration const*>
convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr);

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
	Integer,
	Mapping,
	Optional,
	Struct,
	TvmCell,
	TvmSlice,
	VarInteger
};

DictValueType toDictValueType(const Type::Category& caterory);

int integerLog2(int value);

} // end solidity::frontend
