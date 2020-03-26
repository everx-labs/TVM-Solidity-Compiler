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
#include <typeinfo>
#include <json/json.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <utility>

#include "TVMConstants.hpp"
#include "TVMStructCompiler.hpp"

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;

namespace solidity::frontend {

template <typename T>	using string_map	= std::map<std::string, T>;
template <typename T>	using ptr_vec		= std::vector<ASTPointer<T>>;

template <typename T1, typename T2>
T1 const* to(T2 const* ptr) { return dynamic_cast<T1 const*>(ptr); }

template<typename T, typename... Args>
constexpr bool isIn(T v, Args... args) {
	return (... || (v == (args)));
}

constexpr unsigned int str2int(const char* str, int h = 0) {
	return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
}

template <typename T, typename TT>
T get_from_map(const string_map<T>& map, const string& key, TT defValue) {
	if (map.count(key) > 0)
		return map.at(key);
	return defValue;
}

bool ends_with(const string& str, const string& suffix);

std::string functionName(FunctionDefinition const* _function) {
	if (_function->isConstructor()) {
		solAssert(false, "");
	}

	if (_function->isReceive()) {
		return "receive";
	}
	if (_function->isFallback()) {
		return "fallback";
	}
	return _function->name();
}

class TVMStack {
	int m_size;
	// map parameters or local variables to their absolute stack position
	string_map<int> m_params;

public:

	TVMStack() : m_size(0) {}

	int size() const {
		return m_size;
	}

	void change(int diff) {
		m_size += diff;
	}

	bool isParam(const string& name) const {
		return m_params.count(name) > 0;
	}

	void add(const string& name, bool doAllocation) {
		m_params[name] = doAllocation? m_size++ : m_size - 1;
	}

	std::vector<string> dropLocals(int savedStackSize) {
		std::vector<string> locals;
		for (const auto& p : m_params) {
			if (p.second >= savedStackSize)
				locals.push_back(p.first);
		}
		for (const auto& name : locals) {
			m_params.erase(name);
		}
		return locals;
	}

	int getOffset(const string& name) const {
		solAssert(isParam(name), "");
		return getOffset(m_params.at(name));
	}

	int getOffset(int stackPos) const {
		return m_size - 1 - stackPos;
	}

	std::string dumpParams() const {
		std::ostringstream o;
		for (const auto& p : m_params) {
			o << p.first << "=" << p.second << ", ";
		}
		return o.str();
	}

	void ensureSize(int savedStackSize, const string& location) const {
		solAssert(savedStackSize == m_size, "stack: " + toString(savedStackSize)
		                                    + " vs " + toString(m_size) + " at " + location);
	}

};

struct CodeLines {
	vector<string> lines;
	int tabQty{};

	string str(const string& indent = "") const {
		std::ostringstream o;
		for (const string& s : lines) {
			o << indent << s << endl;
		}
		return o.str();
	}

	void addTabs(const int qty = 1) {
		tabQty += qty;
	}

	void subTabs(const int qty = 1) {
		tabQty -= qty;
	}

	void startContinuation() {
		push("PUSHCONT {");
		++tabQty;
	}

	void endContinuation() {
		--tabQty;
		push("}");
		solAssert(tabQty >= 0, "");
	}

	void push(const string& cmd) {
		if (cmd.empty() || cmd == "\n") {
			return;
		}

		// space means empty line
		if (cmd == " ")
			lines.emplace_back("");
		else {
			solAssert(tabQty >= 0, "");
			lines.push_back(std::string(tabQty, '\t') + cmd);
		}
	}

	void append(const CodeLines& oth) {
		for (const auto& s : oth.lines) {
			lines.push_back(std::string(tabQty, '\t') + s);
		}
	}
};

struct ContInfo {
	bool canReturn = false;
	bool canBreak = false;
	bool canContinue = false;
	bool alwaysReturns = false;
	bool alwaysBreak = false;
	bool alwaysContinue = false;
	ContInfo() = default;

	bool doThatAlways() {
		return alwaysReturns || alwaysBreak || alwaysContinue;
	}
};

class TVMScanner: public ASTConstVisitor
{
public:
	explicit TVMScanner(const ASTNode& node) {
		node.accept(*this);
		solAssert(m_loopDepth == 0, "");
	}

protected:
	bool visit(WhileStatement const&) override {
		m_loopDepth++;
		return true;
	}

	void endVisit(WhileStatement const&) override {
		m_loopDepth--;
	}

	bool visit(ForStatement const&) override {
		m_loopDepth++;
		return true;
	}

	void endVisit(ForStatement const&) override {
		m_loopDepth--;
	}

	void endVisit(Return const&) override {
		m_info.canReturn = true;
	}

	void endVisit(Break const&) override {
		if (m_loopDepth == 0)
			m_info.canBreak = true;
	}

	void endVisit(Continue const&) override {
		if (m_loopDepth == 0)
			m_info.canContinue = true;
	}

	bool visit(FunctionCall const& _functionCall) override {
		auto ma = to<MemberAccess>(&_functionCall.expression());
        if (ma && ma->memberName() == "pubkey" && ma->expression().annotation().type->category() == Type::Category::Magic) {
	        auto expr = to<Identifier>(&ma->expression());
	        if (expr && expr->name() == "msg") {
		        haveMsgPubkey = true;
	        }
        }


		auto identifier = to<Identifier>(&_functionCall.expression());
		if (identifier) {
			auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
			if (functionDefinition && !functionDefinition->isInline()) {
				havePrivateFunctionCall = true;
			}
		}

		return true;
	}

private:
	int m_loopDepth = 0;

public:
	ContInfo m_info;
	bool haveMsgPubkey = false;
	bool havePrivateFunctionCall = false;
};

template <typename T>
static bool doesAlways(const Statement* st) {
	auto rec = [] (const Statement* s) {
		return doesAlways<T>(s);
	};
	if (to<T>(st))
		return true;
	if (to<ExpressionStatement>(st) ||
		to<VariableDeclarationStatement>(st) ||
		to<EmitStatement>(st) ||
		to<PlaceholderStatement>(st) ||
		to<Assignment>(st))
		return false;
	if (to<Continue>(st) || to<Break>(st) || to<Return>(st))
		return false;
	if (auto block = to<Block>(st)) {
		for (const auto& s : block->statements()) {
			if (rec(s.get()))
				return true;
		}
		return false;
	}
	if (auto ifStatement = to<IfStatement>(st)) {
		if (!ifStatement->falseStatement())
			return false;
		return rec(&ifStatement->trueStatement()) && rec(ifStatement->falseStatement());
	}
	if (auto forStatement = to<ForStatement>(st)) {
		return rec(&forStatement->body());
	}
	if (auto whileStatement = to<WhileStatement>(st)) {
		return rec(&whileStatement->body());
	}
	solAssert(false, string("Unsupported statement type: ") + typeid(*st).name());
	return false;
}

ContInfo getInfo(const Statement& statement) {
	TVMScanner scanner(statement);
	ContInfo info = scanner.m_info;
	info.alwaysReturns = doesAlways<Return>(&statement);
	info.alwaysContinue = doesAlways<Continue>(&statement);
	info.alwaysBreak = doesAlways<Break>(&statement);
	return info;
}

bool isAddressOrContractType(const Type* type) {
	return to<AddressType>(type) || to<ContractType>(type);
}

bool isUsualArray(const Type* type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && !arrayType->isByteArray();
}

bool isByteArrayOrString(const Type* type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && arrayType->isByteArray();
}

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

struct TypeInfo {
	bool isNumeric {false};
	bool isSigned {false};
	int numBits {0};
	Type::Category category {Type::Category::Integer};

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
	}
};

bool isTvmIntrinsic(const string& name) {
	return 0 == name.find("tvm_");
}

bool isFunctionForInlining(FunctionDefinition const* f) {
	return ends_with(f->name(), "_inline") || f->isInline() || f->isFallback() || f->isReceive() || f->name() == "onBounce";
}

const Type* getType(const Expression* expr) {
	return expr->annotation().type;
}

const Type* getType(const VariableDeclaration* var) {
	return var->annotation().type;
}

bool isIntegralType(const Type* type) {
	return TypeInfo(type).isNumeric;
}

bool isStringOrStringLiteralOrBytes(const Type* type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isByteArray());
}

std::string typeToDictChar(Type const* keyType) {
	TypeInfo ti(keyType);
	if (ti.isNumeric) {
		return ti.isSigned? "I" : "U";
	} else if (isStringOrStringLiteralOrBytes(keyType)) {
		return "U";
	}
	return ""; // dict key is slice
}

int lengthOfDictKey(Type const* key) {
	if (isIn(key->category(), Type::Category::Address, Type::Category::Contract)) {
		return AddressInfo::stdAddrLength();
	}

	TypeInfo ti{key};
	if (ti.isNumeric) {
		return ti.numBits;
	}

	if (isStringOrStringLiteralOrBytes(key)){
		return 256;
	}

	solAssert(false, "");
}

IntegerType getKeyTypeOfC4() {
	return IntegerType(TvmConst::C4::KeyLength);
}

IntegerType getKeyTypeOfArray() {
	return IntegerType(TvmConst::ArrayKeyLength);
}

string storeIntegralOrAddress(const Type* type, bool reverse) {
	if (isAddressOrContractType(type))
		return reverse ? "STSLICER" : "STSLICE";
	auto ti = TypeInfo(type);
	if (ti.isNumeric) {
		string cmd = ti.isSigned? "STI" : "STU";
		if (reverse) cmd = cmd + "R";
		solAssert(cmd != "STU 267", "");
		return cmd + " " + toString(ti.numBits);
	}
	solAssert(false, "Unsupported param type " + type->toString());
}

bool isExpressionExactTypeKnown(Expression const* expr) {
	if (to<Literal>(expr)) return true;
	if (to<Identifier>(expr)) return true;
	if (to<FunctionCall>(expr)) return true;
	if (to<IndexAccess>(expr)) return true;
	if (to<MemberAccess>(expr)) return true;
	return false;
}

bool isNonNegative(Expression const* expr) {
	auto type = getType(expr);
	if (isExpressionExactTypeKnown(expr)) {
		if (auto type2 = to<RationalNumberType>(type)) {
			if (!type2->integerType()->isSigned())
				return true;
		}
		if (auto type2 = to<IntegerType>(type)) {
			if (!type2->isSigned())
				return true;
		}
	}
	if (auto binaryOp = to<BinaryOperation>(expr)) {
		if (isNonNegative(&binaryOp->leftExpression()) && isNonNegative(&binaryOp->rightExpression())) {
			switch (binaryOp->getOperator()) {
				case Token::Add:
				case Token::Mul:
					return true;
				default:
					break;
			}
		}
	}
	return false;
}

vector<ContractDefinition const*> getContractsChain(ContractDefinition const* contract) {
	vector<FunctionDefinition const*> result;
	auto contracts = contract->annotation().linearizedBaseContracts;
	std::reverse(contracts.begin(), contracts.end());
	return contracts;
}

vector<std::pair<FunctionDefinition const*, ContractDefinition const*>>
getContractFunctionPairs(ContractDefinition const* contract) {
	vector<pair<FunctionDefinition const*, ContractDefinition const*>> result;
	for (ContractDefinition const* c : getContractsChain(contract)) {
		for (const auto f : c->definedFunctions())
			result.emplace_back(f, c);
	}
	return result;
}

auto getFunction(ContractDefinition const* contract, const string& functionName) {
	const FunctionDefinition* result = nullptr;
	for (const auto f : contract->definedFunctions()) {
		if (f->name() == functionName)
			return f;
	}
	return result;
}

bool isSuper(Expression const* expr) {
	if (auto identifier = to<Identifier>(expr)) {
		return identifier->name() == "super";
	}
	return false;
}

bool isMacro(const std::string& functionName) {
	return ends_with(functionName, "_macro");
}

bool isAddressThis(const FunctionCall* fcall) {
	if (!fcall)
		return false;
	auto arguments = fcall->arguments();
	if (auto etn = to<ElementaryTypeNameExpression>(&fcall->expression())) {
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

// List of all function but constructors with a given name
vector<FunctionDefinition const*>
getContractFunctions(ContractDefinition const* contract, const string& funcName) {
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

// List of all contract but constructor and  TvmIntrinsic functions including derived
vector<FunctionDefinition const*>
getContractFunctions(ContractDefinition const* contract) {
	vector<FunctionDefinition const*> result;
	for (auto &[functionDefinition, contractDefinition] : getContractFunctionPairs(contract)) {
		(void)contractDefinition;	// suppress unused variable error
		if (functionDefinition->isConstructor())
			continue;
		const std::string funName = functionName(functionDefinition); // for fallback and recieve name is empty
		if (isTvmIntrinsic(funName))
			continue;
		// TODO: not needed check?
		if (functionDefinition != getContractFunctions(contract, funName).back())
			continue;
		result.push_back(functionDefinition);
	}
	return result;
}

const ContractDefinition* getSuperContract(const ContractDefinition* currentContract,
											const ContractDefinition* mainContract,
											const string& fname) {
	ContractDefinition const* prev = nullptr;
	for (auto c : getContractsChain(mainContract)) {
		if (c == currentContract)
			break;
		if (getFunction(c, fname))
			prev = c;
	}
	return prev;
}

bool ends_with(const string& str, const string& suffix) {
	if (suffix.size() > str.size())
		return false;
	return 0 == str.compare(str.size()-suffix.size(), suffix.size(), suffix);
}

std::string ASTNode2String(const ASTNode& node, const string& error_messag = "", bool isWarning = false) {
	ErrorList		m_errors;
	ErrorReporter	m_errorReporter(m_errors);
	m_errorReporter.parserError(node.location(), error_messag);
	string message = SourceReferenceFormatter::formatExceptionInformation(
		*(m_errorReporter.errors())[0],
		isWarning ? "Warning" : "Error"
	);
	return message;
}

[[noreturn]]
void cast_error(const ASTNode& node, const string& error_message) {
	cerr << ASTNode2String(node, error_message) << endl;
	std::exit(EXIT_FAILURE);
}

void cast_warning(const ASTNode& node, const string& error_message) {
	cerr << ASTNode2String(node, error_message, true) << endl;
}

struct FuncInfo {
	const FunctionDefinition*	m_function;
	const ContractDefinition*	m_contract;
	string						m_internalName;

	FuncInfo(const FunctionDefinition* f, const ContractDefinition* c)
		: m_function(f), m_contract(c)
	{
	}
};

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

private:
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};

class TVMCompilerContext {
	const ContractDefinition*				m_contract = nullptr;
	string_map<const FunctionDefinition*>	m_functions;
	map<const FunctionDefinition*, const ContractDefinition*>	m_function2contract;
	string_map<const EventDefinition*>		m_events;
	set<string>								m_allContractNames;
	bool haveFallback = false;
	bool haveOnBounce = false;
	bool haveReceive = false;
	bool ignoreIntOverflow = false;
	bool m_haveSetDestAddr = false;
	PragmaDirectiveHelper const& m_pragmaHelper;
	std::map<VariableDeclaration const *, int> m_stateVarIndex;

	void addEvent(EventDefinition const *event) {
		std::string name = event->name();
		solAssert(m_events.count(name) == 0, "Duplicate event " + name);
		m_events[name] = event;
	}

	void addFunction(FunctionDefinition const* _function) {
		if (!_function->isConstructor()) {
			string name = functionName(_function);
			if (m_functions.count(name) > 0) {
				// TODO: check for signature match!
				cast_error(*_function, "Function overloading is not supported yet");
			}
			m_functions[name] = _function;
		}
	}

	void initMembers(ContractDefinition const* contract, const std::vector<ContractDefinition const*>& allContracts) {
		solAssert(!m_contract, "");
		m_contract = contract;
		for (ContractDefinition const* c : getContractsChain(contract)) {
			for (EventDefinition const *event : c->events())
				addEvent(event);
		}
		for (const auto pair : getContractFunctionPairs(contract)) {
			m_function2contract.insert(pair);
		}
		map<ASTString, vector<TypePointer>> FuncToParams;
		for (auto c: getContractsChain(contract)) {
			for (auto f : getContractFunctions(c)) {
				if (f->isConstructor())
					continue;
				vector<TypePointer> params;
				for (const auto& param: f->parameters())
					params.push_back(param->annotation().type);
				auto presentParams = FuncToParams.find(f->name());
				if (presentParams != FuncToParams.end()) {
					if (presentParams->second.size() != params.size())
						cast_error(*f, "Overloading of methods is not allowed.");
					for (size_t i = 0; i < params.size(); i++) {
						if (*params[i] != *presentParams->second[i])
							cast_error(*f, "Overloading of methods is not allowed.");
					}
				} else {
					FuncToParams.insert({f->name(), params});
				}
			}
		}


		for (FunctionDefinition const* f : contract->definedFunctions()) {
			ignoreIntOverflow |= f->name() == "tvm_ignore_integer_overflow";
			m_haveSetDestAddr |=  f->name() == "tvm_set_ext_dest_address";
			haveFallback |= f->isFallback();
			haveOnBounce |= f->name() == "onBounce";
			haveReceive  |= f->isReceive();
		}
		ignoreIntOverflow |= m_pragmaHelper.haveIgnoreIntOverflow();
		for (const auto f : getContractFunctions(contract)) {
			if (isPureFunction(f))
				continue;
			addFunction(f);
		}
		for (const auto pair : getContractFunctionPairs(contract)) {
			auto f = pair.first;
			auto c = pair.second;
			if (!isTvmIntrinsic(f->name()) && !isPureFunction(f) && !isFunctionForInlining(f)) {
				FuncInfo fi(f, c);
				fi.m_internalName = getFunctionInternalName(f);
				if (!f->isConstructor() && f != getContractFunctions(contract, f->name()).back()) {
					fi.m_internalName = c->name() + "_" + f->name();
				}
				m_functionsList.push_back(fi);
			}
		}
		for (auto c : allContracts) {
			m_allContractNames.insert(c->name());
		}

		for (VariableDeclaration const *variable: notConstantStateVariables()) {
			m_stateVarIndex[variable] = 10 + m_stateVarIndex.size();
		}
	}

public:
	TVMCompilerContext(ContractDefinition const* contract, const std::vector<ContractDefinition const*>& allContracts,
			PragmaDirectiveHelper const& pragmaHelper) : m_pragmaHelper{pragmaHelper} {
		initMembers(contract, allContracts);
	}

	mutable set<string>		m_remoteFunctions;
	vector<FuncInfo>		m_functionsList;
	const FuncInfo*			m_currentFunction = nullptr;
	map<string, CodeLines>	m_inlinedFunctions;

	int getStateVarIndex(VariableDeclaration const *variable) const {
		return m_stateVarIndex.at(variable);
	}

	std::vector<VariableDeclaration const *> notConstantStateVariables() const {
		std::vector<VariableDeclaration const*> variableDeclarations;
		std::vector<ContractDefinition const*> mainChain = getContractsChain(getContract());
		for (ContractDefinition const* contract : mainChain) {
			for (VariableDeclaration const *variable: contract->stateVariables()) {
				if (!variable->isConstant()) {
					variableDeclarations.push_back(variable);
				}
			}
		}
		return variableDeclarations;
	}

	PragmaDirectiveHelper const& pragmaHelper() const {
		return m_pragmaHelper;
	}

	bool haveTimeInAbiHeader() const {
		if (m_pragmaHelper.abiVersion() == 1) {
			return true;
		}
		if (m_pragmaHelper.abiVersion() == 2) {
			return m_pragmaHelper.haveTime() || afterSignatureCheck() == nullptr;
		}
		solAssert(false, "");
	}

	bool isStdlib() const {
		return m_contract->name() == "stdlib";
	}

	bool haveSetDestAddr() const {
		return m_haveSetDestAddr;
	}

	string getFunctionInternalName(FunctionDefinition const* _function) const {
		if (isStdlib()) {
			return _function->name();
		}
        if (_function->name() == "onCodeUpgrade") {
            return ":onCodeUpgrade";
        }
		return _function->name() + "_internal";
	}

	static string getFunctionExternalName(FunctionDefinition const* _function) {
		const string& fname = _function->name();
		solAssert(_function->isPublic(), "Internal error: expected public function: " + fname);
		if (_function->isConstructor()) {
			return "constructor";
		}
		if (_function->isFallback()) {
			return "fallback";
		}
		return getFunctionExternalName(fname);
	}

	static string getFunctionExternalName(const string& fname) {
		return fname;
	}

	bool isPureFunction(FunctionDefinition const* f) const {
		const auto& vec = getContract(f)->annotation().unimplementedFunctions;
		return std::find(vec.cbegin(), vec.cend(), f) != vec.end();
	}

	const ContractDefinition* getContract() const {
		return m_contract;
	}

	const ContractDefinition* getContract(const FunctionDefinition* f) const {
		return m_function2contract.at(f);
	}

	const FunctionDefinition* getLocalFunction(string fname) const {
		return get_from_map(m_functions, std::move(fname), nullptr);
	}

	const EventDefinition* getEvent(const string& name) const {
		return get_from_map(m_events, name, nullptr);
	}

	bool isContractName(const string& name) const {
		return m_allContractNames.count(name) > 0;
	}

	bool haveFallbackFunction() const {
		return haveFallback;
	}

	bool haveReceiveFunction() const {
		return haveReceive;
	}

	bool haveOnBounceHandler() const {
		return haveOnBounce;
	}

	bool ignoreIntegerOverflow() const {
		return ignoreIntOverflow;
	}

	std::vector<const EventDefinition*> events() const {
		std::vector<const EventDefinition*> result;
		for (const auto& [name, event] : m_events) {
			(void)name;
			result.push_back(event);
		}
		return result;
	}

	FunctionDefinition const* afterSignatureCheck() const {
		for (FunctionDefinition const* f : m_contract->definedFunctions()) {
			if (f->name() == "afterSignatureCheck") {
				return f;
			}
		}
		return nullptr;
	}

	bool storeTimestampInC4() const {
		return haveTimeInAbiHeader() && afterSignatureCheck() == nullptr;
	}
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

class StackPusherHelper {
protected:
	TVMStack m_stack;
	CodeLines m_code;
	const TVMCompilerContext* const m_ctx;
	StructCompiler m_structCompiler;

public:
	explicit StackPusherHelper(const TVMCompilerContext* ctx) :
		m_ctx(ctx),
		m_structCompiler{this,
		                 ctx->notConstantStateVariables(),
				         256 + (m_ctx->storeTimestampInC4()? 64 : 0) + 1, // pubkey + timestamp + constructor_flag
				         1,
				         true,
				         nullptr} {
	}

	void append(const CodeLines& oth) {
		m_code.append(oth);
	}

	void addTabs(const int qty = 1) {
		m_code.addTabs(qty);
	}

	void subTabs(const int qty = 1) {
		m_code.subTabs(qty);
	}

	void pushCont(const CodeLines& cont, const string& comment = "") {
		if (comment.empty())
			push(0, "PUSHCONT {");
		else
			push(0, "PUSHCONT { ; " + comment);
		for (const auto& l : cont.lines)
			push(0, string("\t") + l);
		push(+1, "}"); // adjust stack
	}

	void generateGlobl(const string& fname, const bool isPublic) {
		push(0, ".globl\t" + fname);
		if (isPublic) {
			push(0, ".public\t" + fname);
		}
		push(0, ".type\t"  + fname + ", @function");
	}

	void generateInternal(const string& fname, const int id) {
		push(0, ".internal-alias :" + fname + ",        " + toString(id));
		push(0, ".internal\t:" + fname);
	}

	void generateMacro(const string& functionName) {
		push(0, ".macro " + functionName);
	}

	CodeLines code() const {
		return m_code;
	}

	[[nodiscard]]
	const TVMCompilerContext& ctx() const {
		return *m_ctx;
	}

	void push(int stackDiff, const string& cmd) {
		m_code.push(cmd);
		m_stack.change(stackDiff);
	}

	void startContinuation() {
		m_code.startContinuation();
	}

	void endContinuation() {
		m_code.endContinuation();
	}

	StructCompiler& structCompiler() {
		return m_structCompiler;
	}

	TVMStack& getStack() {
		return m_stack;
	}

	void pushLog(const std::string& str);

	void pushLines(const std::string& lines) {
		std::istringstream stream{lines};
		std::string line;
		while (std::getline(stream, line)) {
			push(0, line);
		}
	}

	void untuple(int n) {
		solAssert(0 <= n, "");
		if (n <= 15) {
			push(-1 + n, "UNTUPLE " + toString(n));
		} else {
			solAssert(n <= 255, "");
			pushInt(n);
			push(-2 + n, "UNTUPLEVAR");
		}
	}

	void index(int index) {
		solAssert(0 <= index, "");
		if (index <= 15) {
			push(-1 + 1, "INDEX " + toString(index));
		} else {
			solAssert(index <= 254, "");
			pushInt(index);
			push(-2 + 1, "INDEXVAR");
		}
	}

	void set_index(int index) {
		solAssert(0 <= index, "");
		if (index <= 15) {
			push(-2 + 1, "SETINDEX " + toString(index));
		} else {
			solAssert(index <= 254, "");
			pushInt(index);
			push(-1 - 2 + 1, "SETINDEXVAR");
		}
	}

	void tuple(int qty) {
		solAssert(0 <= qty, "");
		if (qty <= 15) {
			push(-qty + 1, "TUPLE " + toString(qty));
		} else {
			solAssert(qty <= 255, "");
			pushInt(qty);
			push(-1 - qty + 1, "TUPLEVAR");
		}
	}

	void resetAllStateVars() {
		push(0, ";; set default state vars");
		for (VariableDeclaration const *variable: ctx().notConstantStateVariables()) {
			pushDefaultValue(variable->type());
			setGlob(variable);
		}
		push(0, ";; end set default state vars");
	}

	void getGlob(VariableDeclaration const * vd) {
		const int index = ctx().getStateVarIndex(vd);
		getGlob(index);
	}

	void getGlob(int index) {
		solAssert(index >= 0, "");
		if (index <= 30) { // TODO 31
			push(+1, "GETGLOB " + toString(index));
		} else {
			solAssert(index < 255, "");
			pushInt(index);
			push(-1 + 1, "GETGLOBVAR");
		}
	}

	void setGlob(VariableDeclaration const * vd) {
		const int index = ctx().getStateVarIndex(vd);
		solAssert(index >= 0, "");
		if (index <= 30) { // TODO 31
			push(-1, "SETGLOB " + toString(index));
		} else {
			solAssert(index < 255, "");
			pushInt(index);
			push(-1 - 1, "SETGLOBVAR");
		}
	}

	void pushS(int i) {
		solAssert(i >= 0, "");
		if (i == 0) {
			push(+1, "DUP");
		} else {
			push(+1, "PUSH S" + toString(i));
		}
	}

	void pushInt(int i) {
		push(+1, "PUSHINT " + toString(i));
	}

	void loadArray(bool directOrder = true) {
		pushLines(R"(LDU 32
LDDICT
ROTREV
PAIR
)");
		if (directOrder) {
			exchange(0, 1);
		}
		push(-1 + 2, ""); // fix stack
		// stack: array slice
	}

	void preLoadArray() {
		pushLines(R"(LDU 32
PLDDICT
PAIR
)");
		push(-1 + 1, ""); // fix stack
		// stack: array
	}

	void load(const Type* type) {
		if (isUsualArray(type)) {
			loadArray();
		} else {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			string cmd = ti.isSigned ? "LDI " : "LDU ";
			push(-1 + 2, cmd + toString(ti.numBits));
		}
	}

	void preload(const Type* type) {
		if (isUsualArray(type)) {
			preLoadArray();
		} else if (type->category() == Type::Category::Mapping) {
			push(0, "PLDDICT");
		} else {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			string cmd = ti.isSigned ? "PLDI " : "PLDU ";
			push(-1 + 1, cmd + toString(ti.numBits));
		}
	}

	void pushZeroAddress() {
		push(+1, "PUSHSLICE x8000000000000000000000000000000000000000000000000000000000000000001_");
	}

	void generateC7ToT4Macro() {
		pushLines(R"(
.macro	c7_to_c4
GETGLOB 2
NEWC
STU 256
)");
		if (ctx().storeTimestampInC4()) {
			pushLines(R"(
GETGLOB 3
STUR 64
)");
		}
		pushLines(R"(
GETGLOB 6
STUR 1
)");
		if (!ctx().notConstantStateVariables().empty()) {
			structCompiler().stateVarsToBuilder();
		}
		pushLines(R"(
ENDC
POP C4
)");
	}

	static void addBinaryNumberToString(std::string &s, u256 value, int bitlen = 256) {
		for (int i = 0; i < bitlen; ++i) {
			s += value % 2 == 0? "0" : "1";
			value /= 2;
		}
		std::reverse(s.rbegin(), s.rbegin() + bitlen);
	}

	static std::string binaryStringToSlice(std::string & s) {
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

	static std::string gramsToBinaryString(Literal const* literal) {
		Type const* type = literal->annotation().type;
		u256 value = type->literalValue(literal);
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
		solAssert(len < 120, "Gram value should fit 120 bit");
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

	std::string literalToSliceAddress(Literal const* literal, bool pushSlice = true) {
		Type const* type = literal->annotation().type;
		u256 value = type->literalValue(literal);
//		addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt;
		std::string s;
		s += "10";
		s += "0";
		s += std::string(8, '0');
		addBinaryNumberToString(s, value);
		if (pushSlice)
			push(+1, "PUSHSLICE x" + binaryStringToSlice(s));
		return s;
	}

	bool tryImplicitConvert(Type const *leftType, Type const *rightType) {
		if (leftType->category() == Type::Category::FixedBytes && rightType->category() == Type::Category::StringLiteral) {
			auto stringLiteralType = to<StringLiteralType>(rightType);
			u256 value = 0;
			for (char c : stringLiteralType->value()) {
				value = value * 256 + c;
			}
			push(+1, "PUSHINT " + toString(value));
			return true;
		}
		return false;
	}

	void push(const CodeLines& codeLines) {
		for (const std::string& s : codeLines.lines) {
			push(0, s);
		}
	}

	void pushPrivateFunctionOrMacroCall(const int stackDelta, const string& fname) {
		push(stackDelta, "CALL $" + fname + "$");
	}

	void pushCall(const string& functionName, const FunctionType* ft) {
		int params  = ft->parameterTypes().size();
		int retVals = ft->returnParameterTypes().size();
		push(-params + retVals, "CALL $" + functionName + "$");
	}

	void pushFunctionIndex(const string& fname) {
		push(+1, "PUSHINT $" + fname + "$");
	}

	void dumpStackSize(const string& prefix = "") {
		push(0, prefix + ";; stack=" + toString(getStack().size()));
	}

	void drop(int cnt) {
		solAssert(cnt >= 0, "");
		if (cnt == 0)
			return;

		if (cnt == 1) {
			push(-1, "DROP");
		} else if (cnt == 2) {
			push(-2, "DROP2");
		} else {
			if (cnt > 15) {
				pushInt(cnt);
				push(-(cnt + 1), "DROPX");
			} else {
				push(-cnt, "BLKDROP " + toString(cnt));
			}
		}
	}

	void popS(int i) {
		solAssert(i >= 0, "");
		push(-1, "POP S" + toString(i));
	}

	void blockSwap(int m, int n) {
		if (m == 0 || n == 0) {
			return;
		}
		if (m == 1 && n == 1) {
			exchange(0, 1);
		} else if (m == 2 && n == 2) {
			push(0, "SWAP2");
		} else {
			push(0, "BLKSWAP " + toString(m) + ", " + toString(n));
		}
	}

	void reverse(int i, int j) {
		solAssert(i >= 2, "");
		solAssert(j >= 0, "");
		if (i == 2 && j == 0) {
			push(0, "SWAP");
		} else if (i == 3 && j == 0) {
			push(0, "XCHG s2");
		} else if (i - 2 <= 15 && j <= 15) {
			push(0, "REVERSE " + toString(i) + ", " + toString(j));
		} else {
			pushInt(i);
			pushInt(j);
			push(-2, "REVX");
		}
	}

	void dropUnder(int leftCount, int droppedCount) {
		// drop dropCount elements that are situated under top leftCount elements
		solAssert(leftCount >= 0, "");
		solAssert(droppedCount >= 0, "");

		auto f = [this, leftCount, droppedCount](){
			if (droppedCount > 15 || leftCount > 15) {
				pushInt(droppedCount);
				pushInt(leftCount);
				push(-2, "BLKSWX");
			} else {
				blockSwap(droppedCount, leftCount);
			}
			drop(droppedCount);
		};

		if (droppedCount == 0) {
			// nothing do
		} else if (leftCount == 0) {
			drop(droppedCount);
		} else if (droppedCount == 1) {
			if (leftCount == 1) {
				push(-1, "NIP");
			} else {
				f();
			}
		} else if (droppedCount == 2) {
			if (leftCount == 1) {
				push(-1, "NIP");
				push(-1, "NIP");
			} else {
				f();
			}
		} else {
			if (leftCount == 1) {
				exchange(0, droppedCount);
				drop(droppedCount);
			} else {
				f();
			}
		}
	}

	void exchange(int i, int j) {
		solAssert(i <= j, "");
		solAssert(i >= 0, "");
		solAssert(j >= 1, "");
		if (i == 0 && j <= 255) {
			if (j == 1) {
				push(0, "SWAP");
			} else if (j <= 15) {
				push(0, "XCHG s" + toString(j));
			} else {
				push(0, "XCHG s0,s" + toString(j));
			}
		} else if (i == 1 && 2 <= j && j <= 15) {
			push(0, "XCHG s1,s" + toString(j));
		} else if (1 <= i && i < j && j <= 15) {
			push(0, "XCHG s" + toString(i) + ",s" + toString(j));
		} else if (j <= 255) {
			exchange(0, i);
			exchange(0, j);
			exchange(0, i);
		} else {
			solAssert(false, "");
		}
	}

	static void restoreKeyAfterDictOperations(Type const* keyType, ASTNode const& node) {
		if (isStringOrStringLiteralOrBytes(keyType)) {
			cast_error(node, "Unsupported for mapping key type: " + keyType->toString(true));
		}
	}

	void prepareKeyForDictOperations(Type const* key) {
	    // stack: key dict
	    if (isStringOrStringLiteralOrBytes(key)) {
		    push(+1, "PUSH s1"); // str dict str
		    push(-1 + 1, "HASHCU"); // str dict hash
		    push(-1, "POP s2"); // hash dict
	    }
	}

	[[nodiscard]]
	// return true if result is a builder
	bool prepareValueForDictOperations(Type const* keyType, Type const* dictValueType, bool isValueBuilder) {
		// value
		if (isIntegralType(dictValueType)) {
			if (!isValueBuilder) {
				push(0, "NEWC");
				push(0, storeIntegralOrAddress(dictValueType, false));
				return true;
			}
		} else if (dictValueType->category() == Type::Category::Struct) {
			if (StructCompiler::isCompatibleWithSDK(lengthOfDictKey(keyType), to<StructType>(dictValueType))) {
				if (isValueBuilder) {
					return true;
				} else {
					StructCompiler sc{this, to<StructType>(dictValueType)};
					sc.tupleToBuilder();
					return true;
				}
			} else {
				if (!isValueBuilder) {
					StructCompiler sc{this, to<StructType>(dictValueType)};
					sc.tupleToBuilder();
				}
				push(+1, "NEWC");
				push(-2 + 1, "STBREF");
				return true;
			}
		} else if (isUsualArray(dictValueType)) {
			if (!isValueBuilder) {
				push(-1 + 2, "UNPAIR"); // size dict
				push(0, "SWAP"); // dict size
				push(+1, "NEWC"); // dict size builder
				push(-1, "STU 32"); // dict builder
				push(-1, "STDICT"); // builder
				return true;
			}
		} else if (to<TvmCellType>(dictValueType) || (to<ArrayType>(dictValueType) && to<ArrayType>(dictValueType)->isByteArray())) {
			if (isValueBuilder) {
				push(0, "ENDC");
				return false;
			}
		} else if (dictValueType->category() == Type::Category::Mapping) {
			if (!isValueBuilder) {
				push(+1, "NEWC"); // dict builder
				push(-1, "STDICT"); // builder
				return true;
			}
		}

		return isValueBuilder;
	}

	static TypePointer parseIndexType(Type const* type) {
		if (to<ArrayType>(type)) {
			return TypePointer(new IntegerType(32));
		}
		if (auto mappingType = to<MappingType>(type)) {
			return mappingType->keyType();
		}
		solAssert(false, "");
	}

	void setDict(Type const &keyType, Type const &valueType, bool isValueBuilder, ASTNode const& node) {
		// stack: value index dict
		int keyLength = lengthOfDictKey(&keyType);
		pushInt(keyLength);

		// stack: value index dict keyBitLength
		string dict_cmd;
		switch (valueType.category()) {
			case Type::Category::Address:
			case Type::Category::Contract:
				if (isValueBuilder) {
					dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
				} else {
					dict_cmd = "DICT" + typeToDictChar(&keyType) + "SET";
				}
				break;
			case Type::Category::TvmCell:
				solAssert(!isValueBuilder, "");
				dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETREF";
				break;
			case Type::Category::Struct:
				if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(&valueType))) {
					if (isValueBuilder) {
						dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
					} else {
						dict_cmd = "DICT" + typeToDictChar(&keyType) + "SET";
					}
				} else {
					solAssert(isValueBuilder, "");
					dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
				}
				break;
			case Type::Category::Integer:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
			case Type::Category::Enum:
				solAssert(isValueBuilder, "");
				dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
				break;
			case Type::Category::Array:
				if (!to<ArrayType>(&valueType)->isByteArray()) {
					solAssert(isValueBuilder, "");
					dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
				} else {
					solAssert(!isValueBuilder, "");
					dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETREF";
				}
				break;
			case Type::Category::Mapping:
				solAssert(isValueBuilder, "");
				dict_cmd = "DICT" + typeToDictChar(&keyType) + "SETB";
				break;
			default:
				cast_error(node, "Unsupported value type: " + valueType.toString());
		}

		push(-3, dict_cmd);
	}

	bool tryAssignParam(const string& name) {
		auto& stack = getStack();
		if (stack.isParam(name)) {
			int idx = stack.getOffset(name);
			solAssert(idx >= 0, "");
			if (idx == 0) {
				// nothing
			} else if (idx == 1) {
				push(-1, "NIP");
			} else {
				push(-1, "POP s" + toString(idx));
			}
			return true;
		}
		return false;
	}

	enum class DictOperation {
		GetFromArray,
		GetFromMapping,
		Fetch,
		Exist,
		MoveToC7
	};

	void getFromDict(const Type& keyType, const Type& valueType, ASTNode const& node, const DictOperation op,
						const bool resultAsSliceForStruct);



	void ensureValueFitsType(const ElementaryTypeNameToken& typeName, const ASTNode& node) {
		push(0, ";; " + typeName.toString());
		switch (typeName.token()) {
			case Token::IntM:
				push(0, "FITS " + toString(typeName.firstNumber()));
				break;
			case Token::UIntM:
				push(0, "UFITS " + toString(typeName.firstNumber()));
				break;
			case Token::BytesM:
				push(0, "UFITS " + toString(8 * typeName.firstNumber()));
				break;
			case Token::Int:
				push(0, "FITS 256");
				break;
			case Token::Address:
				// Address is a slice
				break;
			case Token::UInt:
				push(0, "UFITS 256");
				break;
			case Token::Bool:
				push(0, "FITS 1");
				break;
			default:
				cast_error(node, "Unimplemented casting");
		}
	}

	enum class ReasonOfOutboundMessage {
		EmitEventExternal,
		FunctionReturnExternal,
		RemoteCallInternal
	};

	class EncodePosition : private boost::noncopyable {
		int restSliceBits;
		int restFef;
		int qtyOfCreatedBuilders;

	public:
		explicit EncodePosition(int bits) :
				restSliceBits{TvmConst::CellBitLength - bits},
				restFef{4},
				qtyOfCreatedBuilders{0}
		{

		}

		bool needNewCell(Type const* type) {
			ABITypeSize size(type);
			solAssert(0 <= size.maxRefs && size.maxRefs <= 1, "");

			restSliceBits -= size.maxBits;
			restFef -= size.maxRefs;

			if (restSliceBits < 0 || restFef == 0) {
				restSliceBits =  TvmConst::CellBitLength - size.maxBits;
				restFef = 4 - size.maxRefs;
				++qtyOfCreatedBuilders;
				return true;
			}
			return false;
		}

		int countOfCreatedBuilders() {
			return qtyOfCreatedBuilders;
		}
	};

	void encodeFunctionAndParams(const string& functionName,
	                             const std::vector<Type const*>& types,
	                             const std::vector<ASTNode const*>& nodes,
	                             const std::function<void(size_t)>& pushParam, const ReasonOfOutboundMessage& reason) {
		push(+1, "NEWC");
		push(+1, "PUSHINT $" + functionName + "$");
		switch (reason) {
			case ReasonOfOutboundMessage::FunctionReturnExternal:
				push(+1, "PUSHINT " + to_string(0x80000000));
				push(-1, "OR");
				break;

			case ReasonOfOutboundMessage::EmitEventExternal:
				push(+1, "PUSHINT " + to_string(0x7fffffff));
				push(-1, "AND");
				break;

			default:
				break;
		}

		push(-1, "STUR 32");
		EncodePosition position{32};

		encodeParameters(types, nodes, pushParam, position);
	}

	void encodeParameters(const std::vector<Type const*>& types,
	                      const std::vector<ASTNode const*>& nodes,
	                      const std::function<void(size_t)>& pushParam,
	                      EncodePosition& position) {
		// builder must be situated on top stack
		solAssert(types.size() == nodes.size(), "");
		for (size_t idx = 0; idx < types.size(); idx++) {
			auto type = types[idx];
			encodeParameter(type, position, [&](){pushParam(idx);}, nodes[idx]);
		}
		for (int idx = 0; idx < position.countOfCreatedBuilders(); idx++) {
			push(-1, "STBREFR");
		}
	}

	void encodeParameter(Type const* type, EncodePosition& position, const std::function<void()>& pushParam, ASTNode const* node) {
		// stack: builder...
		if (auto structType = to<StructType>(type)) {
			pushParam(); // builder... struct
			encodeStruct(structType, node, position); // stack: builder...
		} else {
			if (position.needNewCell(type)) {
				push(+1, "NEWC");
			}

			if (isIntegralType(type) || isAddressOrContractType(type)) {
				pushParam();
				push(-1, storeIntegralOrAddress(type, true));
			} else if (auto arrayType = to<ArrayType>(type)) {
				if (arrayType->isByteArray()) {
					pushParam();
					push(-1, "STREFR");
				} else {
					pushParam();
					// builder array
					push(-1 + 2, "UNPAIR"); // builder size dict
					exchange(0, 2); // dict size builder
					push(-1, "STU 32"); // dict builder
					push(-1, "STDICT"); // builder
				}
			} else if (to<TvmCellType>(type)) {
				pushParam();
				push(-1, "STREFR");
			} else if (to<MappingType>(type)) {
				pushParam();
				push(0, "SWAP");
				push(-1, "STDICT");
			} else {
				cast_error(*node, "Unsupported type for encoding: " + type->toString());
			}
		}
	}

	void encodeStruct(const StructType* structType, ASTNode const* node, EncodePosition& position);
	void pushDefaultValue(Type const* type, bool isResultBuilder = false);
};

class IExpressionCompiler {
public:
	virtual void compileNewExpr(const Expression* expr) = 0;
	static bool isWithoutLogstr();

	struct LValueInfo {
		std::vector<Expression const*> expressions;
		std::vector<bool> isResultBuilder;
		bool isValueBuilder{};
	};

	virtual LValueInfo expandLValue(Expression const* const _expr, const bool withExpandLastValue, bool willNoStackPermutarion = false) = 0;
	virtual void collectLValue(const LValueInfo &lValueInfo, const bool haveValueOnStackTop, bool isValueBuilder) = 0;
};

}	// solidity
