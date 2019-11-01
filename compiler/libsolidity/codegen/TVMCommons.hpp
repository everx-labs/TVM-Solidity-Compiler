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

#include "TVMConstants.hpp"

using namespace std;
using namespace dev;
using namespace dev::solidity;
using namespace langutil;

#define DBG(x) cout << x << endl;

namespace dev {
namespace solidity {
	
template <typename T>	using string_map	= std::map<std::string, T>;
template <typename T>	using ptr_vec		= std::vector<ASTPointer<T>>;

template <typename T1, typename T2>
T1 const* to(T2 const* ptr) { return dynamic_cast<T1 const*>(ptr); }

template <typename T, typename TT>
T get_from_map(const string_map<T>& map, string key, TT defValue) {
	if (map.count(key) > 0)
		return map.at(key);
	return defValue;
}
	
class TVMStack {
	int m_size;
	// map paramters or local variables to their absolute stack position
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

	int getOffset(const string& name) /*const*/ {
		solAssert(isParam(name), "");
		return getOffset(m_params[name]);
	}

	int getOffset(int stackPos) {
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
	
	string str(const string& indent) const {
		std::ostringstream o;
		for (const string& s : lines) {
			o << indent << s << endl;
		}
		return o.str();
	}
	
	void push(const string& cmd) {
		if (cmd != "") {
			// space means empty line
			if (cmd == " ")
				lines.push_back("");
			else
				lines.push_back(cmd);
		}
	}
	
	void pushCont(const CodeLines& cont, const string& comment = "") {
		if (comment.empty())
			push("PUSHCONT {");
		else
			push("PUSHCONT { ; " + comment);
		for (const auto& l : cont.lines)
			push(string("\t") + l);
		push("}");
	}
	
	void append(const CodeLines& oth) {
		for (const auto& s : oth.lines) {
			lines.push_back(s);
		}
	}

	void generateGlobl(const string& fname,  const bool isPublic) {
		push(".globl\t" + fname);
		if (isPublic) {
			push(".public\t" + fname);
		}
		push(".type\t"  + fname + ", @function");
	}

	void generateInternal(const string& fname, const int id) {
		push(".internal-alias :" + fname + ",        " + toString(id));
		push(".internal\t:" + fname);
	}
};

struct ContInfo {
	bool canReturn = false;
	bool canBreak = false;
	bool canContinue = false;
	bool alwaysReturns = false;
	ContInfo() {}
};

class TVMScanner: public ASTConstVisitor
{
	int m_loopDepth = 0;
public:
	TVMScanner(const ASTNode& node) {
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

public:
	ContInfo m_info;
};

struct StructInfo {
	string_map<int> m_memberId;
	string_map<VariableDeclaration const*> m_memberDecl;
	StructDefinition const* m_def;

	StructInfo(StructDefinition const* structDef) : m_def(structDef) {
		auto members = structDef->members();
		for (size_t i = 0; i < members.size(); i++) {
			auto name = members[i]->name();
			m_memberId[name] = i;
			m_memberDecl[name] = members[i].get();
		}
	}

	StructInfo() : m_def(nullptr) {}
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
	return info;
}

bool isAddressType(const Type* type) {
	return to<AddressType>(type) || to<ContractType>(type);
}

struct TypeInfo {
	bool isNumeric {false};
	bool isSigned {false};
	int numBits {0};
	Type::Category category {Type::Category::Integer};

	explicit TypeInfo(const Type* type) {
		//category = type->category();
		if (auto* integerType = to<IntegerType>(type)) {
			isNumeric = true;
			isSigned = integerType->isSigned();
			numBits = integerType->numBits();
		} else if (isAddressType(type)) {
			isNumeric = true;
			isSigned = false;
			numBits = 2 + 1 + 8 + 256;
			category = Type::Category::Address;
		} else if (to<BoolType>(type)) {
			isNumeric = true;
			isSigned = true;
			numBits = 1;
		} else if (auto* fixedBytesType = to<FixedBytesType>(type)) {
			isNumeric = true;
			isSigned = false;
			numBits = 8 * static_cast<int>(fixedBytesType->numBytes());
		}
	}
};

bool isTvmIntrinsic(const string& name) {
	return 0 == name.find("tvm_");
}

const Type* getType(const Expression* expr) {
	return expr->annotation().type.get();
}

const Type* getType(const VariableDeclaration* var) {
	return var->annotation().type.get();
}

bool isIntegralType(const Type* type) {
	return TypeInfo(type).isNumeric;
}

string getStructName(const Type* type) {
	if (auto stype = to<StructType>(type))
		return stype->structDefinition().name();
	solAssert(false, "#552");
}

string makeStoreTypeCommand(const Type* type, bool reverse, bool old_mode = false) {
	if (isAddressType(type) && !old_mode)
		return reverse ? "STSLICER" : "STSLICE";
	TypeInfo ti = TypeInfo(type);
	if (ti.isNumeric) {
		string cmd = ti.isSigned? "STI" : "STU";
		if (reverse) cmd = cmd + "R";
		return cmd + " " + toString(ti.numBits);
	}
	solAssert(false, "Unsupported param type " + type->toString());
}

string makeLoadNumericTypeCommand(const Type* type) {
	TypeInfo ti = TypeInfo(type);
	solAssert(ti.isNumeric, "");
	string cmd = ti.isSigned? "LDI " : "LDU ";
	return cmd + toString(ti.numBits);
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

auto getContractFunctionPairs(ContractDefinition const* contract) {
	vector<pair<FunctionDefinition const*, ContractDefinition const*>> result;
	for (ContractDefinition const* c : getContractsChain(contract)) {
		for (const auto f : c->definedFunctions())
			result.push_back(make_pair(f, c));
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

bool isAddressThis(const FunctionCall* fcall) {
	if (!fcall)
		return false;
	auto arguments = fcall->arguments();
	if (auto etn = to<ElementaryTypeNameExpression>(&fcall->expression())) {
		if (etn->typeName().token() == Token::Address) {
			solAssert(arguments.size() == 1, "");
			if (auto arg0 = to<Identifier>(arguments[0].get())) {
				if (arg0->name() == "this")
					return true;
			}
		}
	}
	return false;
}

/*
auto findFunction(ContractDefinition const* contract0, const string& contractName, const string& functionName) {
	for (const auto& p : getContractFunctionPairs(contract0)) {
		if (p.first->name() == functionName && p.second->name() == contractName)
			return p;
	}
	return pair<FunctionDefinition const*, ContractDefinition const*> {nullptr, nullptr};
}
*/

// List of all function with a given name
auto getContractFunctions(ContractDefinition const* contract, const string& fname) {
	vector<FunctionDefinition const*> result;
	for (auto pair : getContractFunctionPairs(contract)) {
		if (pair.first->name() == fname)
			result.push_back(pair.first);
	}
	return result;
}

// List of all contract functions including derived
auto getContractFunctions(ContractDefinition const* contract) {
	vector<FunctionDefinition const*> result;
	for (auto pair : getContractFunctionPairs(contract)) {
		auto f = pair.first;
		auto fn = f->name();
		if (isTvmIntrinsic(fn))
			continue;
		// TODO: not needed check?
		if (!f->isConstructor() && f != getContractFunctions(contract, fn).back())
			continue;
		result.push_back(f);
	}
	return result;
}

auto getBaseContractNames(const ContractDefinition* contract) {
	vector<string> result;
	for (const auto& inheritanceSpecifier : contract->baseContracts()) {
		string name = inheritanceSpecifier->name().namePath()[0];
		result.push_back(name);
	}
	return result;
}
	
const ContractDefinition* getSuperContract(const ContractDefinition* currentContract,
														 const ContractDefinition* mainContract,
														 string fname) {
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

std::string ASTNode2String(const ASTNode& node, const string& error_messag = "") {
	ErrorList		m_errors;
	ErrorReporter	m_errorReporter(m_errors);
	m_errorReporter.parserError(node.location(), error_messag);
	string message = SourceReferenceFormatter::formatExceptionInformation(
		*(m_errorReporter.errors())[0],
		"Error"
	);
	return message;
}

[[noreturn]]
void cast_error(const ASTNode& node, const string& error_message) {
	cerr << ASTNode2String(node, error_message) << endl;
	std::exit(EXIT_FAILURE);
}

struct FuncInfo {
	const FunctionDefinition*	m_function;
	const ContractDefinition*	m_contract;
	string						m_internalName;
	bool						m_needsDecoder;
	
	FuncInfo(const FunctionDefinition* f, const ContractDefinition* c)
		: m_function(f), m_contract(c) 
	{
		m_needsDecoder = f->isPublic();
	}
};

class TVMCompilerContext {
	const ContractDefinition*				m_contract = nullptr;
	string_map<const FunctionDefinition*>	m_functions;
	map<const FunctionDefinition*, const ContractDefinition*>	m_function2contract;
	string_map<StructInfo>					m_structs;
	string_map<int>							m_members;
	string_map<const EventDefinition*>		m_events;
	set<string>								m_allContractNames;
	bool haveFallback = false;
	bool haveOnBounce = false;

	void addMember(VariableDeclaration const* variable) {
		string name = variable->name();
		solAssert(m_members.count(name) == 0, "Duplicate member variable " + name);
		m_members[name] = m_members.size() + TvmConst::C4::PersistenceMembersStartIndex;
	}

	void addEvent(EventDefinition const *event) {
		std::string name = event->name();
		solAssert(m_events.count(name) == 0, "Duplicate event " + name);
		m_events[name] = event;
	}

	void addStruct(StructDefinition const* structDef) {
		std::string name = structDef->name();
		solAssert(m_structs.count(name) == 0, "Duplicate struct " + name);
		m_structs[structDef->name()] = StructInfo(structDef);
	}

	void addFunction(FunctionDefinition const* _function) {
		if (!_function->isConstructor()) {
			string name = _function->name();
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
			for (StructDefinition const *structDef : c->definedStructs()) {
				addStruct(structDef);
			}
		}
		for (ContractDefinition const* c : getContractsChain(contract)) {
			for (VariableDeclaration const* variable: c->stateVariables())
				addMember(variable);
		}
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
		for (const auto f : getContractFunctions(contract)) {
			haveFallback |= f->isFallback();
			haveOnBounce |= f->name() == "onBounce";
			if (isPureFunction(f))
				continue;
			addFunction(f);
		}
		for (const auto pair : getContractFunctionPairs(contract)) {
			auto f = pair.first;
			auto c = pair.second;
			if (!isTvmIntrinsic(f->name()) && !isPureFunction(f)) {
				FuncInfo fi(f, c);
				fi.m_internalName = getFunctionInternalName(f);
				if (!f->isConstructor() && f != getContractFunctions(contract, f->name()).back()) {
					fi.m_internalName = c->name() + "_" + f->name();
					fi.m_needsDecoder = false;
				}
				bool isBaseConstructor = f->isConstructor() && !isMainConstructor(f);
				// Base constructors should not be exported
				if (isBaseConstructor || f->name() == "onTickTock")
					fi.m_needsDecoder = false;
				m_functionsList.push_back(fi);
			}
		}
		for (auto c : allContracts) {
			m_allContractNames.insert(c->name());
		}
	}
	
public:
	TVMCompilerContext(ContractDefinition const* contract, const std::vector<ContractDefinition const*>& allContracts) {
		initMembers(contract, allContracts);
	}
	
	mutable set<string>						m_remoteFunctions;
	vector<FuncInfo>						m_functionsList;
	const FuncInfo *						m_currentFunction = nullptr;
	map<string, CodeLines>					m_inlinedFunctions;

	bool isStdlib() const {
		return m_contract->name() == "stdlib";
	}
	
	static const FunctionDefinition* getConstructor(const ContractDefinition* contract) {
		for (const auto& _function : contract->definedFunctions()) {
			if (_function->isConstructor())
				return _function;
		}
		return nullptr;
	}
	
	// TODO: not needed? Refactor its usage
	static bool hasConstructor(const ContractDefinition* contract) {
		for (const auto& _function : contract->definedFunctions()) {
			if (_function->isConstructor())
				return true;
			if (_function->name() == "tvm_dont_generate_constructor")
				return true;
		}
		return false;
	}
	
	string getFunctionInternalName(FunctionDefinition const* _function) const {
		if (isStdlib())
			return _function->name();
		if (_function->isConstructor()) {
			auto contract = getContract(_function);
			solAssert(contract, "");
			return "constructor_" + contract->name();
		}
        if (_function->isFallback()) {
            return "fallback_internal";
        }
		return _function->name() + "_internal";
	}
	
	static string getFunctionExternalName(FunctionDefinition const* _function) {
		string fname = _function->name();
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
	
	bool isMainConstructor(const FunctionDefinition* func) const {
		return func->isConstructor() && getContract() == getContract(func);
	}

	int getMemberIdx(const string& name) const {
		return get_from_map(m_members, name, -1);
	}
	
	const FunctionDefinition* getLocalFunction(string fname) const {
		return get_from_map(m_functions, fname, nullptr);
	}
	
	bool isStruct(string name) const {
		return m_structs.count(name) > 0;
	}
	
	int getStructFieldIndex(string structName, string memberName) const {
		return m_structs.at(structName).m_memberId.at(memberName);
	}
	
	const Type* getStructFieldType(string structName, string memberName) const {
		return getType(m_structs.at(structName).m_memberDecl.at(memberName));
	}
	
	const ptr_vec<VariableDeclaration>& getStructMembers(string sname) const {
		return m_structs.at(sname).m_def->members();
	}
	
	const EventDefinition* getEvent(string name) const {
		return get_from_map(m_events, name, nullptr);
	}
	
	bool isContractName(string name) const {
		return m_allContractNames.count(name) > 0;
	}

	bool haveFallbackFunction() const {
		return haveFallback;
	}

	bool haveOnBounceHandler() const {
		return haveOnBounce;
	}

	std::vector<const EventDefinition*> events() const {
		std::vector<const EventDefinition*> result;
		for (const auto& [name, event] : m_events) {
			(void)name;
			result.push_back(event);
		}
		return result;
	}
};

class IStackPusher {
public:
	virtual void push(int stackDiff, const string& cmd) = 0;
	virtual TVMStack& getStack() = 0;
};

struct StackPusherImpl : IStackPusher {
	TVMStack&					m_stack;
	CodeLines&					m_code;
	
	StackPusherImpl(TVMStack& stack, CodeLines&	code) 
		: m_stack(stack)
		, m_code(code) 
		{}
	
	void push(int stackDiff, const string& cmd) override {
		m_code.push(cmd);
		m_stack.change(stackDiff);
	}
	
	TVMStack& getStack() override {
		return m_stack;
	}
	
};

class StackPusherHelper {
private:
	IStackPusher* const					m_pusher;
	const TVMCompilerContext* const		m_ctx;

public:
	StackPusherHelper(IStackPusher* compiler, const TVMCompilerContext* ctx) 
		: m_pusher(compiler), m_ctx(ctx) {}
		
	const TVMCompilerContext&  ctx() const {
		return *m_ctx;
	}

	TVMStack& getStack() {
		return m_pusher->getStack();
	}
	
	auto getStackPusher() const {
		return m_pusher;
	}

	void pushLog(const std::string& str) {
		push(0, "PRINTSTR " + str);
	}

	void pushLines(const std::string& lines) {
		std::istringstream stream{lines};
		std::string line;
		while (std::getline(stream, line)) {
			push(0, line);
		}
	}

	void push(int stackDiff, const string& cmd) {
		solAssert(m_pusher, "#18");
		m_pusher->push(stackDiff, cmd);
	}

	void pushInt(int i) {
		push(+1, "PUSHINT " + toString(i));
	}
	
	void pushPrivateFunctionCall(const int stackDelta, const string& fname) {
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

	void pushInlineCall(const FunctionType* ft) {
		auto nParams  = ft->parameterTypes().size();
		auto nRetVals = ft->returnParameterTypes().size();
		push(-nParams-1+nRetVals, "CALLX");
	}
	
	void dumpStackSize(string prefix = "") {
		push(0, prefix + ";; stack=" + toString(m_pusher->getStack().size()));
	}
	
	void drop(int cnt) {
		solAssert(cnt >= 0, "");
		if (cnt == 0)
			return;
		if (cnt == 1)
			push(-1, "DROP");
		else
			push(-cnt, "BLKDROP " + toString(cnt));
	}

	void dropUnder(int leftCount, int droppedCount) {
		// drop dropCount elements that are situated under top leftCount elements
		solAssert(leftCount >= 0, "");
		solAssert(droppedCount >= 0, "");

		if (droppedCount == 0) {
			// nothing do
		} else if (leftCount == 0) {
			drop(droppedCount);
		} else if (droppedCount == 1) {
			if (leftCount == 1) {
				push(-1, "NIP");
			} else {
				push(0, "BLKSWAP " + toString(droppedCount) + ", " + toString(leftCount));
				drop(droppedCount);
			}
		} else if (droppedCount == 2) {
			if (leftCount == 1) {
				push(-1, "NIP");
				push(-1, "NIP");
			} else {
				push(0, "BLKSWAP " + toString(droppedCount) + ", " + toString(leftCount));
				drop(droppedCount);
			}
		} else {
			if (leftCount == 1) {
				push(0, "XCHG s" + toString(droppedCount));
				drop(droppedCount);
			} else {
				push(0, "BLKSWAP " + toString(droppedCount) + ", " + toString(leftCount));
				drop(droppedCount);
			}
		}
	}

	void intToBuilder(const Type* type) {
		if (isIntegralType(type)) {
			push(0, "NEWC " + makeStoreTypeCommand(type, false));
		} else {
			solAssert(false, "");
		}
	}

	void setRootItem(const string& name, const Type* type) {
		if (isIntegralType(type)) {
			intToBuilder(type);
		}
		pushInt(ctx().getMemberIdx(name));
		pushPersistentDataDict();
		setDict(isIntegralType(type), TvmConst::C4::KeyLength, false);
		setPersistentDataDict();
	}

	void setDict(bool isDictValueInteger, int numBits, bool isSigned, Type::Category keyCategory = Type::Category::Integer) {
		// expects on stack: value index dict
		pushInt(numBits);

		string dict_cmd;
		if (isDictValueInteger) {
			if (keyCategory == Type::Category::Integer) {
				dict_cmd = isSigned ? "DICTISETB" : "DICTUSETB";
			} else {
				dict_cmd = "DICTSETB";
			}
		} else {
			if (keyCategory == Type::Category::Integer) {
				dict_cmd = std::string(isSigned? "DICTISETGETOPTREF" : "DICTUSETGETOPTREF") + " DROP";
			} else {
				dict_cmd = "DICTSETGETOPTREF DROP";
			}
		}
		push(-3, dict_cmd);
	}

	void pushPersistentDataDict() {
		push(+1, "PUSH c7 THIRD");
	}

	void setPersistentDataDict() {
		push(+1, "PUSH c7");
		push(0, "SWAP");
		push(-1, "SETTHIRD");
		push(-1, "POP c7");
	}

	bool tryAssignParam(const string& name) {
		auto& stack = m_pusher->getStack();
		if (stack.isParam(name)) {
			int idx = stack.getOffset(name);
			solAssert(idx > 0, "");
			if (idx == 1) {
				push(-1, "NIP");
			} else {
				push(-1, "POP s" + toString(idx));
			}
			return true;
		}
		return false;
	}
	
	void assignVar(Identifier const* variable) {
		auto name = variable->name();
		push(0, string(";; assign ") + name);
		if (tryAssignParam(name)) {
		} else if (ctx().getMemberIdx(name) >= 0) {
			setRootItem(name, getType(variable));
		} else {
			solAssert(false, string("Invalid assigment ") + name);
		}
	}

	void getFromDict(const Type* resultType, int nbits = 256, bool isSigned = false, Type::Category keyCategory = Type::Category::Integer) {
		// expects on stack: index dict
		const Type::Category resultCategory = resultType->category();
		pushInt(nbits);

		if (isIntegralType(resultType)) {
			if (keyCategory == Type::Category::Address) {
				push(-3 + 2, "DICTGET");
			} else {
				push(-3 + 2, isSigned ? "DICTIGET" : "DICTUGET");
			}
			if (isAddressType(resultType)) {
				push(+1, "PUSHCONT {");
				push(0 , "	PUSHINT 0");
				pushPrivateFunctionCall(0, "make__MsgAddressInt__addr_std_10");
				push(0 , "	ENDC");
				push(0 , "	CTOS");
				push(0 , "}");
				push(-2, "IFNOT");
			} else {
				push(+1, "PUSHCONT { " + makeLoadNumericTypeCommand(resultType) + " ENDS }");
				push(+1, "PUSHCONT { PUSHINT 0 }");
				push(-3, "IFELSE");
			}
		} else {
			switch (resultCategory) {
				// TODO: support tuples here
				case Type::Category::Mapping:
				case Type::Category::Struct:
				case Type::Category::Array:
					if (keyCategory == Type::Category::Integer) {
						push(-3 + 1, "DICTUGETOPTREF");
					} else {
						push(-3 + 1, "DICTGETOPTREF");
					}
					break;
				default:
					solAssert(false, "Unsupported identifier type: " + toString((int)resultCategory));
					break;
			}
		}
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
	
	void ensureValueFitsType(const ElementaryTypeNameToken& typeName) {
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
				solAssert(false, "Unsupported typeName " + typeName.toString());
		}
	}

	enum class ReasonOfOutboundMessage {
		EmitEventExternal,
		FunctionReturnExternal,
		RemoteCallInternal
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

		encodeParams(types, nodes, pushParam);
	}

	void encodeParams(const std::vector<Type const*>& types,
	                  const std::vector<ASTNode const*>& nodes,
	                  const std::function<void(size_t)>& pushParam) {
		solAssert(types.size() == nodes.size(), "");
		for (size_t idx = 0; idx < types.size(); idx++) {
			push(+1, "NEWC");
			auto type = types[idx];
			pushParam(idx);
			if (isIntegralType(type)) {
				push(-1, makeStoreTypeCommand(type, true));
			} else if (auto arrayType = to<ArrayType>(type)) {
				if (arrayType->isByteArray()) {
					push(-1, "STREFR");
				} else {
					pushPrivateFunctionCall(-2 + 1, "encode_array");
				}
			} else if (auto structType = to<StructType>(type)) {
				encodeStruct(structType, nodes[idx]);
			} else {
				cast_error(*nodes[idx], "Unsupported type : " + type->toString());
			}
		}
		for(size_t idx = 0; idx < types.size(); idx++) {
			push(-1, "STBREFR");
		}
	}

	void encodeStruct(const StructType* structType, ASTNode const* param) {
		const StructDefinition& strDef = structType->structDefinition();
		const auto& members = strDef.members();
		push(0, ";; store struct " + strDef.name());
		// ; stack: builder struct
		auto savedStackSize = getStack().size();
		push( 0, "SWAP");
		// ; stack: struct builder
		for (size_t i = 0; i < members.size(); i++) {
			auto member = members[i].get();
			auto mtype = getType(member);
			push(0, ";; " + strDef.name() + "." + member->name());

			// ; stack: struct builder
			pushInt(i);
			push(+1, "PUSH s2");
			// ; stack: struct builder i struct
			pushInt(8);
			if (isIntegralType(mtype)) {
				push(-3+2, "DICTUGET");
				push(-1, "THROWIFNOT 98");
				// TODO: This is a place for future optimizations. There is no need
				// to load integer from slice and then store it to builder.
				// Just store slice as is with STSLICE.
				// ; stack: struct builder s.x-cell
				push(+1, makeLoadNumericTypeCommand(mtype));
				push(-1, "DROP");
				push(-1, makeStoreTypeCommand(mtype, true));
			} else if (auto structType2 = to<StructType>(mtype)) {
				// ; stack: struct builder struct2
				push(-3+1, "DICTUGETOPTREF");
				encodeStruct(structType2, param);
			} else {
				cast_error(*param, "Unsupported type in structure: " + mtype->toString());
			}
		}
		getStack().ensureSize(savedStackSize, "encodeStruct");
		// drop struct
		push(-1, "POP s1");
	}
};

class ITVMCompiler : public IStackPusher {
public:
	virtual const FunctionDefinition* getRemoteFunctionDefinition(const MemberAccess* memberAccess)	= 0;
	virtual CodeLines proceedContinuationExpr(const Expression& expression)		= 0;
	virtual void applyContinuation(const CodeLines& lines) 						= 0;
};

class IExpressionCompiler {
public:
	virtual void acceptExpr(const Expression* expr) = 0;
	virtual void acceptExpr2(const Expression* expr, const bool isResultNeeded = true) = 0;
};

}	// solidity
}	// dev
