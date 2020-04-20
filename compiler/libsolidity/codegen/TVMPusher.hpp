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
 */

#pragma once

#include <map>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/Exceptions.h>
#include <liblangutil/SourceReferenceFormatter.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>

#include "TVMCommons.hpp"
#include "TVMConstants.hpp"

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;

namespace solidity::frontend {

class TVMStack {
	int m_size;
	// map parameters or local variables to their absolute stack position
	std::map<Declaration const*, int> m_params;

public:
	TVMStack();
	int size() const;
	void change(int diff);
	bool isParam(Declaration const* name) const;
	void add(Declaration const* name, bool doAllocation);
	int getOffset(Declaration const* name) const;
	int getOffset(int stackPos) const;
	int getStackSize(Declaration const* name) const;
	void ensureSize(int savedStackSize, const string& location) const;
};

struct CodeLines {
	vector<string> lines;
	int tabQty{};

	string str(const string& indent = "") const;
	void addTabs(const int qty = 1);
	void subTabs(const int qty = 1);
	void startContinuation();
	void endContinuation();
	void push(const string& cmd);
	void append(const CodeLines& oth);
};

class TVMCompilerContext {
	const ContractDefinition*				m_contract = nullptr;
	string_map<const FunctionDefinition*>	m_functions;
	map<const FunctionDefinition*, const ContractDefinition*>	m_function2contract;
	string_map<const EventDefinition*>		m_events;
	bool haveFallback = false;
	bool haveOnBounce = false;
	bool haveReceive = false;
	bool ignoreIntOverflow = false;
	bool m_haveSetDestAddr = false;
	PragmaDirectiveHelper const& m_pragmaHelper;
	std::map<VariableDeclaration const *, int> m_stateVarIndex;

	void addEvent(EventDefinition const *event);
	void addFunction(FunctionDefinition const* _function);
	void initMembers(ContractDefinition const* contract);

public:
	TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper);

	mutable set<string>		m_remoteFunctions;
	vector<FunctionDefinition const*>		m_functionsList;
	FunctionDefinition const*               m_currentFunction = nullptr;
	map<string, CodeLines>	m_inlinedFunctions;

	int getStateVarIndex(VariableDeclaration const *variable) const;
	std::vector<VariableDeclaration const *> notConstantStateVariables() const;
	PragmaDirectiveHelper const& pragmaHelper() const;
	bool haveTimeInAbiHeader() const;
	bool isStdlib() const;
	bool haveSetDestAddr() const;
	string getFunctionInternalName(FunctionDefinition const* _function) const;
	static string getFunctionExternalName(FunctionDefinition const* _function);
	bool isPureFunction(FunctionDefinition const* f) const;
	const ContractDefinition* getContract() const;
	const ContractDefinition* getContract(const FunctionDefinition* f) const;
	const FunctionDefinition* getLocalFunction(const string& fname) const;
	const EventDefinition* getEvent(const string& name) const;
	bool haveFallbackFunction() const;
	bool haveReceiveFunction() const;
	bool haveOnBounceHandler() const;
	bool ignoreIntegerOverflow() const;
	std::vector<const EventDefinition*> events() const;
	FunctionDefinition const* afterSignatureCheck() const;
	bool storeTimestampInC4() const;
};

class StructCompiler;

class StackPusherHelper {
protected:
	TVMStack m_stack;
	CodeLines m_code;
	const TVMCompilerContext* const m_ctx;
	StructCompiler* m_structCompiler;

public:
	explicit StackPusherHelper(const TVMCompilerContext* ctx, const int stackSize = 0);
	void tryPollLastRetOpcode();
	void append(const CodeLines& oth);
	void addTabs(const int qty = 1);
	void subTabs(const int qty = 1);
	void pushCont(const CodeLines& cont, const string& comment = "");
	void generateGlobl(const string& fname, const bool isPublic);
	void generateInternal(const string& fname, const int id);
	void generateMacro(const string& functionName);
	CodeLines code() const;

	[[nodiscard]]
	const TVMCompilerContext& ctx() const;
	void push(int stackDiff, const string& cmd);
	void startContinuation();
	void endContinuation();
	StructCompiler& structCompiler();
	TVMStack& getStack();
	void pushLog(const std::string& str);
	void pushLines(const std::string& lines);
	void untuple(int n);
	void index(int index);
	void set_index(int index);
	void tuple(int qty);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const * vd);
	void getGlob(int index);
	void setGlob(int index);
	void setGlob(VariableDeclaration const * vd);
	void pushS(int i);
	void pushInt(int i);
	void stzeroes(int qty);
	void stones(int qty);
	void sendrawmsg();
	void loadArray(bool directOrder = true);
	void preLoadArray();
	void load(const Type* type);
	void preload(const Type* type);
	void pushZeroAddress();
	void generateC7ToT4Macro();
	static void addBinaryNumberToString(std::string &s, u256 value, int bitlen = 256);
	static std::string binaryStringToSlice(const std::string & s);
	static std::string gramsToBinaryString(Literal const* literal);
	static std::string gramsToBinaryString(u256 value);
	std::string literalToSliceAddress(Literal const* literal, bool pushSlice = true);
	bool tryImplicitConvert(Type const *leftType, Type const *rightType);
	void push(const CodeLines& codeLines);
	void pushPrivateFunctionOrMacroCall(const int stackDelta, const string& fname);
	void pushCall(const string& functionName, const FunctionType* ft);
	void drop(int cnt);
	void blockSwap(int m, int n);
	void reverse(int i, int j);
	void dropUnder(int leftCount, int droppedCount);
	void exchange(int i, int j);
	static void restoreKeyAfterDictOperations(Type const* keyType, ASTNode const& node);
	void prepareKeyForDictOperations(Type const* key);
	[[nodiscard]]
	std::pair<std::string, int> int_msg_info(const std::set<int> &isParamOnStack, const std::map<int, std::string> &constParams);
	void appendToBuilder(const std::string& bitString);

	[[nodiscard]]
	// return true if result is a builder
	bool prepareValueForDictOperations(Type const* keyType, Type const* dictValueType, bool isValueBuilder);
	static TypePointer parseIndexType(Type const* type);
	static TypePointer parseValueType(IndexAccess const& indexAccess);
	void setDict(Type const &keyType, Type const &valueType, bool isValueBuilder, ASTNode const& node);
	bool tryAssignParam(Declaration const* name);

	enum class DictOperation {
		GetFromArray,
		GetFromMapping,
		Fetch,
		Exist
	};

	void getFromDict(const Type& keyType, const Type& valueType, ASTNode const& node, const DictOperation op,
	                 const bool resultAsSliceForStruct);

	void ensureValueFitsType(const ElementaryTypeNameToken& typeName, const ASTNode& node);

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
		explicit EncodePosition(int bits);
		bool needNewCell(Type const* type);
		int countOfCreatedBuilders() const;
		int restBits() { return restSliceBits; }
	};

	int encodeFunctionAndParams(const string& functionName,
	                             const std::vector<Type const*>& types,
	                             const std::vector<ASTNode const*>& nodes,
	                             const std::function<void(size_t)>& pushParam, const ReasonOfOutboundMessage& reason);

	void encodeParameters(const std::vector<Type const*>& types,
	                      const std::vector<ASTNode const*>& nodes,
	                      const std::function<void(size_t)>& pushParam,
	                      EncodePosition& position);

	void encodeParameter(Type const* type, EncodePosition& position, const std::function<void()>& pushParam, ASTNode const* node);
	void encodeStruct(const StructType* structType, ASTNode const* node, EncodePosition& position);
	void pushDefaultValue(Type const* type, bool isResultBuilder = false);
	void sendIntMsg(const std::map<int, Expression const*>& exprs,
					const std::map<int, std::string> &constParams,
	                const std::function<int()> &pushBody,
	                const std::function<void()> &pushSendrawmsgFlag);
};

CodeLines switchSelectorIfNeed(FunctionDefinition const* f);

} // end solidity::frontend
