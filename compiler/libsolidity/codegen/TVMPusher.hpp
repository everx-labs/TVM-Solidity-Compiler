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
#include "TVMABI.hpp"

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;

namespace solidity::frontend {

class StructCompiler;

class DictOperation {
public:
	DictOperation(StackPusherHelper& pusher, Type const& keyType, Type const& valueType);
protected:
	StackPusherHelper& pusher;
	Type const& keyType;
	const int keyLength{};
	Type const& valueType;
	const Type::Category valueCategory{};
};

class TVMStack : public boost::noncopyable {
    int m_size{};
	std::vector<Declaration const*> m_stackSize;

public:
	TVMStack() = default;
	int size() const;
	void change(int diff);
	bool isParam(Declaration const* name) const;
	void add(Declaration const* name, bool doAllocation);
	int getOffset(Declaration const* name) const;
	int getOffset(int stackPos) const;
	int getStackSize(Declaration const* name) const;
	void ensureSize(int savedStackSize, const string& location = "", const ASTNode* node = nullptr) const;
};

struct CodeLines {
	vector<string> lines;
	int tabQty{};

	string str(const string& indent = "") const;
	void addTabs(int qty = 1);
	void subTabs(int qty = 1);
	void startContinuation();
	void startIfRef();
	void startIfJmpRef();
	void startIfNotRef();
	void startCallRef();
	void endContinuation();
	void push(const string& cmd);
	void append(const CodeLines& oth);
};

class TVMCompilerContext {
public:
	TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper);
	void initMembers(ContractDefinition const* contract);
	int getStateVarIndex(VariableDeclaration const *variable) const;
	std::vector<VariableDeclaration const *> notConstantStateVariables() const;
	std::vector<Type const *> notConstantStateVariableTypes() const;
	std::vector<std::string> notConstantStateVariableNames() const;
	PragmaDirectiveHelper const& pragmaHelper() const;
	bool haveTimeInAbiHeader() const;
	bool isStdlib() const;
	string getFunctionInternalName(FunctionDefinition const* _function) const;
	static string getFunctionExternalName(FunctionDefinition const* _function);
	const ContractDefinition* getContract() const;
	bool ignoreIntegerOverflow() const;
	FunctionDefinition const* afterSignatureCheck() const;
	bool storeTimestampInC4() const;
	void addLib(FunctionDefinition const* f);
	const std::set<FunctionDefinition const*>& getLibFunctions() const { return m_libFunctions; }
	std::vector<std::pair<VariableDeclaration const*, int>> getStaticVariables() const;
	void setCurrentFunction(FunctionDefinition const* f) { m_currentFunction = f; }
	FunctionDefinition const* getCurrentFunction() { return m_currentFunction; }
	void addInlineFunction(const std::string& name, const CodeLines& code);
	CodeLines getInlinedFunction(const std::string& name);
	void addPublicFunction(uint32_t functoinId, const std::string& functionName);
	const std::vector<std::pair<uint32_t, std::string>>& getPublicFunctions();

	bool addAndDoesHaveLoop(FunctionDefinition const* _v, FunctionDefinition const* _to);
	bool dfs(FunctionDefinition const* v);
	bool isFallBackGenerated() const { return m_isFallBackGenerated; }
	void setIsFallBackGenerated() { m_isFallBackGenerated = true; }
	bool isReceiveGenerated() const { return m_isReceiveGenerated; }
	void setIsReceiveGenerated() { m_isReceiveGenerated = true; }
	bool isOnBounceGenerated() const { return m_isOnBounceGenerated; }
	void setIsOnBounce() { m_isOnBounceGenerated = true; }

private:
	ContractDefinition const* m_contract{};
	bool ignoreIntOverflow{};
	PragmaDirectiveHelper const& m_pragmaHelper;
	std::map<VariableDeclaration const*, int> m_stateVarIndex;
	std::set<FunctionDefinition const*> m_libFunctions;
	FunctionDefinition const* m_currentFunction{};
	std::map<std::string, CodeLines> m_inlinedFunctions;
	std::map<FunctionDefinition const*, std::set<FunctionDefinition const*>> graph;
	enum class Color {
		White, Red, Black
	};
	std::map<FunctionDefinition const*, Color> color;
	std::vector<std::pair<uint32_t, std::string>> m_publicFunctoins;
	bool m_isFallBackGenerated{};
	bool m_isReceiveGenerated{};
	bool m_isOnBounceGenerated{};
};

class StackPusherHelper {
protected:
	TVMStack m_stack;
	CodeLines m_code;
	TVMCompilerContext* m_ctx;
	std::unique_ptr<StructCompiler> m_structCompiler;

public:
	enum StoreFlag {ValueIsBuilder = 1, ArrayIsUntupled = 2, StoreStructInRef = 4, StoreStructInOneCell = 8};

public:
	explicit StackPusherHelper(TVMCompilerContext* ctx, const int stackSize = 0);

	void tryPollLastRetOpcode();
	bool tryPollConvertBuilderToSlice();
	bool tryPollEmptyPushCont();
	bool cmpLastCmd(const std::string& cmd, int offset = 0);
	void pollLastOpcode();
	bool optimizeIf();

	void append(const CodeLines& oth);
	void addTabs(const int qty = 1);
	void subTabs(const int qty = 1);
	void pushCont(const CodeLines& cont, const string& comment = {});
	void generateGlobl(const string& fname);
	void generateInternal(const string& fname, const int id);
	void generateMacro(const string& functionName);
	CodeLines code() const;

	[[nodiscard]]
	TVMCompilerContext& ctx();
	void push(int stackDiff, const string& cmd);

	void startContinuation(int deltaStack = 0);
	void startIfRef(int deltaStack = 0);
	void startIfJmpRef(int deltaStack = 0);
	void startIfNotRef(int deltaStack = 0);
	void startCallRef(int deltaStack = 0);
	void endContinuation(int deltaStack = 0);

	StructCompiler& structCompiler();
	TVMStack& getStack();
	void pushLog();
	void pushLines(const std::string& lines);
	void untuple(int n);
	void index(int index);
	void setIndex(int index);
	void setIndexQ(int index);
	void tuple(int qty);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const * vd);
	void getGlob(int index);
	void setGlob(int index);
	void setGlob(VariableDeclaration const * vd);
	void pushS(int i);
	void popS(int i);
	void pushInt(const bigint& i);
	void stzeroes(int qty);
	void stones(int qty);
	void sendrawmsg();
	// return true if on stack there are (value, slice) else false if (slice, value)
	[[nodiscard]]
	bool fastLoad(const Type* type);
	void load(const Type* type, bool reverseOrder);

	enum Preload { ReturnStructAsSlice = 1, UseCurrentSlice = 2, IsAddressInEnd = 4 };
	void preload(const Type *type, uint32_t mask);

	void store(const Type *type, bool reverse, uint32_t mask);
	void pushZeroAddress();
	void generateC7ToT4Macro();

	static void addBinaryNumberToString(std::string &s, u256 value, int bitlen = 256);
	static std::string binaryStringToSlice(const std::string & s);
	static std::string tonsToBinaryString(Literal const* literal);
	static std::string tonsToBinaryString(const u256& value);
	static std::string tonsToBinaryString(bigint value);
	std::string literalToSliceAddress(Literal const* literal, bool pushSlice = true);
	static bigint pow10(int power);

	void hardConvert(Type const *leftType, Type const *rightType);
	void checkFit(Type const *type);
	void push(const CodeLines& codeLines);
	void pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params);
	void pushMacroCallInCallRef(int stackDelta, const string& fname);
	void pushCallOrCallRef(const string& functionName, FunctionType const* ft, const std::optional<int>& deltaStack = nullopt);
	void pushCall(int delta, const std::string& functionName);
	void drop(int cnt = 1);
	void blockSwap(int m, int n);
	void reverse(int i, int j);
	void dropUnder(int leftCount, int droppedCount);
	void exchange(int i, int j);
	void prepareKeyForDictOperations(Type const* key, bool doIgnoreBytes);
	[[nodiscard]]
	int int_msg_info(const std::set<int> &isParamOnStack, const std::map<int, std::string> &constParams);
	[[nodiscard]]
	int ext_msg_info(const std::set<int> &isParamOnStack, bool isOut);
	void appendToBuilder(const std::string& bitString);
	void checkOptionalValue();
	bool doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value);
	[[nodiscard]]
	int maxBitLengthOfDictValue(Type const* type);
	[[nodiscard]]
	DataType prepareValueForDictOperations(Type const* keyType, Type const* dictValueType, bool isValueBuilder);
	bool doesDictStoreValueInRef(Type const* keyType, Type const* valueType);

	enum class DecodeType {
		DecodeValue,
		DecodeValueOrPushDefault,
		DecodeValueOrPushNull,
		PushNullOrDecodeValue
	};

	void recoverKeyAndValueAfterDictOperation(
		Type const* keyType,
		Type const* valueType,
		bool haveKey,
		bool didUseOpcodeWithRef,
		const DecodeType& decodeType,
		bool resultAsSliceForStruct
	);
	static TypePointer parseIndexType(Type const* type);
	static TypePointer parseValueType(IndexAccess const& indexAccess);

	enum class SetDictOperation { Set, Replace, Add };
	void setDict(
		Type const &keyType,
		Type const &valueType,
		const DataType& dataType,
		SetDictOperation opcode = SetDictOperation::Set
	);

	bool tryAssignParam(Declaration const* name);

	enum class GetDictOperation {
		GetFromMapping,
		GetSetFromMapping,
		GetAddFromMapping,
		GetReplaceFromMapping,
		GetFromArray,
		Fetch,
		Exist
	};
	void getDict(
		const Type& keyType,
		const Type& valueType,
		const GetDictOperation op,
		const bool resultAsSliceForStruct,
  		const DataType& dataType = DataType::Slice
	);

	void pushNull();
	void pushDefaultValue(Type const* type, bool isResultBuilder = false);
	void sendIntMsg(const std::map<int, const Expression *> &exprs,
					const std::map<int, std::string> &constParams,
					const std::function<void(int)> &appendBody,
					const std::function<void()> &pushSendrawmsgFlag);

	enum class MsgType{
		Internal,
		ExternalOut,
		ExternalIn
	};

	void sendMsg(const std::set<int>& isParamOnStack,
				 const std::map<int, std::string> &constParams,
				 const std::function<void(int)> &appendBody,
				 const std::function<void()> &appendStateInit,
				 const std::function<void()> &pushSendrawmsgFlag,
				 MsgType messageType = MsgType::Internal);

	void prepareMsg(const std::set<int>& isParamOnStack,
				 const std::map<int, std::string> &constParams,
				 const std::function<void(int)> &appendBody,
				 const std::function<void()> &appendStateInit,
				 MsgType messageType = MsgType::Internal);

	void switchSelector();
	void byteLengthOfCell();
};



} // end solidity::frontend
