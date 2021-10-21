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
 */

#pragma once

#include <map>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/Exceptions.h>
#include <liblangutil/SourceReferenceFormatter.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>

#include "TVMCommons.hpp"
#include "TvmAst.hpp"
#include "TVMAnalyzer.hpp"

namespace solidity::frontend {

class TVMStack {
public:
	TVMStack() = default;
	int size() const;
	void change(int diff);
	void change(int take, int ret);
	bool isParam(Declaration const* name) const;
	void add(Declaration const* name, bool doAllocation);
	int getOffset(Declaration const* name) const;
	int getOffset(int stackPos) const;
	int getStackSize(Declaration const* name) const;
	void ensureSize(int savedStackSize, const std::string& location = "", const ASTNode* node = nullptr) const;
	void takeLast(int n);

private:
	int m_size{};
	std::vector<Declaration const*> m_stackSize;
};

class TVMCompilerContext {
public:
	TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper);
	void initMembers(ContractDefinition const* contract);
	int getStateVarIndex(VariableDeclaration const *variable) const;
	std::vector<VariableDeclaration const *> notConstantStateVariables() const;
	bool tooMuchStateVariables() const;
	std::vector<Type const *> notConstantStateVariableTypes() const;
	PragmaDirectiveHelper const& pragmaHelper() const;
	bool hasTimeInAbiHeader() const;
	bool isStdlib() const;
	std::string getFunctionInternalName(FunctionDefinition const* _function, bool calledByPoint = true) const;
	static std::string getLibFunctionName(FunctionDefinition const* _function, bool withObject) ;
	static std::string getFunctionExternalName(FunctionDefinition const* _function);
	const ContractDefinition* getContract() const;
	bool ignoreIntegerOverflow() const;
	FunctionDefinition const* afterSignatureCheck() const;
	bool storeTimestampInC4() const;
	int getOffsetC4() const;
	void addLib(FunctionDefinition const* f);
	std::set<FunctionDefinition const*>& getLibFunctions() { return m_libFunctions; }
	std::vector<std::pair<VariableDeclaration const*, int>> getStaticVariables() const;
	void setCurrentFunction(FunctionDefinition const* f) { m_currentFunction = f; }
	FunctionDefinition const* getCurrentFunction() { return m_currentFunction; }
	void addInlineFunction(const std::string& name, Pointer<CodeBlock> body);
	Pointer<CodeBlock> getInlinedFunction(const std::string& name);
	void addPublicFunction(uint32_t functionId, const std::string& functionName);
	const std::vector<std::pair<uint32_t, std::string>>& getPublicFunctions();

	bool addAndDoesHaveLoop(FunctionDefinition const* _v, FunctionDefinition const* _to);
	bool dfs(FunctionDefinition const* v);
	bool isFallBackGenerated() const { return m_isFallBackGenerated; }
	void setIsFallBackGenerated() { m_isFallBackGenerated = true; }
	bool isReceiveGenerated() const { return m_isReceiveGenerated; }
	void setIsReceiveGenerated() { m_isReceiveGenerated = true; }
	bool isOnBounceGenerated() const { return m_isOnBounceGenerated; }
	void setIsOnBounce() { m_isOnBounceGenerated = true; }
	bool isBaseFunction(CallableDeclaration const* d) const;
	ContactsUsageScanner const& usage() const { return m_usage; }

private:
	ContractDefinition const* m_contract{};
	bool ignoreIntOverflow{};
	PragmaDirectiveHelper const& m_pragmaHelper;
	std::map<VariableDeclaration const*, int> m_stateVarIndex;
	std::set<FunctionDefinition const*> m_libFunctions;
	FunctionDefinition const* m_currentFunction{};
	std::map<std::string, Pointer<CodeBlock>> m_inlinedFunctions;
	std::map<FunctionDefinition const*, std::set<FunctionDefinition const*>> graph;
	enum class Color {
		White, Red, Black
	};
	std::map<FunctionDefinition const*, Color> color;
	std::vector<std::pair<uint32_t, std::string>> m_publicFunctions;
	bool m_isFallBackGenerated{};
	bool m_isReceiveGenerated{};
	bool m_isOnBounceGenerated{};
    std::set<CallableDeclaration const*> m_baseFunctions;
    ContactsUsageScanner m_usage;
};

class StackPusher {
public:
	explicit StackPusher(TVMCompilerContext* ctx, int stackSize = 0);

	Pointer<CodeBlock> getBlock() const {
		solAssert(m_instructions.size() == 1, "");
		auto ret = createNode<CodeBlock>(CodeBlock::Type::None, m_instructions.back());
		return ret;
	}

	void pushInlineFunction(const Pointer<CodeBlock>& block, int take, int ret);
	void pollLastRetOpcode();
	bool tryPollEmptyPushCont();

	[[nodiscard]]
	TVMCompilerContext& ctx();
private:
	void change(int delta);
	void change(int take, int ret);
public:
	int stackSize() const;
	void ensureSize(int savedStackSize, const std::string &location = "", const ASTNode* node = nullptr);
	void startOpaque();
	void endOpaque(int take, int ret, bool isPure = false);
	void declRetFlag();
private:
	static Pointer<AsymGen> asym(const std::string& cmd);
public:
	void push(Pointer<Stack> opcode);
	void push(Pointer<AsymGen> opcode);
	void push(Pointer<HardCode> opcode);
	void pushAsym(std::string const& opcode);
	void push(int stackDiff, const std::string& cmd);
	void pushCellOrSlice(Pointer<PushCellOrSlice> opcode);

	void startContinuation();
private:
	void endCont(CodeBlock::Type type);
public:
	void endContinuation();
	void endContinuationFromRef();
	void endRetOrBreakOrCont(int _take);
	void endLogCircuit(LogCircuit::Type type);

private:
	void callRefOrCallX(int take, int ret, SubProgram::Type type);
public:
	void callRef(int take, int ret);
	void callX(int take, int ret);


	void ifElse(bool useJmp = false);
	void pushConditional(int ret);
private:
	void if_or_ifnot(TvmIfElse::Type);
public:
	void _if();
	void _ifNot();
	void ifJmp();

	void ifRef();
	void ifNotRef();
	void ifJmpRef();
	void ifNotJmpRef();

private:
	void repeatOrUntil(std::optional<bool> withBreakOrReturn, bool isRepeat);
public:
	void repeat();
	void until(bool withBreakOrReturn);
	void _while(bool _withBreakOrReturn);
	void ret();
	void retAlt();
	void ifRetAlt();
	void ifret();
	void _throw(std::string cmd);

	TVMStack& getStack();
	void pushLoc(const std::string& file, int line);
    void pushString(const std::string& str, bool toSlice);
	void pushLog();
	void untuple(int n);
	void indexWithExcep(int index);
	void indexNoexcep(int index);
	void setIndex(int index);
	void setIndexQ(int index);
	void tuple(int qty);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const * vd);
	void getGlob(int index);
	void pushC4();
	void popRoot();
	void pushC3();
	void pushC7();
	void popC3();
	void popC7();
	void execute(int take, int ret);
	void setGlob(int index);
	void setGlob(VariableDeclaration const * vd);
	void pushS(int i);
	void dup2();
	void pushS2(int i, int j);
	void popS(int i);
	void pushInt(const bigint& i);
	void stzeroes(int qty);
	void stones(int qty);
	void sendrawmsg();
	// return true if on stack there are (value, slice) else false if (slice, value)
	[[nodiscard]]
	bool fastLoad(const Type* type);
	void load(const Type* type, bool reverseOrder);

	void preload(const Type *type);

	void store(const Type *type, bool reverse);
	void pushZeroAddress();
	Pointer<Function> generateC7ToT4Macro(bool forAwait);

	// TODO move to formatter
	static void addBinaryNumberToString(std::string &s, bigint value, int bitlen = 256);
	static std::string binaryStringToSlice(const std::string & s);
	static std::string toBitString(const std::string& slice);
	static std::vector<std::string> unitSlices(const std::string& sliceA, const std::string& sliceB);
	static std::vector<std::string> unitBitString(const std::string& bitStringA, const std::string& bitStringB);
	static std::string tonsToBinaryString(const u256& value);
	static std::string tonsToBinaryString(bigint value);
	static std::string boolToBinaryString(bool value);
	std::string literalToSliceAddress(Literal const* literal, bool pushSlice = true);
	static bigint pow10(int power);

	void hardConvert(Type const *leftType, Type const *rightType);
	void checkFit(Type const *type);
	void pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params);
	void pushMacroCallInCallRef(int take, int ret, const std::string& fname);
	void pushCallOrCallRef(const std::string& functionName, FunctionType const* ft, const std::optional<std::pair<int, int>>& deltaStack = std::nullopt);
	void pushCall(int take, int ret, const std::string& functionName);
	void drop(int cnt = 1);
	void blockSwap(int down, int up);
	void reverse(int qty, int startIndex);
	void dropUnder(int droppedCount, int leftCount);
	void exchange(int i);
	void rot();
	void rotRev();
	void prepareKeyForDictOperations(Type const* key, bool doIgnoreBytes);
	[[nodiscard]]
	int int_msg_info(const std::set<int> &isParamOnStack, const std::map<int, std::string> &constParams, bool isDestBuilder);
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
		bool saveOrigKeyAndNoTuple = false
	);
	static TypePointer parseIndexType(Type const* type);
	static TypePointer parseValueType(IndexAccess const& indexAccess);

	void setDict(
		Type const &keyType,
		Type const &valueType,
		const DataType& dataType,
		SetDictOperation opcode = SetDictOperation::Set
	);

	bool tryAssignParam(Declaration const* name);

	void getDict(
		const Type& keyType,
		const Type& valueType,
		GetDictOperation op,
  		const DataType& dataType = DataType::Slice
	);

	void pushNull();
	void pushDefaultValue(Type const* type, bool isResultBuilder = false);
	void sendIntMsg(const std::map<int, const Expression *> &exprs,
					const std::map<int, std::string> &constParams,
					const std::function<void(int)> &appendBody,
					const std::function<void()> &pushSendrawmsgFlag,
					bool isAwait,
					size_t callParamsOnStack,
					const std::function<void()> &appendStateInit);

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
				 MsgType messageType = MsgType::Internal,
				 bool isDestBuilder = false);

	void prepareMsg(const std::set<int>& isParamOnStack,
				 const std::map<int, std::string> &constParams,
				 const std::function<void(int)> &appendBody,
				 const std::function<void()> &appendStateInit,
				 MsgType messageType = MsgType::Internal,
				 bool isDestBuilder = false);

	void byteLengthOfCell();

	void was_c4_to_c7_called();
	void checkCtorCalled();
	void checkIfCtorCalled(bool ifFlag);
	bool hasLock() const { return lockStack > 0; }
	void add(StackPusher const& pusher);
	void clear();
	void takeLast(int n);

private:
	int lockStack{};
	TVMStack m_stack{};
	std::vector<std::vector<Pointer<TvmAstNode>>> m_instructions{};
	TVMCompilerContext* m_ctx{};
}; // end StackPusher

} // end solidity::frontend
