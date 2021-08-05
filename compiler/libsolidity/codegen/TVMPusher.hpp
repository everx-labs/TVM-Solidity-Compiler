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
#include "TvmAst.hpp"
#include "TVMAnalyzer.hpp"

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;

namespace solidity::frontend {

class TVMStack : public boost::noncopyable {
    int m_size{};
	std::vector<Declaration const*> m_stackSize;

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
	void ensureSize(int savedStackSize, const string& location = "", const ASTNode* node = nullptr) const;
};

class CodeLines {
public:
	CodeLines() {
		//cerr << "DELETE ME\n";
	}
	vector<string> const &getLines() const { return lines; }
	void setLines(vector<string> const& _lines) { lines = _lines; }
	string str(const string& indent = "") const;
	void startContinuation();
	void startContinuationFromRef();
	void startIfRef();
	void startIfJmpRef();
	void startIfNotJmpRef();
	void startIfNotRef();
	void startCallRef();
	void endContinuation();
	void push(const string& cmd);
	void append(const CodeLines& oth);
private:
	vector<string> lines;
	int tabQty{};
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
	bool hasTimeInAbiHeader() const;
	bool isStdlib() const;
	string getFunctionInternalName(FunctionDefinition const* _function, bool calledByPoint = true) const;
	static string getLibFunctionName(FunctionDefinition const* _function, bool withObject) ;
	static string getFunctionExternalName(FunctionDefinition const* _function);
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

class StackPusherHelper {
public:
	explicit StackPusherHelper(TVMCompilerContext* ctx, int stackSize = 0);

	Pointer<CodeBlock> getBlock() const {
		solAssert(m_instructions.size() == 1, "");
		return createNode<CodeBlock>(m_instructions.at(0).type.value(), m_instructions.at(0).opcodes);
	}

	Pointer<CodeBlock> pollLastBlock() {
		solAssert(m_instructions.size() >= 2, "");
		auto ret = createNode<CodeBlock>(m_instructions.back().type.value(), m_instructions.back().opcodes);
		m_instructions.pop_back();
		return ret;
	}

	void pushInlineFunction(Pointer<CodeBlock> block, int take, int ret);
	void pollLastRetOpcode();
	bool tryPollEmptyPushCont();
	bool cmpLastCmd(const std::string& cmd, int offset = 0);
	void pollLastOpcode();

	[[nodiscard]]
	TVMCompilerContext& ctx();
private:
	void change(int delta);
	void change(int take, int ret);
public:
	int stackSize() const;
	void ensureSize(int savedStackSize, const string &location = "", const ASTNode* node = nullptr);
	void startOpaque();
	void endOpaque(int take, int ret, bool isPure = false);
private:
	static Pointer<AsymGen> asym(const string& cmd);
public:
	void push(Pointer<Stack> opcode);
	void push(Pointer<AsymGen> opcode);
	void push(Pointer<HardCode> opcode);
	void pushAsym(std::string const& opcode);
	void push(int stackDiff, const string& cmd);

	void startContinuation(int deltaStack = 0);
	void startContinuationFromRef();
	void startIfRef(int deltaStack = 0);
	void startIfJmpRef(int deltaStack = 0);
	void startIfNotJmpRef(int deltaStack = 0);
	void startIfNotRef(int deltaStack = 0);
	void startCallRef(int deltaStack = 0);
	void startPushRef();
	void endContinuation(int deltaStack = 0);
	void ifElse(bool useJmp = false);
private:
	void if_or_ifnot(TvmIfElse::Type);
public:
	void _if();
	void _ifNot();
	void ifNotJmp();
	void ifJmp();
private:
	void repeatOrUntil(RepeatOrUntil::Type type);
public:
	void repeat();
	void until();
	void _while();
	void startCallX();
	void endCallX();
	void ret();
	void ifret();
	void _throw(std::string cmd);

	TVMStack& getStack();
	void pushLoc(const std::string& file, int line);
    void pushString(const std::string& str, bool toSlice);
	void pushLog();
	void untuple(int n);
	void index(int index);
	void setIndex(int index);
	void setIndexQ(int index);
	void tuple(int qty);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const * vd);
	void getGlob(int index);
	void pushC4();
	void popRoot();
	void pushC3();
	void popC3();
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
	Pointer<Function> generateC7ToT4Macro();
	Pointer<Function> generateC7ToT4MacroForAwait();

	// TODO move to formatter
	static void addBinaryNumberToString(std::string &s, bigint value, int bitlen = 256);
	static std::string binaryStringToSlice(const std::string & s);
	static std::string toBitString(const std::string& slice);
	static std::vector<std::string> unitSlices(const std::string& sliceA, const std::string& sliceB);
	static std::vector<std::string> unitBitString(const std::string& bitStringA, const std::string& bitStringB);
	static std::string tonsToBinaryString(Literal const* literal);
	static std::string tonsToBinaryString(const u256& value);
	static std::string tonsToBinaryString(bigint value);
	static std::string boolToBinaryString(bool value);
	std::string literalToSliceAddress(Literal const* literal, bool pushSlice = true);
	static bigint pow10(int power);

	void hardConvert(Type const *leftType, Type const *rightType);
	void checkFit(Type const *type);
	void pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params);
	void pushMacroCallInCallRef(int take, int ret, const string& fname);
	void pushCallOrCallRef(const string& functionName, FunctionType const* ft, const std::optional<std::pair<int, int>>& deltaStack = nullopt);
	void pushCall(int take, int ret, const std::string& functionName);
	void drop(int cnt = 1);
	void blockSwap(int m, int n);
	void reverse(int qty, int startIndex);
	void dropUnder(int leftCount, int droppedCount);
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
		const GetDictOperation op,
  		const DataType& dataType = DataType::Slice
	);

	void pushNull();
	void pushDefaultValue(Type const* type, bool isResultBuilder = false);
	void sendIntMsg(const std::map<int, const Expression *> &exprs,
					const std::map<int, std::string> &constParams,
					const std::function<void(int)> &appendBody,
					const std::function<void()> &pushSendrawmsgFlag,
					bool isAwait,
					size_t callParamsOnStack);

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

private:
	int lockStack{};
	TVMStack m_stack2;

	class PusherBlock {
	public:
		explicit PusherBlock(std::optional<CodeBlock::Type> _type) : type{_type} {
		}
//	private:
		std::optional<CodeBlock::Type> type;// is null => opaque
		std::vector<Pointer<TvmAstNode>> opcodes;
	};

	std::vector<PusherBlock> m_instructions;
	TVMCompilerContext* m_ctx;
}; // end StackPusherHelper

} // end solidity::frontend
