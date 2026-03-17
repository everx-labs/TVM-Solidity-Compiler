/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
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

#pragma once

#include <map>
#include <stack>

#include <liblangutil/Exceptions.h>
#include <libsolidity/ast/AST.h>

#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/analysis/TVMAnalyzer.hpp>
#include <libsolidity/codegen/helpers/FunctionCallGraph.hpp>

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
	int getOffset(int stackSize) const;
	int getStackSize(Declaration const* name) const;
	void ensureSize(int savedStackSize, std::string const& location = "", ASTNode const* node = nullptr) const;
	void takeLast(int n);

private:
	int m_size{};
	std::vector<Declaration const*> m_stackSize{};
};

class InherHelper {
public:
	explicit InherHelper(ContractDefinition const* contract);
	bool isBaseFunction(CallableDeclaration const* d) const;

private:
	std::set<CallableDeclaration const*> m_baseFunctions;
};

class StorageLayout {
public:
	explicit StorageLayout(ContractDefinition const* contract);
	int getStateVarIndex(VariableDeclaration const* variable) const;
	std::vector<Type const*> getC4Types() const;
	std::vector<VariableDeclaration const*> usualAndUnpackedStateVariables() const;
	std::vector<VariableDeclaration const*> usualStateVariables() const;
	std::vector<VariableDeclaration const*> unpackedStateVariables() const;
	std::vector<VariableDeclaration const*> transientStateVars() const;
	bool tooMuchStateVariables() const;
	FunctionDefinition const* hasConstructor() const;
	bool storePubkeyInC4() const;
	bool storeTimestampInC4() const;
	int getOffsetC4() const;
	std::vector<std::pair<VariableDeclaration const*, int>> getStaticVariables() const;
	int getUnpackIndex() const;

private:
	ContractDefinition const* m_contract;
	std::map<VariableDeclaration const*, int> m_stateVarIndex;
};

class TVMCompilerContext {
public:
	TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper);
	PragmaDirectiveHelper const& pragmaHelper() const;
	bool isStdlib() const;
	std::pair<std::string, uint32_t>
	functionInternalName(FunctionDefinition const* _function, bool calledByPoint) const;
	static std::string getFunctionExternalName(FunctionDefinition const* _function);
	ContractDefinition const* getContract() const;
	bool ignoreIntegerOverflow() const;
	void setCurrentFunction(FunctionDefinition const* _f, std::string const& _name) {
		solAssert(m_currentFunction == nullptr && !m_currentFunctionName.has_value(), "");
		m_currentFunction = _f;
		m_currentFunctionName = _name;
	}
	FunctionDefinition const* currentFunction() const { return m_currentFunction; }
	std::string currentFunctionName() { return m_currentFunctionName.value(); }
	void resetCurrentFunction() {
		m_currentFunction = nullptr;
		m_currentFunctionName.reset();
	}
	void addInlineFunction(std::string const& name, Pointer<CodeBlock> body);
	Pointer<CodeBlock> getInlinedFunction(std::string const& name);
	void addPublicFunction(FunctionDefinition const* function, uint32_t functionId, std::string const& functionName);
	void addExternalMsgPublicFunction(uint32_t functionId, std::string const& functionName);
	void addInternalMsgPublicFunction(uint32_t functionId, std::string const& functionName);
	std::vector<std::pair<uint32_t, std::string>> const& getExtPublicFunctions();
	std::vector<std::pair<uint32_t, std::string>> const& getIntPublicFunctions();

	FunctionCallGraph& callGraph() { return m_callGraph; }
	FunctionDefinition const* fallBack() const { return m_fallback; }
	void setFallback(FunctionDefinition const* _fallback) {
		solAssert(m_fallback == nullptr, "");
		m_fallback = _fallback;
	}
	bool isReceiveGenerated() const { return m_isReceiveGenerated; }
	void setIsReceiveGenerated() { m_isReceiveGenerated = true; }
	bool isOnBouncedMessageGenerated() const { return m_isOnBouncedMessageGenerated; }
	void setIsOnBouncedMessage() { m_isOnBouncedMessageGenerated = true; }
	bool isBaseFunction(CallableDeclaration const* d) const;
	MsgPubkeyAnalyzer const& usage() const { return m_usage; }

	void addConstArray(std::string const& name, TupleExpression const* arr) { m_constArrays.emplace(name, arr); }
	std::set<std::pair<std::string, TupleExpression const*>>& constArrays() { return m_constArrays; }

	void addNewArray(std::string const& name, FunctionCall const* arr) { m_newArray.emplace(name, arr); }
	std::set<std::pair<std::string, FunctionCall const*>> const& newArrays() const { return m_newArray; }

	void addBuildTuple(std::string const& name, std::vector<Type const*> const& types) {
		m_tuples.emplace(name, types);
	}
	std::map<std::string, std::vector<Type const*>> const& buildTuple() const { return m_tuples; }

	bool getPragmaSaveAllFunctions() const { return m_pragmaSaveAllFunctions; }
	void setPragmaSaveAllFunctions() { m_pragmaSaveAllFunctions = true; }

	void startBlock(bool uncheckedBlock) { m_isUncheckedBlock.push(uncheckedBlock); }
	void endBlock() { m_isUncheckedBlock.pop(); }

	StorageLayout const& storageLayout() const { return m_storageLayout; }

private:
	// TODO split to several classes
	ContractDefinition const* m_contract{};
	bool ignoreIntOverflow{};
	std::stack<bool> m_isUncheckedBlock;
	PragmaDirectiveHelper const& m_pragmaHelper;
	FunctionDefinition const* m_currentFunction{};
	std::optional<std::string> m_currentFunctionName;
	std::map<std::string, Pointer<CodeBlock>> m_inlinedFunctions;
	FunctionCallGraph m_callGraph;
	std::vector<std::pair<uint32_t, std::string>> m_extPublicFunctions;
	std::vector<std::pair<uint32_t, std::string>> m_intPublicFunctions;
	FunctionDefinition const* m_fallback{};
	bool m_isReceiveGenerated{};
	bool m_isOnBouncedMessageGenerated{};
	MsgPubkeyAnalyzer m_usage;

	std::set<std::pair<std::string, TupleExpression const*>> m_constArrays;
	std::set<std::pair<std::string, FunctionCall const*>> m_newArray;
	std::map<std::string, std::vector<Type const*>> m_tuples;
	bool m_pragmaSaveAllFunctions{};
	InherHelper const m_inherHelper;
	StorageLayout const m_storageLayout;
};

class StackPusher {
public:
	explicit StackPusher(TVMCompilerContext* ctx, int stackSize = 0);

	Pointer<CodeBlock> getBlock() const {
		solAssert(m_instructions.size() == 1, "");
		auto ret = createNode<CodeBlock>(CodeBlock::Type::None, m_instructions.back());
		return ret;
	}

	void pushInlineFunction(std::string const& name, int take, int ret);
	void pollLastRetOpcode();
	bool tryPollEmptyPushCont();

	[[nodiscard]]
	TVMCompilerContext& ctx() const;

private:
	void change(int delta);
	void change(int take, int ret);

public:
	int stackSize() const;
	void ensureSize(int savedStackSize, std::string const& location = "", ASTNode const* node = nullptr) const;
	void startOpaque();
	void endOpaque(int take, int ret, bool isPure = false);
	void declRetFlag();
	static Pointer<AsymGen> makeAsym(std::string const& cmd);
	void push(Pointer<Stack> const& opcode);
	void push(Pointer<AsymGen> const& opcode);
	void push(Pointer<HardCode> const& opcode);
	void pushAsym(Pointer<AsymGen>&& node);
	void pushAsym(std::string const& opcode);
	StackPusher& operator<<(std::string const& opcode);
	void push(std::string const& cmd);
	void fixStack(int stackDiff);

private:
	void pushCellOrSlice(Pointer<CellOrSliceOperation> const& opcode);

public:
	void pushSlice(std::string const& data);
	void pushPrivateFunctionId(FunctionDefinition const& funDef, bool isCalledByPoint);
	void startContinuation();

private:
	void endCont(CodeBlock::Type type);

public:
	void endContinuation();
	void endContinuationFromRef();
	void endRetOrBreakOrCont(ReturnOrBreakOrCont::Type type, int _take);
	void endLogCircuit(LogCircuit::Type type);

private:
	void callRefOrCallX(bool _isJmp, CodeBlock::Type _blockType);

public:
	void pushRefContAndCallX();
	void pushContAndCallX();


	void ifElse(bool withJmp = false);
	void pushConditional(int ret);

private:
	void if_or_ifNot(bool _withNot, bool _withJmp);

public:
	void _if();
	void ifNot();
	void ifJmp();
	void ifNotJmp();

private:
	void repeatOrUntil(bool withBreakOrReturn, bool isRepeat);

public:
	void repeat(bool _withBreakOrReturn);
	void until(bool withBreakOrReturn);
	void _while(bool _withBreakOrReturn);
	void ret();
	void retAlt();
	void ifRetAlt();
	void ifret();
	void ifNotRet();
	void _throw(std::string const& cmd);

	TVMStack& getStack();
	void pushLoc(std::string const& file, int line);
	void pushString(std::string const& _str, bool toSlice);
	void pushLog();
	void untuple(int n);
	void unpackFirst(int n);
	void indexWithExcep(int index);
	void indexNoexcep(int index);
	void setIndex(int index);
	void setIndexQ(int index);
	void makeTuple(int qty);
	void pushStackGenOpcode(std::string const& name, int take, int ret);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const* vd);
	void getGlob(int index);
	void pushRoot();
	void popRoot();
	void pushC3();
	void pushC7();
	void popC3();
	void popC7();
	void callx(int take, int ret);
	void call(uint32_t id, int take, int ret);
	void setGlob(int index);
	void setGlob(VariableDeclaration const* vd);
	void pushS(int i);
	void popS(int i);
	void pushInt(bigint const& i);
	void stzeroes(int qty);
	void stones(int qty);
	void sendrawmsg();
	// return true if on stack there are (value, slice) else false if (slice, value)
	[[nodiscard]]
	bool fastLoad(Type const* type);
	void load(Type const* type, bool dataOnTop);

	void preload(Type const* type);
	void loadQ(Type const* type);

	void store(Type const* type);
	void storeQ(Type const* type);
	void pushZeroAddress();
	void convert(Type const* leftType, Type const* rightType);
	void checkFit(Type const* type);
	void pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params);
	void pushFragmentInCallRef(int take, int ret, std::string const& functionName);
	void pushCallOrCallRef(
		FunctionDefinition const* _functionDef,
		std::optional<std::pair<int, int>> const& deltaStack,
		bool isCalledByPoint
	);
	void pushFragment(int take, int ret, std::string const& functionName);
	void computeConstCell(std::string const& expName);
	void computeConstSlice(std::string const& expName);
	void drop(int cnt = 1);
	void blockSwap(int down, int up);
	void reverse(int qty, int startIndex);
	void dropUnder(int droppedCount, int leftCount);
	void exchange(int i);
	void exchange(int i, int j);
	void rot();
	void rotRev();
	void prepareKeyForDictOperations(Type const* key);
	[[nodiscard]]
	std::pair<int, int> build_int_msg_info(
		std::set<int> const& isParamOnStack,
		std::map<int, std::string> const& constParams,
		bool isDestBuilder,
		std::function<void()> const& pushValue,
		std::function<void()> const& pushExtraFlags
	);
	[[nodiscard]]
	int build_ext_msg_info(std::set<int> const& isParamOnStack);
	void appendToBuilder(std::string const& bitString);
	void checkOptionalValue();
	static bool doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value);
	[[nodiscard]]
	DataType prepareValueForDictOperations(Type const* keyType, Type const* valueType);
	[[nodiscard]]
	DataType pushDefaultValueForDict(Type const* keyType, Type const* valueType);
	static bool doesDictStoreValueInRef(Type const* keyType, Type const* valueType);

	enum class DecodeType {
		DecodeValue,
		DecodeValueOrPushDefault,
		DecodeValueOrPushNull,
		PushNullOrDecodeValue
	};

	void recoverKeyAndValueAfterDictOperation(
		Type const* keyType,
		Type const* valueType,
		bool hasKey,
		bool didUseOpcodeWithRef,
		DecodeType const& decodeType,
		bool saveOrigKeyAndNoTuple = false
	);
	static Type const* parseIndexType(Type const* type);

	void setDict(
		Type const& keyType,
		Type const& valueType,
		DataType const& dataType,
		SetDictOperation operation = SetDictOperation::Set
	);

	void assignStackVariable(Declaration const* name);

	void getDict(Type const& keyType, Type const& valueType, GetDictOperation const op);

	void getAndSetDict(
		Type const& keyType,
		Type const& valueType,
		GetDictOperation const op,
		DataType const inputValueType
	);

	void pushEmptyArray();
	void pushNull();
	void pushNaN();
	void pushEmptyCell();
	void pushDefaultValue(Type const* _type);

	void pushParamsAndSendInternalMessage(
		std::map<int, Expression const*> const& exprs,
		std::map<int, std::string> const& constParams,
		std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
		std::function<void()> const& pushSendRawMsgFlag,
		std::function<std::pair<int, int>()> const& appendEitherStateInit,
		std::function<void()> const& pushValue,
		std::function<void()> const& pushExtraFlags
	);

	enum class MsgType {
		Internal,
		ExternalOut
	};

	void sendMessage(
		std::set<int> const& isParamOnStack,
		std::map<int, std::string> const& constParams,
		std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
		std::function<std::pair<int, int>()> const& appendEitherStateInit,
		std::function<void()> const& pushSendRawMsgFlag,
		MsgType messageType,
		bool isDestBuilder,
		std::function<void()> const& pushValue,
		std::function<void()> const& pushExtraFlags
	);

	void prepareMessage(
		std::set<int> const& isParamOnStack,
		std::map<int, std::string> const& constParams,
		std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
		std::function<std::pair<int, int>()> const& appendEitherStateInit,
		MsgType messageType,
		bool isDestBuilder,
		std::function<void()> const& pushValue,
		std::function<void()> const& pushExtraFlags
	);

	void byteLengthOfCell();

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

class TypeConversion {
public:
	explicit TypeConversion(StackPusher& _pusher):
		m_pusher{_pusher} {}
	void convert(Type const* leftType, Type const* rightType);
	void convertIntegerToLibraryContinuation() const;
	void convertIntegerToLibraryExoticCell() const;

private:
	void integerToInteger(IntegerType const* leftType, IntegerType const* rightType) const;
	void fixedPointToInteger(IntegerType const* leftType, FixedPointType const* rightType) const;
	void fixedPointToFixedPoint(FixedPointType const* leftType, FixedPointType const* rightType) const;
	void integerToFixedPoint(FixedPointType const* leftType, IntegerType const* rightType) const;
	void fixedBytesToFixedBytes(FixedBytesType const* leftType, FixedBytesType const* rightType) const;
	void bytesToFixedBytes(FixedBytesType const* rightType) const;
	void stringLiteralToFixedBytes(FixedBytesType const* leftType, StringLiteralType const* rightType) const;
	void fromFixedPoint(Type const* leftType, FixedPointType const* rightType) const;
	void convertIntegerToAddress(Type const* t) const;
	void convertIntegerToEnum(EnumType const* leftType, IntegerType const* rightType) const;
	void fromInteger(Type const* leftType, IntegerType const* rightType) const;
	void fromRational(Type const* leftType, RationalNumberType const* rightType) const;
	void tupleFromTuple(TupleType const* leftType, TupleType const* rightType);
	void fromFixedBytesType(Type const* leftType, FixedBytesType const* rightType) const;
	void fromArray(Type const* leftType, ArrayType const* rightType) const;
	void fromOptional(Type const* leftType, OptionalType const* rightType);
	void fromSlice(Type const* leftType) const;
	void fromTuple(Type const* leftType, TupleType const* rightType);
	void fromStringLiteral(Type const* leftType, StringLiteralType const* rightType) const;
	void fromCell(Type const* leftType) const;

	StackPusher& m_pusher;
}; // end TypeConversion

} // end solidity::frontend
