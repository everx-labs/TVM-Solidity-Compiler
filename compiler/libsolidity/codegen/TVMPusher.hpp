/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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

class InherHelper {
public:
	explicit InherHelper(ContractDefinition const* contract);
	bool isBaseFunction(CallableDeclaration const* d) const;
private:
	std::set<CallableDeclaration const*> m_baseFunctions;
};

class FunctionCallGraph : private boost::noncopyable {
public:
	// Adds the edge and returns true if the edge doesn't create a loop
	bool tryToAddEdge(std::string const& _v, std::string const& _to);
	std::vector<std::string> DAG();
	void addPrivateFunction(uint32_t id, std::string name) {
		if (m_privcateFuncs.count(id))
			solAssert(m_privcateFuncs.at(id) == name);
		else
			m_privcateFuncs.emplace(id, name);
	}
	std::map<uint32_t, std::string> privateFunctions() const { return m_privcateFuncs; }
private:
	bool dfs(std::string const& _v);
private:
	std::map<std::string, std::set<std::string>> m_graph;
	enum class Color {
		White, Red, Black
	};
	std::map<std::string, Color> m_color;
	std::vector<std::string> m_order;
	std::map<uint32_t, std::string> m_privcateFuncs;
};

class TVMCompilerContext {
public:
	TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper);
	void initMembers(ContractDefinition const* contract);
	int getStateVarIndex(VariableDeclaration const *variable) const;
	std::vector<VariableDeclaration const *> c4StateVariables() const;
	std::vector<VariableDeclaration const *> nostorageStateVars() const;
	bool tooMuchStateVariables() const;
	std::vector<Type const *> c4StateVariableTypes() const;
	PragmaDirectiveHelper const& pragmaHelper() const;
	bool isStdlib() const;
	std::string getFunctionInternalName(FunctionDefinition const* _function, bool calledByPoint = true) const;
	static std::string getLibFunctionName(FunctionDefinition const* _function, bool withObject) ;
	static std::string getFunctionExternalName(FunctionDefinition const* _function);
	const ContractDefinition* getContract() const;
	bool ignoreIntegerOverflow() const;
	FunctionDefinition const* afterSignatureCheck() const;
	bool storeTimestampInC4() const;
	int getOffsetC4() const;
	std::vector<std::pair<VariableDeclaration const*, int>> getStaticVariables() const;
	void setCurrentFunction(FunctionDefinition const* _f, std::string _name) {
		solAssert(m_currentFunction == nullptr && !m_currentFunctionName.has_value(),  "");
		m_currentFunction = _f;
		m_currentFunctionName = _name;
	}
	FunctionDefinition const* currentFunction() { return m_currentFunction; }
	std::string currentFunctionName() { return m_currentFunctionName.value(); }
	void resetCurrentFunction() {
		m_currentFunction = nullptr;
		m_currentFunctionName.reset();
	}
	void addInlineFunction(const std::string& name, Pointer<CodeBlock> body);
	Pointer<CodeBlock> getInlinedFunction(const std::string& name);
	void addPublicFunction(uint32_t functionId, const std::string& functionName);
	const std::vector<std::pair<uint32_t, std::string>>& getPublicFunctions();

	FunctionCallGraph& callGraph() { return m_callGraph; }
	bool isFallBackGenerated() const { return m_isFallBackGenerated; }
	void setIsFallBackGenerated() { m_isFallBackGenerated = true; }
	bool isReceiveGenerated() const { return m_isReceiveGenerated; }
	void setIsReceiveGenerated() { m_isReceiveGenerated = true; }
	bool isOnBounceGenerated() const { return m_isOnBounceGenerated; }
	void setIsOnBounce() { m_isOnBounceGenerated = true; }
	bool isBaseFunction(CallableDeclaration const* d) const;
	ContactsUsageScanner const& usage() const { return m_usage; }

	void addConstArray(std::string const& name, TupleExpression const* arr) { m_constArrays.emplace(name, arr); }
	std::set<std::pair<std::string, TupleExpression const*>>& constArrays() { return m_constArrays; }

	void addNewArray(std::string const& name, FunctionCall const* arr) { m_newArray.emplace(name, arr); }
	std::set<std::pair<std::string, FunctionCall const*>> const& newArrays() const { return m_newArray; }

	void addBuildTuple(std::string const& name, const std::vector<Type const*>& types) {
		m_tuples.emplace(name, types);
	}
	std::map<std::string, std::vector<Type const*>> const& buildTuple() const { return m_tuples; }

	bool getPragmaSaveAllFunctions() const { return m_pragmaSaveAllFunctions; }
	void setPragmaSaveAllFunctions() { m_pragmaSaveAllFunctions = true; }

	void startBlock(bool uncheckedBlock) { m_isUncheckedBlock.push(uncheckedBlock); }
	void endBlock() { m_isUncheckedBlock.pop(); }

private:
	// TODO split to several classes
	ContractDefinition const* m_contract{};
	bool ignoreIntOverflow{};
	std::stack<bool> m_isUncheckedBlock;
	PragmaDirectiveHelper const& m_pragmaHelper;
	std::map<VariableDeclaration const*, int> m_stateVarIndex;
	FunctionDefinition const* m_currentFunction{};
	std::optional<std::string> m_currentFunctionName;
	std::map<std::string, Pointer<CodeBlock>> m_inlinedFunctions;
	FunctionCallGraph m_callGraph;
	std::vector<std::pair<uint32_t, std::string>> m_publicFunctions;
	bool m_isFallBackGenerated{};
	bool m_isReceiveGenerated{};
	bool m_isOnBounceGenerated{};
	ContactsUsageScanner m_usage;

	std::set<std::pair<std::string, TupleExpression const*>> m_constArrays;
	std::set<std::pair<std::string, FunctionCall const*>> m_newArray;
	std::map<std::string, std::vector<Type const*>> m_tuples;
	bool m_pragmaSaveAllFunctions{};
	InherHelper const m_inherHelper;
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
	static Pointer<AsymGen> makeAsym(const std::string& cmd);
public:
	void push(const Pointer<Stack>& opcode);
	void push(const Pointer<AsymGen>& opcode);
	void push(const Pointer<HardCode>& opcode);
	void pushAsym(Pointer<AsymGen>&& node);
	void pushAsym(std::string const& opcode);
	StackPusher& operator<<(std::string const& opcode);
	void push(std::string const& opcode);
	void fixStack(int stackDiff);
private:
	void pushCellOrSlice(const Pointer<PushCellOrSlice>& opcode);
public:
	void pushSlice(std::string const& data);
	void pushPrivateFunctionId(FunctionDefinition const& funDef, bool isCalledByPoint);
	void startContinuation();
private:
	void endCont(CodeBlock::Type type);
public:
	void endContinuation();
	void endContinuationFromRef();
	void endRetOrBreakOrCont(int _take);
	void endLogCircuit(LogCircuit::Type type);

private:
	void callRefOrCallX(int take, int ret, bool _isJmp, CodeBlock::Type _blockType, bool isPure);
public:
	void pushRefContAndCallX(int take, int ret, bool isPure);
	void pushContAndCallX(int take, int ret, bool isPure);


	void ifElse(bool useJmp = false);
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
	void tryOpcode(bool saveAltC2);
	void ret();
	void retAlt();
	void ifRetAlt();
	void ifret();
	void ifNotRet();
	void _throw(const std::string& cmd);

	TVMStack& getStack();
	void pushLoc(const std::string& file, int line);
	void pushString(const std::string& str, bool toSlice);
	void pushLog();
	void untuple(int n);
	void unpackFirst(int n);
	void indexWithExcep(int index);
	void indexNoexcep(int index);
	void setIndex(int index);
	void setIndexQ(int index);
	void makeTuple(int qty);
	void resetAllStateVars();
	void getGlob(VariableDeclaration const * vd);
	void getGlob(int index);
	void pushRoot();
	void popRoot();
	void pushC3();
	void pushC7();
	void popC3();
	void popC7();
	void execute(int take, int ret);
	void setGlob(int index);
	void setGlob(VariableDeclaration const * vd);
	void pushS(int i);
	void pushS2(int i, int j); // TODO delete
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
	void loadQ(const Type *type);

	void store(const Type *type, bool reverse);
	void pushZeroAddress();
	Pointer<Function> generateC7ToC4(bool forAwait);
	void convert(Type const *leftType, Type const *rightType);
	void checkFit(Type const *type);
	void pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params);
	void pushFragmentInCallRef(int take, int ret, const std::string& fname);
	void pushCallOrCallRef(FunctionDefinition const* _functionDef, const std::optional<std::pair<int, int>>& deltaStack, bool isCalledByPoint);
	void pushFragment(int take, int ret, const std::string& functionName);
	void computeConstCell(std::string const& expName);
	void computeConstSlice(std::string const& expName);
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
	static bool doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value);
	[[nodiscard]]
	DataType prepareValueForDictOperations(Type const* keyType, Type const* valueType);
	[[nodiscard]]
	DataType pushDefaultValueForDict(Type const* keyType, Type const* valueType);
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
	static Type const* parseIndexType(Type const* type);

	void setDict(
		Type const &keyType,
		Type const &valueType,
		const DataType& dataType,
		SetDictOperation opcode = SetDictOperation::Set
	);

	void assignStackVariable(Declaration const* name);

	void getDict(
		const Type& keyType,
		const Type& valueType,
		const GetDictOperation op
	);

	void getAndSetDict(
		const Type &keyType,
		const Type &valueType,
		const GetDictOperation op,
		const DataType inputValueType
	);

	void pushEmptyArray();
	void pushNull();
	void pushEmptyCell();
	void pushDefaultValue(Type const* _type);
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

class TypeConversion {
public:
	TypeConversion(StackPusher& _pusher) : m_pusher{_pusher} { }
	void convert(Type const* leftType, Type const* rightType);
private:
	void integerToInteger(IntegerType const* leftType, IntegerType const* rightType);
	void fixedPointToInteger(IntegerType const* leftType, FixedPointType const* rightType);
	void fixedPointToFixedPoint(FixedPointType const* leftType, FixedPointType const* rightType);
	void integerToFixedPoint(FixedPointType const* leftType, IntegerType const* rightType);
	void fixedBytesToFixedBytes(FixedBytesType const* leftType, FixedBytesType const* rightType);
	void bytesToFixedBytes(FixedBytesType const* rightType);
	void stringLiteralToFixedBytes(FixedBytesType const* leftType, StringLiteralType const* rightType);
	void fromFixedPoint(Type const* leftType, FixedPointType const* rightType);
	void convertIntegerToAddress(Type const* t);
	void convertIntegerToEnum(EnumType const* leftType, IntegerType const* rightType);
	void fromInteger(Type const* leftType, IntegerType const* rightType);
	void fromRational(Type const* leftType, RationalNumberType const* rightType);
	void tupleFromTuple(TupleType const* leftType, TupleType const* rightType);
	void fromFixedBytesType(Type const* leftType, FixedBytesType const* rightType);
	void fromArray(Type const* leftType, ArrayType const* rightType);
	void fromOptional(Type const* leftType, OptionalType const* rightType);
	void fromSlice(Type const* leftType);
	void fromTuple(Type const* leftType, TupleType const* rightType);
	void fromStringLiteral(Type const* leftType, StringLiteralType const* rightType);
private:
	StackPusher& m_pusher;
}; // end TypeConversion

} // end solidity::frontend
