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
 * Function call compiler for TVM
 */

#pragma once

#include <variant>

namespace solidity::frontend {

class TVMExpressionCompiler;
class StackPusher;

class FunctionCallCompiler {
public:
	FunctionCallCompiler(StackPusher& m_pusher, FunctionCall const& _functionCall, bool isCurrentResultNeeded);
	void structConstructorCall() const;
	void compile();

protected:
	static void arrayPush(StackPusher& pusher, Type const* arrayBaseType, DataType dataType);
	bool checkForMappingOrCurrenciesMethods() const;
	void mappingDelMinOrMax(bool isDelMin) const;
	void mappingGetSet() const;
	void mappingMinMaxMethod(bool isMin) const;
	void mappingPrevNextMethods() const;
	void mappingKeysOrValues(bool areKeys) const;
	void mappingEmpty() const;
	bool structMethodCall() const;
	void superFunctionCall(MemberAccess const& _node) const;
	void userDefinedValueMethods(MemberAccess const& _memberAccess) const;
	void addressMethods(MemberAccess const& _node) const;
	bool libraryCall() const;
	bool checkTvmABIDeployMethods(Type::Category category) const;
	void abiBuildIntMsg() const;
	void abiBuildDataInit() const;
	void abiDecodeData() const;
	int decodeData() const;
	int decodeFunctionParams() const;
	void sliceMethods(MemberAccess const& _node) const;
	void arrayMethods(MemberAccess const& _node) const;
	bool checkForOptionalMethods(MemberAccess const& _node) const;
	void builderMethods(MemberAccess const& _node) const;
	void qIntOrBoolMethods() const;
	void stringBuilderMethods() const;
	void tvmVectorMethods() const;
	void tvmStackMethods() const;
	void cellMethods(MemberAccess const& _node) const;
	void integerMethods() const;
	void variantMethods(MemberAccess const& _node) const;
	void addressMethod();
	bool checkForTvmConfigParamFunction(MemberAccess const& _node) const;
	bool checkForTvmSendFunction(MemberAccess const& _node) const;
	void msgFunction(MemberAccess const& _node) const;
	void rndFunction(MemberAccess const& _node) const;
	void rist255Function() const;
	void blsFunction() const;
	void goshFunction() const;
	void codeSalt() const;
	void setCodeSalt() const;
	void functionId() const;
	void abiEncodeBody() const;
	bool checkForTvmFunction(MemberAccess const& _node) const;
	bool checkForTvmC4(MemberAccess const& _node) const;
	void abiFunction() const;
	void mathFunction(MemberAccess const& _node) const;
	bool checkBaseContractCall(MemberAccess const& _node) const;
	bool checkAddressThis() const;
	void createObject() const;
	void typeConversion() const;
	bool checkLocalFunctionOrLibCall(Identifier const* identifier) const;
	bool checkHashFunctions() const;
	bool checkSolidityUnits() const;
	bool checkLocalFunctionOrLibCallOrFuncVarCall() const;
	bool checkNewExpression() const;
	void creatArrayWithDefaultValue() const;

public:
	void honestArrayCreation(bool onlyDict) const;

protected:
	void createNewContract() const;
	struct StateInitInfo {
		bool isBuilder;
		int bits;
		int refs;
		StateInitInfo(bool isBuilder, int bits, int refs):
			isBuilder{isBuilder},
			bits{bits},
			refs{refs} {}
	};
	void deployNewContract(
		StateInitInfo const& stateInitInfo,
		std::variant<int8_t, std::function<void()>> const& wid,
		std::function<void()> const& pushPrefix,
		std::variant<bigint, std::function<void()>> const& value,
		std::variant<bool, std::function<void()>> const& pushBounce,
		std::function<void()> const& pushCurrency,
		std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
		std::function<void()> const& pushSendRawMsgFlag,
		int const argQty
	) const;

	enum class StateInitMembers {
		PrefixLength,
		Special,
		Code,
		Data,
		Library
	};
	std::pair<int, int> encodeStateInit(std::map<StateInitMembers, std::function<void()>> const& exprs) const;
	std::pair<int, int>
	encodeStateInitAndHash(std::map<StateInitMembers, std::function<void()>> const& exprs, bool savePrefixLength) const;
	std::function<void()> generateDataSection(
		bool data_map_supported,
		std::function<void()> const& pushKey,
		Expression const* vars,
		ContractType const* ct
	) const;
	bool checkRemoteMethodCall(FunctionCall const& _functionCall) const;
	static FunctionDefinition const* getRemoteFunctionDefinition(MemberAccess const* memberAccess);

public:
	void pushArgWithoutConvertion() const;
	void pushArgAndConvertToCommon() const;
	void pushArgs(bool reversed = false, bool doConvertToCommonType = false) const;

protected:
	void pushArgAndConvert(int declarationIndex) const;
	void pushArgAndConvert(int callIndex, std::string const& name) const;
	void pushExprAndConvert(Expression const* expr, Type const* targetType) const;
	void pushAllArgsAndConvertToMobileType() const;
	void pushArgConvertToMobileType(int callIndex) const;
	void acceptExpr(Expression const* expr) const;
	void compileLog() const;
	Expression const* findOption(std::string const& name) const;
	void cellBitRefQty(bool forCell = true) const;

private:
	StackPusher& m_pusher;
	TVMExpressionCompiler m_exprCompiler;
	FunctionCall const& m_functionCall;
	MemberAccess const* m_memberAccess{};
	std::vector<ASTPointer<Expression const>> m_arguments;
	FunctionType const* m_funcType{};
	Type const* m_retType{};
	bool m_isCurrentResultNeeded{};
	std::vector<ASTPointer<ASTString>> const& m_names;

	size_t const INDEX_INF = std::numeric_limits<size_t>::max();
	size_t argQty;
	std::vector<size_t> m_declarationIndex; // m_declarationIndex[callIndex]
	std::vector<size_t> m_callIndex;		// m_callIndex[declarationIndex]
};

} // solidity
