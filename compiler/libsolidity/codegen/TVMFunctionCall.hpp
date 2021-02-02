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
 * Function call compiler for TVM
 */

#pragma once

#include "TVMPusher.hpp"

namespace solidity::frontend {

class TVMExpressionCompiler;

class FunctionCallCompiler {
	StackPusherHelper& m_pusher;
	TVMExpressionCompiler* const m_exprCompiler;
	FunctionCall const& m_functionCall;
	std::vector<ASTPointer<Expression const>> m_arguments;

protected:
	void acceptExpr(const Expression* expr);

public:
	FunctionCallCompiler(StackPusherHelper& m_pusher, TVMExpressionCompiler* exprCompiler, FunctionCall const& _functionCall);
	void structConstructorCall();
	void compile();

protected:
	bool structMethodCall();
	void superFunctionCall(MemberAccess const& _node);
	void typeTypeMethods(MemberAccess const& _node);
	// TODO unite with decodeParameter
	void loadTypeFromSlice(MemberAccess const& _node, TypePointer type);
	bool checkForTvmDeployMethods(MemberAccess const& _node, Type::Category category);
	bool checkForTvmBuildMsgMethods(MemberAccess const& _node, Type::Category category);
	void sliceMethods(MemberAccess const& _node);
	void arrayMethods(MemberAccess const& _node);
	bool checkForOptionalMethods(MemberAccess const& _node);
	bool checkForTvmBuilderMethods(MemberAccess const& _node, Type::Category category);
	void cellMethods(MemberAccess const& _node);
	void addressMethod();
	bool checkForTvmConfigParamFunction(MemberAccess const& _node);
	bool checkForTvmSendFunction(MemberAccess const& _node);
	void msgFunction(MemberAccess const& _node);
	void rndFunction(MemberAccess const& _node);
	bool checkForTvmFunction(MemberAccess const& _node);
	void mathFunction(MemberAccess const& _node);
	bool checkBaseContractCall(MemberAccess const& _node, Type::Category category);
	bool checkAddressThis();
	void typeConversion();
	bool checkLocalFunctionCall(const Identifier* identifier);
	bool checkSolidityUnits();
	bool checkLocalFunctionCallOrFuncVarCall();
	bool checkNewExpression();
	bool createNewContract();
	void deployNewContract(
		const std::function<void()> pushWid,
		const std::function<void()> pushValue,
		const std::function<void()> pushBounce,
		const std::function<void()> pushCurrency,
		const std::function<void(int builderSize)> appendBody,
		const std::function<void()> pushSendrawmsgFlag
	);
	bool checkTvmIntrinsic();

	enum StateInitMembers {
		SplitDepth,
		Special,
		Code,
		Data,
		Library
	};
	void buildStateInit(std::map<StateInitMembers, std::function<void()>> exprs);
	std::function<void()> generateDataSection(std::function<void()> pushKey,
				bool &hasVars,
				ASTPointer<Expression const> vars,
				bool &isNew,
				ASTPointer<const Expression> contr
			);
};

}	// solidity
