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

protected:
	void acceptExpr(const Expression* expr);

public:
	FunctionCallCompiler(StackPusherHelper& m_pusher, TVMExpressionCompiler* exprCompiler);
	void structConstructorCall(FunctionCall const& _functionCall);
	void compile(FunctionCall const& _functionCall, bool isCurrentResultNeeded);

protected:
	bool checkForSuper(MemberAccess const& _node, Type::Category);
	bool checkForTypeTypeMember(MemberAccess const& _node, Type::Category category);
	// TODO unite with decodeParameter
	void loadTypeFromSlice(MemberAccess const& _node, TypePointer type);
	bool checkForTvmDeployMethods(MemberAccess const& _node, Type::Category category,
									const std::vector<ASTPointer<Expression const>> & arguments,
									FunctionCall const& _functionCall);
	bool checkForTvmSliceMethods(MemberAccess const& _node, Type::Category category,
									const std::vector<ASTPointer<Expression const>> & arguments,
									FunctionCall const& _functionCall);
	void store(MemberAccess const& _node, TypePointer type, bool reverse = true);
	bool checkForStringMethods(MemberAccess const& _node, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForTvmBuilderMethods(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForTvmCellMethods(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & /*arguments*/);
	void addressMethod(FunctionCall const &_functionCall);
	bool checkForTvmConfigParamFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForTvmSendFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForMsgFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForTvmFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments);
	bool checkForMemberAccessTypeType(MemberAccess const& _node, Type::Category category);
	bool checkAddressThis(FunctionCall const& _functionCall);
	void typeConversion(FunctionCall const& _functionCall);
	bool checkLocalFunctionCall(const Identifier* identifier);
	bool checkSolidityUnits(FunctionCall const& _functionCall);
	bool checkForIdentifier(FunctionCall const& _functionCall);
	bool checkNewExpression(FunctionCall const& _functionCall);
	bool checkTvmIntrinsic(FunctionCall const& _functionCall);
};

}	// solidity
