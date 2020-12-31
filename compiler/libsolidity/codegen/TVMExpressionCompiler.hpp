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
 * Expression compiler for TVM
 */

#pragma once

#include "TVMPusher.hpp"

namespace solidity::frontend {

class TVMExpressionCompiler {
public:
	struct LValueInfo {
		explicit LValueInfo() {}
		explicit LValueInfo(Type const* rightType) :
			rightType{rightType} {
		}
		std::vector<Expression const*> expressions;
		std::vector<bool> isResultBuilder;
		bool isValueBuilder{};
		bool doesntNeedToCollect = false;
		Type const* rightType{};
	};

private:
	StackPusherHelper &m_pusher;
	int m_expressionDepth;
	bool m_isResultNeeded;
	std::set<Expression const*> m_resultIsSlice;

public:
	explicit TVMExpressionCompiler(StackPusherHelper &pusher);
	void compileNewExpr(const Expression* expr);
	void acceptExpr(const Expression* expr, const bool _isResultNeeded);
	static std::pair<bool, bigint> constValue(Expression const& _e);
	static std::pair<bool, bool> constBool(Expression const& _e);
	static int returnParamQty(Expression const& _e);



protected:
	bool acceptExpr(const Expression* expr);
	bool isCurrentResultNeeded() const;
	void visitStringLiteralAbiV1(Literal const& _node);
	void visitStringLiteralAbiV2(Literal const& _node);
	void visit2(Literal const& _node);
	void visit2(TupleExpression const& _tupleExpression);
	bool tryPushConstant(Identifier const& _identifier);
	bool pushMemberOrLocalVarOrConstant(Identifier const& _identifier);

protected:
	void visit2(Identifier const& _identifier);
	void compileUnaryOperation(UnaryOperation const& _node, const std::string& tvmUnaryOperation, const bool isPrefixOperation);
	void compileUnaryDelete(UnaryOperation const& node);
	void visit2(UnaryOperation const& _node);
	static bool argumentsIsGoodForFixedBytes(Type const* a, Type const* b);
	void compareAddresses(Token op);
	bool tryOptimizeBinaryOperation(BinaryOperation const& _node);
	static std::vector<Expression const*> unroll(BinaryOperation const&  _node);
	void visit2(BinaryOperation const& _node);
	void checkBitFit(Type const* type, Type const* lType, Type const* rType, const std::string& opcode);
	void visitMsgMagic(MemberAccess const& _node);
	void visitMagic(MemberAccess const& _node);
	void visit2(MemberAccess const& _node);
	bool checkForAddressMemberAccess(MemberAccess const& _node, Type::Category category);
	void visitMemberAccessArray(MemberAccess const& _node);
	void visitMemberAccessFixedBytes(MemberAccess const& _node, FixedBytesType const* fbt);
	static void indexTypeCheck(IndexAccess const& _node);
	void visit2(IndexAccess const& indexAccess);
	bool checkAbiMethodCall(FunctionCall const& _functionCall);

protected:
	std::string getDefaultMsgValue();
	bool checkRemoteMethodCall(FunctionCall const& _functionCall);
	const FunctionDefinition* getRemoteFunctionDefinition(const MemberAccess* memberAccess);
	void mappingDelMinOrMax(FunctionCall const& _functionCall, bool isDelMin);
	void mappingGetSet(FunctionCall const& _functionCall);
	void mappingPrevNextMethods(FunctionCall const& _functionCall);
	void mappingMinMaxMethod(FunctionCall const& _functionCall, bool isMin);
	void mappingEmpty(FunctionCall const& _functionCall);
	bool checkForMappingOrCurrenciesMethods(FunctionCall const& _functionCall);
	bool visit2(FunctionCall const& _functionCall);
	void visit2(Conditional const& _conditional);
	void visit2(ElementaryTypeNameExpression const& _node);
	bool fold_constants(const Expression *expr);
	static bool isOptionalGet(Expression const* expr);

public:
	LValueInfo expandLValue(
		Expression const* const _expr,
		const bool withExpandLastValue,
	    bool willNoStackPermutationForLValue = false,
	    bool isLValue = true,
		Type const* rightType = nullptr
	);
	void collectLValue(const LValueInfo &lValueInfo, const bool haveValueOnStackTop, bool isValueBuilder);

protected:
	bool tryAssignLValue(Assignment const& _assignment);
	bool tryAssignTuple(Assignment const& _assignment);
	void visit2(Assignment const& _assignment);
};

class DictMinMax : public DictOperation {
public:
	DictMinMax(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, bool isMin) :
			DictOperation{pusher, keyType, valueType}, isMin{isMin} {

	}

	void minOrMax();

private:
	const bool isMin{};
	std::string dictOpcode;
};

class DictPrevNext : public DictOperation {
public:
	DictPrevNext(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, const std::string& oper) :
			DictOperation{pusher, keyType, valueType},
			oper{oper}
	{
	}

	void prevNext();

private:
	const std::string oper;
};

}	// end solidity::frontend
