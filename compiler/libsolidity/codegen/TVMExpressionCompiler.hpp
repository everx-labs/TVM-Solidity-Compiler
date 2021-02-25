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

	explicit TVMExpressionCompiler(StackPusherHelper &pusher);
	void compileNewExpr(const Expression* expr);
	void acceptExpr(const Expression* expr, bool _isResultNeeded);
	static std::pair<bool, bigint> constValue(Expression const& _e);
	static std::pair<bool, bool> constBool(Expression const& _e);
	static int returnParamQty(Expression const& _e);

	LValueInfo expandLValue(
			Expression const* const _expr,
			const bool withExpandLastValue,
			bool willNoStackPermutationForLValue = false,
			bool isLValue = true,
			Type const* rightType = nullptr
	);
	void collectLValue(const LValueInfo &lValueInfo, bool haveValueOnStackTop, bool isValueBuilder);

protected:
	bool acceptExpr(const Expression* expr);
	bool isCurrentResultNeeded() const;
	void visitStringLiteralAbiV1(Literal const& _node);
	void visitStringLiteralAbiV2(Literal const& _node);
	void visit2(Literal const& _node);
	void visit2(TupleExpression const& _tupleExpression);
	bool tryPushConstant(Identifier const& _identifier);
	bool pushMemberOrLocalVarOrConstant(Identifier const& _identifier);

	void visit2(Identifier const& _identifier);
	void compileUnaryOperation(UnaryOperation const& _node, const std::string& tvmUnaryOperation, bool isPrefixOperation);
	void compileUnaryDelete(UnaryOperation const& node);
	void visit2(UnaryOperation const& _node);
	void compareAddresses(Token op);
	void compareStrings(Token op);
	bool tryOptimizeBinaryOperation(BinaryOperation const& _node);
	static std::vector<Expression const*> unroll(BinaryOperation const&  _node);
	void visitBinaryOperationForString(BinaryOperation const &_binaryOperation);
	void visitLogicalShortCircuiting(BinaryOperation const &_binaryOperation);
	void visit2(BinaryOperation const& _node);
	bool isCheckFitUseless(Type const* type, Token op);
	void visitMathBinaryOperation(
		Token op,
		Type const* commonType,
		const std::function<void()>& pushRight,
		const std::optional<bigint>& rightValue
	);
	void visitMsgMagic(MemberAccess const& _node);
	void visitMagic(MemberAccess const& _node);
	void visit2(MemberAccess const& _node);
	bool checkForAddressMemberAccess(MemberAccess const& _node, Type::Category category);
	void visitMemberAccessArray(MemberAccess const& _node);
	void visitMemberAccessFixedBytes(MemberAccess const& _node, FixedBytesType const* fbt);
	static void indexTypeCheck(IndexAccess const& _node);
	void visit2(IndexAccess const& indexAccess);
	bool visit2(FunctionCall const& _functionCall);
	void visit2(Conditional const& _conditional);
	bool fold_constants(const Expression *expr);
	static bool isOptionalGet(Expression const* expr);

	bool tryAssignLValue(Assignment const& _assignment);
	bool tryAssignTuple(Assignment const& _assignment);
	void visit2(Assignment const& _assignment);
	void pushIndexAndConvert(IndexAccess const& indexAccess);

private:
	StackPusherHelper &m_pusher;
	int m_expressionDepth;
	bool m_isResultNeeded;
	std::set<Expression const*> m_resultIsSlice;
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
