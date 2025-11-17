/*
 * Copyright (C) 2019-2025 EverX. All Rights Reserved.
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
 * Expression compiler for TVM
 */

#pragma once

#include <libsolidity/codegen/TVMPusher.hpp>

namespace solidity::frontend {

class TVMExpressionCompiler {
public:
	explicit TVMExpressionCompiler(StackPusher& pusher);
	void compileNewExpr(Expression const* expr) const;
	void acceptExpr(Expression const* expr, bool _isResultNeeded);
	static int returnParamQty(Expression const& _e);

	LValueInfo expandLValue(Expression const* const _expr, bool const withExpandLastValue) const;
	void collectLValue(LValueInfo const& lValueInfo, bool haveValueOnStackTop) const;

protected:
	bool acceptExpr(Expression const* expr);
	bool isCurrentResultNeeded() const;
	void visitStringLiteralAbiV2(Literal const& _node) const;
	void visit2(Literal const& _node) const;
	void visit2(TupleExpression const& _tupleExpression) const;

public:
	void visitHonest(TupleExpression const& _tupleExpression, bool onlyDict) const;

protected:
	bool tryPushConstant(Declaration const* declaration) const;
	bool pushLocalOrStateVariable(Identifier const& _identifier) const;

	void visit2(Identifier const& _identifier) const;
	void compileUnaryOperation(
		UnaryOperation const& _node,
		std::string const& tvmUnaryOperation,
		bool isPrefixOperation
	) const;
	void compileUnaryDelete(UnaryOperation const& node) const;
	void visit2(UnaryOperation const& _node) const;
	void compareSlices(Token op) const;
	void compareStrings(Token op) const;
	static std::vector<Expression const*> unroll(BinaryOperation const& _node);
	void visitBinaryOperationForTvmCell(
		std::function<void()> const& pushLeft,
		std::function<void()> const& pushRight,
		Token op
	) const;
	void visitBinaryOperationForString(
		std::function<void()> const& pushLeft,
		std::function<void()> const& pushRight,
		Token const op
	) const;
	void visitLogicalShortCircuiting(BinaryOperation const& _binaryOperation) const;
	void visit2(BinaryOperation const& _binaryOperation) const;
	void visitMathBinaryOperation(
		Token op,
		Type const* leftType,
		Type const* rightType,
		Type const* commonType,
		std::optional<bigint> const& leftValue,
		std::function<void()> const& pushRight,
		std::optional<bigint> const& rightValue
	) const;
	void visitMsgMagic(MemberAccess const& _node) const;
	void visitMagic(MemberAccess const& _memberAccess) const;
	void visit2(MemberAccess const& _node);
	void checkForAddressMemberAccess(MemberAccess const& _node) const;
	void visitMemberAccessArray(MemberAccess const& _node) const;
	void visitMemberAccessFixedBytes(MemberAccess const& _node, FixedBytesType const* fbt) const;
	static void indexTypeCheck(IndexAccess const& _node);
	void visit2(IndexRangeAccess const& indexRangeAccess);
	void visit2(IndexAccess const& indexAccess);
	void visit2(FunctionCall const& _functionCall) const;
	void visit2(Conditional const& _conditional) const;
	bool fold_constants(Expression const* expr) const;

	static void unrollTuple(Type const* type, TypePointers& result);
	void assignTuple(Expression const* lhs, TypePointers const& right, int& index);
	bool tryAssignLValue(Assignment const& _assignment) const;
	bool tryAssignTuple(Assignment const& _assignment);
	void visit2(Assignment const& _assignment);
	void pushIndexAndConvert(IndexAccess const& indexAccess) const;

private:
	StackPusher& m_pusher;
	int m_expressionDepth{};
	bool m_isResultNeeded{};
};

} // end solidity::frontend
