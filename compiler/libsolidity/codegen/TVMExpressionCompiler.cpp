/*
 * Copyright (C) 2020-2024 EverX. All Rights Reserved.
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

#include  <boost/core/ignore_unused.hpp>

#include <liblangutil/SourceReferenceExtractor.h>

#include <libsolidity/codegen/DictOperations.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMFunctionCall.hpp>
#include <libsolidity/codegen/TVMStructCompiler.hpp>
#include <libsolidity/codegen/TVM.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace solidity;
using namespace std;

TVMExpressionCompiler::TVMExpressionCompiler(StackPusher &pusher) :
		m_pusher{pusher},
		m_expressionDepth{-1},
		m_isResultNeeded{false}
{
}

void TVMExpressionCompiler::compileNewExpr(const Expression *expr) {
	TVMExpressionCompiler ec{m_pusher};
	ec.acceptExpr(expr, true);
}

void TVMExpressionCompiler::acceptExpr(const Expression *expr, const bool _isResultNeeded) {
	solAssert(expr, "");
	// Recursive call are not allowed.
	solAssert(m_expressionDepth == -1, "");
	m_isResultNeeded = _isResultNeeded;
	bool doDropResultIfNeeded = acceptExpr(expr);
	m_expressionDepth = -1;

	if (doDropResultIfNeeded && !m_isResultNeeded) {
		m_pusher.drop(returnParamQty(*expr));
	}
}

bool TVMExpressionCompiler::acceptExpr(const Expression *expr) {
	const int savedExpressionDepth = m_expressionDepth;
	++m_expressionDepth;
	bool doDropResultIfNeeded = true;
	if (fold_constants(expr)) {
	} else if (auto e = to<Literal>(expr)) {
		visit2(*e);
	} else if (auto e0 = to<Identifier>(expr)) {
		visit2(*e0);
	} else if (auto e1 = to<BinaryOperation>(expr)) {
		visit2(*e1);
	} else if (auto e2 = to<UnaryOperation>(expr)) {
		visit2(*e2);
		doDropResultIfNeeded = false;
	} else if (auto e3 = to<Assignment>(expr)) {
		visit2(*e3);
		doDropResultIfNeeded = false;
	} else if (auto e4 = to<TupleExpression>(expr)) {
		visit2(*e4);
	} else if (auto e5 = to<MemberAccess>(expr)) {
		visit2(*e5);
	} else if (auto e6 = to<IndexAccess>(expr)) {
		visit2(*e6);
	} else if (auto e7 = to<FunctionCall>(expr)) {
		visit2(*e7);
	} else if (auto e8 = to<Conditional>(expr)) {
		visit2(*e8);
	} else if (auto e9 = to<IndexRangeAccess>(expr)) {
		visit2(*e9);
	} else {
		cast_error(*expr, string("Unsupported expression ") + typeid(expr).name());
	}

	--m_expressionDepth;
	solAssert(savedExpressionDepth == m_expressionDepth,
			  "Internal error: depth exp " + toString(savedExpressionDepth) + " got " + toString(m_expressionDepth));
	return doDropResultIfNeeded;
}

int TVMExpressionCompiler::returnParamQty(Expression const& _e) {
	if (auto t = to<TupleType>(getType(&_e))) {
		return t->components().size();
	} else {
		return 1;
	}
}

bool TVMExpressionCompiler::isCurrentResultNeeded() const {
	return m_expressionDepth >= 1 || m_isResultNeeded;
}

void TVMExpressionCompiler::visitStringLiteralAbiV2(Literal const &_node) {
	const std::string &str = _node.value();
	m_pusher.pushString(str, false);
}

void TVMExpressionCompiler::visit2(Literal const &_node) {
	const auto* type = getType(&_node);
	switch (_node.annotation().type->category()) {
	case Type::Category::Bool:
		m_pusher << (_node.token() == Token::TrueLiteral? "TRUE" : "FALSE");
		break;
	case Type::Category::Null:
		m_pusher.pushNull();
		break;
	case Type::Category::TVMNaN:
		m_pusher.pushNaN();
		break;
	case Type::Category::EmptyMap:
		m_pusher << "NULL";
		break;
	case Type::Category::StringLiteral:
		visitStringLiteralAbiV2(_node);
		break;
	default:
		cast_error(_node, string("Unsupported type ") + type->canonicalName());
	}
}

void TVMExpressionCompiler::visit2(TupleExpression const &_tupleExpression) {
	vector<ASTPointer<Expression>> const& components = _tupleExpression.components();
	if (_tupleExpression.isInlineArray()) {
		int n = components.size();
		if (*_tupleExpression.annotation().isPure) {
			const int stackSize = m_pusher.stackSize();
			SourceReference sr = SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &_tupleExpression.location());
			const std::string computeName = "inline_array_line_" +
					toString(sr.position.line) + "_column_" + toString(sr.position.column) + "_ast_id_" +
					toString(_tupleExpression.id());

			m_pusher << "PUSHINT " + toString(n);
			m_pusher.computeConstCell(computeName);
			m_pusher << "TUPLE 2";

			m_pusher.ctx().addConstArray(computeName, &_tupleExpression);
			solAssert(stackSize + 1 == m_pusher.stackSize(), "");
		} else {
			visitHonest(_tupleExpression, false);
		}
	} else {
		for (const auto &comp : components) {
			compileNewExpr(comp.get());
		}
	}
}

void TVMExpressionCompiler::visitHonest(TupleExpression const& _tupleExpression, bool onlyDict) {
	const int stackSize = m_pusher.stackSize();
	vector<ASTPointer<Expression>> const& components = _tupleExpression.components();
	int n = components.size();

	Type const* tupleType = _tupleExpression.annotation().type;
	auto arrayBaseType = to<ArrayType>(tupleType)->baseType();
	IntegerType const& arrayKeyType = getArrayKeyType();

	for (ASTPointer<Expression> const& expr : components | boost::adaptors::reversed) {
		compileNewExpr(expr.get());
		m_pusher.convert(arrayBaseType, expr.get()->annotation().type);
	}
	// values...
	m_pusher.pushInt(0);
	// values... index
	m_pusher << "NULL";
	// values... index dict
	m_pusher.pushInt(n);
	// values... index dict totalSize

	m_pusher.startOpaque();
	m_pusher.startContinuation();
	// values... index dict
	m_pusher.rot();
	// values... index dict valueI
	const DataType& dataType = m_pusher.prepareValueForDictOperations(&arrayKeyType, arrayBaseType);
	// values... index dict valueI'
	m_pusher.pushS(2);
	// values... index dict valueI' index
	m_pusher << "INC";
	// values... index dict valueI' index++
	m_pusher.exchange(3);
	// values... index++ dict valueI' index
	m_pusher.rot();
	// values... index++ valueI' index dict
	m_pusher.setDict(arrayKeyType, *arrayBaseType, dataType);
	// values... index++ dict
	m_pusher.endContinuation();
	m_pusher.repeat(false);
	m_pusher.endOpaque(n + 3, 2);

	// index++ dict
	if (onlyDict)
		m_pusher.dropUnder(1, 1);
	else
		m_pusher << "TUPLE 2";
	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
}

bool TVMExpressionCompiler::tryPushConstant(Declaration const* declaration) {
	const auto* variableDeclaration = to<VariableDeclaration>(declaration);
	if (!variableDeclaration || !variableDeclaration->isConstant()){
		return false;
	}
	compileNewExpr(variableDeclaration->value().get());
	m_pusher.convert(declaration->type(), variableDeclaration->value()->annotation().type);
	return true;
}

bool TVMExpressionCompiler::pushLocalOrStateVariable(Identifier const &_identifier) {
	auto& stack = m_pusher.getStack();
	Declaration const* declaration = _identifier.annotation().referencedDeclaration;
	if (stack.isParam(declaration)) {
		auto offset = stack.getOffset(declaration);
		m_pusher.pushS(offset);
		return true;
	} else if (tryPushConstant(declaration)) {
		return true;
	} else if (auto variable = to<VariableDeclaration>(declaration)) {
		solAssert(variable->isStateVariable() && !variable->isConstant(), "");
		m_pusher.getGlob(variable);
		return true;
	}
	return false;
}

void TVMExpressionCompiler::visit2(Identifier const &_identifier) {
	const string& name = _identifier.name();
	if (pushLocalOrStateVariable(_identifier)) {
	} else if (name == "now") {
		// Getting value of `now` variable
		m_pusher << "NOW";
	} else if (name == "this") {
		// calling this.function() creates an internal message that should be sent to the address of current contract
		m_pusher << "MYADDR";
	} else if (auto funDecl = to<FunctionDefinition>(_identifier.annotation().referencedDeclaration)) {
		m_pusher.pushPrivateFunctionId(*funDecl, false);
	} else {
		cast_error(_identifier, "Unsupported identifier: " + name + ".");
	}
}

void TVMExpressionCompiler::compileUnaryOperation(
	UnaryOperation const &_node,
	const std::string &tvmUnaryOperation,
	const bool isPrefixOperation
) {
	Type const* resType = _node.annotation().type;

	auto checkResult = [&](){
		if (!isFitUselessUnary(resType, _node.getOperator()) &&
			!m_pusher.ctx().ignoreIntegerOverflow()
		)
			m_pusher.checkFit(resType);
	};

	const int saveStackSize = m_pusher.stackSize();
	LValueInfo lValueInfo = expandLValue(&_node.subExpression(), true);;
	const int expandedLValueSize = m_pusher.stackSize() - saveStackSize - 1;
	solAssert(expandedLValueSize >= 0, "");
	if (isCurrentResultNeeded()) {
		if (isPrefixOperation) {
			m_pusher << tvmUnaryOperation;
			checkResult();
			m_pusher.pushS(0); // expanded.. value value
			if (expandedLValueSize != 0)
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. value
		} else {
			m_pusher.pushS(0); // expanded.. value value
			m_pusher << tvmUnaryOperation; // expanded.. value newValue
			checkResult();
			if (expandedLValueSize != 0) {
				m_pusher.exchange(1); // expanded.. newValue value
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. newValue
			}
		}
	} else {
		m_pusher << tvmUnaryOperation;
		checkResult();
	}

	collectLValue(lValueInfo, true);
}

void TVMExpressionCompiler::compileUnaryDelete(UnaryOperation const &node) {
	const LValueInfo lValueInfo = expandLValue(&node.subExpression(), false);
	Expression const* lastExpr = lValueInfo.expressions.back();
	Type const* exprType = node.subExpression().annotation().type;
	if (to<Identifier>(lastExpr)) {
		m_pusher.pushDefaultValue(exprType);
		collectLValue(lValueInfo, true);
	} else if (auto memberAccess = to<MemberAccess>(lastExpr)) {
		MemberAccessAnnotation& a = memberAccess->annotation();
		auto decl = to<VariableDeclaration>(a.referencedDeclaration);
		m_pusher.pushDefaultValue(decl->type());
		collectLValue(lValueInfo, true);
	} else if (auto indexAccess = to<IndexAccess>(lastExpr)) {
		Type const* baseExprType = indexAccess->baseExpression().annotation().type;
		if (baseExprType->category() == Type::Category::Array) {
			// ... index dict
			auto arrayType = to<ArrayType>(baseExprType);
			Type const* valueType = arrayType->baseType();
			m_pusher.pushDefaultValue(valueType); // index dict value
			collectLValue(lValueInfo, true);
		} else if (baseExprType->category() == Type::Category::TvmVector) {
			// ... index vector
			auto arrayType = to<TvmVectorType>(baseExprType);
			Type const* valueType = arrayType->valueType();
			m_pusher.pushDefaultValue(valueType); // index vector value
			collectLValue(lValueInfo, true);
		} else if (baseExprType->category() == Type::Category::Mapping) { // mapping
			// ... index dict
			m_pusher.pushS(1);                            // ... index dict index
			Type const* dictKey = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
			m_pusher.exchange(1);                                // ... index index' dict
			m_pusher.pushInt(dictKeyLength(dictKey)); // ..index index dict nbits
			m_pusher << "DICT" + typeToDictChar(dictKey) + "DEL";  // ... index dict' {-1,0}
			m_pusher.drop();                               // ... index dict'
			collectLValue(lValueInfo, false);
		} else {
			solUnimplemented("");
		}
	} else {
		solUnimplemented("");
	}
}

void TVMExpressionCompiler::visit2(UnaryOperation const &_node) {
	auto type = getType(&_node.subExpression());
	TypeInfo ti{type};
	std::string const prefix = ti.isQuiet ? "Q" : "";

	auto op = _node.getOperator();
	if (op == Token::Inc) {
		compileUnaryOperation(_node, prefix + "INC", _node.isPrefixOperation());
	} else if (op == Token::Dec) {
		compileUnaryOperation(_node, prefix + "DEC", _node.isPrefixOperation());
	} else if (op == Token::Not) {
		solAssert(ti.isNumeric, "! operator is supported only for numbers.");
		compileNewExpr(&_node.subExpression());
		m_pusher << prefix + "NOT";
	} else if (op == Token::Sub) {
		solAssert(ti.isNumeric, "- operator is supported only for numbers.");
		compileNewExpr(&_node.subExpression());
		m_pusher << prefix + "NEGATE";
		if (!m_pusher.ctx().ignoreIntegerOverflow())
			m_pusher.checkFit(getType(&_node.subExpression()));
	} else if (op == Token::BitNot) {
		solAssert(ti.isNumeric, "~ operator is supported only for numbers.");
		if (ti.isSigned) {
			compileNewExpr(&_node.subExpression());
			m_pusher << prefix + "BITNOT";
		} else {
			m_pusher.pushInt((bigint(1) << ti.numBits) - 1);
			compileNewExpr(&_node.subExpression());
			m_pusher << prefix + "SUB";
		}
	} else if (op == Token::Delete) {
		compileUnaryDelete(_node);
	} else {
		cast_error(_node, toString("Unsupported operation: ") + TokenTraits::toString(op));
	}
}

void TVMExpressionCompiler::compareSlices(Token op) {
	switch(op) {
		case Token::GreaterThan:
			m_pusher << "SDLEXCMP";
			m_pusher << "ISPOS";
			break;
		case Token::GreaterThanOrEqual:
			m_pusher << "SDLEXCMP";
			m_pusher << "ISNNEG";
			break;
		case Token::LessThan:
			m_pusher << "SDLEXCMP";
			m_pusher << "ISNEG";
			break;
		case Token::LessThanOrEqual:
			m_pusher << "SDLEXCMP";
			m_pusher << "ISNPOS";
			break;
		case Token::Equal:
			m_pusher << "SDEQ";
			break;
		case  Token::NotEqual:
			m_pusher << "SDEQ";
			m_pusher << "NOT";
			break;
		default:
			solUnimplemented("Wrong compare operation");
	}
}

void TVMExpressionCompiler::compareStrings(Token op) {
	m_pusher.pushFragmentInCallRef(2, 1, "__compareStrings");
	switch(op) {
	case Token::GreaterThan:
		m_pusher << "ISPOS";
		break;
	case Token::GreaterThanOrEqual:
		m_pusher << "ISNNEG";
		break;
	case Token::LessThan:
		m_pusher << "ISNEG";
		break;
	case Token::LessThanOrEqual:
		m_pusher << "ISNPOS";
		break;
	case Token::Equal:
		m_pusher << "EQINT 0";
		break;
	case  Token::NotEqual:
		m_pusher << "NEQINT 0";
		break;
	default:
		solUnimplemented("Wrong compare operation");
	}
}

std::vector<Expression const *> TVMExpressionCompiler::unroll(BinaryOperation const &_node) {
	std::vector<Expression const*> result;
	const Token op = _node.getOperator();
	result.push_back(&_node.rightExpression());
	Expression const* leftExpression = &_node.leftExpression();
	while (to<BinaryOperation>(leftExpression)) {
		auto leftBinOpExpr = to<BinaryOperation>(leftExpression);
		if (leftBinOpExpr->getOperator() == op) {
			result.push_back(&leftBinOpExpr->rightExpression());
		} else {
			break;
		}
		leftExpression = &leftBinOpExpr->leftExpression();
	}
	result.push_back(leftExpression);
	std::reverse(result.begin(), result.end());
	return result;
}

void TVMExpressionCompiler::visitBinaryOperationForString(
	const std::function<void()>& pushLeft,
	const std::function<void()>& pushRight,
	const Token op
) {
	if (op == Token::Add) {
		pushLeft();
		pushRight();
		m_pusher.pushFragmentInCallRef(2, 1, "__concatenateStrings");
	} else if (op == Token::Equal || op == Token::NotEqual) {
		visitBinaryOperationForTvmCell(pushLeft, pushRight, op);
	} else if (TokenTraits::isCompareOp(op)){
		pushLeft();
		pushRight();
		compareStrings(op);
	} else {
		solUnimplemented("Unsupported binary operation");
	}
}

void TVMExpressionCompiler::visitBinaryOperationForTvmCell(
	const std::function<void()>& pushLeft,
	const std::function<void()>& pushRight,
	const Token op
) {
	if (op == Token::Equal || op == Token::NotEqual) {
		pushLeft();
		m_pusher << "HASHCU";
		pushRight();
		m_pusher << "HASHCU";
		if (op == Token::Equal)
			m_pusher << "EQUAL";
		else
			m_pusher << "NEQ";
	} else {
		solUnimplemented("Unsupported binary operation");
	}
}

void TVMExpressionCompiler::visitLogicalShortCircuiting(BinaryOperation const &_binaryOperation) {
	const Token op = _binaryOperation.getOperator();
	solAssert(op == Token::And || op == Token::Or, "");
	bool const isAnd = op == Token::And;
	bool const isQuiet = _binaryOperation.annotation().type->category() == Type::Category::QBool;
	if (isQuiet) {
		int const startStackSize = m_pusher.stackSize();
		std::vector<Expression const *> order = unroll(_binaryOperation);
		compileNewExpr(order[0]); // a
		for (int i = 1; i < static_cast<int>(order.size()); ++i) {
			m_pusher.pushS(0); // a a
			m_pusher << "ISNAN"; // a a.isNaN()
			m_pusher.pushS(0); // a a.isNaN() a.isNaN()
			m_pusher.startContinuation();
			{
				// a a.isNaN()
				m_pusher.drop(); // a
				m_pusher.pushS(0); // a a
				if (!isAnd)
					m_pusher << "NOT"; // a !a
			}
			m_pusher.endContinuation();
			m_pusher.ifNot();
			// a (a.isNaN() || a.get())  // for &&
			// a (a.isNaN() || !a.get()) // for ||
			m_pusher.startContinuation();
			{
				// a
				m_pusher.fixStack(startStackSize - m_pusher.stackSize() + 1);
				compileNewExpr(order[i]); // a b

				m_pusher.pushS(0); // a b b
				m_pusher << "ISNAN"; // a b b.isNaN()
				m_pusher.pushS(0); // a b b.isNaN() b.isNaN()
				m_pusher.startContinuation();
				{
					// a b !b.isNaN()
					m_pusher.drop(); // a b
					m_pusher.pushS(0); // a b
					if (!isAnd)
						m_pusher << "NOT"; // a b !b
				}
				m_pusher.endContinuation();
				m_pusher.ifNot();
				// a b (b.isNaN() || b.get())  // for &&
				// a b (b.isNaN() || !b.get()) // for ||
				m_pusher.startContinuation();
				{
					// a b
					m_pusher << (isAnd ? "QAND" : "QOR");
				}
				m_pusher.endContinuation();
				m_pusher.startContinuation();
				{
					// a b
					m_pusher.dropUnder(1, 1);
					// b
				}
				m_pusher.endContinuation();
				m_pusher.ifElse();
			}
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.fixStack(startStackSize - m_pusher.stackSize() + 1);
		solAssert(m_pusher.stackSize() == startStackSize + 1, "");
	} else {
		std::vector<Expression const *> order = unroll(_binaryOperation);
		compileNewExpr(order[0]);
		for (int i = 1; i < static_cast<int>(order.size()); ++i) {
			m_pusher.pushS(0);
			m_pusher.fixStack(-1); // fix stack
			m_pusher.startContinuation();
			m_pusher.drop();
			compileNewExpr(order[i]);
		}
		for (int i = static_cast<int>(order.size()) - 1; i >= 1; --i)
			m_pusher.endLogCircuit(op == Token::Or ? LogCircuit::Type::OR : LogCircuit::Type::AND);
	}
}

void TVMExpressionCompiler::visit2(BinaryOperation const &_binaryOperation) {
	const Token op = _binaryOperation.getOperator();
	const auto &lexp = _binaryOperation.leftExpression();
	const auto &rexp = _binaryOperation.rightExpression();
	Type const *lt = getType(&lexp);
	Type const *rt = getType(&rexp);
	Type const* &commonType = _binaryOperation.annotation().commonType;
	Type const* leftTargetType = commonType;
	Type const* rightTargetType = TokenTraits::isShiftOp(op) || op == Token::Exp ?
			rexp.annotation().type->mobileType() : commonType;

	if (lt->category() == Type::Category::Function || rt->category() == Type::Category::Function) {
		solUnimplemented("Unsupported binary operation");
	}

	auto acceptLeft = [&]() {
		compileNewExpr(&lexp);
		m_pusher.convert(leftTargetType, lt);
	};

	auto acceptRight = [&]() {
		compileNewExpr(&rexp);
		m_pusher.convert(rightTargetType, rt);
	};

	if (_binaryOperation.userDefinedFunctionType()) {
		acceptLeft();
		acceptRight();
		FunctionDefinition const* functionDef = *_binaryOperation.annotation().userDefinedFunction;
		m_pusher.pushCallOrCallRef(functionDef, std::nullopt, false);
		return ;
	}

	if (isString(commonType)) {
		visitBinaryOperationForString(acceptLeft, acceptRight, op);
		return;
	}

	if ((lt->category() == Type::Category::TvmCell &&
		 rt->category() == Type::Category::TvmCell) ||
		(isByteArrayOrString(lt->mobileType()) && isByteArrayOrString(rt->mobileType()))) {
		visitBinaryOperationForTvmCell(acceptLeft, acceptRight, op);
		return;
	}

	if (isAddressOrAddressStdOrContractType(lt) || isAddressOrAddressStdOrContractType(rt) || (isSlice(lt) && isSlice(rt))) {
		acceptLeft();
		acceptRight();
		compareSlices(op);
		return;
	}

	if (op == Token::And || op == Token::Or) {
		visitLogicalShortCircuiting(_binaryOperation);
		return;
	}

	acceptLeft();
	if (op == Token::SHR) cast_error(_binaryOperation, "Unsupported operation >>>");
	if (op == Token::Comma) cast_error(_binaryOperation, "Unsupported operation ,");

	const auto& leftValue = ExprUtils::constValue(_binaryOperation.leftExpression());
	const auto& rightValue = ExprUtils::constValue(_binaryOperation.rightExpression());
	visitMathBinaryOperation(op, lt, rt, commonType, leftValue, acceptRight, rightValue);
}

// if pushRight is set we haven't value on the stack
// else right value is on the stack
void TVMExpressionCompiler::visitMathBinaryOperation(
	const Token op,
	Type const* leftType,
	Type const* rightType,
	Type const* commonType,
	const std::optional<bigint>& leftValue,
	const std::function<void()>& pushRight,
	const std::optional<bigint>& rightValue
) {
	bool const isQuiet = isIn(commonType->category(), Type::Category::QInteger, Type::Category::QBool);
	std::string const prefix = isQuiet ? "Q" : "";

	bool canOverflow = false;
	if (op == Token::Exp) {
		if (rightValue.has_value() && (*rightValue == 2 || *rightValue == 3 || *rightValue == 4)) {
			if (*rightValue == 2) {
				m_pusher.pushS(0);
				m_pusher << prefix + "MUL";
			} else if (*rightValue == 3) {
				m_pusher.pushS(0);
				m_pusher.pushS(0);
				m_pusher << prefix + "MUL";
				m_pusher << prefix + "MUL";
			} else if (*rightValue == 4) {
				m_pusher.pushS(0);
				m_pusher << prefix + "MUL";
				m_pusher.pushS(0);
				m_pusher << prefix + "MUL";
			} else {
				solUnimplemented("");
			}
		} else {
			if (leftValue.has_value() && leftValue == 2) {
				solAssert(!isQuiet, "TODO support!"); // TODO quint(1) == 1
				m_pusher.drop();
				pushRight();
				m_pusher << "POW2";
			} else {
				pushRight();
				std::string expName = isQuiet ? "__qexp" : "__exp";
				m_pusher.pushFragmentInCallRef(2, 1, expName);
			}
		}
		canOverflow = true;
	} else {
		if (pushRight)
			pushRight();

		if (op == Token::Add) {
			m_pusher << prefix + "ADD";
			canOverflow = true;
		} else if (op == Token::Mul) {
			if (commonType->category() == Type::Category::FixedPoint) {
				solAssert(!isQuiet, "");
				int power = to<FixedPointType>(commonType)->fractionalDigits();
				m_pusher.pushInt(MathConsts::power10().at(power));
				m_pusher << "MULDIV";
			} else {
				m_pusher << prefix + "MUL";
			}
			canOverflow = true;
		} else if (op == Token::Sub) {
			m_pusher << prefix + "SUB";
			canOverflow = true;
		}
		else if (op == Token::Mod)
			m_pusher << prefix + "MOD";
		else if (op == Token::Div) {
			if (commonType->category() == Type::Category::FixedPoint) {
				solAssert(!isQuiet, "");
				int power = to<FixedPointType>(commonType)->fractionalDigits();
				m_pusher.pushInt(MathConsts::power10().at(power)); // a b 10^n
				m_pusher.exchange(1); // a 10^n b
				m_pusher << "MULDIV"; // a * 10^n / b
			} else {
				m_pusher << prefix + "DIV";
			}
			canOverflow = true;
		}
		else if (op == Token::GreaterThan) m_pusher << prefix + "GREATER";
		else if (op == Token::GreaterThanOrEqual) m_pusher << prefix + "GEQ";
		else if (op == Token::LessThan) m_pusher << prefix + "LESS";
		else if (op == Token::LessThanOrEqual) m_pusher << prefix + "LEQ";
		else if (op == Token::Equal) m_pusher << prefix + "EQUAL";
		else if (op == Token::NotEqual) m_pusher << prefix + "NEQ";
		else if (op == Token::BitAnd) {
			if (isQuiet)
				m_pusher.pushFragmentInCallRef(2, 1, "__qand");
			else
				m_pusher << "AND";
		}
		else if (op == Token::BitOr) {
			if (isQuiet)
				m_pusher.pushFragmentInCallRef(2, 1, "__qor");
			else
				m_pusher << "OR";
		}
		else if (op == Token::SHL) {
			if (leftValue.has_value() && leftValue == 1) {
				solAssert(!isQuiet, ""); // TODO quint(1) == 1
				m_pusher.dropUnder(1, 1);
				m_pusher << "POW2";
			} else
				m_pusher << prefix + "LSHIFT";
			canOverflow = true;
		}
		else if (op == Token::SAR) m_pusher << prefix + "RSHIFT";
		else if (op == Token::BitXor) m_pusher << prefix + "XOR";
		else
			solUnimplemented("Unsupported binary operation");
	}

	if (canOverflow &&
		!isFitUseless(leftType, rightType, commonType, op) &&
		!m_pusher.ctx().ignoreIntegerOverflow()
	)
		m_pusher.checkFit(commonType);
}

void TVMExpressionCompiler::visitMsgMagic(MemberAccess const &_node) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool  bounced:Bool
	//                 src:MsgAddress  dest:MsgAddressInt
	//                 value:CurrencyCollection  ihr_fee:Grams  fwd_fee:Grams
	//                 created_lt:uint64  created_at:uint32
	//                 = CommonMsgInfoRelaxed;

	// (DEPTH - 3) - message cell
	// (DEPTH - 4) - slice with payload (message body)
	// (DEPTH - 5) - transaction id (-2, -1, 0) for int, ext and ticktock


	if (_node.memberName() == "sender") { // msg.sender
		m_pusher.getGlob(TvmConst::C7::SenderAddress);
	} else if (_node.memberName() == "value") { // msg.value
		if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
			m_pusher.push("INCOMINGVALUE");
			m_pusher.indexNoexcep(0);
		} else {
			m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -2",
				"PICK",
			}, 0, 1, true));
		}
	} else if (_node.memberName() == "data") { // msg.data
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -3",
			"PICK",
		}, 0, 1, true));
	} else if (_node.memberName() == "forwardFee") { // msg.forwardFee
		m_pusher.pushFragment(0, 1, "__forwardFee");
	} else if (_node.memberName() == "importFee") { // msg.importFee
		m_pusher.pushFragment(0, 1, "__importFee");
	} else  if (isIn(_node.memberName(), "isInternal", "isExternal", "isTickTock")) {
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -5",
			"PICK",
		}, 0, 1, true));
		if (_node.memberName() == "isInternal") {
			m_pusher << "EQINT 0";
		} else if (_node.memberName() == "isExternal") {
			m_pusher << "EQINT -1";
		} else if (_node.memberName() == "isTickTock") {
			m_pusher << "EQINT -2";
		} else {
			solUnimplemented("");
		}
	} else  if (_node.memberName() == "createdAt") { // msg.createdAt
		m_pusher.startContinuation();
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -5",
				"PICK",
				"PUSHCONT {",
				"	PUSHINT 0",
				"}",
				"PUSHCONT {",
				"	DEPTH",
				"	ADDCONST -3",
				"	PICK",
				"	CTOS",
				"	LDU 4",
				"	LDMSGADDR",
				"	LDMSGADDR",
				"	LDGRAMS",
				"	LDDICT",
				"	LDGRAMS",
				"	LDGRAMS",
				"	LDU 64",
				"	PLDU 32",
				"	BLKDROP2 8, 1",
				"}",
				"IFELSE",
		}, 0, 1, true));
		m_pusher.pushRefContAndCallX(0, 1, true);
	} else  if (_node.memberName() == "hasStateInit") { // msg.hasStateInit
		m_pusher.startContinuation();
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -3",
				"PICK",
				"CTOS",
				"DEPTH",
				"ADDCONST -5",
				"PICK",
				"PUSHCONT {",
					// ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt
					//  import_fee:Grams = CommonMsgInfo;
				"	LDU 2",
				"	LDMSGADDR",
				"	LDMSGADDR",
				"	LDGRAMS",
				"	PLDI 1",
				"	BLKDROP2 4, 1",
				"}",
				"PUSHCONT {",
					// int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
					//  src:MsgAddressInt dest:MsgAddressInt
					//  value:CurrencyCollection ihr_fee:Grams fwd_fee:Grams
					//  created_lt:uint64 created_at:uint32 = CommonMsgInfo;
				"	LDU 4",
				"	LDMSGADDR",
				"	LDMSGADDR",
				"	LDGRAMS",
				"	LDDICT",
				"	LDGRAMS",
				"	LDGRAMS",
				"	LDU 96",
				"	PLDI 1",
				"	BLKDROP2 8, 1",
				"}",
				"IFELSE",
		}, 0, 1, true));
		m_pusher.pushRefContAndCallX(0, 1, true);
	} else if (_node.memberName() == "currencies") { // msg.currencies
		m_pusher.startContinuation();
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -5",
			"PICK",
			"PUSHCONT {",
			"	NULL",
			"}",
			"PUSHCONT {",
			"	DEPTH",
			"	ADDCONST -3",
			"	PICK",
			"	CTOS",
			"	LDU 4",
			"	LDMSGADDR",
			"	LDMSGADDR",
			"	LDGRAMS",
			"	PLDDICT",
			"	BLKDROP2 4, 1",
			"}",
			"IFELSE"
		}, 0, 1, true));
		m_pusher.pushRefContAndCallX(0, 1, true);
	} else if (_node.memberName() == "body") {
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -4",
			"PICK",
		}, 0, 1, true));
	} else {
		cast_error(_node, "Unsupported magic");
	}
}

void TVMExpressionCompiler::visitMagic(MemberAccess const &_memberAccess) {
	auto unsupportedMagic = [&]() {
		cast_error(_memberAccess, "Unsupported magic");
	};

	Type const* exprType = _memberAccess.expression().annotation().type;
	auto magicType = to<MagicType>(exprType);
	ASTString const& member = _memberAccess.memberName();

	switch (magicType->kind()) {
	case MagicType::Kind::Message:
		visitMsgMagic(_memberAccess);
		break;
	case MagicType::Kind::Transaction: {
		if (isIn(member, "timestamp", "logicaltime")) {
			m_pusher << "LTIME";
		} else if (member == "storageFee") {
			m_pusher << "STORAGEFEE";
		} else if (member == "storageFees") {
			m_pusher << "STORAGEFEES";
		} else {
			unsupportedMagic();
		}
		break;
	}
	case MagicType::Kind::Block: {
		if (member == "timestamp") {
			m_pusher << "NOW";
		} else if (member == "logicaltime") {
			m_pusher << "BLOCKLT";
		} else {
			unsupportedMagic();
		}
		break;
	}
	case MagicType::Kind::MetaType: {
		if (member == "min" || member == "max") {
			auto const* arg = to<MagicType>(_memberAccess.expression().annotation().type);
			string opcode = "PUSHINT ";
			const Type *argType = arg->typeArgument();
			if (auto const* integerType = to<IntegerType>(argType))
				opcode += toString(member == "min" ? integerType->minValue() : integerType->maxValue());
			else if (auto const* varint = to<VarIntegerType>(argType))
				opcode += toString(member == "min" ? varint->asIntegerType().minValue() : varint->asIntegerType().maxValue());
			else if (auto const* enumType = to<EnumType>(argType))
				opcode += toString(member == "min" ? enumType->minValue() : enumType->maxValue());
			else
				solAssert(false, "min/max not available for the given type.");
			m_pusher << opcode;
		} else {
			unsupportedMagic();
		}
		break;
	}
	default:
		unsupportedMagic();
	}
}

void TVMExpressionCompiler::visit2(MemberAccess const &_node) {
	const std::string& memberName = _node.memberName();
	auto category = getType(&_node.expression())->category();
	if (category == Type::Category::Struct) {
		Expression const* expression = &_node.expression();
		acceptExpr(expression);

		auto structType = to<StructType>(_node.expression().annotation().type);
		StructCompiler structCompiler{&m_pusher, structType};
		structCompiler.pushMember(memberName);

		return;
	}

	if (category == Type::Category::Magic) {
		return visitMagic(_node);
	}
	if (isIn(category, Type::Category::Address, Type::Category::AddressStd)) {
		checkForAddressMemberAccess(_node);
		return;
	}

	if (category == Type::Category::Array)
		return visitMemberAccessArray(_node);

	if (category == Type::Category::FixedBytes)
		return visitMemberAccessFixedBytes(_node, to<FixedBytesType>(getType(&_node.expression())));

	if (category == Type::Category::TypeType) {
		auto typeType = to<TypeType>(_node.expression().annotation().type);
		auto actualType = typeType->actualType();
		if (isIn(actualType->category(), Type::Category::Address, Type::Category::AddressStd)) {
			solAssert(memberName == "addrNone", "");
			m_pusher.pushSlice("x2_");
			return;
		}
		if (auto enumType = to<EnumType>(actualType)) {
			unsigned int value = enumType->memberValue(memberName);
			m_pusher << "PUSHINT " + toString(value);
			return;
		}
		auto conType = to<ContractType>(actualType);
		if (conType != nullptr && conType->contractDefinition().isLibrary()) {
			auto funType = to<FunctionType>(_node.annotation().type);
			if (funType) {
				auto funDef = to<FunctionDefinition>(&funType->declaration());
				m_pusher.pushPrivateFunctionId(*funDef, false);
				return ;
			}
		}
		if (fold_constants(&_node)) {
			return;
		}
		if (tryPushConstant(_node.annotation().referencedDeclaration)) {
			return;
		}
	}

	cast_error(_node, "Not supported.");
}

void TVMExpressionCompiler::checkForAddressMemberAccess(MemberAccess const &_node) {
	if (_node.memberName() == "balance") {
		if (!isAddressThis(to<FunctionCall>(&_node.expression()))) {
			cast_error(_node.expression(), "Only 'address(this).balance' is supported for member balance");
		}
		m_pusher << "GETPARAM 7";
		m_pusher.indexNoexcep(0);
	}
	if (_node.memberName() == "currencies") {
		if (!isAddressThis(to<FunctionCall>(&_node.expression()))) {
			cast_error(_node.expression(), "Only 'address(this).currencies' is supported for member currencies");
		}
		m_pusher << "GETPARAM 7";
		m_pusher.indexNoexcep(1);
	}
	if (_node.memberName() == "wid") {
		compileNewExpr(&_node.expression());
		m_pusher << "PARSEMSGADDR";
		m_pusher.indexWithExcep(2);
	}
	if (_node.memberName() == "value") {
		compileNewExpr(&_node.expression());
		m_pusher << "PARSEMSGADDR";
		m_pusher.indexWithExcep(3);
		m_pusher << "PLDU 256";
	}
}

void TVMExpressionCompiler::visitMemberAccessArray(MemberAccess const &_node) {
	auto arrayType = to<ArrayType>(_node.expression().annotation().type);
	if (_node.memberName() == "length") {
		compileNewExpr(&_node.expression());
		if (arrayType->isByteArrayOrString()) {
			m_pusher.byteLengthOfCell();
		} else {
			m_pusher.indexNoexcep(0);
		}
	} else {
		cast_error(_node, "Unsupported member access");
	}
}

void TVMExpressionCompiler::visitMemberAccessFixedBytes(MemberAccess const &_node, FixedBytesType const *fbt) {
	if (_node.memberName() == "length" && to<Identifier>(&_node.expression())) {
		m_pusher.pushInt(static_cast<int>(fbt->storageBytes()));
		return;
	}
	cast_error(_node, "Unsupported");
}

void TVMExpressionCompiler::indexTypeCheck(IndexAccess const &_node) {
	Type const* baseExprType = _node.baseExpression().annotation().type;
	Type::Category baseExprCategory = _node.baseExpression().annotation().type->category();
	if (!isIn(baseExprCategory, Type::Category::Mapping, Type::Category::TvmVector) && !isUsualArray(baseExprType)) {
		cast_error(_node, "Index access is supported only for dynamic arrays, vectors and mappings");
	}
}

void TVMExpressionCompiler::visit2(IndexRangeAccess const &indexRangeAccess) {
	Type const *baseType = indexRangeAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArrayOrString()) {
			acceptExpr(&indexRangeAccess.baseExpression()); // bytes
			if (indexRangeAccess.startExpression())
				compileNewExpr(indexRangeAccess.startExpression());
			else
				m_pusher.pushInt(0);

			if (indexRangeAccess.endExpression()) {
				compileNewExpr(indexRangeAccess.endExpression());
				m_pusher.pushFragmentInCallRef(3, 1, "__arraySlice");
			} else {
				m_pusher.pushInt(0xFFFF'FFFF);
				m_pusher << "TRUE";
				m_pusher.pushFragmentInCallRef(4, 1, "__subCell");
			}
			return;
		}
	}
	solUnimplemented("Wrong range expression");
}

void TVMExpressionCompiler::visit2(IndexAccess const &indexAccess) {
	Type const *baseType = indexAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArrayOrString()) {
			acceptExpr(&indexAccess.baseExpression()); // bytes
			m_pusher << "CTOS";
			compileNewExpr(indexAccess.indexExpression()); // slice index
			m_pusher.startContinuation();
			m_pusher.pushInt(127);
			m_pusher << "DIVMOD"; // slice cntRef rest
			m_pusher.rotRev(); // rest slice cntRef
			m_pusher.startContinuation();
			m_pusher << "PLDREF";
			m_pusher << "CTOS";
			m_pusher.endContinuation();
			m_pusher.repeat(false);
			m_pusher.fixStack(-1); // fix stack
			// rest slice
			m_pusher.exchange(1); // slice rest
			m_pusher << "MULCONST 8";
			m_pusher << "SDSKIPFIRST";
			m_pusher << "PLDU 8";
			m_pusher.pushRefContAndCallX(2, 1, false);
			return;
		} else {
			compileNewExpr(indexAccess.indexExpression()); // index
			acceptExpr(&indexAccess.baseExpression()); // index array
			m_pusher.indexNoexcep(1); // index dict
		}
	} else if (baseType->category() == Type::Category::TvmVector) {
		acceptExpr(&indexAccess.baseExpression()); // tuple
		const auto& val = ExprUtils::constValue(*indexAccess.indexExpression());
		if (val.has_value() && val <= 15)
			m_pusher.indexWithExcep(boost::lexical_cast<int>(val.value().str()));
		else {
			acceptExpr(indexAccess.indexExpression()); // vector index
			m_pusher << "INDEXVAR";
		}
		auto vectorType = to<TvmVectorType>(indexAccess.baseExpression().annotation().type);
		auto valueTupleType = to<TupleType>(vectorType->valueType());
		if (valueTupleType)
			m_pusher << "UNTUPLE " + toString(valueTupleType->components().size());
		return;
	} else if (baseType->category() == Type::Category::FixedBytes) {
		acceptExpr(&indexAccess.baseExpression()); // integer
		auto fbt = to<FixedBytesType>(getType(&indexAccess.baseExpression()));
		m_pusher.pushInt(static_cast<int>(fbt->storageBytes()) - 1);
		acceptExpr(indexAccess.indexExpression()); // integer byte_len byte_index
		m_pusher << "SUB";
		m_pusher << "MULCONST 8";
		m_pusher << "RSHIFT";
		m_pusher << "MODPOW2 8";
		return;
	}	else {
		pushIndexAndConvert(indexAccess); // index
		m_pusher.prepareKeyForDictOperations(indexAccess.indexExpression()->annotation().type, false);
		acceptExpr(&indexAccess.baseExpression()); // index dict
	}

	m_pusher.getDict(*StackPusher::parseIndexType(baseType),
					 *indexAccess.annotation().type,
					 baseType->category() == Type::Category::Mapping ?
					 GetDictOperation::GetFromMapping :
					 GetDictOperation::GetFromArray);
}

void TVMExpressionCompiler::visit2(FunctionCall const &_functionCall) {
	FunctionCallCompiler fcc(m_pusher, _functionCall, isCurrentResultNeeded());
	fcc.compile();
}

void TVMExpressionCompiler::visit2(Conditional const &_conditional) {
	const int stackSize = m_pusher.stackSize();

	const int paramQty = returnParamQty(_conditional.trueExpression());
	compileNewExpr(&_conditional.condition());
	m_pusher.fixStack(-1); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.trueExpression());
	m_pusher.endContinuation();
	m_pusher.fixStack(-paramQty); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.falseExpression());
	m_pusher.endContinuation();
	m_pusher.fixStack(-paramQty); // fix stack


	m_pusher.pushConditional(paramQty);
	solAssert(stackSize + paramQty == m_pusher.stackSize(), "");
}

namespace {
bool isFunctionKind(Expression const* expr, FunctionType::Kind kind) {
	auto funCall = to<FunctionCall>(expr);
	if (!funCall)
		return false;
	auto functionType = to<FunctionType>(funCall->expression().annotation().type);
	return functionType && functionType->kind() == kind;
}

bool isOptionalGet(Expression const* expr) {
	return isFunctionKind(expr, FunctionType::Kind::OptionalGet);
}

bool isStackTop(Expression const* expr) {
	return isFunctionKind(expr, FunctionType::Kind::TVMStackTop);
}
}

LValueInfo
TVMExpressionCompiler::expandLValue(
	Expression const *const _expr,
	const bool withExpandLastValue
) {
	if (!*_expr->annotation().isLValue)
		cast_error(*_expr, "Expression has to be an lvalue.");
	const int startStackSize = m_pusher.stackSize();
	LValueInfo lValueInfo {};

	Expression const* expr = _expr;
	while (true) {
		lValueInfo.expressions.push_back(expr);
		if (to<Identifier>(expr)) {
			break;
		} else if (auto index2 = to<IndexAccess>(expr)) {
			indexTypeCheck(*index2);
			expr = &index2->baseExpression();
		} else if (
			auto memberAccess = to<MemberAccess>(expr);
			memberAccess && getType(&memberAccess->expression())->category() == Type::Category::Struct
		) {
			expr = &memberAccess->expression();
		} else if (isOptionalGet(expr) || isStackTop(expr)) {
			auto funCall = to<FunctionCall>(expr);
			auto ma = to<MemberAccess>(&funCall->expression());
			expr = &ma->expression();
		} else {
			cast_error(*expr, "Unsupported lvalue.");
		}
	}
	std::reverse(lValueInfo.expressions.begin(), lValueInfo.expressions.end());

	const int n = static_cast<int>(lValueInfo.expressions.size());
	for (int i = 0; i < n; i++) {
		bool isLast = i + 1 == n;
		if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
			auto& stack = m_pusher.getStack();
			auto name = variable->name();
			if (stack.isParam(variable->annotation().referencedDeclaration)) {
				if (isLast && !withExpandLastValue)
					break;
				pushLocalOrStateVariable(*variable);
			} else {
				if (isLast && !withExpandLastValue)
					break;
				auto vd = to<VariableDeclaration>(variable->annotation().referencedDeclaration);
				m_pusher.getGlob(vd);
			}
		} else if (auto index = to<IndexAccess>(lValueInfo.expressions[i])) {
			if (index->baseExpression().annotation().type->category() == Type::Category::Mapping) {
				// dict1
				pushIndexAndConvert(*index); // dict1 index
				m_pusher.prepareKeyForDictOperations(index->indexExpression()->annotation().type, false);
				// dict1 index
				m_pusher.exchange(1);
				// index dict1
				if (isLast && !withExpandLastValue)
					break;
				m_pusher.pushS2(1, 0);
				// index dict1 index dict1

				m_pusher.getDict(*StackPusher::parseIndexType(index->baseExpression().annotation().type),
								 *index->annotation().type,
								 GetDictOperation::GetFromMapping);
				// index dict1 dict2
			} else if (index->baseExpression().annotation().type->category() == Type::Category::TvmVector) {
				// vector
				compileNewExpr(index->indexExpression()); // vector index
				if (isLast && !withExpandLastValue)
					break;
				m_pusher.pushS2(1, 0); // vector index vector index
				m_pusher << "INDEXVAR"; // vector index value
			} else if (index->baseExpression().annotation().type->category() == Type::Category::Array) {
				// array
				m_pusher << "UNTUPLE 2"; // size dict
				compileNewExpr(index->indexExpression()); // size dict index
				m_pusher.exchange(1); // size index dict
				m_pusher.pushS2(1, 2); // size index dict index size
				m_pusher << "LESS"; // size index dict index<size
				m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				if (isLast && !withExpandLastValue)
					break;
				m_pusher.pushS2(1, 0); // size index dict index dict
				m_pusher.getDict(*StackPusher::parseIndexType(index->baseExpression().annotation().type),
								 *index->annotation().type, GetDictOperation::GetFromArray);
				// size index dict value
			} else {
				solUnimplemented("");
			}
		} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
			auto structType = to<StructType>(memberAccess->expression().annotation().type);
			StructCompiler structCompiler{&m_pusher, structType};
			const string &memberName = memberAccess->memberName();
			if (isLast && !withExpandLastValue) {
				break;
			}
			m_pusher.pushS(0);
			structCompiler.pushMember(memberName);
		} else if (isOptionalGet(lValueInfo.expressions[i])) {
			if (!isLast || withExpandLastValue)
				m_pusher.pushS(0);
			m_pusher.checkOptionalValue();
		} else if (isStackTop(lValueInfo.expressions[i])) {
			if (!isLast || withExpandLastValue) {
				// stack
				m_pusher << "UNTUPLE 2"; // value stack
				m_pusher.blockSwap(1, 1); // stack value
			}
		} else {
			solUnimplemented("");
		}
	}
	lValueInfo.stackSizeDiff = m_pusher.stackSize() - startStackSize;
	return lValueInfo;
}

void
TVMExpressionCompiler::collectLValue(
	const LValueInfo &lValueInfo,
	const bool haveValueOnStackTop
) const {
	// variable [arrayIndex | mapIndex | structMember | <optional>.get()]...

	const int n = static_cast<int>(lValueInfo.expressions.size());

	for (int i = n - 1; i >= 0; i--) {
		const bool isLast = i + 1 == n;

		if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
			auto& stack = m_pusher.getStack();
			if (stack.isParam(variable->annotation().referencedDeclaration)) {
				solAssert((haveValueOnStackTop && n == 1) || n > 1, "");
				m_pusher.assignStackVariable(variable->annotation().referencedDeclaration);
			} else {
				// value
				auto vd = to<VariableDeclaration>(variable->annotation().referencedDeclaration);
				m_pusher.setGlob(vd);
			}
		} else if (auto indexAccess = to<IndexAccess>(lValueInfo.expressions[i])) {
			if (indexAccess->baseExpression().annotation().type->category() == Type::Category::Mapping) {
				if (isLast && !haveValueOnStackTop) {
					// index dict
					m_pusher.dropUnder(1, 1); // dict
				} else {
					// index dict value
					Type const* keyType = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
					Type const* valueDictType = indexAccess->annotation().type;
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType);
					m_pusher.rotRev(); // value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // dict'
				}
			} else if (indexAccess->baseExpression().annotation().type->category() == Type::Category::TvmVector) {
				if (isLast && !haveValueOnStackTop) {
					solUnimplemented("Impossible case");
				} else {
					// vector index value
					m_pusher.blockSwap(1, 1);
					m_pusher << "SETINDEXVAR";
				}
			} else if (indexAccess->baseExpression().annotation().type->category() == Type::Category::Array) {
				if (isLast && !haveValueOnStackTop) {
					// size index dict
					solUnimplemented("Impossible case");
				} else {
					// size index dict value
					Type const* keyType = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
					auto valueDictType = getType(indexAccess);
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType);
					m_pusher.rotRev(); // size value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // size dict'
				}
				m_pusher << "TUPLE 2";
			} else {
				solUnimplemented("");
			}
		} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
			auto structType = to<StructType>(memberAccess->expression().annotation().type);
			StructCompiler structCompiler{&m_pusher, structType};
			const string &memberName = memberAccess->memberName();
			structCompiler.setMemberForTuple(memberName);
		} else if (isOptionalGet(lValueInfo.expressions[i])) {
			m_pusher.convert(
				lValueInfo.expressions[i - 1]->annotation().type, // optional(T)
				lValueInfo.expressions[i]->annotation().type      // T
			);
		} else if (isStackTop(lValueInfo.expressions[i])) {
			// stack value
			m_pusher.blockSwap(1, 1); // value stack
			m_pusher << "TUPLE 2";
		} else {
			solUnimplemented("");
		}
	}
}


bool TVMExpressionCompiler::tryAssignLValue(Assignment const &_assignment) {
	const auto& lhs = _assignment.leftHandSide();
	const auto& rhs = _assignment.rightHandSide();
	const Token op  = _assignment.assignmentOperator();
	Token binOp = op == Token::Assign ? op : TokenTraits::AssignmentToBinaryOp(op);

	if (op == Token::Assign) {
		auto push_rhs = [&] () {
			compileNewExpr(&rhs);
			m_pusher.convert(getType(&lhs), getType(&rhs));
		};
		const int saveStackSize0 = m_pusher.stackSize();
		push_rhs();
		const int saveStackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = expandLValue(&lhs, false);
		if (isCurrentResultNeeded()) {
			solAssert(saveStackSize - saveStackSize0 == 1, "");
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize);
		} else {
			m_pusher.blockSwap(saveStackSize - saveStackSize0, m_pusher.stackSize() - saveStackSize);
		}
		collectLValue(lValueInfo, true);
	} else {
		Type const*& commonType = lhs.annotation().type;
		compileNewExpr(&rhs); // r
		m_pusher.convert(commonType, rhs.annotation().type);
		const int saveStackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = expandLValue(&lhs, true); // r expanded... l
		m_pusher.convert(commonType, lhs.annotation().type);
		const int expandedLValueSize = m_pusher.stackSize() - saveStackSize - 1;
		m_pusher.blockSwap(1, expandedLValueSize + 1); // expanded... l r

		if (isString(commonType)) {
			m_pusher.pushFragmentInCallRef(2, 1, "__concatenateStrings");
		} else {
			visitMathBinaryOperation(binOp, lhs.annotation().type, rhs.annotation().type, commonType,
									 nullopt, nullptr, nullopt);
		}

		if (isCurrentResultNeeded()) {
			m_pusher.pushS(0); // expanded... res res
			m_pusher.blockSwap(expandedLValueSize + 1, 1); // res expanded... res
		} else {
			// expanded... res
		}
		collectLValue(lValueInfo, true);
	}

	return true;
}

bool TVMExpressionCompiler::tryAssignTuple(Assignment const &_assignment) {
	auto lhs = to<TupleExpression>(&_assignment.leftHandSide());
	Expression const& rhs = _assignment.rightHandSide();
	if (!lhs) {
		return false;
	}

	compileNewExpr(&_assignment.rightHandSide());
	if (lhs->components().size() >= 2) {
		m_pusher.reverse(lhs->components().size(), 0);
	}
	int i = 0;
	for (const auto & leftComp : lhs->components()) {
		if (leftComp == nullptr) {
			m_pusher.drop();
		} else {
			if (auto rightType = to<TupleType>(rhs.annotation().type)) {
				m_pusher.convert(leftComp->annotation().type, rightType->components().at(i));
			} else {
				solAssert(lhs->components().size() == 1, "");
				m_pusher.convert(leftComp->annotation().type, rhs.annotation().type);
			}
			const int stackSizeForValue = m_pusher.stackSize();
			const LValueInfo lValueInfo = expandLValue(leftComp.get(), false);
			const int stackSize = m_pusher.stackSize();
			const int expandLValueSize = stackSize - stackSizeForValue;
			if (expandLValueSize > 0) {
				m_pusher.blockSwap(1, expandLValueSize);
			}
			collectLValue(lValueInfo, true);
		}
		++i;
	}
	return true;
}

void TVMExpressionCompiler::visit2(Assignment const &_assignment) {
	set <Token> compoundAssignment = { Token::AssignShr };
	if (compoundAssignment.count(_assignment.assignmentOperator()) > 0) {
		cast_error(_assignment, "Unsupported operation.");
	}
	if (tryAssignTuple(_assignment) ||
		tryAssignLValue(_assignment))  {
		return;
	}
	cast_error(_assignment, "Unsupported assignment.");
}


bool TVMExpressionCompiler::fold_constants(const Expression *expr) {
	const auto& val = ExprUtils::constValue(*expr);
	if (val.has_value()) {
		m_pusher << "PUSHINT " + val.value().str();
		return true;
	}

	return false;
}


void TVMExpressionCompiler::pushIndexAndConvert(IndexAccess const& indexAccess) {
	Expression const* arg = indexAccess.indexExpression();
	compileNewExpr(arg); // index
	const auto&[keyT, valT] = dictKeyValue(indexAccess.baseExpression().annotation().type);
	boost::ignore_unused(valT);
	m_pusher.convert(keyT, arg->annotation().type);
}




