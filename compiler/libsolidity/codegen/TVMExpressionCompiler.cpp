/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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

#include  <boost/core/ignore_unused.hpp>

#include <liblangutil/SourceReferenceExtractor.h>

#include "DictOperations.hpp"
#include "TVMConstants.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCall.hpp"
#include "TVMStructCompiler.hpp"

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

std::optional<bigint> TVMExpressionCompiler::constValue(const Expression &_e) {
	auto f = [](VariableDeclaration const*  vd) -> std::optional<bigint> {
		if (vd != nullptr && vd->isConstant() && vd->value() != nullptr) {
			return constValue(*vd->value());
		}
		return {};
	};

	if (_e.annotation().isPure) {
		if (auto ident = to<Identifier>(&_e)) {
			IdentifierAnnotation &identifierAnnotation = ident->annotation();
			const auto *vd = to<VariableDeclaration>(identifierAnnotation.referencedDeclaration);
			return f(vd);
		} else if (_e.annotation().type->category() == Type::Category::RationalNumber) {
			auto number = dynamic_cast<RationalNumberType const *>(_e.annotation().type);
			solAssert(number, "");
			bigint val = number->value();
			return val;
		}
	} else {
		// MyLibName.const_variable
		auto memberAccess = to<MemberAccess>(&_e);
		if (memberAccess) {
			auto identifier = to<Identifier>(&memberAccess->expression());
			if (identifier && identifier->annotation().type->category() == Type::Category::TypeType) {
				auto vd = to<VariableDeclaration>(memberAccess->annotation().referencedDeclaration);
				return f(vd);
			}
		}
	}
	return {};
}

std::optional<bool> TVMExpressionCompiler::constBool(Expression const& _e) {
	auto l = to<Literal>(&_e);
	if (l != nullptr && isIn(l->token(), Token::TrueLiteral, Token::FalseLiteral)) {
		return l->token() == Token::TrueLiteral;
	}
	return {};
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
			m_pusher.push(+1, _node.token() == Token::TrueLiteral? "TRUE" : "FALSE");
			break;
		case Type::Category::Null:
			m_pusher.push(+1, "NULL");
			break;
		case Type::Category::EmpyMap:
			m_pusher.push(+1, "NEWDICT");
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
		if (_tupleExpression.annotation().isPure) {
			const int stackSize = m_pusher.stackSize();
			SourceReference sr = SourceReferenceExtractor::extract(&_tupleExpression.location());
			const std::string computeName = "inline_array_line_" +
					toString(sr.position.line) + "_column_" + toString(sr.position.column) + "_ast_id_" +
					toString(_tupleExpression.id());

			m_pusher << "PUSHINT " + toString(n);
			m_pusher.compureConstCell(computeName);
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
	const IntegerType arrayKeyType = getKeyTypeOfArray();

	for (ASTPointer<Expression> const& expr : components | boost::adaptors::reversed) {
		compileNewExpr(expr.get());
	}
	// values...
	m_pusher.pushInt(0);
	// values... index
	m_pusher.push(+1, "NEWDICT");
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
	m_pusher.push(0, "INC");
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
		m_pusher.push(-2 + 1, "TUPLE 2");
	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
}

bool TVMExpressionCompiler::tryPushConstant(Identifier const &_identifier) {
	IdentifierAnnotation& identifierAnnotation = _identifier.annotation();
	Declaration const* declaration = identifierAnnotation.referencedDeclaration;
	const auto* variableDeclaration = to<VariableDeclaration>(declaration);
	if (!variableDeclaration || !variableDeclaration->isConstant()){
		return false;
	}
	compileNewExpr(variableDeclaration->value().get());
	return true;
}

bool TVMExpressionCompiler::pushLocalOrStateVariable(Identifier const &_identifier) {
	auto& stack = m_pusher.getStack();
	if (stack.isParam(_identifier.annotation().referencedDeclaration)) {
		auto offset = stack.getOffset(_identifier.annotation().referencedDeclaration);
		m_pusher.pushS(offset);
		return true;
	} else if (tryPushConstant(_identifier)) {
		return true;
	} else if (auto variable = to<VariableDeclaration>(_identifier.annotation().referencedDeclaration)) {
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
		m_pusher.push(+1, "NOW");
	} else if (name == "this") {
		// calling this.function() will create internal message that should be sent to the address of current contract
		m_pusher.push(+1, "MYADDR");
	} else if (to<FunctionDefinition>(_identifier.annotation().referencedDeclaration)) {
		m_pusher.push(+1,"PUSHINT $" + name + "_internal$");
	} else {
		cast_error(_identifier, "Unsupported identifier: " + name);
	}
}

void TVMExpressionCompiler::compileUnaryOperation(
	UnaryOperation const &_node,
	const std::string &tvmUnaryOperation,
    const bool isPrefixOperation
) {
	const int saveStackSize = m_pusher.stackSize();
	Type const* resType = _node.annotation().type;
	LValueInfo lValueInfo;
	if (isCurrentResultNeeded()) {
		lValueInfo = expandLValue(&_node.subExpression(), true, true);
		const int expandedLValueSize = m_pusher.stackSize() - saveStackSize - 1;
		solAssert(expandedLValueSize >= 0, "");
		if (isPrefixOperation) {
			m_pusher.push(0, tvmUnaryOperation);
			m_pusher.pushS(0); // expanded.. value value
			if (expandedLValueSize != 0) {
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. value
			}
		} else {
			m_pusher.pushS(0); // expanded.. value value
			m_pusher.push(0, tvmUnaryOperation); // expanded.. value newValue
			if (expandedLValueSize != 0) {
				m_pusher.exchange(1); // expanded.. newValue value
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. newValue
			}
		}
	} else {
		lValueInfo = expandLValue(&_node.subExpression(), true, true);
		m_pusher.push(0, tvmUnaryOperation);
	}

	if (!isCheckFitUseless(resType, _node.getOperator()) && !m_pusher.ctx().ignoreIntegerOverflow()) {
		m_pusher.checkFit(resType);
	}
	collectLValue(lValueInfo, true, false);
}

void TVMExpressionCompiler::compileUnaryDelete(UnaryOperation const &node) {
	const LValueInfo lValueInfo = expandLValue(&node.subExpression(), false, true);
	Expression const* lastExpr = lValueInfo.expressions.back();
	Type const* exprType = node.subExpression().annotation().type;
	if (to<Identifier>(lastExpr)) {
		m_pusher.pushDefaultValue(exprType);
		collectLValue(lValueInfo, true, false);
	} else if (auto memberAccess = to<MemberAccess>(lastExpr)) {
		MemberAccessAnnotation& a = memberAccess->annotation();
		auto decl = to<VariableDeclaration>(a.referencedDeclaration);
		m_pusher.pushDefaultValue(decl->type());
		collectLValue(lValueInfo, true, false);
	} else if (auto indexAccess = to<IndexAccess>(lastExpr)) {
		// ... index dict
		Type const* baseExprType = indexAccess->baseExpression().annotation().type;
		auto arrayType = to<ArrayType>(baseExprType);
		if (arrayType) {
			Type const* valueType = arrayType->baseType();
			m_pusher.pushDefaultValue(valueType); // index dict value
			collectLValue(lValueInfo, true, false);
		} else { // mapping
			m_pusher.pushS(1);                            // ... index dict index
            TypePointer const dictKey = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
			m_pusher.exchange(1);                                // ... index index' dict
			m_pusher.pushInt(lengthOfDictKey(dictKey)); // ..index index dict nbits
			m_pusher.push(-3 + 2, "DICT" + typeToDictChar(dictKey) + "DEL");  // ... index dict' {-1,0}
			m_pusher.drop();                               // ... index dict'
			collectLValue(lValueInfo, false, false); // lValueInfo.isValueBuilder is ignored
		}
	} else {
		solUnimplemented("");
	}
}

void TVMExpressionCompiler::visit2(UnaryOperation const &_node) {
	auto op = _node.getOperator();
	if (op == Token::Inc) {
		compileUnaryOperation(_node, "INC", _node.isPrefixOperation());
	} else if (op == Token::Dec) {
		compileUnaryOperation(_node, "DEC", _node.isPrefixOperation());
	} else if (op == Token::Not) {
		compileNewExpr(&_node.subExpression());
		m_pusher.push(0, "NOT");
	} else if (op == Token::Sub) {
		compileNewExpr(&_node.subExpression());
		m_pusher.push(0, "NEGATE");
	} else if (op == Token::BitNot) {
		compileNewExpr(&_node.subExpression());
		int numBits = 0;
		bool isSigned {};
		auto type = getType(&_node.subExpression());
		TypeInfo ti{type};
		if (ti.isNumeric) {
			numBits = ti.numBits;
			isSigned = ti.isSigned;
		} else {
			cast_error(_node, "~ operation is supported only for numbers.");
		}
		if (isSigned)
			m_pusher.push(0, "BITNOT");
		else {
			m_pusher.push(+1,"PUSHPOW2DEC " + to_string(numBits));
			m_pusher.push(-2+1, "SUBR");
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
			m_pusher.push(-2 + 1, "SDLEXCMP");
			m_pusher.push(0, "ISPOS");
			break;
		case Token::GreaterThanOrEqual:
			m_pusher.push(-2 + 1, "SDLEXCMP");
			m_pusher.push(0, "ISNNEG");
			break;
		case Token::LessThan:
			m_pusher.push(-2 + 1, "SDLEXCMP");
			m_pusher.push(0, "ISNEG");
			break;
		case Token::LessThanOrEqual:
			m_pusher.push(-2 + 1, "SDLEXCMP");
			m_pusher.push(0, "ISNPOS");
			break;
		case Token::Equal:
			m_pusher.push(-2 + 1, "SDEQ");
			break;
		case  Token::NotEqual:
			m_pusher.push(-2 + 1, "SDEQ");
			m_pusher.push(0, "NOT");
			break;
		default:
			solUnimplemented("Wrong compare operation");
	}
}

void TVMExpressionCompiler::compareStrings(Token op) {
	m_pusher.pushMacroCallInCallRef(2, 1, "compareLongStrings_macro");
	switch(op) {
		case Token::GreaterThan:
			m_pusher.push(0, "ISPOS");
			break;
		case Token::GreaterThanOrEqual:
			m_pusher.push(0, "ISNNEG");
			break;
		case Token::LessThan:
			m_pusher.push(0, "ISNEG");
			break;
		case Token::LessThanOrEqual:
			m_pusher.push(0, "ISNPOS");
			break;
		case Token::Equal:
			m_pusher.push(0, "ISZERO");
			break;
		case  Token::NotEqual:
			m_pusher.push(0, "ISZERO");
			m_pusher.push(0, "NOT");
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
		m_pusher.pushMacroCallInCallRef(2, 1, "concatenateStrings_macro");
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
		m_pusher.push(-1 + 1, "HASHCU");
		pushRight();
		m_pusher.push(-1 + 1, "HASHCU");
		if (op == Token::Equal)
			m_pusher.push(-2 + 1, "EQUAL");
		else
			m_pusher.push(-2 + 1, "NEQ");
	} else {
		solUnimplemented("Unsupported binary operation");
	}
}

void TVMExpressionCompiler::visitLogicalShortCircuiting(BinaryOperation const &_binaryOperation) {
	const Token op = _binaryOperation.getOperator();

	std::vector<Expression const *> order = unroll(_binaryOperation);
	compileNewExpr(order[0]);
	for (int i = 1; i < static_cast<int>(order.size()); ++i) {
		m_pusher.pushS(0);
		m_pusher.push(-1, ""); // fix stack
		m_pusher.startContinuation();
		m_pusher.drop();
		compileNewExpr(order[i]);
	}

	for (int i = static_cast<int>(order.size()) - 1; i >= 1; --i) {
		m_pusher.endLogCircuit(op == Token::Or ? LogCircuit::Type::OR : LogCircuit::Type::AND);
	}
}

void TVMExpressionCompiler::visit2(BinaryOperation const &_binaryOperation) {
	const Token op = _binaryOperation.getOperator();
	const auto &lexp = _binaryOperation.leftExpression();
	const auto &rexp = _binaryOperation.rightExpression();
	Type const *lt = getType(&lexp);
	Type const *rt = getType(&rexp);
	TypePointer const &commonType = _binaryOperation.annotation().commonType;
	TypePointer leftTargetType = commonType;
	TypePointer rightTargetType = TokenTraits::isShiftOp(op) ? rexp.annotation().type->mobileType() : commonType;

	if (lt->category() == Type::Category::Function || rt->category() == Type::Category::Function) {
		solUnimplemented("Unsupported binary operation");
	}

	auto acceptLeft = [&]() {
		compileNewExpr(&lexp);
		m_pusher.hardConvert(leftTargetType, lt);
	};

	auto acceptRight = [&]() {
		compileNewExpr(&rexp);
		m_pusher.hardConvert(rightTargetType, rt);
	};

	if (isString(commonType)) {
		visitBinaryOperationForString(acceptLeft, acceptRight, op);
		return;
	}

	if ((lt->category() == Type::Category::TvmCell &&
		 rt->category() == Type::Category::TvmCell) ||
		(isByteArrayOrString(lt) && isByteArrayOrString(rt))) {
		visitBinaryOperationForTvmCell(acceptLeft, acceptRight, op);
		return;
	}

	if (isAddressOrContractType(lt) || isAddressOrContractType(rt) || (isSlice(lt) && isSlice(rt))) {
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

	const auto& val = constValue(_binaryOperation.rightExpression());
	std::optional<bigint> rightValue;
	if (val.has_value())
		rightValue = val;
	visitMathBinaryOperation(op, commonType, acceptRight, rightValue);
}

bool TVMExpressionCompiler::isCheckFitUseless(Type const* commonType, Token op) {
	auto intResult = to<IntegerType>(commonType);
	return
		intResult &&
		!intResult->isSigned() &&
		intResult->numBits() == 256 &&
		isIn(op, Token::Add, Token::Exp, Token::Mul, Token::SHL, Token::Inc);

}

// if pushRight is set we haven't value on stack
// else right value is on stack
void TVMExpressionCompiler::visitMathBinaryOperation(
	const Token op,
	Type const* commonType,
	const std::function<void()>& pushRight,
	const std::optional<bigint>& rightValue
) {
	bool checkOverflow = false;
	if (op == Token::Exp) {
		if (rightValue.has_value() && (*rightValue == 2 || *rightValue == 3 || *rightValue == 4)) {
			if (*rightValue == 2) {
				m_pusher.pushS(0);
				m_pusher.push(-2 + 1, "MUL");
			} else if (*rightValue == 3) {
				m_pusher.pushS(0);
				m_pusher.pushS(0);
				m_pusher.push(-2 + 1, "MUL");
				m_pusher.push(-2 + 1, "MUL");
			} else if (*rightValue == 4) {
				m_pusher.pushS(0);
				m_pusher.push(-2 + 1, "MUL");
				m_pusher.pushS(0);
				m_pusher.push(-2 + 1, "MUL");
			} else {
				solUnimplemented("");
			}
		} else {
			pushRight();
			m_pusher.dup2();
			m_pusher.push(-2 + 1, "OR");
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::Exponent00));
			m_pusher.pushMacroCallInCallRef(2, 1, "__exp_macro");
		}
		checkOverflow = true;
	} else {
		if (pushRight) {
			pushRight();
		}
		if (op == Token::Add) {
			m_pusher.push(-1, "ADD");
			checkOverflow = true;
		} else if (op == Token::Mul) {
			if (commonType->category() == Type::Category::FixedPoint) {
				int power = to<FixedPointType>(commonType)->fractionalDigits();
				m_pusher.pushInt(StackPusher::pow10(power));
				m_pusher.push(-3 + 1, "MULDIV");
			} else {
				m_pusher.push(-2 + 1, "MUL");
			}
			checkOverflow = true;
		} else if (op == Token::Sub) {
			m_pusher.push(-1, "SUB");
			checkOverflow = true;
		}
		else if (op == Token::Mod) { m_pusher.push(-1, "MOD"); }
		else if (op == Token::Div) {
			if (commonType->category() == Type::Category::FixedPoint) {
				int power = to<FixedPointType>(commonType)->fractionalDigits();
				m_pusher.pushInt(StackPusher::pow10(power)); // res 10^n
				m_pusher.exchange(1);
				m_pusher.push(-3 + 1, "MULDIV");
			} else {
				m_pusher.push(-1, "DIV");
			}
		}
		else if (op == Token::GreaterThan) m_pusher.push(-1, "GREATER");
		else if (op == Token::GreaterThanOrEqual) m_pusher.push(-1, "GEQ");
		else if (op == Token::LessThan) m_pusher.push(-1, "LESS");
		else if (op == Token::LessThanOrEqual) m_pusher.push(-1, "LEQ");
		else if (op == Token::Equal) m_pusher.push(-1, "EQUAL");
		else if (op == Token::NotEqual) m_pusher.push(-1, "NEQ");
		else if (op == Token::BitAnd) m_pusher.push(-1, "AND");
		else if (op == Token::BitOr) m_pusher.push(-1, "OR");
		else if (op == Token::SHL) {
			m_pusher.push(-1, "LSHIFT");
			checkOverflow = true;
		}
		else if (op == Token::SAR) m_pusher.push(-1, "RSHIFT");
		else if (op == Token::BitXor) m_pusher.push(-1, "XOR");
		else {
			solUnimplemented("Unsupported binary operation");
		}
	}

	if (checkOverflow && !m_pusher.ctx().ignoreIntegerOverflow()) {
		if (!isCheckFitUseless(commonType, op)) {
			m_pusher.checkFit(commonType);
		}
	}
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
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -2",
			"PICK",
		}, 0, 1, true));
	} else  if (isIn(_node.memberName(), "isInternal", "isExternal", "isTickTock")) {
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -5",
			"PICK",
		}, 0, 1, true));
		if (_node.memberName() == "isInternal") {
			m_pusher.push(-1 + 1, "EQINT 0");
		} else if (_node.memberName() == "isExternal") {
			m_pusher.push(-1 + 1, "EQINT -1");
		} else if (_node.memberName() == "isTickTock") {
			m_pusher.push(-1 + 1, "EQINT -2");
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
			"	NEWDICT",
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
	} else if (_node.memberName() == "data") {
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -4",
			"PICK",
		}, 0, 1, true));
	} else {
		cast_error(_node, "Unsupported magic");
	}
}

void TVMExpressionCompiler::visitMagic(MemberAccess const &_node) {
	auto unsupportedMagic = [&](){
		cast_error(_node, "Unsupported magic");
	};

	auto identifier = to<Identifier>(&_node.expression());
	if (identifier == nullptr) {
		unsupportedMagic();
	}
	if (identifier->name() == "msg") {
		visitMsgMagic(_node);
	} else if (identifier->name() == "tx") {
		if (_node.memberName() == "timestamp") {
			m_pusher.push(+1, "LTIME");
		} else {
			unsupportedMagic();
		}
	} else if (identifier->name() == "block") {
		if (_node.memberName() == "timestamp") {
			m_pusher.push(+1, "BLOCKLT");
		} else {
			unsupportedMagic();
		}
	} else {
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
	if (checkForAddressMemberAccess(_node, category)) {
		return;
	}

	if (category == Type::Category::Array)
		return visitMemberAccessArray(_node);

	if (category == Type::Category::FixedBytes)
		return visitMemberAccessFixedBytes(_node, to<FixedBytesType>(getType(&_node.expression())));

	if (category == Type::Category::TypeType) {
		auto typeType = to<TypeType>(_node.expression().annotation().type);
		if (auto enumType = dynamic_cast<EnumType const *>(typeType->actualType())) {
			unsigned int value = enumType->memberValue(_node.memberName());
			m_pusher.push(+1, "PUSHINT " + toString(value));
			return;
		}

		if (fold_constants(&_node)) {
			return;
		}
	}

	cast_error(_node, "Not supported");
}

bool TVMExpressionCompiler::checkForAddressMemberAccess(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::Address)
		return false;
	if (_node.memberName() == "balance") {
		if (!isAddressThis(to<FunctionCall>(&_node.expression()))) {
			cast_error(_node.expression(), "Only 'address(this).balance' is supported for member balance");
		}
		m_pusher.push(+1, "GETPARAM 7");
		m_pusher.indexNoexcep(0);
		return true;
	}
	if (_node.memberName() == "currencies") {
		if (!isAddressThis(to<FunctionCall>(&_node.expression()))) {
			cast_error(_node.expression(), "Only 'address(this).currencies' is supported for member currencies");
		}
		m_pusher.push(+1, "GETPARAM 7");
		m_pusher.indexNoexcep(1);
		return true;
	}
	if (_node.memberName() == "wid") {
		compileNewExpr(&_node.expression());
		m_pusher.push(-1 + 1, "PARSEMSGADDR");
		m_pusher.indexWithExcep(2);
		return true;
	}
	if (_node.memberName() == "value") {
		compileNewExpr(&_node.expression());
		m_pusher.push(-1 + 1, "PARSEMSGADDR");
		m_pusher.indexWithExcep(3);
		m_pusher.push(0, "PLDU 256");
		return true;
	}
	return false;
}

void TVMExpressionCompiler::visitMemberAccessArray(MemberAccess const &_node) {
	auto arrayType = to<ArrayType>(_node.expression().annotation().type);
	if (_node.memberName() == "length") {
		compileNewExpr(&_node.expression());
		if (arrayType->isByteArray()) {
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
	if (!isIn(baseExprCategory, Type::Category::Mapping, Type::Category::ExtraCurrencyCollection, Type::Category::TvmVector) && !isUsualArray(baseExprType)) {
		cast_error(_node, "Index access is supported only for dynamic arrays, tuples and mappings");
	}
}

void TVMExpressionCompiler::visit2(IndexRangeAccess const &indexRangeAccess) {
	Type const *baseType = indexRangeAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArray()) {
			acceptExpr(&indexRangeAccess.baseExpression()); // bytes
			if (indexRangeAccess.startExpression()) {
				compileNewExpr(indexRangeAccess.startExpression());
			}
			else {
				m_pusher.pushInt(0);
			}
			if (indexRangeAccess.endExpression()) {
				compileNewExpr(indexRangeAccess.endExpression());
				m_pusher.push(+1, "FALSE");
			} else {
				m_pusher.pushInt(0);
				m_pusher.push(+1, "TRUE");
			}

			m_pusher.pushMacroCallInCallRef(4, 1, "bytes_substr");
			return;
		}
	}
	solUnimplemented("Wrong range expression");
}

void TVMExpressionCompiler::visit2(IndexAccess const &indexAccess) {
	Type const *baseType = indexAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArray()) {
			acceptExpr(&indexAccess.baseExpression()); // bytes
			m_pusher.push(-1 + 1, "CTOS");
			compileNewExpr(indexAccess.indexExpression()); // slice index
			m_pusher.startContinuation();
			m_pusher.pushInt(127);
			m_pusher.push(-2 + 2, "DIVMOD"); // slice cntRef rest
			m_pusher.rotRev(); // rest slice cntRef
			m_pusher.startContinuation();
			m_pusher.push(0, "PLDREF");
			m_pusher.push(0, "CTOS");
			m_pusher.endContinuation();
			m_pusher.repeat(false);
			m_pusher.push(-1, ""); // fix stack
			// rest slice
			m_pusher.exchange(1); // slice rest
			m_pusher.push(-1 + 1, "MULCONST 8");
			m_pusher.push(-2 + 1, "SDSKIPFIRST");
			m_pusher.push(-1 + 1, "PLDU 8");
			m_pusher.pushRefContAndCallX(2, 1, false);
			return;
		} else {
			compileNewExpr(indexAccess.indexExpression()); // index
			acceptExpr(&indexAccess.baseExpression()); // index array
			m_pusher.indexNoexcep(1); // index dict
		}
	} else if (baseType->category() == Type::Category::TvmVector) {
		acceptExpr(&indexAccess.baseExpression()); // tuple
		const auto& val = constValue(*indexAccess.indexExpression());
		if (val.has_value() && val <= 15) {
			m_pusher.indexWithExcep(0);
			m_pusher.indexWithExcep(boost::lexical_cast<int>(val.value().str()));
		} else {
			acceptExpr(indexAccess.indexExpression()); // vector index
			m_pusher.pushInt(TvmConst::TvmTupleLen);
			m_pusher.push(-2+2, "DIVMOD");
			// vector tuple_num rest
			m_pusher.rotRev();
			// rest vector tuple_num
			m_pusher.push(-2 + 1, "INDEXVAR");
			// rest tuple
			m_pusher.exchange(1);
			m_pusher.push(-2 + 1, "INDEXVAR");
		}
		return;
	} else if (baseType->category() == Type::Category::FixedBytes) {
		acceptExpr(&indexAccess.baseExpression()); // integer
		auto fbt = to<FixedBytesType>(getType(&indexAccess.baseExpression()));
		m_pusher.pushInt(static_cast<int>(fbt->storageBytes()) - 1);
		acceptExpr(indexAccess.indexExpression()); // integer byte_len byte_index
		m_pusher.push(-1, "SUB");
		m_pusher.push(0, "MULCONST 8");
		m_pusher.push(-1, "RSHIFT");
		m_pusher.push(0, "MODPOW2 8");
		return;
	}	else {
		pushIndexAndConvert(indexAccess); // index
		m_pusher.prepareKeyForDictOperations(indexAccess.indexExpression()->annotation().type, false);
		acceptExpr(&indexAccess.baseExpression()); // index dict
	}

	m_pusher.getDict(*StackPusher::parseIndexType(baseType),
					 *StackPusher::parseValueType(indexAccess),
					 baseType->category() == Type::Category::Mapping ||
					 baseType->category() == Type::Category::ExtraCurrencyCollection ?
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
	m_pusher.push(-1, ""); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.trueExpression());
	m_pusher.endContinuation();
	m_pusher.push(-paramQty, ""); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.falseExpression());
	m_pusher.endContinuation();
	m_pusher.push(-paramQty, ""); // fix stack


	m_pusher.pushConditional(paramQty);
	solAssert(stackSize + paramQty == m_pusher.stackSize(), "");
}

bool TVMExpressionCompiler::isOptionalGet(Expression const* expr) {
	auto funCall = to<FunctionCall>(expr);
	if (!funCall) {
		return false;
	}
	auto ma = to<MemberAccess>(&funCall->expression());
	return ma && ma->expression().annotation().type->category() == Type::Category::Optional;
}

LValueInfo
TVMExpressionCompiler::expandLValue(
	Expression const *const _expr,
	const bool withExpandLastValue,
	bool isLValue
) {
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
		} else if (
			auto funCall = to<FunctionCall>(expr);
			funCall && !isLValue
		) {
			compileNewExpr(_expr);
			lValueInfo.doesntNeedToCollect = true;
			return lValueInfo;
		} else if (isOptionalGet(expr)) {
			auto ma = to<MemberAccess>(&funCall->expression());
			expr = &ma->expression();
		} else {
			cast_error(*expr, "Unsupported lvalue.");
		}
	}
	std::reverse(lValueInfo.expressions.begin(), lValueInfo.expressions.end());

	for (int i = 0; i < static_cast<int>(lValueInfo.expressions.size()); i++) {
		bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());
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
			if (isIn(index->baseExpression().annotation().type->category(), Type::Category::Mapping, Type::Category::ExtraCurrencyCollection)) {
				// dict1
				pushIndexAndConvert(*index); // dict1 index
                m_pusher.prepareKeyForDictOperations(index->indexExpression()->annotation().type, false);
				// dict1 index
				m_pusher.exchange(1);
				// index dict1
				if (isLast && !withExpandLastValue) break;
				m_pusher.pushS2(1, 0);
				// index dict1 index dict1

				m_pusher.getDict(*StackPusher::parseIndexType(index->baseExpression().annotation().type),
				                 *StackPusher::parseValueType(*index),
				                 GetDictOperation::GetFromMapping);
				// index dict1 dict2
			} else if (index->baseExpression().annotation().type->category() == Type::Category::Array) {
				// array
				m_pusher.push(-1 + 2, "UNTUPLE 2"); // size dict
				compileNewExpr(index->indexExpression()); // size dict index
				m_pusher.exchange(1); // size index dict
				m_pusher.pushS2(1, 2); // size index dict index size
				m_pusher.push(-2 + 1, "LESS"); // size index dict index<size
				m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				if (isLast && !withExpandLastValue) {
					break;
				}
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
			if (!isLast || withExpandLastValue) {
				m_pusher.pushS(0);
			}
			m_pusher.checkOptionalValue();
		} else {
			solUnimplemented("");
		}
	}
	return lValueInfo;
}

void
TVMExpressionCompiler::collectLValue(
	const LValueInfo &lValueInfo,
	const bool haveValueOnStackTop,
	bool /*isValueBuilder*/
)
{
	// variable [arrayIndex | mapIndex | structMember | <optional>.get()]...

	if (lValueInfo.doesntNeedToCollect) {
		solAssert(haveValueOnStackTop, "Collect with not needed value is available only for on stack top values.");
		m_pusher.drop();
		return;
	}

	const int n = static_cast<int>(lValueInfo.expressions.size());

	for (int i = n - 1; i >= 0; i--) {
		const bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());

		if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
//				pushLog("colVar");
			auto& stack = m_pusher.getStack();
			if (stack.isParam(variable->annotation().referencedDeclaration)) {
				solAssert((haveValueOnStackTop && n == 1) || n > 1, "");
				m_pusher.tryAssignParam(variable->annotation().referencedDeclaration);
			} else {
				// value
				auto vd = to<VariableDeclaration>(variable->annotation().referencedDeclaration);
				m_pusher.setGlob(vd);
			}
		} else if (auto indexAccess = to<IndexAccess>(lValueInfo.expressions[i])) {
			if (isIn(indexAccess->baseExpression().annotation().type->category(), Type::Category::Mapping, Type::Category::ExtraCurrencyCollection)) {
				//					pushLog("colMapIndex");
				if (isLast && !haveValueOnStackTop) {
					// index dict
					m_pusher.dropUnder(1, 1); // dict
				} else {
					// index dict value
					TypePointer const keyType = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
					TypePointer const valueDictType = StackPusher::parseValueType(*indexAccess);
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType);
					m_pusher.rotRev(); // value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // dict'
				}
			} else if (indexAccess->baseExpression().annotation().type->category() == Type::Category::Array) {
				//					pushLog("colArrIndex");
				if (isLast && !haveValueOnStackTop) {
					// size index dict
					m_pusher.popS(1); // size dict
				} else {
					// size index dict value
					TypePointer const keyType = StackPusher::parseIndexType(indexAccess->baseExpression().annotation().type);
					auto valueDictType = getType(indexAccess);
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType);
					m_pusher.rotRev(); // size value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // size dict'
				}

				m_pusher.push(-2 + 1, "TUPLE 2");
			} else {
				solUnimplemented("");
			}
		} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
//				pushLog("colStruct");
			auto structType = to<StructType>(memberAccess->expression().annotation().type);
			StructCompiler structCompiler{&m_pusher, structType};
			const string &memberName = memberAccess->memberName();
			structCompiler.setMemberForTuple(memberName);
		} else if (isOptionalGet(lValueInfo.expressions[i])) {
			// do nothing
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
			m_pusher.hardConvert(getType(&lhs), getType(&rhs));
			return false;
		};
		const int saveStackSize0 = m_pusher.stackSize();
		bool valueIsBuilder = push_rhs();
		const int saveStackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = expandLValue(&lhs, false, true);
		if (isCurrentResultNeeded()) {
			solAssert(saveStackSize - saveStackSize0 == 1, "");
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize);
		} else {
			m_pusher.blockSwap(saveStackSize - saveStackSize0, m_pusher.stackSize() - saveStackSize);
		}
		collectLValue(lValueInfo, true, valueIsBuilder);
	} else {
		TypePointer const& commonType = lhs.annotation().type;
		compileNewExpr(&rhs); // r
		m_pusher.hardConvert(commonType, rhs.annotation().type);
		const int saveStackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = expandLValue(&lhs, true, true); // r expanded... l
		m_pusher.hardConvert(commonType, lhs.annotation().type);
		const int expandedLValueSize = m_pusher.stackSize() - saveStackSize - 1;
		m_pusher.blockSwap(1, expandedLValueSize + 1); // expanded... l r

		if (isString(commonType)) {
			m_pusher.pushMacroCallInCallRef(2, 1, "concatenateStrings_macro");
		} else {
			visitMathBinaryOperation(binOp, commonType, nullptr, nullopt);
		}

		if (isCurrentResultNeeded()) {
			m_pusher.pushS(0); // expanded... res res
			m_pusher.blockSwap(expandedLValueSize + 1, 1); // res expanded... res
		} else {
			// expanded... res
		}
		collectLValue(lValueInfo, true, false);
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
				m_pusher.hardConvert(leftComp->annotation().type, rightType->components().at(i));
			} else {
				solAssert(lhs->components().size() == 1, "");
				m_pusher.hardConvert(leftComp->annotation().type, rhs.annotation().type);
			}
			const int stackSizeForValue = m_pusher.stackSize();
			const LValueInfo lValueInfo = expandLValue(leftComp.get(), false, true);
			const int stackSize = m_pusher.stackSize();
			const int expandLValueSize = stackSize - stackSizeForValue;
			if (expandLValueSize > 0) {
				m_pusher.blockSwap(1, expandLValueSize);
			}
			collectLValue(lValueInfo, true, false);
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
	const auto& val = constValue(*expr);
	if (val.has_value()) {
		m_pusher.push(+1, "PUSHINT " + val.value().str());
		return true;
	}

	return false;
}


void TVMExpressionCompiler::pushIndexAndConvert(IndexAccess const& indexAccess) {
	Expression const* arg = indexAccess.indexExpression();
	compileNewExpr(arg); // index
	const auto&[keyT, valT] = dictKeyValue(indexAccess.baseExpression().annotation().type);
	boost::ignore_unused(valT);
	m_pusher.hardConvert(keyT, arg->annotation().type);
}




