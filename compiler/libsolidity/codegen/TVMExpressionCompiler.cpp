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


#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCall.hpp"
#include "TVMIntrinsics.hpp"
#include "TVMStructCompiler.hpp"

using namespace solidity::frontend;

TVMExpressionCompiler::TVMExpressionCompiler(StackPusherHelper &pusher) :
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
		doDropResultIfNeeded = visit2(*e7);
	} else if (auto e8 = to<Conditional>(expr)) {
		visit2(*e8);
	} else if (auto e9 = to<ElementaryTypeNameExpression>(expr)) {
		visit2(*e9);
	} else {
		cast_error(*expr, string("Unsupported expression ") + typeid(expr).name());
	}

	--m_expressionDepth;
	solAssert(savedExpressionDepth == m_expressionDepth,
	          "Internal error: depth exp " + toString(savedExpressionDepth) + " got " + toString(m_expressionDepth));
	return doDropResultIfNeeded;
}

std::pair<bool, bigint> TVMExpressionCompiler::constValue(const Expression &_e) {
	auto f = [](VariableDeclaration const*  vd) -> std::pair<bool, bigint> {
		if (vd != nullptr && vd->isConstant() && vd->value() != nullptr) {
			return constValue(*vd->value());
		}
		return {false, 0};
	};

	if (_e.annotation().isPure) {
		if (auto ident = to<Identifier>(&_e)) {
			IdentifierAnnotation &identifierAnnotation = ident->annotation();
			const auto *vd = to<VariableDeclaration>(identifierAnnotation.referencedDeclaration);
			return f(vd);
		} else if (_e.annotation().type->category() == Type::Category::RationalNumber) {
			auto number = dynamic_cast<RationalNumberType const *>(_e.annotation().type);
			solAssert(number, "");
			solAssert(!number->isFractional(), "");
			bigint val = number->numerator();
			solAssert(!number->isFractional(), "");
			return {true, val};
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
	return {false, 0};
}

std::pair<bool, bool> TVMExpressionCompiler::constBool(Expression const& _e) {
	auto l = to<Literal>(&_e);
	if (l != nullptr && isIn(l->token(), Token::TrueLiteral, Token::FalseLiteral)) {
		return {true, l->token() == Token::TrueLiteral};
	}
	return {false, false};
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

void TVMExpressionCompiler::visitStringLiteralAbiV1(Literal const &_node) {
	const std::string &str = _node.value();
	const int size = str.size();
	const int saveStackSize = m_pusher.getStack().size();
	if (size == 0) {
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "ENDC");
	} else {
		const int step = TvmConst::CellBitLength / 8; // 127
		int builderQty = 0;
		int cntBlock = (size + step - 1) / step;
		for (int start = size - cntBlock * step; start < size; start += step, ++builderQty) {
			std::string slice;
			for (int index = max(0, start); index < start + step; ++index) {
				std::stringstream ss;
				ss << std::hex << std::setfill('0') << std::setw(2) << (static_cast<unsigned>(str.at(index)) & 0xFF);
				slice += ss.str();
			}
			if (slice.size() > 124) { // there is bug in linker or in spec.
				m_pusher.push(+1, "NEWC");
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(0, 2 * (127 - 124)));
				m_pusher.push(-1, "STSLICER");
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(2 * (127 - 124)));
				m_pusher.push(-1, "STSLICER");
			} else {
				m_pusher.push(+1, "PUSHSLICE x" + slice);
				m_pusher.push(+1, "NEWC");
				m_pusher.push(-1, "STSLICE");
			}
		}
		--builderQty;
		while (builderQty --> 0) {
			m_pusher.push(-1, "STBREFR");
		}
		m_pusher.push(0, "ENDC");
	}
	m_pusher.getStack().ensureSize(saveStackSize + 1, "");
}

void TVMExpressionCompiler::visitStringLiteralAbiV2(Literal const &_node) {
	const std::string &str = _node.value();
	const int stringLiteralLength = str.size();
	const int saveStackSize = m_pusher.getStack().size();
	if (stringLiteralLength == 0) {
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "ENDC");
	} else {
		const int bytesInCell = TvmConst::CellBitLength / 8; // 127
		int builderQty = 0;
		for (int start = 0; start < stringLiteralLength; start += bytesInCell, ++builderQty) {
			std::string slice;
			for (int index = start; index < std::min(start + bytesInCell, stringLiteralLength); ++index) {
				std::stringstream ss;
				ss << std::hex << std::setfill('0') << std::setw(2)
					<< (static_cast<unsigned>(str.at(index)) & 0xFF);
				slice += ss.str();
			}
			if (slice.size() > TvmConst::MaxPushSliceLength) {
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(TvmConst::MaxPushSliceLength));
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(0, TvmConst::MaxPushSliceLength));
				m_pusher.push(+1, "NEWC");
				m_pusher.push(-1, "STSLICE");
				m_pusher.push(-1, "STSLICE");
			} else {
				m_pusher.push(+1, "PUSHSLICE x" + slice);
				m_pusher.push(+1, "NEWC");
				m_pusher.push(-1, "STSLICE");
			}
		}
		--builderQty;
		while (builderQty --> 0) {
			m_pusher.push(-1, "STBREFR");
		}
		m_pusher.push(0, "ENDC");
	}
	m_pusher.getStack().ensureSize(saveStackSize + 1, "");
}

void TVMExpressionCompiler::visit2(Literal const &_node) {
	const auto* type = getType(&_node);
	switch (_node.annotation().type->category()) {
		case Type::Category::Integer:
		case Type::Category::RationalNumber: {
			if (to<RationalNumberType>(type) && to<RationalNumberType>(type)->isFractional()) {
				cast_error(_node, string("Unsupported type ") + type->canonicalName());
			}
			u256 value = type->literalValue(&_node);
			m_pusher.push(+1, "PUSHINT " + toString(value));
			break;
		}
		case Type::Category::Address:
			solUnimplemented("");
			break;
		case Type::Category::Bool:
			m_pusher.push(+1, _node.token() == Token::TrueLiteral? "TRUE" : "FALSE");
			break;
		case Type::Category::StringLiteral: {
			switch (m_pusher.ctx().pragmaHelper().abiVersion()) {
				case 1:
					visitStringLiteralAbiV1(_node);
					break;
				case 2:
					visitStringLiteralAbiV2(_node);
					break;
				default:
					solAssert(false, "");
			}
			break;
		}
		default:
			cast_error(_node, string("Unsupported type ") + type->canonicalName());
	}
}

void TVMExpressionCompiler::visit2(TupleExpression const &_tupleExpression) {
	if (_tupleExpression.isInlineArray()) {
		m_pusher.pushInt(_tupleExpression.components().size());
		m_pusher.push(+1, "NEWDICT");
		Type const* type = _tupleExpression.annotation().type;
		for (int i = 0; i < static_cast<int>(_tupleExpression.components().size()); ++i) {
			const IntegerType key = getKeyTypeOfArray();
			auto arrayBaseType = to<ArrayType>(type)->baseType();

			compileNewExpr(_tupleExpression.components().at(i).get()); // totalSize dict value
			const DataType& dataType = m_pusher.prepareValueForDictOperations(&key, arrayBaseType, false); // totalSize dict value'
			m_pusher.pushInt(i); // totalSize dict value' index
			m_pusher.push(0, "ROT"); // totalSize value' index dict
			m_pusher.setDict(key, *arrayBaseType, dataType); // totalSize dict
		}
		m_pusher.push(-2 + 1, "PAIR");
	} else {
		for (const auto &comp : _tupleExpression.components()) {
			compileNewExpr(comp.get());
		}
	}
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

bool TVMExpressionCompiler::pushMemberOrLocalVarOrConstant(Identifier const &_identifier) {
	auto& stack = m_pusher.getStack();
	if (stack.isParam(_identifier.annotation().referencedDeclaration)) {
		// push(0, string(";; ") + m_stack.dumpParams());
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
	m_pusher.push(0, string(";; push identifier ") + name);
//		dumpStackSize();
	if (pushMemberOrLocalVarOrConstant(_identifier)) {
	} else if (name == "now") {
		// Getting value of `now` variable
		m_pusher.push(+1, "NOW");
	} else if (name == "this") {
		// calling this.function() will create internal message that should be sent to the address of current contract
		m_pusher.push(0, "; call function of this");
		m_pusher.push(+1, "MYADDR");
	} else if (m_pusher.ctx().getLocalFunction(name)) {
		CodeLines code;
		code.push("CALL $" + name + "_internal$");
		m_pusher.pushCont(code);
	} else {
		cast_error(_identifier, "Unsupported identifier: " + name);
	}
}

void TVMExpressionCompiler::compileUnaryOperation(UnaryOperation const &_node, const std::string &tvmUnaryOperation,
                                                  const bool isPrefixOperation) {
	const int saveStackSize = m_pusher.getStack().size();
	if (isCurrentResultNeeded()) {
		const LValueInfo lValueInfo = expandLValue(&_node.subExpression(), true);
		const int expandedLValueSize = m_pusher.getStack().size() - saveStackSize - 1;
		solAssert(expandedLValueSize >= 0, "");
		if (isPrefixOperation) {
			m_pusher.push(0, tvmUnaryOperation);
			m_pusher.push(+1, "DUP"); // expanded.. value value
			if (expandedLValueSize != 0) {
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. value
			}
		} else {
			m_pusher.push(+1, "DUP"); // expanded.. value value
			m_pusher.push(0, tvmUnaryOperation); // expanded.. value newValue
			if (expandedLValueSize) {
				m_pusher.exchange(0, 1); // expanded.. newValue value
				m_pusher.blockSwap(expandedLValueSize + 1, 1); // value expanded.. newValue
			}
		}
		checkBitFit(_node.annotation().type, _node.annotation().type, _node.annotation().type, tvmUnaryOperation);
		collectLValue(lValueInfo, true, false);
	} else {
		const LValueInfo lValueInfo = expandLValue(&_node.subExpression(), true, true);
		m_pusher.push(0, tvmUnaryOperation);
		checkBitFit(_node.annotation().type, _node.annotation().type, _node.annotation().type, tvmUnaryOperation);
		collectLValue(lValueInfo, true, false);
	}
}

void TVMExpressionCompiler::compileUnaryDelete(UnaryOperation const &node) {
	const LValueInfo lValueInfo = expandLValue(&node.subExpression(), false);
	Expression const* lastExpr = lValueInfo.expressions.back();
	Type const* exprType = node.subExpression().annotation().type;
	if (to<Identifier>(lastExpr)) {
		m_pusher.pushDefaultValue(exprType, lValueInfo.isValueBuilder);
		collectLValue(lValueInfo, true, lValueInfo.isValueBuilder);
	} else if (auto memberAccess = to<MemberAccess>(lastExpr)) {
		MemberAccessAnnotation& a = memberAccess->annotation();
		auto decl = to<VariableDeclaration>(a.referencedDeclaration);
		m_pusher.pushDefaultValue(decl->type(), lValueInfo.isValueBuilder);
		collectLValue(lValueInfo, true, lValueInfo.isValueBuilder);
	} else if (auto indexAccess = to<IndexAccess>(lastExpr)) {
		// ... index dict
		Type const* baseExprType = indexAccess->baseExpression().annotation().type;
		auto arrayType = to<ArrayType>(baseExprType);
		if (arrayType) {
			Type const* valueType = arrayType->baseType();
			m_pusher.pushDefaultValue(valueType, lValueInfo.isValueBuilder); // index dict value
			collectLValue(lValueInfo, true, lValueInfo.isValueBuilder);
		} else { // mapping
			m_pusher.push(+1, "PUSH S1");                            // ... index dict index
            TypePointer const dictKey = StackPusherHelper::parseIndexType(indexAccess->baseExpression().annotation().type);
			m_pusher.push(0, "SWAP");                                // ... index index' dict
			m_pusher.pushInt(lengthOfDictKey(dictKey)); // ..index index dict nbits
			m_pusher.push(-3 + 2, "DICT" + typeToDictChar(dictKey) + "DEL");  // ... index dict' {-1,0}
			m_pusher.push(-1, "DROP");                               // ... index dict'
			collectLValue(lValueInfo, false, lValueInfo.isValueBuilder); // lValueInfo.isValueBuilder is ignored
		}
	} else {
		solAssert(false, "");
	}
}

void TVMExpressionCompiler::visit2(UnaryOperation const &_node) {
	auto op = _node.getOperator();
	m_pusher.push(0, string(";; ") + TokenTraits::toString(op));
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
			m_pusher.push(0, "NOT");
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

bool TVMExpressionCompiler::argumentsIsGoodForFixedBytes(Type const *a, Type const *b) {
	Type::Category lh = a->category();
	Type::Category rh = b->category();
	bool lIsFixedBytes = lh == Type::Category::FixedBytes;
	bool rIsFixedBytes = rh == Type::Category::FixedBytes;
	if (!lIsFixedBytes && !rIsFixedBytes) {
		return true;
	}
	if ((lIsFixedBytes && !rIsFixedBytes) || (!lIsFixedBytes && rIsFixedBytes)) {
		return true;
	}
	return to<FixedBytesType>(a)->storageBytes() == to<FixedBytesType>(b)->storageBytes();
}

void TVMExpressionCompiler::compareAddresses(Token op) {
	switch(op) {
		case Token::GreaterThan:
			m_pusher.push(0, "SDLEXCMP");
			m_pusher.push(0, "ISPOS");
			break;
		case Token::GreaterThanOrEqual:
			m_pusher.push(0, "SDLEXCMP");
			m_pusher.push(0, "ISNNEG");
			break;
		case Token::LessThan:
			m_pusher.push(0, "SDLEXCMP");
			m_pusher.push(0, "ISNEG");
			break;
		case Token::LessThanOrEqual:
			m_pusher.push(0, "SDLEXCMP");
			m_pusher.push(0, "ISNPOS");
			break;
		case Token::Equal:
			m_pusher.push(0, "SDEQ");
			break;
		case  Token::NotEqual:
			m_pusher.push(0, "SDEQ");
			m_pusher.push(0, "NOT");
			break;
		default:
			solAssert(false, "Wrong compare operation");
	}
	m_pusher.push(-2 + 1, "");
}

bool TVMExpressionCompiler::tryOptimizeBinaryOperation(BinaryOperation const &_node) {
	Token op = _node.getOperator();
	Expression const* r = &_node.rightExpression();
	const auto& [ok, val] = TVMExpressionCompiler::constValue(*r);
	if (ok && -128 <= val && val < 128 && isIn(op, Token::NotEqual, Token::Equal, Token::GreaterThan, Token::LessThan)) {
		compileNewExpr(&_node.leftExpression());
		switch (op) {
			case Token::NotEqual:
				m_pusher.push(-1 + 1, "NEQINT " + val.str());
				break;
			case Token::Equal:
				m_pusher.push(-1 + 1, "EQINT " + val.str());
				break;
			case Token::GreaterThan:
				m_pusher.push(-1 + 1, "GTINT " + val.str());
				break;
			case Token::LessThan:
				m_pusher.push(-1 + 1, "LESSINT " + val.str());
				break;
			default:
				solAssert(false, "");
		}
		return true;
	}
	return false;
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

void TVMExpressionCompiler::visit2(BinaryOperation const &_node) {
	const auto& lexp = _node.leftExpression();
	const auto& rexp = _node.rightExpression();
	Type const* lt = getType(&lexp);
	Type const* rt = getType(&rexp);

	if (!argumentsIsGoodForFixedBytes(lt, rt) || lt->category() == Type::Category::Function || rt->category() == Type::Category::Function) {
		cast_error(_node, "Unsupported binary operation");
	}

	if (tryOptimizeBinaryOperation(_node)) {
		return;
	}

	const Token op = _node.getOperator();
	auto acceptLeftAndRight = [&]() {
		// The order of evaluation is important!
		compileNewExpr(&lexp);
		compileNewExpr(&rexp);
	};

	if (isString(lt) && isString(rt)) {
		if (op == Token::Add) {
			compileNewExpr(&lexp);
			m_pusher.push(+1-1,"CTOS");
			compileNewExpr(&rexp);
			m_pusher.push(+1-1,"CTOS");
			m_pusher.exchange(0, 1);
			m_pusher.push(+1, "NEWC");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(+1-1,"ENDC");
		} else if (TokenTraits::isCompareOp(op)){
			compileNewExpr(&lexp);
			m_pusher.push(+1-1,"CTOS");
			compileNewExpr(&rexp);
			m_pusher.push(+1-1,"CTOS");
			compareAddresses(op);
		} else {
			cast_error(_node, "Unsupported binary operation");
		}
		return;
	}

	if (isAddressOrContractType(lt) || isAddressOrContractType(rt)) {
		acceptLeftAndRight();
		compareAddresses(op);
		return;
	}

	if (op == Token::And || op == Token::Or) {
		std::vector<Expression const*> order = unroll(_node);
		compileNewExpr(order[0]);
		for (int i = 1; i < static_cast<int>(order.size()); ++i) {
			if (to<Identifier>(order[i]) || order[i]->annotation().isPure) {
				compileNewExpr(order[i]);
				m_pusher.push(-1, op == Token::Or? "OR" : "AND");
			} else {
				m_pusher.push(0, ";; short-circuiting " + std::string{op == Token::Or? "||" : "&&"});
				m_pusher.push(+1, "DUP");
				m_pusher.push(-1, ""); // fix stack for if

				m_pusher.startContinuation();
				m_pusher.push(-1, "DROP");
				compileNewExpr(order[i]);
			}
		}

		for (int i = static_cast<int>(order.size()) - 1; i >= 1; --i) {
			if (to<Identifier>(order[i]) || order[i]->annotation().isPure) {
				// nothing
			} else {
				m_pusher.endContinuation();
				m_pusher.push(0, op == Token::Or ? "IFNOT" : "IF");
			}
		}
		return;
	}

	acceptLeftAndRight();
	m_pusher.push(0, string(";; ") + TokenTraits::toString(_node.getOperator()));
	if (op == Token::Exp) {
		// TODO: this code is hard to understand. Would it be better to move it to stdlib?
		m_pusher.push(0, "SWAP");
		m_pusher.push(0, "PUSHINT 1");
		m_pusher.push(0, "PUSHCONT {");
		m_pusher.push(0, "\tPUSH s2");
		m_pusher.push(0, "\tPUSHINT 0");
		m_pusher.push(0, "\tGREATER");
		m_pusher.push(0, "\tNOT DUP IFRET DROP");
		m_pusher.push(0, "\tPUSH s2");
		m_pusher.push(0, "\tPUSHINT 1");
		m_pusher.push(0, "\tAND");
		m_pusher.push(0, "\tPUSHINT 1");
		m_pusher.push(0, "\tEQUAL");
		m_pusher.push(0, "\tPUSHCONT {");
		m_pusher.push(0, "\t\tDUP");
		m_pusher.push(0, "\t\tPUSH s2");
		m_pusher.push(0, "\t\tMUL");
		m_pusher.push(0, "\t\tNIP");
		m_pusher.push(0, "\t}");
		m_pusher.push(0, "\tIF");
		m_pusher.push(0, "\tPUSH s2");
		m_pusher.push(0, "\tPUSHINT 1");
		m_pusher.push(0, "\tRSHIFT");
		m_pusher.push(0, "\tPOP s3");
		m_pusher.push(0, "\tPUSH s1");
		m_pusher.push(0, "\tPUSH s2");
		m_pusher.push(0, "\tMUL");
		m_pusher.push(0, "\tPOP s2");
		m_pusher.push(0, "\tFALSE");
		m_pusher.push(0, "}");
		m_pusher.push(0, "UNTIL");
		m_pusher.push(0, "NIP");	// remove operands
		m_pusher.push(0, "NIP");
		m_pusher.push(-2+1, "");	// fixup stack
		return;
	}

	if (op == Token::SHR) cast_error(_node, "Unsupported operation >>>");
	if (op == Token::Comma) cast_error(_node, "Unsupported operation ,");

	bool checkOverflow = false;
	string cmd;
	if      (op == Token::Add) { cmd = "ADD"; checkOverflow = true; }
	else if (op == Token::Mul) { cmd = "MUL"; checkOverflow = true; }
	else if (op == Token::Sub) { cmd = "SUB"; checkOverflow = true; }
	else if (op == Token::Mod) { cmd = "MOD"; }
	else if (op == Token::Div) { cmd = "DIV"; }
	else if (op == Token::GreaterThan) cmd = "GREATER";
	else if (op == Token::GreaterThanOrEqual) cmd = "GEQ";
	else if (op == Token::LessThan) cmd = "LESS";
	else if (op == Token::LessThanOrEqual) cmd = "LEQ";
	else if (op == Token::Equal) cmd = "EQUAL";
	else if (op == Token::NotEqual) cmd = "NEQ";
	else if (op == Token::BitAnd) cmd = "AND";
	else if (op == Token::BitOr) cmd = "OR";
	else if (op == Token::SHL) { cmd = "LSHIFT"; checkOverflow = true; }
	else if (op == Token::SAR) cmd = "RSHIFT";
	else if (op == Token::BitXor) cmd = "XOR";
	else {
		cast_error(_node, "Unsupported binary operation");
	}
	m_pusher.push(-1, cmd);
	if (checkOverflow) {
		checkBitFit(getType(&_node), lt, rt, cmd);
	}
}

void
TVMExpressionCompiler::checkBitFit(Type const *type, Type const *lType, Type const *rType, const std::string &opcode) {
	if (m_pusher.ctx().ignoreIntegerOverflow()) {
		return;
	}

	if (type->category() == Type::Category::RationalNumber) {
		return;
	}
	TypeInfo ti{type};
	if (!ti.isNumeric) {
		solAssert(false, "");
	}

	if (ti.isSigned) {
		m_pusher.push(0, "FITS " + toString(ti.numBits));
	} else {
		auto intL = to<IntegerType>(lType);
		auto intR = to<IntegerType>(rType);
		auto intResult = to<IntegerType>(type);
		bool isCheckNotNeeded = intL && intR && intResult &&
		                        !intL->isSigned() && !intR->isSigned() && !intResult->isSigned() &&
		                        intResult->numBits() == 256 &&
		                        isIn(opcode, "ADD", "MUL", "LSHIFT", "INC");
		if (!isCheckNotNeeded) { // maximal value for int257 is the same as for uint256
			m_pusher.push(0, "UFITS " + toString(ti.numBits));
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
		m_pusher.getGlob(9);
	} else if (_node.memberName() == "value") { // msg.value
		m_pusher.pushPrivateFunctionOrMacroCall(+1, "message_balance_macro");
	} else  if (_node.memberName() == "createdAt") { // msg.createdAt
		m_pusher.pushLines(R"(
DEPTH
PUSHINT 5
SUB
PICK
PUSHCONT {
	PUSHINT 0
}
PUSHCONT {
	DEPTH
	PUSHINT 3
	SUB
	PICK

	CTOS

	LDU 4
	LDMSGADDR
	LDMSGADDR
	LDGRAMS
	LDDICT
	LDGRAMS
	LDGRAMS
	LDU 64
	PLDU 32

	XCHG S8
	BLKDROP 8
}
IFELSE
)");
		m_pusher.push(+1, ""); // fix stack
	} else if (_node.memberName() == "currencies") { // msg.currencies
		m_pusher.pushLines(R"(
DEPTH
PUSHINT 5
SUB
PICK
PUSHCONT {
	NEWDICT
}
PUSHCONT {
	DEPTH
	PUSHINT 3
	SUB
	PICK

	CTOS

	LDU 4
	LDMSGADDR
	LDMSGADDR
	LDGRAMS
	PLDDICT
	XCHG S4
	BLKDROP 4
}
IFELSE
)");
		m_pusher.push(+1, ""); // fix stack
	} else if (_node.memberName() == "data") {
		m_pusher.pushLines(R"(
DEPTH
PUSHINT 4
SUB
PICK
)");
		m_pusher.push(+1, ""); // fix stack
	} else {
		cast_error(_node, "Unsupported magic");
	}
}

void TVMExpressionCompiler::visitMagic(MemberAccess const &_node) {
	auto unsupportedMagic = [&](){cast_error(_node, "Unsupported magic");};

	auto identifier = to<Identifier>(&_node.expression());
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
	m_pusher.push(0, ";; get member " + memberName);
	auto category = getType(&_node.expression())->category();
	if (category == Type::Category::Struct) {
		Expression const* expression = &_node.expression();
		acceptExpr(expression);
		bool isExpressionSlice = m_resultIsSlice.count(expression);
		bool returnStructAsSlice = isExpressionSlice && m_expressionDepth != 0;

		auto structType = to<StructType>(_node.expression().annotation().type);
		StructCompiler structCompiler{&m_pusher, structType};
		structCompiler.pushMember(memberName, !isExpressionSlice, returnStructAsSlice);

		if (returnStructAsSlice) {
			m_resultIsSlice.insert(&_node);
		}
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
		m_pusher.index(0);
		return true;
	}
	if (_node.memberName() == "currencies") {
		if (!isAddressThis(to<FunctionCall>(&_node.expression()))) {
			cast_error(_node.expression(), "Only 'address(this).currencies' is supported for member currencies");
		}
		m_pusher.push(+1, "GETPARAM 7");
		m_pusher.index(1);
		return true;
	}
	if (_node.memberName() == "wid") {
		compileNewExpr(&_node.expression());
		m_pusher.push(-1 + 1, "PARSEMSGADDR");
		m_pusher.index(2);
		return true;
	}
	if (_node.memberName() == "value") {
		compileNewExpr(&_node.expression());
		m_pusher.push(-1 + 1, "PARSEMSGADDR");
		m_pusher.index(3);
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
			m_pusher.push(-1 + 1, "CTOS");
			m_pusher.push(-1 + 1, "SBITS");
			m_pusher.push(-1 + 1, "RSHIFT 3");
		} else {
			m_pusher.index(0);
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
	if (!isIn(baseExprCategory, Type::Category::Mapping, Type::Category::ExtraCurrencyCollection) && !isUsualArray(baseExprType)) {
		cast_error(_node, "Index access is supported only for dynamic arrays and mappings");
	}
}

void TVMExpressionCompiler::visit2(IndexAccess const &indexAccess) {
	m_pusher.push(0, ";; index");
	Type const *baseType = indexAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArray()) {
			acceptExpr(&indexAccess.baseExpression()); // bytes
			m_pusher.push(-1 + 1, "CTOS");
			compileNewExpr(indexAccess.indexExpression()); // slice index
			m_pusher.push(-1 + 1, "MULCONST 8");
			m_pusher.push(-2 + 1, "SDSKIPFIRST");
			m_pusher.push(-1 + 1, "PLDU 8");
			return ;
		} else {
			compileNewExpr(indexAccess.indexExpression()); // index
			acceptExpr(&indexAccess.baseExpression()); // index array
			m_pusher.index(1); // index dict
		}
	} else {
		compileNewExpr(indexAccess.indexExpression()); // index
		m_pusher.prepareKeyForDictOperations(indexAccess.indexExpression()->annotation().type);
		acceptExpr(&indexAccess.baseExpression()); // index dict
	}

	bool returnStructAsSlice = m_expressionDepth != 0;
	m_pusher.getDict(*StackPusherHelper::parseIndexType(baseType),
	                 *StackPusherHelper::parseValueType(indexAccess),
	                 baseType->category() == Type::Category::Mapping ||
	                 baseType->category() == Type::Category::ExtraCurrencyCollection ?
	                 StackPusherHelper::GetDictOperation::GetFromMapping :
	                 StackPusherHelper::GetDictOperation::GetFromArray,
	                 returnStructAsSlice);
	if (returnStructAsSlice) {
		m_resultIsSlice.insert(&indexAccess);
	}
}

bool TVMExpressionCompiler::checkAbiMethodCall(FunctionCall const &_functionCall) {
	// compile "abi.encodePacked()"
	if (auto memberAccess = to<MemberAccess>(&_functionCall.expression())) {
		auto identifier = to<Identifier>(&memberAccess->expression());
		if (identifier != nullptr && identifier->name() == "abi") {
			if (memberAccess->memberName() != "encodePacked") {
				cast_error(_functionCall, "Only method encodePacked is supported for abi");
			}

			int builderSize = 0;
			bool haveArray = false;
			m_pusher.push(+1, "NEWC");
			for (const ASTPointer<Expression const>& argument : _functionCall.arguments()) {
				const Type *type = getType(argument.get());
				if (to<IntegerType>(type) != nullptr) {
					compileNewExpr(argument.get());
					m_pusher.push(-1, storeIntegralOrAddress(type, true));
					builderSize += TypeInfo{type}.numBits;
				} else if (auto array = to<ArrayType>(type)) {
					if (to<IntegerType>(array->baseType()) == nullptr) {
						cast_error(*argument.get(), "Only numeric array is supported for abi.encodePacked(...) ");
					}
					if (_functionCall.arguments().size() != 1) {
						cast_error(_functionCall, "Only one array is supported for abi.encodePacked(...)");
					}
					TypeInfo arrayElementTypeInfo(array->baseType());
					compileNewExpr(argument.get()); // builder
					m_pusher.push(-1 + 2, "UNPAIR"); // builder size dict
					m_pusher.pushInt(arrayElementTypeInfo.numBits); // builder size dict valueLength
					m_pusher.pushPrivateFunctionOrMacroCall(-4 + 1, "abi_encode_packed_macro");
					haveArray = true;
				} else {
					cast_error(_functionCall, "Only numeric types or numeric arrays are supported for abi.encodePacked(...)");
				}
			}
			if (builderSize > TvmConst::CellBitLength) {
				cast_error(_functionCall, "Bit length of params >= 1023");
			} else if (haveArray) {
				cast_warning(_functionCall, "Ensure that sum of array's value bit length fit in 1023 bit");
			}
			m_pusher.push(0, "ENDC");
			return true;
		}
	}
	return false;
}

std::string TVMExpressionCompiler::getDefaultMsgValue() {
	const auto expr = m_pusher.ctx().pragmaHelper().haveMsgValue();
	if (!expr) {
		return StackPusherHelper::tonsToBinaryString(u256{TvmConst::Message::DefaultMsgValue});
	}
	const auto& [ok, val] = constValue(*expr);
	if (!ok) {
		cast_error(*expr, "Default value should be compile time expression of number type");
	}
	return StackPusherHelper::tonsToBinaryString(val);
}

bool TVMExpressionCompiler::checkRemoteMethodCall(FunctionCall const &_functionCall) {
	const ast_vec<Expression const> arguments = _functionCall.arguments();

	std::map<int, Expression const *> exprs;
	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"}};
	std::function<void(int)> appendBody;
	Expression const *sendrawmsgFlag{};
	FunctionDefinition const* fdef{};

	if (auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression())) {
		auto memberAccess = to<MemberAccess>(&functionOptions->expression());
		if (!memberAccess) {
			return false;
		}

		// parse options they are stored in two vectors: names and options
		std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
		for (const auto &option: optionNames)
			if (!isIn(*option, "flag", "value", "currencies", "bounce"))
				cast_error(_functionCall, "Unsupported function call option: " + *option);

		// Search for bounce option
		auto bounceIt = std::find_if(optionNames.begin(), optionNames.end(),
										 [](auto el) { return *el == "bounce"; });
		if (bounceIt != optionNames.end()) {
			size_t index = bounceIt - optionNames.begin();
			exprs[TvmConst::int_msg_info::bounce] = functionOptions->options()[index].get();
		} else {
			constParams[TvmConst::int_msg_info::bounce] = "1";
		}

		// Search for currencies option
		auto currenciesIt = std::find_if(optionNames.begin(), optionNames.end(),
		                                 [](auto el) { return *el == "currencies"; });
		if (currenciesIt != optionNames.end()) {
			size_t index = currenciesIt - optionNames.begin();
			exprs[TvmConst::int_msg_info::currency] = functionOptions->options()[index].get();
		} else {
			constParams[TvmConst::int_msg_info::currency] = "0";
		}

		// Search for value (ton) option
		auto valueIt = std::find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "value"; });
		if (valueIt != optionNames.end()) {
			const size_t index = valueIt - optionNames.begin();
			const auto& [ok, value] = TVMExpressionCompiler::constValue(*functionOptions->options().at(index));
			if (ok) {
				constParams[TvmConst::int_msg_info::tons] = StackPusherHelper::tonsToBinaryString(u256(value));
			} else {
				exprs[TvmConst::int_msg_info::tons] = functionOptions->options()[index].get();
			}
		} else {
			constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		}

		// remote_addr
		exprs[TvmConst::int_msg_info::dest] = &memberAccess->expression();

		fdef = getRemoteFunctionDefinition(memberAccess);

		// Search for senrawmsg flag option
		auto flagIt = std::find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "flag";});
		if (flagIt != optionNames.end()) {
			size_t index = flagIt - optionNames.begin();
			sendrawmsgFlag = functionOptions->options()[index].get();
		}
	} else {
		constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		constParams[TvmConst::int_msg_info::bounce] = "1";

		Expression const *currentExpression = &_functionCall.expression();
		while (true) {
			auto currentFunctionCall = to<FunctionCall>(currentExpression);
			if (currentFunctionCall == nullptr) {
				break;
			}

			auto memberAccess = to<MemberAccess>(&currentFunctionCall->expression());
			if (memberAccess == nullptr) {
				return false;
			}

			if (memberAccess->memberName() == "flag") {
				sendrawmsgFlag = currentFunctionCall->arguments()[0].get();
				currentExpression = &memberAccess->expression();
			} else if (memberAccess->memberName() == "value") {
				exprs[TvmConst::int_msg_info::tons] = currentFunctionCall->arguments()[0].get();
				constParams.erase(TvmConst::int_msg_info::tons);
				currentExpression = &memberAccess->expression();
			} else {
				break;
			}
		}
		auto memberValue = to<MemberAccess>(currentExpression);
		if (memberValue == nullptr) {
			return false;
		}
		exprs[TvmConst::int_msg_info::dest] = &memberValue->expression();
		fdef = getRemoteFunctionDefinition(memberValue);
		if (fdef == nullptr) {
			return false;
		}
	}

	appendBody = [&](int builderSize) {
		return EncodeFunctionParams{&m_pusher}.createMsgBodyAndAppendToBuilder2(
				arguments,
				convertArray(fdef->parameters()),
				EncodeFunctionParams{&m_pusher}.calculateFunctionIDWithReason(fdef, ReasonOfOutboundMessage::RemoteCallInternal),
				builderSize);
	};

	if (isCurrentResultNeeded())
		cast_error(_functionCall, "Calls to remote contract do not return result.");

	if (sendrawmsgFlag)
		m_pusher.sendIntMsg(exprs, constParams, appendBody, [&]() { compileNewExpr(sendrawmsgFlag); });
	else
		m_pusher.sendIntMsg(exprs, constParams, appendBody, nullptr);
	return true;
}

const FunctionDefinition *TVMExpressionCompiler::getRemoteFunctionDefinition(const MemberAccess *memberAccess) {
	auto expr = &memberAccess->expression();
	if (isSuper(expr))
		return nullptr;
	auto ctype = to<ContractType>(getType(expr));
	if (!ctype)
		return nullptr;
	Declaration const* decl = memberAccess->annotation().referencedDeclaration;
	auto f = to<FunctionDefinition>(decl);
	if (!f) {
		cast_error(*memberAccess, "Unsupported remote function call.");
	}
	ContractDefinition const& remoteContract = ctype->contractDefinition();
	m_pusher.push( 0, ";; Remote call " + remoteContract.name() + "." + f->name());
	return f;
}

std::tuple<Type const*, Type const*>
dictKeyValue(MemberAccess const* memberAccess) {
	Type const* keyType{};
	Type const* valueType{};
	if (auto mapType = to<MappingType>(memberAccess->expression().annotation().type)) {
		keyType = mapType->keyType();
		valueType = mapType->valueType();
	} else if (auto ccType = to<ExtraCurrencyCollectionType>(memberAccess->expression().annotation().type)) {
		keyType = ccType->keyType();
		valueType = ccType->realValueType();
	} else {
		solAssert(false, "");
	}
	return {keyType, valueType};
}

class DelMinOrMax : public DictOperation {
public:
	DelMinOrMax(StackPusherHelper& pusher, Type const &keyType, Type const &valueType, bool isDelMin, MemberAccess const* memberAccess)  :
		DictOperation{pusher, keyType, valueType},
		isDelMin{isDelMin},
		memberAccess{memberAccess},
		ec{pusher}
	{

	}

	void delMinOrMax() {
		bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
		opcode = "DICT" + typeToDictChar(&keyType) + "REM" + (isDelMin? "MIN" : "MAX") + (isInRef? "REF" : "");
		stackSize = pusher.getStack().size();

		lValueInfo = ec.expandLValue(&memberAccess->expression(), true, false); // lValue... map
		pusher.pushInt(keyLength); // dict nbits
		pusher.push(-2 + 3, opcode); //  D value key -1

		pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, true, isInRef, StackPusherHelper::DecodeType::DecodeValueOrPushNull, false);

		// mapLValue... D optPair
		const int cntOfValuesOnStack = pusher.getStack().size() - stackSize;
		pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optPair mapLValue... map
		ec.collectLValue(lValueInfo, true, false); // value key
	}

private:
	const bool isDelMin{};
	MemberAccess const* memberAccess{};
	TVMExpressionCompiler::LValueInfo lValueInfo{};
	TVMExpressionCompiler ec;
	int stackSize{};
	std::string opcode;
};

void TVMExpressionCompiler::mappingDelMinOrMax(FunctionCall const &_functionCall, bool isDelMin) {
	auto memberAccess = to<MemberAccess>(&_functionCall.expression());

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess);

	DelMinOrMax d{m_pusher, *keyType, *valueType, isDelMin, memberAccess};
	d.delMinOrMax();
}

void TVMExpressionCompiler::mappingGetSet(FunctionCall const &_functionCall) {
	auto memberAccess = to<MemberAccess>(&_functionCall.expression());

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess);

	const ASTString &memberName = memberAccess->memberName();
	if (isIn(memberName, "fetch", "at")) {
		compileNewExpr(_functionCall.arguments()[0].get()); // index
        m_pusher.prepareKeyForDictOperations(keyType);
		compileNewExpr(&memberAccess->expression()); // index dict
		if (memberName == "fetch")
			m_pusher.getDict(*keyType, *valueType, StackPusherHelper::GetDictOperation::Fetch, false);
		else
			m_pusher.getDict(*keyType, *valueType, StackPusherHelper::GetDictOperation::GetFromArray, false);
	} else if (memberName == "exists") {
		compileNewExpr(_functionCall.arguments()[0].get()); // index
        m_pusher.prepareKeyForDictOperations(keyType);
		compileNewExpr(&memberAccess->expression()); // index dict
		m_pusher.getDict(*keyType, *valueType, StackPusherHelper::GetDictOperation::Exist, false);
	} else if (isIn(memberName, "replace", "add", "getSet", "getAdd", "getReplace")) {
		const int stackSize = m_pusher.getStack().size();
		auto ma = to<MemberAccess>(&_functionCall.expression());
		const LValueInfo lValueInfo = expandLValue(&ma->expression(), true); // lValue... map
		compileNewExpr(_functionCall.arguments()[1].get()); // lValue... map value
		const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueType, false); // lValue... map value'
		compileNewExpr(_functionCall.arguments()[0].get()); // mapLValue... map value key
        m_pusher.prepareKeyForDictOperations(keyType);
		m_pusher.push(0, "ROT"); // mapLValue... value key map

		if (isIn(memberName, "replace", "add")) {
			StackPusherHelper::SetDictOperation op;
			if (memberName == "replace") {
				op = StackPusherHelper::SetDictOperation::Replace;
			} else if (memberName == "add") {
				op = StackPusherHelper::SetDictOperation::Add;
			} else {
				solAssert(false, "");
			}
			m_pusher.setDict(*keyType, *valueType, dataType, op); // mapLValue... map {0, -1}
		} else {
			StackPusherHelper::GetDictOperation op;
			if (memberName == "getSet") {
				op = StackPusherHelper::GetDictOperation::GetSetFromMapping;
			} else if (memberName == "getAdd") {
				op = StackPusherHelper::GetDictOperation::GetAddFromMapping;
			} else if (memberName == "getReplace") {
				op = StackPusherHelper::GetDictOperation::GetReplaceFromMapping;
			} else {
				solAssert(false, "");
			}
			m_pusher.getDict(*keyType, *valueType, op, false, dataType);
			// mapLValue... map optValue
		}
		const int cntOfValuesOnStack = m_pusher.getStack().size() - stackSize;  // mapLValue... map optValue
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optValue mapLValue... map
		collectLValue(lValueInfo, true, false); // optValue
	} else {
		solAssert(false, "");
	}
}

void TVMExpressionCompiler::mappingPrevNextMethods(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess);

	compileNewExpr(_functionCall.arguments()[0].get()); // index
    m_pusher.prepareKeyForDictOperations(keyType, true); // index'
    compileNewExpr(&memberAccess->expression()); // index' dict
	m_pusher.pushInt(lengthOfDictKey(keyType)); // index' dict nbits

	DictPrevNext compiler{m_pusher, *keyType, *valueType, memberAccess->memberName()};
	compiler.prevNext();
}

void TVMExpressionCompiler::mappingMinMaxMethod(FunctionCall const &_functionCall, bool isMin) {
	auto expr = &_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess);

	compileNewExpr(&memberAccess->expression()); // dict

	DictMinMax compiler{m_pusher, *keyType, *valueType, isMin};
	compiler.minOrMax();
}

void TVMExpressionCompiler::mappingEmpty(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	compileNewExpr(&ma->expression());
	m_pusher.push(0, "DICTEMPTY");
}

bool TVMExpressionCompiler::checkForMappingOrCurrenciesMethods(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	if (ma == nullptr || (!to<MappingType>(ma->expression().annotation().type) && !to<ExtraCurrencyCollectionType>(ma->expression().annotation().type)))
		return false;

	const ASTString &memberName = ma->memberName();
	m_pusher.push(0, ";; map." + memberName);

	if (isIn(memberName, "delMin", "delMax")) {
		mappingDelMinOrMax(_functionCall, memberName == std::string{"delMin"});
	} else  if (isIn(memberName, "at", "fetch", "exists", "replace", "add", "getSet", "getAdd", "getReplace")) {
		mappingGetSet(_functionCall);
	} else if (isIn(memberName, "min", "max")) {
		mappingMinMaxMethod(_functionCall, memberName == std::string{"min"});
	} else if (isIn(memberName, "next", "prev", "nextOrEq", "prevOrEq")) {
		mappingPrevNextMethods(_functionCall);
	} else if (memberName == "empty") {
		mappingEmpty(_functionCall);
	} else {
		cast_error(_functionCall, "Unsupported mapping method");
	}

	return true;
}

bool TVMExpressionCompiler::visit2(FunctionCall const &_functionCall) {
	if (checkRemoteMethodCall(_functionCall)) {
		// To avoid situation when we call a function of a remote contract and don't save the result.
		// Remote function can return a result but in fact we don't get it.
		return false;
	}

	auto ma = to<MemberAccess>(&_functionCall.expression());
	if (ma) {
		if (auto libFunction = to<FunctionDefinition>(ma->annotation().referencedDeclaration)) {
			DeclarationAnnotation const &da = libFunction->annotation();
			if (da.contract->contractKind() == ContractKind::Library) {
				m_pusher.ctx().addLib(libFunction);

				auto t = getType(&ma->expression());
				std::vector<ASTPointer<Expression const>> const &args = _functionCall.arguments();
				const int argQty = static_cast<int>(args.size());
				const int retQty = static_cast<int>(libFunction->returnParameters().size());
				if (t->category() == Type::Category::TypeType) {
					for (const ASTPointer<Expression const> &arg : args) {
						acceptExpr(arg.get());
					}
					m_pusher.pushPrivateFunctionOrMacroCall(-argQty + retQty,
															da.contract->name() + "_no_obj_" + libFunction->name());
				} else {
					const int stakeSize0 = m_pusher.getStack().size();
					const LValueInfo lValueInfo = expandLValue(&ma->expression(), true);
					const int stakeSize1 = m_pusher.getStack().size();
					const int lValueQty = stakeSize1 - stakeSize0;

					for (const ASTPointer<Expression const> &arg : args) {
						acceptExpr(arg.get());
					}

					m_pusher.pushPrivateFunctionOrMacroCall((-1 - argQty) + (+1 + retQty),
															da.contract->name() + "_with_obj_" + libFunction->name());

					m_pusher.blockSwap(lValueQty, retQty);

					collectLValue(lValueInfo, true, false);
				}
				return true;
			}
		}
	}

	if (!checkAbiMethodCall(_functionCall) && !checkForMappingOrCurrenciesMethods(_functionCall) ) {
		FunctionCallCompiler fcc(m_pusher, this, _functionCall);
		fcc.compile();
	}
	return true;
}

void TVMExpressionCompiler::visit2(Conditional const &_conditional) {
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

	m_pusher.push(paramQty, "IFELSE");
}

void TVMExpressionCompiler::visit2(ElementaryTypeNameExpression const &_node) {
	m_pusher.ensureValueFitsType(_node.type().typeName(), _node);
}

bool TVMExpressionCompiler::isOptionalGet(Expression const* expr) {
	auto funCall = to<FunctionCall>(expr);
	if (!funCall) {
		return false;
	}
	auto ma = to<MemberAccess>(&funCall->expression());
	return ma && ma->expression().annotation().type->category() == Type::Category::Optional;
}

TVMExpressionCompiler::LValueInfo
TVMExpressionCompiler::expandLValue(
	Expression const *const _expr,
	const bool withExpandLastValue,
	bool willNoStackPermutationForLValue,
	bool isLValue,
	Type const* rightType
) {
	LValueInfo lValueInfo {rightType};

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
			cast_error(*expr, "Unsupported lvalue");
		}
	}
	std::reverse(lValueInfo.expressions.begin(), lValueInfo.expressions.end());
	bool haveIndexAccess = false;
	for (int i = 0; i < static_cast<int>(lValueInfo.expressions.size()); ++i) {
		lValueInfo.isResultBuilder.push_back(haveIndexAccess);
		if (to<IndexAccess>(lValueInfo.expressions[i])) {
			haveIndexAccess = true;
		}
	}
	lValueInfo.isValueBuilder = haveIndexAccess;

	m_pusher.push(0, "; expValue");
	for (int i = 0; i < static_cast<int>(lValueInfo.expressions.size()); i++) {
		bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());
		if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
			auto& stack = m_pusher.getStack();
			auto name = variable->name();
			if (stack.isParam(variable->annotation().referencedDeclaration)) {
				if (isLast && !withExpandLastValue)
					break;
				if (!willNoStackPermutationForLValue || stack.getOffset(variable->annotation().referencedDeclaration) != 0) {
					pushMemberOrLocalVarOrConstant(*variable);
				}
			} else {
				if (isLast && !withExpandLastValue)
					break;
				m_pusher.push(0, ";; fetch " + name);
				auto vd = to<VariableDeclaration>(variable->annotation().referencedDeclaration);
				m_pusher.getGlob(vd);
			}
		} else if (auto index = to<IndexAccess>(lValueInfo.expressions[i])) {
			if (isIn(index->baseExpression().annotation().type->category(), Type::Category::Mapping, Type::Category::ExtraCurrencyCollection)) {
				// dict1
				compileNewExpr(index->indexExpression());
                m_pusher.prepareKeyForDictOperations(index->indexExpression()->annotation().type);
				// dict1 index
				m_pusher.push(0, "SWAP");
				// index dict1
				if (isLast && !withExpandLastValue) break;
				m_pusher.push(+2, "PUSH2 S1, S0");
				// index dict1 index dict1

				m_pusher.getDict(*StackPusherHelper::parseIndexType(index->baseExpression().annotation().type),
				                 *StackPusherHelper::parseValueType(*index),
				                 StackPusherHelper::GetDictOperation::GetFromMapping, true);
				// index dict1 dict2
			} else if (index->baseExpression().annotation().type->category() == Type::Category::Array) {
				// array
				m_pusher.push(-1 + 2, "UNPAIR"); // size dict
				compileNewExpr(index->indexExpression()); // size dict index
				m_pusher.push(0, "SWAP"); // size index dict
				m_pusher.push(+2, "PUSH2 s1,s2"); // size index dict index size
				m_pusher.push(-2 + 1, "LESS"); // size index dict index<size
				m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				if (isLast && !withExpandLastValue) {
					break;
				}
				m_pusher.push(+2, "PUSH2 S1, S0"); // size index dict index dict
				m_pusher.getDict(*StackPusherHelper::parseIndexType(index->baseExpression().annotation().type),
				                 *index->annotation().type, StackPusherHelper::GetDictOperation::GetFromArray,
				                 true);
				// size index dict value
			} else {
				solAssert(false, "");
			}
		} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
			auto structType = to<StructType>(memberAccess->expression().annotation().type);
			StructCompiler structCompiler{&m_pusher, structType};
			const string &memberName = memberAccess->memberName();
			if (lValueInfo.isResultBuilder[i]) {
				structCompiler.expandStruct(memberName, !isLast || withExpandLastValue);
			} else {
				if (isLast && !withExpandLastValue) {
					break;
				}
				m_pusher.push(+1, "DUP");
				structCompiler.pushMember(memberName, true, false);
			}
		} else if (isOptionalGet(lValueInfo.expressions[i])) {
			if (!isLast || withExpandLastValue) {
				m_pusher.pushS(0);
			}
			m_pusher.checkOptionalValue();
		} else {
			solAssert(false, "");
		}
	}
	m_pusher.push(0, "; end expValue");
	return lValueInfo;
}

void
TVMExpressionCompiler::collectLValue(
	const LValueInfo &lValueInfo,
	const bool haveValueOnStackTop,
	bool isValueBuilder
)
{
	// variable [arrayIndex | mapIndex | structMember | <optional>.get()]...

	m_pusher.push(0, "; colValue");
	if (lValueInfo.doesntNeedToCollect) {
		solAssert(haveValueOnStackTop, "Collect with not needed value is available only for on stack top values.");
		m_pusher.push(-1, "DROP");
		return;
	}

	const int n = static_cast<int>(lValueInfo.expressions.size());

	if (haveValueOnStackTop) {
		if (auto tuple = to<TupleType>(lValueInfo.rightType)) {
			m_pusher.tuple(tuple->components().size());
		}
	}

	for (int i = n - 1; i >= 0; i--) {
		const bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());
		const bool isCurrentValueBuilder = (isLast && isValueBuilder) || (!isLast && lValueInfo.isResultBuilder[i + 1]);

		if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
//				pushLog("colVar");
			if (isCurrentValueBuilder) {
				m_pusher.push(0, "ENDC");
				m_pusher.push(0, "CTOS"); // TODO add test with TvmCell
			}
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
					m_pusher.push(-1, "NIP"); // dict
				} else {
					// index dict value
					TypePointer const keyType = StackPusherHelper::parseIndexType(indexAccess->baseExpression().annotation().type);
					TypePointer const valueDictType = StackPusherHelper::parseValueType(*indexAccess);
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType, isCurrentValueBuilder);
					m_pusher.push(0, "ROTREV"); // value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // dict'
				}
				if (lValueInfo.isResultBuilder[i]) {
					m_pusher.push(+1, "NEWC");
					m_pusher.push(-1, "STDICT");
				}
			} else if (indexAccess->baseExpression().annotation().type->category() == Type::Category::Array) {
				//					pushLog("colArrIndex");
				if (isLast && !haveValueOnStackTop) {
					// size index dict
					m_pusher.push(-1, "NIP"); // size dict
				} else {
					// size index dict value
					TypePointer const keyType = StackPusherHelper::parseIndexType(indexAccess->baseExpression().annotation().type);
					auto valueDictType = getType(indexAccess);
					const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueDictType, isCurrentValueBuilder);
					m_pusher.push(0, "ROTREV"); // size value index dict
					m_pusher.setDict(*keyType, *valueDictType, dataType); // size dict'
				}

				if (lValueInfo.isResultBuilder[i]) {
					// size dict'
					m_pusher.push(0, "SWAP"); // dict size
					m_pusher.push(+1, "NEWC"); // dict size builder
					m_pusher.push(-1, "STU 32"); // dict builder
					m_pusher.push(-1, "STDICT"); // builder
				} else {
					m_pusher.push(-2 + 1, "PAIR");
				}
			} else {
				solAssert(false, "");
			}
		} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
//				pushLog("colStruct");
			auto structType = to<StructType>(memberAccess->expression().annotation().type);
			StructCompiler structCompiler{&m_pusher, structType};
			const string &memberName = memberAccess->memberName();
			if (lValueInfo.isResultBuilder[i]) {
				structCompiler.collectStruct(memberName, isCurrentValueBuilder);
			} else {
				structCompiler.setMemberForTuple(memberName);
			}
		} else if (isOptionalGet(lValueInfo.expressions[i])) {
			// do nothing
		} else {
			solAssert(false, "");
		}
	}
	m_pusher.push(0, "; end colValue");
}


class TVMExpressionAnalyzer : private ASTConstVisitor
{
public:
	explicit TVMExpressionAnalyzer(const Expression& expr) {
		expr.accept(*this);
	}

	bool hasSideEffects() const {
		return m_hasSideEffects;
	}

private:
	bool visit(Assignment const&) override {
		m_hasSideEffects = true;
		return true;
	}

	bool visit(UnaryOperation const& _node) override {
		switch (_node.getOperator()) {
			case Token::Inc:
			case Token::Dec:
			case Token::Delete:
				m_hasSideEffects = true;
				break;
			default:
				break;
		}
		return true;
	}

	bool visit(FunctionCall const&) override {
		// TODO: check if it is indeed function (e.g. not structure) and
		//       is neither Pure nor View
		m_hasSideEffects = true;
		return true;
	}

	bool m_hasSideEffects = false;
};

bool TVMExpressionCompiler::tryAssignLValue(Assignment const &_assignment) {
	const auto& lhs = _assignment.leftHandSide();
	const auto& rhs = _assignment.rightHandSide();
	const Token op  = _assignment.assignmentOperator();

	if (op == Token::Assign) {
		auto push_rhs = [&] () {
			if (!m_pusher.tryImplicitConvert(getType(&lhs), getType(&rhs)))
				compileNewExpr(&rhs);
			bool valueIsBuilder = m_pusher.tryPollConvertBuilderToSlice();
			return valueIsBuilder;
		};
		bool hasNoSideEffects =
			!TVMExpressionAnalyzer(lhs).hasSideEffects() &&
			!TVMExpressionAnalyzer(rhs).hasSideEffects();
		if (!isCurrentResultNeeded() && hasNoSideEffects) {
			const LValueInfo lValueInfo = expandLValue(&lhs, false, false, true, getType(&rhs));
			bool valueIsBuilder = push_rhs();
			collectLValue(lValueInfo, true, valueIsBuilder);
		} else {
			bool valueIsBuilder = push_rhs();
			const int saveStackSize = m_pusher.getStack().size();
			const LValueInfo lValueInfo = expandLValue(&lhs, false, false, true, getType(&rhs));
			if (isCurrentResultNeeded()) {
				m_pusher.push(+1, "PUSH s" + toString(m_pusher.getStack().size() - saveStackSize));
			} else {
				m_pusher.blockSwap(1, m_pusher.getStack().size() - saveStackSize);
			}
			collectLValue(lValueInfo, true, valueIsBuilder);
		}
	} else if (isString(getType(&lhs)) && isString(getType(&rhs))) {
		if (op == Token::AssignAdd) {
			compileNewExpr(&rhs);
			m_pusher.push(+1-1,"CTOS");
			const LValueInfo lValueInfo = expandLValue(&lhs, false);
			m_pusher.push(+1-1,"CTOS");
			m_pusher.push(+1, "NEWC");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(+1-1,"ENDC");
			collectLValue(lValueInfo, true, false);
		} else {
			cast_error(_assignment, "Unsupported operation.");
		}
		return true;
	} else {
		string cmd;
		if      (op == Token::AssignAdd)    { cmd = "ADD"; }
		else if (op == Token::AssignMul)    { cmd = "MUL"; }
		else if (op == Token::AssignSub)    { cmd = "SUB"; }
		else if (op == Token::AssignMod)    { cmd = "MOD"; }
		else if (op == Token::AssignDiv)    { cmd = "DIV"; }
		else if (op == Token::AssignBitAnd) { cmd = "AND"; }
		else if (op == Token::AssignBitOr)  { cmd = "OR";  }
		else if (op == Token::AssignBitXor) { cmd = "XOR"; }
		else if (op == Token::AssignShl)    { cmd = "LSHIFT"; }
		else if (op == Token::AssignSar)    { cmd = "RSHIFT"; }
		else {
			cast_error(_assignment, "Unsupported operation.");
		}

		compileNewExpr(&rhs); // r
		const int saveStackSize = m_pusher.getStack().size();
		const LValueInfo lValueInfo = expandLValue(&lhs, true); // r expanded... l
		const int expandedLValueSize = m_pusher.getStack().size() - saveStackSize - 1;
		m_pusher.blockSwap(1, expandedLValueSize + 1); // expanded... l r

		if (isCurrentResultNeeded()) {
			m_pusher.push(-1, cmd); // expanded... res
			m_pusher.push(+1, "DUP"); // expanded... res res
			m_pusher.blockSwap(expandedLValueSize + 1, 1); // res expanded... res
		} else {
			m_pusher.push(-1, cmd); // expanded... res
		}
		checkBitFit(getType(&_assignment), getType(&lhs), getType(&rhs), cmd);
		collectLValue(lValueInfo, true, false);
	}

	return true;
}

bool TVMExpressionCompiler::tryAssignTuple(Assignment const &_assignment) {
	auto lhs = to<TupleExpression>(&_assignment.leftHandSide());
	if (!lhs) {
		return false;
	}

	compileNewExpr(&_assignment.rightHandSide());
	if (lhs->components().size() >= 2) {
		m_pusher.reverse(lhs->components().size(), 0);
	}
	for (const auto & i : lhs->components()) {
		if (!i) {
			m_pusher.push(-1, "DROP");
			continue;
		}
		const int stackSizeForValue = m_pusher.getStack().size();
		const LValueInfo lValueInfo = expandLValue(i.get(), false);
		const int stackSize = m_pusher.getStack().size();
		const int expandLValueSize = stackSize - stackSizeForValue;
		if (expandLValueSize > 0) {
			m_pusher.blockSwap(1, expandLValueSize);
		}
		collectLValue(lValueInfo, true, false);
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
	const auto& [ok, val] = constValue(*expr);
	if (ok) {
		m_pusher.push(+1, "PUSHINT " + val.str());
		return true;
	}

	return false;
}


void DictMinMax::minOrMax() {
	// stack: dict
	pusher.pushInt(lengthOfDictKey(&keyType)); // dict nbits

	const bool haveKey = true;
	bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
	dictOpcode = "DICT" + typeToDictChar(&keyType) + (isMin? "MIN" : "MAX") + (isInRef? "REF" : "");

	pusher.push(-2 + 2, dictOpcode); // (value, key, -1) or 0
	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			haveKey,
			isInRef,
			StackPusherHelper::DecodeType::DecodeValueOrPushNull,
			false
	);
}

void DictPrevNext::prevNext() {
	// stack: index dict nbits
	std::string dictOpcode = std::string{"DICT"} + typeToDictChar(&keyType) + "GET";
	if (oper == "next"){
		dictOpcode += "NEXT";
	} else if (oper == "prev") {
		dictOpcode += "PREV";
	} else if (oper == "nextOrEq") {
		dictOpcode += "NEXTEQ";
	} else if (oper == "prevOrEq") {
		dictOpcode += "PREVEQ";
	} else {
		solAssert(false, "");
	}

	int ss = pusher.getStack().size();
	pusher.push(-3 + 2, dictOpcode); // value key -1 or 0

	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			true,
			false,
			StackPusherHelper::DecodeType::DecodeValueOrPushNull,
			false
	);

	pusher.getStack().ensureSize(ss - 3 + 2 - 2 + 1);
}
