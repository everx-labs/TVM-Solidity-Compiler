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
	ec.acceptExpr2(expr, true);
}

void TVMExpressionCompiler::acceptExpr2(const Expression *expr, const bool _isResultNeeded) {
	solAssert(expr, "");
	// Recursive call are not allowed.
	solAssert(m_expressionDepth == -1, "");
	m_isResultNeeded = _isResultNeeded;
	acceptExpr(expr);
	m_expressionDepth = -1;
}

void TVMExpressionCompiler::acceptExpr(const Expression *expr) {
	const int savedExpressionDepth = m_expressionDepth;
	++m_expressionDepth;
	if (fold_constants(expr)) {
	} else if (auto e = to<Literal>(expr)) {
		visit2(*e);
	} else if (auto e0 = to<Identifier>(expr)) {
		visit2(*e0);
	} else if (auto e1 = to<BinaryOperation>(expr)) {
		visit2(*e1);
	} else if (auto e2 = to<UnaryOperation>(expr)) {
		visit2(*e2);
	} else if (auto e3 = to<Assignment>(expr)) {
		visit2(*e3);
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
	} else if (auto e9 = to<ElementaryTypeNameExpression>(expr)) {
		visit2(*e9);
	} else {
		cast_error(*expr, string("Unsupported expression ") + typeid(expr).name());
	}

	--m_expressionDepth;
	solAssert(savedExpressionDepth == m_expressionDepth,
	          "Internal error: depth exp " + toString(savedExpressionDepth) + " got " + toString(m_expressionDepth));
}

bool TVMExpressionCompiler::isLiteral(Expression const &_e) {
	return dynamic_cast<Literal const*>(&_e) || _e.annotation().type->category() == Type::Category::RationalNumber;
}

bool TVMExpressionCompiler::isCurrentResultNeeded() {
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
			if (slice.size() / 2 >= 126) {
				m_pusher.push(+1, "NEWC");
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(0, slice.size() / 2));
				m_pusher.push(-1, "STSLICER");
				m_pusher.push(+1, "PUSHSLICE x" + slice.substr(slice.size() / 2));
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
			m_pusher.literalToSliceAddress(&_node);
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

			acceptExpr(_tupleExpression.components().at(i).get()); // totalSize dict value
			bool isValueBuilder = m_pusher.prepareValueForDictOperations(&key, arrayBaseType, false); // totalSize dict value'
			m_pusher.pushInt(i); // totalSize dict value' index
			m_pusher.push(0, "ROT"); // totalSize value' index dict
			m_pusher.setDict(key, *arrayBaseType, isValueBuilder, _tupleExpression); // totalSize dict
		}
		m_pusher.push(-2 + 1, "PAIR");
	} else {
		for (const auto &comp : _tupleExpression.components()) {
			acceptExpr(comp.get());
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
	acceptExpr(variableDeclaration->value().get());
	return true;
}

bool TVMExpressionCompiler::pushMemberOrLocalVarOrConstant(Identifier const &_identifier) {
	auto& stack = m_pusher.getStack();
	if (stack.isParam(_identifier.annotation().referencedDeclaration)) {
		// push(0, string(";; ") + m_stack.dumpParams());
		auto offset = stack.getOffset(_identifier.annotation().referencedDeclaration);
		if (offset == 0) {
			m_pusher.push(+1, "DUP");
		} else {
			m_pusher.pushS(offset);
		}
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
	if (!isCurrentResultNeeded()) {
		m_pusher.drop(1);
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
			m_pusher.push(0, "SWAP");                                // ... index index dict
			TypePointer const dictKey = StackPusherHelper::parseIndexType(indexAccess->baseExpression().annotation().type);
			m_pusher.prepareKeyForDictOperations(dictKey); // ..index index' dict
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
		acceptExpr(&_node.subExpression());
		m_pusher.push(0, "NOT");
	} else if (op == Token::Sub) {
		acceptExpr(&_node.subExpression());
		m_pusher.push(0, "NEGATE");
	} else if (op == Token::BitNot) {
		acceptExpr(&_node.subExpression());
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
	auto r = to<Literal>(&_node.rightExpression());
	if (!r) {
		return false;
	}
	if (r->token() != Token::Number) {
		return false;
	}
	if (r->annotation().type->category() != Type::Category::Integer && r->annotation().type->category() != Type::Category::RationalNumber) {
		return false;
	}
	if (op == Token::NotEqual || op == Token::Equal || op == Token::GreaterThan || op == Token::LessThan) {
		const auto* type = getType(r);
		u256 value = type->literalValue(r);
		if (value < 128) {
			acceptExpr(&_node.leftExpression());
			switch (op) {
				case Token::NotEqual:
					m_pusher.push(-1 + 1, "NEQINT " + toString(value));
					break;
				case Token::Equal:
					m_pusher.push(-1 + 1, "EQINT " + toString(value));
					break;
				case Token::GreaterThan:
					m_pusher.push(-1 + 1, "GTINT " + toString(value));
					break;
				case Token::LessThan:
					m_pusher.push(-1 + 1, "LESSINT " + toString(value));
					break;
				default:
					solAssert(false, "");
			}
			return true;
		}
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
		acceptExpr(&lexp);
		acceptExpr(&rexp);
	};

	if (isString(lt) && isString(rt)) {
		if (op == Token::Add) {
			acceptExpr(&lexp);
			m_pusher.push(+1-1,"CTOS");
			acceptExpr(&rexp);
			m_pusher.push(+1-1,"CTOS");
			m_pusher.exchange(0, 1);
			m_pusher.push(+1, "NEWC");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(-1, "STSLICE");
			m_pusher.push(+1-1,"ENDC");
		} else if (TokenTraits::isCompareOp(op)){
			acceptExpr(&lexp);
			m_pusher.push(+1-1,"CTOS");
			acceptExpr(&rexp);
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
		acceptExpr(order[0]);
		for (int i = 1; i < static_cast<int>(order.size()); ++i) {
			if (to<Identifier>(order[i]) || to<Literal>(order[i])) {
				acceptExpr(order[i]);
				m_pusher.push(-1, op == Token::Or? "OR" : "AND");
			} else {
				m_pusher.push(0, ";; short-circuiting " + std::string{op == Token::Or? "||" : "&&"});
				m_pusher.push(+1, "DUP");
				m_pusher.push(-1, ""); // fix stack for if

				m_pusher.startContinuation();
				m_pusher.push(-1, "DROP");
				acceptExpr(order[i]);
			}
		}

		for (int i = static_cast<int>(order.size()) - 1; i >= 1; --i) {
			if (to<Identifier>(order[i]) || to<Literal>(order[i])) {
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

	// DEPTH - 5 it's transaction id. Check that it's int msg
	// DEPTH - 3 it's message cell
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
	} else {
		cast_error(_node, "Unsupported magic");
	}
}

bool TVMExpressionCompiler::visitMagic(MemberAccess const &_node) {
	auto identifier = to<Identifier>(&_node.expression());
	if (!identifier) {
		return false;
	}

	if (identifier->name() == "msg") {
		visitMsgMagic(_node);
	} else {
		cast_error(_node, "Unsupported magic");
	}
	return true;
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
		visitMagic(_node);
		return ;
	}
	if (checkForAddressMemberAccess(_node, category)) return;

	if (category == Type::Category::Array)
		return visitMemberAccessArray(_node);

	if (category == Type::Category::FixedBytes)
		return visitMemberAccessFixedBytes(_node, to<FixedBytesType>(getType(&_node.expression())));

	if (auto type = to<TypeType>(_node.expression().annotation().type)) {
		if (auto enumType = dynamic_cast<EnumType const *>(type->actualType())) {
			unsigned int value = enumType->memberValue(_node.memberName());
			m_pusher.push(+1, "PUSHINT " + toString(value));
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
		acceptExpr(&_node.expression());
		m_pusher.pushInt(3);
		m_pusher.push(-1, "SDSKIPFIRST");
		m_pusher.push(0, "PLDI 8");
		return true;

	}
	if (_node.memberName() == "value") {
		acceptExpr(&_node.expression());
		m_pusher.pushInt(3 + 8);
		m_pusher.push(-1, "SDSKIPFIRST");
		m_pusher.push(0, "PLDU 256");
		return true;
	}
	return false;
}

void TVMExpressionCompiler::visitMemberAccessArray(MemberAccess const &_node) {
	if (_node.memberName() == "length") {
		auto arrayType = to<ArrayType>(_node.expression().annotation().type);
		if (!arrayType || arrayType->isByteArray()) {
			cast_error(_node, "Unsupported member access");
		}
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "FIRST");
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
	indexTypeCheck(indexAccess);
	compileNewExpr(indexAccess.indexExpression()); // index
	acceptExpr(&indexAccess.baseExpression()); // index container
	m_pusher.push(0, ";; index");
	Type const *baseType = indexAccess.baseExpression().annotation().type;
	if (baseType->category() == Type::Category::Array) {
		auto baseArrayType = to<ArrayType>(baseType);
		if (baseArrayType->isByteArray()) {
			// in function indexTypeCheck() there is cast_error
			solAssert(false, "");
		} else {
			// index array
			m_pusher.index(1);
		}
	}

	bool returnStructAsSlice = m_expressionDepth != 0;
	m_pusher.getFromDict(*StackPusherHelper::parseIndexType(baseType),
	                     *StackPusherHelper::parseValueType(indexAccess),
	                     indexAccess,
	                     baseType->category() == Type::Category::Mapping || baseType->category() == Type::Category::ExtraCurrencyCollection?
	                     StackPusherHelper::DictOperation::GetFromMapping :
	                     StackPusherHelper::DictOperation::GetFromArray,
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
					acceptExpr(argument.get());
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
					acceptExpr(argument.get()); // builder
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

int TVMExpressionCompiler::encodeOutboundMessageBody2(const string &name, const ast_vec<Expression const> &arguments,
                                                       const ast_vec<VariableDeclaration> &parameters,
                                                       const StackPusherHelper::ReasonOfOutboundMessage reason) {
	solAssert(m_expressionDepth == -1, "");
	m_isResultNeeded = true;
	return encodeOutboundMessageBody(name, arguments, parameters, reason);
}

int TVMExpressionCompiler::encodeOutboundMessageBody(const string &name, const ast_vec<Expression const> &arguments,
                                                      const ast_vec<VariableDeclaration> &parameters,
                                                      const StackPusherHelper::ReasonOfOutboundMessage reason) {
	solAssert(parameters.size() == arguments.size(), "");
	auto& stack = m_pusher.getStack();
	auto savedStackSize = stack.size();

	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (const auto & argument : parameters) {
		types.push_back(argument->annotation().type);
		nodes.push_back(argument.get());
	}

	int bodySize = m_pusher.encodeFunctionAndParams(
			name,
			types,
			nodes,
			[&](size_t idx) {
				m_pusher.push(0, ";; " + parameters[idx]->name());
				acceptExpr(arguments[idx].get());
			},
			reason
	);

	stack.ensureSize(savedStackSize + 1, "encodeRemoteFunctionCall");
	return bodySize;
}

bool TVMExpressionCompiler::checkRemoteMethodCall(FunctionCall const &_functionCall) {
	const ast_vec<Expression const> arguments = _functionCall.arguments();
	if (auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression())) {
		if (isCurrentResultNeeded())
			cast_error(_functionCall, "Calls to remote contract do not return result.");

		auto memberAccess = to<MemberAccess>(&functionOptions->expression());
		if (!memberAccess) {
			cast_error(_functionCall, "Unsupported usage of function call options.");
		}

		// parse options they are stored in two vectors: names and options
		std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
		for (const auto &option: optionNames)
			if (!isIn(*option, "flag", "value", "currencies"))
				cast_error(_functionCall, "Unsupported function call option: " + *option);

		std::map<int, Expression const *> exprs;
		std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"},
		                                          {TvmConst::int_msg_info::bounce,       "1"}};
		Expression const* sendrawmsgFlag{};


		// Search for currencies option
		auto currenciesIt = std::find_if(optionNames.begin(), optionNames.end(),
		                                 [](auto el) { return *el == "currencies"; });
		if (currenciesIt != optionNames.end()) {
			size_t index = currenciesIt - optionNames.begin();
			exprs[TvmConst::int_msg_info::currency] = functionOptions->options()[index].get();
		} else {
			constParams[TvmConst::int_msg_info::currency] = "0";
		}

		// Search for value (gram) option
		auto valueIt = std::find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "value"; });
		if (valueIt != optionNames.end()) {
			size_t index = valueIt - optionNames.begin();
			exprs[TvmConst::int_msg_info::grams] = functionOptions->options()[index].get();
		} else {
			constParams[TvmConst::int_msg_info::grams] = StackPusherHelper::gramsToBinaryString(10'000'000);
		}

		// remote_addr
		exprs[TvmConst::int_msg_info::dest] = &memberAccess->expression();

		auto pushBody = [&]() {
			const FunctionDefinition *fdef = getRemoteFunctionDefinition(memberAccess);
			solAssert(fdef, "");
			const auto &functionName = memberAccess->memberName();
			return encodeOutboundMessageBody(functionName, arguments, fdef->parameters(),
			                          StackPusherHelper::ReasonOfOutboundMessage::RemoteCallInternal);
		};


		// Search for senrawmsg flag option
		auto flagIt = std::find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "flag";});
		if (flagIt != optionNames.end()) {
			size_t index = flagIt - optionNames.begin();
			sendrawmsgFlag = functionOptions->options()[index].get();
		}

		if (sendrawmsgFlag)
			m_pusher.sendIntMsg(exprs, constParams, pushBody, [&](){acceptExpr(sendrawmsgFlag);});
		else
			m_pusher.sendIntMsg(exprs, constParams, pushBody, nullptr);
		return true;
	}


	std::map<int, Expression const *> exprs;
	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"},
	                                          {TvmConst::int_msg_info::bounce,       "1"}};
	constParams[TvmConst::int_msg_info::grams] = StackPusherHelper::gramsToBinaryString(10'000'000);
	Expression const *sendrawmsgFlag{};

	Expression const* currentExpression = &_functionCall.expression();
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
			exprs[TvmConst::int_msg_info::grams] = currentFunctionCall->arguments()[0].get();
			constParams.erase(TvmConst::int_msg_info::grams);
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
	const FunctionDefinition *fdef = getRemoteFunctionDefinition(memberValue);
	if (fdef == nullptr) {
		return false;
	}

	if (isCurrentResultNeeded()) {
		cast_error(_functionCall, "Calls to remote contract do not return result.");
	}

	auto pushBody = [&]() {
		return encodeOutboundMessageBody(memberValue->memberName(), arguments, fdef->parameters(),
		                          StackPusherHelper::ReasonOfOutboundMessage::RemoteCallInternal);
	};

	if (sendrawmsgFlag)
		m_pusher.sendIntMsg(exprs, constParams, pushBody, [&](){acceptExpr(sendrawmsgFlag);});
	else
		m_pusher.sendIntMsg(exprs, constParams, pushBody, nullptr);
	return true;
}

const FunctionDefinition *TVMExpressionCompiler::getRemoteFunctionDefinition(const MemberAccess *memberAccess) {
	auto expr = &memberAccess->expression();
	if (isSuper(expr))
		return nullptr;
	if (auto ctype = to<ContractType>(getType(expr))) {
		auto remoteContract = &ctype->contractDefinition();
		const string& fname = memberAccess->memberName();
		if (auto f = getFunction(remoteContract, fname)) {
			if (!m_pusher.ctx().getLocalFunction(fname))
				m_pusher.ctx().m_remoteFunctions.insert(fname);
			m_pusher.push( 0, ";; Remote call " + remoteContract->name() + "." + fname);
			return f;
		}
		cast_error(*memberAccess, "Unsupported remote function call.");
	} else {
		return nullptr;
	}
}

bool TVMExpressionCompiler::checkForArrayMethods(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	auto array = ma ? to<ArrayType>(ma->expression().annotation().type) : nullptr;
	if (!ma || !array || array->isString())
		return false;

	const LValueInfo lValueInfo = expandLValue(&ma->expression(), true);

	if (isCurrentResultNeeded()) {
		cast_error(ma->expression(), "Don't use result of push/pop functions");
	}

	if (ma->memberName() == "push") {
		auto arrayBaseType = to<ArrayType>(getType(&ma->expression()))->baseType();
		const IntegerType key = getKeyTypeOfArray();
		bool isValueBuilder{};
		if (_functionCall.arguments().empty()) {
			isValueBuilder = arrayBaseType->category() == Type::Category::Struct;
			m_pusher.pushDefaultValue(arrayBaseType, isValueBuilder);
		} else {
			acceptExpr(_functionCall.arguments()[0].get());
			isValueBuilder = false;
		}
		// stack: arr value
		m_pusher.push(0, ";; array.push(..)");
		isValueBuilder = m_pusher.prepareValueForDictOperations(&key, arrayBaseType, isValueBuilder); // arr value'
		m_pusher.exchange(0, 1); // value' arr
		m_pusher.push(-1 + 2, "UNPAIR");  // value' size dict
		m_pusher.push(+1, "PUSH S1"); // value' size dict size
		m_pusher.push(0, "INC"); // value' size dict newSize
		m_pusher.exchange(0, 3); // newSize size dict value'
		m_pusher.push(0, "ROTREV"); // newSize value' size dict
		m_pusher.setDict(key, *arrayBaseType, isValueBuilder, _functionCall); // newSize dict'
		m_pusher.push(-2 + 1, "PAIR");  // arr
	} else if (ma->memberName() == "pop") {
		// arr
		m_pusher.push(-1 + 2, "UNPAIR"); // size dict
		m_pusher.push(+1, "PUSH s1"); // size dict size
		m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
		m_pusher.push(0, "SWAP"); // dict size
		m_pusher.push(0, "DEC"); // dict newSize
		m_pusher.push(0, "DUP"); // dict newSize newSize
		m_pusher.push(+1, "ROT"); // newSize newSize dict
		m_pusher.pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
		m_pusher.push(-3 + 2, "DICTUDEL"); // newSize dict ?
		m_pusher.drop(1);  // newSize dict
		m_pusher.push(-2 + 1, "PAIR");  // arr
	} else {
		cast_error(*ma, "Unsupported function call for array");
	}

	collectLValue(lValueInfo, true, false);
	return true;
}

void TVMExpressionCompiler::mappingDelMin(FunctionCall const &_functionCall) {
	auto memberAccess = to<MemberAccess>(&_functionCall.expression());

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
	Type::Category valueCategory = valueType->category();
	const int keyLength = lengthOfDictKey(keyType);
	const std::string dictOpcode = "DICT" + typeToDictChar(keyType);

	auto mapIdentifier = to<Identifier>(&memberAccess->expression());
	if (mapIdentifier == nullptr) {
		cast_error(_functionCall.expression(), "Should be identifier");
	}

	acceptExpr(&memberAccess->expression()); // dict
	m_pusher.pushInt(keyLength); // dict nbits

	auto assignMap = [this, mapIdentifier, &keyType, &_functionCall]() {
		// D value key
		StackPusherHelper::restoreKeyAfterDictOperations(keyType, _functionCall);
		m_pusher.exchange(0, 2); // key value D
		const int saveStackSize = m_pusher.getStack().size();
		const LValueInfo lValueInfo = expandLValue(mapIdentifier, false);
		m_pusher.blockSwap(1, m_pusher.getStack().size() - saveStackSize);
		collectLValue(lValueInfo, true, false);
	};

	if (valueCategory == Type::Category::TvmCell) {
		// dict nbits
		m_pusher.push(-2 + 4, dictOpcode + "REMMINREF"); //  D value key -1
		m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
		assignMap(); // key value
	} else if (valueCategory == Type::Category::Struct) {
		if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
			// dict nbits
			m_pusher.push(-2 + 4, dictOpcode + "REMMIN"); //  D value key -1
			m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
			assignMap(); // key value
		} else {
			// dict nbits
			m_pusher.push(-2 + 4, dictOpcode + "REMMINREF"); //  D cellValue key -1
			m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
			assignMap(); // key cellValue
			m_pusher.push(0, "CTOS");
		}
		StructCompiler sc{&m_pusher, to<StructType>(valueType)};
		sc.convertSliceToTuple();
	} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(valueType)) {
		if (isByteArrayOrString(valueType)) {
			m_pusher.push(-2 + 4, dictOpcode + "REMMINREF"); //  D cellValue key -1
		} else {
			m_pusher.push(-2 + 4, dictOpcode + "REMMIN"); //  D value key -1
		}
		m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
		assignMap(); // key cellValue
	} else if (isIntegralType(valueType) || isUsualArray(valueType) || isIn(valueCategory, Type::Category::Mapping, Type::Category::VarInteger)) {
		// dict nbits
		m_pusher.push(-2 + 4, dictOpcode + "REMMIN"); //  D cellValue key -1
		m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
		assignMap(); // key cellValue
		m_pusher.preload(valueType);
	} else {
		cast_error(_functionCall, "Unsupported value type: ");
	}
}

void TVMExpressionCompiler::mappingFetchOrExists(FunctionCall const &_functionCall) {
	auto memberAccess = to<MemberAccess>(&_functionCall.expression());

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

	acceptExpr(_functionCall.arguments()[0].get()); // index
	acceptExpr(&memberAccess->expression()); // index dict

	if (memberAccess->memberName() == "fetch") {
		m_pusher.getFromDict(*keyType, *valueType, _functionCall, StackPusherHelper::DictOperation::Fetch, false);
	} else if (memberAccess->memberName() == "exists") {
		m_pusher.getFromDict(*keyType, *valueType, _functionCall,
		                     StackPusherHelper::DictOperation::Exist, false);
	} else {
		solAssert(false, "");
	}
}

void TVMExpressionCompiler::mappingPrevNextMethods(FunctionCall const &_functionCall, bool isNext) {
	auto expr = &_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);
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
	Type::Category valueCategory = valueType->category();
	const int keyLength = lengthOfDictKey(keyType);
	const std::string dictOpcode = std::string{"DICT"} + typeToDictChar(keyType) + "GET" + (isNext? "NEXT" : "PREV");

	acceptExpr(_functionCall.arguments()[0].get()); // index
	acceptExpr(&memberAccess->expression()); // index dict
	m_pusher.prepareKeyForDictOperations(keyType);
	m_pusher.pushInt(lengthOfDictKey(keyType)); // index dict nbits

	m_pusher.push(-3 + 3, dictOpcode); // value key -1 or 0


	StackPusherHelper pusherHelperOk(&m_pusher.ctx());
	StackPusherHelper::restoreKeyAfterDictOperations(keyType, _functionCall);
	pusherHelperOk.push(0, "SWAP"); // key value


	StackPusherHelper pusherHelperFail(&m_pusher.ctx());
	pusherHelperFail.pushDefaultValue(keyType);
	pusherHelperFail.pushDefaultValue(valueType);
	pusherHelperFail.push(0, "FALSE");


	if (isIn(valueCategory, Type::Category::Mapping, Type::Category::TvmCell)) {
		pusherHelperOk.push(0, "PLDREF");
	} else if (valueCategory == Type::Category::Struct) {
		if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
			// nothing
		} else {
			pusherHelperOk.push(0, "LDREFRTOS");
			pusherHelperOk.push(0, "NIP");
		}
		StructCompiler sc{&pusherHelperOk, to<StructType>(valueType)};
		sc.convertSliceToTuple();
	} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract, Type::Category::Array)) {
		if (valueCategory == Type::Category::Array && to<ArrayType>(valueType)->isByteArray()) {
			pusherHelperOk.push(0, "PLDREF");
		} else if (isUsualArray(valueType)){
			pusherHelperOk.preload(valueType);
		}
	} else if (isIntegralType(valueType) || valueCategory == Type::Category::VarInteger) {
		pusherHelperOk.preload(valueType);
	} else {
		cast_error(_functionCall, "Unsupported for mapping value type: " + valueType->toString(true));
	}

	pusherHelperOk.push(0, "TRUE");

	m_pusher.pushCont(pusherHelperOk.code());
	m_pusher.pushCont(pusherHelperFail.code());
	m_pusher.push(-2, ""); // fix stack
	m_pusher.push(0, "IFELSE");

}

void TVMExpressionCompiler::mappingMinMaxMethod(FunctionCall const &_functionCall, bool isMin) {
	auto expr = &_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);

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

	Type::Category valueCategory = valueType->category();
	const int keyLength = lengthOfDictKey(keyType);
	const std::string dictOpcode = "DICT" + typeToDictChar(keyType) + (isMin? "MIN" : "MAX");

	acceptExpr(&memberAccess->expression());
	m_pusher.pushInt(lengthOfDictKey(keyType)); // dict nbits

	auto f = [this, &keyType, &valueType, &valueCategory, &_functionCall, &keyLength]() {
		// (value, key, -1) or 0
		{
			// value key -1
			StackPusherHelper pusherHelperOk(&m_pusher.ctx());
			StackPusherHelper::restoreKeyAfterDictOperations(keyType, _functionCall);
			// value key
			pusherHelperOk.push(0, "SWAP"); // key value
			if (isIntegralType(valueType) || isUsualArray(valueType) || isIn(valueType->category(), Type::Category::Mapping, Type::Category::VarInteger)) {
				pusherHelperOk.preload(valueType);
			} else if (valueCategory == Type::Category::Struct) {
				if (!StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
					pusherHelperOk.push(0, "CTOS");
				}
				StructCompiler sc{&pusherHelperOk, to<StructType>(valueType)};
				sc.convertSliceToTuple();
			}
			pusherHelperOk.push(0, "TRUE");
			m_pusher.pushCont(pusherHelperOk.code());
		}
		{
			StackPusherHelper pusherHelperFail(&m_pusher.ctx());
			pusherHelperFail.pushDefaultValue(keyType);
			pusherHelperFail.pushDefaultValue(valueType);
			pusherHelperFail.push(0, "FALSE");
			m_pusher.pushCont(pusherHelperFail.code());
		}
		m_pusher.push(-2, "IFELSE");
	};

	if (valueCategory == Type::Category::TvmCell) {
		m_pusher.push(-2 + 3, dictOpcode + "REF");
		f(); // key value
	} else if (valueCategory == Type::Category::Struct) {
		if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
			m_pusher.push(-2 + 3, dictOpcode);
			f(); // key value
		} else {
			m_pusher.push(-2 + 3, dictOpcode + "REF");
			f(); // key value
		}
	} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(valueType)) {
		if (isByteArrayOrString(valueType)) {
			m_pusher.push(-2 + 3, dictOpcode + "REF");
			f(); // key value
		} else {
			m_pusher.push(-2 + 3, dictOpcode);
			f(); // key value
		}
	} else if (isIntegralType(valueType) || isUsualArray(valueType) || isIn(valueCategory, Type::Category::Mapping, Type::Category::VarInteger)) {
		m_pusher.push(-2 + 3, dictOpcode);
		f(); // key value
	} else {
		cast_error(_functionCall, "Unsupported value type: " + valueType->toString(true));
	}
}

void TVMExpressionCompiler::mappingEmpty(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	acceptExpr(&ma->expression());
	m_pusher.push(0, "DICTEMPTY");
}

bool TVMExpressionCompiler::checkForMappingOrCurrenciesMethods(FunctionCall const &_functionCall) {
	auto expr = &_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	if (ma == nullptr || (!to<MappingType>(ma->expression().annotation().type) && !to<ExtraCurrencyCollectionType>(ma->expression().annotation().type)))
		return false;

	m_pusher.push(0, ";; map." + ma->memberName());

	if (ma->memberName() == "delMin") {
		mappingDelMin(_functionCall);
	} else  if (isIn(ma->memberName(), "fetch", "exists")) {
		mappingFetchOrExists(_functionCall);
	} else if (isIn(ma->memberName(), "min", "max")) {
		mappingMinMaxMethod(_functionCall, ma->memberName() == std::string{"min"});
	} else if (isIn(ma->memberName(), "next", "prev")) {
		mappingPrevNextMethods(_functionCall, ma->memberName() == std::string{"next"});
	} else if (ma->memberName() == "empty") {
		mappingEmpty(_functionCall);
	} else {
		cast_error(_functionCall, "Unsupported");
	}

	return true;
}

void TVMExpressionCompiler::visit2(FunctionCall const &_functionCall) {
	if (checkForArrayMethods(_functionCall) || checkRemoteMethodCall(_functionCall)) {
		// To avoid situation when we call a function of a remote contract and don't save the result.
		// Remote function can return a result but in fact we don't get it.
		return;
	}

	if (!checkAbiMethodCall(_functionCall) &&
	    !checkForMappingOrCurrenciesMethods(_functionCall)) {
		FunctionCallCompiler fcc(m_pusher, this);
		fcc.compile(_functionCall);
	}

	if (!isCurrentResultNeeded()) {
		if (_functionCall.expression().annotation().type->category() == Type::Category::Function) {
			auto ft = to<FunctionType>(_functionCall.expression().annotation().type);
			auto size = ft->returnParameterNames().size();
			if (size != 0) {
				m_pusher.drop(size);
			}
			if (auto ma = to<MemberAccess>(&_functionCall.expression())) {
				if (ma->expression().annotation().type->category() == Type::Category::TvmSlice) {
					if (ma->memberName() == "decode") {
						auto types = TypePointers{_functionCall.annotation().type};
						m_pusher.drop(types.size());
					}
				}
			}
		}
	}
}

void TVMExpressionCompiler::visit2(Conditional const &_conditional) {
	acceptExpr(&_conditional.condition());
	m_pusher.push(-1, ""); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.trueExpression());
	m_pusher.endContinuation();
	m_pusher.push(-1, ""); // fix stack

	m_pusher.startContinuation();
	compileNewExpr(&_conditional.falseExpression());
	m_pusher.endContinuation();
	m_pusher.push(-1, ""); // fix stack

	m_pusher.push(+1, "IFELSE");
}

void TVMExpressionCompiler::visit2(ElementaryTypeNameExpression const &_node) {
	m_pusher.ensureValueFitsType(_node.type().typeName(), _node);
}

TVMExpressionCompiler::LValueInfo
TVMExpressionCompiler::expandLValue(Expression const *const _expr, const bool withExpandLastValue,
                                    bool willNoStackPermutarion, bool isLValue) {
	LValueInfo lValueInfo {};

	Expression const* expr = _expr;
	while (true) {
		lValueInfo.expressions.push_back(expr);
		if (to<Identifier>(expr)) {
			break;
		} else if (auto index2 = to<IndexAccess>(expr)) {
			indexTypeCheck(*index2);
			expr = &index2->baseExpression();
		} else if (auto memberAccess = to<MemberAccess>(expr); memberAccess && getType(&memberAccess->expression())->category() == Type::Category::Struct) {
			expr = &memberAccess->expression();
		} else if (to<FunctionCall>(expr) && !isLValue){
			acceptExpr(expr);
			lValueInfo.doesntNeedToCollect = true;
			return lValueInfo;
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
				if (!willNoStackPermutarion || stack.getOffset(variable->annotation().referencedDeclaration) != 0) {
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
				acceptExpr(index->indexExpression());
				// dict1 index
				m_pusher.push(0, "SWAP");
				// index dict1
				if (isLast && !withExpandLastValue) break;
				m_pusher.push(+2, "PUSH2 S1, S0");
				// index dict1 index dict1

				m_pusher.getFromDict(*StackPusherHelper::parseIndexType(index->baseExpression().annotation().type),
				                     *StackPusherHelper::parseValueType(*index), *index,
				                     StackPusherHelper::DictOperation::GetFromMapping, true);
				// index dict1 dict2
			} else if (index->baseExpression().annotation().type->category() == Type::Category::Array) {
				// array
				m_pusher.push(-1 + 2, "UNPAIR"); // size dict
				acceptExpr(index->indexExpression()); // size dict index
				m_pusher.push(0, "SWAP"); // size index dict
				m_pusher.push(+2, "PUSH2 s1,s2"); // size index dict index size
				m_pusher.push(-2 + 1, "LESS"); // size index dict index<size
				m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				if (isLast && !withExpandLastValue) {
					break;
				}
				m_pusher.push(+2, "PUSH2 S1, S0"); // size index dict index dict
				m_pusher.getFromDict(*StackPusherHelper::parseIndexType(index->baseExpression().annotation().type),
				                     *index->annotation().type, *index, StackPusherHelper::DictOperation::GetFromArray, true);
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
		} else {
			solAssert(false, "");
		}
	}
	m_pusher.push(0, "; end expValue");
	return lValueInfo;
}

void
TVMExpressionCompiler::collectLValue(const LValueInfo &lValueInfo, const bool haveValueOnStackTop,
                                     bool isValueBuilder) {
	// variable [arrayIndex | mapIndex | structMember]...

	m_pusher.push(0, "; colValue");
	if (lValueInfo.doesntNeedToCollect) {
		solAssert(haveValueOnStackTop, "Collect with not needed value is available only for on stack top values.");
		m_pusher.push(-1, "DROP");
		return;
	}

	const int n = static_cast<int>(lValueInfo.expressions.size());

	for (int i = n - 1; i >= 0; i--) {
		const bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());
		const bool isCurrentValueBuilder = (isLast && isValueBuilder) || (!isLast && lValueInfo.isResultBuilder[i + 1]);

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
					m_pusher.push(-1, "NIP"); // dict
				} else {
					// index dict value
					TypePointer const keyType = StackPusherHelper::parseIndexType(indexAccess->baseExpression().annotation().type);
					TypePointer const valueDictType = StackPusherHelper::parseValueType(*indexAccess);
					bool isMapValueBuilder = m_pusher.prepareValueForDictOperations(keyType, valueDictType, isCurrentValueBuilder);
					m_pusher.push(0, "ROTREV"); // value index dict
					m_pusher.prepareKeyForDictOperations(keyType);
					m_pusher.setDict(*keyType, *valueDictType, isMapValueBuilder, *lValueInfo.expressions[i]); // dict'
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
					bool isArrValueBuilder = m_pusher.prepareValueForDictOperations(keyType, valueDictType, isCurrentValueBuilder);
					m_pusher.push(0, "ROTREV"); // size value index dict
					m_pusher.prepareKeyForDictOperations(keyType);
					m_pusher.setDict(*keyType, *valueDictType, isArrValueBuilder, *lValueInfo.expressions[i]); // size dict'
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
				m_hasSideEffects = true;
				break;
			default:
				break;
		}
		return true;
	}

	bool visit(FunctionCall const&) override {
		// TODO: check if it is indeed function (e.g. not strucure) and
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

	auto push_rhs = [&] () {
		if (!m_pusher.tryImplicitConvert(getType(&lhs), getType(&rhs)))
			compileNewExpr(&rhs);
	};

	if (op == Token::Assign) {
		bool hasNoSideEffects =
			!TVMExpressionAnalyzer(lhs).hasSideEffects() &&
			!TVMExpressionAnalyzer(rhs).hasSideEffects();
		if (!isCurrentResultNeeded() && hasNoSideEffects) {
			const LValueInfo lValueInfo = expandLValue(&lhs, false);
			push_rhs();
			collectLValue(lValueInfo, true, false);
		} else {
			push_rhs();
			const int saveStackSize = m_pusher.getStack().size();
			const LValueInfo lValueInfo = expandLValue(&lhs, false);
			if (isCurrentResultNeeded()) {
				m_pusher.push(+1, "PUSH s" + toString(m_pusher.getStack().size() - saveStackSize));
			} else {
				m_pusher.blockSwap(1, m_pusher.getStack().size() - saveStackSize);
			}
			collectLValue(lValueInfo, true, false);
		}
	} else if (isString(getType(&lhs)) && isString(getType(&rhs))) {
		if (op == Token::AssignAdd) {
			acceptExpr(&rhs);
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

		acceptExpr(&rhs); // r
			// TODO: should compileNewExpr() be used here as above?
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

	acceptExpr(&_assignment.rightHandSide());
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
	if (expr->annotation().isPure && expr->annotation().type->category() == Type::Category::RationalNumber) {
		auto folding = dynamic_cast<RationalNumberType const *>(expr->annotation().type);
		if (!folding->isNegative())
			m_pusher.push(+1, "PUSHINT " + toString(folding->literalValue(nullptr)));
		else
			m_pusher.push(+1, "PUSHINT -" + toString(0 - folding->literalValue(nullptr)));
		return true;
	}
	return false;
}
