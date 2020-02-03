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

#include "TVMCommons.hpp"
#include "TVMIntrinsics.hpp"
#include "TVMFunctionCall.hpp"

namespace dev::solidity {

class TVMExpressionCompiler : public IExpressionCompiler, private StackPusherHelper {
private:
	ITVMCompiler* const m_compiler;
	int m_expressionDepth;
	bool m_isResultNeeded;

public:
	TVMExpressionCompiler(ITVMCompiler* compiler, const TVMCompilerContext& ctx) 
		: StackPusherHelper(compiler, &ctx)
		, m_compiler(compiler)
		, m_expressionDepth{-1}
		, m_isResultNeeded{false}
	{
	}

	void acceptExpr2(const Expression* expr, const bool _isResultNeeded) override {
		solAssert(expr, "");
		// Recursive call are not allowed.
		solAssert(m_expressionDepth == -1, "");
		m_isResultNeeded = _isResultNeeded;
		acceptExpr(expr);
		m_expressionDepth = -1;
	}

protected:
	void acceptExpr(const Expression* expr) override {
		const int savedExpressionDepth = m_expressionDepth;
		++m_expressionDepth;
		if (auto e = to<Literal>(expr)) {
			visit2(*e);
		} else if (auto e = to<Identifier>(expr)) {
			visit2(*e);
		} else if (auto e = to<BinaryOperation>(expr)) {
			visit2(*e);
		} else if (auto e = to<UnaryOperation>(expr)) {
			visit2(*e);
		} else if (auto e = to<Assignment>(expr)) {
			visit2(*e);
		} else if (auto e = to<TupleExpression>(expr)) {
			visit2(*e);
		} else if (auto e = to<MemberAccess>(expr)) {
			visit2(*e);
		} else if (auto e = to<IndexAccess>(expr)) {
			visit2(*e);
		} else if (auto e = to<FunctionCall>(expr)) {
			visit2(*e);
		} else if (auto e = to<Conditional>(expr)) {
			visit2(*e);
		} else if (auto e = to<ElementaryTypeNameExpression>(expr)) {
			visit2(*e);
		} else {
			cast_error(*expr, string("Unsupported expression ") + typeid(expr).name());
		}
		--m_expressionDepth;
		solAssert(savedExpressionDepth == m_expressionDepth, 
				  "Internal error: depth exp " + toString(savedExpressionDepth) + " got " + toString(m_expressionDepth));
	}

	bool isCurrentResultNeeded() {
		return m_expressionDepth >= 1 || m_isResultNeeded;
	}

	void visit2(Literal const& _node) {
		const auto* type = getType(&_node);
		switch (_node.annotation().type->category()) {
			case Type::Category::Integer:
			case Type::Category::RationalNumber: {
				if (to<RationalNumberType>(type) && to<RationalNumberType>(type)->isFractional()) {
					cast_error(_node, string("Unsupported type ") + type->canonicalName());
				}
				dev::u256 value = type->literalValue(&_node);
				push(+1, "PUSHINT " + toString(value));
				break;
			}
			case Type::Category::Address:
				literalToSliceAddress(&_node);
				break;
			case Type::Category::Bool:
				push(+1, _node.token() == Token::TrueLiteral? "TRUE" : "FALSE");
				break;
			case Type::Category::StringLiteral: {
				const std::string &str = _node.value();
				const int size = str.size();
				const int saveStackSize = getStack().size();
				if (size == 0) {
					push(+1, "NEWC");
					push(-1 + 1, "ENDC");
				} else {
				 const int step = TvmConst::CellBitLength / 8; // 127
					int builderQty = 0;
					int cntBlock = (size + step - 1) / step;
					for (int start = size - cntBlock * step; start < size; start += step, ++builderQty) {
						std::string slice;
						for (int index = max(0, start); index < start + step; ++index) {
							std::stringstream ss;
							ss << std::hex << std::setfill('0') << std::setw(2) << static_cast<unsigned>(str.at(index));
							slice += ss.str();
						}
						if (slice.size() > 124) { // there is bug in linker or in spec.
							push(+1, "NEWC");
							push(+1, "PUSHSLICE x" + slice.substr(0, 2 * (127 - 124)));
							push(-1, "STSLICER");
							push(+1, "PUSHSLICE x" + slice.substr(2 * (127 - 124)));
							push(-1, "STSLICER");
						} else {
							push(+1, "PUSHSLICE x" + slice);
							push(+1, "NEWC");
							push(-1, "STSLICE");
						}
					}
					--builderQty;
					while (builderQty --> 0) {
						push(-1, "STBREFR");
					}
					push(0, "ENDC");
				}
				getStack().ensureSize(saveStackSize + 1, "");
				break;
			}
			default:
				cast_error(_node, string("Unsupported type ") + type->canonicalName());
		}
	}

	void visit2(TupleExpression const& _tupleExpression) {
		if (_tupleExpression.isInlineArray()) {
			pushInt(_tupleExpression.components().size());
			push(+1, "NEWDICT");
			Type const* type = _tupleExpression.annotation().type.get();
			for (int i = 0; i < static_cast<int>(_tupleExpression.components().size()); ++i) {
				const IntegerType key = getKeyTypeOfArray();
				auto arrayBaseType = to<ArrayType>(type)->baseType().get();

				acceptExpr(_tupleExpression.components().at(i).get()); // totalSize dict value
				bool isValueBuilder = prepareValueForDictOperations(&key, arrayBaseType, false); // totalSize dict value'
				pushInt(i); // totalSize dict value' index
				push(0, "ROT"); // totalSize value' index dict
				setDict(key, *arrayBaseType, isValueBuilder, _tupleExpression); // totalSize dict
			}
			push(-2 + 1, "PAIR");
		} else {
			for (const auto &comp : _tupleExpression.components()) {
				acceptExpr(comp.get());
			}
		}
	}

	bool tryPushConstant(Identifier const& _identifier) {
		IdentifierAnnotation& identifierAnnotation = _identifier.annotation();
		Declaration const* declaration = identifierAnnotation.referencedDeclaration;
		const auto* variableDeclaration = to<VariableDeclaration>(declaration);
		if (!variableDeclaration || !variableDeclaration->isConstant()){
			return false;
		}
		acceptExpr(variableDeclaration->value().get());
		return true;
	}

	bool pushMemberOrLocalVarOrConstant(Identifier const& _identifier) {
		const string& name = _identifier.name();
		auto& stack = getStack();
		if (stack.isParam(name)) {
			// push(0, string(";; ") + m_stack.dumpParams());
			auto offset = stack.getOffset(name);
			if (offset == 0) {
				push(+1, "DUP");
			} else {
				pushS(offset);
			}
			return true;
		} else if (tryPushConstant(_identifier)) {
			return true;
		} else if (auto vd = to<VariableDeclaration>(_identifier.annotation().referencedDeclaration)) {
			pushPersistentDataCellTree();
			structCompiler().pushMember(vd->name());
			return true;
		}
		return false;
	}

protected:
	void visit2(Identifier const& _identifier) {
		const string& name = _identifier.name();
		push(0, string(";; push identifier ") + name);
//		dumpStackSize();
		if (pushMemberOrLocalVarOrConstant(_identifier)) {
		} else if (name == "now") {
			// Getting value of `now` variable
			push(+1, "NOW");
		} else if (name == "this") {
			// calling this.function() will create internal message that should be sent to the address of current contract
			push(0, "; call function of this");
			push(+1, "MYADDR");
		} else if (ctx().getLocalFunction(name)) {
			CodeLines code;
			code.push("CALL $" + name + "_internal$");
			pushCont(code);
		} else {
			cast_error(_identifier, "Unsupported identifier: " + name);
		}
		if (m_expressionDepth == 0 && !m_isResultNeeded) {
			drop(1);
		}
	}

	void compileUnaryOperation(UnaryOperation const& _node, const std::string& tvmUnaryOperation, const bool isPrefixOperation) {
		const int saveStackSize = getStack().size();
		if (isCurrentResultNeeded()) {
			const LValueInfo lValueInfo = expandLValue(&_node.subExpression(), true);
			const int expandedLValueSize = getStack().size() - saveStackSize - 1;
			solAssert(expandedLValueSize >= 0, "");
			if (isPrefixOperation) {
				push(0, tvmUnaryOperation);
				push(+1, "DUP"); // expanded.. value value
				if (expandedLValueSize != 0) {
					blockSwap(expandedLValueSize + 1, 1); // value expanded.. value
				}
			} else {
				push(+1, "DUP"); // expanded.. value value
				push(0, tvmUnaryOperation); // expanded.. value newValue
				if (expandedLValueSize) {
					exchange(0, 1); // expanded.. newValue value
					blockSwap(expandedLValueSize + 1, 1); // value expanded.. newValue
				}
			}
			checkBitFit(_node.annotation().type.get(), _node.annotation().type.get(), _node.annotation().type.get(), tvmUnaryOperation);
			collectLValue(lValueInfo);
		} else {
			const LValueInfo lValueInfo = expandLValue(&_node.subExpression(), true, true);
			push(0, tvmUnaryOperation);
			checkBitFit(_node.annotation().type.get(), _node.annotation().type.get(), _node.annotation().type.get(), tvmUnaryOperation);
			collectLValue(lValueInfo);
		}
	}

	void compileUnaryDelete(UnaryOperation const& node) {
		const LValueInfo lValueInfo = expandLValue(&node.subExpression(), false);
		Expression const* lastExpr = lValueInfo.expressions.back();
		if (to<Identifier>(lastExpr)) {
			Type const* exprType = node.subExpression().annotation().type.get();
			bool isValueBuilder = isUsualStruct(exprType);
			pushDefaultValue(exprType, isValueBuilder);
			collectLValue(lValueInfo, true, isValueBuilder);
		} else if (auto memberAccess = to<MemberAccess>(lastExpr)) {
			MemberAccessAnnotation& a = memberAccess->annotation();
			auto decl = to<VariableDeclaration>(a.referencedDeclaration);
			pushDefaultValue(decl->type().get(), true);
			collectLValue(lValueInfo, true, true);
		} else if (auto indexAccess = to<IndexAccess>(lastExpr)) {
			// ... index dict
			Type const* baseExprType = indexAccess->baseExpression().annotation().type.get();
			auto arrayType = to<ArrayType>(baseExprType);
			if (arrayType) {
				Type const* valueType = arrayType->baseType().get();
				pushDefaultValue(valueType, true); // index dict value
				collectLValue(lValueInfo, true, true);
			} else {
				push(+1, "PUSH S1");                            // ... index dict index
				push(0, "SWAP");                                // ... index index dict
				TypePointer const dictKey = parseIndexType(indexAccess->baseExpression().annotation().type.get());
				prepareKeyForDictOperations(dictKey.get()); // ..index index' dict
				pushInt(lengthOfDictKey(dictKey.get())); // ..index index dict nbits
				push(-3 + 2, "DICT" + typeToDictChar(dictKey.get()) + "DEL");  // ... index dict' {-1,0}
				push(-1, "DROP");                               // ... index dict'
				collectLValue(lValueInfo, false);
			}
		} else {
			solAssert(false, "");
		}
	}

	void visit2(UnaryOperation const& _node) {
		auto op = _node.getOperator();
		push(0, string(";; ") + TokenTraits::toString(op));
		if (op == Token::Inc) {
			compileUnaryOperation(_node, "INC", _node.isPrefixOperation());
		} else if (op == Token::Dec) {
			compileUnaryOperation(_node, "DEC", _node.isPrefixOperation());
		} else if (op == Token::Not) {
			acceptExpr(&_node.subExpression());
			push(0, "NOT");
		} else if (op == Token::Sub) {
			acceptExpr(&_node.subExpression());
			push(0, "NEGATE");
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
				push(0, "NOT");
			else {
				push(+1,"PUSHPOW2DEC " + to_string(numBits));
				push(-2+1, "SUBR");
			}
		} else if (op == Token::Delete) {
			compileUnaryDelete(_node);
		} else {
			cast_error(_node, toString("Unsupported operation: ") + TokenTraits::toString(op));
		}
	}

	static bool argumentsIsGoodForFixedBytes(Type const* a, Type const* b) {
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

	static string compareAddresses(Token op) {
		if (op == Token::GreaterThan)
			return "SDLEXCMP ISPOS";
		if (op == Token::GreaterThanOrEqual)
			return "SDLEXCMP ISNNEG";
		if (op == Token::LessThan)
			return "SDLEXCMP ISNEG";
		if (op == Token::LessThanOrEqual)
			return "SDLEXCMP ISNPOS";
		if (op == Token::Equal)
			return "SDEQ";
		if (op == Token::NotEqual)
			return "SDEQ NOT";
		solAssert(false, "Wrong compare operation");
	}

	bool tryOptimizeBinaryOperation(BinaryOperation const& _node) {
		Token op = _node.getOperator();
		auto r = to<Literal>(&_node.rightExpression());
		if (!r) {
			return false;
		}
		if (r->token() != Token::Number) {
			return false;
		}
		if (op == Token::NotEqual || op == Token::Equal || op == Token::GreaterThan || op == Token::LessThan) {
			int value;
			try {
				value = boost::lexical_cast<int>(r->valueWithoutUnderscores());
			} catch (...) {
				return false;
			}
			if (-128 <= value && value < 128) {
				acceptExpr(&_node.leftExpression());
				switch (op) {
					case Token::NotEqual:
						push(-1 + 1, "NEQINT " + toString(value));
						break;
					case Token::Equal:
						push(-1 + 1, "EQINT " + toString(value));
						break;
					case Token::GreaterThan:
						push(-1 + 1, "GTINT " + toString(value));
						break;
					case Token::LessThan:
						push(-1 + 1, "LESSINT " + toString(value));
						break;
					default:
						solAssert(false, "");
				}
				return true;
			}
		}
		return false;
	}

	void visit2(BinaryOperation const& _node) {
		Type const* lt = _node.leftExpression().annotation().type.get();
		Type const* rt = _node.leftExpression().annotation().type.get();
		if (!argumentsIsGoodForFixedBytes(lt, rt) || lt->category() == Type::Category::Function || rt->category() == Type::Category::Function) {
			cast_error(_node, "Unsupported binary operation");
		}

		if (tryOptimizeBinaryOperation(_node)) {
			return;
		}

		const Token op = _node.getOperator();
		const bool isLeftAddress = isIn(lt->category(),  Type::Category::Address, Type::Category::Contract);
		const bool isRightAddress = isIn(rt->category(),  Type::Category::Address, Type::Category::Contract);
		if (isLeftAddress || isRightAddress) {
			acceptExpr(&_node.leftExpression());
			acceptExpr(&_node.rightExpression());
			push(-1, compareAddresses(op));
			return;
		}

		acceptExpr(&_node.leftExpression());
		if ((op == Token::And || op == Token::Or) && !to<Identifier>(&_node.rightExpression()) && !to<Literal>(&_node.rightExpression()) ){
			push(0, ";; shortCirc");
			push(0, "DUP");
			push(0, "PUSHCONT {");
			acceptExpr(&_node.rightExpression());
			push(-1, op == Token::Or? "OR" : "AND");
			push(+1, "}");
			push(-1, op == Token::Or? "IFNOT" : "IF");
			return;
		}
		acceptExpr(&_node.rightExpression());

		push(0, string(";; ") + TokenTraits::toString(_node.getOperator()));
		if (op == Token::Exp) {
			// TODO: this code is hard to understand. Would it be better to move it to stdlib?
			push(0, "SWAP");
			push(0, "PUSHINT 1");
			push(0, "PUSHCONT {");
			push(0, "\tPUSH s2");
			push(0, "\tPUSHINT 0");
			push(0, "\tGREATER");
			push(0, "\tNOT DUP IFRET DROP");
			push(0, "\tPUSH s2");
			push(0, "\tPUSHINT 1");
			push(0, "\tAND");
			push(0, "\tPUSHINT 1");
			push(0, "\tEQUAL");
			push(0, "\tPUSHCONT {");
			push(0, "\t\tDUP");
			push(0, "\t\tPUSH s2");
			push(0, "\t\tMUL");
			push(0, "\t\tNIP");
			push(0, "\t}");
			push(0, "\tIF");
			push(0, "\tPUSH s2");
			push(0, "\tPUSHINT 1");
			push(0, "\tRSHIFT");
			push(0, "\tPOP s3");
			push(0, "\tPUSH s1");
			push(0, "\tPUSH s2");
			push(0, "\tMUL");
			push(0, "\tPOP s2");
			push(0, "\tFALSE");
			push(0, "}");
			push(0, "UNTIL");
			push(0, "NIP NIP");	// remove operands
			push(-2+1, "");	// fixup stack
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
		else if (op == Token::And || op == Token::BitAnd) cmd = "AND";
		else if (op == Token::Or  || op == Token::BitOr) cmd = "OR";
		else if (op == Token::SHL) { cmd = "LSHIFT"; checkOverflow = true; }
		else if (op == Token::SAR) cmd = "RSHIFT";
		else if (op == Token::BitXor) cmd = "XOR";
		else {
			cast_error(_node, "Unsupported binary operation");
		}
		push(-1, cmd);
		if (checkOverflow) {
			Type const* type = _node.annotation().type.get();
			checkBitFit(type, _node.leftExpression().annotation().type.get(), _node.rightExpression().annotation().type.get(), cmd);
		}
	}

	void checkBitFit(Type const* type, Type const* lType, Type const* rType, const std::string& opcode) {
		if (ctx().ignoreIntegerOverflow()) {
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
			push(0, "FITS " + toString(ti.numBits));
		} else {
			auto intL = to<IntegerType>(lType);
			auto intR = to<IntegerType>(rType);
			auto intResult = to<IntegerType>(type);
			bool isCheckNotNeeded = intL && intR && intResult &&
					!intL->isSigned() && !intR->isSigned() && !intResult->isSigned() &&
					intResult->numBits() == 256 &&
					isIn(opcode, "ADD", "MULL", "LSHIFT", "INC");
			if (!isCheckNotNeeded) { // maximal value for int257 is the same as for uint256
				push(0, "UFITS " + toString(ti.numBits));
			}
		}
	}

	bool checkForMagic(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::Magic)
			return false;
		auto expr = to<Identifier>(&_node.expression());
		if (expr != nullptr && expr->name() == "msg") {
			if (_node.memberName() == "sender") { // msg.sender
				pushPrivateFunctionOrMacroCall(+1, "sender_address_macro");
				return true;
			}
			if (_node.memberName() == "value") { // msg.value
				pushPrivateFunctionOrMacroCall(+1, "message_balance_macro");
				return true;
			}
		}
		cast_error(_node, "Unsupported magic");
		return false;
	}
	
	void visit2(MemberAccess const& _node) {
		const std::string& memberName = _node.memberName();
		push(0, ";; get member " + memberName);
		auto category = getType(&_node.expression())->category();
		if (category == Type::Category::Struct) {
			acceptExpr(&_node.expression());
			auto structType = to<StructType>(_node.expression().annotation().type.get());
			StructCompiler structCompiler{this, structType};
			structCompiler.pushMember(memberName);
			return;
		}

		if (checkForMagic(_node, category)) return; 
		if (checkForAddressMemberAccess(_node, category)) return;
		
		if (category == Type::Category::Array) 
			return visitMemberAccessArray(_node);

		if (category == Type::Category::FixedBytes)
			return visitMemberAccessFixedBytes(_node, to<FixedBytesType>(getType(&_node.expression())));
		
		auto type = to<TypeType>(_node.expression().annotation().type.get());
		if (auto enumType = dynamic_cast<EnumType const*>(type->actualType().get())) {
			unsigned int value = enumType->memberValue(_node.memberName());
			push(+1, "PUSHINT " + toString(value));
			return;
		}
		
		cast_error(_node, "Not supported");
	}
	
	bool checkForAddressMemberAccess(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::Address)
			return false;
		if (_node.memberName() == "balance") {
			if (!isAddressThis(to<FunctionCall>(&_node.expression())))
				cast_error(_node.expression(), "only 'address(this).balance' is supported for balance");
			pushPrivateFunctionOrMacroCall(+1, "get_contract_balance_macro");
			return true;
		}
		if (_node.memberName() == "wid") {
			acceptExpr(&_node.expression());
			pushInt(3);
			push(-1, "SDSKIPFIRST");
			push(0, "PLDI 8");
			return true;
			
		}
		if (_node.memberName() == "value") {
			acceptExpr(&_node.expression());
			pushInt(3 + 8);
			push(-1, "SDSKIPFIRST");
			push(0, "PLDU 256");
			return true;
		}
		return false;
	}

	void visitMemberAccessArray(MemberAccess const& _node) {
		if (_node.memberName() == "length") {
			auto arrayType = to<ArrayType>(_node.expression().annotation().type.get());
			if (!arrayType || arrayType->isByteArray()) {
				cast_error(_node, "Unsupported member access");
			}
			acceptExpr(&_node.expression());
			push(-1 + 1, "FIRST");
		} else {
			cast_error(_node, "Unsupported member access");
		}
	}

	void visitMemberAccessFixedBytes(MemberAccess const& _node, FixedBytesType const* fbt) {
		if (_node.memberName() == "length" && to<Identifier>(&_node.expression())) {
			pushInt(static_cast<int>(fbt->storageBytes()));
			return;
		}
		cast_error(_node, "Unsupported");
	}

	static void indexTypeCheck(IndexAccess const& _node) {
		Type const* baseExprType = _node.baseExpression().annotation().type.get();
		auto arrayType = to<ArrayType>(baseExprType);
		Type::Category baseExprCategory = _node.baseExpression().annotation().type->category();
		if ((baseExprCategory != Type::Category::Array && baseExprCategory != Type::Category::Mapping) || (arrayType && arrayType->isByteArray())) {
			cast_error(_node, "Index access is supported only for dynamic arrays and mappings");
		}
	}

	void visit2(IndexAccess const& _node) {
		indexTypeCheck(_node);
		acceptExpr(_node.indexExpression()); // index
		acceptExpr(&_node.baseExpression()); // index container
		push(0, ";; index");
		Type const *baseType = _node.baseExpression().annotation().type.get();
		if (baseType->category() == Type::Category::Array) {
			auto baseArrayType = to<ArrayType>(baseType);
			if (baseArrayType->isByteArray()) {
				// in function indexTypeCheck() there is cast_error
				solAssert(false, "");
			} else {
				// index array
				push(-1 + 2, "UNPAIR"); // index size dict
				dropUnder(1, 1); // index dict
			}
		}
		getFromDict(*parseIndexType(baseType).get(), *getType(&_node), _node, baseType->category() == Type::Category::Mapping);
	}

	bool checkAbiMethodCall(FunctionCall const& _functionCall) {
		// compile "abi.encodePacked()"
		if (auto memberAccess = to<MemberAccess>(&_functionCall.expression())) {
			auto identifier = to<Identifier>(&memberAccess->expression());
			if (identifier != nullptr && identifier->name() == "abi") {
				if (memberAccess->memberName() != "encodePacked") {
					cast_error(_functionCall, "Only method encodePacked is supported for abi");
				}

				int builderSize = 0;
				bool haveArray = false;
				push(+1, "NEWC");
				for (const ASTPointer<Expression const>& argument : _functionCall.arguments()) {
					const Type *type = getType(argument.get());
					if (to<IntegerType>(type) != nullptr) {
						acceptExpr(argument.get());
						push(-1, storeIntegralOrAddress(type, true));
						builderSize += TypeInfo{type}.numBits;
					} else if (auto array = to<ArrayType>(type)) {
						if (to<IntegerType>(array->baseType().get()) == nullptr) {
							cast_error(*argument.get(), "Only numeric array is supported for abi.encodePacked(...) ");
						}
						if (_functionCall.arguments().size() != 1) {
							cast_error(_functionCall, "Only one array is supported for abi.encodePacked(...)");
						}
						TypeInfo arrayElementTypeInfo(array->baseType().get());
						acceptExpr(argument.get()); // builder
						push(-1 + 2, "UNPAIR"); // builder size dict
						pushInt(arrayElementTypeInfo.numBits); // builder size dict valueLength
						pushPrivateFunctionOrMacroCall(-4 + 1, "abi_encode_packed_macro");
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
				push(0, "ENDC");
				return true;
			}
		}
		return false;
	}

public:
	void encodeOutboundMessageBody2(
			const string& name,
			const ptr_vec<Expression const>&	arguments,
			const ptr_vec<VariableDeclaration>&	parameters,
			const StackPusherHelper::ReasonOfOutboundMessage reason)
	{
		solAssert(m_expressionDepth == -1, "");
		m_isResultNeeded = true;
		encodeOutboundMessageBody(name, arguments, parameters, reason);
	}

protected:
	void encodeOutboundMessageBody(
			const string& name,
			const ptr_vec<Expression const>&	arguments,
			const ptr_vec<VariableDeclaration>&	parameters,
			const StackPusherHelper::ReasonOfOutboundMessage reason)
	{
		solAssert(parameters.size() == arguments.size(), "");
		auto& stack = getStack();
		auto savedStackSize = stack.size();

		std::vector<Type const*> types;
		std::vector<ASTNode const*> nodes;
		for (const auto & argument : parameters) {
			types.push_back(argument->annotation().type.get());
			nodes.push_back(argument.get());
		}

		encodeFunctionAndParams(
				name,
				types,
				nodes,
				[&](size_t idx) {
					push(0, ";; " + parameters[idx]->name());
					acceptExpr(arguments[idx].get());
				},
				reason
		);

		stack.ensureSize(savedStackSize + 1, "encodeRemoteFunctionCall");
	}

	bool checkRemoteMethodCallWithValue(FunctionCall const& _functionCall) {
		// TODO: simplify this code
		const ptr_vec<Expression const> arguments = _functionCall.arguments();
		if (auto functionValue = to<FunctionCall>(&_functionCall.expression())) {
			auto argumentsValue = functionValue->arguments();
			if (argumentsValue.size() != 1)
				return false;
			if (auto memberValue = to<MemberAccess>(&functionValue->expression())) {
				if (memberValue->memberName() != "value")
					return false;
				if (auto memberAccess = to<MemberAccess>(&memberValue->expression())) {
					acceptExpr(argumentsValue[0].get());
					acceptExpr(&memberAccess->expression());
					if (const FunctionDefinition* fdef = m_compiler->getRemoteFunctionDefinition(memberAccess)) {
						auto fn = TVMCompilerContext::getFunctionExternalName(memberAccess->memberName());
						if (isCurrentResultNeeded())
							cast_error(_functionCall, "Calls to remote contract do not return result.");
						encodeOutboundMessageBody(fn, arguments, fdef->parameters(), StackPusherHelper::ReasonOfOutboundMessage::RemoteCallInternal);
						pushPrivateFunctionOrMacroCall(-3, "send_internal_message_macro");
//						dumpStackSize();
						return true;
					}
				}
			}
		}
		return false;
	}

	bool checkRemoteMethodCall(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		if (auto memberAccess = to<MemberAccess>(&_functionCall.expression())) {
			if (const FunctionDefinition* fdef = m_compiler->getRemoteFunctionDefinition(memberAccess)) {
				pushInt(10'000'000); // // 10_000_000 is 10_000 * gas price (1000)
				acceptExpr(&memberAccess->expression());
				auto fn = TVMCompilerContext::getFunctionExternalName(memberAccess->memberName());
				if (isCurrentResultNeeded())
					cast_error(_functionCall, "Calls to remote contract do not return result.");
				encodeOutboundMessageBody(fn, arguments, fdef->parameters(), StackPusherHelper::ReasonOfOutboundMessage::RemoteCallInternal);
				pushPrivateFunctionOrMacroCall(-3, "send_internal_message_macro");
				return true;
			}
		}
		return false;
	}
	
	bool checkForArrayMethods(FunctionCall const& _functionCall) {
		auto expr = &_functionCall.expression();
		auto ma = to<MemberAccess>(expr);
		if (!ma || !to<ArrayType>(ma->expression().annotation().type.get()))
			return false;
		
		const LValueInfo lValueInfo = expandLValue(&ma->expression(), true);
		
		for (const auto& arg : _functionCall.arguments())
			acceptExpr(arg.get());
		
		if (isCurrentResultNeeded()) {
			cast_error(ma->expression(), "arr.push(..) and arr.pop(..) are supported only in simple expressions");
		}
		
		if (ma->memberName() == "push") {
			auto arrayBaseType = to<ArrayType>(getType(&ma->expression()))->baseType().get();
			const IntegerType key = getKeyTypeOfArray();

			// stack: arr value
			push(0, ";; array.push(..)");
			bool isValueBuilder = prepareValueForDictOperations(&key, arrayBaseType, false); // arr value'
			exchange(0, 1); // value' arr
			push(-1 + 2, "UNPAIR");  // value' size dict
			push(+1, "PUSH S1"); // value' size dict size
			push(0, "INC"); // value' size dict newSize
			exchange(0, 3); // newSize size dict value'
			push(0, "ROTREV"); // newSize value' size dict
			setDict(key, *arrayBaseType, isValueBuilder, _functionCall); // newSize dict'
			push(-2 + 1, "PAIR");  // arr
		} else if (ma->memberName() == "pop") {
			// arr
			push(-1 + 2, "UNPAIR"); // size dict
			push(+1, "PUSH s1"); // size dict size
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
			push(0, "SWAP"); // dict size
			push(0, "DEC"); // dict newSize
			push(0, "DUP"); // dict newSize newSize
			push(+1, "ROT"); // newSize newSize dict
			pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
			push(-3 + 2, "DICTUDEL"); // newSize dict ?
			drop(1);  // newSize dict
			push(-2 + 1, "PAIR");  // arr
		} else {
			cast_error(*ma, "Unsupported function call");
		}
		
		collectLValue(lValueInfo, true, false);
		return true;
	}

	void mappingDelMin(FunctionCall const& _functionCall) {
		auto ma = to<MemberAccess>(&_functionCall.expression());
		auto mapType = to<MappingType>(ma->expression().annotation().type.get());
		Type const* keyType = mapType->keyType().get();
		Type const* valueType = mapType->valueType().get();
		Type::Category valueCategory = mapType->valueType()->category();
		const int keyLength = lengthOfDictKey(keyType);
		const std::string dictOpcode = "DICT" + typeToDictChar(keyType);

		auto mapIdentifier = to<Identifier>(&ma->expression());
		if (mapIdentifier == nullptr) {
			cast_error(_functionCall.expression(), "Should be identifier");
		}

		acceptExpr(&ma->expression()); // dict
		pushInt(keyLength); // dict nbits

		auto assignMap = [this, mapIdentifier, &keyType, &_functionCall]() {
			// D value key
			restoreKeyAfterDictOperations(keyType, _functionCall);
			exchange(0, 2); // key value D
			const int saveStackSize = getStack().size();
			const LValueInfo lValueInfo = expandLValue(mapIdentifier, false);
			blockSwap(1, getStack().size() - saveStackSize);
			collectLValue(lValueInfo);
		};

		if (isIn(valueCategory, Type::Category::Mapping, Type::Category::TvmCell)) {
			// dict nbits
			push(-2 + 4, dictOpcode + "REMMINREF"); //  D value key -1
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
			assignMap(); // key value
		} else if (valueCategory == Type::Category::Struct) {
			if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
				// dict nbits
				push(-2 + 4, dictOpcode + "REMMIN"); //  D value key -1
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
				assignMap(); // key value
			} else {
				// dict nbits
				push(-2 + 4, dictOpcode + "REMMINREF"); //  D cellValue key -1
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
				assignMap(); // key cellValue
				push(0, "CTOS");
			}
		} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(valueType)) {
			if (isByteArrayOrString(valueType)) {
				push(-2 + 4, dictOpcode + "REMMINREF"); //  D cellValue key -1
			} else {
				push(-2 + 4, dictOpcode + "REMMIN"); //  D value key -1
			}
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
			assignMap(); // key cellValue
		} else if (isIntegralType(valueType) || isUsualArray(valueType)) {
			// dict nbits
			push(-2 + 4, dictOpcode + "REMMIN"); //  D cellValue key -1
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::DelMinFromEmptyMap));
			assignMap(); // key cellValue
			preload(valueType);
		} else {
			cast_error(_functionCall, "Unsupported value type: ");
		}
	}

	void mappingFetchOrExists(FunctionCall const& _functionCall) {
		auto memberAccess = to<MemberAccess>(&_functionCall.expression());
		auto mapType = to<MappingType>(memberAccess->expression().annotation().type.get());
		Type const* keyType = mapType->keyType().get();
		Type const* valueType = mapType->valueType().get();
		Type::Category valueCategory = mapType->valueType()->category();
		const int keyLength = lengthOfDictKey(keyType);
		const std::string dictOpcode = "DICT" + typeToDictChar(keyType);

		acceptExpr(_functionCall.arguments()[0].get()); // index
		acceptExpr(&memberAccess->expression()); // index dict
		prepareKeyForDictOperations(keyType);
		pushInt(keyLength); // index dict nbits

		StackPusherImpl2 pusher;
		StackPusherHelper pusherHelper(&pusher, &ctx());

		if (isIn(valueCategory, Type::Category::Mapping, Type::Category::TvmCell)) {
			push(-3 + 2, dictOpcode + "GETREF");
			push(0, "DUP");
			pusherHelper.push(0, "SWAP");
		} else if (valueCategory == Type::Category::Struct) {
			if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
				// index dict nbits
				push(-3 + 2, dictOpcode + "GET"); // [value] {-1, 0}
				push(0, "DUP");
				pusherHelper.push(0, "SWAP");
			} else {
				push(-3 + 2, dictOpcode + "GETREF");
				push(0, "DUP");
				pusherHelper.push(0, "SWAP");
				pusherHelper.push(0, "CTOS");
			}
		} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(valueType)) {
			if (isByteArrayOrString(valueType)) {
				push(-3 + 2, dictOpcode + "GETREF");
			} else {
				push(-3 + 2, dictOpcode + "GET");
			}
			push(0, "DUP");
			pusherHelper.push(0, "SWAP");
		} else if (isIntegralType(valueType) || isUsualArray(valueType)) {
			push(-3 + 2, dictOpcode + "GET");
			push(0, "DUP");
			pusherHelper.push(0, "SWAP");
			pusherHelper.preload(valueType);
		} else {
			cast_error(_functionCall, "Unsupported value type: ");
		}


		CodeLines code2;
		TVMStack& stack2 = getStack();
		StackPusherImpl pusher2{stack2, code2};
		StackPusherHelper pusherHelper2(&pusher2, &ctx());
		if (memberAccess->memberName() == "fetch") {
			pushCont(pusher.m_code);
			pusherHelper2.pushDefaultValue(valueType, false);
			pushCont(code2);
			push(-3, "IFELSE");
		} else if (memberAccess->memberName() == "exists") {
			pusherHelper2.push(-1,"NIP"); // delete value
			pushCont(code2);
			push(-1, "IF");
		} else {
			solAssert(false, "");
		}
	}

	void mappingNextMethod(FunctionCall const& _functionCall) {
		auto expr = &_functionCall.expression();
		auto ma = to<MemberAccess>(expr);
		auto mapType = to<MappingType>(getType(&ma->expression()));
		Type const* keyType = mapType->keyType().get();
		Type const* valueType = mapType->valueType().get();
		Type::Category valueCategory = mapType->valueType()->category();
		const int keyLength = lengthOfDictKey(keyType);
		const std::string dictOpcode = "DICT" + typeToDictChar(keyType);

		acceptExpr(_functionCall.arguments()[0].get()); // index
		acceptExpr(&ma->expression()); // index dict
		prepareKeyForDictOperations(keyType);
		pushInt(lengthOfDictKey(keyType)); // index dict nbits

		push(-3 + 3, dictOpcode + "GETNEXT"); // value key -1 or 0


		StackPusherImpl2 pusherOk;
		StackPusherHelper pusherHelperOk(&pusherOk, &ctx());
		pusherHelperOk.restoreKeyAfterDictOperations(keyType, _functionCall);
		pusherHelperOk.push(0, "SWAP"); // key value


		StackPusherImpl2 pusherFail;
		StackPusherHelper pusherHelperFail(&pusherFail, &ctx());
		pusherHelperFail.pushDefaultValue(keyType);
		pusherHelperFail.pushDefaultValue(valueType);
		pusherHelperFail.push(0, "FALSE");


		if (isIn(valueCategory, Type::Category::Mapping, Type::Category::TvmCell)) {
			pusherHelperOk.push(0, "PLDREFIDX 0");
		} else if (valueCategory == Type::Category::Struct) {
			if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
				// nothing
			} else {
				pusherHelperOk.push(0, "LDREFRTOS");
				pusherHelperOk.push(0, "NIP");
			}
		} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract, Type::Category::Array)) {
			if (valueCategory == Type::Category::Array && to<ArrayType>(valueType)->isByteArray()) {
				pusherHelperOk.push(0, "PLDREFIDX 0");
			} else if (isUsualArray(valueType)){
				pusherHelperOk.preload(valueType);
			}
		} else if (isIntegralType(valueType)) {
			pusherHelperOk.preload(valueType);
		} else {
			cast_error(_functionCall, "Unsupported for mapping value type: " + valueType->toString(true));
		}

		pusherHelperOk.push(0, "TRUE");

		pushCont(pusherOk.m_code);
		pushCont(pusherFail.m_code);
		push(-2, ""); // fix stack
		push(0, "IFELSE");

	}

	void mappingMinMethod(FunctionCall const& _functionCall) {
		auto expr = &_functionCall.expression();
		auto ma = to<MemberAccess>(expr);
		auto mapType = to<MappingType>(getType(&ma->expression()));
		Type const* keyType = mapType->keyType().get();
		Type const* valueType = mapType->valueType().get();
		Type::Category valueCategory = mapType->valueType()->category();
		const int keyLength = lengthOfDictKey(keyType);
		const std::string dictOpcode = "DICT" + typeToDictChar(keyType);

		acceptExpr(&ma->expression());
		pushInt(lengthOfDictKey(keyType)); // dict nbits

		auto f = [this, &keyType, &valueType, &valueCategory, &_functionCall, &keyLength]() {
			// (value, key, -1) or 0
			{
				// value key -1
				StackPusherImpl2 pusherOk;
				StackPusherHelper pusherHelperOk(&pusherOk, &ctx());
				pusherHelperOk.restoreKeyAfterDictOperations(keyType, _functionCall);
				// value key
				pusherHelperOk.push(0, "SWAP"); // key value
				if (isIntegralType(valueType) || isUsualArray(valueType)) {
					pusherHelperOk.preload(valueType);
				} else if (valueCategory == Type::Category::Struct && !StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
					pusherHelperOk.push(0, "CTOS");
				}
				pusherHelperOk.push(0, "TRUE");
				pushCont(pusherOk.m_code);
			}
			{
				StackPusherImpl2 pusherFail;
				StackPusherHelper pusherHelperFail(&pusherFail, &ctx());
				pusherHelperFail.pushDefaultValue(keyType);
				pusherHelperFail.pushDefaultValue(valueType);
				pusherHelperFail.push(0, "FALSE");
				pushCont(pusherFail.m_code);
			}
			push(-2, "IFELSE");
		};

		if (isIn(valueCategory, Type::Category::Mapping, Type::Category::TvmCell)) {
			push(-2 + 3, dictOpcode + "MINREF");
			f(); // key value
		} else if (valueCategory == Type::Category::Struct) {
			if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(valueType))) {
				push(-2 + 3, dictOpcode + "MIN");
				f(); // key value
			} else {
				push(-2 + 3, dictOpcode + "MINREF");
				f(); // key value
			}
		} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(valueType)) {
			if (isByteArrayOrString(valueType)) {
				push(-2 + 3, dictOpcode + "MINREF");
				f(); // key value
			} else {
				push(-2 + 3, dictOpcode + "MIN");
				f(); // key value
			}
		} else if (isIntegralType(valueType) || isUsualArray(valueType)) {
			push(-2 + 3, dictOpcode + "MIN");
			f(); // key value
		} else {
			cast_error(_functionCall, "Unsupported value type: " + valueType->toString(true));
		}
	}

	void mappingEmpty(FunctionCall const& _functionCall) {
		auto expr = &_functionCall.expression();
		auto ma = to<MemberAccess>(expr);
		acceptExpr(&ma->expression());
		push(0, "DICTEMPTY");
	}

	bool checkForMappingMethods(FunctionCall const& _functionCall) {
		auto expr = &_functionCall.expression();
		auto ma = to<MemberAccess>(expr);
		if (!ma || !to<MappingType>(ma->expression().annotation().type.get()))
			return false;

		push(0, ";; map." + ma->memberName());

		if (ma->memberName() == "delMin") {
			mappingDelMin(_functionCall);
		} else  if (isIn(ma->memberName(), "fetch", "exists")) {
			mappingFetchOrExists(_functionCall);
		} else if (ma->memberName() == "min") {
			mappingMinMethod(_functionCall);
		} else if (ma->memberName() =="next") {
			mappingNextMethod(_functionCall);
		} else if (ma->memberName() == "empty") {
			mappingEmpty(_functionCall);
		} else {
			cast_error(_functionCall, "Unsupported");
		}

		return true;
	}
	
	void visit2(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();

		if (checkForArrayMethods(_functionCall) ||
			checkRemoteMethodCall(_functionCall) || // To avoid situation when we call a function of a remote contract and don't save the result.
			checkRemoteMethodCallWithValue(_functionCall)) { // // Remote function can return a result but in fact we don't get it.
			return;
		}

		FunctionCallCompiler fcc(m_compiler, this, ctx());
		if (!checkAbiMethodCall(_functionCall) &&
			!checkForMappingMethods(_functionCall)) {
			fcc.compile(_functionCall);
		}
		
		if (!isCurrentResultNeeded()) {
			if (_functionCall.expression().annotation().type->category() == Type::Category::Function) {
				auto ft = to<FunctionType>(_functionCall.expression().annotation().type.get());
				auto size = ft->returnParameterNames().size();
				if (size != 0) {
					cast_warning(_functionCall, "Result is not used.");
					drop(size);
				}
			}
		}
	}
		
	void visit2(Conditional const& _conditional) {
		CodeLines codeTrue  = m_compiler->proceedContinuationExpr(_conditional.trueExpression());
		CodeLines codeFalse = m_compiler->proceedContinuationExpr(_conditional.falseExpression());
		acceptExpr(&_conditional.condition());
		// TODO: no need to call compiler here
		m_compiler->applyContinuation(codeTrue);
		m_compiler->applyContinuation(codeFalse);
		push(-3+1, "IFELSE");
	}

	void visit2(ElementaryTypeNameExpression const& _node) {
		ensureValueFitsType(_node.typeName(), _node);
	}

	struct LValueInfo {
//		bool willNoStackPermutarion = false;
		std::vector<Expression const*> expressions;
	};



protected:
	LValueInfo expandLValue(Expression const* expr, const bool withExpandLastValue, bool willNoStackPermutarion = false) {
		LValueInfo lValueInfo {};
//		lValueInfo.willNoStackPermutarion = willNoStackPermutarion;

		while (true) {
			lValueInfo.expressions.push_back(expr);
			if (to<Identifier>(expr)) {
				break;
			} else if (auto index2 = to<IndexAccess>(expr)) {
				indexTypeCheck(*index2);
				expr = &index2->baseExpression();
			} else if (auto memberAccess = to<MemberAccess>(expr); memberAccess && getType(&memberAccess->expression())->category() == Type::Category::Struct) {
				expr = &memberAccess->expression();
			} else {
				cast_error(*expr, "Unsupported lvalue");
			}
		}
		std::reverse(lValueInfo.expressions.begin(), lValueInfo.expressions.end());

		push(0, "; expValue");
		for (size_t i = 0; i < lValueInfo.expressions.size(); i++) {
			bool isLast = (i + 1) == lValueInfo.expressions.size();
			if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
				auto& stack = getStack();
				auto name = variable->name();
				if (stack.isParam(name)) {
					if (isLast && !withExpandLastValue)
						break;
					if (!willNoStackPermutarion || stack.getOffset(name) != 0) {
						pushMemberOrLocalVarOrConstant(*variable);
					}
				} else {
					push(0, ";; fetch " + name);
					push(+1, "PUSH C7"); // c7
					push(+1, "DUP");
					push(-1 + 1, "SECOND"); // c7 struct
					structCompiler().expandStruct(variable->name(), !isLast || withExpandLastValue);
				}
			} else if (auto index = to<IndexAccess>(lValueInfo.expressions[i])) {
				if (index->baseExpression().annotation().type->category() == Type::Category::Mapping) {
					// dict1
					acceptExpr(index->indexExpression());
					// dict1 index
					push(0, "SWAP");
					// index dict1
					if (isLast && !withExpandLastValue) break;
					push(+2, "PUSH2 S1, S0");
					// index dict1 index dict1

					getFromDict(*parseIndexType(index->baseExpression().annotation().type.get()).get(),
					            *index->annotation().type.get(), *index, true);
					// index dict1 dict2
				} else {
					// array
					push(-1 + 2, "UNPAIR"); // size dict
					acceptExpr(index->indexExpression()); // size dict index
					push(0, "SWAP"); // size index dict
					push(+2, "PUSH2 s1,s2"); // size index dict index size
					push(-2 + 1, "LESS"); // size index dict index<size
					push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
					if (isLast && !withExpandLastValue) {
						break;
					}
					push(+2, "PUSH2 S1, S0"); // size index dict index dict
					getFromDict(*parseIndexType(index->baseExpression().annotation().type.get()).get(),
					            *index->annotation().type.get(), *index, false);
					// size index dict value
				}
			} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
				auto structType = to<StructType>(memberAccess->expression().annotation().type.get());
				StructCompiler structCompiler{this, structType};
				const string& memberName = memberAccess->memberName();
				structCompiler.expandStruct(memberName, !isLast || withExpandLastValue);
			} else {
				solAssert(false, "");
			}
		}
		push(0, "; end expValue");
		return lValueInfo;
	}

	static bool isIndexAccessOfDynamicArray(Expression const* expr) {
		auto indexAccess = to<IndexAccess>(expr);
		if (!indexAccess) {
			return false;
		}
		auto arrayType = to<ArrayType>(indexAccess->baseExpression().annotation().type.get());
		return arrayType && !arrayType->isByteArray();
	}

	static bool isStateVariable(Expression const* expr) {
		auto variable = to<Identifier>(expr);
		if (!variable) {
			return false;
		}
		auto vd = to<VariableDeclaration>(variable->annotation().referencedDeclaration);
		return vd->isStateVariable();
	}

	void collectLValue(const LValueInfo &lValueInfo, const bool haveValueOnStackTop = true,
																					const bool _isLastValueBuilder  = false) {
		// variable [arrayIndex | mapIndex | structMember]...

		push(0, "; colValue");
		const int n = static_cast<int>(lValueInfo.expressions.size());
		bool isValueBuilder = _isLastValueBuilder;

		for (int i = n - 1; i >= 0; i--) {
			bool isLast = (i + 1) == static_cast<int>(lValueInfo.expressions.size());

			if (auto variable = to<Identifier>(lValueInfo.expressions[i])) {
//				pushLog("colVar");
				auto& stack = getStack();
				const auto& name = variable->name();
				if (stack.isParam(name)) {
					if (isValueBuilder) {
						Type const* type = variable->annotation().type.get();
						if (isUsualStruct(type)) {
							push(0, "ENDC");
							push(0, "CTOS");
						} else {
							cast_error(*lValueInfo.expressions[0], ":");
						}
					}
					solAssert((haveValueOnStackTop && n == 1) || n > 1, "");
					tryAssignParam(name);
				} else {
					// c7 struct... value
					structCompiler().collectStruct(variable->name(), isValueBuilder, false); // c7 struct
					push(-1, "SETSECOND"); // c7
					push(-1, "POP c7");
				}
			} else if (auto index = to<IndexAccess>(lValueInfo.expressions[i])) {
				if (isIndexAccessOfDynamicArray(lValueInfo.expressions[i])) {
//					pushLog("colArrIndex");
					if (isLast && !haveValueOnStackTop) {
						// size index dict
						push(-1, "NIP"); // size dict
					} else {
						// size index dict value
						TypePointer const keyType = parseIndexType(index->baseExpression().annotation().type.get());
						auto valueDictType = getType(index);
						isValueBuilder = prepareValueForDictOperations(keyType.get(), valueDictType, isValueBuilder);
						push(0, "ROTREV"); // size value index dict
						prepareKeyForDictOperations(keyType.get());
						setDict(*keyType.get(), *valueDictType, isValueBuilder, *lValueInfo.expressions[i]); // size dict'
					}

					// arrIndex | mapIndex | struct | stateVar
					if (to<MemberAccess>(lValueInfo.expressions[i - 1]) ||
					        to<IndexAccess>(lValueInfo.expressions[i - 1]) ||
							isStateVariable(lValueInfo.expressions[i - 1])) {
						// size dict'
						push(0, "SWAP"); // dict size
						push(+1, "NEWC"); // dict size builder
						push(-1, "STU 32"); // dict builder
						push(-1, "STDICT"); // builder
						isValueBuilder = true;
					} else {
						push(-2 + 1, "PAIR");
						isValueBuilder = false;
					}
				} else {
//					pushLog("colMapIndex");
					if (isLast && !haveValueOnStackTop) {
						// index dict
						push(-1, "NIP"); // dict
					} else {
						// index dict value
						TypePointer const keyType = parseIndexType(index->baseExpression().annotation().type.get());
						auto valueDictType = getType(index);
						isValueBuilder = prepareValueForDictOperations(keyType.get(), valueDictType, isValueBuilder);
						push(0, "ROTREV"); // value index dict
						prepareKeyForDictOperations(keyType.get());
						setDict(*keyType.get(), *valueDictType, isValueBuilder, *lValueInfo.expressions[i]); // dict'
					}
					isValueBuilder = false;
				}
			} else if (auto memberAccess = to<MemberAccess>(lValueInfo.expressions[i])) {
//				pushLog("colStruct");
				auto structType = to<StructType>(memberAccess->expression().annotation().type.get());
				StructCompiler structCompiler{this, structType};
				const string& memberName = memberAccess->memberName();
				structCompiler.collectStruct(memberName, isValueBuilder, true);
				isValueBuilder = true;
			} else {
				solAssert(false, "");
			}
		}
		push(0, "; end colValue");
	}

	bool tryAssignLValue(Assignment const& _assignment) {
		const Token op = _assignment.assignmentOperator();
		if (op == Token::Assign){
			if (!tryImplicitConvert(_assignment.leftHandSide().annotation().type.get(),
			                        _assignment.rightHandSide().annotation().type.get())) {
				acceptExpr(&_assignment.rightHandSide());
			}
			const int saveStackSize = getStack().size();
			const LValueInfo lValueInfo = expandLValue(&_assignment.leftHandSide(), false);
			if (isCurrentResultNeeded()) {
				push(+1, "PUSH s" + toString(getStack().size() - saveStackSize));
			} else {
				blockSwap(1, getStack().size() - saveStackSize);
			}
			collectLValue(lValueInfo);
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

			acceptExpr(&_assignment.rightHandSide()); // r
			const int saveStackSize = getStack().size();
			const LValueInfo lValueInfo = expandLValue(&_assignment.leftHandSide(), true); // r expanded... l
			const int expandedLValueSize = getStack().size() - saveStackSize - 1;
			blockSwap(1, expandedLValueSize + 1); // expanded... l r

			if (isCurrentResultNeeded()) {
				push(-1, cmd); // expanded... res
				push(+1, "DUP"); // expanded... res res
				blockSwap(expandedLValueSize + 1, 1); // res expanded... res
			} else {
				push(-1, cmd); // expanded... res
			}
			checkBitFit(_assignment.annotation().type.get(), _assignment.leftHandSide().annotation().type.get(),
			            _assignment.rightHandSide().annotation().type.get(), cmd);
			collectLValue(lValueInfo);
		}

		return true;
	}


	bool tryAssignTuple(Assignment const& _assignment) {
		auto lhs = to<TupleExpression>(&_assignment.leftHandSide());
		if (!lhs) {
			return false;
		}

		acceptExpr(&_assignment.rightHandSide());
		reverse(lhs->components().size(), 0);
		for (const auto & i : lhs->components()) {
			if (!i) {
				push(-1, "DROP");
				continue;
			}
			const int stackSizeForValue = getStack().size();
			const LValueInfo lValueInfo = expandLValue(i.get(), false);
			const int stackSize = getStack().size();
			const int expandLValueSize = stackSize - stackSizeForValue;
			if (expandLValueSize > 0) {
				blockSwap(1, expandLValueSize);
			}
			collectLValue(lValueInfo);
		}
		return true;
	}


	static bool tryAssignArrayLength(Assignment const& _assignment) {
		if (auto index = to<MemberAccess>(&_assignment.leftHandSide());
				index != nullptr && index->memberName() == "length") {

			auto arrayType = to<ArrayType>(getType(&index->expression()));
			if (arrayType == nullptr) {
				return false; // maybe it's struct
			}

			cast_error(_assignment, "Array length assignment is not supported");
		}
		return false;
	}

	void visit2(Assignment const& _assignment) {
		set <Token> compoundAssignment = { Token::AssignShr };
		if (compoundAssignment.count(_assignment.assignmentOperator()) > 0)
			cast_error(_assignment, "Unsupported operation.");
		if (tryAssignArrayLength(_assignment)) return;
		if (tryAssignTuple(_assignment)) return;
		if (tryAssignLValue(_assignment)) return;
		cast_error(_assignment, "Unsupported assignment.");
	}

};

}	// end dev::solidity
