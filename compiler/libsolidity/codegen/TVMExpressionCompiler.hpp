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

namespace dev {
namespace solidity {

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

	void acceptExpr2(const Expression* expr, const bool _isResultNeeded = true) override {
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

protected:
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
			cast_error(_tupleExpression, "Inline array is not supported");
		}
		for (const auto& comp : _tupleExpression.components()) {
			acceptExpr(comp.get());
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
				push(+1, "PUSH s" + toString(offset));
			}
			return true;
		} else if (auto idx = ctx().getMemberIdx(name); idx >= 0) {
			pushInt(idx);
			pushPersistentDataDict();
			getFromDict(getKeyTypeOfC4(), *_identifier.annotation().type.get(), _identifier);
			return true;
		} else if (tryPushConstant(_identifier)) {
			return true;
		}
		return false;
	}

protected:
	void visit2(Identifier const& _identifier) {
		const string& name = _identifier.name();
		push(0, string(";; ") + name);
		dumpStackSize();
		if (pushMemberOrLocalVarOrConstant(_identifier)) {
		} else if (name == "now") {
			// Getting value of `now` variable
			push(+1, "NOW");
		} else if (name == "this") {
			// calling this.function() will create intrenal message that should be sent to the address of current contract
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

	void compileUnaryOperation(UnaryOperation const& _node, const std::string& tvmUnaryOperation) {
		if (m_expressionDepth >= 1 || m_isResultNeeded) {
			cast_error(_node, "++ operation is supported only in simple expressions.");
		}

		if (auto identifier = to<Identifier>(&_node.subExpression())) {
			const auto& name = identifier->name();
			TVMStack& stack = getStack();
			if (stack.isParam(name) && stack.getOffset(name) == 0) {
				push(0, tvmUnaryOperation);
				return;
			}
		}

		if (const std::vector<Expression const*> exprs = unrollLHS(&_node.subExpression());
			exprs[0] != nullptr) {
			expandMappingValue(exprs, true);
			push(0, tvmUnaryOperation);
			collectMappingValue(exprs);
			return ;
		}

		cast_error(_node, "++ operation is supported only for simple or mapping variables.");
	}

	// TODO
	void compileUnaryDelete(UnaryOperation const& node) {
		const std::vector<Expression const*> exprs = unrollLHS(&node.subExpression());
		if (exprs[0] == nullptr) {
			cast_error(node, "delete operation is supported only for simple or mapping variables.");
		}

		if (to<Identifier>(exprs.back())) {
			expandMappingValue(exprs, false);
			Type const* exprType = getType(&node.subExpression());
			if (to<AddressType>(exprType)) {
				pushZeroAddress();
			} else if (to<IntegerType>(exprType) ||
				to<BoolType>(exprType) ||
				to<FixedBytesType>(exprType)) {
				pushInt(0);
			} else if (auto arrayType = to<ArrayType>(exprType)) {
				if (arrayType->isDynamicallySized()) {
					push(+1, "NEWDICT");
				} else {
					cast_error(node, "delete operation is not supported for statically-sized arrays.");
				}
			} else if (to<StructType>(exprType)) {
				auto const& structType = to<StructType>(exprType);
				auto const& structDefinition = structType->structDefinition();
				StructCompiler structCompiler{this, &structDefinition};
				structCompiler.createDefaultStruct();
			} else {
				cast_error(node, "delete operation is not supported for " + exprType->toString());
			}
			collectMappingValue(exprs);
		} else if (auto memberAccess = to<MemberAccess>(exprs.back())) {
			expandMappingValue(exprs, false);
			MemberAccessAnnotation& a = memberAccess->annotation();
			auto decl = to<VariableDeclaration>(a.referencedDeclaration);
			pushDefaultValue(decl->type().get());
			collectMappingValue(exprs, true);
		} else if (auto indexAccess = to<IndexAccess>(exprs.back())) {
			expandMappingValue(exprs, false);               // ... index dict
			push(+1, "PUSH S1");                            // ... index dict index
			push(0, "SWAP");                                // ... index index dict
			TypePointer const dictKey = parseIndexType(indexAccess->baseExpression().annotation().type.get());
			if (isIn(dictKey->category(), Type::Category::Address, Type::Category::Contract)) {
				prepareKeyForDictOperations(dictKey.get()); // ..index index' dict
				pushInt(AddressInfo::maxBitLength()); // ..index index' dict nbits
			} else {
				pushInt(getKeyDictLength(dictKey.get())); // ..index index dict nbits
			}
			push(-3+2, "DICT" + getKeyDict(dictKey.get()) + "DEL");  // ... index dict' {-1,0}
			push(-1, "DROP");                               // ... index dict'
			collectMappingValue(exprs, false);
		} else {
			solAssert(false, "");
		}
	}

	void visit2(UnaryOperation const& _node) {
		auto op = _node.getOperator();
		push(0, string(";; ") + TokenTraits::toString(op));
		if (op == Token::Inc) {
			compileUnaryOperation(_node, "INC");
		} else if (op == Token::Dec) {
			compileUnaryOperation(_node, "DEC");
		} else if (op == Token::Not) {
			acceptExpr(&_node.subExpression());
			push(0, "NOT");
		} else if (op == Token::Sub) {
			acceptExpr(&_node.subExpression());
			push(0, "NEGATE");
		} else if (op == Token::BitNot) {
			acceptExpr(&_node.subExpression());
			int numBits = 0;
			bool isSigned = false;
			auto type = getType(&_node.subExpression());
			TypeInfo ti = TypeInfo(type);
			if (ti.isNumeric) {
				numBits = ti.numBits;
				isSigned = ti.isSigned;
			} else {
				cast_error(_node, "~ operation is supported only for numbers.");
			}
			if (isSigned)
				push(0, "NOT");
			else {
				push(0,"UFITS " + to_string(numBits));
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
		if ((op == Token::NotEqual || op == Token::Equal || op == Token::GreaterThan || op == Token::LessThan) && r) {
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
		string cmd = "???";
		if (op == Token::Add) cmd = "ADD";
		if (op == Token::Mul) cmd = "MUL";
		if (op == Token::Sub) cmd = "SUB";
		if (op == Token::Mod) cmd = "MOD";
		if (op == Token::Div) cmd = "DIV";
		if (op == Token::GreaterThan) cmd = "GREATER";
		if (op == Token::GreaterThanOrEqual) cmd = "GEQ";
		if (op == Token::LessThan) cmd = "LESS";
		if (op == Token::LessThanOrEqual) cmd = "LEQ";
		if (op == Token::Equal) cmd = "EQUAL";
		if (op == Token::NotEqual) cmd = "NEQ";
		if (op == Token::And) cmd = "AND";
		if (op == Token::Or) cmd = "OR";
		if (op == Token::SHL) cmd = "LSHIFT";
		if (op == Token::SAR) cmd = "RSHIFT";
		if (op == Token::BitAnd) cmd = "AND";
		if (op == Token::BitOr) cmd = "OR";
		if (op == Token::BitXor) cmd = "XOR";
		push(-1, cmd);
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
			StructDefinition const* structDefinition =
					&to<StructType>(_node.expression().annotation().type.get())->structDefinition();
			StructCompiler structCompiler{this, structDefinition};
			structCompiler.getMember(memberName);
			return;
		}

		if (checkForMagic(_node, category)) return; 
		if (checkForMemberAccessBalance(_node, category)) return;
		
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
	
	bool checkForMemberAccessBalance(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::Address)
			return false;
		if (_node.memberName() == "balance") {
			if (!isAddressThis(to<FunctionCall>(&_node.expression())))
				cast_error(_node.expression(), "only 'address(this).balance' is supported for balance");
			pushPrivateFunctionOrMacroCall(+1, "get_contract_balance_macro");
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
			pushInt(32);
			push(-2+3, "DICTUMAX");
			push(+1, "PUSHCONT { POP s1 INC }");
			push(+1, "PUSHCONT { PUSHINT 0 }");
			push(-3-1, "IFELSE");
			return;
		}
		cast_error(_node, "Unsupported 489");
	}

	void visitMemberAccessFixedBytes(MemberAccess const& _node, FixedBytesType const* fbt) {
		if (_node.memberName() == "length" && to<Identifier>(&_node.expression())) {
			pushInt(fbt->storageBytes());
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
		acceptExpr(_node.indexExpression());
		acceptExpr(&_node.baseExpression());
		push( 0, ";; index");
		getFromDict(*parseIndexType(_node.baseExpression().annotation().type.get()).get(), *getType(&_node), _node);
	}

	bool checkAbiMethodCall(FunctionCall const& _functionCall) {
		// compile "abi.encodePacked()"
		if (auto memberAccess = to<MemberAccess>(&_functionCall.expression())) {
			auto identifier = to<Identifier>(&memberAccess->expression());
			if (identifier != nullptr && identifier->name() == "abi") {
				if (memberAccess->memberName() != "encodePacked") {
					cast_error(_functionCall, "Only method encodePacked is supported for abi");
				}
				// TODO check builder overflow?
				push(+1, "NEWC");
				for (const ASTPointer<Expression const>& argument : _functionCall.arguments()) {
					const Type *type = getType(argument.get());
					if (to<IntegerType>(type) != nullptr) {
						acceptExpr(argument.get());
						push(-1, storeIntegralOrAddress(type, true));
					} else if (auto array = to<ArrayType>(type)) {
						if (to<IntegerType>(array->baseType().get()) == nullptr) {
							cast_error(*argument.get(), "Only numeric array is supported for abi.encodePacked(...) ");
						}
						if (_functionCall.arguments().size() != 1) {
							cast_error(_functionCall, "Only one array is supported for abi.encodePacked(...)");
						}
						TypeInfo arrayElementTypeInfo(array->baseType().get());
						acceptExpr(argument.get());
						pushInt(arrayElementTypeInfo.numBits);
						pushPrivateFunctionOrMacroCall(-3 + 1, "abi_encode_packed_macro");
					} else {
						cast_error(_functionCall, "Only numeric types or numeric arrays are supported for abi.encodePacked(...)");
					}
				}
				push(0, "ENDC CTOS");
				// DOTO return bytes?
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
						auto fn = ctx().getFunctionExternalName(memberAccess->memberName());
						if (m_expressionDepth >= 1 || m_isResultNeeded)
							cast_error(_functionCall, "Calls to remote contract do not return result.");
						encodeOutboundMessageBody(fn, arguments, fdef->parameters(), StackPusherHelper::ReasonOfOutboundMessage::RemoteCallInternal);
						pushPrivateFunctionOrMacroCall(-3, "send_internal_message_macro");
						dumpStackSize();
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
				auto fn = ctx().getFunctionExternalName(memberAccess->memberName());
				if (m_expressionDepth >= 1 || m_isResultNeeded)
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
		
		vector<Expression const*> exprs;
		exprs = unrollLHS(&ma->expression());
		expandMappingValue(exprs, true);
		
		for (const auto& arg : _functionCall.arguments())
			acceptExpr(arg.get());
		
		if (m_expressionDepth >= 1 || m_isResultNeeded) {
			cast_error(ma->expression(), "arr.push(..) and arr.pop(..) are supported only in simple expressions");
		}
		
		if (ma->memberName() == "push") {
			auto arrayBaseType = to<ArrayType>(getType(&ma->expression()))->baseType().get();

			// stack: arr value
			push(0, ";; array.push(..)");
			prepareValueForDictOperations(arrayBaseType); // arr value'
			push(+1, "PUSH s1"); // arr value' arr
			pushInt(TvmConst::ArrayKeyLength); // arr value' arr 32
			push(-2+3,	"DICTUMAX"); // stack: arr value' max_value max_idx -1
			push(+1,		"PUSHCONT { POP s1 INC}");
			push(+1,		"PUSHCONT { PUSHINT 0 }");
			push(-3-2+1,	"IFELSE"); // stack: arr value' (max_idx+1)
			push(0, "ROT"); // stack: value' (max_idx+1) arr
			setDict(getKeyTypeOfArray(), *arrayBaseType, _functionCall);
		} else if (ma->memberName() == "pop") {
			// arr
			pushInt(32); // arr 32
			push(-2 + 4, "DICTUREMMAX"); // arr' value key -1
			push(-1, "THROWIFNOT 10"); // arr' value key
			drop(2); // arr'
		} else {
			cast_error(*ma, "Unsupported function call");
		}
		
		collectMappingValue(exprs, true);
		return true;
	}
	
	void visit2(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();

		FunctionCallCompiler fcc(m_compiler, this, ctx());
		
		if (checkAbiMethodCall(_functionCall)) return;
		if (checkRemoteMethodCall(_functionCall)) return;
		if (checkRemoteMethodCallWithValue(_functionCall)) return;
		if (checkForArrayMethods(_functionCall)) return;
		
		fcc.compile(_functionCall, m_expressionDepth >= 1 || m_isResultNeeded);
		
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

	static std::vector<Expression const*> unrollLHS(Expression const* expr) {
		std::vector<Expression const*> res;
		if (to<Identifier>(expr)) {
			res.push_back(expr);
			return res;
		}
		if (auto index = to<IndexAccess>(expr)) {
			indexTypeCheck(*index);
			res = unrollLHS(&index->baseExpression());
			res.push_back(expr);
			return res;
		}
		if (auto index = to<MemberAccess>(expr)) {
			if (getType(&index->expression())->category() == Type::Category::Struct) {
				res = unrollLHS(&index->expression());
				res.push_back(expr);
				return res;
			}
		}
		res.push_back(nullptr);
		return res;
	}

protected:
	void expandMappingValue(const std::vector<Expression const*> &exprs, const bool withExpandLastValue) {
		for (size_t i = 0; i < exprs.size(); i++) {
			bool isLast = (i + 1) == exprs.size();
			if (auto variable = to<Identifier>(exprs[i])) {
				auto& stack = getStack();
				auto name = variable->name();
				if (stack.isParam(name)) {
					if (isLast && !withExpandLastValue)
						break;
					pushMemberOrLocalVarOrConstant(*variable);
				} else if (auto idx = ctx().getMemberIdx(name); idx >= 0) {
					push(0, ";; fetch " + name);
					push(+1, "PUSH C7"); // c7

					push(+1, "DUP");
					push(-1 + 1, "SECOND"); // c7 dict
					if (isLast && !withExpandLastValue)
						break;

					pushInt(idx); // c7 dict index
					push(+1, "PUSH s1"); // c7 dict index dict
					getFromDict(getKeyTypeOfC4(), *getType(variable), *variable); // c7 dict value
				} else {
					cast_error(*variable, "Unsupported expr");
				}
			} else if (auto index = to<IndexAccess>(exprs[i])) {
				// dict1
				acceptExpr(index->indexExpression());
				// dict1 index
				push(0, "SWAP");
				// index dict1
				if (isLast && !withExpandLastValue) break;
				push(+2, "PUSH2 S1, S0");
				// index dict1 index dict1

				getFromDict(*parseIndexType(index->baseExpression().annotation().type.get()).get(),
				            *index->annotation().type.get(), *index);
				// index dict1 dict2
			} else if (auto memberAccess = to<MemberAccess>(exprs[i])) {
				auto structType = to<StructType>(memberAccess->expression().annotation().type.get());
				StructDefinition const* structDefinition = &structType->structDefinition();
				StructCompiler structCompiler{this, structDefinition};
				const string& memberName = memberAccess->memberName();
				structCompiler.expandStruct(memberName, !isLast || withExpandLastValue);
			} else {
				solAssert(false, "");
			}
		}
	}

	void collectMappingValue(const std::vector<Expression const*> &exprs, const bool haveValueOnStackTop = true) {
		const int n = static_cast<int>(exprs.size());
		for (int i = n - 1; i >= 0; i--) {
//			pushLog("cmv" + toString(i));
			bool isLast = (i + 1) == (int)exprs.size();
			if (auto variable = to<Identifier>(exprs[i])) {
				auto& stack = getStack();
				auto name = variable->name();
				if (stack.isParam(name)) {
					tryAssignParam(name);
				} else if (auto idx = ctx().getMemberIdx(name); idx >= 0) {
					// c7 dict value
					prepareValueForDictOperations(variable->annotation().type.get()); // c7 dict value'
					pushInt(ctx().getMemberIdx(name)); // c7 dict value' index
					push(0, "ROT"); // c7 value' index dict
					setDict(getKeyTypeOfC4(), *variable->annotation().type.get(), *exprs[i]); // c7 dict
					push(-1, "SETSECOND");
					push(-1, "POP c7");
				} else {
					cast_error(*variable, "Unsupported expr");
				}
			} else if (auto index = to<IndexAccess>(exprs[i])) {
				if (isLast && !haveValueOnStackTop) {
					// index dict
					push(-1, "NIP"); // dict
				} else {
					// index dict value
					auto valueDictType = getType(index);
					if (isLast) {
						prepareValueForDictOperations(valueDictType);
					}
					TypePointer const keyType = parseIndexType(index->baseExpression().annotation().type.get());
					push(0, "ROTREV"); // value index dict
					prepareKeyForDictOperations(keyType.get());
					setDict(*keyType.get(), *valueDictType, *exprs[i]); // dict'
				}
			} else if (auto memberAccess = to<MemberAccess>(exprs[i])) {
				auto structType = to<StructType>(memberAccess->expression().annotation().type.get());
				StructDefinition const* structDefinition = &structType->structDefinition();
				StructCompiler structCompiler{this, structDefinition};
				const string& memberName = memberAccess->memberName();
				const bool isValueBuilder = i + 1 < n && to<MemberAccess>(exprs[i + 1]);
				const bool isResultBuilder = i - 1 >= 0 && to<MemberAccess>(exprs[i - 1]);
				structCompiler.collectStruct(memberName, isValueBuilder, isResultBuilder);
			} else {
				solAssert(false, "");
			}
		}
	}

	bool tryAssignMapping(Assignment const& _assignment) {
		const vector<Expression const*> exprs = unrollLHS(&_assignment.leftHandSide());
		if (!exprs[0]) {
			return false;
		}

		if (m_expressionDepth >= 1 || m_isResultNeeded) {
			cast_error(_assignment, "Assignment is supported only in simple expressions.");
		}

		const Token op = _assignment.assignmentOperator();
		if (op == Token::Assign){
			expandMappingValue(exprs, false);

			if (!tryImplicitConvert(_assignment.leftHandSide().annotation().type.get(),
			                        _assignment.rightHandSide().annotation().type.get())) {
				acceptExpr(&_assignment.rightHandSide());
			}
		} else {
			string cmd;
			if      (op == Token::AssignAdd)    cmd = "ADD";
			else if (op == Token::AssignMul)    cmd = "MUL";
			else if (op == Token::AssignSub)    cmd = "SUB";
			else if (op == Token::AssignMod)    cmd = "MOD";
			else if (op == Token::AssignDiv)    cmd = "DIV";
			else if (op == Token::AssignBitAnd) cmd = "AND";
			else if (op == Token::AssignBitOr)  cmd = "OR";
			else if (op == Token::AssignBitXor) cmd = "XOR";
			else if (op == Token::AssignShl)    cmd = "LSHIFT";
			else if (op == Token::AssignSar)    cmd = "RSHIFT";
			else {
				cast_error(_assignment, "Unsupported operation.");
			}

			expandMappingValue(exprs, true);
			acceptExpr(&_assignment.rightHandSide());
			push(-1, cmd);
		}


		collectMappingValue(exprs);
		return true;
	}

	bool tryAssign(const TupleExpression * lhs, const TupleExpression * rhs) {
		for (size_t i = 0; i < lhs->components().size(); i++) {
			const vector<Expression const*> exprs = unrollLHS(lhs->components()[i].get());
			if (!exprs[0])
				return false;
			expandMappingValue(exprs, false);
			acceptExpr(rhs->components()[i].get());
			collectMappingValue(exprs);
		}
		return true;
	}

	bool tryAssignTuple(Assignment const& _assignment) {
		if (auto lhs = to<TupleExpression>(&_assignment.leftHandSide())) {
			if (auto te_rhs = to<TupleExpression>(&_assignment.rightHandSide())) {
				if (lhs->components().size() != te_rhs->components().size())
					cast_error(_assignment, "Tuples in assignment should be the same size.");
				if (_assignment.assignmentOperator() != Token::Assign)
					cast_error(_assignment, "Unsupported operation.");
				return tryAssign(lhs, te_rhs);
			} else if (auto fc_rhs = to<FunctionCall>(&_assignment.rightHandSide())) {
				acceptExpr(fc_rhs);
				push(0, "REVERSE " + to_string(lhs->components().size()) + ", 0");
				for (const auto & i : lhs->components()) {
					if (!i) {
						push(-1, "DROP");
						continue;
					}
					const int stackSizeForValue = getStack().size();
					const vector<Expression const*> exprs = unrollLHS(i.get());
					if (!exprs[0])
						cast_error(_assignment, "Unsupported tuple field.");
					expandMappingValue(exprs, false);
					const int stackSize = getStack().size();
					const int expandMappingValueSize = stackSize - stackSizeForValue;
					if (expandMappingValueSize > 0) {
						push(0, "BLKSWAP 1," + toString(stackSize - stackSizeForValue));
					}
					collectMappingValue(exprs);
				}
				return true;
			}
		}
		return false;
	}


	bool tryAssignArrayLength(Assignment const& _assignment) {
		if (auto index = to<MemberAccess>(&_assignment.leftHandSide());
				index != nullptr && index->memberName() == "length") {

			auto arrayType = to<ArrayType>(getType(&index->expression()));
			if (arrayType == nullptr) {
				return false; // maybe it's struct
			}

			const Token &op = _assignment.assignmentOperator();
			if (op != Token::Assign) {
				cast_error(_assignment, "Only simple assignment supported for array.length.");
			}

			const std::vector<Expression const*> exprs = unrollLHS(&index->expression());
			if (!exprs[0]) {
				return false;
			}

			expandMappingValue(exprs, true); // some_expanded_data... arr
			auto arrayBaseType = arrayType->baseType().get();
			if (isIntegralType(arrayBaseType)) {
				pushDefaultValue(arrayBaseType);
				prepareValueForDictOperations(arrayBaseType);
			} else if (to<StructType>(arrayBaseType)) {
				pushDefaultValue(arrayBaseType);
				push(+1, "NEWC");
				push(-1, "STSLICE");
			} else {
				push(+1, "NEWDICT");
			}
			push(0, "SWAP"); // some_expanded_data... defaultValue arr
			acceptExpr(&_assignment.rightHandSide()); // some_expanded_data... defaultValue arr newArrSize
			pushPrivateFunctionOrMacroCall(-3 + 1, "change_array_length_macro"); // some_expanded_data arr'
			collectMappingValue(exprs);
			return true;
		}
		return false;
	}

	void visit2(Assignment const& _assignment) {
		set <Token> compoundAssignment = { Token::AssignShr };
		if (compoundAssignment.count(_assignment.assignmentOperator()) > 0)
			cast_error(_assignment, "Unsupported operation.");
		if (tryAssignArrayLength(_assignment)) return;
		if (tryAssignMapping(_assignment)) return;
		if (tryAssignTuple(_assignment)) return;
		cast_error(_assignment, "Unsupported assignment.");
	}

};

}	// solidity
}	// dev
