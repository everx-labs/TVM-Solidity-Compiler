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

#include "TVMFunctionCall.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMIntrinsics.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMABI.hpp"

using namespace solidity::frontend;

void FunctionCallCompiler::acceptExpr(const Expression *expr) {
	m_exprCompiler->compileNewExpr(expr);
}

FunctionCallCompiler::FunctionCallCompiler(StackPusherHelper &m_pusher, TVMExpressionCompiler *exprCompiler) :
		m_pusher{m_pusher},
		m_exprCompiler{exprCompiler} {

}

void FunctionCallCompiler::structConstructorCall(FunctionCall const &_functionCall) {
	auto const& type = dynamic_cast<TypeType const&>(*_functionCall.expression().annotation().type);
	auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
	for (const ASTPointer<Expression const> &arg : _functionCall.arguments()) {
		acceptExpr(arg.get());
	}
	StructCompiler structCompiler{&m_pusher, &structType};
	structCompiler.structConstructor(_functionCall.names()); // create struct and drop args
}

void FunctionCallCompiler::compile(FunctionCall const &_functionCall) {

	if (checkNewExpression(_functionCall)) return;
	if (checkTvmIntrinsic(_functionCall)) return;
	if (checkAddressThis(_functionCall)) return;
	if (checkSolidityUnits(_functionCall)) return;
	if (_functionCall.annotation().kind == FunctionCallKind::StructConstructorCall) {
		structConstructorCall(_functionCall);
		return ;
	}
	if (_functionCall.annotation().kind == FunctionCallKind::TypeConversion) {
		typeConversion(_functionCall);
		return;
	}
	if (checkForIdentifier(_functionCall)) return;

	auto arguments = _functionCall.arguments();
	auto expr = &_functionCall.expression();
	if (auto ma = to<MemberAccess>(expr)) {
		auto category = getType(&ma->expression())->category();
		auto ident = to<Identifier>(&ma->expression());
		if (checkForTvmSliceMethods(*ma, category, arguments, _functionCall)) return;
		if (checkForTvmBuilderMethods(*ma, category, arguments)) return;
		if (checkForStringMethods(*ma, arguments)) return;
		if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
			if (checkForTvmSendFunction(*ma, category, arguments)) return;
			if (checkForTvmConfigParamFunction(*ma, category, arguments)) return;
			if (checkForTvmFunction(*ma, category, arguments)) return;
			if (checkForTvmDeployMethods(*ma, category, arguments, _functionCall)) return;
		} else {
			if (!isIn(ma->memberName(), "log", "transfer", "checkSign", "deployAndCallConstructor")) { // TODO delete log,  checkSign
				for (const auto &arg : arguments) {
					acceptExpr(arg.get());
				}
			}
			if (checkForSuper(*ma, category)) return;
			if (ma->expression().annotation().type->category() == Type::Category::Address) {
				addressMethod(_functionCall);
				return;
			}
			if (checkForTvmCellMethods(*ma, category, arguments)) return;
			if (checkForMemberAccessTypeType(*ma, category)) return;
			if (checkForMsgFunction(*ma, category, arguments)) return;
			if (checkForTypeTypeMember(*ma, category)) return;
		}
	}
	cast_error(_functionCall, "Unsupported function call");
}

bool FunctionCallCompiler::checkForSuper(MemberAccess const &_node, Type::Category) {
	// argument are on stack
	if (!isSuper(&_node.expression()))
		return false;
	m_pusher.push(0, ";; super");
	string fname = _node.memberName();
	auto super = getSuperContract(m_pusher.ctx().getContract(m_pusher.ctx().m_currentFunction),
	                              m_pusher.ctx().getContract(), fname);
	solAssert(super, "#1000");
	if (getFunction(super, fname)) {
		auto functionName = super->name() + "_" + fname;
		m_pusher.push( 0, ";; Super call " + functionName);
		if (auto ft = to<FunctionType>(getType(&_node))) {
			m_pusher.pushCall(functionName, ft);
			return true;
		}
	}
	solAssert(false, "");
}

bool FunctionCallCompiler::checkForTypeTypeMember(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TypeType)
		return false;
	if (_node.memberName() == "makeAddrExtern") {
		// addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
		m_pusher.push(0, ";; address.makeAddrExtern()");
		m_pusher.push(+1, "DUP"); // numb cntBit cntBit
		m_pusher.pushInt(1); // numb cntBit cntBit 1
		m_pusher.push(+1, "NEWC"); // numb cntBit cntBit 1 builder
		m_pusher.push(-1, "STU 2"); // numb cntBit cntBit builder'
		m_pusher.push(-1, "STU 9"); // numb cntBit builder''
		m_pusher.push(0, "SWAP"); // numb builder'' cntBit
		m_pusher.push(-3 + 1, "STUX"); // builder'''
		m_pusher.push(0, "ENDC");
		m_pusher.push(0, "CTOS"); // extAddress
		return true;
	}
	if (_node.memberName() == "makeAddrNone") {
		m_pusher.push(0, ";; address.makeAddrNone()");
		m_pusher.push(+1, "PUSHSLICE x2_");
		return true;
	}
	if (_node.memberName() == "makeAddrStd") {
		m_pusher.push(0, ";; address.makeAddrStd()");
		m_pusher.pushPrivateFunctionOrMacroCall(-2 + 1, "make_std_address_with_wid_macro");
		return true;
	}
	return false;
}

void FunctionCallCompiler::loadTypeFromSlice(MemberAccess const &_node, TypePointer type) {
	const Type::Category category = type->category();
	if (to<TvmCellType>(type)) {
		m_pusher.push(0, ";; decode TvmCell");
		m_pusher.push(+1, "LDREF");
	} else if (auto structType = to<StructType>(type)) {
		ASTString const& structName = structType->structDefinition().name();
		m_pusher.push(0, ";; decode struct " + structName);
		std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
		for (const ASTPointer<VariableDeclaration> &m : members) {
			m_pusher.push(0, ";; decode " + structName + "." + m->name());
			loadTypeFromSlice(_node, m->type());
		}
		m_pusher.push(0, ";; build struct " + structName);
		// members... slice
		const int memberQty = static_cast<int>(members.size());
		m_pusher.blockSwap(memberQty, 1); // slice members...
		m_pusher.tuple(memberQty); // slice struct
		m_pusher.push(0, "SWAP"); // ... struct slice
	} else if (category == Type::Category::Address || category == Type::Category::Contract) {
		m_pusher.push(0, ";; decode address");
		m_pusher.push(+1, "LDMSGADDR");
	} else if (isIntegralType(type)) {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		m_pusher.push(+1, (ti.isSigned ? "LDI " : "LDU ") + toString(ti.numBits));
	} else {
		cast_error(_node, "Unsupported parameter type for decoding: " + type->toString());
	}
}

bool FunctionCallCompiler::checkForTvmDeployMethods(MemberAccess const &_node, Type::Category category,
                                                    const std::vector<ASTPointer<Expression const>> &arguments,
                                                    FunctionCall const &_functionCall) {
	auto pushArgs = [&]() {
		for (const ASTPointer<const Expression> &e : arguments) {
			acceptExpr(e.get());
		}
	};

	auto functionType = dynamic_cast<FunctionType const*>(_functionCall.expression().annotation().type);
	if (category != Type::Category::Magic || functionType->kind() != FunctionType::Kind::TVMDeploy)
		return false;

	if(_node.memberName() == "buildStateInit") {
		pushArgs();
		m_pusher.pushPrivateFunctionOrMacroCall(-2 + 1, "build_state_init_macro");
		return true;
	}

	if(_node.memberName() == "insertPubkey") {
		pushArgs();
		m_pusher.pushPrivateFunctionOrMacroCall(-2 + 1, "insert_pubkey_macro");
		return true;
	}

	if(_node.memberName() == "deploy") {
		pushArgs();
		m_pusher.pushPrivateFunctionOrMacroCall(-4, "deploy_contract2_macro");
		return true;
	}

	if(_node.memberName() == "deployAndCallConstructor") {
		for (size_t i = 0; i < 4; i++)
			acceptExpr(arguments[i].get());
		std::vector<Type const*> types;
		std::vector<ASTNode const*> nodes;
		for (size_t i = 4; i < arguments.size(); ++i) {
			types.push_back(arguments[i]->annotation().type);
			nodes.push_back(arguments[i].get());
		}
		m_pusher.push(+1, "NEWC");
		StackPusherHelper::EncodePosition position{0};
		m_pusher.encodeParameters(types, nodes, [&](std::size_t index) {
			acceptExpr(arguments[index + 4].get());
		}, position);
		m_pusher.pushPrivateFunctionOrMacroCall(-5, "deploy_contract_macro");
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkForTvmSliceMethods(MemberAccess const &_node, Type::Category category,
                                                   const std::vector<ASTPointer<Expression const>> &arguments,
                                                   FunctionCall const &_functionCall) {
	if (category != Type::Category::TvmSlice)
		return false;

	if (_node.memberName() == "size") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+2, "SBITREFS");
		return true;
	}
	if (_node.memberName() == "bits") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "SBITS");
		return true;
	}
	if (_node.memberName() == "refs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "SREFS");
		return true;
	}
	if (_node.memberName() == "loadRefAsSlice") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		m_pusher.push(-1+2, "LDREFRTOS");
		m_pusher.exchange(0, 1);
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}
	if (_node.memberName() == "loadRef") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		m_pusher.push(-1+2, "LDREF");
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}
	if (_node.memberName() == "decode") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		TypePointers targetTypes;
		if (auto const* targetTupleType = dynamic_cast<TupleType const*>(_functionCall.annotation().type))
			targetTypes = targetTupleType->components();
		else
			targetTypes = TypePointers{_functionCall.annotation().type};

		for (auto type: targetTypes) {
			loadTypeFromSlice(_node, type);
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}
	if (_node.memberName() == "decodeFunctionParams") {
		const int saveStackSize = m_pusher.getStack().size();

		Expression const* expr = arguments.at(0).get();
		auto identifier = to<Identifier>(expr);
		Declaration const* declaration = identifier->annotation().referencedDeclaration;
		auto functionDefinition = to<FunctionDefinition>(declaration);

		const TVMExpressionCompiler::LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(),true, false, _node.expression().annotation().isLValue);

		DecodeFunctionParams decoder{&m_pusher};
		decoder.decodeParameters(functionDefinition->parameters());

		const int saveStackSize2 = m_pusher.getStack().size();
		const int paramQty = functionDefinition->parameters().size();
		m_pusher.blockSwap(saveStackSize2 - saveStackSize - paramQty, paramQty);

		m_pusher.push(+1, "PUSHSLICE x8_");
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "loadUnsigned" || _node.memberName() == "loadSigned") {
		const TVMExpressionCompiler::LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(),true, false, _node.expression().annotation().isLValue);
		std::string cmd = "LD";
		cmd += (_node.memberName() == "loadSigned" ? "I" : "U");
		if (auto bitsLiteral = to<Literal>(arguments[0].get()))
			m_pusher.push(+1, cmd + " " + bitsLiteral->value());
		else {
			acceptExpr(arguments[0].get());
			m_pusher.push(-1+1, cmd + "X");
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}
	return false;
}

void FunctionCallCompiler::store(MemberAccess const &_node, TypePointer type, bool reverse) {
	if (auto itype = to<IntegerType>(type)) {
		if (itype->isSigned())
			m_pusher.push(-1, std::string("STI") + (reverse ? "R " : " ") + std::to_string(itype->numBits()));
		else
			m_pusher.push(-1, std::string("STU") + (reverse ? "R " : " ") + std::to_string(itype->numBits()));
	} else if (to<AddressType>(type) || to<TvmSliceType>(type)) {
		m_pusher.push(-1, std::string("STSLICE") + (reverse ? "R " : ""));
	} else if (to<TvmBuilderType>(type)) {
		m_pusher.push(-1, std::string("STB")  + (reverse ? "R " : ""));
	} else if (to<TvmCellType>(type)) {
		m_pusher.push(-1, std::string("STREF")  + (reverse ? "R " : ""));
	} else if (to<MappingType>(type)) {
		if (reverse)
			m_pusher.push(0, "SWAP");
		m_pusher.push(-1, "STDICT");
	} else if (auto structType = to<StructType>(type)) {
		if (!reverse)
			m_pusher.push(0, "SWAP");
		auto members = structType->structDefinition().members();
		m_pusher.untuple(members.size());
		m_pusher.reverse(members.size(), 0);
		m_pusher.blockSwap(1, members.size());
		for(auto member: members)
			store(_node, member->type(), false);
	} else if (auto arrayType = to<ArrayType>(type)) {
		if (!reverse)
			m_pusher.push(0, "SWAP");
		if (arrayType->isByteArray()) {
			m_pusher.push(-1, "STREFR");
		} else {
			m_pusher.push(-1 + 2, "UNPAIR"); // builder size dict
			m_pusher.exchange(0, 2); // dict size builder
			m_pusher.push(-1, "STU 32"); // dict builder
			m_pusher.push(-1, "STDICT"); // builder
		}
	} else {
		cast_error(_node, "Unsupported type for store().");
	}
}

bool FunctionCallCompiler::checkForTvmBuilderMethods(MemberAccess const &_node, Type::Category category,
                                                     const std::vector<ASTPointer<Expression const>> &arguments) {
	if (category != Type::Category::TvmBuilder)
		return false;

	if (_node.memberName().size() >= 5 && _node.memberName().substr(0,5) == "store") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);

		if (_node.memberName() == "storeRef") {
			acceptExpr(arguments[0].get());
			m_pusher.push(-1, "STBREFR");
		} else if (_node.memberName() == "store") {
			for (auto argument: arguments) {
				acceptExpr(argument.get());
				store(_node, argument->annotation().type);
			}
		} else if (_node.memberName() == "storeSigned" || _node.memberName() == "storeUnsigned") {
			std::string cmd = "ST";
			cmd += (_node.memberName() == "storeSigned" ? "I" : "U");
			acceptExpr(arguments[0].get());
			if (auto bitsLiteral = to<Literal>(arguments[1].get()))
				m_pusher.push(-1, cmd + "R " + bitsLiteral->value());
			else {
				acceptExpr(arguments[1].get());
				m_pusher.push(-2, cmd + "XR");
			}
		}

		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	acceptExpr(&_node.expression());

	if (_node.memberName() == "bits") {
		m_pusher.push(-1+1, "BBITS");
		return true;
	}

	if (_node.memberName() == "refs") {
		m_pusher.push(-1+1, "BREFS");
		return true;
	}

	if (_node.memberName() == "bitsAndRefs") {
		m_pusher.push(-1+2, "BBITREFS");
		return true;
	}

	if (_node.memberName() == "remBits") {
		m_pusher.push(-1+1, "BREMBITS");
		return true;
	}

	if (_node.memberName() == "remRefs") {
		m_pusher.push(-1+1, "BREMREFS");
		return true;
	}

	if (_node.memberName() == "remBitsAndRefs") {
		m_pusher.push(-1+2, "BREMBITREFS");
		return true;
	}

	if (_node.memberName() == "toCell") {
		m_pusher.push(-1+1, "ENDC");
		return true;
	}

	if (_node.memberName() == "toSlice") {
		m_pusher.push(-1+1, "ENDC");
		m_pusher.push(-1+1, "CTOS");
		return true;
	}

	return false;
}

bool FunctionCallCompiler::checkForStringMethods(MemberAccess const &_node,
																  const std::vector<ASTPointer<Expression const>> &arguments) {
	auto array = to<ArrayType>(_node.expression().annotation().type);
	if (!array || !array->isString())
		return false;
	acceptExpr(&_node.expression());
	m_pusher.push(+1 - 1, "CTOS");

	if (_node.memberName() == "substr") {
		for (const auto &arg : arguments) {
			acceptExpr(arg.get());
			m_pusher.push(+1 - 1, "MULCONST 8");
		}
		m_pusher.push(-3 + 1, "SDSUBSTR");
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1, "STSLICE");
		m_pusher.push(+1 - 1, "ENDC");
		return true;
	}

	if (_node.memberName() == "byteLength") {
		m_pusher.push(-1 + 1, "SBITS");
		m_pusher.push(-1 + 1, "RSHIFT 3");
		return true;
	}

	return false;
}

bool FunctionCallCompiler::checkForTvmCellMethods(MemberAccess const &_node, Type::Category category,
                                                  const std::vector<ASTPointer<Expression const>> &) {
	if (category != Type::Category::TvmCell)
		return false;
	acceptExpr(&_node.expression());

	if (_node.memberName() == "toSlice") {
		m_pusher.push(-1+1, "CTOS");
		return true;
	}

	return false;
}

void FunctionCallCompiler::addressMethod(FunctionCall const &_functionCall) {
	auto _node = to<MemberAccess>(&_functionCall.expression());
	const std::vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	const std::vector<ASTPointer<ASTString>>& names = _functionCall.names();
	if (_node->memberName() == "transfer") { // addr.transfer(...)
		m_pusher.push(0, ";; transfer()");
		if (!_functionCall.names().empty()) {
			std::map<int, Expression const *> exprs;
			std::map<int, std::string> constParams{{TvmConst::int_msg_info::ihr_disabled, "1"}, {TvmConst::int_msg_info::bounce, "1"}};
			std::function<int()> pushBody;
			std::function<void()> pushSendrawmsgFlag;

			exprs[TvmConst::int_msg_info::dest] = &_node->expression();
			// string("value"), string("bounce"), string("flag"), string("body"), string("currencies")
			for (int arg = 0; arg < static_cast<int>(arguments.size()); ++arg) {
				switch (str2int(names[arg]->c_str())) {
					case str2int("value"):
						exprs[TvmConst::int_msg_info::grams] = arguments[arg].get();
						break;
					case str2int("bounce"):
						exprs[TvmConst::int_msg_info::bounce] = arguments[arg].get();
						constParams.erase(TvmConst::int_msg_info::bounce);
						break;
					case str2int("flag"):
						pushSendrawmsgFlag = [e = arguments[arg], this](){
							TVMExpressionCompiler{m_pusher}.compileNewExpr(e.get());
						};
						break;
					case str2int("body"):
						pushBody = [e = arguments[arg], this](){
							TVMExpressionCompiler{m_pusher}.compileNewExpr(e.get());
							return -1;
						};
						break;
					case str2int("currencies"):
						exprs[TvmConst::int_msg_info::currency] = arguments[arg].get();
						break;
				}
			}
			m_pusher.sendIntMsg(exprs, constParams, pushBody, pushSendrawmsgFlag);
		} else {
			if (arguments.size() == 3) {
				const std::map<int, Expression const *> exprs{
						{TvmConst::int_msg_info::grams,  arguments[0].get()},
						{TvmConst::int_msg_info::bounce, arguments[1].get()},
						{TvmConst::int_msg_info::dest,   &_node->expression()},
				};
				m_pusher.sendIntMsg(
						exprs,
						{{TvmConst::int_msg_info::ihr_disabled, "1"}},
						nullptr, [&]() { acceptExpr(arguments[2].get()); });
			} else if (arguments.size() == 4) {
				const std::map<int, Expression const *> exprs{
						{TvmConst::int_msg_info::grams,  arguments[0].get()},
						{TvmConst::int_msg_info::bounce, arguments[1].get()},
						{TvmConst::int_msg_info::dest,   &_node->expression()},
				};

				m_pusher.sendIntMsg(
						exprs,
						{{TvmConst::int_msg_info::ihr_disabled, "1"}},
						[&]() {
							acceptExpr(arguments[3].get());
							m_pusher.push(0, "CTOS");
							m_pusher.push(+1, "NEWC");
							m_pusher.push(-1, "STSLICE");
							return -1;
						},
						[&]() { acceptExpr(arguments[2].get()); });
			} else if (arguments.size() == 1) {
				m_pusher.sendIntMsg(
						{{TvmConst::int_msg_info::grams, arguments[0].get()},
						 {TvmConst::int_msg_info::dest,  &_node->expression()}},
						{{TvmConst::int_msg_info::ihr_disabled, "1"},
						 {TvmConst::int_msg_info::bounce,       "1"}},
						nullptr, nullptr);


			} else {
				solAssert(false, "");
			}
		}
		return;
	}
	if (_node->memberName() == "isStdZero") {
		m_pusher.push(0, ";; address.isStdZero()");
		acceptExpr(&_node->expression());
		m_pusher.pushZeroAddress();
		m_pusher.push(-2 + 1, "SDEQ");
		return;
	}
	if (_node->memberName() == "isExternZero") {
		m_pusher.push(0, ";; address.isExternZero()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1, "PUSHSLICE x401_");
		m_pusher.push(-2 + 1, "SDEQ");
		return;
	}
	if (_node->memberName() == "isNone") {
		m_pusher.push(0, ";; address.isNone()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1, "PUSHSLICE x2_");
		m_pusher.push(-2 + 1, "SDEQ");
		return ;
	}
	if (_node->memberName() == "unpack") {
		m_pusher.push(0, ";; address.unpack()");
		acceptExpr(&_node->expression());
		m_pusher.pushPrivateFunctionOrMacroCall(-1 + 2, "unpack_address_macro");
		return;
	}
	if (_node->memberName() == "getType") {
		m_pusher.push(0, ";; address.getType()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1 - 1, "PLDU 2");
		return;
	}
}

bool FunctionCallCompiler::checkForTvmConfigParamFunction(MemberAccess const &_node, Type::Category /*category*/,
                                                          const std::vector<ASTPointer<Expression const>> &arguments) {
	if (_node.memberName() == "configParam") { // tvm.configParam
		auto paramNumberLiteral = dynamic_cast<const Literal *>(arguments[0].get());

		Type const* type = paramNumberLiteral->annotation().type;
		u256 value = type->literalValue(paramNumberLiteral);
		std::string paramNumber = value.str();

		//	function tvm_config_param1() pure private returns (uint256, bool) { }
		if (paramNumber == "1") {
			//_ elector_addr:bits256 = ConfigParam 1;
			m_pusher.push(0, "PUSHINT 1");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 256");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 2, "IFELSE");
		}

		//	function tvm_config_param15() pure private returns (uint32, uint32, uint32, uint32, bool) { }
		//	function tvm_config_param17() pure private returns (uint32, uint32, uint32, uint32, bool) { }
		if (paramNumber == "15") {
			//_ validators_elected_for:uint32 elections_start_before:uint32
			//  elections_end_before:uint32 stake_held_for:uint32
			//  = ConfigParam 15;
			m_pusher.push(0, "PUSHINT 15");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 5, "IFELSE");
		}

		if (paramNumber == "17"){
			//_    min_stake:Grams    max_stake:Grams
			//     min_total_stake:Grams    max_stake_factor:uint32 = ConfigParam 17;
			m_pusher.push(0, "PUSHINT 17");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDGRAMS");
			contOk.push("LDGRAMS");
			contOk.push("LDGRAMS");
			contOk.push("LDU 32");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 5, "IFELSE");
		}

		//    function tvm_config_param34() private pure returns (
		//        uint8 /*constructor_id*/,
		//        uint32 /*utime_since*/,
		//        uint32 /*utime_until*/,
		//        uint16 /*total*/,
		//        uint16 /*main*/,
		//        uint64 /*total_weight*/,
		//        mapping(uint16 => ValidatorDescr73) memory,
		//        bool ok
		//    ) { }
		if (paramNumber == "34") {
			// _ cur_validators:ValidatorSet = ConfigParam 34;
			// validators#11 utime_since:uint32 utime_until:uint32
			// total:(## 16) main:(## 16) { main <= total } { main >= 1 }
			// list:(Hashmap 16 ValidatorDescr) = ValidatorSet;
			// validators_ext#12 utime_since:uint32 utime_until:uint32
			// total:(## 16) main:(## 16) { main <= total } { main >= 1 }
			// total_weight:uint64 list:(HashmapE 16 ValidatorDescr) = ValidatorSet;
			// validator#53 public_key:SigPubKey weight:uint64 = ValidatorDescr;
			// validator_addr#73 public_key:SigPubKey weight:uint64 adnl_addr:bits256 = ValidatorDescr;
			// ed25519_pubkey#8e81278a pubkey:bits256 = SigPubKey;  // 288 bits

			m_pusher.push(0, "PUSHINT 34");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 8"); // constructor
			contOk.push("LDU 32"); // utime_since
			contOk.push("LDU 32"); // utime_until
			contOk.push("LDU 16"); // total
			contOk.push("LDU 16"); // main
			contOk.push("LDU 64"); // total_weight
			contOk.push("LDDICT"); // ValidatorDescr
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0"); // constructor
			contFail.push("PUSHINT 0"); // utime_since
			contFail.push("PUSHINT 0"); // utime_until
			contFail.push("PUSHINT 0"); // total
			contFail.push("PUSHINT 0"); // main
			contFail.push("PUSHINT 0"); // total_weight
			contFail.push("NEWDICT"); // ValidatorDescr
			contFail.push("PUSHINT 0"); //
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 8, "IFELSE");
		}
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkForTvmSendFunction(MemberAccess const &_node, Type::Category /*category*/,
                                                   const std::vector<ASTPointer<Expression const>> &arguments) {
	if (_node.memberName().find("send") == std::string::npos)
		return false;

	if (_node.memberName() == "sendMsg") { // tvm.sendMsg(dest_address, funcId_literal)
		m_pusher.push(+1, "NEWC");
		std::string s;
		s += "0"; // int_msg_info$0
		s += "1"; // ihr_disabled
		s += "1"; // bounce
		s += "0"; // bounced
		s += "00"; // src: addr_none$00
		if (auto dstLiteral = to<Literal>(arguments[0].get())) { // dest
			s += m_pusher.literalToSliceAddress(dstLiteral, false);
		} else {
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(s));
			m_pusher.push(-1, "STSLICER");
			s = "";
			acceptExpr(arguments[0].get());
			m_pusher.push(-1, "STSLICER");
		}
		// value:CurrencyCollection
		if (arguments.size() > 3) {
			if (auto valueLiteral = to<Literal>(arguments[3].get())) { // dest
				s += StackPusherHelper::gramsToBinaryString(valueLiteral);
			} else {
				cast_error(_node, "tvm.sendMsg() forth param should be value literal");
			}
		} else {
			// default value
			s += "0011"; // value len
			s += "100110001001011010000000"; // value  10_000_000 is 10_000 * gas price (1000)
		}
		s += "0"; // other:ExtraCurrencyCollection
		s += "0000"; // ihr_fee:Grams
		s += "0000"; // fwd_fee:Grams
		s += std::string(64, '0'); // created_lt:uint64
		s += std::string(32, '0'); // created_at:uint32
		if (auto funcidLiteral = to<Literal>(arguments[1].get())) {
			Type const* type = funcidLiteral->annotation().type;
			u256 value = type->literalValue(funcidLiteral);
			s += "0"; // Maybe (Either StateInit ^StateInit)
			s += "0"; // body:(Either X ^X)
			StackPusherHelper::addBinaryNumberToString(s, value, 32);
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(s));

		} else {
			cast_error(_node, "tvm.sendMsg() second param should be funcID literal");
		}
		m_pusher.push(-1, "STSLICER");
		m_pusher.push(+1-1, "ENDC");

		if (auto flagLiteral = to<Literal>(arguments[2].get())) {
			Type const* type = flagLiteral->annotation().type;
			u256 value = type->literalValue(flagLiteral);
			m_pusher.push(+1, "PUSHINT " + value.str());
		} else {
			cast_error(_node, "tvm.sendMsg() third param should be flag literal");
		}
		m_pusher.push(-2, "SENDRAWMSG");
		return true;
	}
	if (_node.memberName() == "sendrawmsg") { // tvm.sendrawmsg
		for (const auto &arg : arguments) {
			acceptExpr(arg.get());
		}
		m_pusher.push(-2, "SENDRAWMSG");
		return true;
	}
	cast_error(_node, "Unsupported tvm.send* function.");
}

bool FunctionCallCompiler::checkForMsgFunction(MemberAccess const &_node, Type::Category category,
                                               const std::vector<ASTPointer<Expression const>> &/*arguments*/) {
	if (category != Type::Category::Magic)
		return false;

	auto identifier = to<Identifier>(&_node.expression());
	if (!identifier) {
		return false;
	}
	if (identifier->name() == "msg") {
		if (_node.memberName() == "pubkey") { // msg.pubkey
			m_pusher.pushLines(R"(
GETGLOB 5
DUP
ISNULL
PUSHCONT {
	DROP
	PUSHINT 0
}
IF
)");
			m_pusher.push(+1, ""); // fix stack
			return true;
		}
	}
	cast_error(_node, "Unsupported magic");
	return false;
}

bool FunctionCallCompiler::checkForTvmFunction(const MemberAccess &_node, Type::Category /*category*/,
                                               const vector<ASTPointer<const Expression>> &arguments) {
	auto pushArgs = [&]() {
		for (const ASTPointer<const Expression> &e : arguments) {
			acceptExpr(e.get());
		}
	};

	if (_node.memberName() == "cdatasize") { // tvm.cdatasize(cell, uint)
		pushArgs();
		m_pusher.push(-2 + 3, "CDATASIZE");
		return true;
	}
	if (_node.memberName() == "pubkey") { // tvm.pubkey
		m_pusher.push(+1, "GETGLOB 2");
		return true;
	}
	if (_node.memberName() == "accept") { // tvm.accept
		m_pusher.push(0, "ACCEPT");
		return true;
	}
	if (_node.memberName() == "transfer") { // tvm.transfer
		if (arguments.size() == 4) {
			const std::map<int, Expression const *> exprs {
					{TvmConst::int_msg_info::grams, arguments[1].get()},
					{TvmConst::int_msg_info::bounce, arguments[2].get()},
					{TvmConst::int_msg_info::dest, arguments[0].get()},
			};
			m_pusher.sendIntMsg(
					exprs,
					{{TvmConst::int_msg_info::ihr_disabled, "1"}},
					nullptr,
					[&](){acceptExpr(arguments[3].get());});
		} else {
			const std::map<int, Expression const *> exprs {
					{TvmConst::int_msg_info::grams, arguments[1].get()},
					{TvmConst::int_msg_info::bounce, arguments[2].get()},
					{TvmConst::int_msg_info::dest, arguments[0].get()},
			};
			m_pusher.sendIntMsg(
					exprs,
					{{TvmConst::int_msg_info::ihr_disabled, "1"}},
					[&](){
						acceptExpr(arguments[4].get());
						m_pusher.push(0, "CTOS");
						m_pusher.push(+1, "NEWC");
						m_pusher.push(-1, "STSLICE");
						return -1;
					},
					[&](){acceptExpr(arguments[3].get());});
		}
		return true;
	}
	if (_node.memberName() == "hash") { // tvm.hash
		pushArgs();
		m_pusher.push(0, "HASHCU");
		return true;
	}
	if (_node.memberName() == "checkSign") { // tvm.checkSign
		acceptExpr(arguments[0].get());
		m_pusher.push(+1, "NEWC");
		acceptExpr(arguments[1].get());
		m_pusher.push(-1, "STUR 256");
		acceptExpr(arguments[2].get());
		m_pusher.push(-1, "STUR 256");
		m_pusher.push(0, "ENDC CTOS");
		acceptExpr(arguments[3].get());
		m_pusher.push(-3+1, "CHKSIGNU");
		return true;
	}
	if (_node.memberName() == "setcode") { // tvm.setcode
		pushArgs();
		m_pusher.push(-1, "SETCODE");
		return true;
	}
	if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
		pushArgs();
		m_pusher.push(-1+1, "CTOS");
		m_pusher.push(0, "BLESS");
		m_pusher.push(-1, "POP c3");
		return true;
	}
	if (_node.memberName() == "commit") { // tvm.commit
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
		m_pusher.push(0, "COMMIT");
		return true;
	}
	if (_node.memberName() == "log") { // tvm.log
		auto logstr = arguments[0].get();
		if (auto literal = to<Literal>(logstr)) {
			if (literal->value().length() > 15)
				cast_error(_node, "logtvm param should no more than 15 chars");
			if (TVMContractCompiler::g_without_logstr) {
				return true;
			}
			m_pusher.push(0, "PRINTSTR " + literal->value());
		} else {
			cast_error(_node, "tvm.log() param should be literal");
		}
		return true;
	}
	if (_node.memberName() == "transLT") { // tvm.transLT
		pushArgs();
		m_pusher.push(+1, "LTIME");
		return true;
	}
	if (_node.memberName() == "resetStorage") { //tvm.resetStorage
		m_pusher.resetAllStateVars();
		return true;
	}
	if (_node.memberName() == "setExtDestAddr") {
		pushArgs();
		m_pusher.push(-1, "SETGLOB " + toString(TvmConst::C7::ExtDestAddrIndex));
		return true;
	}
	if (_node.memberName() == "functionId") { // tvm.functionId
		Expression const* expr = arguments.at(0).get();
		auto identifier = to<Identifier>(expr);
		Declaration const* declaration = identifier->annotation().referencedDeclaration;
		m_pusher.push(+1, "PUSHINT $" + declaration->name() + "$");
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkForMemberAccessTypeType(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TypeType)
		return false;
	if (auto identifier = to<Identifier>(&_node.expression())) {
		const auto& iname = identifier->name();
		if (auto funcitonType = to<FunctionType>(getType(&_node))) {
			// calling base contract method
			auto functionName = iname + "_" + _node.memberName();
			m_pusher.pushCall(functionName, funcitonType);
			return true;
		}
	}
	return false;
}

bool FunctionCallCompiler::checkAddressThis(FunctionCall const &_functionCall) {
	auto arguments = _functionCall.arguments();
	// compile  "address(this)"
	if (isAddressThis(&_functionCall)) {
		m_pusher.push(+1, "MYADDR");
		return true;
	}
	return false;
}

void FunctionCallCompiler::typeConversion(FunctionCall const &_functionCall) {
	auto arguments = _functionCall.arguments();
	Type::Category argCategory = arguments[0]->annotation().type->category();
	Type const* argType = arguments[0]->annotation().type;
	Type const* resultType = _functionCall.annotation().type;

	auto acceptArg = [&arguments, this] () {
		for (const auto &arg : arguments)
			acceptExpr(arg.get());
		solAssert(arguments.size() == 1, "");
	};
	auto conversionToAddress = [&_functionCall, &argCategory, &acceptArg, &arguments, this](){
		switch (argCategory) {
			case Type::Category::Contract:
			case Type::Category::Address:
				acceptArg();
				break;
			case Type::Category::RationalNumber:
			case Type::Category::Integer: {
				auto literal = to<Literal>(arguments[0].get());
				if (literal) {
					m_pusher.literalToSliceAddress(literal);
				} else {
					acceptArg();
					m_pusher.pushPrivateFunctionOrMacroCall(-1 + 1, "make_std_address_with_zero_wid_macro");
				}
				break;
			}
			default:
				cast_error(_functionCall, "Unsupported casting.");
		}
	};

	if (auto etn = to<ElementaryTypeNameExpression>(&_functionCall.expression())) {
		auto defaultActionForCasting = [&acceptArg, &etn, this]() {
			acceptArg();
			acceptExpr(etn);
		};
		switch (etn->type().typeName().token()) {
			case Token::BytesM: {
				acceptArg();
				int diff = 0;
				if (argCategory == Type::Category::FixedBytes) {
					auto fixedBytesType = to<FixedBytesType>(arguments[0]->annotation().type);
					diff = 8 * static_cast<int>(etn->type().typeName().firstNumber()) -
					       8 * static_cast<int>(fixedBytesType->storageBytes());
				} else if (argCategory == Type::Category::Address) {
					m_pusher.pushPrivateFunctionOrMacroCall(-1 + 1, "address2uint_macro");
					diff = 8 * static_cast<int>(etn->type().typeName().firstNumber()) - static_cast<int>(256);
					if (diff < 0) {
						cast_warning(_functionCall,
						             "Such conversion can cause loss of data. Only first bytes are used in such assignment.");
					}
				} else {
					cast_error(_functionCall, "Unsupported casting.");
				}

				if (diff > 0) {
					m_pusher.push(0, "LSHIFT " + std::to_string(diff));
				} else if (diff < 0) {
					m_pusher.push(0, "RSHIFT " + std::to_string(-diff));
				}
				break;
			}
			case Token::Address: {
				conversionToAddress();
				break;
			}
			case Token::IntM:
			case Token::UIntM: {
				auto a = to<IntegerType>(argType);
				auto r = to<IntegerType>(resultType);
				if (a && r && a->isSigned() == r->isSigned() && a->numBits() <= r->numBits()) {
					// nothing to do here
					acceptArg();
				} else {
					defaultActionForCasting();
				}
				break;
			}
			case Token::Int:
			case Token::UInt: {
				auto a = to<IntegerType>(argType);
				auto r = to<IntegerType>(resultType);
				if (a && r && a->isSigned() == r->isSigned() && a->numBits() <= r->numBits()) {
					// nothing to do here
					acceptArg();
				} else if (argCategory == Type::Category::Contract || argCategory == Type::Category::Address) {
					auto literal = to<Literal>(arguments[0].get());
					if (literal) {
						const u256 value = literal->annotation().type->literalValue(literal);
						m_pusher.push(+1, "PUSHINT " + toString(value));
					} else {
						acceptArg();
						m_pusher.pushPrivateFunctionOrMacroCall(-1 + 1, "address2uint_macro");
						cast_warning(_functionCall, "Workchain id is lost in this conversion.");
					}
				} else {
					defaultActionForCasting();
				}
				break;
			}
			case Token::Bytes:
			case Token::String: {
				if (isStringOrStringLiteralOrBytes(argType)) {
					acceptArg(); // nothing to do here
				} else {
					cast_error(_functionCall, "Unsupported casting.");
				}
				break;
			}
			default:
				defaultActionForCasting();
		}
	} else if (auto identifier = to<Identifier>(&_functionCall.expression())) {
		if (to<ContractDefinition>(identifier->annotation().referencedDeclaration)) {
			conversionToAddress();
		} else if (auto enumDef = to<EnumDefinition>(identifier->annotation().referencedDeclaration)) {

			if (auto e = to<UnaryOperation>(_functionCall.arguments()[0].get())) {
				auto literal = to<Literal>(&e->subExpression());
				if ((e->getOperator() == Token::Sub) && (literal)) {
					const auto* type = literal->annotation().type;
					u256 value = type->literalValue(literal);
					if (value != 0)
						cast_error(_functionCall, "Argument for enum casting should not be negative.");
					m_pusher.pushInt(0);
					return;
				}
			}
			if (auto literal = to<Literal>(_functionCall.arguments()[0].get())) {
				const auto* type = literal->annotation().type;
				u256 value = type->literalValue(literal);
				if (value >= enumDef->members().size())
					cast_error(_functionCall, "Argument for enum casting should not exceed enum limit.");
				m_pusher.push(+1, "PUSHINT " + toString(value));
				return;
			}

			acceptExpr(_functionCall.arguments()[0].get());
			m_pusher.push(+1, "DUP");
			m_pusher.pushInt(enumDef->members().size());
			m_pusher.push(-1, "GEQ");

			auto type = _functionCall.arguments()[0].get()->annotation().type;
			TypeInfo ti(type);
			if (!ti.isNumeric || ti.isSigned)
			{
				m_pusher.push(+1, "OVER");
				m_pusher.push(0, "ISNEG");
				m_pusher.push(-1, "OR");
			}
			m_pusher.push(-1, "THROWIF 5");
		} else {
			cast_error(_functionCall, "Unsupported type conversion");
		}
	} else {
		cast_error(_functionCall, "Unsupported type conversion");
	}
}

bool FunctionCallCompiler::checkLocalFunctionCall(const Identifier *identifier) {
	string functionName = identifier->name();
	auto function = m_pusher.ctx().getLocalFunction(functionName);
	if (!function)
		return false;
	// Calling local function
	auto functionType = to<FunctionType>(getType(identifier));
	auto funDef = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
	solAssert(functionType, "#209");
	if (isFunctionForInlining(funDef)) {
		auto &codeLines = m_pusher.ctx().m_inlinedFunctions.at(functionName);
		int nParams = functionType->parameterTypes().size();
		int nRetVals = functionType->returnParameterTypes().size();

		m_pusher.push(codeLines);
		m_pusher.push(-nParams + nRetVals, "");
	} else {
		m_pusher.pushCall(m_pusher.ctx().getFunctionInternalName(function), functionType);
	}
	return true;
}

bool FunctionCallCompiler::checkSolidityUnits(FunctionCall const &_functionCall) {
	auto identifier = to<Identifier>(&_functionCall.expression());
	if (identifier == nullptr) {
		return false;
	}

	string iname = identifier->name();
	if (iname == "sha256") {
		acceptExpr(_functionCall.arguments()[0].get());
		m_pusher.push(0, "CTOS");
		m_pusher.push(0, "SHA256U");
		return true;
	}
	if (iname == "selfdestruct") {
		m_pusher.push(+1, "NEWC");
		m_pusher.push(+1-1, "ENDC");
		m_pusher.push(-1, "SETCODE");

		const std::map<int, std::string> constParams {
				{TvmConst::int_msg_info::ihr_disabled, "1"},
				{TvmConst::int_msg_info::grams,  StackPusherHelper::gramsToBinaryString(1000)}, // TODO why not 0? Is here bug in local node?
				{TvmConst::int_msg_info::bounce,  "0"},
		};
		m_pusher.sendIntMsg(
				{{TvmConst::int_msg_info::dest, _functionCall.arguments()[0].get()}},
				constParams,
				nullptr,
				[&](){ m_pusher.push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::CarryAllMoney)); });
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkForIdentifier(FunctionCall const &_functionCall) {
	auto arguments = _functionCall.arguments();
	auto expr = &_functionCall.expression();
	auto identifier = to<Identifier>(expr);
	if (!identifier)
		return false;

	string iname = identifier->name();

	if (iname == "logtvm") {
		auto logstr = arguments[0].get();
		if (auto literal = to<Literal>(logstr)) {
			if (literal->value().length() > 15)
				cast_error(_functionCall, "logtvm param should no more than 15 chars");
			if (TVMContractCompiler::g_without_logstr) {
				return true;
			}
			m_pusher.push(0, "PRINTSTR " + literal->value());
		} else {
			cast_error(_functionCall, "logtvm param should be literal");
		}
		return true;
	}
	for (const auto& arg : arguments) {
		acceptExpr(arg.get());
	}

	if (checkLocalFunctionCall(identifier)) {
	} else if (m_pusher.getStack().isParam(identifier->annotation().referencedDeclaration)) {
		// Local variable of functional type
		acceptExpr(expr);
		auto functionType = to<FunctionType>(identifier->annotation().type);
		int returnCnt = functionType->returnParameterTypes().size();
		int paramCnt = functionType->parameterTypes().size();
		m_pusher.push(-1 - paramCnt + returnCnt, "CALLX");
	} else {
		cast_error(*identifier, "Unknown identifier");
	}
	return true;
}

bool FunctionCallCompiler::checkNewExpression(FunctionCall const &_functionCall) {

	if (to<NewExpression>(&_functionCall.expression()) == nullptr) {
		return false;
	}
	Type const *resultType = _functionCall.annotation().type;
	if (resultType->category() == Type::Category::Contract) {
		cast_error(_functionCall, "Unsupported such contract creating. Use tvm_deploy_contract(...)");
	}

	int size = m_pusher.getStack().size();

	m_pusher.push(0, ";; new " + resultType->toString(true));
	m_pusher.push(+1, "NEWDICT"); // dict
	acceptExpr(_functionCall.arguments()[0].get()); // dict size
	m_pusher.push(+1, "DUP"); // dict size sizeIter


	auto arrayType = to<ArrayType>(resultType);
	const IntegerType key = getKeyTypeOfArray();
	Type const* arrayBaseType = arrayType->baseType();

	{
		CodeLines code;
		code.push("DUP");
		m_pusher.pushCont(code);
	}
	{
		StackPusherHelper pusherHelper(&m_pusher.ctx(), 1);
		pusherHelper.push(0, "DEC"); // dict size sizeIter'
		pusherHelper.pushDefaultValue(arrayBaseType, true); // dict size sizeIter' value
		// TODO optimize. Locate default value on stack (don't create default value in each iteration)
		bool isValueBuilder = pusherHelper.prepareValueForDictOperations(&key, arrayBaseType, true); // arr value'
		pusherHelper.push(2, "PUSH2 S1,S3"); // dict size sizeIter' value sizeIter' dict
		pusherHelper.setDict(key, *arrayType->baseType(), isValueBuilder, _functionCall); // dict size sizeIter' dict'
		pusherHelper.push(0, "POP S3"); // dict' size sizeIter'
		m_pusher.pushCont(pusherHelper.code());
	}
	m_pusher.push(-2, "WHILE");
	// dict size 0
	m_pusher.drop(1);  // dict size

	m_pusher.push(0, "SWAP");
	m_pusher.push(-2 + 1, "PAIR");

	solAssert(size + 1 == m_pusher.getStack().size(), "");
	return true;
}

bool FunctionCallCompiler::checkTvmIntrinsic(FunctionCall const &_functionCall) {
	IntrinsicsCompiler ic(m_pusher);
	return ic.checkTvmIntrinsic(_functionCall);
}
