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

#pragma once

#include "TVMCommons.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMIntrinsics.hpp"

namespace solidity::frontend {

class FunctionCallCompiler {
	StackPusherHelper& m_pusher;
	IExpressionCompiler* const m_exprCompiler;

protected:
	void acceptExpr(const Expression* expr) {
		m_exprCompiler->compileNewExpr(expr);
	}

public:
	FunctionCallCompiler(StackPusherHelper& m_pusher, IExpressionCompiler* exprCompiler) :
		m_pusher{m_pusher},
		m_exprCompiler{exprCompiler} {

	}

	void structConstructorCall(FunctionCall const& _functionCall) {
		auto const& type = dynamic_cast<TypeType const&>(*_functionCall.expression().annotation().type);
		auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
		for (const ASTPointer<Expression const> &arg : _functionCall.arguments()) {
			acceptExpr(arg.get());
		}
		StructCompiler structCompiler{&m_pusher, &structType};
		structCompiler.structConstructor(_functionCall.names()); // create struct and drop args
	}

	void compile(FunctionCall const& _functionCall) {

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

		// TODO: move to function
		auto arguments = _functionCall.arguments();
		auto expr = &_functionCall.expression();
		if (auto ma = to<MemberAccess>(expr)) {
			auto category = getType(&ma->expression())->category();
			if (checkForTvmSendFunction(*ma, category, arguments)) return;
			if (checkForTvmConfigParamFunction(*ma, category, arguments)) return;
			if (checkForTvmSliceMethods(*ma, category, arguments, _functionCall)) return;

			if (!isIn(ma->memberName(), "log", "transfer", "checkSign")) {
				for (const auto &arg : arguments) {
					acceptExpr(arg.get());
				}
			}
			if (checkForSuper(*ma, category)) return;
			if (checkForAddressMethods(*ma, category, arguments)) return;
			if (checkForTvmCellMethods(*ma, category, arguments)) return;
			if (checkForMemberAccessTypeType(*ma, category)) return;
			if (checkForMagicFunction(*ma, category, arguments)) return;
			if (checkForTypeTypeMember(*ma, category)) return;
			cast_error(*ma, "Unsupported function call");
		}

		solAssert(false, "#1036");
	}

protected:
	bool checkForSuper(MemberAccess const& _node, Type::Category) {
		// argument are on stack
		if (!isSuper(&_node.expression()))
			return false;
		m_pusher.push(0, ";; super");
		string fname = _node.memberName();
		auto super = getSuperContract(m_pusher.ctx().getContract(m_pusher.ctx().m_currentFunction->m_function),
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

	bool checkForTypeTypeMember(MemberAccess const& _node, Type::Category category) {
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

	// TODO unite with decodeParameter
	void loadTypeFromSlice(MemberAccess const& _node, TypePointer type) {
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

	bool checkForTvmSliceMethods(MemberAccess const& _node, Type::Category category,
									const std::vector<ASTPointer<Expression const>> & /*arguments*/,
										FunctionCall const& _functionCall) {
		if (category != Type::Category::TvmSlice)
			return false;

		if (_node.memberName() == "size") {
			acceptExpr(&_node.expression());
			m_pusher.push(-1+2, "SBITREFS");
			return true;
		}
		if (_node.memberName() == "loadRefAsSlice") {
			const IExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
			m_pusher.push(-1+2, "LDREFRTOS");
			m_pusher.exchange(0, 1);
			m_exprCompiler->collectLValue(lValueInfo, true, false);
			return true;
		}
		if (_node.memberName() == "loadRef") {
			const IExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
			m_pusher.push(-1+2, "LDREF");
			m_exprCompiler->collectLValue(lValueInfo, true, false);
			return true;
		}
		if (_node.memberName() == "decode") {
			const IExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);

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
		return false;
	}

	bool checkForTvmCellMethods(MemberAccess const& _node, Type::Category category,
									const std::vector<ASTPointer<Expression const>> & /*arguments*/) {
		if (category != Type::Category::TvmCell)
			return false;
		acceptExpr(&_node.expression());

		if (_node.memberName() == "toSlice") {
			m_pusher.push(-1+1, "CTOS");
			return true;
		}

		return false;
	}

	bool checkForAddressMethods(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments) {
		if (category != Type::Category::Address)
			return false;
		if (_node.memberName() == "transfer") {
			// address.transfer(value)
			m_pusher.push(0, ";; transfer()");
			if (arguments.size() == 3) {
				acceptExpr(&_node.expression());
				for (const auto& arg : arguments)
					acceptExpr(arg.get());
				m_pusher.pushPrivateFunctionOrMacroCall(-4, "send_accurate_internal_message_macro");
			} else if (arguments.size() == 4) {
				acceptExpr(&_node.expression());
				for (const auto& arg : arguments)
					acceptExpr(arg.get());
				m_pusher.pushPrivateFunctionOrMacroCall(-5, "send_accurate_internal_message_with_body_macro");
			} else {
				for (const auto& arg : arguments)
					acceptExpr(arg.get());
				acceptExpr(&_node.expression());
				// stack: value address
				m_pusher.push(+1, "NEWC"); // empty body
				m_pusher.pushPrivateFunctionOrMacroCall(-3, "send_internal_message_macro");
			}
			return true;
		}
		if (_node.memberName() == "isStdZero") {
			m_pusher.push(0, ";; address.isStdZero()");
			acceptExpr(&_node.expression());
			m_pusher.pushZeroAddress();
			m_pusher.push(-2 + 1, "SDEQ");
			return true;
		}
		if (_node.memberName() == "isExternZero") {
			m_pusher.push(0, ";; address.isExternZero()");
			acceptExpr(&_node.expression());
			m_pusher.push(+1, "PUSHSLICE x401_");
			m_pusher.push(-2 + 1, "SDEQ");
			return true;
		}
		if (_node.memberName() == "isNone") {
			m_pusher.push(0, ";; address.isNone()");
			acceptExpr(&_node.expression());
			m_pusher.push(+1, "PUSHSLICE x2_");
			m_pusher.push(-2 + 1, "SDEQ");
			return true;
		}
		if (_node.memberName() == "unpack") {
			m_pusher.push(0, ";; address.unpack()");
			acceptExpr(&_node.expression());
			m_pusher.pushPrivateFunctionOrMacroCall(-1 + 2, "unpack_address_macro");
			return true;
		}
		if (_node.memberName() == "getType") {
			m_pusher.push(0, ";; address.getType()");
			acceptExpr(&_node.expression());
			m_pusher.push(+1 - 1, "PLDU 2");
			return true;
		}
		return false;
	}

	bool checkForTvmConfigParamFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments) {
		if (category != Type::Category::Magic)
			return false;

		auto expr = to<Identifier>(&_node.expression());

		if (expr == nullptr || expr->name() != "tvm")
			return false;

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

	bool checkForTvmSendFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments) {
		if (category != Type::Category::Magic)
			return false;

		auto expr = to<Identifier>(&_node.expression());

		if (expr == nullptr || expr->name() != "tvm" || _node.memberName().find("send") == std::string::npos)
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

	bool checkForMagicFunction(MemberAccess const& _node, Type::Category category, const std::vector<ASTPointer<Expression const>> & arguments) {
		if (category != Type::Category::Magic)
			return false;

		auto expr = to<Identifier>(&_node.expression());
		if (expr != nullptr && expr->name() == "msg") {
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
		if (expr != nullptr && expr->name() == "tvm") {
			if (_node.memberName() == "cdatasize") { // tvm.cdatasize(cell, uint)
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
				for (const auto& arg : arguments)
					acceptExpr(arg.get());
				if (arguments.size() == 4)
					m_pusher.pushPrivateFunctionOrMacroCall(-4, "send_accurate_internal_message_macro");
				else
					m_pusher.pushPrivateFunctionOrMacroCall(-5, "send_accurate_internal_message_with_body_macro");
				return true;
			}
			if (_node.memberName() == "hash") { // tvm.hash
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
				m_pusher.push(-1, "SETCODE");
				return true;
			}
			if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
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
					if (IExpressionCompiler::isWithoutLogstr()) {
						return true;
					}
					m_pusher.push(0, "PRINTSTR " + literal->value());
				} else {
					cast_error(_node, "tvm.log() param should be literal");
				}
				return true;
			}
			if (_node.memberName() == "transLT") { // tvm.transLT
				m_pusher.push(+1, "LTIME");
				return true;
			}
			if (_node.memberName() == "resetStorage") { //tvm.resetStorage
				m_pusher.resetAllStateVars();
				return true;
			}
		}
		cast_error(_node, "Unsupported magic");
		return false;
	}

	bool checkForMemberAccessTypeType(MemberAccess const& _node, Type::Category category) {
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

	bool checkAddressThis(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		// compile  "address(this)"
		if (isAddressThis(&_functionCall)) {
			m_pusher.push(+1, "MYADDR");
			return true;
		}
		return false;
	}

	void typeConversion(FunctionCall const& _functionCall) {
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

	bool checkLocalFunctionCall(const Identifier* identifier) {
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

	bool checkSolidityUnits(FunctionCall const& _functionCall) {
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
			for (const auto& arg : _functionCall.arguments()) {
				acceptExpr(arg.get()); //  remote_addr
			}
			m_pusher.push(+1, "NEWC");
			m_pusher.push(+1-1, "ENDC");
			m_pusher.push(-1, "SETCODE");

			m_pusher.push(+1, "PUSHINT 1000"); // grams_value // TODO why not 0? Is here bug in local node?
			m_pusher.push(+1, "PUSHINT 0"); // bounce
			m_pusher.push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::CarryAllMoney)); // sendrawmsg_flag
			m_pusher.pushPrivateFunctionOrMacroCall(-4, "send_accurate_internal_message_macro");
			return true;
		}
		return false;
	}

	bool checkForIdentifier(FunctionCall const& _functionCall) {
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
				if (IExpressionCompiler::isWithoutLogstr()) {
					return true;
				}
				m_pusher.push(0, "PRINTSTR " + literal->value());
			} else {
				cast_error(_functionCall, "logtvm param should be literal");
			}
			return true;
		}
		// TODO: Refactor this ugly function
		for (const auto& arg : arguments) {
			acceptExpr(arg.get());
		}

		if (checkLocalFunctionCall(identifier)) {
		} else if (m_pusher.getStack().isParam(iname)) {
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

	bool checkNewExpression(FunctionCall const& _functionCall) {

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
			StackPusherHelper pusherHelper(&m_pusher.ctx());
			pusherHelper.push(0, "DEC"); // dict size sizeIter'
			pusherHelper.pushDefaultValue(arrayBaseType, true); // dict size sizeIter' value
			// TODO optimize. Locate default value on stack (don't create default value in each iteration)
			bool isValueBuilder = pusherHelper.prepareValueForDictOperations(&key, arrayBaseType, true); // arr value'
			pusherHelper.push(0, "PUSH2 S1,S3"); // dict size sizeIter' value sizeIter' dict
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

	bool checkTvmIntrinsic(FunctionCall const& _functionCall) {
		IntrinsicsCompiler ic(m_pusher, m_exprCompiler);
		return ic.checkTvmIntrinsic(_functionCall);
	}

};

}	// solidity
