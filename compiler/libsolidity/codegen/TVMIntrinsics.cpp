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
 * TVM intrinsics codegen routines
 */

#include <string>

#include <boost/algorithm/string.hpp>

#include <libsolidity/ast/Types.h>

#include "TVMCommons.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMIntrinsics.hpp"


namespace solidity::frontend {


bool IntrinsicsCompiler::checkTvmIntrinsic(FunctionCall const &_functionCall) {

	std::vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	auto identifier = to<Identifier>(&_functionCall.expression());
	if (!identifier) {
		return false;
	}

	string iname = identifier->name();
	const auto opcode = boost::starts_with(iname, "tvm_")?
	                    boost::to_upper_copy<std::string>(iname.substr(4)) : "UNKNOWN";

	auto defaultAction = [&arguments, this, &opcode](const int delta) {
		for (const ASTPointer<Expression const> &arg : arguments) {
			acceptExpr(arg.get());
		}
		m_pusher.push(-static_cast<int>(arguments.size()) + delta, opcode);
	};

	auto ensureParamIsIdentifier = [&] (int idx) -> Identifier const* {
		auto param = to<Identifier>(arguments[idx].get());
		solAssert(param && m_pusher.getStack().isParam(param->annotation().referencedDeclaration), "");
		return param;
	};

	auto checkArgCount = [&](std::size_t argCount) {
		if (arguments.size() != argCount) {
			if (argCount == 0) {
				cast_error(_functionCall, iname + " should have no argument");
			} else if (argCount == 1) {
				cast_error(_functionCall, iname + " should have one argument");
			} else {
				cast_error(_functionCall, iname + " should have " + toString(argCount) + " arguments");
			}
		}
	};

	if (iname == "tvm_ldu" || iname == "tvm_ldi")  {
		auto slice = ensureParamIsIdentifier(0);
		if (arguments.size() >= 3) {
			acceptExpr(slice);
			for (size_t i = 1; i < arguments.size(); ++i) {
				auto bits = arguments[i].get();
				auto literal = to<Literal>(bits);
				if (!literal) {
					cast_error(*arguments[i].get(), "Should be literal");
				}
				m_pusher.push(+1, opcode + " " + literal->value());
			}
			solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		} else {
			auto bits = arguments[1].get();
			auto literal = to<Literal>(bits);
			if (m_pusher.getStack().getOffset(slice->annotation().referencedDeclaration) == 0 && literal) {
				m_pusher.push(+1, opcode + " " + literal->value());
				m_pusher.push(0, "SWAP");
			} else {
				acceptExpr(slice);
				if (literal) {
					m_pusher.push(+1, opcode + " " + literal->value());
				} else {
					acceptExpr(bits);
					m_pusher.push(0, opcode + "X");
				}
				solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
			}
		}
		return true;
	}
	if (iname == "tvm_pldu") {
		checkArgCount(2);
		if (auto literal = to<Literal>(arguments[1].get())) {
			acceptExpr(arguments[0].get());
			m_pusher.push(-1 + 1, opcode + " " + literal->value());
		} else {
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			m_pusher.push(-2 + 1, opcode + "X");
		}
		return true;
	}
	if (iname == "tvm_ldmsgaddr") {
		auto slice = ensureParamIsIdentifier(0);
		acceptExpr(slice);
		m_pusher.push(-1 + 2, opcode);
		solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		return true;
	}
	if (iname == "tvm_pldmsgaddr") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 2, "LDMSGADDR");
		m_pusher.push(-1, "DROP");
		return true;
	}
	if (iname == "tvm_ldslice") {
		auto slice = ensureParamIsIdentifier(0);
		acceptExpr(slice);
		auto bits = arguments[1].get();
		if (auto literal = to<Literal>(bits)) {
			m_pusher.push(+1, "LDSLICE " + literal->value());
		} else {
			acceptExpr(bits);
			m_pusher.push(0, "LDSLICEX");
		}
		solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		return true;
	}
	if (isIn(iname, "tvm_ldref", "tvm_lddict")) {
		Identifier const* slice = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(slice->annotation().referencedDeclaration) == 0) {
			m_pusher.push(+1, opcode);
			m_pusher.push(0, "SWAP");
		} else {
			acceptExpr(slice);
			m_pusher.push(+1, opcode);
			solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (iname == "tvm_setindex") {
		const Identifier* tuple = ensureParamIsIdentifier(0);
		auto literal = to<Literal>(arguments[1].get());
		if (!literal)
			cast_error(*arguments[1].get(),"Should be literal.");
		if (m_pusher.getStack().getOffset(tuple->annotation().referencedDeclaration) == 0) {
			acceptExpr(arguments[2].get());
			m_pusher.push(-2 + 1, "SETINDEX " + literal->value());
		} else {
			cast_error(_functionCall, "");
		}
		return true;
	}
	if (iname == "tvm_sti" || iname == "tvm_stu") {
		const Identifier* builder = ensureParamIsIdentifier(0);
		auto bitLen = arguments[1].get();
		auto literal = to<Literal>(bitLen);
		if (m_pusher.getStack().getOffset(builder->annotation().referencedDeclaration) == 0) {
			acceptExpr(arguments[2].get());
			if (literal) {
				m_pusher.push(-1, opcode + "R " + literal->value());
			} else {
				acceptExpr(bitLen);
				m_pusher.push(-2, opcode + "XR");
			}
		} else {
			acceptExpr(arguments[2].get());
			acceptExpr(builder);
			if (literal) {
				m_pusher.push(-1, opcode + " " + literal->value());
			} else {
				acceptExpr(bitLen);
				m_pusher.push(-2, opcode + "X");
			}
			solAssert(m_pusher.tryAssignParam(builder->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (iname == "tvm_stslice" || iname == "tvm_stref") {
		// function(builder, arg)
		checkArgCount(2);
		auto builder = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(builder->annotation().referencedDeclaration) == 0) {
			acceptExpr(arguments[1].get());
			m_pusher.push(- 1, opcode + "R");
		} else {
			acceptExpr(arguments[1].get());
			acceptExpr(builder);
			m_pusher.push(-1, opcode);
			solAssert(m_pusher.tryAssignParam(builder->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (iname == "tvm_dictuset" || iname == "tvm_dictusetb") {
		// tvm_dictset(uint value, uint key, uint dict, uint keyBits)
		auto dict = ensureParamIsIdentifier(2);
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		acceptExpr(dict);
		acceptExpr(arguments[3].get());
		m_pusher.push(-4 + 1, opcode);
		solAssert(m_pusher.tryAssignParam(dict->annotation().referencedDeclaration), "");
		return true;
	}
	if (iname == "tvm_bchkbitsq" || iname == "tvm_schkbitsq") {
		// function(uint builderOrSlice, uint nbits) returns (bool)
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		m_pusher.push(-2 + 1, opcode);
		return true;
	}
	if (iname == "tvm_sbitrefs") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 2, opcode);
		return true;
	}
	if (iname == "tvm_sbits") {
		// function tvm_sbits(uint slice) private pure returns (uint /*the number of data bits in slice*/)
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 1, "SBITS");
		return true;
	}
	if (iname == "tvm_srefs") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1+1, "SREFS");
		return true;
	}
	if (iname == "tvm_brembits")
	{
		// function tvm_brembits(uint builder) private pure returns (uint /*the number of data bits that can still be stored in b*/)
		acceptExpr(arguments[0].get());
		m_pusher.push(-1+1, "BREMBITS");
		return true;
	}
	if (iname == "tvm_get_slice_from_integer_dict") {
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		acceptExpr(arguments[2].get());
		acceptExpr(arguments[3].get()); // bitSizeOfArrayElement, idx, array, 32
		m_pusher.push(-4+1, "DICTUGET");
		m_pusher.push(0,    "PUSHCONT { ");
		m_pusher.push(0,    "\tNIP");
		m_pusher.push(0,    "}");
		m_pusher.push(0,    "PUSHCONT {");
		m_pusher.push(0,    "\tNEWC      ; valueBits builder");
		m_pusher.push(0,    "\tPUSHINT 0 ; valueBits builder 0");
		m_pusher.push(0,    "\tXCHG S2   ; 0 builder valueBits");
		m_pusher.push(0,    "\tSTUX      ; builder");
		m_pusher.push(0,    "\tENDC      ; cell");
		m_pusher.push(0,    "\tCTOS      ; default_value");
		m_pusher.push(0,    "}");
		m_pusher.push(0,    "IFELSE");
		return true;
	}
	if (isIn(iname, "tvm_ctos", "tvm_tlen")) {
		acceptExpr(arguments[0].get());
		m_pusher.push(0, opcode);
		return true;
	}
	if (iname == "tvm_ends") {
		auto slice = ensureParamIsIdentifier(0);
		acceptExpr(slice);
		m_pusher.push(-1, "ENDS");
		m_pusher.push(+1, "NULL");
		solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		return true;
	}
	if (iname == "tvm_newc") {
		m_pusher.push(+1, "NEWC");
		return true;
	}
	if (iname == "tvm_endc") {
		// tvm_endc(uint slice) returns (uint /* cell */)
		acceptExpr(arguments[0].get());
		m_pusher.push( 0, "ENDC");
		return true;
	}
	if (iname == "tvm_my_public_key") {
		m_pusher.pushPrivateFunctionOrMacroCall(+1, "my_pubkey_macro");
		return true;
	}
	if (iname == "tvm_c4_key_length") {
		m_pusher.push(+1,   "PUSHINT " + toString(TvmConst::C4::KeyLength));
		return true;
	}
	if (iname == "tvm_exception_replayprotection") {
		m_pusher.push(+1, "PUSHINT " + toString(TvmConst::RuntimeException::ReplayProtection));
		return true;
	}
	if (iname == "tvm_exception_unpackaddress") {
		m_pusher.push(+1, "PUSHINT " + toString(TvmConst::RuntimeException::AddressUnpackException));
		return true;
	}
	if (iname == "tvm_exception_insertpubkey") {
		m_pusher.push(+1, "PUSHINT " + toString(TvmConst::RuntimeException::InsertPubkeyException));
		return true;
	}
	if (iname == "tvm_default_replay_protection_interval"){
		m_pusher.push(+1, "PUSHINT " + toString(TvmConst::Message::ReplayProtection::Interval));
		return true;
	}
	if (iname == "tvm_now") {
		m_pusher.push(+1, "NOW");
		return true;
	}
	if (iname == "tvm_balance") {
		m_pusher.pushPrivateFunctionOrMacroCall(+1, "get_balance");
		return true;
	}
	if (iname == "tvm_newdict") {
		m_pusher.push(+1, "NEWDICT");
		return true;
	}
	if (iname == "tvm_c4") {
		m_pusher.push(+1, "PUSHROOT");
		return true;
	}
	if (iname == "tvm_c7") {
		m_pusher.push(+1, "PUSHCTR c7");
		return true;
	}
	if (iname == "tvm_first" || iname == "tvm_second" || iname == "tvm_third") {
		acceptExpr(arguments[0].get());
		m_pusher.push(0, opcode);
		return true;
	}
	if (iname == "tvm_index") {
		acceptExpr(arguments[0].get());
		auto elementNum = arguments[1].get();
		if (auto literal = to<Literal>(elementNum)) {
			m_pusher.push(0, "INDEX " + literal->value());
		} else {
			cast_error(_functionCall, "tvm_index param should be integer");
		}
		return true;
	}
	if (iname == "tvm_sendrawmsg") {
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		m_pusher.push(-2, "SENDRAWMSG");
		return true;
	}
	if (iname == "tvm_ubitsize") {
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "UBITSIZE");
		return true;
	}
	if (iname == "tvm_stdict") {
		Identifier const* builder = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(builder->annotation().referencedDeclaration) == 0) {
			// there is no stdictr. Using swap
			acceptExpr(arguments[1].get());
			m_pusher.push(0, "SWAP");
			m_pusher.push(-1, "STDICT");
		} else {
			acceptExpr(arguments[1].get());
			acceptExpr(builder);
			m_pusher.push(-1, "STDICT");
			solAssert(m_pusher.tryAssignParam(builder->annotation().referencedDeclaration), "");
		}
		return true;
	}

	if (iname == "tvm_tpush" || iname == "tvm_setthird" ||
	    iname == "tvm_stbrefr" || iname == "tvm_stbr") {
		// function(container, value)
		checkArgCount(2);
		Identifier const* holder = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(holder->annotation().referencedDeclaration) == 0) {
			acceptExpr(arguments[1].get());
			m_pusher.push(-1, opcode);
		} else {
			acceptExpr(holder);
			acceptExpr(arguments[1].get());
			m_pusher.push(-1, opcode);
			solAssert(m_pusher.tryAssignParam(holder->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (iname == "tvm_tuple0") {
		m_pusher.push(+1, "TUPLE 0");
		return true;
	}
	if (iname == "tvm_popctr") {
		acceptExpr(arguments[1].get());
		if (auto literal = to<Literal>(arguments[0].get())) {
			m_pusher.push(-1, "POPCTR c" + literal->value());
		} else {
			cast_error(_functionCall, "tvm_popctr first param should be integer");
		}
		return true;
	}
	if (isIn(iname, "tvm_sdskipfirst")) {
		checkArgCount(2);
		for (auto & argument : arguments) {
			acceptExpr(argument.get());
		}
		m_pusher.push(-static_cast<int>(arguments.size()) + 1, opcode);
		return true;
	}
	if (iname == "tvm_dictudel") {
		acceptExpr(arguments[1].get());
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[2].get());
		m_pusher.push(-1, "DICTUDEL");
		m_pusher.push(-1, "DROP");
		return true;
	}
	if (iname == "tvm_isnull") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "ISNULL");
		return true;
	}
	if (iname == "tvm_srempty") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "SREMPTY");
		return true;
	}
	if (iname == "tvm_sdempty") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "SDEMPTY");
		return true;
	}
	if (iname == "tvm_ldrefrtos") {
		checkArgCount(1);
		auto builder = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(builder->annotation().referencedDeclaration) == 0) {
			m_pusher.push(+1, "LDREFRTOS");
		} else {
			cast_error(_functionCall, R"(Use "tvm_ldrefrtos" only if)" + builder->name() + " is on stack top");
		}
		return true;
	}
	if (iname == "tvm_pldref_and_to_slice") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(+1, "LDREFRTOS");
		m_pusher.push(-1, "NIP");
		return true;
	}
	if (iname == "tvm_hashsu") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "HASHSU");
		return true;
	}
	if (iname == "tvm_hashcu") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "HASHCU");
		return true;
	}
	if (iname == "tvm_subslice") {
		defaultAction(+1);
		return true;
	}
	if (iname == "tvm_pldslice") {
		acceptExpr(arguments[0].get());
		auto literal = to<Literal>(arguments[1].get());
		if (!literal)
			cast_error(*arguments[1].get(), "Should be literal");
		u256 value = arguments[1]->annotation().type->literalValue(literal);
		m_pusher.push(-1 + 1, "PLDSLICE " + toString(value));
		return true;
	}
	if (iname == "tvm_selector_call") {
		checkArgCount(2);
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		m_pusher.push(-2, "JMP 1");
		return true;
	}
	if (iname == "tvm_sempty") {
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "SEMPTY");
		return true;
	}
	if (iname == "tvm_bremrefs" || iname == "tvm_sbits" || iname == "tvm_bbits") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1+1, opcode);
		return true;
	}
	if (iname == "tvm_pldrefvar") {
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		m_pusher.push(-1, "PLDREFVAR");
		return true;
	}
	if (iname == "tvm_setcode") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "SETCODE");
		return true;
	}
	if (iname == "tvm_push_minus_one") {
		checkArgCount(0);
		m_pusher.push(+1, "PUSHINT -1");
		return true;
	}
	if (iname == "tvm_myaddr") {
		checkArgCount(0);
		m_pusher.push(+1, "MYADDR");
		return true;
	}
	if (iname == "tvm_stzeroes" || iname == "tvm_stones" || iname == "tvm_stgrams") {
		// function(builder, argument)
		checkArgCount(2);
		Identifier const* builder = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(builder->annotation().referencedDeclaration) == 0) {
			acceptExpr(arguments[1].get());
			m_pusher.push(- 1, opcode);
		} else {
			acceptExpr(builder);
			acceptExpr(arguments[1].get());
			m_pusher.push(-2 + 1, opcode);
			solAssert(m_pusher.tryAssignParam(builder->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (iname == "tvm_skipdict") {
		// tvm_skipdict(uint slice)
		checkArgCount(1);
		Identifier const* slice = ensureParamIsIdentifier(0);
		if (m_pusher.getStack().getOffset(slice->annotation().referencedDeclaration) == 0) {
			m_pusher.push(0, opcode);
		} else {
			acceptExpr(slice);
			m_pusher.push(0, opcode);
			solAssert(m_pusher.tryAssignParam(slice->annotation().referencedDeclaration), "");
		}
		return true;
	}
	if (isIn(iname, "tvm_plddict", "tvm_plddicts")) {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 1, opcode);
		return true;
	}
	if (iname == "tvm_getparam") {
		if (auto literal = to<Literal>(arguments[0].get())) {
			m_pusher.push(+1, "GETPARAM " + literal->value());
		} else {
			cast_error(_functionCall, "First param should be integer literal");
		}
		return true;
	}
	if (iname == "tvm_plddict" || iname == "tvm_iszero") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(-1+1, opcode);
		return true;
	}
	if (iname == "tvm_poproot") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "POPROOT");
		return true;
	}
	if (iname == "tvm_throwany") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "THROWANY");
		return true;
	}
	if (iname == "tvm_throw") {
		checkArgCount(1);
		auto literal = to<Literal>(arguments[0].get());
		if (!literal) {
			cast_error(*arguments[0].get(), "Must be string");
		}
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "THROW " + literal->value());
		return true;
	}
	if (iname == "tvm_accept") {
		checkArgCount(0);
		m_pusher.push(0, opcode);
		return true;
	}
	if (iname == "tvm_parsemsgaddr") {
		checkArgCount(1);
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 1, opcode);
		return true;
	}
	if (iname == "tvm_skip_and_load_uint256_in_slice_copy"){
		// tvm_skip_and_load_in_slice_copy(uint slice, uint skippedLength) returns (uint)
		checkArgCount(2);
		acceptExpr(arguments[0].get());
		acceptExpr(arguments[1].get());
		m_pusher.push(-1, "SDSKIPFIRST");
		m_pusher.push(0, "PLDU 256");
		return true;
	}
	if (iname == "tvm_config_param1") {
		//_ elector_addr:bits256 = ConfigParam 1;
//		pushLog("config_param1");
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

		return true;
	}
	if (iname == "tvm_config_param15") {
		//_ validators_elected_for:uint32 elections_start_before:uint32
		//  elections_end_before:uint32 stake_held_for:uint32
		//  = ConfigParam 15;
//		pushLog("config_param15");
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

		return true;
	}
	if (iname == "tvm_config_param17") {
		//_    min_stake:Grams    max_stake:Grams    min_total_stake:Grams    max_stake_factor:uint32 = ConfigParam 17;
//		pushLog("config_param17");
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

		return true;
	}
	if (iname == "tvm_config_param34") {
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

		StackPusherHelper pusherHelper(&m_pusher.ctx());
		pusherHelper.push(0, "CTOS");

		pusherHelper.push(0, "LDU 8"); // constructor
		pusherHelper.push(0, "LDU 32"); // utime_since
		pusherHelper.push(0, "LDU 32"); // utime_until
		pusherHelper.push(0, "LDU 16"); // total
		pusherHelper.push(0, "LDU 16"); // main
		pusherHelper.push(0, "LDU 64"); // total_weight
		pusherHelper.push(0, "LDDICT"); // ValidatorDescr
		pusherHelper.push(0, "ENDS");
		pusherHelper.push(0, "PUSHINT -1");

		CodeLines contFail;
		contFail.push("PUSHINT 0"); // constructor
		contFail.push("PUSHINT 0"); // utime_since
		contFail.push("PUSHINT 0"); // utime_until
		contFail.push("PUSHINT 0"); // total
		contFail.push("PUSHINT 0"); // main
		contFail.push("PUSHINT 0"); // total_weight
		contFail.push("NEWDICT"); // ValidatorDescr
		contFail.push("PUSHINT 0"); //

		m_pusher.pushCont(pusherHelper.code());
		m_pusher.pushCont(contFail);
		m_pusher.push(-2 + 8, "IFELSE");

		return true;
	}
	if (iname == "tvm_reverse_push") { // index is staring from one
		m_pusher.push(+1, "DEPTH");
		acceptExpr(arguments[0].get());
		m_pusher.push(-2 + 1, "SUB");
		m_pusher.push(-1 + 1, "PICK");
		return true;
	}
	if (iname == "tvm_is_zero_address") {
		acceptExpr(arguments[0].get());
		m_pusher.pushLines(R"(PUSHSLICE x8000000000000000000000000000000000000000000000000000000000000000001_
SDEQ
)");
		return true;
	}
	if (iname == "tvm_zero_ext_address") {
		m_pusher.push(+1, "PUSHSLICE x2_");
		return true;
	}
	if (iname == "tvm_make_zero_address") {
		m_pusher.pushZeroAddress();
		return true;
	}
	if (iname == "tvm_unpackfirst4") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1 + 4, "UNPACKFIRST 4");
		return true;
	}
	if (iname == "tvm_tree_cell_size") {
		acceptExpr(arguments[0].get());
		m_pusher.pushLines(R"(NULL
SWAP
PUSHINT 0
PUSHINT 1 ; null s b r
PUSHCONT {
    ; null s... b r
	PUSH S2 ; null s... b r s
	SREFS   ; null s... b r cntRef
	PUSHCONT {
		; null s... b r
		ROT         ; null s... b r s
		LDREFRTOS   ; null s... b r s' new_s
		SWAP2       ; null s... s' new_s b r
		INC
	}
	PUSHCONT {
		; null s... b r
		XCHG S2 ; null s... r b s
		SBITS   ; null s... r b bs
		ADD     ; null s... r b
		SWAP    ; null s... b r
	}
	IFELSE
	PUSH S2
	ISNULL
}
UNTIL
; null b r
ROT  ; b r null
DROP ; b r
)");
		m_pusher.push(-1 + 2, ""); // fix stack
		return true;
	}
	if (iname == "tvm_reset_storage") {
		m_pusher.resetAllStateVars();
		return true;
	}
	if (iname == "tvm_pop_c3") {
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "POP c3");
		return true;
	}
	if (iname == "tvm_bless") {
		acceptExpr(arguments[0].get());
		m_pusher.push(0, "BLESS");
		return true;
	}
	if (iname == "tvm_stack") {
		m_pusher.push(+1, "DEPTH");
		return true;
	}
	if (iname == "tvm_sender_pubkey") {
		m_pusher.pushLines(R"(
PUSH C7
INDEXQ 5
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

	if (iname == "tvm_getglob") {
		auto literal = to<Literal>(arguments[0].get());
		if (!literal) {
			cast_error(*arguments[0].get(), "Must be string");
		}
		m_pusher.push(+1, "GETGLOB " + literal->value());
		return true;
	}
	if (iname == "tvm_setglob") {
		auto literal = to<Literal>(arguments[1].get());
		if (!literal) {
			cast_error(*arguments[1].get(), "Must be string");
		}
		acceptExpr(arguments[0].get());
		m_pusher.push(-1, "SETGLOB " + literal->value());
		return true;
	}
	if (iname == "tvm_migratePubkey") {
		std::string str = R"(
DEPTH
TUPLEVAR
SETGLOB 8
PUSHCONT {
	PUSHINT 0
	GETGLOB 2
	PUSHINT 64
	DICTUGET
	THROWIFNOT MigratePubkey
	PLDU 256
	SETGLOB 2
}
PUSHCONT {
	DROP2
}
TRY
GETGLOB 8
DUP
TLEN
UNTUPLEVAR)";
		boost::replace_all(str, "MigratePubkey", toString(TvmConst::RuntimeException::MigratePubkey));
		m_pusher.pushLines(str);
		return true;
	}
	return false;
}

void IntrinsicsCompiler::acceptExpr(const Expression *expr) {
	TVMExpressionCompiler{m_pusher}.compileNewExpr(expr);
}

} // end solidity::frontend
