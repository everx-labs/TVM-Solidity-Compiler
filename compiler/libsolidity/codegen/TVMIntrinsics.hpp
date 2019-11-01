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

#pragma once

#include <boost/algorithm/string.hpp>
#include <string>
#include "TVMCommons.hpp"
#include <libsolidity/ast/Types.h>

namespace dev {
namespace solidity {

class IntrinsicsCompiler : public StackPusherHelper {
	IExpressionCompiler* const m_exprCompiler;

protected:
	void acceptExpr(const Expression* expr) {
		m_exprCompiler->acceptExpr(expr);
	}
	
public:
	IntrinsicsCompiler(IStackPusher* pusher, IExpressionCompiler* exprCompiler, const TVMCompilerContext& ctx)
		: StackPusherHelper(pusher, &ctx)
		, m_exprCompiler(exprCompiler) {}
		
	bool checkTvmIntrinsic(FunctionCall const& _functionCall) {

		auto arguments = _functionCall.arguments();
		auto identifier = to<Identifier>(&_functionCall.expression());
		if (!identifier) {
			return false;
		}

		string iname = identifier->name();
		const auto opcode = boost::starts_with(iname, "tvm_")?
				boost::to_upper_copy<std::string>(iname.substr(4)) : "UNKNOWN";

		auto ensureParamIsIdentifier = [&] (int idx) -> const Identifier* {
			auto param = to<Identifier>(arguments[idx].get());
			solAssert(param && getStack().isParam(param->name()), "");
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

		auto checkArgCountMore = [&](std::size_t argCount) {
			if (arguments.size() < argCount) {
				cast_error(_functionCall, iname + " should have " + toString(argCount) + " arguments");
			}
		};

		if (iname == "tvm_logstr") {
			auto logstr = arguments[0].get();
			if (auto literal = to<Literal>(logstr)) {
				if (literal->value().length() > 15)
					cast_error(_functionCall, "tvm_logstr param should no more than 15 chars");
				push(0, "PRINTSTR " + literal->value());
			} else {
				cast_error(_functionCall, "tvm_logstr param should be literal");
			}
			return true;
		}
		if (iname == "tvm_ldu") { // TODO optimize
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			auto bits = arguments[1].get();
			if (auto literal = to<Literal>(bits)) {
				push(+1, "LDU " + literal->value());
			} else {
				acceptExpr(bits);
				push(0, "LDUX");
			}
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_ldi") { // TODO optimize
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			auto bits = arguments[1].get();
			if (auto literal = to<Literal>(bits)) {
				push(+1, "LDI " + literal->value());
			} else {
				acceptExpr(bits);
				push(0, "LDIX");
			}
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_pldu") {
			checkArgCount(2);
			auto literal = to<Literal>(arguments[1].get());
			if (literal == nullptr) {
				cast_error(_functionCall, "second param should be literal");
			}
			acceptExpr(arguments[0].get());
			push(-1 + 1, opcode + " " + literal->value());
			return true;
		}
		if (iname == "tvm_ldmsgaddr") {
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			push(-1 + 2, opcode);
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_ldslice") {
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			auto bits = arguments[1].get();
			if (auto literal = to<Literal>(bits)) {
				push(+1, "LDSLICE " + literal->value());
			} else {
				acceptExpr(bits);
				push(0, "LDSLICEX");
			}
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_ldref") {
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			push(+1, "LDREF");
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_sti" || iname == "tvm_stu") {
			const Identifier* builder = ensureParamIsIdentifier(0);
			auto bitLen = arguments[1].get();
			auto literal = to<Literal>(bitLen);
			if (getStack().getOffset(builder->name()) == 0) {
				acceptExpr(arguments[2].get());
				if (literal) {
					push(-1, opcode + "R " + literal->value());
				} else {
					acceptExpr(bitLen);
					push(-2, opcode + "XR");
				}
			} else {
				acceptExpr(arguments[2].get());
				acceptExpr(builder);
				if (literal) {
					push(-1, opcode + " " + literal->value());
				} else {
					acceptExpr(bitLen);
					push(-2, opcode + "X");
				}
				solAssert(tryAssignParam(builder->name()), "");
			}
			return true;
		}
		if (iname == "tvm_stslice" || iname == "tvm_stref") {
			// function(builder, arg)
			checkArgCount(2);
			auto builder = ensureParamIsIdentifier(0);
			if (getStack().getOffset(builder->name()) == 0) {
				acceptExpr(arguments[1].get());
				push(- 1, opcode + "R");
			} else {
				acceptExpr(arguments[1].get());
				acceptExpr(builder);
				push(-1, opcode);
				solAssert(tryAssignParam(builder->name()), "");
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
			push(-4 + 1, opcode);
			solAssert(tryAssignParam(dict->name()), "");
			return true;
		}
		if (iname == "tvm_bchkbitsq" || iname == "tvm_schkbitsq") {
			// function(uint builderOrSlice, uint nbits) returns (bool)
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-2 + 1, opcode);
			return true;
		}
		if (iname == "tvm_sbits") {
			// function tvm_sbits(uint slice) private pure returns (uint /*the number of data bits in slice*/)
			acceptExpr(arguments[0].get());
			push(-1+1, "SBITS ; tvm_sbits");
			return true;
		}
		if (iname == "tvm_srefs") {
			acceptExpr(arguments[0].get());
			push(-1+1, "SREFS ; tvm_srefs");
			return true;
		}
		if (iname == "tvm_brembits")
		{
			// function tvm_brembits(uint builder) private pure returns (uint /*the number of data bits that can still be stored in b*/)
			acceptExpr(arguments[0].get());
			push(-1+1, "BREMBITS ; tvm_brembits");
			return true;
		}
		if (iname == "tvm_getfromdict") {
			// tvm_getfromdict(uint dict, uint nbits, uint idx)
			// returns (uint /* slice */)
			acceptExpr(arguments[2].get());
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-3+2, "DICTUGET");
			push(-1, "THROWIFNOT 100");		// TODO!
			return true;
		}
		if (iname == "tvm_getfromdictdef") {
			// tvm_getfromdictdef(uint dict, uint keyBits, uint idx, uint valueBits)
			// returns (uint /* slice */)
			acceptExpr(arguments[3].get());
			acceptExpr(arguments[2].get());
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-4+1, "DICTUGET");
			push(0,    "PUSHCONT { ");
			push(0,    "\tNIP");
			push(0,    "}");
			push(0,    "PUSHCONT {");
			push(0,    "\tNEWC      ; valueBits builder");
			push(0,    "\tPUSHINT 0 ; valueBits builder 0");
			push(0,    "\tXCHG S2   ; 0 builder valueBits");
			push(0,    "\tSTUX      ; builder");
			push(0,    "\tENDC      ; cell");
			push(0,    "\tCTOS      ; default_value");
			push(0,    "}");
			push(0,    "IFELSE");
			return true;
		}
		if (iname == "tvm_array_length") {
			// tvm_array_length(uint array)
			// returns (uint)
			acceptExpr(arguments[0].get());
			pushInt(32);
			push(-2+3, 	"DICTUMAX");
			push(+1, 	"PUSHCONT { POP s1 INC }");
			push(+1, 	"PUSHCONT { PUSHINT 0 }");
			push(-3-1, 	"IFELSE");
			return true;
		}
		if (iname == "tvm_ctos") {
			// tvm_ctos(uint cell)
			// returns (uint /* slice */)
			acceptExpr(arguments[0].get());
			push(0, "CTOS");
			return true;
		}
		if (iname == "tvm_ends") {
			auto slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			push(-1, "ENDS");
			push(+1, "NULL");
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_init_storage") {
			push(+1, "NEWDICT");
			push( 0, "NEWC STDICT ENDC");
			push(-1, "POPROOT");
			return true;
		}
		if (iname == "tvm_newc") {
			push(+1, "NEWC");
			return true;
		}
		if (iname == "tvm_endc") {
			// tvm_endc(uint slice) returns (uint /* cell */)
			acceptExpr(arguments[0].get());
			push( 0, "ENDC");
			return true;
		}
		if (iname == "tvm_sender_pubkey") {
			push(+1, "PUSHCTR c7");
			push(0, "SECOND");
			push(0, "FIRST");
			return true;
		}
		if (iname == "tvm_my_public_key") {
			pushPrivateFunctionCall(+1, "my_pubkey");
			return true;
		}
		if (iname == "tvm_c4_key_length") {
			push(+1,   "PUSHINT " + toString(TvmConst::C4::KeyLength));
			return true;
		}
		if (iname == "tvm_exception_constructoriscalledtwice") {
			push(+1, "PUSHINT " + toString(TvmConst::Exception::ConstructorIsCalledTwice));
			return true;
		}
		if (iname == "tvm_exception_replayprotection") {
			push(+1, "PUSHINT " + toString(TvmConst::Exception::ReplayProtection));
			return true;
		}
		if (iname == "tvm_default_replay_protection_interval"){
			push(+1, "PUSHINT " + toString(TvmConst::ReplayProtection::Interval));
			return true;
		}
		if (iname == "tvm_now") {
			push(+1, "NOW");
			return true;
		}
		if (iname == "tvm_block_lt") {
			push(+1,   "BLOCKLT");
			return true;
		}
		if (iname == "tvm_trans_lt") {
			push(+1,   "LTIME");
			return true;
		}
		if (iname == "tvm_rand_seed") {
			pushPrivateFunctionCall(+1,   "get_rand_seed");
			return true;
		}
		if (iname == "tvm_balance") {
			pushPrivateFunctionCall(+1,   "get_balance");
			return true;
		}
		/*
		if (iname == "tvm_address") {
			pushPrivateFunctionCall(+1,   "get_self_address");
			return true;
		}
		*/
		if (iname == "tvm_newdict") {
			push(+1, "NEWDICT");
			return true;
		}
		if (iname == "tvm_c4") {
			push(+1, "PUSHROOT");
			return true;
		}
		if (iname == "tvm_c7") {
			push(+1, "PUSHCTR c7");
			return true;
		}
		if (iname == "tvm_first" || iname == "tvm_second" || iname == "tvm_third") {
			acceptExpr(arguments[0].get());
			push(0, opcode);
			return true;
		}
		if (iname == "tvm_index") {
			acceptExpr(arguments[0].get());
			auto elementNum = arguments[1].get();
			if (auto literal = to<Literal>(elementNum)) {
				push(0, "INDEX " + literal->value());
			} else {
				cast_error(_functionCall, "tvm_index param should be integer");
			}
			return true;
		}
		if (iname == "tvm_sendrawmsg") {
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-2, "SENDRAWMSG");
			return true;
		}
		if (iname == "tvm_ubitsize") {
			acceptExpr(arguments[0].get());
			push(0, "UBITSIZE");
			return true;
		}
		if (iname == "tvm_stdict") {
			Identifier const* builder = ensureParamIsIdentifier(0);
			if (getStack().getOffset(builder->name()) == 0) {
				// there is no stdictr. Using swap
				acceptExpr(arguments[1].get());
				push(0, "SWAP");
				push(-1, "STDICT");
			} else {
				acceptExpr(arguments[1].get());
				acceptExpr(builder);
				push(-1, "STDICT");
				solAssert(tryAssignParam(builder->name()), "");
			}
			return true;
		}

		if (iname == "tvm_tpush" || iname == "tvm_setthird" || 
			iname == "tvm_stbrefr" || iname == "tvm_stbr") {
			// function(container, value)
			checkArgCount(2);
			Identifier const* holder = ensureParamIsIdentifier(0);
			if (getStack().getOffset(holder->name()) == 0) {
				acceptExpr(arguments[1].get());
				push(-1, opcode);
			} else {
				acceptExpr(holder);
				acceptExpr(arguments[1].get());
				push(-1, opcode);
				solAssert(tryAssignParam(holder->name()), "");
			}
			return true;
		}
		if (iname == "tvm_tuple0") {
			push(+1, "TUPLE 0");
			return true;
		}
		if (iname == "tvm_popctr") {
			acceptExpr(arguments[1].get());
			if (auto literal = to<Literal>(arguments[0].get())) {
				push(-1, "POPCTR c" + literal->value());
			} else {
				cast_error(_functionCall, "tvm_popctr first param should be integer");
			}
			return true;
		}
		if (iname == "tvm_sdskipfirst") {
			checkArgCount(2);
			Identifier const* slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			acceptExpr(arguments[1].get());
			push(-1, "SDSKIPFIRST");
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_dictudel") {
			acceptExpr(arguments[1].get());
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[2].get());
			push(-1, "DICTUDEL");
			push(-1, "DROP");
			return true;
		}
		if (iname == "tvm_isnull") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(0, "ISNULL");
			return true;
		}
		if (iname == "tvm_srempty") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(0, "SREMPTY");
			return true;
		}
		if (iname == "tvm_sdempty") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(0, "SDEMPTY");
			return true;
		}
		if (iname == "tvm_chksignu") {
			if (arguments.size() == 3) {
				acceptExpr(arguments[0].get());
				acceptExpr(arguments[1].get());
				acceptExpr(arguments[2].get());
				push(-3 + 1, "CHKSIGNU");
				return true;
			} else if (arguments.size() == 4) {
				acceptExpr(arguments[0].get());

				push(+1, "NEWC");
				acceptExpr(arguments[1].get());
				push(-1, "STUR 256");
				acceptExpr(arguments[2].get());
				push(-1, "STUR 256");
				push(0, "ENDC CTOS");

				acceptExpr(arguments[3].get());
				push(-3+1, "CHKSIGNU");
				return true;
			}
		}
		if (iname == "tvm_ldrefrtos") {
			checkArgCount(1);
			auto builder = ensureParamIsIdentifier(0);
			if (getStack().getOffset(builder->name()) == 0) {
				push(+1, "LDREFRTOS");
			} else {
				cast_error(_functionCall, R"(Use "tvm_ldrefrtos" only if)" + builder->name() + " is on stack top");
			}
			return true;
		}
		if (iname == "tvm_hashsu") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(0, "HASHSU");
			return true;
		}
		if (iname == "tvm_hashcu") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(0, "HASHCU");
			return true;
		}
		if (iname == "tvm_deploy_contract") {
			checkArgCountMore(4);
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			acceptExpr(arguments[2].get());
			acceptExpr(arguments[3].get());

			Type const * type3 = arguments[3].get()->annotation().type.get();
			if (type3->category() == Type::Category::Struct && to<StructType>(type3)->structDefinition().name() == "TvmCell"){
				if (arguments.size() != 4) {
					cast_error(_functionCall,
					           "Correct argument types: (TvmCell memory my_contract, address addr, uint128 grams, TvmCell memory payload) "
					           "or (TvmCell memory my_contract, address addr, uint128 gram, uint constuctor_id, some_type0 constuctor_param0, some_type1 constuctor_param1, ...)");
				}
				pushPrivateFunctionCall(-4, "deploy_contract2");
			} else {
				pushLog("insr");
				// auto x = _functionCall.annotation().argumentTypes; why argumentTypes is always null ?
				std::vector<Type const*> types;
				std::vector<ASTNode const*> nodes;
				for (std::size_t i = 4; i < arguments.size(); ++i) {
					types.push_back(arguments[i]->annotation().type.get());
					nodes.push_back(arguments[i].get());
				}
				push(+1, "NEWC");
				encodeParams(types, nodes, [&](std::size_t index) {
					acceptExpr(arguments[index + 4].get());
				});
				pushLog("insr1");
				pushPrivateFunctionCall(-5, "deploy_contract");
			}
			return true;
		}
		if (iname == "tvm_drop_stack_and_jump") {
			checkArgCount(2);
			auto dropCount = getStack().size();
			acceptExpr(arguments[1].get());		// rest_body_slice
			acceptExpr(arguments[0].get());		// func_id
			dropUnder(2, dropCount);
			push(0, "JMP 1");
			push(-2+dropCount, "");		// fix stack
			return true;
		}
		if (iname == "tvm_sempty") {
			acceptExpr(arguments[0].get());
			push(0, "SEMPTY");
			return true;
		}
		if (iname == "tvm_ldrefnpop"){
			acceptExpr(arguments[0].get());
			push(+1, "LDREF");
			push(-1, "NIP");
			return true;
		}
		if (iname == "tvm_lddict") {
			checkArgCount(1);
			Identifier const* slice = ensureParamIsIdentifier(0);
			acceptExpr(slice);
			push(+1, "LDDICT");
			solAssert(tryAssignParam(slice->name()), "");
			return true;
		}
		if (iname == "tvm_bremrefs" || iname == "tvm_sbits" || iname == "tvm_bbits") {
			acceptExpr(arguments[0].get());
			push(-1+1, opcode);
			return true;
		}
		if (iname == "tvm_pldrefvar") {
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-1, "PLDREFVAR");
			return true;
		}
		if (iname == "tvm_setcode") {
			acceptExpr(arguments[0].get());
			push(-1, "SETCODE");
			return true;
		}
		if (iname == "tvm_unpack_address") {
			acceptExpr(arguments[0].get());
			pushPrivateFunctionCall(-1 + 2,"unpack_address");
			return true;
		}
		if (iname == "tvm_make_address") {
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			pushPrivateFunctionCall(-2 + 1, "make_address");
			return true;
		}
		if (iname == "tvm_push_callback_func_id") {
			checkArgCount(0);
			pushFunctionIndex("fallback");
			return true;
		}
		if (iname == "tvm_push_on_bounce_id") {
			checkArgCount(0);
			pushFunctionIndex("onBounce");
			return true;
		}
		if (iname == "tvm_push_minus_one") {
			checkArgCount(0);
			push(+1, "PUSHINT -1");
			return true;
		}
		if (iname == "tvm_myaddr") {
			checkArgCount(0);
			push(+1, "MYADDR");
			return true;
		}
		if (iname == "tvm_stzeroes" || iname == "tvm_stones" || iname == "tvm_stgrams") {
			// function(builder, argument)
			checkArgCount(2);
			Identifier const* builder = ensureParamIsIdentifier(0);
			if (getStack().getOffset(builder->name()) == 0) {
				acceptExpr(arguments[1].get());
				push(- 1, opcode);
			} else {
				acceptExpr(builder);
				acceptExpr(arguments[1].get());
				push(-2 + 1, opcode);
				solAssert(tryAssignParam(builder->name()), "");
			}
			return true;
		}
		if (iname == "tvm_skipdict") {
			// tvm_skipdict(uint slice)
			checkArgCount(1);
			Identifier const* slice = ensureParamIsIdentifier(0);
			if (getStack().getOffset(slice->name()) == 0) {
				push(0, opcode);
			} else {
				acceptExpr(slice);
				push(0, opcode);
				solAssert(tryAssignParam(slice->name()), "");
			}
			return true;
		}
		if (iname == "tvm_plddict") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(-1 + 1, opcode);
			return true;
		}
		if (iname == "tvm_insert_pubkey") {
			checkArgCount(2);
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			pushPrivateFunctionCall(-2+1, "insert_pubkey");
			return true;
		}
		if (iname == "tvm_getparam") {
			if (auto literal = to<Literal>(arguments[0].get())) {
				push(+1, "GETPARAM " + literal->value());
			} else {
				cast_error(_functionCall, "First param should be integer");
			}
			return true;
		}
		if (iname == "require") {
			acceptExpr(arguments[0].get());
			if (arguments.size() == 1)
				push(-1, "THROWIFNOT 100");
			else {
				if (auto literal = to<Literal>(arguments[1].get())) {
					push(-1, "THROWIFNOT " + literal->value());
				} else {
					cast_error(_functionCall, "tvm_throwifnot second param should be integer");
				}
			}
			return true;
		}
		if (iname == "tvm_plddict" || iname == "tvm_iszero") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(-1+1, opcode);
			return true;
		}
		if (iname == "tvm_transfer") {
			checkArgCount(4);
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			acceptExpr(arguments[2].get());
			acceptExpr(arguments[3].get());
			pushPrivateFunctionCall(-4, "accurate_transfer");
			return true;
		}
		if (iname == "tvm_poproot") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(-1, "POPROOT");
			return true;
		}
		if (iname == "tvm_throwany") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(-1, "THROWANY");
			return true;
		}
		if (iname == "tvm_accept") {
			checkArgCount(0);
			push(0, opcode);
			return true;
		}
		if (iname == "tvm_parsemsgaddr") {
			checkArgCount(1);
			acceptExpr(arguments[0].get());
			push(-1 + 1, opcode);
			return true;
		}
		if (iname == "tvm_skip_and_load_address_in_slice_copy"){ // TODO delete this
			// tvm_skip_and_load_in_slice_copy(uint slice, uint skippedLength) returns (uint)
			checkArgCount(2);
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			push(-1, "SDSKIPFIRST");
			push(0, "PLDU 256");
			return true;
		}
		if (iname == "tvm_build_state_init") {
			checkArgCount(2);
			acceptExpr(arguments[0].get());
			acceptExpr(arguments[1].get());
			pushPrivateFunctionCall(-2 + 1, "build_state_init");
			return true;
		}
		if (iname == "tvm_config_param15") {
			//_ validators_elected_for:uint32 elections_start_before:uint32
			//  elections_end_before:uint32 stake_held_for:uint32
			//  = ConfigParam 15;
			pushLog("config_param15");
			push(0, "PUSHINT 15");
			push(0, "CONFIGPARAM");

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

			pushCont(contOk);
			pushCont(contFail);

			push(-2 + 5, "IFELSE");

			return true;
		}
		if (iname == "tvm_config_param17") {
			//_    min_stake:Grams    max_stake:Grams    min_total_stake:Grams    max_stake_factor:uint32 = ConfigParam 17;
			pushLog("config_param17");
			push(0, "PUSHINT 17");
			push(0, "CONFIGPARAM");

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

			pushCont(contOk);
			pushCont(contFail);

			push(-2 + 5, "IFELSE");

			return true;
		}
		return false;
	}
};
	
}	// solidity
}	// dev
