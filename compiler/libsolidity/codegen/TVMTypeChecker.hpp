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
 */

#pragma once

#include <libsolidity/ast/ASTForward.h>
#include <boost/range/adaptor/reversed.hpp>
#include "TVMCommons.hpp"

namespace solidity::frontend {

class TVMTypeChecker {
private:
	explicit TVMTypeChecker(ContractDefinition const* contractDefinition,
							std::vector<PragmaDirective const *> const& pragmaDirectives) :
		contractDefinition{contractDefinition},
		pragmaDirectives{pragmaDirectives} {

	}

public:
	static void check(ContractDefinition const* contractDefinition, std::vector<PragmaDirective const *> const& pragmaDirectives) {
		TVMTypeChecker checker{contractDefinition, pragmaDirectives};
		checker.checkInlineFunctions();
		checker.checkEncodeDecodeParams();
		checker.checkIntrinsic();
		checker.checkStateVariables();
		if (contractDefinition->name() != "stdlib") {
			checker.checkPragma();
		}
		checker.check_onCodeUpgrade();
	}

private:
	void checkPragma() {
		PragmaDirectiveHelper pragmaHelper{pragmaDirectives};

		if (pragmaHelper.abiVersion() == 1) {
			for (const std::string& s : {"expire", "time", "pubkey"}) {
				auto [have, astNode] = pragmaHelper.haveHeader(s);
				if (have) {
					cast_error(*astNode, R"("pragma AbiHeader v1" are not compatible with "pragma AbiHeader expire", "pragma AbiHeader time" and "pragma AbiHeader pubkey")");
				}
			}
		}

		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			if (f->name() == "afterSignatureCheck") {
				const std::string s = "function afterSignatureCheck(TvmSlice restOfMessageBody, TvmCell message) private inline returns (TvmSlice) { /*...*/ }";
				if (f->parameters().size() != 2) {
					cast_error(*f, "Should have follow format: " + s);
				}
				if (f->returnParameters().size() != 1) {
					cast_error(*f, "Should have follow format: " + s);
				}
				if (f->isPublic()) {
					cast_error(*f, "Should have follow format: " + s);
				}
				if (!f->isInline()) {
					cast_error(*f, "Should have follow format: " + s);
				}
			}
		}
	}

	void checkStateVariables() {
		std::set<std::string> usedNames;
		for (ContractDefinition const* c : contractDefinition->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
			for (VariableDeclaration const *variable: c->stateVariables()) {
				if (usedNames.count(variable->name()) != 0) {
					cast_error(*variable, "Duplicate member variable");
				}
				usedNames.insert(variable->name());
			}
		}
	}

	void checkIntrinsic() {
		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			if (isTvmIntrinsic(f->name())) {
				checkTvmIntrinsic(f, contractDefinition);
			}
		}
	}

	void checkEncodeDecodeParams() {
		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			checkDecodeEncodeParams(f);
		}
	}

	void checkInlineFunctions() {
		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			if (ends_with(f->name(), "_inline")) {
				cast_warning(*f, "Suffix is deprecated it will be removed from compiler soon.");
			}
			if ((ends_with(f->name(), "_inline") || f->isInline()) && f->isPublic()) {
				cast_error(*f, "Inline function should have private visibility");
			}
		}
	}

	static void checkDecodeEncodeParams(FunctionDefinition const* f) {
		if (!f->isPublic()) {
			return;
		}

		for (const ASTPointer<VariableDeclaration>& param : f->parameters()) {
			checkDecodeEncodeParam(param.get());
		}

		for (const ASTPointer<VariableDeclaration>& param : f->returnParameters()) {
			checkDecodeEncodeParam(param.get());
		}
	}

	static void checkDecodeEncodeParam(VariableDeclaration const* param) {
		switch (param->type()->category()) {
			case Type::Category::Mapping: {
				auto mappingType = to<MappingType>(param->type());
				auto intKey = to<IntegerType>(mappingType->keyType());
				if (!intKey) {
					cast_error(*param, "Key type must be any of int<M>/uint<M> types with M from 8 to 256");
				}

				auto valueStruct = to<StructType>(mappingType->valueType());
				if (valueStruct) {
					if (!StructCompiler::isCompatibleWithSDK(static_cast<int>(intKey->numBits()), valueStruct)) {
						cast_error(*param, "Value type is not compatible with SDK");
					}
				}

				break;
			}
			default:
				break;
		}
	}

	static void checkTvmIntrinsic(FunctionDefinition const* f, ContractDefinition const* contractDefinition) {
		if (f->visibility() != Visibility::Private) {
			cast_error(*f, "Intrinsic should have private visibility");
		}

		if (isIn(f->name(), "tvm_sender_pubkey", "tvm_my_public_key", "tvm_chksignu", "tvm_hashcu", "tvm_accept",
					"tvm_unpack_address", "tvm_make_address", "tvm_transfer", "tvm_make_zero_address", "tvm_setcode",
					"tvm_is_zero_address", "tvm_zero_ext_address", "tvm_make_external_address", "tvm_commit", "tvm_logstr",
					"tvm_tree_cell_size", "tvm_trans_lt", "tvm_reset_storage", "tvm_config_param1", "tvm_config_param15",
					"tvm_config_param17", "tvm_config_param34")) {
			cast_warning(*f, "Function is deprecated it will be removed from compiler soon.");
		}

		if ((contractDefinition->name() != "stdlib") && (isIn(f->name(),
			"tvm_ldu", "tvm_ldi", "tvm_pldu", "tvm_ldmsgaddr", "tvm_pldmsgaddr",
			"tvm_ldslice", "tvm_ldref", "tvm_lddict", "tvm_setindex", "tvm_sti",
			"tvm_stu", "tvm_stslice", "tvm_stref", "tvm_dictuset", "tvm_dictusetb",
			"tvm_bchkbitsq", "tvm_schkbitsq", "tvm_sbitrefs", "tvm_srefs",
			"tvm_brembits", "tvm_getfromdict", "tvm_get_slice_from_integer_dict",
			"tvm_tlen", "tvm_ends", "tvm_newc", "tvm_endc", "tvm_c4_key_length",
			"tvm_exception_constructoriscalledtwice", "tvm_exception_replayprotection",
			"tvm_exception_unpackaddress", "tvm_exception_insertpubkey", "tvm_stbr",
			"tvm_default_replay_protection_interval", "tvm_newdict", "tvm_c4",
			"tvm_c7", "tvm_first", "tvm_second", "tvm_third", "tvm_index", "tvm_sendrawmsg",
			"tvm_ubitsize", "tvm_stdict", "tvm_tpush", "tvm_setthird", "tvm_stbrefr",
			"tvm_tuple0", "tvm_popctr", "tvm_sdskipfirst", "tvm_dictudel", "tvm_isnull",
			"tvm_srempty", "tvm_sdempty", "tvm_ldrefrtos", "tvm_pldref_and_to_slice",
			"tvm_hashsu", "tvm_subslice", "tvm_pldslice", "tvm_selector_call",
			"tvm_sempty", "tvm_bremrefs", "tvm_sbits", "tvm_bbits", "tvm_pldrefvar",
			"tvm_push_fallback_func_id", "tvm_push_on_bounce_id", "tvm_push_minus_one",
			"tvm_stzeroes", "tvm_stones", "tvm_stgrams", "tvm_skipdict",
			"tvm_plddicts", "tvm_getparam", "tvm_plddict", "tvm_iszero",
			"tvm_poproot", "tvm_throwany", "tvm_parsemsgaddr", "tvm_reverse_push",
			"tvm_skip_and_load_uint256_in_slice_copy", "tvm_unpackfirst4",
			"tvm_stack"
			))) {
			cast_warning(*f, "Function is internal, use at your own risk.");
		}

		if (isIn(f->name(), "tvm_commit", "tvm_reset_storage")) {
			if (f->stateMutability() != StateMutability::NonPayable) {
				cast_error(*f, R"(Should have "NonPayable" state mutability)");
			}
		} else {
			if (f->stateMutability() != StateMutability::Pure) {
				cast_error(*f, R"(Should have "pure" state mutability)");
			}
		}

		auto arguments = f->parameters();

		switch (str2int(f->name().c_str())) {
			case str2int("tvm_deploy_contract"): {
				Type const * type3 = f->parameters()[3]->annotation().type;
				if (!to<TvmCellType>(type3)) {
					TypeInfo ti(type3);
					if (!ti.isNumeric || ti.numBits != 32 || ti.isSigned)
						cast_error(*(f->parameters()[3].get()),"Constructor id argument should be of type uint32.");
				}
				break;
			}
			default:
				break;
		}
	}

	void check_onCodeUpgrade() {
		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			if (f->name() == "onCodeUpgrade") {
				const std::string s = "function onCodeUpgrade(...) private { /*...*/ }";
				if (!f->returnParameters().empty()) {
					cast_error(*f->returnParameters()[0], "Should have follow format: " + s);
				}
				if (f->isPublic()) {
					cast_error(*f, "Should have follow format: " + s);
				}
			}
		}
	}

private:
	ContractDefinition const* contractDefinition;
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};

}

