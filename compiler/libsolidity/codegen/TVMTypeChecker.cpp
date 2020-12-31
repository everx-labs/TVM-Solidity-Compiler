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


#include <boost/range/adaptor/reversed.hpp>

#include <libsolidity/ast/ASTForward.h>

#include "TVMCommons.hpp"
#include "TVMConstants.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMTypeChecker.hpp"

using namespace solidity::frontend;

TVMTypeChecker::TVMTypeChecker(ContractDefinition const *contractDefinition,
                               std::vector<PragmaDirective const *> const &pragmaDirectives) :
		contractDefinition{contractDefinition},
		pragmaDirectives{pragmaDirectives} {

}

void TVMTypeChecker::check(ContractDefinition const *contractDefinition,
                           std::vector<PragmaDirective const *> const &pragmaDirectives) {
	TVMTypeChecker checker{contractDefinition, pragmaDirectives};
	checker.checkInlineFunctions();
	checker.checkEncodeDecodeParams();
	checker.checkIntrinsic();
	checker.checkStateVariables();
	checker.checkOverrideAndOverload();
	if (contractDefinition->name() != "stdlib") {
		checker.checkPragma();
	}
	checker.check_onCodeUpgrade();

	contractDefinition->accept(checker);
	if (TVMContractCompiler::g_errorReporter->hasErrors()) {
		BOOST_THROW_EXCEPTION(FatalError());
	}
}

void TVMTypeChecker::checkPragma() {
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

void TVMTypeChecker::checkStateVariables() {
	std::set<std::string> usedNames;
	for (ContractDefinition const* c : contractDefinition->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (VariableDeclaration const *variable: c->stateVariables()) {
			if (usedNames.count(variable->name()) != 0) {
				// TODO print prev name
				cast_error(*variable, "Duplicate member variable");
			}
			usedNames.insert(variable->name());

			if (variable->isPublic()) {
				checkDecodeEncodeParam(variable->type(), *variable, 0);
			}
		}
	}
}

void TVMTypeChecker::checkOverrideAndOverload() {
	std::set<CallableDeclaration const*> overridedFunctions;
	std::set<CallableDeclaration const*> functions;
	for (ContractDefinition const* cd : contractDefinition->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const *f : cd->definedFunctions()) {
			if (f->isConstructor() || f->isReceive() || f->isFallback() || f->isFallback() ||
				f->isOnTickTock() || isTvmIntrinsic(f->name())) {
				continue;
			}

			FunctionDefinitionAnnotation &annotation = f->annotation();
			if (!annotation.baseFunctions.empty()) {
				overridedFunctions.insert(f);
				for (CallableDeclaration const *base : annotation.baseFunctions) {
					auto fBase = to<FunctionDefinition>(base);
					overridedFunctions.insert(base);
					if ((!f->functionID().has_value() && fBase->functionID()) || (f->functionID().has_value() && !fBase->functionID())) {
						cast_error(*f, "Both override and base functions should have functionID if for one is defined.");
					}
					if (f->functionID().has_value() && f->functionID() != fBase->functionID()) {
						cast_error(*f, "Override function should have functionID = " + toString(fBase->functionID().value()) + ".");
					}
				}
			}
			functions.insert(f);
		}
	}

	for (CallableDeclaration const* f : functions) {
		if (overridedFunctions.count(f)) {
			continue;
		}
		for (CallableDeclaration const* ff : functions) {
			if (overridedFunctions.count(ff) || f == ff) {
				continue;
			}
			if (f->name() == ff->name()) {
				cast_error(*f, "Function overloading is not supported.");
			}
		}
	}
}

void TVMTypeChecker::checkIntrinsic() {
	for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
		if (isTvmIntrinsic(f->name())) {
			checkTvmIntrinsic(f, contractDefinition);
		}
	}
}

void TVMTypeChecker::checkEncodeDecodeParams() {
	for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
		checkDecodeEncodeParams(f);
	}
}

void TVMTypeChecker::checkInlineFunctions() {
	for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
		if (ends_with(f->name(), "_inline")) {
			cast_warning(*f, "Suffix is deprecated it will be removed from compiler soon.");
		}
		if ((ends_with(f->name(), "_inline") || f->isInline()) && f->isPublic()) {
			cast_error(*f, "Inline function should have private or internal visibility");
		}
	}
}

void TVMTypeChecker::checkDecodeEncodeParams(FunctionDefinition const *f) {
	if (!f->isPublic()) {
		return;
	}

	for (const ASTPointer<VariableDeclaration>& param : f->parameters()) {
		checkDecodeEncodeParam(param->type(), *param, 0);
	}

	for (const ASTPointer<VariableDeclaration>& param : f->returnParameters()) {
		checkDecodeEncodeParam(param->type(), *param, 0);
	}
}

void TVMTypeChecker::checkDecodeEncodeParam(Type const* type, const ASTNode &node, int keyLength) {

	switch (type->category()) {
		case Type::Category::Mapping: {
			auto mappingType = to<MappingType>(type);
			auto intKey = to<IntegerType>(mappingType->keyType());
			auto addrKey = to<AddressType>(mappingType->keyType());
			int mapKeyLength;
			if (intKey) {
				mapKeyLength = static_cast<int>(intKey->numBits());
			} else if (addrKey) {
				mapKeyLength = AddressInfo::stdAddrLength();
			} else {
				cast_error(node, "Key type must be any of int<M>/uint<M> types with M from 8 to 256 or std address.");
			}


			checkDecodeEncodeParam(mappingType->valueType(), node, mapKeyLength);
			break;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (!arrayType->isByteArray()) {
				checkDecodeEncodeParam(arrayType->baseType(), node, TvmConst::ArrayKeyLength);
			}
			break;
		}
		case Type::Category::Struct: {
			if (keyLength > 0) {
				auto valueStruct = to<StructType>(type);
				if (!StructCompiler::isCompatibleWithSDK(keyLength, valueStruct)) {
					cast_error(node, "Struct is not compatible with SDK. "
					                 "Struct must have no nested structs and all members of the struct must fit in one cell.");
				}
			}
			break;
		}
		default:
			break;
	}
}

void TVMTypeChecker::checkTvmIntrinsic(FunctionDefinition const *f, ContractDefinition const *contractDefinition) {
	if (f->visibility() != Visibility::Private) {
		cast_error(*f, "Intrinsic should have private visibility");
	}

	std::map<std::string, std::string> deprecatedFunctionsReplacement;

	deprecatedFunctionsReplacement["tvm_sender_pubkey"] = "msg.pubkey()";
	deprecatedFunctionsReplacement["tvm_my_public_key"] = "tvm.pubkey()";
	deprecatedFunctionsReplacement["tvm_chksignu"] = "tvm.checkSign()";
	deprecatedFunctionsReplacement["tvm_hashcu"] = "tvm.hash()";
	deprecatedFunctionsReplacement["tvm_accept"] = "tvm.accept()";
	deprecatedFunctionsReplacement["tvm_unpack_address"] = "address.unpack()";
	deprecatedFunctionsReplacement["tvm_make_address"] = "address.makeAddrStd()";
	deprecatedFunctionsReplacement["tvm_transfer"] = "address.transfer()";
	deprecatedFunctionsReplacement["tvm_make_zero_address"] = "address.makeAddrStd()";
	deprecatedFunctionsReplacement["tvm_setcode"] = "tvm.setcode()";
	deprecatedFunctionsReplacement["tvm_is_zero_address"] = "address.isNone()";
	deprecatedFunctionsReplacement["tvm_zero_ext_address"] = "address.makeAddrNone()";
	deprecatedFunctionsReplacement["tvm_make_external_address"] = "address.makeAddrExtern()";
	deprecatedFunctionsReplacement["tvm_commit"] = "tvm.commit()";
	deprecatedFunctionsReplacement["tvm_logstr"] = "tvm.log()";
	deprecatedFunctionsReplacement["tvm_reset_storage"] = "tvm.resetStorage()";
	deprecatedFunctionsReplacement["tvm_config_param1"] = "tvm.configParam()";
	deprecatedFunctionsReplacement["tvm_config_param15"] = "tvm.configParam()";
	deprecatedFunctionsReplacement["tvm_config_param17"] = "tvm.configParam()";
	deprecatedFunctionsReplacement["tvm_config_param34"] = "tvm.configParam()";
	deprecatedFunctionsReplacement["tvm_deploy_contract"] = "tvm.deploy()";
	deprecatedFunctionsReplacement["tvm_insert_pubkey"] = "tvm.insertPubkey()";
	deprecatedFunctionsReplacement["tvm_build_state_init"] = "tvm.buildStateInit()";
	deprecatedFunctionsReplacement["tvm_ignore_integer_overflow"] = "pragma ignoreIntOverflow";

	if (auto it = deprecatedFunctionsReplacement.find(f->name()); it != deprecatedFunctionsReplacement.end())
		cast_warning(*f, "Function is deprecated it will be removed from compiler soon. Use " + it->second + " instead.");

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

	if (f->name() == "tvm_deploy_contract") {
		Type const * type3 = f->parameters()[3]->annotation().type;
		if (!to<TvmCellType>(type3)) {
			TypeInfo ti(type3);
			if (!ti.isNumeric || ti.numBits != 32 || ti.isSigned)
				cast_error(*(f->parameters()[3].get()),"Constructor id argument should be of type uint32.");
		}
	}
}

void TVMTypeChecker::check_onCodeUpgrade() {
	for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
		if (f->name() == "onCodeUpgrade") {
			const std::string s = "function onCodeUpgrade(...) (internal|private) { /*...*/ }";
			if (!f->returnParameters().empty()) {
				cast_error(*f->returnParameters()[0], "Should have follow format: " + s);
			}
			if (f->isPublic()) {
				cast_error(*f, "Should have follow format: " + s);
			}
		}
	}
}

bool TVMTypeChecker::visit(const Mapping &_mapping) {
    if (auto keyType = to<UserDefinedTypeName>(&_mapping.keyType())) {
        if (keyType->annotation().type->category() == Type::Category::Struct) {
            auto structType = to<StructType>(_mapping.keyType().annotation().type);
            int bitLength = 0;
            StructDefinition const& structDefinition = structType->structDefinition();
            for (const auto& member : structDefinition.members()) {
                TypeInfo ti {member->type()};
                if (!ti.isNumeric) {
                    cast_error(_mapping.keyType(), "If struct are used as key for mapping than "
                                                   "fields of struct must have integer types, boolean types, fixed bytes types or enums");
                    // TODO also print this field (see SecondarySourceLocation)
                }
                bitLength += ti.numBits;
            }
            if (bitLength > TvmConst::CellBitLength) {
                cast_error(_mapping.keyType(), "If struct are used as key for mapping than "
                                               "struct size must be no more than " + toString(TvmConst::CellBitLength) + " bits");
            }
        }
    }
    return true;
}

bool TVMTypeChecker::visit(FunctionCall const& functionCall) {
	if (auto functionOptions = to<FunctionCallOptions>(&functionCall.expression())) {
		if (auto newExpr = to<NewExpression>(&functionOptions->expression())) {
			std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();

			// check that option set is valid
			auto stateInit = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "stateInit"; });
			auto code = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "code"; });

			auto pubkey = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "pubkey"; });
			auto varInit = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "varInit"; });

			auto value = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "value"; });

			auto have = [&](auto it){ return it != optionNames.end(); };
			auto getLocation = [&](auto it) {
				int index = it - optionNames.begin();
				return functionOptions->options().at(index)->location();
			};

			if (!have(stateInit) && !have(code)) {
				cast_error(*functionOptions, R"(Either option "stateInit" or option "code" must be set.)");
			}

			if (have(stateInit) && have(pubkey)) {
				TVMContractCompiler::g_errorReporter->declarationError(
					getLocation(pubkey),
					SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", getLocation(stateInit)),
					R"(Option "pubkey" is not compatible with option "stateInit". Only with option "code".)"
				);
			}

			if (have(stateInit) && have(varInit)) {
				TVMContractCompiler::g_errorReporter->declarationError(
					getLocation(varInit),
					SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", getLocation(stateInit)),
					R"(Option "varInit" is not compatible with option "stateInit". Only with option "code".)"
				);
			}

			if (!have(value)) {
				cast_error(*functionOptions, R"(Option "value" must be set.)");
			}

			if (have(varInit)) {
				size_t initVarsIndex = varInit - optionNames.begin();
				auto initVars =  to<InitializerList>(functionOptions->options().at(initVarsIndex).get());
				const TypePointer type = newExpr->typeName().annotation().type;
				for (size_t i = 0; i < initVars->names().size(); ++i) {
					const ASTPointer<ASTString> & name = initVars->names().at(i);
					auto ct = to<ContractType>(type);

					bool find = false;
					for (const auto& [v, _a, _b] : ct->stateVariables()) {
						(void)_a;
						(void)_b;
						if (v->isStatic() && v->name() == *name) {
							find = true;
							TypePointer valueType = initVars->options().at(i)->annotation().type;
							TypePointer varType = v->type();
							if (!valueType->isImplicitlyConvertibleTo(*varType)) {
								TVMContractCompiler::g_errorReporter->declarationError(
									initVars->options().at(i)->location(),
									SecondarySourceLocation().append(R"(Variable is defined here: )", v->location()),
									"Type " + valueType->toString(true) +
									" is not implicitly convertible to expected type " +
									varType->toString(true) +
									"."
								);
							}
							break;
						}
					}
					if (!find) {
						TVMContractCompiler::g_errorReporter->declarationError(
							initVars->options().at(i)->location(),
							SecondarySourceLocation().append(R"(Contract is defined here: )", ct->contractDefinition().location()),
							"In contract there is no \"" + *name + "\" static state variable."
						);
					}
				}
			}
		}
	}
	return true;
}