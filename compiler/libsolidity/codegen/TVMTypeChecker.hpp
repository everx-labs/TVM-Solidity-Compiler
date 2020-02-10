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

namespace dev::solidity {

class TVMTypeChecker {
public:
	static void check(ContractDefinition const* contractDefinition) {
		for (FunctionDefinition const* f : contractDefinition->definedFunctions()) {
			if (isTvmIntrinsic(f->name())) {
				checkTvmIntrinsic(f);
			}
			if (ends_with(f->name(), "_inline")) {
				cast_warning(*f, "Suffix is deprecated it will be removed from compiler soon.");
			}
			if (isInlineFunction(f) && f->isPublic()) {
				cast_error(*f, "Inline function should have private visibility");
			}

			checkDecodeEncodeParams(f);
		}

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

private:
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
				auto mappingType = to<MappingType>(param->type().get());
				auto intKey = to<IntegerType>(mappingType->keyType().get());
				if (!intKey) {
					cast_error(*param, "Key type must be any of int<M>/uint<M> types with M from 8 to 256");
				}

				auto valueStruct = to<StructType>(mappingType->valueType().get());
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

	static void checkTvmIntrinsic(FunctionDefinition const* f) {
		if (f->visibility() != Declaration::Visibility::Private) {
			cast_error(*f, "Intrinsic should have private visibility");
		}

		if (isIn(f->name(), "tvm_sender_pubkey", "tvm_my_public_key", "tvm_chksignu", "tvm_hashcu", "tvm_accept",
					"tvm_unpack_address", "tvm_make_address", "tvm_transfer", "tvm_make_zero_address", "tvm_setcode",
					"tvm_is_zero_address", "tvm_zero_ext_address", "tvm_make_external_address", "tvm_commit", "tvm_logstr")) {
			cast_warning(*f, "Function is deprecated it will be removed from compiler soon.");
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
				Type const * type3 = f->parameters()[3]->annotation().type.get();
				if (!(to<TvmCellType>(type3)))
				{
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
};

}

