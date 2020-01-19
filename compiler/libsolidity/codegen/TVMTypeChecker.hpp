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
		}

		std::set<std::string> usedNames;
		for (ContractDefinition const* c : contractDefinition->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
			for (VariableDeclaration const *variable: c->stateVariables()) {
				if (variable->isPublic() && variable->value() != nullptr) {
					cast_error(*variable, "Use sdk to init public state variables");
				}
				if (usedNames.count(variable->name()) != 0) {
					cast_error(*variable, "Duplicate member variable");
				}
				usedNames.insert(variable->name());
			}
		}
	}

	static void checkTvmIntrinsic(FunctionDefinition const* f) {
		if (f->visibility() != Declaration::Visibility::Private) {
			cast_error(*f, "Intrinsic should have private visibility");
		}

		if (isIn(f->name(), "tvm_init_storage", "tvm_commit")) {
			if (f->stateMutability() != StateMutability::NonPayable) {
				cast_error(*f, R"(Should have "NonPayable" state mutability)");
			}
		} else if (f->name() == "tvm_my_public_key") {
			if (f->stateMutability() != StateMutability::View) {
				cast_error(*f, R"(Should have "view" state mutability)");
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
				if (!isTvmCell(type3))
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

