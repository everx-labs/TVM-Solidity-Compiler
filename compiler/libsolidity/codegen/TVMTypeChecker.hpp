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

#include "TVMCommons.hpp"

namespace dev::solidity {

class TVMTypeChecker {
public:
	static void check(ContractDefinition const* c) {
		for (FunctionDefinition const* f : c->definedFunctions()) {
			if (isTvmIntrinsic(f->name())) {
				checkTvmIntrinsic(f);
			}
		}
	}

	static void checkTvmIntrinsic(FunctionDefinition const* f) {
		if (f->visibility() != Declaration::Visibility::Private) {
			cast_error(*f, "Intrinsic should have private visibility");
		}

		if (f->name() == "tvm_init_storage") {
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
		auto checkArgCount = [&](std::size_t argCount) {
			if (arguments.size() != argCount) {
				if (argCount == 0) {
					cast_error(*f, "Should have no argument");
				} else if (argCount == 1) {
					cast_error(*f, "Should have one argument");
				} else {
					cast_error(*f, "Should have " + toString(argCount) + " arguments");
				}
			}
		};

		switch (str2int(f->name().c_str())) {
			case str2int("tvm_dictgetnext"): {
				checkArgCount(2);
				VariableDeclaration const *keyDeclaration = f->parameters()[0].get();
				VariableDeclaration const *mapDeclaration = f->parameters()[1].get();
				auto mapType = to<MappingType>(mapDeclaration->type().get());
				if (!mapType) {
					cast_error(*mapDeclaration, "Should have mapping type");
				}
				if (*keyDeclaration->type().get() != *mapType->keyType().get()) {
					cast_error(*keyDeclaration, "Should have type " + mapType->keyType()->toString());
				}
				[[fallthrough]];
			}
			case str2int("tvm_dictmin"): {
				if (f->name() == "tvm_dictmin") {
					checkArgCount(1);
				}
				VariableDeclaration const *mapDeclaration = f->parameters()[f->name() == "tvm_dictmin" ? 0 : 1].get();
				auto mapType = to<MappingType>(mapDeclaration->type().get());
				if (!mapType) {
					cast_error(*mapDeclaration, "Should have mapping type");
				}
				std::vector<ASTPointer<VariableDeclaration>> const& ret = f->returnParameters();
				if (ret.size() != 3) {
					cast_error(*f, "Should return 3 value");
				}
				if (*ret[0].get()->annotation().type.get() != *mapType->keyType().get()) {
					cast_error(*ret[0], "Should have type " + mapType->keyType()->toString());
				}
				if (*ret[1].get()->annotation().type.get() != *mapType->valueType().get()) {
					cast_error(*ret[1], "Should have type " + mapType->valueType()->toString());
				}
				if (ret[2].get()->annotation().type->category() != Type::Category::Bool) {
					cast_error(*ret[2], "Should have bool type");
				}
				break;
			}
			default:
				break;
		}
	}
};

}

