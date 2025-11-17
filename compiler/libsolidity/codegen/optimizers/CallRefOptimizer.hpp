/*
 * Copyright (C) 2019-2025 EverX. All Rights Reserved.
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

#pragma once

#include <libsolidity/codegen/TvmAstVisitor.hpp>

namespace solidity::frontend {

/// Optimizer that changes `CALLREF { BLOCK }` on `CALLX { BLOCK }` if `BLOCK` is used once
class CallRefToCallXOptimizer {
public:
	static void optimize(Pointer<Contract> const& contract);
};

/// Optimizer that changes `CALLREF { BLOCK }` on `BLOCK`
class CallRefInliner {
public:
	static void optimize(Pointer<Contract> const& contract, std::vector<std::string> const& functionDag);
};

}
