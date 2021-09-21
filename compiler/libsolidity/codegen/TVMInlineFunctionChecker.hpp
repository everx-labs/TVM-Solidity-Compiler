/*
 * Copyright 2018-2021 TON DEV SOLUTIONS LTD.
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
 * @date 2020-01-29
 */

#pragma once

#include <libsolidity/ast/ASTVisitor.h>

namespace solidity::frontend {

class TVMInlineFunctionChecker : public ASTConstVisitor {
public:
	TVMInlineFunctionChecker() = default;
	bool visit(Identifier const& _node) override;
	bool visit(FunctionDefinition const& _node) override;
	bool dfs(FunctionDefinition const* v);
	std::vector<FunctionDefinition const*> functionOrder();

private:
	FunctionDefinition const* currentFunctionDefinition{};
	std::map<FunctionDefinition const*, std::set<FunctionDefinition const*>> graph;

	bool oneCall{};
	std::vector<FunctionDefinition const*> order;
	std::map<FunctionDefinition const*, int> color;
	std::map<FunctionDefinition const*, FunctionDefinition const*> parent;
	FunctionDefinition const* cycleEnd{};
	FunctionDefinition const* cycleStart{};
};

} // end solidity::frontend

