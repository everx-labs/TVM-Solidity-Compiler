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
 * @date 2020-01-29
 */

#include <boost/range/adaptor/map.hpp>

#include "TVMInlineFunctionChecker.hpp"
#include "TVMCommons.hpp"

using namespace solidity::frontend;

bool TVMInlineFunctionChecker::visit(Identifier const &_identifier) {
	auto functionType = to<FunctionType>(_identifier.annotation().type);
	auto funDef = to<FunctionDefinition>(_identifier.annotation().referencedDeclaration);
	if (functionType && funDef && funDef->isInline()) {
		graph[currentFunctionDefinition].insert(funDef);
	}
	return false;
}

bool TVMInlineFunctionChecker::visit(FunctionDefinition const &_node) {
	currentFunctionDefinition = &_node;
	graph[currentFunctionDefinition];
	solAssert(currentFunctionDefinition->isInline(), "");
	return ASTConstVisitor::visit(_node);
}


bool TVMInlineFunctionChecker::dfs(FunctionDefinition const* v) {
	color[v] = 1;
	for (FunctionDefinition const* to : graph[v]) {
		if (color[to] == 0) {
			parent[to] = v;
			if (dfs (to)) {
				return true;
			}
		} else if (color[to] == 1) {
			cycleEnd = v;
			cycleStart = to;
			return true;
		} else if (color[to] == 2) {
			continue;
		} else {
			solUnimplemented("");
		}
	}
	order.push_back(v);
	color[v] = 2;
	return false;
}


std::vector<FunctionDefinition const*> TVMInlineFunctionChecker::functionOrder() {
	solAssert(!oneCall, "");
	oneCall = true;

	for (FunctionDefinition const* v : graph | boost::adaptors::map_keys) {
		if (color[v] == 0) {
			if (dfs(v)) {
				break;
			}
		}
	}

	if (cycleStart) {
		std::vector<FunctionDefinition const*> cycle;
		cycle.push_back (cycleStart);
		for (FunctionDefinition const* v = cycleEnd; v != cycleStart; v = parent[v]) {
			cycle.push_back(v);
		}
		cycle.push_back (cycleStart);
		reverse (cycle.begin(), cycle.end());

		std::string errMsg{};
		for (size_t i = 0; i < cycle.size(); ++i) {
			errMsg += cycle[i]->name();
			if (i + 1 != cycle.size()) {
				errMsg += " -> ";
			}
		}
		cast_error(*cycle[0], "There are a cycle of inline function calls: " + errMsg);
	}

	return order;
}