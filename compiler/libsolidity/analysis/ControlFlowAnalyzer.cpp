/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0

#include <libsolidity/analysis/ControlFlowAnalyzer.h>

#include <liblangutil/SourceLocation.h>
#include <libsolutil/Algorithms.h>

#include <range/v3/algorithm/sort.hpp>

#include <functional>

using namespace std::placeholders;
using namespace solidity::langutil;
using namespace solidity::frontend;


bool ControlFlowAnalyzer::run()
{
	for (auto& [pair, flow]: m_cfg.allFunctionFlows())
		analyze(*pair.function, pair.contract, *flow);

	return !Error::containsErrors(m_errorReporter.errors());
}

void ControlFlowAnalyzer::analyze(FunctionDefinition const& _function, ContractDefinition const* _contract, FunctionFlow const& _flow)
{
	if (!_function.isImplemented())
		return;

	std::optional<std::string> mostDerivedContractName;

	// The name of the most derived contract only required if it differs from
	// the functions contract
	if (_contract && _contract != _function.annotation().contract)
		mostDerivedContractName = _contract->name();

	checkUnreachable(_flow.entry, _flow.exit, _flow.revert, _flow.transactionReturn);
}


void ControlFlowAnalyzer::checkUnreachable(CFGNode const* _entry, CFGNode const* _exit, CFGNode const* _revert, CFGNode const* _transactionReturn)
{
	// collect all nodes reachable from the entry point
	std::set<CFGNode const*> reachable = util::BreadthFirstSearch<CFGNode const*>{{_entry}}.run(
		[](CFGNode const* _node, auto&& _addChild) {
			for (CFGNode const* exit: _node->exits)
				_addChild(exit);
		}
	).visited;

	// traverse all paths backwards from exit, revert and transaction return
	// and extract (valid) source locations of unreachable nodes into sorted set
	std::set<SourceLocation> unreachable;
	util::BreadthFirstSearch<CFGNode const*>{{_exit, _revert, _transactionReturn}}.run(
		[&](CFGNode const* _node, auto&& _addChild) {
			if (!reachable.count(_node) && _node->location.isValid())
				unreachable.insert(_node->location);
			for (CFGNode const* entry: _node->entries)
				_addChild(entry);
		}
	);

	for (auto it = unreachable.begin(); it != unreachable.end();)
	{
		SourceLocation location = *it++;
		// Extend the location, as long as the next location overlaps (unreachable is sorted).
		for (; it != unreachable.end() && it->start <= location.end; ++it)
			location.end = std::max(location.end, it->end);

		if (m_unreachableLocationsAlreadyWarnedFor.emplace(location).second)
			m_errorReporter.warning(5740_error, location, "Unreachable code.");
	}
}
