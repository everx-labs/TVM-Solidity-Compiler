/*
 * Copyright (C) 2025 EverX. All Rights Reserved.
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

#include <map>
#include <set>

#include <boost/range/adaptor/map.hpp>

#include <liblangutil/Exceptions.h>

#include <libsolidity/codegen/helpers/FunctionCallGraph.hpp>

using namespace solidity::frontend;

bool FunctionCallGraph::tryToAddEdge(std::string const& _v, std::string const& _to) {
	if (m_graph[_v].contains(_to))
		return false;
	m_graph[_v].insert(_to);
	m_graph[_to]; // do nothing or create default value

	pen.updateColors();
	m_order.clear();

	bool hasLoop = false;
	if (dfs(_v)) {
		hasLoop = true;
		m_graph[_v].erase(_to);
	}
	return hasLoop;
}

std::vector<std::string> FunctionCallGraph::DAG() {
	pen.updateColors();
	m_order.clear();

	for (auto const& k: m_graph | boost::adaptors::map_keys)
		dfs(k);
	return m_order;
}

bool FunctionCallGraph::dfs(std::string const& v) {
	if (m_color[v] == pen.black())
		return false;
	if (m_color[v] == pen.red())
		return true;

	// It's white
	m_color[v] = pen.red();
	for (std::string const& _to: m_graph[v])
		if (dfs(_to))
			return true;
	m_order.emplace_back(v);

	m_color[v] = pen.black();
	return false;
}

void FunctionCallGraph::addDictFunction(uint32_t id, std::string name) {
	if (m_dictFuncs.contains(id))
		solAssert(m_dictFuncs.at(id) == name);
	else
		m_dictFuncs.emplace(id, name);
}
