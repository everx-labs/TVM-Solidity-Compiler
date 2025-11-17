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

#pragma once

namespace solidity::frontend {

class Pen {
public:
	int red() const { return m_white + 1; }
	int black() const { return m_white + 2; }
	int updateColors() { return m_white += 10; }

private:
	int m_white = 0;
};

class FunctionCallGraph: boost::noncopyable {
public:
	// Adds the edge and returns true if the edge create a loop
	bool tryToAddEdge(std::string const& _v, std::string const& _to);
	std::vector<std::string> DAG();
	void addDictFunction(uint32_t id, std::string name);
	std::map<uint32_t, std::string> privateFunctions() const { return m_dictFuncs; }

private:
	bool dfs(std::string const& v);

	std::map<std::string, std::set<std::string>> m_graph;
	std::map<std::string, int> m_color;
	std::vector<std::string> m_order;
	std::map<uint32_t, std::string> m_dictFuncs;
	Pen pen;
};

} // end solidity::frontend
