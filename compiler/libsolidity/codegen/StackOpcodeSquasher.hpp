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
 * @date 2021
 *
 */

#pragma once

#include "TvmAst.hpp"
#include <unordered_map>

namespace solidity::frontend {

	class StackState {
	public:
		constexpr static int maxStackDepth = 9;

		StackState();
		bool apply(Stack const& opcode);
		bool operator==(const StackState &other) const {
			return m_values == other.m_values;
		}
		bool operator!=(const StackState &other) const {
			return m_values != other.m_values;
		}
		std::size_t getHash() const { return m_hash; }
	private:
		std::size_t m_hash{};
		std::array<int8_t, maxStackDepth> m_values{};
	};

	class StackOpcodeSquasher {
	public:
		static int steps(StackState const& _state);
	private:
		static void init();
	public:
		static std::vector<Pointer<TvmAstNode>> recover(StackState state);
	private:
		static std::unordered_map<StackState, int8_t> m_dp;
		static std::unordered_map<StackState, std::pair<StackState, Pointer<Stack>>> m_prev;
	};
} // end solidity::frontend

namespace std {

template <>
struct hash<solidity::frontend::StackState> {
	std::size_t operator()(const solidity::frontend::StackState& k) const {
		return k.getHash();
	}
};

}

