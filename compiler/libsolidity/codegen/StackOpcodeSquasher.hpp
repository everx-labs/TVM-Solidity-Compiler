/*
 * Copyright (C) 2021-2023 EverX. All Rights Reserved.
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

#include "TvmAst.hpp"
#include <map>
#include <unordered_map>

namespace solidity::frontend {

constexpr static int MAX_NEW_OPCODES = 3;

class StackState {
public:
	constexpr static int MAX_STACK_DEPTH = 9;

	explicit StackState(int _size);
	explicit StackState(int8_t _size, std::array<int8_t, MAX_STACK_DEPTH> _values) : m_size{_size}, m_values{_values} {
		updHash();
	}
	bool apply(Stack const& opcode);
	bool operator==(const StackState &other) const {
		if (m_size != other.m_size)
			return false;
		for (int i = 0; i < m_size; ++i)
			if (m_values[i] != other.m_values[i])
				return false;
		return true;
	}
	bool operator!=(const StackState &other) const {
		return !operator==(other);
	}
	std::size_t getHash() const { return m_hash; }
	int8_t size() const { return m_size; }
	std::array<int8_t, MAX_STACK_DEPTH> const& values() const { return m_values; }
private:
	void updHash();
private:
	std::size_t m_hash{};
	int8_t m_size;
	std::array<int8_t, MAX_STACK_DEPTH> m_values{};
};

class StackOpcodeSquasher {
public:
	static std::optional<int> gasCost(int startStackSize, StackState const& _state, bool _withCompoundOpcodes);
private:
	static void init();
public:
	static std::vector<Pointer<TvmAstNode>> recover(int startStackSize, StackState state, bool _withCompoundOpcodes);
	struct DpState {
		int gasCost;
		StackState prevState;
		Pointer<Stack> opcode;
	};
private:
	static std::array<std::array<std::unordered_map<StackState, DpState>, StackState::MAX_STACK_DEPTH + 1>, 2> m_dp;
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

