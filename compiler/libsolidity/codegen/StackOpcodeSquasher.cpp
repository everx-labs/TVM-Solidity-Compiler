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
 * @date 2021
 *
 */

#include <chrono>
#include <deque>

#include "StackOpcodeSquasher.hpp"

using namespace solidity::frontend;

StackState::StackState() {
	m_hash = 0;
	for (int i = 0; i < maxStackDepth; ++i) {
		m_values[i] = i;
		m_hash = m_hash * 31 + m_values[i];
	}
}

bool StackState::apply(Stack const &opcode) {
	bool ok = false;
	switch (opcode.opcode()) {
		case Stack::Opcode::BLKSWAP: {
			int down = opcode.i();
			int up = opcode.j();
			if (down + up > maxStackDepth) {
				break;
			}
			ok = true;
			std::reverse(m_values.begin(), m_values.begin() + up);
			std::reverse(m_values.begin() + up, m_values.begin() + up + down);
			std::reverse(m_values.begin(), m_values.begin() + up + down);
			break;
		}
		case Stack::Opcode::REVERSE: {
			int qty = opcode.i();
			int index = opcode.j();
			if (index + qty > maxStackDepth) {
				break;
			}
			std::reverse(m_values.begin() + index, m_values.begin() + index + qty);
			ok = true;
			break;
		}
		case Stack::Opcode::XCHG: {
			int i = opcode.i();
			int j = opcode.j();
			if (i >= maxStackDepth || j >= maxStackDepth) {
				break;
			}
			std::swap(m_values[i], m_values[j]);
			ok = true;
			break;
		}

		case Stack::Opcode::DROP:
		case Stack::Opcode::BLKDROP2:
		case Stack::Opcode::POP_S:
		case Stack::Opcode::BLKPUSH:
		case Stack::Opcode::PUSH2_S:
		case Stack::Opcode::PUSH3_S:
		case Stack::Opcode::PUSH_S:
		case Stack::Opcode::TUCK:
		case Stack::Opcode::PUXC: {
			ok = false;
			break;
		}
	}
	m_hash = 0;
	for (int i = 0; i < maxStackDepth; ++i) {
		m_hash = m_hash * 31 + m_values[i];
	}
	return ok;
}

int StackOpcodeSquasher::steps(StackState const& _state) {
	if (m_dp.empty()) {
		init();
	}
	auto it = m_dp.find(_state);
	if (it == m_dp.end()) {
		return -1;
	}
	return it->second;
}

std::unordered_map<StackState, int8_t> StackOpcodeSquasher::m_dp {};
std::unordered_map<StackState, std::pair<StackState, Pointer<Stack>>> StackOpcodeSquasher::m_prev {};

void StackOpcodeSquasher::init() {
	std::vector<Pointer<Stack>> edges;
	for (int down = 1; down < StackState::maxStackDepth; ++down) {
		for (int up = 1; down + up < StackState::maxStackDepth; ++up) {
			edges.emplace_back(std::make_shared<Stack>(Stack::Opcode::BLKSWAP, down, up));
		}
	}
	for (int i = 0; i < StackState::maxStackDepth; ++i) {
		for (int j = i + 1; j < StackState::maxStackDepth; ++j) {
			edges.emplace_back(std::make_shared<Stack>(Stack::Opcode::XCHG, i, j));
		}
	}
	for (int i = 0; i < StackState::maxStackDepth; ++i) {
		for (int n = 2; i + n <= StackState::maxStackDepth; ++n) {
			edges.emplace_back(std::make_shared<Stack>(Stack::Opcode::REVERSE, n, i));
		}
	}


	//std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
	StackState state;
	m_dp[state] = 0;
	std::deque<StackState> q;
	q.push_back(state);
	while (!q.empty()) {
		state = q.front();
		q.pop_front();
		int8_t nextDp = m_dp.at(state) + 1;
		if (nextDp == 4) {
			break;
		}
		for (Pointer<Stack> const &e: edges) {
			StackState nextState = state;
			nextState.apply(*e.get());
			auto it = m_dp.find(nextState);
			if (it == m_dp.end()) {
				m_dp.emplace_hint(it, nextState, nextDp);
				m_prev.emplace(nextState, std::make_pair(state, e));
				q.push_back(nextState);
			}
		}
	}
	//std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
	//std::cerr << "Time difference = " << std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count() << "[ms]" << std::endl;
}


std::vector<Pointer<TvmAstNode>> StackOpcodeSquasher::recover(StackState state) {
	std::vector<Pointer<TvmAstNode>> res;
	StackState start;
	while (state != start) {
		std::pair<StackState, Pointer<Stack>> p = m_prev.at(state);
		state = p.first;
		res.push_back(p.second);
	}
	std::reverse(res.begin(), res.end());
	return res;
}

