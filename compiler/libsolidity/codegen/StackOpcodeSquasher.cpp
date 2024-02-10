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

#include <chrono>
#include <deque>

#include "StackOpcodeSquasher.hpp"

using namespace solidity::frontend;
using namespace std;

StackState::StackState(int _size) {
	m_size = _size;
	for (int8_t i = 0; i < m_size; ++i) {
		m_values[i] = i;
	}
	updHash();
}

bool StackState::apply(Stack const &opcode) {
	auto execPUSHS = [this](int index) -> bool {
		if (index < m_size && m_size + 1 <= StackState::MAX_STACK_DEPTH) {
			int8_t val = m_values[index];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 1] = m_values[i];
			m_values[0] = val;
			++m_size;
			return true;
		}
		return false;
	};
	auto execPUXC = [this](int index, int j) -> bool {
		if (j + 1 < m_size && index < m_size && m_size + 1 <= StackState::MAX_STACK_DEPTH) {
			int8_t val = m_values[index];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 1] = m_values[i];
			m_values[0] = val;
			std::swap(m_values[0], m_values[1]);
			std::swap(m_values[0], m_values[j + 1]);
			++m_size;
			return true;
		}
		return false;
	};

	bool ok = false;
	int const index0 = opcode.i();
	int const index1 = opcode.j();
	int const index2 = opcode.k();
	switch (opcode.opcode()) {
	case Stack::Opcode::BLKSWAP: {
		int down = opcode.i();
		int up = opcode.j();
		if (down + up > m_size) {
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
		if (index + qty <= m_size) {
			// [index..index+qty-1]
			std::reverse(m_values.begin() + index, m_values.begin() + index + qty);
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCHG: {
		int i = opcode.i();
		int j = opcode.j();
		if (i >= m_size || j >= m_size) {
			break;
		}
		solAssert(i != -1, "");
		solAssert(j != -1, "");
		std::swap(m_values[i], m_values[j]);
		ok = true;
		break;
	}
	case Stack::Opcode::DROP: {
		int n = opcode.i();
		if (n < m_size) {
			for (int i = n; i < m_size; ++i)
				m_values[i - n] = m_values[i];
			m_size -= n;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::BLKDROP2: {
		int const down = opcode.i();
		int const top = opcode.j();
		if (down + top <= m_size) {
			for (int i = top + down; i < m_size; ++i) {
				m_values[i - down] = m_values[i];
			}
			m_size -= down;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::POP_S: {
		if (opcode.i() < m_size) {
			m_values[opcode.i()] = m_values[0];
			for (int i = 1; i < m_size; ++i)
				m_values[i - 1] = m_values[i];
			--m_size;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::BLKPUSH: {
		int const qty = opcode.i();
		int const index = opcode.j();
		if (m_size + qty <= StackState::MAX_STACK_DEPTH && index < m_size) {
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + qty] = m_values[i];
			for (int i = qty - 1, j = index + qty; 0 <= i; --i, --j)
				m_values[i] = m_values[j];
			m_size += qty;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::PUSH_S: {
		int const index = opcode.i();
		if (execPUSHS(index)) {
			ok = true;
		}
		break;
	}
	case Stack::Opcode::PUSH2_S: {
		if (std::max(index0, index1) < m_size && m_size + 2 <= StackState::MAX_STACK_DEPTH) {
			int8_t val0 = m_values[index0];
			int8_t val1 = m_values[index1];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 2] = m_values[i];
			m_values[1] = val0;
			m_values[0] = val1;
			m_size += 2;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::PUSH3_S: {
		if (std::max(std::max(index0, index1), index2) < m_size && m_size + 3 <= StackState::MAX_STACK_DEPTH) {
			int8_t val0 = m_values[index0];
			int8_t val1 = m_values[index1];
			int8_t val2 = m_values[index2];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 3] = m_values[i];
			m_values[2] = val0;
			m_values[1] = val1;
			m_values[0] = val2;
			m_size += 3;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XC2PU: {
		if (std::max({index0, index1, index2, 1}) < m_size && m_size + 1 <= StackState::MAX_STACK_DEPTH) {
			std::swap(m_values[1], m_values[index0]);
			std::swap(m_values[0], m_values[index1]);
			int8_t value = m_values[index2];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 1] = m_values[i];
			m_values[0] = value;
			++m_size;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCPU: {
		if (std::max(index0, index1) < m_size && m_size + 1 <= StackState::MAX_STACK_DEPTH) {
			std::swap(m_values[0], m_values[index0]);
			int8_t value = m_values[index1];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 1] = m_values[i];
			m_values[0] = value;
			++m_size;
			ok = true;
		}
		break;
	}
	case Stack::Opcode::PUXC: {
		int const index = opcode.i();
		int const j = opcode.j();
		if (execPUXC(index, j)) {
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCHG2: {
		int const i = opcode.i();
		int const j = opcode.j();
		if (std::max(i, j) < m_size) {
			std::swap(m_values[1], m_values[i]);
			std::swap(m_values[0], m_values[j]);
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCHG3: {
		int const i = opcode.i();
		int const j = opcode.j();
		int const k = opcode.k();
		if (std::max(std::max(2, i), std::max(j, k)) < m_size) {
			std::swap(m_values[2], m_values[i]);
			std::swap(m_values[1], m_values[j]);
			std::swap(m_values[0], m_values[k]);
			ok = true;
		}
		break;
	}
	case Stack::Opcode::PU2XC: {
		if (execPUSHS(index0)) {
			std::swap(m_values[0], m_values[1]);
			if (execPUXC(index1 + 1, index2 + 1)) {
				ok = true;
			}
		}
		break;
	}
	case Stack::Opcode::PUXCPU: {
		if (execPUXC(index0, index1) && execPUSHS(index2 + 1)) {
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCPUXC: {
		if (std::max(index0, 1) < m_size) {
			std::swap(m_values[1], m_values[index0]);
			if (execPUXC(index1, index2)) {
				ok = true;
			}
		}
		break;
	}
	case Stack::Opcode::PUXC2: {
		if (std::max(std::max(1, index0), std::max(index1, index2)) < m_size && m_size + 1 <= StackState::MAX_STACK_DEPTH) {
			int8_t val = m_values[index0];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 1] = m_values[i];
			m_values[0] = val;
			++m_size;
			std::swap(m_values[2], m_values[0]);
			std::swap(m_values[1], m_values[index1 + 1]);
			std::swap(m_values[0], m_values[index2 + 1]);
			ok = true;
		}
		break;
	}
	case Stack::Opcode::XCPU2: {
		if (std::max(index0, std::max(index1, index2)) < m_size && m_size + 2 <= StackState::MAX_STACK_DEPTH) {
			std::swap(m_values[0], m_values[index0]);
			int8_t val1 = m_values[index1];
			int8_t val2 = m_values[index2];
			for (int i = m_size - 1; 0 <= i; --i)
				m_values[i + 2] = m_values[i];
			m_values[1] = val1;
			m_values[0] = val2;
			m_size += 2;
			ok = true;
		}
		break;
	}
	}

	if (ok)
		updHash();
	return ok;
}

void StackState::updHash() {
	m_hash = m_size;
	for (int i = 0; i < m_size; ++i) {
		m_hash = m_hash * 31 + m_values[i];
	}
}

std::optional<int> StackOpcodeSquasher::gasCost(int startStackSize, StackState const& _state, bool _withCompoundOpcodes) {
	if (m_dp[_withCompoundOpcodes][StackState::MAX_STACK_DEPTH].empty()) {
		init();
	}
	auto it = m_dp[_withCompoundOpcodes][startStackSize].find(_state);
	if (it == m_dp[_withCompoundOpcodes][startStackSize].end()) {
		return std::nullopt;
	}
	return it->second.gasCost;
}

std::array<std::array<std::unordered_map<StackState, StackOpcodeSquasher::DpState>, StackState::MAX_STACK_DEPTH + 1>, 2> StackOpcodeSquasher::m_dp {};

void StackOpcodeSquasher::init() {
	for (int _withCompoundOpcodes = 0; _withCompoundOpcodes <= 1; ++_withCompoundOpcodes) {
		struct Edge {
			Pointer<Stack> opcode;
			int gas{};
		};
		std::vector<Edge> edges;
		auto addEdge = [&](auto const& opcode){
			edges.emplace_back(Edge{opcode, OpcodeUtils::gasCost(*opcode)});
		};

		for (int i = 1; i < StackState::MAX_STACK_DEPTH; ++i)
			addEdge(std::make_shared<Stack>(Stack::Opcode::POP_S, i));
		for (int down = 1; down <= StackState::MAX_STACK_DEPTH; ++down)
			for (int up = 1; down + up <= StackState::MAX_STACK_DEPTH; ++up)
				addEdge(std::make_shared<Stack>(Stack::Opcode::BLKDROP2, down, up));
		for (int n = 1; n <= StackState::MAX_STACK_DEPTH; ++n)
			addEdge(std::make_shared<Stack>(Stack::Opcode::DROP, n));
		for (int down = 1; down < StackState::MAX_STACK_DEPTH; ++down)
			for (int up = 1; down + up < StackState::MAX_STACK_DEPTH; ++up)
				addEdge(std::make_shared<Stack>(Stack::Opcode::BLKSWAP, down, up));
		for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
			for (int j = i + 1; j < StackState::MAX_STACK_DEPTH; ++j)
				addEdge(std::make_shared<Stack>(Stack::Opcode::XCHG, i, j));
		for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
			for (int n = 2; i + n <= StackState::MAX_STACK_DEPTH; ++n)
				addEdge(std::make_shared<Stack>(Stack::Opcode::REVERSE, n, i));
		for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
			addEdge(std::make_shared<Stack>(Stack::Opcode::PUSH_S, i));

		if (_withCompoundOpcodes)
		{
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int qty = 2; i + 1 + qty <= StackState::MAX_STACK_DEPTH; ++qty)
					addEdge(std::make_shared<Stack>(Stack::Opcode::BLKPUSH, qty, i));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					addEdge(std::make_shared<Stack>(Stack::Opcode::PUSH2_S, i, j));
			for (int i = 1; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					addEdge(std::make_shared<Stack>(Stack::Opcode::XCPU, i, j));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = -1; j < StackState::MAX_STACK_DEPTH; ++j)
					addEdge(std::make_shared<Stack>(Stack::Opcode::PUXC, i, j));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					addEdge(std::make_shared<Stack>(Stack::Opcode::XCHG2, i, j));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = 0; k < StackState::MAX_STACK_DEPTH; ++k) {
						addEdge(std::make_shared<Stack>(Stack::Opcode::XC2PU, i, j, k));
						addEdge(std::make_shared<Stack>(Stack::Opcode::XCPU2, i, j, k));
						addEdge(std::make_shared<Stack>(Stack::Opcode::XCHG3, i, j, k));
					}
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = -1; j + 1 < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = -1; k + 1 < StackState::MAX_STACK_DEPTH; ++k)
						addEdge(std::make_shared<Stack>(Stack::Opcode::PUXC2, i, j, k));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = -1; k + 1 < StackState::MAX_STACK_DEPTH; ++k)
						addEdge(std::make_shared<Stack>(Stack::Opcode::XCPUXC, i, j, k));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = -1; j + 1 < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = -1; k + 1 < StackState::MAX_STACK_DEPTH; ++k)
						addEdge(std::make_shared<Stack>(Stack::Opcode::PUXCPU, i, j, k));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = -1; j + 1 < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = -2; k + 2 < StackState::MAX_STACK_DEPTH; ++k)
						addEdge(std::make_shared<Stack>(Stack::Opcode::PU2XC, i, j, k));
			for (int i = 0; i < StackState::MAX_STACK_DEPTH; ++i)
				for (int j = 0; j < StackState::MAX_STACK_DEPTH; ++j)
					for (int k = 0; k < StackState::MAX_STACK_DEPTH; ++k)
						addEdge(std::make_shared<Stack>(Stack::Opcode::PUSH3_S, i, j, k));
		}

		std::stable_sort(edges.begin(), edges.end(), [](Edge const &a, Edge const &b){
			return a.gas < b.gas;
		});

		int const MAX_DEPTH = _withCompoundOpcodes ? 2 : MAX_NEW_OPCODES;
		using namespace std::chrono;
		//auto begin = high_resolution_clock::now();
		for (int stackSize = 0; stackSize <= StackState::MAX_STACK_DEPTH; ++stackSize) {
			struct QData {
				StackState state;
				int gas{};
				int8_t opcodeQty{};
			};
			std::unordered_map<StackState, DpState> dp;
			auto comp = [](QData const& x, QData const& y){
				if (x.gas != y.gas)
					return x.gas < y.gas;
				return x.state.getHash() < y.state.getHash();
			};
			auto q = std::set<QData, decltype(comp)>(comp);
			{
				StackState state{stackSize};
				dp.emplace(state, DpState{0, StackState{stackSize}, nullptr});
				q.emplace(QData{state, 0, 0});
			}
			while (!q.empty()) {
				auto front = q.begin();
				StackState const state = front->state;
				int8_t const nextOpcodeQty = front->opcodeQty + 1;
				int const gas = front->gas;
				q.erase(front);
				for (Edge const &e: edges) {
					StackState nextState = state;
					if (nextState.apply(*e.opcode)) {
						auto it = dp.find(nextState);
						int nextGasCost = gas + e.gas;
						if (it == dp.end()) {
							dp.emplace(nextState, DpState{nextGasCost, state, e.opcode});
							if (nextOpcodeQty < MAX_DEPTH)
								q.emplace(QData{nextState, nextGasCost, nextOpcodeQty});
						} else if (it->second.gasCost > nextGasCost) {
							q.erase(QData{nextState, it->second.gasCost});
							dp.erase(it);
							dp.emplace(nextState, DpState{nextGasCost, state, e.opcode});
							if (nextOpcodeQty < MAX_DEPTH) {
								q.emplace(QData{nextState, nextGasCost, nextOpcodeQty});
							}
						}
					}
				}
			}
			m_dp[_withCompoundOpcodes][stackSize] = dp;
		}
		//auto end = high_resolution_clock::now();
		//auto duration = duration_cast<std::chrono::milliseconds>(end - begin);
		//std::cerr << "Time difference = " << duration.count() << "[ms]" << std::endl;
		//double size = m_dp.at(_withCompoundOpcodes).at(StackState::MAX_STACK_DEPTH).size();
		//std::cerr << "dp.size() = " << size << ": " << (size * 20) / (1024 * 1024) << " MB" << std::endl;
	}
}

std::vector<Pointer<TvmAstNode>> StackOpcodeSquasher::recover(int startStackSize, StackState state, bool _withCompoundOpcodes) {
	std::vector<Pointer<TvmAstNode>> res;
	StackState start{startStackSize};
	auto const& prev = m_dp.at(_withCompoundOpcodes).at(startStackSize);
	while (state != start) {
		auto const& it = prev.at(state);
		state = it.prevState;
		res.push_back(it.opcode);
	}
	std::reverse(res.begin(), res.end());
	return res;
}
