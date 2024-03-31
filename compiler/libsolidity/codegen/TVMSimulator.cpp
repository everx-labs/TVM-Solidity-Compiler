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
/**
 * Simulator of TVM execution
 */

#include "TVMCommons.hpp"
#include "TVMSimulator.hpp"

using namespace solidity::frontend;

void Simulator::run(const std::vector<Pointer<TvmAstNode>>::const_iterator _beg,
					const std::vector<Pointer<TvmAstNode>>::const_iterator _end) {
	m_iter = _beg;
	for (; m_iter != _end; ++m_iter) {
		auto nextIter = m_iter + 1;
		if (nextIter != _end && isPopAndDrop(*m_iter, *nextIter)) {
			solAssert(m_isDropped, "");
			solAssert(m_ableToConvertOpcode, "");
			m_iter += 2;
			break;
		}

		auto node = m_iter->get();
		node->accept(*this);
		if (m_wasSet && m_stopSimulationIfSet) {
			break;
		}
		if (m_wasMoved && m_stopSimulationIfMoved) {
			++m_iter;
			break;
		}
		if (!m_ableToConvertOpcode || m_isDropped) {
			++m_iter;
			break;
		}
		solAssert(m_stackSize >= m_segment, "");
	}
	if (success()) {
		for (; m_iter != _end; ++m_iter) {
			m_commands.emplace_back(*m_iter);
		}
	}
}

bool Simulator::visit(AsymGen &/*_node*/) {
	solUnimplemented("");
}

bool Simulator::visit(DeclRetFlag &_node) {
	++m_stackSize;
	m_commands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Opaque &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_commands.emplace_back(_node.shared_from_this());
	}
	return false;
}

bool Simulator::visit(HardCode &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_commands.emplace_back(_node.shared_from_this());
	}
	return false;
}

bool Simulator::visit(Loc &_node) {
	m_commands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(TvmReturn &_node) {
	if (_node.withIf()) {
		solUnimplemented("Only in ReturnChecker");
	}
	m_commands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(ReturnOrBreakOrCont &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
		return false;
	}
	m_wasReturnBreakContinue = true;
	Simulator sim{_node.body()->instructions().begin(), _node.body()->instructions().end(), m_stackSize, m_segment};
	if (!sim.m_ableToConvertOpcode || !sim.m_isDropped) {
		// check if value has been dropped because in otherwise, this value is from scope that hasn't been dropped
		unableToConvertOpcode();
	} else {
		solAssert(sim.m_isDropped, "");
		m_isDropped = true;
		auto body = createNode<CodeBlock>(_node.body()->type(), sim.commands());
		m_commands.emplace_back(createNode<ReturnOrBreakOrCont>(_node.take(), body));
	}
	return false;
}

bool Simulator::visit(TvmException &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
	} else {
		m_stackSize -= _node.take();
		m_commands.emplace_back(_node.shared_from_this());
	}
	return false;
}

bool Simulator::visit(StackOpcode &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_commands.emplace_back(_node.shared_from_this());
	}
	m_wasCall |= isIn(_node.opcode(), "CALL", "EXECUTE", ".inline");
	return false;
}

bool Simulator::visit(PushCellOrSlice &_node) {
	++m_stackSize;
	m_commands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Glob &_node) {
	switch (_node.opcode()) {
		case Glob::Opcode::GetOrGetVar:
		case Glob::Opcode::PUSHROOT:
		case Glob::Opcode::PUSH_C3:
		case Glob::Opcode::PUSH_C7:
			++m_stackSize;
			break;

		case Glob::Opcode::SetOrSetVar: {
			m_setGlobs.insert(_node.index());
			[[fallthrough]];
		}
		case Glob::Opcode::POPROOT:
		case Glob::Opcode::POP_C3:
		case Glob::Opcode::POP_C7:
			if (rest() >= 1) {
				--m_stackSize;
			} else {
				unableToConvertOpcode();
			}
			break;
	}
	m_commands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Stack &_node) {
	int i = _node.i();
	int j = _node.j();
	int k = _node.k();
	int maxDownIndex = m_stackSize - 1;
	int minUpIndex = m_stackSize - m_segment;
	int newi = i < maxDownIndex ? i : i - m_segment;
	int newj = j < maxDownIndex ? j : j - m_segment;
	int newk = k < maxDownIndex ? k : k - m_segment;
	switch (_node.opcode()) {
	case Stack::Opcode::PUSH_S: {
		if (minUpIndex <= i && i <= maxDownIndex) {
			unableToConvertOpcode();
		} else if (i < maxDownIndex) {
			m_commands.emplace_back(_node.shared_from_this());
			++m_stackSize;
		} else if (i > maxDownIndex) {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi));
			++m_stackSize;
		}
		break;
	}
	case Stack::Opcode::POP_S: {
		if (m_segment == 1 && i == 1 && m_stackSize == 2) { // it equals to BLKDROP2 1, 1
			m_isDropped = true;
		} else if (rest() == 0) {
			unableToConvertOpcode();
		} else if (i == maxDownIndex && m_segment == 1) {
			m_wasSet = true;
			--m_stackSize; // TODO delete this
			// don't push to m_commands, just ignore the opcode
		} else if (i < minUpIndex) {
			--m_stackSize;
			m_commands.emplace_back(_node.shared_from_this());
		} else if (i > maxDownIndex) {
			// here _node.i() >= 2
			--m_stackSize;
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi));
		} else {
			unableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::DROP: {
		int n = i;
		if (m_stackSize <= n) {
			m_isDropped = true;
			if (n - m_segment > 0)
				m_commands.emplace_back(makeDROP(n - m_segment));
		} else if (rest() >= n) {
			m_stackSize -= n;
			m_commands.emplace_back(makeDROP(n));
		} else {
			unableToConvertOpcode();
		}
		break;
	}

	case Stack::Opcode::BLKDROP2: {
		int drop = i;
		int rest = j;
		if (rest >= m_stackSize) {
			m_commands.emplace_back(makeBLKDROP2(drop, rest - m_segment));
		} else if (rest <= this->rest() && rest + drop >= m_stackSize) {
			if (drop - m_segment > 0) {
				m_commands.emplace_back(makeBLKDROP2(drop - m_segment, rest));
			}
			m_isDropped = true;
		} else if (rest + drop <= this->rest()) {
			m_commands.emplace_back(makeBLKDROP2(drop, rest));
			m_stackSize -= drop;
		} else {
			unableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::BLKPUSH: {
		int n = i;
		int maxDownPushIndex = j;
		int minUpPushIndex = maxDownPushIndex - n + 1; // include
		if (std::max(minUpIndex, minUpPushIndex) > std::min(maxDownIndex, maxDownPushIndex)) {
			m_commands.emplace_back(makeBLKPUSH(n, newj));
			m_stackSize += n;
		} else {
			unableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::REVERSE: {
		int n = i;
		int minUpReverseIndex = j;
		int maxDownReverseIndex = minUpReverseIndex + n - 1; // include
		if (m_segment == 1 && minUpReverseIndex <= m_stackSize - 1 && m_stackSize - 1 <= maxDownReverseIndex) {
			if (i - 1 >= 2)
				m_commands.emplace_back(makeREVERSE(i - 1, j));
			m_stackSize = minUpReverseIndex + (maxDownReverseIndex - (m_stackSize - 1)) + 1;
		} else if (std::max(minUpIndex, minUpReverseIndex) > std::min(maxDownIndex, maxDownReverseIndex)) {
			if (maxDownIndex < minUpReverseIndex) {
				m_commands.emplace_back(makeREVERSE(n, j - m_segment));
			} else {
				m_commands.emplace_back(_node.shared_from_this());
			}
		} else {
			unableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::BLKSWAP: {
		// int down = i;
		// int up = j;
		if (i == 1 && j + i == m_stackSize && m_segment == 1 && m_stopSimulationIfMoved) {
			m_wasMoved = true; // TODO call unableToConvertOpcode with reason: moved, set, etc
		} else if (j <= rest() && i + j >= m_stackSize) {
			if (i - m_segment >= 1)
				m_commands.emplace_back(makeBLKSWAP(i - m_segment, j));
			m_stackSize -= j;
		} else if (i + j <= rest()) {
			m_commands.emplace_back(makeBLKSWAP(i, j));
		} else if (j >= m_stackSize) {
			if (j - m_segment >= 1) {
				m_commands.emplace_back(makeBLKSWAP(i, j - m_segment));
			}
			m_stackSize += i; // we take i elements and push ones to the top of the stack
		} else {
			unableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::XCHG:
		solAssert(i < j, "");
		solAssert(j != -1, "");
		if (m_segment == 1 && i == m_stackSize - 1) {
			if (j >= 2) {
				m_commands.emplace_back(makeREVERSE(j, 0));
				if (j - i - 1 >= 1)
					m_commands.emplace_back(makeBLKSWAP(j - i - 1, 1));
				m_commands.emplace_back(makeREVERSE(j, 0));
			}
			m_stackSize = j + 1;
		} else if (m_segment == 1 && j == m_stackSize - 1) {
			if (i > 0)
				m_commands.emplace_back(makeBLKSWAP(1, i));
			if (j - 1 >= 1)
				m_commands.emplace_back(makeBLKSWAP(j - 1, 1));
			m_stackSize = i + 1;
		} else if ((minUpIndex <= i && i <= maxDownIndex) ||
			(minUpIndex <= j && j <= maxDownIndex)) { // it's ok if j==-1
			unableToConvertOpcode();
		} else {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi, newj));
		}
		break;
	case Stack::Opcode::PUSH2_S:
		if ((minUpIndex <= i && i <= maxDownIndex) ||
			(minUpIndex <= j && j <= maxDownIndex)) {
			unableToConvertOpcode();
		} else {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi, newj));
		}
		m_stackSize += 2;
		break;
	case Stack::Opcode::PUSH3_S:
		if ((minUpIndex <= i && i <= maxDownIndex) ||
			(minUpIndex <= j && j <= maxDownIndex) ||
			(minUpIndex <= k && k <= maxDownIndex)) {
			unableToConvertOpcode();
		} else {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi, newj, newk));
		}
		m_stackSize += 3;
		break;

	case Stack::Opcode::PUXC:
		if ((minUpIndex <= i && i <= maxDownIndex) ||
			(minUpIndex <= j && j <= maxDownIndex) ||  // note: it's false if j==-1
			rest() == 0 // because we make PUSH Si, SWAP ...
		) {
			unableToConvertOpcode();
		} else {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi, newj));
			++m_stackSize;
		}
		break;

	case Stack::Opcode::XCPU: {
		if ((minUpIndex <= i && i <= maxDownIndex) ||
			(minUpIndex <= j && j <= maxDownIndex)
		) {
			unableToConvertOpcode();
		} else {
			m_commands.emplace_back(createNode<Stack>(_node.opcode(), newi, newj));
			++m_stackSize;
		}
		break;
	}
	case Stack::Opcode::PUXC2:
	case Stack::Opcode::XC2PU:
	case Stack::Opcode::XCPU2:
	case Stack::Opcode::XCHG2:
	case Stack::Opcode::XCPUXC:
	case Stack::Opcode::PUXCPU:
	case Stack::Opcode::PU2XC:
	case Stack::Opcode::XCHG3: {
		// TODO implement
		unableToConvertOpcode();
	}
	}

	return false;
}

bool Simulator::visit(CodeBlock &/*_node*/) {
	solUnimplemented("Don't call me");
}

bool Simulator::visit(SubProgram &_node) {
	if (_node.take() > rest()) {
		unableToConvertOpcode();
		return false;
	}
	m_stackSize -= _node.take();

	auto res = trySimulate(*_node.block(), m_stackSize + _node.take(), m_stackSize + _node.ret());
	if (!res) {
		return false;
	}
	m_stackSize += _node.ret();
	m_commands.emplace_back(createNode<SubProgram>(_node.take(), _node.ret(), _node.isJmp(), res.value().first, _node.isPure()));
	return false;
}

bool Simulator::visit(LogCircuit &_node) {
	if (rest() == 0) {
		unableToConvertOpcode();
		return false;
	}
	m_stackSize -= 2;

	auto res = trySimulate(*_node.body(), m_stackSize + 1, m_stackSize + 1);
	if (!res) {
		return false;
	}
	++m_stackSize;
	m_commands.emplace_back(createNode<LogCircuit>(_node.type(), res.value().first));
	return false;
}

bool Simulator::visit(TryCatch &/*_node*/) {
	// TODO implement
	unableToConvertOpcode();
	return false;
}

bool Simulator::visit(TvmIfElse &_node) {
	if (rest() == 0) {
		unableToConvertOpcode();
		return false;
	}
	--m_stackSize;

	bool wasDroped = true;
	std::array<Pointer<CodeBlock>, 2> bodies = {_node.trueBody(), _node.falseBody()};
	for (Pointer<CodeBlock>& body : bodies) {
		if (body) {
			std::optional<std::pair<Pointer<CodeBlock>, bool>> res = trySimulate(*body, m_stackSize, m_stackSize);
			if (!res) {
				return false;
			}
			body = res.value().first;
			wasDroped &= res.value().second;
		}
	}
	if (_node.falseBody() != nullptr && wasDroped) {
		m_isDropped = true;
	}
	m_stackSize += _node.ret();
	m_commands.emplace_back(createNode<TvmIfElse>(_node.withNot(), _node.withJmp(), bodies.at(0), bodies.at(1), _node.ret()));
	return false;
}

bool Simulator::visit(TvmRepeat &_node) {
	if (rest() == 0) {
		unableToConvertOpcode();
		return false;
	}
	--m_stackSize;

	auto res = trySimulate(*_node.body(), m_stackSize, m_stackSize);
	if (!res) {
		return false;
	}

	m_commands.emplace_back(createNode<TvmRepeat>(_node.withBreakOrReturn(), res.value().first));
	return false;
}

bool Simulator::visit(TvmUntil &_node) {
	if (rest() == 0) {
		unableToConvertOpcode();
		return false;
	}

	auto res = trySimulate(*_node.body(), m_stackSize, m_stackSize + 1);
	if (!res) {
		return false;
	}
	m_commands.emplace_back(createNode<TvmUntil>(_node.withBreakOrReturn(), res.value().first));
	return false;
}

bool Simulator::visit(While &_node) {
	// condition
	Pointer<CodeBlock> condition = _node.condition();
	{
		auto res = trySimulate(*condition, m_stackSize, m_stackSize + 1);
		if (!res) {
			return false;
		}
		condition = res.value().first;
	}
	// body
	Pointer<CodeBlock> body = _node.body();
	{
		auto res = trySimulate(*body, m_stackSize, m_stackSize);
		if (!res) {
			return false;
		}
		body = res.value().first;
	}
	m_commands.emplace_back(createNode<While>(_node.isInfinite(), _node.withBreakOrReturn(), condition, body));
	return false;
}

bool Simulator::visit(Contract &/*_node*/) {
	solUnimplemented("");
}

bool Simulator::visit(Function &/*_node*/) {
	solUnimplemented("");
}

void Simulator::endVisit(CodeBlock &/*_node*/) {

}

std::optional<std::pair<Pointer<CodeBlock>, bool>>
Simulator::trySimulate(CodeBlock const& body, int begStackSize, int /*endStackSize*/) {
	Simulator sim{body.instructions().begin(), body.instructions().end(), begStackSize, m_segment};
	if (!sim.m_ableToConvertOpcode || sim.m_wasSet) {
		unableToConvertOpcode();
		return {};
	}
	if (sim.m_isDropped && !sim.m_wasReturnBreakContinue) {
		unableToConvertOpcode();
		return {};
	}
	m_setGlobs.insert(sim.m_setGlobs.begin(), sim.m_setGlobs.end());
	m_wasCall |= sim.m_wasCall;
	// solAssert(sim.m_wasReturnBreakContinue || sim.m_stackSize == endStackSize, "");
	return {{createNode<CodeBlock>(body.type(), sim.commands()), sim.m_isDropped && sim.m_wasReturnBreakContinue}};
}

bool Simulator::isPopAndDrop(Pointer<TvmAstNode> const& a, Pointer<TvmAstNode> const& b) {
	auto pop = isPOP(a);
	auto drop = isDrop(b);
	if (m_segment == 1 && pop && drop && pop.value() == m_stackSize - 1 && pop.value() == drop.value() + 1) {
		m_isDropped = true;
		m_commands.emplace_back(makeBLKDROP2(drop.value(), 1));
		return true;
	}
	return false;
}

bool Simulator::success() const {
	if (!m_ableToConvertOpcode) {
		return false;
	}
	return m_isDropped && !m_wasSet;
}

bool Simulator::visitNode(TvmAstNode const&) {
	solUnimplemented("");
}

void Simulator::endVisitNode(TvmAstNode const&) {
}

int Simulator::rest() const {
	int r = m_stackSize - m_segment;
	solAssert(r >= 0, "");
	return r;
}
