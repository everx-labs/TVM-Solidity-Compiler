/*
 * Copyright (C) 2021-2026 EverX. All Rights Reserved.
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

#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/optimizers/TVMSimulator.hpp>

using namespace solidity::frontend;

Simulator::Simulator(
	std::vector<Pointer<TvmAstNode>>::const_iterator _beg,
	std::vector<Pointer<TvmAstNode>>::const_iterator _end,
	int _stackSize,
	int _segment,
	Type _type,
	bool is_nested_scope,
	std::function<
		void(std::vector<Pointer<TvmAstNode>>&, std::vector<Pointer<TvmAstNode>>::const_iterator& iter)> const&
		onSuccess
):
	SEGMENT{_segment},
	TYPE{_type},
	IS_NESTED_SCOPE{is_nested_scope},
	m_stackSize{_stackSize},
	END_CMD{_end},
	m_onSuccess{onSuccess} {
	solAssert(_segment <= _stackSize, "");
	run(_beg);
}

void Simulator::run(std::vector<Pointer<TvmAstNode>>::const_iterator const _beg) {
	m_curCmd = _beg;
	for (; m_curCmd != END_CMD; ++m_curCmd) {
		auto node = m_curCmd->get();
		node->accept(*this);
		if (!m_ableToConvertOpcode || m_success) {
			break;
		}
	}

	if (m_ableToConvertOpcode) {
		if (m_curCmd != END_CMD) {
			++m_curCmd;
			m_newCommands.insert(m_newCommands.end(), m_curCmd, END_CMD);
		}
	}
}

bool Simulator::visit(AsymGen& /*_node*/) { solUnimplemented(""); }

bool Simulator::visit(DeclRetFlag& _node) {
	++m_stackSize;
	m_newCommands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Opaque& _node) {
	if (_node.take() > rest()) {
		setUnableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_newCommands.emplace_back(_node.shared_from_this());
	}
	return false;
}

bool Simulator::visit(HardCode& _node) {
	if (_node.take() > rest()) {
		setUnableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_newCommands.emplace_back(_node.shared_from_this());
	}
	return false;
}

bool Simulator::visit(Loc& _node) {
	m_newCommands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(TvmReturn& _node) {
	if (_node.withIf()) {
		if (rest() == 0) {
			setUnableToConvertOpcode();
		}
		--m_stackSize;
	}
	m_newCommands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(ReturnOrBreakOrCont& _node) {
	if (_node.take() > rest()) {
		setUnableToConvertOpcode();
		return false;
	}

	Simulator
		sim{_node.body()->instructions().begin(),
			_node.body()->instructions().end(),
			m_stackSize,
			SEGMENT,
			Type::DROP_VALUE_IN_RBC,
			IS_NESTED_SCOPE,
			m_onSuccess};
	if (!sim.m_ableToConvertOpcode) {
		setUnableToConvertOpcode();
		return false;
	}

	m_alwaysDroppedFromRBC = sim.m_success;
	if (sim.m_success) {
		switch (TYPE) {
		case Type::DROP_VALUE:
		case Type::DROP_VALUE_IN_RBC:
			m_success = true;
			break;
		case Type::SET_VALUE:
		case Type::MOVE_VALUE:
			if (!IS_NESTED_SCOPE) {
				setUnableToConvertOpcode();
				return false;
			}
		}
	}

	m_wasFunctionCall = m_wasFunctionCall || sim.m_wasFunctionCall;
	m_setGlobs.insert(sim.m_setGlobs.begin(), sim.m_setGlobs.end());
	m_stackSize = sim.m_stackSize;
	auto body = createNode<CodeBlock>(_node.body()->type(), sim.newCommands());
	m_newCommands.emplace_back(createNode<ReturnOrBreakOrCont>(_node.type(), _node.take(), body));
	return false;
}

bool Simulator::visit(TvmException& _node) {
	if (_node.take() > rest()) {
		setUnableToConvertOpcode();
	} else {
		m_stackSize -= _node.take();
		m_newCommands.emplace_back(_node.shared_from_this());
		if (!_node.withIf()) {
			switch (TYPE) {
			case Type::DROP_VALUE:
			case Type::DROP_VALUE_IN_RBC:
				m_success = true;
				break;
			case Type::SET_VALUE:
			case Type::MOVE_VALUE:
				break;
			}
		}
	}
	return false;
}

bool Simulator::visit(StackGen& _node) {
	if (_node.take() > rest()) {
		setUnableToConvertOpcode();
	} else {
		m_stackSize += _node.ret() - _node.take();
		m_newCommands.emplace_back(_node.shared_from_this());
	}
	m_wasFunctionCall |= isIn(_node.opcode(), "CALL", ".inline");
	return false;
}

bool Simulator::visit(CellOrSliceOperation& _node) {
	solAssert(_node.take() == 0 && _node.ret() == 1, "");
	++m_stackSize;
	m_newCommands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Glob& _node) {
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
			setUnableToConvertOpcode();
		}
		break;
	}
	m_newCommands.emplace_back(_node.shared_from_this());
	return false;
}

bool Simulator::visit(Stack& _node) {
	int i = _node.i();
	int j = _node.j();
	// int k = _node.k();
	int maxDownIndex = m_stackSize - 1;
	int minUpIndex = m_stackSize - SEGMENT;
	int newi = i < maxDownIndex ? i : i - SEGMENT;
	int newj = j < maxDownIndex ? j : j - SEGMENT;
	// int newk = k < maxDownIndex ? k : k - SEGMENT;
	switch (_node.opcode()) {
	case Stack::Opcode::PUSH_S: {
		if (minUpIndex <= i && i <= maxDownIndex) {
			setUnableToConvertOpcode();
		} else if (i < maxDownIndex) {
			m_newCommands.emplace_back(_node.shared_from_this());
			++m_stackSize;
		} else if (i > maxDownIndex) {
			m_newCommands.emplace_back(makePUSH(newi));
			++m_stackSize;
		}
		break;
	}
	case Stack::Opcode::POP_S: {
		// `POP S1` <==> `BLKDROP2 1, 1`
		if (SEGMENT == 1 && i == 1 && m_stackSize == 2) {
			switch (TYPE) {
			case Type::DROP_VALUE: {
				if (IS_NESTED_SCOPE) {
					m_newCommands.emplace_back(makeDROP());
					m_stackSize = 1;
				} else {
					m_success = true;
				}
				break;
			}
			case Type::DROP_VALUE_IN_RBC:
				// Note: in PeepholeOptimizer.hpp StackOpcodeSquasher::gasCost can replace BLKDROP2 1, 1 => NIP
				m_success = true;
				break;
			case Type::SET_VALUE:
				if (IS_NESTED_SCOPE) {
					setUnableToConvertOpcode();
				} else {
					m_success = true;
					m_onSuccess(m_newCommands, m_curCmd);
				}
				break;
			case Type::MOVE_VALUE:
				setUnableToConvertOpcode();
				break;
			}
		} else if (rest() == 0) {
			setUnableToConvertOpcode();
		} else if (i == maxDownIndex && SEGMENT == 1) {
			switch (TYPE) {
			case Type::DROP_VALUE:
				m_newCommands.emplace_back(makeDROP());
				--m_stackSize;
				break;
			case Type::DROP_VALUE_IN_RBC:
				solUnimplemented("");
			case Type::SET_VALUE:
				if (IS_NESTED_SCOPE)
					setUnableToConvertOpcode();
				else {
					m_success = true;
					m_onSuccess(m_newCommands, m_curCmd);
				}
				break;
			case Type::MOVE_VALUE:
				setUnableToConvertOpcode();
				break;
			}
		} else if (i < minUpIndex) {
			--m_stackSize;
			m_newCommands.emplace_back(_node.shared_from_this());
		} else if (i > maxDownIndex) {
			--m_stackSize;
			m_newCommands.emplace_back(makePOP(newi));
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::DROP: {
		int n = i;
		if (m_stackSize <= n) {
			switch (TYPE) {
			case Type::DROP_VALUE:
				if (IS_NESTED_SCOPE) {
					// TODO it's hard to support logic with negative m_stackSize
					setUnableToConvertOpcode();
				} else {
					m_success = true;
					if (n != SEGMENT)
						m_newCommands.emplace_back(makeDROP(n - SEGMENT));
				}
				break;
			case Type::DROP_VALUE_IN_RBC:
				m_success = true;
				if (n != SEGMENT)
					m_newCommands.emplace_back(makeDROP(n - SEGMENT));
				break;
			case Type::SET_VALUE:
			case Type::MOVE_VALUE:
				setUnableToConvertOpcode();
				break;
			}
		} else if (rest() >= n) {
			m_stackSize -= n;
			m_newCommands.emplace_back(makeDROP(n));
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}

	case Stack::Opcode::BLKDROP2: {
		int drop = i;
		int restUp = j;
		if (TYPE == Type::SET_VALUE && restUp == 1 && drop == 1 && SEGMENT == 1 && m_stackSize == 2) {
			if (IS_NESTED_SCOPE) {
				setUnableToConvertOpcode();
			} else {
				m_success = true;
				m_onSuccess(m_newCommands, m_curCmd);
			}
		} else if (restUp >= m_stackSize) {
			m_newCommands.emplace_back(makeBLKDROP2(drop, restUp - SEGMENT));
		} else if (restUp <= rest() && restUp + drop >= m_stackSize) {
			switch (TYPE) {
			case Type::DROP_VALUE:
				if (IS_NESTED_SCOPE) {
					m_stackSize = m_stackSize - drop;
					if (m_stackSize < SEGMENT) {
						setUnableToConvertOpcode();
					} else {
						if (drop != SEGMENT) {
							m_newCommands.emplace_back(makeBLKDROP2(drop - SEGMENT, restUp));
						}
						m_newCommands.emplace_back(makeBLKDROP2(SEGMENT, m_stackSize - SEGMENT));
					}
				} else {
					if (drop != SEGMENT)
						m_newCommands.emplace_back(makeBLKDROP2(drop - SEGMENT, restUp));
					m_success = true;
				}
				break;
			case Type::DROP_VALUE_IN_RBC:
				if (drop != SEGMENT)
					m_newCommands.emplace_back(makeBLKDROP2(drop - SEGMENT, restUp));
				m_success = true;
				break;
			case Type::SET_VALUE:
			case Type::MOVE_VALUE:
				setUnableToConvertOpcode();
			}
		} else if (restUp + drop <= rest()) {
			m_newCommands.emplace_back(makeBLKDROP2(drop, restUp));
			m_stackSize -= drop;
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::BLKPUSH: {
		int n = i;
		int maxDownPushIndex = j;
		int minUpPushIndex = maxDownPushIndex - n + 1; // include
		if (std::max(minUpIndex, minUpPushIndex) > std::min(maxDownIndex, maxDownPushIndex)) {
			m_newCommands.emplace_back(makeBLKPUSH(n, newj));
			m_stackSize += n;
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::REVERSE: {
		int n = i;
		int minUpReverseIndex = j;
		int maxDownReverseIndex = minUpReverseIndex + n - 1; // include
		if (SEGMENT == 1 && minUpReverseIndex <= m_stackSize - 1 && m_stackSize - 1 <= maxDownReverseIndex) {
			if (i - 1 >= 2)
				m_newCommands.emplace_back(makeREVERSE(i - 1, j));
			m_stackSize = minUpReverseIndex + (maxDownReverseIndex - (m_stackSize - 1)) + 1;
		} else if (std::max(minUpIndex, minUpReverseIndex) > std::min(maxDownIndex, maxDownReverseIndex)) {
			if (maxDownIndex < minUpReverseIndex) {
				m_newCommands.emplace_back(makeREVERSE(n, j - SEGMENT));
			} else {
				m_newCommands.emplace_back(_node.shared_from_this());
			}
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::BLKSWAP: {
		// int down = i;
		// int up = j;
		if (i == 1 && j + i == m_stackSize && SEGMENT == 1 && TYPE == Type::MOVE_VALUE) {
			if (IS_NESTED_SCOPE) {
				setUnableToConvertOpcode();
			} else {
				m_success = true;
				m_onSuccess(m_newCommands, m_curCmd);
			}
		} else if (j <= rest() && i + j >= m_stackSize) {
			if (i - SEGMENT >= 1)
				m_newCommands.emplace_back(makeBLKSWAP(i - SEGMENT, j));
			m_stackSize -= j;
		} else if (i + j <= rest()) {
			m_newCommands.emplace_back(makeBLKSWAP(i, j));
		} else if (j >= m_stackSize) {
			if (j - SEGMENT >= 1) {
				m_newCommands.emplace_back(makeBLKSWAP(i, j - SEGMENT));
			}
			m_stackSize += i; // we take i elements and push ones to the top of the stack
		} else {
			setUnableToConvertOpcode();
		}
		break;
	}
	case Stack::Opcode::XCHG:
		solAssert(i < j, "");
		solAssert(j != -1, "");
		if (SEGMENT == 1 && i == m_stackSize - 1) {
			if (j >= 2) {
				m_newCommands.emplace_back(makeREVERSE(j, 0));
				if (j - i - 1 >= 1)
					m_newCommands.emplace_back(makeBLKSWAP(j - i - 1, 1));
				m_newCommands.emplace_back(makeREVERSE(j, 0));
			}
			m_stackSize = j + 1;
		} else if (SEGMENT == 1 && j == m_stackSize - 1) {
			if (i > 0)
				m_newCommands.emplace_back(makeBLKSWAP(1, i));
			if (j - 1 >= 1)
				m_newCommands.emplace_back(makeBLKSWAP(j - 1, 1));
			m_stackSize = i + 1;
		} else if ((minUpIndex <= i && i <= maxDownIndex) || (minUpIndex <= j && j <= maxDownIndex)) {
			// Note: it's ok if j==-1
			setUnableToConvertOpcode();
		} else {
			m_newCommands.emplace_back(makeXCH_S_S(newi, newj));
		}
		break;
	case Stack::Opcode::PUSH2_S:
	case Stack::Opcode::PUSH3_S:
	case Stack::Opcode::PUXC:
	case Stack::Opcode::XCPU:
	case Stack::Opcode::PUXC2:
	case Stack::Opcode::XC2PU:
	case Stack::Opcode::XCPU2:
	case Stack::Opcode::XCHG2:
	case Stack::Opcode::XCPUXC:
	case Stack::Opcode::PUXCPU:
	case Stack::Opcode::PU2XC:
	case Stack::Opcode::XCHG3: {
		solUnimplemented("");
	}
	}

	return false;
}

bool Simulator::visit(CodeBlock& /*_node*/) { solUnimplemented("Don't call me"); }

bool Simulator::visit(SubProgram& _node) {
	auto res = simulateBlock(*_node.block(), false);
	if (!res) {
		return false;
	}
	if (res->success) {
		switch (TYPE) {
		case Type::DROP_VALUE:
		case Type::DROP_VALUE_IN_RBC:
		case Type::SET_VALUE:
		case Type::MOVE_VALUE:
			m_success = true;
		}
	}
	m_stackSize = res->stackSize;
	m_newCommands.emplace_back(createNode<SubProgram>(_node.isJmp(), res->block));
	return false;
}

bool Simulator::visit(LogCircuit& _node) {
	if (rest() == 0) {
		setUnableToConvertOpcode();
		return false;
	}

	// `PUSHCONT {} IF[NOT]` takes one value
	--m_stackSize;

	auto res = simulateBlock(*_node.body());
	if (!res) {
		return false;
	}
	m_newCommands.emplace_back(createNode<LogCircuit>(_node.type(), res->block));
	return false;
}

bool Simulator::visit(TryCatch& /*_node*/) {
	setUnableToConvertOpcode();
	return false;
}

bool Simulator::visit(TvmIfElse& _node) {
	if (rest() == 0) {
		setUnableToConvertOpcode();
		return false;
	}
	--m_stackSize;

	bool wasDropped = true;
	std::array<Pointer<CodeBlock>, 2> bodies = {_node.trueBody(), _node.falseBody()};
	std::vector<int> stackSizes;
	for (Pointer<CodeBlock>& body: bodies) {
		if (body) {
			auto res = simulateBlock(*body);
			if (!res) {
				return false;
			}
			body = res->block;
			wasDropped = wasDropped && res->success;
			stackSizes.push_back(res->stackSize);
		}
	}
	if (TYPE == Type::DROP_VALUE && _node.falseBody() != nullptr && wasDropped) {
		m_success = true;
	}
	if (_node.withJmp() && _node.falseBody() != nullptr && stackSizes.at(0) == stackSizes.at(1)) {
		m_stackSize = stackSizes.at(0);
	} else {
		m_stackSize += _node.ret();
	}
	m_newCommands.emplace_back(
		createNode<TvmIfElse>(_node.withNot(), _node.withJmp(), bodies.at(0), bodies.at(1), _node.ret())
	);
	return false;
}

bool Simulator::visit(TvmRepeat& _node) {
	if (rest() == 0) {
		setUnableToConvertOpcode();
		return false;
	}
	--m_stackSize;

	auto res = simulateBlock(*_node.body());
	if (!res) {
		return false;
	}

	m_newCommands.emplace_back(createNode<TvmRepeat>(_node.withBreakOrReturn(), res->block));
	return false;
}

bool Simulator::visit(TvmUntil& _node) {
	if (rest() == 0) {
		setUnableToConvertOpcode();
		return false;
	}

	auto res = simulateBlock(*_node.body());
	if (!res) {
		return false;
	}
	m_newCommands.emplace_back(createNode<TvmUntil>(_node.withBreakOrReturn(), res->block));
	return false;
}

bool Simulator::visit(While& _node) {
	// condition
	Pointer<CodeBlock> condition = _node.condition();
	{
		auto res = simulateBlock(*condition);
		if (!res) {
			return false;
		}
		condition = res->block;
	}
	// body
	Pointer<CodeBlock> body = _node.body();
	{
		auto res = simulateBlock(*body);
		if (!res) {
			return false;
		}
		body = res->block;
	}
	m_newCommands.emplace_back(createNode<While>(_node.isInfinite(), _node.withBreakOrReturn(), condition, body));
	return false;
}

bool Simulator::visit(Contract& /*_node*/) { solUnimplemented(""); }

bool Simulator::visit(Function& /*_node*/) { solUnimplemented(""); }

void Simulator::endVisit(CodeBlock& /*_node*/) {}

std::optional<Simulator::Result> Simulator::simulateBlock(CodeBlock const& body, bool bodyIsNested) {
	Simulator
		sim{body.instructions().begin(),
			body.instructions().end(),
			m_stackSize,
			SEGMENT,
			TYPE,
			bodyIsNested,
			m_onSuccess};
	if (!sim.m_ableToConvertOpcode) {
		setUnableToConvertOpcode();
		return {};
	}
	m_setGlobs.insert(sim.m_setGlobs.begin(), sim.m_setGlobs.end());
	m_wasFunctionCall = m_wasFunctionCall || sim.m_wasFunctionCall;
	m_alwaysDroppedFromRBC = m_alwaysDroppedFromRBC && sim.m_alwaysDroppedFromRBC;
	return {
		{.block = createNode<CodeBlock>(body.type(), sim.newCommands()),
		 .stackSize = sim.m_stackSize,
		 .success = sim.success()}
	};
}

bool Simulator::success() const { return m_success && m_ableToConvertOpcode; }

bool Simulator::visitNode(TvmAstNode const&) { solUnimplemented(""); }

int Simulator::rest() const {
	int r = m_stackSize - SEGMENT;
	solAssert(r >= 0, "");
	return r;
}
