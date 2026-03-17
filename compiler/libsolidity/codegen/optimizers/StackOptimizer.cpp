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
 * Stack optimizer
 */

#include <boost/format.hpp>

#include <libsolidity/codegen/Printer.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/optimizers/StackOptimizer.hpp>
#include <libsolidity/codegen/optimizers/TVMSimulator.hpp>

namespace solidity::frontend {

bool hasTvmReturn(TvmAstNode const& node);
bool hasTvmReturn(TvmAstNode const& node) {
	if (node.category() == TvmAstNode::Category::TvmReturn)
		return true;

	auto opaque = convertToOpaque(&node);
	if (opaque)
		for (auto const& cmd: opaque->block()->instructions())
			if (hasTvmReturn(*cmd))
				return true;

	return false;
}

bool StackOptimizer::visit(DeclRetFlag& /*_node*/) {
	delta(+1);
	return false;
}

bool StackOptimizer::visit(Opaque& _node) {
	delta(-_node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(HardCode& _node) {
	delta(-_node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(Loc& /*_node*/) { return false; }

bool StackOptimizer::visit(TvmReturn& _node) {
	int take{};
	if (_node.withIf()) {
		take = 1;
	}
	delta(-take);
	return false;
}

bool StackOptimizer::visit(ReturnOrBreakOrCont& _node) {
	_node.body()->accept(*this);
	return false;
}

bool StackOptimizer::visit(TvmException& _node) {
	delta(-_node.take());
	return false;
}

bool StackOptimizer::visit(StackGen& _node) {
	delta(-_node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(CellOrSliceOperation& _node) {
	delta(-_node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(Glob& _node) {
	int take{};
	int ret{};
	switch (_node.opcode()) {
	case Glob::Opcode::GetOrGetVar:
	case Glob::Opcode::PUSHROOT:
	case Glob::Opcode::PUSH_C3:
	case Glob::Opcode::PUSH_C7:
		ret = 1;
		break;

	case Glob::Opcode::SetOrSetVar:
	case Glob::Opcode::POPROOT:
	case Glob::Opcode::POP_C3:
	case Glob::Opcode::POP_C7:
		take = 1;
		break;
	}
	delta(-take + ret);
	return false;
}

bool StackOptimizer::visit(Stack& _node) {
	int delta{};
	switch (_node.opcode()) {
	case Stack::Opcode::BLKPUSH:
		delta = _node.i();
		break;
	case Stack::Opcode::DROP:
	case Stack::Opcode::BLKDROP2:
		delta = -_node.i();
		break;
	case Stack::Opcode::POP_S:
		delta = -1;
		break;
	case Stack::Opcode::BLKSWAP:
	case Stack::Opcode::REVERSE:
	case Stack::Opcode::XCHG:
	case Stack::Opcode::XCHG2:
	case Stack::Opcode::XCHG3:
		break;
	case Stack::Opcode::PUXC:
	case Stack::Opcode::XCPU:
	case Stack::Opcode::XC2PU:
	case Stack::Opcode::PUXC2:
	case Stack::Opcode::XCPUXC:
	case Stack::Opcode::PUSH_S:
		delta = 1;
		break;
	case Stack::Opcode::PUSH2_S:
	case Stack::Opcode::XCPU2:
	case Stack::Opcode::PUXCPU:
	case Stack::Opcode::PU2XC:
		delta = 2;
		break;
	case Stack::Opcode::PUSH3_S:
		delta = 3;
		break;
	}
	this->delta(delta);
	return false;
}

bool StackOptimizer::visit(CodeBlock& _node) {
	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	std::vector<int> savedStackSize = m_stackSize;

	for (size_t i = 0; i < instructions.size();) {
		if (auto res = successfullyUpdate(i, instructions)) {
			instructions.resize(i);
			instructions += res.value().first;

			m_didSome = true;
			m_stackSize = savedStackSize;
			i = 0;

			// if (m_currentFunctionName == "libWTEnv_with_obj_createGE_b658c66f")
			// {
			// 	Printer p{std::cout};
			// 	std::cout << i << " " << res.value().second << "\n";
			// 	std::cout << "<<<<<\n";
			// 	for (auto const& x: _node.instructions())
			// 		x->accept(p);
			// 	std::cout << "=====\n";
			// 	for (auto const& x: instructions)
			// 		x->accept(p);
			// 	std::cout << ">>>>>\n";
			// 	_node.changeInstructions(instructions);
			// 	std::cout << "\n\n";
			// }
		} else {
			Pointer<TvmAstNode> const& op = instructions.at(i);
			op->accept(*this);
			++i;
		}
	}
	_node.changeInstructions(instructions);
	return false;
}

bool StackOptimizer::visit(SubProgram& _node) {
	_node.block()->accept(*this);
	return false;
}

bool StackOptimizer::visit(LogCircuit& _node) {
	int savedStack = size();
	delta(-2);

	startScope();
	delta(+1);
	_node.body()->accept(*this);
	solAssert(savedStack - 1 == size(), "");
	endScope();

	delta(+1);
	solAssert(savedStack - 1 == size(), "");
	return false;
}

bool StackOptimizer::visit(TvmIfElse& _node) {
	delta(-1);
	for (auto const& body: {_node.trueBody(), _node.falseBody()}) {
		if (body) {
			int savedStack = size();
			startScope();
			body->accept(*this);
			endScope();
			solAssert(savedStack == size(), "");
		}
	}
	delta(_node.ret());
	return false;
}

bool StackOptimizer::visit(TvmRepeat& _node) {
	int savedStack = size();
	delta(-1);
	startScope();
	_node.body()->accept(*this);
	solAssert(savedStack - 1 == size(), "");
	endScope();
	solAssert(savedStack - 1 == size(), "");
	return false;
}

bool StackOptimizer::visit(TvmUntil& _node) {
	int savedStack = size();
	startScope();
	_node.body()->accept(*this);
	solAssert(savedStack + 1 == size(), "");
	endScope();
	solAssert(savedStack == size(), "");
	return false;
}

bool StackOptimizer::visit(While& _node) {
	int savedStack = size();

	startScope();
	_node.condition()->accept(*this);
	solAssert(savedStack + 1 == size(), "");
	endScope();
	solAssert(savedStack == size(), "");

	startScope();
	_node.body()->accept(*this);
	// solAssert(savedStack == size(), "");
	endScope();
	solAssert(savedStack == size(), "");

	return false;
}

bool StackOptimizer::visit(Function& f) {
	m_currentFunctionName = f.name();

	for (;;) {
		m_didSome = false;
		initStack(f.take());
		f.block()->accept(*this);
		if (!m_didSome)
			break;
	}
	return false;
}

bool StackOptimizer::visit(Contract& _node) {
	for (Pointer<Function> const& f: _node.functions()) {
		f->accept(*this);
	}
	return false;
}

void StackOptimizer::endVisit(CodeBlock& /*_node*/) {
	// do nothing
}

bool StackOptimizer::visitNode(TvmAstNode const&) { solUnimplemented("StackOptimizer::visitNode"); }

void StackOptimizer::endVisitNode(TvmAstNode const&) { solUnimplemented("StackOptimizer::endVisitNode"); }

std::optional<std::pair<std::vector<Pointer<TvmAstNode>>, std::string>>
StackOptimizer::successfullyUpdate(int index, std::vector<Pointer<TvmAstNode>>& instructions) const {
	Pointer<TvmAstNode> const& op = instructions.at(index);
	if (convertToLoc(op.get()))
		return {};

	size_t index2 = index + 1;
	while (index2 < instructions.size() && convertToLoc(instructions.at(index2).get()))
		++index2;
	Pointer<TvmAstNode> cmd2;
	if (index2 != instructions.size()) {
		cmd2 = instructions.at(index2);
	}

	auto stack = convertToStack(op.get());
	std::vector<Pointer<TvmAstNode>> commands;

	auto appendTail = [&](std::vector<Pointer<TvmAstNode>>::const_iterator iter) {
		for (; iter != instructions.end(); ++iter) {
			commands.emplace_back(*iter);
		}
	};

	bool inTryCatch = m_tryCatchQty != 0;

	// ==================================== MOVE ====================================

	// gen(0,1) / GETGLOB
	// ...
	// BLKSWAP N, 1
	// =>
	// ...
	// gen(0, 1)
	if (auto gen = convertToGen(op.get()); !inTryCatch && gen && gen->isPure() && gen->take() == 0 && gen->ret() == 1) {
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				1,
				1,
				Simulator::Type::MOVE_VALUE,
				false,
				[&op](std::vector<Pointer<TvmAstNode>>& newCommands, auto const&) { newCommands.emplace_back(op); }};
		bool good = true;
		if (auto glob = convertToGlob(op.get())) {
			good = glob->opcode() == Glob::Opcode::GetOrGetVar &&
				   !sim.setGlobIndexes().contains(glob->index()) &&
				   !sim.wasFunctionCall();
		}
		if (good && sim.success()) {
			commands = sim.newCommands();
			return {{commands, "move"}};
		}
	}

	// ==================================== SET ====================================

	// DUP
	// ...
	// POP Si
	// =>
	// ...
	// BLKSWAP i-1, 1
	if (isPUSH(op) && isPUSH(op).value() == 0) {
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				2,
				1,
				Simulator::Type::SET_VALUE,
				false,
				[](std::vector<Pointer<TvmAstNode>>& newCommands,
				   std::vector<Pointer<TvmAstNode>>::const_iterator& iter) {
					Pointer<TvmAstNode> const& cmd = *iter;
					if (auto popSi = isPOP(cmd)) {
						if (popSi >= 2)
							newCommands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
					} else if (cmd->category() == TvmAstNode::Category::TvmException)
						newCommands.emplace_back(cmd);
					else
						solUnimplemented("");
				}};
		if (sim.success() && (scopeSize() >= 1 || sim.alwaysDroppedFromRBC())) {
			commands = sim.newCommands();
			return {{commands, "set0"}};
		}
	}

	// ROLLREV X
	// ...
	// POP Si
	// =>
	// DROP
	// ...
	// BLKSWAP 1, i-1
	if (isBLKSWAP(op)) {
		auto [bottom, top] = isBLKSWAP(op).value();
		if (top == 1) {
			Simulator
				sim{instructions.begin() + index + 1,
					instructions.end(),
					bottom + 1,
					1,
					Simulator::Type::SET_VALUE,
					false,
					[](std::vector<Pointer<TvmAstNode>>& newCommands,
					   std::vector<Pointer<TvmAstNode>>::const_iterator& iter) {
						auto cmd = *iter;
						if (auto popSi = isPOP(cmd)) {
							if (popSi >= 2)
								newCommands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
						} else if (cmd->category() == TvmAstNode::Category::TvmException)
							newCommands.emplace_back(cmd);
						else
							solUnimplemented("");
					}};
			if (sim.success()) {
				commands.emplace_back(makeDROP());
				commands += sim.newCommands();
				return {{commands, "set1"}};
			}
		}
	}

	// gen01
	// ...
	// POP Si
	// =>
	// ...
	// BLKSWAP 1, i-1
	if (isPureGen01(*op) && !inTryCatch) {
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				1,
				1,
				Simulator::Type::SET_VALUE,
				false,
				[](std::vector<Pointer<TvmAstNode>>& newCommands,
				   std::vector<Pointer<TvmAstNode>>::const_iterator& iter) {
					auto popSi = isPOP(*iter);
					solAssert(popSi && *popSi >= 1, "");
					if (*popSi >= 2)
						newCommands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
				}};
		if (sim.success()) {
			commands = sim.newCommands();
			return {{commands, "set2"}};
		}
	}

	// BLKSWAP N, 1
	// PUSH S[N]
	// ...
	// POP S?
	// =>
	// ...
	// BLKSWAP ?, 1
	if (isBLKSWAP(op) && isBLKSWAP(op).value().second == 1) {
		int const n = isBLKSWAP(op).value().first;
		if (index2 != instructions.size()) {
			auto pushS = isPUSH(instructions.at(index2));
			if (pushS && pushS.value() == n) {
				Simulator
					sim{instructions.begin() + index2 + 1,
						instructions.end(),
						n + 2,
						1,
						Simulator::Type::SET_VALUE,
						false,
						[](std::vector<Pointer<TvmAstNode>>& newCommands,
						   std::vector<Pointer<TvmAstNode>>::const_iterator& iter) {
							auto popSi = isPOP(*iter);
							solAssert(popSi && *popSi >= 1, "");
							if (*popSi >= 2)
								newCommands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
						}};
				if (sim.success()) {
					commands.insert(commands.end(), instructions.begin() + index + 1, instructions.begin() + index2);
					commands += sim.newCommands();
					return {{commands, "set3"}};
				}
			}
		}
	}

	// POP S[i]
	// PUSH S[i-1]
	// ...
	// POP S?
	// =>
	// ...
	// POP S?
	if (isPOP(op)) {
		if (index2 != instructions.size()) {
			auto pushS = isPUSH(instructions.at(index2));
			if (pushS && *pushS + 1 == *isPOP(op)) {
				Simulator
					sim{instructions.begin() + index2 + 1,
						instructions.end(),
						*isPOP(op) + 1,
						1,
						Simulator::Type::SET_VALUE,
						false,
						[](std::vector<Pointer<TvmAstNode>>&, std::vector<Pointer<TvmAstNode>>::const_iterator&) {

						}};
				if (sim.success()) {
					// we take original commands because we don't remove value from stack
					commands.insert(commands.end(), instructions.begin() + index + 1, instructions.begin() + index2);
					appendTail(instructions.begin() + index2 + 1);
					return {{commands, "set4"}};
				}
			}
		}
	}

	// PUSH Si
	// ...
	// NIP
	// =>
	// BLKSWAP
	// ...
	if (isPUSH(op)) {
		int Si = stack->i();
		int startStackSize = Si + 2;
		bool hasNIP = false;
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				startStackSize,
				1,
				Simulator::Type::SET_VALUE,
				false,
				[&hasNIP](std::vector<Pointer<TvmAstNode>>&, std::vector<Pointer<TvmAstNode>>::const_iterator& iter) {
					auto popSi = isPOP(*iter);
					if (popSi && *popSi == 1) {
						hasNIP = true;
					}
				}};
		if (hasNIP && sim.success() && (scopeSize() >= Si + 1 || sim.alwaysDroppedFromRBC())) {
			if (Si >= 1) {
				commands.emplace_back(makeBLKSWAP(1, Si));
			}
			commands += sim.newCommands();
			return {{commands, "set5"}};
		}
	}

	// PUSH Si
	// ...
	// POP Sj
	// =>
	// BLKSWAP
	// ...
	// BLKSWAP
	if (isPUSH(op)) {
		int Si = stack->i();
		int startStackSize = Si + 2;
		bool validPOP = false;
		std::optional<int> popSi;
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				startStackSize,
				1,
				Simulator::Type::SET_VALUE,
				false,
				[&validPOP, &Si, &popSi](
					std::vector<Pointer<TvmAstNode>>& newCommands,
					std::vector<Pointer<TvmAstNode>>::const_iterator& iter
				) {
					popSi = isPOP(*iter);
					if (popSi && Si <= 2 && *popSi - 1 <= 2) {
						validPOP = true;
						if (*popSi >= 2) {
							newCommands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
						}
					}
				}};
		if (sim.success() && scopeSize() >= Si + 1) {
			if (validPOP) {
				if (Si >= 1) {
					commands.emplace_back(makeBLKSWAP(1, Si));
				}
				commands += sim.newCommands();
				return {{commands, "set6"}};
			}
		}
	}

	// ==================================== DROP ====================================

	// POP Si
	// ...
	// =>
	// DROP
	// ...
	// Delete useless opcode
	if (isPOP(op) && !inTryCatch) {
		int startStackSize = isPOP(op).value();
		Simulator sim{
			instructions.begin() + index + 1,
			instructions.end(),
			startStackSize,
			1,
			Simulator::Type::DROP_VALUE,
			false,
			nullptr,
		};
		if (sim.success()) {
			commands.emplace_back(makeDROP());
			appendTail(instructions.begin() + index + 1);
			return {{commands, "drop0"}};
		}
	}

	if (isBLKSWAP(op) || isREVERSE(op) || isXCHG_S0(op)) {
		int len{};
		if (isREVERSE(op)) {
			auto [n, i] = isREVERSE(op).value();
			len = i + n;
		} else if (isBLKSWAP(op)) {
			auto [down, up] = isBLKSWAP(op).value();
			len = down + up;
		} else if (isXCHG_S0(op)) {
			int Si = isXCHG_S0(op).value();
			len = Si + 1;
		} else {
			solUnimplemented("");
		}

		// try to just ignore this opcode
		{
			Simulator
				sim{instructions.begin() + index + 1,
					instructions.end(),
					len,
					len,
					Simulator::Type::DROP_VALUE,
					false,
					nullptr};
			if (sim.success()) {
				appendTail(instructions.begin() + index + 1);
				return {{commands, "drop1"}};
			}
		}
		// SWAP
		// ...
		// =>
		// DROP
		// ...
		if (isSWAP(op) && m_withDrop) {
			Simulator
				sim{instructions.begin() + index + 1,
					instructions.end(),
					2,
					1,
					Simulator::Type::DROP_VALUE,
					false,
					nullptr};
			if (sim.success()) {
				commands.emplace_back(makeDROP());
				commands += sim.newCommands();
				return {{commands, "drop2"}};
			}
		}
	}

	if (isPUSH(op) && !inTryCatch && m_withDrop) {
		int Si = stack->i();

		// PUSH Si
		// ... S[0..i-1] are not used anymore
		// =>
		// BLKDROP i
		// PUSH S0
		// ...
		if (Si > 0) {
			Simulator
				sim{instructions.begin() + index + 1,
					instructions.end(),
					Si + 1,
					Si,
					Simulator::Type::DROP_VALUE,
					false,
					nullptr};
			if (sim.success() && (scopeSize() >= Si || sim.alwaysDroppedFromRBC())) {
				commands.emplace_back(makeDROP(Si));
				commands.emplace_back(makePUSH(0));
				commands += sim.newCommands();
				return {{commands, "drop3"}};
			}
		}

		// PUSH Si
		// ... S[i] isn't used anymore
		// =>
		// ROLL i
		// ...
		{
			int startStackSize = Si + 2;
			Simulator
				sim{instructions.begin() + index + 1,
					instructions.end(),
					startStackSize,
					1,
					Simulator::Type::DROP_VALUE,
					false,
					nullptr};
			if (sim.success() && (scopeSize() >= Si + 1 || sim.alwaysDroppedFromRBC())) {
				if (Si >= 1)
					commands.emplace_back(makeBLKSWAP(1, Si));
				commands += sim.newCommands();
				return {{commands, "drop4"}};
			}
		}
	}

	// gen01
	// ...
	// =>
	// ...
	if (isPureGen01(*op) && !inTryCatch) {
		Simulator
			sim{instructions.begin() + index + 1,
				instructions.end(),
				1,
				1,
				Simulator::Type::DROP_VALUE,
				false,
				nullptr};
		if (sim.success()) {
			commands = sim.newCommands();
			return {{commands, "drop5"}};
		}
	}

	// Delete useless variable from the stack
	// ...
	// =>
	// DROP
	// ...
	if (!isDrop(op) && !inTryCatch && m_withDrop) {
		auto beg = instructions.begin() + index;
		Simulator sim{beg, instructions.end(), 1, 1, Simulator::Type::DROP_VALUE, false, nullptr};
		if (sim.success() && (scopeSize() >= 1 || (m_stackSize.back() >= 1 && sim.alwaysDroppedFromRBC()))) {
			commands.emplace_back(makeDROP());
			commands += sim.newCommands();
			return {{commands, "drop6"}};
		}
	}

	// Delete useless variable from the stack
	// ...
	// =>
	// BLKDROP2 1, N
	// ...
	for (int droppedCount = 1; droppedCount <= 1; ++droppedCount) {
		for (int leftCount = 1; leftCount <= 10; ++leftCount) {
			if (!inTryCatch &&
				!isPOP(op) &&
				!isBLKDROP2(op) &&
				!isDrop(op) &&
				!hasTvmReturn(*op) &&
				(cmd2 == nullptr || !hasTvmReturn(*cmd2)) &&
				m_withDrop) {
				Simulator
					sim{instructions.begin() + index,
						instructions.end(),
						droppedCount + leftCount,
						droppedCount,
						Simulator::Type::DROP_VALUE,
						false,
						nullptr};
				if (scopeSize() >= droppedCount + leftCount ||
					(m_stackSize.back() >= droppedCount + leftCount && sim.alwaysDroppedFromRBC())) {
					if (sim.success()) {
						commands.emplace_back(makeBLKDROP2(droppedCount, leftCount));
						commands += sim.newCommands();
						return {{commands, "drop7"}};
					}
				}
			}
		}
	}

	// Delete useless variable from the stack
	// DROP N
	// ...
	// =>
	// DROP N+1
	// ...
	if (isDrop(op) && !inTryCatch && m_withDrop) {
		int n = isDrop(op).value();
		auto beg = instructions.begin() + index + 1;
		if (beg != instructions.end() && scopeSize() >= n + 1) {
			Simulator sim{beg, instructions.end(), 1, 1, Simulator::Type::DROP_VALUE, false, nullptr};
			if (sim.success()) {
				commands.emplace_back(makeDROP(n + 1));
				commands += sim.newCommands();
				return {{commands, "drop8"}};
			}
		}
	}

	return {};
}


void StackOptimizer::initStack(int size) {
	m_stackSize.clear();
	m_stackSize.emplace_back(size);
}

void StackOptimizer::delta(int delta) {
	solAssert(!m_stackSize.empty(), "");
	m_stackSize.back() += delta;
}

int StackOptimizer::size() const {
	solAssert(!m_stackSize.empty(), "");
	return m_stackSize.back();
}

int StackOptimizer::scopeSize() const {
	solAssert(!m_stackSize.empty(), "");
	int n = m_stackSize.size();
	int scopeSize = m_stackSize.at(n - 1) - (n == 1 ? 0 : m_stackSize.at(n - 2));
	// Sometimes 'scopeSize' maybe negative, but it's ok because we move some variables.
	// But after each code block we restore stack size.
	return scopeSize;
}

void StackOptimizer::startScope() {
	solAssert(!m_stackSize.empty(), "");
	m_stackSize.emplace_back(m_stackSize.back());
}

void StackOptimizer::endScope() {
	solAssert(!m_stackSize.empty(), "");
	m_stackSize.pop_back();
	solAssert(!m_stackSize.empty(), "");
}

} // end solidity::frontend
