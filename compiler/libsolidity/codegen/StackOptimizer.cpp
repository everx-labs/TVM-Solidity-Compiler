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
 * Stack optimizer
 */

#include <boost/format.hpp>

#include "TvmAst.hpp"
#include "TVMCommons.hpp"
#include "TVMConstants.hpp"
#include "StackOptimizer.hpp"
#include "TVMSimulator.hpp"

namespace solidity::frontend {

bool StackOptimizer::visit(DeclRetFlag &/*_node*/) {
	delta(+1);
	return false;
}

bool StackOptimizer::visit(Opaque &_node) {
	delta(- _node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(HardCode &_node) {
	delta(- _node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(Loc &/*_node*/) {
	return false;
}

bool StackOptimizer::visit(TvmReturn &_node) {
	int take{};
	if (_node.withIf()) {
		take = 1;
	}
	delta(-take);
	return false;
}

bool StackOptimizer::visit(ReturnOrBreakOrCont &/*_node*/) {
	int ss = scopeSize();
	delta(-ss);
	return false;
}

bool StackOptimizer::visit(TvmException &_node) {
	delta(-_node.take());
	return false;
}

bool StackOptimizer::visit(StackOpcode &_node) {
	delta(- _node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(PushCellOrSlice &_node) {
	delta(- _node.take() + _node.ret());
	return false;
}

bool StackOptimizer::visit(Glob &_node) {
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
	delta(- take + ret);
	return false;
}

bool StackOptimizer::visit(Stack &_node) {
	int delta{};
	switch (_node.opcode()) {
	case Stack::Opcode::BLKPUSH:
		delta = _node.i();
		break;
	case Stack::Opcode::DROP:
	case Stack::Opcode::BLKDROP2:
		delta = - _node.i();
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

bool StackOptimizer::visit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	for (size_t i = 0; i < instructions.size(); ) {
		if (successfullyUpdate(i, instructions)) {
			m_didSome = true;

			//Printer p{std::cout};
			//std::cout << i << "\n";
			//std::cout << "<<<<<\n";
			//for (const auto& x  : _node.instructions())
			//    x->accept(p);
			//std::cout << "=====\n";
			//for (const auto& x  : instructions)
			//    x->accept(p);
			//std::cout << ">>>>>\n";
			//_node.upd(instructions);

			// do nothing
		} else {
			Pointer<TvmAstNode> const& op = instructions.at(i);
			op->accept(*this);
			++i;
		}
	}
	_node.upd(instructions);
	return false;
}

bool StackOptimizer::visit(SubProgram &_node) {
	int savedStack = size();
	delta(- _node.take());

	startScope();
	delta(+ _node.take());
	_node.block()->accept(*this);
	endScope();

	delta(_node.ret());
	solAssert(savedStack - _node.take() + _node.ret() == size(), "");
	return false;
}

bool StackOptimizer::visit(LogCircuit &_node) {
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

bool StackOptimizer::visit(TvmIfElse &_node) {
	delta(-1);
	for (const auto& body : {_node.trueBody(), _node.falseBody()}) {
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

bool StackOptimizer::visit(TvmRepeat &_node) {
	int savedStack = size();
	delta(-1);
	startScope();
	_node.body()->accept(*this);
	solAssert(savedStack - 1 == size(), "");
	endScope();
	solAssert(savedStack - 1 == size(), "");
	return false;
}

bool StackOptimizer::visit(TvmUntil &_node) {
	int savedStack = size();
	startScope();
	_node.body()->accept(*this);
	solAssert(savedStack + 1 == size(), "");
	endScope();
	solAssert(savedStack == size(), "");
	return false;
}

bool StackOptimizer::visit(TryCatch &_node) {
	const int savedStack = size();

	// try body
	startScope();
	_node.tryBody()->accept(*this);
	endScope();
	solAssert(savedStack == size(), "");

	// catch body
	startScope();
	delta(2); // 2 error variables
	_node.catchBody()->accept(*this);
	solAssert(savedStack <= size(), "");
	endScope();

	solAssert(savedStack == size(), "");
	return false;
}

bool StackOptimizer::visit(While &_node) {
	int savedStack = size();

	startScope();
	_node.condition()->accept(*this);
	solAssert(savedStack + 1 == size(), "");
	endScope();
	solAssert(savedStack == size(), "");

	startScope();
	_node.body()->accept(*this);
	solAssert(savedStack == size(), "");
	endScope();
	solAssert(savedStack == size(), "");

	return false;
}

bool StackOptimizer::visit(Function &f) {
	switch (f.type()) {
		case Function::FunctionType::PrivateFunctionWithObj:
		case Function::FunctionType::Fragment:
		case Function::FunctionType::OnCodeUpgrade:
		case Function::FunctionType::OnTickTock: {
			if (f.name() != "c7_to_c4_for_await") {
				for (int iter = 0; iter < TvmConst::IterStackOptQty; ++iter) {
					m_didSome = false;
					m_stackSize.clear();
					initStack(f.take());
					f.block()->accept(*this);
					if (!m_didSome)
						break;
				}
			}
			break;
		}

		case Function::FunctionType::PublicStateVariableGetter:
		case Function::FunctionType::MainInternal:
		case Function::FunctionType::MainExternal:
			break;
	}
	return false;
}

bool StackOptimizer::visit(Contract &_node) {
	for (Pointer<Function> const& f: _node.functions()) {
		f->accept(*this);
	}
	return false;
}

void StackOptimizer::endVisit(CodeBlock &/*_node*/) {
	// do nothing
}

bool StackOptimizer::visitNode(TvmAstNode const&) {
	solUnimplemented("StackOptimizer::visitNode");
}

void StackOptimizer::endVisitNode(TvmAstNode const&) {
	solUnimplemented("StackOptimizer::endVisitNode");
}

bool StackOptimizer::successfullyUpdate(int index, std::vector<Pointer<TvmAstNode>>& instructions) {
	Pointer<TvmAstNode> const& op = instructions.at(index);
	if (to<Loc>(op.get()))
		return false;

	size_t index2 = index + 1;
	while (index2 < instructions.size() && isLoc(instructions.at(index2)))
		++index2;

	auto stack = to<Stack>(op.get());
	bool ok = false;
	std::vector<Pointer<TvmAstNode>> commands;

	// gen(0,1) / GETGLOB
	// ...
	// BLKSWAP N, 1
	// =>
	// ...
	// gen(0, 1)
	if (auto gen = to<Gen>(op.get());
		gen && gen->isPure() && std::make_pair(gen->take(), gen->ret()) == std::make_pair(0, 1)
	) {
		Simulator sim{instructions.begin() + index + 1, instructions.end(), 1, 1, false, true};
		bool good = true;
		{
			auto glob = to<Glob>(op.get());
			if (glob) {
				good = glob->opcode() == Glob::Opcode::GetOrGetVar &&
						sim.setGlobIndexes().count(glob->index()) == 0 &&
						!sim.wasCall();
			}
		}
		if (good && sim.wasMoved() && !sim.wasSet()) {
			ok = true;
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end()); // TODO!!!!!
			commands.emplace_back(op);
			for (auto iter = sim.getIter();
				iter != instructions.end();
				++iter
			) {
				commands.emplace_back(*iter);
			}
		}
	}

	// DUP
	// ...
	// POP Si
	// =>
	// ...
	// BLKSWAP i-1, 1
	if (!ok && isPUSH(op) && isPUSH(op).value() == 0) {
		Simulator sim{instructions.begin() + index + 1, instructions.end(), 2, 1, true};
		if (sim.wasSet()) {
			ok = true;
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			auto iter = sim.getIter();
			auto popSi = isPOP(*iter);
			solAssert(popSi && *popSi >= 2, "");
			commands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
			++iter;
			for ( ; iter != instructions.end(); ++iter) {
				commands.emplace_back(*iter);
			}
		}
	}

	// ROLLREV X
	// ...
	// POP Si
	// =>
	// DROP
	// ...
	// BLKSWAP 1, i-1
	if (!ok && isBLKSWAP(op)) {
		auto [bottom, top] = isBLKSWAP(op).value();
		if (top == 1) {
			Simulator sim{instructions.begin() + index + 1, instructions.end(), bottom + 1, 1, true};
			if (sim.wasSet()) {
				ok = true;
				commands.emplace_back(makeDROP());
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
				auto iter = sim.getIter();
				auto popSi = isPOP(*iter);
				solAssert(popSi && *popSi >= 2, "");
				commands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
				++iter;
				for ( ; iter != instructions.end(); ++iter) {
					commands.emplace_back(*iter);
				}
			}
		}
	}

	// gen01
	// ...
	// POP Si
	// =>
	// ...
	// BLKSWAP 1, i-1
	if (!ok && isPureGen01(*op)) {
		Simulator sim{instructions.begin() + index + 1, instructions.end(), 1, 1, true};
		if (sim.wasSet()) {
			ok = true;
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			auto iter = sim.getIter();
			auto popSi = isPOP(*iter);
			solAssert(popSi && *popSi >= 2, "");
			commands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
			++iter;
			for ( ; iter != instructions.end(); ++iter) {
				commands.emplace_back(*iter);
			}
		}
	}

	// BLKSWAP N, 1
	// PUSH S[N]
	// ...
	// POP S?
	// =>
	// ...
	// BLKSWAP ?, 1
	if (!ok && isBLKSWAP(op) && isBLKSWAP(op).value().second == 1) {
		int const n = isBLKSWAP(op).value().first;
		if (index2 != instructions.size()) {
			auto pushS = isPUSH(instructions.at(index2));
			if (pushS && pushS.value() == n) {
				Simulator sim{instructions.begin() + index2 + 1, instructions.end(), n + 2, 1, true};
				if (sim.wasSet()) {
					ok = true;
					commands.insert(commands.end(), instructions.begin() + index + 1, instructions.begin() + index2);
					commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
					auto iter = sim.getIter();
					auto popSi = isPOP(*iter);
					solAssert(popSi && *popSi >= 2, "");
					commands.emplace_back(makeBLKSWAP(*popSi - 1, 1));
					++iter;
					for ( ; iter != instructions.end(); ++iter) {
						commands.emplace_back(*iter);
					}
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
	if (!ok && isPOP(op)) {
		if (index2 != instructions.size()) {
			auto pushS = isPUSH(instructions.at(index2));
			if (pushS && *pushS + 1 == *isPOP(op)) {
				Simulator sim{instructions.begin() + index2 + 1, instructions.end(), *isPOP(op) + 1, 1, true};
				if (sim.wasSet()) {
					ok = true;
					// we take original commands because we don't remove value from stack
					commands.insert(commands.end(), instructions.begin() + index + 1, instructions.begin() + index2);
					commands.insert(commands.end(), instructions.begin() + index2 + 1, instructions.end());
				}
			}
		}
	}

	// POP Si
	// =>
	// DROP
	if (!ok && isPOP(op)) {
		int startStackSize = isPOP(op).value();
		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
		if (sim.wasSet() || sim.success()) {
			ok = true;
			commands.emplace_back(makeDROP());
			commands.insert(commands.end(), instructions.begin() + index + 1, instructions.end());
		}
	}

	if (!ok && (isBLKSWAP(op) || isREVERSE(op) || isXCHG_S0(op))) {
		int len{};
		if (isREVERSE(op)) {
			auto[n, i] = isREVERSE(op).value();
			len = i + n;
		} else if (isBLKSWAP(op)) {
			auto[down, up] = isBLKSWAP(op).value();
			len = down + up;
		} else if (isXCHG_S0(op)) {
			int Si = isXCHG_S0(op).value();
			len = Si + 1;
		} else {
			solUnimplemented("");
		}

		// try to just ignore this opcode
		{
			Simulator sim{instructions.begin() + index + 1, instructions.end(), len, len};
			if (sim.success()) {
				ok = true;
				commands.insert(commands.end(), instructions.begin() + index + 1, instructions.end());
			}
		}
		if (!ok && isSWAP(op)) {
			int startStackSize = 2;
			Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
			if (sim.success()) {
				ok = true;
				commands.emplace_back(makeDROP());
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}
	}

	if (!ok && isPUSH(op)) {
		int Si = stack->i();

		// PUSH Si
		// ... S[0..i-1] aren not used anymore
		// =>
		// BLKDROP i
		// PUSH S0
		// ...
		if (Si <= scopeSize() && Si > 0) {
			Simulator sim{instructions.begin() + index + 1, instructions.end(), Si + 1, Si};
			if (sim.success()) {
				ok = true;
				commands.emplace_back(makeDROP(Si));
				commands.emplace_back(makePUSH(0));
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}

		// PUSH Si
		// ... S[i] isn't used anymore
		// =>
		// ROLL i
		// ...
		if (!ok) {
			int startStackSize = Si + 2;
			Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
			if (sim.success()) {
				ok = true;
				if (Si >= 1)
					commands.emplace_back(makeBLKSWAP(1, Si));
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}
	}

	// gen01
	// ...
	// =>
	// ...
	if (!ok && isPureGen01(*op)) {
		int startStackSize = 1;
		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
		if (sim.success()) {
			ok = true;
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
		}
	}

	// Delete useless variable from the stack
	// ...
	// =>
	// DROP
	// ...
	if (!ok && !isDrop(op)) {
		bool isPrevFlag{};
		if (index > 0) {
			Pointer<TvmAstNode> prevOp = instructions.at(index - 1);
			isPrevFlag = to<DeclRetFlag>(prevOp.get()) != nullptr;
		}
		if (scopeSize() >= 1 && !isPrevFlag) {
			auto beg = instructions.begin() + index;
			Simulator sim{beg, instructions.end(), 1, 1};
			if (sim.success()) {
				ok = true;
				commands.emplace_back(makeDROP());
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}
	}
	// TODO : fails for NIP IFRET
	// Delete useless variable from the stack
	// ...
	// =>
	// NIP
	// ...
	//if (!ok && !isPOP(op) && !isDrop(op)) {
	//	if (scopeSize() >= 2) {
	//		auto beg = instructions.begin() + index;
	//		Simulator sim{beg, instructions.end(), 2, 1};
	//		if (sim.success()) {
	//			ok = true;
	//			commands.emplace_back(makeBLKDROP2(1, 1));
	//			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
	//		}
	//	}
	//}

	// Delete useless variable from the stack
	// DROP N
	// ...
	// =>
	// DROP N+1
	// ...
	if (!ok && isDrop(op)) {
		int n = isDrop(op).value();
		auto beg = instructions.begin() + index + 1;
		if (beg != instructions.end() &&
			scopeSize() >= n + 1
		) {
			Simulator sim{beg, instructions.end(), 1, 1};
			if (sim.success()) {
				ok = true;
				commands.emplace_back(makeDROP(n + 1));
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}
	}


	if (!ok) {
		return false;
	}

	instructions.erase(instructions.begin() + index, instructions.end());
	instructions.insert(instructions.end(), commands.begin(), commands.end());
	return true;
}


void StackOptimizer::initStack(int size) {
	solAssert(m_stackSize.empty(), "");
	m_stackSize.emplace_back(size);
}

void StackOptimizer::delta(int delta) {
	solAssert(!m_stackSize.empty(), "");
	m_stackSize.back() += delta;
	solAssert(m_stackSize.back() >= 0, "");
}

int StackOptimizer::size() {
	solAssert(!m_stackSize.empty(), "");
	return m_stackSize.back();
}

int StackOptimizer::scopeSize() {
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
