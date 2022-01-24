/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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

bool StackOptimizer::visit(GenOpcode &_node) {
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
		case Stack::Opcode::DROP:
		case Stack::Opcode::BLKDROP2:
			delta = - _node.i();
			break;

		case Stack::Opcode::POP_S:
			delta = -1;
			break;

		case Stack::Opcode::BLKPUSH:
			delta = _node.i();
			break;
		case Stack::Opcode::PUSH2_S:
			delta = 2;
			break;
		case Stack::Opcode::PUSH3_S:
			delta = 3;
			break;
		case Stack::Opcode::PUSH_S:
			delta = 1;
			break;

		case Stack::Opcode::BLKSWAP:
		case Stack::Opcode::REVERSE:
		case Stack::Opcode::XCHG:
			break;

		case Stack::Opcode::TUCK:
		case Stack::Opcode::PUXC:
			delta = 1;
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

bool StackOptimizer::visit(TvmCondition &_node) {
	int savedStack = size();
	delta(-1);
	for (const auto& body : {_node.trueBody(), _node.falseBody()}) {
		startScope();
		body->accept(*this);
		solAssert(savedStack - 1 + _node.ret() == size(), "");
		endScope();
	}
	delta(_node.ret());
	solAssert(savedStack - 1 + _node.ret() == size(), "");
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
		case Function::FunctionType::PrivateFunction:
		case Function::FunctionType::Macro:
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

		case Function::FunctionType::MacroGetter:
		case Function::FunctionType::MainInternal:
		case Function::FunctionType::MainExternal:
			break;
	}
	return false;
}

bool StackOptimizer::visit(Contract &_node) {
	for (Pointer<Function> &f: _node.functions()) {
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

	auto stack = to<Stack>(op.get());
	bool cmd1IsPUSH= stack && stack->opcode() == Stack::Opcode::PUSH_S;
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
		Simulator sim{instructions.begin() + index + 1, instructions.end(), 1, 1};
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

	if (!ok && isPOP(op)) {
		int startStackSize = isPOP(op).value();
		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
		if (sim.wasSet()) {
			ok = true;
			commands.emplace_back(makeDROP());
			commands.insert(commands.end(), instructions.begin() + index + 1, instructions.end());
		} else if (sim.success()) {
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

	if (!ok && cmd1IsPUSH) {
		int Si = stack->i();

		// try to delete values
		if (Si <= scopeSize() && Si > 0) {
			Simulator sim{instructions.begin() + index + 1, instructions.end(), Si + 1, Si};
			if (sim.success()) {
				ok = true;
				commands.emplace_back(makeDROP(Si));
				commands.emplace_back(makePUSH(0));
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			}
		}

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

	if (!ok && isPureGen01OrGetGlob(*op)) {
		int startStackSize = 1;
		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize, 1};
		if (sim.success()) {
			ok = true;
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
		}
	}

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
