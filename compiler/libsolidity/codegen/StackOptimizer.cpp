/*
 * Copyright 2018-2020 TON DEV SOLUTIONS LTD.
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
#include "TVMPusher.hpp"
#include "TVMSimulator.hpp"

namespace solidity::frontend {

bool StackOptimizer::visit(DeclRetFlag &/*_node*/) {
	delta(+1);
	return false;
}

bool StackOptimizer::visit(Opaque &_node) {
	// TODO visit me
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
	switch (_node.type()) {
		case TvmReturn::Type::RET:
			break;
		case TvmReturn::Type::IFRET:
		case TvmReturn::Type::IFNOTRET:
			take = 1;
			break;
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
			ret = 1;
			break;

		case Glob::Opcode::SetOrSetVar:
		case Glob::Opcode::POPROOT:
		case Glob::Opcode::POP_C3:
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
	}
	this->delta(delta);
	return false;
}

bool StackOptimizer::visit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	for (size_t i = 0; i < instructions.size(); ) {
		if (successfullyUpdate(i, instructions)) {
			// do nothing
			//_node.upd(instructions);
			//std::cout << "<<<" << std::endl;
			//Printer p{std::cout};
			//_node.accept(p);
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
	solAssert(savedStack - _node.take() + _node.ret() == size(), "");
	endScope();

	delta(_node.ret());
	solAssert(savedStack - _node.take() + _node.ret() == size(), "");
	return false;
}

bool StackOptimizer::visit(TvmCondition &_node) {
	int savedStack = size();
	delta(-1);
	for (auto body : {_node.trueBody(), _node.falseBody()}) {
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
	for (auto body : {_node.trueBody(), _node.falseBody()}) {
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

bool StackOptimizer::visit(Contract &_node) {
	for (Pointer<Function>& f : _node.functions()) {
		switch (f->type()) {
			case Function::FunctionType::PrivateFunction:
			case Function::FunctionType::Macro:
			case Function::FunctionType::OnCodeUpgrade:
			case Function::FunctionType::OnTickTock: {
				// TODO remove the if
				for (int iter = 0; iter < TvmConst::IterStackOptQty; ++iter) {
					if (f->name() != "c7_to_c4_for_await") {
						//Printer p{std::cout};
						//f->accept(p);

						StackOptimizer opt;
						f->accept(opt);
					}
				}
				break;
			}

			case Function::FunctionType::MacroGetter:
			case Function::FunctionType::MainInternal:
			case Function::FunctionType::MainExternal:
				break;
		}
	}
	return false;
}

bool StackOptimizer::visit(Function &_node) {
	initStack(_node.take());

	_node.block()->accept(*this);

	solAssert(size() == _node.ret(), "");
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
	bool cmd1IsGen01 = isPureGen01OrGetGlob(*op);
	bool cmd1IsSWAP = isSWAP(op);
	std::optional<std::vector<Pointer<TvmAstNode>>> newCommands;

	if (isPOP(op)) {
		int startStackSize = isPOP(op).value();
		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize};
		if (sim.wasSet()) {
			vector<Pointer<TvmAstNode>> commands;
			commands.emplace_back(makeDROP());
			commands.insert(commands.end(), instructions.begin() + index + 1, instructions.end());
			newCommands = commands;
		} else if (sim.success()) {
			vector<Pointer<TvmAstNode>> commands;
			commands.emplace_back(makeDROP());
			commands.insert(commands.end(), instructions.begin() + index + 1, instructions.end());
			newCommands = commands;
		}
	} else if (cmd1IsPUSH || cmd1IsGen01 || cmd1IsSWAP) {
		int Si{};
		int startStackSize{};

		if (cmd1IsPUSH) {
			Si = stack->i();
			startStackSize = Si + 2;
		} else if (cmd1IsGen01) {
			startStackSize = 1;
		} else if (cmd1IsSWAP) {
			startStackSize = 2;
		} else {
			solUnimplemented("");
		}


		Simulator sim{instructions.begin() + index + 1, instructions.end(), startStackSize};
		if (sim.success()) {
			vector<Pointer<TvmAstNode>> commands;
			if (cmd1IsPUSH) {
				if (Si >= 1)
					commands.emplace_back(makeBLKSWAP(1, Si));
			} else if (cmd1IsGen01) {
			} else if (cmd1IsSWAP) {
				commands.emplace_back(makeDROP());
			} else {
				solUnimplemented("");
			}
			commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
			newCommands = commands;
		}
	} else if (!isDrop(op)) {
		bool isPrevFlag{};
		if (index > 0) {
			Pointer<TvmAstNode> prevOp = instructions.at(index - 1);
			isPrevFlag = to<DeclRetFlag>(prevOp.get()) != nullptr;
		}
		if (scopeSize() >= 1 && !isPrevFlag) {
			auto beg = instructions.begin() + index;
			Simulator sim{beg, instructions.end(), 1};
			if (sim.success()) {
				vector<Pointer<TvmAstNode>> commands{makeDROP()};
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
				if (qtyWithoutLoc(commands) <= qtyWithoutLoc(beg, instructions.end())) {
					newCommands = commands;
				}
			}
		}
	} else {
		int n = isDrop(op).value();
		auto beg = instructions.begin() + index + 1;
		if (beg != instructions.end() &&
			scopeSize() >= n + 1
		) {
			Simulator sim{beg, instructions.end(), 1};
			if (sim.success()) {
				vector<Pointer<TvmAstNode>> commands{makeDROP(n + 1)};
				commands.insert(commands.end(), sim.commands().begin(), sim.commands().end());
				if (qtyWithoutLoc(commands) <= qtyWithoutLoc(beg, instructions.end())) {
					newCommands = commands;
				}
			}
		}
	}


	if (!newCommands) {
		return false;
	}

	std::vector<Pointer<TvmAstNode>> cmds = newCommands.value();
	instructions.erase(instructions.begin() + index, instructions.end());
	instructions.insert(instructions.end(), cmds.begin(), cmds.end());
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
	// Some times 'scopeSize' maybe negative but it's ok because we move some variables.
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
