/*
 * Copyright (C) 2025 EverX. All Rights Reserved.
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

#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <libsolidity/codegen/optimizers/MiscOptimizer.hpp>


using namespace solidity::util;
using namespace solidity::frontend;


bool LocSquasher::visit(CodeBlock& _node) {
	std::vector<Pointer<TvmAstNode>> res0;

	{
		std::vector<Pointer<TvmAstNode>> const& a = _node.instructions();
		std::vector<Pointer<TvmAstNode>> b;
		if (!a.empty()) {
			b.push_back(a.front());
			for (size_t i = 1; i < a.size(); ++i) {
				if (!b.empty() && convertToLoc(b.back().get()) && convertToLoc(a[i].get()))
					b.pop_back();
				b.push_back(a[i]);
			}
		}
		res0 = b;
	}


	std::vector<Pointer<TvmAstNode>> res;
	std::optional<Loc const*> lastLoc;
	for (Pointer<TvmAstNode> const& node: res0) {
		auto loc = convertToLoc(node.get());
		if (loc) {
			if (!lastLoc || std::make_pair(lastLoc.value()->file(), lastLoc.value()->line()) !=
								std::make_pair(loc->file(), loc->line())) {
				res.push_back(node);
			}
			//
			lastLoc = loc;
		} else {
			res.push_back(node);
		}
	}


	_node.changeInstructions(res);

	return true;
}

void DeleterAfterRet::endVisit(CodeBlock& _node) {
	bool didFind{};
	std::vector<Pointer<TvmAstNode>> newInstrs;
	for (Pointer<TvmAstNode> const& opcode: _node.instructions()) {
		auto ret = convertToReturnOrBreakOrCont(opcode.get());
		auto ifElse = convertToTvmIfElse(opcode.get());
		bool ifElseWithJmp = ifElse && ifElse->falseBody() != nullptr && ifElse->withJmp();
		auto _throw = convertToTvmException(opcode.get());
		bool th = _throw && !_throw->withIf();
		if (!didFind && (ret || ifElseWithJmp || th)) {
			didFind = true;
			newInstrs.emplace_back(opcode);
		} else {
			if (!didFind || convertToLoc(opcode.get())) {
				newInstrs.emplace_back(opcode);
			}
		}
	}
	_node.changeInstructions(newInstrs);
}

void LogCircuitExpander::endVisit(CodeBlock& _node) {
	std::vector<Pointer<TvmAstNode>> block;
	for (Pointer<TvmAstNode> const& opcode: _node.instructions()) {
		auto lc = convertToLogCircuit(opcode.get());
		if (lc) {
			m_stackSize = 1;
			m_newInst = {};
			bool isPure = true;
			std::vector<Pointer<TvmAstNode>> const& inst = lc->body()->instructions();
			for (size_t i = 0; i < inst.size(); ++i) {
				Pointer<TvmAstNode> op = inst.at(i);
				if (i == 0) {
					solAssert(isDrop(inst.at(i)).value() == 1, "");
					continue;
				}
				if (convertToLogCircuit(op.get()) && i + 1 != inst.size()) {
					isPure = false; // never happens
				}

				isPure &= isPureOperation(op);
			}
			if (isPure) {
				solAssert(m_stackSize == 2, "");
				Pointer<TvmAstNode> tail = m_newInst.back();
				bool hasTailLogCircuit = !m_newInst.empty() && convertToLogCircuit(m_newInst.back().get());
				if (hasTailLogCircuit) {
					if (convertToLogCircuit(m_newInst.back().get())->type() != lc->type()) {
						block.emplace_back(opcode);
						continue;
					}

					m_newInst.pop_back(); // DUP
					m_newInst.pop_back(); // LogCircuit
				}
				switch (lc->type()) {
				case LogCircuit::Type::AND:
					m_newInst.emplace_back(gen("AND"));
					break;
				case LogCircuit::Type::OR:
					m_newInst.emplace_back(gen("OR"));
					break;
				}
				if (hasTailLogCircuit) {
					m_newInst.emplace_back(makePUSH(0)); // DUP
					m_newInst.emplace_back(tail);		 // LogCircuit
				}

				block.pop_back(); // remove DUP opcode
				block.insert(block.end(), m_newInst.begin(), m_newInst.end());
				continue;
			}
		}
		block.emplace_back(opcode);
	}
	_node.changeInstructions(block);
}

bool LogCircuitExpander::isPureOperation(Pointer<TvmAstNode> const& op) {
	auto gen = convertToGen(op.get());
	if (gen && gen->isPure()) {
		m_newInst.emplace_back(op);
		m_stackSize += -gen->take() + gen->ret();
		return true;
	}

	if (convertToLogCircuit(op.get())) {
		m_newInst.emplace_back(op);
		m_stackSize += -2 + 1;
		return true;
	}

	if (auto push = isPUSH(op)) {
		int index = *push;
		if (index + 1 < m_stackSize) {
			m_newInst.emplace_back(makePUSH(index));
		} else {
			m_newInst.emplace_back(makePUSH(index + 1));
		}
		++m_stackSize;
		return true;
	}

	return false;
}
