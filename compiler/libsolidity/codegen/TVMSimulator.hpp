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

#pragma once

#include "TvmAstVisitor.hpp"

namespace solidity::frontend {
class Simulator : public TvmAstVisitor {
public:
	explicit Simulator(
		std::vector<Pointer<TvmAstNode>>::const_iterator _beg,
		std::vector<Pointer<TvmAstNode>>::const_iterator _end,
		int _stackSize,
		int _segment,
		bool _stopSimulationIfSet = false,
		bool _stopSimulationIfMoved = false
	) :
		m_stopSimulationIfSet{_stopSimulationIfSet},
		m_stopSimulationIfMoved{_stopSimulationIfMoved},
		m_segment{_segment},
		m_stackSize{_stackSize}
	{
		solAssert(_segment <= _stackSize, "");
		run(_beg, _end);
	}
private:
	void run(std::vector<Pointer<TvmAstNode>>::const_iterator _beg,
			 std::vector<Pointer<TvmAstNode>>::const_iterator _end);
public:
	bool visit(AsymGen &_node) override;
	bool visit(DeclRetFlag &_node) override;
	bool visit(Opaque &_node) override;
	bool visit(HardCode &_node) override;
	bool visit(Loc &_node) override;
	bool visit(TvmReturn &_node) override;
	bool visit(ReturnOrBreakOrCont &_node) override;
	bool visit(TvmException &_node) override;
	bool visit(StackOpcode &_node) override;
	bool visit(PushCellOrSlice &_node) override;
	bool visit(Glob &_node) override;
	bool visit(Stack &_node) override;
	bool visit(CodeBlock &_node) override;
	bool visit(SubProgram &_node) override;
	bool visit(LogCircuit &_node) override;
	bool visit(TryCatch &_node) override;
	bool visit(TvmIfElse &_node) override;
	bool visit(TvmRepeat &_node) override;
	bool visit(TvmUntil &_node) override;
	bool visit(While &_node) override;
	bool visit(Contract &_node) override;
	bool visit(Function &_node) override;
	void endVisit(CodeBlock &_node) override;

	std::optional<std::pair<Pointer<CodeBlock>, bool>> trySimulate(CodeBlock const& block, int begStackSize, int endStackSize);
	bool isPopAndDrop(Pointer<TvmAstNode> const& a, Pointer<TvmAstNode> const& b);

	bool success() const;
	bool wasSet() const { return m_wasSet; }
	bool wasMoved() const { return m_wasMoved; }
	std::vector<Pointer<TvmAstNode>> const& commands() const { return m_commands; }
	std::vector<Pointer<TvmAstNode>>::const_iterator getIter() const { return m_iter; }
	std::set<int> setGlobIndexes() const { return m_setGlobs; }
	bool wasCall() const { return m_wasCall; }

protected:
	bool visitNode(TvmAstNode const&) override;
	void endVisitNode(TvmAstNode const&) override;

private:
	int rest() const;
	void unableToConvertOpcode() {
		m_ableToConvertOpcode = false;
	}

private:
	bool m_wasSet{};
	bool m_stopSimulationIfSet{};
	bool m_stopSimulationIfMoved{};
	bool m_wasMoved{};
	bool m_ableToConvertOpcode = true;
	bool m_isDropped{};
	bool m_wasReturnBreakContinue{};

	int const m_segment;
	int m_stackSize{};
	std::vector<Pointer<TvmAstNode>> m_commands;
	std::vector<Pointer<TvmAstNode>>::const_iterator m_iter{};
	std::set<int> m_setGlobs;
	bool m_wasCall{};
};
} // end solidity::frontend


