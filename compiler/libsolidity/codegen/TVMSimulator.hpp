/*
 * Copyright 2018-2019 TON DEV SOLUTIONS LTD.
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
 * Simulator of TVM execution
 */

#pragma once

#include "TvmAstVisitor.hpp"

//namespace solidity::frontend {
//	class Simulator : public TvmAstVisitor {
//		explicit Simulator(
//			std::vector<Pointer<TvmAstNode>>::iterator _beg,
//			std::vector<Pointer<TvmAstNode>>::iterator _end,
//			int _startSize
//		) :
//			m_beg{_beg},
//			m_end{_end},
//			m_startSize{_startSize}
//		{
//		}
//		bool visit(AsymGen &_node) override;
//		bool visit(Opaque &_node) override;
//		bool visit(HardCode &_node) override;
//		bool visit(Loc &_node) override;
//		bool visit(ConFlowInst &_node) override;
//		bool visit(GenOpcode &_node) override;
//		bool visit(PushCellOrSlice &_node) override;
//		bool visit(Glob &_node) override;
//		bool visit(Stack &_node) override;
//		bool visit(CodeBlock &_node) override;
//		bool visit(TvmIfElse &_node) override;
//		bool visit(RepeatOrUntil &_node) override;
//		bool visit(While &_node) override;
//		bool visit(Contract &_node) override;
//		bool visit(Function &_node) override;
//		void endVisit(CodeBlock &_node) override;
//	protected:
//		bool visitNode(TvmAstNode const&) override;
//		void endVisitNode(TvmAstNode const&) override;
//	private:
//		std::vector<Pointer<TvmAstNode>>::iterator m_beg{};
//		std::vector<Pointer<TvmAstNode>>::iterator m_end{};
//		bool m_success{};
//		int m_startSize{};
//		int m_curSize{};
//	};
//} // end solidity::frontend


