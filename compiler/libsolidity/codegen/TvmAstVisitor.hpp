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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#pragma once

#include <memory>
#include <vector>

#include <boost/noncopyable.hpp>

#include <libsolidity/codegen/TvmAst.hpp>

namespace solidity::frontend
{
	class TvmAstVisitor {
	public:
		virtual ~TvmAstVisitor() = default;
		virtual bool visit(AsymGen &_node) { return visitNode(_node); }
		virtual bool visit(Opaque &_node) { return visitNode(_node); }
		virtual bool visit(HardCode &_node) { return visitNode(_node); }
		virtual bool visit(Loc &_node) { return visitNode(_node); }
		virtual bool visit(ConFlowInst &_node) { return visitNode(_node); }
		virtual bool visit(GenOpcode &_node) { return visitNode(_node); }
		virtual bool visit(PushCellOrSlice &_node) { return visitNode(_node); }
		virtual bool visit(Glob &_node) { return visitNode(_node); }
		virtual bool visit(Stack &_node) { return visitNode(_node); }
		virtual bool visit(CodeBlock &_node) { return visitNode(_node); }
		virtual bool visit(TvmIfElse &_node) { return visitNode(_node); }
		virtual bool visit(RepeatOrUntil &_node) { return visitNode(_node); }
		virtual bool visit(While &_node) { return visitNode(_node); }
		virtual bool visit(Contract &_node) { return visitNode(_node); }
		virtual bool visit(Function &_node) { return visitNode(_node); }

		virtual void endVisit(CodeBlock &_node) { endVisitNode(_node); }
	protected:
		virtual bool visitNode(TvmAstNode const&) { return true; }
		virtual void endVisitNode(TvmAstNode const&) { }
	};

	class Printer : public TvmAstVisitor {
	public:
		explicit Printer(std::ostream& out) : m_out{out} { }
		bool visit(AsymGen &_node) override;
		bool visit(Opaque &_node) override;
		bool visit(HardCode &_node) override;
		bool visit(Loc &_node) override;
		bool visit(ConFlowInst &_node) override;
		bool visit(GenOpcode &_node) override;
		bool visit(PushCellOrSlice &_node) override;
		bool visit(Glob &_node) override;
		bool visit(Stack &_node) override;
		bool visit(CodeBlock &_node) override;
		bool visit(TvmIfElse &_node) override;
		bool visit(RepeatOrUntil &_node) override;
		bool visit(While &_node) override;
		bool visit(Contract &_node) override;
		bool visit(Function &_node) override;
	protected:
		bool visitNode(TvmAstNode const&) override;
	private:
		void endL();
		void tabs();
	private:
		std::ostream& m_out;
		int m_tab{};
	};

	class LocSquasher : public TvmAstVisitor {
	public:
		bool visit(CodeBlock &_node) override;
	};
}	// end solidity::frontend
