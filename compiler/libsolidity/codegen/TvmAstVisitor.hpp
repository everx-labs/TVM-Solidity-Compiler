/*
 * Copyright (C) 2021-2025 EverX. All Rights Reserved.
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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#pragma once

#include <libsolidity/codegen/TvmAst.hpp>

namespace solidity::frontend {
class TvmAstVisitor {
public:
	virtual ~TvmAstVisitor() = default;
	virtual bool visit(AsymGen& _node) { return visitNode(_node); }
	virtual bool visit(DeclRetFlag& _node) { return visitNode(_node); }
	virtual bool visit(Opaque& _node) { return visitNode(_node); }
	virtual bool visit(HardCode& _node) { return visitNode(_node); }
	virtual bool visit(Loc& _node) { return visitNode(_node); }
	virtual bool visit(TvmReturn& _node) { return visitNode(_node); }
	virtual bool visit(ReturnOrBreakOrCont& _node) { return visitNode(_node); }
	virtual bool visit(TvmException& _node) { return visitNode(_node); }
	virtual bool visit(StackGen& _node) { return visitNode(_node); }
	virtual bool visit(CellOrSliceOperation& _node) { return visitNode(_node); }
	virtual bool visit(Glob& _node) { return visitNode(_node); }
	virtual bool visit(Stack& _node) { return visitNode(_node); }
	virtual bool visit(CodeBlock& _node) { return visitNode(_node); }
	virtual bool visit(SubProgram& _node) { return visitNode(_node); }
	virtual bool visit(LogCircuit& _node) { return visitNode(_node); }
	virtual bool visit(TvmIfElse& _node) { return visitNode(_node); }
	virtual bool visit(TvmRepeat& _node) { return visitNode(_node); }
	virtual bool visit(TvmUntil& _node) { return visitNode(_node); }
	virtual bool visit(While& _node) { return visitNode(_node); }
	virtual bool visit(TryCatch& _node) { return visitNode(_node); }
	virtual bool visit(Contract& _node) { return visitNode(_node); }
	virtual bool visit(Function& _node) { return visitNode(_node); }

	virtual void endVisit(CodeBlock& _node) { endVisitNode(_node); }

protected:
	virtual bool visitNode(TvmAstNode const&) { return true; }
	virtual void endVisitNode(TvmAstNode const&) {}
};

} // end solidity::frontend
