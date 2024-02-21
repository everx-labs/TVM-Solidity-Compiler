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

#pragma once

#include <libsolidity/codegen/TvmAstVisitor.hpp>

namespace solidity::frontend {
class StackOptimizer : public TvmAstVisitor {
public:
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
	bool visit(TvmIfElse &_node) override;
	bool visit(TvmRepeat &_node) override;
	bool visit(TvmUntil &_node) override;
	bool visit(TryCatch &_node) override;
	bool visit(While &_node) override;
	bool visit(Function &_node) override;
	bool visit(Contract &_node) override;
	void endVisit(CodeBlock &_node) override;
protected:
	bool visitNode(TvmAstNode const&) override;
	void endVisitNode(TvmAstNode const&) override;
private:
	bool successfullyUpdate(int index, std::vector<Pointer<TvmAstNode>>& instructions);
	void initStack(int size);
	void delta(int delta);
	int size();
	int scopeSize();
	void startScope();
	void endScope();
private:
	bool m_didSome{};
	std::vector<int> m_stackSize;
};
} // end solidity::frontend

