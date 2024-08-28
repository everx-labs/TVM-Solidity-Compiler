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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#pragma once

#include <optional>
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
	virtual bool visit(DeclRetFlag &_node) { return visitNode(_node); }
	virtual bool visit(Opaque &_node) { return visitNode(_node); }
	virtual bool visit(HardCode &_node) { return visitNode(_node); }
	virtual bool visit(Loc &_node) { return visitNode(_node); }
	virtual bool visit(TvmReturn &_node) { return visitNode(_node); }
	virtual bool visit(ReturnOrBreakOrCont &_node) { return visitNode(_node); }
	virtual bool visit(TvmException &_node) { return visitNode(_node); }
	virtual bool visit(StackOpcode &_node) { return visitNode(_node); }
	virtual bool visit(PushCellOrSlice &_node) { return visitNode(_node); }
	virtual bool visit(Glob &_node) { return visitNode(_node); }
	virtual bool visit(Stack &_node) { return visitNode(_node); }
	virtual bool visit(CodeBlock &_node) { return visitNode(_node); }
	virtual bool visit(SubProgram &_node) { return visitNode(_node); }
	virtual bool visit(LogCircuit &_node) { return visitNode(_node); }
	virtual bool visit(TvmIfElse &_node) { return visitNode(_node); }
	virtual bool visit(TvmRepeat &_node) { return visitNode(_node); }
	virtual bool visit(TvmUntil &_node) { return visitNode(_node); }
	virtual bool visit(While &_node) { return visitNode(_node); }
	virtual bool visit(TryCatch &_node) { return visitNode(_node); }
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
	bool visit(Contract &_node) override;
	bool visit(Function &_node) override;
protected:
	bool visitNode(TvmAstNode const&) override;
private:
	void tabs();
	void printPushInt(std::string const& arg, std::string const& comment = "");
	void printPushInt(int i);
private:
	std::ostream& m_out;
	int m_tab{};
};

class LocSquasher : public TvmAstVisitor {
public:
	bool visit(CodeBlock &_node) override;
};

class DeleterAfterRet : public TvmAstVisitor {
public:
	void endVisit(CodeBlock &_node) override;
};

class DeleterCallX : public TvmAstVisitor {
public:
	bool visit(Function &_node) override;
};

class LogCircuitExpander : public TvmAstVisitor {
public:
	void endVisit(CodeBlock &_node) override;
private:
	bool isPureOperation(Pointer<TvmAstNode> const& op);
private:
	int m_stackSize{};
	std::vector<Pointer<TvmAstNode>> m_newInst;
};

}	// end solidity::frontend
