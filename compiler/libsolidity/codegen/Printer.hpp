/*
 * Copyright (C) 2021-2026 EverX. All Rights Reserved.
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
 * Printer for TVM Solidity abstract syntax tree.
 */

#pragma once

#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <optional>
#include <utility>

namespace solidity::frontend {

class Printer: public TvmAstVisitor {
public:
	explicit Printer(std::ostream& out, std::optional<std::string> func_name_to_debug = {}):
		m_out{out},
		m_func_name_to_debug{std::move(func_name_to_debug)} {}
	bool visit(AsymGen& _node) override;
	bool visit(DeclRetFlag& _node) override;
	bool visit(Opaque& _node) override;
	bool visit(HardCode& _node) override;
	bool visit(Loc& _node) override;
	bool visit(TvmReturn& _node) override;
	bool visit(ReturnOrBreakOrCont& _node) override;
	bool visit(TvmException& _node) override;
	bool visit(StackGen& _node) override;
	bool visit(CellOrSliceOperation& _node) override;
	bool visit(Glob& _node) override;
	bool visit(Stack& _node) override;
	bool visit(CodeBlock& _node) override;
	bool visit(SubProgram& _node) override;
	bool visit(LogCircuit& _node) override;
	bool visit(TvmIfElse& _node) override;
	bool visit(TvmRepeat& _node) override;
	bool visit(TvmUntil& _node) override;
	bool visit(TryCatch& _node) override;
	bool visit(While& _node) override;
	bool visit(Contract& _node) override;
	bool visit(Function& _node) override;

protected:
	bool visitNode(TvmAstNode const&) override;

private:
	void tabs() const;
	void printPushInt(std::string const& str, std::string const& comment = "") const;
	void printPushInt(int i) const;

	std::ostream& m_out;
	int m_tab{};
	std::optional<std::string> m_func_name_to_debug;
};

} // end solidity::frontend
