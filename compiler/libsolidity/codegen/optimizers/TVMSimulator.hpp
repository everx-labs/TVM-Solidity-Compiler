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
 * Simulator of TVM execution
 */

#pragma once

#include <libsolidity/codegen/TvmAstVisitor.hpp>

namespace solidity::frontend {
class Simulator: public TvmAstVisitor {
public:
	enum class Type {
		// Drop the value in the current and other nested scopes
		DROP_VALUE,
		DROP_VALUE_IN_RBC,
		// Set the value in the current scope
		SET_VALUE,
		// Move the value in the current scope
		MOVE_VALUE,
	};
	Simulator(
		std::vector<Pointer<TvmAstNode>>::const_iterator _beg,
		std::vector<Pointer<TvmAstNode>>::const_iterator _end,
		int _stackSize,
		int _segment,
		Type _type,
		bool is_nested_scope,
		std::function<
			void(std::vector<Pointer<TvmAstNode>>&, std::vector<Pointer<TvmAstNode>>::const_iterator& iter)> const&
			onSuccess
	);

private:
	void run(std::vector<Pointer<TvmAstNode>>::const_iterator _beg);

public:
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
	bool visit(While& _node) override;
	bool visit(Contract& _node) override;
	bool visit(Function& _node) override;
	void endVisit(CodeBlock& _node) override;

	struct Result {
		Pointer<CodeBlock> block;
		int stackSize;
		bool success;
	};
	std::optional<Result> simulateBlock(CodeBlock const& body, bool bodyIsNested = true);

	bool success() const;
	bool wasFunctionCall() const { return m_wasFunctionCall; }
	bool alwaysDroppedFromRBC() const { return m_alwaysDroppedFromRBC; }
	std::set<int> setGlobIndexes() const { return m_setGlobs; }
	std::vector<Pointer<TvmAstNode>> const& newCommands() const { return m_newCommands; }
	std::vector<Pointer<TvmAstNode>>::const_iterator curCmdIter() const { return m_curCmd; }

protected:
	bool visitNode(TvmAstNode const&) override;

private:
	int rest() const;
	void setUnableToConvertOpcode() { m_ableToConvertOpcode = false; }

private:
	int const SEGMENT;
	Type const TYPE;
	bool const IS_NESTED_SCOPE;

	int m_stackSize = 0;

	bool m_ableToConvertOpcode = true;
	bool m_success = false;
	bool m_wasFunctionCall = false;
	bool m_alwaysDroppedFromRBC = true;
	std::set<int> m_setGlobs;

	std::vector<Pointer<TvmAstNode>> m_newCommands;
	std::vector<Pointer<TvmAstNode>>::const_iterator m_curCmd{};
	std::vector<Pointer<TvmAstNode>>::const_iterator const END_CMD{};
	std::function<
		void(std::vector<Pointer<TvmAstNode>>&, std::vector<Pointer<TvmAstNode>>::const_iterator& iter)> const&
		m_onSuccess;
};

} // end solidity::frontend
