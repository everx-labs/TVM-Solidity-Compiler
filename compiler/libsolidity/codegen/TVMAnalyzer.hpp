/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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
 * @date 2019
 * AST analyzer specified to search for TVM specific issues.
 */

#pragma once

#include <libsolidity/ast/ASTVisitor.h>

#include <libsolidity/codegen/TVMCommons.hpp>

namespace solidity::langutil
{
class ErrorReporter;
}


namespace solidity::frontend
{

class TVMAnalyzer: private ASTConstVisitor
{
public:
	/// @param _errorReporter provides the error logging functionality.
	/// @param _structWarning provides the flag whether struct warning is needed.
	explicit TVMAnalyzer(langutil::ErrorReporter& _errorReporter, bool _structWarning = false);

	/// Performs analysis on the given source unit and all of its sub-nodes.
	/// @returns true if all checks passed. Note even if all checks passed, errors() can still contain warnings
	bool analyze(SourceUnit const& _sourceUnit);

private:
	bool visit(ContractDefinition const& contract) override;
	bool visit(Assignment const& _variable) override;
	bool visit(UnaryOperation const& _node) override;
	bool visit(FunctionDefinition const& _function) override;
	bool visit(Return const& _return) override;
	bool visit(FunctionCall const& _functionCall) override;
	bool visit(VariableDeclaration const& _variable) override;
	void endVisit(FunctionDefinition const&) override;

	langutil::ErrorReporter& m_errorReporter;

	/// Containers to find structs, that were created or loaded but not saved or used.
	/// TODO: fix this check for condition control flow (tests/test_state_var_not_saved_bad.sol)
	std::map<std::pair<size_t, VariableDeclaration const*>, ASTNode const *> m_structLoads;
	std::multimap<std::pair<size_t, VariableDeclaration const*>, ASTNode const *> m_changedStructs;
	std::vector<std::pair<size_t, VariableDeclaration const*>> m_declaredReturns;

	bool m_structWarning = false;
	FunctionDefinition const* m_currentFunction = nullptr;
};

class ContactsUsageScanner: public ASTConstVisitor
{
public:
	explicit ContactsUsageScanner(ContractDefinition const& cd);
	bool visit(FunctionCall const& _functionCall) override;
	bool visit(MemberAccess const &_node) override;
	bool visit(FunctionDefinition const& fd) override;

	bool hasMsgPubkey() const { return m_hasMsgPubkey; }
	bool hasMsgSender() const { return m_hasMsgSender; }
	bool hasResponsibleFunction() const { return m_hasResponsibleFunction; }
	bool hasAwaitCall() const { return m_hasAwaitCall; }
	bool hasTvmCode() const { return m_hasTvmCode; }
	std::set<FunctionDefinition const *> const& awaitFunctions() const { return m_awaitFunctions; }

private:
	bool m_hasMsgPubkey{};
	bool m_hasMsgSender{};
	bool m_hasResponsibleFunction{};
	bool m_hasAwaitCall{};
	bool m_hasTvmCode{};
	std::set<Declaration const*> m_usedFunctions;
	std::set<FunctionDefinition const*> m_awaitFunctions;
};

template <typename T>
static bool doesAlways(const Statement* st) {
	auto rec = [] (const Statement* s) {
		return doesAlways<T>(s);
	};
	if (to<T>(st))
		return true;

	if (
		to<Assignment>(st) ||
		to<Break>(st) ||
		to<Continue>(st) ||
		to<EmitStatement>(st) ||
		to<ExpressionStatement>(st) ||
		to<ForEachStatement>(st) ||
		to<ForStatement>(st) ||
		to<PlaceholderStatement>(st) ||
		to<Return>(st) ||
		to<VariableDeclarationStatement>(st) ||
		to<WhileStatement>(st)
	)
		return false;
	if (auto block = to<Block>(st)) {
		return std::any_of(block->statements().begin(), block->statements().end(), [&](const auto& s){
			return rec(s.get());
		});
	}
	if (auto ifStatement = to<IfStatement>(st)) {
		if (!ifStatement->falseStatement())
			return false;
		return rec(&ifStatement->trueStatement()) && rec(ifStatement->falseStatement());
	}
	solUnimplemented( std::string("Unsupported statement type: ") + typeid(*st).name());
}

class CFAnalyzer: public ASTConstVisitor
{
public:
	CFAnalyzer() = default;
	explicit CFAnalyzer(const Statement& node);

	bool doThatAlways() const {
		return m_alwaysReturns || m_alwaysBreak || m_alwaysContinue;
	}

	bool mayDoThat() const {
		return m_canReturn || m_canBreak || m_canContinue;
	}

private:
	bool startLoop();

protected:
	bool visit(ForEachStatement const&) override;
	bool visit(WhileStatement const&) override;
	bool visit(ForStatement const&) override;
	void endVisit(ForEachStatement const&) override;
	void endVisit(WhileStatement const&) override;
	void endVisit(ForStatement const&) override;
	void endVisit(Return const&) override;
	void endVisit(Break const&) override;
	void endVisit(Continue const&) override;

public:
	bool canReturn() const { return m_canReturn; }
	bool canBreak() const { return m_canBreak; }
	bool canContinue() const { return m_canContinue; }

private:
	int m_loopDepth{};
	bool m_canReturn{};
	bool m_canBreak{};
	bool m_canContinue{};
	bool m_alwaysReturns{};
	bool m_alwaysBreak{};
	bool m_alwaysContinue{};
};

} // end namespace solidity::frontend

solidity::frontend::LocationReturn notNeedsPushContWhenInlining(solidity::frontend::Block const &_block);

bool withPrelocatedRetValues(solidity::frontend::FunctionDefinition const* f);
