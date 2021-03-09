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
 * @date 2019
 * AST analyzer specified to search for TVM specific issues.
 */

#pragma once

#include <libsolidity/ast/ASTVisitor.h>

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

	bool haveMsgPubkey{};
	bool haveMsgSender{};
	bool haveResponsibleFunction{};
};

class FunctionUsageScanner: public ASTConstVisitor
{
public:
	explicit FunctionUsageScanner(const ASTNode& node);
	bool visit(FunctionCall const& _functionCall) override;
	bool havePrivateFunctionCall{};
};

}
