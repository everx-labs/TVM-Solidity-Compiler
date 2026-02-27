/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
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
 * AST analyzer specified to search for TVM specific issues.
 */

#pragma once

#include <libsolidity/ast/ASTVisitor.h>

#include <libsolidity/codegen/TVMCommons.hpp>

namespace solidity::langutil {
class ErrorReporter;
}


namespace solidity::frontend {

class TVMAnalyzer: public ASTConstVisitor {
public:
	/// @param _errorReporter provides the error logging functionality.
	explicit TVMAnalyzer(langutil::ErrorReporter& _errorReporter);

	/// Performs analysis on the given source unit and all of its sub-nodes.
	/// @returns true if all checks passed. Note even if all checks passed, errors() can still contain warnings
	bool analyze(SourceUnit const& _sourceUnit);

private:
	bool visit(RevertStatement const& _revertStatement) override;
	bool visit(MemberAccess const& _node) override;
	bool visit(FunctionDefinition const& _function) override;
	bool visit(Return const& _return) override;
	bool visit(FunctionCall const& _functionCall) override;
	void endVisit(FunctionDefinition const&) override;
	void endVisit(FunctionCall const&) override;
	void endVisit(PragmaDirective const& _pragma) override;

	langutil::ErrorReporter& m_errorReporter;

	FunctionDefinition const* m_currentFunction = nullptr;
	std::vector<FunctionCall const*> m_functionCall;
};

class TVMAnalyzerFlag128: ASTConstVisitor {
public:
	/// @param _errorReporter provides the error logging functionality.
	explicit TVMAnalyzerFlag128(langutil::ErrorReporter& _errorReporter);

	/// Performs analysis on the given source unit and all of its sub-nodes.
	/// @returns true if all checks passed. Note even if all checks passed, errors() can still contain warnings
	bool analyze(SourceUnit const& _sourceUnit);

private:
	bool visit(IfStatement const& _function) override;
	bool visit(FunctionDefinition const& _function) override;
	void endVisit(FunctionDefinition const& _function) override;

	bool visit(FunctionCall const& _functionCall) override;
	bool visit(EmitStatement const& _emit) override;

	langutil::ErrorReporter& m_errorReporter;
	FunctionDefinition const* m_function = nullptr;
	Expression const* m_functionCallWith128Flag = nullptr;
};

class TVMAnalyzerPackUnpack: ASTConstVisitor {
public:
	/// @param _errorReporter provides the error logging functionality.
	explicit TVMAnalyzerPackUnpack(langutil::ErrorReporter& _errorReporter);

	/// Performs analysis on the given source unit and all of its sub-nodes.
	/// @returns true if all checks passed. Note even if all checks passed, errors() can still contain warnings
	bool analyze(SourceUnit const& _sourceUnit);

private:
	bool visit(FunctionCall const& _functionCall) override;
	bool visit(Identifier const& _node) override;
	bool visit(VariableDeclaration const& _node) override;

	langutil::ErrorReporter& m_errorReporter;
};

class ContactsUsageScanner: public ASTConstVisitor {
public:
	explicit ContactsUsageScanner(ContractDefinition const& cd);
	bool visit(FunctionCall const& _functionCall) override;
	bool visit(MemberAccess const& _node) override;
	bool visit(FunctionDefinition const& fd) override;

	bool hasMsgPubkey() const { return m_hasMsgPubkey; }
	bool hasMsgSender() const { return m_hasMsgSender; }
	bool hasResponsibleFunction() const { return m_hasResponsibleFunction; }

private:
	bool m_hasMsgPubkey{};
	bool m_hasMsgSender{};
	bool m_hasResponsibleFunction{};
	std::set<Declaration const*> m_usedFunctions;
};

template <typename T>
static bool doesAlways(Statement const* st) {
	auto rec = [](Statement const* s) { return doesAlways<T>(s); };
	if (to<T>(st))
		return true;

	if (to<Assignment>(st) ||
		to<Break>(st) ||
		to<Continue>(st) ||
		to<EmitStatement>(st) ||
		to<ExpressionStatement>(st) ||
		to<ForEachStatement>(st) ||
		to<ForStatement>(st) ||
		to<PlaceholderStatement>(st) ||
		to<Return>(st) ||
		to<VariableDeclarationStatement>(st) ||
		to<WhileStatement>(st) ||
		to<TryStatement>(st) ||
		to<FreeInlineAssembly>(st))
		return false;
	if (auto block = to<Block>(st)) {
		return std::any_of(block->statements().begin(), block->statements().end(), [&](auto const& s) {
			return rec(s.get());
		});
	}
	if (auto ifStatement = to<IfStatement>(st)) {
		if (!ifStatement->falseStatement())
			return false;
		return rec(&ifStatement->trueStatement()) && rec(ifStatement->falseStatement());
	}
	solUnimplemented(std::string("Unsupported statement type: ") + typeid(*st).name());
}

class CFAnalyzer: public ASTConstVisitor {
public:
	explicit CFAnalyzer(Statement const& node);

	bool doThatAlways() const { return m_alwaysReturns || m_alwaysBreak || m_alwaysContinue; }

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
