/*
 * Copyright (C) 2019-2026 EverX. All Rights Reserved.
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
 * AST to TVM bytecode contract compiler
 */

#pragma once

#include <libsolidity/ast/Types.h>

namespace solidity::frontend {

class PublicFunctionSelector;

class TVMFunctionCompiler: public ASTConstVisitor, boost::noncopyable {
protected:
	TVMFunctionCompiler(StackPusher& pusher, ContractDefinition const* contract);

private:
	TVMFunctionCompiler(
		StackPusher& pusher,
		int modifier,
		FunctionDefinition const* f,
		bool isLibraryWithObj,
		bool pushArgs,
		int startStackSize
	);

public:
	static Pointer<Function> updateOnlyTime(TVMCompilerContext& ctx);
	static Pointer<Function> generateC4ToC7(TVMCompilerContext& ctx);
	static Pointer<Function> generateC7ToC4(TVMCompilerContext& ctx);
	static Pointer<Function> generateDefaultC4(TVMCompilerContext& ctx);
	static Pointer<Function>
	generateBuildTuple(TVMCompilerContext& ctx, std::string const& name, std::vector<Type const*> const& types);
	static Pointer<Function>
	generateNewArrays(TVMCompilerContext& ctx, std::string const& name, FunctionCall const* arr);
	static Pointer<Function>
	generateConstArrays(TVMCompilerContext& ctx, std::string const& name, TupleExpression const* arr);
	static Pointer<Function>
	generateFunction(TVMCompilerContext& ctx, FunctionDefinition const* function, std::string const& name, uint32_t id);
	static Pointer<Function> generateMainExternal(TVMCompilerContext& ctx, ContractDefinition const* contract);
	static Pointer<Function> generateMainInternal(TVMCompilerContext& ctx, ContractDefinition const* contract);
	static Pointer<Function> generateOnCodeUpgrade(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateOnTickTock(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function>
	generateLibFunctionWithObject(TVMCompilerContext& ctx, FunctionDefinition const* function, std::string const& name);
	static Pointer<Function> generateReceive(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateFallback(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateOnBouncedMessage(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generatePublicFunction(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateGetterFunction(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static void generateFunctionWithModifiers(StackPusher& pusher, FunctionDefinition const* function, bool pushArgs);
	void generatePublicFunctionSelector(bool isExternal) const;
	void decodeFunctionParamsAndInitVars(bool isResponsible) const;

protected:
	static Pointer<Function> generateReceiveOrFallbackOrOnBouncedMessage(
		TVMCompilerContext& ctx,
		FunctionDefinition const* function,
		std::string const& name,
		int take
	);
	ast_vec<ModifierInvocation> functionModifiers() const;
	void endContinuation2(bool doDrop) const;

	bool hasLoop() const;
	std::optional<ControlFlowInfo> lastAnalyzeFlag() const;
	std::optional<ControlFlowInfo> lastLoop() const;
	bool lastAnalyzerBeforeLoop() const;

	void emitOnPublicFunctionReturn() const;
	void pushDefaultParameters(ast_vec<VariableDeclaration> const& returnParameters) const;

	void acceptExpr(Expression const* expr, bool isResultNeeded = true) const;

	void visitModifierOrFunctionBlock(Block const& body, int argQty, int nameRetQty);

public:
	void visitFunctionWithModifiers() const;

private:
	void visitForOrWhileCondition(std::function<void()> const& pushCondition) const;
	void afterLoopCheck(std::unique_ptr<CFAnalyzer> const& ci, int const& loopVarQty, bool _doAnalyzeFlag) const;
	ControlFlowInfo beforeTryOrIfCheck(CFAnalyzer const& ci) const;
	void afterTryOrIfCheck(ControlFlowInfo const& info);
	bool visitNode(ASTNode const&) override { solUnimplemented("Internal error: unreachable"); }

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override;
	void acceptBody(Block const& _block, std::optional<std::tuple<int, int>> functionBlock);
	bool visit(Block const& _block) override;
	bool visit(ExpressionStatement const& _statement) override;
	bool visit(TryStatement const& _tryState) override;
	bool visit(IfStatement const& _ifStatement) override;
	bool visit(WhileStatement const& _whileStatement) override;
	bool visit(ForEachStatement const& _forStatement) override;
	std::pair<std::unique_ptr<CFAnalyzer>, ControlFlowInfo> pushControlFlowFlag(Statement const& body);
	void visitBodyOfForLoop(
		std::unique_ptr<CFAnalyzer> const& ci,
		std::function<void()> const& pushStartBody,
		Statement const& body,
		std::function<void()> const& loopExpression
	);
	bool visit(ForStatement const& _forStatement) override;
	bool visit(Return const& _return) override;
	bool visit(Break const&) override;
	bool visit(Continue const&) override;
	bool visit(EmitStatement const& _emit) override;
	bool visit(PlaceholderStatement const& /*_node*/) override;

	void doWhile(WhileStatement const& _whileStatement);

	void setCtorFlag() const;
	void setCopyleft() const;

	void pushMsgPubkey() const;
	void checkSignatureAndReadPublicKey() const;
	void updC4IfItNeeds() const;
	void pushReceiveOrFallbackAndLoadFuncId() const;

	void buildPublicFunctionSelector(
		std::vector<std::pair<uint32_t, std::string>> const& functions,
		int left,
		int right,
		PublicFunctionSelector const& pfs
	);
	void pushLocation(ASTNode const& node, bool reset = false) const;

	StackPusher& m_pusher;
	std::vector<ControlFlowInfo> m_controlFlowInfo;

	int const m_startStackSize{};
	int const m_currentModifier{};
	FunctionDefinition const* m_function{};
	ContractDefinition const* m_contract{};
	bool const m_isLibraryWithObj{};
	bool const m_pushArgs{};
};

class TVMConstructorCompiler: public TVMFunctionCompiler {
public:
	explicit TVMConstructorCompiler(StackPusher& pusher);
	void dfs(ContractDefinition const* c);
	Pointer<Function> generateConstructors();

private:
	StackPusher& m_pusher;
	std::map<ContractDefinition const*, std::vector<ContractDefinition const*>> path;
	std::vector<ContractDefinition const*> dfsOrder;
	std::map<ContractDefinition const*, bool> used;
	std::map<ContractDefinition const*, std::vector<ASTPointer<Expression>> const*> m_args;
};

class PublicFunctionSelector {
public:
	explicit PublicFunctionSelector(int _n);
	std::vector<int> const& groupSizes(int n) const { return prev.at(n); }

private:
	void dfs(int pos, int n);

	std::vector<int> curGroupSize;
	int const INF = 1e9;
	int const OK_JMP = 18 + 23 + 18 + 126;	// DUP / PUSHINT ? / LEQ / IFJMPREF
	int const FAIL_JMP = 18 + 23 + 18 + 26; // DUP / PUSHINT ? / LEQ / IFJMPREF
	std::vector<int> maxPath;
	std::vector<int> sumPaths;
	std::vector<std::vector<int>> prev;
};

} // end solidity::frontend
