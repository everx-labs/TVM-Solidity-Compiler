/*
 * Copyright (C) 2019-2023 EverX. All Rights Reserved.
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

class TVMFunctionCompiler: public ASTConstVisitor, private boost::noncopyable
{
protected:
	TVMFunctionCompiler(StackPusher& pusher, ContractDefinition const *contract);
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
	static Pointer<Function> generateDefaultC4(TVMCompilerContext& ctx);
	static Pointer<Function> generateBuildTuple(TVMCompilerContext& ctx, std::string const& name, const std::vector<Type const*>& types);
	static Pointer<Function> generateNewArrays(TVMCompilerContext& ctx, std::string const& name, FunctionCall const* arr);
	static Pointer<Function> generateConstArrays(TVMCompilerContext& ctx, std::string const& name, TupleExpression const* arr);
	static Pointer<Function> generateFunction(TVMCompilerContext& ctx, FunctionDefinition const* function, std::string const& name);
	static Pointer<Function> generateMainExternal(TVMCompilerContext& ctx, ContractDefinition const *contract);
	static Pointer<Function> generateMainInternal(TVMCompilerContext& ctx, ContractDefinition const *contract);
	static Pointer<Function> generateCheckResume(TVMCompilerContext& ctx);
	static Pointer<Function> generateOnCodeUpgrade(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateOnTickTock(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateLibFunctionWithObject(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateReceive(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateFallback(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generateOnBounce(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static Pointer<Function> generatePublicFunction(TVMCompilerContext& ctx, FunctionDefinition const* function);
	static void generateFunctionWithModifiers(StackPusher& pusher, FunctionDefinition const* function, bool pushArgs);
	static Pointer<Function> generateGetter(StackPusher& pusher, VariableDeclaration const* vd);
	static Pointer<Function> generatePublicFunctionSelector(TVMCompilerContext& pusher, ContractDefinition const *contract);
	void decodeFunctionParamsAndInitVars(bool hasCallback);

protected:
	static Pointer<Function> generateReceiveOrFallbackOrOnBounce(
		TVMCompilerContext& ctx,
		FunctionDefinition const* function,
		const std::string& name,
		int take
	);
	ast_vec<ModifierInvocation> functionModifiers();
	void endContinuation2(bool doDrop);

	bool hasLoop() const;
	std::optional<ControlFlowInfo> lastAnalyzeFlag() const;
	std::optional<ControlFlowInfo> lastLoop() const;
	bool lastAnalyzerBeforeLoop() const;

	void emitOnPublicFunctionReturn();
	void pushDefaultParameters(const ast_vec<VariableDeclaration>& returnParameters);

	void acceptExpr(const Expression* expr, bool isResultNeeded = true);

	void visitModifierOrFunctionBlock(Block const& body, int argQty, int retQty, int nameRetQty);
public:
	void visitFunctionWithModifiers();
private:
	void visitForOrWhileCondition(const std::function<void()>& pushCondition);
	void afterLoopCheck(const std::unique_ptr<CFAnalyzer>& ci, const int& loopVarQty, bool _doAnalyzeFlag);
	ControlFlowInfo beforeTryOrIfCheck(CFAnalyzer const& ci);
	void afterTryOrIfCheck(ControlFlowInfo const& info);
	bool visitNode(ASTNode const&) override { solUnimplemented("Internal error: unreachable"); }

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override;
	void acceptBody(Block const& _block, std::optional<std::tuple<int, int>> functionBlock);
	bool visit(Block const& _block) override;
	bool visit(ExpressionStatement const& _expressionStatement) override;
    bool visit(TryStatement const& _block) override;
	bool visit(IfStatement const& _ifStatement) override;
	bool visit(WhileStatement const& _whileStatement) override;
	bool visit(ForEachStatement const& _forStatement) override;
	std::pair<std::unique_ptr<CFAnalyzer>, ControlFlowInfo> pushControlFlowFlag(Statement const& body);
	void visitBodyOfForLoop(
		const std::unique_ptr<CFAnalyzer>&	 ci,
		const std::function<void()>& pushStartBody,
		Statement const& body,
		const std::function<void()>& loopExpression
	);
	bool visit(ForStatement const& _forStatement) override;
	bool visit(Return const& _return) override;
	bool visit(Break const&) override;
	bool visit(Continue const&) override;
	bool visit(EmitStatement const& _emit) override;
	bool visit(PlaceholderStatement const& /*_node*/) override;

	void doWhile(WhileStatement const& _whileStatement);

	void setGlobSenderAddressIfNeed();
	void setCtorFlag();
	void setCopyleftAndTryCatch();
	Pointer<Function> generateMainExternalForAbiV2();

	void pushMsgPubkey();
	void checkSignatureAndReadPublicKey();
	void defaultReplayProtection();
	void expire();
	void callPublicFunctionOrFallback();
	void pushC4ToC7IfNeed();
	void updC4IfItNeeds();
	void pushReceiveOrFallback();

	void buildPublicFunctionSelector(const std::vector<std::pair<uint32_t, std::string>>& functions, int left, int right,
									 PublicFunctionSelector const& pfs);
    void pushLocation(const ASTNode& node, bool reset = false);

private:
	StackPusher& m_pusher;
	std::vector<ControlFlowInfo> m_controlFlowInfo;

	const int m_startStackSize{};
	const int m_currentModifier{};
	FunctionDefinition const* m_function{};
	ContractDefinition const *m_contract{};
	const bool m_isLibraryWithObj{};
	const bool m_pushArgs{};
};

class TVMConstructorCompiler: public TVMFunctionCompiler {
	StackPusher& m_pusher;
	std::map<ContractDefinition const*, std::vector<ContractDefinition const*>> path;
	std::vector<ContractDefinition const*> dfsOrder;
	std::map<ContractDefinition const*, bool> used;
	std::map<ContractDefinition const*, std::vector<ASTPointer<Expression>> const*> m_args;

public:
	explicit TVMConstructorCompiler(StackPusher& pusher);
	void dfs(ContractDefinition const* c);
	Pointer<Function> generateConstructors();
private:
	void beginConstructor();
};

class PublicFunctionSelector {
public:
	explicit PublicFunctionSelector(int n);
	std::vector<int> const& groupSizes(int n) const { return prev.at(n); }
private:
	void dfs(int pos, int n);
private:
	std::vector<int> curGroupSize;
	const int INF = 1e9;
	const int OK_JMP = 18 + 23 + 18 + 126;  // DUP / PUSHINT ? / LEQ / IFJMPREF
	const int FAIL_JMP = 18 + 23 + 18 + 26; // DUP / PUSHINT ? / LEQ / IFJMPREF
	std::vector<int> maxPath;
	std::vector<int> sumPaths;
	std::vector<std::vector<int>> prev;
};

} // end solidity::frontend
