/*
 * Copyright 2018-2021 TON DEV SOLUTIONS LTD.
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
 * AST to TVM bytecode contract compiler
 */

#pragma once

#include <libsolidity/ast/Types.h>

namespace solidity::frontend {



class TVMFunctionCompiler: public ASTConstVisitor, private boost::noncopyable
{
private:
	TVMFunctionCompiler(StackPusher& pusher, ContractDefinition const *contract);

	TVMFunctionCompiler(
		StackPusher& pusher,
		int modifier,
		FunctionDefinition const* f,
		bool isLibraryWithObj,
		bool pushArgs,
		int startStackSize
	);

public:
	static Pointer<Function> generateC4ToC7(StackPusher& pusher);
	static Pointer<Function> generateC4ToC7WithInitMemory(StackPusher& pusher);
	[[nodiscard]]
	static Pointer<Function> generateMacro(StackPusher& pusher, FunctionDefinition const* function, const std::optional<std::string>& forceName = std::nullopt);
	static Pointer<Function> generateMainExternal(StackPusher& pusher, ContractDefinition const *contract);
	static Pointer<Function> generateMainInternal(StackPusher& pusher, ContractDefinition const *contract);
	static Pointer<Function> generateCheckResume(StackPusher& pusher);
	static Pointer<Function> generateOnCodeUpgrade(StackPusher& pusher, FunctionDefinition const* function);
	static Pointer<Function> generateOnTickTock(StackPusher& pusher, FunctionDefinition const* function);
	static Pointer<Function> generatePrivateFunction(StackPusher& pusher, const std::string& name);
	static Pointer<Function> generateLibraryFunction(StackPusher& pusher, FunctionDefinition const* function, const std::string& name);
	static Pointer<Function> generateLibraryFunctionMacro(StackPusher& pusher, FunctionDefinition const* function, const std::string& name);
	static Pointer<Function> generateReceive(StackPusher& pusher, FunctionDefinition const* function);
	static Pointer<Function> generateFallback(StackPusher& pusher, FunctionDefinition const* function);
	static Pointer<Function> generateOnBounce(StackPusher& pusher, FunctionDefinition const* function);
	static Pointer<Function> generatePublicFunction(StackPusher& pusher, FunctionDefinition const* function);
	static void generateFunctionWithModifiers(StackPusher& pusher, FunctionDefinition const* function, bool pushArgs);
	static Pointer<Function> generateGetter(StackPusher& pusher, VariableDeclaration const* vd);
	static Pointer<Function> generatePublicFunctionSelector(StackPusher& pusher, ContractDefinition const *contract);
	void decodeFunctionParams(bool hasCallback);

protected:
	static Pointer<Function> generateReceiveOrFallbackOrOnBounce(
		StackPusher& pusher,
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
	void afterLoopCheck(const CFAnalyzer& ci, const int& loopVarQty, bool _doAnalyzeFlag);
	bool visitNode(ASTNode const&) override { solUnimplemented("Internal error: unreachable"); }

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override;
	void acceptBody(Block const& _block, std::optional<std::tuple<int, int>> functionBlock);
	bool visit(Block const& _block) override;
	bool visit(ExpressionStatement const& _expressionStatement) override;
	bool visit(IfStatement const& _ifStatement) override;
	bool visit(WhileStatement const& _whileStatement) override;
	bool visit(ForEachStatement const& _forStatement) override;
	std::pair<CFAnalyzer, ControlFlowInfo> pushControlFlowFlag(Statement const& body);
	void visitBodyOfForLoop(
		const CFAnalyzer& ci,
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
	Pointer<Function> generateMainExternalForAbiV1();
	Pointer<Function> generateMainExternalForAbiV2();

	void pushMsgPubkey();
	void checkSignatureAndReadPublicKey();
	void defaultReplayProtection();
	void expire();
	void callPublicFunctionOrFallback();
	void pushC4ToC7IfNeed();
	void pushC7ToC4IfNeed();
	void pushReceiveOrFallback();

	void buildPublicFunctionSelector(const std::vector<std::pair<uint32_t, std::string>>& functions, int left, int right);
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

} // end solidity::frontend
