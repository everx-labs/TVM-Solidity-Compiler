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
 * AST to TVM bytecode contract compiler
 */

#pragma once

#include <libsolidity/ast/Types.h>

namespace solidity::frontend {



class TVMFunctionCompiler: public ASTConstVisitor, private boost::noncopyable
{
private:
	TVMFunctionCompiler(StackPusherHelper& pusher, ContractDefinition const *contract);

	TVMFunctionCompiler(
		StackPusherHelper& pusher,
		int modifier,
		FunctionDefinition const* f,
		bool isLibraryWithObj,
		bool pushArgs,
		int startStackSize
	);

public:
	static void generateC4ToC7(StackPusherHelper& pusher, ContractDefinition const *contract, bool withInitMemory);
	static void generateMacro(StackPusherHelper& pusher, FunctionDefinition const* function, const std::optional<std::string>& forceName = nullopt);
	static void generateMainExternal(StackPusherHelper& pusher, ContractDefinition const *contract);
	static void generateMainInternal(StackPusherHelper& pusher, ContractDefinition const *contract);
	static void generateOnCodeUpgrade(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generateOnTickTock(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generatePrivateFunction(StackPusherHelper& pusher, FunctionDefinition const* function, const std::optional<std::string>& name = nullopt);
	static void generateLibraryFunction(StackPusherHelper& pusher, FunctionDefinition const* function, const std::string& name);
	static void generateLibraryFunctionMacro(StackPusherHelper& pusher, FunctionDefinition const* function, const std::string& name);
	static void generateReceive(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generateFallback(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generateOnBounce(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generatePublicFunction(StackPusherHelper& pusher, FunctionDefinition const* function);
	static void generateFunctionWithModifiers(StackPusherHelper& pusher, FunctionDefinition const* function, bool pushArgs);
	static void generateGetter(StackPusherHelper& pusher, VariableDeclaration const* vd);
	static void generatePublicFunctionSelector(StackPusherHelper& pusher, ContractDefinition const *contract);
	void decodeFunctionParamsAndLocateVars(bool hasCallback);

protected:
	static void generateReceiveOrFallback(StackPusherHelper& pusher, FunctionDefinition const* function, const std::string& name);
	ast_vec<ModifierInvocation> functionModifiers();
	void endContinuation2(bool doDrop);

	[[nodiscard]]
	bool allJmp() const;

	CodeLines loadFromC4();
	void emitOnPublicFunctionReturn();
	void pushDefaultParameters(const ast_vec<VariableDeclaration>& returnParameters);

	void acceptExpr(const Expression* expr, bool isResultNeeded = true);

	void visitModifierOrFunctionBlock(Block const& body, bool isFunc);
public:
	void visitFunctionWithModifiers();
private:
	void visitForOrWhileCondition(const ContInfo& ci, const ControlFlowInfo& info, const std::function<void()>& pushCondition);
	void afterLoopCheck(const ContInfo& ci, const int& loopVarQty);
	bool visitNode(ASTNode const&) override { solUnimplemented("Internal error: unreachable"); }

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override;
	bool visit(Block const& _block) override;
	bool visit(ExpressionStatement const& _expressionStatement) override;
	bool visit(IfStatement const& _ifStatement) override;
	bool visit(WhileStatement const& _whileStatement) override;
	bool visit(ForEachStatement const& _forStatement) override;
	void visitBodyOfForLoop(
		const ContInfo& ci,
		Statement const& body,
		const std::function<void()>& loopExpression
	);
	bool visit(ForStatement const& _forStatement) override;
	bool visit(Return const& _return) override;
	bool visit(Break const&) override;
	bool visit(Continue const&) override;
	bool visit(EmitStatement const& _emit) override;
	bool visit(PlaceholderStatement const& /*_node*/) override;

	ControlFlowInfo pushControlFlowFlagAndReturnControlFlowInfo(ContInfo &ci, bool isLoop);
	void doWhile(WhileStatement const& _whileStatement);
	void breakOrContinue(int code);
	bool tryOptimizeReturn(Expression const* expr);
	static bool isConstNumberOrConstTuple(Expression const* expr);

	void setGlobSenderAddressIfNeed();
	void generateMainExternalForAbiV1();
	void generateMainExternalForAbiV2();

	void pushMsgPubkey();
	void checkSignatureAndReadPublicKey();
	void defaultReplayProtection();
	void expire();
	void callPublicFunctionOrFallback();
	static std::string callSelector();
	void fillInlineFunctionsAndConstants(std::string& pattern);
	void pushC4ToC7IfNeed();
	void pushC7ToC4IfNeed();
	std::string pushReceive();

	void buildPublicFunctionSelector(const std::vector<std::pair<uint32_t, std::string>>& functions, int left, int right);

private:
	StackPusherHelper& m_pusher;
	std::vector<ControlFlowInfo> m_controlFlowInfo;

	const int m_startStackSize{};
	const int m_currentModifier{};
	FunctionDefinition const* m_function{};
	ContractDefinition const *m_contract{};
	const bool m_isLibraryWithObj{};
	const bool m_pushArgs{};
};

}	// end solidity::frontend
