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
	struct ControlFlowInfo {
		int stackSize {-1};
		bool isLoop {false};
		bool useJmp {false};
	};

	StackPusherHelper& m_pusher;
	std::vector<ControlFlowInfo> m_controlFlowInfo;


	const int m_startStackSize{};
	const bool m_isPublic{};
	const int m_currentModifier{};
	FunctionDefinition const* m_function{};

public:
	explicit TVMFunctionCompiler(StackPusherHelper& pusher);

	TVMFunctionCompiler(
		StackPusherHelper& pusher,
		bool isPublic,
		int modifier,
		FunctionDefinition const* f,
		const int startStackSize
	);

protected:
	enum class LocationReturn {
		noReturn,
		Last,
		Anywhere
	};

public:
	void makeInlineFunctionCall(bool alloc);

	void generateC4ToC7(bool withInitMemory);
	void generateMacro();
	void generateMainExternal();
	void generateMainInternal();
	void generateOnCodeUpgrade();
	void generateOnTickTock();
	void generatePrivateFunction();
	void generatePrivateFunctionWithoutHeader();
	void generatePublicFunction();
	void generateTvmGetter(FunctionDefinition const* _function);

public:
	void decodeFunctionParamsAndLocateVars();
protected:
	ast_vec<ModifierInvocation> functionModifiers();
	void endContinuation2(const bool doDrop);

	[[nodiscard]]
	bool allJmp() const;

	static LocationReturn notNeedsPushContWhenInlining(Block const& _block);
	CodeLines loadFromC4();
	void emitOnPublicFunctionReturn();
	void sendExternalMessage(const std::function<void(int)> &appendBody, Expression const* destAddr);
	void pushReturnParameters(const ast_vec<VariableDeclaration>& returnParameters);

	void acceptExpr(const Expression* expr, const bool isResultNeeded = true);

	void visitModifierOrFunctionBlock(Block const& body, bool isFunc);
	void visitFunctionAfterModifiers();
public:
	void visitFunctionWithModifiers();
private:
	void visitForOrWhileCondiction(const ContInfo& ci, const ControlFlowInfo& info, Expression const* condition);
	bool visitNode(ASTNode const&) override { solAssert(false, "Internal error: unreachable"); }

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override;
	bool visit(Block const& /*_block*/) override;
	bool visit(ExpressionStatement const& _expressionStatement) override;
	bool visit(IfStatement const& _ifStatement) override;
	bool visit(WhileStatement const& _whileStatement) override;
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
	static bool areReturnedValuesLiterals(Expression const* expr);

	void setGlobSenderAddressIfNeed();
	void generateMainExternalForAbiV1();
	void generateMainExternalForAbiV2();

	void pushMsgPubkey();
	void checkSignatureAndReadPublicKey();
	void defaultReplayProtection();
	void expire();
	void callPublicFunction();
	void fillInlineFunctionsAndConstants(std::string& pattern);
};

}	// end solidity::frontend
