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

#include <boost/algorithm/string/replace.hpp>

#include "TVM.h"
#include "TVMABI.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMInlineFunctionChecker.hpp"
#include "TVMStructCompiler.hpp"

using namespace solidity::frontend;

TVMFunctionCompiler::TVMFunctionCompiler(StackPusherHelper &pusher) : m_pusher{pusher} {

}

TVMFunctionCompiler::TVMFunctionCompiler(StackPusherHelper &pusher, bool isPublic, int modifier,
                                         FunctionDefinition const *f, const int startStackSize) :
		m_pusher{pusher},
		m_startStackSize{startStackSize},
		m_isPublic{isPublic},
		m_currentModifier{modifier},
		m_function{f} {

}

ast_vec<ModifierInvocation> TVMFunctionCompiler::functionModifiers() {
	ast_vec<ModifierInvocation> ret;
	for (const ASTPointer<ModifierInvocation>& mod : m_function->modifiers()) {
		if (to<ModifierDefinition>(mod->name()->annotation().referencedDeclaration)) {
			ret.push_back(mod);
		}
	}
	return ret;
}

void TVMFunctionCompiler::endContinuation2(const bool doDrop) {
	int delta = m_pusher.getStack().size() - m_controlFlowInfo.back().stackSize;
	if (doDrop) {
		m_pusher.drop(delta);
	} else {
		m_pusher.push(-delta, ""); // fix stack
	}
	m_pusher.endContinuation();
}

bool TVMFunctionCompiler::allJmp() const {
	return std::all_of(m_controlFlowInfo.begin(), m_controlFlowInfo.end(), [](const ControlFlowInfo& info){
		return info.useJmp;
	});
}

TVMFunctionCompiler::LocationReturn TVMFunctionCompiler::notNeedsPushContWhenInlining(Block const &_block) {

	ast_vec<Statement> statements = _block.statements();

	TVMScanner bodyScanner{_block};
	if (!bodyScanner.m_info.canReturn) {
		return LocationReturn::noReturn;
	}

	for (std::vector<int>::size_type i = 0; i + 1 < statements.size(); ++i) {
		TVMScanner scanner{*statements[i].get()};
		if (scanner.m_info.canReturn) {
			return LocationReturn::Anywhere;
		}
	}
	bool isLastStatementReturn = to<Return>(statements.back().get()) != nullptr;
	return isLastStatementReturn ? LocationReturn::Last : LocationReturn::Anywhere;
}

CodeLines TVMFunctionCompiler::loadFromC4() {
	StackPusherHelper pusherHelper(&m_pusher.ctx());


	pusherHelper.pushLines(R"(LDU 256      ; pubkey c4)");
	if (m_pusher.ctx().storeTimestampInC4()) {
		pusherHelper.pushLines(R"(LDU 64      ; pubkey timestamp c4)");
	}
	pusherHelper.pushLines(R"(
LDU 1       ; pubkey [timestamp] constructor_flag memory
)");
	if (!pusherHelper.ctx().notConstantStateVariables().empty()) {
		pusherHelper.getStack().change(+1); // slice
		pusherHelper.structCompiler().sliceToStateVarsToC7();
	} else {
		pusherHelper.push(0, "ENDS");
	}
	pusherHelper.pushLines(R"(
TRUE
SETGLOB 1   ; pubkey [timestamp] constructor_flag
SETGLOB 6   ; pubkey [timestamp]
)");
	if (m_pusher.ctx().storeTimestampInC4()) {
		pusherHelper.pushLines(R"(SETGLOB 3   ; D)");
	}

	pusherHelper.pushLines(R"(SETGLOB 2)");
	return pusherHelper.code();
}

void TVMFunctionCompiler::generateC4ToC7(bool withInitMemory) {
	m_pusher.pushLines(std::string{".macro\t"} + (withInitMemory? "c4_to_c7_with_init_storage": "c4_to_c7"));
	m_pusher.pushLines(R"(
PUSHROOT
CTOS        ; c4
)");
	if (withInitMemory) {
		m_pusher.pushLines(R"(
DUP        ; c4 c4
SBITS      ; c4 bits
GTINT 1    ; c4 bits>1
)");
	}

	if (!withInitMemory) {
		m_pusher.append(loadFromC4());
	} else {
		m_pusher.pushCont(loadFromC4());
		m_pusher.pushLines(R"(
PUSHCONT {
	PLDDICT   ; D
)");
		m_pusher.addTabs();
		int shift = 0;
		for (VariableDeclaration const* v : m_pusher.ctx().getContract()->stateVariablesIncludingInherited()) {
			if (v->isConstant()) {
				continue;
			}
			m_pusher.push(0, "; init " + v->name());
			if (v->isPublic()) {
				m_pusher.pushInt(TvmConst::C4::PersistenceMembersStartIndex + shift++); // index
				m_pusher.pushS(1); // index dict
				m_pusher.getFromDict(getKeyTypeOfC4(), *v->type(), *v, StackPusherHelper::DictOperation::GetFromMapping,
				                     false);
			} else {
				m_pusher.pushDefaultValue(v->type());
			}
			m_pusher.setGlob(v);
		}
		m_pusher.subTabs();
		m_pusher.pushLines(R"(
	; set contract pubkey
	PUSHINT 0
	SWAP
	PUSHINT 64
	DICTUGET
	THROWIFNOT 61
	PLDU 256
	SETGLOB 2

	PUSHINT 0 ; timestamp
	SETGLOB 3
	PUSHINT 0 ; constructor_flag
	SETGLOB 6
)");
		m_pusher.pushLines(R"(
	TRUE
	SETGLOB 1
)");
		m_pusher.addTabs();
		for (VariableDeclaration const *variable: m_pusher.ctx().notConstantStateVariables()) {
			if (auto value = variable->value().get()) {
				m_pusher.push(0, ";; init state var: " + variable->name());
				acceptExpr(value);
				m_pusher.setGlob(variable);
			}
		}
		m_pusher.subTabs();
		m_pusher.pushLines(R"(
}
IFELSE
)");
	}
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generateTvmGetter(FunctionDefinition const *_function) {
	m_pusher.generateGlobl(_function->name(), true);
	visitFunctionWithModifiers();
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generateMacro() {
	m_pusher.generateMacro(m_function->name());
	makeInlineFunctionCall(true);
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::makeInlineFunctionCall(bool alloc) {
	if (alloc) {
		for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
			m_pusher.push(0, string(";; param: ") + variable->name());
			m_pusher.getStack().add(variable.get(), true);
		}
	}

	Block const* block{};
	if (!functionModifiers().empty()) {
		auto modifierDefinition = to<ModifierDefinition>(functionModifiers()[0]->name()->annotation().referencedDeclaration);
		block = &modifierDefinition->body();
	} else {
		block = &m_function->body();
	}
	LocationReturn locationReturn = notNeedsPushContWhenInlining(*block);
	if (locationReturn == LocationReturn::Anywhere) {
		m_pusher.startContinuation();
	}
	visitFunctionWithModifiers();
	m_pusher.tryPollLastRetOpcode();
	if (locationReturn == LocationReturn::Anywhere) {
		m_pusher.endContinuation();
		m_pusher.push(0, "CALLX");
	}
}

void TVMFunctionCompiler::generateOnCodeUpgrade() {
	m_pusher.generateInternal("onCodeUpgrade", 2);
	m_pusher.append(switchSelectorIfNeed(m_function));

	makeInlineFunctionCall(true);

	m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
	m_pusher.push(0, "COMMIT");
	m_pusher.push(0, "THROW 0");
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generateOnTickTock() {
	m_pusher.generateInternal("onTickTock", -2);
	m_pusher.push(0, "PUSHINT -2");
	m_pusher.push(0, "PUSHINT -2");
	solAssert(m_function->parameters().size() == 1, "");
	for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
		m_pusher.push(+1, "PUSH s2");
		m_pusher.push(0, string(";; param: ") + variable->name());
		m_pusher.getStack().add(variable.get(), false);
	}
	if (m_function->stateMutability() != StateMutability::Pure) {
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7");
	}

	visitFunctionWithModifiers();

	if (m_function->stateMutability() != StateMutability::Pure) {
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
	}
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generatePublicFunction() {
	m_pusher.generateGlobl(m_function->name(), m_function->isPublic());
	visitFunctionWithModifiers();
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generatePrivateFunction() {
	std::string name = m_pusher.ctx().getFunctionInternalName(m_function);
	if (m_function != getContractFunctions(m_pusher.ctx().getContract(), m_function->name()).back()) {
		name = m_function->annotation().contract->name() + "_" + m_function->name();
	}
	m_pusher.generateGlobl(name, false);
	generatePrivateFunctionWithoutHeader();
}

void TVMFunctionCompiler::generatePrivateFunctionWithoutHeader() {
	for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
		m_pusher.push(0, string(";; param: ") + variable->name());
		m_pusher.getStack().add(variable.get(), true);
	}

	visitFunctionWithModifiers();
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::emitOnPublicFunctionReturn() {
	const auto& params = m_function->returnParameters();
	auto count = params.size();
	if (count == 0) {
		return;
	}
	m_pusher.push( 0, ";; emitting " + toString(count) + " value(s)");

	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	for (const auto & param : params) {
		types.push_back(param->annotation().type);
		nodes.push_back(param.get());
	}

	int prevStackSize = m_pusher.getStack().size();
	m_pusher.encodeFunctionAndParams(
			TVMCompilerContext::getFunctionExternalName(m_function),
			types,
			nodes,
			[&](size_t idx) {
				int pos = (m_pusher.getStack().size() - prevStackSize) +
				          (static_cast<int>(params.size()) - static_cast<int>(idx) - 1);
				m_pusher.pushS(pos);
			},
			StackPusherHelper::ReasonOfOutboundMessage::FunctionReturnExternal
	);
	sendExternalMessage();
}

void TVMFunctionCompiler::sendExternalMessage(bool addrIsArgument) {
	// stack: builder with encoded params
	if (m_pusher.ctx().haveSetDestAddr()) {
		if (!addrIsArgument)
			m_pusher.push(+1, "GETGLOB " + toString(TvmConst::C7::ExtDestAddrIndex));
		m_pusher.pushLines(R"(DUP
ISNULL
PUSHCONT {
	DROP
	PUSHSLICE x2_
}
IF
)");
		m_pusher.exchange(0, 1);
		m_pusher.pushPrivateFunctionOrMacroCall(-2, "send_external_message_with_dest_macro");
	} else {
		m_pusher.pushPrivateFunctionOrMacroCall(-1, "send_external_message_macro");
	}
}

void TVMFunctionCompiler::visitModifierOrFunctionBlock(Block const &body, bool isFunc) {
	LocationReturn locationReturn = notNeedsPushContWhenInlining(body);
	bool haveSomeNamedReturnParams = false;
	if (isFunc) {
		for (const auto &returnParam: m_function->returnParameters()) {
			haveSomeNamedReturnParams |= !returnParam->name().empty();
		}
		if (haveSomeNamedReturnParams) {
			pushReturnParameters(m_function->returnParameters());
		}
	}

	switch (locationReturn) {
		case LocationReturn::noReturn: {
			body.accept(*this);

			if (isFunc) {
				if (haveSomeNamedReturnParams) {
					int paramQty = m_function->parameters().size();
					int retQty = m_function->returnParameters().size();
					m_pusher.push(0, ";; returning named params");
					int m = m_pusher.getStack().size() - m_startStackSize - paramQty;
					m_pusher.blockSwap(paramQty, m);
					m_pusher.drop(m_pusher.getStack().size() - m_startStackSize - retQty);
				} else if (!m_function->returnParameters().empty()) {
					int delta = (m_pusher.getStack().size() - m_startStackSize);
					m_pusher.drop(delta);
					pushReturnParameters(m_function->returnParameters());
				} else {
					int delta = (m_pusher.getStack().size() - m_startStackSize);
					m_pusher.drop(delta);
				}
			}
			break;
		}
		case LocationReturn::Last: {
			body.accept(*this);
			m_pusher.tryPollLastRetOpcode();

			if (isFunc) {
				int delta = (m_pusher.getStack().size() - m_startStackSize) -
				            static_cast<int>(m_function->returnParameters().size());
				m_pusher.push(-delta, ""); // fix stack
			}
			break;
		}
		case LocationReturn::Anywhere: {
			bool haveContinuation = !functionModifiers().empty() || m_isPublic;

			if (haveContinuation) {
				m_pusher.startContinuation();
			}
			body.accept(*this);
			m_pusher.tryPollLastRetOpcode();
			if (isFunc) {
				bool doFunctionAlwaysReturn = doesAlways<Return>(&m_function->body());
				if (!doFunctionAlwaysReturn && haveSomeNamedReturnParams) {
					int paramQty = m_function->parameters().size();
					int retQty = m_function->returnParameters().size();
					m_pusher.push(0, ";; returning named params");
					int m = m_pusher.getStack().size() - m_startStackSize - paramQty;
					m_pusher.blockSwap(paramQty, m);
					m_pusher.drop(m_pusher.getStack().size() - m_startStackSize - retQty);
				} else if (!doFunctionAlwaysReturn) {
					// if function did not return
					m_pusher.drop(m_pusher.getStack().size() - m_startStackSize);
					pushReturnParameters(m_function->returnParameters());
				} else {
					int delta = (m_pusher.getStack().size() - m_startStackSize) -
					            static_cast<int>(m_function->returnParameters().size());
					m_pusher.push(-delta, ""); // fix stack
				}
			}
			if (haveContinuation) {
				m_pusher.endContinuation();
				m_pusher.push(0, "CALLX");
			}
			break;
		}
	}
}

void TVMFunctionCompiler::visitFunctionAfterModifiers() {
	const std::string text = (m_function->isConstructor()? "constructor " : "function ") + functionName(m_function);
	m_pusher.push(0, "; " + text);

	// accept function body
	visitModifierOrFunctionBlock(m_function->body(), true);

	if (m_isPublic) {
		// emit function result
		// stack: transaction_id return-params...
		bool dontEmitReturn = !!getFunction(m_pusher.ctx().getContract(), "tvm_dont_emit_events_on_return");
		if (!dontEmitReturn) {
			emitOnPublicFunctionReturn();
		}

		// drop function result
		const int retQty = m_function->returnParameters().size();
		m_pusher.drop(retQty);
	} else {
		if (!functionModifiers().empty()) {
			if (!m_function->returnParameters().empty()) {
				const int retQty = m_function->returnParameters().size();
				m_pusher.tuple(retQty);
				m_pusher.setGlob(TvmConst::C7::TempFunctionReturnBuffer);
			}
		}
	}

	m_pusher.push(0, "; end " + text);
}

void TVMFunctionCompiler::visitFunctionWithModifiers() {
	solAssert(m_startStackSize >= 0, "");

	if (m_currentModifier == 0) {
		if (m_isPublic) {
			// c4_to_c7 if need
			if (m_function->stateMutability() != StateMutability::Pure) {
				m_pusher.pushLines(R"(
GETGLOB 1
ISNULL
)");
				m_pusher.startContinuation();
				m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7");
				m_pusher.endContinuation();
				m_pusher.pushLines("IF");
			}

			// decode function params
			// stack: transaction_id arguments-in-slice
			m_pusher.push(+1, ""); // arguments-in-slice
			DecodeFunctionParams{&m_pusher}.decodeParameters(m_function->parameters());
			// stack: transaction_id arguments...
			m_pusher.getStack().change(-static_cast<int>(m_function->parameters().size()));
			for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
				auto name = variable->name();
				m_pusher.push(0, string(";; param: ") + name);
				m_pusher.getStack().add(variable.get(), true);
			}
		} else {
			// function params are allocated
			solAssert(m_pusher.getStack().size() >= static_cast<int>(m_function->parameters().size()), "");
		}
	}


	if (m_currentModifier == static_cast<int>(functionModifiers().size())) {
		visitFunctionAfterModifiers();
	} else {
		ModifierInvocation const *invocation = functionModifiers()[m_currentModifier].get();
		auto modifierDefinition = to<ModifierDefinition>(invocation->name()->annotation().referencedDeclaration);
		m_pusher.push(0, "; modifier " + invocation->name()->name());
		ast_vec<Expression> const *args = invocation->arguments();
		if (args != nullptr) {
			for (int i = 0; i < static_cast<int>(args->size()); ++i) {
				const ASTPointer<Expression> &arg = (*args)[i];
				TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
				m_pusher.getStack().add(modifierDefinition->parameters()[i].get(), false);
			}
		}
		visitModifierOrFunctionBlock(modifierDefinition->body(), false);
		bool doFunctionAlwaysReturn = doesAlways<Return>(&modifierDefinition->body());
		if (!doFunctionAlwaysReturn) {
			// if modifier did not return
			m_pusher.drop(m_pusher.getStack().size() - m_startStackSize);
		}
		m_pusher.push(0, "; end modifier " + invocation->name()->name());
	}


	if (m_currentModifier == 0) {
		if (m_isPublic) {
			// c7_to_c4 if need
			solAssert(m_pusher.getStack().size() == 0, "");
			if (m_function->stateMutability() == StateMutability::NonPayable ||
			    m_function->stateMutability() == StateMutability::Payable) {
				m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
			} else {
				m_pusher.push(0, "EQINT -1"); // is it ext msg?
				m_pusher.startContinuation();
				m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
				m_pusher.endContinuation();
				m_pusher.push(0, "IF");
			}

			// set flag meaning function is called
			m_pusher.push(0, "TRUE");
			m_pusher.push(0, "SETGLOB 7");
		} else {
			if (!functionModifiers().empty()) {
				if (!m_function->returnParameters().empty()) {
					const int retQty = m_function->returnParameters().size();
					m_pusher.getGlob(TvmConst::C7::TempFunctionReturnBuffer);
					m_pusher.untuple(retQty);
				}
			}
		}

		if (m_isPublic) {
			solAssert(m_pusher.getStack().size() == 0, "");
		} else {
			solAssert(m_pusher.getStack().size() ==
			          m_startStackSize + static_cast<int>(m_function->returnParameters().size()), "");
		}
	}
}

void TVMFunctionCompiler::pushReturnParameters(const ast_vec<VariableDeclaration> &returnParameters) {
	int idParam = 0;
	for (const ASTPointer<VariableDeclaration>& returnParam: returnParameters) {
		auto name = returnParam->name();
		if (name.empty()) {
			name = "retParam@" + std::to_string(idParam);
		}
		m_pusher.push(0, string(";; ret param: ") + name);
		m_pusher.pushDefaultValue(returnParam->type());
		m_pusher.getStack().add(returnParam.get(), false);

		++idParam;
	}
}

void TVMFunctionCompiler::acceptExpr(const Expression *expr, const bool isResultNeeded) {
	solAssert(expr, "");
	TVMExpressionCompiler(m_pusher).acceptExpr2(expr, isResultNeeded);
}

bool TVMFunctionCompiler::visit(VariableDeclarationStatement const &_variableDeclarationStatement) {
	const int saveStackSize = m_pusher.getStack().size();

	ast_vec<VariableDeclaration> decls = _variableDeclarationStatement.declarations();
	if (auto init = _variableDeclarationStatement.initialValue()) {
		auto tupleExpression = to<TupleExpression>(init);
		if (tupleExpression && !tupleExpression->isInlineArray()) {
			ast_vec<Expression> const&  tuple = tupleExpression->components();
			for (std::size_t i = 0; i < tuple.size(); ++i) {
				if (!m_pusher.tryImplicitConvert(decls[i]->type(), tuple[i]->annotation().type)) {
					acceptExpr(tuple[i].get());
				}
			}
		} else if (decls[0] == nullptr || !m_pusher.tryImplicitConvert(decls[0]->type(), init->annotation().type)) {
			acceptExpr(init);
		}
	} else {
		for (const auto& decl : decls) {
			m_pusher.pushDefaultValue(decl->type());
		}
	}

	m_pusher.getStack().change(-(int)decls.size());
	int cntVars = 0;
	for (size_t i = 0; i < decls.size(); i++) {
		if (decls[i]) {
			m_pusher.push(0, string(";; decl: ") + decls[i]->name());
			m_pusher.getStack().add(decls[i].get(), true);
			++cntVars;
		} else {
			if (i == decls.size() - 1) {
				m_pusher.push(0, "DROP");
			} else if (i == decls.size() - 2) {
				m_pusher.push(0, "NIP");
			} else {
				m_pusher.blockSwap(1, static_cast<int>(decls.size()) - 1 - i);
				m_pusher.push(0, "DROP");
			}
		}
	}
	m_pusher.getStack().ensureSize(saveStackSize + cntVars, "VariableDeclarationStatement");
	return false;
}

bool TVMFunctionCompiler::visit(Block const &) {
	// TODO: write the code explicitly
	return true;
}

bool TVMFunctionCompiler::visit(ExpressionStatement const &_expressionStatement) {
	auto savedStackSize = m_pusher.getStack().size();
	acceptExpr(&_expressionStatement.expression(), false);
	m_pusher.getStack().ensureSize(savedStackSize, ASTNode2String(_expressionStatement));
	return false;
}

bool TVMFunctionCompiler::visit(IfStatement const &_ifStatement) {
	const int saveStackSize = m_pusher.getStack().size();

	m_pusher.push(0, ";; if");


	// header
	ContInfo ci = getInfo(_ifStatement);
	bool canUseJmp = _ifStatement.falseStatement() != nullptr?
	                 getInfo(_ifStatement.trueStatement()).doThatAlways() && getInfo(*_ifStatement.falseStatement()).doThatAlways() :
	                 getInfo(_ifStatement.trueStatement()).doThatAlways();
	if (canUseJmp) {
		ControlFlowInfo info {};
		info.stackSize = m_pusher.getStack().size();
		info.isLoop = false;
		info.useJmp = true;
		m_controlFlowInfo.push_back(info);
	} else {
		ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, false);
		m_controlFlowInfo.push_back(info);
	}

	// condition
	acceptExpr(&_ifStatement.condition(), true);
	m_pusher.push(-1, ""); // drop condition

	// if
	m_pusher.startContinuation();
	_ifStatement.trueStatement().accept(*this);
	endContinuation2(!canUseJmp);


	if (_ifStatement.falseStatement() != nullptr) {
		// else
		m_pusher.startContinuation();
		_ifStatement.falseStatement()->accept(*this);
		endContinuation2(!canUseJmp);

		if (canUseJmp) {
			m_pusher.push(0, "CONDSEL");
			m_pusher.push(0, "JMPX");
		} else {
			m_pusher.push(0, "IFELSE");
		}
	} else {
		if (canUseJmp) {
			m_pusher.push(0, "IFJMP");
		} else {
			m_pusher.push(0, "IF");
		}
	}

	m_controlFlowInfo.pop_back();

	if (!canUseJmp) {
		// bottom
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			if (ci.canReturn) {
				if (allJmp()) { // no loops, only if-else
					m_pusher.push(0, "EQINT 4");
					m_pusher.push(-1, "IFRET");
				} else {
					m_pusher.push(+1, "DUP");
					m_pusher.push(-1, "IFRET");
					m_pusher.push(-1, "DROP");
				}
			} else {
				m_pusher.push(+1, "DUP");
				m_pusher.push(-1, "IFRET");
				m_pusher.push(-1, "DROP");
			}
		}
	}

	m_pusher.push(0, ";; end if");

	m_pusher.getStack().ensureSize(saveStackSize, "");

	return false;
}

TVMFunctionCompiler::ControlFlowInfo
TVMFunctionCompiler::pushControlFlowFlagAndReturnControlFlowInfo(ContInfo &ci, bool isLoop) {
	ControlFlowInfo info {};
	info.isLoop = isLoop;
	info.stackSize = -1;
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		m_pusher.push(+1, "FALSE ; decl return flag"); // break flag
	}
	info.stackSize = m_pusher.getStack().size();
	return info;
}

void TVMFunctionCompiler::doWhile(WhileStatement const &_whileStatement) {
	int saveStackSize = m_pusher.getStack().size();

	// header
	m_pusher.push(0, "; do-while");
	ContInfo ci = getInfo(_whileStatement.body());
	ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
	m_controlFlowInfo.push_back(info);

	// body
	m_pusher.startContinuation();
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		int ss = m_pusher.getStack().size();
		m_pusher.startContinuation();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size() - ss);
		m_pusher.endContinuation();
		m_pusher.push(0, "CALLX");
	} else {
		int ss = m_pusher.getStack().size();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size() - ss);
	}
	// condition
	m_pusher.push(0, "; condition");
	if (ci.canBreak || ci.canReturn) {
		m_pusher.push(+1, "DUP");
		m_pusher.push(0, "GTINT 1");
		m_pusher.push(+1, "DUP");
		m_pusher.push(-2, ""); // fix stack

		m_pusher.startContinuation();
		m_pusher.push(0, "DROP");
		acceptExpr(&_whileStatement.condition(), true);
		m_pusher.push(0, "NOT");
		m_pusher.endContinuation();
		m_pusher.push(-1, ""); // fix stack

		m_pusher.push(+1, "IFNOT");
	} else {
		acceptExpr(&_whileStatement.condition(), true);
		m_pusher.push(0, "NOT");
	}
	m_pusher.push(-1, ""); // drop condition
	m_pusher.endContinuation();

	m_pusher.push(0, "UNTIL");

	m_controlFlowInfo.pop_back();

	// bottom
	if (ci.canReturn) {
		if (allJmp()) { // no loops, only if-else
			m_pusher.push(0, "EQINT 4");
			m_pusher.push(-1, "IFRET");
		} else {
			m_pusher.push(+1, "DUP");
			if (ci.canBreak || ci.canContinue) {
				m_pusher.push(0, "EQINT 4");
			}
			m_pusher.push(-1, "IFRET");
			m_pusher.push(-1, "DROP");
		}
	} else if (ci.canBreak || ci.canContinue) {
		m_pusher.drop(1);
	}

	m_pusher.push(0, "; end do-while");

	m_pusher.getStack().ensureSize(saveStackSize, "");
}

void
TVMFunctionCompiler::visitForOrWhileCondiction(const ContInfo &ci, const TVMFunctionCompiler::ControlFlowInfo &info,
                                               Expression const *condition) {
	int stackSize = m_pusher.getStack().size();
	m_pusher.startContinuation();
	if (ci.canBreak || ci.canReturn) {
		m_pusher.pushS(m_pusher.getStack().size() - info.stackSize);
		m_pusher.push(0, "LESSINT 2");
		m_pusher.push(-1, ""); // fix stack

		if (condition != nullptr) {
			m_pusher.push(0, "DUP");
			m_pusher.startContinuation();
			m_pusher.push(0, "DROP");
			acceptExpr(condition, true);
			m_pusher.endContinuation();
			m_pusher.push(-1, ""); // fix stack
			m_pusher.push(0, "IF");
		}
	} else {
		acceptExpr(condition, true);
		m_pusher.push(-1, ""); // fix stack
	}
	m_pusher.endContinuation();
	m_pusher.getStack().ensureSize(stackSize, "visitForOrWhileCondiction");
}

bool TVMFunctionCompiler::visit(WhileStatement const &_whileStatement) {
	int saveStackSizeForWhile = m_pusher.getStack().size();

	if (_whileStatement.isDoWhile()) {
		doWhile(_whileStatement);
		return false;
	}

	// header
	m_pusher.push(0, "; while");
	ContInfo ci = getInfo(_whileStatement.body());
	ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
	m_controlFlowInfo.push_back(info);

	int saveStackSize = m_pusher.getStack().size();

	// condition
	visitForOrWhileCondiction(ci, info, &_whileStatement.condition());

	m_pusher.getStack().ensureSize(saveStackSize, "while condition");

	// body
	m_pusher.startContinuation();
	_whileStatement.body().accept(*this);
	m_pusher.drop(m_pusher.getStack().size() - saveStackSize);
	m_pusher.endContinuation();

	m_pusher.push(0, "WHILE");

	m_controlFlowInfo.pop_back();

	// bottom
	// TODO to common function
	if (ci.canReturn) {
		if (allJmp()) { // no loops, only if-else
			m_pusher.push(0, "EQINT 4");
			m_pusher.push(-1, "IFRET");
		} else {
			m_pusher.push(+1, "DUP");
			if (ci.canBreak || ci.canContinue) {
				m_pusher.push(0, "EQINT 4");
			}
			m_pusher.push(-1, "IFRET");
			m_pusher.push(-1, "DROP");
		}
	} else if (ci.canBreak || ci.canContinue) {
		m_pusher.drop(1);
	}

	m_pusher.push(0, "; end while");

	m_pusher.getStack().ensureSize(saveStackSizeForWhile, "");

	return false;
}

bool TVMFunctionCompiler::visit(ForStatement const &_forStatement) {

	// init - opt
	// return break or continue flag  - opt
	// PUSHCONT {
	//     condition
	// }
	// PUSHCONT {
	//     PUSHCONT {
	//        body
	//     }
	//     CALLX
	//     loopExpression
	// }

	// init - opt
	// PUSHCONT {
	//     condition
	// }
	// PUSHCONT {
	//     body
	//     loopExpression
	// }

	int saveStackSize = m_pusher.getStack().size();
	m_pusher.push(0, "; for");

	// init
	if (_forStatement.initializationExpression() != nullptr) {
		_forStatement.initializationExpression()->accept(*this);
	}

	// header
	ContInfo ci = getInfo(_forStatement.body());
	ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
	m_controlFlowInfo.push_back(info);

	// condition
	visitForOrWhileCondiction(ci, info, _forStatement.condition());

	// body and loopExpression
	m_pusher.startContinuation();
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		int ss = m_pusher.getStack().size();
		m_pusher.startContinuation();
		_forStatement.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size() - ss);
		m_pusher.endContinuation();
		m_pusher.push(0, "CALLX");
		m_pusher.pushLines(R"(
DUP
EQINT 4
IFRET
)");
	} else {
		int ss = m_pusher.getStack().size();
		_forStatement.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size() - ss);
	}
	if (_forStatement.loopExpression() != nullptr) {
		_forStatement.loopExpression()->accept(*this);
	}
	m_pusher.endContinuation();

	m_pusher.push(0, "WHILE");

	m_controlFlowInfo.pop_back();


	// bottom
	int cntDrop = 0;
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		++cntDrop;
	}
	if (_forStatement.initializationExpression() != nullptr) {
		++cntDrop;
	}
	if (ci.canReturn) {
		if (allJmp()) {
			m_pusher.push(0, "EQINT 4");
			m_pusher.push(-1, "IFRET");
			if (_forStatement.initializationExpression() != nullptr) {
				m_pusher.drop(1);
			}
		} else {
			m_pusher.push(+1, "DUP");
			if (ci.canBreak || ci.canContinue) {
				m_pusher.push(0, "EQINT 4");
			}
			m_pusher.push(-1, "IFRET");
			m_pusher.drop(cntDrop);
		}
	} else {
		m_pusher.drop(cntDrop);
	}

	m_pusher.push(0, "; end for");
	m_pusher.getStack().ensureSize(saveStackSize, "for");
	return false;
}

bool TVMFunctionCompiler::visit(Return const &_return) {
	m_pusher.push(0, ";; return");
	auto expr = _return.expression();
	if (expr) {
		if (!tryOptimizeReturn(expr)) {
			if (!areReturnedValuesLiterals(expr)) {
				acceptExpr(expr);
			}
		}
	}

	int retCount = 0;
	if (_return.annotation().functionReturnParameters != nullptr) {
		ast_vec<VariableDeclaration> const& params =
				_return.annotation().functionReturnParameters->parameters();
		retCount = params.size();
	}


	const int functionSlots = m_pusher.getStack().size() - m_startStackSize;
	int revertDelta = functionSlots - retCount;
	if (expr && areReturnedValuesLiterals(expr)) {
		m_pusher.drop(functionSlots);
		acceptExpr(expr);
	} else {
		m_pusher.dropUnder(retCount, functionSlots - retCount);
	}


	if (!allJmp()) {
		m_pusher.pushInt(4);

		revertDelta--;
		m_pusher.push(revertDelta, ""); // fix stack
	} else if (!m_controlFlowInfo.empty()) { // all continuation are run by JMPX
		m_pusher.push(revertDelta, ""); // fix stack
	}

	m_pusher.push(0, "RET");
	return false;
}

void TVMFunctionCompiler::breakOrContinue(int code) {
	solAssert(code == 1 || code == 2, "");

	if (code == 1) {
		m_pusher.push(0, ";; continue");
	} else {
		m_pusher.push(0, ";; break");
	}

	ControlFlowInfo controlFlowInfo;
	for (int i = static_cast<int>(m_controlFlowInfo.size()) - 1; ; --i) {
		if (m_controlFlowInfo.at(i).isLoop) {
			controlFlowInfo = m_controlFlowInfo.at(i);
			break;
		}
	}

	const int sizeDelta = m_pusher.getStack().size() - controlFlowInfo.stackSize;
	m_pusher.drop(sizeDelta + 1);
	m_pusher.pushInt(code);
	m_pusher.push(0, "RET");
	m_pusher.push(sizeDelta, ""); // fix stack
}

bool TVMFunctionCompiler::visit(Break const &) {
	breakOrContinue(2);
	return false;
}

bool TVMFunctionCompiler::visit(Continue const &) {
	breakOrContinue(1);
	return false;
}

bool TVMFunctionCompiler::visit(EmitStatement const &_emit) {
	auto eventCall = to<FunctionCall>(&_emit.eventCall());
	solAssert(eventCall, "");
	auto eventName = to<Identifier>(&eventCall->expression());
	solAssert(eventName, "");
	string name = eventName->name();
	m_pusher.push(0, ";; emit " + name);
	auto event = m_pusher.ctx().getEvent(name);
	solAssert(event, "");
	TVMExpressionCompiler ec(m_pusher);
	ec.encodeOutboundMessageBody2(
			name,
			eventCall->arguments(),
			event->parameterList().parameters(),
			StackPusherHelper::ReasonOfOutboundMessage::EmitEventExternal);

	if (auto argument = _emit.externalAddress()) {
		acceptExpr(argument.get());
		sendExternalMessage(true);
	} else
		sendExternalMessage();

	return false;
}

bool TVMFunctionCompiler::tryOptimizeReturn(Expression const *expr) {
	auto identifier = to<Identifier>(expr);
	if (identifier) {
		Declaration const* name = identifier->annotation().referencedDeclaration;
		if (m_pusher.getStack().isParam(name) && m_pusher.getStack().getOffset(name) == 0 &&
		    m_pusher.getStack().getStackSize(name) >= m_startStackSize) {
			return true;
		}
	} else if (auto tuple = to<TupleExpression>(expr)) {
		int size = tuple->components().size();
		int i = 0;
		for (const ASTPointer<Expression>& comp : tuple->components()) {
			identifier = to<Identifier>(comp.get());
			if (!identifier) {
				return false;
			}
			Declaration const* name = identifier->annotation().referencedDeclaration;
			if (!m_pusher.getStack().isParam(name) || m_pusher.getStack().getOffset(name) != size - 1 - i ||
			    m_pusher.getStack().getStackSize(name) < m_startStackSize) {
				return false;
			}

			++i;
		}
		return true;
	}

	return false;
}

bool TVMFunctionCompiler::areReturnedValuesLiterals(Expression const *expr) {
	auto literal = to<Literal>(expr);
	if (literal) {
		return true;
	} else if (auto tuple = to<TupleExpression>(expr)) {
		for (const ASTPointer<Expression>& comp : tuple->components()) {
			if (to<Literal>(comp.get()) == nullptr) {
				return false;
			}
		}
		return true;
	}

	return false;
}

void TVMFunctionCompiler::generateMainExternal() {
	switch (m_pusher.ctx().pragmaHelper().abiVersion()) {
		case 1:
			generateMainExternalForAbiV1();
			break;
		case 2:
			generateMainExternalForAbiV2();
			break;
		default:
			solAssert(false, "");
	}
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::generateMainExternalForAbiV1() {
	m_pusher.generateInternal("main_external", -1);
	// contract_balance msg_balance msg_cell origin_msg_body_slice
	m_pusher.pushLines(R"(
PUSHINT -1 ; main_external trans id
PUSH s1    ; originMsgBodySlice
LDREFRTOS  ; msgBodySlice signSlice
DUP        ; msgBodySlice signSlice signSlice
SDEMPTY    ; msgBodySlice signSlice isSignSliceEmpty
PUSHCONT {
	DROP         ; msgBodySlice
}
PUSHCONT {
	DUP          ; msgBodySlice signSlice signSlice
	PUSHINT 512  ; msgBodySlice signSlice signSlice 512
	SDSKIPFIRST  ; msgBodySlice signSlice signSlice'
	PLDU 256     ; msgBodySlice signSlice pubKey
	PUSH s2      ; msgBodySlice signSlice pubKey msgBodySlice
	HASHSU       ; msgBodySlice signSlice pubKey msgHash
	PUSH2 s2,s1  ; msgBodySlice signSlice pubKey msgHash signSlice pubKey
	CHKSIGNU     ; msgBodySlice signSlice pubKey isSigned
	THROWIFNOT 40; msgBodySlice signSlice pubKey
	SETGLOB 5    ; msgBodySlice signSlice
	DROP         ; msgBodySlice
}
IFELSE
)");
	m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");
	m_pusher.pushLines(R"(
LDU 32                         ; functionId msgSlice
LDU 64                         ; functionId timestamp msgSlice
SWAP                           ; functionId msgSlice timestamp
CALL $replay_protection_macro$ ; functionId msgSlice
SWAP                           ; msgSlice functionId
JMP 1
)");
}

void TVMFunctionCompiler::generateMainExternalForAbiV2() {
	m_pusher.generateInternal("main_external", -1);
	m_pusher.push(0, "PUSHINT -1 ; main_external trans id");
//		stack:
//		contract_balance
//		msg_balance is always zero
//		msg_cell
//		msg_body_slice
//		transaction_id = -1

	m_pusher.push(0, "PUSH S1");

	m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");

	checkSignatureAndReadPublicKey();
	if (m_pusher.ctx().afterSignatureCheck()) {
		// ... msg_cell msg_body_slice -1 rest_msg_body_slice
		m_pusher.push(0, "PUSH S3");
		CodeLines const& codeLines = m_pusher.ctx().m_inlinedFunctions.at("afterSignatureCheck");
		m_pusher.append(codeLines);
	} else {
		defaultReplayProtection();
		if (m_pusher.ctx().pragmaHelper().haveExpire()) {
			expire();
		}
	}

	callPublicFunction();
}

void TVMFunctionCompiler::pushMsgPubkey() {
	// signatureSlice msgSlice hashMsgSlice

	if (m_pusher.ctx().pragmaHelper().havePubkey()) {
		m_pusher.pushLines(R"(
	SWAP  ; signatureSlice hashMsgSlice msgSlice
	LDU 1 ; signatureSlice hashMsgSlice havePubkey msgSlice
	SWAP  ; signatureSlice hashMsgSlice msgSlice havePubkey
	PUSHCONT {
		LDU 256       ; signatureSlice hashMsgSlice pubkey msgSlice
)");
		m_pusher.addTabs(2);
		m_pusher.exchange(0, 3); //  msgSlice hashMsgSlice pubkey signatureSlice
		m_pusher.exchange(0, 1); //  msgSlice hashMsgSlice signatureSlice pubkey
		m_pusher.subTabs(2);
		m_pusher.pushLines(R"(
	}
	PUSHCONT {
)");
		// signatureSlice hashMsgSlice msgSlice
		m_pusher.addTabs(2);
		m_pusher.exchange(0, 2); // msgSlice hashMsgSlice signatureSlice
		m_pusher.getGlob(2);
		m_pusher.subTabs(2);
		m_pusher.pushLines(R"(
	}
	IFELSE)");
	} else {
		// signatureSlice msgSlice hashMsgSlice
		m_pusher.addTabs();
		m_pusher.push(0, "ROT"); // msgSlice hashMsgSlice signatureSlice
		m_pusher.getGlob(2);
		m_pusher.subTabs();
	}

	TVMScanner sc{*m_pusher.ctx().getContract()};
	if (sc.haveMsgPubkey) {
		m_pusher.addTabs();
		m_pusher.pushS(0);
		m_pusher.push(-1, "SETGLOB 5");
		m_pusher.subTabs();
	}

	// msgSlice hashMsgSlice signatureSlice pubkey
}

void TVMFunctionCompiler::checkSignatureAndReadPublicKey() {
	// msgSlice

	m_pusher.pushLines(R"(
LDU 1 ; haveSign msgSlice
SWAP
PUSHCONT {
	PUSHINT 512
	LDSLICEX ; signatureSlice msgSlice
	DUP      ; signatureSlice msgSlice msgSlice
	HASHSU   ; signatureSlice msgSlice hashMsgSlice
)");

	pushMsgPubkey();

	m_pusher.pushLines(R"(
	CHKSIGNU      ; msgSlice isSigned
	THROWIFNOT 40 ; msgSlice)");


	if (m_pusher.ctx().pragmaHelper().havePubkey()) {
		// External inbound message have not signature but have public key
		m_pusher.pushLines(R"(
}
PUSHCONT {
	LDU 1      ; havePubkey msgSlice
	SWAP       ; msgSlice havePubkey
	THROWIF 58 ; msgSlice
}
IFELSE
)");
	} else {
		m_pusher.pushLines(R"(
}
IF
)");
	}
}

void TVMFunctionCompiler::defaultReplayProtection() {
	// msgSlice
	m_pusher.pushLines(R"(
LDU 64                         ; timestamp msgSlice
SWAP                           ; msgSlice timestamp
CALL $replay_protection_macro$ ; msgSlice)");
}

void TVMFunctionCompiler::expire() {
	m_pusher.pushLines(R"(
LDU 32  ; expireAt msgSlice
SWAP    ; msgSlice expireAt
NOW     ; msgSlice expireAt now
GREATER ; msgSlice expireAt>now)");
	m_pusher.pushLines("THROWIFNOT " + toString(TvmConst::RuntimeException::MessageIsExpired));
}

void TVMFunctionCompiler::callPublicFunction() {
	// msg_body
	std::string s = R"(
LDU  32 ; funcId body
SWAP    ; body funcId
CALL 1
GETGLOB 7
ISNULL
PUSHCONT {
	CALL $:fallback_without$
}
IF
)";
	fillInlineFunctionsAndConstants(s);
	m_pusher.pushLines(s);
}

void TVMFunctionCompiler::generateMainInternal() {
	std::string s = R"(
.internal-alias :main_internal,        0
.internal	:main_internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: main_internal
;; param: contract_balance
;; param: msg_balance
;; param: inbound_message
;; param: msg_body_slice
PUSHINT 0  ; main_external trans id


PUSH S1    ; body
SEMPTY     ; isEmpty
PUSHCONT {
	PUSH S2     ; inbound_message
	CTOS        ; inbound_message
	PUSHINT 3   ; inbound_message 3
	SDSKIPFIRST ; inbound_message'
	PLDU 1      ; bounced
	EQINT 1     ; bounced==1
	PUSHCONT {
		CALL $:onBounce$
	}
	PUSHCONT {
		CALL $:receive$
	}
	IFELSE
}
IFJMP

PUSH S1            ; body
LDUQ 32            ; [funcId] body' ok
PUSHCONT {
	CALL $:fallback$
}
IFNOTJMP
PUSH2 S1,S1        ; funcId body' funcId funcId
EQINT 0            ; funcId body' funcId funcId==0
PUSHCONT {
	CALL $:receive$
}
IFJMP
LESSINT first_fun_id            ; funcId body' funcId<first_fun_id
PUSH S2              ; funcId body' funcId<first_fun_id funcId
PUSHINT last_fun_id   ; funcId body' funcId<first_fun_id funcId last_fun_id
GREATER              ; funcId body' funcId<first_fun_id funcId>last_fun_id
OR                   ; funcId body' funcId<first_fun_id||funcId>last_fun_id
PUSHCONT {
	CALL $:fallback$
}
IFJMP

SWAP  ; bodyLen body' funcId
CALL 1

GETGLOB 7
ISNULL
PUSHCONT {
	CALL $:fallback_without$
}
IF
)";
	fillInlineFunctionsAndConstants(s);
	m_pusher.pushLines(s);
	m_pusher.push(0, " ");
}

void TVMFunctionCompiler::fillInlineFunctionsAndConstants(std::string &pattern) {
	boost::replace_all(pattern, "first_fun_id", toString(TvmConst::FunctionId::First));
	boost::replace_all(pattern, "last_fun_id", toString(TvmConst::FunctionId::Last));

	for (std::size_t tabsQty = 2; tabsQty >= 1; --tabsQty) {
		const std::string tab(tabsQty, '\t');

		if (m_pusher.ctx().haveReceiveFunction()) {
			boost::replace_all(pattern, tab + "CALL $:receive$", m_pusher.ctx().m_inlinedFunctions.at("receive").str(tab));
		} else {
			if (m_pusher.ctx().haveFallbackFunction()) {
				boost::replace_all(pattern, tab + "CALL $:receive$", m_pusher.ctx().m_inlinedFunctions.at("fallback").str(tab));
			} else {
				boost::replace_all(pattern, tab + "CALL $:receive$", tab + "THROW 59");
			}
		}

		if (m_pusher.ctx().haveFallbackFunction()) {
			boost::replace_all(pattern, tab + "CALL $:fallback_without$", m_pusher.ctx().m_inlinedFunctions.at("fallback_without").str(tab));
			boost::replace_all(pattern, tab + "CALL $:fallback$", m_pusher.ctx().m_inlinedFunctions.at("fallback").str(tab));
		} else {
			boost::replace_all(pattern, tab + "CALL $:fallback_without$", tab + "THROW 60");
			boost::replace_all(pattern, tab + "CALL $:fallback$", tab + "THROW 60");
		}

		if (m_pusher.ctx().haveOnBounceHandler()) {
			boost::replace_all(pattern, tab + "CALL $:onBounce$", m_pusher.ctx().m_inlinedFunctions.at("onBounce").str(tab));
		} else {
			boost::replace_all(pattern, tab + "CALL $:onBounce$", "");
		}
	}
}

bool TVMFunctionCompiler::visit(PlaceholderStatement const &) {
	TVMFunctionCompiler tvm{m_pusher, m_isPublic, m_currentModifier + 1, m_function, m_pusher.getStack().size()};
	tvm.visitFunctionWithModifiers();
	return false;
}

