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

#include <liblangutil/SourceReferenceExtractor.h>

#include "DictOperations.hpp"
#include "TVMABI.hpp"
#include "TVMAnalyzer.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMConstants.hpp"

using namespace solidity::frontend;
namespace fs = boost::filesystem;


ContInfo getInfo(const Statement &statement) {
	LoopScanner scanner(statement);
	ContInfo info = scanner.m_info;
	info.alwaysReturns = doesAlways<Return>(&statement);
	info.alwaysContinue = doesAlways<Continue>(&statement);
	info.alwaysBreak = doesAlways<Break>(&statement);
	return info;
}

TVMFunctionCompiler::TVMFunctionCompiler(StackPusherHelper &pusher, ContractDefinition const *contract) :
	m_pusher{pusher},
	m_contract{contract}
{

}

TVMFunctionCompiler::TVMFunctionCompiler(
	StackPusherHelper &pusher,
	int modifier,
	FunctionDefinition const *f,
	bool isLibraryWithObj,
	bool pushArgs,
	int startStackSize
) :
	m_pusher{pusher},
	m_startStackSize{startStackSize},
	m_currentModifier{modifier},
	m_function{f},
	m_contract{m_function->annotation().contract},
	m_isLibraryWithObj{isLibraryWithObj},
	m_pushArgs{pushArgs}
{

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
	int delta = m_pusher.stackSize() - m_controlFlowInfo.back().stackSize;
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

Pointer<Function>
TVMFunctionCompiler::generateC4ToC7(StackPusherHelper& pusher) {
	pusher.pushC4();
	pusher.push(-1 + 1, "CTOS");
	pusher.push(-1 + 2, "LDU 256      ; pubkey c4");
	if (pusher.ctx().storeTimestampInC4()) {
		pusher.push(+1, "LDU 64       ; pubkey timestamp c4");
	}
	pusher.push(+1, "LDU 1      ; ctor flag");
	pusher.dropUnder(1, 1); // ignore
	if (pusher.ctx().usage().hasAwaitCall()) {
		pusher.push(-1 + 2, "LDI 1       ; await flag");
		pusher.dropUnder(1, 1);
	}
	if (!pusher.ctx().notConstantStateVariables().empty()) {
		pusher.getStack().change(+1); // slice
		// slice on stack
		std::vector<Type const *> stateVarTypes = pusher.ctx().notConstantStateVariableTypes();
		const int ss = pusher.stackSize();
		ChainDataDecoder decoder{&pusher};
		decoder.decodeData(stateVarTypes, pusher.ctx().getOffsetC4(), true,
						   pusher.ctx().usage().hasAwaitCall() ? 1 : 0);
		for (int i = stateVarTypes.size() - 1; i >= 0; --i) {
			pusher.setGlob(TvmConst::C7::FirstIndexForVariables + i);
		}
		solAssert(ss - 1 == pusher.stackSize(), "");
	} else {
		pusher.push(-1, "ENDS");
	}

	if (pusher.ctx().storeTimestampInC4()) {
		pusher.setGlob(TvmConst::C7::ReplayProtTime);
	}

	pusher.push(+1, ""); // fix stack
	pusher.setGlob(TvmConst::C7::TvmPubkey);

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>("c4_to_c7", Function::FunctionType::Macro, block);
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateC4ToC7WithInitMemory(StackPusherHelper& pusher) {
	TVMFunctionCompiler funCompiler{pusher, pusher.ctx().getContract()};

	pusher.pushC4();
	pusher.push(-1 + 1, "CTOS");
	pusher.push(-1 + 1, "SBITS");
	pusher.push(-1 + 1, "GTINT 1");

	pusher.startContinuationFromRef();
	pusher.pushCall(0, 0, "c4_to_c7");
	pusher.endContinuation();

	pusher.startContinuation();
	pusher.pushInt(0);
	pusher.pushC4();
	pusher.push(0, "CTOS");
	pusher.push(0, "PLDDICT   ; D");

	int shift = 0;
	for (VariableDeclaration const* v : pusher.ctx().notConstantStateVariables()) {
		if (v->isStatic()) {
			pusher.pushInt(TvmConst::C4::PersistenceMembersStartIndex + shift++); // index
			pusher.pushS(1); // index dict
			pusher.getDict(getKeyTypeOfC4(), *v->type(), GetDictOperation::GetFromMapping);
		} else {
			pusher.pushDefaultValue(v->type());
		}
		pusher.setGlob(v);
	}

	pusher.pushInt(64);
	pusher.startOpaque();
	pusher.pushAsym("DICTUGET");
	pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::NoPubkeyInC4));
	pusher.endOpaque(3, 1);

	pusher.push(0, "PLDU 256");
	pusher.setGlob(TvmConst::C7::TvmPubkey);
	pusher.push(+1, "PUSHINT 0 ; timestamp");
	pusher.setGlob(TvmConst::C7::ReplayProtTime);

	for (VariableDeclaration const *variable: pusher.ctx().notConstantStateVariables()) {
		if (auto value = variable->value().get()) {
			funCompiler.acceptExpr(value);
			pusher.setGlob(variable);
		}
	}
	pusher.endContinuation();
	pusher.ifElse();

	auto f = createNode<Function>("c4_to_c7_with_init_storage", Function::FunctionType::Macro, pusher.getBlock());
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateMacro(
	StackPusherHelper& pusher,
	FunctionDefinition const* function,
	const std::optional<std::string>& forceName
) {
	std::string name = forceName.has_value() ? forceName.value() : function->name();
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
    funCompiler.pushLocation(*function);
	funCompiler.visitFunctionWithModifiers();
    funCompiler.pushLocation(*function, true);
	return createNode<Function>(name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateOnCodeUpgrade(StackPusherHelper& pusher, FunctionDefinition const* function) {
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	funCompiler.visitFunctionWithModifiers();

	pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");

	pusher.push(0, "COMMIT");
	pusher._throw("THROW 0");
	return createNode<Function>("onCodeUpgrade", Function::FunctionType::OnCodeUpgrade, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateOnTickTock(StackPusherHelper& pusher, FunctionDefinition const* function) {
	pusher.pushInt(-2);
	pusher.push(-1, ""); // fix stack

	solAssert(function->parameters().size() == 1, "");
	const ASTPointer<VariableDeclaration>& variable = function->parameters().at(0);
	pusher.pushS(2);
	pusher.getStack().add(variable.get(), false);

	bool isPure = function->stateMutability() == StateMutability::Pure;
	if (!isPure) {
		pusher.pushMacroCallInCallRef(0, 0, "c4_to_c7");
	}

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	funCompiler.setGlobSenderAddressIfNeed();
	funCompiler.visitFunctionWithModifiers();

	if (!isPure) {
		pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
	}
	return createNode<Function>("onTickTock", Function::FunctionType::OnTickTock, pusher.getBlock());
}

void TVMFunctionCompiler::decodeFunctionParamsAndLocateVars(bool isResponsible) {
	// decode function params
	// stack: transaction_id arguments-in-slice
	m_pusher.push(+1, ""); // arguments-in-slice
	vector<Type const*> types = getParams(m_function->parameters()).first;
	ChainDataDecoder{&m_pusher}.decodePublicFunctionParameters(types, isResponsible);
	// stack: transaction_id arguments...
	m_pusher.getStack().change(-static_cast<int>(m_function->parameters().size()));
	for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
		auto name = variable->name();
		m_pusher.getStack().add(variable.get(), true);
	}
}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunction(StackPusherHelper& pusher, FunctionDefinition const* function) {
	/* stack:
	 * transaction data (see internal or external main)
	 * function result
	 * [send int/ext msg]
	 */

	std::string name = function->name();
	Function::FunctionType type = Function::FunctionType::Macro;
	Pointer<CodeBlock> block;

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	pusher.push(+1, ""); // fix stack
	pusher.drop(); // drop function id
	pusher.checkCtorCalled();
	funCompiler.pushC4ToC7IfNeed();

    funCompiler.pushLocation(*function);
	const bool isResponsible = function->isResponsible();
	if (isResponsible) {
		const int saveStakeSize = pusher.stackSize();
		pusher.push(+1, "LDU 32"); // callbackId slice
		pusher.getGlob(TvmConst::C7::ReturnParams); // callbackId slice c7[4]
		pusher.blockSwap(1, 2); // slice c7[4] callbackId
		pusher.setIndexQ(TvmConst::C7::ReturnParam::CallbackFunctionId); // slice c7[4]
		pusher.setGlob(TvmConst::C7::ReturnParams); // slice
		solAssert(saveStakeSize == pusher.stackSize(), "");
	}
	funCompiler.decodeFunctionParamsAndLocateVars(isResponsible);
    funCompiler.pushLocation(*function, true);

	int paramQty = function->parameters().size();
	int retQty = function->returnParameters().size();
	if (function->visibility() == Visibility::External) {
		funCompiler.visitFunctionWithModifiers();
	} else {
		pusher.pushMacroCallInCallRef(paramQty, retQty, pusher.ctx().getFunctionInternalName(function) + "_macro");
	}

	// emit
	funCompiler.emitOnPublicFunctionReturn();

	pusher.ensureSize(0, "");

	funCompiler.pushC7ToC4IfNeed();
	// set flag meaning function is called
	pusher.push(+1, "TRUE");
	pusher.setGlob(TvmConst::C7::WasPubFuncCalled);

	block = pusher.getBlock();
	return createNode<Function>(name, type, block);
}

void TVMFunctionCompiler::generateFunctionWithModifiers(
	StackPusherHelper& pusher,
	FunctionDefinition const* function,
	bool pushArgs
) {
	// TODO it can return: inline or constructor
	int ss = pusher.stackSize();
	if (!pushArgs) {
		ss -= function->parameters().size();
	}
	TVMFunctionCompiler compiler{pusher, 0, function, false, pushArgs, ss};
	compiler.visitFunctionWithModifiers();
}

Pointer<Function>
TVMFunctionCompiler::generateGetter(StackPusherHelper &pusher, VariableDeclaration const* vd) {
	TVMFunctionCompiler funCompiler{pusher, nullptr};
	pusher.push(+2, ""); // stack: functionId msgBody
	pusher.drop(); // drop function id
	pusher.push(-1, "ENDS");
	pusher.pushMacroCallInCallRef(0, 0, "c4_to_c7");
	pusher.getGlob(vd);

	// check ext msg
	pusher.pushS(1);
	pusher.startContinuation();
	pusher.push(-1, ""); // fix stack

	const std::vector<VariableDeclaration const*> outputs = {vd};
	auto appendBody = [&](int builderSize) {
		ChainDataEncoder{&pusher}.createMsgBodyAndAppendToBuilder(
				{vd},
				ChainDataEncoder{&pusher}.calculateFunctionIDWithReason(vd->name(), {}, &outputs, ReasonOfOutboundMessage::FunctionReturnExternal, {}, false),
				{},
				builderSize
		);
	};
	pusher.sendMsg({}, {}, appendBody, nullptr, nullptr, StackPusherHelper::MsgType::ExternalOut);

	// check ext msg
	pusher.endContinuation();
	pusher._if();

	pusher.push(+1, "TRUE");
	pusher.setGlob(TvmConst::C7::WasPubFuncCalled);

	return createNode<Function>(vd->name(), Function::FunctionType::Macro, pusher.getBlock());

}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunctionSelector(StackPusherHelper& pusher, ContractDefinition const *contract) {
	const std::vector<std::pair<uint32_t, std::string>>& functions = pusher.ctx().getPublicFunctions();
	TVMFunctionCompiler compiler{pusher, contract};
	compiler.buildPublicFunctionSelector(functions, 0, functions.size());
	return createNode<Function>("public_function_selector", Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generatePrivateFunction(StackPusherHelper& pusher, const std::string& name) {
	const std::string macroName = name + "_macro";
	pusher.pushCall(0, 0, macroName);
	return createNode<Function>(name, Function::FunctionType::PrivateFunction, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateLibraryFunction(
	StackPusherHelper& pusher,
	FunctionDefinition const* function,
	const std::string& name
) {
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	const std::string macroName = name + "_macro";
	pusher.pushCall(0, 0, macroName);
	return createNode<Function>(name, Function::FunctionType::PrivateFunction, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateLibraryFunctionMacro(
		StackPusherHelper& pusher,
		FunctionDefinition const* function,
		const std::string& name
) {
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	funCompiler.visitFunctionWithModifiers();
	return createNode<Function>(name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateReceive(StackPusherHelper& pusher, FunctionDefinition const* function) {
	return generateReceiveOrFallbackOrOnBounce(pusher, function, "receive_macro");
}

Pointer<Function> TVMFunctionCompiler::generateFallback(StackPusherHelper& pusher, FunctionDefinition const* function) {
	return generateReceiveOrFallbackOrOnBounce(pusher, function, "fallback_macro");
}

Pointer<Function> TVMFunctionCompiler::generateOnBounce(StackPusherHelper &pusher, const FunctionDefinition *function) {
	return generateReceiveOrFallbackOrOnBounce(pusher, function, "on_bounce_macro");
}

Pointer<Function> TVMFunctionCompiler::generateReceiveOrFallbackOrOnBounce(
	StackPusherHelper& pusher,
	FunctionDefinition const* function,
	const std::string& name
) {
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	pusher.checkCtorCalled();
	funCompiler.pushC4ToC7IfNeed();
	funCompiler.visitFunctionWithModifiers();
	funCompiler.pushC7ToC4IfNeed();
	return createNode<Function>(name, Function::FunctionType::Macro, pusher.getBlock());
}

// pop params.size() elements from stack top
void TVMFunctionCompiler::emitOnPublicFunctionReturn() {
	const int stackSize = m_pusher.stackSize();

	const std::vector<ASTPointer<VariableDeclaration>>& params = m_function->returnParameters();
	if (params.empty()) {
		return;
	}

	std::vector<VariableDeclaration const *> ret;
	if (m_function->returnParameterList() != nullptr) {
		ret = convertArray(m_function->returnParameters());
	}

	m_pusher.pushS(m_pusher.stackSize());
	m_pusher.push(-1, ""); // fix stack
	bool isResponsible = m_pusher.ctx().getCurrentFunction()->isResponsible();

	// emit for ext
	m_pusher.startContinuation();
	{
		auto appendBody = [&](int builderSize) {
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				ret,
				ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(m_function,
																		  ReasonOfOutboundMessage::FunctionReturnExternal),
				{},
				builderSize
			);
		};

		//	ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt
		//	import_fee:Grams = CommonMsgInfo;

		// get external address of sender
		m_pusher.pushS(m_pusher.stackSize() + 2);
		m_pusher.push(0, "CTOS");
		m_pusher.push(+1, "LDU 2");
		m_pusher.push(+1, "LDMSGADDR");
		m_pusher.drop();
		m_pusher.popS(1);

		m_pusher.sendMsg(
			{TvmConst::ext_msg_info::dest},
			{},
			appendBody,
			nullptr,
			nullptr,
			StackPusherHelper::MsgType::ExternalOut
		);
		m_pusher.push(params.size(), ""); // fix stack
	}
	m_pusher.endContinuation();


	m_pusher.startContinuation();
	if (!isResponsible) {
		m_pusher.drop(params.size());
	} else {
		auto pushFunction = [&](){
			m_pusher.getGlob(TvmConst::C7::ReturnParams);
			m_pusher.index(TvmConst::C7::ReturnParam::CallbackFunctionId);
		};

		auto appendBody = [&](int builderSize) {
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				ret,
				pushFunction,
				{},
				builderSize
			);
		};

		std::function<void()> pushSendrawmsgFlag = [&]() {
			m_pusher.getGlob(TvmConst::C7::ReturnParams);
			m_pusher.index(TvmConst::C7::ReturnParam::Flag);
		};

		m_pusher.getGlob(TvmConst::C7::ReturnParams);
		for (int i = 0; i < 3; ++i) {
			if (i == 2) {
				m_pusher.getGlob(TvmConst::C7::SenderAddress); // dest
				m_pusher.blockSwap(1, 3);
			} else {
				m_pusher.pushS(i);
			}
			m_pusher.index(3 - i);
		}
		// stack: currencies tons dest bounce
		m_pusher.sendMsg(
			{
				TvmConst::int_msg_info::bounce,
				TvmConst::int_msg_info::dest,
				TvmConst::int_msg_info::tons,
				TvmConst::int_msg_info::currency
			},
			{},
			appendBody,
			nullptr,
			pushSendrawmsgFlag,
			StackPusherHelper::MsgType::Internal
		);
		m_pusher.drop(m_pusher.stackSize());
	}
	m_pusher.endContinuation();

	m_pusher.ifElse();

	solAssert(stackSize == int(m_pusher.stackSize()) + int(params.size()), "");
}

void TVMFunctionCompiler::visitModifierOrFunctionBlock(Block const &body, bool isFunc) {
	LocationReturn locationReturn = ::notNeedsPushContWhenInlining(body);

	bool doPushContinuation = locationReturn == LocationReturn::Anywhere;
	if (doPushContinuation) {
		m_pusher.startCallX();
	}
	body.accept(*this);
	if (locationReturn == LocationReturn::Last) {
        m_pusher.pollLastRetOpcode();
    }
	if (doPushContinuation) {
		pushLocation(*m_function);
		m_pusher.endCallX();
        pushLocation(*m_function, true);
	}

	if (isFunc) {
		const int restSlots = m_pusher.stackSize() - m_startStackSize;
		solAssert(restSlots == 0, "");
		const int retQty = m_function->returnParameters().size();
		if (!isFunctionOfFirstType(m_function)) {
			m_pusher.push(retQty, ""); // fix stack
		}
	}

}

/* stack:
 * function params
 * return named params
 * stack of modifer0
 * stack of modifer1
 * stack of modifer2
 * ....
 * stack of function
 * ....
 * rest stack of modifer2 [drop stack modifer2]
 * rest stack of modifer1 [drop stack modifer1]
 * rest stack of modifer0 [drop stack modifer0]
 * [leave only return params]
 */

/* stack:
 * function params
 * ....
 * stack of function
 * ....
 * [leave only return params]
 */
void TVMFunctionCompiler::visitFunctionWithModifiers() {
	const int retQty = m_function->returnParameters().size();

	// inits function params and return named params
	if (m_currentModifier == 0) {
		if (m_pushArgs) {
			solAssert(m_startStackSize == 0, "");
			m_pusher.pushParameter(m_function->parameters());
		} else {
			solAssert(m_startStackSize >= 0, "");
		}
		if (isFunctionOfFirstType(m_function)) {
			pushDefaultParameters(m_function->returnParameters());
		}

		solAssert(!m_function->externalMsg() || !m_function->internalMsg(), "");

		if (m_function->externalMsg() || m_function->internalMsg())
			m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -5",
				"PICK",
			}, 0, 1));

		if (m_function->externalMsg()) {
			m_pusher.push(-1 + 1, "EQINT -1");
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ByExtMsgOnly));
		} else if (m_function->internalMsg()) {
			m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ByIntMsgOnly));
		}
	}


	if (m_currentModifier == static_cast<int>(functionModifiers().size())) {
		const int nextStartStack = m_pusher.stackSize();
		TVMFunctionCompiler funCompiler{m_pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, nextStartStack};
		const std::string text = (m_function->isConstructor()? "constructor " : "function ") + functionName(m_function);
		// accept function body
		funCompiler.visitModifierOrFunctionBlock(m_function->body(), true);
		solAssert(nextStartStack <= m_pusher.stackSize(), "");
	} else {
		int ss = m_pusher.stackSize();
		ModifierInvocation const *invocation = functionModifiers()[m_currentModifier].get();
		auto modifierDefinition = to<ModifierDefinition>(invocation->name()->annotation().referencedDeclaration);
		ast_vec<Expression> const *args = invocation->arguments();
		int modParamQty{};
		if (args != nullptr) {
			modParamQty = args->size();
			for (int i = 0; i < modParamQty; ++i) {
				const ASTPointer<Expression> &arg = (*args)[i];
				TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
				m_pusher.getStack().add(modifierDefinition->parameters()[i].get(), false);
			}
		}
		const int nextStartStack = m_pusher.stackSize();
		TVMFunctionCompiler funCompiler{m_pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, nextStartStack};
		funCompiler.visitModifierOrFunctionBlock(modifierDefinition->body(), false);
		m_pusher.drop(modParamQty);
		solAssert(ss == m_pusher.stackSize(), "");
	}

	if (m_currentModifier == 0) {
		// drop function params
		const int dropQty = (m_pusher.stackSize() - m_startStackSize) - retQty - (m_isLibraryWithObj ? 1 : 0);
		m_pusher.dropUnder(retQty, dropQty);

		if (m_isLibraryWithObj)
			solAssert(m_pusher.stackSize() ==
					  m_startStackSize + static_cast<int>(m_function->returnParameters().size()) + 1, "");
		else
			solAssert(m_pusher.stackSize() ==
					  m_startStackSize + static_cast<int>(m_function->returnParameters().size()), "");
	}
}

void TVMFunctionCompiler::pushDefaultParameters(const ast_vec<VariableDeclaration> &returnParameters) {
	int idParam = 0;
	for (const ASTPointer<VariableDeclaration>& returnParam: returnParameters) {
		auto name = returnParam->name();
		if (name.empty()) {
			name = "retParam@" + std::to_string(idParam);
		}
		m_pusher.pushDefaultValue(returnParam->type());
		m_pusher.getStack().add(returnParam.get(), false);

		++idParam;
	}
}

void TVMFunctionCompiler::acceptExpr(const Expression *expr, const bool isResultNeeded) {
	solAssert(expr, "");
	TVMExpressionCompiler(m_pusher).acceptExpr(expr, isResultNeeded);
}

bool TVMFunctionCompiler::visit(VariableDeclarationStatement const &_variableDeclarationStatement) {
	const int saveStackSize = m_pusher.stackSize();

	ast_vec<VariableDeclaration> decls = _variableDeclarationStatement.declarations();
	if (auto init = _variableDeclarationStatement.initialValue()) {
		auto tupleExpression = to<TupleExpression>(init);
		if (tupleExpression && !tupleExpression->isInlineArray()) {
			ast_vec<Expression> const&  tuple = tupleExpression->components();
			for (std::size_t i = 0; i < tuple.size(); ++i) {
				acceptExpr(tuple[i].get());
				if (decls.at(i) != nullptr) {
					m_pusher.hardConvert(decls.at(i)->type(), tuple.at(i)->annotation().type);
				}
			}
		} else {
			acceptExpr(init);
			if (decls.size() == 1) {
				m_pusher.hardConvert(decls.at(0)->type(), init->annotation().type);
			} else {
				auto tuple = to<TupleType>(init->annotation().type);
				for (int i = decls.size() - 1; i >= 0; --i) {
					if (decls.at(i) != nullptr) {
						m_pusher.hardConvert(decls.at(i)->type(), tuple->components().at(i));
					}
					m_pusher.blockSwap(decls.size() - 1, 1);
					// TODO add test
				}
			}
		}
	} else {
		for (const auto& decl : decls) {
			m_pusher.pushDefaultValue(decl->type());
		}
	}

	m_pusher.getStack().change(-(int)decls.size());
	for (const ASTPointer<VariableDeclaration>& d : decls) {
		if (d != nullptr) {
			m_pusher.getStack().add(d.get(), true);
		} else {
			m_pusher.getStack().change(+1);
		}
	}
	m_pusher.ensureSize(saveStackSize + static_cast<int>(decls.size()), "VariableDeclarationStatement", &_variableDeclarationStatement);
	return false;
}

bool TVMFunctionCompiler::visit(Block const & _block) {
	const int startStackSize = m_pusher.stackSize();
	for (const ASTPointer<Statement>& s : _block.statements()) {
        pushLocation(*s.get());
		s->accept(*this);
	}

	const int delta = m_pusher.stackSize() - startStackSize;
	solAssert(delta >= 0, "");
	if (!_block.statements().empty() && to<Return>(_block.statements().back().get()) == nullptr) {
		m_pusher.drop(delta);
	} else {
		m_pusher.push(-delta, ""); // fix stack
	}
    pushLocation(_block, true);
	return false;
}

bool TVMFunctionCompiler::visit(ExpressionStatement const &_statement) {
	if (!_statement.expression().annotation().isPure) {
	    pushLocation(_statement);
		auto savedStackSize = m_pusher.stackSize();
		acceptExpr(&_statement.expression(), false);
		m_pusher.ensureSize(savedStackSize, _statement.location().text());
        pushLocation(_statement, true);
	}
	return false;
}

bool TVMFunctionCompiler::visit(IfStatement const &_ifStatement) {
	const int saveStackSize = m_pusher.stackSize();

	// header
	ContInfo ci = getInfo(_ifStatement);
	bool canUseJmp = _ifStatement.falseStatement() != nullptr ?
					 getInfo(_ifStatement.trueStatement()).doThatAlways() &&
					 getInfo(*_ifStatement.falseStatement()).doThatAlways() :
					 getInfo(_ifStatement.trueStatement()).doThatAlways();
	if (canUseJmp) {
		ControlFlowInfo info{};
		info.stackSize = m_pusher.stackSize();
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
			m_pusher.ifElse(true);
		} else {
			m_pusher.ifElse();
		}
	} else {
		if (canUseJmp) {
			m_pusher.ifJmp();
		} else {
			m_pusher._if();
		}
        pushLocation(_ifStatement, true);
	}

	m_controlFlowInfo.pop_back();

	if (!canUseJmp) {
		// bottom
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			if (ci.canReturn) {
				if (allJmp()) { // no loops, only if-else
					m_pusher.push(0, "EQINT " + toString(ContInfo::RETURN_FLAG));
					m_pusher.ifret();
				} else {
					m_pusher.pushS(0);
					m_pusher.ifret();
					m_pusher.drop();
				}
			} else {
				m_pusher.pushS(0);
				m_pusher.ifret(); // if case 'break' or 'continue' flag before `if` is dropped
				m_pusher.drop(); // drop flag before 'if'
			}
		}
	}
	m_pusher.ensureSize(saveStackSize, "");

	return false;
}

ControlFlowInfo
TVMFunctionCompiler::pushControlFlowFlagAndReturnControlFlowInfo(ContInfo &ci, bool isLoop) {
	ControlFlowInfo info {};
	info.isLoop = isLoop;
	info.stackSize = -1;
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		m_pusher.push(+1, "FALSE ; decl return flag"); // break flag
	}
	info.stackSize = m_pusher.stackSize();
	return info;
}

void TVMFunctionCompiler::doWhile(WhileStatement const &_whileStatement) {
	int saveStackSize = m_pusher.stackSize();

	// header
	ContInfo ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_whileStatement.body());

	// body
	m_pusher.startContinuation();
	if (ci.canReturn || ci.canBreak || ci.canContinue) {
		int ss = m_pusher.stackSize();
		m_pusher.startCallX();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
		m_pusher.endCallX();
	} else {
		int ss = m_pusher.stackSize();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
	}
	// condition
	if (ci.canBreak || ci.canReturn) {
		m_pusher.pushS(0);
		m_pusher.push(0, "GTINT 1");
		m_pusher.pushS(0);
		m_pusher.push(-2, ""); // fix stack

		m_pusher.startContinuation();
		m_pusher.push(+1, ""); // fix stack
		m_pusher.drop();
		acceptExpr(&_whileStatement.condition(), true);
		m_pusher.push(0, "NOT");
		m_pusher.endContinuation();

		m_pusher._ifNot();
	} else {
		acceptExpr(&_whileStatement.condition(), true);
		m_pusher.push(0, "NOT");
	}
	m_pusher.push(-1, ""); // drop condition
	m_pusher.endContinuation();

	m_pusher.until();

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0);

	m_pusher.ensureSize(saveStackSize, "");
}

void
TVMFunctionCompiler::visitForOrWhileCondition(
	const ContInfo &ci,
	const ControlFlowInfo &info,
    const std::function<void()>& pushCondition
) {
	int stackSize = m_pusher.stackSize();
	m_pusher.startContinuation();
	if (ci.canBreak || ci.canReturn) {
		m_pusher.pushS(m_pusher.stackSize() - info.stackSize);
		m_pusher.push(0, "LESSINT 2");
		m_pusher.push(-1, ""); // fix stack

		if (pushCondition) {
			m_pusher.pushS(0);
			m_pusher.startContinuation();
			m_pusher.drop();
			pushCondition();
			m_pusher.endContinuation();
			m_pusher.push(-1, ""); // fix stack
			m_pusher._if();
		}
	} else {
		if (pushCondition) {
			pushCondition();
			m_pusher.push(-1, ""); // fix stack
		} else {
			m_pusher.push(+1, "TRUE");
			m_pusher.push(-1, ""); // fix stack
		}
	}
	m_pusher.endContinuation();
	m_pusher.ensureSize(stackSize, "visitForOrWhileCondition");
}

void TVMFunctionCompiler::afterLoopCheck(const ContInfo& ci, const int& loopVarQty) {
	int cntDrop = 0;
	cntDrop += loopVarQty;
	if (ci.canReturn || ci.canBreak || ci.canContinue) ++cntDrop;

	if (ci.canReturn) {
		if (allJmp()) { // no loops, only if-else
			m_pusher.push(0, "EQINT " + toString(ContInfo::RETURN_FLAG));
			m_pusher.ifret();
			if (loopVarQty > 0) {
				m_pusher.drop(loopVarQty);
			}
		} else {
			m_pusher.pushS(0);
			if (ci.canBreak || ci.canContinue) {
				m_pusher.push(0, "EQINT " + toString(ContInfo::RETURN_FLAG));
			}
			m_pusher.ifret();
			m_pusher.drop(cntDrop);
		}
	} else {
		m_pusher.drop(cntDrop);
	}
}

bool TVMFunctionCompiler::visit(WhileStatement const &_whileStatement) {
	int saveStackSizeForWhile = m_pusher.stackSize();

	if (_whileStatement.loopType() == WhileStatement::LoopType::DO_WHILE) {
		doWhile(_whileStatement);
		return false;
	}

	// header
	ContInfo ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_whileStatement.body());

	int saveStackSize = m_pusher.stackSize();

	// condition
	if (_whileStatement.loopType() == WhileStatement::LoopType::REPEAT) {
		if (ci.mayDoThat()) {
			cast_error(_whileStatement, "Using 'break', 'continue' or 'return' is not supported yet.");
		}
		acceptExpr(&_whileStatement.condition());
		m_pusher.push(-1, "");
	} else {
		std::function<void()> pushCondition = [&]() {
			acceptExpr(&_whileStatement.condition(), true);
		};
		visitForOrWhileCondition(ci, info, pushCondition);
	}

	m_pusher.ensureSize(saveStackSize, "while condition");

	// body
	m_pusher.startContinuation();
	_whileStatement.body().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - saveStackSize);
	m_pusher.endContinuation();

	if (_whileStatement.loopType() == WhileStatement::LoopType::REPEAT)
		m_pusher.repeat();
	else
		m_pusher._while();

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0);

	m_pusher.ensureSize(saveStackSizeForWhile, "");

	return false;
}

bool TVMFunctionCompiler::visit(ForEachStatement const& _forStatement) {
	// For bytes:
	//
	// cell
	// [return flag] - optional. If have return/break/continue.

	// For array:
	//
	// dict
	// index
	// value
	// [return flag] - optional. If have return/break/continue.

	// For mapping:
	//
	// dict
	// public key (can be changed in solidity code)
	// value
	// private key (not visible in solidity code)
	// [return flag] - optional. If have return/break/continue.

	const int saveStackSize = m_pusher.stackSize();
	TVMExpressionCompiler ec{m_pusher};
	ec.acceptExpr(_forStatement.rangeExpression(), true); // stack: dict

	// init
	auto arrayType = to<ArrayType>(_forStatement.rangeExpression()->annotation().type);
	auto mappingType = to<MappingType>(_forStatement.rangeExpression()->annotation().type);
	auto vds = to<VariableDeclarationStatement>(_forStatement.rangeDeclaration());
	int loopVarQty{};
	if (arrayType) {
		solAssert(vds->declarations().size() == 1, "");
		auto iterVar = vds->declarations().at(0).get();
		if (arrayType->isByteArray()) {
			m_pusher.push(0, "CTOS");
			m_pusher.pushNull(); // stack: dict value
			loopVarQty = 2;
		} else {
			m_pusher.index(1); // stack: {length, dict} -> dict
			m_pusher.pushInt(0); // stack: dict 0
			m_pusher.pushNull(); // stack: dict 0 value
			loopVarQty = 3;
		}
		m_pusher.getStack().add(iterVar, false);
		// stack: dict 0 value
	} else if (mappingType) {
		// stack: dict
		m_pusher.pushS(0); // stack: dict dict
		DictMinMax dictMinMax{m_pusher, *mappingType->keyType(), *mappingType->valueType(), true};
		dictMinMax.minOrMax(true);
		// stack: dict minKey(private) minKey(pub) value

		m_pusher.push(-2, ""); // fix stack
		auto iterKey = vds->declarations().at(0).get();
		auto iterVal = vds->declarations().at(1).get();
		if (iterKey == nullptr)
			m_pusher.push(+1, "");
		else
			m_pusher.getStack().add(iterKey, true);
		if (iterVal == nullptr)
			m_pusher.push(+1, "");
		else
			m_pusher.getStack().add(iterVal, true);

		// stack: dict minKey(pub) value minKey(private)
		loopVarQty = 4;
	} else {
		solUnimplemented("");
	}
	m_pusher.ensureSize(saveStackSize + loopVarQty, "for");

	// header
	ContInfo ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_forStatement.body());

	// condition
	std::function<void()> pushCondition = [&]() {
		if (arrayType) {
			if (arrayType->isByteArray()) {
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1); // stack: cell value [flag] cell
				m_pusher.push(-1 + 1, "SEMPTY");
				m_pusher.push(-1 + 1, "NOT");
			} else {
				// stack: dict index value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict index value [flag] index
				m_pusher.pushS(
						m_pusher.stackSize() - saveStackSize - 1); // stack: dict index value [flag] index dict
				m_pusher.getDict(getKeyTypeOfArray(), *arrayType->baseType(), GetDictOperation::Fetch);
				// stack: dict index value [flag] newValue
				m_pusher.pushS(0); // stack: dict index value [flag] newValue newValue
				m_pusher.popS(
						m_pusher.stackSize() - saveStackSize - 3); // stack: dict index newValue [flag] newValue
				m_pusher.push(-1 + 1, "ISNULL");
				m_pusher.push(-1 + 1, "NOT");
			}
		} else if (mappingType) {
			// stack: dict minKey(private) minKey(pub) value  [flag]
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2);
			m_pusher.push(-1 + 1, "ISNULL");
			m_pusher.push(-1 + 1, "NOT");
		} else {
			solUnimplemented("");
		}
	};
	visitForOrWhileCondition(ci, info, pushCondition);


	// body
	std::function<void()> pushStartBody = [&]() {
		if (arrayType) {
			if (arrayType->isByteArray()) {
				const int ss = m_pusher.stackSize();
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1);
				// stack: cell value [flag] cell

				m_pusher.startOpaque();
				m_pusher.pushAsym("LDUQ 8");
				m_pusher.push(+1, ""); // fix stack
				m_pusher.startContinuation();
				// stack: cell value [flag] slice
				m_pusher.push(-1 + 1, "PLDREF");
				m_pusher.push(-1 + 1, "CTOS");
				m_pusher.push(-1 + 2, "LDU 8");
				m_pusher.push(-2, ""); // fix stack
				m_pusher.endContinuation();
				m_pusher._ifNot();
				m_pusher.endOpaque(1, 2);

				solAssert(ss + 2 == m_pusher.stackSize(), "");
				// stack: cell value [flag] value cell
				m_pusher.popS(m_pusher.stackSize() - saveStackSize - 1);
				// stack: cell value [flag] value
				m_pusher.popS(m_pusher.stackSize() - saveStackSize - 2);
				// stack: cell value [flag]

				solAssert(ss == m_pusher.stackSize(), "");
			}
		}
	};
	std::function<void()> pushLoopExpression = [&]() {
		if (arrayType) {
			if (arrayType->isByteArray()) {
				// do nothing
			} else {
				// stack: dict 0 value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict index value [flag] index
				m_pusher.push(0, "INC"); // stack: dict index value [flag] newIndex
				m_pusher.popS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict newIndex [flag] value
			}
		} else if (mappingType) {
			const int sss = m_pusher.stackSize();
			// stack: dict minKey(private) minKey(pub) value [flag]
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict minKey(private) minKey(pub) value [flag] minKey
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1); // stack: dict minKey(private) minKey(pub) value [flag] minKey dict
			m_pusher.pushInt(lengthOfDictKey(mappingType->keyType()));      // stack: dict minKey(private) minKey(pub) value [flag] minKey dict nbits

			DictPrevNext dictPrevNext{m_pusher, *mappingType->keyType(), *mappingType->valueType(), "next"};
			dictPrevNext.prevNext(true);

			// stack: dict minKey(private) minKey(pub) value [flag] minKey(private) minKey(pub) value
			m_pusher.popS(m_pusher.stackSize() - saveStackSize - 4);
			m_pusher.popS(m_pusher.stackSize() - saveStackSize - 3);
			m_pusher.popS(m_pusher.stackSize() - saveStackSize - 2);
			solAssert(sss == m_pusher.stackSize(), "");
		} else {
			solUnimplemented("");
		}
	};
	visitBodyOfForLoop(ci, pushStartBody, _forStatement.body(), pushLoopExpression);

	// bottom
	afterLoopCheck(ci, loopVarQty);
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

std::pair<ContInfo, ControlFlowInfo> TVMFunctionCompiler::pushControlFlowFlag(Statement const& body) {
	ContInfo ci = getInfo(body);
	ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
	m_controlFlowInfo.push_back(info);
	return {ci, info};
}

void TVMFunctionCompiler::visitBodyOfForLoop(
	const ContInfo& ci,
	const std::function<void()>& pushStartBody,
	Statement const& body,
	const std::function<void()>& loopExpression
) {
	// body and loopExpression
	m_pusher.startContinuation();
	if (pushStartBody) {
		pushStartBody();
	}
	if (ci.canReturn || ci.canBreak || ci.canContinue) { // TODO and have loopExpression
		int ss = m_pusher.stackSize();
		m_pusher.startCallX();
		body.accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
		m_pusher.endCallX();
		if (ci.canReturn || ci.canBreak) {
			solAssert(ContInfo::CONTINUE_FLAG == 1, "");
			m_pusher.pushS(0);
			m_pusher.push(-1 + 1, "GTINT " + toString(ContInfo::CONTINUE_FLAG));
			m_pusher.ifret();
		}
	} else {
		int ss = m_pusher.stackSize();
		body.accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
	}
	if (loopExpression) {
		// TODO optimization: don't pushcont if no loopExpression
		loopExpression();
	}
	m_pusher.endContinuation();
	m_pusher._while();
	m_controlFlowInfo.pop_back();
}

bool TVMFunctionCompiler::visit(ForStatement const &_forStatement) {

	// if in loop body there is at least one 'return', 'break' or `continue`:
	//
	// decl loop var - optional
	// return, break or continue flag  - optional
	// PUSHCONT {
	//     condition
	// }
	// PUSHCONT {
	//     PUSHCONT {
	//        body
	//     }
	//     CALLX
	//     check return flag
	//     loopExpression
	// }

	// in another cases:
	//
	// decl loop var - optional
	// PUSHCONT {
	//     condition
	// }
	// PUSHCONT {
	//     body
	//     loopExpression
	// }

	int saveStackSize = m_pusher.stackSize();
	// init
	bool haveDeclLoopVar = false;
	if (_forStatement.initializationExpression() != nullptr) {
		const int saveStack = m_pusher.stackSize();
		_forStatement.initializationExpression()->accept(*this);
		haveDeclLoopVar = m_pusher.stackSize() != saveStack;
	}

	// header
	ContInfo ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_forStatement.body());

	// condition
	std::function<void()> pushCondition;
	if (_forStatement.condition()) {
		pushCondition = [&](){
			acceptExpr(_forStatement.condition(), true);
		};
	}
	visitForOrWhileCondition(ci, info, pushCondition);

	// body and loopExpression
	std::function<void()> pushLoopExpression;
	if (_forStatement.loopExpression() != nullptr) {
		pushLoopExpression = [&]() {
			_forStatement.loopExpression()->accept(*this);
		};
	}
	visitBodyOfForLoop(ci, {}, _forStatement.body(), pushLoopExpression);

	// bottom
	afterLoopCheck(ci, haveDeclLoopVar);
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

bool TVMFunctionCompiler::visit(Return const &_return) {
	if (!_return.names().empty()) {
		m_pusher.getGlob(TvmConst::C7::ReturnParams);
		for (std::size_t i = 0; i < _return.names().size(); ++i) {
			acceptExpr(_return.options().at(i).get());
			const static std::map<std::string, int> nameToInt = {
				{"bounce", TvmConst::C7::ReturnParam::Bounce},
				{"value", TvmConst::C7::ReturnParam::Value},
				{"currencies", TvmConst::C7::ReturnParam::Currencies},
				{"flag", TvmConst::C7::ReturnParam::Flag}
			};
			const std::string& optionName = *_return.names().at(i);
			m_pusher.setIndexQ(nameToInt.at(optionName));
		}
		m_pusher.setGlob(TvmConst::C7::ReturnParams);
	}

	auto expr = _return.expression();
	if (expr) {
		acceptExpr(expr);
	}

	int retCount = 0;
	if (_return.annotation().functionReturnParameters != nullptr) {
		ast_vec<VariableDeclaration> const &params = _return.annotation().functionReturnParameters->parameters();
		retCount = params.size();
	}

	if (isFunctionOfFirstType(m_function)) {
		const int startRetParams = 0 + // TODO it's not true for constructors
									   m_function->parameters().size();
		int underInputParams = m_pusher.stackSize() - startRetParams;
		m_pusher.dropUnder(underInputParams - retCount, retCount);
		underInputParams = m_pusher.stackSize() - startRetParams;
		m_pusher.blockSwap(underInputParams - retCount, retCount);

		const int trashSlots = m_pusher.stackSize() - m_startStackSize;
		int revertDelta = trashSlots;
		m_pusher.drop(trashSlots);

		if (!allJmp()) {
			m_pusher.pushInt(4);
			--revertDelta;
			m_pusher.push(revertDelta, ""); // fix stack
		} else if (!m_controlFlowInfo.empty()) { // all continuation are run by JMPX
			m_pusher.push(revertDelta, ""); // fix stack
		}
		m_pusher.ret();
	} else {
		const int trashSlots = m_pusher.stackSize() - m_startStackSize;
		int revertDelta = trashSlots - retCount;
		m_pusher.dropUnder(retCount, trashSlots - retCount);

		if (!allJmp()) {
			m_pusher.pushInt(4);
			--revertDelta;
			m_pusher.push(revertDelta, ""); // fix stack
		} else if (!m_controlFlowInfo.empty()) { // all continuation are run by JMPX
			m_pusher.push(revertDelta, ""); // fix stack
		}
		m_pusher.ret();
	}
	return false;
}

void TVMFunctionCompiler::breakOrContinue(int code) {
	// TODO implement for repeat
	solAssert(code == 1 || code == 2, "");

	ControlFlowInfo controlFlowInfo;
	for (int i = static_cast<int>(m_controlFlowInfo.size()) - 1; ; --i) {
		if (m_controlFlowInfo.at(i).isLoop) {
			controlFlowInfo = m_controlFlowInfo.at(i);
			break;
		}
	}

	const int sizeDelta = m_pusher.stackSize() - controlFlowInfo.stackSize;
	m_pusher.drop(sizeDelta + 1);
	m_pusher.pushInt(code);
	m_pusher.ret();
	m_pusher.push(sizeDelta, ""); // fix stack
}

bool TVMFunctionCompiler::visit(Break const &) {
	breakOrContinue(ContInfo::BREAK_FLAG);
	return false;
}

bool TVMFunctionCompiler::visit(Continue const &) {
	breakOrContinue(ContInfo::CONTINUE_FLAG);
	return false;
}

bool TVMFunctionCompiler::visit(EmitStatement const &_emit) {
	auto eventCall = to<FunctionCall>(&_emit.eventCall());
	solAssert(eventCall, "");
	CallableDeclaration const * eventDef = getFunctionDeclarationOrConstructor(&eventCall->expression());
	solAssert(eventDef, "Event Declaration was not found");

	std::vector<ASTPointer<Expression const>> args = eventCall->arguments();
	for (ASTPointer<Expression const> const& arg : args | boost::adaptors::reversed) {
		TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
	}

	auto appendBody = [&](int builderSize) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			convertArray(eventDef->parameters()),
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(eventDef, ReasonOfOutboundMessage::EmitEventExternal),
			{},
			builderSize,
			true
		);
	};

	std::set<int> isParamOnStack;
	if (!_emit.names().empty()) {
		solAssert(_emit.names().size() == 1 && *_emit.names().at(0) == "dest", "");
		solAssert(_emit.options().size() == 1, "");
		isParamOnStack.insert(TvmConst::ext_msg_info::dest);
		acceptExpr(_emit.options().at(0).get(), true);
	}

	m_pusher.sendMsg(isParamOnStack, {}, appendBody, nullptr, nullptr, StackPusherHelper::MsgType::ExternalOut);
	return false;
}

Pointer<Function> TVMFunctionCompiler::generateMainExternal(StackPusherHelper& pusher, ContractDefinition const *contract) {
	TVMFunctionCompiler funCompiler{pusher, contract};
	Pointer<Function> f;
	switch (pusher.ctx().pragmaHelper().abiVersion()) {
		case AbiVersion::V1:
			f = funCompiler.generateMainExternalForAbiV1();
			break;
		case AbiVersion::V2_1:
			f = funCompiler.generateMainExternalForAbiV2();
			break;
		default:
			solUnimplemented("");
	}
	return f;
}

void TVMFunctionCompiler::setGlobSenderAddressIfNeed() {
	if (m_pusher.ctx().usage().hasMsgSender()) {
		m_pusher.push(+1, "PUSHSLICE x8000000000000000000000000000000000000000000000000000000000000000001_");
		m_pusher.setGlob(TvmConst::C7::SenderAddress);
	}
}

void TVMFunctionCompiler::setCtorFlag() {
	m_pusher.pushC4();
	m_pusher.push(0, "CTOS");
	m_pusher.push(0, "SBITS");
	m_pusher.push(0, "NEQINT 1");
	m_pusher.setGlob(TvmConst::C7::ConstructorFlag);
}

Pointer<Function> TVMFunctionCompiler::generateMainExternalForAbiV1() {
	// contract_balance msg_balance msg_cell origin_msg_body_slice
	setCtorFlag();
	setGlobSenderAddressIfNeed();

	m_pusher.pushS(1);
	m_pusher.push(+1, "LDREFRTOS  ; msgBodySlice signSlice");
	m_pusher.pushS(0);
	m_pusher.push(0, "SDEMPTY    ; msgBodySlice signSlice isSignSliceEmpty");
	m_pusher.startContinuation();
	m_pusher.drop();
	m_pusher.endContinuation();
	m_pusher.startContinuation();
	m_pusher.pushS(0);
	m_pusher.pushInt(512);
	m_pusher.push(-2 + 1, "SDSKIPFIRST  ; msgBodySlice signSlice signSlice'");
	m_pusher.push(+0, "PLDU 256     ; msgBodySlice signSlice pubKey");
	m_pusher.pushS(2);
	m_pusher.push(0, "HASHSU       ; msgBodySlice signSlice pubKey msgHash");
	m_pusher.pushS2(2, 1);
	m_pusher.push(-3 + 1, "CHKSIGNU     ; msgBodySlice signSlice pubKey isSigned");
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::BadSignature) + "; msgBodySlice signSlice pubKey");
	m_pusher.setGlob(TvmConst::C7::MsgPubkey);
	m_pusher.drop();
	m_pusher.endContinuation();
	m_pusher.ifElse();

	m_pusher.pushMacroCallInCallRef(0, 0, "c4_to_c7_with_init_storage");

	m_pusher.push(+1, "LDU 32                         ; functionId msgSlice");
	m_pusher.push(+1, "LDU 64                         ; functionId timestamp msgSlice");
	m_pusher.exchange(1);
	m_pusher.pushCall(1, 0, "replay_protection_macro");
	m_pusher.exchange(1); // msgSlice functionId

	callPublicFunctionOrFallback();

	return createNode<Function>("main_external", Function::FunctionType::MainExternal, m_pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateMainExternalForAbiV2() {
//		stack:
//		contract_balance
//		msg_balance is always zero
//		msg_cell
//		msg_body_slice
//		transaction_id = -1

	setCtorFlag();
	setGlobSenderAddressIfNeed();

	m_pusher.pushS(1);

	m_pusher.pushMacroCallInCallRef(0, 0, "c4_to_c7_with_init_storage");

	checkSignatureAndReadPublicKey();
	if (m_pusher.ctx().afterSignatureCheck()) {
		// ... msg_cell msg_body_slice -1 rest_msg_body_slice
		m_pusher.pushS(3);
		Pointer<CodeBlock> block = m_pusher.ctx().getInlinedFunction("afterSignatureCheck");
		m_pusher.pushInlineFunction(block, 2, 1);
	} else {
		defaultReplayProtection();
		if (m_pusher.ctx().pragmaHelper().haveExpire()) {
			expire();
		}
	}

	// msg_body
	m_pusher.push(+1, "LDU 32 ; funcId body");
	m_pusher.exchange(1);

	callPublicFunctionOrFallback();
	return createNode<Function>("main_external", Function::FunctionType::MainExternal, m_pusher.getBlock());
}

void TVMFunctionCompiler::pushMsgPubkey() {
	// signatureSlice msgSlice hashMsgSlice

	if (m_pusher.ctx().pragmaHelper().havePubkey()) {
		m_pusher.exchange(1);
		m_pusher.push(+1, "LDU 1 ; signatureSlice hashMsgSlice havePubkey msgSlice");
		m_pusher.exchange(1); // signatureSlice hashMsgSlice msgSlice havePubkey

		m_pusher.startContinuation();
		m_pusher.push(+1, "LDU 256       ; signatureSlice hashMsgSlice pubkey msgSlice");
		m_pusher.exchange(3); //  msgSlice hashMsgSlice pubkey signatureSlice
		m_pusher.exchange(1); //  msgSlice hashMsgSlice signatureSlice pubkey
		m_pusher.endContinuation();

		m_pusher.startContinuation();
		// signatureSlice hashMsgSlice msgSlice
		m_pusher.exchange(2); // msgSlice hashMsgSlice signatureSlice
		m_pusher.getGlob(TvmConst::C7::TvmPubkey);
		m_pusher.endContinuation();

		m_pusher.ifElse();
	} else {
		// signatureSlice msgSlice hashMsgSlice
		m_pusher.rot(); // msgSlice hashMsgSlice signatureSlice
		m_pusher.getGlob(TvmConst::C7::TvmPubkey);
	}

	if (m_pusher.ctx().usage().hasMsgPubkey()) {
		m_pusher.pushS(0);
		m_pusher.setGlob(TvmConst::C7::MsgPubkey);
	}

	// msgSlice hashMsgSlice signatureSlice pubkey
}

void TVMFunctionCompiler::checkSignatureAndReadPublicKey() {
	// msgSlice

	m_pusher.push(-1 + 2, "LDU 1 ; haveSign msgSlice");
	m_pusher.exchange(1);

	m_pusher.startContinuation();
	m_pusher.pushInt(512);
	m_pusher.push(-2 + 2, "LDSLICEX ; signatureSlice msgSlice");
	m_pusher.pushS(0);
	m_pusher.push(-1 + 1, "HASHSU   ; signatureSlice msgSlice hashMsgSlice");
	pushMsgPubkey();
	m_pusher.push(-3 + 1, "CHKSIGNU      ; msgSlice isSigned");
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::BadSignature) + " ; msgSlice");
	m_pusher.endContinuation();

	if (m_pusher.ctx().pragmaHelper().havePubkey()) {
		// External inbound message have not signature but have public key
		m_pusher.startContinuation();
		m_pusher.push(+1, "LDU 1      ; havePubkey msgSlice");
		m_pusher.exchange(1);
		m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::MessageHasNoSignButHasPubkey) + " ; msgSlice");
		m_pusher.endContinuation();
		m_pusher.ifElse();
	} else {
		m_pusher._if();
	}
}

void TVMFunctionCompiler::defaultReplayProtection() {
	// msgSlice
	m_pusher.push(+1, "LDU 64                         ; timestamp msgSlice");
	m_pusher.exchange(1);
	m_pusher.pushCall(1, 0, "replay_protection_macro");
}

void TVMFunctionCompiler::expire() {
	m_pusher.push(+1, "LDU 32  ; expireAt msgSlice");
	m_pusher.exchange(1);
	m_pusher.push(+1, "NOW     ; msgSlice expireAt now");
	m_pusher.push(-1, "GREATER ; msgSlice expireAt>now");
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::MessageIsExpired));
}

void TVMFunctionCompiler::callPublicFunctionOrFallback() {
	m_pusher.pushMacroCallInCallRef(0, 0, "public_function_selector");

	m_pusher.getGlob(TvmConst::C7::WasPubFuncCalled);
	m_pusher.push(0, "ISNULL");
	if (m_pusher.ctx().isFallBackGenerated()) {
		m_pusher.startContinuation();
		m_pusher.drop(2);
		m_pusher.startCallRef();
		m_pusher.pushCall(0, 0, "fallback_macro");
		m_pusher.endContinuation();
		m_pusher.endContinuation();
		m_pusher._if();
	} else {
		m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::NoFallback));
	}
}

Pointer<Function>
TVMFunctionCompiler::generateMainInternal(StackPusherHelper& pusher, ContractDefinition const *contract) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//                 src:MsgAddress  dest:MsgAddressInt(#4)
	//                 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//                 created_lt:uint64  created_at:uint32
	//                 = CommonMsgInfoRelaxed;

	TVMFunctionCompiler funCompiler{pusher, contract};
	funCompiler.setCtorFlag();

	pusher.pushS(2);
	pusher.push(-1 + 1, "CTOS");
	// stack: int_msg_info

	ContactsUsageScanner const &sc = pusher.ctx().usage();
	if (sc.hasMsgSender() || sc.hasResponsibleFunction() || sc.hasAwaitCall()) {
		pusher.push(-1 + 2, "LDU 4       ; bounced tail");
		pusher.push(-1 + 2, "LDMSGADDR   ; bounced src tail");
		pusher.drop();
		if (sc.hasAwaitCall()) {
			pusher.pushMacroCallInCallRef(0, 0, "check_resume");
		}
		pusher.setGlob(TvmConst::C7::SenderAddress);
		pusher.push(0, "MODPOW2 1");
	} else {
		pusher.push(-1 + 1, "PLDU 4");
		pusher.push(-1 + 1, "MODPOW2 1");
	}
	// stack: isBounced

	// set default params for responsible func
	if (sc.hasResponsibleFunction()) {
		pusher.getGlob(TvmConst::C7::ReturnParams);
		pusher.push(+1, "TRUE"); // bounce
		pusher.setIndexQ(TvmConst::C7::ReturnParam::Bounce);
		pusher.pushInt(TvmConst::Message::DefaultMsgValue); // tons
		pusher.setIndexQ(TvmConst::C7::ReturnParam::Value);
		pusher.pushNull(); // currency
		pusher.setIndexQ(TvmConst::C7::ReturnParam::Currencies);
		pusher.pushInt(TvmConst::SENDRAWMSG::DefaultFlag); // flag
		pusher.setIndexQ(TvmConst::C7::ReturnParam::Flag);
		pusher.setGlob(TvmConst::C7::ReturnParams);
	}

	// bounced
	if (!isEmptyFunction(contract->onBounceFunction())) {
		pusher.startIfJmpRef();
		pusher.pushS(1);
		pusher.push(-1 + 2, "LDSLICE 32");
		pusher.dropUnder(1, 1);
		pusher.pushCall(0, 0, "on_bounce_macro");
		pusher.endContinuation();
	} else {
		pusher.ifret();
	}

	funCompiler.pushReceiveOrFallback();

	pusher.exchange(1);
	funCompiler.callPublicFunctionOrFallback();

	return createNode<Function>("main_internal", Function::FunctionType::MainInternal, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateCheckResume(StackPusherHelper &pusher) {
	// TODO: unite check resume and c4_to_c7 for not to parse c4 2 times
	std::string code = R"(PUSHROOT
CTOS
PUSHINT offset
LDSLICEX  ; beg_slice end_slice
LDI 1
SWAP
PUSHCONT {
	LDREFRTOS   ; beg_slice end_slice ref_slice
	XCHG S2     ; ref_slice end beg
	NEWC
	STSLICE
	STZERO
	STSLICE
	ENDC
	POPROOT
	LDMSGADDR
	ROTREV
	SDEQ
	THROWIFNOT TvmConst::RuntimeException::WrongAwaitAddress
	LDCONT
	DROP
	NIP
	CALLREF {
		CALL $c4_to_c7$
	}
	CALLX
}
PUSHCONT {
	DROP2
}
IFELSE
)";
	boost::replace_all(code, "TvmConst::RuntimeException::WrongAwaitAddress", toString(TvmConst::RuntimeException::WrongAwaitAddress));
	boost::replace_all(code, "offset", toString(256 + (pusher.ctx().storeTimestampInC4() ? 64 : 0) + 1));
	vector<string> lines = split(code);
	pusher.push(createNode<HardCode>(lines, 0, 0));
	return createNode<Function>("check_resume", Function::FunctionType::Macro, pusher.getBlock());
}

bool TVMFunctionCompiler::visit(PlaceholderStatement const &) {
	TVMFunctionCompiler funCompiler{m_pusher, m_currentModifier + 1, m_function, m_isLibraryWithObj, m_pushArgs, m_pusher.stackSize()};
	funCompiler.visitFunctionWithModifiers();
	return false;
}

void TVMFunctionCompiler::pushC4ToC7IfNeed() {
	// c4_to_c7 if need
	if (m_function->stateMutability() != StateMutability::Pure) {
		m_pusher.was_c4_to_c7_called();
		m_pusher.push(-1, ""); // fix stack
		m_pusher.startIfRef();
		m_pusher.pushCall(0, 0, "c4_to_c7");
		m_pusher.endContinuation();
	}
}

void TVMFunctionCompiler::pushC7ToC4IfNeed() {
	// c7_to_c4 if need
	solAssert(m_pusher.stackSize() == 0, "");
	if (m_function->stateMutability() == StateMutability::NonPayable) {
		m_pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
	} else {
		// if it's external message than save values for replay protection
		m_pusher.startIfRef();
		m_pusher.pushCall(0, 0, "c7_to_c4");
		m_pusher.endContinuation();
	}
}

void TVMFunctionCompiler::pushReceiveOrFallback() {
	// stack: body

	auto callFallback = [&](){
		if (m_contract->fallbackFunction()) {
			m_pusher.startContinuation();
			m_pusher.drop();
			m_pusher.pushMacroCallInCallRef(0, 0, "fallback_macro");
			m_pusher._throw("THROW 0");
			m_pusher.endContinuation();
			m_pusher._ifNot();
		} else {
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::NoFallback) + " ; funcId body'");
		}
	};

	if (!isEmptyFunction(m_contract->receiveFunction())) {
		m_pusher.pushS(1);
		m_pusher.push(0, "SEMPTY     ; isEmpty");
		m_pusher.pushS(0);
		m_pusher.startContinuation();
		{
			m_pusher.drop();
			m_pusher.pushS(1); // body

			// body'
			m_pusher.startOpaque();
			m_pusher.pushAsym("LDUQ 32  ; [funcId] body' ok");
			callFallback();
			m_pusher.endOpaque(1, 2);
			// funcId body'

			m_pusher.pushS(1); // funcId body' isZero
			m_pusher.push(0, "EQINT 0 ; funcId body' isZero");
			m_pusher.pushS(0); // funcId body' isZero isZero"
			m_pusher.startContinuation();
			m_pusher.dropUnder(1, 2);
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.endContinuation();
		m_pusher._ifNot();
		m_pusher.startIfJmpRef();
		m_pusher.pushCall(0, 0, "receive_macro");
		m_pusher.endContinuation();
	} else {
		m_pusher.pushS(1);
		m_pusher.push(0, "SEMPTY     ; isEmpty");
		m_pusher.checkIfCtorCalled(true);
		m_pusher.pushS(1);

		// body -> funcId body'
		m_pusher.startOpaque();
		m_pusher.pushAsym("LDUQ 32  ; [funcId] body' ok");
		callFallback();
		m_pusher.endOpaque(1, 2);

		// stack: funcId body'
		m_pusher.pushS(1);
		m_pusher.checkIfCtorCalled(false);
	}
}

void TVMFunctionCompiler::buildPublicFunctionSelector(
	const std::vector<std::pair<uint32_t, std::string>>& functions,
	int left,
	int right
) {
	int qty = right - left;
	int blockSize = 1;
	while (4 * blockSize < qty) {
		blockSize *= 4;
	}
	solAssert(4 * blockSize >= qty, "");

	auto pushOne = [&](uint32_t functionId, const std::string& name) {
		m_pusher.pushS(0);
		m_pusher.pushInt(functionId);
		m_pusher.push(-2 + 1, "EQUAL");
		m_pusher.push(-1, ""); // fix stack
		m_pusher.startIfJmpRef();
		m_pusher.pushCall(0, 0, name);
		m_pusher.endContinuation();
	};

	// stack: functionID
	if (right - left <= 4) {
		for (int i = left; i < right; ++i) {
			const auto& [functionId, name] = functions.at(i);
			pushOne(functionId, name);
		}
	} else {
		for (int i = left; i < right; i += blockSize) {
			int j = std::min(i + blockSize, right);
			const auto& [functionId, name] = functions.at(j - 1);
			if (j - i == 1) {
				pushOne(functionId, name);
			} else {
				m_pusher.pushS(0);
				m_pusher.pushInt(functionId);
				m_pusher.push(-2 + 1, "LEQ");
				m_pusher.startIfJmpRef();
				buildPublicFunctionSelector(functions, i, j);
				m_pusher.endContinuation();
			}
		}
	}
}

void TVMFunctionCompiler::pushLocation(const ASTNode& node, bool reset) {
    SourceLocation const &loc = node.location();
    SourceReference sr = SourceReferenceExtractor::extract(&loc);
    const int line = reset ? 0 : sr.position.line + 1;

	fs::path curDir = fs::current_path();
	std::string p = fs::relative(sr.sourceName, curDir).generic_string();

    m_pusher.pushLoc(p, line);
}