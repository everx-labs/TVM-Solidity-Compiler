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
 * AST to TVM bytecode contract compiler
 */

#include <boost/algorithm/string/replace.hpp>

#include <liblangutil/SourceReferenceExtractor.h>

#include "DictOperations.hpp"
#include "TVMABI.hpp"
#include "TVMAnalyzer.hpp"
#include "TVMConstants.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCall.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMStructCompiler.hpp"

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace std;

namespace fs = boost::filesystem;

TVMFunctionCompiler::TVMFunctionCompiler(StackPusher &pusher, ContractDefinition const *contract) :
	m_pusher{pusher},
	m_contract{contract}
{

}

TVMFunctionCompiler::TVMFunctionCompiler(
	StackPusher &pusher,
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

bool TVMFunctionCompiler::hasLoop() const {
	return std::any_of(m_controlFlowInfo.begin(), m_controlFlowInfo.end(), [](const ControlFlowInfo& info){
		return info.isLoop;
	});
}

std::optional<ControlFlowInfo> TVMFunctionCompiler::lastAnalyzeFlag() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).doAnalyzeFlag) {
			return m_controlFlowInfo.at(i);
		}
	}
	return std::nullopt;
}

std::optional<ControlFlowInfo> TVMFunctionCompiler::lastLoop() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).isLoop) {
			return m_controlFlowInfo.at(i);
		}
	}
	return std::nullopt;
}

bool TVMFunctionCompiler::lastAnalyzerBeforeLoop() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).isLoop) {
			return false;
		}
		if (m_controlFlowInfo.at(i).doAnalyzeFlag) {
			return true;
		}
	}
	solUnimplemented("");
}

Pointer<Function> TVMFunctionCompiler::updateOnlyTime(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	pusher.pushRoot();
	pusher  << "CTOS"
			<< "LDU 256"
			<< "LDU 64";
	pusher.dropUnder(1, 1);
	pusher.getGlob(TvmConst::C7::ReplayProtTime);
	// pubkey rest time
	pusher.rot(); // rest time pubkey
	pusher  << "NEWC" // rest time pubkey builder
			<< "STU 256"
			<< "STU 64"
			<< "STSLICE"
			<< "ENDC";
	pusher.popRoot();

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "upd_only_time_in_c4", Function::FunctionType::Macro, block);
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateC4ToC7(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	pusher.pushRoot();
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
		decoder.decodeData(pusher.ctx().getOffsetC4(),
						   pusher.ctx().usage().hasAwaitCall() ? 1 : 0,
						   stateVarTypes);

		const int varQty = stateVarTypes.size();
		if (pusher.ctx().tooMuchStateVariables()) {
			for (int i = 0; i < TvmConst::C7::FirstIndexForVariables; ++i) {
				pusher.getGlob(i);
			}
			pusher.blockSwap(varQty, TvmConst::C7::FirstIndexForVariables);
			pusher.tuple(varQty + TvmConst::C7::FirstIndexForVariables);
			pusher.popC7();
		} else {
			for (int i = varQty - 1; i >= 0; --i) {
				pusher.setGlob(TvmConst::C7::FirstIndexForVariables + i);
			}
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
	auto f = createNode<Function>(0, 0, "c4_to_c7", Function::FunctionType::Macro, block);
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateC4ToC7WithInitMemory(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, pusher.ctx().getContract()};

	pusher.pushRoot();
	pusher.push(-1 + 1, "CTOS");
	pusher.push(-1 + 1, "SBITS");
	pusher.push(-1 + 1, "GTINT 1");

	pusher.startContinuation();
	pusher.pushCall(0, 0, "c4_to_c7");
	pusher.endContinuationFromRef();

	pusher.startContinuation();
	pusher.pushInt(0);
	pusher.pushRoot();
	pusher.push(0, "CTOS");
	pusher.push(0, "PLDDICT   ; D");

	int varQty = 0;
	bool tooMuchStateVars = pusher.ctx().tooMuchStateVariables();
	if (tooMuchStateVars) {
		for (int i = 0; i < TvmConst::C7::FirstIndexForVariables; ++i) {
			pusher.getGlob(i);
			++varQty;
		}
	}
	int shift = 0;
	for (VariableDeclaration const* v : pusher.ctx().notConstantStateVariables()) {
		if (v->isStatic()) {
			pusher.pushInt(TvmConst::C4::PersistenceMembersStartIndex + shift++); // dict vars... index dict
			pusher.pushS(1 + varQty); // dict vars... index dict
			pusher.getDict(getKeyTypeOfC4(), *v->type(), GetDictOperation::GetFromMapping);
		} else {
			pusher.pushDefaultValue(v->type());
		}
		++varQty;
	}
	if (tooMuchStateVars) {
		pusher.tuple(varQty);
		pusher.popC7();
	} else {
		auto x = pusher.ctx().notConstantStateVariables(); // move
		for (VariableDeclaration const* v : x | boost::adaptors::reversed) {
			pusher.setGlob(v);
		}
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

	auto f = createNode<Function>(0, 0, "c4_to_c7_with_init_storage", Function::FunctionType::Macro, pusher.getBlock());
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateBuildTuple(TVMCompilerContext& ctx, std::string const& name, const std::vector<Type const*>& types) {
	StackPusher pusher{&ctx};
	int n = types.size();
	std::vector<std::string> names(n);
	for (Type const* t : types) {
		pusher.pushDefaultValue(t);
	}
	pusher.tuple(n);
	StructCompiler sc{&pusher, types, names};
	sc.tupleToBuilder();
	pusher << "ENDC";
	return createNode<Function>(0, 0, name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateNewArrays(TVMCompilerContext& ctx, std::string const& name, FunctionCall const* arr) {
	StackPusher pusher{&ctx};
	FunctionCallCompiler{pusher, *arr, true}.honestArrayCreation(true);
	return createNode<Function>(0, 0, name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateConstArrays(TVMCompilerContext& ctx, std::string const& name, TupleExpression const* arr) {
	StackPusher pusher{&ctx};
	solAssert(arr->isInlineArray(), "");
	TVMExpressionCompiler{pusher}.visitHonest(*arr, true);
	return createNode<Function>(0, 0, name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateMacro(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	const std::optional<std::string>& forceName
) {
	StackPusher pusher{&ctx};
	std::string name = forceName.has_value() ? forceName.value() : function->name();
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
    funCompiler.pushLocation(*function);
	funCompiler.visitFunctionWithModifiers();
    funCompiler.pushLocation(*function, true);
    int take = function->parameters().size();
    int ret = function->returnParameters().size();
    return createNode<Function>(take, ret, name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateOnCodeUpgrade(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	funCompiler.visitFunctionWithModifiers();

	pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
	pusher.push(0, "COMMIT");
	pusher._throw("THROW 0");
	int take = function->parameters().size();
	const std::string name = ctx.getFunctionInternalName(function, false);
	return createNode<Function>(take, 0, name, Function::FunctionType::OnCodeUpgrade,
								pusher.getBlock(), function);
}

Pointer<Function>
TVMFunctionCompiler::generateOnTickTock(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	StackPusher pusher{&ctx};
	pusher.startOpaque();
	pusher.pushInt(-2);
	pusher.endOpaque(0, 0); // hide -2 from optimizer, because it may be used in msg.isTickTock

	solAssert(function->parameters().size() == 1, "");
	const ASTPointer<VariableDeclaration>& variable = function->parameters().at(0);
	pusher.pushS(2);
	pusher.getStack().add(variable.get(), false);

	bool isPure = function->stateMutability() == StateMutability::Pure;
	if (!isPure) {
		pusher.pushMacroCallInCallRef(0, 0, "c4_to_c7");
	}

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	funCompiler.setCopyleftAndTryCatch();
	funCompiler.setGlobSenderAddressIfNeed();
	funCompiler.visitFunctionWithModifiers();


	if (!isPure) {
		pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
	}
	return createNode<Function>(0, 0, "onTickTock", Function::FunctionType::OnTickTock, pusher.getBlock());
}

void TVMFunctionCompiler::decodeFunctionParamsAndInitVars(bool isResponsible) {
	// decode function params
	// stack: arguments-in-slice
	vector<Type const*> types = getParams(m_function->parameters()).first;
	ChainDataDecoder{&m_pusher}.decodeFunctionParameters(types, isResponsible);
	// stack: transaction_id arguments...
	m_pusher.getStack().change(-static_cast<int>(m_function->parameters().size()));
	for (const ASTPointer<VariableDeclaration>& variable: m_function->parameters()) {
		auto name = variable->name();
		m_pusher.getStack().add(variable.get(), true);
	}
}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunction(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	/* stack:
	 * transaction data (see internal or external main)
	 * function result
	 * [send int/ext msg]
	 */

	StackPusher pusher{&ctx};
	std::string name = function->name();
	Function::FunctionType type = Function::FunctionType::Macro;
	Pointer<CodeBlock> block;

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	pusher.push(+1, ""); // slice with args
	pusher.push(+1, ""); // functionId
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
	funCompiler.decodeFunctionParamsAndInitVars(isResponsible);
    funCompiler.pushLocation(*function, true);

	int paramQty = function->parameters().size();
	int retQty = function->returnParameters().size();
	// stack: selector, arg0, arg1, arg2 ...
	// +1 because function may use selector
	pusher.pushMacroCallInCallRef(paramQty + 1, retQty + 1, pusher.ctx().getFunctionInternalName(function) + "_macro");

	solAssert(pusher.stackSize() == retQty, "");
	// emit
	funCompiler.emitOnPublicFunctionReturn();

	pusher.ensureSize(0, "");

	funCompiler.updC4IfItNeeds();
	// set flag meaning function is called
	pusher._throw("THROW 0");

	block = pusher.getBlock();
	// takes selector, sliceWithBody, functionId
	// returns nothing
	return createNode<Function>(3, 0, name, type, block);
}

void TVMFunctionCompiler::generateFunctionWithModifiers(
	StackPusher& pusher,
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
TVMFunctionCompiler::generateGetter(StackPusher &pusher, VariableDeclaration const* vd) {
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
	pusher.sendMsg({}, {}, appendBody, nullptr, nullptr, StackPusher::MsgType::ExternalOut);

	// check ext msg
	pusher.endContinuation();
	pusher._if();

	pusher._throw("THROW 0");

	return createNode<Function>(2, 1, vd->name(), Function::FunctionType::MacroGetter, pusher.getBlock());

}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunctionSelector(TVMCompilerContext& ctx, ContractDefinition const *contract) {
	StackPusher pusher{&ctx};
	const std::vector<std::pair<uint32_t, std::string>>& functions = pusher.ctx().getPublicFunctions();
	TVMFunctionCompiler compiler{pusher, contract};
	compiler.buildPublicFunctionSelector(functions, 0, functions.size());
	return createNode<Function>(1, 1, "public_function_selector", Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generatePrivateFunction(TVMCompilerContext& ctx, const std::string& name, FunctionDefinition const* funDef) {
	StackPusher pusher{&ctx};
	const std::string macroName = name + "_macro";
	pusher.pushCall(0, 0, macroName);
	return createNode<Function>(0, 0, name, Function::FunctionType::PrivateFunction, pusher.getBlock(), funDef);
}

Pointer<Function> TVMFunctionCompiler::generateLibraryFunction(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	const std::string& name
) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	const std::string macroName = name + "_macro";
	pusher.pushCall(0, 0, macroName);
	return createNode<Function>(0, 0, name, Function::FunctionType::PrivateFunctionWithObj, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateLibraryFunctionMacro(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	const std::string& name
) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	funCompiler.visitFunctionWithModifiers();
	int take = function->parameters().size();
	int ret = function->returnParameters().size();
	return createNode<Function>(take, ret + 1, name, Function::FunctionType::Macro, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateReceive(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	return generateReceiveOrFallbackOrOnBounce(ctx, function, "receive_macro", 0);
}

Pointer<Function> TVMFunctionCompiler::generateFallback(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	return generateReceiveOrFallbackOrOnBounce(ctx, function, "fallback_macro", 0);
}

Pointer<Function> TVMFunctionCompiler::generateOnBounce(TVMCompilerContext& ctx, const FunctionDefinition *function) {
	return generateReceiveOrFallbackOrOnBounce(ctx, function, "on_bounce_macro", 1);
}

Pointer<Function> TVMFunctionCompiler::generateReceiveOrFallbackOrOnBounce(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	const std::string& name,
	int take
) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	pusher.checkCtorCalled();
	funCompiler.pushC4ToC7IfNeed();
	funCompiler.visitFunctionWithModifiers();
	funCompiler.updC4IfItNeeds();
	return createNode<Function>(take, 0, name, Function::FunctionType::Macro, pusher.getBlock());
}

// pop params.size() elements from stack top
void TVMFunctionCompiler::emitOnPublicFunctionReturn() {
	const int stackSize = m_pusher.stackSize();

	const std::vector<ASTPointer<VariableDeclaration>>& params = m_function->returnParameters();
	if (params.empty()) {
		return;
	}

	m_pusher.startOpaque();

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
			StackPusher::MsgType::ExternalOut
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
			m_pusher.indexNoexcep(TvmConst::C7::ReturnParam::CallbackFunctionId);
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
			m_pusher.indexNoexcep(TvmConst::C7::ReturnParam::Flag);
		};

		m_pusher.getGlob(TvmConst::C7::ReturnParams);
		for (int i = 0; i < 3; ++i) {
			if (i == 2) {
				m_pusher.getGlob(TvmConst::C7::SenderAddress); // dest
				m_pusher.blockSwap(1, 3);
			} else {
				m_pusher.pushS(i);
			}
			m_pusher.indexNoexcep(3 - i);
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
			StackPusher::MsgType::Internal
		);
	}
	m_pusher.endContinuation();

	m_pusher.ifElse();

	m_pusher.endOpaque(ret.size(), 0);

	solAssert(stackSize == int(m_pusher.stackSize()) + int(params.size()), "");
}

void TVMFunctionCompiler::visitModifierOrFunctionBlock(Block const &body, int argQty, int retQty, int nameRetQty) {
	LocationReturn locationReturn = ::notNeedsPushContWhenInlining(body);

	bool doPushContinuation = locationReturn == LocationReturn::Anywhere;
	if (doPushContinuation) {
		m_pusher.startContinuation();
	}
	if (m_currentModifier == static_cast<int>(m_function->modifiers().size()) && withPrelocatedRetValues(m_function)) {
		pushDefaultParameters(m_function->returnParameters());
	}
	acceptBody(body, {{argQty, nameRetQty}});
	if (locationReturn == LocationReturn::Last) {
        m_pusher.pollLastRetOpcode();
    }
	if (doPushContinuation) {
		pushLocation(*m_function);
		if (m_isLibraryWithObj && m_currentModifier == static_cast<int>(m_function->modifiers().size())) {
			++retQty;
			solAssert(argQty > 0, "");
		}
		m_pusher.pushContAndCallX(argQty, retQty, false);
        pushLocation(*m_function, true);
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
	const int argQty = m_function->parameters().size();
	const int retQty = m_function->returnParameters().size();
	const int nameRetQty = withPrelocatedRetValues(m_function) ? retQty : 0;

	// inits function params and return named params
	if (m_currentModifier == 0) {
		if (m_pushArgs) {
			solAssert(m_startStackSize == 0, "");
			m_pusher.pushParameter(m_function->parameters());
		} else {
			solAssert(m_startStackSize >= 0, "");
		}

		// TODO move to function
		solAssert(!m_function->externalMsg() || !m_function->internalMsg(), "");

		if (m_function->externalMsg() || m_function->internalMsg())
			m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -5",
				"PICK",
			}, 0, 1, true));

		if (m_function->externalMsg()) {
			m_pusher.push(-1 + 1, "EQINT -1");
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ByExtMsgOnly));
		} else if (m_function->internalMsg()) {
			m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ByIntMsgOnly));
		}
	}


	if (m_currentModifier == static_cast<int>(functionModifiers().size())) {
		int modSize = m_pusher.stackSize() - argQty;
		m_pusher.blockSwap(argQty, modSize); // break stack

		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.push(-modSize, ""); // fix stack

		TVMFunctionCompiler funCompiler{pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, 0};
		funCompiler.visitModifierOrFunctionBlock(m_function->body(), argQty, retQty, nameRetQty);
		m_pusher.add(pusher);

		m_pusher.blockSwap(modSize, retQty); // break stack
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
		TVMFunctionCompiler funCompiler{m_pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, ss};
		funCompiler.visitModifierOrFunctionBlock(modifierDefinition->body(), modParamQty, 0, 0);
		solAssert(ss == m_pusher.stackSize(), "");
	}
}

void TVMFunctionCompiler::pushDefaultParameters(const ast_vec<VariableDeclaration> &returnParameters) {
	for (const ASTPointer<VariableDeclaration>& returnParam: returnParameters) {
		m_pusher.pushDefaultValue(returnParam->type());
		m_pusher.getStack().add(returnParam.get(), false);
	}
}

void TVMFunctionCompiler::acceptExpr(const Expression *expr, const bool isResultNeeded) {
	solAssert(expr, "");
	TVMExpressionCompiler(m_pusher).acceptExpr(expr, isResultNeeded);
}

bool TVMFunctionCompiler::visit(VariableDeclarationStatement const &_variableDeclarationStatement) {
	const int saveStackSize = m_pusher.stackSize();
	int bad = 0;
	ast_vec<VariableDeclaration> variables = _variableDeclarationStatement.declarations();
	int varQty = variables.size();
	auto deleteUnnamedVars = [&](std::vector<bool> const& hasName) {
		int nameQty = hasName.size();
		int top = 0;
		for (int i = nameQty - 1; 0 <= i; --i) {
			if (!hasName.at(i)) {
				m_pusher.dropUnder(1, top);
			} else {
				++top;
			}
		}
	};


	if (auto init = _variableDeclarationStatement.initialValue()) {
		auto te = to<TupleExpression>(init);
		if (te && !te->isInlineArray() && varQty == int(te->components().size())) {
			ast_vec<Expression> const&  tuple = te->components();
			for (std::size_t i = 0; i < tuple.size(); ++i) {
				acceptExpr(tuple[i].get());
				if (variables.at(i) != nullptr) {
					m_pusher.hardConvert(variables.at(i)->type(), tuple.at(i)->annotation().type);
				} else {
					++bad;
					m_pusher.drop();
				}
			}
		} else {
			acceptExpr(init);
			if (varQty == 1) {
				m_pusher.hardConvert(variables.at(0)->type(), init->annotation().type);
			} else {
				auto tuple = to<TupleType>(init->annotation().type);
				std::vector<bool> hasName(varQty);
				for (int i = varQty - 1; i >= 0; --i) {
					if (variables.at(i) != nullptr) {
						m_pusher.hardConvert(variables.at(i)->type(), tuple->components().at(i));
						hasName[i] = true;
					} else {
						++bad;
						hasName[i] = false;
					}
					m_pusher.blockSwap(varQty - 1, 1);
				}
				deleteUnnamedVars(hasName);
			}
		}
	} else {
		for (const auto& decl : variables) {
			m_pusher.pushDefaultValue(decl->type());
		}
	}

	m_pusher.getStack().change(-varQty + bad);
	for (const ASTPointer<VariableDeclaration>& d : variables) {
		if (d != nullptr) {
	 		m_pusher.getStack().add(d.get(), true);
		}
	}
	m_pusher.ensureSize(saveStackSize + varQty - bad, "VariableDeclarationStatement", &_variableDeclarationStatement);
	return false;
}

void TVMFunctionCompiler::acceptBody(Block const& _block, std::optional<std::tuple<int, int>> functionBlock) {
	const int startStackSize = m_pusher.stackSize();

	for (const ASTPointer<Statement> &s: _block.statements()) {
		pushLocation(*s.get());
		s->accept(*this);
	}

	bool lastIsRet = !_block.statements().empty() && to<Return>(_block.statements().back().get()) != nullptr;

	if (functionBlock) {
		auto [argQty, nameRetQty] = functionBlock.value();
		int funTrash = m_pusher.stackSize() - m_startStackSize - argQty - nameRetQty;
		solAssert(funTrash >= 0, "");
		if (!lastIsRet) {
			m_pusher.drop(funTrash);
			if (m_isLibraryWithObj && m_currentModifier == static_cast<int>(m_function->modifiers().size())) {
				--argQty;
				solAssert(argQty >= 0, "");
			}
			m_pusher.dropUnder(argQty, nameRetQty);
		} else {
			m_pusher.push(-funTrash - argQty, ""); // TODO why -argQty ?
		}
	} else {
		const int delta = m_pusher.stackSize() - startStackSize;
		solAssert(delta >= 0, "");
		// !lastIsRet
		if (!_block.statements().empty() && to<Return>(_block.statements().back().get()) == nullptr) {
			m_pusher.drop(delta);
		} else {
			m_pusher.push(-delta, ""); // fix stack
		}
	}

	pushLocation(_block, true);
}

bool TVMFunctionCompiler::visit(Block const& _block) {
	acceptBody(_block, std::nullopt);
	return false;
}

bool TVMFunctionCompiler::visit(ExpressionStatement const& _statement) {
	if (!_statement.expression().annotation().isPure) {
	    pushLocation(_statement);
		auto savedStackSize = m_pusher.stackSize();
		acceptExpr(&_statement.expression(), false);
		m_pusher.ensureSize(savedStackSize, _statement.location().text());
        pushLocation(_statement, true);
	}
	return false;
}

bool TVMFunctionCompiler::visit(TryStatement const& _tryState) {
    // try body
    m_pusher.startContinuation();
    const int startStackSize = m_pusher.stackSize();
    _tryState.body().accept(*this);
    m_pusher.drop(m_pusher.stackSize() - startStackSize);
    m_pusher.endContinuation();

	// try body
	m_pusher.startContinuation();
    for (ASTPointer<VariableDeclaration> const& variable : _tryState.clause().parameters()->parameters()) {
		m_pusher.getStack().add(variable.get(), true);
    }
	_tryState.clause().block().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - startStackSize);
	m_pusher.endContinuation();

	m_pusher.tryOpcode();

    return false;
}

bool TVMFunctionCompiler::visit(IfStatement const &_ifStatement) {
	const int saveStackSize = m_pusher.stackSize();

	// header
	CFAnalyzer ci(_ifStatement);
	bool canUseJmp = _ifStatement.falseStatement() != nullptr ?
					 CFAnalyzer(_ifStatement.trueStatement()).doThatAlways() &&
					 CFAnalyzer(*_ifStatement.falseStatement()).doThatAlways() :
					 CFAnalyzer(_ifStatement.trueStatement()).doThatAlways();
	ControlFlowInfo info{};
	info.isLoop = false;
	if (canUseJmp) {
		info.doAnalyzeFlag = false;
	} else {
		if (ci.canContinue() || (!hasLoop() && ci.canReturn())) {
			m_pusher.declRetFlag();
			info.doAnalyzeFlag = true;
		}
	}
	info.stackSize = m_pusher.stackSize();
	m_controlFlowInfo.push_back(info);

	// condition
	acceptExpr(&_ifStatement.condition(), true);
	m_pusher.push(-1, ""); // drop condition
	// if
	m_pusher.startContinuation();
	_ifStatement.trueStatement().accept(*this);
	endContinuation2(!canUseJmp); // TODO delete arg, optimizer is gonna delete DROP


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

	// bottom
	if (info.doAnalyzeFlag) {
		std::optional<ControlFlowInfo> lastAnalyze = lastAnalyzeFlag();
		m_pusher.startOpaque();
		if (lastAnalyze.has_value()) {
			m_pusher.pushS(0);
			m_pusher.ifret();
			m_pusher.drop();
		} else {
			m_pusher.ifret();
		}
		m_pusher.endOpaque(1, 0);
	}
	m_pusher.ensureSize(saveStackSize, "");

	return false;
}

void TVMFunctionCompiler::doWhile(WhileStatement const &_whileStatement) {
	int saveStackSize = m_pusher.stackSize();

	// header
	CFAnalyzer ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_whileStatement.body());

	// body
	m_pusher.startContinuation();
	int ss = m_pusher.stackSize();
	if (ci.canReturn() || ci.canBreak() || ci.canContinue()) {
		m_pusher.startContinuation();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
		m_pusher.pushContAndCallX(0, 0, false); // TODO check
	} else {
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
	}

	// condition
	acceptExpr(&_whileStatement.condition(), true);
	m_pusher.push(0, "NOT");
	m_pusher.push(-1, ""); // drop condition
	m_pusher.endContinuation();

	m_pusher.until(ci.canBreak() || ci.canReturn());

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0, info.doAnalyzeFlag);

	m_pusher.ensureSize(saveStackSize, "");
}

void
TVMFunctionCompiler::visitForOrWhileCondition(
    const std::function<void()>& pushCondition
) {
	int stackSize = m_pusher.stackSize();
	m_pusher.startContinuation();
	if (pushCondition) {
		pushCondition();
		m_pusher.push(-1, ""); // fix stack
	} else {
		m_pusher.push(+1, "TRUE");
		m_pusher.push(-1, ""); // fix stack
	}
	m_pusher.endContinuation();
	m_pusher.ensureSize(stackSize, "visitForOrWhileCondition");
}

void TVMFunctionCompiler::afterLoopCheck(const CFAnalyzer& ci, const int& loopVarQty, bool _doAnalyzeFlag) {
	std::optional<ControlFlowInfo> analyzeFlag = lastAnalyzeFlag();
	std::optional<ControlFlowInfo> loopFlag = lastLoop();

	if (_doAnalyzeFlag) {
		m_pusher.startOpaque();
		if (analyzeFlag.has_value()) {
			m_pusher.pushS(0);
		}
		if (ci.canBreak() || ci.canContinue()) {
			m_pusher.push(0, "EQINT " + toString(TvmConst::RETURN_FLAG));
		}
		loopFlag.has_value() ? m_pusher.ifRetAlt() : m_pusher.ifret();
		if (analyzeFlag.has_value()) {
			m_pusher.drop();
		}
		m_pusher.endOpaque(1, 0);
	}
	m_pusher.drop(loopVarQty);
}

bool TVMFunctionCompiler::visit(WhileStatement const &_whileStatement) {
	int saveStackSizeForWhile = m_pusher.stackSize();

	if (_whileStatement.loopType() == WhileStatement::LoopType::DO_WHILE) {
		doWhile(_whileStatement);
		return false;
	}

	// header
	CFAnalyzer ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_whileStatement.body());

	int saveStackSize = m_pusher.stackSize();

	// condition
	if (_whileStatement.loopType() == WhileStatement::LoopType::REPEAT) {
		acceptExpr(&_whileStatement.condition());
		m_pusher.push(-1, "");
	} else {
		std::function<void()> pushCondition = [&]() {
			acceptExpr(&_whileStatement.condition(), true);
		};
		visitForOrWhileCondition(pushCondition);
	}

	m_pusher.ensureSize(saveStackSize, "while condition");

	// body
	m_pusher.startContinuation();
	_whileStatement.body().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - saveStackSize);
	m_pusher.endContinuation();

	if (_whileStatement.loopType() == WhileStatement::LoopType::REPEAT)
		m_pusher.repeat(ci.canBreak() || ci.canReturn());
	else
		m_pusher._while(ci.canBreak() || ci.canReturn());

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0, info.doAnalyzeFlag);

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
	// [return flag] - optional. If we have return/break/continue.

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
			m_pusher.indexNoexcep(1); // stack: {length, dict} -> dict
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
	CFAnalyzer ci;
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
				m_pusher.getDict(getArrayKeyType(), *arrayType->baseType(), GetDictOperation::Fetch);
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
	visitForOrWhileCondition(pushCondition);


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
				m_pusher.ifNot();
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
			m_pusher.pushInt(dictKeyLength(mappingType->keyType()));  // stack: dict minKey(private) minKey(pub) value [flag] minKey dict nbits

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
	afterLoopCheck(ci, loopVarQty, info.doAnalyzeFlag);
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

std::pair<CFAnalyzer, ControlFlowInfo> TVMFunctionCompiler::pushControlFlowFlag(Statement const& body) {
	CFAnalyzer ci(body);
	ControlFlowInfo info;
	info.isLoop = true;
	info.stackSize = -1;
	if (ci.canReturn()) {
		m_pusher.declRetFlag();
		info.doAnalyzeFlag = true;
	}
	info.stackSize = m_pusher.stackSize();
	m_controlFlowInfo.push_back(info);
	return {ci, info};
}

void TVMFunctionCompiler::visitBodyOfForLoop(
	const CFAnalyzer& ci,
	const std::function<void()>& pushStartBody,
	Statement const& body,
	const std::function<void()>& loopExpression
) {
	// body and loopExpression
	m_pusher.startContinuation();
	if (pushStartBody) {
		pushStartBody();
	}

	// take loop body
	if (ci.canContinue()) { // TODO and have loopExpression
		int ss = m_pusher.stackSize();
		m_pusher.startContinuation();
		body.accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
		m_pusher.pushContAndCallX(0, 0, false); // TODO check
	} else {
		int ss = m_pusher.stackSize();
		body.accept(*this);
		m_pusher.drop(m_pusher.stackSize() - ss);
	}

	if (loopExpression) {
		loopExpression();
	}
	m_pusher.endContinuation();
	m_pusher._while(ci.canBreak() || ci.canReturn());
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
	CFAnalyzer ci;
	ControlFlowInfo info;
	std::tie(ci, info) = pushControlFlowFlag(_forStatement.body());

	// condition
	std::function<void()> pushCondition;
	if (_forStatement.condition()) {
		pushCondition = [&](){
			acceptExpr(_forStatement.condition(), true);
		};
	}
	visitForOrWhileCondition(pushCondition);

	// body and loopExpression
	std::function<void()> pushLoopExpression;
	if (_forStatement.loopExpression() != nullptr) {
		pushLoopExpression = [&]() {
			_forStatement.loopExpression()->accept(*this);
		};
	}
	visitBodyOfForLoop(ci, {}, _forStatement.body(), pushLoopExpression);

	// bottom
	afterLoopCheck(ci, haveDeclLoopVar, info.doAnalyzeFlag);
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

bool TVMFunctionCompiler::visit(Return const& _return) {
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

		int retQty = m_function->returnParameters().size();
		std::vector<Type const*> givenTypes;
		if (auto tuple = to<TupleType>(expr->annotation().type)) {
			solAssert(retQty == int(tuple->components().size()), "");
			for (int i = 0; i < retQty; ++i) {
				givenTypes.emplace_back(tuple->components().at(i));
			}
		} else {
			givenTypes.emplace_back(expr->annotation().type);
		}

		for (int i = retQty - 1; i >= 0; --i) {
			Type const* leftType = m_function->returnParameters().at(i)->type();
			Type const* rightType = givenTypes.at(i);
			m_pusher.hardConvert(leftType, rightType);
			if (retQty >= 2) {
				m_pusher.blockSwap(retQty - 1, 1);
			}
		}
	}

	int retCount = 0;
	if (_return.annotation().functionReturnParameters != nullptr) {
		ast_vec<VariableDeclaration> const &params = _return.annotation().functionReturnParameters->parameters();
		retCount = params.size();
	}

	m_pusher.startContinuation();
	int trashSlots = m_pusher.stackSize() - m_startStackSize;
	if (m_isLibraryWithObj && m_currentModifier == static_cast<int>(m_function->modifiers().size())) {
		--trashSlots;
		solAssert(trashSlots >= 0, "");
	}
	int revertDelta = trashSlots - retCount;
	m_pusher.dropUnder(trashSlots - retCount, retCount);
	if (lastAnalyzeFlag().has_value()) {
		m_pusher.pushInt(TvmConst::RETURN_FLAG);
		--revertDelta;
		m_pusher.push(revertDelta, ""); // fix stack
	} else { // all continuation are run by JMPX
		m_pusher.push(revertDelta, ""); // fix stack
	}


	int i{};
	ControlFlowInfo controlFlowInfo;
	for (i = static_cast<int>(m_controlFlowInfo.size()) - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).isLoop) {
			controlFlowInfo = m_controlFlowInfo.at(i);
			break;
		}
	}
	if (i == -1) {
		m_pusher.ret();
	} else {
		m_pusher.retAlt();
	}
	m_pusher.endRetOrBreakOrCont(retCount);

	return false;
}

bool TVMFunctionCompiler::visit(Break const&) {
	const int sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize;
	m_pusher.startContinuation();

	m_pusher.drop(sizeDelta);
	m_pusher.retAlt();

	m_pusher.push(sizeDelta, ""); // fix stack
	m_pusher.endRetOrBreakOrCont(0);

	return false;
}

bool TVMFunctionCompiler::visit(Continue const&) {
	bool hasAnalyzer = lastAnalyzerBeforeLoop();
	const int sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize;
	m_pusher.startContinuation();

	if (hasAnalyzer) {
		m_pusher.drop(sizeDelta + (lastLoop().value().doAnalyzeFlag ? 1 : 0));
		m_pusher.pushInt(TvmConst::CONTINUE_FLAG);
	} else {
		m_pusher.drop(sizeDelta);
	}

	m_pusher.ret();

	m_pusher.push(sizeDelta, ""); // fix stack
	m_pusher.endRetOrBreakOrCont(0);
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

	m_pusher.sendMsg(isParamOnStack, {}, appendBody, nullptr, nullptr, StackPusher::MsgType::ExternalOut);
	return false;
}

Pointer<Function> TVMFunctionCompiler::generateMainExternal(TVMCompilerContext& ctx, ContractDefinition const *contract) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, contract};
	Pointer<Function> f = funCompiler.generateMainExternalForAbiV2();
	return f;
}

void TVMFunctionCompiler::setGlobSenderAddressIfNeed() {
	if (m_pusher.ctx().usage().hasMsgSender()) {
		m_pusher.pushSlice("x8000000000000000000000000000000000000000000000000000000000000000001_");
		m_pusher.setGlob(TvmConst::C7::SenderAddress);
	}
}

void TVMFunctionCompiler::setCtorFlag() {
	m_pusher.pushRoot();
	m_pusher.push(0, "CTOS");
	m_pusher.push(0, "SBITS");
	m_pusher.push(0, "NEQINT 1");
	m_pusher.setGlob(TvmConst::C7::ConstructorFlag);
}

void TVMFunctionCompiler::setCopyleftAndTryCatch() {
	const std::optional<std::vector<ASTPointer<Expression>>> copyleft = m_pusher.ctx().pragmaHelper().hasCopyleft();
	if (copyleft.has_value()) {
		const std::optional<bigint>& addr = ExprUtils::constValue(*copyleft.value().at(1));
		const std::optional<bigint>& type = ExprUtils::constValue(*copyleft.value().at(0));
		const std::string addrSlice = "x" + StrUtils::binaryStringToSlice(StrUtils::toBitString(addr.value(), 256));
		m_pusher.pushSlice(addrSlice);
		m_pusher.pushInt(type.value());
		m_pusher << "COPYLEFT";
	}

	if (m_pusher.ctx().usage().hasTryCatch()) {
		m_pusher << "NEWEXCMODEL";
	}
}

Pointer<Function>
TVMFunctionCompiler::generateMainExternalForAbiV2() {
//		stack:
//		contract_balance
//		msg_balance is always zero
//		msg_cell
//		msg_body_slice
//		transaction_id = -1

	setCopyleftAndTryCatch();
	setCtorFlag(); // TODO unit with setCopyleftAndTryCatch
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
		if (m_pusher.ctx().pragmaHelper().hasExpire()) {
			expire();
		}
	}

	// msg_body
	m_pusher.push(+1, "LDU 32 ; funcId body");
	m_pusher.exchange(1);

	callPublicFunctionOrFallback();
	return createNode<Function>(0, 0, "main_external", Function::FunctionType::MainExternal, m_pusher.getBlock());
}

void TVMFunctionCompiler::pushMsgPubkey() {
	// signatureSlice msgSlice hashMsgSlice

	if (m_pusher.ctx().pragmaHelper().hasPubkey()) {
		m_pusher.exchange(1);
		m_pusher.push(+1, "LDU 1 ; signatureSlice hashMsgSlice hasPubkey msgSlice");
		m_pusher.exchange(1); // signatureSlice hashMsgSlice msgSlice hasPubkey

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
	m_pusher.push(-2 + 2, "LDSLICEX");
	// signatureSlice msgSlice
	m_pusher.pushS(0);

	// signatureSlice msgSlice msgSlice
	m_pusher << "MYADDR";
	// signatureSlice msgSlice msgSlice dest
	m_pusher << "NEWC";
	// signatureSlice msgSlice msgSlice dest builder
	m_pusher << "STSLICE";
	m_pusher << "STSLICE";
	// signatureSlice msgSlice builder
	m_pusher << "ENDC";
	// signatureSlice msgSlice signedCell

	m_pusher.push(-1 + 1, "HASHCU");
	// signatureSlice msgSlice msgHash
	pushMsgPubkey();
	// signatureSlice msgSlice msgHash pubkey
	m_pusher.push(-3 + 1, "CHKSIGNU");
	// msgSlice isSigned
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::BadSignature));
	// msgSlice
	m_pusher.endContinuation();

	if (m_pusher.ctx().pragmaHelper().hasPubkey()) {
		// External inbound message does not have signature but have public key
		m_pusher.startContinuation();
		m_pusher.push(+1, "LDU 1      ; hasPubkey msgSlice");
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

	if (m_pusher.ctx().isFallBackGenerated()) {
		m_pusher.drop(2);
		m_pusher.startContinuation();
		m_pusher.pushCall(0, 0, "fallback_macro");
		m_pusher.pushRefContAndCallX(0, 0, false);
	} else {
		m_pusher._throw("THROW " + toString(TvmConst::RuntimeException::NoFallback));
	}
}

Pointer<Function>
TVMFunctionCompiler::generateMainInternal(TVMCompilerContext& ctx, ContractDefinition const *contract) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//                 src:MsgAddress  dest:MsgAddressInt(#4)
	//                 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//                 created_lt:uint64  created_at:uint32
	//                 = CommonMsgInfoRelaxed;

	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, contract};

	funCompiler.setCopyleftAndTryCatch();
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
		pusher.startContinuation();
		pusher.pushS(1);
		pusher.push(-1 + 2, "LDSLICE 32");
		pusher.dropUnder(1, 1);
		pusher.pushCall(0, 0, "on_bounce_macro");
		pusher.endContinuationFromRef();
		pusher.ifJmp();
	} else {
		pusher.ifret();
	}

	funCompiler.pushReceiveOrFallback();

	pusher.exchange(1);
	funCompiler.callPublicFunctionOrFallback();

	return createNode<Function>(0, 0, "main_internal", Function::FunctionType::MainInternal, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateCheckResume(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
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
	pusher.push(createNode<HardCode>(lines, 0, 0, false));
	return createNode<Function>(0, 0, "check_resume", Function::FunctionType::Macro, pusher.getBlock());
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
		m_pusher.startContinuation();
		m_pusher.pushCall(0, 0, "c4_to_c7");
		m_pusher.endContinuationFromRef();
		m_pusher._if();
	}
}

void TVMFunctionCompiler::updC4IfItNeeds() {
	// c7_to_c4 if need
	//	solAssert(m_pusher.stackSize() == 0, "");
	if (m_function->stateMutability() == StateMutability::NonPayable) {
		m_pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
	} else {
		// if it's external message than save values for replay protection
		if (m_pusher.ctx().afterSignatureCheck() == nullptr &&
			m_pusher.ctx().hasTimeInAbiHeader() &&
			m_pusher.ctx().notConstantStateVariables().size() >= 2 // just optimization
		) {
			m_pusher.pushS(0);
			m_pusher.startContinuation();
			m_pusher.pushCall(0, 0, "upd_only_time_in_c4");
			m_pusher.endContinuationFromRef();
			m_pusher._if();
		} else {
			m_pusher.pushS(0);
			m_pusher.startContinuation();
			m_pusher.pushCall(0, 0, "c7_to_c4");
			m_pusher.endContinuationFromRef();
			m_pusher._if();
		}
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
			m_pusher.ifNot();
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
			m_pusher.dropUnder(2, 1);
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.endContinuation();
		m_pusher.ifNot();
		m_pusher.startContinuation();
		m_pusher.pushCall(0, 0, "receive_macro");
		m_pusher.endContinuationFromRef();
		m_pusher.ifJmp();
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
		m_pusher.startContinuation();
		m_pusher.pushCall(0, 0, name);
		m_pusher.endContinuationFromRef();
		m_pusher.ifJmp();
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
				m_pusher.startContinuation();
				buildPublicFunctionSelector(functions, i, j);
				m_pusher.endContinuationFromRef();
				m_pusher.ifJmp();
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