/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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

#include <tuple>
#include <numeric>
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
#include "TVM.hpp"

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
		if (to<ModifierDefinition>(mod->name().annotation().referencedDeclaration)) {
			ret.push_back(mod);
		}
	}
	return ret;
}

void TVMFunctionCompiler::endContinuation2(const bool doDrop) {
	int delta = m_pusher.stackSize() - m_controlFlowInfo.back().stackSize();
	if (doDrop) {
		m_pusher.drop(delta);
	} else {
		m_pusher.fixStack(-delta); // fix stack
	}
	m_pusher.endContinuation();
}

bool TVMFunctionCompiler::hasLoop() const {
	return std::any_of(m_controlFlowInfo.begin(), m_controlFlowInfo.end(), [](const ControlFlowInfo& info){
		return info.isLoop();
	});
}

std::optional<ControlFlowInfo> TVMFunctionCompiler::lastAnalyzeFlag() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).hasAnalyzeFlag()) {
			return m_controlFlowInfo.at(i);
		}
	}
	return std::nullopt;
}

std::optional<ControlFlowInfo> TVMFunctionCompiler::lastLoop() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).isLoop()) {
			return m_controlFlowInfo.at(i);
		}
	}
	return std::nullopt;
}

bool TVMFunctionCompiler::lastAnalyzerBeforeLoop() const {
	int n = m_controlFlowInfo.size();
	for (int i = n - 1; i >= 0; --i) {
		if (m_controlFlowInfo.at(i).isLoop()) {
			return false;
		}
		if (m_controlFlowInfo.at(i).hasAnalyzeFlag()) {
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
	auto f = createNode<Function>(0, 0, "upd_only_time_in_c4", nullopt, Function::FunctionType::Fragment, block);
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateC4ToC7(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	pusher.pushRoot();
	pusher << "CTOS";
	pusher << "LDU 256      ; pubkey c4";
	if (pusher.ctx().storeTimestampInC4()) {
		pusher << "LDU 64       ; pubkey timestamp c4";
	}
	pusher << "LDU 1      ; ctor flag";
	if (pusher.ctx().usage().hasAwaitCall()) {
		pusher << "LDI 1       ; await flag";
		pusher.dropUnder(1, 1);
	}

	pusher.getStack().change(+1); // slice
	// slice on stack
	std::vector<VariableDeclaration const *> stateVars = pusher.ctx().c4StateVariables();
	std::vector<Type const*> stateVarTypes;
	std::transform(stateVars.begin(), stateVars.end(), std::back_inserter(stateVarTypes),
		[](VariableDeclaration const * var){
		return var->type();
	});
	const int ss = pusher.stackSize();
	ChainDataDecoder decoder{&pusher};
	decoder.decodeData(pusher.ctx().getOffsetC4(),
					   pusher.ctx().usage().hasAwaitCall() ? 1 : 0,
					   stateVarTypes);

	int const varQty = stateVarTypes.size();
	auto const nostorageStateVars = pusher.ctx().nostorageStateVars();
	int const nostorageVarQty = nostorageStateVars.size();
	if (pusher.ctx().tooMuchStateVariables()) {
		for (VariableDeclaration const * var : nostorageStateVars)
			pusher.pushDefaultValue(var->type());
		for (int i = 0; i < TvmConst::C7::FirstIndexForVariables; ++i)
			pusher.getGlob(i);
		pusher.blockSwap(varQty + nostorageVarQty, TvmConst::C7::FirstIndexForVariables);
		pusher.makeTuple(varQty + nostorageVarQty + TvmConst::C7::FirstIndexForVariables);
		pusher.popC7();
	} else {
		for (VariableDeclaration const * var : nostorageStateVars)
			pusher.pushDefaultValue(var->type());
		for (VariableDeclaration const * var : nostorageStateVars | boost::adaptors::reversed)
			pusher.setGlob(var);
		for (int i = varQty - 1; i >= 0; --i)
			pusher.setGlob(TvmConst::C7::FirstIndexForVariables + i);
	}
	solAssert(ss - 1 == pusher.stackSize(), "");

	pusher.fixStack(+1); // fix stack
	pusher.setGlob(TvmConst::C7::ConstructorFlag);

	if (pusher.ctx().storeTimestampInC4()) {
		pusher.setGlob(TvmConst::C7::ReplayProtTime);
	}

	pusher.fixStack(+1); // fix stack
	pusher.setGlob(TvmConst::C7::TvmPubkey);

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "c4_to_c7", nullopt, Function::FunctionType::Fragment, block);
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateDefaultC4(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	std::vector<VariableDeclaration const *> stateVars = ctx.c4StateVariables();
	for (VariableDeclaration const* var : stateVars | boost::adaptors::reversed)
		pusher.pushDefaultValue(var->type());
	if (ctx.storeTimestampInC4())
		pusher.pushInt(0);
	pusher.pushInt(0); // pubkey
	pusher << "NEWC";
	pusher << "STU 256";
	if (ctx.storeTimestampInC4())
		pusher << "STU 64";
	pusher << "STZERO"; // constructor flag
	if (ctx.usage().hasAwaitCall()) {
		pusher << "STZERO";
	}
	const std::vector<Type const *>& memberTypes = ctx.c4StateVariableTypes();
	if (!memberTypes.empty()) {
		ChainDataEncoder encoder{&pusher};
		DecodePositionAbiV2 position{ctx.getOffsetC4(), ctx.usage().hasAwaitCall() ? 1 : 0, memberTypes};
		encoder.encodeParameters(memberTypes, position);
	}
	pusher << "ENDC";

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "default_data_cell", nullopt, Function::FunctionType::Fragment, block);
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
	pusher.makeTuple(n);
	StructCompiler sc{&pusher, types, names};
	sc.tupleToBuilder();
	pusher << "ENDC";
	return createNode<Function>(0, 0, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateNewArrays(TVMCompilerContext& ctx, std::string const& name, FunctionCall const* arr) {
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	FunctionCallCompiler{pusher, *arr, true}.honestArrayCreation(true);
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateConstArrays(TVMCompilerContext& ctx, std::string const& name, TupleExpression const* arr) {
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	solAssert(arr->isInlineArray(), "");
	TVMExpressionCompiler{pusher}.visitHonest(*arr, true);
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
}

Pointer<Function>
TVMFunctionCompiler::generateFunction(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	std::string const& name
) {
	ctx.setCurrentFunction(function, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	funCompiler.pushLocation(*function);
	funCompiler.visitFunctionWithModifiers();
	funCompiler.pushLocation(*function, true);
	int take = function->parameters().size();
	int ret = function->returnParameters().size();
	ctx.resetCurrentFunction();
	uint32_t id = ChainDataEncoder::toHash256(name);
	return createNode<Function>(take, ret, name, id, Function::FunctionType::Fragment, pusher.getBlock(), function);
}

Pointer<Function>
TVMFunctionCompiler::generateOnCodeUpgrade(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	const std::string name = ctx.getFunctionInternalName(function, false);
	ctx.setCurrentFunction(function, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	funCompiler.visitFunctionWithModifiers();

	pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	pusher << "COMMIT";
	pusher._throw("THROW 0");
	int take = function->parameters().size();
	ctx.resetCurrentFunction();
	uint32_t id = function->functionID() ? function->functionID().value() : ChainDataEncoder::toHash256(name);
	return createNode<Function>(take, 0, name,  id, Function::FunctionType::OnCodeUpgrade,
								pusher.getBlock(), function);
}

Pointer<Function>
TVMFunctionCompiler::generateOnTickTock(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	ctx.setCurrentFunction(function, "onTickTock");
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
		pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	}

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	funCompiler.setCopyleftAndTryCatch();
	funCompiler.setGlobSenderAddressIfNeed();
	funCompiler.visitFunctionWithModifiers();


	if (!isPure) {
		pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	}
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, "onTickTock", nullopt, Function::FunctionType::OnTickTock, pusher.getBlock());
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
	std::string name = function->name();
	ctx.setCurrentFunction(function, name);

	StackPusher pusher{&ctx};
	Function::FunctionType type = Function::FunctionType::Fragment;
	Pointer<CodeBlock> block;

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	pusher.fixStack(+1); // slice with args
	pusher.fixStack(+1); // functionId
	pusher.drop(); // drop function id
	pusher.checkCtorCalled();
	funCompiler.pushC4ToC7IfNeed();

	funCompiler.pushLocation(*function);
	const bool isResponsible = function->isResponsible();
	if (isResponsible) {
		const int saveStackSize = pusher.stackSize();
		pusher << "LDU 32"; // callbackId slice
		pusher.getGlob(TvmConst::C7::ReturnParams); // callbackId slice c7[4]
		pusher.blockSwap(1, 2); // slice c7[4] callbackId
		pusher.setIndexQ(TvmConst::C7::ReturnParam::CallbackFunctionId); // slice c7[4]
		pusher.setGlob(TvmConst::C7::ReturnParams); // slice
		solAssert(saveStackSize == pusher.stackSize(), "");
	}
	funCompiler.decodeFunctionParamsAndInitVars(isResponsible);
	funCompiler.pushLocation(*function, true);

	int paramQty = function->parameters().size();
	int retQty = function->returnParameters().size();
	// stack: selector, arg0, arg1, arg2 ...
	// +1 because function may use selector
	pusher.pushFragmentInCallRef(paramQty + 1, retQty + 1, pusher.ctx().getFunctionInternalName(function));

	solAssert(pusher.stackSize() == retQty, "");
	// emit
	funCompiler.emitOnPublicFunctionReturn();

	pusher.ensureSize(0, "");

	funCompiler.updC4IfItNeeds();
	// set flag meaning function is called
	pusher._throw("THROW 0");

	block = pusher.getBlock();
	ctx.resetCurrentFunction();
	// takes selector, sliceWithBody, functionId
	// returns nothing
	return createNode<Function>(3, 0, name, nullopt, type, block);
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
	pusher.ctx().setCurrentFunction(nullptr, vd->name());
	TVMFunctionCompiler funCompiler{pusher, nullptr};
	pusher.fixStack(+2); // stack: functionId msgBody
	pusher.drop(); // drop function id
	pusher << "ENDS";
	pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	pusher.getGlob(vd);

	// check ext msg
	pusher.pushS(1);
	pusher.startContinuation();
	pusher.fixStack(-1); // fix stack

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
	pusher.ctx().resetCurrentFunction();
	return createNode<Function>(2, 1, vd->name(), nullopt, Function::FunctionType::PublicStateVariableGetter, pusher.getBlock());

}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunctionSelector(TVMCompilerContext& ctx, ContractDefinition const *contract) {
	std::string name = "public_function_selector";
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	const std::vector<std::pair<uint32_t, std::string>>& functions = pusher.ctx().getPublicFunctions();

	TVMFunctionCompiler compiler{pusher, contract};
	PublicFunctionSelector pfs{int(functions.size())};
	compiler.buildPublicFunctionSelector(functions, 0, functions.size(), pfs);
	ctx.resetCurrentFunction();
	return createNode<Function>(1, 1, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateLibFunctionWithObject(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function
) {
	bool const withObject = true;
	const std::string name = TVMCompilerContext::getLibFunctionName(function, withObject);
	ctx.setCurrentFunction(function, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	funCompiler.visitFunctionWithModifiers();
	int take = function->parameters().size();
	int ret = function->returnParameters().size();
	ctx.resetCurrentFunction();
	return createNode<Function>(take, ret + 1, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateReceive(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	std::string const name = "receive";
	ctx.setCurrentFunction(function, name);
	auto f = generateReceiveOrFallbackOrOnBounce(ctx, function, name, 0);
	ctx.resetCurrentFunction();
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateFallback(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	std::string const name = "fallback";
	ctx.setCurrentFunction(function, name);
	auto f = generateReceiveOrFallbackOrOnBounce(ctx, function, name, 0);
	ctx.resetCurrentFunction();
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateOnBounce(TVMCompilerContext& ctx, const FunctionDefinition *function) {
	ctx.setCurrentFunction(function, "on_bounce");
	Pointer<Function> f = generateReceiveOrFallbackOrOnBounce(ctx, function, "on_bounce", 1);
	ctx.resetCurrentFunction();
	return f;
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
	return createNode<Function>(take, 0, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
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
	m_pusher.fixStack(-1); // fix stack
	bool isResponsible = m_pusher.ctx().currentFunction()->isResponsible();

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
		m_pusher << "CTOS";
		m_pusher << "LDU 2";
		m_pusher << "LDMSGADDR";
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
		m_pusher.fixStack(params.size()); // fix stack
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
		solAssert(!m_function->isExternalMsg() || !m_function->isInternalMsg(), "");

		if (m_function->isExternalMsg() || m_function->isInternalMsg())
			m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"DEPTH",
				"ADDCONST -5",
				"PICK",
			}, 0, 1, true));

		if (m_function->isExternalMsg()) {
			m_pusher << "EQINT -1";
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ByExtMsgOnly));
		} else if (m_function->isInternalMsg()) {
			m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ByIntMsgOnly));
		}
	}


	if (m_currentModifier == static_cast<int>(functionModifiers().size())) {
		int modSize = m_pusher.stackSize() - argQty;
		m_pusher.blockSwap(argQty, modSize); // break stack

		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.fixStack(-modSize); // fix stack

		TVMFunctionCompiler funCompiler{pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, 0};
		funCompiler.visitModifierOrFunctionBlock(m_function->body(), argQty, retQty, nameRetQty);
		m_pusher.add(pusher);

		m_pusher.blockSwap(modSize, retQty); // break stack
	} else {
		int ss = m_pusher.stackSize();
		ModifierInvocation const *invocation = functionModifiers()[m_currentModifier].get();
		auto modifierDefinition = to<ModifierDefinition>(invocation->name().annotation().referencedDeclaration);
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
					m_pusher.convert(variables.at(i)->type(), tuple.at(i)->annotation().type);
				} else {
					++bad;
					m_pusher.drop();
				}
			}
		} else {
			acceptExpr(init);
			if (varQty == 1) {
				m_pusher.convert(variables.at(0)->type(), init->annotation().type);
			} else {
				auto tuple = to<TupleType>(init->annotation().type);
				std::vector<bool> hasName(varQty);
				for (int i = varQty - 1; i >= 0; --i) {
					if (variables.at(i) != nullptr) {
						m_pusher.convert(variables.at(i)->type(), tuple->components().at(i));
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
			m_pusher.fixStack(-funTrash - argQty); // TODO why -argQty ?
		}
	} else {
		const int delta = m_pusher.stackSize() - startStackSize;
		solAssert(delta >= 0, "");
		// !lastIsRet
		if (!_block.statements().empty() && to<Return>(_block.statements().back().get()) == nullptr) {
			m_pusher.drop(delta);
		} else {
			m_pusher.fixStack(-delta); // fix stack
		}
	}

	pushLocation(_block, true);
}

bool TVMFunctionCompiler::visit(Block const& _block) {
	m_pusher.ctx().startBlock(_block.unchecked());
	acceptBody(_block, std::nullopt);
	m_pusher.ctx().endBlock();
	return false;
}

bool TVMFunctionCompiler::visit(ExpressionStatement const& _statement) {
	if (!*_statement.expression().annotation().isPure) {
		pushLocation(_statement);
		auto savedStackSize = m_pusher.stackSize();
		acceptExpr(&_statement.expression(), false);
		// TODO DELETE FIX ME
//		m_pusher.ensureSize(savedStackSize, _statement.location().hasText());
		m_pusher.ensureSize(savedStackSize, "");
		pushLocation(_statement, true);
	}
	return false;
}

bool TVMFunctionCompiler::visit(TryStatement const& _tryState) {
	// return flag
	const int stackSize = m_pusher.stackSize();
	CFAnalyzer ci{_tryState};
	ControlFlowInfo info = beforeTryOrIfCheck(ci);
	m_controlFlowInfo.push_back(info);

	// try body
	m_pusher.startContinuation();
	const int startStackSize = m_pusher.stackSize();
	_tryState.body().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - startStackSize);
	m_pusher.endContinuation();

	// try body
	m_pusher.startContinuation();
	if (_tryState.clause().parameters()) {
		for (ASTPointer<VariableDeclaration> const& variable : _tryState.clause().parameters()->parameters()) {
			m_pusher.getStack().add(variable.get(), true);
		}
	} else {
		m_pusher.fixStack(+2);
	}
	_tryState.clause().block().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - startStackSize);
	m_pusher.endContinuation();

	m_pusher.tryOpcode(ci.canReturn() || ci.canBreak());

	// bottom
	afterTryOrIfCheck(info);

	solAssert(stackSize == m_pusher.stackSize(), "TryStatement fail");

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
	ControlFlowInfo info = canUseJmp ?
		ControlFlowInfo{m_pusher.stackSize(), false, false} :
		beforeTryOrIfCheck(ci);
	m_controlFlowInfo.push_back(info);

	// condition
	acceptExpr(&_ifStatement.condition(), true);
	m_pusher.fixStack(-1); // drop condition
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

	// bottom
	afterTryOrIfCheck(info);
	m_pusher.ensureSize(saveStackSize, "");

	return false;
}

void TVMFunctionCompiler::doWhile(WhileStatement const &_whileStatement) {
	int saveStackSize = m_pusher.stackSize();

	// header
	auto [ci, info] = pushControlFlowFlag(_whileStatement.body());
//	CFAnalyzer ci;
//	ControlFlowInfo info;
//	std::tie(ci, info) = pushControlFlowFlag(_whileStatement.body());

	// body
	m_pusher.startContinuation();
	int ss = m_pusher.stackSize();
	if (ci->canReturn() || ci->canBreak() || ci->canContinue()) {
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
	m_pusher << "NOT";
	m_pusher.fixStack(-1); // drop condition
	m_pusher.endContinuation();

	m_pusher.until(ci->canBreak() || ci->canReturn());

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0, info.hasAnalyzeFlag());

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
		m_pusher.fixStack(-1); // fix stack
	} else {
		m_pusher << "TRUE";
		m_pusher.fixStack(-1); // fix stack
	}
	m_pusher.endContinuation();
	m_pusher.ensureSize(stackSize, "visitForOrWhileCondition");
}

void TVMFunctionCompiler::afterLoopCheck(const std::unique_ptr<CFAnalyzer>& ci, const int& loopVarQty, bool _doAnalyzeFlag) {
	std::optional<ControlFlowInfo> analyzeFlag = lastAnalyzeFlag();
	std::optional<ControlFlowInfo> loopFlag = lastLoop();

	if (_doAnalyzeFlag) {
		m_pusher.startOpaque();
		if (analyzeFlag.has_value()) {
			m_pusher.pushS(0);
		}
		if (ci->canBreak() || ci->canContinue()) {
			m_pusher << "EQINT " + toString(TvmConst::RETURN_FLAG);
		}
		loopFlag.has_value() ? m_pusher.ifRetAlt() : m_pusher.ifret();
		if (analyzeFlag.has_value()) {
			m_pusher.drop();
		}
		m_pusher.endOpaque(1, 0);
	}
	m_pusher.drop(loopVarQty);
}

ControlFlowInfo TVMFunctionCompiler::beforeTryOrIfCheck(CFAnalyzer const& ci) {
	bool hasAnalyzeFlag{};
	bool isLoop = false;
	if (ci.canContinue() || (!hasLoop() && ci.canReturn())) {
		m_pusher.declRetFlag();
		hasAnalyzeFlag = true;
	}
	return ControlFlowInfo{m_pusher.stackSize(), hasAnalyzeFlag, isLoop};
}

void TVMFunctionCompiler::afterTryOrIfCheck(ControlFlowInfo const& info) {
	m_controlFlowInfo.pop_back();
	if (info.hasAnalyzeFlag()) {
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
}

bool TVMFunctionCompiler::visit(WhileStatement const &_whileStatement) {
	int saveStackSizeForWhile = m_pusher.stackSize();

	if (_whileStatement.loopType() == WhileStatement::LoopType::DO_WHILE) {
		doWhile(_whileStatement);
		return false;
	}

	// header
	auto [ci, info] = pushControlFlowFlag(_whileStatement.body());

	int saveStackSize = m_pusher.stackSize();

	// condition
	if (_whileStatement.loopType() == WhileStatement::LoopType::REPEAT) {
		acceptExpr(&_whileStatement.condition());
		m_pusher.fixStack(-1);
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
		m_pusher.repeat(ci->canBreak() || ci->canReturn());
	else
		m_pusher._while(ci->canBreak() || ci->canReturn());

	m_controlFlowInfo.pop_back();

	// bottom
	afterLoopCheck(ci, 0, info.hasAnalyzeFlag());

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
		if (arrayType->isByteArrayOrString()) {
			m_pusher << "CTOS";
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

		m_pusher.fixStack(-2); // fix stack
		auto iterKey = vds->declarations().at(0).get();
		auto iterVal = vds->declarations().at(1).get();
		if (iterKey == nullptr)
			m_pusher.fixStack(+1);
		else
			m_pusher.getStack().add(iterKey, true);
		if (iterVal == nullptr)
			m_pusher.fixStack(+1);
		else
			m_pusher.getStack().add(iterVal, true);

		// stack: dict minKey(pub) value minKey(private)
		loopVarQty = 4;
	} else {
		solUnimplemented("");
	}
	m_pusher.ensureSize(saveStackSize + loopVarQty, "for");

	// return flag
	auto [ci, info] = pushControlFlowFlag(_forStatement.body());

	// condition
	std::function<void()> pushCondition = [&]() {
		if (arrayType) {
			if (arrayType->isByteArrayOrString()) {
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1); // stack: cell value [flag] cell
				m_pusher << "SEMPTY";
				m_pusher << "NOT";
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
				m_pusher << "ISNULL";
				m_pusher << "NOT";
			}
		} else if (mappingType) {
			// stack: dict minKey(private) minKey(pub) value  [flag]
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2);
			m_pusher << "ISNULL";
			m_pusher << "NOT";
		} else {
			solUnimplemented("");
		}
	};
	visitForOrWhileCondition(pushCondition);


	// body
	std::function<void()> pushStartBody = [&]() {
		if (arrayType) {
			if (arrayType->isByteArrayOrString()) {
				const int ss = m_pusher.stackSize();
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1);
				// stack: cell value [flag] cell

				m_pusher.startOpaque();
				m_pusher.pushAsym("LDUQ 8");
				m_pusher.fixStack(+1); // fix stack
				m_pusher.startContinuation();
				// stack: cell value [flag] slice
				m_pusher << "PLDREF";
				m_pusher << "CTOS";
				m_pusher << "LDU 8";
				m_pusher.fixStack(-2); // fix stack
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
			if (arrayType->isByteArrayOrString()) {
				// do nothing
			} else {
				// stack: dict 0 value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict index value [flag] index
				m_pusher << "INC"; // stack: dict index value [flag] newIndex
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
	afterLoopCheck(ci, loopVarQty, info.hasAnalyzeFlag());
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

std::pair<std::unique_ptr<CFAnalyzer>, ControlFlowInfo> TVMFunctionCompiler::pushControlFlowFlag(Statement const& body) {
	std::unique_ptr<CFAnalyzer> ci = std::make_unique<CFAnalyzer>(body);
	bool isLoop = true;
	int stackSize = -1;
	bool hasAnalyzeFlag = false;
	if (ci->canReturn()) {
		m_pusher.declRetFlag();
		hasAnalyzeFlag = true;
	}
	stackSize = m_pusher.stackSize();
	ControlFlowInfo info {stackSize, hasAnalyzeFlag, isLoop};
	m_controlFlowInfo.push_back(info);
	return {std::move(ci), info};
}

void TVMFunctionCompiler::visitBodyOfForLoop(
	const std::unique_ptr<CFAnalyzer>& ci,
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
	if (ci->canContinue()) { // TODO and have loopExpression
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
	m_pusher._while(ci->canBreak() || ci->canReturn());
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
	auto [ci, info] = pushControlFlowFlag(_forStatement.body());

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
	afterLoopCheck(ci, haveDeclLoopVar, info.hasAnalyzeFlag());
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
			m_pusher.convert(leftType, rightType);
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
		m_pusher.fixStack(revertDelta); // fix stack
	} else { // all continuation are run by JMPX
		m_pusher.fixStack(revertDelta); // fix stack
	}

	if (hasLoop()) {
		m_pusher.retAlt();
	} else {
		m_pusher.ret();
	}
	m_pusher.endRetOrBreakOrCont(retCount);

	return false;
}

bool TVMFunctionCompiler::visit(Break const&) {
	const int sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize();
	m_pusher.startContinuation();

	m_pusher.drop(sizeDelta);
	m_pusher.retAlt();

	m_pusher.fixStack(sizeDelta); // fix stack
	m_pusher.endRetOrBreakOrCont(0);

	return false;
}

bool TVMFunctionCompiler::visit(Continue const&) {
	bool hasAnalyzer = lastAnalyzerBeforeLoop();
	const int sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize();
	m_pusher.startContinuation();

	if (hasAnalyzer) {
		m_pusher.drop(sizeDelta + (lastLoop().value().hasAnalyzeFlag() ? 1 : 0));
		m_pusher.pushInt(TvmConst::CONTINUE_FLAG);
	} else {
		m_pusher.drop(sizeDelta);
	}

	m_pusher.ret();

	m_pusher.fixStack(sizeDelta); // fix stack
	m_pusher.endRetOrBreakOrCont(0);
	return false;
}

bool TVMFunctionCompiler::visit(EmitStatement const &_emit) {
	auto eventCall = to<FunctionCall>(&_emit.eventCall());
	solAssert(eventCall, "");
	CallableDeclaration const * def = getFunctionDeclarationOrConstructor(&eventCall->expression());
	solAssert(def, "Event Declaration was not found");
	auto eventDef = to<EventDefinition>(def);

	std::vector<ASTPointer<Expression const>> args = eventCall->arguments();
	for (ASTPointer<Expression const> const& arg : args | boost::adaptors::reversed) {
		TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
	}

	string name = eventName(eventDef);

	auto appendBody = [&](int builderSize) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			convertArray(eventDef->parameters()),
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(
					name, getTypesFromVarDecls(eventDef->parameters()), nullptr,
					ReasonOfOutboundMessage::EmitEventExternal, std::nullopt, false),
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

void TVMFunctionCompiler::setGlobSenderAddressIfNeed() {
	if (m_pusher.ctx().usage().hasMsgSender()) {
		m_pusher.pushSlice("x8000000000000000000000000000000000000000000000000000000000000000001_");
		m_pusher.setGlob(TvmConst::C7::SenderAddress);
	}
}

void TVMFunctionCompiler::setCtorFlag() {
	m_pusher.pushRoot();
	m_pusher << "CTOS";
	bool hasTime = m_pusher.ctx().storeTimestampInC4();
	if (hasTime)
		m_pusher.pushInt(256 + 64);
	else
		m_pusher.pushInt(256);
	m_pusher << "SDSKIPFIRST";
	m_pusher << "PLDI 1";
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
}

Pointer<Function> TVMFunctionCompiler::generateMainExternal(
	TVMCompilerContext& ctx,
	ContractDefinition const *contract
) {
	//	stack:
	//	contract_balance
	//	msg_balance is always zero
	//	msg_cell
	//	msg_body_slice
	//	transaction_id = -1

	std::string name = "main_external";
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler f{pusher, contract};

	f.setCopyleftAndTryCatch();
	f.setGlobSenderAddressIfNeed();

	pusher.pushS(1);
	pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");

	f.checkSignatureAndReadPublicKey();
	if (pusher.ctx().afterSignatureCheck()) {
		// ... msg_cell msg_body_slice -1 rest_msg_body_slice
		pusher.pushS(3);
		pusher.pushInlineFunction("afterSignatureCheck", 2, 1);
	} else {
		if (pusher.ctx().pragmaHelper().hasTime())
			f.defaultReplayProtection();
		if (pusher.ctx().pragmaHelper().hasExpire())
			f.expire();
	}

	// msg_body
	pusher << "LDU 32 ; funcId body";
	pusher.exchange(1);

	f.callPublicFunctionOrFallback();
	ctx.resetCurrentFunction();
	return createNode<Function>(
		0, 0, name, nullopt,
		Function::FunctionType::MainExternal,
		pusher.getBlock()
	);
}

void TVMFunctionCompiler::pushMsgPubkey() {
	// signatureSlice msgSlice hashMsgSlice

	if (m_pusher.ctx().pragmaHelper().hasPubkey()) {
		m_pusher.exchange(1);
		m_pusher << "LDU 1 ; signatureSlice hashMsgSlice hasPubkey msgSlice";
		m_pusher.exchange(1); // signatureSlice hashMsgSlice msgSlice hasPubkey

		m_pusher.startContinuation();
		m_pusher << "LDU 256       ; signatureSlice hashMsgSlice pubkey msgSlice";
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

	m_pusher << "LDU 1 ; haveSign msgSlice";
	m_pusher.exchange(1);

	m_pusher.startContinuation();
	m_pusher.pushInt(512);
	m_pusher << "LDSLICEX";
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

	m_pusher << "HASHCU";
	// signatureSlice msgSlice msgHash
	pushMsgPubkey();
	// signatureSlice msgSlice msgHash pubkey
	m_pusher << "CHKSIGNU";
	// msgSlice isSigned
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::BadSignature));
	// msgSlice
	m_pusher.endContinuation();

	if (m_pusher.ctx().pragmaHelper().hasPubkey()) {
		// External inbound message does not have signature but have public key
		m_pusher.startContinuation();
		m_pusher << "LDU 1      ; hasPubkey msgSlice";
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
	m_pusher << "LDU 64                         ; timestamp msgSlice";
	m_pusher.exchange(1);
	m_pusher.pushFragment(1, 0, "__replayProtection");
}

void TVMFunctionCompiler::expire() {
	m_pusher << "LDU 32  ; expireAt msgSlice";
	m_pusher.exchange(1);
	m_pusher << "NOW     ; msgSlice expireAt now";
	m_pusher << "GREATER ; msgSlice expireAt>now";
	m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::MessageIsExpired));
}

void TVMFunctionCompiler::callPublicFunctionOrFallback() {
	m_pusher.pushFragmentInCallRef(0, 0, "public_function_selector");

	if (m_pusher.ctx().isFallBackGenerated()) {
		m_pusher.drop(2);
		m_pusher.startContinuation();
		m_pusher.pushFragment(0, 0, "fallback");
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

	std::string name = "main_internal";
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, contract};

	funCompiler.setCopyleftAndTryCatch();
	funCompiler.setCtorFlag();

	pusher.pushS(2);
	pusher << "CTOS";
	// stack: int_msg_info

	ContactsUsageScanner const &sc = pusher.ctx().usage();
	if (sc.hasMsgSender() || sc.hasResponsibleFunction() || sc.hasAwaitCall()) {
		pusher << "LDU 4       ; bounced tail";
		pusher << "LDMSGADDR   ; bounced src tail";
		pusher.drop();
		if (sc.hasAwaitCall()) {
			pusher.pushFragmentInCallRef(0, 0, "check_resume");
		}
		pusher.setGlob(TvmConst::C7::SenderAddress);
		pusher << "MODPOW2 1";
	} else {
		pusher << "PLDU 4";
		pusher << "MODPOW2 1";
	}
	// stack: isBounced

	// set default params for responsible func
	if (sc.hasResponsibleFunction()) {
		pusher.getGlob(TvmConst::C7::ReturnParams);
		pusher << "TRUE"; // bounce
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
		pusher << "LDSLICE 32";
		pusher.dropUnder(1, 1);
		pusher.pushFragment(0, 0, "on_bounce");
		pusher.endContinuationFromRef();
		pusher.ifJmp();
	} else {
		pusher.ifret();
	}

	funCompiler.pushReceiveOrFallback();

	pusher.exchange(1);
	funCompiler.callPublicFunctionOrFallback();
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, nullopt, Function::FunctionType::MainInternal, pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateCheckResume(TVMCompilerContext& ctx) {
	std::string const name = "check_resume";
	ctx.setCurrentFunction(nullptr, name);
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
		.inline c4_to_c7
	}
	CALLX
}
PUSHCONT {
	DROP2
}
IFELSE
)";
	solAssert(!ctx.callGraph().tryToAddEdge(ctx.currentFunctionName(), "c4_to_c7"), "");
	boost::replace_all(code, "TvmConst::RuntimeException::WrongAwaitAddress", toString(TvmConst::RuntimeException::WrongAwaitAddress));
	boost::replace_all(code, "offset", toString(256 + (pusher.ctx().storeTimestampInC4() ? 64 : 0) + 1));
	vector<string> lines = split(code);
	pusher.push(createNode<HardCode>(lines, 0, 0, false));
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, nullopt, Function::FunctionType::Fragment, pusher.getBlock());
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
		m_pusher.fixStack(-1); // fix stack
		m_pusher.startContinuation();
		m_pusher.pushFragment(0, 0, "c4_to_c7");
		m_pusher.endContinuationFromRef();
		m_pusher._if();
	}
}

void TVMFunctionCompiler::updC4IfItNeeds() {
	// c7_to_c4 if need
	//	solAssert(m_pusher.stackSize() == 0, "");
	if (m_function->stateMutability() == StateMutability::NonPayable) {
		m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	} else {
		// if it's external message, then we save values for replay protection
		if (m_pusher.ctx().afterSignatureCheck() == nullptr &&
			m_pusher.ctx().pragmaHelper().hasTime() &&
			m_pusher.ctx().c4StateVariables().size() >= 2 // just optimization: if varQty == 1, then it's better to call c7_to_c4
		) {
			m_pusher.pushS(0);
			m_pusher.startContinuation();
			m_pusher.pushFragment(0, 0, "upd_only_time_in_c4");
			m_pusher.endContinuationFromRef();
			m_pusher._if();
		} else {
			m_pusher.pushS(0);
			m_pusher.startContinuation();
			m_pusher.pushFragment(0, 0, "c7_to_c4");
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
			m_pusher.pushFragmentInCallRef(0, 0, "fallback");
			m_pusher._throw("THROW 0");
			m_pusher.endContinuation();
			m_pusher.ifNot();
		} else {
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::NoFallback) + " ; funcId body'");
		}
	};

	if (!isEmptyFunction(m_contract->receiveFunction())) {
		m_pusher.pushS(1);
		m_pusher << "SEMPTY     ; isEmpty";
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
			m_pusher << "EQINT 0 ; funcId body' isZero";
			m_pusher.pushS(0); // funcId body' isZero isZero"
			m_pusher.startContinuation();
			m_pusher.dropUnder(2, 1);
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.endContinuation();
		m_pusher.ifNot();
		m_pusher.startContinuation();
		m_pusher.pushFragment(0, 0, "receive");
		m_pusher.endContinuationFromRef();
		m_pusher.ifJmp();
	} else {
		m_pusher.pushS(1);
		m_pusher << "SEMPTY     ; isEmpty";
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
	int right,
	PublicFunctionSelector const& pfs
) {
	auto pushOne = [&](uint32_t functionId, const std::string& name) {
		m_pusher.pushS(0);
		m_pusher.pushInt(functionId);
		m_pusher << "EQUAL";
		m_pusher.fixStack(-1); // fix stack
		m_pusher.startContinuation();
		m_pusher.pushFragment(0, 0, name);
		m_pusher.endContinuationFromRef();
		m_pusher.ifJmp();
	};

	int const n = right - left;
	std::vector<int> const& sizes = pfs.groupSizes(n);
	int pos = left;
	for (int const groupSize : sizes) {
		if (groupSize == 1) {
			const auto& [functionId, name] = functions.at(pos);
			pushOne(functionId, name);
		} else {
			const auto& [functionId, name] = functions.at(pos + groupSize - 1);
			m_pusher.pushS(0);
			m_pusher.pushInt(functionId);
			m_pusher << "LEQ";
			m_pusher.startContinuation();
			buildPublicFunctionSelector(functions, pos, pos + groupSize, pfs);
			m_pusher.endContinuationFromRef();
			m_pusher.ifJmp();

		}
		pos += groupSize;
	}
}

void TVMFunctionCompiler::pushLocation(const ASTNode& node, bool reset) {
	SourceReference sr = SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &node.location());
	const int line = reset ? 0 : sr.position.line + 1;
	m_pusher.pushLoc(sr.sourceName, line);
}

TVMConstructorCompiler::TVMConstructorCompiler(StackPusher &pusher) :
	TVMFunctionCompiler{pusher, pusher.ctx().getContract()},
	m_pusher{pusher}
{

}

void TVMConstructorCompiler::dfs(ContractDefinition const *c) {
	if (used[c]) {
		return;
	}
	used[c] = true;
	dfsOrder.push_back(c);
	path[c] = dfsOrder;
	for (const ASTPointer<InheritanceSpecifier>& inherSpec : c->baseContracts()) {
		auto base = to<ContractDefinition>(inherSpec->name().annotation().referencedDeclaration);
		ast_vec<Expression> const*  agrs = inherSpec->arguments();
		if (agrs != nullptr && !agrs->empty()) {
			m_args[base] = inherSpec->arguments();
			dfs(base);
		}
	}
	if (c->constructor() != nullptr) {
		for (const ASTPointer<ModifierInvocation> &modInvoc : c->constructor()->modifiers()) {
			auto base = to<ContractDefinition>(modInvoc->name().annotation().referencedDeclaration);
			if (base != nullptr) {
				if (modInvoc->arguments() != nullptr) {
					m_args[base] = modInvoc->arguments();
					dfs(base);
				}
			}
		}
	}
	dfsOrder.pop_back();
}

Pointer<Function> TVMConstructorCompiler::generateConstructors() {
	FunctionDefinition const* constructor = m_pusher.ctx().getContract()->constructor();
	m_pusher.ctx().setCurrentFunction(constructor, "constructor");

	{
		ChainDataEncoder encode{&m_pusher};
		uint32_t functionId =
			constructor != nullptr ?
			encode.calculateFunctionIDWithReason(constructor, ReasonOfOutboundMessage::RemoteCallInternal) :
			encode.calculateConstructorFunctionID();
		m_pusher.ctx().addPublicFunction(functionId, "constructor");
	}

	m_pusher.fixStack(+1); // push encoded params of constructor
	m_pusher.fixStack(+1); // functionID
	m_pusher.drop();

	beginConstructor();

	std::vector<ContractDefinition const*> linearizedBaseContracts =
		m_pusher.ctx().getContract()->annotation().linearizedBaseContracts; // from derived to base
	for (ContractDefinition const* c : linearizedBaseContracts)
		dfs(c);

	int take{};
	if (constructor == nullptr) {
		m_pusher << "ENDS";
	} else {
		take = constructor->parameters().size();
		vector<Type const*> types = getParams(constructor->parameters()).first;
		ChainDataDecoder{&m_pusher}.decodeFunctionParameters(types, false);
		m_pusher.getStack().change(-static_cast<int>(constructor->parameters().size()));
		for (const ASTPointer<VariableDeclaration>& variable: constructor->parameters())
			m_pusher.getStack().add(variable.get(), true);
	}
	solAssert(m_pusher.stackSize() == take, "");
	std::set<ContractDefinition const*> areParamsOnStack;
	areParamsOnStack.insert(linearizedBaseContracts.at(0));
	for (ContractDefinition const* c : linearizedBaseContracts | boost::adaptors::reversed)
		if (c->constructor() == nullptr || c->constructor()->parameters().empty())
			areParamsOnStack.insert(c);

	bool haveConstructor = false;
	for (ContractDefinition const* c : linearizedBaseContracts | boost::adaptors::reversed) {
		if (c->constructor() == nullptr)
			continue;
		haveConstructor = true;
		for (ContractDefinition const* parent : path[c]) {
			if (areParamsOnStack.count(parent) == 0) {
				areParamsOnStack.insert(parent);
				for (size_t i = 0; i < parent->constructor()->parameters().size(); ++i) {
					TVMExpressionCompiler(m_pusher).acceptExpr((*m_args[parent])[i].get(), true);
					m_pusher.getStack().add(parent->constructor()->parameters()[i].get(), false);
				}
			}
		}
		int take2 = c->constructor()->parameters().size();
		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.takeLast(take2);
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, c->constructor(), false);
		m_pusher.fixStack(-take2); // fix stack
		m_pusher.add(pusher);
	}

	if (!haveConstructor)
		m_pusher << "ACCEPT";

//	solAssert(m_pusher.stackSize() == 0, "");
	m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	m_pusher._throw("THROW 0");

	m_pusher.ctx().resetCurrentFunction();
	Pointer<CodeBlock> block = m_pusher.getBlock();
	// take slice (contains params) and functionID
	Pointer<Function> f = createNode<Function>(2, 0, "constructor", nullopt, Function::FunctionType::Fragment, block);
	return f;
}

void TVMConstructorCompiler::beginConstructor() {
	// copy c4 to c7
	m_pusher.was_c4_to_c7_called();
	m_pusher.fixStack(-1); // fix stack

	m_pusher.startContinuation();
	m_pusher.pushFragment(0, 0, "c4_to_c7");
	m_pusher.endContinuationFromRef();
	m_pusher._if();

	// set state var, e.g. int m_x = 123;
	for (VariableDeclaration const *variable: m_pusher.ctx().c4StateVariables()) {
		if (Expression const* value = variable->value().get()) {
			acceptExpr(value);
			m_pusher.setGlob(variable);
		}
	}


	// generate constructor protection
	m_pusher.getGlob(TvmConst::C7::ConstructorFlag);
	m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ConstructorIsCalledTwice));
}

PublicFunctionSelector::PublicFunctionSelector(int _n) {
	maxPath = vector<int>(_n + 1, INF);
	sumPaths = vector<int>(_n + 1, INF);
	prev = vector<vector<int>>(_n + 1);
	maxPath[0] = INF;
	for (int n = 1; n <= _n; ++n) {
		maxPath[n] = INF;
		dfs(0, n);
	}
	//for (int n = 1; n <= _n; ++n)  cout
	//	<< std::setw(2) << n << ": "
	//	<< std::setw(4) << int(double(sumPaths[n]) / n) << " "
	//	<< std::setw(4) << maxPath[n]
	//	<< endl;
}

void PublicFunctionSelector::dfs(int pos, int n) {
	if (curGroupSize.size() > 4)
		return;
	int curSum = std::accumulate(curGroupSize.begin(), curGroupSize.end(), 0);
	if (curSum > n)
		return;

	if (curSum == n) {
		int curMaxPath = 0;
		int curSumPath = 0;
		for (int i = 0; i < int(curGroupSize.size()); ++i) {
			int giSize = curGroupSize.at(i);
			if (giSize == 1) {
				curMaxPath = max(curMaxPath, FAIL_JMP * i + OK_JMP);
				curSumPath += FAIL_JMP * i + OK_JMP;
			} else {
				curMaxPath = max(curMaxPath, FAIL_JMP * i + OK_JMP + maxPath.at(giSize));
				curSumPath += (FAIL_JMP * i + OK_JMP) * giSize + sumPaths.at(giSize);
			}
		}
		if (maxPath[n] > curMaxPath || (maxPath[n] == curMaxPath && sumPaths[n] > curSumPath)) {
			maxPath[n] = curMaxPath;
			sumPaths[n] = curSumPath;
			prev[n] = curGroupSize;
		}
	} else if (pos < int(curGroupSize.size())) {
		++curGroupSize[pos];
		dfs(pos, n);
		dfs(pos + 1, n);
		--curGroupSize[pos];
	} else {
		curGroupSize.push_back(1);
		dfs(pos, n);
		dfs(pos + 1, n);
		curGroupSize.pop_back();
	}
}
