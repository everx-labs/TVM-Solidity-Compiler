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
 * AST to TVM bytecode contract compiler
 */

#include <numeric>
#include <tuple>

#include <boost/algorithm/string/replace.hpp>

#include <liblangutil/SourceReferenceExtractor.h>

#include <libsolidity/codegen/DictOperations.hpp>
#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMFunctionCall.hpp>
#include <libsolidity/codegen/TVMFunctionCompiler.hpp>
#include <libsolidity/codegen/TVMStructCompiler.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;

namespace fs = boost::filesystem;

TVMFunctionCompiler::TVMFunctionCompiler(StackPusher& pusher, ContractDefinition const* contract):
	m_pusher{pusher},
	m_contract{contract} {}

TVMFunctionCompiler::TVMFunctionCompiler(
	StackPusher& pusher,
	int modifier,
	FunctionDefinition const* f,
	bool isLibraryWithObj,
	bool pushArgs,
	int startStackSize
):
	m_pusher{pusher},
	m_startStackSize{startStackSize},
	m_currentModifier{modifier},
	m_function{f},
	m_contract{m_function->annotation().contract},
	m_isLibraryWithObj{isLibraryWithObj},
	m_pushArgs{pushArgs} {}

ast_vec<ModifierInvocation> TVMFunctionCompiler::functionModifiers() const {
	ast_vec<ModifierInvocation> ret;
	for (ASTPointer<ModifierInvocation> const& mod: m_function->modifiers()) {
		if (to<ModifierDefinition>(mod->name().annotation().referencedDeclaration)) {
			ret.push_back(mod);
		}
	}
	return ret;
}

void TVMFunctionCompiler::endContinuation2(bool const doDrop) const {
	int delta = m_pusher.stackSize() - m_controlFlowInfo.back().stackSize();
	if (doDrop) {
		m_pusher.drop(delta);
	} else {
		m_pusher.fixStack(-delta); // fix stack
	}
	m_pusher.endContinuation();
}

bool TVMFunctionCompiler::hasLoop() const {
	return std::ranges::any_of(m_controlFlowInfo, [](ControlFlowInfo const& info) { return info.isLoop(); });
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
	pusher << "CTOS"
		   << "LDU 256"
		   << "LDU 64";
	pusher.popS(1);
	pusher.getGlob(TvmConst::C7::ReplayProtTime);
	// pubkey rest time
	pusher.rot();	 // rest time pubkey
	pusher << "NEWC" // rest time pubkey builder
		   << "STU 256"
		   << "STU 64"
		   << "STSLICE"
		   << "ENDC";
	pusher.popRoot();

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "upd_only_time_in_c4", std::nullopt, block, nullptr, true);
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateC4ToC7(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	int const startStackSize = pusher.stackSize();

	pusher.pushRoot();
	pusher << "CTOS";

	if (pusher.ctx().storageLayout().storePubkeyInC4())
		pusher << "LDU 256      ; pubkey c4";
	if (pusher.ctx().storageLayout().storeTimestampInC4())
		pusher << "LDU 64       ; pubkey timestamp c4";
	if (ctx.storageLayout().hasConstructor())
		pusher << "LDU 1      ; ctor flag";

	std::vector<VariableDeclaration const*> stateVars = pusher.ctx().storageLayout().usualStateVariables();
	std::vector<Type const*> stateVarTypes = getTypesFromVarDecls(stateVars);
	std::vector<VariableDeclaration const*> unpackedStateVars = pusher.ctx().storageLayout().unpackedStateVariables();
	std::vector<VariableDeclaration const*> transientStateVars = pusher.ctx().storageLayout().transientStateVars();

	ChainDataDecoder decoder{&pusher};
	decoder.decodeData(pusher.ctx().storageLayout().getOffsetC4(), 0, stateVarTypes, !unpackedStateVars.empty());

	int const varQty = stateVarTypes.size();
	int const transientVarQty = transientStateVars.size();
	int const totalVarQty = varQty + transientVarQty + (unpackedStateVars.empty() ? 0 : 1);
	for (VariableDeclaration const* var: transientStateVars)
		pusher.pushDefaultValue(var->type());

	if (pusher.ctx().storageLayout().tooMuchStateVariables()) {
		for (int i = 0; i < TvmConst::C7::FirstIndexForVariables; ++i)
			pusher.getGlob(i);
		pusher.blockSwap(totalVarQty, TvmConst::C7::FirstIndexForVariables);
		pusher.makeTuple(totalVarQty + TvmConst::C7::FirstIndexForVariables);
		pusher.popC7();
	} else {
		for (VariableDeclaration const* var: transientStateVars | std::views::reverse)
			pusher.setGlob(var);
		if (!unpackedStateVars.empty())
			pusher.setGlob(ctx.storageLayout().getUnpackIndex());
		for (VariableDeclaration const* var: stateVars | std::views::reverse)
			pusher.setGlob(var);
	}


	if (ctx.storageLayout().hasConstructor())
		pusher.setGlob(TvmConst::C7::ConstructorFlag);
	if (pusher.ctx().storageLayout().storeTimestampInC4())
		pusher.setGlob(TvmConst::C7::ReplayProtTime);
	if (pusher.ctx().storageLayout().storePubkeyInC4())
		pusher.setGlob(TvmConst::C7::TvmPubkey);

	solAssert(startStackSize == pusher.stackSize(), "");

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "c4_to_c7", std::nullopt, block, nullptr, true);
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateC7ToC4(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	std::vector<Type const*> const usualStateVarTypes = getTypesFromVarDecls(ctx.storageLayout().usualStateVariables());
	bool hasUnpackedStateVars = !ctx.storageLayout().unpackedStateVariables().empty();
	int const stateVarQty = usualStateVarTypes.size() + (hasUnpackedStateVars ? 1 : 0);
	if (ctx.storageLayout().tooMuchStateVariables()) {
		int const saveStack = pusher.stackSize();
		pusher.pushC7();
		pusher << "FALSE";
		pusher.setIndexQ(stateVarQty + TvmConst::C7::FirstIndexForVariables);
		pusher.unpackFirst(stateVarQty + TvmConst::C7::FirstIndexForVariables);
		pusher.reverse(stateVarQty + TvmConst::C7::FirstIndexForVariables, 0);
		pusher.drop(TvmConst::C7::FirstIndexForVariables);
		solAssert(saveStack + stateVarQty == pusher.stackSize(), "");
	} else {
		for (int i = stateVarQty - 1; i >= 0; --i)
			pusher.getGlob(TvmConst::C7::FirstIndexForVariables + i);
	}
	if (ctx.storageLayout().storeTimestampInC4())
		pusher.getGlob(TvmConst::C7::ReplayProtTime);
	if (pusher.ctx().storageLayout().storePubkeyInC4())
		pusher.getGlob(TvmConst::C7::TvmPubkey);
	pusher << "NEWC";
	if (pusher.ctx().storageLayout().storePubkeyInC4())
		pusher << "STU 256";
	if (ctx.storageLayout().storeTimestampInC4())
		pusher << "STU 64";
	if (ctx.storageLayout().hasConstructor())
		pusher << "STSLICECONST 1"; // constructor flag
	ChainDataEncoder encoder{&pusher};
	AbiV2Position position{ctx.storageLayout().getOffsetC4(), 0, usualStateVarTypes};
	encoder.encodeParameters(usualStateVarTypes, position, hasUnpackedStateVars);

	pusher << "ENDC";
	pusher.popRoot();
	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "c7_to_c4", std::nullopt, block, nullptr, true);
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateDefaultC4(TVMCompilerContext& ctx) {
	StackPusher pusher{&ctx};
	std::vector<Type const*> stateVarTypes = ctx.storageLayout().getC4Types();
	for (Type const* type: stateVarTypes | std::views::reverse)
		pusher.pushDefaultValue(type);
	pusher << "NEWC";
	if (!stateVarTypes.empty()) {
		ChainDataEncoder encoder{&pusher};
		AbiV2Position position{0, 0, stateVarTypes};
		encoder.encodeParameters(stateVarTypes, position, false);
	}
	pusher << "ENDC";

	Pointer<CodeBlock> block = pusher.getBlock();
	auto f = createNode<Function>(0, 0, "default_data_cell", std::nullopt, block, nullptr, false);
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateBuildTuple(
	TVMCompilerContext& ctx,
	std::string const& name,
	std::vector<Type const*> const& types
) {
	StackPusher pusher{&ctx};
	int n = types.size();
	std::vector<std::string> names(n);
	for (Type const* t: types) {
		pusher.pushDefaultValue(t);
	}
	pusher.makeTuple(n);
	StructCompiler sc{&pusher, types, names};
	sc.tupleToBuilder();
	pusher << "ENDC";
	return createNode<Function>(0, 0, name, std::nullopt, pusher.getBlock(), nullptr, false);
}

Pointer<Function>
TVMFunctionCompiler::generateNewArrays(TVMCompilerContext& ctx, std::string const& name, FunctionCall const* arr) {
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	FunctionCallCompiler{pusher, *arr, true}.honestArrayCreation(true);
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, std::nullopt, pusher.getBlock(), nullptr, false);
}

Pointer<Function>
TVMFunctionCompiler::generateConstArrays(TVMCompilerContext& ctx, std::string const& name, TupleExpression const* arr) {
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	solAssert(arr->isInlineArray(), "");
	TVMExpressionCompiler{pusher}.visitHonest(*arr, true);
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, std::nullopt, pusher.getBlock(), nullptr, false);
}

Pointer<Function> TVMFunctionCompiler::generateFunction(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	std::string const& name,
	uint32_t id
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
	return createNode<Function>(take, ret, name, id, pusher.getBlock(), function, false);
}

Pointer<Function>
TVMFunctionCompiler::generateOnCodeUpgrade(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	auto const [name, id] = ctx.functionInternalName(function, false);
	ctx.setCurrentFunction(function, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	funCompiler.visitFunctionWithModifiers();

	pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	pusher << "COMMIT";
	pusher._throw("THROW 0");
	int take = function->parameters().size();
	ctx.resetCurrentFunction();

	ctx.callGraph().addDictFunction(id, name);
	return createNode<Function>(take, 0, name, id, pusher.getBlock(), function, true);
}

Pointer<Function> TVMFunctionCompiler::generateOnTickTock(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	ctx.setCurrentFunction(function, "onTickTock");
	StackPusher pusher{&ctx};

	solAssert(function->parameters().size() == 1, "");
	ASTPointer<VariableDeclaration> const& variable = function->parameters().at(0);
	pusher.pushS(0);
	pusher.getStack().add(variable.get(), false);

	bool isPure = function->stateMutability() == StateMutability::Pure;
	if (!isPure) {
		pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	}

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	funCompiler.setCopyleft();
	funCompiler.visitFunctionWithModifiers();


	if (!isPure) {
		pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	}
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, "onTickTock", std::nullopt, pusher.getBlock(), nullptr, false);
}

void TVMFunctionCompiler::decodeFunctionParamsAndInitVars(bool isResponsible) const {
	// decode function params
	// stack: arguments-in-slice
	std::vector<Type const*> types = getParams(m_function->parameters()).first;
	ChainDataDecoder{&m_pusher}.decodeFunctionParameters(types, isResponsible, m_function->isExternalMsg());
	// stack: transaction_id arguments...
	m_pusher.getStack().change(-static_cast<int>(m_function->parameters().size()));
	for (ASTPointer<VariableDeclaration> const& variable: m_function->parameters()) {
		auto name = variable->name();
		m_pusher.getStack().add(variable.get(), true);
	}
}

Pointer<Function>
TVMFunctionCompiler::generatePublicFunction(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	std::string name = function->name();
	ctx.setCurrentFunction(function, name);

	StackPusher pusher{&ctx};

	TVMFunctionCompiler funCompiler{pusher, 0, function, false, false, 0};
	pusher.fixStack(+1); // slice with args
	if (ctx.storageLayout().hasConstructor())
		pusher.checkCtorCalled();
	if (!function->isExternalMsg() && function->stateMutability() != StateMutability::Pure) {
		pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	}

	funCompiler.pushLocation(*function);
	bool const isResponsible = function->isResponsible();
	if (isResponsible) {
		int const saveStackSize = pusher.stackSize();
		pusher << "LDU 32";		// callbackId slice
		pusher.blockSwap(1, 1); // slice callbackId
		pusher.setGlob(TvmConst::C7::ResponsibleCallbackFunctionId);
		solAssert(saveStackSize == pusher.stackSize(), "");
	}
	funCompiler.decodeFunctionParamsAndInitVars(isResponsible);
	funCompiler.pushLocation(*function, true);

	int paramQty = function->parameters().size();
	int retQty = function->returnParameters().size();
	// stack: arg0, arg1, arg2 ...
	pusher.pushFragmentInCallRef(paramQty, retQty, pusher.ctx().functionInternalName(function, false).first);

	solAssert(pusher.stackSize() == retQty, "");
	// emit
	funCompiler.emitOnPublicFunctionReturn();

	pusher.ensureSize(0, "");

	funCompiler.updC4IfItNeeds();
	// set flag meaning function is called

	Pointer<CodeBlock> block = pusher.getBlock();
	ctx.resetCurrentFunction();
	// sliceWithBody
	// returns nothing
	return createNode<Function>(1, 0, name, std::nullopt, block, nullptr, false);
}

Pointer<Function>
TVMFunctionCompiler::generateGetterFunction(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	// stack: function params
	std::string name = function->name();
	ctx.setCurrentFunction(function, name);

	StackPusher pusher{&ctx};

	pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	int paramQty = function->parameters().size();
	int retQty = function->returnParameters().size();
	std::string fragmentName = pusher.ctx().functionInternalName(function, false).first;
	pusher.fixStack(paramQty);
	pusher.pushFragmentInCallRef(paramQty, retQty, fragmentName);

	Pointer<CodeBlock> block = pusher.getBlock();
	ctx.resetCurrentFunction();

	return createNode<Function>(paramQty, retQty, name, std::nullopt, block, nullptr, false);
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

void TVMFunctionCompiler::generatePublicFunctionSelector(bool const isExternal) const {
	auto const functions = isExternal ? m_pusher.ctx().getExtPublicFunctions() : m_pusher.ctx().getIntPublicFunctions();

	if (functions.empty()) {
		m_pusher.fixStack(+1); // fix stack
		m_pusher.drop();
	} else {
		std::vector<std::string> lines;
		lines.emplace_back("DICTPUSHCONST 32, {");
		for (auto const& [id, name]: functions) {
			auto binStr = StrUtils::toBitString(id, 32, false).value();
			auto const slice = StrUtils::binaryStringToSlice(binStr);
			lines.emplace_back("\tx" + slice + " = " + name + ",");
			solAssert(!m_pusher.ctx().callGraph().tryToAddEdge(m_pusher.ctx().currentFunctionName(), name), "");
		}
		lines.emplace_back("}");
		lines.emplace_back("DICTUGETJMP");
		// function selector, slice and function id and return slice
		m_pusher.push(createNode<HardCode>(lines, 100500, 100500 - 1, false));
	}

	// TVMFunctionCompiler compiler{pusher, contract};
	// PublicFunctionSelector pfs{int(functions.size())};
	// compiler.buildPublicFunctionSelector(functions, 0, functions.size(), pfs);
	// return createNode<Function>(1, 1, name, std::nullopt m_pusher.getBlock());
}

Pointer<Function> TVMFunctionCompiler::generateLibFunctionWithObject(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	std::string const& name
) {
	ctx.setCurrentFunction(function, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, true, true, 0};
	funCompiler.visitFunctionWithModifiers();
	int take = function->parameters().size();
	int ret = function->returnParameters().size();
	ctx.resetCurrentFunction();
	return createNode<Function>(take, ret + 1, name, std::nullopt, pusher.getBlock(), nullptr, true);
}

Pointer<Function> TVMFunctionCompiler::generateReceive(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	std::string const name = "receive";
	ctx.setCurrentFunction(function, name);
	auto f = generateReceiveOrFallbackOrOnBouncedMessage(ctx, function, name, 0);
	ctx.resetCurrentFunction();
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateFallback(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	std::string const name = "fallback";
	ctx.setCurrentFunction(function, name);
	auto f = generateReceiveOrFallbackOrOnBouncedMessage(ctx, function, name, 0);
	ctx.resetCurrentFunction();
	return f;
}

Pointer<Function>
TVMFunctionCompiler::generateOnBouncedMessage(TVMCompilerContext& ctx, FunctionDefinition const* function) {
	ctx.setCurrentFunction(function, TvmConst::ON_BOUNCED_MESSAGE);
	Pointer<Function> f = generateReceiveOrFallbackOrOnBouncedMessage(ctx, function, TvmConst::ON_BOUNCED_MESSAGE, 1);
	ctx.resetCurrentFunction();
	return f;
}

Pointer<Function> TVMFunctionCompiler::generateReceiveOrFallbackOrOnBouncedMessage(
	TVMCompilerContext& ctx,
	FunctionDefinition const* function,
	std::string const& name,
	int take
) {
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, 0, function, false, true, 0};
	if (ctx.storageLayout().hasConstructor())
		pusher.checkCtorCalled();
	if (!function->isExternalMsg() && function->stateMutability() != StateMutability::Pure) {
		pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	}
	funCompiler.visitFunctionWithModifiers();
	funCompiler.updC4IfItNeeds();
	return createNode<Function>(take, 0, name, std::nullopt, pusher.getBlock(), nullptr, false);
}

// pop params.size() elements from stack top
void TVMFunctionCompiler::emitOnPublicFunctionReturn() const {
	int const stackSize = m_pusher.stackSize();

	std::vector<ASTPointer<VariableDeclaration>> const& params = m_function->returnParameters();
	std::vector<VariableDeclaration const*> ret = convertArray(params);

	if (params.empty() || (!m_function->isExternalMsg() && !m_function->isResponsible())) {
		m_pusher.fixStack(-static_cast<int>(params.size()));
		return;
	}

	m_pusher.startOpaque();

	// emit for ext
	if (m_function->isExternalMsg()) {
		//	ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt
		//	import_fee:Grams = CommonMsgInfo;

		// get external address of sender
		m_pusher.pushS(m_pusher.stackSize() + 1);
		m_pusher << "CTOS";
		m_pusher << "LDU 2";
		m_pusher << "LDMSGADDR";
		m_pusher.drop();
		m_pusher.popS(1);

		auto appendBodyForExtMsg = [&](int bitSizeBuilder, int refSizeBuilder) {
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				ret,
				ChainDataEncoder::
					calculateFunctionIDWithReason(m_function, ReasonOfOutboundMessage::FunctionReturnExternal),
				{},
				bitSizeBuilder,
				refSizeBuilder
			);
		};

		m_pusher.sendMessage(
			{TvmConst::ext_msg_info::dest},
			{},
			appendBodyForExtMsg,
			nullptr,
			nullptr,
			StackPusher::MsgType::ExternalOut,
			false,
			nullptr,
			nullptr
		);
		m_pusher.fixStack(params.size()); // fix stack
	} else {
		// Call back function

		auto pushFunctionId = [&] { m_pusher.getGlob(TvmConst::C7::ResponsibleCallbackFunctionId); };
		auto appendBody = [&](int bitSizeBuilder, int refSizeBuilder) {
			ChainDataEncoder{&m_pusher}
				.createMsgBodyAndAppendToBuilder(ret, pushFunctionId, {}, bitSizeBuilder, refSizeBuilder);
		};
		std::function<void()> pushSendMessageFlag = [&] { m_pusher.getGlob(TvmConst::C7::ResponsibleMessageFlag); };

		m_pusher.getGlob(TvmConst::C7::ResponsibleParams);
		m_pusher.untuple(4);
		// stack: currency value dest bounce

		m_pusher.sendMessage(
			{TvmConst::int_msg_info::bounce,
			 TvmConst::int_msg_info::dest,
			 TvmConst::int_msg_info::tons,
			 TvmConst::int_msg_info::currency},
			{},
			appendBody,
			nullptr,
			pushSendMessageFlag,
			StackPusher::MsgType::Internal,
			false,
			nullptr,
			nullptr
		);
	}

	m_pusher.endOpaque(ret.size(), 0);

	m_pusher.ensureSize(stackSize - static_cast<int>(params.size()));
}

void TVMFunctionCompiler::visitModifierOrFunctionBlock(Block const& body, int argQty, int nameRetQty) {
	m_pusher.startContinuation();
	acceptBody(body, {{argQty, nameRetQty}});
	pushLocation(*m_function);
	m_pusher.pushContAndCallX();
	pushLocation(*m_function, true);
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
 * rest stack of modifer2 [drop stack modifier2]
 * rest stack of modifer1 [drop stack modifier1]
 * rest stack of modifer0 [drop stack modifier0]
 * [leave only return params]
 */

/* stack:
 * function params
 * ...
 * stack of function
 * ...
 * [leave only return params]
 */
void TVMFunctionCompiler::visitFunctionWithModifiers() const {
	int const argQty = m_function->parameters().size();
	int const retQty = m_function->returnParameters().size();
	int const nameRetQty = retQty;

	// inits function params and return named params
	if (m_currentModifier == 0) {
		if (m_pushArgs) {
			solAssert(m_startStackSize == 0, "");
			m_pusher.pushParameter(m_function->parameters());
		} else {
			solAssert(m_startStackSize >= 0, "");
		}

		pushDefaultParameters(m_function->returnParameters());
	}


	if (m_currentModifier == static_cast<int>(functionModifiers().size())) {
		int modSize = m_pusher.stackSize() - argQty - nameRetQty;
		m_pusher.blockSwap(argQty + nameRetQty, modSize); // break stack

		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.fixStack(-modSize); // fix stack

		TVMFunctionCompiler funCompiler{pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, 0};
		funCompiler.visitModifierOrFunctionBlock(m_function->body(), argQty, nameRetQty);
		m_pusher.add(pusher);

		m_pusher.blockSwap(modSize, retQty); // break stack
	} else {
		int ss = m_pusher.stackSize();
		ModifierInvocation const* invocation = functionModifiers()[m_currentModifier].get();
		auto modifierDefinition = to<ModifierDefinition>(invocation->name().annotation().referencedDeclaration);
		ast_vec<Expression> const* args = invocation->arguments();
		int modParamQty{};
		if (args != nullptr) {
			modParamQty = args->size();
			for (int i = 0; i < modParamQty; ++i) {
				ASTPointer<Expression> const& arg = (*args)[i];
				TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
				m_pusher.getStack().add(modifierDefinition->parameters()[i].get(), false);
			}
		}
		TVMFunctionCompiler funCompiler{m_pusher, m_currentModifier, m_function, m_isLibraryWithObj, m_pushArgs, ss};
		funCompiler.visitModifierOrFunctionBlock(modifierDefinition->body(), modParamQty, 0);
		solAssert(ss == m_pusher.stackSize(), "");
	}
}

void TVMFunctionCompiler::pushDefaultParameters(ast_vec<VariableDeclaration> const& returnParameters) const {
	for (ASTPointer<VariableDeclaration> const& returnParam: returnParameters) {
		m_pusher.pushDefaultValue(returnParam->type());
		m_pusher.getStack().add(returnParam.get(), false);
	}
}

void TVMFunctionCompiler::acceptExpr(Expression const* expr, bool const isResultNeeded) const {
	solAssert(expr, "");
	TVMExpressionCompiler(m_pusher).acceptExpr(expr, isResultNeeded);
}

bool TVMFunctionCompiler::visit(VariableDeclarationStatement const& _variableDeclarationStatement) {
	int const saveStackSize = m_pusher.stackSize();
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
		if (te && !te->isInlineArray() && varQty == static_cast<int>(te->components().size())) {
			ast_vec<Expression> const& tuple = te->components();
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
		for (auto const& decl: variables) {
			m_pusher.pushDefaultValue(decl->type());
		}
	}

	m_pusher.getStack().change(-varQty + bad);
	for (ASTPointer<VariableDeclaration> const& d: variables) {
		if (d != nullptr) {
			m_pusher.getStack().add(d.get(), true);
		}
	}
	m_pusher.ensureSize(saveStackSize + varQty - bad, "VariableDeclarationStatement", &_variableDeclarationStatement);
	return false;
}

void TVMFunctionCompiler::acceptBody(Block const& _block, std::optional<std::tuple<int, int>> functionBlock) {
	int const startStackSize = m_pusher.stackSize();

	for (ASTPointer<Statement> const& s: _block.statements()) {
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
			m_pusher.fixStack(-funTrash - argQty);
		}
	} else {
		int const delta = m_pusher.stackSize() - startStackSize;
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

		SourceReference sr =
			SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &_statement.location());
		auto position = sr.sourceName + " " + toString(sr.position.line + 1) + ":" + toString(sr.position.column + 1);
		m_pusher.ensureSize(savedStackSize, position);
		pushLocation(_statement, true);
	}
	return false;
}

bool TVMFunctionCompiler::visit(TryStatement const& _tryState) {
	// return flag
	int const stackSize = m_pusher.stackSize();
	CFAnalyzer ci{_tryState};
	ControlFlowInfo info = beforeTryOrIfCheck(ci);
	m_controlFlowInfo.push_back(info);

	// try body
	m_pusher.startContinuation();
	int const startStackSize = m_pusher.stackSize();
	_tryState.body().accept(*this);
	m_pusher.drop(m_pusher.stackSize() - startStackSize);
	m_pusher.endContinuation();

	// try body
	m_pusher.startContinuation();
	if (_tryState.clause().parameters()) {
		for (ASTPointer<VariableDeclaration> const& variable: _tryState.clause().parameters()->parameters()) {
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

bool TVMFunctionCompiler::visit(IfStatement const& _ifStatement) {
	int const saveStackSize = m_pusher.stackSize();

	// header
	CFAnalyzer ci(_ifStatement);
	bool canUseJmp = _ifStatement.falseStatement() != nullptr
						 ? CFAnalyzer(_ifStatement.trueStatement()).doThatAlways() &&
							   CFAnalyzer(*_ifStatement.falseStatement()).doThatAlways()
						 : CFAnalyzer(_ifStatement.trueStatement()).doThatAlways();
	ControlFlowInfo info = canUseJmp ? ControlFlowInfo{m_pusher.stackSize(), false, false} : beforeTryOrIfCheck(ci);
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

void TVMFunctionCompiler::doWhile(WhileStatement const& _whileStatement) {
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
		m_pusher.pushContAndCallX();
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

void TVMFunctionCompiler::visitForOrWhileCondition(std::function<void()> const& pushCondition) const {
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

void TVMFunctionCompiler::afterLoopCheck(
	std::unique_ptr<CFAnalyzer> const& ci,
	int const& loopVarQty,
	bool _doAnalyzeFlag
) const {
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

ControlFlowInfo TVMFunctionCompiler::beforeTryOrIfCheck(CFAnalyzer const& ci) const {
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

bool TVMFunctionCompiler::visit(WhileStatement const& _whileStatement) {
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
		std::function<void()> pushCondition = [&] { acceptExpr(&_whileStatement.condition(), true); };
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
	// [return flag] - optional. If we have return/break/continue.

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
	// private key (not visible in solidity code)
	// value
	// [return flag] - optional. If have return/break/continue.

	int const saveStackSize = m_pusher.stackSize();
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
			m_pusher.pushInt(0);	  // stack: dict 0
			m_pusher.pushNull();	  // stack: dict 0 value
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
	std::function<void()> pushCondition = [&] {
		if (arrayType) {
			if (arrayType->isByteArrayOrString()) {
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1); // stack: cell value [flag] cell
				m_pusher << "SEMPTY";
				m_pusher << "NOT";
			} else {
				// stack: dict index value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict index value [flag] index
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1); // stack: dict index value [flag] index dict
				m_pusher.getDict(getArrayKeyType(), *arrayType->baseType(), GetDictOperation::Fetch);
				// stack: dict index value [flag] newValue
				m_pusher.pushS(0); // stack: dict index value [flag] newValue newValue
				m_pusher.popS(m_pusher.stackSize() - saveStackSize - 3); // stack: dict index newValue [flag] newValue
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
	std::function<void()> pushStartBody = [&] {
		if (arrayType) {
			if (arrayType->isByteArrayOrString()) {
				int const ss = m_pusher.stackSize();
				// stack: cell value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1);
				// stack: cell value [flag] cell

				m_pusher.startOpaque();
				m_pusher.pushAsym("LDUQ 8");
				m_pusher.fixStack(+1); // fix stack
				m_pusher.startContinuation();
				// stack: cell value [flag] slice
				m_pusher << "PLDREFIDX 0";
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
			} else if (optValueAsTuple(arrayType->baseType()))
				m_pusher.untuple(1);
		}
	};
	std::function<void()> pushLoopExpression = [&] {
		if (arrayType) {
			if (arrayType->isByteArrayOrString()) {
				// do nothing
			} else {
				// stack: dict 0 value [flag]
				m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2); // stack: dict index value [flag] index
				m_pusher << "INC";										  // stack: dict index value [flag] newIndex
				m_pusher.popS(m_pusher.stackSize() - saveStackSize - 2);  // stack: dict newIndex [flag] value
			}
		} else if (mappingType) {
			int const sss = m_pusher.stackSize();
			// stack: dict minKey(private) minKey(pub) value [flag]
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 2);
			// stack: dict minKey(private) minKey(pub) value [flag] minKey
			m_pusher.pushS(m_pusher.stackSize() - saveStackSize - 1);
			// stack: dict minKey(private) minKey(pub) value [flag] minKey dict
			m_pusher.pushInt(dictKeyLength(mappingType->keyType()));
			// stack: dict minKey(private) minKey(pub) value [flag] minKey dict nbits

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

std::pair<std::unique_ptr<CFAnalyzer>, ControlFlowInfo>
TVMFunctionCompiler::pushControlFlowFlag(Statement const& body) {
	std::unique_ptr<CFAnalyzer> ci = std::make_unique<CFAnalyzer>(body);
	bool isLoop = true;
	bool hasAnalyzeFlag = false;
	if (ci->canReturn()) {
		m_pusher.declRetFlag();
		hasAnalyzeFlag = true;
	}
	int stackSize = m_pusher.stackSize();
	ControlFlowInfo info{stackSize, hasAnalyzeFlag, isLoop};
	m_controlFlowInfo.push_back(info);
	return {std::move(ci), info};
}

void TVMFunctionCompiler::visitBodyOfForLoop(
	std::unique_ptr<CFAnalyzer> const& ci,
	std::function<void()> const& pushStartBody,
	Statement const& body,
	std::function<void()> const& loopExpression
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
		m_pusher.pushContAndCallX();
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

bool TVMFunctionCompiler::visit(ForStatement const& _forStatement) {
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
		int const saveStack = m_pusher.stackSize();
		_forStatement.initializationExpression()->accept(*this);
		haveDeclLoopVar = m_pusher.stackSize() != saveStack;
	}

	// header
	auto [ci, info] = pushControlFlowFlag(_forStatement.body());

	// condition
	std::function<void()> pushCondition;
	if (_forStatement.condition()) {
		pushCondition = [&] { acceptExpr(_forStatement.condition(), true); };
	}
	visitForOrWhileCondition(pushCondition);

	// body and loopExpression
	std::function<void()> pushLoopExpression;
	if (_forStatement.loopExpression() != nullptr) {
		pushLoopExpression = [&] { _forStatement.loopExpression()->accept(*this); };
	}
	visitBodyOfForLoop(ci, {}, _forStatement.body(), pushLoopExpression);

	// bottom
	afterLoopCheck(ci, haveDeclLoopVar, info.hasAnalyzeFlag());
	m_pusher.ensureSize(saveStackSize, "for");

	return false;
}

bool TVMFunctionCompiler::visit(Return const& _return) {
	if (!_return.names().empty()) {
		std::map<std::string, std::function<void()>> pushOption = {
			{"bounce", nullptr},
			{"dest", [&] { m_pusher.getGlob(TvmConst::C7::SenderAddress); }},
			{"value", nullptr},
			{"currencies", [&] { m_pusher << "NULL"; }},
		};
		for (std::size_t i = 0; i < _return.names().size(); ++i) {
			std::string const& optionName = *_return.names().at(i);
			if (pushOption.contains(optionName)) {
				pushOption[optionName] = [i, this, &_return] { acceptExpr(_return.options().at(i).get()); };
			} else if (optionName == "flag") {
				acceptExpr(_return.options().at(i).get());
				m_pusher.setGlob(TvmConst::C7::ResponsibleMessageFlag);
			} else {
				solUnimplemented("");
			}
		}
		auto parames = {"bounce", "dest", "value", "currencies"};
		for (auto const& optionName: parames | std::views::reverse) {
			solAssert(pushOption.contains(optionName), "No such option");
			pushOption.at(optionName)();
		}
		m_pusher.makeTuple(4);
		m_pusher.setGlob(TvmConst::C7::ResponsibleParams);
	}

	auto expr = _return.expression();
	if (expr) {
		acceptExpr(expr);

		int retQty = m_function->returnParameters().size();
		std::vector<Type const*> givenTypes;
		if (auto tuple = to<TupleType>(expr->annotation().type)) {
			solAssert(retQty == static_cast<int>(tuple->components().size()), "");
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
		ast_vec<VariableDeclaration> const& params = _return.annotation().functionReturnParameters->parameters();
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
	} else {							// all continuations are run by JMPX
		m_pusher.fixStack(revertDelta); // fix stack
	}

	if (hasLoop()) {
		m_pusher.retAlt();
	} else {
		m_pusher.ret();
	}
	m_pusher.endRetOrBreakOrCont(ReturnOrBreakOrCont::Type::Return, retCount);

	return false;
}

bool TVMFunctionCompiler::visit(Break const&) {
	int const sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize();
	m_pusher.startContinuation();

	m_pusher.drop(sizeDelta);
	m_pusher.retAlt();

	m_pusher.fixStack(sizeDelta); // fix stack
	m_pusher.endRetOrBreakOrCont(ReturnOrBreakOrCont::Type::Break, 0);

	return false;
}

bool TVMFunctionCompiler::visit(Continue const&) {
	bool hasAnalyzer = lastAnalyzerBeforeLoop();
	int const sizeDelta = m_pusher.stackSize() - lastLoop().value().stackSize();
	m_pusher.startContinuation();

	if (hasAnalyzer) {
		m_pusher.drop(sizeDelta + (lastLoop().value().hasAnalyzeFlag() ? 1 : 0));
		m_pusher.pushInt(TvmConst::CONTINUE_FLAG);
	} else {
		m_pusher.drop(sizeDelta);
	}

	m_pusher.ret();

	m_pusher.fixStack(sizeDelta); // fix stack
	m_pusher.endRetOrBreakOrCont(ReturnOrBreakOrCont::Type::Continue, 0);
	return false;
}

bool TVMFunctionCompiler::visit(EmitStatement const& _emit) {
	auto eventCall = to<FunctionCall>(&_emit.eventCall());
	solAssert(eventCall, "");
	CallableDeclaration const* def = getFunctionDeclarationOrConstructor(&eventCall->expression());
	solAssert(def, "Event Declaration was not found");
	auto eventDef = to<EventDefinition>(def);

	FunctionCallCompiler functionCallCompiler{m_pusher, *eventCall, true};
	functionCallCompiler.pushArgs(true);

	std::string name = eventName(eventDef);

	auto appendBody = [&](int bitSizeBuilder, int refSizeBuilder) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			convertArray(eventDef->parameters()),
			ChainDataEncoder::calculateFunctionIDWithReason(
				name,
				getTypesFromVarDecls(eventDef->parameters()),
				nullptr,
				ReasonOfOutboundMessage::EmitEventExternal,
				std::nullopt,
				false
			),
			{},
			bitSizeBuilder,
			refSizeBuilder,
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

	m_pusher.sendMessage(
		isParamOnStack,
		{},
		appendBody,
		nullptr,
		nullptr,
		StackPusher::MsgType::ExternalOut,
		false,
		nullptr,
		nullptr
	);
	return false;
}

void TVMFunctionCompiler::setCtorFlag() const {
	m_pusher.pushRoot();
	m_pusher << "CTOS";
	int delta = 0;
	if (m_pusher.ctx().storageLayout().storePubkeyInC4())
		delta += 256;
	if (m_pusher.ctx().storageLayout().storeTimestampInC4())
		delta += 64;
	if (delta > 0) {
		m_pusher.pushInt(delta);
		m_pusher << "SDSKIPFIRST";
	}
	m_pusher << "PLDI 1";
	m_pusher.setGlob(TvmConst::C7::ConstructorFlag);
}

void TVMFunctionCompiler::setCopyleft() const {
	std::optional<std::vector<ASTPointer<Expression>>> const copyleft = m_pusher.ctx().pragmaHelper().hasCopyleft();
	if (copyleft.has_value()) {
		std::optional<bigint> const& addr = ExprUtils::constValue(*copyleft.value().at(1));
		std::optional<bigint> const& type = ExprUtils::constValue(*copyleft.value().at(0));
		std::string const addrSlice =
			"x" + StrUtils::binaryStringToSlice(StrUtils::toBitString(addr.value(), 256, false).value());
		m_pusher.pushSlice(addrSlice);
		m_pusher.pushInt(type.value());
		m_pusher << "COPYLEFT";
	}
}

Pointer<Function>
TVMFunctionCompiler::generateMainExternal(TVMCompilerContext& ctx, ContractDefinition const* contract) {
	//	stack:
	//	contract_balance
	//	msg_balance is always zero
	//	msg_cell
	//	msg_body_slice
	//	transaction_id = -1

	std::string name = "main_external";
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, contract};

	funCompiler.setCopyleft();
	pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	pusher.pushS(0);

	funCompiler.checkSignatureAndReadPublicKey();
	auto const replayProt = pusher.ctx().getContract()->replayProtection();
	solAssert(replayProt, "");
	switch (replayProt->type()) {
	case ReplayProtection::ReplayProtectionType::TimeReplayProt:
		pusher.pushFragment(1, 1, "__timeReplayProtection");
		if (pusher.ctx().getContract()->externalMsgHeaders()->hasExpire())
			pusher.pushFragment(1, 1, "__checkExpire");
		break;
	case ReplayProtection::ReplayProtectionType::SeqnoReplayProt:
		pusher.pushFragment(1, 1, "__seqnoReplayProtection");
		if (pusher.ctx().getContract()->externalMsgHeaders()->hasExpire())
			pusher.pushFragment(1, 1, "__checkExpire");
		break;
	case ReplayProtection::ReplayProtectionType::CustomReplayProt:
		solAssert(pusher.ctx().getContract()->afterSignatureCheck(), "");
		// ... msg_cell msg_body_slice rest_msg_body_slice
		pusher.pushS(2);
		auto const funcName =
			pusher.ctx().functionInternalName(pusher.ctx().getContract()->afterSignatureCheck(), false).first;
		pusher.pushInlineFunction(funcName, 2, 1);
		break;
	}

	// msg_body
	pusher << "LDU 32 ; funcId body";
	pusher.exchange(1);

	// body' funcId
	funCompiler.generatePublicFunctionSelector(true);
	// body'
	pusher._throw("THROW " + toString(TvmConst::RuntimeException::NoFallback));
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, -1, pusher.getBlock(), nullptr, false);
}

void TVMFunctionCompiler::pushMsgPubkey() const {
	// signatureSlice msgSlice hashMsgSlice

	if (m_pusher.ctx().getContract()->externalMsgHeaders()->hasPubkey()) {
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

void TVMFunctionCompiler::checkSignatureAndReadPublicKey() const {
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

	if (m_pusher.ctx().getContract()->externalMsgHeaders()->hasPubkey()) {
		// External inbound message does not have signature but have public key
		m_pusher.startContinuation();
		m_pusher << "LDU 1      ; hasPubkey msgSlice";
		m_pusher.exchange(1);
		m_pusher._throw(
			"THROWIF " + toString(TvmConst::RuntimeException::MessageHasNoSignButHasPubkey) + " ; msgSlice"
		);
		m_pusher.endContinuation();
		m_pusher.ifElse();
	} else {
		m_pusher._if();
	}
}

Pointer<Function>
TVMFunctionCompiler::generateMainInternal(TVMCompilerContext& ctx, ContractDefinition const* contract) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//                 src:MsgAddress  dest:MsgAddressInt(#4)
	//                 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//                 created_lt:uint64  created_at:uint32
	//                 = CommonMsgInfoRelaxed;

	std::string name = "main_internal";
	ctx.setCurrentFunction(nullptr, name);
	StackPusher pusher{&ctx};
	TVMFunctionCompiler funCompiler{pusher, contract};

	funCompiler.setCopyleft();
	if (ctx.storageLayout().hasConstructor())
		funCompiler.setCtorFlag();

	pusher.pushS(1);
	pusher << "CTOS";
	// stack: int_msg_info

	ContactsUsageScanner const& sc = pusher.ctx().usage();
	if (sc.hasMsgSender() || sc.hasResponsibleFunction()) {
		pusher << "LDU 4       ; bounced tail";
		pusher << "LDMSGADDR   ; bounced src tail";
		pusher.drop();
		pusher.setGlob(TvmConst::C7::SenderAddress);
		pusher << "MODPOW2 1";
	} else {
		pusher << "PLDU 4";
		pusher << "MODPOW2 1";
	}
	// stack: isBounced

	// bounced
	if (!isEmptyFunction(contract->onBounceFunction())) {
		pusher.startContinuation();
		pusher.pushS(0);
		pusher.pushFragment(0, 0, TvmConst::ON_BOUNCED_MESSAGE);
		pusher.endContinuationFromRef();
		pusher.ifJmp();
	} else {
		pusher.ifret();
	}

	funCompiler.pushReceiveOrFallbackAndLoadFuncId();
	// funcId body'
	pusher.exchange(1);
	// body' funcId
	funCompiler.generatePublicFunctionSelector(false);
	// body'
	auto const fallback = pusher.ctx().fallBack();

	if (fallback != nullptr) {
		pusher.drop(); // drop body'
		pusher.pushFragmentInCallRef(100500, 100500, "fallback");
	} else {
		pusher._throw("THROW " + toString(TvmConst::RuntimeException::NoFallback));
	}
	ctx.resetCurrentFunction();
	return createNode<Function>(0, 0, name, std::nullopt, pusher.getBlock(), nullptr, false);
}

bool TVMFunctionCompiler::visit(PlaceholderStatement const&) {
	TVMFunctionCompiler
		funCompiler{m_pusher, m_currentModifier + 1, m_function, m_isLibraryWithObj, m_pushArgs, m_pusher.stackSize()};
	funCompiler.visitFunctionWithModifiers();
	return false;
}

void TVMFunctionCompiler::updC4IfItNeeds() const {
	// c7_to_c4 if it is necessary
	//	solAssert(m_pusher.stackSize() == 0, "");
	if (m_function->stateMutability() == StateMutability::NonPayable) {
		m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
	} else if (m_function->isExternalMsg()) {
		// if it's external message, then we save values for replay protection

		bool const upd_only_time_in_c4 = m_pusher.ctx().getContract()->afterSignatureCheck() == nullptr &&
										 m_pusher.ctx().getContract()->externalMsgHeaders()->hasTime() &&
										 m_pusher.ctx().storageLayout().usualStateVariables().size() >=
											 2; // just optimization: if varQty == 1, then it's better to call c7_to_c4;

		if (upd_only_time_in_c4) {
			m_pusher.pushFragmentInCallRef(0, 0, "upd_only_time_in_c4");
		} else {
			m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
		}
	}
}

void TVMFunctionCompiler::pushReceiveOrFallbackAndLoadFuncId() const {
	// stack: ... body 0 (internal msg selector)
	bool const hasReceive = !isEmptyFunction(m_contract->receiveFunction());

	m_pusher.pushS(0); // body
	m_pusher << "SEMPTY     ; isEmptyBody";

	if (hasReceive) {
		m_pusher.startContinuation();
		m_pusher.pushFragment(100500, 100500, "receive");
		m_pusher.endContinuationFromRef();
		m_pusher.ifJmp();
	} else {
		if (m_pusher.ctx().storageLayout().hasConstructor())
			m_pusher.checkIfCtorCalled(true);
		else
			m_pusher.ifret();
	}

	m_pusher.pushS(0); // body
	m_pusher.startOpaque();
	m_pusher.pushAsym("LDUQ 32  ; [funcId] body' ok");
	if (m_contract->fallbackFunction()) {
		m_pusher.startContinuation();
		// body'
		m_pusher.drop();
		m_pusher.pushFragmentInCallRef(100500, 100500, "fallback");
		m_pusher.endContinuation();
		m_pusher.ifNotJmp();
	} else {
		m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::NoFallback) + " ; funcId body'");
	}
	m_pusher.endOpaque(1, 2);
	// funcId body'

	if (hasReceive) {
		m_pusher.pushS(1); // funcId body' funcId
		m_pusher << "EQINT 0 ; funcId body' isZero";
		m_pusher.startContinuation();
		m_pusher.drop(2);
		m_pusher.pushFragmentInCallRef(100500, 100500, "receive");
		m_pusher.endContinuation();
		m_pusher.ifJmp();
	} else {
		// funcId body'
		m_pusher.pushS(1); // funcId body' funcId
		if (m_pusher.ctx().storageLayout().hasConstructor())
			m_pusher.checkIfCtorCalled(false);
		else
			m_pusher.ifNotRet();
	}
}

void TVMFunctionCompiler::buildPublicFunctionSelector(
	std::vector<std::pair<uint32_t, std::string>> const& functions,
	int left,
	int right,
	PublicFunctionSelector const& pfs
) {
	auto pushOne = [&](uint32_t functionId, std::string const& name) {
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
	for (int const groupSize: sizes) {
		if (groupSize == 1) {
			auto const& [functionId, name] = functions.at(pos);
			pushOne(functionId, name);
		} else {
			auto const& [functionId, name] = functions.at(pos + groupSize - 1);
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

void TVMFunctionCompiler::pushLocation(ASTNode const& node, bool reset) const {
	// See SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &node.location());
	auto const& location = node.location();
	int line = 0;
	if (!reset) {
		CharStream const& charStream = GlobalParams::g_charStreamProvider->charStream(*location.sourceName);
		LineColumn const interest = charStream.translatePositionToLineColumn(location.start);
		line = interest.line + 1;
	}
	m_pusher.pushLoc(*location.sourceName, line);
}

TVMConstructorCompiler::TVMConstructorCompiler(StackPusher& pusher):
	TVMFunctionCompiler{pusher, pusher.ctx().getContract()},
	m_pusher{pusher} {}

void TVMConstructorCompiler::dfs(ContractDefinition const* c) {
	if (used[c]) {
		return;
	}
	used[c] = true;
	dfsOrder.push_back(c);
	path[c] = dfsOrder;
	for (ASTPointer<InheritanceSpecifier> const& inherSpec: c->baseContracts()) {
		auto base = to<ContractDefinition>(inherSpec->name().annotation().referencedDeclaration);
		ast_vec<Expression> const* agrs = inherSpec->arguments();
		if (agrs != nullptr && !agrs->empty()) {
			m_args[base] = inherSpec->arguments();
			dfs(base);
		}
	}
	if (c->constructor() != nullptr) {
		for (ASTPointer<ModifierInvocation> const& modInvoc: c->constructor()->modifiers()) {
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
	std::vector<ContractDefinition const*> linearizedBaseContracts =
		m_pusher.ctx().getContract()->annotation().linearizedBaseContracts; // from derived to base

	FunctionDefinition const* topConstructor = m_pusher.ctx().getContract()->constructor();
	m_pusher.ctx().setCurrentFunction(topConstructor, "constructor");
	bool isCalledByExtMsg = topConstructor != nullptr && topConstructor->isExternalMsg();
	{
		uint32_t functionId;
		if (topConstructor == nullptr)
			functionId = ChainDataEncoder::calculateConstructorFunctionID();
		else
			functionId = ChainDataEncoder::
				calculateFunctionIDWithReason(topConstructor, ReasonOfOutboundMessage::RemoteCallInternal);

		if (isCalledByExtMsg) {
			m_pusher.ctx().addExternalMsgPublicFunction(functionId, "constructor");
		} else {
			m_pusher.ctx().addInternalMsgPublicFunction(functionId, "constructor");
		}
	}

	if (!isCalledByExtMsg) {
		m_pusher.pushFragmentInCallRef(0, 0, "c4_to_c7");
	}

	// set state var, e.g. int m_x = 123;
	for (VariableDeclaration const* variable: m_pusher.ctx().storageLayout().usualStateVariables()) {
		if (Expression const* value = variable->value().get()) {
			acceptExpr(value);
			m_pusher.setGlob(variable);
		}
	}

	// generate constructor protection
	m_pusher.getGlob(TvmConst::C7::ConstructorFlag);
	m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ConstructorIsCalledTwice));

	for (ContractDefinition const* c: linearizedBaseContracts)
		dfs(c);

	// Decode constructor parameters from the inbound message
	m_pusher.fixStack(+1); // push encoded params of constructor
	int topTake{};
	if (topConstructor == nullptr) {
		m_pusher << "ENDS";
	} else {
		topTake = topConstructor->parameters().size();
		std::vector<Type const*> types = getParams(topConstructor->parameters()).first;
		ChainDataDecoder{&m_pusher}.decodeFunctionParameters(types, false, isCalledByExtMsg);
		m_pusher.getStack().change(-static_cast<int>(topConstructor->parameters().size()));
		for (ASTPointer<VariableDeclaration> const& variable: topConstructor->parameters())
			m_pusher.getStack().add(variable.get(), true);
	}
	solAssert(m_pusher.stackSize() == topTake, "");

	std::set<ContractDefinition const*> areParamsOnStack;
	areParamsOnStack.insert(linearizedBaseContracts.at(0));
	for (ContractDefinition const* c: linearizedBaseContracts | std::views::reverse)
		if (c->constructor() == nullptr || c->constructor()->parameters().empty())
			areParamsOnStack.insert(c);

	for (ContractDefinition const* contract: linearizedBaseContracts | std::views::reverse) {
		if (contract->constructor() == nullptr)
			continue;
		for (ContractDefinition const* baseContract: path[contract]) {
			if (!areParamsOnStack.contains(baseContract)) {
				areParamsOnStack.insert(baseContract);
				auto params = baseContract->constructor()->parameters();
				for (size_t i = 0; i < params.size(); ++i) {
					TVMExpressionCompiler(m_pusher).acceptExpr((*m_args[baseContract])[i].get(), true);
					m_pusher.getStack().add(params[i].get(), false);
				}
			}
		}
		int take = contract->constructor()->parameters().size();
		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.takeLast(take);
		solAssert(pusher.stackSize() == take, "");
		generateFunctionWithModifiers(pusher, contract->constructor(), false);
		solAssert(pusher.stackSize() == take, "");
		m_pusher.fixStack(-take); // fix stack
		m_pusher.add(pusher);
	}

	solAssert(m_pusher.stackSize() == 0, "");
	m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");

	m_pusher.ctx().resetCurrentFunction();
	Pointer<CodeBlock> block = m_pusher.getBlock();
	// Note: take only slice (encoded paramerters), not `topTake`
	Pointer<Function> f = createNode<Function>(1, 0, "constructor", std::nullopt, block, nullptr, false);
	return f;
}

PublicFunctionSelector::PublicFunctionSelector(int _n) {
	maxPath = std::vector<int>(_n + 1, INF);
	sumPaths = std::vector<int>(_n + 1, INF);
	prev = std::vector<std::vector<int>>(_n + 1);
	maxPath[0] = INF;
	for (int n = 1; n <= _n; ++n) {
		maxPath[n] = INF;
		dfs(0, n);
	}
	// for (int n = 1; n <= _n; ++n)  cout
	//	<< std::setw(2) << n << ": "
	//	<< std::setw(4) << int(double(sumPaths[n]) / n) << " "
	//	<< std::setw(4) << maxPath[n]
	//	<< std::endl;
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
		for (int i = 0; i < static_cast<int>(curGroupSize.size()); ++i) {
			int giSize = curGroupSize.at(i);
			if (giSize == 1) {
				curMaxPath = std::max(curMaxPath, FAIL_JMP * i + OK_JMP);
				curSumPath += FAIL_JMP * i + OK_JMP;
			} else {
				curMaxPath = std::max(curMaxPath, FAIL_JMP * i + OK_JMP + maxPath.at(giSize));
				curSumPath += (FAIL_JMP * i + OK_JMP) * giSize + sumPaths.at(giSize);
			}
		}
		if (maxPath[n] > curMaxPath || (maxPath[n] == curMaxPath && sumPaths[n] > curSumPath)) {
			maxPath[n] = curMaxPath;
			sumPaths[n] = curSumPath;
			prev[n] = curGroupSize;
		}
	} else if (pos < static_cast<int>(curGroupSize.size())) {
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
