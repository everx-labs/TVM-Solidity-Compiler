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

#include <boost/algorithm/string/replace.hpp>
#include <boost/range/adaptor/map.hpp>

#include <libsolidity/interface/Version.h>

#include "TVMABI.hpp"
#include "TvmAst.hpp"
#include "TvmAstVisitor.hpp"
#include "TVMConstants.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMInlineFunctionChecker.hpp"
#include "StackOptimizer.hpp"
#include "PeepholeOptimizer.hpp"

using namespace solidity::frontend;
using namespace std;
using namespace solidity::util;

TVMConstructorCompiler::TVMConstructorCompiler(StackPusher &pusher) : m_pusher{pusher} {

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
			auto base = to<ContractDefinition>(modInvoc->name()->annotation().referencedDeclaration);
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
	{
		ChainDataEncoder encode{&m_pusher};
		uint32_t functionId{};
		if (FunctionDefinition const* c = m_pusher.ctx().getContract()->constructor()) {
			functionId = encode.calculateFunctionIDWithReason(c, ReasonOfOutboundMessage::RemoteCallInternal);
		} else {
			functionId = encode.calculateConstructorFunctionID();
		}
		m_pusher.ctx().addPublicFunction(functionId, "constructor");
	}

	m_pusher.push(+1, ""); // push encoded params of constructor
	m_pusher.push(+1, ""); // functionID
	m_pusher.drop();

	c4ToC7WithMemoryInitAndConstructorProtection();

	std::vector<ContractDefinition const*> linearizedBaseContracts =
			m_pusher.ctx().getContract()->annotation().linearizedBaseContracts; // from derived to base
	for (ContractDefinition const* c : linearizedBaseContracts) {
		dfs(c);
	}

	FunctionDefinition const* constructor = linearizedBaseContracts[0]->constructor();
	int take{};
	if (constructor == nullptr) {
		m_pusher.push(-1, "ENDS");
	} else {
		take = constructor->parameters().size();
		vector<Type const*> types = getParams(constructor->parameters()).first;
		ChainDataDecoder{&m_pusher}.decodePublicFunctionParameters(types, false);
		m_pusher.getStack().change(-static_cast<int>(constructor->parameters().size()));
		for (const ASTPointer<VariableDeclaration>& variable: constructor->parameters()) {
			auto name = variable->name();
			m_pusher.getStack().add(variable.get(), true);
		}
	}
	solAssert(m_pusher.stackSize() == take, "");
	std::set<ContractDefinition const*> areParamsOnStack;
	areParamsOnStack.insert(linearizedBaseContracts[0]);
	for (ContractDefinition const* c : linearizedBaseContracts | boost::adaptors::reversed) {
		if (c->constructor() == nullptr || c->constructor()->parameters().empty()) {
			areParamsOnStack.insert(c);
		}
	}

	bool haveConstructor = false;
	for (ContractDefinition const* c : linearizedBaseContracts | boost::adaptors::reversed) {
		if (c->constructor() == nullptr) {
			continue;
		}
		haveConstructor = true;
		for (ContractDefinition const* parent : path[c]) {
			if (areParamsOnStack.count(parent) == 0) {
				areParamsOnStack.insert(parent);
				for (int i = 0; i < static_cast<int>(parent->constructor()->parameters().size()); ++i) {
					TVMExpressionCompiler(m_pusher).acceptExpr((*m_args[parent])[i].get(), true);
					m_pusher.getStack().add(parent->constructor()->parameters()[i].get(), false);
				}
			}
		}


		m_pusher.ctx().setCurrentFunction(c->constructor());

		int take2 = c->constructor()->parameters().size();
		StackPusher pusher = m_pusher;
		pusher.clear();
		pusher.takeLast(take2);
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, c->constructor(), false);
		m_pusher.push(-take2, ""); // fix stack
		m_pusher.add(pusher);
	}

	if (!haveConstructor) {
		m_pusher.push(0, "ACCEPT");
	}

//	solAssert(m_pusher.stackSize() == 0, "");

	m_pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");

	m_pusher._throw("THROW 0");

	Pointer<CodeBlock> block = m_pusher.getBlock();
	// take slice (contains params) and functionID
	Pointer<Function> f = createNode<Function>(2, 0, "constructor", Function::FunctionType::Macro, block);
	return f;
}

void TVMConstructorCompiler::c4ToC7WithMemoryInitAndConstructorProtection() {
	// copy c4 to c7
	m_pusher.was_c4_to_c7_called();
	m_pusher.push(-1, ""); // fix stack

	m_pusher.startContinuation();
	m_pusher.pushCall(0, 0, "c4_to_c7_with_init_storage");
	m_pusher.endContinuationFromRef();
	m_pusher._if();

	// generate constructor protection
	m_pusher.getGlob(TvmConst::C7::ConstructorFlag);
	m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::ConstructorIsCalledTwice));
}

void TVMContractCompiler::printFunctionIds(
	ContractDefinition const& contract,
	PragmaDirectiveHelper const& pragmaHelper
) {
	TVMABI::printFunctionIds(contract, pragmaHelper);
}

void TVMContractCompiler::generateABI(
	const std::string& fileName,
	ContractDefinition const *contract,
	std::vector<PragmaDirective const *> const &pragmaDirectives
) {
	if (!fileName.empty()) {
		ofstream ofile;
		ofile.open(fileName);
		if (!ofile)
			fatal_error("Failed to open the output file: " + fileName);
		TVMABI::generateABI(contract, pragmaDirectives, &ofile);
		ofile.close();
		cout << "ABI was generated and saved to file " << fileName << endl;
	} else {
		TVMABI::generateABI(contract, pragmaDirectives);
	}
}

void TVMContractCompiler::generateCode(
	const std::string& fileName,
	ContractDefinition const& contract,
	PragmaDirectiveHelper const &pragmaHelper
) {
	Pointer<Contract> codeContract = generateContractCode(&contract, pragmaHelper);

    ofstream ofile;
    ofile.open(fileName);
    if (!ofile) {
		fatal_error("Failed to open the output file: " + fileName);
	}
	Printer p{ofile};
	codeContract->accept(p);
    ofile.close();
    cout << "Code was generated and saved to file " << fileName << endl;
}

Pointer<Contract>
TVMContractCompiler::generateContractCode(
	ContractDefinition const *contract,
	PragmaDirectiveHelper const &pragmaHelper
) {
	std::vector<std::string> pragmas;
	std::vector<Pointer<Function>> functions;

	TVMCompilerContext ctx{contract, pragmaHelper};

	if (!ctx.isStdlib()) {
		pragmas.emplace_back(std::string{} + ".version sol " + solidity::frontend::VersionNumber);
	}

	fillInlineFunctions(ctx, contract);

	// generate global constructor which inlines all contract constructors
	if (!ctx.isStdlib()) {
		StackPusher pusher{&ctx};
		TVMConstructorCompiler compiler(pusher);
		Pointer<Function> f = compiler.generateConstructors();
		functions.push_back(f);
	}

	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const *_function : c->definedFunctions()) {
			if (_function->isConstructor() ||
				!_function->isImplemented() ||
				_function->isInline()) {
				continue;
			}

			ctx.setCurrentFunction(_function);

			if (_function->isOnBounce()) {
				if (!ctx.isOnBounceGenerated()) {
					ctx.setIsOnBounce();
					functions.push_back(TVMFunctionCompiler::generateOnBounce(ctx, _function));
				}
			} else if (_function->isReceive()) {
				if (!ctx.isReceiveGenerated()) {
					ctx.setIsReceiveGenerated();
					functions.push_back(TVMFunctionCompiler::generateReceive(ctx, _function));
				}
			} else if (_function->isFallback()) {
				if (!ctx.isFallBackGenerated()) {
					ctx.setIsFallBackGenerated();
					functions.push_back(TVMFunctionCompiler::generateFallback(ctx, _function));
				}
			} else if (_function->isOnTickTock()) {
				functions.push_back(TVMFunctionCompiler::generateOnTickTock(ctx, _function));
			} else if (isMacro(_function->name())) {
				functions.push_back(TVMFunctionCompiler::generateMacro(ctx, _function));
			} else if (_function->name() == "onCodeUpgrade") {
				functions.push_back(TVMFunctionCompiler::generateOnCodeUpgrade(ctx, _function));
			} else {
				if (_function->isPublic()) {
					bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
					if (!isBaseMethod) {
						functions.push_back(TVMFunctionCompiler::generatePublicFunction(ctx, _function));

						StackPusher pusher{&ctx};
						ChainDataEncoder encoder{&pusher}; // TODO delete pusher
						uint32_t functionId = encoder.calculateFunctionIDWithReason(_function,
																					ReasonOfOutboundMessage::RemoteCallInternal);
						ctx.addPublicFunction(functionId, _function->name());
					}
				}
				std::string functionName = ctx.getFunctionInternalName(_function);
				if (_function->visibility() <= Visibility::Public) {
					functions.push_back(TVMFunctionCompiler::generatePrivateFunction(ctx, functionName));
				}
				{
					const std::string macroName = functionName + "_macro";
					functions.push_back(TVMFunctionCompiler::generateMacro(ctx, _function, macroName));
				}
			}
		}
	}

	if (!ctx.isStdlib()) {
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7WithInitMemory(ctx));
		{
			StackPusher pusher{&ctx};
			Pointer<Function> f = pusher.generateC7ToT4Macro(false);
			functions.emplace_back(f);
		}
		if (ctx.usage().hasAwaitCall()) {
			StackPusher pusher{&ctx};
			Pointer<Function> f = pusher.generateC7ToT4Macro(true);
			functions.emplace_back(f);
		}
		functions.emplace_back(TVMFunctionCompiler::updateOnlyTime(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateMainInternal(ctx, contract));
		if (ctx.usage().hasAwaitCall()) {
			functions.emplace_back(TVMFunctionCompiler::generateCheckResume(ctx));
		}
		{
			functions.emplace_back(TVMFunctionCompiler::generateMainExternal(ctx, contract));
		}
	}

	for (VariableDeclaration const* vd : ctx.notConstantStateVariables()) {
		if (vd->isPublic()) {
			StackPusher pusher{&ctx};
			Pointer<Function> f = TVMFunctionCompiler::generateGetter(pusher, vd);
			functions.emplace_back(f);

			ChainDataEncoder encoder{&pusher};
			std::vector<VariableDeclaration const*> outputs = {vd};
			uint32_t functionId = encoder.calculateFunctionIDWithReason(
				vd->name(),
				{},
				&outputs,
				ReasonOfOutboundMessage::RemoteCallInternal,
				nullopt,
				false
			);
			ctx.addPublicFunction(functionId, vd->name());
		}
	}

	map<FunctionDefinition const *, bool> usedFunctions;
	while (!ctx.getLibFunctions().empty()) {
		FunctionDefinition const *function = *ctx.getLibFunctions().begin();
		ctx.getLibFunctions().erase(ctx.getLibFunctions().begin());
		if (usedFunctions[function]) {
			continue;
		}
		usedFunctions[function] = true;

		if (!function->modifiers().empty()) {
			cast_error(*function->modifiers().at(0).get(),
					   "Modifiers for library functions are not supported yet.");
		}

		if (!function->parameters().empty()) {
			{
				const std::string name = TVMCompilerContext::getLibFunctionName(function, true);
				functions.emplace_back(TVMFunctionCompiler::generateLibraryFunction(ctx, function, name));
			}
			{
				const std::string name = TVMCompilerContext::getLibFunctionName(function, true) + "_macro";
				functions.emplace_back(TVMFunctionCompiler::generateLibraryFunctionMacro(ctx, function, name));
			}
		}
		const std::string name = TVMCompilerContext::getLibFunctionName(function, false);
		functions.emplace_back(TVMFunctionCompiler::generatePrivateFunction(ctx, name));
		functions.emplace_back(TVMFunctionCompiler::generateMacro(ctx, function, name + "_macro"));
	}

	for (const auto&[name, arr] : ctx.constArrays()) {
		functions.emplace_back(TVMFunctionCompiler::generateConstArrays(ctx, name, arr));
	}

	for (const auto&[name, arr] : ctx.newArrays()) {
		functions.emplace_back(TVMFunctionCompiler::generateNewArrays(ctx, name, arr));
	}

	if (!ctx.isStdlib()) {
		functions.emplace_back(TVMFunctionCompiler::generatePublicFunctionSelector(ctx, contract));
	}

	if (ctx.usage().hasTvmCode()) {
		pragmas.emplace_back(".pragma selector-save-my-code");
	}

	Pointer<Contract> c = createNode<Contract>(pragmas, functions);

	DeleterAfterRet d;
	c->accept(d);

	LocSquasher sq;
	c->accept(sq);

	optimizeCode(c);

	return c;
}

void TVMContractCompiler::optimizeCode(Pointer<Contract>& c) {
	DeleterCallX dc;
	c->accept(dc);

	LogCircuitExpander lce;
	c->accept(lce);

	StackOptimizer opt;
	c->accept(opt);

	PeepholeOptimizer peepHole{false};
	c->accept(peepHole);

	opt = StackOptimizer{};
	c->accept(opt);

	peepHole = PeepholeOptimizer{false};
	c->accept(peepHole);

	peepHole = PeepholeOptimizer{true};
	c->accept(peepHole);

	LocSquasher sq = LocSquasher{};
	c->accept(sq);
}

void TVMContractCompiler::fillInlineFunctions(TVMCompilerContext &ctx, ContractDefinition const *contract) {
	std::map<std::string, FunctionDefinition const *> inlineFunctions;
	for (ContractDefinition const *base : contract->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const *function : base->definedFunctions()) {
			if (function->isInline()) {
				inlineFunctions[functionName(function)] = function;
			}
		}
	}

	TVMInlineFunctionChecker inlineFunctionChecker;
	for (FunctionDefinition const *function : inlineFunctions | boost::adaptors::map_values) {
		function->accept(inlineFunctionChecker);
	}
	std::vector<FunctionDefinition const *> order = inlineFunctionChecker.functionOrder();

	for (FunctionDefinition const * function : order) {
		ctx.setCurrentFunction(function);
		StackPusher pusher{&ctx};
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, function, true);
		const std::string name = functionName(function);

		Pointer<CodeBlock> body = pusher.getBlock();
		ctx.addInlineFunction(name, body);
	}
}

