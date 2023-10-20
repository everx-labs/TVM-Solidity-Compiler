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

#include <fstream>
#include <boost/algorithm/string/replace.hpp>
#include <boost/range/adaptor/map.hpp>

#include <libsolidity/interface/Version.h>

#include "PeepholeOptimizer.hpp"
#include "SizeOptimizer.hpp"
#include "StackOptimizer.hpp"
#include "TVMABI.hpp"
#include "TvmAst.hpp"
#include "TvmAstVisitor.hpp"
#include "TVMConstants.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMInlineFunctionChecker.hpp"

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

	c4ToC7WithMemoryInitAndConstructorProtection();

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

void TVMConstructorCompiler::c4ToC7WithMemoryInitAndConstructorProtection() {
	// copy c4 to c7
	m_pusher.was_c4_to_c7_called();
	m_pusher.fixStack(-1); // fix stack

	m_pusher.startContinuation();
	m_pusher.pushFragment(0, 0, "c4_to_c7_with_init_storage");
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
	Json::Value functionIds = TVMABI::generateFunctionIdsJson(contract, pragmaHelper);
	cout << functionIds << endl;
}

void TVMContractCompiler::printPrivateFunctionIds(
	ContractDefinition const& contract,
	std::vector<std::shared_ptr<SourceUnit>> _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper
) {
	Json::Value functionIds = TVMABI::generatePrivateFunctionIdsJson(contract, _sourceUnits, pragmaHelper);
	cout << functionIds << endl;
}

void TVMContractCompiler::generateABI(
	const std::string& fileName,
	ContractDefinition const *contract,
	std::vector<std::shared_ptr<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const *> const &pragmaDirectives
) {
	if (!fileName.empty()) {
		ofstream ofile;
		ofile.open(fileName);
		if (!ofile)
			fatal_error("Failed to open the output file: " + fileName);
		TVMABI::generateABI(contract, _sourceUnits, pragmaDirectives, &ofile);
		ofile.close();
		cout << "ABI was generated and saved to file " << fileName << endl;
	} else {
		TVMABI::generateABI(contract, _sourceUnits, pragmaDirectives);
	}
}

void TVMContractCompiler::generateCodeAndSaveToFile(
	const std::string& fileName,
	ContractDefinition const& contract,
	std::vector<std::shared_ptr<SourceUnit>> _sourceUnits,
	PragmaDirectiveHelper const &pragmaHelper
) {
	Pointer<Contract> codeContract = generateContractCode(&contract, _sourceUnits, pragmaHelper);

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
	std::vector<std::shared_ptr<SourceUnit>> _sourceUnits,
	PragmaDirectiveHelper const &pragmaHelper
) {
	std::vector<Pointer<Function>> functions;

	TVMCompilerContext ctx{contract, pragmaHelper};

	fillInlineFunctions(ctx, contract);

	// generate global constructor which inlines all contract's constructors
	if (!ctx.isStdlib()) {
		StackPusher pusher{&ctx};
		TVMConstructorCompiler compiler(pusher);
		Pointer<Function> f = compiler.generateConstructors();
		functions.emplace_back(f);
	}

	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const *_function : c->definedFunctions()) {
			if (_function->isConstructor() ||
				!_function->isImplemented() ||
				_function->isInline()) {
				continue;
			}

			if (_function->isOnBounce()) {
				if (!ctx.isOnBounceGenerated()) {
					ctx.setIsOnBounce();
					functions.emplace_back(TVMFunctionCompiler::generateOnBounce(ctx, _function));
				}
			} else if (_function->isReceive()) {
				if (!ctx.isReceiveGenerated()) {
					ctx.setIsReceiveGenerated();
					functions.emplace_back(TVMFunctionCompiler::generateReceive(ctx, _function));
				}
			} else if (_function->isFallback()) {
				if (!ctx.isFallBackGenerated()) {
					ctx.setIsFallBackGenerated();
					functions.emplace_back(TVMFunctionCompiler::generateFallback(ctx, _function));
				}
			} else if (_function->isOnTickTock()) {
				functions.emplace_back(TVMFunctionCompiler::generateOnTickTock(ctx, _function));
			} else if (_function->name() == "onCodeUpgrade") {
				if (!ctx.isBaseFunction(_function))
					functions.emplace_back(TVMFunctionCompiler::generateOnCodeUpgrade(ctx, _function));
			} else {
				if (!ctx.isStdlib()) {
					if (_function->isPublic()) {
						bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
						if (!isBaseMethod) {
							functions.emplace_back(TVMFunctionCompiler::generatePublicFunction(ctx, _function));

							StackPusher pusher{&ctx};
							ChainDataEncoder encoder{&pusher}; // TODO delete pusher
							uint32_t functionId = encoder.calculateFunctionIDWithReason(_function,
																						ReasonOfOutboundMessage::RemoteCallInternal);
							ctx.addPublicFunction(functionId, _function->name());
						}
					}
				}
				std::string const functionName = ctx.getFunctionInternalName(_function);
				functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, _function, functionName));
			}
		}
	}

	if (!ctx.isStdlib()) {
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7WithInitMemory(ctx));
		{
			StackPusher pusher{&ctx};
			Pointer<Function> f = pusher.generateC7ToC4(false);
			functions.emplace_back(f);
		}
		if (ctx.usage().hasAwaitCall()) {
			StackPusher pusher{&ctx};
			Pointer<Function> f = pusher.generateC7ToC4(true);
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

	// generate library functions
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits) {
		for (ASTPointer<ASTNode> const &node: source->nodes()) {
			if (auto lib = dynamic_cast<ContractDefinition const *>(node.get())) {
				if (lib->isLibrary()) {
					for (FunctionDefinition const *function : lib->definedFunctions()) {
						if (!function->modifiers().empty()) {
							cast_error(*function->modifiers().at(0).get(),
									   "Modifiers for library functions are not supported yet.");
						}
						if (!function->parameters().empty())
							functions.emplace_back(TVMFunctionCompiler::generateLibFunctionWithObject(ctx, function));
						const std::string name = TVMCompilerContext::getLibFunctionName(function, false);
						functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, function, name));
					}
				}
			}
		}
	}

	// generate free functions
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits) {
		for (ASTPointer<ASTNode> const &node: source->nodes()) {
			if (auto function = dynamic_cast<FunctionDefinition const *>(node.get())) {
				if (function->isFree() && !function->isInlineAssembly()) {
					if (!function->modifiers().empty()) {
						cast_error(*function->modifiers().at(0).get(),
								   "Modifiers for free functions are not supported yet.");
					}
					const std::string name = ctx.getFunctionInternalName(function);
					functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, function, name));
				}
			}
		}
	}

	std::map<std::string, bool> usedInlineArrays;
	auto it = ctx.constArrays().begin();
	while (it != ctx.constArrays().end()) {
		auto const [name, arr] = std::tie(it->first, it->second);
		if (!usedInlineArrays[name]) {
			usedInlineArrays[name] = true;
			functions.emplace_back(TVMFunctionCompiler::generateConstArrays(ctx, name, arr));
		}

		ctx.constArrays().erase(it);
		it = ctx.constArrays().begin();
	}

	for (const auto&[name, arr] : ctx.newArrays())
		functions.emplace_back(TVMFunctionCompiler::generateNewArrays(ctx, name, arr));

	for (const auto&[name, types] : ctx.buildTuple())
		functions.emplace_back(TVMFunctionCompiler::generateBuildTuple(ctx, name, types));

	if (!ctx.isStdlib())
		functions.emplace_back(TVMFunctionCompiler::generatePublicFunctionSelector(ctx, contract));


	std::map<std::string, Pointer<Function>> functionsInMap;
	for (const auto& func : functions) {
		solAssert(functionsInMap.count(func->name()) == 0, "");
		functionsInMap[func->name()] = func;
	}
	std::vector<Pointer<Function>> functionOrder;
	for (std::string const& funcDef : ctx.callGraph().DAG()) {
		if (functionsInMap.count(funcDef) == 0) {
			// TODO check stdlib function or inline function
			continue;
		}
		Pointer<Function> f = functionsInMap.at(funcDef);
		functionOrder.emplace_back(f);
		functionsInMap.erase(functionsInMap.find(funcDef));
	}
	for (const auto& func : functions) {
		if (functionsInMap.count(func->name()) != 0)
			functionOrder.emplace_back(func);
	}

	Pointer<Contract> c = createNode<Contract>(
		ctx.isStdlib(), ctx.getPragmaSaveAllFunctions(), pragmaHelper.hasUpgradeFunc(), pragmaHelper.hasUpgradeOldSol(),
		std::string{"sol "} + solidity::frontend::VersionNumber,
		functionOrder, ctx.callGraph().privateFunctions()
	);

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

	PeepholeOptimizer peepHole{false, false};
	c->accept(peepHole);

	opt = StackOptimizer{};
	c->accept(opt);

	peepHole = PeepholeOptimizer{false, false};
	c->accept(peepHole);

	peepHole = PeepholeOptimizer{true, false};
	c->accept(peepHole);

	peepHole = PeepholeOptimizer{true, true};
	c->accept(peepHole);

	LocSquasher sq = LocSquasher{};
	c->accept(sq);

	SizeOptimizer so{};
	so.optimize(c);
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
		const std::string name = functionName(function);
		ctx.setCurrentFunction(function, name);
		StackPusher pusher{&ctx};
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, function, true);
		Pointer<CodeBlock> body = pusher.getBlock();
		ctx.addInlineFunction(name, body);
		ctx.resetCurrentFunction();
	}
}

