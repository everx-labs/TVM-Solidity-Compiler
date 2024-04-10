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

#include <libsolidity/codegen/PeepholeOptimizer.hpp>
#include <libsolidity/codegen/SizeOptimizer.hpp>
#include <libsolidity/codegen/StackOptimizer.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMContractCompiler.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMFunctionCompiler.hpp>
#include <libsolidity/codegen/TVMInlineFunctionChecker.hpp>

using namespace solidity::frontend;
using namespace std;
using namespace solidity::util;


void TVMContractCompiler::printFunctionIds(
	ContractDefinition const& contract,
	PragmaDirectiveHelper const& pragmaHelper
) {
	Json::Value functionIds = TVMABI::generateFunctionIdsJson(contract, pragmaHelper);
	cout << functionIds << endl;
}

void TVMContractCompiler::printPrivateFunctionIds(
	ContractDefinition const& contract,
	std::vector<std::shared_ptr<SourceUnit>> const& _sourceUnits,
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
	std::vector<std::shared_ptr<SourceUnit>>const& _sourceUnits,
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
	std::vector<std::shared_ptr<SourceUnit>>const& _sourceUnits,
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
				if (!ctx.isStdlib() && _function->isPublic() && !ctx.isBaseFunction(_function)) {
					functions.emplace_back(TVMFunctionCompiler::generatePublicFunction(ctx, _function));

					StackPusher pusher{&ctx};
					ChainDataEncoder encoder{&pusher};
					uint32_t functionId = encoder.calculateFunctionIDWithReason(_function,
																				ReasonOfOutboundMessage::RemoteCallInternal);
					ctx.addPublicFunction(functionId, _function->name());
				}
				std::string const functionName = ctx.getFunctionInternalName(_function);
				functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, _function, functionName));
			}
		}
	}

	if (!ctx.isStdlib()) {
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateDefaultC4(ctx));
		{
			StackPusher pusher{&ctx};
			Pointer<Function> f = pusher.generateC7ToC4();
			functions.emplace_back(f);
		}
		functions.emplace_back(TVMFunctionCompiler::updateOnlyTime(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateMainInternal(ctx, contract));
		functions.emplace_back(TVMFunctionCompiler::generateMainExternal(ctx, contract));
	}

	for (VariableDeclaration const* vd : ctx.c4StateVariables()) {
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
						if (!function->parameters().empty()) {
							const std::string name = TVMCompilerContext::getLibFunctionName(function, true);
							functions.emplace_back(TVMFunctionCompiler::generateLibFunctionWithObject(ctx, function, name));
						}
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
					if (!function->modifiers().empty())
						cast_error(*function->modifiers().at(0).get(),
								   "Modifiers for free functions are not supported yet.");
					if (!function->parameters().empty()) {
						const std::string name = ctx.getFunctionInternalName(function, true);
						functions.emplace_back(TVMFunctionCompiler::generateLibFunctionWithObject(ctx, function, name));
					}
					const std::string name = ctx.getFunctionInternalName(function, false);
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

	{
		StackOptimizer opt;
		c->accept(opt);
	}

	lce = LogCircuitExpander{};
	c->accept(lce);

	for (int i = 0; i < 10; ++i) { // TODO
		PeepholeOptimizer::withBlockPush = false; // TODO
		PeepholeOptimizer peepHole{false, false, false};
		c->accept(peepHole);
		PeepholeOptimizer::withBlockPush = true;

		StackOptimizer opt;
		c->accept(opt);
	}

	PeepholeOptimizer peepHole = PeepholeOptimizer{false, false, false};
	c->accept(peepHole);

	peepHole = PeepholeOptimizer{true, false, false}; // TODO provide mask with bits
	c->accept(peepHole);

	peepHole = PeepholeOptimizer{true, true, true}; // TODO provide mask with bits
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

