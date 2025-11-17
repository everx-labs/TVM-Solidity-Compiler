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

#include <fstream>

#include <boost/algorithm/string/replace.hpp>

#include <libsolidity/interface/Version.h>

#include <libsolidity/codegen/Printer.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMContractCompiler.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMFunctionCompiler.hpp>
#include <libsolidity/codegen/TVMInlineFunctionChecker.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <libsolidity/codegen/optimizers/CallRefOptimizer.hpp>
#include <libsolidity/codegen/optimizers/MiscOptimizer.hpp>
#include <libsolidity/codegen/optimizers/PeepholeOptimizer.hpp>
#include <libsolidity/codegen/optimizers/StackOptimizer.hpp>

using namespace solidity::frontend;
using namespace solidity::util;


void TVMContractCompiler::printFunctionIds(
	std::string const& fileName,
	ContractDefinition const& contract,
	PragmaDirectiveHelper const& pragmaHelper
) {
	std::ofstream outFile = openFile(fileName);
	Json functionIds = TVMABI::generateFunctionIdsJson(contract, pragmaHelper);
	outFile << std::setw(TVMABI::INDENT_SPACES) << functionIds << std::endl;
	std::cout << "Function IDs were generated and saved to file " << fileName << std::endl;
}

void TVMContractCompiler::printPrivateFunctionIds(
	std::string const& fileName,
	ContractDefinition const& contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper,
	bool debugMode
) {
	std::ofstream outFile = openFile(fileName);
	Json functionIds = TVMABI::generatePrivateFunctionIdsJson(contract, _sourceUnits, pragmaHelper, debugMode);
	outFile << std::setw(TVMABI::INDENT_SPACES) << functionIds << std::endl;
	std::cout << "Private function IDs were generated and saved to file " << fileName << std::endl;
}

void TVMContractCompiler::generateABI(
	std::string const& fileName,
	ContractDefinition const* contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const*> const& pragmaDirectives
) {
	std::ofstream outFile = openFile(fileName);
	TVMABI::generateABI(contract, _sourceUnits, pragmaDirectives, outFile);
	outFile.close();
	std::cout << "ABI was generated and saved to file " << fileName << std::endl;
}

void TVMContractCompiler::generateCodeAndSaveToFile(
	std::string const& fileName,
	ContractDefinition const& contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper,
	bool debugMode
) {
	Pointer<Contract> codeContract = generateContractCode(&contract, _sourceUnits, pragmaHelper, debugMode);

	std::ofstream outFile = openFile(fileName);
	Printer p{outFile};
	codeContract->accept(p);
	outFile.close();
	std::cout << "Code was generated and saved to file " << fileName << std::endl;
}

Pointer<Contract> TVMContractCompiler::generateContractCode(
	ContractDefinition const* contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper,
	bool debugMode
) {
	std::vector<Pointer<Function>> functions;
	std::map<uint32_t, std::string> getters;

	TVMCompilerContext ctx{contract, pragmaHelper};

	fillInlineFunctions(ctx, contract, _sourceUnits);

	// generate a global constructor which inlines all contract's constructors
	if (!ctx.isStdlib() && !ctx.getContract()->isContractLibrary() && ctx.storageLayout().hasConstructor()) {
		StackPusher pusher{&ctx};
		TVMConstructorCompiler compiler(pusher);
		Pointer<Function> f = compiler.generateConstructors();
		functions.emplace_back(f);
	}

	for (ContractDefinition const* c: contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const* _function: c->definedFunctions()) {
			if (_function->isConstructor() || !_function->isImplemented() || _function->isInline())
				continue;

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
				if (ctx.fallBack() == nullptr) {
					ctx.setFallback(_function);
					functions.emplace_back(TVMFunctionCompiler::generateFallback(ctx, _function));
				}
			} else if (_function->isOnTickTock()) {
				functions.emplace_back(TVMFunctionCompiler::generateOnTickTock(ctx, _function));
			} else if (_function->name() == "onCodeUpgrade") {
				if (!ctx.isBaseFunction(_function))
					functions.emplace_back(TVMFunctionCompiler::generateOnCodeUpgrade(ctx, _function));
			} else {
				if (!ctx.isStdlib() &&
					!ctx.getContract()->isContractLibrary() &&
					_function->isPublic() &&
					!ctx.isBaseFunction(_function)) {
					if (_function->visibility() == Visibility::Getter) {
						functions.emplace_back(TVMFunctionCompiler::generateGetterFunction(ctx, _function));
						uint32_t functionId = crc16(_function->name());
						functionId = (functionId & 0xffff) | 0x10000;
						bool emplace = getters.emplace(functionId, _function->name()).second;
						solAssert(emplace, "");
					} else {
						functions.emplace_back(TVMFunctionCompiler::generatePublicFunction(ctx, _function));
						uint32_t functionId = ChainDataEncoder::
							calculateFunctionIDWithReason(_function, ReasonOfOutboundMessage::RemoteCallInternal);

						ctx.addPublicFunction(_function, functionId, _function->name());
					}
				}
				auto const [functionName, id] = ctx.functionInternalName(_function, true);
				functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, _function, functionName, id));
			}
		}
	}

	if (!ctx.isStdlib() && !ctx.getContract()->isContractLibrary()) {
		functions.emplace_back(TVMFunctionCompiler::generateC4ToC7(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateDefaultC4(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateC7ToC4(ctx));
		if (contract->externalMsgHeaders())
			functions.emplace_back(TVMFunctionCompiler::updateOnlyTime(ctx));
		functions.emplace_back(TVMFunctionCompiler::generateMainInternal(ctx, contract));
		if (contract->externalMsgHeaders()) {
			functions.emplace_back(TVMFunctionCompiler::generateMainExternal(ctx, contract));
		}
	}

	// generate library functions
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits) {
		for (ASTPointer<ASTNode> const& node: source->nodes()) {
			if (auto lib = dynamic_cast<ContractDefinition const*>(node.get())) {
				if (lib->isLibrary()) {
					for (FunctionDefinition const* function: lib->definedFunctions()) {
						if (!function->modifiers().empty()) {
							cast_error(
								*function->modifiers().at(0).get(),
								"Modifiers for library functions are not supported yet."
							);
						}
						if (!function->parameters().empty()) {
							std::string const name = ctx.functionInternalName(function, true).first;
							functions.emplace_back(
								TVMFunctionCompiler::generateLibFunctionWithObject(ctx, function, name)
							);
						}
						auto const [name, id] = ctx.functionInternalName(function, false);
						functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, function, name, id));
					}
				}
			}
		}
	}

	// generate free functions
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits) {
		for (ASTPointer<ASTNode> const& node: source->nodes()) {
			if (auto function = dynamic_cast<FunctionDefinition const*>(node.get())) {
				if (function->isFree() && !function->isInlineAssembly()) {
					if (!function->modifiers().empty())
						cast_error(
							*function->modifiers().at(0).get(),
							"Modifiers for free functions are not supported yet."
						);
					if (!function->parameters().empty()) {
						std::string const name = ctx.functionInternalName(function, true).first;
						functions.emplace_back(TVMFunctionCompiler::generateLibFunctionWithObject(ctx, function, name));
					}
					auto const [name, id] = ctx.functionInternalName(function, false);
					functions.emplace_back(TVMFunctionCompiler::generateFunction(ctx, function, name, id));
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

	for (auto const& [name, arr]: ctx.newArrays())
		functions.emplace_back(TVMFunctionCompiler::generateNewArrays(ctx, name, arr));

	for (auto const& [name, types]: ctx.buildTuple())
		functions.emplace_back(TVMFunctionCompiler::generateBuildTuple(ctx, name, types));

	std::map<std::string, Pointer<Function>> functionsInMap;
	for (auto const& func: functions) {
		std::string name = func->name();
		solAssert(!functionsInMap.contains(name), "");
		functionsInMap[name] = func;
	}
	std::vector<Pointer<Function>> functionOrder;
	std::vector<std::string> functionDag = ctx.callGraph().DAG();
	for (std::string const& funcDef: ctx.callGraph().DAG()) {
		if (!functionsInMap.contains(funcDef)) {
			// TODO check stdlib function or inline function
			continue;
		}
		Pointer<Function> f = functionsInMap.at(funcDef);
		functionOrder.emplace_back(f);
		functionsInMap.erase(functionsInMap.find(funcDef));
	}
	for (auto const& func: functions) {
		if (functionsInMap.contains(func->name()))
			functionOrder.emplace_back(func);
	}

	Contract::ContractType type;
	if (ctx.isStdlib())
		type = Contract::ContractType::StdLibrary;
	else if (ctx.getContract()->isContractLibrary())
		type = Contract::ContractType::ContractLibrary;
	else
		type = Contract::ContractType::Contract;

	Pointer<Contract> c = createNode<Contract>(
		type,
		ctx.getPragmaSaveAllFunctions(),
		pragmaHelper.hasUpgradeOldSol(),
		std::string{"sol "} + solidity::frontend::VersionNumber,
		functionOrder,
		ctx.callGraph().privateFunctions(),
		getters
	);


	if (!debugMode) {
		optimizeCode(c, functionDag);
	}

	return c;
}

void TVMContractCompiler::optimizeCode(Pointer<Contract> const& c, std::vector<std::string> const& functionDag) {
	Printer printer{std::cerr, ""};
	bool debugCode = false;
	if (debugCode)
		c->accept(printer);

	DeleterAfterRet d;
	c->accept(d);

	LocSquasher sq;
	c->accept(sq);

	LogCircuitExpander lce;
	c->accept(lce);
	if (debugCode)
		c->accept(printer);

	CallRefToCallXOptimizer::optimize(c);
	if (debugCode)
		c->accept(printer);

	CallRefInliner::optimize(c, functionDag);
	if (debugCode)
		c->accept(printer);

	{
		StackOptimizer opt{false};
		c->accept(opt);
		if (debugCode)
			c->accept(printer);
	}

	lce = LogCircuitExpander{};
	c->accept(lce);
	if (debugCode)
		c->accept(printer);

	for (int i = 0; i < 2; ++i) {
		PeepholeOptimizer peepHole{{}};
		c->accept(peepHole);
		if (debugCode)
			c->accept(printer);

		StackOptimizer opt{false};
		c->accept(opt);
		if (debugCode)
			c->accept(printer);
	}

	for (int i = 0; i < 2; ++i) {
		PeepholeOptimizer peepHole{{}};
		c->accept(peepHole);
		if (debugCode)
			c->accept(printer);

		StackOptimizer opt{true};
		c->accept(opt);
		if (debugCode)
			c->accept(printer);
	}

	PeepholeOptimizer peepHole{1 << static_cast<size_t>(OptFlags::UnpackOpaque)};
	c->accept(peepHole);
	if (debugCode)
		c->accept(printer);

	peepHole = PeepholeOptimizer{
		1 << static_cast<size_t>(OptFlags::UnpackOpaque) |
		1 << static_cast<size_t>(OptFlags::OptimizeSlice) |
		1 << static_cast<size_t>(OptFlags::UseCompoundOpcodes) |
		1 << static_cast<size_t>(OptFlags::UseR) |
		1 << static_cast<size_t>(OptFlags::UnpackIfElse)
	};
	c->accept(peepHole);

	for (int i = 0; i < 2; ++i) {
		CallRefInliner::optimize(c, functionDag);

		c->accept(peepHole);
	}

	sq = LocSquasher{};
	c->accept(sq);
}

void TVMContractCompiler::fillInlineFunctions(
	TVMCompilerContext& ctx,
	ContractDefinition const* contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits
) {
	std::set<FunctionDefinition const*> inlineFunctions;
	for (ContractDefinition const* base: contract->annotation().linearizedBaseContracts | std::views::reverse) {
		for (FunctionDefinition const* function: base->definedFunctions()) {
			if (function->isInline()) {
				inlineFunctions.insert(function);
			}
		}
	}
	// generate free functions
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits) {
		for (ASTPointer<ASTNode> const& node: source->nodes()) {
			if (auto function = dynamic_cast<FunctionDefinition const*>(node.get())) {
				if (function->isFree() && !function->isInlineAssembly() && function->isInline()) {
					inlineFunctions.insert(function);
				}
			}
		}
	}

	TVMInlineFunctionChecker inlineFunctionChecker;
	for (FunctionDefinition const* function: inlineFunctions) {
		function->accept(inlineFunctionChecker);
	}
	std::vector<FunctionDefinition const*> order = inlineFunctionChecker.functionOrder();

	for (FunctionDefinition const* function: order) {
		std::string const name = ctx.functionInternalName(function, false).first;
		ctx.setCurrentFunction(function, name);
		StackPusher pusher{&ctx};
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, function, true);
		Pointer<CodeBlock> body = pusher.getBlock();
		ctx.addInlineFunction(name, body);
		ctx.resetCurrentFunction();
	}
}

std::ofstream TVMContractCompiler::openFile(std::string const& fileName) {
	std::ofstream outFile;
	outFile.open(fileName);
	if (!outFile)
		fatal_error("Failed to open the output file: " + fileName);
	return outFile;
}
