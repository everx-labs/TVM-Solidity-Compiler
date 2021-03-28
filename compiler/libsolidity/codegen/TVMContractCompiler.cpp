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
#include <boost/range/adaptor/map.hpp>

#include "TVMABI.hpp"
#include "TVMAnalyzer.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCompiler.hpp"
#include "TVMInlineFunctionChecker.hpp"
#include "TVMOptimizations.hpp"

using namespace solidity::frontend;

TVMConstructorCompiler::TVMConstructorCompiler(StackPusherHelper &pusher) : m_pusher{pusher} {

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

void TVMConstructorCompiler::generateConstructors() {
	{
		EncodeFunctionParams encode{&m_pusher};
		uint32_t functionId{};
		if (FunctionDefinition const* c = m_pusher.ctx().getContract()->constructor()) {
			functionId = encode.calculateFunctionIDWithReason(c, ReasonOfOutboundMessage::RemoteCallInternal);
		} else {
			functionId = encode.calculateConstructorFunctionID();
		}
		m_pusher.ctx().addPublicFunction(functionId, "constructor");
	}

	m_pusher.generateMacro("constructor");
	m_pusher.push(0, "DROP");

	c4ToC7WithMemoryInitAndConstructorProtection();

	m_pusher.push(+1, ""); // push encoded params of constructor
	std::vector<ContractDefinition const*> linearizedBaseContracts =
			m_pusher.ctx().getContract()->annotation().linearizedBaseContracts; // from derived to base
	for (ContractDefinition const* c : linearizedBaseContracts) {
		dfs(c);
	}

	if (linearizedBaseContracts[0]->constructor() == nullptr) {
		m_pusher.push(-1, "ENDS");
	} else {
		DecodeFunctionParams{&m_pusher}.decodeParameters(linearizedBaseContracts[0]->constructor()->parameters(), false);
		m_pusher.getStack().change(-static_cast<int>(linearizedBaseContracts[0]->constructor()->parameters().size()));
		for (const ASTPointer<VariableDeclaration>& variable: linearizedBaseContracts[0]->constructor()->parameters()) {
			auto name = variable->name();
			m_pusher.push(0, string(";; param: ") + name);
			m_pusher.getStack().add(variable.get(), true);
		}
	}
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
					m_pusher.push(0, "; decl " + parent->name() + "::" + parent->constructor()->parameters()[i]->name());
					TVMExpressionCompiler(m_pusher).acceptExpr((*m_args[parent])[i].get(), true);
					m_pusher.getStack().add(parent->constructor()->parameters()[i].get(), false);
				}
			}
		}

		m_pusher.ctx().setCurrentFunction(c->constructor());
		TVMFunctionCompiler::generateFunctionWithModifiers(m_pusher, c->constructor(), false);
	}

	if (!haveConstructor) {
		m_pusher.push(0, "ACCEPT");
	}

	solAssert(m_pusher.getStack().size() == 0, "");

	m_pusher.pushMacroCallInCallRef(0, "c7_to_c4");

	m_pusher.push(0, "TRUE");
	m_pusher.push(0, "SETGLOB 7");
	m_pusher.push(0, " ");
}


void TVMConstructorCompiler::c4ToC7WithMemoryInitAndConstructorProtection() {
	// copy c4 to c7
	m_pusher.pushLines(R"(
GETGLOB 1
ISNULL
)");

	m_pusher.startIfRef();
	m_pusher.pushCall(0, "c4_to_c7_with_init_storage");
	m_pusher.endContinuation();

	// generate constructor protection
	std::string str = R"(
;; constructor protection
GETGLOB 6
THROWIF ConstructorIsCalledTwice
PUSHINT 1
SETGLOB 6
;; end constructor protection
)";
	boost::replace_all(str, "ConstructorIsCalledTwice", toString(TvmConst::RuntimeException::ConstructorIsCalledTwice));
	m_pusher.pushLines(str);

}

langutil::ErrorReporter* TVMContractCompiler::g_errorReporter{};

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

void TVMContractCompiler::proceedContract(
	const std::string& fileName,
	ContractDefinition const& contract,
	PragmaDirectiveHelper const &pragmaHelper,
	bool withOptimizations
) {
	CodeLines code = proceedContractMode1(&contract, pragmaHelper, withOptimizations);

	if (!fileName.empty()) {
		ofstream ofile;
		ofile.open(fileName);
		if (!ofile)
			fatal_error("Failed to open the output file: " + fileName);
		ofile << code.str();
		ofile.close();
		cout << "Code was generated and saved to file " << fileName << endl;
	} else {
		cout << code.str();
	}
}

static void optimize_and_append_code(CodeLines& code, const StackPusherHelper& pusher, bool withOptimizations) {
	if (withOptimizations)
		code.append(optimize_code(pusher.code()));
	else
		code.append(pusher.code());
}


CodeLines
TVMContractCompiler::proceedContractMode1(
	ContractDefinition const *contract,
	PragmaDirectiveHelper const &pragmaHelper,
	bool withOptimizations
) {
	TVMCompilerContext ctx{contract, pragmaHelper};
	CodeLines code;

	fillInlineFunctions(ctx, contract);

	// generate global constructor which inlines all contract constructors
	if (!ctx.isStdlib()) {
		StackPusherHelper pusher{&ctx};
		TVMConstructorCompiler compiler(pusher);
		compiler.generateConstructors();
		optimize_and_append_code(code, pusher, withOptimizations);
	}

	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const *_function : c->definedFunctions()) {
			if (_function->isConstructor() ||
				!_function->isImplemented() || isTvmIntrinsic(_function->name()) ||
				_function->isInline()) {
				continue;
			}

			ctx.setCurrentFunction(_function);

			if (_function->isOnBounce()) {
				if (!ctx.isOnBounceGenerated()) {
					ctx.setIsOnBounce();
					StackPusherHelper pusher{&ctx};
					TVMFunctionCompiler::generateOnBounce(pusher, _function);
					optimize_and_append_code(code, pusher, withOptimizations);
				}
			} else if (_function->isReceive()) {
				if (!ctx.isReceiveGenerated()) {
					ctx.setIsReceiveGenerated();
					StackPusherHelper pusher{&ctx};
					TVMFunctionCompiler::generateReceive(pusher, _function);
					optimize_and_append_code(code, pusher, withOptimizations);
				}
			} else if (_function->isFallback()) {
				if (!ctx.isFallBackGenerated()) {
					ctx.setIsFallBackGenerated();
					StackPusherHelper pusher{&ctx};
					TVMFunctionCompiler::generateFallback(pusher, _function);
					optimize_and_append_code(code, pusher, withOptimizations);
				}
			} else if (_function->isOnTickTock()) {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler::generateOnTickTock(pusher, _function);
				optimize_and_append_code(code, pusher, withOptimizations);
			} else if (isMacro(_function->name())) {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler::generateMacro(pusher, _function);
				optimize_and_append_code(code, pusher, withOptimizations);
			} else if (_function->name() == "onCodeUpgrade") {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler::generateOnCodeUpgrade(pusher, _function);
				optimize_and_append_code(code, pusher, withOptimizations);
			} else {
				if (_function->isPublic()) {
					bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
					if (!isBaseMethod) {
						StackPusherHelper pusher{&ctx};
						TVMFunctionCompiler::generatePublicFunction(pusher, _function);
						optimize_and_append_code(code, pusher, withOptimizations);

						EncodeFunctionParams encoder{&pusher};
						uint32_t functionId = encoder.calculateFunctionIDWithReason(_function,
																					ReasonOfOutboundMessage::RemoteCallInternal);
						ctx.addPublicFunction(functionId, _function->name());
					}
				}
				if (_function->visibility() <= Visibility::Public) {
					{
						StackPusherHelper pusher{&ctx};
						TVMFunctionCompiler::generatePrivateFunction(pusher, _function);
						optimize_and_append_code(code, pusher, withOptimizations);
					}
					{
						const std::string macroName = ctx.getFunctionInternalName(_function) + "_macro";
						StackPusherHelper pusher{&ctx};
						TVMFunctionCompiler::generateMacro(pusher, _function, macroName);
						optimize_and_append_code(code, pusher, withOptimizations);
					}
				}
			}
		}
	}

	if (!ctx.isStdlib()) {
		{
			StackPusherHelper pusher{&ctx};
			pusher.generateC7ToT4Macro();
			optimize_and_append_code(code, pusher, withOptimizations);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateC4ToC7(pusher, contract, false);
			optimize_and_append_code(code, pusher, withOptimizations);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateC4ToC7(pusher, contract, true);
			optimize_and_append_code(code, pusher, withOptimizations);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateMainInternal(pusher, contract);
			optimize_and_append_code(code, pusher, withOptimizations);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateMainExternal(pusher, contract);
			optimize_and_append_code(code, pusher, withOptimizations);
		}
	}

	for (VariableDeclaration const* vd : ctx.notConstantStateVariables()) {
		if (vd->isPublic()) {
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateGetter(pusher, vd);
			optimize_and_append_code(code, pusher, withOptimizations);

			EncodeFunctionParams encoder{&pusher};
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

	for (FunctionDefinition const *function : ctx.getLibFunctions()) {
		if (!function->modifiers().empty()) {
			cast_error(*function->modifiers().at(0).get(),
					   "Modifiers for library functions are not supported yet.");
		}

		if (!function->parameters().empty()) {
			{
				StackPusherHelper pusher{&ctx};
				const std::string name = function->annotation().contract->name() + "_with_obj_" + function->name();
				TVMFunctionCompiler::generateLibraryFunction(pusher, function, name);
				optimize_and_append_code(code, pusher, withOptimizations);
			}
			{
				StackPusherHelper pusher{&ctx};
				const std::string name = function->annotation().contract->name() + "_with_obj_" + function->name() + "_macro";
				TVMFunctionCompiler::generateLibraryFunctionMacro(pusher, function, name);
				optimize_and_append_code(code, pusher, withOptimizations);
			}
		}
		StackPusherHelper pusher{&ctx};
		const std::string name = function->annotation().contract->name() + "_no_obj_" + function->name();
		TVMFunctionCompiler::generatePrivateFunction(pusher, function, name);
		TVMFunctionCompiler::generateMacro(pusher, function, name + "_macro");
		optimize_and_append_code(code, pusher, withOptimizations);
	}

	if (!ctx.isStdlib()) {
		StackPusherHelper pusher{&ctx};
		TVMFunctionCompiler::generatePublicFunctionSelector(pusher, contract);
		optimize_and_append_code(code, pusher, withOptimizations);
	}

	return code;
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
		StackPusherHelper pusher{&ctx};
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, function, true);
		const std::string name = functionName(function);
		ctx.addInlineFunction(name, pusher.code());
	}
}

