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
	m_pusher.generateGlobl("constructor", true);

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
	m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");
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
	CodeLines code;
	if (getFunction(&contract, "tvm_mode0")) {
		code = proceedContractMode0(&contract, pragmaHelper, withOptimizations);
	} else {
		code = proceedContractMode1(&contract, pragmaHelper, withOptimizations);
	}

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
TVMContractCompiler::proceedContractMode0(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper, bool disable_optimizer) {
	CodeLines code;
	for (FunctionDefinition const* _function : getContractFunctions(contract)) {
		TVMCompilerContext ctx(contract, pragmaHelper);
		StackPusherHelper pusher{&ctx};
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, _function, true);
		optimize_and_append_code(code, pusher, disable_optimizer);
	}
	return code;
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

	bool isFallBackGenerated = false;
	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const* _function : c->definedFunctions()) {
			if (_function->isConstructor() || _function->isReceive() || _function->isOnBounce() ||
				!_function->isImplemented() || isTvmIntrinsic(_function->name()) ||
			    isFunctionForInlining(_function)) {
				continue;
			}

			ctx.setCurrentFunction(_function);
			if (_function->isFallback()) {
				if (!isFallBackGenerated) {
					isFallBackGenerated = true;
					if (!isEmptyFunction(_function)) {
						StackPusherHelper pusher{&ctx};
						TVMFunctionCompiler::generateFallback(pusher, _function);
						optimize_and_append_code(code, pusher, withOptimizations);
					}
				}
			} else if (_function->isOnTickTock()) {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler::generateOnTickTock(pusher, _function);
				optimize_and_append_code(code, pusher, withOptimizations);
			} else if (_function->visibility() == Visibility::TvmGetter) {
				cast_error(*_function, "Not supported yet");
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
					}
				}
				if (_function->visibility() <= Visibility::Public) {
					StackPusherHelper pusher{&ctx};
					TVMFunctionCompiler::generatePrivateFunction(pusher, _function);
					optimize_and_append_code(code, pusher, withOptimizations);
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
		}
	}

	for (FunctionDefinition const *func : ctx.getLibFunctions()) {
		if (!func->modifiers().empty()) {
			cast_error(*func->modifiers().at(0).get(),
					   "Modifiers for library functions are not supported yet.");
		}

		if (!func->parameters().empty()) {
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler::generateLibraryFunction(pusher, func);
			optimize_and_append_code(code, pusher, withOptimizations);
		}
		StackPusherHelper pusher{&ctx};
		const std::string name = func->annotation().contract->name() + "_no_obj_" + func->name();
		TVMFunctionCompiler::generatePrivateFunction(pusher, func, name);
		optimize_and_append_code(code, pusher, withOptimizations);
	}

	return code;
}

void TVMContractCompiler::fillInlineFunctions(TVMCompilerContext &ctx, ContractDefinition const *contract) {
	std::map<std::string, FunctionDefinition const *> inlineFunctions;
	TVMInlineFunctionChecker inlineFunctionChecker;
	for (ContractDefinition const *base : contract->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const *function : base->definedFunctions()) {
			if (isFunctionForInlining(function)) {
				inlineFunctions[functionName(function)] = function;
			}
		}
	}
	for (FunctionDefinition const *function : inlineFunctions | boost::adaptors::map_values) {
		function->accept(inlineFunctionChecker);
	}

	std::vector<FunctionDefinition const *> order = inlineFunctionChecker.functionOrder();

	for (FunctionDefinition const * function : order) {
		StackPusherHelper pusher{&ctx};
		const bool isSpecialFunction = function->isReceive() || function->isFallback() || function->isOnBounce();
		bool doSelectorSwitch{};

		const bool empty = isEmptyFunction(function);

		if (!empty && isSpecialFunction) {
			FunctionUsageScanner scanner{*function};
			doSelectorSwitch = scanner.havePrivateFunctionCall;
			if (function->stateMutability() >= StateMutability::View) {
				doSelectorSwitch = true;
				pusher.pushMacroCallInCallRef(0, "c4_to_c7");
			}
		}

		ctx.setCurrentFunction(function);
		TVMFunctionCompiler::generateFunctionWithModifiers(pusher, function, true);

		if (!empty  && isSpecialFunction) {
			if (function->stateMutability() >= StateMutability::NonPayable) {
				pusher.pushMacroCallInCallRef(0, "c7_to_c4");
			}
		}

		const std::string name = functionName(function);
		if (function->isFallback()) {
			ctx.addInlineFunction(name + "_without_selector_switch", pusher.code());
		}
		CodeLines code;
		if (doSelectorSwitch) {
			StackPusherHelper switcher{&ctx};
			switcher.switchSelector();
			code.append(switcher.code());
		}
		code.append(pusher.code());
		ctx.addInlineFunction(name, code);
	}
}

