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

	if (m_pusher.ctx().haveOffChainConstructor()) {
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7");
	} else {
		c4ToC7WithMemoryInitAndConstructorProtection();
	}


	m_pusher.push(+1, ""); // push encoded params of constructor
	std::vector<ContractDefinition const*> linearizedBaseContracts =
			m_pusher.ctx().getContract()->annotation().linearizedBaseContracts; // from derived to base
	for (ContractDefinition const* c : linearizedBaseContracts) {
		dfs(c);
	}

	if (linearizedBaseContracts[0]->constructor() == nullptr) {
		m_pusher.push(-1, "ENDS");
	} else {
		DecodeFunctionParams{&m_pusher}.decodeParameters(linearizedBaseContracts[0]->constructor()->parameters());
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
					TVMExpressionCompiler(m_pusher).acceptExpr2((*m_args[parent])[i].get(), true);
					m_pusher.getStack().add(parent->constructor()->parameters()[i].get(), false);
				}
			}
		}

		TVMFunctionCompiler functionCompiler{m_pusher, false, 0, c->constructor(),
														 m_pusher.getStack().size() - static_cast<int>(c->constructor()->parameters().size())};
		functionCompiler.makeInlineFunctionCall(false);
	}

	if (!haveConstructor) {
		m_pusher.push(0, "ACCEPT");
	}

	solAssert(m_pusher.getStack().size() == 0, "");

	m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
	m_pusher.push(0, "TRUE");
	m_pusher.push(0, "SETGLOB 7");
	m_pusher.push(0, " ");
}

void TVMConstructorCompiler::generateOffChainConstructor() {
	FunctionDefinition const* function = m_pusher.ctx().m_currentFunction;
	if (function->visibility() != Visibility::External) {
		cast_error(*function, "This function must be defined as \"external\".");
	}

	m_pusher.generateInternal("offchainConstructor", 3);

	c4ToC7WithMemoryInitAndConstructorProtection();

	TVMFunctionCompiler functionCompiler(m_pusher, true, 0, function, 0);
	functionCompiler.decodeFunctionParamsAndLocateVars();
	functionCompiler.visitFunctionWithModifiers();

	m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
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
	m_pusher.startContinuation();
	m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");
	m_pusher.endContinuation();
	m_pusher.push(0, "IF");

	// generate constructor protection
	m_pusher.pushLines(R"(
;; constructor protection
GETGLOB 6
THROWIF 51
PUSHINT 1
SETGLOB 6
;; end constructor protection
)");
}

bool TVMContractCompiler::m_optionsEnabled = false;
TvmOption TVMContractCompiler::m_tvmOption = TvmOption::Code;
bool TVMContractCompiler::m_outputProduced = false;
bool TVMContractCompiler::g_without_logstr = false;
bool TVMContractCompiler::g_disable_optimizer = false;
std::string TVMContractCompiler::m_outputWarnings;
std::vector<ContractDefinition const*> TVMContractCompiler::m_allContracts;
std::string TVMContractCompiler::m_mainContractName;
std::string TVMContractCompiler::m_fileName;
std::string TVMContractCompiler::m_outputFolder;
bool TVMContractCompiler::m_outputToFile = false;

void TVMContractCompiler::generateABI(ContractDefinition const *contract,
												  std::vector<PragmaDirective const *> const &pragmaDirectives) {
	m_outputProduced = true;

	if (m_outputToFile) {
		ofstream ofile;
		ensurePathExists();
		ofile.open(m_fileName + ".abi.json");
		if (!ofile)
			fatal_error("Failed to open the output file: " + m_fileName + ".abi.json");
		TVMABI::generateABI(contract, pragmaDirectives, &ofile);
		ofile.close();
		cout << "ABI was generated and saved to file " << m_fileName << ".abi.json" << endl;
	} else {
		TVMABI::generateABI(contract, pragmaDirectives);
	}

}

void TVMContractCompiler::printStorageScheme(int v, const std::vector<StructCompiler::Node> &nodes, const int tabs) {
	for (int i = 0; i < tabs; ++i) {
		std::cout << " ";
	}
	for (const StructCompiler::Field& field : nodes[v].fields) {
		std::cout << " " << field.member->name();
	}
	std::cout << std::endl;

	for (const int to : nodes[v].children) {
		printStorageScheme(to, nodes, tabs + 1);
	}
}

void
TVMContractCompiler::proceedDumpStorage(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper) {
	m_outputProduced = true;

	TVMCompilerContext ctx(contract, pragmaHelper);
	CodeLines code;
	StackPusherHelper pusher{&ctx};
	const std::vector<StructCompiler::Node>& nodes = pusher.structCompiler().getNodes();
	printStorageScheme(0, nodes);

}

void TVMContractCompiler::proceedContract(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper) {
	m_outputProduced = true;
	CodeLines code;
	if (getFunction(contract, "tvm_mode0")) {
		code = proceedContractMode0(contract, pragmaHelper);
	} else {
		code = proceedContractMode1(contract, pragmaHelper);
	}

	if (m_outputToFile) {
		ofstream ofile;
		ensurePathExists();
		ofile.open(m_fileName + ".code");
		if (!ofile)
			fatal_error("Failed to open the output file: " + m_fileName + ".code");
		ofile << code.str();
		ofile.close();
		cout << "Code was generated and saved to file " << m_fileName << ".code" << endl;
	} else {
		cout << code.str();
	}

}

static void optimize_and_append_code(CodeLines& code, const StackPusherHelper& pusher, bool disable_optimizer) {
	if (disable_optimizer)
		code.append(pusher.code());
	else
		code.append(optimize_code(pusher.code()));
}

CodeLines
TVMContractCompiler::proceedContractMode0(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper) {
	CodeLines code;
	for (FunctionDefinition const* _function : getContractFunctions(contract)) {
		TVMCompilerContext ctx(contract, pragmaHelper);
		StackPusherHelper pusher{&ctx};
		TVMFunctionCompiler tvm(pusher, false, 0, _function, 0);
		tvm.generatePrivateFunctionWithoutHeader();
		optimize_and_append_code(code, pusher, g_disable_optimizer);
	}

	return code;
}

CodeLines
TVMContractCompiler::proceedContractMode1(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper) {
	TVMCompilerContext ctx(contract, pragmaHelper);
	CodeLines code;

	fillInlineFunctions(ctx, contract);

	// generate global constructor which inlines all contract constructors
	if (!ctx.isStdlib()) {
		StackPusherHelper pusher{&ctx};
		TVMConstructorCompiler compiler(pusher);
		compiler.generateConstructors();
		optimize_and_append_code(code, pusher, g_disable_optimizer);
	}

	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const* _function : c->definedFunctions()) {
			if (_function->isConstructor() || _function->isReceive() || _function->isFallback() ||
			    _function->isOnBounce() || !_function->isImplemented() || isTvmIntrinsic(_function->name()) ||
			    isFunctionForInlining(_function)) {
				continue;
			}
			ctx.m_currentFunction = _function;
			if (_function->visibility() == Visibility::TvmGetter) {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler tvm(pusher, true, 0, _function, 0);
				tvm.generateTvmGetter(_function);
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			} else if (isMacro(_function->name())) {
				// TODO: These four lines below are copied many times across this file.
				// 		 Would it be possible to shorted it by making a pattern?
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler tvm(pusher, false, 0, _function, 0);
				tvm.generateMacro();
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			} else if (_function->name() == "onCodeUpgrade") {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler tvm(pusher, false, 0, _function, 0);
				tvm.generateOnCodeUpgrade();
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			} else if (_function->name() == "onTickTock") {
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler tvm(pusher, false, 0, _function, 0);
				tvm.generateOnTickTock();
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			} else if (_function->name() == "offchainConstructor") {
				StackPusherHelper pusher{&ctx};
				TVMConstructorCompiler constructorCompiler(pusher);
				constructorCompiler.generateOffChainConstructor();
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			} else {
				if (_function->isPublic()) {
					bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
					if (!isBaseMethod) {
						StackPusherHelper pusher0{&ctx};
						TVMFunctionCompiler tvm0(pusher0, true, 0, _function, 0);
						tvm0.generatePublicFunction();
						optimize_and_append_code(code, pusher0, g_disable_optimizer);
					}
				}
				StackPusherHelper pusher{&ctx};
				TVMFunctionCompiler tvm(pusher, false, 0, _function, 0);
				tvm.generatePrivateFunction();
				optimize_and_append_code(code, pusher, g_disable_optimizer);
			}
		}
	}

	if (!ctx.isStdlib()) {
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler tvm(pusher);
			tvm.generateMainExternal();
			optimize_and_append_code(code, pusher, g_disable_optimizer);
		}
		{
			StackPusherHelper pusher{&ctx};
			pusher.generateC7ToT4Macro();
			optimize_and_append_code(code, pusher, g_disable_optimizer);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler tvm(pusher);
			tvm.generateC4ToC7(false);
			optimize_and_append_code(code, pusher, g_disable_optimizer);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler tvm(pusher);
			tvm.generateC4ToC7(true);
			optimize_and_append_code(code, pusher, g_disable_optimizer);
		}
		{
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler tvm(pusher);
			tvm.generateMainInternal();
			optimize_and_append_code(code, pusher, g_disable_optimizer);
		}
	}

	return code;
}

void TVMContractCompiler::fillInlineFunctions(TVMCompilerContext &ctx, ContractDefinition const *contract) {
	std::map<std::string, FunctionDefinition const*> inlineFunctions;
	TVMInlineFunctionChecker inlineFunctionChecker;
	for (ContractDefinition const* base : contract->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const* function : base->definedFunctions()) {
			if (isFunctionForInlining(function)) {
				inlineFunctions[functionName(function)] = function;
			}
		}
	}
	for (FunctionDefinition const* function : inlineFunctions | boost::adaptors::map_values) {
		function->accept(inlineFunctionChecker);
	}

	std::vector<FunctionDefinition const*> order = inlineFunctionChecker.functionOrder();

	for (const auto& function : order) {
		std::string fname = functionName(function);
		CodeLines code;
		CodeLines codeWithout;
		bool isFunctionForMainInternal =
				function->isReceive() || function->isFallback() || function->isOnBounce();

		if (!function->body().statements().empty() || !function->modifiers().empty()) {
			if (isFunctionForMainInternal) {
				code.append(switchSelectorIfNeed(function));
				if (function->stateMutability() >= StateMutability::NonPayable) {
					code.push("CALL $c4_to_c7$");
					codeWithout.push("CALL $c4_to_c7$");
				}
			}

			ctx.m_currentFunction = function;
			StackPusherHelper pusher{&ctx};
			TVMFunctionCompiler compiler{pusher, false, 0, function, 0};
			compiler.makeInlineFunctionCall(true);
			code.append(pusher.code());
			codeWithout.append(pusher.code());

			if (isFunctionForMainInternal) {
				if (function->stateMutability() >= StateMutability::NonPayable) {
					code.push("CALL $c7_to_c4$");
					codeWithout.push("CALL $c7_to_c4$");
				}
			}
		}

		ctx.m_inlinedFunctions[fname] = code;
		ctx.m_inlinedFunctions[fname + "_without"] = codeWithout;
	}
}

void TVMContractCompiler::ensurePathExists()
{
	if (m_outputFolder.empty())
		return;

	namespace fs = boost::filesystem;
	// create directory if not existent
	fs::path p(m_outputFolder);
	// Do not try creating the directory if the first item is . or ..
	if (p.filename() != "." && p.filename() != "..")
		fs::create_directories(p);
}
