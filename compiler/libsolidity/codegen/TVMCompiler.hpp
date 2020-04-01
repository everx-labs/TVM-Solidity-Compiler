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

#pragma once

#include <boost/algorithm/string/replace.hpp>

#include "TVM.h"
#include "TVMCommons.hpp"
#include "TVMABI.hpp"
#include "TVMConstants.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMInlineFunctionChecker.hpp"
#include "TVMABI.hpp"

namespace solidity::frontend {

class TVMCompiler: public ASTConstVisitor, private boost::noncopyable
{
	struct ControlFlowInfo {
		int stackSize {-1};
		bool isLoop {false};
		bool useJmp {false};
	};

	StackPusherHelper m_pusher;
	std::vector<ControlFlowInfo> m_controlFlowInfo;

public:
	static std::vector<ContractDefinition const*> m_allContracts;

	static bool m_optionsEnabled;
	static TvmOption m_tvmOption;
	static bool m_outputProduced;
	static std::string m_outputWarnings;
	static bool g_without_logstr;

public:

	void endContinuation2(const bool doDrop) {
		int delta = m_pusher.getStack().size() - m_controlFlowInfo.back().stackSize;
		if (doDrop) {
			m_pusher.drop(delta);
		} else {
			m_pusher.push(-delta, ""); // fix stack
		}
		m_pusher.endContinuation();
	}

	[[nodiscard]]
	bool allJmp() const {
		return std::all_of(m_controlFlowInfo.begin(), m_controlFlowInfo.end(), [](const ControlFlowInfo& info){
			return info.useJmp;
		});
	}

	explicit TVMCompiler(const TVMCompilerContext* ctx) : m_pusher{ctx} {

	}

public:
	static void generateABI(ContractDefinition const* contract,
							std::vector<PragmaDirective const *> const& pragmaDirectives) {
		m_outputProduced = true;

		TVMABI::generateABI(contract, m_allContracts, pragmaDirectives);
	}

	static void printStorageScheme(int v, const std::vector<StructCompiler::Node>& nodes, const int tabs = 0) {
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

	static void proceedDumpStorage(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper) {
		m_outputProduced = true;

		TVMCompilerContext ctx(contract, m_allContracts, pragmaHelper);
		CodeLines code;
		TVMCompiler tvm(&ctx);
		const std::vector<StructCompiler::Node>& nodes = tvm.m_pusher.structCompiler().getNodes();
		printStorageScheme(0, nodes);
	}

	static void proceedContract(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper) {
		m_outputProduced = true;
		if (getFunction(contract, "tvm_mode0")) {
			proceedContractMode0(contract, pragmaHelper);
		} else {
			proceedContractMode1(contract, pragmaHelper);
		}
	}

	static void proceedContractMode0(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper) {
		solAssert(getContractFunctions(contract).size() == 1, "");
		CodeLines code;
		for (const auto& _function : getContractFunctions(contract)) {
			TVMCompilerContext ctx(contract, m_allContracts, pragmaHelper);
			TVMCompiler tvm(&ctx);
			tvm.visitFunction(*_function);
			tvm.m_pusher.push(0, " ");
			code.append(tvm.m_pusher.code());
		}
		cout << code.str("");
	}

	static bool notNeedsPushContWhenInlining(FunctionDefinition const* _function) {

		std::vector<ASTPointer<Statement>> statements = _function->body().statements();

		TVMScanner bodyScanner{_function->body()};
		if (!bodyScanner.m_info.canReturn) {
			return true;
		}

		for (std::vector<int>::size_type i = 0; i + 1 < statements.size(); ++i) {
			TVMScanner scanner{*statements[i].get()};
			if (scanner.m_info.canReturn) {
				return false;
			}
		}
		bool isLastStatementReturn = to<Return>(statements.back().get()) != nullptr;
		return isLastStatementReturn;
	}

	static CodeLines makeInlineFunctionCall(const TVMCompilerContext& ctx, FunctionDefinition const* _function,
			set<const ContractDefinition *> * calledConstructors) {
		TVMCompiler tvm(&ctx);
		if (_function->isConstructor())
			tvm.visitConstructor(*_function, calledConstructors);
		else
			tvm.visitFunction(*_function);
		bool doInline = notNeedsPushContWhenInlining(_function);

		StackPusherHelper sp{&ctx};
		CodeLines curCode = tvm.m_pusher.code();
		if (doInline) {
			if (!curCode.lines.empty() && curCode.lines.back() == "RET") {
				curCode.lines.pop_back();
			}
			const std::string functionName = _function->isConstructor()? "constr" : _function->name();
			sp.push(0, "; " + functionName);
			sp.append(curCode);
			sp.push(0, "; end " + functionName);
		} else {
			sp.pushCont(curCode, _function->name());
			sp.push(0, "CALLX");
		}
		return sp.code();
	}


	static void fillInlinedFunctions(TVMCompilerContext& ctx, ContractDefinition const* contract) {
		TVMInlineFunctionChecker inlineFunctionChecker;
		for (FunctionDefinition const* _function : getContractFunctions(contract)) {
			if (isFunctionForInlining(_function)) {
				_function->accept(inlineFunctionChecker);
			}
		}
		std::vector<FunctionDefinition const*> order = inlineFunctionChecker.functionOrder();

		for (const auto& _function : order) {
			std::string fname = functionName(_function);
			CodeLines code;
			CodeLines codeWithout;
			bool isFunctionForMainInternal =
					_function->isReceive() || _function->isFallback() || _function->name() == "onBounce";

			if (!_function->body().statements().empty() || !_function->modifiers().empty()) {
				if (isFunctionForMainInternal) {
					code.append(switchSelectorIfNeed(_function));
					if (_function->stateMutability() >= StateMutability::NonPayable) {
						code.push("CALL $c4_to_c7$");
						codeWithout.push("CALL $c4_to_c7$");
					}
				}

				// TODO inline modifiers
				if (!_function->modifiers().empty()) {
					cast_error(*_function, "Modifiers are not supported for inline functions");
				}

				code.append(makeInlineFunctionCall(ctx, _function, nullptr));
				codeWithout.append(makeInlineFunctionCall(ctx, _function, nullptr));
				if (isFunctionForMainInternal) {
					if (_function->stateMutability() >= StateMutability::NonPayable) {
						code.push("CALL $c7_to_c4$");
						codeWithout.push("CALL $c7_to_c4$");
					}
				}
			}

			ctx.m_inlinedFunctions[fname] = code;
			ctx.m_inlinedFunctions[fname + "_without"] = codeWithout;
		}
	}

	static void proceedContractMode1(ContractDefinition const* contract,
	                                 PragmaDirectiveHelper const& pragmaHelper) {
		TVMCompilerContext ctx(contract, m_allContracts, pragmaHelper);
		CodeLines code;

		fillInlinedFunctions(ctx, contract);

		// generate global constructor which inlines all contract constructors
		if (!ctx.isStdlib()) {
			TVMCompiler tvm(&ctx);
			tvm.generateConstructors();
			code.append(tvm.m_pusher.code());
		}

		for (const auto& funcInfo : ctx.m_functionsList) {
			FunctionDefinition const* _function = funcInfo.m_function;
			ctx.m_currentFunction = &funcInfo;
			solAssert(!ctx.isPureFunction(_function), "");
			if (_function->isConstructor() || _function->isReceive() || _function->isFallback() ||
				_function->name() == "onBounce") {
				continue;
			} else if (_function->visibility() == Visibility::TvmGetter) {
				TVMCompiler tvm(&ctx);
				tvm.m_pusher.generateGlobl(_function->name(), true);
				tvm.visitFunction(*_function);
				tvm.m_pusher.push(0, " ");
				code.append(tvm.m_pusher.code());
			} else if (isMacro(funcInfo.m_internalName)) {
				TVMCompiler tvm(&ctx);
				tvm.m_pusher.generateMacro(funcInfo.m_internalName);
				tvm.m_pusher.append(makeInlineFunctionCall(ctx, _function, nullptr));
				tvm.m_pusher.push(0, " ");
				code.append(tvm.m_pusher.code());
			} else {
				if (_function->isPublic()) {
					TVMCompiler tvm0(&ctx);
					TVMCompiler tvm1(&ctx);
					bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
					if (!isBaseMethod) {
						tvm0.generatePublicFunction(funcInfo);
					}
					tvm1.generatePrivateFunction(funcInfo);
					code.append(tvm0.m_pusher.code());
					code.append(tvm1.m_pusher.code());
				} else {
					TVMCompiler tvm(&ctx);
					tvm.generatePrivateFunction(funcInfo);
					code.append(tvm.m_pusher.code());
				}
			}
		}
		for (const auto& fname : ctx.m_remoteFunctions) {
			StackPusherHelper pusher{&ctx};
			pusher.generateGlobl(TVMCompilerContext::getFunctionExternalName(fname), false);
			pusher.push(0, " ");
			code.append(pusher.code());
		}
		for (auto event : ctx.events()) {
			StackPusherHelper pusher{&ctx};
			const string& ename = event->name();
			pusher.generateGlobl(ename, false);
			pusher.push(0, " ");
			code.append(pusher.code());
		}

		if (!ctx.isStdlib()) {
			{
				TVMCompiler tvm(&ctx);
				tvm.generateMainExternal();
				code.append(tvm.m_pusher.code());
				code.push(" ");
			}
			{
				TVMCompiler tvm(&ctx);
				tvm.m_pusher.generateC7ToT4Macro();
				code.append(tvm.m_pusher.code());
				code.push(" ");
			}
			{
				TVMCompiler tvm(&ctx);
				tvm.generateC4ToC7(false);
				code.append(tvm.m_pusher.code());
				code.push(" ");
			}
			{
				TVMCompiler tvm(&ctx);
				tvm.generateC4ToC7(true);
				code.append(tvm.m_pusher.code());
				code.push(" ");
			}
			{
				TVMCompiler tvm(&ctx);
				tvm.generateMainInternal();
				code.append(tvm.m_pusher.code());
				code.push(" ");
			}
		}


		cout << code.str("");
	}

	CodeLines loadFromC4() {
		StackPusherHelper pusherHelper(&m_pusher.ctx());


		pusherHelper.pushLines(R"(LDU 256      ; pubkey c4)");
		if (m_pusher.ctx().storeTimestampInC4()) {
			pusherHelper.pushLines(R"(LDU 64      ; pubkey timestamp c4)");
		}
		pusherHelper.pushLines(R"(
LDU 1       ; pubkey [timestamp] constructor_flag memory
)");
		if (!pusherHelper.ctx().notConstantStateVariables().empty()) {
			pusherHelper.structCompiler().sliceToStateVarsToC7();
		} else {
			pusherHelper.push(0, "ENDS");
		}
		pusherHelper.pushLines(R"(
TRUE
SETGLOB 1   ; pubkey [timestamp] constructor_flag
SETGLOB 6   ; pubkey [timestamp]
)");
		if (m_pusher.ctx().storeTimestampInC4()) {
			pusherHelper.pushLines(R"(SETGLOB 3   ; D)");
		}

		pusherHelper.pushLines(R"(SETGLOB 2)");
	return pusherHelper.code();
}

	void generateC4ToC7(bool withInitMemory) {
		m_pusher.pushLines(std::string{".macro\t"} + (withInitMemory? "c4_to_c7_with_init_storage": "c4_to_c7"));
		m_pusher.pushLines(R"(
PUSHROOT
CTOS        ; c4
)");
		if (withInitMemory) {
			m_pusher.pushLines(R"(
DUP        ; c4 c4
SBITS      ; c4 bits
GTINT 1    ; c4 bits>1
)");
		}

		if (!withInitMemory) {
			m_pusher.append(loadFromC4());
		} else {
			m_pusher.pushCont(loadFromC4());
			m_pusher.pushLines(R"(
PUSHCONT {
	PLDDICT   ; D
)");
			m_pusher.addTabs();
			int shift = 0;
			for (VariableDeclaration const* v : m_pusher.ctx().getContract()->stateVariablesIncludingInherited()) {
				if (v->isConstant()) {
					continue;
				}
				m_pusher.push(0, "; init " + v->name());
				if (v->isPublic()) {
					m_pusher.pushInt(TvmConst::C4::PersistenceMembersStartIndex + shift++); // index
					m_pusher.pushS(1); // index dict
					m_pusher.getFromDict(getKeyTypeOfC4(), *v->type(), *v, StackPusherHelper::DictOperation::GetFromMapping,
					                         false);
				} else {
					m_pusher.pushDefaultValue(v->type());
				}
				m_pusher.setGlob(v);
			}
			m_pusher.subTabs();
			m_pusher.pushLines(R"(
	; set contract pubkey
	PUSHINT 0
	SWAP
	PUSHINT 64
	DICTUGET
	THROWIFNOT 61
	PLDU 256
	SETGLOB 2

	PUSHINT 0 ; timestamp
	SETGLOB 3
	PUSHINT 0 ; constructor_flag
	SETGLOB 6
)");
			m_pusher.pushLines(R"(
	TRUE
	SETGLOB 1
)");
			m_pusher.addTabs();
			for (VariableDeclaration const *variable: m_pusher.ctx().notConstantStateVariables()) {
				if (auto value = variable->value().get()) {
					m_pusher.push(0, ";; init state var: " + variable->name());
					acceptExpr(value);
					m_pusher.setGlob(variable);
				}
			}
			m_pusher.subTabs();
			m_pusher.pushLines(R"(
}
IFELSE
)");
		}
	}

	void generateConstructors() {
		m_pusher.generateGlobl("constructor", true);
		// copy c4 to c7
		m_pusher.pushLines(R"(
GETGLOB 1
ISNULL
)");
		m_pusher.startContinuation();
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");

		m_pusher.endContinuation();
		m_pusher.pushLines(R"(
IF
)");
		// generate constructor protection
		m_pusher.pushLines(R"(
;; constructor protection
GETGLOB 6
PUSHCONT {
	THROW 51
}
PUSHCONT {
	PUSHINT 1
	SETGLOB 6
}
IFELSE
;; end constructor protection
)");

		m_pusher.push(0, "ACCEPT");

		bool newChain = true;
		deque<const ContractDefinition * > constructorHeads;
		std::vector<ContractDefinition const*> mainChain = getContractsChain(m_pusher.ctx().getContract());
		for (auto c = mainChain.rbegin(); c != mainChain.rend(); c++) {
			if ((*c)->constructor()) {
				if (newChain) {
					constructorHeads.push_front(*c);
					newChain = false;
				}
			} else
				newChain = true;
		}

		set<const ContractDefinition *> calledConstructors;
		for (auto c : constructorHeads) {
			if (calledConstructors.count(c) != 0)
				continue;
			m_pusher.push(0, "; call " + c->name() + " constr");
			if (m_pusher.ctx().getContract() == c)
				DecodeFunctionParams{&m_pusher}.decodeParameters(c->constructor()->parameters());
			calledConstructors.insert(c);
			m_pusher.append(makeInlineFunctionCall(m_pusher.ctx(), c->constructor(), &calledConstructors));
		}

		m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
		m_pusher.push(0, "PUSHINT 0");
		m_pusher.push(0, "ONLYTOPX");
		m_pusher.push(0, "TRUE");
		m_pusher.push(0, "SETGLOB 7");
		m_pusher.push(0, " ");
	}

	void generateModifier(FunctionDefinition const* _function) {
		for (const auto& m : _function->modifiers()) {
			string name = m->name()->name();
			if (m_pusher.ctx().isContractName(name))
				continue;
			if (m->arguments() != nullptr) {
				cast_error(*m, "Modifiers with parameters are not supported.");
			}
			m_pusher.push(0, "; Modifier " + name);
			auto chain = getContractsChain(m_pusher.ctx().getContract());
			for (auto contract = chain.rbegin(); contract != chain.rend(); contract++) {
				bool isModifierFound = false;
				for (const ModifierDefinition *modifier : (*contract)->functionModifiers()) {
					if (modifier->name() != name) {
						continue;
					}
					if (!to<PlaceholderStatement>(modifier->body().statements().back().get())) {
						cast_error(*modifier, R"(Code after "_;" is not supported")");
					}

					TVMCompiler tvmCompiler{&m_pusher.ctx()};
					const int saveStackSize = tvmCompiler.m_pusher.getStack().size();
					modifier->body().accept(tvmCompiler);
					if (!doesAlways<Return>(&modifier->body())) {
						tvmCompiler.m_pusher.drop(tvmCompiler.m_pusher.getStack().size() - saveStackSize);
					}

					if (TVMScanner{modifier->body()}.m_info.canReturn) {
						m_pusher.pushCont(tvmCompiler.m_pusher.code());
						m_pusher.push(-1, "CALLX");
					} else {
						m_pusher.append(tvmCompiler.m_pusher.code());
					}
					isModifierFound = true;
					break;
				}
				if (isModifierFound) {
					break;
				}
			}
		}
	}

	static CodeLines switchSelectorIfNeed(FunctionDefinition const* f) {
		TVMScanner scanner{*f};
		CodeLines code;
		if (scanner.havePrivateFunctionCall) {
			code.push("PUSHINT 1");
			code.push("CALL 1");
		}
		return code;
	}

	void generatePublicFunction(const FuncInfo& fi) {
		bool dontEmitReturn = !!getFunction(m_pusher.ctx().getContract(), "tvm_dont_emit_events_on_return");
		FunctionDefinition const* _function = fi.m_function;
		auto fnameInternal = fi.m_internalName;
		auto fnameExternal = TVMCompilerContext::getFunctionExternalName(_function);

		// stack: transaction_id function-argument-slice

		// generate header
		m_pusher.generateGlobl(fnameExternal, _function->isPublic());

		if (_function->stateMutability() != StateMutability::Pure) {
			m_pusher.pushLines(R"(
GETGLOB 1
ISNULL
)");
			m_pusher.startContinuation();
			m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7");
			m_pusher.endContinuation();
			m_pusher.pushLines(R"(
IF
)");
		}
		generateModifier(_function);
		DecodeFunctionParams{&m_pusher}.decodeParameters(_function->parameters());

		// function body
		CodeLines res = makeInlineFunctionCall(m_pusher.ctx(), _function, nullptr);
		m_pusher.append(res);

		// emit function result
		if (!dontEmitReturn) {
			emitOnPublicFunctionReturn(_function);
		}

		const int retQty = fi.m_function->returnParameters().size();
		// stack: transaction_id return-params...
		if (_function->stateMutability() == StateMutability::NonPayable ||
			_function->stateMutability() == StateMutability::Payable) {
			m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
		} else {
			// to save replay protection
			m_pusher.pushS(retQty);
			m_pusher.push(0, "EQINT -1"); // is it ext msg?
			m_pusher.startContinuation();
			m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
			m_pusher.endContinuation();
			m_pusher.push(0, "IF");
		}

		// drop not needed variables from stack
		m_pusher.pushInt(retQty);
		m_pusher.push(-1, "ONLYTOPX");
		m_pusher.push(0, "TRUE");
		m_pusher.push(0, "SETGLOB 7");
		m_pusher.push(0, " ");
	}

	void generatePrivateFunction(const FuncInfo& fi) {
		FunctionDefinition const *_function = fi.m_function;
		auto fnameInternal = fi.m_internalName;
		if (_function->name() == "onTickTock") {
			m_pusher.generateInternal("onTickTock", -2);
			m_pusher.push(0, "PUSHINT -2");
			m_pusher.push(0, "PUSHINT -2");
			m_pusher.push(0, "PUSH s2");
			if (fi.m_function->stateMutability() != StateMutability::Pure) {
				m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7");
			}
		} else if (_function->name() == "onCodeUpgrade") {
			m_pusher.generateInternal("onCodeUpgrade", 2);
			m_pusher.append(switchSelectorIfNeed(_function));
		} else {
			m_pusher.generateGlobl(fnameInternal, false);
		}

		if (_function->name() == "onCodeUpgrade") {

			CodeLines code = makeInlineFunctionCall(m_pusher.ctx(), _function, nullptr);
			m_pusher.append(code);

			m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
			m_pusher.push(0, "COMMIT");
			m_pusher.push(0, "THROW 0");
		} else {
			visitFunction(*_function);
		}

		m_pusher.push(0, " ");
	}

	void emitOnPublicFunctionReturn(FunctionDefinition const* _function) {
		const auto& params = _function->returnParameters();
		auto count = params.size();
		if (count == 0) {
			return;
		}
		m_pusher.push( 0, ";; emitting " + toString(count) + " value(s)");

		std::vector<Type const*> types;
		std::vector<ASTNode const*> nodes;
		for (const auto & param : params) {
			types.push_back(param->annotation().type);
			nodes.push_back(param.get());
		}

		int prevStackSize = m_pusher.getStack().size();
		m_pusher.encodeFunctionAndParams(
				TVMCompilerContext::getFunctionExternalName(_function),
				types,
				nodes,
				[&](size_t idx) {
					int pos = (m_pusher.getStack().size() - prevStackSize) +
							(static_cast<int>(params.size()) - static_cast<int>(idx) - 1);
					m_pusher.pushS(pos);
				},
				StackPusherHelper::ReasonOfOutboundMessage::FunctionReturnExternal
		);
		sendExternalMessage();
	}

	void sendExternalMessage(bool addrIsArgument = false) {
		// stack: builder with encoded params
		if (m_pusher.ctx().haveSetDestAddr()) {
			if (!addrIsArgument)
				m_pusher.push(+1, "GETGLOB " + toString(TvmConst::C7::ExtDestAddrIndex));
			m_pusher.pushLines(R"(DUP
ISNULL
PUSHCONT {
	DROP
	PUSHSLICE x2_
}
IF
)");
			m_pusher.exchange(0, 1);
			m_pusher.pushPrivateFunctionOrMacroCall(-2, "send_external_message_with_dest_macro");
		} else {
			m_pusher.pushPrivateFunctionOrMacroCall(-1, "send_external_message_macro");
		}
	}

	void visitFunction(FunctionDefinition const& _function) {
		int savedStackSize = m_pusher.getStack().size();

		for (const auto& m : _function.modifiers()) {
			string name = m->name()->name();
			if (!_function.isPublic())
				cast_error(*m, "Modifiers are allowed only for public functions.");
		}

		for (const auto& variable: _function.parameters()) {
			auto name = variable->name();
			m_pusher.push(0, string(";; param: ") + name);
			m_pusher.getStack().add(name, true);
		}

		bool haveSomeNamedReturnParams = false;
		for (const auto& returnParam: _function.returnParameters()) {
			haveSomeNamedReturnParams |= !returnParam->name().empty();
		}
		if (haveSomeNamedReturnParams) {
			pushReturnParameters(_function.returnParameters());
		}

		_function.body().accept(*this);

		bool doFunctionAlwaysReturn = doesAlways<Return>(&_function.body());
		if (!doFunctionAlwaysReturn && haveSomeNamedReturnParams) {
			int paramQty = _function.parameters().size();
			int retQty = _function.returnParameters().size();
			m_pusher.push(0, ";; returning named params");
			int m = m_pusher.getStack().size() - savedStackSize - paramQty;
			m_pusher.blockSwap(paramQty, m);
			m_pusher.drop(m_pusher.getStack().size() - savedStackSize - retQty);
		} else if (!doFunctionAlwaysReturn) {
			// if function did not return
			m_pusher.drop(m_pusher.getStack().size() - savedStackSize);
			pushReturnParameters(_function.returnParameters());
		}

		if (_function.name() == "onTickTock") {
			if (_function.stateMutability() != StateMutability::Pure &&
			    _function.stateMutability() != StateMutability::View) {
				m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
			}
		}
	}

	void visitConstructor(FunctionDefinition const& _function, set<const ContractDefinition *> * calledConstructors) {
		for (const auto& m : _function.modifiers()) {
			string name = m->name()->name();
			if (m_pusher.ctx().isContractName(name)) {
				// constructor call is processed in another place
				continue;
			}
			cast_error(*m, "Modifiers for constructors are not supported.");
		}
		for (const auto& variable: _function.parameters()) {
			auto name = variable->name();
			m_pusher.push(0, string(";; param: ") + name);
			m_pusher.getStack().add(name, true);
		}
		callBaseConstructorsExplicit(&_function, calledConstructors);
		_function.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size());
	}

	void pushReturnParameters(const std::vector<ASTPointer<VariableDeclaration>>& returnParameters) {
		int idParam = 0;
		for (const auto& returnParam: returnParameters) {
			auto name = returnParam->name();
			if (name.empty()) {
				name = "retParam@" + std::to_string(idParam);
			}
			m_pusher.push(0, string(";; ret param: ") + name);
			m_pusher.pushDefaultValue(returnParam->type());
			m_pusher.getStack().add(name, false);

			++idParam;
		}
	}

	void callBaseConstructorsExplicit(const FunctionDefinition* function, set<const ContractDefinition *> * calledConstructors) {
		auto contract0 = m_pusher.ctx().getContract(function);
		auto bases = contract0->baseContracts();
		auto chain = getContractsChain(m_pusher.ctx().getContract());
		auto prev = chain.rbegin();
		for (auto c = chain.rbegin(); c != chain.rend(); c++)
		{
			if (*c == contract0)
				break;
			prev = c;
		}
		if (prev != chain.rend())
			if (!(*prev)->constructor())
			{
				auto deriv_bases = (*prev)->baseContracts();
				for (const auto& base: deriv_bases)
				{
					if (base->name().namePath()[0] == contract0->name())
					{
						if (base->arguments()) {
							for (const auto& expr : *base->arguments()) {
								acceptExpr(expr.get());
								m_pusher.push(-1, "");	// fix stack
							}
						}
					}
				}
			}
		for (auto contract : getContractsChain(contract0)) {
			if (find_if(bases.begin(),bases.end(),[&contract](auto base) {
				return base->name().namePath()[0] == contract->name();
			}) == bases.end())
				continue;
			if (contract->constructor()) {
				if (calledConstructors->count(contract) != 0)
					continue;
				for (const auto& m : function->modifiers()) {
					if (m->name()->name() == contract->name()) {
						if (m->arguments()) {
							for (const auto& expr : *m->arguments()) {
								acceptExpr(expr.get());
							}
							m_pusher.push(-(int)(m->arguments()->size()), "");	// fix stack
						}
					}
				}
				for (const auto& base: bases) {
					if (base->name().namePath()[0] == contract->name()) {
						if (base->arguments()) {
							for (const auto& expr : *base->arguments()) {
								acceptExpr(expr.get());
								m_pusher.push(-1, "");	// fix stack
							}
						}
					}
				}
				m_pusher.push(0, "; call " + contract->name() + " constr");
				calledConstructors->insert(contract);
				m_pusher.pushLines(makeInlineFunctionCall(m_pusher.ctx(), contract->constructor(), calledConstructors).str("\n"));
			}
		}
	}
	void acceptExpr(const Expression* expr, const bool isResultNeeded = true) {
		solAssert(expr, "");
		TVMExpressionCompiler(m_pusher).acceptExpr2(expr, isResultNeeded);
	}

	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override {
		const int saveStackSize = m_pusher.getStack().size();

		auto decls = _variableDeclarationStatement.declarations();
		if (auto init = _variableDeclarationStatement.initialValue()) {
			auto tupleExpression = to<TupleExpression>(init);
			if (tupleExpression && !tupleExpression->isInlineArray()) {
				std::vector<ASTPointer<Expression>> const&  tuple = tupleExpression->components();
				for (std::size_t i = 0; i < tuple.size(); ++i) {
					if (!m_pusher.tryImplicitConvert(decls[i]->type(), tuple[i]->annotation().type)) {
						acceptExpr(tuple[i].get());
					}
				}
			} else if (decls[0] == nullptr || !m_pusher.tryImplicitConvert(decls[0]->type(), init->annotation().type)) {
				acceptExpr(init);
			}
		} else {
			for (const auto& decl : decls) {
				m_pusher.pushDefaultValue(decl->type());
			}
		}

		m_pusher.getStack().change(-(int)decls.size());
		int cntVars = 0;
		for (size_t i = 0; i < decls.size(); i++) {
			if (decls[i]) {
				m_pusher.push(0, string(";; decl: ") + decls[i]->name());
				m_pusher.getStack().add(decls[i]->name(), true);
				++cntVars;
			} else {
				if (i == decls.size() - 1) {
					m_pusher.push(0, "DROP");
				} else if (i == decls.size() - 2) {
					m_pusher.push(0, "NIP");
				} else {
					m_pusher.blockSwap(1, static_cast<int>(decls.size()) - 1 - i);
					m_pusher.push(0, "DROP");
				}
			}
		}
		m_pusher.getStack().ensureSize(saveStackSize + cntVars, "VariableDeclarationStatement");
		return false;
	}

	bool visit(Block const& /*_block*/) override {
		// TODO: write the code explicitly
		return true;
	}

	bool visit(ExpressionStatement const& _expressionStatement) override {
		auto savedStackSize = m_pusher.getStack().size();
		acceptExpr(&_expressionStatement.expression(), false);
		m_pusher.getStack().ensureSize(savedStackSize, ASTNode2String(_expressionStatement));
		return false;
	}

	bool visit(IfStatement const& _ifStatement) override {
		const int saveStackSize = m_pusher.getStack().size();

		m_pusher.push(0, ";; if");


		// header
		ContInfo ci = getInfo(_ifStatement);
		bool canUseJmp = _ifStatement.falseStatement() != nullptr?
			getInfo(_ifStatement.trueStatement()).doThatAlways() && getInfo(*_ifStatement.falseStatement()).doThatAlways() :
			getInfo(_ifStatement.trueStatement()).doThatAlways();
		if (canUseJmp) {
			ControlFlowInfo info {};
			info.stackSize = m_pusher.getStack().size();
			info.isLoop = false;
			info.useJmp = true;
			m_controlFlowInfo.push_back(info);
		} else {
			ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, false);
			m_controlFlowInfo.push_back(info);
		}

		// condition
		acceptExpr(&_ifStatement.condition(), true);
		m_pusher.push(-1, ""); // drop condition

		// if
		m_pusher.startContinuation();
		_ifStatement.trueStatement().accept(*this);
		endContinuation2(!canUseJmp);


		if (_ifStatement.falseStatement() != nullptr) {
			// else
			m_pusher.startContinuation();
			_ifStatement.falseStatement()->accept(*this);
			endContinuation2(!canUseJmp);

			if (canUseJmp) {
				m_pusher.push(0, "CONDSEL");
				m_pusher.push(0, "JMPX");
			} else {
				m_pusher.push(0, "IFELSE");
			}
		} else {
			if (canUseJmp) {
				m_pusher.push(0, "IFJMP");
			} else {
				m_pusher.push(0, "IF");
			}
		}

		m_controlFlowInfo.pop_back();

		if (!canUseJmp) {
			// bottom
			if (ci.canReturn || ci.canBreak || ci.canContinue) {
				if (ci.canReturn) {
					if (allJmp()) { // no loops, only if-else
						m_pusher.push(0, "EQINT 4");
						m_pusher.push(-1, "IFRET");
					} else {
						m_pusher.push(+1, "DUP");
						m_pusher.push(-1, "IFRET");
						m_pusher.push(-1, "DROP");
					}
				} else {
					m_pusher.push(+1, "DUP");
					m_pusher.push(-1, "IFRET");
					m_pusher.push(-1, "DROP");
				}
			}
		}

		m_pusher.push(0, ";; end if");

		m_pusher.getStack().ensureSize(saveStackSize, "");

		return false;
	}

	ControlFlowInfo pushControlFlowFlagAndReturnControlFlowInfo(ContInfo &ci, bool isLoop) {
		ControlFlowInfo info {};
		info.isLoop = isLoop;
		info.stackSize = -1;
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			m_pusher.push(+1, "FALSE ; decl return flag"); // break flag
		}
		info.stackSize = m_pusher.getStack().size();
		return info;
	}

	void doWhile(WhileStatement const& _whileStatement) {
		int saveStackSize = m_pusher.getStack().size();

		// header
		m_pusher.push(0, "; do-while");
		ContInfo ci = getInfo(_whileStatement.body());
		ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
		m_controlFlowInfo.push_back(info);

		// body
		m_pusher.startContinuation();
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			int ss = m_pusher.getStack().size();
			m_pusher.startContinuation();
			_whileStatement.body().accept(*this);
			m_pusher.drop(m_pusher.getStack().size() - ss);
			m_pusher.endContinuation();
			m_pusher.push(0, "CALLX");
		} else {
			int ss = m_pusher.getStack().size();
			_whileStatement.body().accept(*this);
			m_pusher.drop(m_pusher.getStack().size() - ss);
		}
		// condition
		m_pusher.push(0, "; condition");
		if (ci.canBreak || ci.canReturn) {
			m_pusher.push(+1, "DUP");
			m_pusher.push(0, "GTINT 1");
			m_pusher.push(+1, "DUP");
			m_pusher.push(-2, ""); // fix stack

			m_pusher.startContinuation();
			m_pusher.push(0, "DROP");
			acceptExpr(&_whileStatement.condition(), true);
			m_pusher.push(0, "NOT");
			m_pusher.endContinuation();
			m_pusher.push(-1, ""); // fix stack

			m_pusher.push(+1, "IFNOT");
		} else {
			acceptExpr(&_whileStatement.condition(), true);
			m_pusher.push(0, "NOT");
		}
		m_pusher.push(-1, ""); // drop condition
		m_pusher.endContinuation();

		m_pusher.push(0, "UNTIL");

		m_controlFlowInfo.pop_back();

		// bottom
		if (ci.canReturn) {
			if (allJmp()) { // no loops, only if-else
				m_pusher.push(0, "EQINT 4");
				m_pusher.push(-1, "IFRET");
			} else {
				m_pusher.push(+1, "DUP");
				if (ci.canBreak || ci.canContinue) {
					m_pusher.push(0, "EQINT 4");
				}
				m_pusher.push(-1, "IFRET");
				m_pusher.push(-1, "DROP");
			}
		} else if (ci.canBreak || ci.canContinue) {
			m_pusher.drop(1);
		}

		m_pusher.push(0, "; end do-while");

		m_pusher.getStack().ensureSize(saveStackSize, "");
	}

	void visitForOrWhileCondiction(const ContInfo& ci, const ControlFlowInfo& info, Expression const* condition) {
		int stackSize = m_pusher.getStack().size();
		m_pusher.startContinuation();
		if (ci.canBreak || ci.canReturn) {
			m_pusher.pushS(m_pusher.getStack().size() - info.stackSize);
			m_pusher.push(0, "LESSINT 2");
			m_pusher.push(-1, ""); // fix stack

			if (condition != nullptr) {
				m_pusher.push(0, "DUP");
				m_pusher.startContinuation();
				m_pusher.push(0, "DROP");
				acceptExpr(condition, true);
				m_pusher.endContinuation();
				m_pusher.push(-1, ""); // fix stack
				m_pusher.push(0, "IF");
			}
		} else {
			acceptExpr(condition, true);
			m_pusher.push(-1, ""); // fix stack
		}
		m_pusher.endContinuation();
		m_pusher.getStack().ensureSize(stackSize, "visitForOrWhileCondiction");
	}

	bool visit(WhileStatement const& _whileStatement) override {
		int saveStackSizeForWhile = m_pusher.getStack().size();

		if (_whileStatement.isDoWhile()) {
			doWhile(_whileStatement);
			return false;
		}

		// header
		m_pusher.push(0, "; while");
		ContInfo ci = getInfo(_whileStatement.body());
		ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
		m_controlFlowInfo.push_back(info);

		int saveStackSize = m_pusher.getStack().size();

		// condition
		visitForOrWhileCondiction(ci, info, &_whileStatement.condition());

		m_pusher.getStack().ensureSize(saveStackSize, "while condition");

		// body
		m_pusher.startContinuation();
		_whileStatement.body().accept(*this);
		m_pusher.drop(m_pusher.getStack().size() - saveStackSize);
		m_pusher.endContinuation();

		m_pusher.push(0, "WHILE");

		m_controlFlowInfo.pop_back();

		// bottom
		// TODO to common function
		if (ci.canReturn) {
			if (allJmp()) { // no loops, only if-else
				m_pusher.push(0, "EQINT 4");
				m_pusher.push(-1, "IFRET");
			} else {
				m_pusher.push(+1, "DUP");
				if (ci.canBreak || ci.canContinue) {
					m_pusher.push(0, "EQINT 4");
				}
				m_pusher.push(-1, "IFRET");
				m_pusher.push(-1, "DROP");
			}
		} else if (ci.canBreak || ci.canContinue) {
			m_pusher.drop(1);
		}

		m_pusher.push(0, "; end while");

		m_pusher.getStack().ensureSize(saveStackSizeForWhile, "");

		return false;
	}

	bool visit(ForStatement const& _forStatement) override {

		// init - opt
		// return break or continue flag  - opt
		// PUSHCONT {
		//     condition
		// }
		// PUSHCONT {
		//     PUSHCONT {
		//        body
		//     }
		//     CALLX
		//     loopExpression
		// }

		// init - opt
		// PUSHCONT {
		//     condition
		// }
		// PUSHCONT {
		//     body
		//     loopExpression
		// }

		int saveStackSize = m_pusher.getStack().size();
		m_pusher.push(0, "; for");

		// init
		if (_forStatement.initializationExpression() != nullptr) {
			_forStatement.initializationExpression()->accept(*this);
		}

		// header
		ContInfo ci = getInfo(_forStatement.body());
		ControlFlowInfo info = pushControlFlowFlagAndReturnControlFlowInfo(ci, true);
		m_controlFlowInfo.push_back(info);

		// condition
		visitForOrWhileCondiction(ci, info, _forStatement.condition());

		// body and loopExpression
		m_pusher.startContinuation();
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			int ss = m_pusher.getStack().size();
			m_pusher.startContinuation();
			_forStatement.body().accept(*this);
			m_pusher.drop(m_pusher.getStack().size() - ss);
			m_pusher.endContinuation();
			m_pusher.push(0, "CALLX");
			m_pusher.pushLines(R"(
DUP
EQINT 4
IFRET
)");
		} else {
			int ss = m_pusher.getStack().size();
			_forStatement.body().accept(*this);
			m_pusher.drop(m_pusher.getStack().size() - ss);
		}
		if (_forStatement.loopExpression() != nullptr) {
			_forStatement.loopExpression()->accept(*this);
		}
		m_pusher.endContinuation();

		m_pusher.push(0, "WHILE");

		m_controlFlowInfo.pop_back();


		// bottom
		int cntDrop = 0;
		if (ci.canReturn || ci.canBreak || ci.canContinue) {
			++cntDrop;
		}
		if (_forStatement.initializationExpression() != nullptr) {
			++cntDrop;
		}
		if (ci.canReturn) {
			if (allJmp()) {
				m_pusher.push(0, "EQINT 4");
				m_pusher.push(-1, "IFRET");
				if (_forStatement.initializationExpression() != nullptr) {
					m_pusher.drop(1);
				}
			} else {
				m_pusher.push(+1, "DUP");
				if (ci.canBreak || ci.canContinue) {
					m_pusher.push(0, "EQINT 4");
				}
				m_pusher.push(-1, "IFRET");
				m_pusher.drop(cntDrop);
			}
		} else {
			m_pusher.drop(cntDrop);
		}

		m_pusher.push(0, "; end for");
		m_pusher.getStack().ensureSize(saveStackSize, "for");
		return false;
	}

	bool visit(Return const& _return) override {
		m_pusher.push(0, ";; return");
		auto expr = _return.expression();
		if (expr) {
			if (!tryOptimizeReturn(expr)) {
				if (!areReturnedValuesLiterals(expr)) {
					acceptExpr(expr);
				}
			}
		}

		int retCount = 0;
		if (_return.annotation().functionReturnParameters != nullptr) {
			std::vector<ASTPointer<VariableDeclaration>> const& params =
					_return.annotation().functionReturnParameters->parameters();
			retCount = params.size();
		}


		int revertDelta = m_pusher.getStack().size() - retCount;
		if (expr && areReturnedValuesLiterals(expr)) {
			m_pusher.drop(m_pusher.getStack().size());
			acceptExpr(expr);
		} else {
			m_pusher.dropUnder(retCount, m_pusher.getStack().size() - retCount);
		}


		if (!allJmp()) {
			m_pusher.pushInt(4);

			revertDelta--;
			m_pusher.push(revertDelta, ""); // fix stack
		} else if (!m_controlFlowInfo.empty()) { // all continuation are run by JMPX
			m_pusher.push(revertDelta, ""); // fix stack
		}
		m_pusher.push(0, "RET");
		return false;
	}

	void breakOrContinue(int code) {
		solAssert(code == 1 || code == 2, "");

		if (code == 1) {
			m_pusher.push(0, ";; continue");
		} else {
			m_pusher.push(0, ";; break");
		}

		ControlFlowInfo controlFlowInfo;
		for (int i = static_cast<int>(m_controlFlowInfo.size()) - 1; ; --i) {
			if (m_controlFlowInfo.at(i).isLoop) {
				controlFlowInfo = m_controlFlowInfo.at(i);
				break;
			}
		}

		const int sizeDelta = m_pusher.getStack().size() - controlFlowInfo.stackSize;
		m_pusher.drop(sizeDelta + 1);
		m_pusher.pushInt(code);
		m_pusher.push(0, "RET");
		m_pusher.push(sizeDelta, ""); // fix stack
	}

	bool visit(Break const&) override {
		breakOrContinue(2);
		return false;
	}

	bool visit(Continue const&) override {
		breakOrContinue(1);
		return false;
	}

	bool visit(EmitStatement const& _emit) override {
		auto eventCall = to<FunctionCall>(&_emit.eventCall());
		solAssert(eventCall, "");
		auto eventName = to<Identifier>(&eventCall->expression());
		solAssert(eventName, "");
		string name = eventName->name();
		m_pusher.push(0, ";; emit " + name);
		auto event = m_pusher.ctx().getEvent(name);
		solAssert(event, "");
		TVMExpressionCompiler ec(m_pusher);
		ec.encodeOutboundMessageBody2(
			name,
			eventCall->arguments(),
			event->parameterList().parameters(),
			StackPusherHelper::ReasonOfOutboundMessage::EmitEventExternal);

		if (auto argument = _emit.externalAddress()) {
			acceptExpr(argument.get());
			sendExternalMessage(true);
		} else
			sendExternalMessage();

		return false;
	}

protected:
	bool tryOptimizeReturn(Expression const* expr) {
		auto identifier = to<Identifier>(expr);
		if (identifier) {
			const std::string& name = identifier->name();
			if (m_pusher.getStack().isParam(name) && m_pusher.getStack().getOffset(name) == 0) {
				return true;
			}
		} else if (auto tuple = to<TupleExpression>(expr)) {
			int size = tuple->components().size();
			int i = 0;
			for (const ASTPointer<Expression>& comp : tuple->components()) {
				identifier = to<Identifier>(comp.get());
				if (!identifier || !m_pusher.getStack().isParam(identifier->name()) ||
							m_pusher.getStack().getOffset(identifier->name()) != size - 1 - i) {
					return false;
				}

				++i;
			}
			return true;
		}

		return false;
	}

	static bool areReturnedValuesLiterals(Expression const* expr) {
		auto literal = to<Literal>(expr);
		if (literal) {
			return true;
		} else if (auto tuple = to<TupleExpression>(expr)) {
			for (const ASTPointer<Expression>& comp : tuple->components()) {
				if (to<Literal>(comp.get()) == nullptr) {
					return false;
				}
			}
			return true;
		}

		return false;
	}

	void generateMainExternal() {
		switch (m_pusher.ctx().pragmaHelper().abiVersion()) {
			case 1:
				generateMainExternalForAbiV1();
				break;
			case 2:
				generateMainExternalForAbiV2();
				break;
			default:
				solAssert(false, "");
		}
	}

	void generateMainExternalForAbiV1() {
		m_pusher.generateInternal("main_external", -1);
		// contract_balance msg_balance msg_cell origin_msg_body_slice
		m_pusher.pushLines(R"(
PUSHINT -1 ; main_external trans id
PUSH s1    ; originMsgBodySlice
LDREFRTOS  ; msgBodySlice signSlice
DUP        ; msgBodySlice signSlice signSlice
SDEMPTY    ; msgBodySlice signSlice isSignSliceEmpty
PUSHCONT {
	DROP         ; msgBodySlice
}
PUSHCONT {
	DUP          ; msgBodySlice signSlice signSlice
	PUSHINT 512  ; msgBodySlice signSlice signSlice 512
	SDSKIPFIRST  ; msgBodySlice signSlice signSlice'
	PLDU 256     ; msgBodySlice signSlice pubKey
	PUSH s2      ; msgBodySlice signSlice pubKey msgBodySlice
	HASHSU       ; msgBodySlice signSlice pubKey msgHash
	PUSH2 s2,s1  ; msgBodySlice signSlice pubKey msgHash signSlice pubKey
	CHKSIGNU     ; msgBodySlice signSlice pubKey isSigned
	THROWIFNOT 40; msgBodySlice signSlice pubKey
	SETGLOB 5    ; msgBodySlice signSlice
	DROP         ; msgBodySlice
}
IFELSE
)");
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");
		m_pusher.pushLines(R"(
LDU 32                         ; functionId msgSlice
LDU 64                         ; functionId timestamp msgSlice
SWAP                           ; functionId msgSlice timestamp
CALL $replay_protection_macro$ ; functionId msgSlice
SWAP                           ; msgSlice functionId
JMP 1
)");
	}

	void generateMainExternalForAbiV2() {
		m_pusher.generateInternal("main_external", -1);
		m_pusher.push(0, "PUSHINT -1 ; main_external trans id");
//		stack:
//		contract_balance
//		msg_balance is always zero
//		msg_cell
//		msg_body_slice
//		transaction_id = -1

		m_pusher.push(0, "PUSH S1");

		m_pusher.pushPrivateFunctionOrMacroCall(0, "c4_to_c7_with_init_storage");

		checkSignatureAndReadPublicKey();
		if (m_pusher.ctx().afterSignatureCheck()) {
			// ... msg_cell msg_body_slice -1 rest_msg_body_slice
			m_pusher.push(0, "PUSH S3");
			CodeLines const& codeLines = m_pusher.ctx().m_inlinedFunctions.at("afterSignatureCheck");
			m_pusher.append(codeLines);
		} else {
			defaultReplayProtection();
			if (m_pusher.ctx().pragmaHelper().haveExpire()) {
				expire();
			}
		}

		callPublicFunction();
	}

	void pushMsgPubkey() {
		// signatureSlice msgSlice hashMsgSlice

		if (m_pusher.ctx().pragmaHelper().havePubkey()) {
			m_pusher.pushLines(R"(
	SWAP  ; signatureSlice hashMsgSlice msgSlice
	LDU 1 ; signatureSlice hashMsgSlice havePubkey msgSlice
	SWAP  ; signatureSlice hashMsgSlice msgSlice havePubkey
	PUSHCONT {
		LDU 256       ; signatureSlice hashMsgSlice pubkey msgSlice
)");
			m_pusher.addTabs(2);
			m_pusher.exchange(0, 3); //  msgSlice hashMsgSlice pubkey signatureSlice
			m_pusher.exchange(0, 1); //  msgSlice hashMsgSlice signatureSlice pubkey
			m_pusher.subTabs(2);
			m_pusher.pushLines(R"(
	}
	PUSHCONT {
)");
			// signatureSlice hashMsgSlice msgSlice
			m_pusher.addTabs(2);
			m_pusher.exchange(0, 2); // msgSlice hashMsgSlice signatureSlice
			m_pusher.getGlob(2);
			m_pusher.subTabs(2);
			m_pusher.pushLines(R"(
	}
	IFELSE)");
		} else {
			// signatureSlice msgSlice hashMsgSlice
			m_pusher.addTabs();
			m_pusher.push(0, "ROT"); // msgSlice hashMsgSlice signatureSlice
			m_pusher.getGlob(2);
			m_pusher.subTabs();
		}

		TVMScanner sc{*m_pusher.ctx().getContract()};
		if (sc.haveMsgPubkey) {
			m_pusher.addTabs();
			m_pusher.pushS(0);
			m_pusher.push(-1, "SETGLOB 5");
			m_pusher.subTabs();
		}

		// msgSlice hashMsgSlice signatureSlice pubkey
	}

	void checkSignatureAndReadPublicKey() {
		// msgSlice

		m_pusher.pushLines(R"(
LDU 1 ; haveSign msgSlice
SWAP
PUSHCONT {
	PUSHINT 512
	LDSLICEX ; signatureSlice msgSlice
	DUP      ; signatureSlice msgSlice msgSlice
	HASHSU   ; signatureSlice msgSlice hashMsgSlice
)");

		pushMsgPubkey();

		m_pusher.pushLines(R"(
	CHKSIGNU      ; msgSlice isSigned
	THROWIFNOT 40 ; msgSlice)");


		if (m_pusher.ctx().pragmaHelper().havePubkey()) {
			// External inbound message have not signature but have public key
			m_pusher.pushLines(R"(
}
PUSHCONT {
	LDU 1      ; havePubkey msgSlice
	SWAP       ; msgSlice havePubkey
	THROWIF 58 ; msgSlice
}
IFELSE
)");
		} else {
			m_pusher.pushLines(R"(
}
IF
)");
		}
	}

	void defaultReplayProtection() {
		// msgSlice
		m_pusher.pushLines(R"(
LDU 64                         ; timestamp msgSlice
SWAP                           ; msgSlice timestamp
CALL $replay_protection_macro$ ; msgSlice)");
	}

	void expire() {
		m_pusher.pushLines(R"(
LDU 32  ; expireAt msgSlice
SWAP    ; msgSlice expireAt
NOW     ; msgSlice expireAt now
GREATER ; msgSlice expireAt>now)");
		m_pusher.pushLines("THROWIFNOT " + toString(TvmConst::RuntimeException::MessageIsExpired));
	}

	void callPublicFunction() {
		// msg_body
		std::string s = R"(
LDU  32 ; funcId body
SWAP    ; body funcId
CALL 1
GETGLOB 7
ISNULL
PUSHCONT {
	CALL $:fallback_without$
}
IF
)";
		fillInlineFunctionsAndConstants(s);
		m_pusher.pushLines(s);
	}

	void generateMainInternal() {
		std::string s = R"(
.internal-alias :main_internal,        0
.internal	:main_internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: main_internal
;; param: contract_balance
;; param: msg_balance
;; param: inbound_message
;; param: msg_body_slice
PUSHINT 0  ; main_external trans id


PUSH S1    ; body
SEMPTY     ; isEmpty
PUSHCONT {
	PUSH S2     ; inbound_message
	CTOS        ; inbound_message
	PUSHINT 3   ; inbound_message 3
	SDSKIPFIRST ; inbound_message'
	PLDU 1      ; bounced
	EQINT 1     ; bounced==1
	PUSHCONT {
		CALL $:onBounce$
	}
	PUSHCONT {
		CALL $:receive$
	}
	IFELSE
}
IFJMP

PUSH S1            ; body
LDUQ 32            ; [funcId] body' ok
PUSHCONT {
	CALL $:fallback$
}
IFNOTJMP
PUSH2 S1,S1        ; funcId body' funcId funcId
EQINT 0            ; funcId body' funcId funcId==0
PUSHCONT {
	CALL $:receive$
}
IFJMP
LESSINT first_fun_id            ; funcId body' funcId<first_fun_id
PUSH S2              ; funcId body' funcId<first_fun_id funcId
PUSHINT last_fun_id   ; funcId body' funcId<first_fun_id funcId last_fun_id
GREATER              ; funcId body' funcId<first_fun_id funcId>last_fun_id
OR                   ; funcId body' funcId<first_fun_id||funcId>last_fun_id
PUSHCONT {
	CALL $:fallback$
}
IFJMP

SWAP  ; bodyLen body' funcId
CALL 1

GETGLOB 7
ISNULL
PUSHCONT {
	CALL $:fallback_without$
}
IF
)";
		fillInlineFunctionsAndConstants(s);
		m_pusher.pushLines(s);
	}

	void fillInlineFunctionsAndConstants(std::string& pattern) {
		boost::replace_all(pattern, "first_fun_id", toString(TvmConst::FunctionId::First));
		boost::replace_all(pattern, "last_fun_id", toString(TvmConst::FunctionId::Last));

		for (std::size_t tabsQty = 2; tabsQty >= 1; --tabsQty) {
			const std::string tab(tabsQty, '\t');

			if (m_pusher.ctx().haveReceiveFunction()) {
				boost::replace_all(pattern, tab + "CALL $:receive$", m_pusher.ctx().m_inlinedFunctions.at("receive").str(tab));
			} else {
				if (m_pusher.ctx().haveFallbackFunction()) {
					boost::replace_all(pattern, tab + "CALL $:receive$", m_pusher.ctx().m_inlinedFunctions.at("fallback").str(tab));
				} else {
					boost::replace_all(pattern, tab + "CALL $:receive$", tab + "THROW 59");
				}
			}

			if (m_pusher.ctx().haveFallbackFunction()) {
				boost::replace_all(pattern, tab + "CALL $:fallback_without$", m_pusher.ctx().m_inlinedFunctions.at("fallback_without").str(tab));
				boost::replace_all(pattern, tab + "CALL $:fallback$", m_pusher.ctx().m_inlinedFunctions.at("fallback").str(tab));
			} else {
				boost::replace_all(pattern, tab + "CALL $:fallback_without$", tab + "THROW 60");
				boost::replace_all(pattern, tab + "CALL $:fallback$", tab + "THROW 60");
			}

			if (m_pusher.ctx().haveOnBounceHandler()) {
				boost::replace_all(pattern, tab + "CALL $:onBounce$", m_pusher.ctx().m_inlinedFunctions.at("onBounce").str(tab));
			} else {
				boost::replace_all(pattern, tab + "CALL $:onBounce$", "");
			}
		}
	}

	bool visit(FunctionDefinition const& /*_function*/) override { solAssert(false, ""); }
	bool visit(TupleExpression const& /*_tupleExpression*/) override 	{ solAssert(false, "Internal error: unreachable"); }
	bool visit(Conditional const& /*_conditional*/) override			{ solAssert(false, "Internal error: unreachable"); }
	bool visit(Assignment const& /*_assignment*/) override 				{ solAssert(false, "Internal error: unreachable"); }
	bool visit(Literal const& /*_node*/) override						{ solAssert(false, "Internal error: unreachable"); }
	bool visit(BinaryOperation const& /*_node*/) override 				{ solAssert(false, "Internal error: unreachable"); }
	bool visit(UnaryOperation const& /*_node*/) override 				{ solAssert(false, "Internal error: unreachable"); }
	bool visit(Identifier const& /*_identifier*/) override 				{ solAssert(false, "Internal error: unreachable"); }
	bool visit(MemberAccess const& /*_node*/) override 					{ solAssert(false, "Internal error: unreachable"); }
	bool visit(ElementaryTypeNameExpression const& /*_node*/) override 	{ solAssert(false, "Internal error: unreachable"); }
	bool visit(IndexAccess const& /*_node*/) override 					{ solAssert(false, "Internal error: unreachable"); }
	bool visit(FunctionCall const& /*_functionCall*/) override 			{ solAssert(false, "Internal error: unreachable"); }
	bool visit(NewExpression const& /*_newExpression*/) override 		{ solAssert(false, "Internal error: unreachable"); }
};

}	// end solidity::frontend
