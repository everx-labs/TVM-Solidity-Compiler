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

#include "TVMCommons.hpp"
#include "TVMABI.hpp"
#include "TVMConstants.hpp"

namespace dev {
namespace solidity {

struct ContSettings {
	bool addReturnFlag = false;
	bool isUntil = false;
	ContSettings() {}
	ContSettings(bool addReturnFlag) : addReturnFlag(addReturnFlag) {}
};

struct CompilerState {
	bool m_addReturnFlag = false;
	bool m_isUntil = false;
	int m_untilStackSize = -1;
	const Statement* m_loopExpression = nullptr;
	
	void overwrite(const ContSettings& settings) {
		m_addReturnFlag  = settings.addReturnFlag;
		m_isUntil		 = settings.isUntil;
	}
	
};

class TVMCompiler: public ASTConstVisitor, public ITVMCompiler, StackPusherHelper
{
	TVMStack					m_stack;
	CodeLines					m_code;
	StackPusherImpl				m_pusherHelperImpl;
	CompilerState				m_state;
	bool						mDropFunctionStack {true};

public:
	static std::vector<ContractDefinition const*> m_allContracts;

	static bool m_optionsEnabled;
	static bool m_dbg;
	static bool m_abiOnly;
	static bool m_outputProduced;
	static std::string m_outputWarnings;

public:
	TVMCompiler(const TVMCompilerContext* ctx)
		: StackPusherHelper(this, ctx)
		, m_pusherHelperImpl(m_stack, m_code)
		{}
		
	TVMCompiler(const TVMCompiler& oth)
		: StackPusherHelper(this, &oth.ctx())
		, m_stack(oth.m_stack)
		, m_pusherHelperImpl(m_stack, m_code)
		{}

public:
	string str(const string& indent) const {
		return m_code.str(indent);
	}

	static void generateABI(ContractDefinition const* contract) {
		if (contract->name() != getLastContractName())
			return;
		m_outputProduced = true;
		
		TVMABI::generateABI(contract, m_allContracts);
	}

	static string getLastContractName() {
		string name;
		for (auto c : m_allContracts) {
			if (c->canBeDeployed()) {
				name = c->name();
			}
		}
		return name;
	}

	static void proceedContract(ContractDefinition const* contract) {
		if (contract->name() != getLastContractName())
			return;
		m_outputProduced = true;
		if (getFunction(contract, "tvm_mode0")) {
			proceedContractMode0(contract);
		} else {
			proceedContractMode1(contract);
		}
	}
	
	static void proceedContractMode0(ContractDefinition const* contract) {
		solAssert(getContractFunctions(contract).size() == 1, "");
		CodeLines code;
		for (const auto& _function : getContractFunctions(contract)) {
			TVMCompilerContext ctx(contract, m_allContracts);
			TVMCompiler tvm(&ctx);
			tvm.visitFunction(*_function);
			code.append(tvm.m_code);
		}
		cout << code.str("");
	}

	static bool notNeedsPushContWhenInlining(FunctionDefinition const* _function) {
		TVMScanner bodyScanner{_function->body()};

		std::vector<ASTPointer<Statement>> statements = _function->body().statements();
		for (std::vector<int>::size_type i = 0; i + 1 < statements.size(); ++i) {
			TVMScanner scanner{*statements[i].get()};
			if (scanner.m_info.canReturn) {
				return false;
			}
		}

		if (statements.empty()) {
			return true;
		}

		bool isLastStatementReturn = to<Return>(statements.back().get()) != nullptr;
		return isLastStatementReturn || (_function->returnParameters().empty() && !bodyScanner.m_info.canReturn);
	}

	static CodeLines makeInlineFunctionCall(const TVMCompilerContext& ctx, FunctionDefinition const* _function,
            bool dropFunctionStack = true,
			set<const ContractDefinition *> * calledConstructors = nullptr) {
		TVMCompiler tvm(&ctx);
		if (_function->isConstructor())
			tvm.visitConstructor(*_function, calledConstructors);
		else
			tvm.visitFunction2(*_function, dropFunctionStack);
		bool doInline = notNeedsPushContWhenInlining(_function);

		CodeLines res;
		if (doInline) {
			if (!tvm.m_code.lines.empty() && tvm.m_code.lines.back() == "RET") {
				tvm.m_code.lines.pop_back();
			}
			res.push("; " + _function->name());
			res.append(tvm.m_code);
			res.push("; end " + _function->name());
		} else {
			res.pushCont(tvm.m_code, _function->name());
			res.push("CALLX");
		}
		return res;
	}


	static void fillInlinedFunctions(TVMCompilerContext& ctx, ContractDefinition const* contract) {
		for (const auto& _function : getContractFunctions(contract)) {
			const auto& fname = _function->name();
			if (ends_with(fname, "_inline")) {
				ctx.m_inlinedFunctions[fname] = makeInlineFunctionCall(ctx, _function);
			}
		}
	}
	
	static void proceedContractMode1(ContractDefinition const* contract) {
		TVMCompilerContext ctx(contract, m_allContracts);
		CodeLines code;
		
		fillInlinedFunctions(ctx, contract);
		
		// generate global constructor which inlines all contract constructors
		if (!ctx.isStdlib())
		{
			TVMCompiler tvm(&ctx);
			tvm.generateConstructors();
			code.append(tvm.m_code);
		}
		
		for (const auto& funcInfo : ctx.m_functionsList) {
			const auto& _function = funcInfo.m_function;
			ctx.m_currentFunction = &funcInfo;
			solAssert(!ctx.isPureFunction(_function), "");
			if (_function->isConstructor())
				continue;
			if (_function->visibility() == Declaration::Visibility::TvmGetter) {
				TVMCompiler tvm(&ctx);
				tvm.m_code.generateGlobl(_function->name(), true);
				tvm.visitFunction(*_function);
				code.append(tvm.m_code);
			} else if (isMacro(funcInfo.m_internalName)) {
				code.generateMacro(funcInfo.m_internalName);
				code.append(makeInlineFunctionCall(ctx, _function));
				code.push(" ");
			} else {
				if (_function->isPublic()) {
					TVMCompiler tvm0(&ctx);
					TVMCompiler tvm1(&ctx);
					bool isBaseMethod = _function != getContractFunctions(contract, _function->name()).back();
					if (!isBaseMethod) {
						tvm0.generatePublicFunction(funcInfo);
					}
					if (!_function->isFallback()) {
						tvm1.generatePrivateFunction(funcInfo);
					}
					code.append(tvm0.m_code);
					code.append(tvm1.m_code);
				} else {
					TVMCompiler tvm(&ctx);
					tvm.generatePrivateFunction(funcInfo);
					code.append(tvm.m_code);
				}
			}
		}
		if (!ctx.haveFallbackFunction() && !ctx.isStdlib()) {
			TVMCompiler tvm(&ctx);
			tvm.generateDefaultFallbackFunction();
			code.append(tvm.m_code);
		}
		if (!ctx.haveOnBounceHandler() && !ctx.isStdlib()) {
			TVMCompiler tvm(&ctx);
			tvm.generateDefaultOnBounceHandler();
			code.append(tvm.m_code);
		}
		for (const auto& fname : ctx.m_remoteFunctions) {
			code.generateGlobl(TVMCompilerContext::getFunctionExternalName(fname), false);
			code.push("RET");
			code.push(" ");
		}
		for (auto event : ctx.events()) {
			const string& ename = event->name();
			code.generateGlobl(ename, false);
			code.push("RET");
			code.push(" ");
		}
		cout << code.str("");
	}

	void generateConstructors() {
		m_code.generateGlobl("constructor", true);
		generateConstructorProtection();
		pushPersistentDataFromC4ToC7();
		
		bool newChain = true;
		deque<const ContractDefinition * > constructorHeads;
		auto mainChain = getContractsChain(ctx().getContract());
		for(auto c: mainChain)
			setDefaultMemberValues(c);
		for(auto c = mainChain.rbegin(); c != mainChain.rend(); c++) {
			if ((*c)->constructor()) {
				if (newChain) {
					constructorHeads.push_front((*c));
					newChain = false;
				}
			} else
				newChain = true;
		}
		
		vector<const ContractDefinition * > chainHeads;
		
		set<const ContractDefinition *> calledConstructors;
		
		for (auto c : constructorHeads){
			if (calledConstructors.count(c) != 0)
				continue;
			push(0, "; call " + c->name() + " constr");
			if (ctx().getContract() == c)
				decodeParameters(c->constructor()->parameters());
			calledConstructors.insert(c);
			m_code.append(makeInlineFunctionCall(ctx(), c->constructor(), true, &calledConstructors));
		}
		
		pushPersistentDataFromC7ToC4();
		push(0, "PUSHINT 0");
		push(0, "ONLYTOPX");
//		push(0, "NOP");
		push(0, "   ");
	}

	void generateDefaultFallbackFunction() {
		push(0, ".globl	fallback");
		push(0, ".public fallback");
		push(0, ".type	fallback, @function");
		push(0, "DROP");
		push(0, "RET");
		push(0, " ");
	}

	void generateDefaultOnBounceHandler() {
		push(0, ".globl	onBounce");
		push(0, ".type	onBounce, @function");
		push(0, "DROP");
		push(0, "RET");
		push(0, " ");
	}

	class DecodePosition : private boost::noncopyable {
		bool isPositionValid;
		int minRestSliceBits;
		int maxRestSliceBits;
		int restFef;
	public:

		enum Algo { JustLoad, NeedLoadNextCell, Unknown };

		DecodePosition() :
				isPositionValid{true},
				minRestSliceBits{TvmConst::CellBitLength - TvmConst::Message::functionIdLength - TvmConst::Message::timestampLength},
				maxRestSliceBits{TvmConst::CellBitLength - TvmConst::Message::functionIdLength},
				restFef{3}
		{

		}

		Algo updateStateAndGetLoadAlgo(Type const* type) {
			ABITypeSize size(type);
			solAssert(0 <= size.refs && size.refs <= 1, "");
			solAssert(0 <= size.minBits && size.minBits <= size.maxBits, "");

			if (!isPositionValid) {
				return Unknown;
			}

			minRestSliceBits -= size.maxBits;
			maxRestSliceBits -= size.minBits;
			restFef -= size.refs;

			if (minRestSliceBits < 0 && maxRestSliceBits >= 0) {
				isPositionValid = false;
				return Unknown;
			}

			if (minRestSliceBits < 0 || restFef == 0) {
				minRestSliceBits = TvmConst::CellBitLength - size.maxBits;
				maxRestSliceBits = TvmConst::CellBitLength - size.minBits;
				restFef = 4 - size.refs;
				return NeedLoadNextCell;
			}

			return JustLoad;
		}
	};

	void decodeParameters(const ptr_vec<VariableDeclaration>& params) {
		DecodePosition position;
		push(+1, "; Decode input parameters"); // locate slice on stack
		for (const auto& variable: params) {
			auto savedStackSize = m_stack.size();
			decodeParameter(variable.get(), &position);
			m_stack.ensureSize(savedStackSize + 1, "decodeParameter-2");
		}
		push(-1, "DROP"); // drop slice
		push(-params.size(), ""); // fix stack
//		push(0, "ENDS");
	}

	void generateModifier(FunctionDefinition const* _function){
		for (const auto& m : _function->modifiers()) {
			string name = m->name()->name();
			if (ctx().isContractName(name))
				continue;
			// TODO: check that "_" is in the end...
			if (m->arguments())
				cast_error(*m, "Modifiers with parameters are not supported.");
			push(0, "; Modifier " + name);
			bool found = false;
			auto chain = getContractsChain(ctx().getContract());
			for (auto contract = chain.rbegin(); contract != chain.rend(); contract++)
			{
				for (const ModifierDefinition* modifier : (*contract)->functionModifiers()) {
					if (modifier->name() != name)
						continue;
					// TODO: use PUSHCONT!
					if (!to<PlaceholderStatement>(modifier->body().statements().back().get()))
						cast_warning(*modifier, R"(Code after "_;" is not supported, it will be executed before "_;".)");
					modifier->body().accept(*this);
					if (!doesAlways<Return>(&modifier->body()))
						drop(m_stack.size());
					found = true;
				}
				if (found)
					break;
			}
		}
	}

	void generatePublicFunction(const FuncInfo& fi) {
		bool dontEmitReturn = !!getFunction(ctx().getContract(), "tvm_dont_emit_events_on_return");
		FunctionDefinition const* _function = fi.m_function;
		auto fnameInternal = fi.m_internalName;
		auto fnameExternal = TVMCompilerContext::getFunctionExternalName(_function);

		// generate header
		m_code.generateGlobl(fnameExternal, _function->isPublic());
		if (fi.m_function->stateMutability() != StateMutability::Pure) {
			pushPersistentDataFromC4ToC7();
		}
		generateModifier(_function);

		decodeParameters(_function->parameters());

		// function body
		CodeLines res = makeInlineFunctionCall(ctx(), _function, false);
		m_code.append(res);

		// emit function result
		if (!dontEmitReturn) {
			emitFunctionResult(_function);
		}

		// copy c7 to c4 back if needed
		if (_function->stateMutability() != StateMutability::Pure &&
			_function->stateMutability() != StateMutability::View) {
			pushPersistentDataFromC7ToC4();
		}

		// drop not needed variables from stack
		pushInt(fi.m_function->returnParameters().size());
		push(0, "ONLYTOPX");
		push(-1, ""); // fix stack

		push(0, "RET");
		push(0, " ");
	}

	void generatePrivateFunction(const FuncInfo& fi) {
		FunctionDefinition const *_function = fi.m_function;
		auto fnameInternal = fi.m_internalName;
		if (_function->name() == "onTickTock") {
			m_code.generateInternal("onTickTock", -2);
			push(0, "PUSHINT -2");
			push(0, "PUSHINT -2");
			push(0, "PUSH s2");
			if (fi.m_function->stateMutability() != StateMutability::Pure) {
				pushPersistentDataFromC4ToC7();
			}
		} else if (_function->name() == "main_external") {
			m_code.generateInternal("main_external", -1);
			m_code.push("PUSHINT -1 ; selector");
		} else if (_function->name() == "main_internal") {
			m_code.generateInternal("main_internal", 0);
			m_code.push("PUSHINT 0 ; selector");
		} else {
			m_code.generateGlobl(fnameInternal, false);
		}
		visitFunction(*_function);
	}

	void emitFunctionResult(FunctionDefinition const* _function) {
		const auto& params = _function->returnParameters();
		auto count = params.size();
		if (count == 0) {
			return;
		}
		push( 0, ";; emitting " + toString(count) + " value(s)");

		std::vector<Type const*> types;
		std::vector<ASTNode const*> nodes;
		for (const auto & param : params) {
			types.push_back(param->annotation().type.get());
			nodes.push_back(param.get());
		}

		int prevStackSize = getStack().size();
		encodeFunctionAndParams(
				TVMCompilerContext::getFunctionExternalName(_function),
				types,
				nodes,
				[&](size_t idx) {
					int pos = (getStack().size() - prevStackSize) + (static_cast<int>(params.size()) - static_cast<int>(idx) - 1);
					push(+1, "PUSH s" + toString(pos));
				},
				StackPusherHelper::ReasonOfOutboundMessage::FunctionReturnExternal
		);
		pushPrivateFunctionOrMacroCall(-1, "send_external_message_macro");
	}

	void decodeRefParameter(Type const*const type, DecodePosition* position) {
		switch (position->updateStateAndGetLoadAlgo(type)) {
			case DecodePosition::JustLoad:
				break;
			case DecodePosition::NeedLoadNextCell:
				loadNextSlice();
				break;
			case DecodePosition::Unknown:
				ifRefsAreEmptyThenLoadNextSlice();
				break;
		}
		push(+1, "LDREF");
	}

	void loadNextSlice() {
		push(0, "LDREF");
		push(0, "ENDS");
		push(0, "CTOS");
	}

	void ifBitsAreEmptyThenLoadNextSlice() {
		pushLines(R"(DUP
SDEMPTY
PUSHCONT {
	LDREF
	ENDS
	CTOS
}
IF
)");
	}

	void ifRefsAreEmptyThenLoadNextSlice() {
		pushLines(R"(DUP
SREFS
EQINT 1
PUSHCONT {
	LDREF
	ENDS
	CTOS
}
IF
)");
	}

	void loadNextSliceIfNeed(const DecodePosition::Algo algo) {
		switch (algo) {
			case DecodePosition::JustLoad:
				break;
			case DecodePosition::NeedLoadNextCell:
				loadNextSlice();
				break;
			case DecodePosition::Unknown:
				ifBitsAreEmptyThenLoadNextSlice();
				break;
		}
	}

	void decodeParameter(VariableDeclaration const* variable, DecodePosition* position) {
		auto type = getType(variable);
		const Type::Category category = variable->type()->category();
		if (auto structType = to<StructType>(type)) {
			auto structName = structType->structDefinition().name();
			if (isTvmCell(structType)) {
				push(0, ";; decode cell " + structName);
				decodeRefParameter(structType, position);
			} else {
				push(0, ";; decode struct " + structName + " " + variable->name());
				auto members = structType->structDefinition().members();
				int saveStackSize = getStack().size() - 1;
				for (const auto &m : members) {
					push(0, ";; decode " + structName + "." + m->name());
					decodeParameter(m.get(), position);
				}
				push(0, ";; build struct " + structName + " ss:" + toString(getStack().size()));
				StructCompiler structCompiler{this, &structType->structDefinition()};
				structCompiler.createStruct([this, saveStackSize](const int memberIndex, const std::string &) {
					int currentStackSize = getStack().size();
					push(+1, "PUSH s" + toString(currentStackSize - saveStackSize - 1 - memberIndex));
				});
				push(0, "SWAP"); // ... struct slice
				dropUnder(2, members.size());
			}
		} else if (category == Type::Category::Address || category == Type::Category::Contract) {
			loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type));
			push(+1, "LDMSGADDR");
		} else if (isIntegralType(type)) {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type));
			push(+1, (ti.isSigned ? "LDI " : "LDU ") + toString(ti.numBits));
		} else if (auto arrayType = to<ArrayType>(type)) {
			if (arrayType->isByteArray()) {
				decodeRefParameter(type, position);
			} else {
				loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type));
				auto baseType = to<StructType>(arrayType->baseType().get());
				if (baseType && !StructCompiler::isPlaneStruct(baseType)) {
					cast_error(*variable, "Only arrays of plane struct are supported");
				}
				pushPrivateFunctionOrMacroCall(+1, "decode_array_macro");
			}
		} else {
			cast_error(*variable, "Unsupported parameter type: " + type->toString());
		}
	}

	bool visit(FunctionDefinition const& /*_function*/) override { solAssert(false, ""); }

	void visitFunction(FunctionDefinition const& _function, bool dropFunctionStack = true) {
		string fname = ctx().getFunctionInternalName(&_function);
		if (isTvmIntrinsic(fname))
			return;
		
		solAssert(!ctx().isPureFunction(&_function), "");
		
		push(0, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
		push(0, string(";; Function: ") + fname);
		
		visitFunction2(_function, dropFunctionStack);

		push(0, " ");
	}

	void visitFunction2(FunctionDefinition const& _function, bool dropFunctionStack = true) {
		int savedStackSize = m_stack.size();

		for (const auto& m : _function.modifiers()) {
			string name = m->name()->name();
			if (!_function.isPublic())
				cast_error(*m, "Modifiers are allowed only for public functions.");
		}

		for (const auto& variable: _function.parameters()) {
			auto name = variable->name();
			push(0, string(";; param: ") + name);
			m_stack.add(name, true);
		}

		bool haveSomeNamedReturnParams = false;
		for (const auto& returnParam: _function.returnParameters()) {
			haveSomeNamedReturnParams |= !returnParam->name().empty();
		}
		if (haveSomeNamedReturnParams) {
			pushReturnParameters(_function.returnParameters());
		}

		mDropFunctionStack = dropFunctionStack;
		_function.body().accept(*this);

		bool doFunctionAlwaysReturn = doesAlways<Return>(&_function.body());
		if (!doFunctionAlwaysReturn && haveSomeNamedReturnParams) {
			int paramQty = _function.parameters().size();
			int retQty = _function.returnParameters().size();
			push(0, ";; returning named params");
			if (dropFunctionStack) {
				int m = getStack().size() - savedStackSize - paramQty;
				blockSwap(paramQty, m);
				drop(getStack().size() - savedStackSize - retQty);
			} else {
				drop(getStack().size() - (savedStackSize + retQty + paramQty));
			}
		} else if (!doFunctionAlwaysReturn) {
			// if function did not return
			drop(m_stack.size() - savedStackSize);
			pushReturnParameters(_function.returnParameters());
		}

		if (_function.name() == "onTickTock") {
			if (_function.stateMutability() != StateMutability::Pure &&
			    _function.stateMutability() != StateMutability::View) {
				pushPersistentDataFromC7ToC4();
			}
		}
	}

	void visitConstructor(FunctionDefinition const& _function, set<const ContractDefinition *> * calledConstructors) {
		for (const auto& m : _function.modifiers()) {
			string name = m->name()->name();
			if (ctx().isContractName(name)) {
				// constructor call is processed in another place
				continue;
			}
			cast_error(*m, "Modifiers for constructors are not supported.");
		}
		for (const auto& variable: _function.parameters()) {
			auto name = variable->name();
			push(0, string(";; param: ") + name);
			m_stack.add(name, true);
		}
		callBaseConstructorsExplicit(&_function, calledConstructors);
		_function.body().accept(*this);
		drop(m_stack.size());
	}

	void pushReturnParameters(const std::vector<ASTPointer<VariableDeclaration>>& returnParameters) {
		int idParam = 0;
		for (const auto& returnParam: returnParameters) {
			auto name = returnParam->name();
			if (name.empty()) {
				name = "retParam@" + std::to_string(idParam);
			}
			push(0, string(";; ret param: ") + name);
			pushDefaultValue(returnParam->type().get());
			m_stack.add(name, false);

			++idParam;
		}
	}

	void generateConstructorProtection() {
		pushPrivateFunctionOrMacroCall(0, "generateConstructorProtection_macro");
//		push( 0, "NOP");
		push( 0, "ACCEPT");
	}

	void pushPersistentDataFromC4ToC7()  {
		pushPrivateFunctionOrMacroCall(0, "push_persistent_data_from_c4_to_c7_macro");
	}

	void pushPersistentDataFromC7ToC4()  {
		pushPrivateFunctionOrMacroCall(0, "push_persistent_data_from_c7_to_c4_macro");
	}

	void callBaseConstructorsExplicit(const FunctionDefinition* function, set<const ContractDefinition *> * calledConstructors) {
		auto contract0 = ctx().getContract(function);
		auto bases = contract0->baseContracts();
		auto chain = getContractsChain(ctx().getContract());
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
								push(-1, "");	// fix stack
							}
						}
					}
				}
			}
		for (auto contract : getContractsChain(contract0)) {
			if (find_if(bases.begin(),bases.end(),[&contract](auto base){
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
							push(-(m->arguments()->size()), "");	// fix stack
						}
					}
				}
				for (const auto& base: bases)
				{
					if (base->name().namePath()[0] == contract->name())
					{
						if (base->arguments()) {
							for (const auto& expr : *base->arguments()) {
								acceptExpr(expr.get());
								push(-1, "");	// fix stack
							}
						}
					}
				}
				push(0, "; call " + contract->name() + " constr");
				calledConstructors->insert(contract);
				pushLines(makeInlineFunctionCall(ctx(), contract->constructor(), true, calledConstructors).str("\n"));
			}
		}
	}
	
	void setDefaultMemberValues(const ContractDefinition* contract) {
		for (VariableDeclaration const* variable: contract->stateVariables()) {
			if (variable->isConstant()) {
				continue;
			}
			// Setting default values if needed
			if (auto value = variable->value().get()) {
				push(0, ";; " + variable->name());
				auto type = getType(variable);
				acceptExpr(value);
				setRootItem(variable->name(), type, *variable);
			}
		}
	}

	TVMStack& getStack() override {	// IStackPusher
		return m_stack;
	}

	void acceptExpr(const Expression* expr, const bool isResultNeeded = true) {
		solAssert(expr, "");
		TVMExpressionCompiler(this, ctx()).acceptExpr2(expr, isResultNeeded);
	}

	// TODO: free function
	static bool isUInt256(const ElementaryTypeNameToken& typeName) {
		switch (typeName.token()) {
			case Token::UIntM:
				return 256 == typeName.firstNumber();
			case Token::UInt:
				return true;
			default:
				return false;
		}
	}

	// TODO: move to TVMExpressionCompiler
	const FunctionDefinition* getRemoteFunctionDefinition(const MemberAccess* memberAccess) override {
		auto expr = &memberAccess->expression();
		if (isSuper(expr))
			return nullptr;
		if (auto ctype = to<ContractType>(getType(expr))) {
			auto remoteContract = &ctype->contractDefinition();
			const string& fname = memberAccess->memberName();
			if (auto f = getFunction(remoteContract, fname)) {
				if (!ctx().getLocalFunction(fname))
					ctx().m_remoteFunctions.insert(fname);
				push( 0, ";; Remote call " + remoteContract->name() + "." + fname);
				return f;
			}
			solAssert(false, "");
		} else {
			return nullptr;
		}
	}

	// statement
	bool visit(VariableDeclarationStatement const& _variableDeclarationStatement) override {
		auto decls = _variableDeclarationStatement.declarations();
		if (auto init = _variableDeclarationStatement.initialValue()) {
			auto tupleExpression = to<TupleExpression>(init);
			if (tupleExpression) {
				std::vector<ASTPointer<Expression>> const&  tuple = tupleExpression->components();
				for (std::size_t i = 0; i < tuple.size(); ++i) {
					if (!tryImplicitConvert(decls[i]->type().get(), tuple[i]->annotation().type.get())) {
						acceptExpr(init);
					}
				}
			} else  if (decls[0] == nullptr || !tryImplicitConvert(decls[0]->type().get(), init->annotation().type.get())) {
				acceptExpr(init);
			}
		} else {
			for (const auto& decl : decls) {
				pushDefaultValue(decl->type().get());
			}
		}

		m_stack.change(-decls.size());
		for (size_t i = 0; i < decls.size(); i++) {
			if (decls[i]) {
				push(0, string(";; decl: ") + decls[i]->name());
				m_stack.add(decls[i]->name(), true);
			} else {
				if (i == decls.size() - 1) {
					push(0, "DROP");
				} else if (i == decls.size() - 2) {
					push(0, "NIP");
				} else {
					push(0, "BLKSWAP 1, " + to_string(decls.size() - 1 - i));
					push(0, "DROP");
				}
			}
		}
		return false;
	}

	// statement
	bool visit(Block const& /*_block*/) override {
		// TODO: write the code explicitly
		return true;
	}

	void on_start() {
		if (m_state.m_addReturnFlag)
			push(0, ";; m_addReturnFlag = " + toString(m_state.m_addReturnFlag));
		if (m_state.m_isUntil)
			push(0, ";; m_isUntil = " + toString(m_state.m_isUntil));
	}

	CodeLines proceedContinuationExpr(const Expression& expression) override {
		TVMCompiler compiler(*this);
		compiler.acceptExpr(&expression);
		return compiler.m_code;
	}

	CodeLines proceedContinuation(const Statement& statement, ContSettings settings = ContSettings()) {
		TVMCompiler compiler(*this);
		compiler.m_state = m_state;
		compiler.m_state.overwrite(settings);
		compiler.on_start();
		statement.accept(compiler);
		if (!doesAlways<Return>(&statement) && !doesAlways<Break>(&statement) && !doesAlways<Continue>(&statement)) {
			// TODO: this condition looks suspisious
			int diff = compiler.m_stack.size() - m_stack.size();
			solAssert(diff >= 0, "diff = " + toString(diff));
			if (diff > 0) {
				compiler.push(0, ";; drop locals");
				compiler.drop(diff);
			}
		}
		if (settings.addReturnFlag)
			addFalseFlagIfNeeded(compiler.m_code);
		return compiler.m_code;
	}

	void applyContinuation(const CodeLines& code) override {
		m_code.pushCont(code);
		push(+1, ""); // adjust stack
	}

	static void addFalseFlagIfNeeded(CodeLines& code) {
		auto s = code.lines.back();
		if (s != "RET" && s != "PUSHINT 2 ; break" && s != "PUSHINT 3 ; continue")
			code.push("FALSE");
	}

	void dispatchFlagsAfterIf(const ContInfo& ci) {
		if (m_state.m_isUntil) {
			dispatchFlagsInUntil(ci);
		} else if (m_state.m_addReturnFlag) {
			push(0,  "DUP");
			push(0,  "IFRET");
			push(0,  "DROP");
		} else {
			push(0,  "IFRET");
		}
	}
	
	void passReturnFlagOut() {
		solAssert(m_state.m_addReturnFlag, "");
		solAssert(m_state.m_isUntil, "");
		push(0,  "PUSHCONT { TRUE TRUE }");
		push(0,  "IFJMP\t; return");
	}

	void dispatchFlagsInUntil(const ContInfo& ci) {
		solAssert(m_state.m_isUntil, "");
		if (ci.canReturn && !(ci.canBreak || ci.canContinue)) {
			passReturnFlagOut();
		} else {
			if (ci.canReturn) {
				push(0,  "DUP DUP");
				push(0,  "PUSHINT 1");
				push(0,  "EQUAL");
				push(0,  "IFRET");
				push(0,  "DROP");
			}
			if (ci.canBreak) {
				push(0,  "DUP");
				push(0,  "PUSHINT 2");
				push(0,  "EQUAL");
				
				// TODO: shouldn't the same check be performed for Continue?
				if (m_state.m_addReturnFlag)
					push(0,  "PUSHCONT { DROP FALSE TRUE }");
				else
					push(0,  "PUSHCONT { DROP TRUE }");
				
				push(0,  "IFJMP");
			}
			if (ci.canContinue) {
				push(0,  "DUP");
				push(0,  "PUSHINT 3");
				push(0,  "EQUAL");
				push(0,  "PUSHCONT { DROP FALSE }");
				push(0,  "IFJMP");
			}
			push(0,  "THROWIF 99");
		}
	}

	// statement
	bool visit(ExpressionStatement const& _expressionStatement) override {
		auto savedStackSize = m_stack.size();
		acceptExpr(&_expressionStatement.expression(), false);
		m_stack.ensureSize(savedStackSize, ASTNode2String(_expressionStatement));
		return false;
	}

	// statement
	bool visit(IfStatement const& _ifStatement) override {
		push(0, ";; if");
		if (!_ifStatement.falseStatement()) {
			ContInfo ci = getInfo(_ifStatement.trueStatement());
			solAssert(ci.alwaysReturns == getInfo(_ifStatement.trueStatement()).alwaysReturns, "");
			bool useJump = !m_state.m_isUntil;
			bool canBranch = ci.canReturn || ci.canBreak || ci.canContinue;
			bool addFlag = m_state.m_addReturnFlag || (
				useJump? (canBranch && !ci.alwaysReturns) : canBranch
			);
			auto ciTrue = proceedContinuation(_ifStatement.trueStatement(), ContSettings(addFlag));
			acceptExpr(&_ifStatement.condition());
			applyContinuation(ciTrue);
			if (useJump && ci.alwaysReturns) {
				push(-2, "IFJMP");
			} else if (!canBranch) {
				push(-2, "IF");
			} else {
				push(+1, "PUSHCONT { FALSE }");
				push(-3, "IFELSE");
				dispatchFlagsAfterIf(ci);
			}
		} else {
			ContInfo ci = getInfo(_ifStatement);
			solAssert(ci.alwaysReturns == (
				getInfo(_ifStatement.trueStatement()).alwaysReturns &&
				getInfo(*_ifStatement.falseStatement()).alwaysReturns), "");
			bool addFlag = m_state.m_addReturnFlag 
					   || (ci.canReturn && !ci.alwaysReturns)
					   || (ci.canBreak || ci.canContinue);
			ContSettings settings(addFlag);
			auto ciTrue  = proceedContinuation(_ifStatement.trueStatement(), settings);
			auto ciFalse = proceedContinuation(*_ifStatement.falseStatement(), settings);
			acceptExpr(&_ifStatement.condition());
			applyContinuation(ciTrue);
			applyContinuation(ciFalse);
			push(-3, "IFELSE");
			if (addFlag) {
				dispatchFlagsAfterIf(ci);
			} else if (ci.alwaysReturns) {
				push(0, "RET");
			}
		}
		return false;
	}

	void loop_processing(Statement const* initial, Expression const* conditionExpr,
						 ExpressionStatement const* loopExpression, Statement const& bodyStatement) {
		auto savedStackSize1 = m_stack.size();
		if (initial)
			initial->accept(*this);

		bool canReturn = getInfo(bodyStatement).canReturn;
		ContSettings settings;
		settings.isUntil = true;
		settings.addReturnFlag = canReturn;
		auto savedUntilStackSize = m_state.m_untilStackSize;
		m_state.m_untilStackSize = m_stack.size();
		m_state.m_loopExpression = loopExpression; ///////////////////////
		CodeLines body = proceedContinuation(bodyStatement, settings);
		if (canReturn) {
			body.push("DROP");
		}
		m_state.m_untilStackSize = savedUntilStackSize;

		CodeLines condition;
		if (conditionExpr) {
			condition = proceedContinuationExpr(*conditionExpr);
			if (canReturn) {
				// TODO: optimize with IFJMP
				condition.push("FALSE SWAP ; no return");
				condition.push("NOT DUP IFRET DROP");
				condition.push("DROP ; drop return flag");
			} else {
				condition.push("NOT DUP IFRET DROP");
			}
		}

		CodeLines loopExpr;
		if (loopExpression) {
			loopExpr = proceedContinuation(*loopExpression);
		}
		loopExpr.push("FALSE");

		CodeLines cont;
		cont.append(condition);
		cont.append(body);
		cont.append(loopExpr);
		applyContinuation(cont);
		
		push(-1, "UNTIL");
		if (canReturn) {
			if (m_state.m_addReturnFlag) {
				passReturnFlagOut();
			} else {
				push(0, "IFRET ; return");
			}
		}
		restoreStack(savedStackSize1);
	}

	// statement
	bool visit(WhileStatement const& _whileStatement) override {
		// TODO: support do while loop expression
		if (_whileStatement.isDoWhile()) {
			cast_error(_whileStatement, "Unsupported loop type do while.");
		}
		push(0, "; while statement");
		loop_processing(nullptr, &_whileStatement.condition(),
						nullptr, _whileStatement.body());
		push(0, "; while end");
		return false;
	}

	// statement
	bool visit(ForStatement const& _forStatement) override {
		push(0, "; for statement");
		loop_processing(_forStatement.initializationExpression(), _forStatement.condition(),
						_forStatement.loopExpression(), _forStatement.body());
		push(0, "; for end");
		return false;
	}

	// statement
	bool visit(Return const& _return) override {
		push(0, ";; return");
		auto expr = _return.expression();
		if (expr) {
			if (!tryOptimizeReturn(expr)) {
				if (!areReturnedValuesLiterals(expr)) {
					acceptExpr(expr);
				}
			}
		}

		auto params = _return.annotation().functionReturnParameters->parameters();
		int retCount = params.size();

		if (expr && areReturnedValuesLiterals(expr)) {
			if (mDropFunctionStack) {
				drop(m_stack.size());
			}
			acceptExpr(expr);
		} else if (mDropFunctionStack) {
			dropUnder(retCount, m_stack.size() - retCount);
		}

		auto ensureFits = [&] (Expression const* expr) {
			if (!isExpressionExactTypeKnown(expr)) {
				// push(0, toString(";; ") + typeid(*expr).name());
				ASTPointer<VariableDeclaration> param = params[0];
				if (auto typeName = to<ElementaryTypeName>(param->typeName())) {
					if (isUInt256(typeName->typeName()) && isNonNegative(expr)) {
						// no need to check
					} else {
						ensureValueFitsType(typeName->typeName(), *expr);
					}
				}
			}
		};
		
		auto retExp = _return.expression();
		if (retCount > 1) {
			if (to<TupleExpression>(retExp) || to<FunctionCall>(retExp)) {
				// TODO: call ensureFits() for all values
			} else {
				cast_error(*retExp, string("Unsupported return type") + typeid(*retExp).name());
			}
		} else if (retCount == 1) {
			ensureFits(retExp);
		}
		
		if (m_state.m_addReturnFlag)
			push(0, "PUSHINT 1 ; return");
		push(0, "RET");
		return false;
	}

	// statement
	bool visit(Break const&) override {
		push(0, ";; break");
		solAssert(m_state.m_untilStackSize >= 0, "");
		restoreStack(m_state.m_untilStackSize);
		push(0, "PUSHINT 2 ; break");
		return false;
	}

	// statement
	bool visit(Continue const&) override {
		push(0, ";; continue");
		restoreStack(m_state.m_untilStackSize);
		solAssert(m_state.m_loopExpression, "");
		// TODO: get rid of accept()
		m_state.m_loopExpression->accept(*this);
		push(0, "PUSHINT 3 ; continue");
		return false;
	}

	// statement
	bool visit(EmitStatement const& _emit) override {
		auto eventCall = to<FunctionCall>(&_emit.eventCall());
		solAssert(eventCall, "");
		auto eventName = to<Identifier>(&eventCall->expression());
		solAssert(eventName, "");
		string name = eventName->name();
		push(0, ";; emit " + name);
		auto event = ctx().getEvent(name);
		solAssert(event, "");
		TVMExpressionCompiler ec(this, ctx());
		ec.encodeOutboundMessageBody2(
			name, 
			eventCall->arguments(), 
			event->parameterList().parameters(),
			StackPusherHelper::ReasonOfOutboundMessage::EmitEventExternal);
		pushPrivateFunctionOrMacroCall(-1, "send_external_message_macro");
		return false;
	}

protected:
	/////////////////////////////////////////////////////
	// various helpers

	void push(int stackDiff, const string& cmd) override {
		if (m_dbg) DBG(cmd);
		m_pusherHelperImpl.push(stackDiff, cmd);
	}

	void restoreStack(int savedStackSize) {
		solAssert(savedStackSize <= m_stack.size(), "");
		std::vector<string> locals = m_stack.dropLocals(savedStackSize);
		solAssert(int(locals.size()) + savedStackSize == m_stack.size(), "");
		for (const auto& name : locals)
			push(0, "; erase " + name);
		drop(locals.size());
	}

	bool tryOptimizeReturn(Expression const* expr) {
		auto identifier = to<Identifier>(expr);
		if (identifier) {
			const std::string& name = identifier->name();
			if (getStack().isParam(name) && getStack().getOffset(name) == 0) {
				return true;
			}
		} else if (auto tuple = to<TupleExpression>(expr)) {
			int size = tuple->components().size();
			int i = 0;
			for (const ASTPointer<Expression>& comp : tuple->components()) {
				identifier = to<Identifier>(comp.get());
				if (!identifier || !getStack().isParam(identifier->name()) ||
							getStack().getOffset(identifier->name()) != size - 1 - i) {
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
	
protected:
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

}	// solidity
}	// dev
