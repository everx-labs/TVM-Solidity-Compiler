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
		int cnt = getContractFunctions(contract).size();
		bool specialMode = (cnt > 1);
		CodeLines code;
		int idx = 0;
		for (const auto& _function : getContractFunctions(contract)) {
			TVMCompilerContext ctx(contract, m_allContracts);
			TVMCompiler tvm(&ctx);
			tvm.visitFunction(*_function, true);
			if (!specialMode) {
				code.append(tvm.m_code);
			} else {
				code.pushCont(tvm.m_code, toString(idx));
			}
			idx++;
		}
		if (specialMode) {
			// Add Selector
			code.push(";;; params method_id cont0..cont" + toString(cnt-1));
			code.push("PUSH s"   + toString(cnt));
			code.push("PUSHINT " + toString(cnt-1));
			code.push("SUBR");
			code.push("XCHGX");
			code.push("XCHG s"   + toString(cnt));
			code.push("BLKDROP " + toString(cnt));
			code.push("JMPX");
		}
		cout << code.str("");
	}
	
	static void fillInlinedFunctions(TVMCompilerContext& ctx, ContractDefinition const* contract) {
		for (const auto& _function : getContractFunctions(contract)) {
			auto fname = _function->name();
			if (ends_with(fname, "_inline")) {
				TVMCompiler tvm(&ctx);
				tvm.visitFunction2(*_function, false);
				ctx.m_inlinedFunctions[fname] = tvm.m_code;
			}
		}
	}
	
	static void proceedContractMode1(ContractDefinition const* contract) {
		TVMCompilerContext ctx(contract, m_allContracts);
		CodeLines code;
		
		fillInlinedFunctions(ctx, contract);

		{
			// generate default constructor
			TVMCompiler tvm(&ctx);
			tvm.generateDefaultConstructors();
			code.append(tvm.m_code);
		}

		for (const auto& fi : ctx.m_functionsList) {
			const auto& _function = fi.m_function;
			ctx.m_currentFunction = &fi;
			TVMCompiler tvm(&ctx);
			// if (tvm.ctx().isPureFunction(_function))
				// continue;
			solAssert(!ctx.isPureFunction(_function), "");
			tvm.generateHeaderAndDecoder(fi);
			tvm.visitFunction(*_function);
			code.append(tvm.m_code);
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
	
	// TODO: move
	auto getConstructorsList() const {
		auto mainContract = ctx().getContract();
		vector<pair<const ContractDefinition*, const FunctionDefinition*>> result;
		for (auto& c : getContractsChain(mainContract)) {
			if (c->name() == "stdlib")
				continue;
			auto constructor = ctx().getConstructor(c);
			result.push_back(make_pair(c, constructor));
		}
		return result;
	}
	
	void generateDefaultConstructors() {
		auto mainContract = ctx().getContract();
		
		for (auto& pair : getConstructorsList()) {
			auto c = pair.first;
			if (/*auto f = */pair.second) {
			} else {
				string fnameInternal = "constructor_" + c->name();
				m_code.generateGlobl(fnameInternal, false);
				callBaseConstructorsImplicit(c);
				setDefaultMemberValues(c);
				push(0, "RET");
				push(0, " ");
			}
		}

		if (!TVMCompilerContext::hasConstructor(mainContract)) {
			// create external constructor
			m_code.generateGlobl("constructor", true);
			generateConstructorProtection();
			decodeParameters(ptr_vec<VariableDeclaration>());
			for (const auto& name : getBaseContractNames(mainContract)) {
				pushPrivateFunctionCall(0, "constructor_" + name);
			}
			setDefaultMemberValues(mainContract);
			pushPersistentDataFromC7ToC4();
			push(0, " ");
		}
	}

	void generateDefaultFallbackFunction() {
		push(0, ".globl	fallback");
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

	void decodeParameters(const ptr_vec<VariableDeclaration>& params) {
		push(0, "; Decode input parameters");
		for (const auto& variable: params) {
			auto savedStackSize = m_stack.size();
			decodeParameter(variable.get());
			m_stack.ensureSize(savedStackSize, "decodeParameter-2");
		}
		push(0, "DROP");
//		push(0, "ENDS");
	}

	void generateHeaderAndDecoder(const FuncInfo& fi) {
		bool dontEmitReturn = !!getFunction(ctx().getContract(), "tvm_dont_emit_events_on_return");
		FunctionDefinition const* _function = fi.m_function;
		auto fnameInternal = fi.m_internalName;
		if (fi.m_needsDecoder) {
			auto fnameExternal = TVMCompilerContext::getFunctionExternalName(_function);
			if (ctx().isMainConstructor(_function)) {
				m_code.generateGlobl(fnameExternal, true);
				generateConstructorProtection();
				decodeParameters(_function->parameters());
			} else {
				m_code.generateGlobl(fnameExternal, _function->visibility() == Declaration::Visibility::Public);
				for (const auto& m : _function->modifiers()) {
					string name = m->name()->name();
					if (ctx().isContractName(name))
						continue;
					// TODO: check that "_" is in the end...
					if (m->arguments())
						cast_error(*m, "Modifiers with parameters are not supported.");
					push(0, "; Modifier " + name);
					for (const ModifierDefinition* modifier : ctx().getContract()->functionModifiers()) {
						if (modifier->name() != name)
							continue;
						// TODO: use PUSHCONT!
						modifier->body().accept(*this);
						if (!doesAlways<Return>(&modifier->body()))
							drop(m_stack.size());
					}
				}
				decodeParameters(_function->parameters());
			}
			pushPrivateFunctionCall(0, fnameInternal);
			if (!dontEmitReturn) {
				emitFunctionResult(_function);
			}
			if (_function->stateMutability() != StateMutability::Pure &&
				_function->stateMutability() != StateMutability::View) {
				pushPersistentDataFromC7ToC4();
			}
			push(0, "RET");
			push(0, " ");
		}
		if (_function->name() == "onTickTock") {
			m_code.generateInternal("onTickTock", -2);
			push(0, "PUSHINT 0");
			push(0, "PUSHINT 0");
			push(0, "PUSHINT 0");
			pushLines(R"(;; Function: store_inbound_stack_and_c4_to_c7_inline
;; param: msg_balance
;; param: src_addr
;; param: sign
PRINTSTR store_sig
TUPLE 0
;; decl: tuple
;; sign
;; stack=4
PUSH s1
TPUSH
;; src_addr
;; stack=4
PUSH s2
TPUSH
;; msg_balance
;; stack=4
PUSH s3
TPUSH
PUSHCTR c7
;; decl: c7
;; tuple
;; stack=5
PUSH s1
TPUSH
PUSHROOT
CTOS
PLDDICT
TPUSH
;; c7
;; stack=5
DUP
POPCTR c7
BLKDROP 5
)");
		} else if (_function->name() == "main_external") {
			m_code.generateInternal("main_external", -1);
		} else if (_function->name() == "main_internal") {
			m_code.generateInternal("main_internal", 0);
		} else {
			m_code.generateGlobl(fnameInternal, false);
		}
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

		encodeFunctionAndParams(
				TVMCompilerContext::getFunctionExternalName(_function),
				types,
				nodes,
				[&](size_t /*idx*/) {
					push(+1, "PUSH s" + toString(params.size() + 1));
				},
				StackPusherHelper::ReasonOfOutboundMessage::FunctionReturnExternal
		);
		pushPrivateFunctionCall(-1, "send_ext_msg");
	}
	
	void decodeStructParameter(const StructType* stype) {
		auto sname = stype->structDefinition().name();
		push(0, ";; struct " + sname);
		if (sname == "TvmCell") {
			push(0, "LDREF");
			return;
		}
		auto members = stype->structDefinition().members();
		// slice
		push(0, "NEWDICT");
		// slice dict
		push(0, "SWAP");
		// dict slice
		for (size_t i = 0; i < members.size(); i++) {
			auto& m = members[i];
			push(0, ";; " + sname + "." + m->name());
			// dict slice
			auto savedStackSize = m_stack.size();
			decodeParameter(m.get());
			m_stack.ensureSize(savedStackSize, "decodeParameter");
			// dict value slice
			push(0, "SWAP");
			// dict slice value 
			auto memberType = getType(m.get());
			if (isIntegralType(memberType)) {
				intToBuilder(memberType);
			}
			// dict slice value'
			pushInt(i);
			// dict slice value' idx
			push(0, "PUSH s3");
			// dict slice value' idx dict
			setDict(isIntegralType(memberType), 8, false);
			push(+1, "");	// fixup stack
			// dict slice dict'
			push(0, "POP s2");
			// dict' slice
		}
		// dict slice
	}

	void decodeParameter(VariableDeclaration const* variable) {
		auto type = getType(variable);
		const Type::Category category = variable->type()->category();
		if (auto stype = to<StructType>(type)) {
			decodeStructParameter(stype);
		} else if (category == Type::Category::Address || category == Type::Category::Contract) {
			pushPrivateFunctionCall(0, "decode_addr");
		} else if (isIntegralType(type)) {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			pushInt(ti.numBits);
			pushPrivateFunctionCall(-1, ti.isSigned? "smart_ldi" : "smart_ldu");
		} else if (auto arrayType = to<ArrayType>(type)) {
			if (arrayType->isByteArray()) {
				push(0, "LDREF");
			} else {
				pushPrivateFunctionCall(0, "decode_array");
			}
		} else {
			cast_error(*variable, "Unsupported parameter type: " + type->toString());
		}
	}

	bool visit(FunctionDefinition const& /*_function*/) override { solAssert(false, ""); }

	void visitFunction(FunctionDefinition const& _function, bool tvm_mode0 = false) {
		string fname = ctx().getFunctionInternalName(&_function);
		if (isTvmIntrinsic(fname))
			return;
		
		solAssert(!ctx().isPureFunction(&_function), "");
		
		push(0, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
		push(0, string(";; Function: ") + fname);
		
		visitFunction2(_function, tvm_mode0);
		
		push(0, " ");
	}

	void visitFunction2(FunctionDefinition const& _function, bool tvm_mode0) {
		for (const auto& m : _function.modifiers()) {
			string name = m->name()->name();
			if (ctx().isContractName(name)) {
				// constructor call is processed in another place
				continue;
			}
			// Ensure that modifier is allowed
			if (_function.isConstructor())
				cast_error(*m, "Modifiers for costructors are not supported.");
			if (!_function.isPublic())
				cast_error(*m, "Modifiers are allowed only for public functions.");
		}

		for (const auto& variable: _function.parameters()) {
			auto name = variable->name();
			push(0, string(";; param: ") + name);
			m_stack.add(name, true);
		}

		bool doFunctionAlwaysReturn = doesAlways<Return>(&_function.body());
		bool haveSomeNamedReturnParams = false;
		for (const auto& returnParam: _function.returnParameters()) {
			haveSomeNamedReturnParams |= !returnParam->name().empty();
		}
		if (!doFunctionAlwaysReturn && haveSomeNamedReturnParams) {
			pushReturnParameters(_function.returnParameters());
		}

		if (_function.isConstructor()) {
			visitConstructor(_function, tvm_mode0);
		}

		_function.body().accept(*this);

		if (!doFunctionAlwaysReturn && haveSomeNamedReturnParams) {
			int paramQty = _function.parameters().size();
			int retQty = _function.returnParameters().size();
			push(0, ";; returning named params");
			drop(m_stack.size() - (paramQty + retQty));
			dropUnder(retQty, paramQty);
		} else if (!doFunctionAlwaysReturn) {
			// if function did not return
			drop(m_stack.size());
			pushReturnParameters(_function.returnParameters());
		}

		if (_function.name() == "onTickTock") {
			if (_function.stateMutability() != StateMutability::Pure &&
			    _function.stateMutability() != StateMutability::View) {
				pushPersistentDataFromC7ToC4();
			}
		}
	}

	void pushReturnParameters(const std::vector<ASTPointer<VariableDeclaration>>& returnParameters) {
		int idParam = 0;
		for (const auto& returnParam: returnParameters) {
			auto name = returnParam->name();
			if (name.empty()) {
				name = "retParam@" + std::to_string(idParam);
			}
			push(0, string(";; ret param: ") + name);
			pushDefaultValue(returnParam.get()->typeName());
			m_stack.add(name, false);

			++idParam;
		}
	}

	void pushDefaultValue(TypeName* typeName) {
		Type::Category cat = typeName->annotation().type->category();
		switch (cat) {
			case Type::Category::Address:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
			case Type::Category::Function:
			case Type::Category::Integer:
				push(+1, "PUSHINT 0");
				break;
			case Type::Category::Array:
			case Type::Category::Mapping:
			case Type::Category::Struct:
				push(+1, "NEWDICT");
				break;
			default:
				solAssert(false, "");
		}
	}

	void generateConstructorProtection() {
		// this is copy pasta from generated file stdlib.code
		pushLines(R"(PRINTSTR constr_prot_0
PUSHINT 0
;; decl: dict
PUSHINT 0
;; decl: previous_message_timestamp
PUSHINT 1800000
;; decl: interval
PUSHROOT
CTOS
;; decl: c4
;; c4
;; stack=4
DUP
LDDICT
POP s2
;; assign dict
POP s4
;; if
;; c4
;; stack=4
DUP
PUSHINT 128
SCHKBITSQ
PUSHCONT {
	;; c4
	;; stack=4
	DUP
	LDI 64
	POP s2
	;; assign previous_message_timestamp
	POP s3
	;; c4
	;; stack=4
	DUP
	LDI 64
	POP s2
	;; assign interval
	POP s2
	;; if
	;; c4
	;; stack=4
	DUP
	PUSHINT 1
	SCHKBITSQ
	PUSHCONT {
		PUSHINT 123
		THROWANY
	}
	IF
}
IF
NEWC
;; decl: c4_builder
;; dict
;; stack=5
PUSH s4
SWAP
STDICT
;; previous_message_timestamp
;; stack=5
PUSH s3
STUR 64
;; interval
;; stack=5
PUSH s2
STUR 64
PUSHINT 1
STONES
;; c4
;; stack=5
PUSH s1
STSLICER
;; c4_builder
;; stack=5
DUP
ENDC
POPROOT
PRINTSTR constr_prot_1
BLKDROP 5)");
		push( 0, "ACCEPT");
	}

	void pushPersistentDataFromC7ToC4()  {
		// copy pasta from generated file stdlib.code
		pushLines(R"(PRINTSTR pushpdc7toc4
PUSHROOT
CTOS
;; decl: c4
SKIPDICT
NEWC
;; decl: c4_builder
PUSHCTR c7
THIRD
SWAP
STDICT
;; c4
;; stack=2
PUSH s1
STSLICER
;; c4_builder
;; stack=2
DUP
ENDC
POPROOT
PRINTSTR pushpdc7toc40
BLKDROP 2
)");
	}

	void visitConstructor(FunctionDefinition const& _function, bool tvm_mode0) {
		if (tvm_mode0) {
			push(+1, "NEWDICT");
			push( 0, "NEWC STDICT ENDC");
			push(-1, "POPROOT");
		}
		// TODO: check if they are not called twice!
		auto contract = ctx().getContract(&_function);
		callBaseConstructorsImplicit(contract);
		callBaseConstructorsExplicit(&_function);
		setDefaultMemberValues(contract);
	}
	
	void callBaseConstructorsImplicit(const ContractDefinition* contract) {
		// TODO: refactor this function!
		set<string> definedConstructors;
		for (auto p : getContractFunctionPairs(contract)) {
			auto f = p.first;
			if (f->isConstructor()) {
				definedConstructors.insert(ctx().getFunctionInternalName(f));
			}
		}
		for (auto name1 : getBaseContractNames(contract)) {
			auto name = "constructor_" + name1;
			if (definedConstructors.count(name) > 0)
				continue;
			pushPrivateFunctionCall(0, name);
		}
	}

	void callBaseConstructorsExplicit(const FunctionDefinition* function) {
		auto contract0 = ctx().getContract(function);
		auto bases = contract0->baseContracts();
		for (auto p : getContractFunctionPairs(contract0)) {
			auto f = p.first;
			if (f->isConstructor() && f != function) {
				auto contract = p.second;
				for (auto m : function->modifiers()) {
					if (m->name()->name() == contract->name()) {
						// add parameters for base constructor
						// TODO: check two base constructors with parameters!
						if (m->arguments()) {
							for (auto expr : *m->arguments()) {
								acceptExpr(expr.get());
								push(-1, "");	// fix stack
							}
						}
					}
				}
				for (auto base: bases)
				{
					if (base->name().namePath()[0] == contract->name())
					{
						if (base->arguments()) {
							for (auto expr : *base->arguments()) {
								acceptExpr(expr.get());
								push(-1, "");	// fix stack
							}
						}
					}
				}
				pushPrivateFunctionCall(0, ctx().getFunctionInternalName(f));
			}
		}
	}
	
	void setDefaultMemberValues(const ContractDefinition* contract) {
		for (VariableDeclaration const* variable: contract->stateVariables()) {
			// Setting default values if needed
			if (auto value = variable->value().get()) {
				push(0, ";; " + variable->name());
				auto type = getType(variable);
				if (!isIntegralType(type)) {
					cast_error(*value, "only int member type is supported!");
				}
				acceptExpr(value);
				setRootItem(variable->name(), type);
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
			string fname = memberAccess->memberName();
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
			acceptExpr(init);
		} else {
			for (const auto& decl : decls) {
				pushDefaultValue(decl.get()->typeName());
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

	void addFalseFlagIfNeeded(CodeLines& code) {
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
		if (auto expr = _return.expression()) {
			acceptExpr(expr);
		}
		push(0, ";; return");
		auto params = _return.annotation().functionReturnParameters->parameters();
		int retCount = params.size();
		dropUnder(retCount, m_stack.size() - retCount);
		
		auto ensureFits = [&] (Expression const* expr) {
			if (!isExpressionExactTypeKnown(expr)) {
				// push(0, toString(";; ") + typeid(*expr).name());
				ASTPointer<VariableDeclaration> param = params[0];
				if (auto typeName = to<ElementaryTypeName>(param->typeName())) {
					if (isUInt256(typeName->typeName()) && isNonNegative(expr)) {
						// no need to check
					} else {
						ensureValueFitsType(typeName->typeName());
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
		ec.encodeOutboundMessageBody(
			name, 
			eventCall->arguments(), 
			event->parameterList().parameters(),
			StackPusherHelper::ReasonOfOutboundMessage::EmitEventExternal);
		pushPrivateFunctionCall(-1, "send_ext_msg");
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
