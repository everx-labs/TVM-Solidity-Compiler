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
 * Function call compiler for TVM
 */

#include "TVMFunctionCall.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMIntrinsics.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMABI.hpp"

#include <boost/algorithm/string.hpp>
#include <libsolidity/ast/TypeProvider.h>

using namespace solidity::frontend;

void FunctionCallCompiler::acceptExpr(const Expression *expr) {
	m_exprCompiler->compileNewExpr(expr);
}

FunctionCallCompiler::FunctionCallCompiler(StackPusherHelper &m_pusher, TVMExpressionCompiler *exprCompiler,
											FunctionCall const& _functionCall) :
		m_pusher{m_pusher},
		m_exprCompiler{exprCompiler},
		m_functionCall{_functionCall},
		m_arguments{_functionCall.arguments()} {

}

void FunctionCallCompiler::structConstructorCall() {
	auto const& type = dynamic_cast<TypeType const&>(*m_functionCall.expression().annotation().type);
	auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
	auto pushParam = [&](int index) {
		acceptExpr(m_arguments.at(index).get());
	};
	StructCompiler structCompiler{&m_pusher, &structType};
	structCompiler.structConstructor(m_functionCall.names(), pushParam);
}

void FunctionCallCompiler::compile() {
	auto ma = to<MemberAccess>(&m_functionCall.expression());
	auto reportError = [&](){
		cast_error(m_functionCall, "Unsupported function call");
	};

	if (checkNewExpression() ||
		checkTvmIntrinsic() ||
		checkAddressThis() ||
		checkSolidityUnits() ||
		checkLocalFunctionCallOrFuncVarCall()) {
	} else if (ma != nullptr && getType(&ma->expression())->category() == Type::Category::Struct) {
		if (!structMethodCall()) {
			reportError();
		}
	} else if (m_functionCall.annotation().kind == FunctionCallKind::StructConstructorCall) {
		structConstructorCall();
	} else if (m_functionCall.annotation().kind == FunctionCallKind::TypeConversion) {
		typeConversion();
	} else {
		if (ma != nullptr) {
			auto category = getType(&ma->expression())->category();
			auto ident = to<Identifier>(&ma->expression());
			if (category == Type::Category::Array) {
				arrayMethods(*ma);
			} else if (category == Type::Category::TvmSlice) {
				sliceMethods(*ma);
			} else if (
				checkForTvmBuilderMethods(*ma, category) ||
				checkForOptionalMethods(*ma))
			{
				// nothing
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
				if (checkForTvmSendFunction(*ma) ||
					checkForTvmConfigParamFunction(*ma) ||
					checkForTvmFunction(*ma) ||
					checkForTvmDeployMethods(*ma, category) ||
					checkForTvmBuildMsgMethods(*ma, category)) {
				} else {
					reportError();
				}
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "rnd") {
				rndFunction(*ma);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "msg") {
				msgFunction(*ma);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "math") {
				mathFunction(*ma);
			} else if (category == Type::Category::Address) {
				addressMethod();
			} else if (category == Type::Category::TvmCell) {
				cellMethods(*ma);
			} else if (isSuper(&ma->expression())) {
				superFunctionCall(*ma);
			} else if (category == Type::Category::TypeType) {
				if (checkBaseContractCall(*ma, category)) {
					// nothing
				} else {
					typeTypeMethods(*ma);
				}
			} else {
				reportError();
			}
		} else {
			reportError();
		}
	}
}

void FunctionCallCompiler::superFunctionCall(MemberAccess const &_node) {
	for (const auto &arg : m_arguments)
		acceptExpr(arg.get());
	m_pusher.push(0, ";; super");

	string fname = _node.memberName();
	auto super = getSuperContract(m_pusher.ctx().getCurrentFunction()->annotation().contract,
								  m_pusher.ctx().getContract(), fname);
	solAssert(super, "#1000");
	if (getFunction(super, fname)) {
		auto functionName = super->name() + "_" + fname;
		m_pusher.push( 0, ";; Super call " + functionName);
		if (auto ft = to<FunctionType>(getType(&_node))) {
			m_pusher.pushCall(functionName, ft);
			return;
		}
	}
	solUnimplemented("");
}

void FunctionCallCompiler::typeTypeMethods(MemberAccess const &_node) {
	if (_node.memberName() == "makeAddrExtern") {
		// addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
		const auto& [isNumConst, num] = TVMExpressionCompiler::constValue(*m_arguments.at(0));
		const auto& [isLenConst, len] = TVMExpressionCompiler::constValue(*m_arguments.at(1));
		if (isNumConst && isLenConst) {
			std::string addr = "01";
			StackPusherHelper::addBinaryNumberToString(addr, u256(len), 9);
			StackPusherHelper::addBinaryNumberToString(addr, u256(num), int(len));
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(addr));
		} else {
			for (const auto &arg : m_arguments) {
				acceptExpr(arg.get());
			}
			m_pusher.push(0, ";; address.makeAddrExtern()");
			m_pusher.push(+1, "DUP"); // numb cntBit cntBit
			m_pusher.push(+1, "NEWC"); // numb cntBit cntBit builder
			m_pusher.push(-1 + 1, "STSLICECONST x6_");
			m_pusher.push(-1, "STU 9"); // numb cntBit builder''
			m_pusher.push(0, "SWAP"); // numb builder'' cntBit
			m_pusher.push(-3 + 1, "STUX"); // builder'''
			m_pusher.push(0, "ENDC");
			m_pusher.push(0, "CTOS"); // extAddress
		}
	} else if (_node.memberName() == "makeAddrNone") {
		m_pusher.push(0, ";; address.makeAddrNone()");
		m_pusher.push(+1, "PUSHSLICE x2_");
	} else if (_node.memberName() == "makeAddrStd") {
		m_pusher.push(0, ";; address.makeAddrStd()");
		const auto& [isWidConst, wid] = TVMExpressionCompiler::constValue(*m_arguments.at(0));
		const auto& [isValConst, val] = TVMExpressionCompiler::constValue(*m_arguments.at(1));
		if (isWidConst && isValConst) {
			std::string addr = "100";
			StackPusherHelper::addBinaryNumberToString(addr, u256(wid), 8);
			StackPusherHelper::addBinaryNumberToString(addr, u256(val), 256);
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(addr));
		} else {
			for (const auto &arg : m_arguments | boost::adaptors::reversed) {
				acceptExpr(arg.get());
			}
			m_pusher.push(+1, "NEWC");
			m_pusher.push(-1 + 1, "STSLICECONST x9_");
			m_pusher.push(-1, "STI 8");
			m_pusher.push(-1, "STU 256");
			m_pusher.push(-1 + 1, "ENDC");
			m_pusher.push(-1 + 1, "CTOS");
		}
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::loadTypeFromSlice(MemberAccess const &_node, TypePointer type) {
	const Type::Category category = type->category();
	auto arrayType = to<ArrayType>(type);
	if (to<TvmCellType>(type) || (arrayType && arrayType->isByteArray())) {
		if (!arrayType)
			m_pusher.push(0, ";; decode TvmCell");
		else if (arrayType->isString())
			m_pusher.push(0, ";; decode string");
		else
			m_pusher.push(0, ";; decode bytes");
		m_pusher.push(+1, "LDREF");
	} else if (auto structType = to<StructType>(type)) {
		ASTString const& structName = structType->structDefinition().name();
		m_pusher.push(0, ";; decode struct " + structName);
		std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
		for (const ASTPointer<VariableDeclaration> &m : members) {
			m_pusher.push(0, ";; decode " + structName + "." + m->name());
			loadTypeFromSlice(_node, m->type());
		}
		m_pusher.push(0, ";; build struct " + structName);
		// members... slice
		const int memberQty = static_cast<int>(members.size());
		m_pusher.blockSwap(memberQty, 1); // slice members...
		m_pusher.tuple(memberQty); // slice struct
		m_pusher.push(0, "SWAP"); // ... struct slice
	} else if (category == Type::Category::Address || category == Type::Category::Contract) {
		m_pusher.push(0, ";; decode address");
		m_pusher.push(+1, "LDMSGADDR");
	} else if (isIntegralType(type)) {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		m_pusher.push(+1, (ti.isSigned ? "LDI " : "LDU ") + toString(ti.numBits));
	} else if (category == Type::Category::Mapping) {
		m_pusher.push(+1, "LDDICT");
	} else {
		cast_error(_node, "Unsupported parameter type for decoding: " + type->toString());
	}
}


std::function<void()> FunctionCallCompiler::generateDataSection(
			std::function<void()> pushKey,
			bool & hasVars,
			ASTPointer<Expression const> vars,
			bool & isNew,
			ASTPointer<Expression const> contr
		) {
	return [pushKey, this, hasVars, vars, isNew, contr]() {
		// creat dict with variable values
		m_pusher.push(+1, "NEWDICT");
		// stake: builder dict

		IntegerType keyType = getKeyTypeOfC4();
		TypePointer valueType = TypeProvider::uint256();

		pushKey();
		const DataType& dataType = m_pusher.prepareValueForDictOperations(&keyType, valueType, false);
		m_pusher.pushInt(0); // index of pubkey
		// stack: dict value key
		m_pusher.push(0, "ROT");
		// stack: value key dict
		m_pusher.setDict(getKeyTypeOfC4(), *valueType, dataType);
		// stack: dict'
		if (hasVars) {
			const Type * type;
			if (!isNew) {
				type = contr->annotation().type;
				auto tt = dynamic_cast<const TypeType*>(type);
				type = tt->actualType();
			} else {
				auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
				auto newExpr = to<NewExpression>(&functionOptions->expression());
				type = newExpr->typeName().annotation().type;
			}
			auto ct = dynamic_cast<const ContractType*>(type);
			std::vector<PragmaDirective const *> _pragmaDirectives;
			PragmaDirectiveHelper pragmaHelper{_pragmaDirectives};
			TVMCompilerContext cc{&ct->contractDefinition(), pragmaHelper};
			std::vector<std::pair<VariableDeclaration const*, int>> staticVars = cc.getStaticVaribles();
			auto getDeclAndIndex = [&](const std::string& name) {
				auto pos = find_if(staticVars.begin(), staticVars.end(), [&](auto v) { return v.first->name() == name; });
				solAssert(pos != staticVars.end(), "");
				return *pos;
			};
			auto initVars = to<InitializerList>(vars.get());
			for (size_t i = 0; i < initVars->names().size(); ++i) {
				const ASTPointer<ASTString> &name = initVars->names().at(i);
				const auto &[varDecl, varIndex] = getDeclAndIndex(*name);
				valueType = varDecl->type();
				acceptExpr(initVars->options().at(i).get());
				const DataType& dataType2 = m_pusher.prepareValueForDictOperations(&keyType, valueType, false);
				m_pusher.pushInt(varIndex);
				// stack: dict value key
				m_pusher.push(0, "ROT");
				// stack: value key dict
				StackPusherHelper sp{&cc, m_pusher.getStack().size()};
				sp.setDict(getKeyTypeOfC4(), *varDecl->type(), dataType2);
				m_pusher.append(sp.code());
				m_pusher.push(-2, ""); // fix stack
				// stack: dict'
			}
		}
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-2 + 1, "STDICT");
		m_pusher.push(-1 + 1, "ENDC");
	};
}


bool FunctionCallCompiler::checkForTvmBuildMsgMethods(MemberAccess const &_node, Type::Category category) {

	auto functionType = dynamic_cast<FunctionType const*>(m_functionCall.expression().annotation().type);
	if (category != Type::Category::Magic || (functionType->kind() != FunctionType::Kind::TVMBuildExtMsg))
		return false;

	if (_node.memberName() == "buildExtMsg") {
		int destArg = -1;
		int callArg = -1;
		int timeArg = -1;
		int expireArg = -1;
		int pubkeyArg = -1;
		int signArg = -1;
		if (!m_functionCall.names().empty()) {
			const std::vector<ASTPointer<ASTString>>& names = m_functionCall.names();
			for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
				switch (str2int(names[arg]->c_str())) {
					case str2int("dest"):
						destArg = arg;
						break;
					case str2int("call"):
						callArg = arg;
						break;
					case str2int("time"):
						timeArg = arg;
						break;
					case str2int("expire"):
						expireArg = arg;
						break;
					case str2int("pubkey"):
						pubkeyArg = arg;
						break;
					case str2int("sign"):
						signArg = arg;
						break;
				}
			}
		}
		auto funcCall = to<CallList>(m_arguments[callArg].get());
		auto functionDefinition = getFunctionDeclarationOrConstructor(funcCall->function().get());
		bool addSignature = false;
		if (signArg != -1) {
			const auto& [ok, value] = TVMExpressionCompiler::constBool(*m_arguments[signArg]);
			if (ok) {
				addSignature = value;
			}
		}

		m_exprCompiler->generateExtInboundMsg(addSignature, m_arguments[destArg].get(),
											  (pubkeyArg != -1) ? m_arguments[pubkeyArg].get() : nullptr,
											  (expireArg != -1) ? m_arguments[expireArg].get() : nullptr,
											  (timeArg != -1) ? m_arguments[timeArg].get() : nullptr,
											  functionDefinition, funcCall->arguments());
		return true;
	}

	return false;
}

bool FunctionCallCompiler::checkForTvmDeployMethods(MemberAccess const &_node, Type::Category category) {
	auto pushArgs = [&]() {
		for (const ASTPointer<const Expression> &e : m_arguments) {
			acceptExpr(e.get());
		}
	};

	auto functionType = dynamic_cast<FunctionType const*>(m_functionCall.expression().annotation().type);
	if (category != Type::Category::Magic || ((functionType->kind() != FunctionType::Kind::TVMDeploy)
			&& functionType->kind() != FunctionType::Kind::TVMBuildStateInit))
		return false;

	if (_node.memberName() == "buildStateInit") {
		int keyArg = -1;
		int varArg = -1;
		int contrArg = -1;
		int codeArg = -1;
		int dataArg = -1;
		int depthArg = -1;
		bool hasVars;
		bool isNew = false;
		std::map<StateInitMembers, std::function<void()>> exprs;
		if (m_functionCall.names().empty()) {
			solAssert(m_arguments.size() == 2 || m_arguments.size() == 3, "");
			exprs[StateInitMembers::Code] = [&](){
				acceptExpr(m_arguments.at(0).get());
			};
			exprs[StateInitMembers::Data] = [&](){
				acceptExpr(m_arguments.at(1).get());
			};
			if (m_arguments.size() >= 3) {
				exprs[StateInitMembers::SplitDepth] = [&](){
					acceptExpr(m_arguments.at(2).get());
				};
			}
		} else {
			const std::vector<ASTPointer<ASTString>>& names = m_functionCall.names();

			bool dataIsSet = false;
			// string("code"), string("data"), string("splitDepth"), string("varInit"), string("pubkey")
			for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
				switch (str2int(names[arg]->c_str())) {
					case str2int("code"):
						codeArg = arg;
						exprs[StateInitMembers::Code] = [&](){
							acceptExpr(m_arguments[codeArg].get());
						};
						break;
					case str2int("data"):
						dataArg = arg;
						exprs[StateInitMembers::Data] = [&](){
							acceptExpr(m_arguments[dataArg].get());
						};
						dataIsSet = true;
						break;
					case str2int("splitDepth"):
						depthArg = arg;
						exprs[StateInitMembers::SplitDepth] = [&](){
							acceptExpr(m_arguments[depthArg].get());
						};
						break;
					case str2int("varInit"):
						varArg = arg;
						break;
					case str2int("pubkey"):
						keyArg = arg;
						break;
					case str2int("contr"):
						contrArg = arg;
						break;
				}
			}
			if (!dataIsSet) {
				auto pushKey = [this, keyArg]() {
					if (keyArg == -1) {
						m_pusher.pushInt(0);
					} else {
						acceptExpr(m_arguments[keyArg].get());
					}
				};
				hasVars = (varArg != -1);
				exprs[StateInitMembers::Data] = generateDataSection(pushKey, hasVars,
																	hasVars ? m_arguments[varArg] : nullptr,
																	isNew,
																	hasVars ? m_arguments[contrArg] : nullptr);
			}
		}

		buildStateInit(exprs);
		return true;
	}

	if (_node.memberName() == "buildEmptyData") {
		pushArgs(); // push public key
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1, ""); // fix stack
		m_pusher.pushLines(R"(
STU 256
PUSHINT 0
NEWDICT
PUSHINT 64
DICTUSETB
)");

		m_pusher.push(+1, "NEWC");
		m_pusher.push(-2 + 1, "STDICT");
		m_pusher.push(0, "ENDC");
		return true;
	}

	if (_node.memberName() == "insertPubkey") {
		pushArgs();
		m_pusher.pushPrivateFunctionOrMacroCall(-2 + 1, "insert_pubkey_macro");
		return true;
	}

	if (_node.memberName() == "deploy") {
		const std::function<void()> pushWid = [&](){
			acceptExpr(m_arguments.at(3).get());
		};
		const std::function<void()> pushValue = [&](){
			acceptExpr(m_arguments.at(2).get());
		};
		const std::function<void(int builderSize)> appendBody = [&](int /*builderSize*/){
			m_pusher.stones(1);
			acceptExpr(m_arguments.at(1).get());
			m_pusher.push(-1, "STREFR");
		};
		const std::function<void()> pushSendrawmsgFlag;
		acceptExpr(m_arguments.at(0).get());
		// stack: stateInit
		deployNewContract(pushWid, pushValue, {}, {}, appendBody, pushSendrawmsgFlag);
		// stack: destAddress
		return true;
	}

	return false;
}

void FunctionCallCompiler::sliceMethods(MemberAccess const &_node) {
	if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.push(-2 + 3, "SDATASIZE");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.pushLines(R"(
SDATASIZEQ
PUSHCONT {
	TRIPLE
}
PUSHCONT {
	NULL
}
IFELSE
)");
		m_pusher.push(-2 + 1, ""); // fix stake
	} else if (_node.memberName() == "size") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+2, "SBITREFS");
	} else if (_node.memberName() == "bits") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "SBITS");
	} else if (_node.memberName() == "refs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "SREFS");
	} else if (_node.memberName() == "depth") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "SDEPTH");
	} else if (_node.memberName() == "decode") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		TypePointers targetTypes;
		if (auto const* targetTupleType = dynamic_cast<TupleType const*>(m_functionCall.annotation().type))
			targetTypes = targetTupleType->components();
		else
			targetTypes = TypePointers{m_functionCall.annotation().type};

		for (auto type: targetTypes) {
			loadTypeFromSlice(_node, type);
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "decodeFunctionParams") {
		const int saveStackSize = m_pusher.getStack().size();
		CallableDeclaration const* functionDefinition = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		const TVMExpressionCompiler::LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(),true, false, _node.expression().annotation().isLValue);
		// lvalue.. slice
		if (functionDefinition) {
			bool hasCallback = !functionDefinition->returnParameters().empty();
			if (hasCallback) {
				m_pusher.push(-1 + 2, "LDU 32");
			}
			// lvalue.. callback slice
			DecodeFunctionParams decoder{&m_pusher};
			decoder.decodeParameters(functionDefinition->parameters(), hasCallback);

			const int saveStackSize2 = m_pusher.getStack().size();
			const int paramQty = functionDefinition->parameters().size() + (hasCallback ? 1 : 0);
			m_pusher.blockSwap(saveStackSize2 - saveStackSize - paramQty, paramQty);

			m_pusher.push(+1, "PUSHSLICE x8_");
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (boost::starts_with(_node.memberName(), "load")) {
		const TVMExpressionCompiler::LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		if (_node.memberName() == "loadRefAsSlice") {
			m_pusher.push(-1 + 2, "LDREFRTOS");
			m_pusher.exchange(0, 1);
		} else if (_node.memberName() == "loadRef") {
			m_pusher.push(-1 + 2, "LDREF");
		} else if (_node.memberName() == "loadUnsigned" || _node.memberName() == "loadSigned") {
			std::string cmd = "LD";
			cmd += (_node.memberName() == "loadSigned" ? "I" : "U");
			const auto&[ok, val] = TVMExpressionCompiler::constValue(*m_arguments[0]);
			if (ok) {
				if (val < 1 || val > 256) {
					cast_error(*m_arguments[0], "The value must be in the range 1 - 256.");
				}
				m_pusher.push(-1 + 2, cmd + " " + val.str());
			} else {
				acceptExpr(m_arguments[0].get());
				m_pusher.push(-2 + 2, cmd + "X");
			}
		} else if (_node.memberName() == "loadTons") {
			m_pusher.push(-1 + 2, "LDVARUINT16");
		} else {
			solUnimplemented("");
		}

		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForTvmBuilderMethods(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TvmBuilder)
		return false;

	if (boost::starts_with(_node.memberName(), "store")) {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);

		if (_node.memberName() == "storeRef") {
			acceptExpr(m_arguments[0].get());
			m_pusher.push(-1, "STBREFR");
		} else if (_node.memberName() == "store") {
			for (const auto& argument: m_arguments) {
				acceptExpr(argument.get());
				m_pusher.store(argument->annotation().type, true, StackPusherHelper::StoreFlag::StoreStructInRef | StackPusherHelper::StoreFlag::StoreStructInOneCell);
			}
		} else if (_node.memberName() == "storeSigned" || _node.memberName() == "storeUnsigned") {
			std::string cmd = "ST";
			cmd += (_node.memberName() == "storeSigned" ? "I" : "U");
			acceptExpr(m_arguments[0].get());
			const auto& [ok, val] = TVMExpressionCompiler::constValue(*m_arguments[1]);
			if (ok) {
				if (val < 1 || val > 256) {
					cast_error(*m_arguments[1], "The value must be in the range 1 - 256.");
				}
				m_pusher.push(-1, cmd + "R " + val.str());
			} else {
				acceptExpr(m_arguments[1].get());
				m_pusher.push(-2, cmd + "XR");
			}
		} else if (_node.memberName() == "storeTons") {
			acceptExpr(m_arguments[0].get());
			m_pusher.push(-1, "STGRAMS");
		} else {
			solUnimplemented("");
		}

		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	acceptExpr(&_node.expression()); // TODO may return false?

	if (_node.memberName() == "bits") {
		m_pusher.push(-1+1, "BBITS");
		return true;
	}

	if (_node.memberName() == "refs") {
		m_pusher.push(-1+1, "BREFS");
		return true;
	}

	if (_node.memberName() == "bitsAndRefs") {
		m_pusher.push(-1+2, "BBITREFS");
		return true;
	}

	if (_node.memberName() == "remBits") {
		m_pusher.push(-1+1, "BREMBITS");
		return true;
	}

	if (_node.memberName() == "remRefs") {
		m_pusher.push(-1+1, "BREMREFS");
		return true;
	}

	if (_node.memberName() == "remBitsAndRefs") {
		m_pusher.push(-1+2, "BREMBITREFS");
		return true;
	}

	if (_node.memberName() == "toCell") {
		m_pusher.push(-1+1, "ENDC");
		return true;
	}

	if (_node.memberName() == "toSlice") {
		m_pusher.push(-1+1, "ENDC");
		m_pusher.push(-1+1, "CTOS");
		return true;
	}

	if (_node.memberName() == "depth") {
		m_pusher.push(-1 + 1, "BDEPTH");
		return true;
	}

	return false;
}

void FunctionCallCompiler::arrayMethods(MemberAccess const &_node) {
	if (_node.memberName() == "substr") {
		acceptExpr(&_node.expression());
		m_pusher.push(+1 - 1, "CTOS");
		for (const auto &arg : m_arguments) {
			acceptExpr(arg.get());
			m_pusher.push(+1 - 1, "MULCONST 8");
		}
		m_pusher.push(-3 + 1, "SDSUBSTR");
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1, "STSLICE");
		m_pusher.push(+1 - 1, "ENDC");
	} else if (_node.memberName() == "byteLength") {
		acceptExpr(&_node.expression());
		m_pusher.push(+1 - 1, "CTOS");
		m_pusher.push(-1 + 1, "SBITS");
		m_pusher.push(-1 + 1, "RSHIFT 3");
	} else if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.push(-2  + 3, "CDATASIZE");
	} else if (_node.memberName() == "toSlice") {
		m_exprCompiler->compileNewExpr(&_node.expression());
		m_pusher.push(-1 + 1, "CTOS");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.pushLines(R"(
CDATASIZEQ
PUSHCONT {
	TRIPLE
}
PUSHCONT {
	NULL
}
IFELSE
)");
		m_pusher.push(-2 + 1, ""); // fix stake
	} else if (_node.memberName() == "push") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		auto arrayBaseType = to<ArrayType>(getType(&_node.expression()))->baseType();
		const IntegerType key = getKeyTypeOfArray();
		bool isValueBuilder{};
		if (m_functionCall.arguments().empty()) {
			isValueBuilder = true;
			m_pusher.pushDefaultValue(arrayBaseType, true);
		} else {
			isValueBuilder = false;
			m_exprCompiler->compileNewExpr(m_functionCall.arguments()[0].get());
		}
		// stack: arr value
		m_pusher.push(0, ";; array.push(..)");
		const DataType& dataType = m_pusher.prepareValueForDictOperations(&key, arrayBaseType, isValueBuilder); // arr value'
		m_pusher.exchange(0, 1); // value' arr
		m_pusher.push(-1 + 2, "UNPAIR");  // value' size dict
		m_pusher.push(+1, "PUSH S1"); // value' size dict size
		m_pusher.push(0, "INC"); // value' size dict newSize
		m_pusher.exchange(0, 3); // newSize size dict value'
		m_pusher.push(0, "ROTREV"); // newSize value' size dict
		m_pusher.setDict(key, *arrayBaseType, dataType); // newSize dict'
		m_pusher.push(-2 + 1, "PAIR");  // arr
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "pop") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		// arr
		m_pusher.push(-1 + 2, "UNPAIR"); // size dict
		m_pusher.push(+1, "PUSH s1"); // size dict size
		m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
		m_pusher.push(0, "SWAP"); // dict size
		m_pusher.push(0, "DEC"); // dict newSize
		m_pusher.push(0, "DUP"); // dict newSize newSize
		m_pusher.push(+1, "ROT"); // newSize newSize dict
		m_pusher.pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
		m_pusher.push(-3 + 2, "DICTUDEL"); // newSize dict ?
		m_pusher.drop(1);  // newSize dict
		m_pusher.push(-2 + 1, "PAIR");  // arr
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "append") {
		m_pusher.push(0, ";; string.append");
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		acceptExpr(m_arguments.at(0).get());
		m_pusher.push(-1, "PUSHINT 1");
		m_pusher.push(0, "ROT");
		// cell with original string
		m_pusher.push(0, "CTOS");
		// slice with original string
		m_pusher.startContinuation();
		m_pusher.push(0, "PUSH S0");
		m_pusher.push(0, "SREFS");
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "LDREF");
		m_pusher.push(0, "NEWC");
		m_pusher.push(0, "STSLICE");

		m_pusher.push(0, "XCHG S2");
		m_pusher.push(0, "INC");
		m_pusher.push(0, "SWAP");

		m_pusher.push(0, "CTOS");
		m_pusher.endContinuation();
		m_pusher.push(0, "WHILE");
		m_pusher.push(0, "NEWC");
		m_pusher.push(0, "STSLICE");
		// builder_0
		// ...
		// builder_lastb1
		// number of builders
		// builder_last
		m_pusher.push(0, "PUSH S0");
		m_pusher.push(0, "BREMBITS");
		m_pusher.push(0, "LESSINT 8");
		// if less than 8 remaining bits create new builder
		m_pusher.startContinuation();
		m_pusher.push(0, "SWAP");
		m_pusher.push(0, "INC");
		m_pusher.push(0, "NEWC");
		m_pusher.endContinuation();
		m_pusher.push(0, "IF");
		// tail
		// builder_0
		// ...
		// number of builders
		// builder_last (not full)
		m_pusher.push(0, "PUSH S1");
		m_pusher.push(0, "INC");
		m_pusher.push(0, "ROLLX");

		// cell with tail string
		m_pusher.push(0, "CTOS");
		// slice with tail string
		m_pusher.startContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "PUSH S0");
		m_pusher.push(0, "SBITS");
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "LDU 8");
		m_pusher.push(0, "ROTREV");
		m_pusher.push(0, "STUR 8");
		m_pusher.push(0, "PUSH S0");
		m_pusher.push(0, "BREMBITS");
		m_pusher.push(0, "LESSINT 8");
		// if less than 8 remaining bits create new builder
		m_pusher.startContinuation();
		m_pusher.push(0, "XCHG S2");
		m_pusher.push(0, "INC");
		m_pusher.push(0, "SWAP");
		m_pusher.push(0, "NEWC");
		m_pusher.endContinuation();
		m_pusher.push(0, "IF");
		m_pusher.push(0, "SWAP");
		m_pusher.endContinuation();
		m_pusher.push(0, "WHILE");
		m_pusher.push(0, "PUSH S0");
		m_pusher.push(0, "SREFS");
		m_pusher.startContinuation();
		m_pusher.push(0, "LDREF");
		m_pusher.push(0, "DROP");
		m_pusher.push(0, "CTOS");
		m_pusher.push(0, "PUSHINT 0");
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "DROP");
		m_pusher.push(0, "PUSHINT 1");
		m_pusher.endContinuation();
		m_pusher.push(0, "IFELSE");
		m_pusher.endContinuation();
		m_pusher.push(0, "UNTIL");
		m_pusher.push(0, "SWAP");
		m_pusher.push(0, "DEC");
		// builder_0
		// ...
		// builder_last
		// number of builders - 1

		m_pusher.startContinuation();
		m_pusher.push(0, "PUSH S0");
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "DEC");
		m_pusher.push(0, "BLKSWAP 2,1");
		m_pusher.push(0, "STBREFR");
		m_pusher.push(0, "SWAP");
		m_pusher.endContinuation();
		m_pusher.push(0, "WHILE");
		m_pusher.push(0, "DROP");
		m_pusher.push(0, "ENDC");
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForOptionalMethods(MemberAccess const &_node) {
	auto optional = to<OptionalType>(_node.expression().annotation().type);
	if (!optional)
		return false;

	if (_node.memberName() == "hasValue") {
		acceptExpr(&_node.expression());
		m_pusher.push(+1 - 1, "ISNULL");
		m_pusher.push(0, "NOT");
		return true;
	}

	if (_node.memberName() == "get") {
		acceptExpr(&_node.expression());
		m_pusher.pushS(0);
		m_pusher.checkOptionalValue();
		Type::Category valueCat = m_functionCall.annotation().type->category();
		if (auto tt = to<TupleType>(m_functionCall.annotation().type)) {
			m_pusher.untuple(tt->components().size());
		} else if (isIn(valueCat, Type::Category::Mapping, Type::Category::Optional)) {
			m_pusher.untuple(1);
		}
		return true;
	}

	if (_node.memberName() == "set") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), false);
		for (const ASTPointer<Expression const>& arg : m_arguments) {
			acceptExpr(arg.get());
		}
		Type::Category val0Cat = m_arguments.at(0)->annotation().type->category();
		if (m_arguments.size() >= 2) {
			m_pusher.tuple(m_arguments.size());
		} else if (isIn(val0Cat, Type::Category::Mapping, Type::Category::Optional)) {
			m_pusher.tuple(1);
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "reset") {
		const TVMExpressionCompiler::LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), false);
		m_pusher.pushDefaultValue(optional);
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	return false;
}

void FunctionCallCompiler::cellMethods(MemberAccess const &_node) {
	if (_node.memberName() == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "CTOS");
	} else if (_node.memberName() == "depth") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "CDEPTH");
	} else if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.push(-2 + 3, "CDATASIZE");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		acceptExpr(m_arguments.at(0).get());
		m_pusher.pushLines(R"(
CDATASIZEQ
PUSHCONT {
	TRIPLE
}
PUSHCONT {
	NULL
}
IFELSE
)");
		m_pusher.push(-2 + 1, ""); // fix stake
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethod() {
	auto _node = to<MemberAccess>(&m_functionCall.expression());
	const std::vector<ASTPointer<ASTString>>& names = m_functionCall.names();
	if (_node->memberName() == "transfer") { // addr.transfer(...)
		m_pusher.push(0, ";; transfer()");

		std::map<int, Expression const *> exprs;
		std::map<int, std::string> constParams{{TvmConst::int_msg_info::ihr_disabled, "1"}, {TvmConst::int_msg_info::bounce, "1"}};
		std::function<void(int)> appendBody;
		std::function<void()> pushSendrawmsgFlag;

		auto setValue = [&](Expression const* expr) {
			const auto& [ok, value] = TVMExpressionCompiler::constValue(*expr);
			if (ok) {
				constParams[TvmConst::int_msg_info::tons] = StackPusherHelper::tonsToBinaryString(u256(value));
			} else {
				exprs[TvmConst::int_msg_info::tons] = expr;
			}
		};

		auto setBounce = [&](auto expr){
			const auto& [ok, value] = TVMExpressionCompiler::constBool(*expr);
			if (ok) {
				constParams[TvmConst::int_msg_info::bounce] = value? "1" : "0";
			} else {
				exprs[TvmConst::int_msg_info::bounce] = expr;
				constParams.erase(TvmConst::int_msg_info::bounce);
			}
		};

		exprs[TvmConst::int_msg_info::dest] = &_node->expression();

		int argumentQty = static_cast<int>(m_arguments.size());
		if (!m_functionCall.names().empty() || argumentQty == 0) {
			// string("value"), string("bounce"), string("flag"), string("body"), string("currencies")
			for (int arg = 0; arg < argumentQty; ++arg) {
				switch (str2int(names[arg]->c_str())) {
					case str2int("value"):
						setValue(m_arguments[arg].get());
						break;
					case str2int("bounce"):
						setBounce(m_arguments[arg].get());
						break;
					case str2int("flag"):
						pushSendrawmsgFlag = [e = m_arguments[arg], this](){
							TVMExpressionCompiler{m_pusher}.compileNewExpr(e.get());
						};
						break;
					case str2int("body"):
						appendBody = [e = m_arguments[arg], this](int /*size*/){
							m_pusher.stones(1);
							TVMExpressionCompiler{m_pusher}.compileNewExpr(e.get());
							m_pusher.push(-1, "STREFR");
							return false;
						};
						break;
					case str2int("currencies"):
						exprs[TvmConst::int_msg_info::currency] = m_arguments[arg].get();
						break;
				}
			}
		} else {
			solAssert(1 <= argumentQty && argumentQty <= 4, "");
			setValue(m_arguments[0].get());
			if (argumentQty >= 2) {
				setBounce(m_arguments[1].get());
			}
			if (argumentQty >= 3) {
				pushSendrawmsgFlag = [&]() { acceptExpr(m_arguments[2].get()); };
			}
			if (argumentQty >= 4) {
				appendBody = [&](int /*size*/) {
					m_pusher.stones(1);
					acceptExpr(m_arguments[3].get());
					m_pusher.push(-1, "STREFR");
					return false;
				};
			}
		}
		m_pusher.sendIntMsg(exprs, constParams, appendBody, pushSendrawmsgFlag);
	} else if (_node->memberName() == "isStdZero") {
		m_pusher.push(0, ";; address.isStdZero()");
		acceptExpr(&_node->expression());
		m_pusher.pushZeroAddress();
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "isExternZero") {
		m_pusher.push(0, ";; address.isExternZero()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1, "PUSHSLICE x401_");
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "isNone") {
		m_pusher.push(0, ";; address.isNone()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1, "PUSHSLICE x2_");
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "unpack") {
		m_pusher.push(0, ";; address.unpack()");
		acceptExpr(&_node->expression());
		m_pusher.pushPrivateFunctionOrMacroCall(-1 + 2, "unpack_address_macro");
	} else if (_node->memberName() == "getType") {
		m_pusher.push(0, ";; address.getType()");
		acceptExpr(&_node->expression());
		m_pusher.push(+1 - 1, "PLDU 2");
	} else if (_node->memberName() == "isStdAddrWithoutAnyCast") {
		m_pusher.push(0, ";; addr.isStdAddrWithoutAnyCast()");
		acceptExpr(&_node->expression());
		// t = (2, u, x, s); check t[0] == 2 and t[1] is null
		m_pusher.pushLines(R"(
PARSEMSGADDR
DUP
FIRST
EQINT 2
PUSHCONT {
	SECOND
	ISNULL
}
PUSHCONT {
	DROP
	FALSE
}
IFELSE
)");

	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForTvmConfigParamFunction(MemberAccess const &_node) {
	if (_node.memberName() == "rawConfigParam") { // tvm.rawConfigParam
		acceptExpr(m_arguments[0].get());
		m_pusher.push(-1 + 2, "CONFIGPARAM");
		m_pusher.push(0, "NULLSWAPIFNOT");
		return true;
	}
	if (_node.memberName() == "configParam") { // tvm.configParam
		auto paramNumberLiteral = dynamic_cast<const Literal *>(m_arguments[0].get());

		Type const* type = paramNumberLiteral->annotation().type;
		u256 value = type->literalValue(paramNumberLiteral);
		std::string paramNumber = value.str();

		//	function tvm_config_param1() pure private returns (uint256, bool) { }
		if (paramNumber == "1") {
			//_ elector_addr:bits256 = ConfigParam 1;
			m_pusher.push(0, "PUSHINT 1");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 256");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 2, "IFELSE");
		}

		//	function tvm_config_param15() pure private returns (uint32, uint32, uint32, uint32, bool) { }
		//	function tvm_config_param17() pure private returns (uint32, uint32, uint32, uint32, bool) { }
		if (paramNumber == "15") {
			//_ validators_elected_for:uint32 elections_start_before:uint32
			//  elections_end_before:uint32 stake_held_for:uint32
			//  = ConfigParam 15;
			m_pusher.push(0, "PUSHINT 15");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("LDU 32");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 5, "IFELSE");
		}

		if (paramNumber == "17"){
			//_    min_stake:Grams    max_stake:Grams
			//     min_total_stake:Grams    max_stake_factor:uint32 = ConfigParam 17;
			m_pusher.push(0, "PUSHINT 17");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDGRAMS");
			contOk.push("LDGRAMS");
			contOk.push("LDGRAMS");
			contOk.push("LDU 32");
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			contFail.push("PUSHINT 0");
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 5, "IFELSE");
		}

		//    function tvm_config_param34() private pure returns (
		//        uint8 /*constructor_id*/,
		//        uint32 /*utime_since*/,
		//        uint32 /*utime_until*/,
		//        uint16 /*total*/,
		//        uint16 /*main*/,
		//        uint64 /*total_weight*/,
		//        mapping(uint16 => ValidatorDescr73) memory,
		//        bool ok
		//    ) { }
		if (paramNumber == "34") {
			// _ cur_validators:ValidatorSet = ConfigParam 34;
			// validators#11 utime_since:uint32 utime_until:uint32
			// total:(## 16) main:(## 16) { main <= total } { main >= 1 }
			// list:(Hashmap 16 ValidatorDescr) = ValidatorSet;
			// validators_ext#12 utime_since:uint32 utime_until:uint32
			// total:(## 16) main:(## 16) { main <= total } { main >= 1 }
			// total_weight:uint64 list:(HashmapE 16 ValidatorDescr) = ValidatorSet;
			// validator#53 public_key:SigPubKey weight:uint64 = ValidatorDescr;
			// validator_addr#73 public_key:SigPubKey weight:uint64 adnl_addr:bits256 = ValidatorDescr;
			// ed25519_pubkey#8e81278a pubkey:bits256 = SigPubKey;  // 288 bits

			m_pusher.push(0, "PUSHINT 34");
			m_pusher.push(0, "CONFIGPARAM");
			CodeLines contOk;
			contOk.push("CTOS");
			contOk.push("LDU 8"); // constructor
			contOk.push("LDU 32"); // utime_since
			contOk.push("LDU 32"); // utime_until
			contOk.push("LDU 16"); // total
			contOk.push("LDU 16"); // main
			contOk.push("LDU 64"); // total_weight
			contOk.push("LDDICT"); // ValidatorDescr
			contOk.push("ENDS");
			contOk.push("PUSHINT -1");
			CodeLines contFail;
			contFail.push("PUSHINT 0"); // constructor
			contFail.push("PUSHINT 0"); // utime_since
			contFail.push("PUSHINT 0"); // utime_until
			contFail.push("PUSHINT 0"); // total
			contFail.push("PUSHINT 0"); // main
			contFail.push("PUSHINT 0"); // total_weight
			contFail.push("NEWDICT"); // ValidatorDescr
			contFail.push("PUSHINT 0"); //
			m_pusher.pushCont(contOk);
			m_pusher.pushCont(contFail);
			m_pusher.push(-2 + 8, "IFELSE");
		}
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkForTvmSendFunction(MemberAccess const &_node) {
	if (_node.memberName() == "sendrawmsg") { // tvm.sendrawmsg
		for (const auto &arg : m_arguments) {
			acceptExpr(arg.get());
		}
		m_pusher.push(-2, "SENDRAWMSG");
	}  else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::msgFunction(MemberAccess const &_node) {
	if (_node.memberName() == "pubkey") { // msg.pubkey
		m_pusher.pushLines(R"(
GETGLOB 5
DUP
ISNULL
PUSHCONT {
	DROP
	PUSHINT 0
}
IF
)");
		m_pusher.push(+1, ""); // fix stack
	} else {
		cast_error(_node, "Unsupported function call");
	}
}

void FunctionCallCompiler::rndFunction(MemberAccess const &_node) {
	Type const* expressionType = getType(&m_functionCall.expression());
	auto functionType = dynamic_cast<FunctionType const*>(expressionType);
	switch (functionType->kind()) {
		case FunctionType::Kind::RndNext:
			if (m_arguments.empty()) {
				m_pusher.push(+1, "RANDU256");
			} else {
				acceptExpr(m_arguments.at(0).get());
				m_pusher.push(-1 + 1, "RAND");
			}
			break;
		case FunctionType::Kind::RndSetSeed:
		{
			acceptExpr(m_arguments.at(0).get());
			m_pusher.push(-1, "SETRAND");
			break;
		}
		case FunctionType::Kind::RndGetSeed:
		{
			m_pusher.push(+1, "RANDSEED");
			break;
		}
		case FunctionType::Kind::RndShuffle:
		{
			if (m_arguments.empty()) {
				m_pusher.push(+1, "LTIME");
			} else {
				acceptExpr(m_arguments.at(0).get());
			}
			m_pusher.push(-1, "ADDRAND");
			break;
		}
		default:
			cast_error(_node, "Unsupported function call");
	}
}

bool FunctionCallCompiler::checkForTvmFunction(const MemberAccess &_node) {
	auto pushArgs = [&]() {
		for (const ASTPointer<const Expression> &e : m_arguments) {
			acceptExpr(e.get());
		}
	};

	if (_node.memberName() == "pubkey") { // tvm.pubkey
		m_pusher.push(+1, "GETGLOB 2");
	} else if (_node.memberName() == "accept") { // tvm.accept
		m_pusher.push(0, "ACCEPT");
	} else if (_node.memberName() == "hash") { // tvm.hash
		pushArgs();
		m_pusher.push(0, "HASHCU");
	} else if (_node.memberName() == "checkSign") { // tvm.checkSign
		size_t cnt = m_arguments.size();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmSlice) {
			pushArgs();
			m_pusher.push(-3+1, "CHKSIGNS");
		} else {
			acceptExpr(m_arguments[0].get());
			if (cnt == 4) {
				m_pusher.push(+1, "NEWC");
				acceptExpr(m_arguments[1].get());
				m_pusher.push(-1, "STUR 256");
				acceptExpr(m_arguments[2].get());
				m_pusher.push(-1, "STUR 256");
				m_pusher.push(0, "ENDC");
				m_pusher.push(0, "CTOS");
			} else {
				acceptExpr(m_arguments[1].get());
			}
			acceptExpr(m_arguments[cnt - 1].get());
			m_pusher.push(-3+1, "CHKSIGNU");
		}
	} else if (_node.memberName() == "setcode") { // tvm.setcode
		pushArgs();
		m_pusher.push(-1, "SETCODE");
	} else if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
		pushArgs();
		m_pusher.push(-1+1, "CTOS");
		m_pusher.push(0, "BLESS");
		m_pusher.push(-1, "POP c3");
	} else if (_node.memberName() == "commit") { // tvm.commit
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
		m_pusher.push(0, "COMMIT");
	} else if (_node.memberName() == "log") { // tvm.log
		auto logstr = m_arguments[0].get();
		if (auto literal = to<Literal>(logstr)) {
			if (literal->value().length() > 15)
				cast_error(_node, "Parameter string should have length no more than 15 chars");
			if (!TVMContractCompiler::g_without_logstr) {
				m_pusher.push(0, "PRINTSTR " + literal->value());
			}
		} else {
			cast_error(_node, "Parameter should be a literal");
		}
	} else if (_node.memberName() == "resetStorage") { //tvm.resetStorage
		m_pusher.resetAllStateVars();
	} else if (_node.memberName() == "functionId") { // tvm.functionId
		auto callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		EncodeFunctionParams encoder(&m_pusher);
		uint32_t funcID;
		bool isManuallyOverridden{};
		if (callDef == nullptr) {
			funcID = encoder.calculateConstructorFunctionID();
		} else {
			std::tie(funcID, isManuallyOverridden) = encoder.calculateFunctionID(callDef);
		}
		if (!isManuallyOverridden) {
			funcID &= 0x7FFFFFFFu;
		}
		m_pusher.pushInt(funcID);
	} else if (_node.memberName() == "encodeBody") { // tvm.encodeBody
		m_pusher.push(+1, "NEWC");
		CallableDeclaration const* callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		if (callDef == nullptr) { // if no constructor (default constructor)
			EncodeFunctionParams{&m_pusher}.createDefaultConstructorMessage2();
		} else {
			const bool needCallback = !callDef->returnParameters().empty();
			const int shift = needCallback ? 1 : 0;
			std::optional<uint32_t> callbackFunctionId;
			if (needCallback) {
				CallableDeclaration const* callback = getFunctionDeclarationOrConstructor(m_arguments.at(1).get());
				callbackFunctionId = EncodeFunctionParams{&m_pusher}.calculateFunctionIDWithReason(callback, ReasonOfOutboundMessage::RemoteCallInternal);
			}
			std::vector<Type const *> types = getParams(callDef->parameters()).first;
			EncodePosition position{32, types};
			const ast_vec<VariableDeclaration> &parameters = callDef->parameters();
			EncodeFunctionParams{&m_pusher}.createMsgBody(
				[&](size_t idx) {
					m_pusher.push(0, ";; " + parameters[idx]->name());
					TVMExpressionCompiler{m_pusher}.compileNewExpr(m_arguments[1 + shift + idx].get());
				},
				convertArray(callDef->parameters()),
				EncodeFunctionParams{&m_pusher}.calculateFunctionIDWithReason(callDef, ReasonOfOutboundMessage::RemoteCallInternal),
				callbackFunctionId,
				position
			);
		}
		m_pusher.push(+1 - 1, "ENDC");
	} else if (_node.memberName() == "rawReserve") {
		pushArgs();
		int n = m_arguments.size();
		solAssert(isIn(n, 2, 3), "");
		m_pusher.push(-n, n == 2? "RAWRESERVE" : "RAWRESERVEX");
	} else if (isIn(_node.memberName(), "exit", "exit1")) {
		m_pusher.getGlob(1);
		m_pusher.push(-1 + 1, "ISNULL");
		m_pusher.push(-1, ""); // fix stack
		m_pusher.startContinuation();
		m_pusher.pushPrivateFunctionOrMacroCall(0, "c7_to_c4");
		m_pusher.endContinuation();
		m_pusher.push(0, "IFNOT");
		if (_node.memberName() == "exit")
			m_pusher.push(0, "THROW 0");
		else
			m_pusher.push(0, "THROW 1");
	} else {
		return false;
	}
	return true;
}

std::string checkValFitsType(TypePointer type) {
	TypeInfo ti{type};
	return (ti.isSigned ? "FITS " : "UFITS ") + toString(ti.numBits);
}

void FunctionCallCompiler::mathFunction(const MemberAccess &_node) {
	auto pushArgs = [&]() {
		for (const ASTPointer<const Expression> &e : m_arguments) {
			acceptExpr(e.get());
		}
	};
	auto ret = m_functionCall.annotation().type;
	if (_node.memberName() == "max") {
		pushArgs();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher.push(-2 + 1, "MAX");
	} else if (_node.memberName() == "min") {
		pushArgs();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher.push(-2 + 1, "MIN");
	} else if (_node.memberName() == "minmax") {
		pushArgs();
		m_pusher.push(-2 + 2, "MINMAX");
	} else if (isIn(_node.memberName(), "divr", "divc")) {
		pushArgs();
		m_pusher.push(-2 + 1, boost::to_upper_copy<std::string>(_node.memberName()));
	} else if (isIn(_node.memberName(), "muldiv", "muldivr", "muldivc")) {
		pushArgs();
		m_pusher.push(-3 + 1, boost::to_upper_copy<std::string>(_node.memberName()));
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.push(0, checkValFitsType(ret));
		}
	} else if (_node.memberName() == "muldivmod") {
		pushArgs();
		m_pusher.push(-3 + 2, "MULDIVMOD");
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			auto retTuple = to<TupleType>(ret);
			m_pusher.push(0, checkValFitsType(retTuple->components()[0]));
		}
	} else if (_node.memberName() == "abs") {
		pushArgs();
		m_pusher.push(-1 + 1, "ABS");
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.push(0, checkValFitsType(ret));
		}
	} else if (_node.memberName() == "modpow2") {
		acceptExpr(m_arguments[0].get());
		const Expression * expression = m_arguments[1].get();
		const auto& [ok, value] = TVMExpressionCompiler::constValue(*expression);
		if (ok) {
			if (value < 0 || value >= 256) {
				cast_error(m_functionCall, "Second argument must be in the range 1 - 255.");
			}
			m_pusher.push(-1 + 1, "MODPOW2 " + value.str());
		} else {
			cast_error(m_functionCall, "Second argument must be a constant integer.");
		}
	} else {
		cast_error(m_functionCall, "Unsupported function call");
	}
}

bool FunctionCallCompiler::checkBaseContractCall(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TypeType)
		return false;
	if (auto identifier = to<Identifier>(&_node.expression())) {
		const auto& iname = identifier->name();
		if (auto functionType = to<FunctionType>(getType(&_node))) {
			// calling base contract method
			auto functionName = iname + "_" + _node.memberName();
			m_pusher.pushCall(functionName, functionType);
			return true;
		}
	}
	return false;
}

bool FunctionCallCompiler::checkAddressThis() {
	// compile  "address(this)"
	if (isAddressThis(&m_functionCall)) {
		m_pusher.push(+1, "MYADDR");
		return true;
	}
	return false;
}

std::string convertToStr(bool hex = true, bool endc = true, bool leading_zeros = false, int digits_cnt = 64) {
	std::string ret = R"(
;; convert int to string
DUP
LESSINT 0
SWAP
ABS
PUSHINT 1
PUSHCONT {
	PUSH S1
	PUSHINT )" + to_string(hex ? 16 : 10) + R"(
	GEQ
}
PUSHCONT {
	INC
	SWAP
	PUSHINT )" + to_string(hex ? 16 : 10) + R"(
	DIVMOD
	XCHG S2
}
WHILE
NEWC
)";
	if (leading_zeros) {
		ret += "PUSHINT " + to_string(digits_cnt) + "\n";
		ret += R"(
PUSH S2
SUB
DUP
ISNEG
PUSHCONT {
	DROP
	ZERO
}
IF
PUSHCONT {
	PUSHINT 48
	STUR 8
}
REPEAT
)";
	}
	ret += string(R"(
PUSHCONT {
	PUSH S1
	NEQINT 0
}
PUSHCONT {
	SWAP
	DEC
	XCHG S2)")
	+ (hex ? R"(
	DUP
	PUSHINT 10
	GEQ
	PUSHCONT {
		ADDCONST 55
	}
	PUSHCONT {
		ADDCONST 48
	}
	IFELSE)"
	: R"(
	ADDCONST 48)") + R"(
	STUR 8
}
WHILE
NIP
SWAP
PUSHCONT {
	NEWC
	STSLICECONST x2D
	STB
}
IF
)";
	if (endc)
		ret += "ENDC\n";
	return ret;
}

void FunctionCallCompiler::typeConversion() {
	auto printError = [&]() {
		const std::string from = m_arguments[0]->annotation().type->toString(true);
		const std::string to = m_functionCall.annotation().type->toString(true);
		cast_error(m_functionCall, "Unsupported casting from " + from + " to " + to + ".");
	};

	Type::Category argCategory = m_arguments[0]->annotation().type->category();
	Type const* argType = m_arguments[0]->annotation().type;
	Type const* resultType = m_functionCall.annotation().type;

	auto acceptArg = [this] () {
		for (const auto &arg : m_arguments)
			acceptExpr(arg.get());
		solAssert(m_arguments.size() == 1, "");
	};
	auto conversionToAddress = [&](){
		switch (argCategory) {
			case Type::Category::Contract:
			case Type::Category::Address:
				acceptArg();
				break;
			case Type::Category::RationalNumber:
			case Type::Category::Integer: {
				auto literal = to<Literal>(m_arguments[0].get());
				if (literal) {
					m_pusher.literalToSliceAddress(literal);
				} else {
					acceptArg();
					m_pusher.push(+1, "NEWC");
					m_pusher.push(-1 + 1, "STSLICECONST x801_"); // addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 // 10 0  00000000 1 = 801
					m_pusher.push(-1, "STU 256"); // address:bits256
					m_pusher.push(-1 + 1, "ENDC");
					m_pusher.push(-1 + 1, "CTOS");
				}
				break;
			}
			default:
				printError();
		}
	};

	if (auto etn = to<ElementaryTypeNameExpression>(&m_functionCall.expression())) {
		auto defaultActionForCasting = [&acceptArg, &etn, this]() {
			acceptArg();
			acceptExpr(etn);
		};
		switch (etn->type().typeName().token()) {
			case Token::BytesM: {
				acceptArg();
				int diff = 0;
				if (argCategory == Type::Category::FixedBytes) {
					auto fixedBytesType = to<FixedBytesType>(m_arguments[0]->annotation().type);
					diff = 8 * static_cast<int>(etn->type().typeName().firstNumber()) -
						   8 * static_cast<int>(fixedBytesType->storageBytes());
				} else if (argCategory == Type::Category::Address) {
					cast_error(m_functionCall, "Unsupported type conversion. Use address.wid or address.value.");
				} else {
					printError();
				}

				if (diff > 0) {
					m_pusher.push(0, "LSHIFT " + std::to_string(diff));
				} else if (diff < 0) {
					m_pusher.push(0, "RSHIFT " + std::to_string(-diff));
				}
				break;
			}
			case Token::Address: {
				conversionToAddress();
				break;
			}
			case Token::IntM:
			case Token::UIntM: {
				auto a = to<IntegerType>(argType);
				auto r = to<IntegerType>(resultType);
				if (a && r && a->isSigned() == r->isSigned() && a->numBits() <= r->numBits()) {
					// nothing to do here
					acceptArg();
				} else {
					defaultActionForCasting();
				}
				break;
			}
			case Token::Int:
			case Token::UInt: {
				auto a = to<IntegerType>(argType);
				auto r = to<IntegerType>(resultType);
				if (a && r && a->isSigned() == r->isSigned() && a->numBits() <= r->numBits()) {
					// nothing to do here
					acceptArg();
				} else if (argCategory == Type::Category::Contract || argCategory == Type::Category::Address) {
					cast_error(m_functionCall, "Unsupported type conversion. Use address.wid or address.value.");
				} else {
					defaultActionForCasting();
				}
				break;
			}
			case Token::Bytes:
			case Token::String: {
				if (isStringOrStringLiteralOrBytes(argType)) {
					acceptArg(); // nothing to do here
				} else if ((argCategory == Type::Category::Integer) ||
				(argCategory == Type::Category::RationalNumber)) {
					acceptArg();
					m_pusher.pushLines(convertToStr(false));
				} else {
					printError();
				}
				break;
			}
			default:
				defaultActionForCasting();
		}
	} else if (auto identifier = to<Identifier>(&m_functionCall.expression())) {
		if (to<ContractDefinition>(identifier->annotation().referencedDeclaration)) {
			conversionToAddress();
		} else if (auto enumDef = to<EnumDefinition>(identifier->annotation().referencedDeclaration)) {

			const auto& [ok, value] = TVMExpressionCompiler::constValue(*m_arguments[0]);
			if (ok) {
				if (value < 0 || value >= enumDef->members().size()) {
					cast_error(m_functionCall, "The value must be in the range 1 - " +
											   toString(enumDef->members().size()) + ".");
				}
				m_pusher.push(+1, "PUSHINT " + value.str());
				return;
			}

			acceptExpr(m_arguments[0].get());
			m_pusher.push(+1, "DUP");
			m_pusher.pushInt(enumDef->members().size());
			m_pusher.push(-1, "GEQ");

			auto type = m_arguments[0].get()->annotation().type;
			TypeInfo ti(type);
			if (!ti.isNumeric || ti.isSigned)
			{
				m_pusher.push(+1, "OVER");
				m_pusher.push(0, "ISNEG");
				m_pusher.push(-1, "OR");
			}
			m_pusher.push(-1, "THROWIF 5");
		} else {
			cast_error(m_functionCall, "Unsupported type conversion");
		}
	} else {
		cast_error(m_functionCall, "Unsupported type conversion");
	}
}

bool FunctionCallCompiler::checkLocalFunctionCall(const Identifier *identifier) {
	const string& functionName = identifier->name();
	auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
	if (!functionDefinition)
		return false;
	auto functionType = to<FunctionType>(getType(identifier));
	for (const auto& arg : m_arguments) {
		acceptExpr(arg.get());
	}
	if (isFunctionForInlining(functionDefinition)) {
		auto codeLines = m_pusher.ctx().getInlinedFunction(functionName);
		int nParams = functionType->parameterTypes().size();
		int nRetVals = functionType->returnParameterTypes().size();

		m_pusher.push(codeLines);
		m_pusher.push(-nParams + nRetVals, "");
	} else {
		m_pusher.pushCall(m_pusher.ctx().getFunctionInternalName(functionDefinition), functionType);
	}
	return true;
}

bool FunctionCallCompiler::checkSolidityUnits() {
	auto identifier = to<Identifier>(&m_functionCall.expression());
	if (identifier == nullptr) {
		return false;
	}

	const string& name = identifier->name();

	auto checkAndParseExceptionCode = [](Expression const* e) -> std::pair<bool, bigint> {
		const auto& [ok, val] = TVMExpressionCompiler::constValue(*e);
		if (ok) {
			if (val >= 65536 || val < 0) {
				cast_error(*e, "Exception code must be in the range 0 - 65535.");
			}
			return {true, val};
		}
		TypeInfo ti{e->annotation().type};
		if (ti.category != Type::Category::Integer || ti.isSigned) {
			cast_error(*e, "Exception code must be an unsigned number.");
		}
		return {false, bigint{}};
	};

	if (name == "sha256") {
		acceptExpr(m_arguments[0].get());
		m_pusher.push(0, "SHA256U");
	} else if (name == "selfdestruct") {
		const std::map<int, std::string> constParams {
				{TvmConst::int_msg_info::ihr_disabled, "1"},
				{TvmConst::int_msg_info::tons, StackPusherHelper::tonsToBinaryString(u256(1'000))}, // TODO use 0?
				{TvmConst::int_msg_info::bounce,  "0"},
		};
		m_pusher.sendIntMsg(
				{{TvmConst::int_msg_info::dest, m_arguments[0].get()}},
				constParams,
				nullptr,
				[&](){ m_pusher.push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::SelfDestruct)); });
	} else if (name == "require") {
		if (m_arguments.size() == 1) {
			acceptExpr(m_arguments[0].get());
			m_pusher.push(-1, "THROWIFNOT 100");
		} else if (m_arguments.size() == 2 || m_arguments.size() == 3) {
			if (m_arguments.size() == 3)
				acceptExpr(m_arguments[2].get());
			const auto &[ok, exceptionCode] = checkAndParseExceptionCode(m_arguments[1].get());
			if (ok && exceptionCode <= 1) {
				cast_error(*m_arguments[1].get(), "Error code must be at least two");
			}
			if (ok && exceptionCode < 2048) {
				acceptExpr(m_arguments[0].get());
				if (m_arguments.size() == 3)
					m_pusher.push(-2, "THROWARGIFNOT " + toString(exceptionCode));
				else
					m_pusher.push(-1, "THROWIFNOT " + toString(exceptionCode));
			} else {
				acceptExpr(m_arguments[1].get());
				if (!ok) {
					m_pusher.pushLines(R"(
DUP
LESSINT 2
PUSHCONT {
	DROP
	PUSHINT 100
}
IF
)");
				}
				acceptExpr(m_arguments[0].get());
				if (m_arguments.size() == 3)
					m_pusher.push(-3, "THROWARGANYIFNOT");
				else
					m_pusher.push(-2, "THROWANYIFNOT");
			}
		} else {
			cast_error(m_functionCall, R"("require" takes from one to three m_arguments.)");
		}
	} else if (name == "revert") {
		if (m_arguments.empty()) {
			m_pusher.push(0, "THROW 100");
		} else {
			if (!isIn(static_cast<int>(m_arguments.size()), 1, 2)) {
				cast_error(m_functionCall, R"("revert" takes up to two m_arguments.)");
			}
			const auto &[ok, exceptionCode] = checkAndParseExceptionCode(m_arguments[0].get());
			bool withArg = m_arguments.size() == 2;
			if (withArg) {
				acceptExpr(m_arguments[1].get());
			}
			if (ok && exceptionCode <= 1) {
				cast_error(*m_arguments[0].get(), "Error code must be at least two");
			}
			if (ok && exceptionCode < 2048) {
				m_pusher.push(withArg? -1 : 0, (withArg? "THROWARG " : "THROW ") + toString(exceptionCode));
			} else {
				acceptExpr(m_arguments[0].get());
				if (!ok) {
					m_pusher.pushLines(R"(
DUP
LESSINT 2
PUSHCONT {
	DROP
	PUSHINT 100
}
IF
)");
				}
				m_pusher.push(withArg? -2 : -1, withArg? "THROWARGANY" : "THROWANY");
			}
		}
	} else 	if (name == "logtvm") {
		auto logstr = m_arguments[0].get();
		if (auto literal = to<Literal>(logstr)) {
			if (literal->value().length() > 15)
				cast_error(m_functionCall, "Parameter string should have length no more than 15 chars");
			if (TVMContractCompiler::g_without_logstr) {
				return true;
			}
			m_pusher.push(0, "PRINTSTR " + literal->value());
		} else {
			cast_error(m_functionCall, "Parameter should be a literal");
		}
	} else if (name == "format") {
		auto literal = to<Literal>(m_arguments[0].get());
		if (!literal)
			cast_error(m_functionCall, "First parameter should be a literal string");
		std::string formatStr = literal->value();
		size_t pos = 0;
		std::vector<std::pair<std::string, bool> > substrings;
		while (true) {
			pos = formatStr.find('{', pos);
			size_t close_pos = formatStr.find('}', pos);
			if (pos == string::npos || close_pos == string::npos)
				break;
			if ((close_pos - pos != 1) && (close_pos - pos != 3)) {
				pos++;
				continue;
			}
			bool isHex = false;
			if (close_pos - pos == 3) {
				if (formatStr[close_pos - 2] != ':') {
					pos++;
					continue;
				}
				char formatCharacter = formatStr[close_pos - 1];
				if (formatCharacter != 'x')
					cast_error(m_functionCall, "The only supported specified format is x");
				isHex = true;
			}
			substrings.emplace_back(formatStr.substr(0, pos), isHex);
			formatStr = formatStr.substr(close_pos + 1);
			pos = 0;
		}
		if (substrings.size() + 1 != m_arguments.size())
			cast_error(m_functionCall, "Number of arguments is not equal to the number of placeholders!");
		m_pusher.push(+1, "NEWC");
		for(size_t it = 0; it < substrings.size(); it++) {
			if (substrings[it].first.length())
				m_pusher.storeStringInABuilder(substrings[it].first);
			Type::Category cat = m_arguments[it + 1]->annotation().type->category();
			if (cat == Type::Category::Integer || cat == Type::Category::RationalNumber) {
				acceptExpr(m_arguments[it + 1].get());
				m_pusher.pushLines(convertToStr(substrings[it].second, false));
				m_pusher.push(-1, "STBR");
			} else if (cat == Type::Category::Address) {
				acceptExpr(m_arguments[it + 1].get());
				m_pusher.pushLines(R"(
LDU 3
NIP
LDI 8
LDU 256
DROP
SWAP
)");
				m_pusher.pushLines(convertToStr(true, false));

				m_pusher.pushLines(R"(
STSLICECONST x3A
SWAP
)");
				m_pusher.pushLines(convertToStr(true, false));
				m_pusher.push(0, "STBR");
				m_pusher.push(-1, "STBR");
			} else {
				cast_error(*m_arguments[it + 1].get(), "Unsupported argument type");
			}
		}
		if (formatStr.length())
			m_pusher.storeStringInABuilder(formatStr);
		m_pusher.push(0, "ENDC");
	} else if (name == "stoi") {
		m_pusher.push(+1, "TRUE");
		acceptExpr(m_arguments[0].get());
		m_pusher.push(0, "CTOS");
		m_pusher.pushS(0);
		m_pusher.push(+1, "PLDU 8");

		m_pusher.push(0, "EQINT 45");
		m_pusher.pushS(0);
		m_pusher.pushLines(
				R"(
PUSHCONT {
	SWAP
	PUSHINT 8
	SDSKIPFIRST
}
PUSHCONT {
	SWAP
}
IFELSE
)");
		m_pusher.pushS(0);
		m_pusher.pushInt(16);
		m_pusher.push(-2+1, "SCHKBITSQ");
		m_pusher.pushLines(
				R"(
PUSHCONT {
	DUP
	PLDU 16
	PUSHINT 12408
	EQUAL
}
PUSHCONT {
	FALSE
}
IFELSE
PUSHINT 0
ROTREV
PUSHCONT {
	PUSHINT 16
	SDSKIPFIRST
	PUSHCONT {
		DUP
		PUSHINT 8
		SCHKBITSQ
	}
	PUSHCONT {
		LDU 8
		SWAP
		DUP
		PUSHINT 65
		GEQ
		PUSHCONT {
			DUP
			PUSHINT 97
			GEQ
			PUSHCONT {
				ADDCONST -87
			}
			PUSHCONT {
				ADDCONST -55
			}
			IFELSE
		}
		PUSHCONT {
			ADDCONST -48
		}
		IFELSE
		DUP
		GTINT 15
		PUSH S1
		ISNEG
		OR
		PUSHCONT {
			BLKSWAP 1, 4
			DROP
			FALSE
			BLKSWAP 4, 1
		}
		IF
		BLKSWAP 1, 2
		MULCONST 16
		ADD
		SWAP
	}
	WHILE
}
PUSHCONT {
	PUSHCONT {
		DUP
		PUSHINT 8
		SCHKBITSQ
	}
	PUSHCONT {
		LDU 8
		SWAP
		ADDCONST -48
		DUP
		GTINT 9
		PUSH S1
		ISNEG
		OR
		PUSHCONT {
			BLKSWAP 1, 4
			DROP
			FALSE
			BLKSWAP 4, 1
		}
		IF
		BLKSWAP 1, 2
		MULCONST 10
		ADD
		SWAP
	}
	WHILE
}
IFELSE
DROP
SWAP
PUSHCONT {
	NEGATE
}
IF
SWAP
)");
		m_pusher.push(-4, "");
	}else if (name == "hexstring") {
		Type::Category cat = m_arguments[0]->annotation().type->category();
		if (cat == Type::Category::Integer || cat == Type::Category::RationalNumber) {
			TypeInfo ti{m_arguments[0]->annotation().type};
			acceptExpr(m_arguments[0].get());
			m_pusher.pushLines(convertToStr(true, true, true, ti.numBits / 4));
		} else if (cat == Type::Category::Address) {
			acceptExpr(m_arguments[0].get());
			m_pusher.pushLines(R"(
LDU 3
NIP
LDI 8
LDU 256
DROP
SWAP
)");
			m_pusher.pushLines(convertToStr(true, false));
			m_pusher.pushLines(R"(
STSLICECONST x3A
SWAP
)");
			m_pusher.pushLines(convertToStr(true, false, true));
			m_pusher.push(0, "STBR");
			m_pusher.push(0, "ENDC");
		} else {
			cast_error(m_functionCall, "Parameter should be integer or address");
		}
	} else {
		return false;
	}
	return true;
}

bool FunctionCallCompiler::checkLocalFunctionCallOrFuncVarCall() {
	auto expr = &m_functionCall.expression();
	if (auto identifier = to<Identifier>(expr); identifier && checkLocalFunctionCall(identifier)) {
	} else if (expr->annotation().type->category() == Type::Category::Function) {
		if (auto ma = to<MemberAccess>(expr)) {
			auto category = getType(&ma->expression())->category();
			if (category == Type::Category::TypeType || isSuper(&ma->expression())) {
				// calling of base/super method or typeTypeMethods
				return false;
			}
		}

		auto ft = to<FunctionType>(expr->annotation().type);
		if (ft->kind() != FunctionType::Kind::Internal) {
			return false;
		}

		for (const auto& arg : m_arguments) {
			acceptExpr(arg.get());
		}
		// Local variable of functional type
		acceptExpr(expr);
		m_pusher.pushS(0);
		m_pusher.pushInt(TvmConst::FunctionId::DefaultValueForFunctionType);
		m_pusher.push(-2 + 1, "EQUAL");
		m_pusher.push(-1, "THROWIF " + toString(TvmConst::RuntimeException::BadFunctionIdOfFuncCall));
		auto functionType = to<FunctionType>(expr->annotation().type);
		int returnCnt = functionType->returnParameterTypes().size();
		int paramCnt = functionType->parameterTypes().size();
		m_pusher.push(+1, "PUSH C3");
		m_pusher.push(-1 - 1 - paramCnt + returnCnt, "EXECUTE");
	} else {
		return false;
	}
	return true;
}

bool FunctionCallCompiler::createNewContract() {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
	auto newExpr = to<NewExpression>(&functionOptions->expression());
	if (!newExpr)
		return false;
	const TypePointer type = newExpr->typeName().annotation().type;

	std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
	auto stateIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "stateInit"; });
	auto codeIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "code"; });
	int pkIndex = -1;
	auto pubkeyIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "pubkey"; });
	if (pubkeyIt != optionNames.end())
		pkIndex = pubkeyIt - optionNames.begin();
	std::function<void()> pushKey = [&]() {
		if (pkIndex == -1) {
			m_pusher.pushInt(0);
		} else {
				auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
				acceptExpr(functionOptions->options().at(pkIndex).get());
		}
	};

	if (stateIt != optionNames.end()) {
		size_t index = stateIt - optionNames.begin();
		acceptExpr(functionOptions->options()[index].get()); // stack: stateInit
	} else if (codeIt != optionNames.end()) {
		const int ss = m_pusher.getStack().size();
		std::map<StateInitMembers, std::function<void()>> stateInitExprs;

		auto varIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "varInit"; });
		bool hasVars = (varIt != optionNames.end());
		bool isNew = true;
		stateInitExprs[StateInitMembers::Data] = generateDataSection(pushKey, hasVars,
																	 hasVars ? functionOptions->options().at(varIt - optionNames.begin()) : nullptr,
																	 isNew,
																	 nullptr
					);

		stateInitExprs[StateInitMembers::Code] = [&]() {
			size_t codeIndex = codeIt - optionNames.begin();
			acceptExpr(functionOptions->options().at(codeIndex).get());
		};

		auto splitDepthIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "splitDepth"; });
		if (splitDepthIt != optionNames.end()) {
			stateInitExprs[StateInitMembers::SplitDepth] = [&]() {
				size_t splitDepthIndex = splitDepthIt - optionNames.begin();
				acceptExpr(functionOptions->options()[splitDepthIndex].get()); // stack: data code split_depth
			};
		}

		buildStateInit(stateInitExprs);

		// stack: stateInit
		solAssert(ss + 1 == m_pusher.getStack().size(), "");
	} else {
		solUnimplemented("");
	}

	std::function<void()> pushWid;
	auto widIt = find_if(optionNames.begin(), optionNames.end(), [](auto el) { return *el == "wid"; });
	if (widIt != optionNames.end()) {
		pushWid = [&](){
			size_t widIndex = widIt - optionNames.begin();
			acceptExpr(functionOptions->options()[widIndex].get()); // stack: stateInit hash wid
		};
	}

	const std::function<void()> pushValue = [&](){
		auto valueIt = find_if(optionNames.begin(), optionNames.end(),  [](auto el) { return *el == "value"; });
		solAssert(valueIt != optionNames.end(), "");
		size_t index = valueIt - optionNames.begin();
		TVMExpressionCompiler{m_pusher}.compileNewExpr(functionOptions->options()[index].get());
	};

	std::function<void()> pushBounce;
	auto bounceIt = find_if(optionNames.begin(), optionNames.end(),  [](auto el) { return *el == "bounce"; });
	if (bounceIt != optionNames.end()) {
		pushBounce = [&]() {
			size_t index = bounceIt - optionNames.begin();
			TVMExpressionCompiler{m_pusher}.compileNewExpr(functionOptions->options()[index].get());
		};
	}

	std::function<void()> pushCurrency;
	auto currencyIt = find_if(optionNames.begin(), optionNames.end(),  [](auto el) { return *el == "currencies"; });
	if (currencyIt != optionNames.end()) {
		pushCurrency = [&]() {
			size_t index = currencyIt - optionNames.begin();
			TVMExpressionCompiler{m_pusher}.compileNewExpr(functionOptions->options()[index].get());
		};
	}

	const std::function<void(int builderSize)> pushBody = [&](int builderSize){
		auto constructor = (to<ContractType>(type))->contractDefinition().constructor();
		if (constructor)
			return EncodeFunctionParams{&m_pusher}.createMsgBodyAndAppendToBuilder(
				[&](size_t idx) {
					m_pusher.push(0, ";; " + constructor->parameters()[idx]->name());
					TVMExpressionCompiler{m_pusher}.compileNewExpr(m_arguments[idx].get());
				},
				convertArray(constructor->parameters()),
				EncodeFunctionParams{&m_pusher}.calculateFunctionIDWithReason(constructor, ReasonOfOutboundMessage::RemoteCallInternal),
				{},
				builderSize
			);
		else
			return EncodeFunctionParams{&m_pusher}.createDefaultConstructorMsgBodyAndAppendToBuilder(builderSize);
	};

	std::function<void()> pushSendrawmsgFlag;
	auto flagIt = find_if(optionNames.begin(), optionNames.end(),  [](auto el) { return *el == "flag"; });
	if (flagIt != optionNames.end()) {
		Expression const *sendrawmsgFlag = functionOptions->options()[flagIt - optionNames.begin()].get();
		pushSendrawmsgFlag = [&]() { acceptExpr(sendrawmsgFlag); };
	}

	deployNewContract(pushWid, pushValue, pushBounce, pushCurrency, pushBody, pushSendrawmsgFlag);
	// stack: destAddress
	return true;
}

void FunctionCallCompiler::deployNewContract(
	const std::function<void()> pushWid,
	const std::function<void()> pushValue,
	const std::function<void()> pushBounce,
	const std::function<void()> pushCurrency,
	const std::function<void(int builderSize)> appendBody,
	const std::function<void()> pushSendrawmsgFlag
) {
	std::map<int, std::function<void()>> exprs;

	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"}};

	if (pushBounce) {
		exprs[TvmConst::int_msg_info::bounce] = pushBounce;
	} else {
		constParams[TvmConst::int_msg_info::bounce] = "1";
	}

	if (pushCurrency) {
		exprs[TvmConst::int_msg_info::currency] = pushCurrency;
	} else {
		constParams[TvmConst::int_msg_info::currency] = "0";
	}

	// stack: stateInit

	int stateInitStack = m_pusher.getStack().size();
	// stack: stateInit
	m_pusher.pushS(0);
	m_pusher.push(-1 + 1, "HASHCU"); // stack: stateInit hash

	if (pushWid) {
		pushWid();
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "STSLICECONST x9_"); // addr_std$10 anycast:(Maybe Anycast) // 10 0 1 = 9
		m_pusher.push(-1, "STI 8"); // workchain_id:int8
	} else {
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "STSLICECONST x801_"); // addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 // 10 0  00000000 1 = 801
	}
	m_pusher.push(-1, "STU 256"); // address:bits256
	m_pusher.push(-1 + 1, "ENDC");
	m_pusher.push(-1 + 1, "CTOS");

	int destAddressStack = m_pusher.getStack().size();
	// stack: stateInit destAddress

	exprs[TvmConst::int_msg_info::tons] = [&](){
		pushValue();
	};
	exprs[TvmConst::int_msg_info::dest] = [&](){
		int stackIndex = m_pusher.getStack().size() - destAddressStack;
		m_pusher.pushS(stackIndex);
	};


	std::function<void()> appendStateInit = [&]() {
		m_pusher.stones(1);
		int stackIndex = m_pusher.getStack().size() - stateInitStack;
		m_pusher.pushS(stackIndex);
		m_pusher.push(-1, "STREFR");
	};

	std::set<int> isParamOnStack;
	for (auto &[param, expr] : exprs | boost::adaptors::reversed) {
		isParamOnStack.insert(param);
		expr();
	}

	m_pusher.sendMsg(
		isParamOnStack,
		constParams,
		appendBody,
		appendStateInit,
		pushSendrawmsgFlag
	);
	m_pusher.dropUnder(1, 1);
	// stack: destAddress
}

bool FunctionCallCompiler::checkNewExpression() {

	if (to<FunctionCallOptions>(&m_functionCall.expression())) {
		return createNewContract();
	}

	if (to<NewExpression>(&m_functionCall.expression()) == nullptr) {
		return false;
	}
	Type const *resultType = m_functionCall.annotation().type;
	if (resultType->category() == Type::Category::Contract) {
		cast_error(m_functionCall, R"(Unsupported contract creating. Use call options: "stateInit", "value", "flag")");
	}

	int size = m_pusher.getStack().size();

	m_pusher.push(0, ";; new " + resultType->toString(true));
	m_pusher.push(+1, "NEWDICT"); // dict
	acceptExpr(m_arguments[0].get()); // dict size
	m_pusher.push(+1, "DUP"); // dict size sizeIter


	auto arrayType = to<ArrayType>(resultType);
	const IntegerType key = getKeyTypeOfArray();
	Type const* arrayBaseType = arrayType->baseType();

	// TODO optimize if size is constant and size == 0 or size == 1
	m_pusher.pushS(0);
	{
		StackPusherHelper pusherHelper(&m_pusher.ctx(), 1);
		pusherHelper.push(0, "DEC"); // dict size sizeIter'
		pusherHelper.pushDefaultValue(arrayBaseType, true); // dict size sizeIter' value
		// TODO optimize. Locate default value on stack (don't create default value in each iteration)
		const DataType& dataType = pusherHelper.prepareValueForDictOperations(&key, arrayBaseType, true); // arr value'
		pusherHelper.push(2, "PUSH2 S1,S3"); // dict size sizeIter' value sizeIter' dict
		pusherHelper.setDict(key, *arrayType->baseType(), dataType); // dict size sizeIter' dict'
		pusherHelper.push(0, "POP S3"); // dict' size sizeIter'
		m_pusher.pushCont(pusherHelper.code());
	}
	m_pusher.push(-2, "REPEAT");
	// dict size 0
	m_pusher.drop(1);  // dict size

	m_pusher.push(0, "SWAP");
	m_pusher.push(-2 + 1, "PAIR");

	solAssert(size + 1 == m_pusher.getStack().size(), "");
	return true;
}

bool FunctionCallCompiler::checkTvmIntrinsic() {
	IntrinsicsCompiler ic(m_pusher);
	return ic.checkTvmIntrinsic(m_functionCall);
}

bool FunctionCallCompiler::structMethodCall() {
	auto ma = to<MemberAccess>(&m_functionCall.expression());
	if (ma->memberName() != "unpack") {
		return false;
	}
	acceptExpr(&ma->expression());
	auto structType = to<StructType>(getType(&ma->expression()));
	int memberQty = structType->structDefinition().members().size();
	m_pusher.untuple(memberQty);
	return true;
}

void FunctionCallCompiler::buildStateInit(std::map<StateInitMembers, std::function<void()>> exprs) {
	solAssert(exprs.count(StateInitMembers::Special) == 0, "TODO");
	solAssert(exprs.count(StateInitMembers::Library) == 0, "TODO");
	solAssert(exprs.count(StateInitMembers::Code) == 1, "Code must present");
	solAssert(exprs.count(StateInitMembers::Data) == 1, "Data must present");

	const int ss = m_pusher.getStack().size();

	// _ split_depth:(Maybe (## 5)) special:(Maybe TickTock)
	// code:(Maybe ^Cell) data:(Maybe ^Cell)
	// library:(HashmapE 256 SimpleLib) = StateInit;

	// stack: data
	exprs.at(StateInitMembers::Data)();
	// stack: code
	exprs.at(StateInitMembers::Code)();

	// stake: data code
	// let's store split_depth and special options
	if (exprs.count(StateInitMembers::SplitDepth) > 0) {
		exprs.at(StateInitMembers::SplitDepth)();
		m_pusher.push(+1, "NEWC");
		m_pusher.stones(1);
		m_pusher.push(-2 + 1, "STU 5");
		m_pusher.stzeroes(1);
	} else {
		m_pusher.push(+1, "NEWC");
		// stake: data code builder
		m_pusher.push(-1 + 1, "STSLICECONST x2_"); // no split_depth and no special // 0 0
	}

	// stake: data code builder
	m_pusher.push(-2 + 1, "STOPTREF"); // store code
	m_pusher.push(-2 + 1, "STOPTREF"); // store data
	m_pusher.push(0, "STZERO"); // store library
	m_pusher.push(0, "ENDC");
	// stack: stateInit
	solAssert(ss + 1 == m_pusher.getStack().size(), "");
}
