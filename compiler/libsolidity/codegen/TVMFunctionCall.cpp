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

#include "DictOperations.hpp"
#include "TVMFunctionCall.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMStructCompiler.hpp"

#include <boost/algorithm/string.hpp>
#include <libsolidity/ast/TypeProvider.h>

using namespace solidity::frontend;

FunctionCallCompiler::FunctionCallCompiler(StackPusherHelper &m_pusher, TVMExpressionCompiler *exprCompiler,
											FunctionCall const& _functionCall, bool isCurrentResultNeeded) :
		m_pusher{m_pusher},
		m_exprCompiler{exprCompiler},
		m_functionCall{_functionCall},
		m_arguments{_functionCall.arguments()},
		m_funcType{to<FunctionType>(m_functionCall.expression().annotation().type)},
		m_retType{m_functionCall.annotation().type},
		m_isCurrentResultNeeded{isCurrentResultNeeded}
{
}

void FunctionCallCompiler::structConstructorCall() {
	auto const& type = dynamic_cast<TypeType const&>(*m_functionCall.expression().annotation().type);
	auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
	auto pushParam = [&](int index, Type const* targetType) {
		pushExprAndConvert(m_arguments.at(index).get(), targetType);
	};
	StructCompiler structCompiler{&m_pusher, &structType};
	structCompiler.structConstructor(m_functionCall.names(), pushParam);
}

bool FunctionCallCompiler::compile() {
	if (checkRemoteMethodCall(m_functionCall)) {
		// To avoid situation when we call a function of a remote contract and don't save the result.
		// Remote function can return a result but in fact we don't get it.
		return false;
	}


	auto ma = to<MemberAccess>(&m_functionCall.expression());
	auto reportError = [&](){
		cast_error(m_functionCall, "Unsupported function call");
	};

	if ((ma != nullptr && libraryCall(*ma)) ||
		checkForMappingOrCurrenciesMethods() ||
		checkNewExpression() ||
		checkAddressThis() ||
		checkSolidityUnits() ||
		checkLocalFunctionOrLibCallOrFuncVarCall()) {
		// do nothing
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
				checkForTvmTupleMethods(*ma, category) ||
				checkForOptionalMethods(*ma))
			{
				// nothing
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
				if (m_funcType->kind() == FunctionType::Kind::TVMBuildIntMsg) {
					tvmBuildIntMsg();
				} else if (m_funcType->kind() == FunctionType::Kind::TVMBuildExtMsg) {
					tvmBuildMsgMethod();
				} else if (
					checkForTvmSendFunction(*ma) ||
					checkForTvmConfigParamFunction(*ma) ||
					checkForTvmFunction(*ma) ||
					checkForTvmDeployMethods(*ma, category)
				) {
					// do nothing
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
	return true;
}

bool FunctionCallCompiler::checkForMappingOrCurrenciesMethods() {
	auto expr = &m_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	if (ma == nullptr || (!to<MappingType>(ma->expression().annotation().type) && !to<ExtraCurrencyCollectionType>(ma->expression().annotation().type)))
		return false;

	const ASTString &memberName = ma->memberName();
	m_pusher.push(0, ";; map." + memberName);

	if (isIn(memberName, "delMin", "delMax")) {
		mappingDelMinOrMax(memberName == std::string{"delMin"});
	} else  if (isIn(memberName, "at", "fetch", "exists", "replace", "add", "getSet", "getAdd", "getReplace")) {
		mappingGetSet();
	} else if (isIn(memberName, "min", "max")) {
		mappingMinMaxMethod(memberName == std::string{"min"});
	} else if (isIn(memberName, "next", "prev", "nextOrEq", "prevOrEq")) {
		mappingPrevNextMethods();
	} else if (memberName == "empty") {
		mappingEmpty();
	} else {
		cast_error(m_functionCall, "Unsupported mapping method");
	}

	return true;
}

void FunctionCallCompiler::mappingDelMinOrMax(bool isDelMin) {
	auto memberAccess = to<MemberAccess>(&m_functionCall.expression());

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess->expression().annotation().type);

	DelMinOrMax d{m_pusher, *keyType, *valueType, isDelMin, memberAccess};
	d.delMinOrMax();
}

void FunctionCallCompiler::mappingGetSet() {
	auto memberAccess = to<MemberAccess>(&m_functionCall.expression());

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess->expression().annotation().type);

	const ASTString &memberName = memberAccess->memberName();
	if (isIn(memberName, "fetch", "at")) {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType, false);
		acceptExpr(&memberAccess->expression()); // index dict
		if (memberName == "fetch")
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::Fetch);
		else
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::GetFromArray);
	} else if (memberName == "exists") {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType, false);
		acceptExpr(&memberAccess->expression()); // index dict
		m_pusher.getDict(*keyType, *valueType, GetDictOperation::Exist);
	} else if (isIn(memberName, "replace", "add", "getSet", "getAdd", "getReplace")) {
		const int stackSize = m_pusher.getStack().size();
		auto ma = to<MemberAccess>(&m_functionCall.expression());
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&ma->expression(), true); // lValue... map
		pushArgAndConvert(1); // lValue... map value
		const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueType, false); // lValue... map value'
		pushArgAndConvert(0); // mapLValue... map value key
		m_pusher.prepareKeyForDictOperations(keyType, false);
		m_pusher.push(0, "ROT"); // mapLValue... value key map

		if (isIn(memberName, "replace", "add")) {
			SetDictOperation op;
			if (memberName == "replace") {
				op = SetDictOperation::Replace;
			} else if (memberName == "add") {
				op = SetDictOperation::Add;
			} else {
				solUnimplemented("");
			}
			m_pusher.setDict(*keyType, *valueType, dataType, op); // mapLValue... map {0, -1}
		} else {
			GetDictOperation op;
			if (memberName == "getSet") {
				op = GetDictOperation::GetSetFromMapping;
			} else if (memberName == "getAdd") {
				op = GetDictOperation::GetAddFromMapping;
			} else if (memberName == "getReplace") {
				op = GetDictOperation::GetReplaceFromMapping;
			} else {
				solUnimplemented("");
			}
			m_pusher.getDict(*keyType, *valueType, op, dataType);
			// mapLValue... map optValue
		}
		const int cntOfValuesOnStack = m_pusher.getStack().size() - stackSize;  // mapLValue... map optValue
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optValue mapLValue... map
		m_exprCompiler->collectLValue(lValueInfo, true, false); // optValue
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::mappingMinMaxMethod(bool isMin) {
	auto expr = &m_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);

	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess->expression().annotation().type);

	acceptExpr(&memberAccess->expression()); // dict

	DictMinMax compiler{m_pusher, *keyType, *valueType, isMin};
	compiler.minOrMax();
}

void FunctionCallCompiler::mappingPrevNextMethods() {
	auto expr = &m_functionCall.expression();
	auto memberAccess = to<MemberAccess>(expr);
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(memberAccess->expression().annotation().type);

	pushArgAndConvert(0); // index
	m_pusher.prepareKeyForDictOperations(keyType, true); // index'
	acceptExpr(&memberAccess->expression()); // index' dict
	m_pusher.pushInt(lengthOfDictKey(keyType)); // index' dict nbits

	DictPrevNext compiler{m_pusher, *keyType, *valueType, memberAccess->memberName()};
	compiler.prevNext();
}

void FunctionCallCompiler::mappingEmpty() {
	auto expr = &m_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	acceptExpr(&ma->expression());
	m_pusher.push(0, "DICTEMPTY");
}

void FunctionCallCompiler::superFunctionCall(MemberAccess const &_node) {
	pushArgs();
	m_pusher.push(0, ";; super");

	string fname = _node.memberName();
	auto super = getSuperContract(m_pusher.ctx().getCurrentFunction()->annotation().contract,
								  m_pusher.ctx().getContract(), fname);
	solAssert(super, "");
	if (getFunction(super, fname)) {
		auto functionName = super->name() + "_" + fname;
		m_pusher.push( 0, ";; Super call " + functionName);
		if (auto ft = to<FunctionType>(getType(&_node))) {
			m_pusher.pushCallOrCallRef(functionName, ft);
			return;
		}
	}
	solUnimplemented("");
}

void FunctionCallCompiler::typeTypeMethods(MemberAccess const &_node) {
	if (_node.memberName() == "makeAddrExtern") {
		// addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
		const auto& num = TVMExpressionCompiler::constValue(*m_arguments.at(0));
		const auto& len = TVMExpressionCompiler::constValue(*m_arguments.at(1));
		if (num.has_value() && len.has_value()) {
			std::string addr = "01";
			StackPusherHelper::addBinaryNumberToString(addr, u256(len.value()), 9);
			StackPusherHelper::addBinaryNumberToString(addr, u256(num.value()), int(len.value()));
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(addr));
		} else {
			pushArgs();
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
		const auto& wid = TVMExpressionCompiler::constValue(*m_arguments.at(0));
		const auto& val = TVMExpressionCompiler::constValue(*m_arguments.at(1));
		if (wid.has_value() && val.has_value()) {
			std::string addr = "100";
			StackPusherHelper::addBinaryNumberToString(addr, u256(wid.value()), 8);
			StackPusherHelper::addBinaryNumberToString(addr, u256(val.value()), 256);
			m_pusher.push(+1, "PUSHSLICE x" + StackPusherHelper::binaryStringToSlice(addr));
		} else {
			pushArgs(true);
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

bool FunctionCallCompiler::libraryCall(MemberAccess const& ma) {
	if (auto libFunction = to<FunctionDefinition>(ma.annotation().referencedDeclaration)) {
		DeclarationAnnotation const &da = libFunction->annotation();
		if (da.contract->contractKind() == ContractKind::Library) {
			m_pusher.ctx().addLib(libFunction);

			auto t = getType(&ma.expression());
			const int argQty = static_cast<int>(m_arguments.size());
			const int retQty = static_cast<int>(libFunction->returnParameters().size());
			FunctionType const* funType = libFunction->functionType(!libFunction->isPublic());
			if (t->category() == Type::Category::TypeType) {
				// uint z = MyLib.sum(a, b);
				pushArgs();
				m_pusher.pushCallOrCallRef(m_pusher.ctx().getLibFunctionName(libFunction, false), funType);
			} else {
				// using MathLib for uint;
				// a.add(b);
				const int stakeSize0 = m_pusher.getStack().size();
				const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&ma.expression(),
																								  true);
				const int stakeSize1 = m_pusher.getStack().size();
				const int lValueQty = stakeSize1 - stakeSize0;

				pushArgs();
				m_pusher.pushCallOrCallRef(
					m_pusher.ctx().getLibFunctionName(libFunction, true),
					funType,
					(-1 - argQty) + (+1 + retQty)
				);

				m_pusher.blockSwap(lValueQty, retQty);

				m_exprCompiler->collectLValue(lValueInfo, true, false);
			}
			return true;
		}
	}
	return false;
}

std::function<void()> FunctionCallCompiler::generateDataSection(
			const std::function<void()>& pushKey,
			bool & hasVars,
			const ASTPointer<Expression const>& vars,
			bool & isNew,
			const ASTPointer<Expression const>& contr
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
			std::vector<std::pair<VariableDeclaration const*, int>> staticVars = cc.getStaticVariables();
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
				pushExprAndConvert(initVars->options().at(i).get(), valueType); // stack: dict value
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

bool FunctionCallCompiler::checkRemoteMethodCall(FunctionCall const &_functionCall) {
	std::map<int, Expression const *> exprs;
	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"}};
	std::function<void(int)> appendBody;
	std::function<void()> pushSendrawmsgFlag;
	FunctionDefinition const* functionDefinition{};
	std::optional<uint32_t> callbackFunctionId;

	if (auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression())) {
		auto memberAccess = to<MemberAccess>(&functionOptions->expression());
		if (!memberAccess) {
			return false;
		}

		std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
		auto getIndex = [&](const std::string& name) -> int {
			auto it = std::find_if(optionNames.begin(), optionNames.end(), [&](auto el) { return *el == name;});
			if (it == optionNames.end()) {
				return -1;
			}
			return it - optionNames.begin();
		};

		// Search for bounce option
		const int extIndex = getIndex("extMsg");
		if (extIndex != -1) {
			checkExtMsgSend();
			return true;
		}

		// parse options they are stored in two vectors: names and options
		for (const auto &option: optionNames)
			if (!isIn(*option, "flag", "value", "currencies", "bounce", "callback"))
				cast_error(_functionCall, "Unsupported function call option: " + *option);


		// Search for bounce option
		const int bounceIndex = getIndex("bounce");
		if (bounceIndex != -1) {
			exprs[TvmConst::int_msg_info::bounce] = functionOptions->options()[bounceIndex].get();
		} else {
			constParams[TvmConst::int_msg_info::bounce] = "1";
		}

		// Search for currencies option
		const int currenciesIndex = getIndex("currencies");
		if (currenciesIndex != -1) {
			exprs[TvmConst::int_msg_info::currency] = functionOptions->options()[currenciesIndex].get();
		} else {
			constParams[TvmConst::int_msg_info::currency] = "0";
		}

		// Search for value (ton) option
		const int valueIndex = getIndex("value");
		if (valueIndex != -1) {
			const auto& value = TVMExpressionCompiler::constValue(*functionOptions->options().at(valueIndex));
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::tons] = StackPusherHelper::tonsToBinaryString(u256(value.value()));
			} else {
				exprs[TvmConst::int_msg_info::tons] = functionOptions->options()[valueIndex].get();
			}
		} else {
			constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		}

		// remote_addr
		exprs[TvmConst::int_msg_info::dest] = &memberAccess->expression();

		// function definition
		functionDefinition = getRemoteFunctionDefinition(memberAccess);


		const int callbackIndex = getIndex("callback");
		if (callbackIndex != -1) {
			CallableDeclaration const* remoteFunction =
					getFunctionDeclarationOrConstructor(functionOptions->options()[callbackIndex].get());
			callbackFunctionId = ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(
					remoteFunction,
					ReasonOfOutboundMessage::RemoteCallInternal
			);
		}

		// Search for sendRawMsg flag option
		const int flagIndex = getIndex("flag");
		if (flagIndex != -1) {
			pushSendrawmsgFlag = [flagIndex, functionOptions, this]() {
				acceptExpr(functionOptions->options()[flagIndex].get());
			};
		}
	} else {
		constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		constParams[TvmConst::int_msg_info::bounce] = "1";

		Expression const *currentExpression = &_functionCall.expression();
		while (true) {
			auto currentFunctionCall = to<FunctionCall>(currentExpression);
			if (currentFunctionCall == nullptr) {
				break;
			}

			auto memberAccess = to<MemberAccess>(&currentFunctionCall->expression());
			if (memberAccess == nullptr) {
				return false;
			}

			if (memberAccess->memberName() == "flag") {
				pushSendrawmsgFlag = [currentFunctionCall, this]() {
					acceptExpr(currentFunctionCall->arguments().at(0).get());
				};
				currentExpression = &memberAccess->expression();
			} else if (memberAccess->memberName() == "value") {
				exprs[TvmConst::int_msg_info::tons] = currentFunctionCall->arguments()[0].get();
				constParams.erase(TvmConst::int_msg_info::tons);
				currentExpression = &memberAccess->expression();
			} else {
				break;
			}
		}
		auto memberValue = to<MemberAccess>(currentExpression);
		if (memberValue == nullptr) {
			return false;
		}
		exprs[TvmConst::int_msg_info::dest] = &memberValue->expression();
		functionDefinition = getRemoteFunctionDefinition(memberValue);
		if (functionDefinition == nullptr) {
			return false;
		}
	}

	pushArgs(true);

	appendBody = [&](int builderSize) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			convertArray(functionDefinition->parameters()),
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(functionDefinition, ReasonOfOutboundMessage::RemoteCallInternal),
			callbackFunctionId,
			builderSize,
			true
		);
	};

	if (m_isCurrentResultNeeded)
		cast_error(_functionCall, "Calls to remote contract do not return result.");

	m_pusher.sendIntMsg(exprs, constParams, appendBody, pushSendrawmsgFlag);
	return true;
}

void FunctionCallCompiler::checkExtMsgSend() {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());

	std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
	auto getIndex = [&](const std::string& name) -> int {
		// TODO move to function
		auto it = std::find_if(optionNames.begin(), optionNames.end(), [&](auto el) { return *el == name;});
		if (it == optionNames.end()) {
			return -1;
		}
		return it - optionNames.begin();
	};

	const int extIndex = getIndex("extMsg");
	const std::optional<bool> value = TVMExpressionCompiler::constBool(*functionOptions->options()[extIndex]);
	solAssert(value.has_value(), "\"extMsg\" option should be set to constant true if set.");

	bool addSignature = false;
	const int signIndex = getIndex("sign");
	if (signIndex != -1) {
		addSignature = TVMExpressionCompiler::constBool(*functionOptions->options()[signIndex]).value();
	}

	const Expression * pubkey = nullptr;
	ASTPointer<Expression const> expire = nullptr;
	ASTPointer<Expression const> time = nullptr;
	ASTPointer<Expression const> callbackid = nullptr;
	ASTPointer<Expression const> abiVer = nullptr;
	ASTPointer<Expression const> onerrorid = nullptr;
	const Expression * stateInit= nullptr;
    const Expression * signBoxHandle = nullptr;

	const int pubkeyIndex = getIndex("pubkey");
	if (pubkeyIndex != -1) {
		pubkey = functionOptions->options()[pubkeyIndex].get();
	}

	const int stateIndex = getIndex("stateInit");
	if (stateIndex != -1) {
		stateInit = functionOptions->options()[stateIndex].get();
	}

	const int expireIndex = getIndex("expire");
	if (expireIndex != -1) {
		expire = functionOptions->options()[expireIndex];
	}

	const int timeIndex = getIndex("time");
	if (timeIndex != -1) {
		time = functionOptions->options()[timeIndex];
	}

	const int callbackIndex = getIndex("callbackId");
	callbackid = functionOptions->options()[callbackIndex];

	const int abiIndex = getIndex("abiVer");
	abiVer = functionOptions->options()[abiIndex];

	const int onErrorIndex = getIndex("onErrorId");
	onerrorid = functionOptions->options()[onErrorIndex];

	auto memberAccess = to<MemberAccess>(&functionOptions->expression());
	FunctionDefinition const* functionDefinition = getRemoteFunctionDefinition(memberAccess);

	const Expression * destination = &memberAccess->expression();

    const int signBoxIndex = getIndex("signBoxHandle");
    if (signBoxIndex != -1) {
        signBoxHandle = functionOptions->options()[signBoxIndex].get();
    }

	generateExtInboundMsg(addSignature, destination, pubkey, expire.get(), time.get(), callbackid.get(),
						  abiVer.get(), onerrorid.get(), stateInit, signBoxHandle, functionDefinition, m_arguments);
	m_pusher.pushInt(TvmConst::SENDRAWMSG::DefaultFlag);
	m_pusher.sendrawmsg();
}

std::string FunctionCallCompiler::getDefaultMsgValue() {
	const auto expr = m_pusher.ctx().pragmaHelper().haveMsgValue();
	if (!expr) {
		return StackPusherHelper::tonsToBinaryString(u256{TvmConst::Message::DefaultMsgValue});
	}
	const auto& val = TVMExpressionCompiler::constValue(*expr);
	if (!val.has_value()) {
		cast_error(*expr, "Default value should be compile time expression of number type");
	}
	return StackPusherHelper::tonsToBinaryString(val.value());
}

const FunctionDefinition* FunctionCallCompiler::getRemoteFunctionDefinition(const MemberAccess* memberAccess) {
	auto expr = &memberAccess->expression();
	if (isSuper(expr))
		return nullptr;
	auto ctype = to<ContractType>(getType(expr));
	if (!ctype)
		return nullptr;
	Declaration const* decl = memberAccess->annotation().referencedDeclaration;
	auto f = to<FunctionDefinition>(decl);
	if (!f) {
		cast_error(*memberAccess, "Unsupported remote function call.");
	}
	ContractDefinition const& remoteContract = ctype->contractDefinition();
	m_pusher.push( 0, ";; Remote call " + remoteContract.name() + "." + f->name());
	return f;
}

void FunctionCallCompiler::generateExtInboundMsg(
	bool addSignature,
	const Expression *destination,
	const Expression *pubkey,
	const Expression *expire,
	const Expression *time,
	const Expression *callbackid,
	const Expression *abiVer,
	const Expression *onerrorid,
	const Expression *stateInit,
    const Expression *signBoxHandle,
	const CallableDeclaration *functionDefinition,
	const ast_vec<Expression const> arguments
) {
	const int stackSize = m_pusher.getStack().size();

	auto appendBody = [&](int builderSize) {
		/* builder size:
		2 header
		119 src abi + callbackid
		267 dest addr_std
		4 import_fee:Grams
		1 stateInit
		= 393
		*/


		// store body in a reference. In the end we store new builder to this builder
		builderSize++;
		m_pusher.stones(1); // body:(Either X ^X)
		builderSize = 0;


		for (const ASTPointer<Expression const>& arg : arguments | boost::adaptors::reversed) {
			TVMExpressionCompiler{m_pusher}.compileNewExpr(arg.get());
		}

		m_pusher.push(+1, "NEWC");
		if (addSignature) {
			builderSize += 513;
			m_pusher.stones(1);
			m_pusher.stzeroes(512);	// Signature
		} else {
			builderSize += 1;
			m_pusher.stzeroes(1);	// Signature
		}

		// store header
		// [optional]pubkey: 1 + [256] bits
		// time: 64 bits
		// expire: 32 bits


		if (pubkey != nullptr) {
			// pubkey is set
			builderSize += addSignature ? 257 : 1;
			// pubkey is optional, check whether it presents
			acceptExpr(pubkey);

			if (addSignature) {
				m_pusher.pushS(0);
				m_pusher.push(-1, "ISNULL");

				m_pusher.startContinuation();
				m_pusher.drop(1);
				m_pusher.stzeroes(1);
				m_pusher.endContinuation();

				m_pusher.startContinuation();
				m_pusher.blockSwap(1, 1);
				m_pusher.stones(1);
				m_pusher.push(0, "STU 256");
				m_pusher.endContinuation();

				m_pusher.push(0, "IFELSE");
			} else {
				m_pusher.push(-1 + 1, "ISNULL");
				m_pusher.push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::MsgWithKeyButNoSign));
				m_pusher.stzeroes(1);
			}
		}
		// if no pubkey - encode nothing

		if (time != nullptr) {
			builderSize += 64;
			acceptExpr(time);
			m_pusher.push(-1, "STUR 64");
		}

		if (expire != nullptr) {
			builderSize += 32;
			acceptExpr(expire);
			m_pusher.push(-1, "STUR 32");
		}

		// function call body
		std::vector<ASTPointer<VariableDeclaration>> funcParams;
		if (functionDefinition != nullptr)
			funcParams = functionDefinition->parameters();
		const std::vector<VariableDeclaration const*> &params = convertArray(funcParams);

		std::vector<Type const*> types;
		types = getParams(params).first;
		builderSize += 32;
		std::unique_ptr<EncodePosition> position = std::make_unique<EncodePosition>(builderSize, types);
		uint32_t functionId;
		std::optional<uint32_t> callbackFunctionId;
		ChainDataEncoder encoder{&m_pusher};
		if (functionDefinition != nullptr) {
			functionId = encoder.calculateFunctionIDWithReason(functionDefinition, ReasonOfOutboundMessage::RemoteCallInternal);
			auto fd = to<FunctionDefinition>(functionDefinition);
			solAssert(fd, "");
			if (fd->isResponsible()) {
				callbackFunctionId = 0; // set to 0, because it's ext msg and it does matter
			}
		} else {
			functionId = encoder.calculateConstructorFunctionID();
		}

		ChainDataEncoder{&m_pusher}.createMsgBody(params, functionId, callbackFunctionId, *position);

		m_pusher.push(-1, "STBREFR");
	};

	// store dest address
	acceptExpr(destination);


	// generate payload to store it as a src address with addr_extern type
	if (signBoxHandle != nullptr)
		acceptExpr(signBoxHandle);
	acceptExpr(abiVer);
	acceptExpr(onerrorid);
	acceptExpr(callbackid);
	m_pusher.push(+1, "NEWC");
	// stack: [signBoxHandle] abiVer onerrorid callbackid builder
    m_pusher.push(0, "STSLICECONST x6_"); // header 01
    if (signBoxHandle == nullptr)
        m_pusher.push(0, "STSLICECONST x264_"); // const length 76
    else {
        m_pusher.pushS(4);
        m_pusher.push(0, "ISNULL");
        m_pusher.startContinuation();
        m_pusher.push(0, "STSLICECONST x264_"); // const length 76
        m_pusher.endContinuation();
        m_pusher.startContinuation();
        m_pusher.push(0, "STSLICECONST x364_"); // const length 108
        m_pusher.endContinuation();
        m_pusher.push(-1, "IFELSE");
    }
	// stack: [signBoxHandle] abiVer onerrorid callbackid builder
	m_pusher.push(-1, "STU 32"); // stack: [signBoxHandle] abiVer onerrorid builder
	m_pusher.push(-1, "STU 32"); // stack: [signBoxHandle] abiVer builder
	m_pusher.push(-1, "STU 8");  // stack: [signBoxHandle] builder
	if (time != nullptr)
		m_pusher.push(0, "STONE");
	else
		m_pusher.push(0, "STZERO");
	if (expire != nullptr)
		m_pusher.push(0, "STONE");
	else
		m_pusher.push(0, "STZERO");
	if (pubkey != nullptr)
		m_pusher.push(0, "STONE");
	else
		m_pusher.push(0, "STZERO");
	if (signBoxHandle == nullptr)
        m_pusher.push(0, "STZERO");
	else {
		// stack: [signBoxHandle] builder
        m_pusher.pushS(1);
        m_pusher.push(0, "ISNULL");
        m_pusher.startContinuation();
        m_pusher.dropUnder(1, 1);
        m_pusher.stzeroes(1);
        m_pusher.endContinuation();
        m_pusher.startContinuation();
        m_pusher.stones(1);
        m_pusher.push(0, "STU 32");
        m_pusher.endContinuation();
        m_pusher.push(-1, "IFELSE");
	}


	std::function<void()> appendStateInit = nullptr;
	if (stateInit != nullptr)
		appendStateInit = [&]() {
			m_pusher.stones(1);
			acceptExpr(stateInit);
			m_pusher.push(-1, "STREFR");
		};

	m_pusher.prepareMsg({TvmConst::ext_msg_info::src, TvmConst::ext_msg_info::dest}, {}, appendBody, appendStateInit, StackPusherHelper::MsgType::ExternalIn);

	solAssert(stackSize + 1 == m_pusher.getStack().size(), "");
}

void FunctionCallCompiler::tvmBuildIntMsg() {
	const int stackSize = m_pusher.getStack().size();

	int destArg = -1;
	int valueArg = -1;
	int currenciesArg = -1;
	int bounceArg = -1;
	int callArg = -1;
	const std::vector<ASTPointer<ASTString>>& names = m_functionCall.names();
	for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
		switch (str2int(names[arg]->c_str())) {
			case str2int("dest"):
				destArg = arg;
				break;
			case str2int("value"):
				valueArg = arg;
				break;
			case str2int("currencies"):
				currenciesArg = arg;
				break;
			case str2int("bounce"):
				bounceArg = arg;
				break;
			case str2int("call"):
				callArg = arg;
				break;
			default:
				solUnimplemented("");
		}
	}


	auto callList = to<CallList>(m_arguments[callArg].get());
	const std::vector<ASTPointer<Expression const>> args = callList->arguments();
	auto functionDefinition = to<FunctionDefinition>(getFunctionDeclarationOrConstructor(callList->function().get()));
	const bool needCallback = functionDefinition->isResponsible();
	const int shift = needCallback ? 1 : 0;

	for (int idx = static_cast<int>(args.size()) - 1; shift <= idx; --idx) {
		ASTPointer<Expression const> const &arg = args.at(idx);
		acceptExpr(arg.get());
		m_pusher.hardConvert(functionDefinition->parameters().at(idx - shift)->type(), getType(arg.get()));
	}

	std::set<int> isParamOnStack;
	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"}};
	std::function<void(int)> appendBody = [&](int msgInfoSize){
		std::optional<uint32_t> callbackFunctionId;
		if (needCallback) {
			CallableDeclaration const* callback = getFunctionDeclarationOrConstructor(args.at(0).get());
			callbackFunctionId = ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(callback, ReasonOfOutboundMessage::RemoteCallInternal);
		}
		std::vector<VariableDeclaration const *> params = convertArray(functionDefinition->parameters());
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			params,
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(functionDefinition, ReasonOfOutboundMessage::RemoteCallInternal),
			callbackFunctionId,
			msgInfoSize,
			true
		);
	};
	if (currenciesArg != -1) {
		// TODO check constParams
		pushArgAndConvert(currenciesArg, "currencies");
		isParamOnStack.insert(TvmConst::int_msg_info::currency);
	}
	if (valueArg != -1) {
		// TODO check constParams
		pushArgAndConvert(valueArg, "value");
		isParamOnStack.insert(TvmConst::int_msg_info::tons);
	}
	if (destArg != -1) {
		// TODO check constParams
		pushArgAndConvert(destArg, "dest");
		isParamOnStack.insert(TvmConst::int_msg_info::dest);
	}
	if (bounceArg != -1) {
		// TODO check constParams
		pushArgAndConvert(bounceArg, "bounce");
		isParamOnStack.insert(TvmConst::int_msg_info::bounce);
	}

	m_pusher.prepareMsg(
		isParamOnStack,
		constParams,
		appendBody,
		nullptr,
		StackPusherHelper::MsgType::Internal
	);

	solAssert(m_pusher.getStack().size() == stackSize + 1, "");
}

void FunctionCallCompiler::tvmBuildMsgMethod() {
	int destArg = -1;
	int callArg = -1;
	int timeArg = -1;
	int expireArg = -1;
	int pubkeyArg = -1;
	int signArg = -1;
	int abiArg = -1;
	int callbackArg = -1;
	int onerrorArg = -1;
	int stateArg = -1;
	int signHandlerArg = -1;
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
				case str2int("abiVer"):
					abiArg = arg;
					break;
				case str2int("callbackId"):
					callbackArg = arg;
					break;
				case str2int("onErrorId"):
					onerrorArg = arg;
					break;
				case str2int("stateInit"):
					stateArg = arg;
					break;
				case str2int("signBoxHandle"):
					signHandlerArg = arg;
					break;
				default:
					solUnimplemented("");
			}
		}
	}
	auto funcCall = to<CallList>(m_arguments[callArg].get());
	auto functionDefinition = getFunctionDeclarationOrConstructor(funcCall->function().get());
	bool addSignature = false;
	if (signArg != -1) {
		const std::optional<bool> value = TVMExpressionCompiler::constBool(*m_arguments[signArg]);
		if (value.has_value()) {
			addSignature = *value;
		}
	}

	generateExtInboundMsg(addSignature, m_arguments[destArg].get(),
										  (pubkeyArg != -1) ? m_arguments[pubkeyArg].get() : nullptr,
										  (expireArg != -1) ? m_arguments[expireArg].get() : nullptr,
										  (timeArg != -1) ? m_arguments[timeArg].get() : nullptr,
										  m_arguments[callbackArg].get(),
										  m_arguments[abiArg].get(),
										  m_arguments[onerrorArg].get(),
										  (stateArg != -1) ? m_arguments[stateArg].get() : nullptr,
										  (signHandlerArg != -1) ? m_arguments[signHandlerArg].get() : nullptr,
										  functionDefinition, funcCall->arguments());
}

bool FunctionCallCompiler::checkForTvmDeployMethods(MemberAccess const &_node, Type::Category category) {
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
		const std::vector<ASTPointer<ASTString>>& names = m_functionCall.names();
		if (names.empty()) {
			solAssert(m_arguments.size() == 2 || m_arguments.size() == 3, "");
			exprs[StateInitMembers::Code] = [&](){
				pushArgAndConvert(0);
			};
			exprs[StateInitMembers::Data] = [&](){
				pushArgAndConvert(1);
			};
			if (m_arguments.size() >= 3) {
				exprs[StateInitMembers::SplitDepth] = [&](){
					pushArgAndConvert(2);
				};
			}
		} else {


			bool dataIsSet = false;
			// string("code"), string("data"), string("splitDepth"), string("varInit"), string("pubkey")
			for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
				switch (str2int(names[arg]->c_str())) {
					case str2int("code"):
						codeArg = arg;
						exprs[StateInitMembers::Code] = [this, codeArg, name = *names.at(arg)](){
							pushArgAndConvert(codeArg, name);
						};
						break;
					case str2int("data"):
						dataArg = arg;
						exprs[StateInitMembers::Data] = [this, dataArg, name = *names.at(arg)](){
							pushArgAndConvert(dataArg, name);
						};
						dataIsSet = true;
						break;
					case str2int("splitDepth"):
						depthArg = arg;
						exprs[StateInitMembers::SplitDepth] = [this, depthArg, name = *names.at(arg)](){
							pushArgAndConvert(depthArg, name);
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
						pushArgAndConvert(keyArg, "pubkey");
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
		m_pusher.pushMacroCallInCallRef(-2 + 1, "insert_pubkey_macro");
		return true;
	}

	if (_node.memberName() == "deploy") {
		const std::function<void()> pushWid = [&](){
			pushArgAndConvert(3);
		};
		const std::function<void()> pushValue = [&](){
			pushArgAndConvert(2);
		};
		const std::function<void(int builderSize)> appendBody = [&](int /*builderSize*/){
			m_pusher.stones(1);
			pushArgAndConvert(1);
			m_pusher.push(-1, "STREFR");
		};
		const std::function<void()> pushSendrawmsgFlag;
		pushArgAndConvert(0);
		// stack: stateInit
		deployNewContract(pushWid, pushValue, {}, {}, appendBody, pushSendrawmsgFlag, 0);
		// stack: destAddress
		return true;
	}

	return false;
}

void FunctionCallCompiler::sliceMethods(MemberAccess const &_node) {
	if (_node.memberName() == "empty") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "SEMPTY");
	} else if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher.push(-2 + 3, "SDATASIZE");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
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
	} else if (_node.memberName() == "compare") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher.push(-2+1, "SDLEXCMP");
	} else if (_node.memberName() == "hasNBits") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher.push(-2+1, "SCHKBITSQ");
	} else if (_node.memberName() == "hasNRefs") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher.push(-2+1, "SCHKREFSQ");
	} else if (_node.memberName() == "hasNBitsAndRefs") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		pushArgAndConvert(1);
		m_pusher.push(-3+1, "SCHKBITREFSQ");
	} else if (_node.memberName() == "refs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "SREFS");
	} else if (_node.memberName() == "depth") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "SDEPTH");
	} else if (_node.memberName() == "decode") {
		const int stackSize = m_pusher.getStack().size();
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		TypePointers targetTypes;
		if (auto const* targetTupleType = dynamic_cast<TupleType const*>(m_retType))
			targetTypes = targetTupleType->components();
		else
			targetTypes = TypePointers{m_retType};

		ChainDataDecoder decode{&m_pusher};
		std::unique_ptr<DecodePosition> pos = std::make_unique<DecodePositionFromOneSlice>();
		decode.decodeParameters(targetTypes, *pos, false);

		m_exprCompiler->collectLValue(lValueInfo, true, false);
		solAssert(stackSize + static_cast<int>(targetTypes.size()) == m_pusher.getStack().size(), "");
	} else if (_node.memberName() == "decodeFunctionParams") {
		const int saveStackSize = m_pusher.getStack().size();
		CallableDeclaration const* functionDefinition = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		const LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(),true, false, _node.expression().annotation().isLValue);
		// lvalue.. slice
		if (functionDefinition) {
			auto fd = to<FunctionDefinition>(functionDefinition);
			bool isResponsible = fd->isResponsible();
			if (isResponsible) {
				m_pusher.push(-1 + 2, "LDU 32");
			}
			// lvalue.. callback slice
			ChainDataDecoder decoder{&m_pusher};

			vector<Type const*> types = getParams(functionDefinition->parameters()).first;
			decoder.decodePublicFunctionParameters(types, isResponsible);

			const int saveStackSize2 = m_pusher.getStack().size();
			const int paramQty = functionDefinition->parameters().size() + (isResponsible ? 1 : 0);
			m_pusher.blockSwap(saveStackSize2 - saveStackSize - paramQty, paramQty);

			m_pusher.push(+1, "PUSHSLICE x8_");
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (boost::starts_with(_node.memberName(), "load")) {
		const LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		if (_node.memberName() == "loadRefAsSlice") {
			m_pusher.push(-1 + 2, "LDREFRTOS");
			m_pusher.exchange(0, 1);
		} else if (_node.memberName() == "loadRef") {
			m_pusher.push(-1 + 2, "LDREF");
		} else if (_node.memberName() == "loadUnsigned" || _node.memberName() == "loadSigned") {
			std::string cmd = "LD";
			cmd += (_node.memberName() == "loadSigned" ? "I" : "U");
			const auto& val = TVMExpressionCompiler::constValue(*m_arguments[0]);
			if (val.has_value()) {
				if (val < 1 || val > 256) {
					cast_error(*m_arguments[0], "The value must be in the range 1 - 256.");
				}
				m_pusher.push(-1 + 2, cmd + " " + val.value().str());
			} else {
				pushArgAndConvert(0);
				m_pusher.push(-2 + 2, cmd + "X");
			}
		} else if (_node.memberName() == "loadTons") {
			m_pusher.push(-1 + 2, "LDVARUINT16");
		} else if (_node.memberName() == "loadSlice") {
			if (m_arguments.size() == 1) {
				const auto& value = TVMExpressionCompiler::constValue(*m_arguments[0].get());
				if (value.has_value()) {
					m_pusher.push(+1, "LDSLICE " + value.value().str());
				} else {
					pushArgAndConvert(0);
					m_pusher.push(-2+2, "LDSLICEX");
				}
			} else {
				pushArgAndConvert(0);
				pushArgAndConvert(1);
				m_pusher.push(-3+2, "SPLIT");
			}
		} else {
			solUnimplemented("");
		}

		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "skip") {
		const LValueInfo lValueInfo =
				m_exprCompiler->expandLValue(&_node.expression(), true, false, _node.expression().annotation().isLValue);
		if (m_arguments.size() == 1) {
			pushArgAndConvert(0);
			m_pusher.push(-2+1, "SDSKIPFIRST");
		} else {
			pushArgAndConvert(0);
			pushArgAndConvert(1);
			m_pusher.push(-3+1, "SSKIPFIRST");
		}
		m_exprCompiler->collectLValue(lValueInfo, true, false);
	} else {
		solUnimplemented("");
	}
}


bool FunctionCallCompiler::checkForTvmTupleMethods(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TvmTuple)
		return false;

	if (_node.memberName() == "push") {
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		acceptExpr(m_arguments[0].get());
		m_pusher.push(-1, "TPUSH");
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "pop") {
		const int stackSize = m_pusher.getStack().size();
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		const int stackChange = m_pusher.getStack().size() - stackSize;
		solAssert(stackChange >= 1, "Wrong stack change");
		m_pusher.push(+1, "TPOP");
		m_pusher.blockSwap(stackChange, 1);
		m_exprCompiler->collectLValue(lValueInfo, true, false);
		const int finalChange = m_pusher.getStack().size() - stackSize;
		solAssert(finalChange == 1, "Wrong stack change");
		return true;
	}

	if (_node.memberName() == "length") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "TLEN");
		return true;
	}

	if (_node.memberName() == "empty") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "TLEN");
		m_pusher.push(-1 + 1, "ISZERO");
		return true;
	}

	return false;
}

bool FunctionCallCompiler::checkForTvmBuilderMethods(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TvmBuilder)
		return false;

	if (boost::starts_with(_node.memberName(), "store")) {
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);

		if (_node.memberName() == "storeOnes") {
			pushArgs();
			m_pusher.push(-2 + 1, "STONES");
		} else if (_node.memberName() == "storeZeros") {
			pushArgs();
			m_pusher.push(-2 + 1, "STZEROES");
		} else if (_node.memberName() == "storeRef") {
			pushArgAndConvert(0);
			Type::Category cat = m_arguments.at(0)->annotation().type->category();
			switch (cat) {
				case Type::Category::TvmBuilder:
					m_pusher.push(-1, "STBREFR");
					break;
				case Type::Category::TvmCell:
					m_pusher.push(-1, "STREFR");
					break;
				case Type::Category::TvmSlice:
					m_pusher.push(+1, "NEWC");
					m_pusher.push(-2 + 1, "STSLICE");
					m_pusher.push(-1, "STBREFR");
					break;
				default:
					solUnimplemented("");
			}
		} else if (_node.memberName() == "store") {
			int args = 0;
			for (const auto &argument: m_arguments | boost::adaptors::reversed) {
				if (TVMExpressionCompiler::constBool(*argument)) {
					continue;
				}
				acceptExpr(argument.get());
				++args;
			}
			m_pusher.blockSwap(1, args);
			for (const auto &argument: m_arguments) {
				std::optional<bool> value = TVMExpressionCompiler::constBool(*argument);
				if (value) {
					if (*value)
						m_pusher.stones(1);
					else
						m_pusher.stzeroes(1);
				} else {
					m_pusher.store(argument->annotation().type->mobileType(), false);
				}
			}
		} else if (_node.memberName() == "storeSigned" || _node.memberName() == "storeUnsigned") {
			std::string cmd = "ST";
			cmd += (_node.memberName() == "storeSigned" ? "I" : "U");
			pushArgAndConvert(0);
			const auto& val = TVMExpressionCompiler::constValue(*m_arguments[1]);
			if (val.has_value()) {
				if (val < 1 || val > 256) {
					cast_error(*m_arguments[1], "The value must be in the range 1 - 256.");
				}
				m_pusher.push(-1, cmd + "R " + val.value().str());
			} else {
				pushArgAndConvert(1);
				m_pusher.push(-2, cmd + "XR");
			}
		} else if (_node.memberName() == "storeTons") {
			pushArgAndConvert(0);
			m_pusher.push(-1, "STGRAMS");
		} else {
			solUnimplemented("");
		}

		m_exprCompiler->collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "bits") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "BBITS");
		return true;
	}

	if (_node.memberName() == "refs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "BREFS");
		return true;
	}

	if (_node.memberName() == "bitsAndRefs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+2, "BBITREFS");
		return true;
	}

	if (_node.memberName() == "remBits") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "BREMBITS");
		return true;
	}

	if (_node.memberName() == "remRefs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "BREMREFS");
		return true;
	}

	if (_node.memberName() == "remBitsAndRefs") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+2, "BREMBITREFS");
		return true;
	}

	if (_node.memberName() == "toCell") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "ENDC");
		return true;
	}

	if (_node.memberName() == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1+1, "ENDC");
		m_pusher.push(-1+1, "CTOS");
		return true;
	}

	if (_node.memberName() == "depth") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "BDEPTH");
		return true;
	}

	return false;
}

void FunctionCallCompiler::arrayMethods(MemberAccess const &_node) {
	Type const *type = _node.expression().annotation().type;
	if (_node.memberName() == "empty") {
		acceptExpr(&_node.expression());
		if (isUsualArray(type)) {
			m_pusher.index(0);
			m_pusher.push(-1 + 1, "EQINT 0");
		} else {
			m_pusher.push(-1 + 1, "CTOS");
			m_pusher.push(-1 + 1, "SEMPTY");
		}
	} else if (_node.memberName() == "substr") {
		acceptExpr(&_node.expression());
		pushArgs();
		if (m_arguments.size() == 1) {
			m_pusher.pushInt(-1);
		}
		m_pusher.pushMacroCallInCallRef(-3 +1, "__substr_macro");
	} else if (_node.memberName() == "byteLength") {
		acceptExpr(&_node.expression());
		m_pusher.byteLengthOfCell();
	} else if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher.push(-2  + 3, "CDATASIZE");
	} else if (_node.memberName() == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher.push(-1 + 1, "CTOS");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
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
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		auto arrayBaseType = to<ArrayType>(getType(&_node.expression()))->baseType();
		const IntegerType key = getKeyTypeOfArray();
		bool isValueBuilder{};
		if (m_arguments.empty()) {
			isValueBuilder = true;
			m_pusher.pushDefaultValue(arrayBaseType, true);
		} else {
			isValueBuilder = false;
			pushArgs();
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
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
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
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), true);
		pushArgAndConvert(0);
		m_pusher.pushMacroCallInCallRef(-2 +1, "concatenateStrings_macro");
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
		Type::Category valueCat = m_retType->category();
		if (auto tt = to<TupleType>(m_retType)) {
			m_pusher.untuple(tt->components().size());
		} else if (isIn(valueCat, Type::Category::Mapping, Type::Category::Optional)) {
			m_pusher.untuple(1);
		}
		return true;
	}

	if (_node.memberName() == "set") {
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), false);
		pushArgs();
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
		const LValueInfo lValueInfo = m_exprCompiler->expandLValue(&_node.expression(), false);
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
		pushArgAndConvert(0);
		m_pusher.push(-2 + 3, "CDATASIZE");
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
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
			const auto& value = TVMExpressionCompiler::constValue(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::tons] = StackPusherHelper::tonsToBinaryString(u256(value.value()));
			} else {
				exprs[TvmConst::int_msg_info::tons] = expr;
			}
		};

		auto setBounce = [&](auto expr){
			const std::optional<bool> value = TVMExpressionCompiler::constBool(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::bounce] = value.value() ? "1" : "0";
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
				pushSendrawmsgFlag = [&]() {
					pushArgAndConvert(2);
				};
			}
			if (argumentQty >= 4) {
				appendBody = [&](int /*size*/) {
					m_pusher.stones(1);
					pushArgAndConvert(3);
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
		m_pusher.push(-1 + 2, "REWRITESTDADDR");
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
		pushArgAndConvert(0);
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
		pushArgs();
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
			pushArgs();
			if (m_arguments.empty()) {
				m_pusher.push(+1, "RANDU256");
			} else {
				m_pusher.push(-1 + 1, "RAND");
			}
			break;
		case FunctionType::Kind::RndSetSeed:
		{
			pushArgAndConvert(0);
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
				pushArgs();
			}
			m_pusher.push(-1, "ADDRAND");
			break;
		}
		default:
			cast_error(_node, "Unsupported function call");
	}
}

bool FunctionCallCompiler::checkForTvmFunction(const MemberAccess &_node) {
	if (_node.memberName() == "pubkey") { // tvm.pubkey
		m_pusher.getGlob(TvmConst::C7::TvmPubkey);
	} else if (_node.memberName() == "setPubkey") { // tvm.setPubkey
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::TvmPubkey);
	} else if (_node.memberName() == "accept") { // tvm.accept
		m_pusher.push(0, "ACCEPT");
	} else if (_node.memberName() == "hash") { // tvm.hash
		pushArgs();
		switch (m_arguments.at(0)->annotation().type->category()) {
			case Type::Category::TvmCell:
			case Type::Category::Array:
			case Type::Category::StringLiteral:
				m_pusher.push(0, "HASHCU");
				break;
			case Type::Category::TvmSlice:
				m_pusher.push(0, "HASHSU");
				break;
			default:
				solUnimplemented("");
		}
	} else if (_node.memberName() == "checkSign") { // tvm.checkSign
		size_t cnt = m_arguments.size();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmSlice) {
			pushArgs();
			m_pusher.push(-3+1, "CHKSIGNS");
		} else {
			pushArgAndConvert(0);
			if (cnt == 4) {
				m_pusher.push(+1, "NEWC");
				pushArgAndConvert(1);
				m_pusher.push(-1, "STUR 256");
				pushArgAndConvert(2);
				m_pusher.push(-1, "STUR 256");
				m_pusher.push(0, "ENDC");
				m_pusher.push(0, "CTOS");
			} else {
				pushArgAndConvert(1);
			}
			pushArgAndConvert(cnt - 1);
			m_pusher.push(-3+1, "CHKSIGNU");
		}
	} else if (_node.memberName() == "setcode") { // tvm.setcode
		pushArgs();
		m_pusher.push(-1, "SETCODE");
	} else if (_node.memberName() == "bindump") { // tvm.bindump
		pushArgs();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmCell)
			m_pusher.push(-1+1, "CTOS");
		m_pusher.push(0, "BINDUMP");
		m_pusher.drop(1);
	} else if (_node.memberName() == "hexdump") { // tvm.hexdump
		pushArgs();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmCell)
		m_pusher.push(-1+1, "CTOS");
		m_pusher.push(0, "HEXDUMP");
		m_pusher.drop(1);
	} else if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
		pushArgs();
		string code = R"(
CTOS
DUP
PUSHSLICE xSelectorRootCodeCell
SDEQ
PUSHCONT {
	PLDREFIDX 1
	CTOS
}
IF
PLDREF
CTOS
BLESS
POP c3
)";
		boost::replace_all(code, "SelectorRootCodeCell", TvmConst::Selector::RootCodeCell());
		m_pusher.pushLines(code);
		m_pusher.push(-1, "");
	} else if (_node.memberName() == "setData") { // tvm.setData
		pushArgs();
		m_pusher.push(-1, "POP C4");
	} else if (_node.memberName() == "rawCommit") { // tvm.rawCommit
		m_pusher.push(0, "COMMIT");
	} else if (_node.memberName() == "commit") { // tvm.commit
		m_pusher.pushMacroCallInCallRef(0, "c7_to_c4");
		m_pusher.push(0, "COMMIT");
	} else if (_node.memberName() == "log") { // tvm.log
		compileLog();
	} else if (_node.memberName() == "resetStorage") { //tvm.resetStorage
		m_pusher.resetAllStateVars();
	} else if (_node.memberName() == "functionId") { // tvm.functionId
		auto callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		ChainDataEncoder encoder(&m_pusher);
		uint32_t funcID;
		if (callDef == nullptr) {
			funcID = encoder.calculateConstructorFunctionID();
		} else {
			bool isManuallyOverridden{};
			std::tie(funcID, isManuallyOverridden) = encoder.calculateFunctionID(callDef);
			if (!isManuallyOverridden) {
				funcID &= 0x7FFFFFFFu;
			}
		}
		m_pusher.pushInt(funcID);
	} else if (_node.memberName() == "encodeBody") { // tvm.encodeBody
		CallableDeclaration const* callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		if (callDef == nullptr) { // if no constructor (default constructor)
			m_pusher.push(+1, "NEWC");
			ChainDataEncoder{&m_pusher}.createDefaultConstructorMessage2();
		} else {
			auto funcDef = to<FunctionDefinition>(callDef);
			const bool needCallback = funcDef->isResponsible();
			const int shift = needCallback ? 1 : 0;
			std::optional<uint32_t> callbackFunctionId;
			if (needCallback) {
				CallableDeclaration const* callback = getFunctionDeclarationOrConstructor(m_arguments.at(1).get());
				callbackFunctionId = ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(callback, ReasonOfOutboundMessage::RemoteCallInternal);
			}
			const ast_vec<VariableDeclaration> &parameters = callDef->parameters();
			std::vector<Type const *> types = getParams(parameters).first;
			EncodePosition position{32, types};
			for (int i = m_arguments.size() - 1; i >= 1 + shift; --i) {
				TVMExpressionCompiler{m_pusher}.compileNewExpr(m_arguments.at(i).get());
			}
			m_pusher.push(+1, "NEWC");
			ChainDataEncoder{&m_pusher}.createMsgBody(
				convertArray(parameters),
				ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(callDef, ReasonOfOutboundMessage::RemoteCallInternal),
				callbackFunctionId,
				position
			);
		}
		m_pusher.push(-1 + 1, "ENDC");
	} else if (_node.memberName() == "rawReserve") {
		pushArgs();
		int n = m_arguments.size();
		solAssert(isIn(n, 2, 3), "");
		m_pusher.push(-n, n == 2? "RAWRESERVE" : "RAWRESERVEX");
	} else if (isIn(_node.memberName(), "exit", "exit1")) {
		m_pusher.was_c4_to_c7_called();
		m_pusher.push(-1, ""); // fix stack

		m_pusher.startIfNotRef();
		m_pusher.pushMacroCallInCallRef(0, "c7_to_c4");
		m_pusher.endContinuation();

		if (_node.memberName() == "exit")
			m_pusher.push(0, "THROW 0");
		else
			m_pusher.push(0, "THROW 1");
	} else if (_node.memberName() == "code") {
		m_pusher.getGlob(TvmConst::C7::MyCode);

		m_pusher.push(0, "PUSHREF {");
		m_pusher.push(0, "\tDUP");
		m_pusher.push(0, "\tSETGLOB 1");
		m_pusher.push(0, "\tBLESS");
		m_pusher.push(0, "\tJMPX");
		m_pusher.push(0, "}");
		m_pusher.push(+1, ""); // fix ref

		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "STSLICECONST x" + TvmConst::Selector::RootCodeCell()); // root selector
		m_pusher.push(-1, "STREF"); //
		m_pusher.push(-1, "STSLICE"); // main selector + salt
		m_pusher.push(-1 + 1, "ENDC");
		m_pusher.ctx().setSaveMyCodeSelector();
	} else if (_node.memberName() == "codeSalt") {
		pushArgs();
		string getSaltFromUsualSelector = R"(
			PLDREF
			CTOS

			PUSHSLICE xPrivateOpcode0
			SDBEGINSX

			LDDICT
			NIP

			LDU 10
			NIP

			PUSHSLICE xPrivateOpcode1
			SDBEGINSX

			DUP
			SREFS
			GTINT 1
			PUSHCONT {
				PLDREFIDX 1
			}
			PUSHCONT {
				DROP
				NULL
			}
			IFELSE
)";
		string code = R"(
CALLREF {
	CTOS
	PUSH S0
	PUSHSLICE xSelectorRootCodeCell
	SDEQ
	PUSHREFCONT {
		PLDREFIDX 1
		CTOS
		CALLREF {
			getSaltFromUsualSelector
		}
	}
	PUSHREFCONT {
		getSaltFromUsualSelector
	}
	IFELSE
}
)";
		boost::replace_all(code, "SelectorRootCodeCell", TvmConst::Selector::RootCodeCell());
		boost::replace_all(code, "getSaltFromUsualSelector", getSaltFromUsualSelector);
		boost::replace_all(code, "PrivateOpcode0", TvmConst::Selector::PrivateOpcode0());
		boost::replace_all(code, "PrivateOpcode1", TvmConst::Selector::PrivateOpcode1());
		m_pusher.pushLines(code);
	} else if (_node.memberName() == "setCodeSalt") {
		pushArgAndConvert(0);
		m_pusher.push(-1 + 1, "CTOS"); // sliceCode
		pushArgAndConvert(1); // sliceCode salt
		string insertSaltInUsualSelector = R"(
			LDREFRTOS  ; selfCallCode salt restUsualSelector intSelector

			PUSHSLICE xPrivateOpcode0
			SDBEGINSX
			LDDICT     ; selfCallCode salt restUsualSelector dict intSelector
			LDU 10
			NIP
			DUP
			SREFS      ; selfCallCode salt restUsualSelector dict intSelector refs
			PUSHCONT {
				LDREF
			}
			PUSHCONT {
				PUSHREF {
				}
				SWAP
			}
			IFELSE
		                ; selfCallCode salt restUsualSelector dict version intSelector
			PUSHSLICE xPrivateOpcode1
			SDBEGINSX
			DROP
			            ; selfCallCode salt restUsualSelector dict version
			SWAP        ; selfCallCode salt restUsualSelector version dict
			NEWC        ; selfCallCode salt restUsualSelector version dict builder
			STSLICECONST xPrivateOpcode0 ; DICTPUSHCONST
			STDICT
			PUSHINT 32
			STUR 10
			STSLICECONST xPrivateOpcode1 ; DICTUGETJMP
			STREF       ; selfCallCode salt restUsualSelector builder
			XCHG S1, S2 ; selfCallCode restUsualSelector salt builder
			STREF       ; selfCallCode restUsualSelector builder
			NEWC        ; selfCallCode restUsualSelector builder usualBuilder
			STBREF      ; selfCallCode restUsualSelector usualBuilder
			STSLICE     ; selfCallCode usualBuilder
)";

		string code = R"(
CALLREF {
	PUSH S1
	PUSHSLICE xSelectorRootCodeCell
	SDEQ
	PUSHREFCONT {
		SWAP      ; salt sliceCode
		LDREF
		LDREF
		DROP         ; salt selfCallCode usualSelector
		XCHG S1, S2  ; selfCallCode salt usualSelector
		CTOS         ; selfCallCode salt usualSelector
		CALLREF {
			insertSaltInUsualSelector
		}
		NEWC        ; selfCallCode usualBuilder mainBuilder
		STSLICECONST xSelectorRootCodeCell
		XCHG S1, S2 ; usualBuilder selfCallCode mainBuilder
		STREF
		STBREF
		ENDC
	}
	PUSHREFCONT {
		SWAP
		CALLREF {
			insertSaltInUsualSelector
		}
		ENDC
	}
	IFELSE
}
)";
		boost::replace_all(code, "insertSaltInUsualSelector", insertSaltInUsualSelector);
		boost::replace_all(code, "SelectorRootCodeCell", TvmConst::Selector::RootCodeCell());
		boost::replace_all(code, "PrivateOpcode0", TvmConst::Selector::PrivateOpcode0());
		boost::replace_all(code, "PrivateOpcode1", TvmConst::Selector::PrivateOpcode1());
		m_pusher.pushLines(code);
		m_pusher.push(-2 + 1, ""); // fix stack
	} else if (_node.memberName() == "replayProtTime") {
		m_pusher.getGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "setReplayProtTime") {
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "replayProtInterval") {
		m_pusher.pushInt(TvmConst::Message::ReplayProtection::Interval);
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::mathFunction(const MemberAccess &_node) {
	auto retTuple = to<TupleType>(m_retType);
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
		if (m_retType->category() == Type::Category::FixedPoint) {
			int power = to<FixedPointType>(m_retType)->fractionalDigits();
			m_pusher.pushInt(StackPusherHelper::pow10(power)); // res 10^n
			m_pusher.exchange(0, 1);
			m_pusher.push(-3 + 1, "MUL" + boost::to_upper_copy<std::string>(_node.memberName()));
		} else {
			m_pusher.push(-2 + 1, boost::to_upper_copy<std::string>(_node.memberName()));
		}
	} else if (isIn(_node.memberName(), "muldiv", "muldivr", "muldivc")) {
		pushArgs();
		m_pusher.push(-3 + 1, boost::to_upper_copy<std::string>(_node.memberName()));
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.checkFit(m_retType);
		}
	} else if (_node.memberName() == "divmod") {
		pushArgs();
		m_pusher.push(-2 + 2, "DIVMOD");
	} else if (_node.memberName() == "muldivmod") {
		pushArgs();
		m_pusher.push(-3 + 2, "MULDIVMOD");
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.push(0, "SWAP");
			m_pusher.checkFit(retTuple->components().at(0));
			m_pusher.push(0, "SWAP");
		}
	} else if (_node.memberName() == "abs") {
		pushArgs();
		m_pusher.push(-1 + 1, "ABS");
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.checkFit(m_retType);
		}
	} else if (_node.memberName() == "modpow2") {
		pushExprAndConvert(m_arguments[0].get(), m_retType);
		const Expression * expression = m_arguments[1].get();
		const auto& value = TVMExpressionCompiler::constValue(*expression);
		if (value.has_value()) {
			if (value < 0 || value >= 256) {
				cast_error(m_functionCall, "Second argument must be in the range 1 - 255.");
			}
			m_pusher.push(-1 + 1, "MODPOW2 " + value.value().str());
		} else {
			cast_error(m_functionCall, "Second argument must be a constant integer.");
		}
	} else if (_node.memberName() == "sign") {
		pushArgs();
		m_pusher.push(-1 + 1, "SGN");
	} else {
		cast_error(m_functionCall, "Unsupported function call");
	}
}

bool FunctionCallCompiler::checkBaseContractCall(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TypeType)
		return false;
	if (to<Identifier>(&_node.expression())) {
		if (auto functionType = to<FunctionType>(getType(&_node))) {
			// calling base contract method
			pushArgs();
			auto fd = to<FunctionDefinition>(&functionType->declaration());
			const std::string functionName = m_pusher.ctx().getFunctionInternalName(fd);
			m_pusher.pushCallOrCallRef(functionName, functionType);
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

void FunctionCallCompiler::typeConversion() {
	Type::Category argCategory = m_arguments[0]->annotation().type->category();
	Type const* argType = m_arguments[0]->annotation().type;
	Type const* resultType = m_functionCall.annotation().type;
	solAssert(m_arguments.size() == 1, "");

	auto conversionToAddress = [&](){
		switch (argCategory) {
			case Type::Category::Contract:
			case Type::Category::Address:
				acceptExpr(m_arguments.at(0).get()); // it's correct
				break;
			case Type::Category::RationalNumber:
			case Type::Category::Integer: {
				auto literal = to<Literal>(m_arguments[0].get());
				if (literal) {
					m_pusher.literalToSliceAddress(literal);
				} else {
					acceptExpr(m_arguments.at(0).get()); // it's correct
					m_pusher.push(+1, "NEWC");
					m_pusher.push(-1 + 1, "STSLICECONST x801_"); // addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 // 10 0  00000000 1 = 801
					m_pusher.push(-1, "STU 256"); // address:bits256
					m_pusher.push(-1 + 1, "ENDC");
					m_pusher.push(-1 + 1, "CTOS");
				}
				break;
			}
			default:
				solUnimplemented("");
		}
	};

	if (auto etn = to<ElementaryTypeNameExpression>(&m_functionCall.expression())) {
		switch (etn->type().typeName().token()) {
			case Token::Address: {
				conversionToAddress();
				return;
			}
			default:
				break;
		}
	} else if (auto identifier = to<Identifier>(&m_functionCall.expression())) {
		if (to<ContractDefinition>(identifier->annotation().referencedDeclaration)) {
			conversionToAddress();
			return;
		} else if (auto enumDef = to<EnumDefinition>(identifier->annotation().referencedDeclaration)) {

			const auto& value = TVMExpressionCompiler::constValue(*m_arguments[0]);
			if (value.has_value()) {
				if (value < 0 || value >= enumDef->members().size()) {
					cast_error(m_functionCall, "The value must be in the range 1 - " +
											   toString(enumDef->members().size()) + ".");
				}
				m_pusher.push(+1, "PUSHINT " + value.value().str());
				return;
			}

			acceptExpr(m_arguments[0].get()); // it's correct
			m_pusher.push(+1, "DUP");
			m_pusher.pushInt(enumDef->members().size());
			m_pusher.push(-1, "GEQ");

			auto type = m_arguments[0].get()->annotation().type;
			TypeInfo ti(type);
			if (!ti.isNumeric || ti.isSigned) {
				m_pusher.push(+1, "OVER");
				m_pusher.push(0, "ISNEG");
				m_pusher.push(-1, "OR");
			}
			m_pusher.push(-1, "THROWIF 5"); // TODO set normal error code
			return;
		}
	}


	solAssert(m_arguments.size() == 1, "");
	acceptExpr(m_arguments.at(0).get()); // it's correct
	m_pusher.push(0, ";; " + resultType->toString());
	m_pusher.hardConvert(resultType, argType);
}

bool FunctionCallCompiler::checkLocalFunctionOrLibCall(const Identifier *identifier) {
	const string& functionName = identifier->name();
	auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
	if (!functionDefinition)
		return false;
	auto functionType = to<FunctionType>(getType(identifier));
	pushArgs();
	if (functionDefinition->isInline()) {
		auto codeLines = m_pusher.ctx().getInlinedFunction(functionName);
		int nParams = functionType->parameterTypes().size();
		int nRetVals = functionType->returnParameterTypes().size();

		m_pusher.push(codeLines);
		m_pusher.push(-nParams + nRetVals, "");
	} else {
		Declaration const& funDecl = m_funcType->declaration();
		ContractDefinition const* contractDecl= funDecl.annotation().contract;
		bool isLib = contractDecl->isLibrary();
		std::string name;
		if (isLib) {
			auto fd = to<FunctionDefinition>(&funDecl);
			name = m_pusher.ctx().getLibFunctionName(fd, false);
			m_pusher.ctx().addLib(fd);
		} else {
			name = m_pusher.ctx().getFunctionInternalName(functionDefinition, false);
		}
		m_pusher.pushCallOrCallRef(name, functionType);
	}
	return true;
}

bool FunctionCallCompiler::checkSolidityUnits() {
	auto funcType = to<FunctionType>(m_functionCall.expression().annotation().type);
	if (funcType == nullptr) {
		return false;
	}

	auto checkAndParseExceptionCode = [](Expression const* e) -> std::optional<bigint> {
		const auto& val = TVMExpressionCompiler::constValue(*e);
		if (val.has_value()) {
			if (val >= 65536 || val < 0) {
				cast_error(*e, "Exception code must be in the range 2 - 65535.");
			}
			return val;
		}
		TypeInfo ti{e->annotation().type};
		if (ti.category != Type::Category::Integer || ti.isSigned) {
			cast_error(*e, "Exception code must be an unsigned number.");
		}
		return {};
	};

	switch (funcType->kind()) {
		case FunctionType::Kind::GasToValue: {
			pushArgs();
			m_pusher.pushMacroCallInCallRef(-2 + 1, "__gasToTon_macro");
			return true;
		}
		case FunctionType::Kind::ValueToGas: {
			pushArgs();
			m_pusher.pushMacroCallInCallRef(-2 + 1, "__tonToGas_macro");
			return true;
		}

		case FunctionType::Kind::SHA256: { // "sha256"
			pushArgAndConvert(0);
			Type const* arg = m_funcType->parameterTypes().at(0);
			auto arrType = to<ArrayType>(arg);
			if (arrType && arrType->isByteArray()) {
				m_pusher.push(0, "CTOS");
			}
			m_pusher.push(0, "SHA256U");
			return true;
		}

		case FunctionType::Kind::Selfdestruct: { // "selfdestruct"
			const std::map<int, std::string> constParams{
					{TvmConst::int_msg_info::ihr_disabled, "1"},
					{TvmConst::int_msg_info::tons,         StackPusherHelper::tonsToBinaryString(u256(0))},
					{TvmConst::int_msg_info::bounce,       "0"},
			};
			m_pusher.sendIntMsg(
					{{TvmConst::int_msg_info::dest, m_arguments[0].get()}},
					constParams,
					nullptr,
					[&]() { m_pusher.push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::SelfDestruct)); });
			return true;
		}

		case FunctionType::Kind::Require: {
			if (m_arguments.size() == 1) {
				pushArgAndConvert(0);
				m_pusher.push(-1, "THROWIFNOT 100");
			} else if (m_arguments.size() == 2 || m_arguments.size() == 3) {
				if (m_arguments.size() == 3)
					pushArgAndConvert(2);
				const auto &exceptionCode = checkAndParseExceptionCode(m_arguments[1].get());
				if (exceptionCode.has_value() && exceptionCode.value() <= 1) {
					cast_error(*m_arguments[1].get(), "Error code must be at least two");
				}
				if (exceptionCode.has_value() && exceptionCode.value() < 2048) {
					pushArgAndConvert(0);
					if (m_arguments.size() == 3)
						m_pusher.push(-2, "THROWARGIFNOT " + toString(exceptionCode.value()));
					else
						m_pusher.push(-1, "THROWIFNOT " + toString(exceptionCode.value()));
				} else {
					pushArgAndConvert(1);
					if (!exceptionCode.has_value()) {
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
					pushArgAndConvert(0);
					if (m_arguments.size() == 3)
						m_pusher.push(-3, "THROWARGANYIFNOT");
					else
						m_pusher.push(-2, "THROWANYIFNOT");
				}
			} else {
				cast_error(m_functionCall, R"("require" takes from one to three m_arguments.)");
			}
			return true;
		}
		case FunctionType::Kind::Revert: {
			if (m_arguments.empty()) {
				m_pusher.push(0, "THROW 100");
			} else {
				if (!isIn(static_cast<int>(m_arguments.size()), 1, 2)) {
					cast_error(m_functionCall, R"("revert" takes up to two m_arguments.)");
				}
				const auto &exceptionCode = checkAndParseExceptionCode(m_arguments[0].get());
				bool withArg = m_arguments.size() == 2;
				if (withArg) {
					pushArgAndConvert(1);
				}
				if (exceptionCode.has_value() && exceptionCode.value() <= 1) {
					cast_error(*m_arguments[0].get(), "Error code must be at least two");
				}
				if (exceptionCode.has_value() && exceptionCode.value() < 2048) {
					m_pusher.push(withArg ? -1 : 0, (withArg ? "THROWARG " : "THROW ") + toString(exceptionCode.value()));
				} else {
					pushArgAndConvert(0);
					if (!exceptionCode.has_value()) {
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
					m_pusher.push(withArg ? -2 : -1, withArg ? "THROWARGANY" : "THROWANY");
				}

			}
			return true;
		}
		case FunctionType::Kind::LogTVM: {
			compileLog();
			return true;
		}
		case FunctionType::Kind::Format: {
			auto literal = to<Literal>(m_arguments[0].get());
			std::string formatStr = literal->value();
			size_t pos = 0;
			std::vector<std::pair<std::string, std::string> > substrings;
			while (true) {
				pos = formatStr.find('{', pos);
				size_t close_pos = formatStr.find('}', pos);
				if (pos == string::npos || close_pos == string::npos)
					break;
				if ((formatStr[pos + 1] != ':') && (close_pos != pos + 1)) {
					pos++;
					continue;
				}

				std::string format = formatStr.substr(pos + 1, close_pos - pos - 1);
				if (format[0] == ':') format.erase(0, 1);
				substrings.emplace_back(formatStr.substr(0, pos), format);
				formatStr = formatStr.substr(close_pos + 1);
				pos = 0;
			}
			// create new vector(TvmBuilder)
			m_pusher.push(+1, "NIL");
			// create new builder to store data in it
			m_pusher.push(+1, "NEWC");

			auto pushConstStr = [&](const string& constStr) {
                if (!constStr.empty()) {
                    size_t maxSlice = TvmConst::CellBitLength / 8;
                    for(size_t i = 0; i  < constStr.length(); i += maxSlice) {
                        m_pusher.pushString(constStr.substr(i, min(maxSlice, constStr.length() - i)), true);
                        // stack: vector(TvmBuilder) builder Slice
                        m_pusher.pushMacroCallInCallRef(-1, "storeStringInBuilders_macro");
                    }
                    // stack: vector(TvmBuilder) builder
                }
			};
			for (size_t it = 0; it < substrings.size(); it++) {
				// stack: vector(TvmBuilder) builder
				pushConstStr(substrings[it].first);

				Type::Category cat = m_arguments[it + 1]->annotation().type->category();
				Type const *argType = m_arguments[it + 1]->annotation().type;
				if (cat == Type::Category::Integer || cat == Type::Category::RationalNumber) {
					// stack: vector(TvmBuilder) builder
					std::string format = substrings[it].second;
					bool leadingZeroes = !format.empty() && (format[0] == '0');
					bool isHex = !format.empty() && (format.back() == 'x' || format.back() == 'X');
					bool isLower = isHex && (format.back() == 'x');
					bool isTon = !format.empty() && format.back() == 't';
					if (!isTon) {
						while (!format.empty() && (format.back() < '0' || format.back() > '9')) {
							format.erase(format.size() - 1, 1);
						}
						int width = 0;
						if (format.length() > 0)
							width = std::stoi(format);
						if (width < 0)
							solUnimplemented("Width should be a positive integer.");
						auto mt = m_arguments[it + 1]->annotation().type->mobileType();
						auto isInt = dynamic_cast<IntegerType const *>(mt);
						acceptExpr(m_arguments[it + 1].get());
						if (isInt->isSigned())
							m_pusher.push(0, "ABS");
						m_pusher.pushInt(width);
						m_pusher.push(+1, leadingZeroes ? "TRUE" : "FALSE");
						if (isHex) {
							if (isLower)
								m_pusher.push(+1, "TRUE");
							else
								m_pusher.push(+1, "FALSE");
						}
						if (isInt->isSigned()) {
							acceptExpr(m_arguments[it + 1].get());
							m_pusher.push(0, "ISNEG");
						} else {
							m_pusher.push(+1, "FALSE");
						}
						// stack: vector(TvmBuilder) builder abs(number) width leadingZeroes addMinus
						if (isHex) {
							m_pusher.pushMacroCallInCallRef(-5, "convertIntToHexStr_macro");
						} else {
							m_pusher.pushMacroCallInCallRef(-4, "convertIntToDecStr_macro");
						}
						// stack: vector(TvmBuilder) builder
					} else {
						acceptExpr(m_arguments[it + 1].get());
						m_pusher.pushInt(9);
						m_pusher.pushMacroCallInCallRef(-2, "convertFixedPointToString_macro");
					}
				} else if (cat == Type::Category::Address) {
					// stack: vector(TvmBuilder) builder
					acceptExpr(m_arguments[it + 1].get());
					// stack: vector(TvmBuilder) builder address
					m_pusher.pushMacroCallInCallRef(-1, "convertAddressToHexString_macro");
					// stack: vector(TvmBuilder) builder
				} else if (isStringOrStringLiteralOrBytes(argType)) {
					// stack: vector(TvmBuilder) builder
					acceptExpr(m_arguments[it + 1].get());
					// stack: vector(TvmBuilder) builder string(cell)
					m_pusher.push(0, "CTOS");
					// stack: vector(TvmBuilder) builder string(slice)
					m_pusher.pushMacroCallInCallRef(-1, "storeStringInBuilders_macro");
					// stack: vector(TvmBuilder) builder
				} else if (cat == Type::Category::FixedPoint) {
					int power = to<FixedPointType>(argType)->fractionalDigits();
					acceptExpr(m_arguments[it + 1].get());
					m_pusher.pushInt(power);
					m_pusher.pushMacroCallInCallRef(-2, "convertFixedPointToString_macro");
				} else {
					cast_error(*m_arguments[it + 1].get(), "Unsupported argument type");
				}
			}
            pushConstStr(formatStr);

			m_pusher.pushMacroCallInCallRef(-1, "assembleList_macro");
			return true;
		}
		case FunctionType::Kind::Stoi: {
			pushArgAndConvert(0);
			m_pusher.pushMacroCallInCallRef(+1, "__stoi_macro");
			return true;
		}
		default:
			break;
	}
	return false;
}

bool FunctionCallCompiler::checkLocalFunctionOrLibCallOrFuncVarCall() {
	auto expr = &m_functionCall.expression();
	if (auto identifier = to<Identifier>(expr); identifier && checkLocalFunctionOrLibCall(identifier)) {
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

		pushArgs();

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

	pushArgs(true);
	const TypePointer type = newExpr->typeName().annotation().type;

	std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
	// unit with getIndex()
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
		stateInitExprs[StateInitMembers::Data] = generateDataSection(
			pushKey, hasVars,
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
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				convertArray(constructor->parameters()),
				ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(constructor, ReasonOfOutboundMessage::RemoteCallInternal),
				{},
				builderSize,
				true
			);
		else
			ChainDataEncoder{&m_pusher}.createDefaultConstructorMsgBodyAndAppendToBuilder(builderSize);
	};

	std::function<void()> pushSendrawmsgFlag;
	auto flagIt = find_if(optionNames.begin(), optionNames.end(),  [](auto el) { return *el == "flag"; });
	if (flagIt != optionNames.end()) {
		Expression const *sendrawmsgFlag = functionOptions->options()[flagIt - optionNames.begin()].get();
		pushSendrawmsgFlag = [&]() { acceptExpr(sendrawmsgFlag); };
	}

	deployNewContract(pushWid, pushValue, pushBounce, pushCurrency, pushBody, pushSendrawmsgFlag, m_arguments.size());
	// stack: destAddress
	return true;
}

// TODO pushBounce and another params can be const
void FunctionCallCompiler::deployNewContract(
	const std::function<void()>& pushWid,
	const std::function<void()>& pushValue,
	const std::function<void()>& pushBounce,
	const std::function<void()>& pushCurrency,
	const std::function<void(int builderSize)>& appendBody,
	const std::function<void()>& pushSendrawmsgFlag,
	const int argQty
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

	// stack: arg[n-1], ..., arg[1], arg[0], stateInit destAddress
	m_pusher.blockSwap(argQty + 1, 1);
	// stack:  destAddress, arg[n-1], ..., arg[1], arg[0], stateInit
	int destAddressStack = m_pusher.getStack().size() - 1 - argQty;

	exprs[TvmConst::int_msg_info::tons] = [&](){
		pushValue();
	};
	exprs[TvmConst::int_msg_info::dest] = [&](){
		int stackIndex = m_pusher.getStack().size() - destAddressStack;
		m_pusher.pushS(stackIndex);
	};


	std::function<void()> appendStateInit = [&]() {
		m_pusher.stones(1);
		m_pusher.push(-1, "STREF");
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
	pushArgAndConvert(0); // dict size
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


void FunctionCallCompiler::pushArgs(bool reversed) {
	auto func = [&](const ASTPointer<const Expression> &e, int i) {
		acceptExpr(e.get());
		Type const* targetType{};
		if (m_funcType->parameterTypes().empty()) {
			targetType = m_functionCall.annotation().arguments->targetTypes.at(i);
		} else {
			targetType = m_funcType->parameterTypes().at(i);
		}
		m_pusher.hardConvert(targetType, e->annotation().type);
	};

	if (reversed) {
		int i = m_funcType->parameterTypes().size() - 1;
		for (const auto &e : m_arguments | boost::adaptors::reversed) {
			func(e, i);
			--i;
		}
	} else {
		int i = 0;
		for (const ASTPointer<const Expression> &e : m_arguments) {
			func(e, i);
			++i;
		}
	}
}

void FunctionCallCompiler::pushArgAndConvert(int index, const std::string& name) {
	const ASTPointer<Expression const> &arg = m_arguments.at(index);
	acceptExpr(arg.get());

	Type const* targetType{};
	if (!name.empty()) {
		bool find = false;
		const vector<string>& names = m_funcType->parameterNames();
		for (int i = 0; i < static_cast<int>(names.size()); ++i) {
			if (names.at(i) == name) {
				targetType = m_funcType->parameterTypes().at(i);
				find = true;
				break;
			}
		}
		solAssert(find, "");
	} else {
		targetType = m_functionCall.annotation().arguments->targetTypes.at(index);
	}

	m_pusher.hardConvert(targetType, arg->annotation().type);
}

void FunctionCallCompiler::pushExprAndConvert(const Expression *expr, Type const* targetType) {
	acceptExpr(expr);
	m_pusher.hardConvert(targetType, expr->annotation().type);
}

void FunctionCallCompiler::acceptExpr(const Expression *expr) {
	m_exprCompiler->compileNewExpr(expr);
}

void FunctionCallCompiler::compileLog()
{
	auto logstr = m_arguments[0].get();
	auto literal = to<Literal>(logstr);
	if (literal && literal->value().size() < 16) {
		std::string hexStr = stringToBytes(literal->value());
		m_pusher.push(0, "PRINTSTR x" + hexStr);
	} else {
		pushArgs();
		m_pusher.pushLog();
	}
}
