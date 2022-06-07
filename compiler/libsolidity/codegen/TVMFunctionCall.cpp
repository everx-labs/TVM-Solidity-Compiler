/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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

#include <boost/algorithm/string.hpp>

#include <liblangutil/SourceReferenceExtractor.h>
#include <libsolidity/ast/TypeProvider.h>

#include "DictOperations.hpp"
#include "TVMABI.hpp"
#include "TVMConstants.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMFunctionCall.hpp"
#include "TVMStructCompiler.hpp"

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace std;

FunctionCallCompiler::FunctionCallCompiler(
	StackPusher &m_pusher,
	FunctionCall const& _functionCall,
	bool isCurrentResultNeeded
) :
	m_pusher{m_pusher},
	m_exprCompiler{m_pusher},
	m_functionCall{_functionCall},
	m_arguments{_functionCall.arguments()},
	m_funcType{to<FunctionType>(m_functionCall.expression().annotation().type)},
	m_retType{m_functionCall.annotation().type},
	m_isCurrentResultNeeded{isCurrentResultNeeded},
	m_names{m_functionCall.names()}
{
}

void FunctionCallCompiler::structConstructorCall() {
	auto const& type = dynamic_cast<TypeType const&>(*m_functionCall.expression().annotation().type);
	auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
	auto pushParam = [&](int index, Type const* targetType) {
		pushExprAndConvert(m_arguments.at(index).get(), targetType);
	};
	StructCompiler structCompiler{&m_pusher, &structType};
	structCompiler.structConstructor(m_names, pushParam);
}

void FunctionCallCompiler::compile() {
	auto ma = to<MemberAccess>(&m_functionCall.expression());
	auto reportError = [&](){
		cast_error(m_functionCall, "Unsupported function call");
	};

	if (checkRemoteMethodCall(m_functionCall) ||
		(ma != nullptr && libraryCall(*ma)) ||
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
				checkForTvmVectorMethods(*ma, category) ||
				checkForOptionalMethods(*ma))
			{
				// nothing
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
				if (m_funcType->kind() == FunctionType::Kind::TVMBuildIntMsg) {
					tvmBuildIntMsg();
				} else if (m_funcType->kind() == FunctionType::Kind::TVMBuildDataInit) {
					tvmBuildDataInit();
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
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "gosh") {
				goshFunction();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "msg") {
				msgFunction(*ma);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "abi") {
				abiFunction();
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

void FunctionCallCompiler::arrayPush(StackPusher& pusher, Type const* arrayBaseType, DataType dataType) {
	// arr value
	pusher.exchange(1); // value' arr
	pusher.push(-1 + 2, "UNTUPLE 2");  // value' size dict
	pusher.pushS(1); // value' size dict size
	pusher.push(0, "INC"); // value' size dict newSize
	pusher.blockSwap(3, 1); // newSize value' size dict
	pusher.setDict(getArrayKeyType(), *arrayBaseType, dataType); // newSize dict'
	pusher.push(-2 + 1, "TUPLE 2");  // arr
}

bool FunctionCallCompiler::checkForMappingOrCurrenciesMethods() {
	auto expr = &m_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	if (ma == nullptr || (!to<MappingType>(ma->expression().annotation().type) && !to<ExtraCurrencyCollectionType>(ma->expression().annotation().type)))
		return false;

	const ASTString &memberName = ma->memberName();
	if (isIn(memberName, "delMin", "delMax")) {
		mappingDelMinOrMax(memberName == std::string{"delMin"});
	} else  if (isIn(memberName, "at", "fetch", "exists", "replace", "add", "getSet", "getAdd", "getReplace")) {
		mappingGetSet();
	} else if (isIn(memberName, "min", "max")) {
		mappingMinMaxMethod(memberName == std::string{"min"});
	} else if (isIn(memberName, "next", "prev", "nextOrEq", "prevOrEq")) {
		mappingPrevNextMethods();
	} else if (isIn(memberName, "keys", "values")) {
		mappingKeysOrValues(memberName == "keys");
	} else if (memberName == "empty") {
		mappingEmpty();
	} else {
		solUnimplemented("Unsupported mapping method");
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
		const int stackSize = m_pusher.stackSize();
		auto ma = to<MemberAccess>(&m_functionCall.expression());
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&ma->expression(), true, true); // lValue... map
		pushArgAndConvert(1); // lValue... map value
		const DataType& dataType = m_pusher.prepareValueForDictOperations(keyType, valueType); // lValue... map value'
		pushArgAndConvert(0); // mapLValue... map value key
		m_pusher.prepareKeyForDictOperations(keyType, false);
		m_pusher.rot(); // mapLValue... value key map

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
		const int cntOfValuesOnStack = m_pusher.stackSize() - stackSize;  // mapLValue... map optValue
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optValue mapLValue... map
		m_exprCompiler.collectLValue(lValueInfo, true, false); // optValue
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
	m_pusher.pushInt(dictKeyLength(keyType)); // index' dict nbits

	DictPrevNext compiler{m_pusher, *keyType, *valueType, memberAccess->memberName()};
	compiler.prevNext();
}

void FunctionCallCompiler::mappingKeysOrValues(bool areKeys) {
	m_pusher.pushEmptyArray();

	auto ma = to<MemberAccess>(&m_functionCall.expression());
	acceptExpr(&ma->expression());
	// array map
	m_pusher.pushS(0);
	// array map map
	Type const* mapKeyType{};
	Type const* mapValueType{};
	std::tie(mapKeyType, mapValueType) = realDictKeyValue(ma->expression().annotation().type);
	DictMinMax compiler{m_pusher, *mapKeyType, *mapValueType, true};
	compiler.minOrMax();
	// array map minPair

	m_pusher.startContinuation();
	// array map curPair
	m_pusher.pushS(0);
	m_pusher << "ISNULL";
	m_pusher << "NOT";
	m_pusher.push(-1, "");
	m_pusher.endContinuation();

	m_pusher.startContinuation();

	// Adding value
	// array map curPair
	m_pusher.pushS(2);
	// array map curPair array
	m_pusher.pushS(1);
	// array map curPair array curPair
	m_pusher.indexNoexcep(areKeys ? 0 : 1);
	// array map curPair array key/value
	IntegerType const& arrayKeyType = getArrayKeyType();
	Type const* arrayValueType = areKeys ? mapKeyType : mapValueType;
	DataType dataType = m_pusher.prepareValueForDictOperations(&arrayKeyType, arrayValueType);
	// array map curPair array key'/value'
	arrayPush(m_pusher, arrayValueType, dataType);
	// array map curPair array'
	m_pusher.popS(3);
	// array' map curPair

	// Updating nextPair
	// array' map curPair
	m_pusher.indexNoexcep(0);
	// array' map curKey
	m_pusher.pushS(1);
	// array' map curKey map
	m_pusher.pushInt(dictKeyLength(mapKeyType));
	// array' map curKey map nbits
	DictPrevNext dictPrevNext{m_pusher, *mapKeyType, *mapValueType, "next"};
	dictPrevNext.prevNext();
	// TODO don't parse value for keys
	// array' map nextPair
	m_pusher.endContinuation();

	m_pusher._while(false);

	// keys map minValue
	m_pusher.drop(2);
	// keys
}

void FunctionCallCompiler::mappingEmpty() {
	auto expr = &m_functionCall.expression();
	auto ma = to<MemberAccess>(expr);
	acceptExpr(&ma->expression());
	m_pusher.push(0, "DICTEMPTY");
}

void FunctionCallCompiler::superFunctionCall(MemberAccess const &_node) {
	pushArgs();
	string fname = _node.memberName();
	auto super = getSuperContract(m_pusher.ctx().getCurrentFunction()->annotation().contract,
								  m_pusher.ctx().getContract(), fname);
	solAssert(super, "");
	if (getFunction(super, fname)) {
		auto functionName = super->name() + "_" + fname;
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
		const auto& num = ExprUtils::constValue(*m_arguments.at(0));
		const auto& len = ExprUtils::constValue(*m_arguments.at(1));
		if (num.has_value() && len.has_value()) {
			std::string addr = "01";
			addr += StrUtils::toBitString(u256(len.value()), 9);
			addr += StrUtils::toBitString(u256(num.value()), int(len.value()));
			m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(addr));
		} else {
			pushArgs();
			m_pusher.pushS(0); // numb cntBit cntBit
			m_pusher.push(+1, "NEWC"); // numb cntBit cntBit builder
			m_pusher.push(-1 + 1, "STSLICECONST x6_");
			m_pusher.push(-1, "STU 9"); // numb cntBit builder''
			m_pusher.exchange(1); // numb builder'' cntBit
			m_pusher.push(-3 + 1, "STUX"); // builder'''
			m_pusher.push(0, "ENDC");
			m_pusher.push(0, "CTOS"); // extAddress
		}
	} else if (_node.memberName() == "makeAddrNone") {
		m_pusher.pushSlice("x2_");
	} else if (_node.memberName() == "makeAddrStd") {
		const auto& wid = ExprUtils::constValue(*m_arguments.at(0));
		const auto& val = ExprUtils::constValue(*m_arguments.at(1));
		if (wid.has_value() && val.has_value()) {
			std::string addr = "100";
			addr += StrUtils::toBitString(u256(wid.value()), 8);
			addr += StrUtils::toBitString(u256(val.value()), 256);
			m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(addr));
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
				m_pusher.pushCallOrCallRef(TVMCompilerContext::getLibFunctionName(libFunction, false), funType);
			} else {
				// using MathLib for uint;
				// a.add(b);
				const int stakeSize0 = m_pusher.stackSize();
				const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&ma.expression(), true, true);
				const int stakeSize1 = m_pusher.stackSize();
				const int lValueQty = stakeSize1 - stakeSize0;

				pushArgs();
				m_pusher.pushCallOrCallRef(
					TVMCompilerContext::getLibFunctionName(libFunction, true),
					funType,
					std::make_pair(argQty + 1, retQty + 1)
				);

				m_pusher.blockSwap(lValueQty, retQty);

				m_exprCompiler.collectLValue(lValueInfo, true, false);
			}
			return true;
		}
	}
	return false;
}

std::function<void()> FunctionCallCompiler::generateDataSection(
	const std::function<void()>& pushKey,
	Expression const* vars,
	ContractType const* ct
) {
	return [pushKey, this, vars, ct]() {
		// creat dict with variable values
		m_pusher.push(+1, "NEWDICT");
		// stake: builder dict

		IntegerType keyType = getKeyTypeOfC4();
		TypePointer valueType = TypeProvider::uint256();

		pushKey();
		const DataType& dataType = m_pusher.prepareValueForDictOperations(&keyType, valueType);
		m_pusher.pushInt(0); // index of pubkey
		// stack: dict value key
		m_pusher.rot();
		// stack: value key dict
		m_pusher.setDict(getKeyTypeOfC4(), *valueType, dataType);
		// stack: dict'
		if (vars) {
			std::vector<PragmaDirective const *> _pragmaDirectives;
			PragmaDirectiveHelper pragmaHelper{_pragmaDirectives};
			TVMCompilerContext cc{&ct->contractDefinition(), pragmaHelper};
			std::vector<std::pair<VariableDeclaration const*, int>> staticVars = cc.getStaticVariables();
			auto getDeclAndIndex = [&](const std::string& name) {
				auto pos = find_if(staticVars.begin(), staticVars.end(), [&](auto v) { return v.first->name() == name; });
				solAssert(pos != staticVars.end(), "");
				return *pos;
			};
			auto initVars = to<InitializerList>(vars);
			for (size_t i = 0; i < initVars->names().size(); ++i) {
				const ASTPointer<ASTString> &name = initVars->names().at(i);
				const auto &[varDecl, varIndex] = getDeclAndIndex(*name);
				valueType = varDecl->type();
				pushExprAndConvert(initVars->options().at(i).get(), valueType); // stack: dict value
				const DataType& dataType2 = m_pusher.prepareValueForDictOperations(&keyType, valueType);
				m_pusher.pushInt(varIndex);
				// stack: dict value key
				m_pusher.rot();
				// stack: value key dict
				m_pusher.setDict(getKeyTypeOfC4(), *varDecl->type(), dataType2);
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

		// Search for bounce option
		if (_functionCall.isExtMsg()) {
			checkExtMsgSend();
			return true;
		}

		// parse options they are stored in two vectors: names and options
		for (const auto &option: functionOptions->names())
			if (!isIn(*option, "flag", "value", "currencies", "bounce", "callback"))
				cast_error(_functionCall, "Unsupported function call option: " + *option);


		// Search for bounce option
		if (Expression const* bounce = findOption("bounce")) {
			exprs[TvmConst::int_msg_info::bounce] = bounce;
		} else {
			constParams[TvmConst::int_msg_info::bounce] = "1";
		}

		// Search for currencies option
		if (Expression const* currencies = findOption("currencies")) {
			exprs[TvmConst::int_msg_info::currency] = currencies;
		} else {
			constParams[TvmConst::int_msg_info::currency] = "0";
		}

		// Search for value (ton) option
		if (Expression const* valueExpr = findOption("value")) {
			const auto& value = ExprUtils::constValue(*valueExpr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(u256(value.value()));
			} else {
				exprs[TvmConst::int_msg_info::tons] = valueExpr;
			}
		} else {
			constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		}

		// remote_addr
		exprs[TvmConst::int_msg_info::dest] = &memberAccess->expression();

		// function definition
		functionDefinition = getRemoteFunctionDefinition(memberAccess);


		if (Expression const* callback = findOption("callback")) {
			CallableDeclaration const* remoteFunction = getFunctionDeclarationOrConstructor(callback);
			callbackFunctionId = ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(
					remoteFunction,
					ReasonOfOutboundMessage::RemoteCallInternal
			);
		}

		// Search for sendRawMsg flag option
		if (Expression const* flag = findOption("flag")) {
			pushSendrawmsgFlag = [flag, this]() {
				acceptExpr(flag);
			};
		}
	} else {
		constParams[TvmConst::int_msg_info::tons] = getDefaultMsgValue();
		constParams[TvmConst::int_msg_info::bounce] = "1";

		Expression const *currentExpression = &_functionCall.expression();

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

	if (_functionCall.isAwait()) {
		const std::string name = "_await_" + functionDefinition->annotation().contract->name() + "_" + functionDefinition->name();
		std::vector<VariableDeclaration const*> ret;
		callbackFunctionId = ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(
			name,
			getTypesFromVarDecls(functionDefinition->returnParameters()),
			&ret,
			ReasonOfOutboundMessage::RemoteCallInternal,
			{},
			false // Function isn't responsible. It's answer of responsible function
		);
		m_pusher.pushInt(callbackFunctionId.value());
		m_pusher.setGlob(TvmConst::C7::AwaitAnswerId);
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

	if (m_isCurrentResultNeeded && !_functionCall.isAwait())
		cast_error(_functionCall, "Calls to remote contract do not return result.");

	m_pusher.sendIntMsg(exprs, constParams, appendBody, pushSendrawmsgFlag, _functionCall.isAwait(), m_arguments.size(),
						nullptr);

	if (_functionCall.isAwait()) {
		// stack: remote addr
		m_pusher.startOpaque();
		m_pusher.startContinuation();
		m_pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4_for_await");
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"DEPTH",
			"ADDCONST -5; sys vars",
			"PUSHINT 5",
			"BLKSWX",
			"SETGLOB " + toString(TvmConst::C7::AwaitAnswerId),
			"SETGLOB " + toString(TvmConst::C7::SenderAddress),
			"SETGLOB " + toString(TvmConst::C7::MsgPubkey),
			"DEPTH",
			"ADDCONST -4",
			"PICK",
			"LDU 32",
			"SWAP",
		}, 0, 0, false));
		m_pusher.getGlob(TvmConst::C7::AwaitAnswerId);
		m_pusher.push(-2 + 1, "EQUAL");
		m_pusher._throw("THROWIFNOT " + to_string(TvmConst::RuntimeException::WrongAwaitFuncId));
		m_pusher.push(+2, ""); // fix stack
		// decode returned vars
		ChainDataDecoder decoder{&m_pusher};
		vector<Type const*> types = getParams(functionDefinition->returnParameters()).first;
		decoder.decodePublicFunctionParameters(types, false, true);
		m_pusher.push(-1, ""); // fix stack
		m_pusher.pushRefContAndCallX(0, 0, false);
		m_pusher.endOpaque(1, types.size()); // take one parameter - it's dest address
	}
	return true;
}

void FunctionCallCompiler::checkExtMsgSend() {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());

	bool addSignature = false;
	Expression const* sign = findOption("sign");
	if (sign != nullptr) {
		addSignature = ExprUtils::constBool(*sign).value();
	}

	Expression const* pubkey = findOption("pubkey");
	Expression const* expire = findOption("expire");
	Expression const* time = findOption("time");
	Expression const* callbackid = findOption("callbackId");
	Expression const* onerrorid = findOption("onErrorId");
	Expression const* stateInit = findOption("stateInit");
	Expression const* signBoxHandle = findOption("signBoxHandle");

	auto memberAccess = to<MemberAccess>(&functionOptions->expression());
	FunctionDefinition const* functionDefinition = getRemoteFunctionDefinition(memberAccess);

	Expression const* destination = &memberAccess->expression();

	generateExtInboundMsg(addSignature, destination, pubkey, expire, time, callbackid,
						  onerrorid, stateInit, signBoxHandle, functionDefinition, m_arguments);
	m_pusher.pushInt(TvmConst::SENDRAWMSG::DefaultFlag);
	m_pusher.sendrawmsg();
}

std::string FunctionCallCompiler::getDefaultMsgValue() {
	const std::optional<std::vector<ASTPointer<Expression>>> expr = m_pusher.ctx().pragmaHelper().hasMsgValue();
	if (!expr) {
		return StrUtils::tonsToBinaryString(u256{TvmConst::Message::DefaultMsgValue});
	}
	const auto& val = ExprUtils::constValue(*expr.value().at(0).get());
	if (!val.has_value()) {
		cast_error(*expr.value().at(0).get(), "Default value should be compile time expression of number type");
	}
	return StrUtils::tonsToBinaryString(val.value());
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
	return f;
}

void FunctionCallCompiler::acceptExprOrPushFunctionId(Expression const* onerrorid) {
	if (CallableDeclaration const* cd = getFunctionDeclarationOrConstructor(onerrorid, true)) {
		auto fd = to<FunctionDefinition>(cd);
		int funId = ChainDataEncoder{&m_pusher}.
					calculateFunctionIDWithReason(fd, ReasonOfOutboundMessage::RemoteCallInternal);
		m_pusher.pushInt(funId);
		return ;
	}

	auto t = to<IntegerType>(onerrorid->annotation().type->mobileType());
	solAssert(t && !t->isSigned() && t->numBits() <= 32, "");

	acceptExpr(onerrorid);
}

void FunctionCallCompiler::generateExtInboundMsg(
	bool addSignature,
	const Expression *destination,
	const Expression *pubkey,
	const Expression *expire,
	const Expression *time,
	const Expression *callbackid,
	const Expression *onerrorid,
	const Expression *stateInit,
	const Expression *signBoxHandle,
	const CallableDeclaration *functionDefinition,
	const ast_vec<Expression const>& arguments
) {
	const int stackSize = m_pusher.stackSize();

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
			acceptExpr(arg.get());
		}

		m_pusher.push(+1, "NEWC");
		builderSize += 1 + 512;
		if (addSignature) {
			m_pusher.stones(1);
			m_pusher.stzeroes(512);	// Signature
		} else {
			m_pusher.stzeroes(1);	// Signature
		}

		// store header
		// [optional]pubkey: 1 + [256] bits
		// time: 64 bits
		// expire: 32 bits


		if (pubkey != nullptr) {
			// pubkey is set
			builderSize += 1 + 256;
			// pubkey is optional, check whether it presents
			acceptExpr(pubkey);

			if (addSignature) {
				m_pusher.pushS(0);
				m_pusher.startOpaque();
				m_pusher.push(0, "ISNULL");
				m_pusher.push(-1, "");

				m_pusher.startContinuation();
				m_pusher.drop(1);
				m_pusher.stzeroes(1);
				m_pusher.endContinuation();

				m_pusher.startContinuation();
				m_pusher.exchange(1);
				m_pusher.stones(1);
				m_pusher.push(+1, ""); // fix stack
				m_pusher.push(-1, "STU 256");
				m_pusher.endContinuation();

				m_pusher.ifElse();
				m_pusher.endOpaque(3, 1);
			} else {
				m_pusher.push(-1 + 1, "ISNULL");
				m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::MsgWithKeyButNoSign));
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
		DecodePositionAbiV2 position{builderSize, 0, types};
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

		ChainDataEncoder{&m_pusher}.createMsgBody(params, functionId, callbackFunctionId, position);

		m_pusher.push(-1, "STBREFR");
	};

	// store dest address
	acceptExpr(destination);


	// generate payload to store it as a src address with addr_extern type
	if (signBoxHandle != nullptr)
		acceptExpr(signBoxHandle);
	m_pusher << "PUSHINT " + toString(TvmConst::Message::MajorAbiVersion);
	acceptExprOrPushFunctionId(onerrorid);
	acceptExprOrPushFunctionId(callbackid);
	m_pusher.push(+1, "NEWC");
	// stack: [signBoxHandle] abiVer onerrorid callbackid builder
	m_pusher.push(0, "STSLICECONST x6_"); // header 01
	if (signBoxHandle == nullptr)
		m_pusher.push(0, "STSLICECONST x264_"); // const length 76
	else {
		m_pusher.pushS(4);
		m_pusher.startOpaque();
		m_pusher.push(0, "ISNULL");
		m_pusher.startContinuation();
		m_pusher.push(0, "STSLICECONST x264_"); // const length 76
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.push(0, "STSLICECONST x364_"); // const length 108
		m_pusher.endContinuation();
		m_pusher.ifElse();
		m_pusher.push(-1, "");
		m_pusher.endOpaque(2, 1);
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
		m_pusher.startOpaque();
		m_pusher.push(0, "ISNULL");
		m_pusher.startContinuation();
		m_pusher.dropUnder(1, 1);
		m_pusher.stzeroes(1);
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.stones(1);
		m_pusher.push(+1, ""); // fix stack
		m_pusher.push(-1, "STU 32");
		m_pusher.endContinuation();
		m_pusher.ifElse();
		m_pusher.push(-1, ""); // fix stack
		m_pusher.endOpaque(3, 1);
	}


	std::function<void()> appendStateInit = nullptr;
	if (stateInit != nullptr)
		appendStateInit = [&]() {
			m_pusher.stones(1);
			acceptExpr(stateInit);
			m_pusher.pushS(0);
			checkStateInit();
			m_pusher.push(-1, "STREFR");
		};

	m_pusher.prepareMsg({TvmConst::ext_msg_info::src, TvmConst::ext_msg_info::dest}, {}, appendBody, appendStateInit, StackPusher::MsgType::ExternalIn);

	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
}

void FunctionCallCompiler::tvmBuildIntMsg() {
	const int stackSize = m_pusher.stackSize();

	int destArg = -1;
	int valueArg = -1;
	int currenciesArg = -1;
	int bounceArg = -1;
	int callArg = -1;
	int stateInit = -1;
	for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
		switch (str2int(m_names[arg]->c_str())) {
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
			case str2int("stateInit"):
				stateInit = arg;
				break;
			default:
				solUnimplemented("");
		}
	}


	auto callList = to<CallList>(m_arguments[callArg].get());
	const std::vector<ASTPointer<Expression const>> args = callList->arguments();
	auto functionDefinition = to<FunctionDefinition>(getFunctionDeclarationOrConstructor(callList->function()));
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

	for (const auto& [argIndex, name, id] : std::vector<std::tuple<int, std::string, int>>{
		{currenciesArg, "currencies", TvmConst::int_msg_info::currency},
		{valueArg, "value", TvmConst::int_msg_info::tons},
		{destArg, "dest", TvmConst::int_msg_info::dest},
		{bounceArg, "bounce", TvmConst::int_msg_info::bounce}
	}) {
		if (argIndex != - 1) {
			std::optional<bigint> value = ExprUtils::constValue(*m_arguments.at(argIndex));
			std::optional<bool> flag = ExprUtils::constBool(*m_arguments.at(argIndex));
			if (value) {
				constParams[id] = StrUtils::tonsToBinaryString(*value);
			} else if (flag) {
				constParams[id] = StrUtils::boolToBinaryString(*flag);
			} else {
				pushArgAndConvert(argIndex, name);
				isParamOnStack.insert(id);
			}
		}
	}

	std::function<void()> appendStateInit;
	if (stateInit != -1) {
		appendStateInit = [&](){
			// Either StateInit ^StateInit
			m_pusher.push(-1 + 1, "STONE"); // ^StateInit
			pushArgAndConvert(stateInit, "stateInit");
			m_pusher.pushS(0);
			checkStateInit();
			m_pusher.push(-2 + 1, "STREFR");
		};
	}

	m_pusher.prepareMsg(
		isParamOnStack,
		constParams,
		appendBody,
		appendStateInit,
		StackPusher::MsgType::Internal
	);

	solAssert(m_pusher.stackSize() == stackSize + 1, "");
}

void FunctionCallCompiler::tvmBuildDataInit() {
	const int stackSize = m_pusher.stackSize();
	int keyArg = -1;
	int varArg = -1;
	int contrArg = -1;
	if (m_names.empty()) {
		if (!m_arguments.empty()) {
			keyArg = 0;
		}
	} else {
		for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
			switch (str2int(m_names[arg]->c_str())) {
				case str2int("varInit"):
					varArg = arg;
					break;
				case str2int("pubkey"):
					keyArg = arg;
					break;
				case str2int("contr"):
					contrArg = arg;
					break;
				default:
					solUnimplemented("");
			}
		}
	}
	auto pushKey = [this, keyArg]() {
		if (keyArg == -1) {
			m_pusher.pushInt(0);
		} else {
			pushArgAndConvert(keyArg, "pubkey");
		}
	};
	ContractType const* ct{};
	if (varArg != -1) {
		Type const* type = m_arguments[contrArg]->annotation().type;
		auto tt = dynamic_cast<const TypeType*>(type);
		type = tt->actualType();
		ct = to<ContractType>(type);
	}
	generateDataSection(
		pushKey,
		varArg != -1 ? m_arguments[varArg].get() : nullptr,
		ct
	)();

	solAssert(m_pusher.stackSize() == stackSize + 1, "");
}

void FunctionCallCompiler::tvmBuildMsgMethod() {
	int destArg = -1;
	int callArg = -1;
	int timeArg = -1;
	int expireArg = -1;
	int pubkeyArg = -1;
	int signArg = -1;
	int callbackArg = -1;
	int onerrorArg = -1;
	int stateArg = -1;
	int signHandlerArg = -1;
	if (!m_names.empty()) {
		for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
			switch (str2int(m_names[arg]->c_str())) {
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
	auto functionDefinition = getFunctionDeclarationOrConstructor(funcCall->function());
	bool addSignature = false;
	if (signArg != -1) {
		const std::optional<bool> value = ExprUtils::constBool(*m_arguments[signArg]);
		if (value.has_value()) {
			addSignature = *value;
		}
	}

	generateExtInboundMsg(addSignature, m_arguments[destArg].get(),
										  (pubkeyArg != -1) ? m_arguments[pubkeyArg].get() : nullptr,
										  (expireArg != -1) ? m_arguments[expireArg].get() : nullptr,
										  (timeArg != -1) ? m_arguments[timeArg].get() : nullptr,
										  m_arguments[callbackArg].get(),
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
		bool hasVars{};
		std::map<StateInitMembers, std::function<void()>> exprs;
		if (m_names.empty()) {
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
				switch (str2int(m_names[arg]->c_str())) {
					case str2int("code"):
						codeArg = arg;
						exprs[StateInitMembers::Code] = [this, codeArg, name = *m_names.at(arg)](){
							pushArgAndConvert(codeArg, name);
						};
						break;
					case str2int("data"):
						dataArg = arg;
						exprs[StateInitMembers::Data] = [this, dataArg, name = *m_names.at(arg)](){
							pushArgAndConvert(dataArg, name);
						};
						dataIsSet = true;
						break;
					case str2int("splitDepth"):
						depthArg = arg;
						exprs[StateInitMembers::SplitDepth] = [this, depthArg, name = *m_names.at(arg)](){
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
				ContractType const* ct{};
				if (hasVars) {
					Type const* type{};
					type = m_arguments[contrArg]->annotation().type;
					auto tt = dynamic_cast<const TypeType *>(type);
					type = tt->actualType();
					ct = to<ContractType>(type);
				}
				exprs[StateInitMembers::Data] = generateDataSection(pushKey,
																	hasVars ? m_arguments[varArg].get() : nullptr,
																	ct);
			}
		}

		buildStateInit(exprs);
		return true;
	}

	if (_node.memberName() == "insertPubkey") {
		pushArgs();
		m_pusher.pushMacroCallInCallRef(2, 1, "insert_pubkey_macro");
		return true;
	}

	if (_node.memberName() == "stateInitHash") {
		pushArgs();
		m_pusher.pushMacroCallInCallRef(4, 1, "stateInitHash_macro");
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
		cellBitRefQty(false);
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
		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true,
																   _node.expression().annotation().isLValue);
		TypePointers targetTypes;
		if (auto const* targetTupleType = dynamic_cast<TupleType const*>(m_retType))
			targetTypes = targetTupleType->components();
		else
			targetTypes = TypePointers{m_retType};

		ChainDataDecoder decode{&m_pusher};
		DecodePositionFromOneSlice pos;
		decode.decodeParameters(targetTypes, pos, false);

		m_exprCompiler.collectLValue(lValueInfo, true, false);
		solAssert(stackSize + static_cast<int>(targetTypes.size()) == m_pusher.stackSize(), "");
	} else if (_node.memberName() == "decodeQ") {

		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true,
																  _node.expression().annotation().isLValue);
		auto optType = to<OptionalType>(m_retType);
		TypePointers targetTypes;
		if (auto const* targetTupleType = dynamic_cast<TupleType const*>(optType->valueType()))
			targetTypes = targetTupleType->components();
		else
			targetTypes = TypePointers{optType->valueType()};

		ChainDataDecoder decode{&m_pusher};
		DecodePositionFromOneSlice pos;
		decode.decodeParametersQ(targetTypes, pos);

		m_exprCompiler.collectLValue(lValueInfo, true, false);
		solAssert(stackSize + 1 == m_pusher.stackSize(), toString(stackSize) + " vs" + toString(m_pusher.stackSize()));
	} else if (_node.memberName() == "decodeFunctionParams") {
		const int saveStackSize = m_pusher.stackSize();
		CallableDeclaration const* functionDefinition = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
		const LValueInfo lValueInfo =
				m_exprCompiler.expandLValue(&_node.expression(), true,
											 _node.expression().annotation().isLValue);
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
			decoder.decodePublicFunctionParameters(types, isResponsible, true);

			const int saveStackSize2 = m_pusher.stackSize();
			const int paramQty = functionDefinition->parameters().size() + (isResponsible ? 1 : 0);
			m_pusher.blockSwap(saveStackSize2 - saveStackSize - paramQty, paramQty);

			m_pusher.pushSlice("x8_");
		}
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (boost::starts_with(_node.memberName(), "load")) {
		const LValueInfo lValueInfo =
				m_exprCompiler.expandLValue(&_node.expression(), true, _node.expression().annotation().isLValue);
		if (_node.memberName() == "loadRefAsSlice") {
			m_pusher.push(-1 + 2, "LDREFRTOS");
			m_pusher.exchange(1);
		} else if (_node.memberName() == "loadRef") {
			m_pusher.push(-1 + 2, "LDREF");
		} else if (_node.memberName() == "loadUnsigned" || _node.memberName() == "loadSigned") {
			std::string cmd = "LD";
			cmd += (_node.memberName() == "loadSigned" ? "I" : "U");
			const auto& val = ExprUtils::constValue(*m_arguments[0]);
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
			m_pusher.push(-1 + 2, "LDGRAMS");
		} else if (_node.memberName() == "loadSlice") {
			if (m_arguments.size() == 1) {
				const auto& value = ExprUtils::constValue(*m_arguments[0].get());
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

		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "skip") {
		const LValueInfo lValueInfo =
				m_exprCompiler.expandLValue(&_node.expression(), true,
											 _node.expression().annotation().isLValue);
		if (m_arguments.size() == 1) {
			pushArgAndConvert(0);
			m_pusher.push(-2+1, "SDSKIPFIRST");
		} else {
			pushArgAndConvert(0);
			pushArgAndConvert(1);
			m_pusher.push(-3+1, "SSKIPFIRST");
		}
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "decodeStateVars") {
		const int saveStackSize = m_pusher.stackSize();

		std::vector<Type const*> stateVarTypes;

		auto retTuple = to<TupleType>(m_retType);
		for (TypePointer const type : retTuple->components()) {
			stateVarTypes.push_back(type);
		}


		const LValueInfo lValueInfo =
				m_exprCompiler.expandLValue(&_node.expression(), true,
											_node.expression().annotation().isLValue);
		// lvalue.. slice
		ChainDataDecoder decoder{&m_pusher};
		decoder.decodeData(0, 0, stateVarTypes);
		const int saveStackSize2 = m_pusher.stackSize();
		const int paramQty = stateVarTypes.size();
		m_pusher.blockSwap(saveStackSize2 - saveStackSize - paramQty, paramQty);
		m_pusher.pushSlice("x8_");
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else {
		solUnimplemented("");
	}
}


bool FunctionCallCompiler::checkForTvmVectorMethods(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TvmVector)
		return false;

	if (_node.memberName() == "push") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);
		acceptExpr(m_arguments[0].get());

		//start callref
		m_pusher.startContinuation();

		// vector new_el
		m_pusher.startOpaque();
		m_pusher.exchange(1);
		// new_el vector
		m_pusher.pushS(0);
		m_pusher.push(0, "TLEN");
		//new_el vector vector_len

		// if vector is not empty get last tuple
		m_pusher.startContinuation();
		m_pusher.push(+1, "TPOP");
		// new_el vector' last_tuple
		m_pusher.pushS(0);
		m_pusher.push(0, "TLEN");
		m_pusher.pushInt(TvmConst::TvmTupleLen);
		m_pusher.push(-1, "SUB");

		// if last tuple is full (his length is equal to 255) push it back to vector and create new tuple
		m_pusher.startContinuation();
		m_pusher.push(-1, "TPUSH");
		m_pusher.tuple(0);
		m_pusher.endContinuation();
		m_pusher.ifNot();
		// new_el vector' tuple
		m_pusher.endContinuation();

		// if vector is empty create new tuple
		m_pusher.startContinuation();
		m_pusher.tuple(0);
		m_pusher.endContinuation();

		m_pusher.ifElse();

		// new_el vector' tuple
		m_pusher.rot();
		// vector' tuple new_el
		m_pusher.push(-1, "TPUSH");
		// vector' tuple'
		m_pusher.push(-1, "TPUSH");
		// vector''
		m_pusher.endOpaque(2, 1);

		// end callref
		m_pusher.pushRefContAndCallX(2, 1, false);

		m_exprCompiler.collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "pop") {
		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);
		const int stackChange = m_pusher.stackSize() - stackSize;

		//start callref
		m_pusher.startContinuation();

		// vector
		m_pusher.startOpaque();
		m_pusher.push(+1, "TPOP");
		// vector' last_tuple
		m_pusher.push(+1, "TPOP");
		// [vector_expand] vector' last_tuple' last_el

		// put last el before vector and it's expand components
		m_pusher.blockSwap(stackChange + 1, 1);
		// last_el [vector_expand] vector' last_tuple'

		// if the last tuple is empty delete it
		m_pusher.pushS(0);
		m_pusher.push(0, "TLEN");
		// last_el [vector_expand] vector' last_tuple' last_tuple_len

		// if last_tuple is not empty push it back to the vector
		m_pusher.startContinuation();
		m_pusher.push(-1, "TPUSH");
		m_pusher.endContinuation();

		// if last_tuple is empty drop it
		m_pusher.startContinuation();
		m_pusher.drop(1);
		m_pusher.endContinuation();

		m_pusher.ifElse();
		// last_el [vector_expand] vector'

		m_pusher.endOpaque(1, 2);

		// end callref
		m_pusher.pushRefContAndCallX(1, 2, false);

		m_exprCompiler.collectLValue(lValueInfo, true, false);
		// last_el
		return true;
	}

	if (_node.memberName() == "length") {
		acceptExpr(&_node.expression());
		// vector

		//start callref
		m_pusher.startContinuation();

		m_pusher.startOpaque();
		m_pusher.pushS(0);
		m_pusher.push(-1 + 1, "TLEN");
		// vector len
		m_pusher.pushS(0);
		// vector len len

		// if len is not zero count length of full tuples and add length of the last tuple
		m_pusher.startContinuation();
		// vector len
		m_pusher.push(0, "DEC");
		// vector len--
		m_pusher.pushInt(TvmConst::TvmTupleLen);
		m_pusher.push(-1, "MUL");
		// vector full_tuples_len
		m_pusher.exchange(1);
		m_pusher.push(-1+1, "LAST");
		// full_tuples_len last_tuple
		m_pusher.push(+1-1, "TLEN");
		// full_tuples_len last_tuple_len
		m_pusher.push(-1, "ADD");
		m_pusher.endContinuation();

		// if length is zero just leave it on stack
		m_pusher.startContinuation();
		m_pusher.popS(1);
		m_pusher.endContinuation();

		m_pusher.ifElse();

		m_pusher.endOpaque(1, 1, true);

		// end callref
		m_pusher.pushRefContAndCallX(1, 1, false);
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
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);

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
				if (ExprUtils::constBool(*argument)) {
					continue;
				}
				acceptExpr(argument.get());
				++args;
			}
			m_pusher.blockSwap(1, args);
			for (const auto &argument: m_arguments) {
				std::optional<bool> value = ExprUtils::constBool(*argument);
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
			const auto& val = ExprUtils::constValue(*m_arguments[1]);
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

		m_exprCompiler.collectLValue(lValueInfo, true, false);
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

	if (_node.memberName() == "size") {
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
			m_pusher.indexNoexcep(0);
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
		m_pusher.pushMacroCallInCallRef(3, 1, "__substr_macro");
	} else if (_node.memberName() == "find") {
		acceptExpr(&_node.expression());
		pushArgs();
		Type::Category cat = m_arguments.at(0)->annotation().type->category();
		if (cat == Type::Category::FixedBytes) {
			m_pusher.pushMacroCallInCallRef(2, 1, "__strchr_macro");
		} else {
			m_pusher.pushMacroCallInCallRef(2, 1, "__strstr_macro");
		}
	} else if (_node.memberName() == "findLast") {
		acceptExpr(&_node.expression());
		pushArgs();
		m_pusher.pushMacroCallInCallRef(2, 1, "__strrchr_macro");
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
		cellBitRefQty();
	} else if (_node.memberName() == "push") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);
		auto arrayBaseType = to<ArrayType>(getType(&_node.expression()))->baseType();
		IntegerType const& key = getArrayKeyType();
		DataType dataType;
		if (m_arguments.empty()) {
			dataType = m_pusher.pushDefaultValueForDict(&key, arrayBaseType);
		} else {
			pushArgs();
			dataType = m_pusher.prepareValueForDictOperations(&key, arrayBaseType); // arr value'
		}
		// stack: arr value
		arrayPush(m_pusher, arrayBaseType, dataType);
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "pop") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);
		// arr
		m_pusher.push(-1 + 2, "UNTUPLE 2"); // size dict
		m_pusher.pushS(1); // size dict size
		m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
		m_pusher.exchange(1); // dict size
		m_pusher.push(0, "DEC"); // dict newSize
		m_pusher.pushS(0); // dict newSize newSize
		m_pusher.rot(); // newSize newSize dict
		m_pusher.pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
		m_pusher.push(-3 + 2, "DICTUDEL"); // newSize dict ?
		m_pusher.drop(1);  // newSize dict
		m_pusher.push(-2 + 1, "TUPLE 2");  // arr
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "append") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true, true);
		pushArgAndConvert(0);
		m_pusher.pushMacroCallInCallRef(2, 1, "concatenateStrings_macro");
		m_exprCompiler.collectLValue(lValueInfo, true, false);
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
		if (auto tt = to<TupleType>(m_retType)) {
			m_pusher.untuple(tt->components().size());
		} else if (optValueAsTuple(m_retType)) {
			m_pusher.untuple(1);
		}
		return true;
	}

	if (_node.memberName() == "set") {
		Type const* rightType{};
		if (m_arguments.size() >= 2) {
			vector<Type const*> types;
			for (ASTPointer<Expression const> const& arg : m_arguments) {
				types.push_back(getType(arg.get()));
			}
			rightType = TypeProvider::tuple(types);
		} else {
			rightType = getType(m_arguments.at(0).get());
		}
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false, true);
		pushArgs(false, false);
		m_pusher.hardConvert(getType(&_node.expression()), rightType);
		m_exprCompiler.collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "reset") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false, true);
		m_pusher.pushDefaultValue(optional);
		m_exprCompiler.collectLValue(lValueInfo, true, false);
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
		cellBitRefQty();
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethod() {
	auto _node = to<MemberAccess>(&m_functionCall.expression());
	if (_node->memberName() == "transfer") { // addr.transfer(...)
		std::map<int, Expression const *> exprs;
		std::map<int, std::string> constParams{{TvmConst::int_msg_info::ihr_disabled, "1"}, {TvmConst::int_msg_info::bounce, "1"}};
		std::function<void(int)> appendBody;
		std::function<void()> pushSendrawmsgFlag;
		std::function<void()> appendStateInit;

		auto setValue = [&](Expression const* expr) {
			const auto& value = ExprUtils::constValue(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(u256(value.value()));
			} else {
				exprs[TvmConst::int_msg_info::tons] = expr;
			}
		};

		auto setBounce = [&](auto expr){
			const std::optional<bool> value = ExprUtils::constBool(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::bounce] = value.value() ? "1" : "0";
			} else {
				exprs[TvmConst::int_msg_info::bounce] = expr;
				constParams.erase(TvmConst::int_msg_info::bounce);
			}
		};

		auto setAppendStateInit = [&](Expression const* expr) {
			appendStateInit = [expr, this](){
				// Either StateInit ^StateInit
				m_pusher.push(-1 + 1, "STONE"); // ^StateInit
				acceptExpr(expr);
				m_pusher.pushS(0);
				checkStateInit();
				m_pusher.push(-2 + 1, "STREFR");
			};
		};

		exprs[TvmConst::int_msg_info::dest] = &_node->expression();

		int argumentQty = static_cast<int>(m_arguments.size());
		if (!m_names.empty() || argumentQty == 0) {
			// string("value"), string("bounce"), string("flag"), string("body"), string("currencies")
			for (int arg = 0; arg < argumentQty; ++arg) {
				switch (str2int(m_names[arg]->c_str())) {
					case str2int("value"):
						setValue(m_arguments[arg].get());
						break;
					case str2int("bounce"):
						setBounce(m_arguments[arg].get());
						break;
					case str2int("flag"):
						pushSendrawmsgFlag = [e = m_arguments[arg], this](){
							acceptExpr(e.get());
						};
						break;
					case str2int("body"):
						appendBody = [e = m_arguments[arg], this](int /*size*/){
							m_pusher.stones(1);
							acceptExpr(e.get());
							m_pusher.push(-1, "STREFR");
							return false;
						};
						break;
					case str2int("currencies"):
						exprs[TvmConst::int_msg_info::currency] = m_arguments[arg].get();
						break;
					case str2int("stateInit"):
						setAppendStateInit(m_arguments[arg].get());
						break;
					default:
						solUnimplemented("");
				}
			}
		} else {
			solAssert(1 <= argumentQty && argumentQty <= 6, "");
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
			if (argumentQty >= 5) {
				exprs[TvmConst::int_msg_info::currency] = m_arguments[4].get();
			}
			if (argumentQty >= 6) {
				setAppendStateInit(m_arguments.at(5).get());
			}
		}
		m_pusher.sendIntMsg(exprs, constParams, appendBody, pushSendrawmsgFlag, false, 0, appendStateInit);
	} else if (_node->memberName() == "isStdZero") {
		acceptExpr(&_node->expression());
		m_pusher.pushZeroAddress();
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "isExternZero") {
		acceptExpr(&_node->expression());
		m_pusher.pushSlice("x401_");
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "isNone") {
		acceptExpr(&_node->expression());
		m_pusher.pushSlice("x2_");
		m_pusher.push(-2 + 1, "SDEQ");
	} else if (_node->memberName() == "unpack") {
		acceptExpr(&_node->expression());
		m_pusher.push(-1 + 2, "REWRITESTDADDR");
	} else if (_node->memberName() == "getType") {
		acceptExpr(&_node->expression());
		m_pusher.push(+1 - 1, "PLDU 2");
	} else if (_node->memberName() == "isStdAddrWithoutAnyCast") {
		acceptExpr(&_node->expression());
		// t = (2, u, x, s); check t[0] == 2 and t[1] is null
		m_pusher.push(-1 + 1, "PARSEMSGADDR");
		m_pusher.pushS(0);
		m_pusher.push(-1 + 1, "INDEX_NOEXCEP 0");
		m_pusher.push(-1 + 1, "EQINT 2"); // t tag==2

		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
			"PUSHCONT {",
			"	SECOND",
			"	ISNULL",
			"}",
			"PUSHCONT {",
			"	DROP",
			"	FALSE",
			"}",
			"IFELSE"
		}, 2, 1, true));
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForTvmConfigParamFunction(MemberAccess const &_node) {
	if (_node.memberName() == "rawConfigParam") { // tvm.rawConfigParam
		const int stackSize = m_pusher.stackSize();
		pushArgAndConvert(0);
		m_pusher.startOpaque();
		m_pusher.pushAsym("CONFIGPARAM");
		m_pusher.pushS(0);
		m_pusher.startContinuation();
		m_pusher.pushEmptyCell();
		m_pusher.exchange(1);
		m_pusher.endContinuation();
		m_pusher.ifNot();
		m_pusher.endOpaque(1, 2);
		solAssert(stackSize + 2 == m_pusher.stackSize(), "");
		return true;
	}
	if (_node.memberName() == "configParam") { // tvm.configParam
		const int stackSize = m_pusher.stackSize();
		auto paramNumberLiteral = dynamic_cast<const Literal *>(m_arguments[0].get());

		Type const* type = paramNumberLiteral->annotation().type;
		u256 value = type->literalValue(paramNumberLiteral);
		std::string paramNumber = value.str();

		if (paramNumber == "1") {
			//_ elector_addr:bits256 = ConfigParam 1;
			m_pusher.pushInt(1);

			m_pusher.startOpaque();
			m_pusher.pushAsym("CONFIGPARAM");

			m_pusher.startContinuation();
			m_pusher.push(0, "CTOS");
			m_pusher.push(+1, "LDU 256");
			m_pusher.push(-1, "ENDS");
			m_pusher.push(+1, "TRUE");
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher.pushInt(0);
			m_pusher.push(+1, "FALSE");
			m_pusher.endContinuation();

			m_pusher.ifElse();
			m_pusher.endOpaque(1, 2);

			solAssert(stackSize + 2 == m_pusher.stackSize(), "");
		}

		//	config_param15: uint32, uint32, uint32, uint32, bool
		//	config_param17: uint32, uint32, uint32, uint32, bool
		if (paramNumber == "15") {
			//_ validators_elected_for:uint32 elections_start_before:uint32
			//  elections_end_before:uint32 stake_held_for:uint32
			//  = ConfigParam 15;
			m_pusher.pushInt(15);

			m_pusher.startOpaque();
			m_pusher.pushAsym("CONFIGPARAM");

			m_pusher.startContinuation();
			m_pusher.push(0, "CTOS");
			m_pusher.push(+1, "LDU 32");
			m_pusher.push(+1, "LDU 32");
			m_pusher.push(+1, "LDU 32");
			m_pusher.push(+1, "LDU 32");
			m_pusher.push(-1, "ENDS");
			m_pusher.push(+1, "TRUE");
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.endContinuation();

			m_pusher.ifElse();
			m_pusher.endOpaque(1, 5);
		}

		if (paramNumber == "17"){
			//_    min_stake:Grams    max_stake:Grams
			//     min_total_stake:Grams    max_stake_factor:uint32 = ConfigParam 17;
			m_pusher.pushInt(17);

			m_pusher.startOpaque();
			m_pusher.pushAsym("CONFIGPARAM");

			m_pusher.startContinuation();
			m_pusher.push(0, "CTOS");
			m_pusher.push(+1, "LDGRAMS");
			m_pusher.push(+1, "LDGRAMS");
			m_pusher.push(+1, "LDGRAMS");
			m_pusher.push(+1, "LDU 32");
			m_pusher.push(-1, "ENDS");
			m_pusher.push(+1, "TRUE");
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.pushInt(0);
			m_pusher.endContinuation();

			m_pusher.ifElse();
			m_pusher.endOpaque(1, 5);
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

			m_pusher.pushInt(34);

			m_pusher.startOpaque();

			m_pusher.pushAsym("CONFIGPARAM");

			m_pusher.startContinuation();
			m_pusher.push(0, "CTOS");
			m_pusher.push(+1, "LDU 8"); // constructor
			m_pusher.push(+1, "LDU 32"); // utime_since
			m_pusher.push(+1, "LDU 32"); // utime_until
			m_pusher.push(+1, "LDU 16"); // total
			m_pusher.push(+1, "LDU 16"); // main
			m_pusher.push(+1, "LDU 64"); // total_weight
			m_pusher.push(+1, "LDDICT"); // ValidatorDescr
			m_pusher.push(-1, "ENDS");
			m_pusher.push(+1, "TRUE");
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher.push(+1, "PUSHINT 0"); // constructor
			m_pusher.push(+1, "PUSHINT 0"); // utime_since
			m_pusher.push(+1, "PUSHINT 0"); // utime_until
			m_pusher.push(+1, "PUSHINT 0"); // total
			m_pusher.push(+1, "PUSHINT 0"); // main
			m_pusher.push(+1, "PUSHINT 0"); // total_weight
			m_pusher.push(+1, "NEWDICT"); // ValidatorDescr
			m_pusher.push(+1, "FALSE"); //
			m_pusher.endContinuation();

			m_pusher.ifElse();

			m_pusher.endOpaque(1, 8);
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
		m_pusher.getGlob(TvmConst::C7::MsgPubkey);
		m_pusher.startOpaque();
		m_pusher.pushS(0);
		m_pusher.push(-1 + 1, "ISNULL");
		m_pusher.startContinuation();
		m_pusher.drop();
		m_pusher.pushInt(0);
		m_pusher.endContinuation();
		m_pusher._if();
		m_pusher.endOpaque(1, 1, true);
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

void FunctionCallCompiler::goshFunction() {
	Type const* expressionType = getType(&m_functionCall.expression());
	auto functionType = dynamic_cast<FunctionType const*>(expressionType);
	switch (functionType->kind()) {
		case FunctionType::Kind::GoshDiff:
			pushArgs();
			m_pusher << "DIFF";
			break;
		case FunctionType::Kind::GoshApplyPatch:
			pushArgs();
			m_pusher << "DIFF_PATCH";
			break;
		case FunctionType::Kind::GoshZip:
			pushArgs();
			m_pusher << "ZIP";
			break;
		case FunctionType::Kind::GoshUnzip:
			pushArgs();
			m_pusher << "UNZIP";
			break;
		case FunctionType::Kind::GoshZipDiff:
			pushArgs();
			m_pusher << "DIFF_ZIP";
			break;
		case FunctionType::Kind::GoshApplyZipPatch:
			pushArgs();
			m_pusher << "DIFF_PATCH_ZIP";
			break;
		case FunctionType::Kind::GoshApplyPatchQ:
			pushArgs();
			m_pusher << "DIFF_PATCHQ";
			break;
		case FunctionType::Kind::GoshApplyZipPatchQ:
			pushArgs();
			m_pusher << "DIFF_PATCH_ZIPQ";
			break;
		default:
			solUnimplemented("Unsupported function call");
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
				pushArgAndConvert(2);
				pushArgAndConvert(1);
				m_pusher.push(+1, "NEWC");
				m_pusher.push(-1, "STU 256");
				m_pusher.push(-1, "STU 256");
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
		m_pusher.drop();
	} else if (_node.memberName() == "hexdump") { // tvm.hexdump
		pushArgs();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmCell)
		m_pusher.push(-1+1, "CTOS");
		m_pusher.push(0, "HEXDUMP");
		m_pusher.drop();
	} else if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
		const int stackSize = m_pusher.stackSize();
		pushArgs();

		m_pusher.push(0, "CTOS");
		m_pusher.pushS(0);
		m_pusher.pushSlice("x" + TvmConst::Selector::RootCodeCell());
		m_pusher.push(-2 + 1, "SDEQ");

		m_pusher.startOpaque();
		m_pusher.startContinuation();
		m_pusher.push(0, "PLDREFIDX 1");
		m_pusher.push(0, "CTOS");
		m_pusher.endContinuation();
		m_pusher._if();
		m_pusher.endOpaque(2, 1);

		m_pusher.push(0, "PLDREF");
		m_pusher.push(0, "CTOS");
		m_pusher.push(0, "BLESS");
		m_pusher.popC3();
		solAssert(stackSize == m_pusher.stackSize(), "");
	} else if (_node.memberName() == "getData") { // tvm.getData
		m_pusher.pushRoot();
	} else if (_node.memberName() == "setData") { // tvm.setData
		pushArgs();
		m_pusher.popRoot();
	} else if (_node.memberName() == "rawCommit") { // tvm.rawCommit
		m_pusher.push(0, "COMMIT");
	} else if (_node.memberName() == "commit") { // tvm.commit
		m_pusher.pushMacroCallInCallRef(0, 0, "c7_to_c4");
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
			DecodePositionAbiV2 position{32, 0, types};
			for (int i = m_arguments.size() - 1; i >= 1 + shift; --i) {
				acceptExpr(m_arguments.at(i).get());
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

		m_pusher.startContinuation();
		m_pusher.pushCall(0, 0, "c7_to_c4");
		m_pusher.endContinuationFromRef();
		m_pusher.ifNot();

		if (_node.memberName() == "exit")
			m_pusher._throw("THROW 0");
		else
			m_pusher._throw("THROW 1");
	} else if (_node.memberName() == "code") {
		m_pusher << "MYCODE";
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
		std::vector<string> codeLines = split(code);
		m_pusher.push(createNode<HardCode>(codeLines, 1, 1, false));
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
		std::vector<string> codeLines = split(code);
		m_pusher.push(createNode<HardCode>(codeLines, 2, 1, false));
	} else if (_node.memberName() == "replayProtTime") {
		m_pusher.getGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "setReplayProtTime") {
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "replayProtInterval") {
		m_pusher.pushInt(TvmConst::Message::ReplayProtection::Interval);
	} else if (_node.memberName() == "setGasLimit") {
		pushArgs();
		m_pusher.push(-1, "SETGASLIMIT");
	} else if (_node.memberName() == "initCodeHash") {
		m_pusher << "INITCODEHASH";
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::abiFunction() {
	switch (m_funcType->kind()) {
		case FunctionType::Kind::ABIEncode: {
			std::vector<Type const *> types;
			for (ASTPointer<Expression const> const& arg : m_arguments) {
				types.emplace_back(arg->annotation().type->mobileType());
			}
			DecodePositionAbiV2 position{0, 0, types};

			for (ASTPointer<Expression const> const& arg : m_arguments | boost::adaptors::reversed) {
				acceptExpr(arg.get());
			}
			m_pusher << "NEWC";
			ChainDataEncoder encoder{&m_pusher};
			encoder.encodeParameters(types, position);
			m_pusher << "ENDC";
			break;
		}
		case FunctionType::Kind::ABIDecode: {
			std::vector<Type const*> types;
			auto te = to<TupleExpression>(m_arguments.at(1).get());
			if (te) {
				for (const ASTPointer<Expression>& e : te->components()) {
					auto const* argTypeType = dynamic_cast<TypeType const*>(e->annotation().type);
					TypePointer actualType = argTypeType->actualType();
					types.emplace_back(actualType);
				}
			} else {
				auto const* argTypeType = dynamic_cast<TypeType const*>(m_arguments.at(1)->annotation().type);
				TypePointer actualType = argTypeType->actualType();
				types.emplace_back(actualType);
			}

			acceptExpr(m_arguments.at(0).get());
			m_pusher << "CTOS";
			ChainDataDecoder decoder{&m_pusher};
			decoder.decodeData(0, 0, types);
			break;
		}
		default:
			solUnimplemented("");
	}
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
			m_pusher.pushInt(StackPusher::pow10(power)); // res 10^n
			m_pusher.exchange(1);
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
			m_pusher.exchange(1);
			m_pusher.checkFit(retTuple->components().at(0));
			m_pusher.exchange(1);
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
		const auto& value = ExprUtils::constValue(*expression);
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
					m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(StrUtils::literalToSliceAddress(literal)));
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

			const auto& value = ExprUtils::constValue(*m_arguments[0]);
			if (value.has_value()) {
				if (value < 0 || value >= enumDef->members().size()) {
					cast_error(m_functionCall, "The value must be in the range 1 - " +
											   toString(enumDef->members().size()) + ".");
				}
				m_pusher.push(+1, "PUSHINT " + value.value().str());
				return;
			}

			acceptExpr(m_arguments[0].get()); // it's correct
			m_pusher.pushS(0);
			m_pusher.pushInt(enumDef->members().size());
			m_pusher.push(-1, "GEQ");
			m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::WrongValueOfEnum));

			auto type = m_arguments[0].get()->annotation().type;
			TypeInfo ti(type);
			if (!ti.isNumeric || ti.isSigned) {
				m_pusher.push(0, "UFITS 256"); // checks whether value >= 0
			}

			return;
		}
	}

	solAssert(m_arguments.size() == 1, "");
	acceptExpr(m_arguments.at(0).get()); // it's correct
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
		Pointer<CodeBlock> body = m_pusher.ctx().getInlinedFunction(functionName);
		int take = functionType->parameterTypes().size();
		int ret = functionType->returnParameterTypes().size();
		m_pusher.pushInlineFunction(body, take, ret);
	} else {
		Declaration const& funDecl = m_funcType->declaration();
		ContractDefinition const* contractDecl= funDecl.annotation().contract;
		bool isLib = contractDecl->isLibrary();
		std::string name;
		if (isLib) {
			auto fd = to<FunctionDefinition>(&funDecl);
			name = TVMCompilerContext::getLibFunctionName(fd, false);
			m_pusher.ctx().addLib(fd);
		} else {
			name = m_pusher.ctx().getFunctionInternalName(functionDefinition, false);
		}
		m_pusher.pushCallOrCallRef(name, functionType);
	}
	return true;
}

bool FunctionCallCompiler::checkSolidityUnits() {
	if (m_funcType == nullptr) {
		return false;
	}

	auto checkAndParseExceptionCode = [](Expression const* e) -> std::optional<bigint> {
		const auto& val = ExprUtils::constValue(*e);
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

	switch (m_funcType->kind()) {
		case FunctionType::Kind::GasToValue: {
			pushArgs();
			m_pusher.pushMacroCallInCallRef(2, 1, "__gasToTon_macro");
			return true;
		}
		case FunctionType::Kind::ValueToGas: {
			pushArgs();
			m_pusher.pushMacroCallInCallRef(2, 1, "__tonToGas_macro");
			return true;
		}
		case FunctionType::Kind::BitSize: {
			pushArgs();
			m_pusher.push(-1 + 1, "BITSIZE");
			return true;
		}
		case FunctionType::Kind::UBitSize: {
			pushArgs();
			m_pusher.push(-1 + 1, "UBITSIZE");
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
					{TvmConst::int_msg_info::tons,         StrUtils::tonsToBinaryString(u256(0))},
					{TvmConst::int_msg_info::bounce,       "0"},
			};
			m_pusher.sendIntMsg(
					{{TvmConst::int_msg_info::dest, m_arguments[0].get()}},
					constParams,
					nullptr,
					[&]() { m_pusher.push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::SelfDestruct)); },
					false, 0, nullptr);
			return true;
		}

		case FunctionType::Kind::Require: {
			if (m_arguments.size() == 1) {
				pushArgAndConvert(0);
				m_pusher._throw("THROWIFNOT 100");
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
						m_pusher._throw("THROWARGIFNOT " + toString(exceptionCode.value()));
					else
						m_pusher._throw("THROWIFNOT " + toString(exceptionCode.value()));
				} else {
					pushArgAndConvert(1);
					if (!exceptionCode.has_value()) {
						m_pusher.pushInt(2);
						m_pusher.push(-2 + 1, "MAX");
					}
					pushArgAndConvert(0);
					if (m_arguments.size() == 3)
						m_pusher._throw("THROWARGANYIFNOT");
					else
						m_pusher._throw("THROWANYIFNOT");
				}
			} else {
				cast_error(m_functionCall, R"("require" takes from one to three m_arguments.)");
			}
			return true;
		}
		case FunctionType::Kind::Revert: {
			if (m_arguments.empty()) {
				m_pusher._throw("THROW 100");
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
					m_pusher._throw((withArg ? "THROWARG " : "THROW ") + toString(exceptionCode.value()));
				} else {
					pushArgAndConvert(0);
					if (!exceptionCode.has_value()) {
						m_pusher.pushInt(2);
						m_pusher.push(-2 + 1, "MAX");
					}
					m_pusher._throw(withArg ? "THROWARGANY" : "THROWANY");
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
			m_pusher.pushDefaultValue(TypeProvider::tvmtuple(TypeProvider::tvmbuilder()));
			// create new builder to store data in it
			m_pusher.push(+1, "NEWC");

			auto pushConstStr = [&](const string& constStr) {
				if (!constStr.empty()) {
					size_t maxSlice = TvmConst::CellBitLength / 8;
					for(size_t i = 0; i  < constStr.length(); i += maxSlice) {
						m_pusher.pushString(constStr.substr(i, min(maxSlice, constStr.length() - i)), true);
						// stack: BldrList builder Slice
						m_pusher.pushMacroCallInCallRef(3, 2, "storeStringInBuilders_macro");
					}
					// stack: BldrList builder
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
							m_pusher.pushMacroCallInCallRef(7, 2, "convertIntToHexStr_macro");
						} else {
							m_pusher.pushMacroCallInCallRef(6, 2, "convertIntToDecStr_macro");
						}
						// stack: vector(TvmBuilder) builder
					} else {
						acceptExpr(m_arguments[it + 1].get());
						m_pusher.pushInt(9);
						m_pusher.pushMacroCallInCallRef(4, 2, "convertFixedPointToString_macro");
					}
				} else if (cat == Type::Category::Address) {
					// stack: vector(TvmBuilder) builder
					acceptExpr(m_arguments[it + 1].get());
					// stack: vector(TvmBuilder) builder address
					m_pusher.pushMacroCallInCallRef(3, 2, "convertAddressToHexString_macro");
					// stack: vector(TvmBuilder) builder
				} else if (isStringOrStringLiteralOrBytes(argType)) {
					// stack: vector(TvmBuilder) builder
					acceptExpr(m_arguments[it + 1].get());
					// stack: vector(TvmBuilder) builder string(cell)
					m_pusher.push(0, "CTOS");
					// stack: vector(TvmBuilder) builder string(slice)
					m_pusher.pushMacroCallInCallRef(3, 2, "storeStringInBuilders_macro");
					// stack: vector(TvmBuilder) builder
				} else if (cat == Type::Category::FixedPoint) {
					int power = to<FixedPointType>(argType)->fractionalDigits();
					acceptExpr(m_arguments[it + 1].get());
					m_pusher.pushInt(power);
					m_pusher.pushMacroCallInCallRef(4, 2, "convertFixedPointToString_macro");
				} else {
					cast_error(*m_arguments[it + 1].get(), "Unsupported argument type");
				}
			}
			pushConstStr(formatStr);

			m_pusher.pushMacroCallInCallRef(2, 1, "assembleList_macro");
			return true;
		}
		case FunctionType::Kind::Stoi: {
			pushArgAndConvert(0);
			m_pusher.pushMacroCallInCallRef(1, 1, "__stoi_macro");
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
		m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::BadFunctionIdOfFuncCall));
		auto functionType = to<FunctionType>(expr->annotation().type);
		int returnCnt = functionType->returnParameterTypes().size();
		int paramCnt = functionType->parameterTypes().size();
		m_pusher.pushC3();
		m_pusher.execute(paramCnt + 2, returnCnt);
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

	std::function<void()> pushKey = [&]() {
		if (Expression const* stateInit = findOption("pubkey")) {
			acceptExpr(stateInit);
		} else {
			m_pusher.pushInt(0);
		}
	};

	if (Expression const* stateInit = findOption("stateInit")) {
		acceptExpr(stateInit); // stack: stateInit
	} else if (Expression const* code = findOption("code")) {
		const int ss = m_pusher.stackSize();
		std::map<StateInitMembers, std::function<void()>> stateInitExprs;

		Expression const* varInit = findOption("varInit");
		bool hasVars = varInit != nullptr;
		auto ct = to<ContractType>(newExpr->typeName().annotation().type);
		stateInitExprs[StateInitMembers::Data] = generateDataSection(
			pushKey,
			hasVars ? varInit : nullptr,
			ct
		);

		stateInitExprs[StateInitMembers::Code] = [&]() {
			acceptExpr(code);
		};

		if (Expression const* splitDepth = findOption("splitDepth")) {
			stateInitExprs[StateInitMembers::SplitDepth] = [this, splitDepth]() {
				acceptExpr(splitDepth); // stack: data code split_depth
			};
		}

		buildStateInit(stateInitExprs);

		// stack: stateInit
		solAssert(ss + 1 == m_pusher.stackSize(), "");
	} else {
		solUnimplemented("");
	}

	std::variant<int8_t, std::function<void()>> pushWid = int8_t{0};
	if (Expression const* wid = findOption("wid")) {
		std::optional<bigint> value = ExprUtils::constValue(*wid);
		if (value) {
			pushWid = int8_t(value.value());
		} else {
			pushWid = [&, wid]() {
				acceptExpr(wid);
			};
		}
	}

	std::variant<bigint, std::function<void()>> pushValue;
	{
		Expression const *value = findOption("value");
		solAssert(value, "");
		std::optional<bigint> v = ExprUtils::constValue(*value);
		if (v) {
			pushValue = v.value();
		} else {
			pushValue = [&, value]() {
				acceptExpr(value);
			};
		}
	}

	std::variant<bool, std::function<void()>> pushBounce = true;
	if (Expression const* bounce = findOption("bounce")) {
		if (std::optional<bool> value = ExprUtils::constBool(*bounce)) {
			pushBounce = value.value();
		} else {
			pushBounce = [&, bounce]() {
				acceptExpr(bounce);
			};
		}
	}

	std::function<void()> pushCurrency;
	if (Expression const* currencies = findOption("currencies")) {
		pushCurrency = [this, currencies]() {
			acceptExpr(currencies);
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
	if (Expression const* flag = findOption("flag")) {
		pushSendrawmsgFlag = [flag, this]() { acceptExpr(flag); };
	}

	deployNewContract(pushWid, pushValue, pushBounce, pushCurrency, pushBody, pushSendrawmsgFlag, m_arguments.size());
	// stack: destAddress
	return true;
}

void FunctionCallCompiler::deployNewContract(
	const std::variant<int8_t, std::function<void()>>& wid,
	const std::variant<bigint, std::function<void()>>& value,
	const std::variant<bool, std::function<void()>>& pushBounce,
	const std::function<void()>& pushCurrency,
	const std::function<void(int builderSize)>& appendBody,
	const std::function<void()>& pushSendrawmsgFlag,
	const int argQty
) {
	std::map<int, std::function<void()>> exprs;

	std::map<int, std::string> constParams = {{TvmConst::int_msg_info::ihr_disabled, "1"}};

	if (pushBounce.index() == 0) {
		constParams[TvmConst::int_msg_info::bounce] = StrUtils::boolToBinaryString(std::get<0>(pushBounce));
	} else if (pushBounce.index() == 1) {
		exprs[TvmConst::int_msg_info::bounce] = std::get<1>(pushBounce);
	}

	if (pushCurrency) {
		exprs[TvmConst::int_msg_info::currency] = pushCurrency;
	}

	// stack: stateInit
	m_pusher.pushS(0);
	m_pusher.push(-1 + 1, "HASHCU"); // stack: stateInit hash

	if (wid.index() == 0) {
		int8_t w = std::get<0>(wid);
		std::string binWID = "100";
		binWID += StrUtils::toBitString(u256(w), 8);
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "STSLICECONST x" + StrUtils::binaryStringToSlice(binWID));
	} else {
		std::get<1>(wid)();
		m_pusher.push(+1, "NEWC");
		m_pusher.push(-1 + 1, "STSLICECONST x9_"); // addr_std$10 anycast:(Maybe Anycast) // 10 0 1 = 9
		m_pusher.push(-1, "STI 8"); // workchain_id:int8
	}
	m_pusher.push(-1, "STU 256"); // address:bits256
	bool isDestBuilder = !m_isCurrentResultNeeded;
	if (!isDestBuilder) {
		m_pusher.push(-1 + 1, "ENDC");
		m_pusher.push(-1 + 1, "CTOS");
	}

	// stack: arg[n-1], ..., arg[1], arg[0], stateInit, destAddress
	m_pusher.blockSwap(argQty + 1, 1);
	// stack:  destAddress, arg[n-1], ..., arg[1], arg[0], stateInit
	int destAddressStack = m_pusher.stackSize() - 1 - argQty;

	if (value.index() == 0) {
		constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(std::get<0>(value));
	} else {
		exprs[TvmConst::int_msg_info::tons] = [&](){
			std::get<1>(value)();
		};
	}

	exprs[TvmConst::int_msg_info::dest] = [&](){
		int stackIndex = m_pusher.stackSize() - destAddressStack;
		m_pusher.pushS(stackIndex);
	};


	std::function<void()> appendStateInit = [&]() {
		m_pusher.stones(1); // stateInit builder
		m_pusher.pushS(1);
		checkStateInit();
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
		pushSendrawmsgFlag,
		StackPusher::MsgType::Internal,
		isDestBuilder
	);
	// stack: destAddress
}

void FunctionCallCompiler::checkStateInit() {
	// _ split_depth:(Maybe (## 5)) special:(Maybe TickTock)
	//  code:(Maybe ^Cell) data:(Maybe ^Cell)
	//  library:(HashmapE 256 SimpleLib) = StateInit;

	m_pusher.startContinuation();
	m_pusher.push(0, "CTOS");

	// split_depth:(Maybe (## 5))
	m_pusher.push(+1, "LDI 1");
	m_pusher.exchange(1);
	m_pusher.push(-1, ""); // fix stack: drop condition
	m_pusher.startContinuation();
	m_pusher.push(+1, "LDI 5");
	m_pusher.dropUnder(1, 1);
	m_pusher.endContinuation();
	m_pusher._if();

	// special:(Maybe TickTock)
	m_pusher.push(+1, "LDI 1");
	m_pusher.exchange(1);
	m_pusher.push(-1, ""); // fix stack: drop condition
	m_pusher.startContinuation();
	// tick_tock$_ tick:Bool tock:Bool = TickTock;
	m_pusher.push(+1, "LDI 2");
	m_pusher.dropUnder(1, 1);
	m_pusher.endContinuation();
	m_pusher._if();

	// code:(Maybe ^Cell) data:(Maybe ^Cell)
	// library:(HashmapE 256 SimpleLib)
	m_pusher.push(+1, "LDOPTREF");
	m_pusher.push(+1, "LDOPTREF");
	m_pusher.push(+1, "LDDICT");
	m_pusher.push(-1, "ENDS");
	m_pusher.drop(3);

	m_pusher.pushRefContAndCallX(1, 0, false);
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

	creatArrayWithDefaultValue();
	return true;
}

void FunctionCallCompiler::creatArrayWithDefaultValue() {
	std::optional<bigint> num = ExprUtils::constValue(*m_arguments.at(0));
	if (num.has_value() && num.value() == 0) {
		auto arrayType = to<ArrayType>(m_functionCall.annotation().type);
		m_pusher.pushDefaultValue(arrayType);
		return ;
	}

	if (m_functionCall.annotation().isPure) {
		pushArgs();
		SourceReference sr = SourceReferenceExtractor::extract(&m_functionCall.location());
		const std::string computeName = "new_array_line_" +
										toString(sr.position.line) + "_column_" + toString(sr.position.column) + "_ast_id_" +
										toString(m_functionCall.id());
		m_pusher.compureConstCell(computeName);
		m_pusher << "TUPLE 2";
		m_pusher.ctx().addNewArray(computeName, &m_functionCall);
		return ;
	}

	honestArrayCreation(false);
}

void FunctionCallCompiler::honestArrayCreation(bool onlyDict) {
	const int stackSize = m_pusher.stackSize();
	auto arrayType = to<ArrayType>(m_functionCall.annotation().type);
	IntegerType const& key = getArrayKeyType();
	Type const* arrayBaseType = arrayType->baseType();

	pushArgAndConvert(0); // N
	DataType const& dataType = m_pusher.pushDefaultValueForDict(&key, arrayBaseType); // N value
	m_pusher.pushInt(0);   // N value iter
	m_pusher << "NEWDICT"; // N value iter dict
	m_pusher.pushS(3);     // N value iter dict N

	solAssert(stackSize + 5 == m_pusher.stackSize(), "");
	m_pusher.push(-1, ""); // fix stack: drop replay iterator
	solAssert(stackSize + 4 == m_pusher.stackSize(), "");
	{
		// N value iter dict
		m_pusher.startContinuation();
		m_pusher.pushS(2);    // N value iter dict value
		m_pusher.pushS(2);    // N value iter dict value iter
		m_pusher << "INC";    // N value iter dict value iter++
		m_pusher.exchange(3); // N value iter++ dict value iter
		m_pusher.rot();       // N value iter++ value iter dict
		m_pusher.setDict(key, *arrayType->baseType(), dataType); // N value iter++ dict'
		m_pusher.endContinuation();
	}
	m_pusher.repeat(false);
	solAssert(stackSize + 4 == m_pusher.stackSize(), "");
	// N value iter dict
	if (onlyDict) {
		m_pusher.dropUnder(3, 1); // dict
	} else {
		m_pusher.dropUnder(2, 1); // N dict
		m_pusher << "TUPLE 2";
	}
	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
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
	solAssert(exprs.count(StateInitMembers::Special) == 0, "");
	solAssert(exprs.count(StateInitMembers::Library) == 0, "");
	solAssert(exprs.count(StateInitMembers::Code) == 1, "Code must present");
	solAssert(exprs.count(StateInitMembers::Data) == 1, "Data must present");

	const int ss = m_pusher.stackSize();

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
	solAssert(ss + 1 == m_pusher.stackSize(), "");
}


void FunctionCallCompiler::pushArgs(bool reversed, bool doConvert) {
	auto func = [&](const ASTPointer<const Expression> &e, int i) {
		acceptExpr(e.get());
		if (doConvert) {
			Type const *targetType{};
			if (m_funcType->parameterTypes().empty()) {
				targetType = m_functionCall.annotation().arguments->targetTypes.at(i);
			} else {
				targetType = m_funcType->parameterTypes().at(i);
			}
			m_pusher.hardConvert(targetType, e->annotation().type);
		}
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
	m_exprCompiler.compileNewExpr(expr);
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

Expression const* FunctionCallCompiler::findOption(const std::string& name) {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
	if (!functionOptions)
		return {};
	std::vector<ASTPointer<ASTString>> const &optionNames = functionOptions->names();
	auto iter = find_if(optionNames.begin(), optionNames.end(), [&](auto el) { return *el == name; });
	if (iter == optionNames.end())
		return {};
	size_t index = iter - optionNames.begin();
	return functionOptions->options().at(index).get();
}

void FunctionCallCompiler::cellBitRefQty(bool forCell) {
	m_pusher.startOpaque();

	if (forCell)
		m_pusher.pushAsym("CDATASIZEQ");
	else
		m_pusher.pushAsym("SDATASIZEQ");

	m_pusher.startContinuation();
	m_pusher.push(-3 + 1, "TUPLE 3");
	m_pusher.endContinuation();

	m_pusher.startContinuation();
	m_pusher.pushNull();
	m_pusher.endContinuation();

	m_pusher.ifElse();

	m_pusher.endOpaque(2, 1, true);
}