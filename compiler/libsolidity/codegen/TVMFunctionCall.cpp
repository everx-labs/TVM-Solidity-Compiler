/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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
#include "TVM.hpp"

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
	m_memberAccess{to<MemberAccess>(&m_functionCall.expression())},
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
	auto reportError = [&](){
		cast_error(m_functionCall, "Unsupported function call");
	};

	if (m_funcType) {
		switch (m_funcType->kind()) {
			case FunctionType::Kind::GasLeft: {
				m_pusher << "GASREMAINING";
				return;
			}
			default:
				break;
		}
	}

	if (checkRemoteMethodCall(m_functionCall) ||
		(m_memberAccess != nullptr && libraryCall(*m_memberAccess)) ||
		checkForMappingOrCurrenciesMethods() ||
		checkNewExpression() ||
		checkAddressThis() ||
		checkSolidityUnits() ||
		checkLocalFunctionOrLibCallOrFuncVarCall()) {
		// do nothing
	} else if (m_memberAccess != nullptr && getType(&m_memberAccess->expression())->category() == Type::Category::Struct) {
		if (!structMethodCall()) {
			reportError();
		}
	} else if (*m_functionCall.annotation().kind == FunctionCallKind::StructConstructorCall) {
		structConstructorCall();
	} else if (*m_functionCall.annotation().kind == FunctionCallKind::TypeConversion) {
		if (m_arguments.empty()) { // TODO separate to another kind FunctionCallKind::some-type
			createObject();
		} else {
			typeConversion();
		}
	} else {
		if (m_memberAccess != nullptr) {
			auto category = getType(&m_memberAccess->expression())->category();
			auto ident = to<Identifier>(&m_memberAccess->expression());
			if (category == Type::Category::Array) {
				arrayMethods(*m_memberAccess);
			} else if (category == Type::Category::TvmSlice) {
				sliceMethods(*m_memberAccess);
			} else if (category == Type::Category::TvmBuilder) {
				builderMethods(*m_memberAccess);
			} else if (
				checkForTvmVectorMethods(*m_memberAccess, category) ||
				checkForOptionalMethods(*m_memberAccess))
			{
				// nothing
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
				if (m_funcType->kind() == FunctionType::Kind::ABIBuildIntMsg) {
					abiBuildIntMsg();
				} else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeData) {
					abiBuildDataInit();
				} else if (m_funcType->kind() == FunctionType::Kind::ABIBuildExtMsg) {
					abiBuildExtMsg();
				} else if (
					checkForTvmSendFunction(*m_memberAccess) ||
					checkForTvmConfigParamFunction(*m_memberAccess) ||
					checkForTvmFunction(*m_memberAccess) ||
					checkTvmABIDeployMethods(category)
				) {
					// do nothing
				} else {
					reportError();
				}
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "rnd") {
				rndFunction(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "gosh") {
				goshFunction();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "msg") {
				msgFunction(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "abi") {
				if (m_funcType->kind() == FunctionType::Kind::ABIDecodeData)
					abiDecodeData();
				else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeBody)
					abiEncodeBody();
				else if (m_funcType->kind() == FunctionType::Kind::ABIBuildIntMsg)
					abiBuildIntMsg();
				else if (m_funcType->kind() == FunctionType::Kind::ABIBuildExtMsg)
					abiBuildExtMsg();
				else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeData)
					abiBuildDataInit();
				else if (!checkTvmABIDeployMethods(category))
					abiFunction();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "math") {
				mathFunction(*m_memberAccess);
			} else if (category == Type::Category::Address) {
				addressMethod();
			} else if (category == Type::Category::TvmCell) {
				cellMethods(*m_memberAccess);
			} else if (category == Type::Category::Integer) {
				integerMethods();
			} else if (category == Type::Category::Variant) {
				variantMethods(*m_memberAccess);
			} else if (isSuper(&m_memberAccess->expression())) {
				superFunctionCall(*m_memberAccess);
			} else if (category == Type::Category::TypeType) {
				Type const* actualType = to<TypeType>(m_memberAccess->expression().annotation().type)->actualType();
				if (checkBaseContractCall(*m_memberAccess)) {
					// nothing
				} else if (to<UserDefinedValueType>(actualType)) {
					userDefinedValueMethods(*m_memberAccess);
				} else if (to<AddressType>(actualType)) {
					addressMethods(*m_memberAccess);
				} else {
					reportError();
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
	pusher << "UNTUPLE 2";  // value' size dict
	pusher.pushS(1); // value' size dict size
	pusher << "INC"; // value' size dict newSize
	pusher.blockSwap(3, 1); // newSize value' size dict
	pusher.setDict(getArrayKeyType(), *arrayBaseType, dataType); // newSize dict'
	pusher << "TUPLE 2";  // arr
}

bool FunctionCallCompiler::checkForMappingOrCurrenciesMethods() {
	if (m_memberAccess == nullptr || !to<MappingType>(m_memberAccess->expression().annotation().type))
		return false;

	const ASTString &memberName = m_memberAccess->memberName();
	if (isIn(memberName, "delMin", "delMax")) {
		mappingDelMinOrMax(memberName == std::string{"delMin"});
	} else  if (isIn(memberName, "at", "fetch", "exists", "replace", "add",
									"getSet", "getAdd", "getDel", "getReplace")) {
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
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	DelMinOrMax d{m_pusher, *keyType, *valueType, isDelMin, m_memberAccess};
	d.delMinOrMax();
}

void FunctionCallCompiler::mappingGetSet() {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	const ASTString &memberName = m_memberAccess->memberName();
	if (isIn(memberName, "fetch", "at")) {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType, false);
		acceptExpr(&m_memberAccess->expression()); // index dict
		if (memberName == "fetch")
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::Fetch);
		else
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::GetFromArray);
	} else if (memberName == "exists") {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType, false);
		acceptExpr(&m_memberAccess->expression()); // index dict
		m_pusher.getDict(*keyType, *valueType, GetDictOperation::Exist);
	} else if (isIn(memberName, "getDel")) {
		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);

		pushArgAndConvert(0); // lValue... map key
		m_pusher.prepareKeyForDictOperations(keyType, false);
		m_pusher.blockSwap(1, 1); // lValue... key map

		m_pusher.getDict(*keyType, *valueType, GetDictOperation::GetDelFromMapping); // lValue... map' value

		const int cntOfValuesOnStack = m_pusher.stackSize() - stackSize;
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1); // value lValue... map'
		m_exprCompiler.collectLValue(lValueInfo, true, false); // value
	} else if (isIn(memberName, "replace", "add", "getSet", "getAdd", "getReplace")) {
		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true); // lValue... map
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
			} else if (memberName == "getAdd") {
				op = GetDictOperation::GetDelFromMapping;
			} else if (memberName == "getReplace") {
				op = GetDictOperation::GetReplaceFromMapping;
			} else {
				solUnimplemented("");
			}
			m_pusher.getAndSetDict(*keyType, *valueType, op, dataType);
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
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	acceptExpr(&m_memberAccess->expression()); // dict

	DictMinMax compiler{m_pusher, *keyType, *valueType, isMin};
	compiler.minOrMax();
}

void FunctionCallCompiler::mappingPrevNextMethods() {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	pushArgAndConvert(0); // index
	m_pusher.prepareKeyForDictOperations(keyType, true); // index'
	acceptExpr(&m_memberAccess->expression()); // index' dict
	m_pusher.pushInt(dictKeyLength(keyType)); // index' dict nbits

	DictPrevNext compiler{m_pusher, *keyType, *valueType, m_memberAccess->memberName()};
	compiler.prevNext();
}

void FunctionCallCompiler::mappingKeysOrValues(bool areKeys) {
	m_pusher.pushEmptyArray();

	acceptExpr(&m_memberAccess->expression());
	// array map
	m_pusher.pushS(0);
	// array map map
	Type const* mapKeyType{};
	Type const* mapValueType{};
	std::tie(mapKeyType, mapValueType) = realDictKeyValue(m_memberAccess->expression().annotation().type);
	DictMinMax compiler{m_pusher, *mapKeyType, *mapValueType, true};
	compiler.minOrMax();
	// array map minPair

	m_pusher.startContinuation();
	// array map curPair
	m_pusher.pushS(0);
	m_pusher << "ISNULL";
	m_pusher << "NOT";
	m_pusher.fixStack(-1);
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
	acceptExpr(&m_memberAccess->expression());
	m_pusher << "DICTEMPTY";
}

void FunctionCallCompiler::superFunctionCall(MemberAccess const &_node) {
	pushArgs();
	auto someFunDecl = to<FunctionDefinition>(_node.annotation().referencedDeclaration);
	FunctionDefinition const* superFunc = getSuperFunction(
		m_pusher.ctx().currentFunction()->annotation().contract,
		m_pusher.ctx().getContract(),
		someFunDecl->externalIdentifierHex()
	);
	solAssert(superFunc, "");
	std::string functionName = m_pusher.ctx().getFunctionInternalName(superFunc, true);
	m_pusher.pushCallOrCallRef(superFunc, std::nullopt, true);
}

void FunctionCallCompiler::userDefinedValueMethods(MemberAccess const &_memberAccess) {
	if (isIn(_memberAccess.memberName(), "wrap", "unwrap")) {
		pushArgs();
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethods(MemberAccess const &_node) {
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
			m_pusher << "NEWC"; // numb cntBit cntBit builder
			m_pusher << "STSLICECONST x6_";
			m_pusher << "STU 9"; // numb cntBit builder''
			m_pusher.exchange(1); // numb builder'' cntBit
			m_pusher << "STUX"; // builder'''
			m_pusher << "ENDC";
			m_pusher << "CTOS"; // extAddress
		}
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
			m_pusher << "NEWC";
			m_pusher << "STSLICECONST x9_";
			m_pusher << "STI 8";
			m_pusher << "STU 256";
			m_pusher << "ENDC";
			m_pusher << "CTOS";
		}
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::libraryCall(MemberAccess const& ma) {
	if (auto libFunction = to<FunctionDefinition>(ma.annotation().referencedDeclaration)) {
		DeclarationAnnotation const &da = libFunction->annotation();
		if (da.contract->contractKind() == ContractKind::Library) {
			auto t = getType(&ma.expression());
			const int argQty = static_cast<int>(m_arguments.size());
			const int retQty = static_cast<int>(libFunction->returnParameters().size());
			if (t->category() == Type::Category::TypeType) {
				// uint z = MyLib.sum(a, b);
				pushArgs();
				m_pusher.pushCallOrCallRef(
					libFunction,
					std::nullopt,
					false
				);
			} else {
				// using MathLib for uint;
				// a.add(b);
				const int stackSize0 = m_pusher.stackSize();
				const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&ma.expression(), true);
				const int stackSize1 = m_pusher.stackSize();
				const int lValueQty = stackSize1 - stackSize0;

				pushArgs();
				m_pusher.pushCallOrCallRef(
					libFunction,
					std::make_pair(argQty + 1, retQty + 1),
					true
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
	bool data_map_supported,
	const std::function<void()>& pushKey,
	Expression const* vars,
	ContractType const* ct
) {
	auto getDeclAndIndex =
		[](std::vector<std::pair<VariableDeclaration const*, int>> staticVars, const std::string& name)
	{
		auto pos = find_if(staticVars.begin(), staticVars.end(), [&](auto v) {
			return v.first->name() == name;
		});
		solAssert(pos != staticVars.end(), "");
		return *pos;
	};

	if (data_map_supported) {
		return [pushKey, this, vars, ct, getDeclAndIndex]() {
			// creat dict with variable values
			m_pusher << "NEWDICT";
			// stack: builder dict
			IntegerType keyType = getKeyTypeOfC4();
			Type const* valueType = TypeProvider::uint256();

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
				auto initVars = to<InitializerList>(vars);
				for (size_t i = 0; i < initVars->names().size(); ++i) {
					const ASTPointer<ASTString> &name = initVars->names().at(i);
					const auto &[varDecl, varIndex] = getDeclAndIndex(staticVars, *name);
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
			m_pusher << "NEWC";
			m_pusher << "STDICT";
			m_pusher << "ENDC";
		};
	} else {
		return [pushKey, this, vars, ct, getDeclAndIndex]() {
			std::vector<PragmaDirective const *> _pragmaDirectives;
			PragmaDirectiveHelper pragmaHelper{_pragmaDirectives};
			solAssert(ct, "ct == null");
			TVMCompilerContext ctx{&ct->contractDefinition(), pragmaHelper};

			std::map<VariableDeclaration const*, Expression const*> varValue;
			if (vars != nullptr) {
				std::vector<std::pair<VariableDeclaration const*, int>> staticVars = ctx.getStaticVariables();
				auto initVars = to<InitializerList>(vars);
				for (size_t i = 0; i < initVars->names().size(); ++i) {
					const ASTPointer<ASTString> &name = initVars->names().at(i);
					const auto &[varDecl, varIndex] = getDeclAndIndex(staticVars, *name);
					varValue[varDecl] = initVars->options().at(i).get();
				}
			}

			std::vector<VariableDeclaration const *> stateVars = ctx.c4StateVariables();
			for (VariableDeclaration const* var : stateVars | boost::adaptors::reversed) {
				if (varValue.count(var) == 0)
					m_pusher.pushDefaultValue(var->type());
				else
					pushExprAndConvert(varValue.at(var), var->type());
			}

			if (ctx.storeTimestampInC4())
				m_pusher.pushInt(0);
			pushKey();
			m_pusher << "NEWC";
			m_pusher << "STU 256";
			if (ctx.storeTimestampInC4())
				m_pusher << "STU 64";
			m_pusher << "STZERO"; // constructor flag
			if (ctx.usage().hasAwaitCall())
				m_pusher << "STZERO";
			const std::vector<Type const *>& memberTypes = ctx.c4StateVariableTypes();
			if (!memberTypes.empty()) {
				ChainDataEncoder encoder{&m_pusher};
				DecodePositionAbiV2 position{ctx.getOffsetC4(), ctx.usage().hasAwaitCall() ? 1 : 0, memberTypes};
				encoder.encodeParameters(memberTypes, position);
			}
			m_pusher << "ENDC";
		};
	}
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

	std::vector<VariableDeclaration const*> argDecl = convertArray(functionDefinition->parameters());
	const bool isLib = functionDefinition->annotation().contract->isLibrary();
	if (isLib) {
		argDecl.erase(argDecl.begin(), argDecl.begin() + 1);
	}
	solAssert(m_arguments.size() == argDecl.size(), "");

	appendBody = [&](int builderSize) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			argDecl,
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(
				functionDefinition,
				ReasonOfOutboundMessage::RemoteCallInternal,
				isLib
			),
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
		m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4_for_await");
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
		m_pusher << "EQUAL";
		m_pusher._throw("THROWIFNOT " + to_string(TvmConst::RuntimeException::WrongAwaitFuncId));
		m_pusher.fixStack(+2); // fix stack
		// decode returned vars
		ChainDataDecoder decoder{&m_pusher};
		vector<Type const*> types = getParams(functionDefinition->returnParameters()).first;
		decoder.decodePublicFunctionParameters(types, false, true);
		m_pusher.fixStack(-1); // fix stack
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
	Expression const* abiVer = findOption("abiVer");
	Expression const* flags = findOption("flags");

	auto memberAccess = to<MemberAccess>(&functionOptions->expression());
	FunctionDefinition const* functionDefinition = getRemoteFunctionDefinition(memberAccess);

	Expression const* destination = &memberAccess->expression();

	generateExtInboundMsg(addSignature, destination, pubkey, expire, time, callbackid,
						  onerrorid, stateInit, signBoxHandle, abiVer, flags, functionDefinition, m_arguments);
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
	const Expression *abiVer,
	const Expression *flags,
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

		m_pusher << "NEWC";
		builderSize += TvmConst::Abi::MaxOptionalSignLength;
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
				m_pusher << "ISNULL";
				m_pusher.fixStack(-1);

				m_pusher.startContinuation();
				m_pusher.drop(1);
				m_pusher.stzeroes(1);
				m_pusher.endContinuation();

				m_pusher.startContinuation();
				m_pusher.exchange(1);
				m_pusher.stones(1);
				m_pusher.fixStack(+1); // fix stack
				m_pusher << "STU 256";
				m_pusher.endContinuation();

				m_pusher.ifElse();
				m_pusher.endOpaque(3, 1);
			} else {
				m_pusher << "ISNULL";
				m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::MsgWithKeyButNoSign));
				m_pusher.stzeroes(1);
			}
		}
		// if no pubkey - encode nothing

		if (time != nullptr) {
			builderSize += 64;
			acceptExpr(time);
			m_pusher << "STUR 64";
		}

		if (expire != nullptr) {
			builderSize += 32;
			acceptExpr(expire);
			m_pusher << "STUR 32";
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

		m_pusher << "STBREFR";
	};

	// store dest address
	acceptExpr(destination);


	// generate payload to store it as a src address with addr_extern type
	if (flags != nullptr)
		acceptExpr(flags);
	else
		m_pusher << "PUSHINT 0";
	if (signBoxHandle != nullptr)
		acceptExpr(signBoxHandle);
	if (abiVer == nullptr)
		m_pusher << "PUSHINT " + toString(TvmConst::Message::MajorAbiVersion);
	else
		acceptExprOrPushFunctionId(abiVer);
	acceptExprOrPushFunctionId(onerrorid);
	acceptExprOrPushFunctionId(callbackid);
	m_pusher << "NEWC";
	// stack: flags [signBoxHandle] abiVer onerrorid callbackid builder
	m_pusher << "STSLICECONST x6_"; // header 01
	if (signBoxHandle == nullptr)
		m_pusher << "STSLICECONST x2A4_"; // const length 84 (32 callback + 32 onerror + 8 abiVer + 3 header mask + 1 opt signBox + 8 flags)
	else {
		m_pusher.pushS(4);
		m_pusher.startOpaque();
		m_pusher << "ISNULL";
		m_pusher.startContinuation();
		m_pusher << "STSLICECONST x2A4_"; // const length 84 (32 callback + 32 onerror + 8 abiVer + 3 header mask + 1 opt signBox + 8 flags)
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher << "STSLICECONST x3A4_"; // const length 116 (32 callback + 32 onerror + 8 abiVer + 3 header mask + 33 opt signBox + 8 flags)
		m_pusher.endContinuation();
		m_pusher.ifElse();
		m_pusher.fixStack(-1);
		m_pusher.endOpaque(2, 1);
	}
	// stack: flags [signBoxHandle] abiVer onerrorid callbackid builder
	m_pusher << "STU 32"; // stack: flags [signBoxHandle] abiVer onerrorid builder
	m_pusher << "STU 32"; // stack: flags [signBoxHandle] abiVer builder
	m_pusher << "STU 8";  // stack: flags [signBoxHandle] builder
	if (time != nullptr)
		m_pusher << "STONE";
	else
		m_pusher << "STZERO";
	if (expire != nullptr)
		m_pusher << "STONE";
	else
		m_pusher << "STZERO";
	if (pubkey != nullptr)
		m_pusher << "STONE";
	else
		m_pusher << "STZERO";
	if (signBoxHandle == nullptr)
		m_pusher << "STZERO";
	else {
		// stack: flags [signBoxHandle] builder
		m_pusher.pushS(1);
		m_pusher.startOpaque();
		m_pusher << "ISNULL";
		m_pusher.startContinuation();
		m_pusher.dropUnder(1, 1);
		m_pusher.stzeroes(1);
		m_pusher.endContinuation();
		m_pusher.startContinuation();
		m_pusher.stones(1);
		m_pusher.fixStack(+1); // fix stack
		m_pusher << "STU 32";
		m_pusher.endContinuation();
		m_pusher.ifElse();
		m_pusher.fixStack(-1); // fix stack
		m_pusher.endOpaque(3, 1);
	}
	// stack: flags builder
	m_pusher << "STU 8";

	std::function<void()> appendStateInit = nullptr;
	if (stateInit != nullptr)
		appendStateInit = [&]() {
			m_pusher.stones(1);
			acceptExpr(stateInit);
			m_pusher.pushS(0);
			checkStateInit();
			m_pusher << "STREFR";
		};

	m_pusher.prepareMsg({TvmConst::ext_msg_info::src, TvmConst::ext_msg_info::dest}, {}, appendBody, appendStateInit, StackPusher::MsgType::ExternalIn);

	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
}

void FunctionCallCompiler::abiBuildIntMsg() {
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
		m_pusher.convert(functionDefinition->parameters().at(idx - shift)->type(), getType(arg.get()));
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
			m_pusher << "STONE"; // ^StateInit
			pushArgAndConvert(stateInit, "stateInit");
			m_pusher.pushS(0);
			checkStateInit();
			m_pusher << "STREFR";
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

void FunctionCallCompiler::abiBuildDataInit() {
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
	if (contrArg != -1) {
		Type const* type = m_arguments.at(contrArg)->annotation().type;
		auto tt = dynamic_cast<const TypeType*>(type);
		type = tt->actualType();
		ct = to<ContractType>(type);
	}

	bool data_map_supported = m_memberAccess->memberName() == "encodeOldDataInit";
	generateDataSection(
		data_map_supported,
		pushKey,
		varArg != -1 ? m_arguments[varArg].get() : nullptr,
		ct
	)();

	solAssert(m_pusher.stackSize() == stackSize + 1, "");
}

void FunctionCallCompiler::abiBuildExtMsg() {
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
	int abiVerArg = -1;
	int flagsArg = -1;
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
					abiVerArg = arg;
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
				case str2int("flags"):
					flagsArg = arg;
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
										  (abiVerArg != -1) ? m_arguments[abiVerArg].get() : nullptr,
										  (flagsArg != -1) ? m_arguments[flagsArg].get() : nullptr,
										  functionDefinition, funcCall->arguments());
}

bool FunctionCallCompiler::checkTvmABIDeployMethods(Type::Category category) {
	if (category != Type::Category::Magic)
		return false;

	if (m_funcType->kind() == FunctionType::Kind::ABIEncodeStateInit) {
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
				exprs[StateInitMembers::Data] = generateDataSection(false, pushKey,
																	hasVars ? m_arguments[varArg].get() : nullptr,
																	ct);
			}
		}

		encodeStateInit(exprs);
		return true;
	}

	if (m_funcType->kind() == FunctionType::Kind::ABIStateInitHash) {
		pushArgs();
		m_pusher.pushFragmentInCallRef(4, 1, "__stateInitHash");
		return true;
	}

	return false;
}

void FunctionCallCompiler::abiDecodeData() {
	pushArgAndConvert(1);
	decodeData();
}

int FunctionCallCompiler::decodeData() {
	std::vector<Type const*> stateVarTypes;
	auto retTuple = to<TupleType>(m_retType);
	for (Type const* type : retTuple->components()) {
		stateVarTypes.push_back(type);
	}

	// lvalue.. slice
	ChainDataDecoder decoder{&m_pusher};
	decoder.decodeData(0, 0, stateVarTypes);
	// lvalue.. stateVars...
	return stateVarTypes.size();
}

int FunctionCallCompiler::decodeFunctionParams() {
	CallableDeclaration const *functionDefinition = getFunctionDeclarationOrConstructor(
					m_arguments.at(0).get());
	if (functionDefinition) {
		// lvalue.. slice
		auto fd = to<FunctionDefinition>(functionDefinition);
		bool isResponsible = fd->isResponsible();
		if (isResponsible) {
			m_pusher << "LDU 32";
		}
		// lvalue.. callback slice
		ChainDataDecoder decoder{&m_pusher};
		vector<Type const *> types = getParams(functionDefinition->parameters()).first;
		decoder.decodePublicFunctionParameters(types, isResponsible, true);

		return functionDefinition->parameters().size() + (isResponsible ? 1 : 0);
	}

	m_pusher << "ENDS";
	return 0;
}

void FunctionCallCompiler::sliceMethods(MemberAccess const &_node) {
	auto returnTypes = [&](bool fromOptional){
		Type const* type{};
		if (fromOptional) {
			solAssert(to<OptionalType>(m_retType), "");
			type= to<OptionalType>(m_retType)->valueType();
		} else {
			type = m_retType;
		}

		TypePointers returnTypes;
		if (auto const *targetTupleType = to<TupleType>(type))
			returnTypes = targetTupleType->components();
		else
			returnTypes = TypePointers{type};
		return returnTypes;
	};

	const auto& value = m_arguments.empty() ? nullopt : ExprUtils::constValue(*m_arguments[0]);
	ASTString const& memberName = _node.memberName();
	if (memberName == "empty") {
		acceptExpr(&_node.expression());
		m_pusher << "SEMPTY";
	} else if (memberName == "dataSize") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher << "SDATASIZE";
	} else if (memberName == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		cellBitRefQty(false);
	} else if (memberName == "size") {
		acceptExpr(&_node.expression());
		m_pusher << "SBITREFS";
	} else if (memberName == "bits") {
		acceptExpr(&_node.expression());
		m_pusher << "SBITS";
	} else if (memberName == "compare") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher << "SDLEXCMP";
	} else if (memberName == "hasNBits") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher << "SCHKBITSQ";
	} else if (memberName == "hasNRefs") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher << "SCHKREFSQ";
	} else if (memberName == "hasNBitsAndRefs") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		pushArgAndConvert(1);
		m_pusher << "SCHKBITREFSQ";
	} else if (memberName == "refs") {
		acceptExpr(&_node.expression());
		m_pusher << "SREFS";
	} else if (memberName == "depth") {
		acceptExpr(&_node.expression());
		m_pusher << "SDEPTH";
	} else if (memberName == "skip") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		if (m_arguments.size() == 1) {
			pushArgAndConvert(0);
			m_pusher << "SDSKIPFIRST";
		} else {
			pushArgAndConvert(0);
			pushArgAndConvert(1);
			m_pusher << "SSKIPFIRST";
		}
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (isIn(memberName, "loadFunctionParams", "decodeFunctionParams", "loadStateVars", "decodeStateVars")) {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		int paramQty = -1;
		if (isIn(memberName, "loadFunctionParams", "decodeFunctionParams")) {
			paramQty = decodeFunctionParams();
		} else if (isIn(memberName, "loadStateVars", "decodeStateVars")) {
			paramQty = decodeData();
		} else {
			solUnimplemented("");
		}
		if (paramQty != -1) {
			m_pusher.blockSwap(lValueInfo.stackSizeDiff - 1, paramQty);
			m_pusher.pushSlice("x8_");
		}
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (isIn(memberName, "load", "decode", "loadQ", "decodeQ") || boost::starts_with(memberName, "load")) {
		int stackDelta = 0;
		const int stackSize = m_pusher.stackSize();
		bool isLValue = *_node.expression().annotation().isLValue;
		LValueInfo lValueInfo;
		if (isLValue) {
			lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		} else {
			acceptExpr(&_node.expression());
		}
		if (isIn(memberName, "load", "decode")) {
			TypePointers types = returnTypes(false);
			ChainDataDecoder decode{&m_pusher};
			DecodePositionFromOneSlice pos;
			decode.decodeParameters(types, pos);
			stackDelta = types.size();
		} else if (isIn(memberName, "loadQ", "decodeQ")) {
			ChainDataDecoder decode{&m_pusher};
			DecodePositionFromOneSlice pos;
			decode.decodeParametersQ(returnTypes(true), pos);
			stackDelta = 1;
		} else if (boost::starts_with(memberName, "load")) {
			stackDelta = 1;
			std::optional<std::string> opcode;
			if (memberName == "loadRefAsSlice") {
				m_pusher << "LDREFRTOS";
				m_pusher.exchange(1);
			} else if (memberName == "loadRef") {
				opcode = "LDREF";
			} else if (isIn(memberName, "loadUint", "loadUnsigned", "loadInt", "loadSigned")) {
				std::string cmd = "LD";
				cmd += isIn(memberName, "loadInt", "loadSigned") ? "I" : "U";
				if (value.has_value() && value != 0) {
					m_pusher << cmd + " " + value->str();
				} else {
					pushArgAndConvert(0);
					m_pusher << cmd + "X";
				}
			} else if (isIn(memberName, "loadUintQ", "loadIntQ")) {
				std::string cmd = "LD";
				cmd += memberName == "loadIntQ" ? "I" : "U";
				int take{};
				if (value.has_value() && value != 0) {
					cmd += "Q " + value->str();
					take = 1;
				} else {
					pushArgs();
					cmd += "XQ";
					take = 2;
				}
				m_pusher.startOpaque();
				m_pusher.pushAsym(cmd);
				m_pusher.pushAsym(getZeroOrNullAlignment(false, false, true));
				m_pusher.drop();
				m_pusher.endOpaque(take, 2);
			} else if (isIn(memberName, "loadIntLE4Q", "loadIntLE8Q", "loadUintLE4Q", "loadUintLE8Q")) {
				std::string cmd = "LD";
				cmd += boost::starts_with(memberName, "loadIntLE") ? "I" : "U";
				cmd += "LE";
				cmd += boost::ends_with(memberName, "4Q") ? "4" : "8";
				cmd += "Q";
				m_pusher.startOpaque();
				m_pusher.pushAsym(cmd);
				m_pusher.pushAsym(getZeroOrNullAlignment(false, false, true));
				m_pusher.drop();
				m_pusher.endOpaque(1, 2);
			} else if (memberName == "loadTons") {
				opcode = "LDGRAMS";
			} else if (memberName == "loadSlice") {
				const auto& value2 = m_arguments.size() == 2 ? ExprUtils::constValue(*m_arguments[1]) : nullopt;
				if (m_arguments.size() == 1 || (value2.has_value() && value2.value() == 0)) {
					if (value.has_value() && 0 < value && value <= 256) {
						m_pusher << "LDSLICE " + value->str();
					} else {
						pushArgAndConvert(0);
						m_pusher << "LDSLICEX";
					}
				} else {
					pushArgAndConvert(0);
					pushArgAndConvert(1);
					m_pusher << "SPLIT";
				}
			} else if (memberName == "loadSliceQ") {
				string cmd;
				int take{};
				if (m_arguments.size() == 1) {
					if (value.has_value() && 0 < value && value <= 256) {
						take = 1;
						cmd = "LDSLICEQ " + value->str();
					} else {
						take = 2;
						pushArgs();
						cmd = "LDSLICEXQ";
					}
				} else {
					take = 3;
					pushArgs();
					cmd = "SPLITQ";
				}
				m_pusher.startOpaque();
				m_pusher.pushAsym(cmd);
				m_pusher.pushAsym(getZeroOrNullAlignment(false, false, true));
				m_pusher.drop();
				m_pusher.endOpaque(take, 2);
			} else if (memberName == "loadOnes") {
				opcode = "LDONES";
			} else if (memberName == "loadZeroes") {
				opcode = "LDZEROES";
			} else if (memberName == "loadSame") {
				opcode = "LDSAME";
			} else if (memberName == "loadIntLE2") {
				m_pusher << "LDU 8"
						 << "LDU 8";
				m_pusher.blockSwap(2, 1);
				m_pusher << "LSHIFT 8"
						 << "ADD";
				m_pusher.pushS(0); // s v v
				m_pusher.pushInt((1<<15) - 1); // s v v 32767
				m_pusher << "GREATER"; // s v v>32767
				m_pusher.fixStack(-1); // fix stack
				m_pusher.startContinuation();
				// s v
				m_pusher.pushInt(1 << 16);
				m_pusher << "SUB";
				m_pusher.endContinuation();
				m_pusher._if();
				m_pusher.blockSwap(1, 1);
			} else if (memberName == "loadIntLE4") {
				opcode = "LDILE4";
			} else if (memberName == "loadIntLE8") {
				opcode = "LDILE8";
			} else if (memberName == "loadUintLE2") {
				m_pusher << "LDU 8"
						 << "LDU 8";
				m_pusher.blockSwap(2, 1);
				m_pusher << "LSHIFT 8"
						 << "ADD";
				m_pusher.blockSwap(1, 1);
			} else if (memberName == "loadUintLE4") {
				opcode = "LDULE4";
			} else if (memberName == "loadUintLE8") {
				opcode = "LDULE8";
			} else {
				solUnimplemented("");
			}

			if (opcode.has_value()) {
				pushArgs();
				m_pusher << opcode.value();
			}

		} else {
			solUnimplemented("");
		}
		if (isLValue) {
			// lvalue... decodedValues... slice
			m_pusher.blockSwap(stackDelta, 1);
			// lvalue... slice decodedValues...
			m_pusher.blockSwap(lValueInfo.stackSizeDiff, stackDelta);
			// decodedValues... lvalue... slice
			m_exprCompiler.collectLValue(lValueInfo, true, false);
		} else {
			// decodedValues... slice
			m_pusher.drop();
			// decodedValues...
		}
		solAssert(stackSize + stackDelta == m_pusher.stackSize(), "");
	} else if (memberName == "preload") {
		acceptExpr(&_node.expression());

		ChainDataDecoder decode{&m_pusher};
		DecodePositionFromOneSlice pos;
		decode.decodeParameters(returnTypes(false), pos);
		m_pusher.drop();
	} else if (memberName == "preloadQ") {
		acceptExpr(&_node.expression());

		ChainDataDecoder decode{&m_pusher};
		DecodePositionFromOneSlice pos;
		decode.decodeParametersQ(returnTypes(true), pos);
		m_pusher.drop();
	} else if (memberName == "preloadRef") {
		acceptExpr(&_node.expression());
		if (m_arguments.empty()) {
			m_pusher << "PLDREF";
		} else {
			if (value.has_value()) {
				m_pusher << "PLDREFIDX " + toString(value.value());
			} else {
				pushArgs();
				m_pusher << "PLDREFVAR";
			}
		}
	} else if (isIn(memberName, "preloadInt", "preloadUint")) {
		acceptExpr(&_node.expression());
		string cmd = string{} + "PLD" + (memberName == "preloadInt" ? "I" : "U");
		if (value.has_value() && value != 0) {
			cmd += " " + value->str();
		} else {
			pushArgs();
			cmd += "X";
		}
		m_pusher << cmd;
	} else if (isIn(memberName, "preloadIntLE4", "preloadIntLE8", "preloadUintLE4", "preloadUintLE8")) {
		acceptExpr(&_node.expression());
		string cmd = "PLD";
		cmd += boost::starts_with(memberName, "preloadInt") ? "I" : "U";
		cmd += "LE";
		cmd += boost::ends_with(memberName, "4") ? "4" : "8";
		m_pusher << cmd;
	} else if (isIn(memberName, "preloadIntQ", "preloadUintQ")) {
		acceptExpr(&_node.expression());
		string cmd = string{} + "PLD" + (memberName == "preloadIntQ" ? "I" : "U");
		int take{};
		if (value.has_value() && value != 0) {
			take = 1;
			cmd += "Q " + toString(value.value());
		} else {
			pushArgs();
			take = 2;
			cmd += "XQ";
		}
		m_pusher.startOpaque();
		m_pusher.pushAsym(cmd);
		m_pusher.pushAsym(getZeroOrNullAlignment(false, true, true));
		m_pusher.drop();
		m_pusher.endOpaque(take, 1);
	} else if (isIn(memberName, "preloadIntLE4Q", "preloadIntLE8Q", "preloadUintLE4Q", "preloadUintLE8Q")) {
		acceptExpr(&_node.expression());
		string cmd = "PLD";
		cmd += boost::starts_with(memberName, "preloadInt") ? "I" : "U";
		cmd += "LE";
		cmd += boost::ends_with(memberName, "4Q") ? "4" : "8";
		cmd += "Q";
		m_pusher.startOpaque();
		m_pusher.pushAsym(cmd);
		m_pusher.pushAsym(getZeroOrNullAlignment(false, true, true));
		m_pusher.drop();
		m_pusher.endOpaque(1, 1);
	} else if (memberName == "preloadSlice") {
		acceptExpr(&_node.expression());
		if (m_arguments.size() == 1) {
			if (value.has_value() && 0 < value && value <= 256) {
				m_pusher << "PLDSLICE " + value->str();
			} else {
				pushArgs();
				m_pusher << "PLDSLICEX";
			}
		} else {
			pushArgs();
			m_pusher << "SCUTFIRST";
		}
	} else if (memberName == "preloadSliceQ") {
		acceptExpr(&_node.expression());
		string cmd;
		int take{};
		int ret{};
		if (m_arguments.size() == 1) {
			if (value.has_value() && 0 < value && value <= 256) {
				cmd += "PLDSLICEQ " + value->str();
				take = 1;
				ret = 1;
			} else {
				pushArgs();
				cmd += "PLDSLICEXQ";
				take = 2;
				ret = 1;
			}
		} else {
			pushArgs();
			cmd += "SPLITQ";
			take = 3;
			ret = 2;
		}
		m_pusher.startOpaque();
		m_pusher.pushAsym(cmd);
		m_pusher.pushAsym(getZeroOrNullAlignment(false, true, true));
		m_pusher.drop();
		m_pusher.endOpaque(take, ret);
		if (ret == 2) {
			m_pusher.drop();
		}
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForTvmVectorMethods(MemberAccess const &_node, Type::Category category) {
	if (category != Type::Category::TvmVector)
		return false;

	if (_node.memberName() == "push") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		acceptExpr(m_arguments[0].get());

		//start callref
		m_pusher.startContinuation();

		// vector new_el
		m_pusher.startOpaque();
		m_pusher.exchange(1);
		// new_el vector
		m_pusher.pushS(0);
		m_pusher << "TLEN";
		//new_el vector vector_len

		// if vector is not empty get last tuple
		m_pusher.startContinuation();
		m_pusher << "TPOP";
		// new_el vector' last_tuple
		m_pusher.pushS(0);
		m_pusher << "TLEN";
		m_pusher.pushInt(TvmConst::TvmTupleLen);
		m_pusher << "SUB";

		// if last tuple is full (his length is equal to 255) push it back to vector and create new tuple
		m_pusher.startContinuation();
		m_pusher << "TPUSH";
		m_pusher.makeTuple(0);
		m_pusher.endContinuation();
		m_pusher.ifNot();
		// new_el vector' tuple
		m_pusher.endContinuation();

		// if vector is empty create new tuple
		m_pusher.startContinuation();
		m_pusher.makeTuple(0);
		m_pusher.endContinuation();

		m_pusher.ifElse();

		// new_el vector' tuple
		m_pusher.rot();
		// vector' tuple new_el
		m_pusher << "TPUSH";
		// vector' tuple'
		m_pusher << "TPUSH";
		// vector''
		m_pusher.endOpaque(2, 1);

		// end callref
		m_pusher.pushRefContAndCallX(2, 1, false);

		m_exprCompiler.collectLValue(lValueInfo, true, false);
		return true;
	}

	if (_node.memberName() == "pop") {
		const int stackSize = m_pusher.stackSize();
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		const int stackChange = m_pusher.stackSize() - stackSize;

		//start callref
		m_pusher.startContinuation();

		// vector
		m_pusher.startOpaque();
		m_pusher << "TPOP";
		// vector' last_tuple
		m_pusher << "TPOP";
		// [vector_expand] vector' last_tuple' last_el

		// put last el before vector and it's expand components
		m_pusher.blockSwap(stackChange + 1, 1);
		// last_el [vector_expand] vector' last_tuple'

		// if the last tuple is empty delete it
		m_pusher.pushS(0);
		m_pusher << "TLEN";
		// last_el [vector_expand] vector' last_tuple' last_tuple_len

		// if last_tuple is not empty push it back to the vector
		m_pusher.startContinuation();
		m_pusher << "TPUSH";
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
		m_pusher << "TLEN";
		// vector len
		m_pusher.pushS(0);
		// vector len len

		// if len is not zero count length of full tuples and add length of the last tuple
		m_pusher.startContinuation();
		// vector len
		m_pusher << "DEC";
		// vector len--
		m_pusher.pushInt(TvmConst::TvmTupleLen);
		m_pusher << "MUL";
		// vector full_tuples_len
		m_pusher.exchange(1);
		m_pusher << "LAST";
		// full_tuples_len last_tuple
		m_pusher << "TLEN";
		// full_tuples_len last_tuple_len
		m_pusher << "ADD";
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
		m_pusher << "TLEN";
		m_pusher << "ISZERO";
		return true;
	}

	return false;
}

void FunctionCallCompiler::builderMethods(MemberAccess const &_node) {
	ASTString const& memberName = _node.memberName();
	if (boost::starts_with(memberName, "store")) {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);

		std::optional<std::string> opcode;
		bool doSwap = false;
		if (memberName == "storeOnes") {
			opcode = "STONES";
		} else if (memberName == "storeZeroes") {
			opcode = "STZEROES";
		} else if (memberName == "storeRef") {
			pushArgAndConvert(0);
			Type::Category cat = m_arguments.at(0)->annotation().type->category();
			switch (cat) {
				case Type::Category::TvmBuilder:
					m_pusher << "STBREFR";
					break;
				case Type::Category::TvmCell:
					m_pusher << "STREFR";
					break;
				case Type::Category::TvmSlice:
					m_pusher << "NEWC";
					m_pusher << "STSLICE";
					m_pusher << "STBREFR";
					break;
				default:
					solUnimplemented("");
			}
		} else if (memberName == "store") {
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
		} else if (isIn(memberName, "storeSigned", "storeInt", "storeUnsigned", "storeUint")) {
			std::string cmd = "ST";
			cmd += isIn(memberName, "storeSigned", "storeInt") ? "I" : "U";
			pushArgAndConvert(0);
			const auto& val = ExprUtils::constValue(*m_arguments[1]);
			if (val.has_value()) {
				if (val < 1 || val > 256) {
					// TODO val == 0 -> ok
					cast_error(*m_arguments[1], "The value must be in the range 1 - 256.");
				}
				m_pusher << cmd + "R " + val.value().str();
			} else {
				pushArgAndConvert(1);
				m_pusher << cmd + "XR";
			}
		} else if (memberName == "storeTons") {
			opcode = "STGRAMS";
		} else if (memberName == "storeSame") {
			opcode = "STSAME";
		} else if (memberName == "storeIntLE2") {
			pushArgs();
			m_pusher.pushS(0);
			m_pusher << "ISNEG";
			m_pusher.fixStack(-1); // fix stack
			m_pusher.startContinuation();
			m_pusher.pushInt(1 << 16);
			m_pusher << "ADD";
			m_pusher.endContinuation();
			m_pusher._if();
			m_pusher.pushInt(1 << 8);
			m_pusher << "DIVMOD"; // s a1 a0
			m_pusher.blockSwap(1, 2); // a1 a0 s
			m_pusher << "STU 8"
					 << "STU 8";
		} else if (memberName == "storeIntLE4") {
			opcode = "STILE4";
			doSwap = true;
		} else if (memberName == "storeIntLE8") {
			opcode = "STILE8";
			doSwap = true;
		} else if (memberName == "storeUintLE2") {
			pushArgs();
			m_pusher.pushInt(1 << 8);
			m_pusher << "DIVMOD"; // s a1 a0
			m_pusher.blockSwap(1, 2); // a1 a0 s
			m_pusher << "STU 8"
					 << "STU 8";
		} else if (memberName == "storeUintLE4") {
			opcode = "STULE4";
			doSwap = true;
		} else if (memberName == "storeUintLE8") {
			opcode = "STULE8";
			doSwap = true;
		} else {
			solUnimplemented("");
		}

		if (opcode.has_value()) {
			pushArgs();
			if (doSwap)
				m_pusher.blockSwap(1, 1);
			m_pusher << *opcode;
		}

		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else  if (memberName == "bits") {
		acceptExpr(&_node.expression());
		m_pusher << "BBITS";
	} else if (memberName == "refs") {
		acceptExpr(&_node.expression());
		m_pusher << "BREFS";
	} else if (memberName == "size") {
		acceptExpr(&_node.expression());
		m_pusher << "BBITREFS";
	} else if (memberName == "remBits") {
		acceptExpr(&_node.expression());
		m_pusher << "BREMBITS";
	} else if (memberName == "remRefs") {
		acceptExpr(&_node.expression());
		m_pusher << "BREMREFS";
	} else if (memberName == "remBitsAndRefs") {
		acceptExpr(&_node.expression());
		m_pusher << "BREMBITREFS";
	} else if (memberName == "toCell") {
		acceptExpr(&_node.expression());
		m_pusher << "ENDC";
	} else if (memberName == "toExoticCell") {
		acceptExpr(&_node.expression());
		m_pusher << "TRUE";
		m_pusher << "ENDXC";
	} else if (memberName == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher << "ENDC";
		m_pusher << "CTOS";
	} else if (memberName == "depth") {
		acceptExpr(&_node.expression());
		m_pusher << "BDEPTH";
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::arrayMethods(MemberAccess const &_node) {
	Type const *type = _node.expression().annotation().type;
	if (_node.memberName() == "empty") {
		acceptExpr(&_node.expression());
		if (isUsualArray(type)) {
			m_pusher.indexNoexcep(0);
			m_pusher << "EQINT 0";
		} else {
			m_pusher << "CTOS";
			m_pusher << "SEMPTY";
		}
	} else if (_node.memberName() == "substr") {
		acceptExpr(&_node.expression());
		pushArgs();
		if (m_arguments.size() == 1) {
			m_pusher.pushInt(0xFFFF'FFFF);
		}
		m_pusher << "TRUE";
		m_pusher.pushFragmentInCallRef(4, 1, "__subCell");
	} else if (_node.memberName() == "find") {
		acceptExpr(&_node.expression());
		pushArgs();
		Type::Category cat = m_arguments.at(0)->annotation().type->category();
		if (cat == Type::Category::FixedBytes) {
			m_pusher.pushFragmentInCallRef(2, 1, "__strchr");
		} else {
			m_pusher.pushFragmentInCallRef(2, 1, "__strstr");
		}
	} else if (_node.memberName() == "findLast") {
		acceptExpr(&_node.expression());
		pushArgs();
		m_pusher.pushFragmentInCallRef(2, 1, "__strrchr");
	} else if (_node.memberName() == "toLowerCase") {
		acceptExpr(&_node.expression());
		m_pusher.pushFragmentInCallRef(1, 1, "__toLowerCase");
	} else if (_node.memberName() == "toUpperCase") {
		acceptExpr(&_node.expression());
		m_pusher.pushFragmentInCallRef(1, 1, "__toUpperCase");
	} else if (_node.memberName() == "byteLength") {
		acceptExpr(&_node.expression());
		m_pusher.byteLengthOfCell();
	} else if (_node.memberName() == "dataSize") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		m_pusher << "CDATASIZE";
	} else if (_node.memberName() == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher << "CTOS";
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		cellBitRefQty();
	} else if (_node.memberName() == "push") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
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
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		// arr
		m_pusher << "UNTUPLE 2"; // size dict
		m_pusher.pushS(1); // size dict size
		m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
		m_pusher.exchange(1); // dict size
		m_pusher << "DEC"; // dict newSize
		m_pusher.pushS(0); // dict newSize newSize
		m_pusher.rot(); // newSize newSize dict
		m_pusher.pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
		m_pusher << "DICTUDEL"; // newSize dict ?
		m_pusher.drop(1);  // newSize dict
		m_pusher << "TUPLE 2";  // arr
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (_node.memberName() == "append") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		pushArgAndConvert(0);
		m_pusher.pushFragmentInCallRef(2, 1, "__concatenateStrings");
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForOptionalMethods(MemberAccess const &_node) {
	auto optional = to<OptionalType>(_node.expression().annotation().type);
	if (!optional)
		return false;

	auto retTuple = to<TupleType>(m_retType);
	int retQty = retTuple ? retTuple->components().size() : 1;

	ASTString const &memberName = _node.memberName();
	if (memberName == "hasValue") {
		acceptExpr(&_node.expression());
		m_pusher << "ISNULL";
		m_pusher << "NOT";
	} else  if (memberName == "get") {
		acceptExpr(&_node.expression());
		m_pusher.pushS(0);
		m_pusher.checkOptionalValue();
		if (retTuple) {
			m_pusher.untuple(retTuple->components().size());
		} else if (optValueAsTuple(m_retType)) {
			m_pusher.untuple(1);
		}
	} else if (isIn(memberName, "getOr", "getOrDefault")) {
		int startSize = m_pusher.stackSize();
		acceptExpr(&_node.expression());

		m_pusher.startOpaque();
		m_pusher.pushS(0);
		m_pusher << "ISNULL";

		m_pusher.startContinuation();
		m_pusher.drop();
		if (memberName == "getOr") {
			pushArgs();
		} else if (memberName == "getOrDefault") {
			m_pusher.pushDefaultValue(optional->valueType());
		} else {
			solUnimplemented("");
		}
		m_pusher.endContinuation();

		m_pusher.startContinuation();
		if (retTuple) {
			m_pusher.untuple(retTuple->components().size());
		} else if (optValueAsTuple(m_retType)) {
			m_pusher.untuple(1);
		}
		m_pusher.endContinuation();

		m_pusher.ifElse();
		m_pusher.endOpaque(1, retQty);

		solAssert(startSize + retQty == m_pusher.stackSize(), "");
	} else if (memberName == "set") {
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
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false);
		pushArgs(false, false);
		m_pusher.convert(getType(&_node.expression()), rightType);
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else if (memberName == "reset") {
		const LValueInfo lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false);
		m_pusher.pushDefaultValue(optional);
		m_exprCompiler.collectLValue(lValueInfo, true, false);
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::cellMethods(MemberAccess const &_node) {
	ASTString const& memberName = _node.memberName();
	acceptExpr(&_node.expression());
	if (memberName == "toSlice")
		m_pusher << "CTOS";
	else if (memberName == "exoticToSlice")
		m_pusher << "XCTOS";
	else if (memberName == "loadExoticCell")
		m_pusher << "XLOAD";
	else if (memberName == "loadExoticCellQ")
		m_pusher << "XLOADQ";
	else if (memberName == "depth")
		m_pusher << "CDEPTH";
	else if (memberName == "dataSize") {
		pushArgAndConvert(0);
		m_pusher << "CDATASIZE";
	} else if (memberName == "dataSizeQ") {
		pushArgAndConvert(0);
		cellBitRefQty();
	} else
		solUnimplemented("");
}

void FunctionCallCompiler::integerMethods() {
	acceptExpr(&m_memberAccess->expression());
	switch (m_funcType->kind()) {
	case FunctionType::Kind::IntCast: {
		m_pusher.convert(m_retType, m_memberAccess->expression().annotation().type);
		break;
	}
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::variantMethods(MemberAccess const& _node) {

	auto isUint = [&](){
		m_pusher.push(createNode<HardCode>(std::vector<std::string>{
				"PUSHCONT {",
				"	UFITS 256",
				"	TRUE",
				"}",
				"PUSHCONT {",
				"	FALSE",
				"}",
				"TRYARGS 1, 1"
		}, 1, 1, true));
	};

	switch (m_funcType->kind()) {
		case FunctionType::Kind::VariantToUint: {
			acceptExpr(&_node.expression());
			m_pusher.pushS(0);
			isUint();
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::BadVariant));
			break;
		}
		case FunctionType::Kind::VariantIsUint: {
			acceptExpr(&_node.expression());
			isUint();
			break;
		}
		default:
			solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethod() {
	if (m_memberAccess->memberName() == "transfer") { // addr.transfer(...)
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
				m_pusher << "STONE"; // ^StateInit
				acceptExpr(expr);
				m_pusher.pushS(0);
				checkStateInit();
				m_pusher << "STREFR";
			};
		};

		exprs[TvmConst::int_msg_info::dest] = &m_memberAccess->expression();

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
							m_pusher << "STREFR";
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
					m_pusher << "STREFR";
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
	} else if (m_memberAccess->memberName() == "isStdZero") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.pushZeroAddress();
		m_pusher << "SDEQ";
	} else if (m_memberAccess->memberName() == "isExternZero") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.pushSlice("x401_");
		m_pusher << "SDEQ";
	} else if (m_memberAccess->memberName() == "isNone") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.pushSlice("x2_");
		m_pusher << "SDEQ";
	} else if (m_memberAccess->memberName() == "unpack") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "REWRITESTDADDR";
	} else if (m_memberAccess->memberName() == "getType") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "PLDU 2";
	} else if (m_memberAccess->memberName() == "isStdAddrWithoutAnyCast") {
		acceptExpr(&m_memberAccess->expression());
		// t = (2, u, x, s); check t[0] == 2 and t[1] is null
		m_pusher << "PARSEMSGADDR";
		m_pusher.pushS(0);
		m_pusher << "INDEX_NOEXCEP 0";
		m_pusher << "EQINT 2"; // t tag==2

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
		m_pusher << "CONFIGOPTPARAM";
		solAssert(stackSize + 1 == m_pusher.stackSize(), "");
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
			m_pusher << "CTOS";
			m_pusher << "LDU 256";
			m_pusher << "ENDS";
			m_pusher << "TRUE";
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher.pushInt(0);
			m_pusher << "FALSE";
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
			m_pusher << "CTOS";
			m_pusher << "LDU 32";
			m_pusher << "LDU 32";
			m_pusher << "LDU 32";
			m_pusher << "LDU 32";
			m_pusher << "ENDS";
			m_pusher << "TRUE";
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
			m_pusher << "CTOS";
			m_pusher << "LDGRAMS";
			m_pusher << "LDGRAMS";
			m_pusher << "LDGRAMS";
			m_pusher << "LDU 32";
			m_pusher << "ENDS";
			m_pusher << "TRUE";
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
			m_pusher << "CTOS";
			m_pusher << "LDU 8"; // constructor
			m_pusher << "LDU 32"; // utime_since
			m_pusher << "LDU 32"; // utime_until
			m_pusher << "LDU 16"; // total
			m_pusher << "LDU 16"; // main
			m_pusher << "LDU 64"; // total_weight
			m_pusher << "LDDICT"; // ValidatorDescr
			m_pusher << "ENDS";
			m_pusher << "TRUE";
			m_pusher.endContinuation();

			m_pusher.startContinuation();
			m_pusher << "PUSHINT 0"; // constructor
			m_pusher << "PUSHINT 0"; // utime_since
			m_pusher << "PUSHINT 0"; // utime_until
			m_pusher << "PUSHINT 0"; // total
			m_pusher << "PUSHINT 0"; // main
			m_pusher << "PUSHINT 0"; // total_weight
			m_pusher << "NEWDICT"; // ValidatorDescr
			m_pusher << "FALSE"; //
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
		m_pusher << "SENDRAWMSG";
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
		m_pusher << "ISNULL";
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
	switch (m_funcType->kind()) {
		case FunctionType::Kind::RndNext:
			pushArgs();
			if (m_arguments.empty()) {
				m_pusher << "RANDU256";
			} else {
				m_pusher << "RAND";
			}
			break;
		case FunctionType::Kind::RndSetSeed:
		{
			pushArgAndConvert(0);
			m_pusher << "SETRAND";
			break;
		}
		case FunctionType::Kind::RndGetSeed:
		{
			m_pusher << "RANDSEED";
			break;
		}
		case FunctionType::Kind::RndShuffle:
		{
			if (m_arguments.empty()) {
				m_pusher << "LTIME";
			} else {
				pushArgs();
			}
			m_pusher << "ADDRAND";
			break;
		}
		default:
			cast_error(_node, "Unsupported function call");
	}
}

void FunctionCallCompiler::goshFunction() {
	auto typeToOpcode = [&]() -> std::string {
		switch (m_funcType->kind()) {
			case FunctionType::Kind::GoshDiff:
				return "DIFF";
			case FunctionType::Kind::GoshApplyPatch:
				return "DIFF_PATCH";
			case FunctionType::Kind::GoshZip:
				return "ZIP";
			case FunctionType::Kind::GoshUnzip:
				return "UNZIP";
			case FunctionType::Kind::GoshZipDiff:
				return "DIFF_ZIP";
			case FunctionType::Kind::GoshApplyZipPatch:
				return "DIFF_PATCH_ZIP";
			case FunctionType::Kind::GoshApplyPatchQ:
				return "DIFF_PATCHQ";
			case FunctionType::Kind::GoshApplyZipPatchQ:
				return "DIFF_PATCH_ZIPQ";
			case FunctionType::Kind::GoshApplyBinPatch:
				return "DIFF_PATCH_BINARY";
			case FunctionType::Kind::GoshApplyBinPatchQ:
				return "DIFF_PATCH_BINARYQ";
			case FunctionType::Kind::GoshApplyZipBinPatch:
				return "DIFF_PATCH_BINARY_ZIP";
			case FunctionType::Kind::GoshApplyZipBinPatchQ:
				return "DIFF_PATCH_BINARY_ZIPQ";
			default:
				solUnimplemented("Unsupported function call");
		}
	};

	pushArgs();
	string opcode = typeToOpcode();
	m_pusher << opcode;
}

void FunctionCallCompiler::codeSalt() {
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
}

void FunctionCallCompiler::setCodeSalt() {
			pushArgAndConvert(0);
		m_pusher << "CTOS"; // sliceCode
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
			STSLICECONST xPrivateOpcode1 ; DICTUGETJMPZ THROW 78
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
}

void FunctionCallCompiler::functionId() {
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
}

void FunctionCallCompiler::abiEncodeBody() {
	CallableDeclaration const* callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
	if (callDef == nullptr) { // if no constructor (default constructor)
		m_pusher << "NEWC";
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
		m_pusher << "NEWC";
		ChainDataEncoder{&m_pusher}.createMsgBody(
			convertArray(parameters),
			ChainDataEncoder{&m_pusher}.calculateFunctionIDWithReason(callDef, ReasonOfOutboundMessage::RemoteCallInternal),
			callbackFunctionId,
			position
		);
	}
	m_pusher << "ENDC";
}

bool FunctionCallCompiler::checkForTvmFunction(const MemberAccess &_node) {
	if (_node.memberName() == "pubkey") { // tvm.pubkey
		m_pusher.getGlob(TvmConst::C7::TvmPubkey);
	} else if (_node.memberName() == "setPubkey") { // tvm.setPubkey
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::TvmPubkey);
	} else if (_node.memberName() == "accept") { // tvm.accept
		m_pusher << "ACCEPT";
	} else if (_node.memberName() == "hash") { // tvm.hash
		pushArgs();
		switch (m_arguments.at(0)->annotation().type->category()) {
			case Type::Category::TvmCell:
			case Type::Category::Array:
			case Type::Category::StringLiteral:
				m_pusher << "HASHCU";
				break;
			case Type::Category::TvmSlice:
				m_pusher << "HASHSU";
				break;
			default:
				solUnimplemented("");
		}
	} else if (_node.memberName() == "checkSign") { // tvm.checkSign
		size_t cnt = m_arguments.size();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmSlice) {
			pushArgs();
			m_pusher << "CHKSIGNS";
		} else {
			pushArgAndConvert(0);
			if (cnt == 4) {
				pushArgAndConvert(2);
				pushArgAndConvert(1);
				m_pusher << "NEWC";
				m_pusher << "STU 256";
				m_pusher << "STU 256";
				m_pusher << "ENDC";
				m_pusher << "CTOS";
			} else {
				pushArgAndConvert(1);
			}
			pushArgAndConvert(cnt - 1);
			m_pusher << "CHKSIGNU";
		}
	} else if (_node.memberName() == "setcode") { // tvm.setcode
		pushArgs();
		m_pusher << "SETCODE";
	} else if (_node.memberName() == "bindump") { // tvm.bindump
		pushArgs();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmCell)
			m_pusher << "CTOS";
		m_pusher << "BINDUMP";
		m_pusher.drop();
	} else if (_node.memberName() == "hexdump") { // tvm.hexdump
		pushArgs();
		if (getType(m_arguments[0].get())->category() == Type::Category::TvmCell)
		m_pusher << "CTOS";
		m_pusher << "HEXDUMP";
		m_pusher.drop();
	} else if (_node.memberName() == "setCurrentCode") { // tvm.setCurrentCode
		const int stackSize = m_pusher.stackSize();
		pushArgs();

		m_pusher << "CTOS";
		m_pusher.pushS(0);
		m_pusher.pushSlice("x" + TvmConst::Selector::RootCodeCell());
		m_pusher << "SDEQ";

		m_pusher.startOpaque();
		m_pusher.startContinuation();
		m_pusher << "PLDREFIDX 1";
		m_pusher << "CTOS";
		m_pusher.endContinuation();
		m_pusher._if();
		m_pusher.endOpaque(2, 1);

		m_pusher << "PLDREF";
		m_pusher << "CTOS";
		m_pusher << "BLESS";
		m_pusher.popC3();
		solAssert(stackSize == m_pusher.stackSize(), "");
	} else if (_node.memberName() == "getData") { // tvm.getData
		m_pusher.pushRoot();
	} else if (_node.memberName() == "setData") { // tvm.setData
		pushArgs();
		m_pusher.popRoot();
	} else if (_node.memberName() == "rawCommit") { // tvm.rawCommit
		m_pusher << "COMMIT";
	} else if (_node.memberName() == "commit") { // tvm.commit
		m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
		m_pusher << "COMMIT";
	} else if (_node.memberName() == "log") { // tvm.log
		compileLog();
	} else if (_node.memberName() == "resetStorage") { //tvm.resetStorage
		m_pusher.resetAllStateVars();
	} else if (_node.memberName() == "functionId") { // tvm.functionId
		functionId();
	} else if (_node.memberName() == "encodeBody") { // tvm.encodeBody
		abiEncodeBody();
	} else if (_node.memberName() == "rawReserve") {
		pushArgs();
		int n = m_arguments.size();
		solAssert(isIn(n, 2, 3), "");
		m_pusher << (n == 2 ? "RAWRESERVE" : "RAWRESERVEX");
	} else if (isIn(_node.memberName(), "exit", "exit1")) {
		m_pusher.was_c4_to_c7_called();
		m_pusher.fixStack(-1); // fix stack

		m_pusher.startContinuation();
		m_pusher.pushFragment(0, 0, "c7_to_c4");
		m_pusher.endContinuationFromRef();
		m_pusher.ifNot();

		if (_node.memberName() == "exit")
			m_pusher._throw("THROW 0");
		else
			m_pusher._throw("THROW 1");
	} else if (_node.memberName() == "code") {
		m_pusher << "MYCODE";
	} else if (_node.memberName() == "codeSalt") {
		codeSalt();
	} else if (_node.memberName() == "setCodeSalt") {
		setCodeSalt();
	} else if (_node.memberName() == "replayProtTime") {
		m_pusher.getGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "setReplayProtTime") {
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::ReplayProtTime);
	} else if (_node.memberName() == "replayProtInterval") {
		m_pusher.pushInt(TvmConst::Message::ReplayProtection::Interval);
	} else if (_node.memberName() == "setGasLimit") {
		pushArgs();
		m_pusher << "SETGASLIMIT";
	} else if (_node.memberName() == "initCodeHash") {
		m_pusher << "INITCODEHASH";
	} else if (_node.memberName() == "buyGas") {
		pushArgs();
		m_pusher << "BUYGAS";
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
				Type const* actualType = argTypeType->actualType();
				types.emplace_back(actualType);
			}
		} else {
			auto const* argTypeType = dynamic_cast<TypeType const*>(m_arguments.at(1)->annotation().type);
			Type const* actualType = argTypeType->actualType();
			types.emplace_back(actualType);
		}

		acceptExpr(m_arguments.at(0).get());
		m_pusher << "CTOS";
		ChainDataDecoder decoder{&m_pusher};
		decoder.decodeData(0, 0, types);
		break;
	}
	case FunctionType::Kind::ABICodeSalt: {
		codeSalt();
		break;
	}
	case FunctionType::Kind::ABISetCodeSalt: {
		setCodeSalt();
		break;
	}
	case FunctionType::Kind::ABIFunctionId: {
		functionId();
		break;
	}
	case FunctionType::Kind::ABIDecodeFunctionParams: {
		pushArgAndConvert(1);
		decodeFunctionParams();
		break;
	}
	default:
		cast_error(m_functionCall, "Not supported");
	}
}

void FunctionCallCompiler::mathFunction(const MemberAccess &_node) {
	auto retTuple = to<TupleType>(m_retType);
	if (_node.memberName() == "max") {
		pushArgs();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher << "MAX";
	} else if (_node.memberName() == "min") {
		pushArgs();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher << "MIN";
	} else if (_node.memberName() == "minmax") {
		pushArgs();
		m_pusher << "MINMAX";
	} else if (isIn(_node.memberName(), "divr", "divc")) {
		pushArgs();
		if (m_retType->category() == Type::Category::FixedPoint) {
			int power = to<FixedPointType>(m_retType)->fractionalDigits();
			m_pusher.pushInt(MathConsts::power10().at(power)); // res 10^n
			m_pusher.exchange(1);
			m_pusher << "MUL" + boost::to_upper_copy<std::string>(_node.memberName());
		} else {
			m_pusher << boost::to_upper_copy<std::string>(_node.memberName());
		}
	} else if (isIn(_node.memberName(), "muldiv", "muldivr", "muldivc")) {
		pushArgs();
		m_pusher << boost::to_upper_copy<std::string>(_node.memberName());
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.checkFit(m_retType);
		}
	} else if (_node.memberName() == "divmod") {
		pushArgs();
		m_pusher << "DIVMOD";
	} else if (_node.memberName() == "muldivmod") {
		pushArgs();
		m_pusher << "MULDIVMOD";
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.exchange(1);
			m_pusher.checkFit(retTuple->components().at(0));
			m_pusher.exchange(1);
		}
	} else if (_node.memberName() == "abs") {
		pushArgs();
		m_pusher << "ABS";
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
			m_pusher << "MODPOW2 " + value->str();
		} else {
			cast_error(m_functionCall, "Second argument must be a constant integer.");
		}
	} else if (_node.memberName() == "sign") {
		pushArgs();
		m_pusher << "SGN";
	} else {
		cast_error(m_functionCall, "Unsupported function call");
	}
}

bool FunctionCallCompiler::checkBaseContractCall(MemberAccess const &_node) {
	auto funDef = to<FunctionDefinition>(_node.annotation().referencedDeclaration);
	if (funDef) {
		// calling base contract method
		pushArgs();
		m_pusher.pushCallOrCallRef(funDef, std::nullopt, true);
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkAddressThis() {
	// compile  "address(this)"
	if (isAddressThis(&m_functionCall)) {
		m_pusher << "MYADDR";
		return true;
	}
	return false;
}

void FunctionCallCompiler::createObject() {
	switch (m_retType->category()) {
	case Type::Category::TvmCell:
	case Type::Category::TvmBuilder:
		m_pusher.pushDefaultValue(m_retType);
		break;
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::typeConversion() {
	solAssert(m_arguments.size() == 1, "");
	Type const* argType = m_arguments[0]->annotation().type;

	if (auto funCall = to<FunctionCall>(m_arguments[0].get())) {
		if (*funCall->annotation().kind == FunctionCallKind::TypeConversion && funCall->arguments().size() == 1) {
			// c(b(a)), e.g. uint8 x; int(uint(x));
			auto a = to<IntegerType>(funCall->arguments().at(0)->annotation().type);
			auto b = to<IntegerType>(funCall->annotation().type);
			auto c = to<IntegerType>(m_retType);
			if (a && b && c) {
				if (!a->isSigned() && !b->isSigned() && c->isSigned() &&
					a->numBits() < b->numBits() && b->numBits() == c->numBits()
				) {
					acceptExpr(funCall->arguments().at(0).get());
					// no conversion
					return;
				}
			}
		}
	}

	auto getDigits = [](Type const* type)-> int {
		if (auto fix = to<FixedPointType>(type))
			return fix->fractionalDigits();
		if (isIn(type->category(), Type::Category::Integer, Type::Category::VarInteger, Type::Category::Enum))
			return 0;
		solUnimplemented("");
	};

	auto adjustDigits = [&]() {
		int const delta = getDigits(m_retType) - getDigits(argType->mobileType());
		if (delta > 0) {
			m_pusher.pushInt(MathConsts::power10().at(delta));
			m_pusher << "MUL";
		}
		if (delta < 0) {
			m_pusher.pushInt(MathConsts::power10().at(-delta));
			m_pusher << "DIV";
		}
	};

	solAssert(m_arguments.size() == 1, "");
	acceptExpr(m_arguments.at(0).get());
	switch (m_retType->category()) {
	case Type::Category::Enum:
	case Type::Category::FixedPoint:
	case Type::Category::Integer:
	case Type::Category::VarInteger:
		if (argType->category() == Type::Category::FixedBytes) {
			// do nothing
		} else {
			adjustDigits();
			if (!argType->isImplicitlyConvertibleTo(*m_retType))
				m_pusher.checkFit(m_retType);
		}
		break;
	case Type::Category::Address:
	case Type::Category::Contract:
	case Type::Category::FixedBytes:
	case Type::Category::Array:
	case Type::Category::TvmSlice:
		m_pusher.convert(m_retType, argType);
		break;
	default:
		solUnimplemented(m_retType->humanReadableName());
	}
}

bool FunctionCallCompiler::checkLocalFunctionOrLibCall(const Identifier *identifier) {
	const string& functionName = identifier->name();
	auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
	if (!functionDefinition)
		return false;
	pushArgs();
	if (functionDefinition->isInline()) {
		int take = m_funcType->parameterTypes().size();
		int ret = m_funcType->returnParameterTypes().size();
		m_pusher.pushInlineFunction(functionName, take, ret);
	} else {
		ContractDefinition const* contractDecl = functionDefinition->annotation().contract;
		if (contractDecl && contractDecl->isLibrary()) {
			m_pusher.pushCallOrCallRef(functionDefinition, std::nullopt, false);
		} else if (functionDefinition->isInlineAssembly()) {
			int take = functionDefinition->parameters().size();
			int ret = functionDefinition->returnParameters().size();
			std::vector<std::string> lines;
			for(ASTPointer<Statement> const& s : functionDefinition->body().statements()) {
				auto assembly = to<FreeInlineAssembly>(s.get());
				for (ASTPointer<Expression> const& line : assembly->lines()) {
					auto str = to<Literal>(line.get());
					lines.push_back(str->value());
				}
			}
			m_pusher.push(createNode<HardCode>(lines, take, ret, false));
		} else {
			m_pusher.pushCallOrCallRef(functionDefinition, std::nullopt, false);
		}
	}
	return true;
}

bool FunctionCallCompiler::checkSolidityUnits() {
	if (m_funcType == nullptr) {
		return false;
	}

	switch (m_funcType->kind()) {
		case FunctionType::Kind::GasToValue: {
			pushArgs();
			if (m_arguments.size() == 1) {
				m_pusher << "GASTOGRAM";
			} else {
				m_pusher.pushFragmentInCallRef(2, 1, "__gasToTon");
			}
			return true;
		}
		case FunctionType::Kind::ValueToGas: {
			pushArgs();
			if (m_arguments.size() == 1) {
				m_pusher << "GRAMTOGAS";
			} else {
				m_pusher.pushFragmentInCallRef(2, 1, "__tonToGas");
			}
			return true;
		}
		case FunctionType::Kind::BitSize: {
			pushArgs();
			m_pusher << "BITSIZE";
			return true;
		}
		case FunctionType::Kind::UBitSize: {
			pushArgs();
			m_pusher << "UBITSIZE";
			return true;
		}

		case FunctionType::Kind::SHA256: { // "sha256"
			pushArgAndConvert(0);
			Type const* arg = m_arguments.at(0)->annotation().type;
			auto arrType = to<ArrayType>(arg);
			if (arrType && arrType->isByteArrayOrString()) {
				m_pusher << "CTOS";
			}
			m_pusher << "SHA256U";
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
					[&]() { m_pusher << "PUSHINT " + toString(TvmConst::SENDRAWMSG::SelfDestruct); },
					false, 0, nullptr);
			return true;
		}

		case FunctionType::Kind::Require: {
			if (m_arguments.size() == 1) {
				pushArgAndConvert(0);
				m_pusher._throw("THROWIFNOT 100");
			} else if (m_arguments.size() == 2 || m_arguments.size() == 3) {
				Type const* type1 = m_arguments.at(1)->annotation().type;
				auto arr = dynamic_cast<ArrayType const*>(type1);
				if (dynamic_cast<StringLiteralType const*>(type1) || (arr && arr->isString())){
					pushArgAndConvert(1);
					pushArgAndConvert(0);
					m_pusher._throw("THROWARGIFNOT 100");
				} else {
					if (m_arguments.size() == 3)
						pushArgAndConvert(2);
					const auto &exceptionCode = ExprUtils::constValue(*m_arguments[1].get());
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
							m_pusher << "MAX";
						}
						pushArgAndConvert(0);
						if (m_arguments.size() == 3)
							m_pusher._throw("THROWARGANYIFNOT");
						else
							m_pusher._throw("THROWANYIFNOT");
					}
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
				const auto &exceptionCode = ExprUtils::constValue(*m_arguments[0].get());
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
						m_pusher << "MAX";
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
			const int stackSize = m_pusher.stackSize();
			auto literal = to<Literal>(m_arguments[0].get());
			std::string formatStr = literal->value();
			size_t pos = 0;
			std::vector<std::pair<std::string, std::string> > substrings;
			while (true) {
				pos = formatStr.find('{', pos);
				size_t close_pos = formatStr.find('}', pos);
				if (pos == string::npos || close_pos == string::npos)
					break;
				if (formatStr[pos + 1] != ':' && close_pos != pos + 1) {
					pos++;
					continue;
				}

				std::string format = formatStr.substr(pos + 1, close_pos - pos - 1);
				if (format[0] == ':') format.erase(0, 1);
				substrings.emplace_back(formatStr.substr(0, pos), format);
				formatStr = formatStr.substr(close_pos + 1);
				pos = 0;
			}
			// stack: Stack(TvmBuilder)
			m_pusher << "NEWC";
			m_pusher << "NULL";
			m_pusher << "TUPLE 2";

			auto pushConstStr = [&](const string& constStr) {
				if (!constStr.empty()) {
					size_t maxSlice = TvmConst::CellBitLength / 8;
					for(size_t i = 0; i  < constStr.length(); i += maxSlice) {
						m_pusher.pushString(constStr.substr(i, min(maxSlice, constStr.length() - i)), true);
						// stack: Stack(TvmBuilder) slice
						m_pusher.pushFragmentInCallRef(2, 1, "__appendSliceToStringBuilder");
					}
					// stack: Stack(TvmBuilder) slice
				}
			};
			for (size_t it = 0; it < substrings.size(); it++) {
				// stack: Stack(TvmBuilder)
				pushConstStr(substrings[it].first);

				Type::Category cat = m_arguments[it + 1]->annotation().type->category();
				Type const *argType = m_arguments[it + 1]->annotation().type;
				if (cat == Type::Category::Integer || cat == Type::Category::RationalNumber) {
					// stack: Stack(TvmBuilder)
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
						if (width < 0 || width > 127)
							cast_error(m_functionCall, "Width should be in range of 0 to 127.");
						acceptExpr(m_arguments[it + 1].get());
						// stack: stack x
						m_pusher.pushInt(width);
						m_pusher << (leadingZeroes ? "TRUE" : "FALSE");
						// stack: stack x width leadingZeroes
						if (isHex) {
							if (isLower)
								m_pusher << "TRUE";
							else
								m_pusher << "FALSE";
							m_pusher.pushFragmentInCallRef(5, 1, "__convertIntToHexString");
						} else {
							m_pusher.pushFragmentInCallRef(4, 1, "__convertIntToString");
						}
					} else {
						acceptExpr(m_arguments[it + 1].get());
						m_pusher.pushInt(9);
						m_pusher.pushInt(MathConsts::power10().at(9));
						m_pusher.pushFragmentInCallRef(4, 1, "__convertFixedPointToString");
					}
				} else if (cat == Type::Category::Address) {
					acceptExpr(m_arguments[it + 1].get());
					m_pusher.pushFragmentInCallRef(2, 1, "__convertAddressToHexString");
				} else if (isStringOrStringLiteralOrBytes(argType)) {
					acceptExpr(m_arguments[it + 1].get());
					m_pusher << "CTOS";
					m_pusher.pushFragmentInCallRef(2, 1, "__appendStringToStringBuilder");
				} else if (cat == Type::Category::FixedPoint) {
					int power = to<FixedPointType>(argType)->fractionalDigits();
					acceptExpr(m_arguments[it + 1].get());
					m_pusher.pushInt(power);
					m_pusher.pushInt(MathConsts::power10().at(power));
					m_pusher.pushFragmentInCallRef(4, 1, "__convertFixedPointToString");
				} else {
					cast_error(*m_arguments[it + 1].get(), "Unsupported argument type");
				}
			}
			pushConstStr(formatStr);

			m_pusher.pushFragmentInCallRef(1, 1, "__makeString");

			solAssert(stackSize + 1 == m_pusher.stackSize(), "");
			return true;
		}
		case FunctionType::Kind::Stoi: {
			pushArgAndConvert(0);
			m_pusher.pushFragmentInCallRef(1, 1, "__stoi");
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
		if (m_memberAccess) {
			auto category = getType(&m_memberAccess->expression())->category();
			if (category == Type::Category::TypeType || isSuper(&m_memberAccess->expression())) {
				// calling of base/super method or typeTypeMethods
				return false;
			}
		}

		if (m_funcType->kind() != FunctionType::Kind::Internal) {
			return false;
		}

		pushArgs();

		// Local variable of functional type
		acceptExpr(expr);
		int returnCnt = m_funcType->returnParameterTypes().size();
		int paramCnt = m_funcType->parameterTypes().size();
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
	const Type* type = newExpr->typeName().annotation().type;

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
			false,
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

		encodeStateInit(stateInitExprs);

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
			pushWid = [this, wid]() {
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
			pushValue = [this, value]() {
				acceptExpr(value);
			};
		}
	}

	std::variant<bool, std::function<void()>> pushBounce = true;
	if (Expression const* bounce = findOption("bounce")) {
		if (std::optional<bool> value = ExprUtils::constBool(*bounce)) {
			pushBounce = value.value();
		} else {
			pushBounce = [this, bounce]() {
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
	m_pusher << "HASHCU"; // stack: stateInit hash

	if (wid.index() == 0) {
		int8_t w = std::get<0>(wid);
		std::string binWID = "100";
		binWID += StrUtils::toBitString(u256(w), 8);
		m_pusher << "NEWC";
		m_pusher << "STSLICECONST x" + StrUtils::binaryStringToSlice(binWID);
	} else {
		std::get<1>(wid)();
		m_pusher << "NEWC";
		m_pusher << "STSLICECONST x9_"; // addr_std$10 anycast:(Maybe Anycast) // 10 0 1 = 9
		m_pusher << "STI 8"; // workchain_id:int8
	}
	m_pusher << "STU 256"; // address:bits256
	bool isDestBuilder = !m_isCurrentResultNeeded;
	if (!isDestBuilder) {
		m_pusher << "ENDC";
		m_pusher << "CTOS";
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
		m_pusher << "STREF";
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
	m_pusher << "CTOS";

	// split_depth:(Maybe (## 5))
	m_pusher << "LDI 1";
	m_pusher.exchange(1);
	m_pusher.fixStack(-1); // fix stack: drop condition
	m_pusher.startContinuation();
	m_pusher << "LDI 5";
	m_pusher.dropUnder(1, 1);
	m_pusher.endContinuation();
	m_pusher._if();

	// special:(Maybe TickTock)
	m_pusher << "LDI 1";
	m_pusher.exchange(1);
	m_pusher.fixStack(-1); // fix stack: drop condition
	m_pusher.startContinuation();
	// tick_tock$_ tick:Bool tock:Bool = TickTock;
	m_pusher << "LDI 2";
	m_pusher.dropUnder(1, 1);
	m_pusher.endContinuation();
	m_pusher._if();

	// code:(Maybe ^Cell) data:(Maybe ^Cell)
	// library:(HashmapE 256 SimpleLib)
	m_pusher << "LDDICT";
	m_pusher << "LDDICT";
	m_pusher << "LDDICT";
	m_pusher << "ENDS";
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
	if (m_retType->category() == Type::Category::Contract) {
		cast_error(m_functionCall, R"(Unsupported contract creating. Use call options: "stateInit", "value", "flag")");
	}

	creatArrayWithDefaultValue();
	return true;
}

void FunctionCallCompiler::creatArrayWithDefaultValue() {
	std::optional<bigint> num = ExprUtils::constValue(*m_arguments.at(0));
	if (num.has_value() && num.value() == 0) {
		auto arrayType = to<ArrayType>(m_retType);
		m_pusher.pushDefaultValue(arrayType);
		return ;
	}

	if (*m_functionCall.annotation().isPure) {
		pushArgs();
		SourceReference sr = SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &m_functionCall.location());
		const std::string computeName = "new_array_line_" +
										toString(sr.position.line) + "_column_" + toString(sr.position.column) + "_ast_id_" +
										toString(m_functionCall.id());
		m_pusher.computeConstCell(computeName);
		m_pusher << "TUPLE 2";
		m_pusher.ctx().addNewArray(computeName, &m_functionCall);
		return ;
	}

	honestArrayCreation(false);
}

void FunctionCallCompiler::honestArrayCreation(bool onlyDict) {
	const int stackSize = m_pusher.stackSize();
	auto arrayType = to<ArrayType>(m_retType);
	IntegerType const& key = getArrayKeyType();
	Type const* arrayBaseType = arrayType->baseType();

	pushArgAndConvert(0); // N
	DataType const& dataType = m_pusher.pushDefaultValueForDict(&key, arrayBaseType); // N value
	m_pusher.pushInt(0);   // N value iter
	m_pusher << "NEWDICT"; // N value iter dict
	m_pusher.pushS(3);     // N value iter dict N

	solAssert(stackSize + 5 == m_pusher.stackSize(), "");
	m_pusher.fixStack(-1); // fix stack: drop replay iterator
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
	if (m_memberAccess->memberName() != "unpack") {
		return false;
	}
	acceptExpr(&m_memberAccess->expression());
	auto structType = to<StructType>(getType(&m_memberAccess->expression()));
	int memberQty = structType->structDefinition().members().size();
	m_pusher.untuple(memberQty);
	return true;
}

void FunctionCallCompiler::encodeStateInit(std::map<StateInitMembers, std::function<void()>> exprs) {
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

	// stack: data code
	// let's store split_depth and special options
	if (exprs.count(StateInitMembers::SplitDepth) > 0) {
		exprs.at(StateInitMembers::SplitDepth)();
		m_pusher << "NEWC";
		m_pusher.stones(1);
		m_pusher << "STU 5";
		m_pusher.stzeroes(1);
	} else {
		m_pusher << "NEWC";
		// stack: data code builder
		m_pusher << "STSLICECONST x2_"; // no split_depth and no special // 0 0
	}

	// stack: data code builder
	m_pusher << "STDICT"; // store code
	m_pusher << "STDICT"; // store data
	m_pusher << "STZERO"; // store library
	m_pusher << "ENDC";
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
			m_pusher.convert(targetType, e->annotation().type);
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

	m_pusher.convert(targetType, arg->annotation().type);
}

void FunctionCallCompiler::pushExprAndConvert(const Expression *expr, Type const* targetType) {
	acceptExpr(expr);
	m_pusher.convert(targetType, expr->annotation().type);
}

void FunctionCallCompiler::acceptExpr(const Expression *expr) {
	m_exprCompiler.compileNewExpr(expr);
}

void FunctionCallCompiler::compileLog()
{
	auto logstr = m_arguments[0].get();
	auto literal = to<Literal>(logstr);
	if (literal && literal->value().size() < 16) {
		std::string hexStr = StrUtils::stringToHex(literal->value());
		m_pusher << "PRINTSTR x" + hexStr;
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
	m_pusher << "TUPLE 3";
	m_pusher.endContinuation();

	m_pusher.startContinuation();
	m_pusher.pushNull();
	m_pusher.endContinuation();

	m_pusher.ifElse();

	m_pusher.endOpaque(2, 1, true);
}
