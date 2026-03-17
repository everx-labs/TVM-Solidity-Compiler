/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
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

#include <format>

#include <boost/algorithm/string.hpp>

#include <liblangutil/SourceReferenceExtractor.h>
#include <libsolidity/ast/TypeProvider.h>

#include <libsolidity/codegen/DictOperations.hpp>
#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMFunctionCall.hpp>
#include <libsolidity/codegen/TVMStructCompiler.hpp>

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;

FunctionCallCompiler::FunctionCallCompiler(
	StackPusher& m_pusher,
	FunctionCall const& _functionCall,
	bool isCurrentResultNeeded
):
	m_pusher{m_pusher},
	m_exprCompiler{m_pusher},
	m_functionCall{_functionCall},
	m_memberAccess{to<MemberAccess>(&m_functionCall.expression())},
	m_arguments{_functionCall.arguments()},
	m_funcType{to<FunctionType>(m_functionCall.expression().annotation().type)},
	m_retType{m_functionCall.annotation().type},
	m_isCurrentResultNeeded{isCurrentResultNeeded},
	m_names{m_functionCall.names()} {
	if (m_funcType != nullptr) {
		std::vector<std::string> const& functionNames = m_funcType->parameterNames();
		std::vector<ASTPointer<ASTString>> const& functionCallNames = m_functionCall.names();
		argQty = std::max(m_arguments.size(), std::max(functionNames.size(), functionCallNames.size()));
		m_declarationIndex = std::vector<size_t>(argQty, INDEX_INF); // argOrder[callIndex] = declarationIndex;

		if (functionCallNames.empty()) {
			// ff(a, b);
			for (size_t i = 0; i < argQty; ++i) {
				m_declarationIndex[i] = i;
			}
		} else {
			// ff({b: b, a: a});
			// solAssert(functionCallNames.size() == argQty, "");
			std::map<std::string, int> nameToIndex;
			for (size_t i = 0; i < functionCallNames.size(); ++i) {
				nameToIndex[*functionCallNames[i]] = i;
			}
			for (size_t i = 0; i < functionNames.size(); ++i) {
				std::string functionName = functionNames[i];
				if (nameToIndex.contains(functionName))
					m_declarationIndex[nameToIndex.at(functionName)] = i;
			}
		}

		m_callIndex = std::vector<size_t>(argQty, INDEX_INF);
		for (size_t callIndex = 0; callIndex < argQty; ++callIndex) {
			if (m_declarationIndex[callIndex] != INDEX_INF) {
				m_callIndex[m_declarationIndex[callIndex]] = callIndex;
			}
		}
	}
}

void FunctionCallCompiler::structConstructorCall() const {
	auto const& type = dynamic_cast<TypeType const&>(*m_functionCall.expression().annotation().type);
	auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
	auto pushParam = [&](int index, Type const* targetType) {
		pushExprAndConvert(m_arguments.at(index).get(), targetType);
	};
	StructCompiler structCompiler{&m_pusher, &structType};
	structCompiler.structConstructor(m_names, pushParam);
}

void FunctionCallCompiler::compile() {
	auto reportError = [&] { cast_error(m_functionCall, "Unsupported function call"); };

	if (m_funcType) {
		switch (m_funcType->kind()) {
		case FunctionType::Kind::GasLeft: {
			m_pusher << "GASREMAINING";
			return;
		}
		case FunctionType::Kind::GasConsumed: {
			m_pusher << "GASCONSUMED";
			return;
		}
		default:
			break;
		}
	}

	if (checkRemoteMethodCall(m_functionCall) ||
		(m_memberAccess != nullptr && libraryCall()) ||
		checkForMappingOrCurrenciesMethods() ||
		checkNewExpression() ||
		checkAddressThis() ||
		checkHashFunctions() ||
		checkSolidityUnits() ||
		checkLocalFunctionOrLibCallOrFuncVarCall()) {
		// do nothing
	} else if (
		m_memberAccess != nullptr && getType(&m_memberAccess->expression())->category() == Type::Category::Struct
	) {
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
			} else if (category == Type::Category::QInteger || category == Type::Category::QBool) {
				qIntOrBoolMethods();
			} else if (category == Type::Category::StringBuilder) {
				stringBuilderMethods();
			} else if (category == Type::Category::TvmVector) {
				tvmVectorMethods();
			} else if (category == Type::Category::TvmStack) {
				tvmStackMethods();
			} else if (checkForOptionalMethods(*m_memberAccess)) {
				// nothing
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "tvm") {
				if (m_funcType->kind() == FunctionType::Kind::ABIEncodeIntMsg) {
					abiBuildIntMsg();
				} else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeData) {
					abiBuildDataInit();
				} else if (
					checkForTvmSendFunction(*m_memberAccess) ||
					checkForTvmFunction(*m_memberAccess) ||
					checkForTvmC4(*m_memberAccess) ||
					checkTvmABIDeployMethods(category)
				) {
					// do nothing
				} else {
					reportError();
				}
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "config") {
				configFunction(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "secp256k1") {
				secp256k1Function(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "secp256r1") {
				secp256r1Function(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "bls") {
				blsFunction();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "rist255") {
				rist255Function();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "rnd") {
				rndFunction(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "msg") {
				msgFunction(*m_memberAccess);
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "abi") {
				if (m_funcType->kind() == FunctionType::Kind::ABIDecodeData)
					abiDecodeData();
				else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeBody)
					abiEncodeBody();
				else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeIntMsg)
					abiBuildIntMsg();
				else if (m_funcType->kind() == FunctionType::Kind::ABIEncodeData)
					abiBuildDataInit();
				else if (!checkTvmABIDeployMethods(category))
					abiFunction();
			} else if (category == Type::Category::Magic && ident != nullptr && ident->name() == "math") {
				mathFunction(*m_memberAccess);
			} else if (isIn(category, Type::Category::Address, Type::Category::AddressStd)) {
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
				} else if (to<AddressType>(actualType) || to<AddressStdType>(actualType)) {
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
	pusher.exchange(1);											 // value' arr
	pusher << "UNTUPLE 2";										 // value' size dict
	pusher.pushS(1);											 // value' size dict size
	pusher << "INC";											 // value' size dict newSize
	pusher.blockSwap(3, 1);										 // newSize value' size dict
	pusher.setDict(getArrayKeyType(), *arrayBaseType, dataType); // newSize dict'
	pusher << "TUPLE 2";										 // arr
}

bool FunctionCallCompiler::checkForMappingOrCurrenciesMethods() const {
	if (m_memberAccess == nullptr || !to<MappingType>(m_memberAccess->expression().annotation().type))
		return false;

	ASTString const& memberName = m_memberAccess->memberName();
	if (isIn(memberName, "delMin", "delMax")) {
		mappingDelMinOrMax(memberName == std::string{"delMin"});
	} else if (
		isIn(memberName, "at", "fetch", "exists", "replace", "add", "getSet", "getAdd", "getDel", "getReplace")
	) {
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

void FunctionCallCompiler::mappingDelMinOrMax(bool isDelMin) const {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	DelMinOrMax d{m_pusher, *keyType, *valueType, isDelMin, m_memberAccess};
	d.delMinOrMax();
}

void FunctionCallCompiler::mappingGetSet() const {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	ASTString const& memberName = m_memberAccess->memberName();
	if (isIn(memberName, "fetch", "at")) {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType);
		acceptExpr(&m_memberAccess->expression()); // index dict
		if (memberName == "fetch")
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::Fetch);
		else
			m_pusher.getDict(*keyType, *valueType, GetDictOperation::GetFromArray);
	} else if (memberName == "exists") {
		pushArgs(); // index
		m_pusher.prepareKeyForDictOperations(keyType);
		acceptExpr(&m_memberAccess->expression()); // index dict
		m_pusher.getDict(*keyType, *valueType, GetDictOperation::Exist);
	} else if (isIn(memberName, "getDel")) {
		int const stackSize = m_pusher.stackSize();
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);

		pushArgAndConvert(0); // lValue... map key
		m_pusher.prepareKeyForDictOperations(keyType);
		m_pusher.blockSwap(1, 1); // lValue... key map

		m_pusher.getDict(*keyType, *valueType, GetDictOperation::GetDelFromMapping); // lValue... map' value

		int const cntOfValuesOnStack = m_pusher.stackSize() - stackSize;
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1);	// value lValue... map'
		m_exprCompiler.collectLValue(lValueInfo, true); // value
	} else if (isIn(memberName, "replace", "add", "getSet", "getAdd", "getReplace")) {
		int const stackSize = m_pusher.stackSize();
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true); // lValue... map
		pushArgAndConvert(1);																   // lValue... map value
		DataType const& dataType = m_pusher.prepareValueForDictOperations(keyType, valueType); // lValue... map value'
		pushArgAndConvert(0); // mapLValue... map value key
		m_pusher.prepareKeyForDictOperations(keyType);
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
		int const cntOfValuesOnStack = m_pusher.stackSize() - stackSize; // mapLValue... map optValue
		m_pusher.blockSwap(cntOfValuesOnStack - 1, 1);					 // optValue mapLValue... map
		m_exprCompiler.collectLValue(lValueInfo, true);					 // optValue
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::mappingMinMaxMethod(bool isMin) const {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	acceptExpr(&m_memberAccess->expression()); // dict

	DictMinMax compiler{m_pusher, *keyType, *valueType, isMin};
	compiler.minOrMax();
}

void FunctionCallCompiler::mappingPrevNextMethods() const {
	Type const* keyType{};
	Type const* valueType{};
	std::tie(keyType, valueType) = dictKeyValue(m_memberAccess->expression().annotation().type);

	pushArgAndConvert(0);						   // index
	m_pusher.prepareKeyForDictOperations(keyType); // index'
	acceptExpr(&m_memberAccess->expression());	   // index' dict
	m_pusher.pushInt(dictKeyLength(keyType));	   // index' dict nbits

	DictPrevNext compiler{m_pusher, *keyType, *valueType, m_memberAccess->memberName()};
	compiler.prevNext();
}

void FunctionCallCompiler::mappingKeysOrValues(bool areKeys) const {
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

void FunctionCallCompiler::mappingEmpty() const {
	acceptExpr(&m_memberAccess->expression());
	m_pusher << "DICTEMPTY";
}

void FunctionCallCompiler::superFunctionCall(MemberAccess const& _node) const {
	pushArgs();
	auto someFunDecl = to<FunctionDefinition>(_node.annotation().referencedDeclaration);
	FunctionDefinition const* superFunc = getSuperFunction(
		m_pusher.ctx().currentFunction()->annotation().contract,
		m_pusher.ctx().getContract(),
		someFunDecl->externalIdentifierHex()
	);
	solAssert(superFunc, "");
	std::string functionName = m_pusher.ctx().functionInternalName(superFunc, true).first;
	m_pusher.pushCallOrCallRef(superFunc, std::nullopt, true);
}

void FunctionCallCompiler::userDefinedValueMethods(MemberAccess const& _memberAccess) const {
	if (isIn(_memberAccess.memberName(), "wrap", "unwrap")) {
		pushArgs();
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethods(MemberAccess const& _node) const {
	if (_node.memberName() == "makeAddrExtern") {
		// addr_extern$01 len:(## 9) external_address:(bits len) = MsgAddressExt;
		auto const& num = ExprUtils::constValue(*m_arguments.at(0));
		auto const& len = ExprUtils::constValue(*m_arguments.at(1));
		if (num.has_value() && len.has_value()) {
			std::string addr = "01";
			addr += StrUtils::toBitString(len.value(), 9, false).value();
			addr += StrUtils::toBitString(num.value(), static_cast<int>(len.value()), false).value();
			m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(addr));
		} else {
			pushArgs();
			m_pusher.pushS(0);	// numb cntBit cntBit
			m_pusher << "NEWC"; // numb cntBit cntBit builder
			m_pusher << "STSLICECONST x6_";
			m_pusher << "STU 9";  // numb cntBit builder''
			m_pusher.exchange(1); // numb builder'' cntBit
			m_pusher << "STUX";	  // builder'''
			m_pusher << "BTOS";	  // extAddress
		}
	} else if (_node.memberName() == "makeAddrStd") {
		auto const& wid = ExprUtils::constValue(*m_arguments.at(0));
		auto const& val = ExprUtils::constValue(*m_arguments.at(1));
		if (wid.has_value() && val.has_value()) {
			// TODO delete this
			std::string addr = "100";
			addr += StrUtils::toBitString(wid.value(), 8, true).value();
			addr += StrUtils::toBitString(val.value(), 256, false).value();
			m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(addr));
		} else {
			pushArgs(true);
			m_pusher << "NEWC";
			m_pusher << "STSLICECONST x9_";
			m_pusher << "STI 8";
			m_pusher << "STU 256";
			m_pusher << "BTOS";
		}
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::libraryCall() const {
	auto funcViaObject = [this](FunctionDefinition const* function) {
		int const argQty = static_cast<int>(m_arguments.size());
		int const retQty = static_cast<int>(function->returnParameters().size());
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		// lValue.. arg0
		pushArgs(); // lValue.. arg0 arg1 arg2 ...
		m_pusher.pushCallOrCallRef(function, std::make_pair(argQty + 1, retQty + 1), true);
		// lValue.. arg0 ret0 ret1 ...
		m_pusher.blockSwap(lValueInfo.stackSizeDiff, retQty);
		// ret0 ret1 ... lValue.. arg0
		m_exprCompiler.collectLValue(lValueInfo, true);
	};

	if (auto function = to<FunctionDefinition>(m_memberAccess->annotation().referencedDeclaration)) {
		DeclarationAnnotation const& da = function->annotation();
		if (da.contract == nullptr) {
			// using {add} for uint; // free function
			// a.add(b);
			funcViaObject(function);
			return true;
		} else if (da.contract->contractKind() == ContractKind::Library) {
			auto t = getType(&m_memberAccess->expression());
			if (t->category() == Type::Category::TypeType) {
				// uint z = MyLib.sum(a, b);
				pushArgs();
				m_pusher.pushCallOrCallRef(function, std::nullopt, false);
			} else
				// using MathLib for uint; // library
				// a.add(b);
				funcViaObject(function);
			return true;
		}
	}
	return false;
}

std::function<void()> FunctionCallCompiler::generateDataSection(
	bool data_map_supported,
	std::function<void()> const& pushKey,
	Expression const* vars,
	ContractType const* ct
) const {
	auto getDeclAndIndex = [](std::vector<std::pair<VariableDeclaration const*, int>> staticVars,
							  std::string const& name) {
		auto pos = std::ranges::find_if(staticVars, [&](auto v) { return v.first->name() == name; });
		solAssert(pos != staticVars.end(), "");
		return *pos;
	};

	if (data_map_supported) {
		return [pushKey, this, vars, ct, getDeclAndIndex] {
			// creat dict with variable values
			m_pusher << "NULL";
			// stack: builder dict
			IntegerType keyType = getKeyTypeOfC4();
			Type const* valueType = TypeProvider::uint256();

			pushKey();
			DataType const& dataType = m_pusher.prepareValueForDictOperations(&keyType, valueType);
			m_pusher.pushInt(0); // index of pubkey
			// stack: dict value key
			m_pusher.rot();
			// stack: value key dict
			m_pusher.setDict(getKeyTypeOfC4(), *valueType, dataType);
			// stack: dict'
			if (vars) {
				std::vector<PragmaDirective const*> _pragmaDirectives;
				PragmaDirectiveHelper pragmaHelper{_pragmaDirectives};
				TVMCompilerContext cc{&ct->contractDefinition(), pragmaHelper};
				std::vector<std::pair<VariableDeclaration const*, int>> staticVars =
					cc.storageLayout().getStaticVariables();
				auto initVars = to<InitializerList>(vars);
				for (size_t i = 0; i < initVars->names().size(); ++i) {
					ASTPointer<ASTString> const& name = initVars->names().at(i);
					auto const& [varDecl, varIndex] = getDeclAndIndex(staticVars, *name);
					valueType = varDecl->type();
					pushExprAndConvert(initVars->options().at(i).get(), valueType); // stack: dict value
					DataType const& dataType2 = m_pusher.prepareValueForDictOperations(&keyType, valueType);
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
		return [pushKey, this, vars, ct, getDeclAndIndex] {
			StorageLayout const storageLayout{&ct->contractDefinition()};

			std::map<VariableDeclaration const*, Expression const*> varValue;
			if (vars != nullptr) {
				std::vector<std::pair<VariableDeclaration const*, int>> staticVars = storageLayout.getStaticVariables();
				auto initVars = to<InitializerList>(vars);
				for (size_t i = 0; i < initVars->names().size(); ++i) {
					ASTPointer<ASTString> const& name = initVars->names().at(i);
					auto const& [varDecl, _] = getDeclAndIndex(staticVars, *name);
					varValue[varDecl] = initVars->options().at(i).get();
				}
			}

			std::vector<VariableDeclaration const*> stateVars = storageLayout.usualAndUnpackedStateVariables();
			for (VariableDeclaration const* var: stateVars | std::views::reverse) {
				if (!varValue.contains(var))
					m_pusher.pushDefaultValue(var->type());
				else
					pushExprAndConvert(varValue.at(var), var->type());
			}

			if (storageLayout.storeTimestampInC4())
				m_pusher.pushInt(0);
			if (storageLayout.storePubkeyInC4())
				pushKey();
			m_pusher << "NEWC";
			if (storageLayout.storePubkeyInC4())
				m_pusher << "STU 256";
			if (storageLayout.storeTimestampInC4())
				m_pusher << "STU 64";
			if (storageLayout.hasConstructor())
				m_pusher << "STSLICECONST 0"; // constructor flag
			std::vector<Type const*> const memberTypes = getTypesFromVarDecls(stateVars);
			if (!memberTypes.empty()) {
				ChainDataEncoder encoder{&m_pusher};
				AbiV2Position position{storageLayout.getOffsetC4(), 0, memberTypes};
				encoder.encodeParameters(memberTypes, position, false);
			}
			m_pusher << "ENDC";
		};
	}
}

bool FunctionCallCompiler::checkRemoteMethodCall(FunctionCall const& _functionCall) const {
	std::map<int, Expression const*> exprs;
	std::map<int, std::string> constParams;
	std::function<void()> pushSendRawMsgFlag;
	FunctionDefinition const* functionDefinition{};
	std::optional<uint32_t> callbackFunctionId;
	std::function<std::pair<int, int>()> appendEitherStateInit;
	std::function<void()> pushValue;
	std::function<void()> pushExtraFlags;

	auto functionOptions = to<FunctionCallOptions>(&_functionCall.expression());
	if (functionOptions == nullptr) {
		Expression const* currentExpression = &_functionCall.expression();

		auto memberAccess = to<MemberAccess>(currentExpression);
		if (memberAccess == nullptr) {
			return false;
		}

		functionDefinition = getRemoteFunctionDefinition(memberAccess);
		if (functionDefinition == nullptr) {
			return false;
		}

		auto contract = functionDefinition->annotation().contract;
		if (contract && contract->isContractLibrary()) {
			pushArgs();
			auto [_, id] = m_pusher.ctx().functionInternalName(functionDefinition, false);
			m_pusher.pushInt(id);
			acceptExpr(&memberAccess->expression());
			m_pusher.callx(m_arguments.size() + 2, functionDefinition->returnParameters().size());
			return true;
		}

		cast_error(_functionCall, "Explicitly define message value, e.g. f{value: 1 ton}(...)");
	}

	auto memberAccess = to<MemberAccess>(&functionOptions->expression());
	if (!memberAccess)
		return false;

	// function definition
	functionDefinition = getRemoteFunctionDefinition(memberAccess);
	if (functionDefinition == nullptr) {
		return false;
	}

	// parse options they are stored in two vectors: names and options
	for (auto const& option: functionOptions->names())
		if (!isIn(*option, "stateInit", "flag", "value", "currencies", "bounce", "callback", "extra_flags"))
			cast_error(_functionCall, "Unsupported function call option: " + *option);

	// Search for stateInit option
	if (Expression const* stateInit = findOption("stateInit"))
		appendEitherStateInit = [this, stateInit] {
			// Either StateInit ^StateInit
			m_pusher << "STSLICECONST 1"; // ^StateInit
			acceptExpr(stateInit);
			m_pusher.blockSwap(1, 1);
			m_pusher << "STREF";
			return std::pair<int, int>{1, 1};
		};

	// Search for bounce option
	if (Expression const* bounce = findOption("bounce"))
		exprs[TvmConst::int_msg_info::bounce] = bounce;
	else
		constParams[TvmConst::int_msg_info::bounce] = "1";

	// Search for currencies option
	if (Expression const* currencies = findOption("currencies"))
		exprs[TvmConst::int_msg_info::currency] = currencies;
	else
		constParams[TvmConst::int_msg_info::currency] = "0";

	// Search for value (ton) option
	if (Expression const* valueExpr = findOption("value")) {
		auto const& value = ExprUtils::constValue(*valueExpr);
		if (value.has_value())
			constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(u256(value.value()));
		else {
			pushValue = [this, valueExpr] { acceptExpr(valueExpr); };
		}
	} else
		cast_error(_functionCall, "Explicitly define message value, e.g. f{value: 1 ton}(...)");

	// Search for extra_flags option
	if (Expression const* valueExpr = findOption("extra_flags")) {
		auto const& value = ExprUtils::constValue(*valueExpr);
		if (value.has_value())
			constParams[TvmConst::int_msg_info::extra_flags] = StrUtils::tonsToBinaryString(u256(value.value()));
		else {
			pushExtraFlags = [this, valueExpr] { acceptExpr(valueExpr); };
		}
	}

	// remote_addr
	exprs[TvmConst::int_msg_info::dest] = &memberAccess->expression();


	if (Expression const* callback = findOption("callback")) {
		CallableDeclaration const* remoteFunction = getFunctionDeclarationOrConstructor(callback);
		callbackFunctionId = ChainDataEncoder::
			calculateFunctionIDWithReason(remoteFunction, ReasonOfOutboundMessage::RemoteCallInternal);
	}

	// Search for sendRawMsg flag option
	if (Expression const* flag = findOption("flag")) {
		pushSendRawMsgFlag = [flag, this] { acceptExpr(flag); };
	}


	pushArgs(true);

	std::vector<VariableDeclaration const*> argDecl = convertArray(functionDefinition->parameters());
	bool const isLib = functionDefinition->annotation().contract->isLibrary();
	if (isLib) {
		argDecl.erase(argDecl.begin(), argDecl.begin() + 1);
	}
	solAssert(m_arguments.size() == argDecl.size(), "");

	std::function<void(int bitSizeBuilder, int refSizeBuilder)> appendBody = [&](int bitSizeBuilder,
																				 int refSizeBuilder) {
		ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
			argDecl,
			ChainDataEncoder::
				calculateFunctionIDWithReason(functionDefinition, ReasonOfOutboundMessage::RemoteCallInternal, isLib),
			callbackFunctionId,
			bitSizeBuilder,
			refSizeBuilder,
			true
		);
	};

	m_pusher.pushParamsAndSendInternalMessage(
		exprs,
		constParams,
		appendBody,
		pushSendRawMsgFlag,
		appendEitherStateInit,
		pushValue,
		pushExtraFlags
	);
	return true;
}

FunctionDefinition const* FunctionCallCompiler::getRemoteFunctionDefinition(MemberAccess const* memberAccess) {
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

void FunctionCallCompiler::abiBuildIntMsg() const {
	int const stackSize = m_pusher.stackSize();
	std::function<void()> pushValue;
	std::function<void()> pushExtraFlags;

	int destArg = -1;
	int valueArg = -1;
	int currenciesArg = -1;
	int bounceArg = -1;
	int extraFlagsArg = -1;
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
		case str2int("extra_flags"):
			extraFlagsArg = arg;
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
	std::vector<ASTPointer<Expression const>> const args = callList->arguments();
	auto functionDefinition = to<FunctionDefinition>(getFunctionDeclarationOrConstructor(callList->function()));
	bool const needCallback = functionDefinition != nullptr && functionDefinition->isResponsible();
	int const shift = needCallback ? 1 : 0;

	for (int idx = static_cast<int>(args.size()) - 1; shift <= idx; --idx) {
		ASTPointer<Expression const> const& arg = args.at(idx);
		acceptExpr(arg.get());
		m_pusher.convert(functionDefinition->parameters().at(idx - shift)->type(), getType(arg.get()));
	}

	std::set<int> isParamOnStack;
	std::map<int, std::string> constParams;
	std::function<void(int bitSizeBuilder, int refSizeBuilder)> appendBody = [&](int bitSizeBuilder,
																				 int refSizeBuilder) {
		if (functionDefinition != nullptr) {
			std::optional<uint32_t> callbackFunctionId;
			if (needCallback) {
				CallableDeclaration const* callback = getFunctionDeclarationOrConstructor(args.at(0).get());
				callbackFunctionId = ChainDataEncoder::
					calculateFunctionIDWithReason(callback, ReasonOfOutboundMessage::RemoteCallInternal);
			}
			std::vector<VariableDeclaration const*> params = convertArray(functionDefinition->parameters());
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				params,
				ChainDataEncoder::
					calculateFunctionIDWithReason(functionDefinition, ReasonOfOutboundMessage::RemoteCallInternal),
				callbackFunctionId,
				bitSizeBuilder,
				refSizeBuilder,
				true
			);
		}
	};

	for (auto const& [argIndex, name, id]: std::vector<std::tuple<int, std::string, int>>{
			 {currenciesArg, "currencies", TvmConst::int_msg_info::currency},
			 {valueArg, "value", TvmConst::int_msg_info::tons},
			 {destArg, "dest", TvmConst::int_msg_info::dest},
			 {bounceArg, "bounce", TvmConst::int_msg_info::bounce},
			 {extraFlagsArg, "extra_flags", TvmConst::int_msg_info::extra_flags}

		 }) {
		if (argIndex != -1) {
			std::optional<bigint> value = ExprUtils::constValue(*m_arguments.at(argIndex));
			std::optional<bool> flag = ExprUtils::constBool(*m_arguments.at(argIndex));
			if (value) {
				constParams[id] = StrUtils::tonsToBinaryString(*value);
			} else if (flag) {
				constParams[id] = StrUtils::boolToBinaryString(*flag);
			} else if (name == "value") {
				pushValue = [this, argIndex, name] { pushArgAndConvert(argIndex, name); };
			} else if (name == "extra_flags") {
				pushExtraFlags = [this, argIndex, name] { pushArgAndConvert(argIndex, name); };
			} else {
				pushArgAndConvert(argIndex, name);
				isParamOnStack.insert(id);
			}
		}
	}

	std::function<std::pair<int, int>()> appendEitherStateInit;
	if (stateInit != -1) {
		appendEitherStateInit = [&] {
			// Either StateInit ^StateInit
			m_pusher << "STSLICECONST 1"; // ^StateInit
			pushArgAndConvert(stateInit, "stateInit");
			m_pusher.blockSwap(1, 1);
			m_pusher << "STREF";
			return std::pair<int, int>{1, 1};
		};
	}

	m_pusher.prepareMessage(
		isParamOnStack,
		constParams,
		appendBody,
		appendEitherStateInit,
		StackPusher::MsgType::Internal,
		false,
		pushValue,
		pushExtraFlags
	);

	solAssert(m_pusher.stackSize() == stackSize + 1, "");
}

void FunctionCallCompiler::abiBuildDataInit() const {
	int const stackSize = m_pusher.stackSize();
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
	auto pushKey = [this, keyArg] {
		if (keyArg == -1) {
			m_pusher.pushInt(0);
		} else {
			pushArgAndConvert(keyArg, "pubkey");
		}
	};
	ContractType const* ct{};
	if (contrArg != -1) {
		Type const* type = m_arguments.at(contrArg)->annotation().type;
		auto tt = dynamic_cast<TypeType const*>(type);
		type = tt->actualType();
		ct = to<ContractType>(type);
	}

	bool data_map_supported = m_memberAccess->memberName() == "encodeOldDataInit";
	generateDataSection(data_map_supported, pushKey, varArg != -1 ? m_arguments[varArg].get() : nullptr, ct)();

	solAssert(m_pusher.stackSize() == stackSize + 1, "");
}

bool FunctionCallCompiler::checkTvmABIDeployMethods(Type::Category category) const {
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
			exprs[StateInitMembers::Code] = [&] { pushArgAndConvert(0); };
			exprs[StateInitMembers::Data] = [&] { pushArgAndConvert(1); };
			if (m_arguments.size() >= 3) {
				exprs[StateInitMembers::PrefixLength] = [&] { pushArgAndConvert(2); };
			}
		} else {
			bool dataIsSet = false;
			// std::string("code"), std::string("data"), std::string("prefixLength"), std::string("varInit"),
			// std::string("pubkey")
			for (int arg = 0; arg < static_cast<int>(m_arguments.size()); ++arg) {
				switch (str2int(m_names[arg]->c_str())) {
				case str2int("code"):
					codeArg = arg;
					exprs[StateInitMembers::Code] = [this, codeArg, name = *m_names.at(arg)] {
						pushArgAndConvert(codeArg, name);
					};
					break;
				case str2int("data"):
					dataArg = arg;
					exprs[StateInitMembers::Data] = [this, dataArg, name = *m_names.at(arg)] {
						pushArgAndConvert(dataArg, name);
					};
					dataIsSet = true;
					break;
				case str2int("prefixLength"):
					depthArg = arg;
					exprs[StateInitMembers::PrefixLength] = [this, depthArg, name = *m_names.at(arg)] {
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
				default:
					solUnimplemented("");
				}
			}
			if (!dataIsSet) {
				auto pushKey = [this, keyArg] {
					if (keyArg == -1) {
						m_pusher.pushInt(0);
					} else {
						pushArgAndConvert(keyArg, "pubkey");
					}
				};
				hasVars = (varArg != -1);
				ContractType const* contractType{};
				if (contrArg != -1) {
					Type const* type = m_arguments[contrArg]->annotation().type;
					auto tt = dynamic_cast<TypeType const*>(type);
					type = tt->actualType();
					contractType = to<ContractType>(type);
				}
				exprs[StateInitMembers::Data] =
					generateDataSection(false, pushKey, hasVars ? m_arguments[varArg].get() : nullptr, contractType);
			}
		}

		encodeStateInitAndHash(exprs, false);
		m_pusher << "ENDC";
		return true;
	}

	if (m_funcType->kind() == FunctionType::Kind::ABIStateInitHash) {
		pushArgs();
		m_pusher.pushFragmentInCallRef(4, 1, "__stateInitHash");
		return true;
	}

	return false;
}

void FunctionCallCompiler::abiDecodeData() const {
	pushArgConvertToMobileType(1);
	decodeData();
}

int FunctionCallCompiler::decodeData() const {
	std::vector<Type const*> stateVarTypes;

	if (auto retTuple = to<TupleType>(m_retType)) {
		for (Type const* type: retTuple->components())
			stateVarTypes.push_back(type);
	} else {
		stateVarTypes.push_back(m_retType);
	}

	// lvalue.. slice
	ChainDataDecoder decoder{&m_pusher};
	decoder.decodeData(0, 0, stateVarTypes, false);
	// lvalue.. stateVars...
	return stateVarTypes.size();
}

int FunctionCallCompiler::decodeFunctionParams() const {
	CallableDeclaration const* functionDefinition = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
	if (functionDefinition) {
		// lvalue.. slice
		auto fd = to<FunctionDefinition>(functionDefinition);
		bool isResponsible = fd->isResponsible();
		if (isResponsible) {
			m_pusher << "LDU 32";
		}
		// lvalue.. callback slice
		ChainDataDecoder decoder{&m_pusher};
		std::vector<Type const*> types = getParams(functionDefinition->parameters()).first;
		decoder.decodePublicFunctionParameters(types, isResponsible, true);

		return functionDefinition->parameters().size() + (isResponsible ? 1 : 0);
	}

	m_pusher << "ENDS";
	return 0;
}

void FunctionCallCompiler::sliceMethods(MemberAccess const& _node) const {
	auto returnTypes = [&](bool fromOptional) {
		Type const* type{};
		if (fromOptional) {
			solAssert(to<OptionalType>(m_retType), "");
			type = to<OptionalType>(m_retType)->valueType();
		} else {
			type = m_retType;
		}

		TypePointers types;
		if (auto const* targetTupleType = to<TupleType>(type))
			types = targetTupleType->components();
		else
			types = TypePointers{type};
		return types;
	};

	auto const& value = m_arguments.empty() ? std::nullopt : ExprUtils::constValue(*m_arguments[0]);
	ASTString const& memberName = _node.memberName();
	if (memberName == "empty") {
		acceptExpr(&_node.expression());
		m_pusher << "SEMPTY";
	} else if (memberName == "bitEmpty") {
		acceptExpr(&_node.expression());
		m_pusher << "SDEMPTY";
	} else if (memberName == "refEmpty") {
		acceptExpr(&_node.expression());
		m_pusher << "SREMPTY";
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
	} else if (memberName == "startsWithOne") {
		acceptExpr(&_node.expression());
		m_pusher << "SDFIRST";
	} else if (memberName == "startsWith") {
		acceptExpr(&_node.expression());
		pushArgs();
		m_pusher << "SDPFXREV";
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
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		if (m_arguments.size() == 1) {
			pushArgAndConvert(0);
			m_pusher << "SDSKIPFIRST";
		} else {
			pushArgAndConvert(0);
			pushArgAndConvert(1);
			m_pusher << "SSKIPFIRST";
		}
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (isIn(memberName, "loadFunctionParams", "decodeFunctionParams", "loadStateVars", "decodeStateVars")) {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
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
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (isIn(memberName, "load", "decode", "loadQ", "decodeQ") || boost::starts_with(memberName, "load")) {
		int stackDelta = 0;
		int const stackSize = m_pusher.stackSize();
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
			AbiPositionFromOneSlice pos;
			decode.decodeParameters(types, pos);
			stackDelta = types.size();
		} else if (isIn(memberName, "loadQ", "decodeQ")) {
			ChainDataDecoder decode{&m_pusher};
			AbiPositionFromOneSlice pos;
			decode.decodeParametersQ(returnTypes(true), pos);
			stackDelta = 1;
		} else if (boost::starts_with(memberName, "load")) {
			stackDelta = 1;
			std::optional<std::string> opcode;
			if (memberName == "loadBouncedMsgTag") {
				m_pusher << "LDU 32";
				m_pusher.blockSwap(1, 1);
				m_pusher.pushInt(0xFFFF'FFFE);
				m_pusher << "EQUAL";
				m_pusher.blockSwap(1, 1);
			} else if (memberName == "loadRefAsSlice") {
				m_pusher << "LDREFRTOS";
				m_pusher.exchange(1);
			} else if (memberName == "loadRef") {
				opcode = "LDREF";
			} else if (isIn(memberName, "loadUint", "loadUnsigned", "loadInt", "loadSigned")) {
				std::string cmd = "LD";
				cmd += isIn(memberName, "loadInt", "loadSigned") ? "I" : "U";
				if (value.has_value() && 1 <= value && value <= 256) {
					m_pusher << cmd + " " + value->str();
				} else {
					pushArgAndConvert(0);
					m_pusher << cmd + "X";
				}
			} else if (isIn(memberName, "loadUintQ", "loadIntQ")) {
				std::string cmd = "LD";
				cmd += memberName == "loadIntQ" ? "I" : "U";
				int take{};
				if (value.has_value() && 1 <= value && value <= 256) {
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
				opcode = "LDVARUINT16";
			} else if (memberName == "loadSlice") {
				auto const& value2 = m_arguments.size() == 2 ? ExprUtils::constValue(*m_arguments[1]) : std::nullopt;
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
				std::string cmd;
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
				m_pusher.pushS(0);				 // s v v
				m_pusher.pushInt((1 << 15) - 1); // s v v 32767
				m_pusher << "GREATER";			 // s v v>32767
				m_pusher.fixStack(-1);			 // fix stack
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
			m_exprCompiler.collectLValue(lValueInfo, true);
		} else {
			// decodedValues... slice
			m_pusher.drop();
			// decodedValues...
		}
		solAssert(stackSize + stackDelta == m_pusher.stackSize(), "");
	} else if (memberName == "preload") {
		acceptExpr(&_node.expression());

		ChainDataDecoder decode{&m_pusher};
		AbiPositionFromOneSlice pos;
		decode.decodeParameters(returnTypes(false), pos);
		m_pusher.drop();
	} else if (memberName == "preloadQ") {
		acceptExpr(&_node.expression());

		ChainDataDecoder decode{&m_pusher};
		AbiPositionFromOneSlice pos;
		decode.decodeParametersQ(returnTypes(true), pos);
		m_pusher.drop();
	} else if (memberName == "preloadRef") {
		acceptExpr(&_node.expression());
		if (m_arguments.empty()) {
			m_pusher << "PLDREFIDX 0";
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
		std::string cmd = std::string{} + "PLD" + (memberName == "preloadInt" ? "I" : "U");
		if (value.has_value() && 1 <= value && value <= 256) {
			cmd += " " + value->str();
		} else {
			pushArgs();
			cmd += "X";
		}
		m_pusher << cmd;
	} else if (isIn(memberName, "preloadIntLE4", "preloadIntLE8", "preloadUintLE4", "preloadUintLE8")) {
		acceptExpr(&_node.expression());
		std::string cmd = "PLD";
		cmd += boost::starts_with(memberName, "preloadInt") ? "I" : "U";
		cmd += "LE";
		cmd += boost::ends_with(memberName, "4") ? "4" : "8";
		m_pusher << cmd;
	} else if (isIn(memberName, "preloadIntQ", "preloadUintQ")) {
		acceptExpr(&_node.expression());
		std::string cmd = std::string{} + "PLD" + (memberName == "preloadIntQ" ? "I" : "U");
		int take{};
		if (value.has_value() && 1 <= value && value <= 256) {
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
		std::string cmd = "PLD";
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
		std::string cmd;
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
	} else if (memberName == "hash") {
		acceptExpr(&_node.expression());
		m_pusher << "HASHSU";
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::tvmVectorMethods() const {
	auto vectorType = to<TvmVectorType>(m_memberAccess->expression().annotation().type);
	auto valueTupleType = to<TupleType>(vectorType->valueType());

	ASTString const& memberName = m_memberAccess->memberName();
	if (memberName == "push") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		pushArgs();
		if (valueTupleType) {
			// lValue... vector element...
			m_pusher << "TUPLE " + toString(valueTupleType->components().size());
		}
		// lValue... vector element
		m_pusher << "TPUSH";
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (memberName == "pop") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		m_pusher << "TPOP";
		// lValue... vector lastElement
		m_pusher.blockSwap(lValueInfo.stackSizeDiff, 1);
		m_exprCompiler.collectLValue(lValueInfo, true);
		// lastElement
		if (valueTupleType)
			m_pusher << "UNTUPLE " + toString(valueTupleType->components().size());
	} else if (memberName == "length") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "TLEN";
	} else if (memberName == "empty") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "TLEN";
		m_pusher << "EQINT 0";
	} else if (memberName == "last") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "LAST";
		if (valueTupleType)
			m_pusher << "UNTUPLE " + toString(valueTupleType->components().size());
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::tvmStackMethods() const {
	ASTString const& memberName = m_memberAccess->memberName();
	if (memberName == "push") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		acceptExpr(m_arguments[0].get());
		// lValue... stack element
		m_pusher.blockSwap(1, 1);
		m_pusher << "TUPLE 2";
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (memberName == "pop") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		// lValue... stack
		m_pusher << "UNTUPLE 2";						 // lValue... value stack
		m_pusher.blockSwap(1, 1);						 // lValue... stack value
		m_pusher.blockSwap(lValueInfo.stackSizeDiff, 1); // value lValue... stack
		m_exprCompiler.collectLValue(lValueInfo, true);	 // value
	} else if (memberName == "top") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.indexWithExcep(0);
	} else if (memberName == "empty") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "ISNULL";
	} else if (memberName == "sort") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		// lValue... stack
		pushArgs();											 // lValue... stack lessFunc
		m_pusher.pushFragmentInCallRef(2, 1, "__stackSort"); // lValue... stack
		m_exprCompiler.collectLValue(lValueInfo, true);		 // value
	} else if (memberName == "reverse") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		// lValue... stack
		m_pusher.pushFragmentInCallRef(1, 1, "__stackReverse"); // lValue... stack
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::builderMethods(MemberAccess const& _node) const {
	ASTString const& memberName = _node.memberName();
	if (boost::starts_with(memberName, "store")) {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);

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
				m_pusher.blockSwap(1, 1);
				m_pusher << "STREF";
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
			for (auto const& argument: m_arguments | std::views::reverse) {
				if (ExprUtils::constBool(*argument)) {
					continue;
				}
				acceptExpr(argument.get());
				++args;
			}
			m_pusher.blockSwap(1, args);
			for (auto const& argument: m_arguments) {
				std::optional<bool> value = ExprUtils::constBool(*argument);
				if (value) {
					if (*value)
						m_pusher.stones(1);
					else
						m_pusher.stzeroes(1);
				} else {
					m_pusher.store(argument->annotation().type->mobileType());
				}
			}
		} else if (memberName == "storeQ") {
			solAssert(m_arguments.size() == 1, "");
			acceptExpr(m_arguments.at(0).get());
			m_pusher.blockSwap(1, 1);
			m_pusher.storeQ(m_arguments.at(0)->annotation().type->mobileType());
			// lValue... builder flag
			m_pusher.blockSwap(lValueInfo.stackSizeDiff, 1);
			// flag lValue... builder
		} else if (isIn(memberName, "storeSigned", "storeInt", "storeUnsigned", "storeUint")) {
			std::string cmd = "ST";
			cmd += isIn(memberName, "storeSigned", "storeInt") ? "I" : "U";
			pushArgs();
			m_pusher << cmd + "X" + "R";
		} else if (memberName == "storeTons") {
			opcode = "STVARUINT16";
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
			m_pusher << "DIVMOD";	  // s a1 a0
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
			m_pusher << "DIVMOD";	  // b a1 a0
			m_pusher.blockSwap(1, 2); // a1 a0 b
			m_pusher << "STU 8"
					 << "STU 8";
		} else if (memberName == "storeUintLE4") {
			opcode = "STULE4";
			doSwap = true;
		} else if (memberName == "storeUintLE8") {
			opcode = "STULE8";
			doSwap = true;
		} else if (isIn(memberName, "storeSha256", "storeSha512", "storeBlake2b", "storeKeccak256", "storeKeccak512")) {
			// lValue... builder
			pushAllArgsAndConvertToMobileType();  // lValue... builder args...
			m_pusher.pushInt(m_arguments.size()); // lValue... builder args... n
			m_pusher.startOpaque();
			m_pusher.pushAsym("HASHEXTA_" + boost::to_upper_copy<std::string>(memberName.substr(5)));
			m_pusher.endOpaque(1 + m_arguments.size() + 1, 1);
		} else {
			solUnimplemented("");
		}

		if (opcode.has_value()) {
			pushArgs();
			if (doSwap)
				m_pusher.blockSwap(1, 1);
			m_pusher << *opcode;
		}

		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (memberName == "bits") {
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
	} else if (memberName == "hash") {
		acceptExpr(&_node.expression());
		m_pusher << "HASHBU";
	} else if (memberName == "toSlice") {
		acceptExpr(&_node.expression());
		m_pusher << "BTOS";
	} else if (memberName == "depth") {
		acceptExpr(&_node.expression());
		m_pusher << "BDEPTH";
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::qIntOrBoolMethods() const {
	ASTString const& memberName = m_memberAccess->memberName();
	Type const* qType = m_memberAccess->expression().annotation().type;
	if (memberName == "isNaN") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher << "ISNAN";
	} else if (memberName == "get") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.pushS(0);
		m_pusher << "ISNAN";
		m_pusher._throw("THROWIF " + toString(TvmConst::RuntimeException::IsNaN));
	} else if (isIn(memberName, "getOr", "getOrDefault", "toOptional")) {
		int const startSize = m_pusher.stackSize();
		acceptExpr(&m_memberAccess->expression());
		if (memberName == "getOr")
			pushArgs(); // q default
		else if (memberName == "getOrDefault")
			m_pusher.pushDefaultValue(qType); // q default
		else if (memberName == "toOptional")
			m_pusher.pushNull(); // q null
		else
			solUnimplemented("");
		m_pusher.pushS(1);		 // q default q
		m_pusher << "ISNAN";	 // q default isNaN
		m_pusher.exchange(0, 2); // isNaN default q
		m_pusher << "CONDSEL";	 // default | q
		solAssert(startSize + 1 == m_pusher.stackSize(), "");
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::stringBuilderMethods() const {
	ASTString const& memberName = m_memberAccess->memberName();
	if (memberName == "toString") {
		acceptExpr(&m_memberAccess->expression());
		m_pusher.pushFragmentInCallRef(1, 1, "__makeString");
	} else if (memberName == "append") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&m_memberAccess->expression(), true);
		pushArgs();
		if (m_funcType->kind() == FunctionType::Kind::StringBuilderAppendByte) {
			m_pusher.pushFragmentInCallRef(2, 1, "__appendBytes1");
		} else if (m_funcType->kind() == FunctionType::Kind::StringBuilderAppendByteNTimes) {
			m_pusher.pushFragmentInCallRef(3, 1, "__appendBytes1NTimes");
		} else if (m_funcType->kind() == FunctionType::Kind::StringBuilderAppendString) {
			m_pusher.pushFragmentInCallRef(2, 1, "__appendStringToStringBuilder");
		} else {
			solUnimplemented("");
		}
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else {
		solUnimplemented("");
	}
}

void FunctionCallCompiler::arrayMethods(MemberAccess const& _node) const {
	Type const* type = _node.expression().annotation().type;
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
	} else if (_node.memberName() == "hash") {
		acceptExpr(&_node.expression());
		m_pusher << "HASHCU";
	} else if (_node.memberName() == "dataSizeQ") {
		acceptExpr(&_node.expression());
		pushArgAndConvert(0);
		cellBitRefQty();
	} else if (_node.memberName() == "push") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
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
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (_node.memberName() == "pop") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		// arr
		m_pusher << "UNTUPLE 2";																  // size dict
		m_pusher.pushS(1);																		  // size dict size
		m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::PopFromEmptyArray)); // size dict
		m_pusher.exchange(1);																	  // dict size
		m_pusher << "DEC";																		  // dict newSize
		m_pusher.pushS(0);							// dict newSize newSize
		m_pusher.rot();								// newSize newSize dict
		m_pusher.pushInt(TvmConst::ArrayKeyLength); // newSize newSize dict 32
		m_pusher << "DICTUDEL";						// newSize dict ?
		m_pusher.drop(1);							// newSize dict
		m_pusher << "TUPLE 2";						// arr
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (_node.memberName() == "append") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), true);
		pushArgAndConvert(0);
		m_pusher.pushFragmentInCallRef(2, 1, "__concatenateStrings");
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForOptionalMethods(MemberAccess const& _node) const {
	auto optional = to<OptionalType>(_node.expression().annotation().type);
	if (!optional)
		return false;

	auto retTuple = to<TupleType>(m_retType);
	int retQty = retTuple ? retTuple->components().size() : 1;

	ASTString const& memberName = _node.memberName();
	if (memberName == "hasValue") {
		acceptExpr(&_node.expression());
		m_pusher << "ISNULL";
		m_pusher << "NOT";
	} else if (memberName == "get") {
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
		acceptExpr(&_node.expression()); // opt
		solAssert(startSize + 1 == m_pusher.stackSize(), "");
		if (memberName == "getOr")
			pushArgs();
		else if (memberName == "getOrDefault") {
			m_pusher.pushDefaultValue(optional->valueType());
			solAssert(startSize + 1 + retQty == m_pusher.stackSize(), "");
		} else
			solUnimplemented("");

		// opt default... isNull
		m_pusher.pushS(retQty);
		m_pusher << "ISNULL"; // opt default... isNull
		solAssert(startSize + 1 + retQty + 1 == m_pusher.stackSize(), "");
		if (retTuple || optValueAsTuple(m_retType)) {
			m_pusher.fixStack(-1); // opt default...
			m_pusher.startOpaque();
			m_pusher.startContinuation();
			{
				// opt default
				m_pusher.dropUnder(1, retQty); // default...
				m_pusher.fixStack(+1);
			}
			m_pusher.endContinuation();
			m_pusher.startContinuation();
			{
				// opt default...
				m_pusher.drop(retQty); // opt
				if (retTuple)
					m_pusher.untuple(retTuple->components().size());
				else if (optValueAsTuple(m_retType))
					m_pusher.untuple(1);
				else
					solUnimplemented("");
			}
			m_pusher.endContinuation();
			m_pusher.ifElse();
			m_pusher.endOpaque(1 + retQty, retQty);
			solAssert(startSize + retQty == m_pusher.stackSize(), "");
		} else {
			// opt default isNull
			m_pusher.exchange(0, 2); // isNull default opt
			m_pusher << "CONDSEL";	 // default | opt
			solAssert(startSize + retQty == m_pusher.stackSize(), "");
		}
	} else if (memberName == "set") {
		Type const* rightType{};
		if (m_arguments.size() >= 2) {
			std::vector<Type const*> types;
			for (ASTPointer<Expression const> const& arg: m_arguments) {
				types.push_back(getType(arg.get()));
			}
			rightType = TypeProvider::tuple(types);
		} else {
			rightType = getType(m_arguments.at(0).get());
		}
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false);
		pushArgWithoutConvertion();

		m_pusher.convert(getType(&_node.expression()), rightType);
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else if (memberName == "reset") {
		LValueInfo const lValueInfo = m_exprCompiler.expandLValue(&_node.expression(), false);
		m_pusher.pushDefaultValue(optional);
		m_exprCompiler.collectLValue(lValueInfo, true);
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::cellMethods(MemberAccess const& _node) const {
	acceptExpr(&_node.expression());

	switch (m_funcType->kind()) {
	case FunctionType::Kind::TVMCellToSlice: {
		m_pusher << "CTOS";
		break;
	}
	case FunctionType::Kind::TVMCellExoticToSlice: {
		m_pusher << "XCTOS";
		break;
	}
	case FunctionType::Kind::TVMCellLoadExoticCell: {
		m_pusher << "XLOAD";
		break;
	}
	case FunctionType::Kind::TVMCellLoadExoticCellQ: {
		m_pusher << "XLOADQ";
		break;
	}
	case FunctionType::Kind::TVMCellDepth: {
		if (m_arguments.empty()) {
			m_pusher << "CDEPTH";
		} else {
			pushArgs();
			m_pusher << "CDEPTHIX";
		}
		break;
	}
	case FunctionType::Kind::TVMCellHash: {
		if (m_arguments.empty()) {
			m_pusher << "HASHCU";
		} else {
			pushArgs();
			m_pusher << "CHASHIX";
		}
		break;
	}
	case FunctionType::Kind::TVMCellDataSize: {
		pushArgAndConvert(0);
		m_pusher << "CDATASIZE";
		break;
	}
	case FunctionType::Kind::TVMCellDataSizeQ: {
		pushArgAndConvert(0);
		cellBitRefQty();
		break;
	}
	case FunctionType::Kind::TVMCellLevel: {
		m_pusher << "CLEVEL";
		break;
	}
	case FunctionType::Kind::TVMCellLevelMask: {
		m_pusher << "CLEVELMASK";
		break;
	}
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::integerMethods() const {
	acceptExpr(&m_memberAccess->expression());
	// stack: value
	switch (m_funcType->kind()) {
	case FunctionType::Kind::IntCast: {
		m_pusher.convert(m_retType, m_memberAccess->expression().annotation().type);
		break;
	}
	case FunctionType::Kind::Uint256Prefix: {
		// stack: value
		m_pusher.pushInt(256);
		// stack: value 256
		pushArgs();
		// stack: value 256 prefixLength
		m_pusher << "SUB";
		// stack: value 256-prefixLength
		m_pusher << "RSHIFT";
		// stack: value >> (256-prefixLength)
		break;
	}
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::variantMethods(MemberAccess const& _node) const {
	auto isUint = [&] {
		m_pusher.push(
			createNode<HardCode>(
				std::vector<std::string>{
					"PUSHCONT {",
					"	UFITS 256",
					"	TRUE",
					"}",
					"PUSHCONT {",
					"	FALSE",
					"}",
					"TRYARGS 1, 1"
				},
				1,
				1,
				true
			)
		);
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
	case FunctionType::Kind::VariantUncheckedCast: {
		acceptExpr(&_node.expression());
		break;
	}
	case FunctionType::Kind::VariantIsNull: {
		acceptExpr(&_node.expression());
		m_pusher << "ISNULL";
		break;
	}
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::addressMethod() {
	if (m_memberAccess->memberName() == "transfer") { // addr.transfer(...)
		std::map<int, Expression const*> exprs;
		std::map<int, std::string> constParams{{TvmConst::int_msg_info::bounce, "1"}};
		std::function<void(int bitSizeBuilder, int refSizeBuilder)> appendBody;
		std::function<void()> pushSendRawMsgFlag;
		std::function<std::pair<int, int>()> appendEitherStateInit;
		std::function<void()> pushValue;
		std::function<void()> pushExtraFlags;

		auto setValue = [&](Expression const* expr) {
			auto const& value = ExprUtils::constValue(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(u256(value.value()));
			} else {
				pushValue = [this, expr] { acceptExpr(expr); };
			}
		};

		auto setExtraFlags = [&](Expression const* expr) {
			auto const& value = ExprUtils::constValue(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::extra_flags] = StrUtils::tonsToBinaryString(u256(value.value()));
			} else {
				pushExtraFlags = [this, expr] { acceptExpr(expr); };
			}
		};

		auto setBounce = [&](auto expr) {
			std::optional<bool> const value = ExprUtils::constBool(*expr);
			if (value.has_value()) {
				constParams[TvmConst::int_msg_info::bounce] = value.value() ? "1" : "0";
			} else {
				exprs[TvmConst::int_msg_info::bounce] = expr;
				constParams.erase(TvmConst::int_msg_info::bounce);
			}
		};

		auto setAppendStateInit = [&](Expression const* expr) {
			appendEitherStateInit = [expr, this] {
				// Either StateInit ^StateInit
				m_pusher << "STSLICECONST 1"; // ^StateInit
				acceptExpr(expr);
				m_pusher.blockSwap(1, 1);
				m_pusher << "STREF";
				return std::pair<int, int>{1, 1};
			};
		};

		exprs[TvmConst::int_msg_info::dest] = &m_memberAccess->expression();

		int argumentQty = static_cast<int>(m_arguments.size());
		if (!m_names.empty() || argumentQty == 0) {
			for (int arg = 0; arg < argumentQty; ++arg) {
				switch (str2int(m_names[arg]->c_str())) {
				case str2int("value"):
					setValue(m_arguments[arg].get());
					break;
				case str2int("extra_flags"):
					setExtraFlags(m_arguments[arg].get());
					break;
				case str2int("bounce"):
					setBounce(m_arguments[arg].get());
					break;
				case str2int("flag"):
					pushSendRawMsgFlag = [e = m_arguments[arg], this] { acceptExpr(e.get()); };
					break;
				case str2int("body"):
					appendBody = [e = m_arguments[arg], this](int /*bitSizeBuilder*/, int /*refSizeBuilder*/) {
						m_pusher.stones(1);
						acceptExpr(e.get());
						m_pusher.blockSwap(1, 1);
						m_pusher << "STREF";
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
				pushSendRawMsgFlag = [&] { pushArgAndConvert(2); };
			}
			if (argumentQty >= 4) {
				appendBody = [&](int /*bitSizeBuilder*/, int /*refSizeBuilder*/) {
					m_pusher.stones(1);
					pushArgAndConvert(3);
					m_pusher.blockSwap(1, 1);
					m_pusher << "STREF";
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
		m_pusher.pushParamsAndSendInternalMessage(
			exprs,
			constParams,
			appendBody,
			pushSendRawMsgFlag,
			appendEitherStateInit,
			pushValue,
			pushExtraFlags
		);
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
	} else if (m_memberAccess->memberName() == "currency") {
		pushArgs();
		m_pusher << "GETEXTRABALANCE";
	} else {
		solUnimplemented("");
	}
}

bool FunctionCallCompiler::checkForTvmSendFunction(MemberAccess const& _node) const {
	if (_node.memberName() == "sendrawmsg") {
		// tvm.sendrawmsg
		pushArgs();
		m_pusher << "SENDRAWMSG";
	} else if (_node.memberName() == "sendMsg") { // tvm.sendMsg
		pushArgs();
		m_pusher << "SENDMSG";
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::msgFunction(MemberAccess const& _node) const {
	if (_node.memberName() == "pubkey") { // msg.pubkey()
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

void FunctionCallCompiler::rndFunction(MemberAccess const& _node) const {
	switch (m_funcType->kind()) {
	case FunctionType::Kind::RndNext:
		pushArgs();
		if (m_arguments.empty()) {
			m_pusher << "RANDU256";
		} else {
			m_pusher << "RAND";
		}
		break;
	case FunctionType::Kind::RndSetSeed: {
		pushArgAndConvert(0);
		m_pusher << "SETRAND";
		break;
	}
	case FunctionType::Kind::RndGetSeed: {
		m_pusher << "RANDSEED";
		break;
	}
	case FunctionType::Kind::RndShuffle: {
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

void FunctionCallCompiler::configFunction(MemberAccess const& _node) const {
	switch (m_funcType->kind()) {
	case FunctionType::Kind::ConfigGlobalId:
		m_pusher << "GLOBALID";
		break;
	case FunctionType::Kind::ConfigUnpackedConfig:
		m_pusher << "UNPACKEDCONFIGTUPLE";
		break;
	case FunctionType::Kind::ConfigGetGasFee:
		pushArgs();
		m_pusher << "GETGASFEE";
		break;
	case FunctionType::Kind::ConfigValueToGas: {
		pushArgs();
		m_pusher.pushFragmentInCallRef(2, 1, "__tonToGas");
		break;
	}
	case FunctionType::Kind::ConfigGetGasFeeSimple:
		pushArgs();
		m_pusher << "GETGASFEESIMPLE";
		break;
	case FunctionType::Kind::ConfigGetStorageFee:
		pushArgs();
		m_pusher << "GETSTORAGEFEE";
		break;
	case FunctionType::Kind::ConfigGetForwardFee:
		pushArgs();
		m_pusher << "GETFORWARDFEE";
		break;
	case FunctionType::Kind::ConfigGetForwardFeeSimple:
		pushArgs();
		m_pusher << "GETFORWARDFEESIMPLE";
		break;
	case FunctionType::Kind::ConfigGetOriginalFwdFee:
		pushArgs();
		m_pusher << "GETORIGINALFWDFEE";
		break;
	case FunctionType::Kind::ConfigGetParam:
		pushArgs();
		m_pusher << "CONFIGOPTPARAM";
		break;
	case FunctionType::Kind::ConfigGetPrecompiledGas:
		m_pusher << "GETPRECOMPILEDGAS";
		break;
	default:
		cast_error(_node, "Unsupported function call");
	}
}

void FunctionCallCompiler::secp256k1Function(MemberAccess const& _node) const {
	auto kind = m_funcType->kind();

	switch (kind) {
	case FunctionType::Kind::Secp256k1ECRecover: {
		pushArgs();
		m_pusher.startOpaque();
		{
			m_pusher.pushAsym("ECRECOVER");
			m_pusher.pushAsym("NULLSWAPIFNOT");
			m_pusher.startContinuation();
			{
				m_pusher.makeTuple(3);
			}
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.endOpaque(4, 1);
		break;
	}
	case FunctionType::Kind::Secp256k1AddTweakPublicKey: {
		pushArgs();
		m_pusher.startOpaque();
		{
			m_pusher.pushAsym("SECP256K1_XONLY_PUBKEY_TWEAK_ADD");
			m_pusher.pushAsym("NULLSWAPIFNOT");
			m_pusher.startContinuation();
			{
				m_pusher.makeTuple(3);
			}
			m_pusher.endContinuation();
			m_pusher._if();
		}
		m_pusher.endOpaque(2, 1);
		break;
	}
	default:
		cast_error(_node, "Unsupported function call");
	}
}

void FunctionCallCompiler::secp256r1Function(MemberAccess const& _node) const {
	auto kind = m_funcType->kind();

	switch (kind) {
	case FunctionType::Kind::Secp256r1CheckSign: {
		pushArgs();
		auto pubkeyType = m_funcType->parameterTypes().at(0);
		if (pubkeyType->category() == Type::Category::TvmSlice)
			m_pusher << "P256_CHKSIGNS";
		else
			m_pusher << "P256_CHKSIGNU";
		break;
	}
	default:
		cast_error(_node, "Unsupported function call");
	}
}

void FunctionCallCompiler::rist255Function() const {
	pushArgs();

	switch (m_funcType->kind()) {
	case FunctionType::Kind::Rist255FromHash: {
		m_pusher << "RIST255_FROMHASH";
		break;
	}
	case FunctionType::Kind::Rist255Validate: {
		m_pusher << "RIST255_VALIDATE";
		break;
	}
	case FunctionType::Kind::Rist255QValidate: {
		m_pusher << "RIST255_QVALIDATE";
		break;
	}
	case FunctionType::Kind::Rist255Add: {
		m_pusher << "RIST255_ADD";
		break;
	}
	case FunctionType::Kind::Rist255Sub: {
		m_pusher << "RIST255_SUB";
		break;
	}
	case FunctionType::Kind::Rist255Mul: {
		m_pusher << "RIST255_MUL";
		break;
	}
	case FunctionType::Kind::Rist255Mulbase: {
		m_pusher << "RIST255_MULBASE";
		break;
	}
	case FunctionType::Kind::Rist255QMulbase: {
		m_pusher.startOpaque();
		m_pusher.pushAsym("RIST255_QMULBASE");
		m_pusher.pushAsym("NULLSWAPIFNOT");
		m_pusher.drop();
		m_pusher.endOpaque(1, 1);
		break;
	}
	case FunctionType::Kind::Rist255QAdd: {
		m_pusher.startOpaque();
		m_pusher.pushAsym("RIST255_QADD");
		m_pusher.pushAsym("NULLSWAPIFNOT");
		m_pusher.drop();
		m_pusher.endOpaque(2, 1);
		break;
	}
	case FunctionType::Kind::Rist255QSub: {
		m_pusher.startOpaque();
		m_pusher.pushAsym("RIST255_QSUB");
		m_pusher.pushAsym("NULLSWAPIFNOT");
		m_pusher.drop();
		m_pusher.endOpaque(2, 1);
		break;
	}
	case FunctionType::Kind::Rist255QMul: {
		m_pusher.startOpaque();
		m_pusher.pushAsym("RIST255_QMUL");
		m_pusher.pushAsym("NULLSWAPIFNOT");
		m_pusher.drop();
		m_pusher.endOpaque(2, 1);
		break;
	}
	case FunctionType::Kind::Rist255L: {
		m_pusher << "RIST255_PUSHL";
		break;
	}
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::blsFunction() const {
	bool useTuple =
		!m_arguments.empty() && m_arguments.at(0)->annotation().type->category() == Type::Category::TvmVector;

	switch (m_funcType->kind()) {
	case FunctionType::Kind::BlsVerify:
		pushArgs();
		m_pusher << "BLS_VERIFY";
		break;
	case FunctionType::Kind::BlsAggregate:
		pushArgs();
		if (useTuple)
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						"DUP",
						"TLEN",
						"EXPLODEVAR",
						"BLS_AGGREGATE",
					},
					1,
					1,
					false
				)
			);
		else {
			m_pusher.pushInt(m_arguments.size());
			m_pusher.push(
				createNode<HardCode>(std::vector<std::string>{"BLS_AGGREGATE"}, m_arguments.size() + 1, 1, false)
			);
		}
		break;
	case FunctionType::Kind::BlsFastAggregateVerify:
		if (useTuple) {
			pushArgs();
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						// pks msg sig
						"ROT",		  // msg sig pks
						"DUP",		  // msg sig pks pks
						"TLEN",		  // msg sig pks n
						"EXPLODEVAR", // msg sig pk1 .. pksN n
						"PUSHINT 2",  // msg sig pk1 .. pksN n 2
						"PUSH S1",	  // msg sig pk1 .. pksN n 2 n
						"INC",		  // msg sig pk1 .. pksN n 2 n+1
						"BLKSWX",
						"BLS_FASTAGGREGATEVERIFY",
					},
					3,
					1,
					false
				)
			);
		} else {
			int n = m_arguments.size() - 2;
			for (int i = 0; i < n; ++i)
				pushArgAndConvert(i);
			m_pusher.pushInt(n);
			pushArgAndConvert(n);
			pushArgAndConvert(n + 1);
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						"BLS_FASTAGGREGATEVERIFY",
					},
					n + 3,
					1,
					false
				)
			);
		}
		break;
	case FunctionType::Kind::BlsAggregateVerify:
		if (useTuple) {
			pushArgs();
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						// pksMsgs sig
						"SWAP",		  // sig pksMsgs
						"DUP",		  // sig pksMsgs pksMsgs
						"TLEN",		  // sig pksMsgs n
						"EXPLODEVAR", // sig (pk, msg)... n
						"DUP",		  // sig (pk, msg)... n i
						"ADDCONST 1", // sig (pk, msg)... n i  // i = n+1..1
						"PUSH S1",	  // sig (pk, msg)... n i n
						"PUSHCONT {",
						// sig (pk, msg)... n i
						"DUP",		   // sig (pk, msg)... n i i
						"\tROLLX",	   // sig (pk, msg)... n i (pk[i], msg[i])
						"\tUNPAIR",	   // sig (pk, msg)... n i pk[i] msg[i]
						"\tPUSH S2",   // sig (pk, msg)... n i pk[i] msg[i] i
						"\tPUSHINT 2", // sig (pk, msg)... n i pk[i] msg[i] i 2
						"\tBLKSWX",	   // sig (pk, msg)... n i
						"\tDEC",
						"}",
						"REPEAT",
						// sig pk0, msg0 ... pkN, msgN n 1
						"PUSH S1",	  // sig pk0, msg0 ... pkN, msgN n 1 n
						"MULCONST 2", // sig pk0, msg0 ... pkN, msgN n 1 2*n
						"ADD",		  // sig pk0, msg0 ... pkN, msgN n 2*n+1
						"ROLLX",	  // pk0, msg0 ... pkN, msgN n sig
						"BLS_AGGREGATEVERIFY",
					},
					2,
					1,
					false
				)
			);
		} else {
			int n = (m_arguments.size() - 1) / 2;
			for (int i = 0; i < 2 * n; ++i)
				pushArgAndConvert(i);
			m_pusher.pushInt(n);
			pushArgAndConvert(2 * n);
			solAssert(2 * n == static_cast<int>(m_arguments.size()) - 1, "ddddd");
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						"BLS_AGGREGATEVERIFY",
					},
					2 * n + 2,
					1,
					false
				)
			);
		}
		break;
	case FunctionType::Kind::BlsG1Add:
		pushArgs();
		m_pusher << "BLS_G1_ADD";
		break;
	case FunctionType::Kind::BlsG1Sub:
		pushArgs();
		m_pusher << "BLS_G1_SUB";
		break;
	case FunctionType::Kind::BlsG1Neg:
		pushArgs();
		m_pusher << "BLS_G1_NEG";
		break;
	case FunctionType::Kind::BlsG1Mul:
		pushArgs();
		m_pusher << "BLS_G1_MUL";
		break;
	case FunctionType::Kind::BlsMapToG1:
		pushArgs();
		m_pusher << "BLS_MAP_TO_G1";
		break;
	case FunctionType::Kind::BlsG1IsZero:
		pushArgs();
		m_pusher << "BLS_G1_ISZERO";
		break;
	case FunctionType::Kind::BlsG1InGroup:
		pushArgs();
		m_pusher << "BLS_G1_INGROUP";
		break;

	case FunctionType::Kind::BlsG2Add:
		pushArgs();
		m_pusher << "BLS_G2_ADD";
		break;
	case FunctionType::Kind::BlsG2Sub:
		pushArgs();
		m_pusher << "BLS_G2_SUB";
		break;
	case FunctionType::Kind::BlsG2Neg:
		pushArgs();
		m_pusher << "BLS_G2_NEG";
		break;
	case FunctionType::Kind::BlsG2Mul:
		pushArgs();
		m_pusher << "BLS_G2_MUL";
		break;
	case FunctionType::Kind::BlsMapToG2:
		pushArgs();
		m_pusher << "BLS_MAP_TO_G2";
		break;
	case FunctionType::Kind::BlsG2IsZero:
		pushArgs();
		m_pusher << "BLS_G2_ISZERO";
		break;
	case FunctionType::Kind::BlsG2InGroup:
		pushArgs();
		m_pusher << "BLS_G2_INGROUP";
		break;
	case FunctionType::Kind::BlsG1Zero:
		pushArgs();
		m_pusher << "BLS_G1_ZERO";
		break;
	case FunctionType::Kind::BlsG2Zero:
		pushArgs();
		m_pusher << "BLS_G2_ZERO";
		break;
	case FunctionType::Kind::BlsPushR:
		pushArgs();
		m_pusher << "BLS_PUSHR";
		break;
	case FunctionType::Kind::BlsPairing:
	case FunctionType::Kind::BlsG1MultiExp:
	case FunctionType::Kind::BlsG2MultiExp: {
		pushArgs();
		std::string opcode;
		if (m_funcType->kind() == FunctionType::Kind::BlsG1MultiExp)
			opcode = "BLS_G1_MULTIEXP";
		else if (m_funcType->kind() == FunctionType::Kind::BlsG2MultiExp)
			opcode = "BLS_G2_MULTIEXP";
		else if (m_funcType->kind() == FunctionType::Kind::BlsPairing)
			opcode = "BLS_PAIRING";
		else
			solUnimplemented("");
		if (useTuple) {
			m_pusher.push(
				createNode<HardCode>(
					std::vector<std::string>{
						// xs
						"DUP",		  // xs xs
						"TLEN",		  // xs n
						"EXPLODEVAR", // (x, s)... n
						"DUP",		  // (x, s)... n i
						"ADDCONST 1", // (x, s)... n i  // i = n+1..1
						"PUSH S1",	  // (x, s)... n i n
						"PUSHCONT {",
						// (x, s)... n i
						"DUP",		   // (x, s)... n i i
						"\tROLLX",	   // (x, s)... n i (x[i], s[i])
						"\tUNPAIR",	   // (x, s)... n i x[i] s[i]
						"\tPUSH S2",   // (x, s)... n i x[i] s[i] i
						"\tPUSHINT 2", // (x, s)... n i x[i] s[i] i 2
						"\tBLKSWX",	   // (x, s)... n i
						"\tDEC",
						"}",
						"REPEAT",
						// x0, s0 ... xN, sN n 1
						"DROP", // x0, s0 ... xN, sN n
						opcode
					},
					1,
					1,
					false
				)
			);
		} else {
			m_pusher.pushInt(m_arguments.size() / 2);
			m_pusher.push(createNode<HardCode>(std::vector<std::string>{opcode}, m_arguments.size() + 1, 1, false));
		}
		break;
	}
	default:
		solUnimplemented("Unsupported bls function call");
	}
}


void FunctionCallCompiler::codeSalt() const {
	pushArgs();									// code
	m_pusher << "CTOS";							// sliceCode
	m_pusher << "PLDREFIDX 0";					// dict
	m_pusher.pushInt(crc16("__codeSaltIndex")); // dict index
	m_pusher.blockSwap(1, 1);					// index dict
	GetFromDict op{m_pusher, *TypeProvider::int_(19), *TypeProvider::tvmcell(), GetDictOperation::Fetch, std::nullopt};
	op.getDict();
}

void FunctionCallCompiler::setCodeSalt() const {
	pushArgs();				  // salt sliceCode
	m_pusher.blockSwap(1, 1); // salt sliceCode
	m_pusher << "CTOS";		  // salt sliceCode
	m_pusher << "LDREF";	  // salt dict sliceCode
	m_pusher.blockSwap(1, 1); // salt sliceCode dict
	m_pusher.pushInt(crc16("__codeSaltIndex"));
	// salt sliceCode dict index
	m_pusher.blockSwap(1, 1);  // salt sliceCode index dict
	m_pusher.blockSwap(1, 3);  // sliceCode index dict salt
	m_pusher.blockSwap(2, 1);  // sliceCode salt index dict
	m_pusher.pushInt(19);	   // sliceCode salt index dict 19
	m_pusher << "DICTISETREF"; // sliceCode dict
	m_pusher << "NEWC";		   // sliceCode dict b
	m_pusher << "STREF";
	m_pusher << "STSLICE";
	m_pusher << "ENDC";
}

void FunctionCallCompiler::functionId() const {
	auto callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
	uint32_t funcID;
	if (callDef == nullptr) {
		funcID = ChainDataEncoder::calculateConstructorFunctionID();
	} else {
		bool isManuallyOverridden{};
		std::tie(funcID, isManuallyOverridden) = ChainDataEncoder::calculateFunctionID(callDef);
		if (!isManuallyOverridden) {
			funcID &= 0x7FFFFFFFu;
		}
	}
	m_pusher.pushInt(funcID);
}

void FunctionCallCompiler::abiEncodeBody() const {
	CallableDeclaration const* callDef = getFunctionDeclarationOrConstructor(m_arguments.at(0).get());
	if (callDef == nullptr) {
		m_pusher << "NEWC";
	} else {
		auto funcDef = to<FunctionDefinition>(callDef);
		bool const needCallback = funcDef->isResponsible();
		int const shift = needCallback ? 1 : 0;
		std::optional<uint32_t> callbackFunctionId;
		if (needCallback) {
			CallableDeclaration const* callback = getFunctionDeclarationOrConstructor(m_arguments.at(1).get());
			callbackFunctionId =
				ChainDataEncoder::calculateFunctionIDWithReason(callback, ReasonOfOutboundMessage::RemoteCallInternal);
		}
		ast_vec<VariableDeclaration> const& parameters = callDef->parameters();
		std::vector<Type const*> types = getParams(parameters).first;
		AbiV2Position position{32, 0, types};
		for (int i = m_arguments.size() - 1; i >= 1 + shift; --i) {
			acceptExpr(m_arguments.at(i).get());
		}
		m_pusher << "NEWC";
		ChainDataEncoder{&m_pusher}.createMsgBody(
			convertArray(parameters),
			ChainDataEncoder::calculateFunctionIDWithReason(callDef, ReasonOfOutboundMessage::RemoteCallInternal),
			callbackFunctionId,
			position,
			true
		);
	}
	m_pusher << "ENDC";
}

bool FunctionCallCompiler::checkForTvmC4(MemberAccess const& _node) const {
	auto const& name = _node.memberName();
	if (name != "unpackData" && name != "packData")
		return false;

	auto const& usualStateVars = m_pusher.ctx().storageLayout().usualStateVariables();
	std::vector<VariableDeclaration const*> allStateVars =
		m_pusher.ctx().storageLayout().usualAndUnpackedStateVariables();
	std::vector<Type const*> const& varTypes = getTypesFromVarDecls(allStateVars);
	std::vector<bool> varNeeded(allStateVars.size());

	if (name == "unpackData") {
		std::vector<VariableDeclaration const*> expectedOrder;
		std::set<VariableDeclaration const*> neededStateVarSet;
		for (auto const& arg: m_arguments) {
			auto identifier = to<Identifier>(arg.get());
			Declaration const* declaration = identifier->annotation().referencedDeclaration;
			auto variableDeclaration = to<VariableDeclaration>(declaration);
			neededStateVarSet.insert(variableDeclaration);
			expectedOrder.emplace_back(variableDeclaration);
		}

		m_pusher.getGlob(m_pusher.ctx().storageLayout().getUnpackIndex());
		// stack: slice

		std::vector<VariableDeclaration const*> stackOrder;
		for (size_t i = 0; i < allStateVars.size(); ++i) {
			auto const stateVar = allStateVars.at(i);
			if (neededStateVarSet.contains(stateVar)) {
				varNeeded[i] = true;
				stackOrder.emplace_back(stateVar);
			}
		}

		UnpackedCoderDecoder decoder{
			m_pusher,
			m_pusher.ctx().storageLayout().getOffsetC4(),
			0,
			static_cast<int>(usualStateVars.size()),
			varTypes,
			varNeeded
		};
		decoder.unpackedData();
		// stack: unpackedStateVars...

		// set the correct order of return values
		size_t n = expectedOrder.size();
		for (size_t i = 0; i < expectedOrder.size(); ++i) {
			if (*expectedOrder.at(i) != *stackOrder.at(i)) {
				size_t j = i + 1;
				for (;; ++j) {
					if (*expectedOrder.at(i) == *stackOrder.at(j)) {
						break;
					}
				}
				m_pusher.exchange(n - j - 1, n - i - 1);
				std::swap(stackOrder.at(i), stackOrder.at(j));
			}
		}
	} else if (name == "packData") {
		std::map<int, std::function<void()>> varIndexToPush;
		for (size_t i = 0; i < m_names.size(); ++i) {
			int varIndex = 0;
			for (;; ++varIndex)
				if (allStateVars.at(varIndex)->name() == *m_names[i])
					break;
			varNeeded[varIndex] = true;
			varIndexToPush[varIndex] = [this, i, varIndex, &varTypes] {
				pushExprAndConvert(m_arguments.at(i).get(), varTypes.at(varIndex));
			};
		}

		int const stackSize = m_pusher.stackSize();
		m_pusher.getGlob(m_pusher.ctx().storageLayout().getUnpackIndex());
		// stack: slice

		UnpackedCoderDecoder decoder{
			m_pusher,
			m_pusher.ctx().storageLayout().getOffsetC4(),
			0,
			static_cast<int>(usualStateVars.size()),
			varTypes,
			varNeeded
		};
		decoder.packData(varIndexToPush);

		m_pusher.setGlob(m_pusher.ctx().storageLayout().getUnpackIndex());
		solAssert(stackSize == m_pusher.stackSize(), "stackSize == m_pusher.stackSize()");
	} else {
		solUnimplemented("");
	}
	return true;
}

bool FunctionCallCompiler::checkForTvmFunction(MemberAccess const& _node) const {
	auto const& name = _node.memberName();
	if (name == "pubkey") { // tvm.pubkey
		m_pusher.getGlob(TvmConst::C7::TvmPubkey);
	} else if (name == "setPubkey") { // tvm.setPubkey
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::TvmPubkey);
	} else if (name == "accept") { // tvm.accept
		m_pusher << "ACCEPT";
	} else if (name == "hash") { // tvm.hash
		pushArgConvertToMobileType(0);
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
	} else if (name == "checkSign") { // tvm.checkSign
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
				m_pusher << "BTOS";
			} else {
				pushArgAndConvert(1);
			}
			pushArgAndConvert(cnt - 1);
			m_pusher << "CHKSIGNU";
		}
	} else if (name == "setcode") { // tvm.setcode
		pushArgs();
		m_pusher << "SETCODE";
	} else if (name == "setCurrentCode") { // tvm.setCurrentCode
		int const stackSize = m_pusher.stackSize();
		pushArgs();
		m_pusher << "CTOS";
		m_pusher << "BLESS";
		m_pusher.popC3();
		solAssert(stackSize == m_pusher.stackSize(), "");
	} else if (name == "getData") { // tvm.getData
		m_pusher.pushRoot();
	} else if (name == "setData") { // tvm.setData
		pushArgs();
		m_pusher.popRoot();
	} else if (name == "rawCommit") { // tvm.rawCommit
		m_pusher << "COMMIT";
	} else if (name == "commit") { // tvm.commit
		m_pusher.pushFragmentInCallRef(0, 0, "c7_to_c4");
		m_pusher << "COMMIT";
	} else if (name == "log") { // tvm.log
		compileLog();
	} else if (name == "resetStorage") { // tvm.resetStorage
		m_pusher.resetAllStateVars();
	} else if (name == "functionId") { // tvm.functionId
		functionId();
	} else if (name == "encodeBody") { // tvm.encodeBody
		abiEncodeBody();
	} else if (name == "rawReserve") {
		pushArgs();
		int n = m_arguments.size();
		solAssert(isIn(n, 2, 3), "");
		m_pusher << (n == 2 ? "RAWRESERVE" : "RAWRESERVEX");
	} else if (isIn(name, "exit", "exit1")) {
		m_pusher.pushFragment(0, 0, "c7_to_c4");
		if (name == "exit")
			m_pusher._throw("THROW 0");
		else
			m_pusher._throw("THROW 1");
	} else if (name == "code") {
		m_pusher << "MYCODE";
	} else if (name == "codeSalt") {
		codeSalt();
	} else if (name == "setCodeSalt") {
		setCodeSalt();
	} else if (name == "replayProtectionValue") {
		m_pusher.getGlob(TvmConst::C7::ReplayProtTime);
	} else if (name == "setReplayProtectionValue") {
		pushArgs();
		m_pusher.setGlob(TvmConst::C7::ReplayProtTime);
	} else if (name == "replayProtInterval") {
		m_pusher.pushInt(TvmConst::Message::ReplayProtection::Interval);
	} else if (name == "setGasLimit") {
		pushArgs();
		m_pusher << "SETGASLIMIT";
	} else if (name == "buyGas") {
		pushArgs();
		m_pusher << "BUYGAS";
	} else if (name == "duePayment") {
		m_pusher << "DUEPAYMENT";
	} else if (name == "loadLibrary") {
		pushArgs();
		TypeConversion tc{m_pusher};
		tc.convertIntegerToLibraryExoticCell();
	} else if (name == "prevBlocksInfo") {
		m_pusher << "PREVBLOCKSINFOTUPLE";
	} else if (name == "prevMCBlocks") {
		m_pusher << "PREVMCBLOCKS";
	} else if (name == "prevKeyBlock") {
		m_pusher << "PREVKEYBLOCK";
	} else if (name == "prevMCBlocks100") {
		m_pusher << "PREVMCBLOCKS_100";
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::abiFunction() const {
	switch (m_funcType->kind()) {
	case FunctionType::Kind::ABIEncode: {
		std::vector<Type const*> types;
		for (ASTPointer<Expression const> const& arg: m_arguments) {
			types.emplace_back(arg->annotation().type->mobileType());
		}
		AbiV2Position position{0, 0, types};

		for (ASTPointer<Expression const> const& arg: m_arguments | std::views::reverse) {
			acceptExpr(arg.get());
		}
		m_pusher << "NEWC";
		ChainDataEncoder encoder{&m_pusher};
		encoder.encodeParameters(types, position, false);
		m_pusher << "ENDC";
		break;
	}
	case FunctionType::Kind::ABIDecode: {
		std::vector<Type const*> types;
		auto te = to<TupleExpression>(m_arguments.at(1).get());
		if (te) {
			for (ASTPointer<Expression> const& e: te->components()) {
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
		decoder.decodeData(0, 0, types, false);
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
		pushArgConvertToMobileType(1);
		decodeFunctionParams();
		break;
	}
	default:
		cast_error(m_functionCall, "Not supported.");
	}
}

void FunctionCallCompiler::mathFunction(MemberAccess const& _node) const {
	auto isCombArithOpers = [](std::string const& name) {
		auto const combArithOpers = tonCombinedArithmeticOperations();
		return std::ranges::find_if(combArithOpers, [&](auto const& op) { return op.name == name; }) !=
			   combArithOpers.end();
	};

	bool isQuiet = false;
	auto retTuple = to<TupleType>(m_retType);
	if (m_retType->category() == Type::Category::Tuple) {
		if (retTuple->components().at(0)->category() == Type::Category::QInteger)
			isQuiet = true;
	} else if (m_retType->category() == Type::Category::QInteger)
		isQuiet = true;
	std::string const prefix = isQuiet ? "Q" : "";

	auto const& memberName = _node.memberName();
	if (memberName == "max") {
		pushArgAndConvertToCommon();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher << prefix + "MAX";
	} else if (memberName == "min") {
		pushArgAndConvertToCommon();
		for (int i = 0; i + 1 < static_cast<int>(m_arguments.size()); ++i)
			m_pusher << prefix + "MIN";
	} else if (memberName == "minmax") {
		pushArgAndConvertToCommon();
		m_pusher << prefix + "MINMAX";
	} else if (isIn(memberName, "divr", "divc")) {
		pushArgAndConvertToCommon();
		if (m_retType->category() == Type::Category::FixedPoint) {
			int power = to<FixedPointType>(m_retType)->fractionalDigits();
			m_pusher.pushInt(MathConsts::power10().at(power)); // res 10^n
			m_pusher.exchange(1);
			m_pusher << "MUL" + boost::to_upper_copy<std::string>(memberName);
		} else {
			m_pusher << prefix + boost::to_upper_copy<std::string>(memberName);
		}
		Type const* leftType = m_arguments.at(0)->annotation().type;
		Type const* rightType = m_arguments.at(1)->annotation().type;
		if (!isFitUseless(leftType, rightType, m_retType, Token::Div) && !m_pusher.ctx().ignoreIntegerOverflow())
			m_pusher.checkFit(m_retType);
	} else if (isIn(memberName, "mulmod")) {
		pushArgAndConvertToCommon();
		m_pusher << prefix + boost::to_upper_copy<std::string>(memberName);
	} else if (isCombArithOpers(memberName)) {
		pushArgAndConvertToCommon();
		m_pusher << boost::to_upper_copy<std::string>(memberName);
	} else if (isIn(memberName, "muldiv", "muldivr", "muldivc")) {
		pushArgAndConvertToCommon();
		m_pusher << prefix + boost::to_upper_copy<std::string>(memberName);
		if (!m_pusher.ctx().ignoreIntegerOverflow())
			m_pusher.checkFit(m_retType);
	} else if (memberName == "divmod") {
		pushArgAndConvertToCommon();
		m_pusher << prefix + "DIVMOD";
		Type const* leftType = m_arguments.at(0)->annotation().type;
		Type const* rightType = m_arguments.at(1)->annotation().type;
		Type const* resType = retTuple->components().at(0);
		if (!isFitUseless(leftType, rightType, resType, Token::Div) && !m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.blockSwap(1, 1);
			m_pusher.checkFit(resType);
			m_pusher.blockSwap(1, 1);
		}
	} else if (memberName == "muldivmod") {
		pushArgAndConvertToCommon();
		m_pusher << prefix + "MULDIVMOD";
		if (!m_pusher.ctx().ignoreIntegerOverflow()) {
			m_pusher.exchange(1);
			m_pusher.checkFit(retTuple->components().at(0));
			m_pusher.exchange(1);
		}
	} else if (memberName == "abs") {
		pushArgWithoutConvertion();
		m_pusher << "ABS";
		if (!m_pusher.ctx().ignoreIntegerOverflow())
			m_pusher.checkFit(m_retType);
	} else if (memberName == "modpow2") {
		pushExprAndConvert(m_arguments[0].get(), m_retType);
		Expression const* expression = m_arguments[1].get();
		auto const& value = ExprUtils::constValue(*expression);
		if (!value.has_value() || value < 0 || value >= 256)
			cast_error(*expression, "Expected a constant integer in the range 1 - 255.");
		m_pusher << prefix + "MODPOW2 " + value->str();
	} else if (memberName == "sign") {
		pushArgConvertToMobileType(0);
		m_pusher << prefix + "SGN";
	} else
		cast_error(m_functionCall, "Unsupported function call");
}

bool FunctionCallCompiler::checkBaseContractCall(MemberAccess const& _node) const {
	auto funDef = to<FunctionDefinition>(_node.annotation().referencedDeclaration);
	if (funDef) {
		// calling base contract method
		pushArgs();
		m_pusher.pushCallOrCallRef(funDef, std::nullopt, true);
		return true;
	}
	return false;
}

bool FunctionCallCompiler::checkAddressThis() const {
	// compile  "address(this)"
	if (isAddressThis(&m_functionCall)) {
		m_pusher << "MYADDR";
		return true;
	}
	return false;
}

void FunctionCallCompiler::createObject() const {
	switch (m_retType->category()) {
	case Type::Category::TvmCell:
	case Type::Category::TvmBuilder:
		m_pusher.pushDefaultValue(m_retType);
		break;
	default:
		solUnimplemented("");
	}
}

void FunctionCallCompiler::typeConversion() const {
	solAssert(m_arguments.size() == 1, "");
	Type const* argType = m_arguments[0]->annotation().type;

	if (auto funCall = to<FunctionCall>(m_arguments[0].get())) {
		if (*funCall->annotation().kind == FunctionCallKind::TypeConversion && funCall->arguments().size() == 1) {
			// c(b(a)), e.g. uint8 x; int(uint(x));
			auto a = to<IntegerType>(funCall->arguments().at(0)->annotation().type);
			auto b = to<IntegerType>(funCall->annotation().type);
			auto c = to<IntegerType>(m_retType);
			if (a && b && c) {
				if (!a->isSigned() &&
					!b->isSigned() &&
					c->isSigned() &&
					a->numBits() < b->numBits() &&
					b->numBits() == c->numBits()) {
					acceptExpr(funCall->arguments().at(0).get());
					// no conversion
					return;
				}
			}
		}
	}

	auto getDigits = [](Type const* type) -> int {
		if (auto fix = to<FixedPointType>(type))
			return fix->fractionalDigits();
		if (isIn(
				type->category(),
				Type::Category::Integer,
				Type::Category::QInteger,
				Type::Category::VarInteger,
				Type::Category::Enum
			))
			return 0;
		solUnimplemented("");
	};

	auto adjustDigits = [&] {
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
	case Type::Category::QInteger:
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
	case Type::Category::AddressStd:
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

bool FunctionCallCompiler::checkLocalFunctionOrLibCall(Identifier const* identifier) const {
	auto functionDefinition = to<FunctionDefinition>(identifier->annotation().referencedDeclaration);
	if (!functionDefinition)
		return false;
	pushArgs();
	if (functionDefinition->isInline()) {
		std::string const& functionName = m_pusher.ctx().functionInternalName(functionDefinition, false).first;
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
			for (ASTPointer<Statement> const& s: functionDefinition->body().statements()) {
				auto assembly = to<FreeInlineAssembly>(s.get());
				for (ASTPointer<Expression> const& line: assembly->lines()) {
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

bool FunctionCallCompiler::checkHashFunctions() const {
	if (m_funcType == nullptr)
		return false;

	auto hashExt = [&] {
		auto ident = to<Identifier>(&m_functionCall.expression());
		auto opcode = "HASHEXT_" + boost::to_upper_copy<std::string>(ident->name());

		pushAllArgsAndConvertToMobileType();
		m_pusher.pushInt(m_arguments.size());
		m_pusher.pushStackGenOpcode(opcode, m_arguments.size() + 1, 1);
	};

	switch (m_funcType->kind()) {
	case FunctionType::Kind::SHA256: {
		if (m_arguments.size() == 1) {
			pushArgConvertToMobileType(0);
			Type const* arg = m_arguments.at(0)->annotation().type;
			auto arrType = to<ArrayType>(arg);
			if (arrType && arrType->isByteArrayOrString())
				m_pusher << "CTOS";
			m_pusher << "SHA256U";
		} else {
			hashExt();
		}
		break;
	}
	case FunctionType::Kind::HashExt: {
		hashExt();
		break;
	}
	default:
		return false;
	}

	return true;
}

bool FunctionCallCompiler::checkSolidityUnits() const {
	if (m_funcType == nullptr) {
		return false;
	}

	switch (m_funcType->kind()) {
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

	case FunctionType::Kind::Selfdestruct: { // "selfdestruct"
		std::map<int, std::string> const constParams{
			{TvmConst::int_msg_info::tons, StrUtils::tonsToBinaryString(u256(0))},
			{TvmConst::int_msg_info::bounce, "0"},
		};
		m_pusher.pushParamsAndSendInternalMessage(
			{{TvmConst::int_msg_info::dest, m_arguments[0].get()}},
			constParams,
			nullptr,
			[&] { m_pusher << "PUSHINT " + toString(TvmConst::SENDRAWMSG::SelfDestruct); },
			nullptr,
			nullptr,
			nullptr
		);
		return true;
	}

	case FunctionType::Kind::Require: {
		if (m_arguments.size() == 1) {
			pushArgAndConvert(0);
			m_pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::DefaultError));
		} else if (m_arguments.size() == 2 || m_arguments.size() == 3) {
			Type const* type1 = m_arguments.at(1)->annotation().type;
			auto arr = dynamic_cast<ArrayType const*>(type1);
			if (dynamic_cast<StringLiteralType const*>(type1) || (arr && arr->isString())) {
				pushArgAndConvert(1);
				pushArgAndConvert(0);
				m_pusher._throw("THROWARGIFNOT " + toString(TvmConst::RuntimeException::DefaultError));
			} else {
				if (m_arguments.size() == 3)
					pushArgAndConvert(2);
				auto const& exceptionCode = ExprUtils::constValue(*m_arguments[1].get());
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
			m_pusher._throw("THROW " + toString(TvmConst::RuntimeException::DefaultError));
		} else {
			if (!isIn(static_cast<int>(m_arguments.size()), 1, 2)) {
				cast_error(m_functionCall, R"("revert" takes up to two m_arguments.)");
			}
			auto const& exceptionCode = ExprUtils::constValue(*m_arguments[0].get());
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
		int const stackSize = m_pusher.stackSize();
		auto literal = to<Literal>(m_arguments[0].get());
		std::string formatStr = literal->value();
		size_t pos = 0;
		std::vector<std::pair<std::string, std::string>> substrings;
		while (true) {
			pos = formatStr.find('{', pos);
			size_t close_pos = formatStr.find('}', pos);
			if (pos == std::string::npos || close_pos == std::string::npos)
				break;
			if (formatStr[pos + 1] != ':' && close_pos != pos + 1) {
				pos++;
				continue;
			}

			std::string format = formatStr.substr(pos + 1, close_pos - pos - 1);
			if (format[0] == ':')
				format.erase(0, 1);
			substrings.emplace_back(formatStr.substr(0, pos), format);
			formatStr = formatStr.substr(close_pos + 1);
			pos = 0;
		}
		// stack: Stack(TvmBuilder)
		m_pusher << "NEWC";
		m_pusher << "NULL";
		m_pusher << "TUPLE 2";

		auto pushConstStr = [&](std::string const& constStr) {
			if (!constStr.empty()) {
				size_t maxSlice = TvmConst::CellBitLength / 8;
				for (size_t i = 0; i < constStr.length(); i += maxSlice) {
					m_pusher.pushString(constStr.substr(i, std::min(maxSlice, constStr.length() - i)), true);
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
			Type const* argType = m_arguments[it + 1]->annotation().type;
			acceptExpr(m_arguments[it + 1].get());
			if (cat == Type::Category::Integer || cat == Type::Category::RationalNumber) {
				// stack: Stack(TvmBuilder)
				std::string format = substrings[it].second;
				bool leadingZeroes = !format.empty() && format[0] == '0';
				bool isHex = !format.empty() && (format.back() == 'x' || format.back() == 'X');
				bool isLower = isHex && !format.empty() && format.back() == 'x';
				bool isTon = !format.empty() && format.back() == 't';
				if (!isTon) {
					while (!format.empty() && (format.back() < '0' || format.back() > '9')) {
						format.pop_back();
					}
					int width = 0;
					if (!format.empty()) {
						try {
							width = boost::lexical_cast<int>(format);
						} catch (boost::bad_lexical_cast const&) {
							cast_error(
								*m_arguments[0],
								"Invalid format width."
								" Can not convert \"" +
									format +
									"\" to integer."
							);
						}
					}
					if (width < 0 || width > 127)
						cast_error(m_functionCall, "Width should be in range of 0 to 127.");
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
					m_pusher.pushInt(9);
					m_pusher.pushInt(MathConsts::power10().at(9));
					m_pusher.pushFragmentInCallRef(4, 1, "__convertFixedPointToString");
				}
			} else if (cat == Type::Category::Address || cat == Type::Category::AddressStd) {
				m_pusher.pushFragmentInCallRef(2, 1, "__convertAddressToHexString");
			} else if (isStringOrStringLiteralOrBytes(argType)) {
				m_pusher.pushFragmentInCallRef(2, 1, "__appendStringToStringBuilder");
			} else if (cat == Type::Category::FixedPoint) {
				int power = to<FixedPointType>(argType)->fractionalDigits();
				m_pusher.pushInt(power);
				m_pusher.pushInt(MathConsts::power10().at(power));
				m_pusher.pushFragmentInCallRef(4, 1, "__convertFixedPointToString");
			} else if (cat == Type::Category::Bool) {
				m_pusher.pushFragmentInCallRef(2, 1, "__convertBoolToStringBuilder");
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

bool FunctionCallCompiler::checkLocalFunctionOrLibCallOrFuncVarCall() const {
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
		int paramCnt = m_funcType->parameterTypes().size();
		int returnCnt = m_funcType->returnParameterTypes().size();
		m_pusher.pushC3();
		m_pusher.callx(paramCnt + 1 + 1, returnCnt);
	} else {
		return false;
	}
	return true;
}

void FunctionCallCompiler::createNewContract() const {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
	solAssert(functionOptions, "");
	auto newExpr = to<NewExpression>(&functionOptions->expression());
	solAssert(newExpr, "");

	pushArgs(true);
	// stack: arg[n-1] ... arg[1] arg[0]
	Type const* type = newExpr->typeName().annotation().type;

	std::function<void()> pushKey = [&] {
		if (Expression const* stateInit = findOption("pubkey")) {
			acceptExpr(stateInit);
		} else {
			m_pusher.pushInt(0);
		}
	};

	std::function<void()> pushPrefix;
	if (Expression const* prefix = findOption("prefix")) {
		pushPrefix = [this, prefix] { acceptExpr(prefix); };
	}

	std::optional<StateInitInfo> stateInitInfo;
	if (Expression const* stateInit = findOption("stateInit")) {
		stateInitInfo = StateInitInfo{false, 0, 0};
		acceptExpr(stateInit); // stack: stateInit
		m_pusher.pushS(0);
		m_pusher << "HASHCU";
		// stack: stateInit hash
	} else if (Expression const* code = findOption("code")) {
		std::map<StateInitMembers, std::function<void()>> stateInitExprs;

		Expression const* varInit = findOption("varInit");
		bool hasVars = varInit != nullptr;
		auto ct = to<ContractType>(newExpr->typeName().annotation().type);
		stateInitExprs[StateInitMembers::Data] = generateDataSection(false, pushKey, hasVars ? varInit : nullptr, ct);

		stateInitExprs[StateInitMembers::Code] = [&] { acceptExpr(code); };

		if (Expression const* prefixLength = findOption("prefixLength")) {
			stateInitExprs[StateInitMembers::PrefixLength] = [this, prefixLength] { acceptExpr(prefixLength); };
		}

		int bits;
		int refs;
		bool const savePrefixLength = pushPrefix != nullptr;
		std::tie(bits, refs) = encodeStateInitAndHash(stateInitExprs, savePrefixLength);
		// stack: [prefixLength] stateInit
		m_pusher.pushS(0);
		// stack: [prefixLength] stateInit stateInit
		m_pusher << "HASHBU";
		// stack: [prefixLength] stateInit hash
		stateInitInfo = StateInitInfo{true, bits, refs};
	} else {
		solUnimplemented("");
	}
	// stack: arg[n-1] ... arg[1] arg[0] [prefixLength] stateInit hash

	std::variant<int8_t, std::function<void()>> pushWid = int8_t{0};
	if (Expression const* wid = findOption("wid")) {
		std::optional<bigint> value = ExprUtils::constValue(*wid);
		if (value) {
			pushWid = static_cast<int8_t>(value.value());
		} else {
			pushWid = [this, wid] { acceptExpr(wid); };
		}
	}

	std::variant<bigint, std::function<void()>> pushValue;
	{
		Expression const* value = findOption("value");
		solAssert(value, "");
		std::optional<bigint> v = ExprUtils::constValue(*value);
		if (v) {
			pushValue = v.value();
		} else {
			pushValue = [this, value] { acceptExpr(value); };
		}
	}

	std::variant<bigint, std::function<void()>> pushExtraFlags;
	if (Expression const* extra_flags = findOption("extra_flags")) {
		std::optional<bigint> efs = ExprUtils::constValue(*extra_flags);
		if (efs) {
			pushExtraFlags = efs.value();
		} else {
			pushExtraFlags = [this, extra_flags] { acceptExpr(extra_flags); };
		}
	}

	std::variant<bool, std::function<void()>> pushBounce = true;
	if (Expression const* bounce = findOption("bounce")) {
		if (std::optional<bool> bounceValue = ExprUtils::constBool(*bounce)) {
			pushBounce = bounceValue.value();
		} else {
			pushBounce = [this, bounce] { acceptExpr(bounce); };
		}
	}

	std::function<void()> pushCurrency;
	if (Expression const* currencies = findOption("currencies")) {
		pushCurrency = [this, currencies] { acceptExpr(currencies); };
	}

	std::function<void(int bitSizeBuilder, int refSizeBuilder)> const pushBody = [&](int bitSizeBuilder,
																					 int refSizeBuilder) {
		auto constructor = to<ContractType>(type)->contractDefinition().constructor();
		if (constructor)
			ChainDataEncoder{&m_pusher}.createMsgBodyAndAppendToBuilder(
				convertArray(constructor->parameters()),
				ChainDataEncoder::
					calculateFunctionIDWithReason(constructor, ReasonOfOutboundMessage::RemoteCallInternal),
				{},
				bitSizeBuilder,
				refSizeBuilder,
				true
			);
		else {
			// body:(Either X ^X) and empty body
			m_pusher.stzeroes(1);
		}
	};

	std::function<void()> pushSendRawMsgFlag;
	if (Expression const* flag = findOption("flag")) {
		pushSendRawMsgFlag = [flag, this] { acceptExpr(flag); };
	}

	// stack: [prefixLength] stateInit hash
	deployNewContract(
		stateInitInfo.value(),
		pushWid,
		pushPrefix,
		pushValue,
		pushExtraFlags,
		pushBounce,
		pushCurrency,
		pushBody,
		pushSendRawMsgFlag,
		m_arguments.size()
	);
	// stack: destAddress
}

void FunctionCallCompiler::deployNewContract(
	StateInitInfo const& stateInitInfo,
	std::variant<int8_t, std::function<void()>> const& wid,
	std::function<void()> const& pushPrefix,
	std::variant<bigint, std::function<void()>> const& value,
	std::variant<bigint, std::function<void()>> const& extraFlags,
	std::variant<bool, std::function<void()>> const& pushBounce,
	std::function<void()> const& pushCurrency,
	std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
	std::function<void()> const& pushSendRawMsgFlag,
	int const argQty
) const {
	// stack: [prefixLength] stateInit hash

	std::map<int, std::function<void()>> exprs;

	std::map<int, std::string> constParams;

	if (pushBounce.index() == 0) {
		constParams[TvmConst::int_msg_info::bounce] = StrUtils::boolToBinaryString(std::get<0>(pushBounce));
	} else if (pushBounce.index() == 1) {
		exprs[TvmConst::int_msg_info::bounce] = std::get<1>(pushBounce);
	}

	if (pushCurrency) {
		exprs[TvmConst::int_msg_info::currency] = pushCurrency;
	}

	if (wid.index() == 0) {
		int8_t w = std::get<0>(wid);
		std::string binWID = "100";
		binWID += StrUtils::toBitString(w, 8, true).value();
		m_pusher << "NEWC";
		m_pusher << "STSLICECONST x" + StrUtils::binaryStringToSlice(binWID);
	} else {
		std::get<1>(wid)();
		m_pusher << "NEWC";
		m_pusher << "STSLICECONST x9_"; // addr_std$10 anycast:(Maybe Anycast) // 10 0 1 = 9
		m_pusher << "STI 8";			// workchain_id:int8
	}

	// [prefixLength] stateInit hash builder
	if (pushPrefix) {
		// prefixLength stateInit hash builder
		int const stackSize = m_pusher.stackSize();

		pushPrefix();
		// prefixLength stateInit hash builder prefix
		m_pusher.ensureSize(stackSize + 1);
		m_pusher.blockSwap(1, 1);
		// prefixLength stateInit hash prefix builder
		m_pusher.pushS(4);
		// prefixLength stateInit hash prefix builder prefixLength
		m_pusher << "STUX";
		// prefixLength stateInit hash builder
		m_pusher.blockSwap(1, 1);
		// prefixLength stateInit builder hash
		m_pusher.blockSwap(1, 3);
		// stateInit builder hash prefixLength
		m_pusher.pushInt(256);
		// stateInit builder hash prefixLength 256
		m_pusher << "SUBR";
		// stateInit builder hash 256-prefixLength
		m_pusher.pushS(0);
		// stateInit builder hash 256-prefixLength 256-prefixLength
		m_pusher << "POW2";
		// stateInit builder hash 256-prefixLength 2**(256-prefixLength)
		m_pusher.exchange(1, 2);
		// stateInit builder 256-prefixLength hash 2**(256-prefixLength)
		m_pusher << "MOD";
		// stateInit builder 256-prefixLength hash%mod
		m_pusher.blockSwap(2, 1);
		// stateInit hash%mod builder 256-prefixLength
		m_pusher << "STUX";
		// stateInit builder

		m_pusher.ensureSize(stackSize - 2);
	} else {
		m_pusher << "STU 256"; // address:bits256
	}


	bool isDestBuilder = !m_isCurrentResultNeeded;
	if (!isDestBuilder) {
		m_pusher << "BTOS";
	}

	// stack: arg[n-1], ..., arg[1], arg[0], stateInit, destAddress
	m_pusher.blockSwap(argQty + 1, 1);
	// stack:  destAddress, arg[n-1], ..., arg[1], arg[0], stateInit
	int destAddressStack = m_pusher.stackSize() - 1 - argQty;

	std::function<void()> pushValue;
	if (value.index() == 0) {
		constParams[TvmConst::int_msg_info::tons] = StrUtils::tonsToBinaryString(std::get<0>(value));
	} else {
		pushValue = [value] { std::get<1>(value)(); };
	}

	std::function<void()> pushExtraFlags;
	if (extraFlags.index() == 0) {
		constParams[TvmConst::int_msg_info::extra_flags] = StrUtils::tonsToBinaryString(std::get<0>(extraFlags));
	} else {
		pushExtraFlags = [extraFlags] { std::get<1>(extraFlags)(); };
	}

	exprs[TvmConst::int_msg_info::dest] = [&] {
		int stackIndex = m_pusher.stackSize() - destAddressStack;
		m_pusher.pushS(stackIndex);
	};

	std::function<std::pair<int, int>()> appendEitherStateInit = [&] {
		// init:(Maybe (Either StateInit ^StateInit))
		if (stateInitInfo.isBuilder) {
			m_pusher.stzeroes(1); // stateInit builder
			m_pusher << "STB";
			return std::pair<int, int>{stateInitInfo.bits + 1, stateInitInfo.refs};
		}

		m_pusher.stones(1); // stateInit builder
		m_pusher << "STREF";
		return std::pair<int, int>{1, 1};
	};

	// stack: stateInit hash
	std::set<int> isParamOnStack;
	for (auto& [param, expr]: exprs | std::views::reverse) {
		isParamOnStack.insert(param);
		expr();
	}

	m_pusher.sendMessage(
		isParamOnStack,
		constParams,
		appendBody,
		appendEitherStateInit,
		pushSendRawMsgFlag,
		StackPusher::MsgType::Internal,
		isDestBuilder,
		pushValue,
		pushExtraFlags
	);
	// stack: destAddress
}

bool FunctionCallCompiler::checkNewExpression() const {
	auto functionCallOptions = to<FunctionCallOptions>(&m_functionCall.expression());
	if (functionCallOptions != nullptr) {
		auto newExpression = to<NewExpression>(&functionCallOptions->expression());
		if (newExpression != nullptr) {
			createNewContract();
			return true;
		}
	}

	if (to<NewExpression>(&m_functionCall.expression()) == nullptr) {
		return false;
	}
	if (m_retType->category() == Type::Category::Contract) {
		cast_error(m_functionCall, R"(Use options: "stateInit", "value", "flag", etc.)");
	}

	creatArrayWithDefaultValue();
	return true;
}

void FunctionCallCompiler::creatArrayWithDefaultValue() const {
	std::optional<bigint> num = ExprUtils::constValue(*m_arguments.at(0));
	if (num.has_value() && num.value() == 0) {
		auto arrayType = to<ArrayType>(m_retType);
		m_pusher.pushDefaultValue(arrayType);
		return;
	}

	if (*m_functionCall.annotation().isPure) {
		pushArgs();
		SourceReference sr =
			SourceReferenceExtractor::extract(*GlobalParams::g_charStreamProvider, &m_functionCall.location());
		std::string const computeName = "new_array_line_" +
										toString(sr.position.line) +
										"_column_" +
										toString(sr.position.column) +
										"_ast_id_" +
										toString(m_functionCall.id());
		m_pusher.computeConstCell(computeName);
		m_pusher << "TUPLE 2";
		m_pusher.ctx().addNewArray(computeName, &m_functionCall);
		return;
	}

	honestArrayCreation(false);
}

void FunctionCallCompiler::honestArrayCreation(bool onlyDict) const {
	int const stackSize = m_pusher.stackSize();
	auto arrayType = to<ArrayType>(m_retType);
	IntegerType const& key = getArrayKeyType();
	Type const* arrayBaseType = arrayType->baseType();

	pushArgAndConvert(0);															  // N
	DataType const& dataType = m_pusher.pushDefaultValueForDict(&key, arrayBaseType); // N value
	m_pusher.pushInt(0);															  // N value iter
	m_pusher << "NULL";																  // N value iter dict
	m_pusher.pushS(3);																  // N value iter dict N

	solAssert(stackSize + 5 == m_pusher.stackSize(), "");
	m_pusher.fixStack(-1); // fix stack: drop replay iterator
	solAssert(stackSize + 4 == m_pusher.stackSize(), "");
	{
		// N value iter dict
		m_pusher.startContinuation();
		m_pusher.pushS(2);										 // N value iter dict value
		m_pusher.pushS(2);										 // N value iter dict value iter
		m_pusher << "INC";										 // N value iter dict value iter++
		m_pusher.exchange(3);									 // N value iter++ dict value iter
		m_pusher.rot();											 // N value iter++ value iter dict
		m_pusher.setDict(key, *arrayType->baseType(), dataType); // N value iter++ dict'
		m_pusher.endContinuation();
	}
	m_pusher.repeat(false);
	solAssert(stackSize + 4 == m_pusher.stackSize(), "");
	// N value iter dict
	if (onlyDict) {
		m_pusher.dropUnder(3, 1);
		// dict
	} else {
		m_pusher.dropUnder(2, 1); // N dict
		m_pusher << "TUPLE 2";
	}
	solAssert(stackSize + 1 == m_pusher.stackSize(), "");
}

bool FunctionCallCompiler::structMethodCall() const {
	if (m_memberAccess->memberName() != "unpack") {
		return false;
	}
	acceptExpr(&m_memberAccess->expression());
	auto structType = to<StructType>(getType(&m_memberAccess->expression()));
	int memberQty = structType->structDefinition().members().size();
	m_pusher.untuple(memberQty);
	return true;
}

void FunctionCallCompiler::pushArgWithoutConvertion() const {
	for (auto const& callIndex: m_declarationIndex) {
		acceptExpr(m_arguments.at(callIndex).get());
	}
}

void FunctionCallCompiler::pushArgAndConvertToCommon() const { pushArgs(false, true); }

std::pair<int, int> FunctionCallCompiler::encodeStateInitAndHash(
	std::map<StateInitMembers, std::function<void()>> const& exprs,
	bool savePrefixLength
) const {
	solAssert(!exprs.contains(StateInitMembers::Special), "");
	solAssert(!exprs.contains(StateInitMembers::Library), "");
	solAssert(exprs.contains(StateInitMembers::Code), "Code must be present");
	solAssert(exprs.contains(StateInitMembers::Data), "Data must be present");

	// _ split_depth:(Maybe (## 5)) special:(Maybe TickTock)
	// code:(Maybe ^Cell) data:(Maybe ^Cell)
	// library:(HashmapE 256 SimpleLib) = StateInit;

	bool const hasPrefixLength = exprs.contains(StateInitMembers::PrefixLength);
	if (hasPrefixLength && savePrefixLength) {
		exprs.at(StateInitMembers::PrefixLength)();
	}
	// stack: [prefixLength]
	exprs.at(StateInitMembers::Data)();
	// stack: [prefixLength] data
	exprs.at(StateInitMembers::Code)();
	// stack: [prefixLength] data code
	if (hasPrefixLength) {
		if (savePrefixLength)
			m_pusher.pushS(2);
		else
			exprs.at(StateInitMembers::PrefixLength)();
	}

	// stack: [prefixLength] data code [prefixLength]
	m_pusher << "NEWC";
	// stack: [prefixLength] data code [prefixLength] builder

	int bitQty = 0;
	if (hasPrefixLength) {
		m_pusher.stones(1);
		m_pusher << "STU 5";
		bitQty += 6;
	} else {
		m_pusher.stzeroes(1);
		++bitQty;
	}
	m_pusher.stzeroes(1);		  // special:(Maybe TickTock)
	m_pusher.stones(1);			  // code:(Maybe ^Cell)
	m_pusher.stones(1);			  // data:(Maybe ^Cell)
	m_pusher << "STSLICECONST 0"; // library:(HashmapE 256 SimpleLib)
	bitQty += 4;
	m_pusher << "STREF"; // store code
	m_pusher << "STREF"; // store data
	// stack: [prefixLength] stateInit

	return {bitQty, 2};
}


void FunctionCallCompiler::pushArgs(bool reversed, bool doConvertToCommonType) const {
	auto func = [&](ASTPointer<Expression const> const& e, int declarationIndex) {
		acceptExpr(e.get());
		Type const* targetType;
		if (doConvertToCommonType) {
			if (auto tuple = to<TupleType>(m_retType))
				// See math.muldiv
				targetType = tuple->components().at(0);
			else
				// See math.max
				targetType = m_retType;
		} else {
			targetType = m_funcType->parameterTypes().at(declarationIndex);
		}
		m_pusher.convert(targetType, e->annotation().type);
	};

	if (reversed) {
		int declarationIndex = argQty - 1;
		for (auto const& callIndex: m_declarationIndex | std::views::reverse) {
			func(m_arguments.at(callIndex), declarationIndex);
			--declarationIndex;
		}
	} else {
		int declarationIndex = 0;
		for (auto const& callIndex: m_declarationIndex) {
			func(m_arguments.at(callIndex), declarationIndex);
			++declarationIndex;
		}
	}
}

void FunctionCallCompiler::pushArgAndConvert(int declarationIndex) const {
	int callIndex = m_callIndex.at(declarationIndex);
	solAssert(callIndex != -1);
	ASTPointer<Expression const> const& arg = m_arguments.at(callIndex);
	acceptExpr(arg.get());
	Type const* targetType = m_funcType->parameterTypes().at(declarationIndex);
	m_pusher.convert(targetType, arg->annotation().type);
}

void FunctionCallCompiler::pushArgAndConvert(int callIndex, std::string const& name) const {
	for (std::size_t i = 0;; ++i) {
		if (name == *m_names.at(i)) {
			solAssert(static_cast<std::size_t>(callIndex) == i);
			break;
		}
	}

	int declarationIndex = m_declarationIndex.at(callIndex);
	solAssert(declarationIndex != -1);

	ASTPointer<Expression const> const& arg = m_arguments.at(callIndex);
	acceptExpr(arg.get());
	Type const* targetType = m_funcType->parameterTypes().at(declarationIndex);
	m_pusher.convert(targetType, arg->annotation().type);
}

void FunctionCallCompiler::pushExprAndConvert(Expression const* expr, Type const* targetType) const {
	acceptExpr(expr);
	m_pusher.convert(targetType, expr->annotation().type);
}

void FunctionCallCompiler::pushAllArgsAndConvertToMobileType() const {
	for (std::size_t i = 0; i < m_arguments.size(); ++i) {
		pushArgConvertToMobileType(i);
	}
}

void FunctionCallCompiler::pushArgConvertToMobileType(int callIndex) const {
	int declarationIndex = m_declarationIndex.at(callIndex);
	solAssert(declarationIndex != -1);

	ASTPointer<Expression const> const& arg = m_arguments.at(callIndex);
	acceptExpr(arg.get());

	Type const* argType = arg->annotation().type;
	m_pusher.convert(argType->mobileType(), argType);
}

void FunctionCallCompiler::acceptExpr(Expression const* expr) const { m_exprCompiler.compileNewExpr(expr); }

void FunctionCallCompiler::compileLog() const {
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

Expression const* FunctionCallCompiler::findOption(std::string const& name) const {
	auto functionOptions = to<FunctionCallOptions>(&m_functionCall.expression());
	if (!functionOptions)
		return {};
	std::vector<ASTPointer<ASTString>> const& optionNames = functionOptions->names();
	auto iter = std::ranges::find_if(optionNames, [&](auto const& el) { return *el == name; });
	if (iter == optionNames.end())
		return {};
	size_t index = iter - optionNames.begin();
	return functionOptions->options().at(index).get();
}

void FunctionCallCompiler::cellBitRefQty(bool forCell) const {
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
