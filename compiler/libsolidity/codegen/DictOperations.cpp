/*
 * Copyright (C) 2021-2023 EverX. All Rights Reserved.
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

#include "DictOperations.hpp"
#include "TVMPusher.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMConstants.hpp"

using namespace solidity;
using namespace solidity::frontend;
using namespace solidity::util;

DictOperation::DictOperation(StackPusher& pusher, Type const& keyType, Type const& valueType) :
		pusher{pusher},
		keyType{keyType},
		keyLength{dictKeyLength(&keyType)},
		valueType{valueType} {
}

void DictMinMax::minOrMax(bool saveOrigKeyAndNoTuple) {
	// stack: dict
	pusher.pushInt(dictKeyLength(&keyType)); // dict nbits

	const bool haveKey = true;
	bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
	dictOpcode = "DICT" + typeToDictChar(&keyType) + (isMin? "MIN" : "MAX") + (isInRef? "REF" : "");

	pusher.startOpaque();
	pusher.pushAsym(dictOpcode); // (value, key, -1) or 0
	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			haveKey,
			isInRef,
			StackPusher::DecodeType::DecodeValueOrPushNull,
			saveOrigKeyAndNoTuple
	);
	if (saveOrigKeyAndNoTuple)
		pusher.endOpaque(2, 3);
	else
		pusher.endOpaque(2, 1);
}

void DictPrevNext::prevNext(bool saveOrigKeyAndNoTuple) {
	// stack: index dict nbits
	std::string dictOpcode = std::string{"DICT"} + typeToDictChar(&keyType) + "GET";
	if (oper == "next"){
		dictOpcode += "NEXT";
	} else if (oper == "prev") {
		dictOpcode += "PREV";
	} else if (oper == "nextOrEq") {
		dictOpcode += "NEXTEQ";
	} else if (oper == "prevOrEq") {
		dictOpcode += "PREVEQ";
	} else {
		solUnimplemented("");
	}

	int ss = pusher.stackSize();
	pusher.startOpaque();
	pusher.pushAsym(dictOpcode); // value key -1 or 0

	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			true,
			false,
			StackPusher::DecodeType::DecodeValueOrPushNull,
			saveOrigKeyAndNoTuple
	);
	pusher.endOpaque(3, saveOrigKeyAndNoTuple ? 3 : 1);

	if (saveOrigKeyAndNoTuple) {
		pusher.ensureSize(ss - 3 + 3);
	} else {
		pusher.ensureSize(ss - 3 + 1);
	}
}

GetFromDict::GetFromDict(StackPusher &pusher, const Type &keyType, const Type &valueType,
						 const GetDictOperation op,
						 std::optional<DataType> inputValueType) :
		DictOperation{pusher, keyType, valueType},
		op{op},
		inputValueType{inputValueType}
{
	bool hasInputValue = isIn(op,GetDictOperation::GetSetFromMapping, GetDictOperation::GetAddFromMapping,
		GetDictOperation::GetReplaceFromMapping);
	solAssert(hasInputValue == inputValueType.has_value());
}

void GetFromDict::getDict() {
	pusher.pushInt(keyLength); // push keyLength on stack
	// if op == GetSetFromMapping than stack: value key dict keyLength
	// else                            stack: key dict keyLength

	const int saveStack = pusher.stackSize();
	std::string opcode = "DICT" + typeToDictChar(&keyType);
	int take{};
	int ret{};
	pusher.startOpaque();
	switch (op) {
	case GetDictOperation::GetSetFromMapping:
	case GetDictOperation::GetAddFromMapping:
	case GetDictOperation::GetReplaceFromMapping: {
		take = 4;
		ret = 2;

		if (op == GetDictOperation::GetSetFromMapping)
			opcode += "SETGET";
		else if (op == GetDictOperation::GetAddFromMapping)
			opcode += "ADDGET";
		else if (op == GetDictOperation::GetReplaceFromMapping)
			opcode += "REPLACEGET";
		else
			solUnimplemented("");

		bool didUseOpcodeWithRef = false;
		switch (inputValueType.value()) {
		case DataType::Builder:
			opcode += "B";
			break;
		case DataType::Cell:
			didUseOpcodeWithRef = true;
			opcode += "REF";
			break;
		case DataType::Slice:
			break;
		}

		pusher.pushAsym(opcode);

		StackPusher::DecodeType decodeType{};
		if (op == GetDictOperation::GetAddFromMapping)
			decodeType = StackPusher::DecodeType::PushNullOrDecodeValue;
		else if (
			op == GetDictOperation::GetSetFromMapping ||
			op == GetDictOperation::GetReplaceFromMapping
		)
			decodeType = StackPusher::DecodeType::DecodeValueOrPushNull;
		else
			solUnimplemented("");

		int ss = pusher.stackSize();
		pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, didUseOpcodeWithRef, decodeType);

		solAssert(ss == pusher.stackSize(), "");
		break;
	}
	case GetDictOperation::Exist: {
		take = 3;
		ret = 1;
		opcode += "GET";
		pusher.pushAsym(opcode);
		checkExist();
		break;
	}
	case GetDictOperation::Fetch:
	case GetDictOperation::GetFromArray:
	case GetDictOperation::GetFromMapping: {
		take = 3;
		ret = 1;
		opcode += "GET";
		bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
		if (isInRef) {
			opcode += "REF";
		}
		pusher.pushAsym(opcode);
		if (op == GetDictOperation::GetFromArray) {
			pusher._throw("THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
		}
		StackPusher::DecodeType decodeType{};
		if (op == GetDictOperation::Fetch) {
			decodeType = StackPusher::DecodeType::DecodeValueOrPushNull;
		} else if (op == GetDictOperation::GetFromArray) {
			decodeType = StackPusher::DecodeType::DecodeValue;
		} else if (op == GetDictOperation::GetFromMapping) {
			decodeType = StackPusher::DecodeType::DecodeValueOrPushDefault;
		} else {
			solUnimplemented("");
		}
		pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, isInRef, decodeType);
		break;
	}
	case GetDictOperation::GetDelFromMapping: {
		take = 3;
		ret = 2;
		opcode += "DELGET";
		bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
		if (isInRef) {
			opcode += "REF";
		}
		pusher.pushAsym(opcode);
		StackPusher::DecodeType decodeType = StackPusher::DecodeType::DecodeValueOrPushNull;
		pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, isInRef, decodeType);
		break;
	}
	}
	pusher.endOpaque(take, ret);

	pusher.ensureSize(saveStack - take + ret);
}

void GetFromDict::checkExist() {
	pusher.startOpaque();
	pusher.pushAsym("NULLSWAPIFNOT");
	pusher.dropUnder(1, 1);
	pusher.endOpaque(1, 1, true);
}

DictSet::DictSet(StackPusher &pusher, const Type &keyType, const Type &valueType, const DataType &dataType,
				 SetDictOperation operation) :
		DictOperation{pusher, keyType, valueType},
		dataType{dataType},
		operation{operation}
{

}

void DictSet::dictSet() {
	// stack: value key dict
	int keyLength = dictKeyLength(&keyType);
	pusher.pushInt(keyLength);
	// stack: value index dict keyBitLength
	opcode = "DICT" + typeToDictChar(&keyType);
	switch (operation) {
		case SetDictOperation::Set:
			opcode += "SET";
			break;
		case SetDictOperation::Replace:
			opcode += "REPLACE";
			break;
		case SetDictOperation::Add:
			opcode += "ADD";
			break;
	}

	switch (dataType) {
		case DataType::Builder:
			opcode += "B";
			break;
		case DataType::Cell:
			opcode += "REF";
			break;
		case DataType::Slice:
			break;
	}

	switch (operation) {
		case SetDictOperation::Set:
			pusher << opcode;
			break;
		case SetDictOperation::Replace:
		case SetDictOperation::Add:
			pusher << opcode;
			break;
	}
}

DelMinOrMax::DelMinOrMax(StackPusher &pusher, const Type &keyType, const Type &valueType, bool isDelMin,
						 const MemberAccess *memberAccess) :
		DictOperation{pusher, keyType, valueType},
		isDelMin{isDelMin},
		memberAccess{memberAccess},
		ec{new TVMExpressionCompiler{pusher}}
{

}

void DelMinOrMax::delMinOrMax() {
	bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
	opcode = "DICT" + typeToDictChar(&keyType) + "REM" + (isDelMin? "MIN" : "MAX") + (isInRef? "REF" : "");
	stackSize = pusher.stackSize();

	lValueInfo = ec->expandLValue(&memberAccess->expression(), true); // lValue... map
	pusher.pushInt(keyLength); // dict nbits

	pusher.startOpaque();
	pusher.pushAsym(opcode); //  D value key -1
	pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, true, isInRef, StackPusher::DecodeType::DecodeValueOrPushNull);
	pusher.endOpaque(2, 2);

	// mapLValue... D optPair
	const int cntOfValuesOnStack = pusher.stackSize() - stackSize;
	pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optPair mapLValue... map
	ec->collectLValue(lValueInfo, true, false); // value key
}
