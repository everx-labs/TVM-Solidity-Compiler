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
 * @date 2021
 */

#include "DictOperations.hpp"
#include "TVMPusher.hpp"
#include "TVMExpressionCompiler.hpp"


DictOperation::DictOperation(StackPusherHelper& pusher, Type const& keyType, Type const& valueType) :
		pusher{pusher},
		keyType{keyType},
		keyLength{lengthOfDictKey(&keyType)},
		valueType{valueType},
		valueCategory{valueType.category()} {
}

void DictMinMax::minOrMax(bool saveOrigKeyAndNoTuple) {
	// stack: dict
	pusher.pushInt(lengthOfDictKey(&keyType)); // dict nbits

	const bool haveKey = true;
	bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
	dictOpcode = "DICT" + typeToDictChar(&keyType) + (isMin? "MIN" : "MAX") + (isInRef? "REF" : "");

	pusher.push(-2 + 2, dictOpcode); // (value, key, -1) or 0
	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			haveKey,
			isInRef,
			StackPusherHelper::DecodeType::DecodeValueOrPushNull,
			saveOrigKeyAndNoTuple
	);
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

	int ss = pusher.getStack().size();
	pusher.push(-3 + 2, dictOpcode); // value key -1 or 0

	pusher.recoverKeyAndValueAfterDictOperation(
			&keyType,
			&valueType,
			true,
			false,
			StackPusherHelper::DecodeType::DecodeValueOrPushNull,
			saveOrigKeyAndNoTuple
	);

	if (saveOrigKeyAndNoTuple) {
		pusher.getStack().ensureSize(ss - 3 + 3);
	} else {
		pusher.getStack().ensureSize(ss - 3 + 2 - 2 + 1);
	}
}

GetFromDict::GetFromDict(StackPusherHelper &pusher, const Type &keyType, const Type &valueType,
						 const GetDictOperation op,
						 const DataType &dataType) :
		DictOperation{pusher, keyType, valueType},
		op{op},
		dataType{dataType}
{

}

void GetFromDict::getDict() {
	pusher.pushInt(keyLength); // push keyLength on stack
	// if op == GetSetFromMapping than stack: value key dict keyLength
	// else                            stack: key dict keyLength

	const int saveStake = pusher.getStack().size();
	std::string opcode = "DICT" + typeToDictChar(&keyType);
	int stackDelta{};
	switch (op) {
		case GetDictOperation::GetSetFromMapping:
		case GetDictOperation::GetAddFromMapping:
		case GetDictOperation::GetReplaceFromMapping: {
			stackDelta = -4 + 2;
			if (op == GetDictOperation::GetSetFromMapping)
				opcode += "SETGET";
			else if (op == GetDictOperation::GetAddFromMapping)
				opcode += "ADDGET";
			else if (op == GetDictOperation::GetReplaceFromMapping)
				opcode += "REPLACEGET";
			else
				solUnimplemented("");

			bool didUseOpcodeWithRef = false;
			switch (dataType) {
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
			pusher.push(stackDelta, opcode);

			StackPusherHelper::DecodeType decodeType{};
			if (op == GetDictOperation::GetAddFromMapping)
				decodeType = StackPusherHelper::DecodeType::PushNullOrDecodeValue;
			else if (
					op == GetDictOperation::GetSetFromMapping ||
					op == GetDictOperation::GetReplaceFromMapping
			)
				decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushNull;
			else
				solUnimplemented("");
			int ss = pusher.getStack().size();
			pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, didUseOpcodeWithRef, decodeType);
			solAssert(ss == pusher.getStack().size(), "");
			break;
		}

		case GetDictOperation::Exist: {
			stackDelta = -3 + 1;
			opcode += "GET";
			pusher.push(stackDelta, opcode);
			checkExist();
			break;
		}
		case GetDictOperation::Fetch:
		case GetDictOperation::GetFromArray:
		case GetDictOperation::GetFromMapping: {
			stackDelta = -3 + 1;
			opcode += "GET";
			bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
			if (isInRef) {
				opcode += "REF";
			}
			pusher.push(stackDelta, opcode);
			if (op == GetDictOperation::GetFromArray) {
				pusher.push(0, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
			}
			StackPusherHelper::DecodeType decodeType{};
			if (op == GetDictOperation::Fetch) {
				decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushNull;
			} else if (op == GetDictOperation::GetFromArray) {
				decodeType = StackPusherHelper::DecodeType::DecodeValue;
			} else if (op == GetDictOperation::GetFromMapping) {
				decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushDefault;
			} else {
				solUnimplemented("");
			}
			pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, isInRef, decodeType);
			break;
		}
	}

	pusher.getStack().ensureSize(saveStake + stackDelta);
}

void GetFromDict::checkExist() {
	pusher.push(0, "DUP");
	pusher.startContinuation();
	pusher.push(0, "NIP");
	pusher.endContinuation();
	pusher.push(0, "IF");
}

DictSet::DictSet(StackPusherHelper &pusher, const Type &keyType, const Type &valueType, const DataType &dataType,
				 SetDictOperation operation) :
		DictOperation{pusher, keyType, valueType},
		dataType{dataType},
		operation{operation}
{

}

void DictSet::dictSet() {
	// stack: value key dict
	int keyLength = lengthOfDictKey(&keyType);
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
			pusher.push(-4 + 1, opcode);
			break;
		case SetDictOperation::Replace:
		case SetDictOperation::Add:
			pusher.push(-4 + 2, opcode);
			break;
	}
}

DelMinOrMax::DelMinOrMax(StackPusherHelper &pusher, const Type &keyType, const Type &valueType, bool isDelMin,
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
	stackSize = pusher.getStack().size();

	lValueInfo = ec->expandLValue(&memberAccess->expression(), true, false); // lValue... map
	pusher.pushInt(keyLength); // dict nbits
	pusher.push(-2 + 3, opcode); //  D value key -1

	pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, true, isInRef, StackPusherHelper::DecodeType::DecodeValueOrPushNull);

	// mapLValue... D optPair
	const int cntOfValuesOnStack = pusher.getStack().size() - stackSize;
	pusher.blockSwap(cntOfValuesOnStack - 1, 1); // optPair mapLValue... map
	ec->collectLValue(lValueInfo, true, false); // value key
}
