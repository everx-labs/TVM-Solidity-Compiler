/*
 * Copyright (C) 2019-2026 EverX. All Rights Reserved.
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
 * TVM constants and runtime error codes
 */

#pragma once

#include <cstdint>
#include <string>

struct TvmConst {
	// F12_n — CALL n for 0 ≤ n < 2**14 ( – n), an encoding of CALL n for larger values of n.
	constexpr static int MaxCallN = 1 << 14;

	constexpr static int CellBitLength = 1023;
	constexpr static int ArrayKeyLength = 32;
	constexpr static int MaxPushSliceBitLength = 997; // max count of bits in an arg of the opcode PUSHSLICE
	constexpr static int MaxSTSLICECONST = 7 * 8;	  // STSLICECONST xSSSS;    SSSS.length() <= MaxSTSLICECONST / 4
	constexpr static int MAX_HASH_MAP_INFO_ABOUT_KEY = 2 + 10; // hml_long$10 + log2(1023)

	constexpr static int IterStackOptQty = 10;
	constexpr static int TvmTupleLen = 255;

	constexpr static int CONTINUE_FLAG = 1;
	constexpr static int RETURN_FLAG = 4;

	constexpr static int EXTRA_FLAG_SIZE = 8;

	// https://tonviewer.com/config#18
	constexpr static int CELL_PRICE_PS = 500;

	inline static std::string const ON_BOUNCED_MESSAGE = "onBouncedMessage";

	struct C4 {
		// length of key in dict c4
		constexpr static int KeyLength = 64;
		constexpr static int PersistenceMembersStartIndex = 1;
	};
	struct C7 {
		constexpr static int TvmPubkey = 2;
		constexpr static int ReplayProtTime = 3;
		constexpr static int ResponsibleCallbackFunctionId = 4;
		constexpr static int MsgPubkey = 5;
		constexpr static int ConstructorFlag = 6;
		constexpr static int ResponsibleMessageFlag = 7;
		constexpr static int ResponsibleParams = 8;
		constexpr static int FirstIndexForVariables = 10;
	};
	struct SENDRAWMSG {
		constexpr static int DefaultFlag = 0;
		constexpr static int DestroyAccount = 32;
		constexpr static int CarryAllMoney = 128;
		constexpr static int SelfDestruct = CarryAllMoney | DestroyAccount;
	};
	struct Message {
		constexpr static int functionIdLength = 32;
		constexpr static int timestampLength = 64;
		struct ReplayProtection {
			constexpr static int Interval = 30 * 60 * 1000; // 30 min = 30 * 60 * 1000 millisecond;
		};
		constexpr static int MajorAbiVersion = 2;
	};
	struct Abi {
		// ## Signing Algorithm
		// 1. ABI serialization generates bag of cells containing header parameters, function ID and function
		// parameters. 591 free bits are reserved in the root cell for destination address ([the maximum size of
		// address](#address)).
		constexpr static int MaxOptionalSignatureLength = 591;
	};
	struct RuntimeException {
		constexpr static int NoFunctionInTopSelector = 11;
		constexpr static int NoFunctionInContractLibrary = 39;
		constexpr static int BadSignature = 40;
		constexpr static int ArrayIndexOutOfRange = 50;
		constexpr static int ConstructorIsCalledTwice = 51;
		constexpr static int ReplayProtection = 52;
		constexpr static int PopFromEmptyArray = 54;
		constexpr static int MessageIsExpired = 57;
		constexpr static int MessageHasNoSignButHasPubkey = 58;
		constexpr static int NoFallback = 60;
		constexpr static int GetOptionalException = 63;
		constexpr static int NoConfigParam20Or21 = 68;
		constexpr static int Exponent00 = 69;
		constexpr static int TooLongSubstr = 70;
		constexpr static int WrongValueOfEnum = 73;
		constexpr static int CallThatWasBeforeCtorCall = 76;
		constexpr static int BadVariant = 77;
		constexpr static int IsNaN = 80;
		constexpr static int DefaultError = 100;
	};
	struct FunctionId {
		constexpr static uint32_t DefaultValueForFunctionType = (1 << 18) - 1;
	};
	struct int_msg_info {
		constexpr static int bounce = 1;
		constexpr static int dest = 4;
		constexpr static int tons = 5;
		constexpr static int currency = 6;
		constexpr static int extra_flags = 7;
	};
	struct ext_msg_info {
		constexpr static int src = 0;
		constexpr static int dest = 1;
	};
};
