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
 * TVM constants and runtime error codes
 */

#pragma once
#include <string>

namespace TvmConst {
	namespace C4 {
		// length of key in dict c4
		const int KeyLength = 64;
		const int PersistenceMembersStartIndex = 1;
	}
	namespace C7 {
		const int ReturnParams = 4;
		namespace ReturnParam {
			const int Bounce = 1;
			const int Value = 2;
			const int Currencies = 3;
			const int Flag = 4;
			const int CallbackFunctionId = 5;
		}
		const int TempFunctionReturnBuffer = 8;
		constexpr int FirstIndexForVariables = 10;
	}
	namespace SENDRAWMSG {
		const int DefaultFlag = 0;
		const int DestroyAccount = 32;
		const int CarryAllMoney = 128;
		const int SelfDestruct = CarryAllMoney | DestroyAccount;
	}
	namespace Message {
		const int DefaultMsgValue = 10'000 * 1'000; // 10'000 gas * 1'000 ton / gas (in workchain)
		const int functionIdLength = 32;
		const int timestampLength = 64;
		namespace ReplayProtection {
			const int Interval = 30 * 60 * 1000; // 30 min = 30 * 60 * 1000 millisecond;
		}
	}
	const int CellBitLength = 1023;
	const int ArrayKeyLength = 32;
	const int MaxPushSliceLength = 249; // PUSHSLICE xSSSS;    SSSS.length() <= MaxPushSliceLength
	const int MaxSTSLICECONST = 7 * 8; // STSLICECONST xSSSS;    SSSS.length() <= MaxSTSLICECONST
	const int ExtInboundSrcLength = 72 + 9 + 2 + 3; // src field of external inbound message. Contains addr_extern with
													// abi version (8 bit), callback id (32 bit), on error id (32 bit),
													// header mask (3 bit).

	namespace RuntimeException {
		const int BadSignature = 40;
		const int ArrayIndexOutOfRange = 50;
		const int ConstructorIsCalledTwice  = 51;
		const int ReplayProtection  = 52;
		const int AddressUnpackException = 53;
		const int PopFromEmptyArray = 54;
		const int InsertPubkeyException = 55;
		const int MessageIsExpired = 57;
		const int MessageHasNoSignButHasPubkey = 58;
		const int NoFallback = 60;
		const int NoPubkeyInC4 = 61;
		const int MigratePubkey = 62;
		const int GetOptionalException = 63;
		const int MsgWithKeyButNoSign = 64;
		const int BadFunctionIdOfFuncCall = 65;
		const int FormatWrongWidth = 66;
		const int WrongWid = 67;
		const int NoConfigParam20Or21 = 68;
		const int Exponent00 = 69;
		const int TooLongSubstr = 70;
	}

	namespace FunctionId {
		const uint32_t DefaultValueForFunctionType = 0xFFFFFFFF;
	}

	namespace int_msg_info {
		const int ihr_disabled = 0;
		const int bounce = 1;
		const int dest = 4;
		const int tons = 5;
		const int currency = 6;
	}

	namespace ext_msg_info {
		const int src = 0;
		const int dest = 1;
	}

	namespace Attributes {
		const std::string MuteStructWarning{"maybe_unsaved"};
	}
}
