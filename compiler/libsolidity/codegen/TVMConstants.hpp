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
		const int TempFunctionReturnBuffer = 8;
		constexpr int FirstIndexForVariables = 10;
	}
	namespace SENDRAWMSG {
		const int DefaultFlag = 1;
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
		namespace Exception {
			const int ConstructorIsCalledTwice  = 51;
			const int ReplayProtection  = 52;
			const int AddressUnpackException = 53;
			const int InsertPubkeyException = 55;
			const int GetOptionalException = 63;
		}
	}
	const int CellBitLength = 1023;
	const int ArrayKeyLength = 32;
	const int MaxPushSliceLength = 249; // PUSHSLICE xSSSS;    SSSS.length() <= MaxPushSliceLength
	const int MaxSTSLICECONST = 7 * 8; // STSLICECONST xSSSS;    SSSS.length() <= MaxSTSLICECONST

	namespace RuntimeException {
		const int ArrayIndexOutOfRange = 50;
		const int PopFromEmptyArray = 54;
		const int MessageIsExpired = 57;
	}

	namespace FunctionId {
		const int First = 3;
		const int64_t Last = 0xFFFFFFFD;
	}

	namespace int_msg_info {
		const int ihr_disabled = 0;
		const int bounce = 1;
		const int dest = 4;
		const int tons = 5;
		const int currency = 6;
	}

	namespace ext_msg_info {
		const int dest = 1;
	}

	namespace Attributes {
		const std::string MuteStructWarning{"maybe_unsaved"};
	}
}
