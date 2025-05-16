pragma tvm-solidity >= 0.72.0;
pragma ignoreIntOverflow;

struct DigitStack {
    uint digit;
    optional(DigitStack) tail;
}

function getUnchecked(optional(DigitStack) x) assembly pure returns (DigitStack) {
}

function __QAND(qint a, qint b) assembly pure returns (qint) {
    "QAND"
}

function __QOR(qint a, qint b) assembly pure returns (qint) {
    "QOR"
}

contract stdlib {
    function __replayProtection(uint64 msg_timestamp) view private {
        require(tvm.replayProtTime() < msg_timestamp, 52);
        require(msg_timestamp < block.timestamp * 1000 + tvm.replayProtInterval(), 52);
        tvm.setReplayProtTime(msg_timestamp);
    }

    function __tonToGas(uint128 _ton, bool isMasterChain) private pure returns(uint128) {
        return math.muldiv(_ton, 65536, __gasGasPrice(isMasterChain)); // round down
    }

    function __gasToTon(uint128 gas, bool isMasterChain) private pure returns(uint128) {
        return math.muldivc(gas, __gasGasPrice(isMasterChain), 65536); // round up
    }

    function __gasGasPrice(bool isMasterChain) private pure returns(uint64 gasPrice) {
        optional(TvmCell) optCell = tvm.rawConfigParam(isMasterChain? int32(20) : int32(21));
        require(optCell.hasValue(), 68);
        TvmSlice s = optCell.get().toSlice();
        (, , , , gasPrice) = s.load(uint8, uint64, uint64, uint8, uint64);
    }

    // a ** n
    function __exp(int a, uint n) private pure returns (int) {
        require(a | n != 0, 69);
        int res = 1;
        while (n != 0) {
            if (n % 2 != 0)
                res *= a;
            a *= a;
            n >>= 1;
        }
        return res;
    }

    function __qexp(qint _a, quint _n) private pure returns (qint) {
        if (_n.isNaN() ||
            (_n.get() == 0 && !_a.isNaN() && _a.get() == 0)
        )
            return NaN;
        uint n = _n.get();
        qint res = 1;
        while (n != 0) {
            if (n % 2 != 0)
                res *= _a;
            _a *= _a;
            n >>= 1;
        }
        return res;
    }

    function __parseInteger(uint integer, uint8 modulo) internal pure returns (optional(DigitStack), uint8) {
        if (integer == 0) {
            return (DigitStack(0, null), 1);
        }
        optional(DigitStack) digits;
        uint8 length = 0;
        while (integer != 0) {
            uint dig;
            (integer, dig) = math.divmod(integer, modulo);
            digits = DigitStack(dig, digits);
            ++length;
        }
        return (digits, length);
    }

    // StringBuilder constructor
    function __createStringBuilder() pure private returns (stack(TvmBuilder)) {
        stack(TvmBuilder) st;
        st.push(TvmBuilder());
        return st;
    }

    // <StringBuilder>.toString()
    function __makeString(stack(TvmBuilder) st) pure private returns(TvmCell) {
        TvmBuilder res = st.pop();
        while (!st.empty()) {
            TvmBuilder b = st.pop();
            b.storeRef(res);
            res = b;
        }
        return res.toCell();
    }

    // <StringBuilder>.append(<bytes1>)
    function __appendBytes1(stack(TvmBuilder) st, bytes1 ch) pure private returns(stack(TvmBuilder)) {
        if (st.top().remBits() < 8)
            st.push(TvmBuilder());
        st.top().store(ch);
        return st; 
    }

    // <StringBuilder>.append(<bytes1>, <uint31>)
    function __appendBytes1NTimes(stack(TvmBuilder) st, bytes1 ch, uint31 n) pure private returns(stack(TvmBuilder)) {
        repeat(n) {
            st = __appendBytes1(st, ch);
        }
        return st;
    }

    // Append slice in hex format with tag _ if needs
    function __appendSliceDataAsHex(stack(TvmBuilder) st, TvmSlice data) pure private returns(stack(TvmBuilder)) {
        while (data.bits() >= 4) {
            uint9 bits = uint9(math.min(data.bits() / 4 * 4, 256));
            uint value = data.loadUint(bits);
            st = __convertIntToHexString(st, value, bits / 4, true, true);
        }
        if (data.bits() != 0) {
            uint10 shift = 4 - data.bits();
            int value = data.loadUint(uint9(data.bits()));
            value = (value << 1) + 1;
            value <<= (shift - 1);
            st = __convertIntToHexString(st, value, 1, true, true);
            st = __appendBytes1(st, bytes1("_"));
        }
        return st;
    }

    function __appendSliceToStringBuilder(stack(TvmBuilder) st, TvmSlice s) pure private returns(stack(TvmBuilder)) {
        uint10 restBits = st.top().remBits() - 7; // 1023 - 7 = 1016 = 127 * 8
        TvmSlice ss = s.loadSlice(math.min(restBits, s.bits()), 0);
        st.top().store(ss);
        if (!s.empty()) {
            TvmBuilder b;
            b.store(s);
            st.push(b);
        }
        return st;
    }

    function __appendStringToStringBuilderWithNoShift(stack(TvmBuilder) st, TvmCell _str) pure private returns(stack(TvmBuilder)) {
        TvmSlice str = _str.toSlice();
        while(true) {
            st.top().store(str.loadSlice(str.bits(), 0));
            if (str.empty())
                break;
            st.push(TvmBuilder());
            str = str.loadRefAsSlice();
        }
        return st;
    }

    // <StringBuilder>.append(<string>)
    function __appendStringToStringBuilder(stack(TvmBuilder) st, TvmCell cell) private pure returns (stack(TvmBuilder)) {
        if (st.top().bits() == 0) {
            return __appendStringToStringBuilderWithNoShift(st, cell);
        }
        TvmSlice s = cell.toSlice();
        while (true) {
            TvmSlice curSlice = s.loadSlice(s.bits(), 0);
            st = __appendSliceToStringBuilder(st, curSlice);
            if (s.empty())
                break;
            s = s.loadRefAsSlice();
        }
        return st;
    }

    function __convertIntToString(stack(TvmBuilder) st, int257 _integer, uint16 width, bool leadingZeros)
        private pure returns (stack(TvmBuilder))
    {
        bool addMinus = _integer < 0;
        uint integer = math.abs(_integer);
        uint16 remBytes = st.top().remBits() / 8;

        if (addMinus) {
            if (remBytes == 0) {
                st.push(TvmBuilder());
                remBytes = 127;
            }
            st.top().store(bytes1("-")); // store "-"
            --remBytes;
        }

        (optional(DigitStack) digits, uint8 length) = __parseInteger(integer, 10);

        if (width > length) {
            bytes1 fill = leadingZeros ? bytes1("0") : bytes1(" ");
            uint16 zeroes = width - length;
            repeat(math.min(zeroes, remBytes)) {
                st.top().store(fill);
            }
            if (zeroes > remBytes) {
                st.push(TvmBuilder());
                repeat(zeroes - remBytes) {
                    st.top().store(fill);
                }
                remBytes = remBytes + 127 - zeroes;
            } else {
                remBytes -= zeroes;
            }
        }

        repeat(math.min(length, remBytes)) {
            uint dig;
            (dig, digits) = getUnchecked(digits).unpack();
            st.top().storeUint(dig + uint8(bytes1("0")), 8);
        }
        if (length > remBytes) {
            st.push(TvmBuilder());
            repeat(length - remBytes) {
                uint dig;
                (dig, digits) = getUnchecked(digits).unpack();
                st.top().storeUint(dig + uint8(bytes1("0")), 8);
            }
        }

        return st;
    }

    function __appendAnyCast(stack(TvmBuilder) st, TvmSlice addr) private pure returns (stack(TvmBuilder), TvmSlice) {
        bool hasAnycast = addr.load(bool);
        if (hasAnycast) {
            uint5 depth = addr.load(uint5);
            st = __appendSliceDataAsHex(st, addr.loadSlice(depth));
            st = __appendBytes1(st, bytes1(":"));
        }
        return (st, addr);
    }

    function __convertAddressToHexString(stack(TvmBuilder) st, TvmSlice addr) private pure returns (stack(TvmBuilder)) {
        uint2 addrType = addr.load(uint2);
        if (addrType == 2) { // addr_std
            (st, addr) = __appendAnyCast(st, addr);
            st = __convertIntToString(st, addr.load(int8), 0, false);
            st = __appendBytes1(st, bytes1(":"));
            st = __convertIntToHexString(st, addr.load(uint256), 64, true, true);
        } else if (addrType == 1) { // addr_extern
            st = __appendBytes1(st, bytes1(":"));
            addr.load(uint9);
            st = __appendSliceDataAsHex(st, addr);
        } else if (addrType == 3) { // addr_var
            (st, addr) = __appendAnyCast(st, addr);
            addr.skip(9); // addr_len
            st = __convertIntToString(st, addr.load(int32), 0, false);
            st = __appendBytes1(st, bytes1(":"));
            st = __appendSliceDataAsHex(st, addr);
        }
        return st;
    }

    function __convertFixedPointToString(stack(TvmBuilder) st, int257 value, uint16 fractionalDigits, uint fractionPow10) private pure returns (stack(TvmBuilder)) {
        if (value < 0) {
            if (st.top().remBits() < 8) {
                st.push(TvmBuilder());
            }
            st.top().store(bytes1("-")); // store "-"
        }
        (uint integer, uint fractional) = math.divmod(math.abs(value), fractionPow10);
        st = __convertIntToString(st, integer, 0, false);
        if (st.top().remBits() < 8) {
            st.push(TvmBuilder());
        }
        st.top().store(bytes1("."));
        return __convertIntToString(st, fractional, fractionalDigits, true);
    }

    function __convertIntToHexString(stack(TvmBuilder) st, int257 _integer, uint16 width, bool leadingZeros, bool isLow)
        private pure returns (stack(TvmBuilder))
    {
        bool addMinus = _integer < 0;
        uint integer = math.abs(_integer);
        uint16 remBytes = st.top().remBits() / 8;

        if (addMinus) {
            if (remBytes == 0) {
                st.push(TvmBuilder());
                remBytes = 127;
            }
            st.top().store(bytes1("-")); // store "-"
            --remBytes;
        }

        (optional(DigitStack) digits, uint8 length) = __parseInteger(integer, 16);

        if (width > length) {
            bytes1 fill = leadingZeros ? bytes1("0") : bytes1(" ");
            uint16 zeroes = width - length;
            repeat(math.min(zeroes, remBytes)) {
                st.top().store(fill);
            }
            if (zeroes > remBytes) {
                st.push(TvmBuilder());
                repeat(zeroes - remBytes) {
                    st.top().store(fill);
                }
                remBytes = remBytes + 127 - zeroes;
            } else {
                remBytes -= zeroes;
            }
        }

        uint8 letterA = uint8(isLow ? bytes1("a") : bytes1("A")) - 10;
        repeat(math.min(length, remBytes)) {
            uint dig;
            (dig, digits) = digits.get().unpack();
            st.top().storeUint(dig + (dig < 10 ? uint8(bytes1("0")) : letterA), 8);
        }
        if (length > remBytes) {
            st.push(TvmBuilder());
            repeat(length - remBytes) {
                uint dig;
                (dig, digits) = digits.get().unpack();
                st.top().storeUint(dig + (dig < 10 ? uint8(bytes1("0")) : letterA), 8);
            }
        }
        return st;
    }

    function __convertBoolToStringBuilder(stack(TvmBuilder) st, bool value) private pure returns (stack(TvmBuilder)) {
        st = __appendSliceToStringBuilder(st,
            value ?
            TvmSlice(string("true")) :
            TvmSlice(string("false"))
        );
        return st;
    }

    function __stoi(TvmCell _str) private pure returns (optional(int)) {
        TvmSlice str = _str.toSlice();
        if (str.bits() < 8) {
            return null;
        }

        bool isNeg = str.bits() >= 8 && str.preload(uint8) == uint8(bytes1("-"));
        if (isNeg)
            str.skip(8);

        bool isHex = str.bits() >= 16 && str.preload(uint16) == uint16(bytes2("0x"));
        if (isHex)
            str.skip(16);

        int res = 0;
        uint16 digits = str.bits() >> 3;
        if (isHex) {
            repeat(digits) {
                uint8 dig = str.load(uint8);
                res *= 16;
                if (dig >= 0x30 && dig <= 0x39) {
                    res += dig - 0x30;
                } else if (dig >= 0x41 && dig <= 0x46) {
                    res += dig - 0x41 + 10;
                } else if (dig >= 0x61 && dig <= 0x66) {
                    res += dig - 0x61 + 10;
                } else {
                    return null;
                }
            }
        } else {
            repeat(digits) {
                uint8 dig = str.load(uint8);
                if (dig < uint8(bytes1("0")) || dig > uint8(bytes1("9")))
                    return null;
                res = res * 10 + int(uint(dig - uint8(bytes1("0"))));
            }
        }
        if (isNeg)
            res = -res;
        return res;
    }

    // string text = ...;
    // string sub = text[3:10];
    function __arraySlice(TvmCell data, uint10 from, uint end) private pure returns (TvmCell) {
        require(from <= end, 70);
        uint count = end - from;
        return __subCell(data, from, count, false);
    }

    function __subCell(TvmCell _str, uint10 from, uint count, bool notStrictCount) private pure returns (TvmCell) {
        (uint10 skipCells, uint10 skipBytes) = math.divmod(from, 127);
        if (skipCells != 0 && skipBytes == 0) {
            --skipCells;
            skipBytes = 127;
        }

        TvmSlice str = _str.toSlice();
        repeat(skipCells) {
            require(str.refs() == 1, 70);
            str = str.loadRefAsSlice();
        }

        skipBytes *= 8;
        require(str.bits() >= skipBytes, 70);
        str.skip(skipBytes);

        count = 8 * count;
        stack(TvmBuilder) st = __createStringBuilder();
        while (true) {
            uint10 take = uint10(math.min(str.bits(), count));
            count -= take;
            TvmSlice curSlice = str.loadSlice(take, 0);
            st = __appendSliceToStringBuilder(st, curSlice);
            if (count == 0 || str.empty()) {
                break;
            }
            str = str.loadRefAsSlice();
        }
        require(notStrictCount || count == 0, 70);
        return __makeString(st);
    }

    function __compareStrings(TvmCell lstr, TvmCell rstr) private pure returns (int8) {
        // 1   lstr  > rstr
        // 0   lstr == rstr
        // -1  lstr  < rstr
        TvmSlice left = lstr.toSlice();
        TvmSlice right = rstr.toSlice();
        while (true) {
            int8 res = left.compare(right);
            if (res != 0) {
                return res;
            }
            uint8 lRefs = left.refs();
            uint8 rRefs = right.refs();
            if (lRefs > rRefs)
                return 1;
            if (rRefs > lRefs)
                return -1;
            if (lRefs + rRefs == 0)
                return 0;
            left = left.loadRefAsSlice();
            right = right.loadRefAsSlice();
        }
        return 0;
    }

    function __concatenateStrings(TvmCell a, TvmCell b) pure private returns(TvmCell) {
        stack(TvmBuilder) st = __createStringBuilder();
        st = __appendStringToStringBuilderWithNoShift(st, a);
        st = __appendStringToStringBuilder(st, b);
        return __makeString(st);
    }

    function __strchr(bytes str, bytes1 char) private pure returns (optional(uint32) res) {
        uint32 i = 0;
        for (bytes1 b : str) {
            if (b == char)
                return i;
            ++i;
        }
    }

    function __strrchr(bytes str, bytes1 char) private pure returns (optional(uint32) res) {
        uint32 i = 0;
        for (bytes1 b : str) {
            if (b == char)
                res.set(i);
            ++i;
        }
    }

    // check whether `str` starts with `prefix`
    function __isPrefix(TvmSlice str, TvmSlice prefix) private pure inline returns (bool equal) {
        uint10 strBits = str.bits();
        uint10 prefixBits = prefix.bits();
        while (!prefix.empty()) {
            if (strBits == 0) {
                if (str.refs() == 0)
                    return false;
                str = str.loadRefAsSlice();
                strBits = str.bits();
            }
            if (prefixBits == 0) {
                prefix = prefix.loadRefAsSlice();
                prefixBits = prefix.bits();
            }
            uint10 len = math.min(strBits, prefixBits);
            TvmSlice s1 = str.loadSlice(len);
            TvmSlice s2 = prefix.loadSlice(len);
            if (s2 != s1)
                return false;
            strBits -= len;
            prefixBits -= len;
        }
        return true;
    }

    function __strstr(TvmCell _str, TvmCell _substr) private pure returns (optional(uint32)) {
        TvmSlice str = _str.toSlice();
        TvmSlice substr = _substr.toSlice();
        for (uint32 pos = 0; ; ) {
            if (__isPrefix(str, substr))
                return pos;
            if (str.empty())
                break;
            if (str.bits() == 0)
                str = str.loadRefAsSlice();
            str.load(uint8);
            ++pos;
        }
        return null;
    }

    // <string>.toLowerCase()
    function __toLowerCase(string s) private pure returns (string) {
        StringBuilder sb;
        for (bytes1 b : s) {
            uint8 ch = uint8(b);
            if (uint8(bytes1("A")) <= ch && ch <= uint8(bytes1("Z")))
                ch = ch - uint8(bytes1("A")) + uint8(bytes1("a")) ; // to 'a'..'a'
            sb.append(bytes1(ch));
        }
        return sb.toString();
    }

    // <string>.toUpperCase()
    function __toUpperCase(string s) private pure returns (string) {
        StringBuilder sb;
        for (bytes1 b : s) {
            uint8 ch = uint8(b);
            if (uint8(bytes1("a")) <= ch && ch <= uint8(bytes1("z")))
                ch = ch - uint8(bytes1("a")) + uint8(bytes1("A")) ; // to 'A'..'Z'
            sb.append(bytes1(ch));
        }
        return sb.toString();
    }

    // tvm.stateInitHash()
    function __stateInitHash(uint256 codeHash, uint256 dataHash, uint16 codeDepth, uint16 dataDepth) private pure returns (uint256) {
        TvmBuilder builder;
        // amount of refs - code + data = 2
        builder.storeUint(2, 8);
        // amount of data bytes - 1 byte
        builder.storeUint(1, 8);
        // data bits
        // split_depth:(Maybe (## 5)) special:(Maybe TickTock)
        // code:(Maybe ^Cell) data:(Maybe ^Cell)
        // library:(Maybe ^Cell)
        // b00110
        builder.storeUint(6, 5);
        // completion tag b100
        builder.storeUint(4, 3);
        // refs cell depth
        builder.storeUint(codeDepth,16);
        builder.storeUint(dataDepth,16);
        // refs cell hash
        builder.storeUint(codeHash,256);
        builder.storeUint(dataHash,256);
        return sha256(builder.toSlice());
    }

    function __forwardFee() private pure returns (varuint16) {
        TvmSlice s = msg.data.toSlice();
        uint1 tag = s.load(uint1);
        if (tag == 0) {
            // int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
            //  src:MsgAddressInt dest:MsgAddressInt
            //  value:CurrencyCollection ihr_fee:Grams fwd_fee:Grams
            //  created_lt:uint64 created_at:uint32 = CommonMsgInfo;
            s.load(uint3,
                address, address,
                varuint16, mapping(uint32 => varuint32), varuint16
            );
            return s.load(varuint16);
        } else {
            return 0;
        }
    }

    function __importFee() private pure returns (varuint16) {
        TvmSlice s = msg.data.toSlice();
        uint2 tag = s.load(uint2);
        if (tag == 2) {
            // ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt
            //  import_fee:Grams = CommonMsgInfo;
            s.load(address, address);
            return s.load(varuint16);
        } else {
            return 0;
        }
    }

    struct Type {
        int foo;
    }

    // <stack(T)>.sort(<cmp>)
    function __stackSort(stack(Type) st, function(Type, Type) internal pure returns(bool) isLess)
        pure private returns(stack(Type))
    {
        if (st.empty()) {
            stack(Type) s;
            return s;
        }
		stack(stack(Type)) arr;
		int size = 0;
		for (; !st.empty(); ++size) {
			stack(Type) arr2;
			arr2.push(st.pop());
			arr.push(arr2);
		}

		while (size > 1) {
			stack(stack(Type)) newArr;
			if (size % 2 != 0)
				newArr.push(arr.pop());
			while (!arr.empty()) {
				stack(Type) res;
				stack(Type) left = arr.pop();
				stack(Type) right = arr.pop();
				while (!left.empty() && !right.empty()) {
					if (isLess(left.top(), right.top()))
						res.push(left.pop());
					else
						res.push(right.pop());
				}
				while (!left.empty())
					res.push(left.pop());
				while (!right.empty())
					res.push(right.pop());
				res.reverse();
				newArr.push(res);
			}
			arr = newArr;
			size = (size + 1) / 2;
		}
		return arr.pop();
	}

    // <stack(T)>.reverse()
    function __stackReverse(stack(Type) st) pure private returns(stack(Type))
    {
        stack(Type) res;
        while (!st.empty())
            res.push(st.pop());
        return res;
    }

    function __qand(qint a, qint b) pure private returns(qint) {
        if ((a.isNaN() || a.get() != 0) &&
            (b.isNaN() || b.get() != 0)
        )
            return __QAND(a, b);
        return 0;
    }

    function __qor(qint a, qint b) pure private returns(qint) {
        if ((a.isNaN() || a.get() != -1) &&
            (b.isNaN() || b.get() != -1)
        )
            return __QOR(a, b);
        return -1;
    }
}
