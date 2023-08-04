# **TVM Solidity API**

TVM Solidity compiler expands Solidity language with different API functions to facilitate TVM contract development.

## Table of Contents

* [Compiler version](#compiler-version)
* [TVM specific types](#tvm-specific-types)
  * [TVM units](#tvm-units)
  * [TvmCell](#tvmcell)
    * [constructing TvmCell](#constructing-tvmcell)
    * [\<TvmCell\>.depth()](#tvmcelldepth)
    * [\<TvmCell\>.dataSize()](#tvmcelldatasize)
    * [\<TvmCell\>.dataSizeQ()](#tvmcelldatasizeq)
    * [\<TvmCell\>.toSlice()](#tvmcelltoslice)
  * [TvmSlice](#tvmslice)
    * [\<TvmSlice\>.empty()](#tvmsliceempty)
    * [\<TvmSlice\>.size()](#tvmslicesize)
    * [\<TvmSlice\>.bits()](#tvmslicebits)
    * [\<TvmSlice\>.refs()](#tvmslicerefs)
    * [\<TvmSlice\>.dataSize()](#tvmslicedatasize)
    * [\<TvmSlice\>.dataSizeQ()](#tvmslicedatasizeq)
    * [\<TvmSlice\>.depth()](#tvmslicedepth)
    * [\<TvmSlice\>.hasNBits(), \<TvmSlice\>.hasNRefs() and \<TvmSlice\>.hasNBitsAndRefs()](#tvmslicehasnbits-tvmslicehasnrefs-and-tvmslicehasnbitsandrefs)
    * [\<TvmSlice\>.compare()](#tvmslicecompare)
    * [TvmSlice load primitives](#tvmslice-load-primitives)
      * [\<TvmSlice\>.load()](#tvmsliceload)
      * [\<TvmSlice\>.loadQ()](#tvmsliceloadq)
      * [\<TvmSlice\>.loadRef()](#tvmsliceloadref)
      * [\<TvmSlice\>.loadRefAsSlice()](#tvmsliceloadrefasslice)
      * [\<TvmSlice\>.loadInt() and \<TvmSlice\>.loadIntQ()](#tvmsliceloadint-and-tvmsliceloadintq)
      * [\<TvmSlice\>.loadUint() and \<TvmSlice\>.loadUintQ()](#tvmsliceloaduint-and-tvmsliceloaduintq)
      * [Load little-endian integers](#load-little-endian-integers)
      * [\<TvmSlice\>.loadTons()](#tvmsliceloadtons)
      * [\<TvmSlice\>.loadSlice() and \<TvmSlice\>.loadSliceQ()](#tvmsliceloadslice-and-tvmsliceloadsliceq)
      * [\<TvmSlice\>.loadFunctionParams()](#tvmsliceloadfunctionparams)
      * [\<TvmSlice\>.loadStateVars()](#tvmsliceloadstatevars)
      * [\<TvmSlice\>.skip()](#tvmsliceskip)
      * [\<TvmSlice\>.loadZeroes(), \<TvmSlice\>.loadOnes() and \<TvmSlice\>.loadSame()](#tvmsliceloadzeroes-tvmsliceloadones-and-tvmsliceloadsame)
    * [TvmSlice preload primitives](#tvmslice-preload-primitives)
      * [\<TvmSlice\>.preload()](#tvmslicepreload)
      * [\<TvmSlice\>.preloadQ()](#tvmslicepreloadq)
      * [\<TvmSlice\>.preloadRef()](#tvmslicepreloadref)
      * [\<TvmSlice\>.preloadInt() and \<TvmSlice\>.preloadIntQ()](#tvmslicepreloadint-and-tvmslicepreloadintq)
      * [\<TvmSlice\>.preloadUint() and \<TvmSlice\>.preloadUintQ()](#tvmslicepreloaduint-and-tvmslicepreloaduintq)
      * [Preload little-endian integers](#preload-little-endian-integers)
      * [\<TvmSlice\>.preloadSlice() and \<TvmSlice\>.preloadSliceQ()](#tvmslicepreloadslice-and-tvmslicepreloadsliceq)
  * [TvmBuilder](#tvmbuilder)
    * [\<TvmBuilder\>.toSlice()](#tvmbuildertoslice)
    * [\<TvmBuilder\>.toCell()](#tvmbuildertocell)
    * [\<TvmBuilder\>.size()](#tvmbuildersize)
    * [\<TvmBuilder\>.bits()](#tvmbuilderbits)
    * [\<TvmBuilder\>.refs()](#tvmbuilderrefs)
    * [\<TvmBuilder\>.remBits()](#tvmbuilderrembits)
    * [\<TvmBuilder\>.remRefs()](#tvmbuilderremrefs)
    * [\<TvmBuilder\>.remBitsAndRefs()](#tvmbuilderrembitsandrefs)
    * [\<TvmBuilder\>.depth()](#tvmbuilderdepth)
    * [\<TvmBuilder\>.store()](#tvmbuilderstore)
    * [\<TvmBuilder\>.storeZeroes(), \<TvmBuilder\>.storeOnes() and \<TvmBuilder\>.storeSame()](#tvmbuilderstorezeroes-tvmbuilderstoreones-and-tvmbuilderstoresame)
    * [\<TvmBuilder\>.storeInt()](#tvmbuilderstoreint)
    * [\<TvmBuilder\>.storeUint()](#tvmbuilderstoreuint)
    * [Store little-endian integers](#store-little-endian-integers)
    * [\<TvmBuilder\>.storeRef()](#tvmbuilderstoreref)
    * [\<TvmBuilder\>.storeTons()](#tvmbuilderstoretons)
  * [ExtraCurrencyCollection](#extracurrencycollection)
  * [optional(T)](#optionalt)
    * [constructing an optional](#constructing-an-optional)
    * [\<optional(T)\>.hasValue()](#optionalthasvalue)
    * [\<optional(T)\>.get()](#optionaltget)
    * [\<optional(T)\>.getOr()](#optionaltgetor)
    * [\<optional(T)\>.getOrDefault()](#optionaltgetordefault)
    * [\<optional(T)\>.set()](#optionaltset)
    * [Keyword `null`](#keyword-null)
  * [variant](#variant)
    * [variant.isUint()](#variantisuint)
    * [variant.toUint()](#varianttouint)
  * [vector(Type)](#vectortype)
    * [\<vector(Type)\>.push(Type)](#vectortypepushtype)
    * [\<vector(Type)\>.pop()](#vectortypepop)
    * [\<vector(Type)\>.length()](#vectortypelength)
    * [\<vector(Type)\>.empty()](#vectortypeempty)
* [TVM specific control structures](#tvm-specific-control-structures)
  * [Range-based for loop](#range-based-for-loop)
  * [repeat](#repeat)
  * [try-catch](#try-catch)
* [Changes and extensions in Solidity types](#changes-and-extensions-in-solidity-types)
  * [Integers](#integers)
    * [bitSize() and uBitSize()](#bitsize-and-ubitsize)
  * [varInt and varUint](#varint-and-varuint)
  * [struct](#struct)
    * [struct constructor](#struct-constructor)
    * [\<struct\>.unpack()](#structunpack)
  * [Arrays](#arrays)
    * [Array literals](#array-literals)
    * [Creating new arrays](#creating-new-arrays)
    * [\<array\>.empty()](#arrayempty)
  * [bytesN](#bytesn)
  * [bytes](#bytes)
    * [\<bytes\>.empty()](#bytesempty)
    * [\<bytes\>.operator[]](#bytesoperator)
    * [\<bytes\> slice](#bytes-slice)
    * [\<bytes\>.length](#byteslength)
    * [\<bytes\>.dataSize()](#bytesdatasize)
    * [\<bytes\>.dataSizeQ()](#bytesdatasizeq)
    * [\<bytes\>.append()](#bytesappend)
    * [bytes conversion](#bytes-conversion)
  * [string](#string)
    * [\<string\>.empty()](#stringempty)
    * [\<string\>.byteLength()](#stringbytelength)
    * [\<string\>.substr()](#stringsubstr)
    * [\<string\>.append()](#stringappend)
    * [\<string\>.operator+](#stringoperator)
    * [\<string\>.find() and \<string\>.findLast()](#stringfind-and-stringfindlast)
    * [\<string\>.dataSize()](#stringdatasize)
    * [\<string\>.dataSizeQ()](#stringdatasizeq)
    * [\<string\>.toUpperCase()` and \<string\>.toLowerCase()](#stringtouppercase-and-stringtolowercase)
    * [format()](#format)
    * [stoi()](#stoi)
    * [string conversion](#string-conversion)
  * [address](#address)
    * [Object creating](#object-creating)
      * [constructor()](#constructor)
      * [address.makeAddrStd()](#addressmakeaddrstd)
      * [address.makeAddrNone()](#addressmakeaddrnone)
      * [address.makeAddrExtern()](#addressmakeaddrextern)
    * [Members](#members)
      * [\<address\>.wid](#addresswid)
      * [\<address\>.value](#addressvalue)
      * [\<address\>.balance](#addressbalance)
      * [\<address\>.currencies](#addresscurrencies)
    * [Functions](#functions)
      * [\<address\>.getType()](#addressgettype)
      * [\<address\>.isStdZero()](#addressisstdzero)
      * [\<address\>.isStdAddrWithoutAnyCast()](#addressisstdaddrwithoutanycast)
      * [\<address\>.isExternZero()](#addressisexternzero)
      * [\<address\>.isNone()](#addressisnone)
      * [\<address\>.unpack()](#addressunpack)
      * [\<address\>.transfer()](#addresstransfer)
  * [mapping](#mapping)
    * [Keyword `emptyMap`](#keyword-emptymap)
    * [\<mapping\>.operator[]](#mappingoperator)
    * [\<mapping\>.at()](#mappingat)
    * [\<mapping\>.min() and \<mapping\>.max()](#mappingmin-and-mappingmax)
    * [\<mapping\>.next() and \<mapping\>.prev()](#mappingnext-and-mappingprev)
    * [\<mapping\>.nextOrEq() and \<mapping\>.prevOrEq()](#mappingnextoreq-and-mappingprevoreq)
    * [\<mapping\>.delMin() and \<mapping\>.delMax()](#mappingdelmin-and-mappingdelmax)
    * [\<mapping\>.fetch()](#mappingfetch)
    * [\<mapping\>.exists()](#mappingexists)
    * [\<mapping\>.empty()](#mappingempty)
    * [\<mapping\>.replace()](#mappingreplace)
    * [\<mapping\>.add()](#mappingadd)
    * [\<mapping\>.getSet()](#mappinggetset)
    * [\<mapping\>.getAdd()](#mappinggetadd)
    * [\<mapping\>.getDel()](#mappinggetdel)
    * [\<mapping\>.getReplace()](#mappinggetreplace)
    * [\<mapping\>.keys() \<mapping\>.values()](#mappingkeys-mappingvalues)
  * [Fixed point number](#fixed-point-number)
  * [Function type](#function-type)
  * [require, revert](#require-revert)
    * [require](#require)
    * [revert](#revert)
  * [Libraries](#libraries)
    * [Function call via library name](#function-call-via-library-name)
    * [Function call via object](#function-call-via-object)
* [Pragmas](#pragmas)
  * [pragma ever-solidity](#pragma-ever-solidity)
  * [pragma copyleft](#pragma-copyleft)
  * [pragma ignoreIntOverflow](#pragma-ignoreintoverflow)
  * [pragma AbiHeader](#pragma-abiheader)
  * [pragma msgValue](#pragma-msgvalue)
  * [pragma upgrade func/oldsol](#pragma-upgrade-funcoldsol)
* [State variables](#state-variables)
  * [Decoding state variables](#decoding-state-variables)
  * [Keyword `constant`](#keyword-constant)
  * [Keyword `static`](#keyword-static)
  * [Keyword `public`](#keyword-public)
* [Special contract functions](#special-contract-functions)
  * [receive](#receive)
  * [fallback](#fallback)
  * [onBounce](#onbounce)
  * [onTickTock](#onticktock)
  * [onCodeUpgrade](#oncodeupgrade)
  * [afterSignatureCheck](#aftersignaturecheck)
* [Function specifiers](#function-specifiers)
  * [Function mutability: pure, view and default](#function-mutability-pure-view-and-default)
  * [Keyword inline](#keyword-inline)
  * [Assembly](#assembly)
  * [functionID()](#functionid)
  * [externalMsg and internalMsg](#externalmsg-and-internalmsg)
* [Events and return](#events-and-return)
  * [emit](#emit)
  * [return](#return)
* [External function calls](#external-function-calls)
  * [Synchronous calls](#synchronous-calls)
* [Delete variables](#delete-variables)
* [API functions and members](#api-functions-and-members)
  * [Type information](#type-information)
  * [**msg** namespace](#msg-namespace)
    * [msg.sender](#msgsender)
    * [msg.value](#msgvalue)
    * [msg.currencies](#msgcurrencies)
    * [msg.pubkey()](#msgpubkey)
    * [msg.isInternal, msg.isExternal and msg.isTickTock](#msgisinternal-msgisexternal-and-msgisticktock)
    * [msg.createdAt](#msgcreatedat)
    * [msg.data](#msgdata)
    * [msg.forwardFee](#msgforwardfee)
    * [msg.importFee](#msgimportFee)
    * [msg.body](#msgbody)
    * [msg.hasStateInit](#msghasstateinit)
  * [**tvm** namespace](#tvm-namespace)
    * [TVM instructions](#tvm-instructions)
      * [tvm.accept()](#tvmaccept)
      * [tvm.setGasLimit()](#tvmsetgaslimit)
      * [tvm.buyGas()](#tvmbuygas)
      * [tvm.commit()](#tvmcommit)
      * [tvm.rawCommit()](#tvmrawcommit)
      * [tvm.getData()](#tvmgetdata)
      * [tvm.setData()](#tvmsetdata)
      * [tvm.log()](#tvmlog)
      * [tvm.hexdump() and tvm.bindump()](#tvmhexdump-and-tvmbindump)
      * [tvm.setcode()](#tvmsetcode)
      * [tvm.configParam()](#tvmconfigparam)
      * [tvm.rawConfigParam()](#tvmrawconfigparam)
      * [tvm.rawReserve()](#tvmrawreserve)
      * [tvm.initCodeHash()](#tvminitcodehash)
    * [Hashing and cryptography](#hashing-and-cryptography)
      * [tvm.hash()](#tvmhash)
      * [tvm.checkSign()](#tvmchecksign)
    * [Deploy contract from contract](#deploy-contract-from-contract)
      * [tvm.insertPubkey()](#tvminsertpubkey)
      * [tvm.buildStateInit()](#tvmbuildstateinit)
      * [tvm.buildDataInit()](#tvmbuilddatainit)
      * [tvm.stateInitHash()](#tvmstateinithash)
      * [Deploy via new](#deploy-via-new)
        * [`stateInit` option usage](#stateinit-option-usage)
        * [`code` option usage](#code-option-usage)
      * [Other deploy options](#other-deploy-options)
      * [Deploy via \<address\>.transfer()](#deploy-via-addresstransfer)
      * [New contract address problem](#new-contract-address-problem)
    * [Misc functions from `tvm`](#misc-functions-from-tvm)
      * [tvm.code()](#tvmcode)
      * [tvm.codeSalt()](#tvmcodesalt)
      * [tvm.setCodeSalt()](#tvmsetcodesalt)
      * [tvm.pubkey()](#tvmpubkey)
      * [tvm.setPubkey()](#tvmsetpubkey)
      * [tvm.setCurrentCode()](#tvmsetcurrentcode)
      * [tvm.resetStorage()](#tvmresetstorage)
      * [tvm.functionId()](#tvmfunctionid)
      * [tvm.encodeBody()](#tvmencodebody)
      * [tvm.exit() and tvm.exit1()](#tvmexit-and-tvmexit1)
      * [tvm.buildExtMsg()](#tvmbuildextmsg)
      * [tvm.buildIntMsg()](#tvmbuildintmsg)
      * [tvm.sendrawmsg()](#tvmsendrawmsg)
  * [**math** namespace](#math-namespace)
    * [math.min() math.max()](#mathmin-mathmax)
    * [math.minmax()](#mathminmax)
    * [math.abs()](#mathabs)
    * [math.modpow2()](#mathmodpow2)
    * [math.divr() math.divc()](#mathdivr-mathdivc)
    * [math.muldiv() math.muldivr() math.muldivc()](#mathmuldiv-mathmuldivr-mathmuldivc)
    * [math.muldivmod()](#mathmuldivmod)
    * [math.divmod()](#mathdivmod)
    * [math.sign()](#mathsign)
  * [**tx** namespace](#tx-namespace)
    * [tx.logicaltime](#txlogicaltime)
    * [tx.storageFee](#txstoragefee)
  * [**block** namespace](#block-namespace)
    * [block.timestamp](#blocktimestamp)
    * [block.logicaltime](#blocklogicaltime)
  * [**rnd** namespace](#rnd-namespace)
    * [rnd.next](#rndnext)
    * [rnd.getSeed](#rndgetseed)
    * [rnd.setSeed](#rndsetseed)
    * [rnd.shuffle](#rndshuffle)
  * [**abi** namespace](#abi-namespace)
    * [abi.encode(), abi.decode()](#abiencode-abidecode)
  * [**gosh** namespace](#gosh-namespace)
    * [gosh.diff and gosh.zipDiff](#goshdiff-and-goshzipdiff)
    * [gosh.applyPatch, gosh.applyPatchQ, gosh.applyZipPatch, gosh.applyZipPatchQ, gosh.applyZipBinPatch and gosh.applyZipBinPatchQ](#goshapplypatch-goshapplypatchq-goshapplyzippatch-goshapplyzippatchq-goshapplyzipbinpatch-and-goshapplyzipbinpatchq)
    * [gosh.zip and gosh.unzip](#goshzip-and-goshunzip)
  * [Exponentiation](#exponentiation)
  * [selfdestruct](#selfdestruct)
  * [sha256](#sha256)
  * [gasToValue](#gastovalue)
  * [valueToGas](#valuetogas)
  * [gasleft](#gasleft)
* [TVM exception codes](#tvm-exception-codes)
* [Solidity runtime errors](#solidity-runtime-errors)
* [Division and rounding](#division-and-rounding)
* [Contract execution](#contract-execution)
* [Gas optimization hints](#gas-optimization-hints)

## Detailed description

### Compiler version

TVM Solidity compiler add its current version to the generated code. This version can be obtained:

1) using [tvm_linker](https://github.com/tonlabs/TVM-linker#2-decoding-of-boc-messages-prepared-externally) from a `*.tvc` file:

    ```bash
    tvm_linker decode --tvm <tvc-file>
    ```

2) using [tonos-cli](https://github.com/tonlabs/tonos-cli#48-decode-commands) from a `*.boc` file, a `*.tvc` file, or a network account:

```bash
tonos-cli decode tvc [--tvc] [--boc] <input>
```

### TVM specific types

TVM Solidity compiler expands functionality of some existing types and adds several new TVM specific types: TvmCell, TvmSlice, TvmBuilder and ExtraCurrencyCollection. Full description of these types can be found in [TVM][1] and [Blockchain][2] specifications.

#### TVM units

A literal number can take a suffix to specify a subdenomination of currency, where numbers without a postfix are assumed to be nanoevers.

```TVMSolidity
uint a0 = 1 nano; // a0 == 1 == 1e-9 ever
uint a1 = 1 nanoever; // a1 == 1 == 1e-9 ever
uint a3 = 1 ever; // a3 == 1 000 000 000 (1e9)
uint a4 = 1 Ever; // a4 == 1 000 000 000 (1e9)
uint a5 = 1 micro; // a5 == 1 000 == 1e-6 ever
uint a6 = 1 microever; // a6 == 1 000 == 1e-6 ever
uint a7 = 1 milli; // a7 == 1 000 000 == 1e-3 ever
uint a8 = 1 milliever; // a8 == 1 000 000 == 1e-3 ever
uint a9 = 1 kiloever; // a9 == 1 000 000 000 000 (1e12) == 1e3 ever
uint a10 = 1 kEver; // a10 == 1 000 000 000 000 (1e12) == 1e3 ever
uint a11 = 1 megaever; // a11 == 1 000 000 000 000 000 (1e15) == 1e6 ever
uint a12 = 1 MEver; // a12 == 1 000 000 000 000 000 (1e15) == 1e6 ever
uint a13 = 1 gigaever; // a13 == 1 000 000 000 000 000 000 (1e18) == 1e9 ever
uint a14 = 1 GEver; // a14 == 1 000 000 000 000 000 000 (1e18) == 1e9 ever
```

```TVMSolidity
uint a0 = 1 nano; // a0 == 1
uint a1 = 1 nanoton; // a1 == 1
uint a2 = 1 nTon; // a2 == 1
uint a3 = 1 ton; // a3 == 1 000 000 000 (1e9)
uint a4 = 1 Ton; // a4 == 1 000 000 000 (1e9)
uint a5 = 1 micro; // a5 == 1 000
uint a6 = 1 microton; // a6 == 1 000
uint a7 = 1 milli; // a7 == 1 000 000
uint a8 = 1 milliton; // a8 == 1 000 000
uint a9 = 1 kiloton; // a9 == 1 000 000 000 000 (1e12)
uint a10 = 1 kTon; // a10 == 1 000 000 000 000 (1e12)
uint a11 = 1 megaton; // a11 == 1 000 000 000 000 000 (1e15)
uint a12 = 1 MTon; // a12 == 1 000 000 000 000 000 (1e15)
uint a13 = 1 gigaton; // a13 == 1 000 000 000 000 000 000 (1e18)
uint a14 = 1 GTon; // a14 == 1 000 000 000 000 000 000 (1e18)
```

#### TvmCell

`TvmCell` represents *TVM cell* ([TVM][1] - 1.1.3). The compiler defines the following
operators and functions to work with this type:

Comparison operators:
`==`, `!=` (evaluate to `bool`)

##### constructing TvmCell

To create empty cell use `TvmCell()`. Example:

```TVMSolidity
TvmCell cell = ...;
if (cell == TvmCell()) { // check whether `cell` is empty

}
```

##### \<TvmCell\>.depth()

```TVMSolidity
<TvmCell>.depth() returns(uint16);
```

Returns the depth **d** of the `TvmCell` **c**. If **c** has no references, then **d** = 0;
otherwise **d** is equal to one plus the maximum of depths of cells referred to from **c**.
If **c** is a Null instead of a Cell, returns zero.

#### \<TvmCell\>.dataSize()

```TVMSolidity
<TvmCell>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Returns the number of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If number of the distinct cells
exceeds `n+1` then a cell overflow [exception](#tvm-exception-codes) is thrown.
This function is a wrapper for the `CDATASIZE` opcode ([TVM][1] - A.11.7).

#### \<TvmCell\>.dataSizeQ()

```TVMSolidity
<TvmCell>.dataSizeQ(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Returns the number of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If number of the distinct cells
exceeds `n+1` then this function returns an `optional` that has no value.
This function is a wrapper for the `CDATASIZEQ` opcode ([TVM][1] - A.11.7).

##### \<TvmCell\>.toSlice()

```TVMSolidity
<TvmCell>.toSlice() returns (TvmSlice);
```

Converts a `TvmCell` to `TvmSlice`.

#### TvmSlice

`TvmSlice` represents *TVM cell slice* ([TVM][1] - 1.1.3). The compiler defines the following
operators and functions to work with this type:

Comparison operators:
`<=`, `<`, `==`, `!=`, `>=`, `>` (evaluate to bool).

Note: only data bits from the root cells are compared. References are ignored.

`TvmSlice` can be converted to `bytes`. It costs at least 500 gas units.

##### \<TvmSlice\>.empty()

```TVMSolidity
<TvmSlice>.empty() returns (bool);
```

Checks whether the `TvmSlice` is empty (i.e., contains no data bits and no cell references).

##### \<TvmSlice\>.size()

```TVMSolidity
<TvmSlice>.size() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns the number of data bits and references in the `TvmSlice`.

##### \<TvmSlice\>.bits()

```TVMSolidity
<TvmSlice>.bits() returns (uint16);
```

Returns the number of data bits in the `TvmSlice`.

##### \<TvmSlice\>.refs()

```TVMSolidity
<TvmSlice>.refs() returns (uint8);
```

Returns the number of references in the `TvmSlice`.

#### \<TvmSlice\>.dataSize()

```TVMSolidity
<TvmSlice>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Returns the number of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If number of the distinct cells
exceeds `n+1` then a cell overflow [exception](#tvm-exception-codes) is thrown.
Note that the returned `count of distinct cells` does not take into
account the cell that contains the slice itself.
This function is a wrapper for `SDATASIZE` opcode ([TVM][1] - A.11.7).

#### \<TvmSlice\>.dataSizeQ()

```TVMSolidity
<TvmSlice>.dataSizeQ(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Returns the number of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If number of the distinct cells
exceeds `n+1` then this function returns an `optional` that has no value.
Note that the returned `count of distinct cells` does not take into
account the cell that contains the slice itself.
This function is a wrapper for `SDATASIZEQ` opcode ([TVM][1] - A.11.7).

##### \<TvmSlice\>.depth()

```TVMSolidity
<TvmSlice>.depth() returns (uint16);
```

Returns the depth of `TvmSlice`. If the `TvmSlice` has no references, then 0 is returned,
otherwise function result is one plus the maximum of depths of the cells referred to from the slice.

##### \<TvmSlice\>.hasNBits(), \<TvmSlice\>.hasNRefs() and \<TvmSlice\>.hasNBitsAndRefs()

```TVMSolidity
<TvmSlice>.hasNBits(uint10 bits) returns (bool);
<TvmSlice>.hasNRefs(uint2 refs) returns (bool);
<TvmSlice>.hasNBitsAndRefs(uint10 bits, uint2 refs) returns (bool);
```

Checks whether the `TvmSlice` contains the specified amount of data bits and references.

##### \<TvmSlice\>.compare()

```TVMSolidity
<TvmSlice>.compare(TvmSlice other) returns (int2);
```

Lexicographically compares the `slice` and `other` data bits of the root slices and returns result as an integer:

* 1 - `slice` > `other`
* 0 - `slice` == `other`
* -1 - `slice` < `other`

##### TvmSlice load primitives

All `load*` functions below modify the `TvmSlice` object. If you wants to load second reference from the `TvmSlice`, you should load the first one with [\<TvmSlice\>.loadRef()](#tvmsliceloadref) and then load the reference you need. The same rule is applied to data bits. To load bits from 2 to 10 positions, you should load or skip first two bits.

###### \<TvmSlice\>.load()

```TVMSolidity
<TvmSlice>.load(TypeA, TypeB, ...) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Sequentially loads values of the specified types from the `TvmSlice`.
Supported types: `uintN`, `intN`, `bytesN`, `bool`, `ufixedMxN`, `fixedMxN`, `address`, `contract`,
`TvmCell`, `bytes`, `string`, `mapping`, `ExtraCurrencyCollection`, `array`, `optional` and 
`struct`.  Example:

```TVMSolidity
TvmSlice slice = ...;
(uint8 a, uint16 b) = slice.load(uint8, uint16);
(uint16 num0, uint32 num1, address addr) = slice.load(uint16, uint32, address);
```

See also: [\<TvmBuilder\>.store()](#tvmbuilderstore).
**Note**: if all the argument types can't be loaded from the slice a cell underflow [exception](#tvm-exception-codes) is thrown.

###### \<TvmSlice\>.loadQ()

```TVMSolidity
<TvmSlice>.loadQ(TypeA, TypeB, ...) returns (optional(TypeA, TypeB, ...));
```

Sequentially decodes values of the specified types from the `TvmSlice` 
if the `TvmSlice` holds sufficient data for all specified types. Otherwise, returns `null`.

```TVMSolidity
TvmSlice slice = ...;
optional(uint) a = slice.loadQ(uint);
optional(uint8, uint16) b = slice.loadQ(uint8, uint16);
```

See also: [\<TvmBuilder\>.store()](#tvmbuilderstore).

###### \<TvmSlice\>.loadRef()

```TVMSolidity
<TvmSlice>.loadRef() returns (TvmCell);
```

Loads a cell from the `TvmSlice` reference.

###### \<TvmSlice\>.loadRefAsSlice()

```TVMSolidity
<TvmSlice>.loadRefAsSlice() returns (TvmSlice);
```

Loads a cell from the `TvmSlice` reference and converts it into a `TvmSlice`.

###### \<TvmSlice\>.loadInt() and \<TvmSlice\>.loadIntQ()

```TVMSolidity
(1)
<TvmSlice>.loadInt(uint9 bitSize) returns (int);
(2)
<TvmSlice>.loadIntQ(uint9 bitSize) returns (optional(int));
```

(1) Loads a signed integer with the given **bitSize** from the `TvmSlice`.

(2) Loads a signed integer with the given **bitSize** from the `TvmSlice` if `TvmSlice` contains it. Otherwise, returns `null`.

###### \<TvmSlice\>.loadUint() and \<TvmSlice\>.loadUintQ()

```TVMSolidity
(1)
<TvmSlice>.loadUint(uint9 bitSize) returns (uint);
(2)
<TvmSlice>.loadUintQ(uint9 bitSize) returns (optional(uint));
```

(1) Loads an unsigned integer with the given **bitSize** from the `TvmSlice`.

(2) Loads an unsigned integer with the given **bitSize** from the `TvmSlice` if `TvmSlice` contains it. Otherwise, returns `null`.

###### Load little-endian integers

```TVMSolidity
(1)
<TvmSlice>.loadIntLE2() returns (int16)
<TvmSlice>.loadIntLE4() returns (int32)
<TvmSlice>.loadIntLE8() returns (int64)
<TvmSlice>.loadUintLE2() returns (uint16)
<TvmSlice>.loadUintLE4() returns (uint32)
<TvmSlice>.loadUintLE8() returns (uint64)
(2)
<TvmSlice>.loadIntLE4Q() returns (optional(int32))
<TvmSlice>.loadIntLE8Q() returns (optional(int64))
<TvmSlice>.loadUintLE4Q() returns (optional(uint32))
<TvmSlice>.loadUintLE8Q() returns (optional(uint64))
```

(1) Loads the little-endian integer from `TvmSlice`.

(2) Same as (1) but returns `null` if it's impossible to load the integer.

###### \<TvmSlice\>.loadTons()

```TVMSolidity
<TvmSlice>.loadTons() returns (uint128);
```

Loads (deserializes) **VarUInteger 16** and returns an unsigned 128-bit integer. See [TL-B scheme][3].

###### \<TvmSlice\>.loadSlice() and \<TvmSlice\>.loadSliceQ()

```TVMSolidity
(1)
<TvmSlice>.loadSlice(uint10 bits) returns (TvmSlice);
(2)
<TvmSlice>.loadSlice(uint10 bits, uint refs) returns (TvmSlice);
(3)
<TvmSlice>.loadSliceQ(uint10 bits) returns (optional(TvmSlice));
(4)
<TvmSlice>.loadSliceQ(uint10 bits, uint2 refs) returns (optional(TvmSlice));
```

(1) Loads the first `bits` bits from `TvmSlice`.
(2) Loads the first `bits` bits and `refs` references from `TvmSlice`.
(3) and (4) are same as (1) and (2) but return `optional` type.

###### \<TvmSlice\>.loadFunctionParams()

```TVMSolidity
// Loads parameters of the public/external function without "responsible" attribute
<TvmSlice>.loadFunctionParams(functionName) returns (TypeA /*a*/, TypeB /*b*/, ...);

// Loads parameters of the public/external function with "responsible" attribute
<TvmSlice>.loadFunctionParams(functionName) returns (uint32 callbackFunctionId, TypeA /*a*/, TypeB /*b*/, ...);

// Loads constructor parameters
<TvmSlice>.loadFunctionParams(ContractName) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Loads parameters of the function or constructor (if contract type is provided). This function is usually used in
**[onBounce](#onbounce)** function.

See example of how to use **onBounce** function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

###### \<TvmSlice\>.loadStateVars()

```TVMSolidity
<TvmSlice>.loadStateVars(ContractName) returns (uint256 /*pubkey*/, uint64 /*timestamp*/, bool /*constructorFlag*/, Type1 /*var1*/, Type2 /*var2*/, ...);
```

Loads state variables from `slice` that is obtained from the field `data` of `stateInit`

Example:

```
contract A {
	uint a = 111;
	uint b = 22;
	uint c = 3;
	uint d = 44;
	address e = address(12);
	address f;
}

contract B {
	function f(TvmCell data) public pure {
		TvmSlice s = data.toSlice();
		(uint256 pubkey, uint64 timestamp, bool flag,
			uint a, uint b, uint c, uint d, address e, address f) = s.loadStateVars(A);
			
		// pubkey - pubkey of the contract A
		// timestamp - timestamp that used for replay protection
		// flag - always is equal to true
		// a == 111
		// b == 22
		// c == 3
		// d == 44
		// e == address(12)
		// f == address(0)
		// s.empty()
	}
}
```

###### \<TvmSlice\>.skip()

```TVMSolidity
<TvmSlice>.skip(uint10 bits);
<TvmSlice>.skip(uint10 bits, uint2 refs);
```

Skips the first `bits` bits and `refs` references from the `TvmSlice`.

###### \<TvmSlice\>.loadZeroes(), \<TvmSlice\>.loadOnes() and \<TvmSlice\>.loadSame()

```TVMSolidity
(1)
<TvmSlice>.loadZeroes() returns (uint10 n);
(2)
<TvmSlice>.loadOnes() returns (uint10 n);
(3)
<TvmSlice>.loadSame(uint1 value) returns (uint10 n);
```

(1) Returns the count `n` of leading zero bits in `TvmSlice`, and removes these bits from `TvmSlice`.

(2) Returns the count `n` of leading one bits in `TvmSlice`, and removes these bits from `TvmSlice`.

(3) Returns the count `n` of leading bits equal to `0 ≤ value ≤ 1` in `TvmSlice`, and removes these bits from `TvmSlice`.

See also: [\<TvmBuilder\>.storeZeroes(), \<TvmBuilder\>.storeOnes() and \<TvmBuilder\>.storeSame()](#tvmbuilderstorezeroes-tvmbuilderstoreones-and-tvmbuilderstoresame).

##### TvmSlice preload primitives

All `preload*` functions below don't modify the `TvmSlice` object.

###### \<TvmSlice\>.preload()

```TVMSolidity
<TvmSlice>.preload(TypeA, TypeB, ...) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Same as [\<TvmSlice\>.load()](#tvmsliceload) but doesn't modify `TvmSlice`.

###### \<TvmSlice\>.preloadQ()

```TVMSolidity
<TvmSlice>.preloadQ(TypeA, TypeB, ...) returns (optional(TypeA, TypeB, ...));
```

Same as [\<TvmSlice\>.loadQ()](#tvmsliceloadq) but doesn't modify `TvmSlice`.

###### \<TvmSlice\>.preloadRef()

```TVMSolidity
(1)
<TvmSlice>.preloadRef() returns (TvmCell);
(2)
<TvmSlice>.preloadRef(uint2 index) returns (TvmCell);
```

(1) Returns the first cell reference of `TvmSlice`.

(2) Returns the `index` cell reference of `TvmSlice`, where `0 ≤ index ≤ 3`.

###### \<TvmSlice\>.preloadInt() and \<TvmSlice\>.preloadIntQ()

```TVMSolidity
(1)
<TvmSlice>.preloadInt(uint9 bitSize) returns (int);
(2)
<TvmSlice>.preloadIntQ(uint9 bitSize) returns (optional(int));
```

Same as [\<TvmSlice\>.loadInt() and \<TvmSlice\>.loadIntQ()](#tvmsliceloadint-and-tvmsliceloadintq) but doesn't modify `TvmSlice`.

###### \<TvmSlice\>.preloadUint() and \<TvmSlice\>.preloadUintQ()

```TVMSolidity
(1)
<TvmSlice>.preloadUint(uint9 bitSize) returns (uint);
(2)
<TvmSlice>.preloadUintQ(uint9 bitSize) returns (optional(uint));
```

Same as [\<TvmSlice\>.loadUint() and \<TvmSlice\>.loadUintQ()](#tvmsliceloaduint-and-tvmsliceloaduintq) but doesn't modify `TvmSlice`.

###### Preload little-endian integers

```TVMSolidity
<TvmSlice>.preloadIntLE4() returns (int32)
<TvmSlice>.preloadIntLE8() returns (int64)
<TvmSlice>.preloadUintLE4() returns (uint32)
<TvmSlice>.preloadUintLE8() returns (uint64)

<TvmSlice>.preloadIntLE4Q() returns (optional(int32))
<TvmSlice>.preloadIntLE8Q() returns (optional(int64))
<TvmSlice>.preloadUintLE4Q() returns (optional(uint32))
<TvmSlice>.preloadUintLE8Q() returns (optional(uint64))
```

Same as [Load little-endian integers](#load-little-endian-integers) but doesn't modify `TvmSlice`.

###### \<TvmSlice\>.preloadSlice() and \<TvmSlice\>.preloadSliceQ()

```TVMSolidity
(1)
<TvmSlice>.preloadSlice(uint10 bits) returns (TvmSlice);
(2)
<TvmSlice>.preloadSlice(uint10 bits, uint refs) returns (TvmSlice);
(3)
<TvmSlice>.preloadSliceQ(uint10 bits) returns (optional(TvmSlice));
(4)
<TvmSlice>.preloadSliceQ(uint10 bits, uint4 refs) returns (optional(TvmSlice));
```

Same as [\<TvmSlice\>.loadSlice() and \<TvmSlice\>.loadSliceQ()](#tvmsliceloadslice-and-tvmsliceloadsliceq) but doesn't modify `TvmSlice`.

#### TvmBuilder

`TvmBuilder` represents *TVM cell builder* ([TVM][1] - 1.1.3). TVM Solidity compiler defines the following
functions to work with this type:

##### \<TvmBuilder\>.toSlice()

```TVMSolidity
<TvmBuilder>.toSlice() returns (TvmSlice);
```

Converts a `TvmBuilder` into `TvmSlice`.

##### \<TvmBuilder\>.toCell()

```TVMSolidity
<TvmBuilder>.toCell() returns (TvmCell);
```

Converts a `TvmBuilder` into `TvmCell`.

##### \<TvmBuilder\>.size()

```TVMSolidity
<TvmBuilder>.size() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns the number of data bits and references already stored in the `TvmBuilder`.

##### \<TvmBuilder\>.bits()

```TVMSolidity
<TvmBuilder>.bits() returns (uint16);
```

Returns the number of data bits already stored in the `TvmBuilder`.

##### \<TvmBuilder\>.refs()

```TVMSolidity
<TvmBuilder>.refs() returns (uint8);
```

Returns the number of references already stored in the `TvmBuilder`.

##### \<TvmBuilder\>.remBits()

```TVMSolidity
<TvmBuilder>.remBits() returns (uint16);
```

Returns the number of data bits that can still be stored in the `TvmBuilder`.

##### \<TvmBuilder\>.remRefs()

```TVMSolidity
<TvmBuilder>.remRefs() returns (uint8);
```

Returns the number of references that can still be stored in the `TvmBuilder`.

##### \<TvmBuilder\>.remBitsAndRefs()

```TVMSolidity
<TvmBuilder>.remBitsAndRefs() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns the number of data bits and references that can still be stored in the `TvmBuilder`.

##### \<TvmBuilder\>.depth()

```TVMSolidity
<TvmBuilder>.depth() returns (uint16);
```

Returns the depth of `TvmBuilder`. If no cell references are stored
in the builder, then 0 is returned; otherwise function result is one plus the maximum of
depths of cells referred to from the builder.

##### \<TvmBuilder\>.store()

```TVMSolidity
<TvmBuilder>.store(/*list_of_values*/);
```

Stores the list of values into the `TvmBuilder`.

Internal representation of the stored data:

* `uintN`/`intN`/`bytesN` - stored as an N-bit string.
For example, `uint8(100), int16(-3), bytes2(0xaabb)` stored as `0x64fffdaabb`.
* `bool` - stored as a binary zero for `false` or a binary one for `true`. For example,
`true, false, true` stored as `0xb_`.
* `ufixedMxN`/`fixedMxN` - stored as an M-bit string.
* `address`/`contract` - stored according to the [TL-B scheme][3] of `MsgAddress`.
* `TvmCell`/`bytes`/`string` - stored as a cell in reference.
* `TvmSlice`/`TvmBuilder` - all data bits and references of the `TvmSlice` or the `TvmBuilder`
are appended to the `TvmBuilder`, not in a reference as `TvmCell`. To store `TvmSlice`/`TvmBuilder` in
the references use [\<TvmBuilder\>.storeRef()](#tvmbuilderstoreref).
* `mapping`/`ExtraCurrencyCollection` - stored according to the [TL-B scheme][3] of `HashmapE`: if map is
empty then stored as a binary zero, otherwise as a binary one and the dictionary `Hashmap` in a reference.
* `array` - stored as a 32 bit value - size of the array and a `HashmapE` that contains all values of
the array.
* `optional` - stored as a binary zero if the `optional` doesn't contain value. Otherwise, stored as
a binary one and the cell with serialized value in a reference.
* `struct` - stored in the order of its members in the builder. Make sure the entire struct fits into the
builder.

**Note:** there is no gap or offset between two consecutive data assets stored in the `TvmBuilder`.

See [TVM][1] to read about notation for bit strings.

Example:

```TVMSolidity
uint8 a = 11;
int16 b = 22;
TvmBuilder builder;
builder.store(a, b, uint(33));
```

See also: [\<TvmSlice\>.load()](#tvmsliceload).

##### \<TvmBuilder\>.storeZeroes(), \<TvmBuilder\>.storeOnes() and \<TvmBuilder\>.storeSame()

```TVMSolidity
(1)
<TvmBuilder>.storeZeroes(uint10 n);
(2)
<TvmBuilder>.storeOnes(uint10 n);
(3)
<TvmBuilder>.storeSame(uint10 n, uint1 value);
```

(1) Stores `n` binary zeroes into the `TvmBuilder`.

(2) Stores `n` binary ones into the `TvmBuilder`.

(3) Stores `n` binary `value`s (0 ≤ value ≤ 1) into the `TvmBuilder`.

See also: [\<TvmSlice\>.loadZeroes(), \<TvmSlice\>.loadOnes() and \<TvmSlice\>.loadSame()](#tvmsliceloadzeroes-tvmsliceloadones-and-tvmsliceloadsame).

##### \<TvmBuilder\>.storeInt()

```TVMSolidity
<TvmBuilder>.storeInt(int256 value, uint9 bitSize);
```

Stores a signed integer **value** with given **bitSize** in the `TvmBuilder`.

##### \<TvmBuilder\>.storeUint()

```TVMSolidity
<TvmBuilder>.storeUint(uint256 value, uint9 bitSize);
```

Stores an unsigned integer **value** with given **bitSize** in the `TvmBuilder`.

##### Store little-endian integers

```TVMSolidity
<TvmBuilder>.storeIntLE2(int16)
<TvmBuilder>.storeIntLE4(int32)
<TvmBuilder>.storeIntLE8(int64)
<TvmBuilder>.storeUintLE2(uint16)
<TvmBuilder>.storeUintLE4(uint32)
<TvmBuilder>.storeUintLE8(uint64)
```

Stores the little-endian integer.

##### \<TvmBuilder\>.storeRef()

```TVMSolidity
<TvmBuilder>.storeRef(TvmBuilder b);
<TvmBuilder>.storeRef(TvmCell c);
<TvmBuilder>.storeRef(TvmSlice s);
```

Stores `TvmBuilder b`/`TvmCell c`/`TvmSlice s` in the reference of the `TvmBuilder`.

##### \<TvmBuilder\>.storeTons()

```TVMSolidity
<TvmBuilder>.storeTons(uint128 value);
```

Stores (serializes) an integer **value** and stores it in the `TvmBuilder` as
**VarUInteger 16**. See [TL-B scheme][3].

See example of how to work with TVM specific types:

* [Message_construction](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageSender.sol)
* [Message_parsing](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageReceiver.sol)

#### ExtraCurrencyCollection

ExtraCurrencyCollection represents TVM type ExtraCurrencyCollection. It has the same functions as [**mapping(uint32 => uint256)**](#mapping):

```TVMSolidity
ExtraCurrencyCollection curCol;
uint32 key;
uint256 value;
optional(uint32, uint256) res = curCol.min();
optional(uint32, uint256) res = curCol.next(key);
optional(uint32, uint256) res = curCol.prev(key);
optional(uint32, uint256) res = curCol.nextOrEq(key);
optional(uint32, uint256) res = curCol.prevOrEq(key);
optional(uint32, uint256) res = curCol.delMin();
optional(uint32, uint256) res = curCol.delMax();
optional(uint256) optValue = curCol.fetch(key);
bool exists = curCol.exists(key);
bool isEmpty = curCol.empty();
bool success = curCol.replace(key, value);
bool success = curCol.add(key, value);
optional(uint256) res = curCol.getSet(key, value);
optional(uint256) res = curCol.getAdd(key, value);
optional(uint256) res = curCol.getReplace(key, value);
uint256 uintValue = curCol[index];
```

#### optional(T)

The template optional type manages an optional contained value, i.e. a value that may or may not be present.

##### constructing an optional

There are many ways to set a value:

```TVMSolidity
optional(uint) opt;
opt.set(11); // just sets value
opt = 22; // just sets value, too
opt.get() = 33; // if 'opt' has value then set value. Otherwise throws an exception.

optional(uint) another = 44;
opt = another;
```

##### \<optional(T)\>.hasValue()

```TVMSolidity
<optional(T)>.hasValue() returns (bool);
```

Checks whether the `optional` contains a value.

##### \<optional(T)\>.get()

```TVMSolidity
<optional(T)>.get() returns (T);
```

Returns the contained value, if the `optional` contains one. Otherwise, throws an exception.

##### \<optional(T)\>.getOr()

```TVMSolidity
<optional(T)>.getOr(T default) returns (T);
```

Returns the contained value, if the `optional` contains one. Otherwise, returns `default`.

##### \<optional(T)\>.getOrDefault()

```TVMSolidity
<optional(T)>.getOrDefault() returns (T);
```

Returns the contained value, if the `optional` contains one. Otherwise, returns the default value for `T` type.

##### \<optional(T)\>.set()

```TVMSolidity
<optional(T)>.set(Type value);
```

Replaces content of the `optional` with **value**.

##### Keyword `null`

Keyword `null` is a constant that is used to indicate an optional type with uninitialized state.
Example:

```TVMSolidity
optional(uint) x = 123;
x = null; // reset value
```

#### variant

The `variant` type acts like a union for the most common solidity data types. Supported only `uint` so far.

#### variant.isUint()

```TVMSolidity
<variant>.isUint() returns (bool)
```

Checks whether `<variant>` holds `uint` type. 

#### variant.toUint()

Converts `<variant>` to `uint` type if it's possible. Otherwise, throws an exception with code `77`.

#### vector(Type)

`vector(Type)` is a template container type capable of storing an arbitrary set of values of a
single type, pretty much like dynamic-sized array.
Two major differences are that `vector(Type)`:

1. is much more efficient than a dynamic-sized array;
2. has a lifespan of a smart-contract execution, so it can't be neither passed nor returned as an
external function call parameter, nor stored in a state variable.

**Note:** `vector` implementation based on `TVM Tuple` type, and it has a limited
length of 255 * 255 = 65025 values.

##### \<vector(Type)\>.push(Type)

```TVMSolidity
<vector(Type)>.push(Type obj);
```

Appends **obj** to the `vector`.

```TVMSolidity
vector(uint) vect;
uint a = 11;
vect.push(a);
vect.push(111);
```

##### \<vector(Type)\>.pop()

```TVMSolidity
<vector(Type)>.pop() returns (Type);
```

Pops the last value from the `vector` and returns it.

```TVMSolidity
vector(uint) vect;
...
uint a = vect.pop();
```

##### \<vector(Type)\>.length()

```TVMSolidity
<vector(Type)>.length() returns (uint8);
```

Returns length of the `vector`.

```TVMSolidity
vector(uint) vect;
...
uint8 len = vect.length();
```

##### \<vector(Type)\>.empty()

```TVMSolidity
<vector(Type)>.empty() returns (bool);
```

Checks whether the `vector` is empty.

```TVMSolidity
vector(uint) vect;
...
bool is_empty = vect.empty();
```

### TVM specific control structures

### Range-based for loop

Executes a `for` loop over a range. Used as a more readable equivalent to the traditional `for` loop
operating over a range of values, such as all elements in an array or mapping.

```TVMSolidity
for ( range_declaration : range_expression ) loop_statement
```

`range_expression` is calculated only once, and the result is copied. Then iteration goes over the copy of
the array or mapping.

```TVMSolidity
uint[] arr = ...;
uint sum = 0;
for (uint val : arr) { // iteration over the array
    sum += val;
}


bytes byteArray = "Hello!";
for (byte b : byteArray) { // iteration over the byte array

}

mapping(uint32 => uint) map = ...;
uint keySum = 0;
uint valueSum = 0;
for ((uint32 key, uint value) : map) { // iteration over the mapping
    keySum += key;
    valueSum += value;
}
```

Key or value can be omitted if you iterate over a mapping:

```TVMSolidity
mapping(uint32 => uint) map = ...;

uint keySum = 0;
for ((uint32 key, ) : map) { // value is omitted
    keySum += key;
}

uint valueSum = 0;
for ((, uint value) : map) { // key is omitted
    valueSum += value;
}
```

#### repeat

Allows repeating block of code several times.
A `repeat` loop evaluates the expression only one time.
This expression must have an unsigned integer type.

```TVMSolidity
uint a = 0;
repeat(10) {
    a ++;
}
// a == 10

// Despite a is changed in the cycle, code block will be repeated 10 times (10 is the initial value of a)
repeat(a) {
    a += 2;
}
// a == 30

a = 11;
repeat(a - 1) {
    a -= 1;
}
// a == 1
```

#### try-catch

It is an experimental feature available only in certain blockchain networks.

The `try` statement allows you to define a block of code to be tested for errors while it is executed. The 
`catch` statement allows you to define a block of code to be executed, if an error occurs in the try block.
`catch` block gets two parameters of type variant and uint16, which contain exception argument and code respectively.
Example:

```TVMSolidity
TvmBuilder builder;
uint c = 0;
try {
    c = a + b;
    require(c != 42, 100, 22);
    require(c != 43, 100, 33);
    builder.store(c);
} catch (variant value, uint16 errorCode) {
    uint errorValue;
    if (value.isUint()) {
        errorValue = value.toUint();
    }

    if (errorCode == 100) {
        if (errorValue == 22) {
            // it was line: `require(c != 42, 100, 22);`
        } else if (errorValue == 33) {
            // it was line: `require(c != 43, 100, 33);`
        }
    } else if (errorCode == 8) {
        // Cell overflow
        // It was line: `builder.store(c);`
    } else if (errorCode == 4) {
        // Integer overflow
        // It was line: `c = a + b;`
    }
}
```

You can pass either of the parameters:

```TVMSolidity
uint c;
try {
    c = a + b;
} catch (variant , uint16 errorCode) {
    if (errorCode == 4) {

    }
}
```

Or you can pass all parameters:

```TVMSolidity
uint c;
try {
    c = a + b;
} catch {
}
```

### Changes and extensions in Solidity types

#### Integers

``int`` / ``uint``: Signed and unsigned integers of various sizes. Keywords ``uintN`` and ``intN``
where ``N`` is a number from ``1``  to ``256`` in steps of 1 denotes the number of bits. ``uint`` and ``int``
are aliases for ``uint256`` and ``int256``, respectively.

Operators:

* Comparison: ``<=``, ``<``, ``==``, ``!=``, ``>=``, ``>`` (evaluate to ``bool``)
* Bit operators: ``&``, ``|``, ``^`` (bitwise exclusive or), ``~`` (bitwise negation)
* Shift operators: ``<<`` (left shift), ``>>`` (right shift)
* Arithmetic operators: ``+``, ``-``, unary ``-``, ``*``, ``/``, ``%`` (modulo), ``**`` (exponentiation)

##### bitSize() and uBitSize()

```TVMSolidity
bitSize(int x) returns (uint16)
uBitSize(uint x) returns (uint16)
```

`bitSize` computes the smallest `c` ≥ 0 such that `x` fits into a `c`-bit signed integer
(−2<sup>c−1</sup> ≤ x < 2<sup>c−1</sup>).

`uBitSize` computes the smallest `c` ≥ 0 such that `x` fits into a `c`-bit unsigned integer
(0 ≤ x < 2<sup>c</sup>).

Example:

```TVMSolidity
uint16 s = bitSize(12); // s == 5
uint16 s = bitSize(1); // s == 2
uint16 s = bitSize(-1); // s == 1
uint16 s = bitSize(0); // s == 0

uint16 s = uBitSize(10); // s == 4
uint16 s = uBitSize(1); // s == 1
uint16 s = uBitSize(0); // s == 0
```

#### varInt and varUint

`varInt`/`varInt16`/`varInt32`/`varUint`/`varUint16`/`varUint32` are kinds of [Integer](#integers)
types. But they are serialized/deserialized according to [their TLB schemes](https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb#L112).
These schemes are effective if you want to store or send integers, and they usually have small size.
Use these types only if you are sure.
`varInt` is equal to `varInt32`, `varInt32` - `int248` , `varInt16` - `int120`.
`varUint` is equal to `varUint32`, `varUint32` - `uint248` , `varUint16` - `uint120`.
Example:

```TVMSolidity
mapping(uint => varInt) m_map; // use `varInt` as mapping value only if values have small size
m_map[10] = 15;
```

#### struct

Structs are custom defined types that can group several variables.

##### struct constructor

```TVMSolidity
struct Stakes {
    uint total;
    mapping(uint => uint) stakes;
}

// create struct with default values of its members
Stakes stakes;

// create struct using parameters
mapping(uint => uint) values;
values[100] = 200;
Stakes stakes = Stakes(200, values);

// create struct using named parameters
Stakes stakes = Stakes({stakes: values, total: 200});
```

##### \<struct\>.unpack()

```TVMSolidity
<struct>.unpack() returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Unpacks all members stored in the `struct`.

Example:

```TVMSolidity
struct MyStruct {
    uint a;
    int b;
    address c;
}

function f() pure public {
    MyStruct s = MyStruct(1, -1, address(2));
    (uint a, int b, address c) = s.unpack();
}
```

#### Arrays


##### Array literals

An array literal is a comma-separated list of one or more expressions, enclosed in square brackets.
For example: `[100, 200, 300]`.

Initializing constant state variable:
`uint[] constant fib = [uint(2), 3, 5, 8, 12, 20, 32];`

##### Creating new arrays

```TVMSolidity
uint[] arr; // create 0-length array

uint[] arr = new uint[](0); // create 0-length array

uint constant N = 5;
uint[] arr = new uint[](N); // create 5-length array

uint[] arr = new uint[](10); // create 10-length array
```

Note: If `N` is constant expression or integer literal then the complexity of array creation -
`O(1)`. Otherwise, `O(N)`.

##### \<array\>.empty()

```TVMSolidity
<array>.empty() returns (bool);
```

Returns status flag whether the `array` is empty (its length is 0).

Example:

```TVMSolidity
uint[] arr;
bool b = arr.empty(); // b == true
arr.push(...);
bool b = arr.empty(); // b == false
```

#### bytesN

Variables of the `bytesN` types can be explicitly converted to `bytes`. Note: it costs ~500 gas.

```TVMSolidity
bytes3 b3 = 0x112233;
bytes b = bytes(b3);
```

#### bytes

`bytes` is an array of `byte`. It is similar to `byte[]`, but they are encoded in different ways.

Example of `bytes` initialization:

```TVMSolidity
// initialised with string
bytes a = "abzABZ0129";
// initialised with hex data
bytes b = hex"01239abf";
```

`bytes` can be converted to `TvmSlice`. Warning: if length of the array is greater than 127 then extra bytes are stored in the first reference of the slice. Use [\<TvmSlice\>.loadRef()](#tvmsliceloadref) to load that extra bytes.

##### \<bytes\>.empty()

```TVMSolidity
<bytes>.empty() returns (bool);
```

Returns status flag whether the `bytes` is empty (its length is 0).

##### \<bytes\>.operator[]

```TVMSolidity
<bytes>.operator[](uint index) returns (byte);
```

Returns the byte located at the **index** position.

Example:

```TVMSolidity
bytes byteArray = "abba";
int index = 0;
byte a0 = byteArray[index]; // a0 = 0x61
```

##### \<bytes\> slice

```TVMSolidity
<bytes>.operator[](uint from, uint to) returns (bytes);
```

Returns the slice of `bytes` [**from**, **to**), including **from** byte and
excluding **to**.
Example:

```TVMSolidity
bytes byteArray = "01234567890123456789";
bytes slice = byteArray[5:10]; // slice == "56789"
slice = byteArray[10:]; // slice == "0123456789"
slice = byteArray[:10]; // slice == "0123456789"
slice = byteArray[:];  // slice == "01234567890123456789"
```

##### \<bytes\>.length

```TVMSolidity
<bytes>.length returns (uint)
```

Returns length of the `bytes` array.

##### \<bytes\>.dataSize()

```TVMSolidity
<bytes>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Same as [\<TvmCell\>.dataSize()](#tvmcelldatasize).

##### \<bytes\>.dataSizeQ()

```TVMSolidity
<bytes>.dataSizeQ(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Same as [\<TvmCell\>.dataSizeQ()](#tvmcelldatasizeq).

##### \<bytes\>.append()

```TVMSolidity
<bytes>.append(bytes tail);
```

Modifies the `bytes` by concatenating **tail** data to the end of the `bytes`.

##### bytes conversion

```TVMSolidity
bytes byteArray = "1234";
bytes4 bb = byteArray;
```

`bytes` can be converted to `bytesN`.
If `bytes` object has less than **N** bytes, extra bytes are padded with zero bits.

#### string

TVM Solidity compiler expands `string` type with the following functions:

**Note**: Due to VM restrictions string length can't exceed `1024 * 127 = 130048` bytes.

`string` can be converted to `TvmSlice`.

##### \<string\>.empty()

```TVMSolidity
<string>.empty() returns (bool);
```

Returns status flag whether the `string` is empty (its length is 0).

##### \<string\>.byteLength()

```TVMSolidity
<string>.byteLength() returns (uint32);
```

Returns byte length of the `string` data.

##### \<string\>.substr()

```TVMSolidity
<string>.substr(uint from[, uint count]) returns (string);
```

Returns the substring starting from the byte with number **from** with byte length **count**.  
**Note**: if count is not set, then the new `string` will be cut from the **from** byte to the end
of the string.

```TVMSolidity
string long = "0123456789";
string a = long.substr(1, 2); // a = "12"
string b = long.substr(6); // b = "6789"
```

##### \<string\>.append()

```TVMSolidity
<string>.append(string tail);
```

Appends the tail `string` to the `string`.

##### \<string\>.operator+

```TVMSolidity
<string>.operator+(string) returns (string);
<string>.operator+(bytesN) returns (string);
```

Creates new `string` as a concatenation of left and right arguments of the operator +. Example:

```TVMSolidity
string a = "abc";
bytes2 b = "12";
string c = a + b; // "abc12"
```

##### \<string\>.find() and \<string\>.findLast()

```TVMSolidity
<string>.find(bytes1 symbol) returns (optional(uint32));
<string>.find(string substr) returns (optional(uint32));
<string>.findLast(bytes1 symbol) returns (optional(uint32));
```

Looks for **symbol** (or substring) in the `string` and returns index of the first (`find`) or the
last (`findLast`) occurrence. If there is no such symbol in the `string`, an empty optional is returned.

Example:

```TVMSolidity
string str = "01234567890";
optional(uint32) a = str.find(byte('0'));
bool s = a.hasValue(); // s == true
uint32 n = a.get(); // n == 0

byte symbol = 'a';
optional(uint32) b = str.findLast(symbol);
bool s = b.hasValue(); // s == false

string sub = "111";
optional(uint32) c = str.find(sub);
bool s = c.hasValue(); // s == false
```

##### \<string\>.dataSize()

```TVMSolidity
<string>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Same as [\<TvmCell\>.dataSize()](#tvmcelldatasize).

##### \<string\>.dataSizeQ()

```TVMSolidity
<string>.dataSizeQ(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Same as [\<TvmCell\>.dataSizeQ()](#tvmcelldatasizeq).

##### \<string\>.toUpperCase()` and \<string\>.toLowerCase()

```TVMSolidity
<string>.toUpperCase() returns (string)
<string>.toLowerCase() returns (string)
```

Converts `string` to upper/lower case. It treats `string` as ASCII string and converts only 'A'..'Z'
and 'a'..'z' symbols. Example:

```TVMSolidity
string s = "Hello";
string a = s.toUpperCase(); // a == "HELLO" 
string b = s.toLowerCase(); // b == "hello" 
```

##### format()

```TvmSolidity
format(string template, TypeA a, TypeB b, ...) returns (string);
```

Builds a `string` with arbitrary parameters. Empty placeholder {} can be filled with integer
(in decimal view), address, string or fixed point number. Fixed point number is printed
based on its type (`fixedMxN` is printed with `N` digits after dot).
Placeholder should be specified in such formats:

* `"{}"` - empty placeholder
* `"{:[0]<width>{"x","d","X","t"}}"` - placeholder for integers. Fills num with 0 if format starts with "0".
Formats integer to have specified `width`. Can format integers in decimal ("d" postfix), lower hex ("x")
or upper hex ("X") form. Format "t" prints number (in nanoevers) as a fixed point sum.

Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
string str = format("Hello {} 0x{:X} {}  {}.{} evers", 123, 255, address.makeAddrStd(-33,0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE), 100500, 32);
// str == "Hello 123 0xFF -21:7fffffffffffffffffffffffffffffffffffffffffffffffff123456789abcde  100500.32 evers"
str = format("Hello {}", 123); // str == "Hello 123"
str = format("Hello 0x{:X}", 123); // str == "Hello 0x7B"
str = format("{}", -123); // str == "-123"
str = format("{}", address.makeAddrStd(127,0)); // str == "7f:0000000000000000000000000000000000000000000000000000000000000000"
str = format("{}", address.makeAddrStd(-128,0)); // str == "-80:0000000000000000000000000000000000000000000000000000000000000000"
str = format("{:6}", 123); // str == "   123"
str = format("{:06}", 123); // str == "000123"
str = format("{:06d}", 123); // str == "000123"
str = format("{:06X}", 123); // str == "00007B"
str = format("{:6x}", 123); // str == "    7b"
uint128 a = 1 ever;
str = format("{:t}", a); // str == "1.000000000"
a = 123;
str = format("{:t}", a); // str == "0.000000123"
fixed32x3 v = 1.5;
str = format("{}", v); // str == "1.500"
fixed256x10 vv = -987123.4567890321;
str = format("{}", vv); // str == "-987123.4567890321"
```

##### stoi()

```TvmSolidity
stoi(string inputStr) returns (optional(int) /*result*/);
```

Converts a `string` into an integer. If `string` starts with '0x' it will be converted from a hexadecimal format,
otherwise it is meant to be number in decimal format. Function returns the integer in case of success.
Otherwise, returns `null`.

Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
optional(int) res;
res = stoi("123"); // res ==123
string hexstr = "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE";
res = stoi(hexstr); // res == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE
res = stoi("0xag"); // res == null
```

##### string conversion

```TVMSolidity
string s = "1";
bytes2 b = bytes2(s); // b == 0x3100

string s = "11";
bytes2 b = bytes2(s); // b = 0x3131
```

`string` can be converted to `bytesN` which causes **N** * 8 bits being loaded from the cell and saved to variable.
If `string` object has less than **N** bytes, extra bytes are padded with zero bits.

#### address

`address` represents different types of TVM addresses: **addr_none**, **addr_extern**,
**addr_std** and **addr_var**. TVM Solidity compiler expands `address` type with the following
members and functions:

##### Object creating

##### constructor()

```TVMSolidity
uint address_value;
address addrStd = address(address_value);
```

Constructs an `address` of type **addr_std** with zero workchain id and given address value.

##### address.makeAddrStd()

```TVMSolidity
int8 wid;
uint address;
address addrStd = address.makeAddrStd(wid, address);
```

Constructs an `address` of type **addr_std** with given workchain id **wid** and value **address_value**.

##### address.makeAddrNone()

```TVMSolidity
address addrNone = address.makeAddrNone();
```

Constructs an `address` of type **addr_none**.

##### address.makeAddrExtern()

```TVMSolidity
uint addrNumber;
uint bitCnt;
address addrExtern = address.makeAddrExtern(addrNumber, bitCnt);
```

Constructs an `address` of type **addr_extern** with given **value** with **bitCnt** bit-length.

##### Members

##### \<address\>.wid

```TVMSolidity
<address>.wid returns (int8);
```

Returns the workchain id of **addr_std** or **addr_var**. Throws "range check error" [exception](#tvm-exception-codes) for other `address` types.

##### \<address\>.value

```TVMSolidity
<address>.value returns (uint);
```

Returns the `address` value of **addr_std** or **addr_var** if **addr_var** has 256-bit `address` value. Throws "range check error" [exception](#tvm-exception-codes) for other `address` types.

##### \<address\>.balance

```TVMSolidity
address(this).balance returns (uint128);
```

Returns balance of the current contract account in nanoevers.

##### \<address\>.currencies

```TVMSolidity
address(this).currencies returns (ExtraCurrencyCollection);
```

Returns currencies on the balance of the current contract account.

##### Functions

##### \<address\>.getType()

```TVMSolidity
<address>.getType() returns (uint8);
```

Returns type of the `address`:
0 - **addr_none**
1 - **addr_extern**
2 - **addr_std**

##### \<address\>.isStdZero()

```TVMSolidity
<address>.isStdZero() returns (bool);
```

Returns the result of comparison between this `address` with zero `address` of type **addr_std**.

##### \<address\>.isStdAddrWithoutAnyCast()

```TVMSolidity
<address>.isStdAddrWithoutAnyCast() returns (bool);
```

Checks whether this `address` is of type **addr_std** without any cast.

##### \<address\>.isExternZero()

```TVMSolidity
<address>.isExternZero() returns (bool);
```

Returns the result of comparison between this `address` with zero `address` of type **addr_extern**.

##### \<address\>.isNone()

```TVMSolidity
<address>.isNone() returns (bool);
```

Checks whether this `address` is of type **addr_none**.

##### \<address\>.unpack()

```TVMSolidity
<address>.unpack() returns (int32 /*wid*/, uint256 /*value*/);
```

Parses `address` containing a valid `MsgAddressInt` (`addr_std`), applies rewriting
from the anycast (if exists) to the same-length prefix of the address, and returns both the
workchain `wid` and the 256-bit address `value`. If the address `value` is not 256-bit, or if
`address` is not a valid serialization of `MsgAddressInt`, throws a cell deserialization
[exception](#tvm-exception-codes).

It's wrapper for opcode `REWRITESTDADDR`.

Example:

```TVMSolidity
(int32 wid, uint addr) = address(this).unpack();
```

##### \<address\>.transfer()

```TVMSolidity
<address>.transfer(uint128 value, bool bounce, uint16 flag, TvmCell body, ExtraCurrencyCollection currencies, TvmCell stateInit);
```

Sends an internal outbound message to the `address`. Function parameters:

* `value` (`uint128`) - amount of nanoevers sent attached to the message. Note: the sent value is
withdrawn from the contract's balance even if the contract has been called by internal inbound message.
* `currencies` (`ExtraCurrencyCollection`) - additional currencies attached to the message. Defaults to
an empty set.
* `bounce` (`bool`) - if it's set and transaction (generated by the internal outbound message) falls
(only at the computing phase, not at the action phase!) then funds will be returned. Otherwise, (flag isn't
set or transaction terminated successfully) the address accepts the funds even if the account
doesn't exist or is frozen. Defaults to `true`.
* `flag` (`uint16`) - flag that used to send the internal outbound message. Defaults to `0`.
* `body` (`TvmCell`) -  body (payload) attached to the internal message. Defaults to an empty
TvmCell.
* `stateInit` (`TvmCell`) - represents field `init` of `Message X`. If `stateInit` has a wrong
format, a cell underflow [exception](#tvm-exception-codes) at the computing phase is thrown.
See [here](https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb#L148).
Normally, `stateInit` is used in 2 cases: to deploy the contract or to unfreeze the contract.

All parameters can be omitted, except ``value``.

Possible values of parameter `flag`:

* `0` - message carries funds equal to the `value` parameter. Forward fee is subtracted from
the `value`.
* `128` - message carries all the remaining balance of the current smart contract. Parameter `value` is
ignored. The contract's balance will be equal to zero after the message processing.
* `64` - carries funds equal to the `value` parameter plus all the remaining value of the inbound message
(that initiated the contract execution).

Parameter `flag` can also be modified:

* `flag + 1` - means that the sender wants to pay transfer fees separately from contract's balance.
* `flag + 2` - means that any errors arising while processing this message during the action phase
should be ignored. But if the message has wrong format, then the transaction fails and `+ 2` has
no effect.
* `flag + 32` - means that the current account must be destroyed if its resulting balance is zero.
For example, `flag: 128 + 32` is used to send all balance and destroy the contract.

In order to clarify flags usage see [this sample](https://github.com/tonlabs/samples/blob/master/solidity/20_bomber.sol).

```TVMSolidity
address dest = ...;
uint128 value = ...;
bool bounce = ...;
uint16 flag = ...;
TvmCell body = ...;
ExtraCurrencyCollection c = ...;
TvmCell stateInit = ...;
// sequential order of parameters
addr.transfer(value);
addr.transfer(value, bounce);
addr.transfer(value, bounce, flag);
addr.transfer(value, bounce, flag, body);
addr.transfer(value, bounce, flag, body, c);
// using named parameters
destination.transfer({value: 1 ever, bounce: false, flag: 128, body: cell, currencies: c});
destination.transfer({bounce: false, value: 1 ever, flag: 128, body: cell});
destination.transfer({value: 1 ever, bounce: false, stateInit: stateInit});
```

See example of `address.transfer()` usage:

* [giver](https://github.com/tonlabs/samples/blob/master/solidity/7_Giver.sol)

#### mapping

TVM Solidity compiler expands `mapping` type with the following functions. In examples
below `\<map\>` defines the object of `mapping(KeyType => ValueType)` type.

Address, bytes, string, bool, contract, enum, fixed bytes, integer and struct types can
be used as a `KeyType`. Struct type can be used as `KeyType` only if it contains only
integer, boolean, fixed bytes or enum types and fits ~1023 bit. Example of mapping that
has a struct as a `KeyType`:

```TVMSolidity
struct Point {
    uint x;
    uint y;
    uint z;
}

mapping(Point => address) map;

function test(uint x, uint y, uint z, address addr) public {
    Point p = Point(x, y, z);
    map[p] = addr;
}
```

If you use `mapping(KeyType => TvmSlice) map;` be sure that sum of bit-length of `KeyType`
and bit-length of `TvmSlice` is less than 1023 bit.

Struct `KeyType` can be used to sort keys of the mapping in ascending order. In the example
above addresses in the mapping are sorted by their keys. `x` field of the Point struct
is used in comparison first, second is `y` and the last is `z`.

If `bytes`, `string` or `TvmCell` types are used as `KeyType` then `mapping` stores
only hashes of mapping keys. That's why for these types the `delMin`/`min`/`next` and
another mapping methods return `uint256` as key (not `bytes`/`string`/`TvmCell`).

If you use mapping as an input or output param for public/external functions,
`KeyType` of the mapping can only be of type `address` or of `int<M>`/`uint<M>` types with M from 8 to 256.

See example of how to work with mappings:

* [database](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollector.sol)
* [client](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollectorClient.sol)

##### Keyword `emptyMap`

Keyword `emptyMap` is a constant that is used to indicate a mapping of arbitrary type without values.

Example:

```TVMSolidity
struct Stakes {
    uint total;
    mapping(uint => uint) stakes;
}

// create struct with empty mapping `stakes`
Stakes stakes = Stakes({stakes: emptyMap, total: 200});
```

##### \<mapping\>.operator[]

```TVMSolidity
<map>.operator[](KeyType index) returns (ValueType);
```

Returns the item of `ValueType` with **index** key or returns the default value
if key is not in the mapping.

##### \<mapping\>.at()

```TVMSolidity
<map>.operator[](KeyType index) returns (ValueType);
```

Returns the item of `ValueType` with **index** key. Throws an [exception](#tvm-exception-codes) if key
is not in the mapping.

##### \<mapping\>.min() and \<mapping\>.max()

```TVMSolidity
<map>.min() returns (optional(KeyType, ValueType));
<map>.max() returns (optional(KeyType, ValueType));
```

Computes the minimal (maximal) key in the `mapping` and returns an `optional`
value containing that key and the associated value. If `mapping` is empty,
this function returns an empty `optional`.

##### \<mapping\>.next() and \<mapping\>.prev()

```TVMSolidity
<map>.next(KeyType key) returns (optional(KeyType, ValueType));
<map>.prev(KeyType key) returns (optional(KeyType, ValueType));
```

Computes the minimal (maximal) key in the `mapping` that is lexicographically
greater (less) than **key** and returns an `optional` value containing that
key and the associated value. Returns an empty `optional` if there is no such key.
If KeyType is an integer type, argument for this functions can not possibly fit `KeyType`.

Example:

```TVMSolidity
KeyType key;
// init key
optional(KeyType, ValueType) nextPair = map.next(key);
optional(KeyType, ValueType) prevPair = map.prev(key);

if (nextPair.hasValue()) {
    (KeyType nextKey, ValueType nextValue) = nextPair.get(); // unpack optional value
    // using nextKey and nextValue
}

mapping(uint8 => uint) m;
optional(uint8, uint) = m.next(-1); // ok, param for next/prev can be negative 
optional(uint8, uint) = m.prev(65537); // ok, param for next/prev can not possibly fit to KeyType (uint8 in this case)
```

##### \<mapping\>.nextOrEq() and \<mapping\>.prevOrEq()

```TVMSolidity
<map>.nextOrEq(KeyType key) returns (optional(KeyType, ValueType));
<map>.prevOrEq(KeyType key) returns (optional(KeyType, ValueType));
```

Computes the minimal (maximal) key in the `mapping` that is lexicographically greater than
or equal to (less than or equal to) **key** and returns an `optional` value containing that
key and the associated value. Returns an empty `optional` if there is no such key.
If KeyType is an integer type, argument for this functions can not possibly fit `KeyType`.

##### \<mapping\>.delMin() and \<mapping\>.delMax()

```TVMSolidity
<map>.delMin() returns (optional(KeyType, ValueType));
<map>.delMax() returns (optional(KeyType, ValueType));
```

If mapping is not empty then this function computes the minimal (maximum) key of the `mapping`,
deletes that key and the associated value from the `mapping` and returns an `optional` value
containing that key and the associated value. Returns an empty `optional` if there is no such key.

##### \<mapping\>.fetch()

```TVMSolidity
<map>.fetch(KeyType key) returns (optional(ValueType));
```

Checks whether **key** exists in the `mapping` and returns an `optional` with the associated value.
Returns an empty `optional` if there is no such key.

##### \<mapping\>.exists()

```TVMSolidity
<map>.exists(KeyType key) returns (bool);
```

Returns whether **key** exists in the `mapping`.

##### \<mapping\>.empty()

```TVMSolidity
<map>.empty() returns (bool);
```

Returns whether the `mapping` is empty.

##### \<mapping\>.replace()

```TVMSolidity
<map>.replace(KeyType key, ValueType value) returns (bool);
```

Sets the value associated with **key** only if **key** exists in the `mapping` and
returns the success flag.

##### \<mapping\>.add()

```TVMSolidity
<map>.add(KeyType key, ValueType value) returns (bool);
```

Sets the value associated with **key** only if **key** does not exist in the `mapping`.

##### \<mapping\>.getSet()

```TVMSolidity
<map>.getSet(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but also returns an `optional` with the
previous value associated with the **key**, if any. Otherwise, returns an empty `optional`.

##### \<mapping\>.getAdd()

```TVMSolidity
<map>.getAdd(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but only if **key** does not exist in the `mapping`.
Returns an `optional` with the old value without changing the dictionary if that value exists
in the `mapping`, otherwise returns an empty `optional`.

##### \<mapping\>.getDel()

```TVMSolidity
<map>.getDel(KeyType key) returns (optional(ValueType));
```

Deletes the **key** from the `mapping` **map** and returns an `optional` 
with the corresponding value. Returns an empty optional if the key does not exist.

##### \<mapping\>.getReplace()

```TVMSolidity
<map>.getReplace(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but only if **key** exists in the `mapping`.
On success, returns an `optional` with the old value associated with the **key**.
Otherwise, returns an empty `optional`.

#### Fixed point number

`fixed` / `ufixed`: Signed and unsigned fixed point number of various sizes. Keywords `ufixedMxN`
and `fixedMxN`, where `M` represents the number of bits taken by the type and `N` represents how
many decimal points are available. `M` must be divisible by 8 and goes from 8 to 256 bits. `N` must
be between 0 and 80, inclusive. `ufixed` and `fixed` are aliases for `ufixed128x18` and
`fixed128x18`, respectively.

Operators:

* Comparison: `<=`, `<`, `==`, `!=`, `>=`, `>` (evaluate to `bool`)
* Arithmetic operators: `+`, `-`, unary `-`, `*`, `/`, `%` (modulo)
* Math operations: [math.min(), math.max()](#mathmin-mathmax), [math.minmax()](#mathminmax),
[math.abs()](#mathabs), [math.divr(), math.divc()](#mathdivr-mathdivc)

#### \<mapping\>.keys() \<mapping\>.values()

```TVMSolidity
(1)
<map>.keys() returns (KeyType[]);
(2)
<map>.values() returns (ValueType[]);
```

(1) Returns all mapping's keys/values.
(2) Returns all values of the mapping as an array.
Note: these functions iterate over the whole mapping, thus the cost is proportional to the 
mapping's size.

```TVMSolidity
mapping(uint16 => uint8) map;
map[11] = 10;
map[22] = 20;
map[33] = 30;
uint16[] keys = map.keys(); // keys == [11, 22, 33] 
uint8[] values = map.values(); // values == [10, 20, 30] 
```

#### Function type

Function types are the types of functions. Variables of function type can be assigned from functions
and function parameters of function type can be used to pass functions to and return functions from
function calls.

If unassigned variable of function type is called then exception with code 65 is thrown.

```solidity
contract C {
  function getSum(int a, int b) internal pure returns (int) {
    return a + b;
  }

  function getSub(int a, int b) internal pure returns (int) {
    return a - b;
  }

  function router(uint8 mode) internal returns (function (int, int) returns (int)) {
    function (int, int) returns (int) func;
    if (mode == 0) {
      func = getSum;
    } else if (mode == 1) {
      func = getSub;
    }
    return func; // if `func` isn't initialized then exception is thrown
  }

  function process(int a, int b, uint8 mode) public returns (int) {
    return router(mode)(a, b);
  }
}
```

#### require, revert

In case of exception state variables of the contract are reverted to the state before
[tvm.commit()](#tvmcommit) or to the state of the contract before it was called.
Use error codes that are greater than 100 because other error codes can be
[reserved](#solidity-runtime-errors).
**Note**: if a nonconstant error code is passed as the function argument and the error code
is less than 2 then the error code will be set to 100.

##### require

```TVMSolidity
require(bool condition, [uint errorCode = 100, [Type exceptionArgument]]);
```

**require** function can be used to check the condition and throw an exception if the condition
is not met. The function takes condition and optional parameters: error code (unsigned integer)
and the object of any type.

Example:

```TVMSolidity
uint a = 5;

require(a == 5); // ok
require(a == 6); // throws an exception with code 100
require(a == 6, 101); // throws an exception with code 101
require(a == 6, 101, "a is not equal to six"); // throws an exception with code 101 and string
require(a == 6, 101, a); // throws an exception with code 101 and number a
```

##### revert

```TVMSolidity
revert(uint errorCode = 100, [Type exceptionArgument]);
```

**revert** function can be used to throw exceptions. The function takes an optional error code
(unsigned integer) and the object of any type.

Example:

```TVMSolidity
uint a = 5;
revert(); // throw exception 100
revert(101); // throw exception 101
revert(102, "We have a some problem"); // throw exception 102 and string
revert(101, a); // throw exception 101 and number a
```

#### Libraries

Libraries are similar to contracts, but they can't have state variables
and can't inherit nor be inherited. Libraries can be seen as implicit
base contracts of the contracts that use them. They will not be
explicitly visible in the inheritance hierarchy, but calls of library
functions look just like calls of functions of explicit base contracts
(using qualified access like `LibName.func(a, b, c)`). There is also
another way to call library function: `obj.func(b, c)`.
For now libraries are stored as a part of the code of the contact that
uses libraries. In the future, it can be changed.

##### Function call via library name

Example of using library in the manner `LibName.func(a, b, c)`:

```TVMSolidity
// file MathHelper.sol
pragma solidity >= 0.6.0;

// Library declaration
library MathHelper {
    // State variables are forbidden in library but constants are not
    uint constant MAX_VALUE = 300;

    // Library function
    function sum(uint a, uint b) internal pure returns (uint) {
        uint c = a + b;
        require(c < MAX_VALUE);
        return c;
    }
}


// file MyContract.sol
pragma solidity >= 0.6.0;

import "MathHelper.sol";

contract MyContract {
    uint s;

    function addValue(uint value) public returns (uint) {
        s = MathHelper.sum(s, value);
        return s;
    }
}
```

##### Function call via object

In TVM Solidity **arguments of a function call passed by value not by
reference**. It's effective for numbers and even for huge arrays.
See ([TVM][1] - A.2.3.2).
**But if a library function is called like `obj.func(b, c)` then the
first argument `obj` is passed by reference.**  It's similar to
the `self` variable in Python.
The directive `using A for B;` can be used to attach library functions
(from the library `A`) to any type (`B`) in the context of the contract.
These functions will receive the object they were called for as their
first parameter.
The effect of `using A for *;` is that the functions from the library
`A` are attached to all types.

Example of using library in the manner `obj.func(b, c)`:

```TVMSolidity
// file ArrayHelper.sol
pragma solidity >= 0.6.0;

library ArrayHelper {
    // Delete value from the `array` at `index` position
    function del(uint[] array, uint index) internal pure {
        for (uint i = index; i + 1 < array.length; ++i){
            array[i] = array[i + 1];
        }
        array.pop();
    }
}


// file MyContract.sol
pragma solidity >= 0.6.0;

import "ArrayHelper.sol";

contract MyContract {
    // Attach library function `del` to the type `uint[]`
    using ArrayHelper for uint[];

    uint[] array;

    constructor() public {
        array = [uint(100), 200, 300];
    }

    function deleteElement(uint index) public {
        // Library function call via object.
        // Note: library function `del` has 2 arguments:
        // array is passed by reference and index is passed by value
        array.del(index);
    }
}
```

### Import

TVM Solidity compiler allows user to import remote files using link starting with `http`.
If import file name starts with `http`, then compiler tries to download the file using this
link and saves it to the folder `.solc_imports`. If compiler fails to create this folder or
to download the file, then an error is emitted.

**Note**: to import file from GitHub, one should use link to the raw version of the file.

Example:

```TVMSolidity
pragma ever-solidity >= 0.35.0;
pragma AbiHeader expire;
pragma AbiHeader pubkey;

import "https://github.com/tonlabs/debots/raw/9c6ca72b648fa51962224ec0d7ce91df2a0068c1/Debot.sol";
import "https://github.com/tonlabs/debots/raw/9c6ca72b648fa51962224ec0d7ce91df2a0068c1/Terminal.sol";

contract HelloDebot is Debot {
  ...
}
```

### Pragmas

`pragma` keyword is used to enable certain compiler features or checks.
A pragma directive is always local to a source file, so you have to add
the pragma to all your files if you want to enable it in your whole project.
If you import another file, the pragma from that file is not
automatically applied to the importing file.

#### pragma ever-solidity

```TVMSolidity
pragma ever-solidity >= 0.35.5;      // Check if the compiler version is greater or equal than 0.35.5
pragma ever-solidity ^ 0.35.5;       // Check if the compiler version is greater or equal than 0.35.5 and less than 0.36.0
pragma ever-solidity < 0.35.5;       // Check if the compiler version is less than 0.35.5
pragma ever-solidity >= 0.35.5 < 0.35.7; // Check if the compiler version is equal to either 0.35.5 or 0.35.6
```

Used to restrict source file compilation to the particular compiler versions.

#### pragma-copyleft

```TVMSolidity
pragma copyleft <type>, <wallet_address>; 
```

It is an experimental feature available only in certain blockchain deployments.

Parameters: 
 * `<type>` (`uint8`) - copyleft type. 
 * `<wallet_address>` (`uint256`) - author's wallet address in masterchain.

If contract has the `copyleft` pragma, it means that after each transaction some part of validator's fee
is transferred to `<wallet_address>` according to the `<type>` rule.

For example:

```TVMSolidity
pragma copyleft 0, 0x2cfbdc31c9c4478b61472c72615182e9567595b857b1bba9e0c31cd9942f6ca41;
```

#### pragma ignoreIntOverflow

```TVMSolidity
pragma ignoreIntOverflow;
```

Turns off binary operation result overflow check.

#### pragma AbiHeader

```TVMSolidity
pragma AbiHeader notime;
pragma AbiHeader pubkey;
pragma AbiHeader expire;
```

Defines headers that are used in external messages:

* `notime` - disables `time` abi header, which is enabled by default. Abi header `time` – `uint64` local time when message was created, used for replay protection
* `pubkey` (`uint256`) - optional public key that the message can be signed with.
* `expire` (`uint32`)  - time when the message should be meant as expired.

**Note:**

Defined headers are listed in `*.abi.json` file in `header` section.

See also: [Contract execution](#contract-execution), [afterSignatureCheck](#aftersignaturecheck),
[msg.pubkey()](#msgpubkey) and [tvm.pubkey()](#tvmpubkey).
To read more about these fields and ABI follow this [link](https://docs.ton.dev/86757ecb2/p/40ba94-abi-specification-v2).
Here is example of [message expiration time](https://docs.ton.dev/86757ecb2/p/88321a-message-expiration-time) usage.

#### pragma msgValue

```TVMSolidity
pragma msgValue <value>;
```

Allows specifying default value in nanoevers attached to the
internal outbound messages used to call another contract. If it's not
specified, this value is set to 10 000 000 nanoevers.

Example:

```TVMSolidity
pragma msgValue 123456789;
pragma msgValue 1e8;
pragma msgValue 10 ever;
pragma msgValue 10_000_000_123;
```

#### pragma upgrade func/oldsol

```TVMSolidity
pragma upgrade func;
pragma upgrade oldsol;
```

Defines that code is compiled with special selector that is needed to upgrade FunC/Solidity contracts.

### State variables

#### Decoding state variables

You can decode state variables using tonos-cli. See `tonos-cli decode account --help`.

See also: [\<TvmSlice\>.loadStateVars()](#tvmsliceloadstatevars).

#### Keyword `constant`

For `constant` variables, the value has to be a compile time constant and this value is
substituted where the variable is used. The value has to be assigned where the variable is declared.
Example:

```TVMSolidity
contract MyContract {
    uint constant cost = 100;
    uint constant cost2 = cost + 200;
    address constant dest = address.makeAddrStd(-1, 0x89abcde);
}
```

#### Keyword `static`

Static state variables are used in the contract initial state generation.
Such variables can be set while deploying contract from contract
(onchain) or by tvm-linker (offchain). Example:

```TVMSolidity
contract C {
    uint static a; // ok
    uint static b = 123; // error
}
```

See also:

* [`code` option usage](#code-option-usage)
* [New contract address problem](#new-contract-address-problem)

#### Keyword `public`

For each public state variable, a getter function is generated. Generated
function has the same name and return type as the public variable. This
function can be called only locally. Public state variables are useful,
because you don't need to write functions that return a particular state variable manually.

Example:

```TVMSolidity
contract C {
    uint public a;
    uint public static b; // it's ok. Variable is both public and static.
}
```

### Special contract functions

#### receive

`receive` function is called in two cases:

1. [msg.body](#msgbody) (or message body) is empty.
2. [msg.body](#msgbody) starts with 32-bit zero. Then message body may contain data,
for example [string](#string) with comment.

If in the contract there is no `receive` function then the contract has an implicit empty `receive`
function.

```TVMSolidity
// file sink.sol
contract Sink {
    uint public counter = 0;
    uint public msgWithPayload = 0;
    receive() external {
        ++counter;
        // if the inbound internal message has payload then we can get it using `msg.body`
        TvmSlice s = msg.body;
        if (!s.empty()) {
            ++msgWithPayload;
        }
    }
}

// file bomber.sol
contract Bomber {
    // This function send evers 3 times to the Sink contract. Sink's function receive will handle 
    // that messages.
    function f(address addr) pure public {
        tvm.accept();
        addr.transfer({value: 1 ever}); // message's body is empty

        TvmBuilder b;
        addr.transfer({value: 1 ever, body: b.toCell()}); // message's body is empty, too

        b.store(uint32(0), "Thank you for the coffee!");
        // body of the message contains 32-bit zero number and the string
        addr.transfer({value: 20 ever, body: b.toCell()});
    }
}
```

##### fallback

`fallback` function is called on receiving an inbound internal/external message in such cases:

1. The message contains a function id that the contract doesn't contain.
2. Bit-length of the message is from 1 to 31 (including).
3. Bit-length of the message is equal to zero, but the message contains reference(s).

**Note**: if the message has correct function id but invalid encoded function parameters then
the transaction fail with an exception (e.g. [cell underflow exception](#tvm-exception-codes)).

If in the contract there is no fallback function then contract has implicit fallback function
that throws [exception](#solidity-runtime-errors).

Example:

```TVMSolidity
// file ContractA.sol
contract ContractA {
    uint public counter = 0;

    function f(uint a, uint b) public pure { /*...*/ }

    fallback() external {
        ++counter;
    }

}

// file ContractB.sol
import "ContractA.sol";
import "ContractAnother.sol";

contract ContractB {
    // Let's condider that `addr` is ContractA's address. 4 messages are send. ContractA's fallback
    // function will handle that messages except the last one.
    function g(address addr) public pure {
        tvm.accept();
        // The message contains a function id that the contract doesn't contain.
        // There is wrong casting to ContractAnother. `addr` is ContractA's address.
        ContractAnother(addr).sum{value: 1 ever}(2, 2);

        {
            TvmBuilder b;
            b.storeUnsigned(1, 1);
            // Bit-length of the message is equal to 20 bits.
            addr.transfer({value: 2 ever, body: b.toCell()});
        }

        {
            TvmBuilder b;
            b.storeRef(b);
            // Bit-length of the message is equal to zero but the message contains one reference.
            addr.transfer({value: 1 ever, body: b.toCell()});
        }

        TvmBuilder b;
        uint32 id = tvm.functionId(ContractA.f);
        b.store(id, uint(2));
        // ContractA's fallback function won't be called because the message body doesn't contain
        // the second ContractA.f's parameter. It will cause cell underflow exception in ContractA.
        addr.transfer({value: 1 ever, body: b.toCell()});
    }
}
```

#### onBounce

```TVMSolidity
onBounce(TvmSlice body) external {
    /*...*/
}
```

`onBounce` function is executed when contract receives a bounced inbound internal message.
The message is generated by the network if the contract sends an internal message with `bounce: true` and either

* called contract doesn't exist;
* called contract fails at the storage/credit/computing phase (not at the action phase!)

The message is generated only if the remaining message value is enough for sending one back.

`body` is empty or contains at most **256** data bits of the original message (without references).
The function id takes **32** bits and parameters can take at most **224** bits.
It depends on the network config. If `onBounce` function is not defined then the contract does
nothing on receiving a bounced inbound internal message.

If the `onBounce` function throws an exception then another bounced messages are not generated.

Example of how to use `onBounce` function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### onTickTock

`onTickTock` function is executed on tick/tock transaction.
These transactions are automatically generated for certain special accounts.
See ([TBLKCH][2] - 4.2.4.)
For tick transactions **isTock** is false, for tock transactions - true.

Prototype:

```TVMSolidity
onTickTock(bool isTock) external {
    /*...*/
}
```

#### onCodeUpgrade

`onCodeUpgrade` function can have an arbitrary set of arguments and should be
executed after [tvm.setcode()](#tvmsetcode) function call. In this function
[tvm.resetStorage()](#tvmresetstorage) should be called if the set of state
variables is changed in the new version of the contract. This function implicitly
calls [tvm.commit()](#tvmcommit). `onCodeUpgrade` function does not return value. `onCodeUpgrade` function
finishes TVM execution with exit code 0.

Prototype:

```TVMSolidity
function onCodeUpgrade(...) private {
    /*...*/
}
```

Function `onCodeUpgrade` had function id = 2 (for compiler <= 0.65.0). Now, it has another id, but you can set 
`functionID(2)` in new contracts for the `onCodeUpgrade` to upgrade old ones.

See example of how to upgrade code of the contract:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

It's good to pass `TvmCell cell` to the public function that calls `onCodeUpgrade(TvmCell cell, ...)`
function. `TvmCell cell` may contain some data that may be useful for the new contract.

```TVMSolidity
// old contract
// Public function that changes the code and takes some cell
function updateCode(TvmCell newcode, TvmCell cell) public pure checkPubkeyAndAccept {
    tvm.setcode(newcode);
    tvm.setCurrentCode(newcode);
    // pass cell to new contract
    onCodeUpgrade(cell);
}

function onCodeUpgrade(TvmCell cell) private pure {
}


// new contract
function onCodeUpgrade(TvmCell cell) private pure {
    // new code can use cell that was passed from the old version of the contract
}
```

#### afterSignatureCheck

```TVMSolidity
function afterSignatureCheck(TvmSlice body, TvmCell message) private inline returns (TvmSlice) {
    /*...*/
}
```

`afterSignatureCheck` function is used to define custom replay protection function instead of the
default one.

NB: Do not use [tvm.commit()](#tvmcommit) or [tvm.accept()](#tvmaccept) in this function as it called before the constructor call.
**time** and **expire** header fields can be used for replay protection and, if set, they should be read in `afterSignatureCheck`.
See also: [Contract execution](#contract-execution).
See an example of how to define this function:

* [Custom replay protection](https://github.com/tonlabs/samples/blob/master/solidity/14_CustomReplayProtection.sol)

### Function specifiers

#### Function mutability: pure, view and default

Function mutability shows how this function treats state variables.
Possible values of the function mutability:

* `pure` - function neither reads nor assigns state variables;
* `view` - function may read but doesn't assign state variables;
* default - function may read and/or assign state variables.

Example:

```TVMSolidity
contract Test {

    uint a;

    event MyEvent(uint val);

    // pure mutability
    function f() public pure {
        emit MyEvent(123);
    }

    // view mutability
    function g() public view {
        emit MyEvent(a);
    }

    // default mutability (not set)
    function e(uint newA) public {
        a = newA;
    }
}
```

#### Keyword inline

`inline` specifier instructs the compiler to insert a copy of the private function
body into each place where the function is called.
Keyword can be used only for private and internal functions.

Example:

```TVMSolidity
// This function is called as a usual function.
function getSum(uint a, uint b) public returns (uint) {
    return sum(a, b);
}

// Code of this function is inserted in place of the call site.
function sum(uint a, uint b) private inline returns (uint) {
    return a + b;
}
```

#### Assembly

To make inline assembler you should mark free function as `assembly`. Function body must contain lines of assembler code separated by commas.

It is up to user to set correct mutability (`pure`, `view` or default), return parameters of the function and so on. 

```TVMSolidity
function checkOverflow(uint a, uint b) assembly pure returns (bool) {
    "QADD",
    "ISNAN",
    "NOT",
}

contract Contract {
    function f(uint a, uint b) private {
        bool ok =  checkOverflow(a, b);
        if (ok) {
            uint c = a + b;
        }
    }
}
```

You can use inline assembler to support new opcodes in experimental or another implementations of TVM. 

```TVMSolidity
function incomingValue() assembly pure returns (uint) {
    ".blob xF82B", // it's opcode INCOMINGVALUE 
}
```

#### functionID()

`functionID` keyword allows assigning function identifier explicitly.
Each public function has a unique 32-bit identifier (id). id 0 is reserved for [receive](#receive) function.
In case `functionID` is not defined explicitly, it is calculated as a hash of the function signature.
In general, there is no purpose to set the function id manually.

Example:

```TVMSolidity
function f() public pure functionID(123) {
    /*...*/
}
 ```

#### externalMsg and internalMsg

Keywords `externalMsg` and `internalMsg` specify which messages the function can handle.
If the function marked by keyword `externalMsg` is called by internal message, the function throws an
exception with code 71.
If the function marked by keyword `internalMsg` is called by external message, the function throws
an exception with code 72.

Example:

```TVMSolidity
function f() public externalMsg { // this function receives only external messages
    /*...*/
}

// Note: keyword `external` specifies function visibility
function ff() external externalMsg { // this function also receives only external messages
    /*...*/
}

function g() public internalMsg { // this function receives only internal messages
    /*...*/
}

// These function receives both internal and external messages.
function fun() public { /*...*/ }
```

### Events and return

#### emit

`emit` statement sends an external outbound message. Use `{dest: ...}`to set destination address.
The address must be of **addr_extern** type.

If option `dest` is not used, the destination address is set to **addr_none**.

**Note:** fee for creating the external outbound message is withdrawn from the contract's
balance even if the contract has been called by internal inbound message.

Example:

```TVMSolidity
event SomethingIsReceived(uint a, uint b, uint sum);
...
address addr = address.makeAddrExtern(...);
emit SomethingIsReceived{dest: addr}(2, 8, 10); // dest address is set
emit SomethingIsReceived(10, 15, 25); // dest address == addr_none
```

#### return

`return` statement has different effects depending on:

* visibility of the function;
* type of the inbound message;
* presence of the `responsible` modifier.

In `private` or `internal` visible functions, `return` assigns the return variables to the specified parameters;
In `public` or `external` visible functions, `return` statement:

* on an external inbound message, generates an external outbound message with a destination address set to
the source address of the inbound (external) message. All arguments of the return statement are ignored.
* on an external inbound message:
  * if the function is marked as `responsible`, generates an internal outbound message;
  * otherwise has no effect.

`value`, `bounce`, `flag` and `currencies` options are used to create the message. Some options can
be omitted. See [\<address\>.transfer()](#addresstransfer) where these options and their default
values are described.

**Note**: if the function `f` below is called with `n = 5` and internal/external message must be
generated then only one message is sent with result `120` (120 = 5 * 4 * 3 * 2 * 1).

**Hint:** Use `{value: 0, bounce: false, flag: 64}` for `responsible` function.
Because `flag: 0` is used by default.

See also: [External function calls](#external-function-calls).

```TVMSolidity
function f(uint n) public pure {
    return n <= 1 ? 1 : n * f(n - 1);
}

function f(uint n) public responsible pure {
    return{value: 0, bounce: false, flag: 64} n <= 1 ? 1 : n * f(n - 1);
}
```

### External function calls

TVM Solidity compiler allows specifying different parameters of the outbound internal message that
is sent via external function call. Note, all external function calls are asynchronous, so
callee function will be called after termination of the current transaction.
`value`, `currencies`, `bounce` or `flag` options can be set. See [\<address\>.transfer()](#addresstransfer)
where these options are described.
**Note:** if `value` isn't set, then the default value is equal to 0.01 ever, or 10^7 nanoever. It's equal
to 10_000 units of gas in workchain.
If the callee function returns some value and marked as `responsible` then `callback` option must be set.
This callback function will be called by another contract. Remote function will pass its return
values as function arguments for the callback function. That's why types of return values of the
callee function must be equal to function arguments of the callback function.
If the function marked as `responsible` then field `answerId` appears in the list of input parameters of the
function in `*abi.json` file. `answerId` is function id that will be called.

Example of the external call of the function that returns nothing:

```TVMSolidity
interface IContract {
    function f(uint a) external;
}

contract Caller {
    function callExt(address addr) public {
        IContract(addr).f(123); // attached default value: 0.01 ever
        IContract(addr).f{value: 10 ever}(123);
        IContract(addr).f{value: 10 ever, flag: 3}(123);
        IContract(addr).f{value: 10 ever, bounce: true}(123);
        IContract(addr).f{value: 1 micro, bounce: false, flag: 128}(123);
        ExtraCurrencyCollection cc;
        cc[12] = 1000;
        IContract(addr).f{value: 10 ever, currencies:cc}(123);
    }
}
```

Example of the external call of the function that returns some values:

```TVMSolidity
contract RemoteContract {
    // Note this function is marked as responsible to call callback function
    function getCost(uint x) public pure responsible returns (uint) {
        uint cost = x == 0 ? 111 : 222;
        // return cost and set option for outbound internal message.
        return{value: 0, bounce: false, flag: 64} cost;
    }
}

contract Caller {
    function test(address addr, uint x) public pure {
        // `getCost` returns result to `onGetCost`
        RemoteContract(addr).getCost{value: 1 ever, callback: Caller.onGetCost}(x);
    }

    function onGetCost(uint cost) public {
        // Check if msg.sender is expected address
        // we get cost value, we can handle this value
    }
}
```

`*.abi.json` for `responsible` function `getCost`:

```json
{
    "name": "getCost",
    "inputs": [
        {"name":"answerId","type":"uint32"},
        {"name":"x","type":"uint256"}
    ],
    "outputs": [
        {"name":"value0","type":"uint256"}
    ]
}
```

See also:

* Example of callback usage: [24_SquareProvider](https://github.com/tonlabs/samples/blob/master/solidity/24_SquareProvider.sol)
* Example of callback usage: [4.1_CentralBank](https://github.com/tonlabs/samples/blob/master/solidity/4.1_CentralBank.sol)
and [4.1_CurrencyExchange.sol](https://github.com/tonlabs/samples/blob/master/solidity/4.1_CurrencyExchange.sol)
* [return](#return)

#### Synchronous calls

TVM Solidity compiler allows user to perform synchronous calls. To do it user should call a remote contract
function with `.await` suffix. Example:

```TVMSolidity
interface IContract {
    function getNum(uint a) external responsible returns (uint) ;
}

contract Caller {
    function call(address addr) public pure {
        ...
        uint res = IContract(addr).getNum(123).await;
        require(res == 124, 101);
        ...
    }
}
```

When function `call` is called this code:

1) Executes code before the `.await` call;
2) Generates and sends an internal message to **IContract** contract with address **addr**;
3) Saves current state of the contract including continuation, that is currently executed;
4) Waits for the answer from the remote contract;
5) If an internal message is received from the required address, contract unpacks the state and continues function execution.

Such await calls can be used everywhere in the code (e.g. inside a cycle), but be aware that usage of such
construction means that your contract function could possibly execute in more than one transaction.
Should be mentioned, that execution after the await call will spend money, that were attached to the responce
message. It means, that for correct work the receiver contract should make an accept after the await call,
or be sure that remote contract attaches enough currency for further execution.

**Note**: This feature was designed to be used in debots, in usual contracts use it at your own risk.

### Delete variables

As in classic Solidity `delete` operation assigns the initial value for the type to a variable.
Delete operation can be applied not only to variables itself, but to its fields or index values.

Example:

```TVMSolidity
int a = 5;
delete a; // a == 0

uint[] arr;
arr.push(11);
arr.push(22);
arr.push(33);

delete arr[1];
// arr[0] == 11
// arr[1] == 0
// arr[2] == 33

delete arr; // arr.length == 0

mapping(uint => uint) l_map;
l_map[1] = 2;
delete l_map[1];
bool e = l_map.exists(1); // e == false
l_map[1] = 2;
delete l_map;
bool e = l_map.exists(1); // e == false

struct DataStruct {
    uint m_uint;
    bool m_bool;
}

DataStruct l_struct;
l_struct.m_uint = 1;
delete l_struct; // l_struct.m_uint == 0

TvmBuilder b;
uint i = 0x54321;
b.store(i);
TvmCell c = b.toCell();
delete c;
TvmCell empty; // tvm.hash(empty) == tvm.hash(c)
b.store(c);

TvmSlice slice = b.toSlice();
uint16 bits = slice.bits(); // bits == 256
uint8 refs = slice.refs(); // refs == 1
delete slice;
uint16 bits = slice.bits(); // bits == 256
uint8 refs = slice.refs(); // refs == 1


uint16 bits = b.bits(); // bits == 256
uint8 refs = b.refs(); // refs == 1
delete b;
uint16 bits = b.bits(); // bits == 256
uint8 refs = b.refs(); // refs == 1
```

### API functions and members

#### Type information

The expression `type(T)` can be used to retrieve information about the type T. 

The following properties are available for an integer, variable integer and enum type `T`:
 * `type(T).min` - the smallest value representable by type `T`.

 * `type(T).max` - the largest value representable by type `T`.

#### **msg** namespace

##### msg.sender

```TVMSolidity
msg.sender (address)
```

Returns:

* sender of the message for internal message.
* address(0) for external message.
* address(0) for tick/tock transaction.

##### msg.value

```TVMSolidity
msg.value (uint128)
```

Returns:

* Balance of the inbound message in nanoevers for internal message.
* 0 for external message.
* Undefined value for tick/tock transaction.

##### msg.currencies

```TVMSolidity
msg.currencies (ExtraCurrencyCollection)
```

Collections of arbitrary currencies contained in the balance of
the inbound message.

##### msg.pubkey()

```TVMSolidity
msg.pubkey() returns (uint256);
```

Returns public key that is used to check the message signature. If the message isn't signed then it's equal to `0`.
See also: [Contract execution](#contract-execution), [pragma AbiHeader](#pragma-abiheader).

##### msg.isInternal, msg.isExternal and msg.isTickTock

Returns flag whether the contract is called by internal message, external message or by tick/tock transactions.

##### msg.createdAt

```TVMSolidity
msg.createdAt (uint32)
```

Returns the field **created_at** of the external inbound message.

##### msg.data

```TVMSolidity
msg.data (TvmCell)
```

Returns [the whole message](https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb#L155).

##### msg.forwardFee

```TVMSolidity
msg.forwardFee (varUint16)
```

Returns:
 * the [forward fee](https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb#L126) for the internal inbound message.   
 * `0` for the external inbound message.   

##### msg.importFee

```TVMSolidity
msg.importFee (varUint16)
```

Returns:
 * the field [import_fee](https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb#L130) for external inbound message. **Note:** field `import_fee` is set offchain by user as they want and does not reflect the real import fee of the message. 
 * `0` for the internal inbound message.

##### msg.body

```TVMSolidity
msg.body (TvmSlice)
```

Returns the payload (message body) of an inbound message.

##### msg.hasStateInit

```TVMSolidity
msg.hasStateInit (bool)
```

Whether the internal/external inbound message contains field `stateInit`.
Returns undefined value for tick/tock transaction. See [TL-B scheme][3] of `Message X`.

#### **tvm** namespace

##### TVM instructions

##### tvm.accept()

```TVMSolidity
tvm.accept();
```

Executes TVM instruction "ACCEPT" ([TVM][1] - A.11.2).
This instruction sets current gas limit to its maximal allowed value.
This action is required to process external messages that bring no value.

See example of how to use this function:

* [accumulator](https://github.com/tonlabs/samples/blob/master/solidity/1_Accumulator.sol)

##### tvm.setGasLimit()

```TVMSolidity
tvm.setGasLimit(uint g);
```

Executes TVM instruction "SETGASLIMIT" ([TVM][1] - A.11.2).
Sets current gas limit **g<sub>l</sub>** to the minimum of **g** and **g<sub>m</sub>**, and resets the gas
credit **g<sub>c</sub>** to zero. If the gas consumed so far (including the present instruction) exceeds
the resulting value of **g<sub>l</sub>**, an (unhandled) out of gas exception is thrown before setting
new gas limits. Notice that `tvm.setGasLimit(...)` with an argument **g** ≥ 2<sup>63</sup> - 1 is
equivalent to `tvm.accept()`.
`tvm.setGasLimit()` is similar to `tvm.accept()`. `tvm.accept()` sets gas limit **g<sub>l</sub>** to
the maximum possible value (depends on the network configuration parameters, usually is equal to
1_000_000 units of gas). `tvm.setGasLimit()` is generally used for accepting external messages and restricting
max possible gas consumption. It may be used to protect from flood by "bad" owner
in a contract that is used by multiple users. Let's consider some scenario:

1. Check whether msg.pubkey() != 0 and msg.pubkey() belongs to the list of trusted public keys;
2. Check whether `m_floodCounter[msg.pubkey()] < 5` where **m_floodCounter** is count of pending
operations of `msg.pubkey()` user.
3. `tvm.setGasLimit(75_000);` accept external message and set gas limit to 75_000.
4. `++m_floodCounter[msg.pubkey()];` increase count of pending operations for current users.
5. `tvm.commit();` save current state if it needs
6. Do other things.

So if some user's public key will be stolen, then a hacker can spam with external messages and
burn at most `5 * 75_000` units of gas instead of `5 * 1_000_000`, because we use `tvm.setGasLimit()` instead
of `tvm.accept()`.

##### tvm.buyGas()

```TVMSolidity
tvm.buyGas(uint value);
```

Computes the amount of gas that can be bought for `value` nanoevers, and sets **g<sub>l</sub>**  
accordingly in the same way as [tvm.setGasLimit()](#tvmsetgaslimit).

##### tvm.commit()

```TVMSolidity
tvm.commit();
```

Creates a "check point" of the state variables (by copying them from c7 to c4) and register c5.
If the contract throws an exception at the computing phase then the state variables and register c5
will roll back to the "check point", and the computing phase will be considered "successful".
If contract doesn't throw an exception, it has no effect.

##### tvm.rawCommit()

```TVMSolidity
tvm.rawCommit();
```

Same as [tvm.commit()](#tvmcommit) but doesn't copy the state variables from c7 to c4. It's a wrapper
for opcode `COMMIT`. See [TVM][1].

**Note**: Don't use `tvm.rawCommit()` after `tvm.accept()` in processing external messages because
you don't save from c7 to c4 the hidden state variable `timestamp` that is used for replay protection.

##### tvm.getData()

```TVMSolidity
tvm.getData() returns (TvmCell);
```

**Note:** Function is experimental.

A dual of the `tvm.setData()`function. It returns value of the `c4` register. Obtaining a raw storage 
cell can be useful when upgrading a new version of the contract that introduces an altered data layout.

Manipulation with a raw storage cell requires understanding of the way the compiler stores the data.
Refer to the description of `tvm.setData()` below to get more details.

**Note:** state variables and replay protection timestamp stored in the data cell have the same values
that were before the transaction. See [tvm.commit()](#tvmcommit) to learn about register `c4` update.

##### tvm.setData()

```TVMSolidity
tvm.setData(TvmCell data);
```

**Note:** Function is experimental.

Stores cell `data` in the register `c4`. Mind that after returning from a public function all state variables
from `c7` are copied to `c4` and `tvm.setData` will have no effect. Example hint, how to set `c4`:

```TVMSolidity
TvmCell data = ...;
tvm.setData(data); // set register c4
tvm.rawCommit();   // save register c4 and c5
revert(200);       // throw the exception to terminate the transaction
```

Be careful with the hidden state variable `timestamp` and think about possibility of external
messages replaying.

##### tvm.log()

```TVMSolidity
tvm.log(string log);
logtvm(string log);
```

Dumps `log` string. This function is a wrapper for TVM instructions
`PRINTSTR` (for constant literal strings shorter than 16 symbols) and
`STRDUMP` (for other strings). `logtvm` is an alias for `tvm.log(string)`. Example:

```TVMSolidity
tvm.log("Hello, world!");
logtvm("99_Bottles");

string s = "Some_text";
tvm.log(s);
```

**Note:** For long strings dumps only the first 127 symbols.

##### tvm.hexdump() and tvm.bindump()

```TVMSolidity
tvm.hexdump(T a);
tvm.bindump(T a);
```

Dumps cell data or integer. Note that for cells this function dumps data only
from the first cell. `T` must be an integer type or TvmCell.

Example:

```TVMSolidity
TvmBuilder b;
b.storeUnsigned(0x9876543210, 40);
TvmCell c = b.toCell();
tvm.hexdump(c);
tvm.bindump(c);
uint a = 123;
tvm.hexdump(a);
tvm.bindump(a);
int b = -333;
tvm.hexdump(b);
tvm.bindump(b);
```

Expected output for the example:

```TVMLog
CS<9876543210>(0..40)
CS<10011000011101100101010000110010000100001>(0..40)
7B
1111011
-14D
-101001101
```

##### tvm.setcode()

```TVMSolidity
tvm.setcode(TvmCell newCode);
```

This command creates an output action that would change this smart contract
code to that given by the `TvmCell` **newCode** (this change will take effect only
after the successful termination of the current run of the smart contract).

See example of how to use this function:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.configParam()

```TVMSolidity
tvm.configParam(uint8 paramNumber) returns (TypeA a, TypeB b, ...);
```

Executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832).
This command returns the value of the global configuration parameter with
integer index **paramNumber**. Argument should be an integer literal.
Supported **paramNumbers**: 1, 15, 17, 34.

##### tvm.rawConfigParam()

```TVMSolidity
tvm.rawConfigParam(uint8 paramNumber) returns (TvmCell cell, bool status);
```

Executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832).
Returns the value of the global configuration parameter with
integer index **paramNumber** as a `TvmCell` and a boolean status.

##### tvm.rawReserve()

```TVMSolidity
tvm.rawReserve(uint value, uint8 flag);
tvm.rawReserve(uint value, ExtraCurrencyCollection currency, uint8 flag);
```

Creates an output action that reserves **reserve** nanoevers. It is roughly equivalent to
create an outbound message carrying **reserve** nanoevers to oneself, so that the subsequent output
actions would not be able to spend more money than the remainder. It's a wrapper for opcodes
"RAWRESERVE" and "RAWRESERVEX". See [TVM][1].

Let's denote:

* `original_balance` is balance of the contract before the computing phase that is equal to balance
of the contract before the transaction minus storage fee. Note: `original_balance` doesn't include
`msg.value` and `original_balance` is not equal to `address(this).balance`.
* `remaining_balance` is contract's current remaining balance at the action phase after some handled
actions and before handing the "rawReserve" action.

Let's consider how much nanoevers (**reserve**) are reserved in all cases of **flag**:

* 0 -> `reserve = value` nanoevers.
* 1 -> `reserve = remaining_balance - value` nanoevers.
* 2 -> `reserve = min(value, remaining_balance)` nanoevers.
* 3 = 2 + 1 -> `reserve = remaining_balance - min(value, remaining_balance)` nanoevers.

* 4 -> `reserve = original_balance + value` nanoevers.
* 5 = 4 + 1 -> `reserve = remaining_balance - (original_balance + value)` nanoevers.
* 6 = 4 + 2 -> `reserve = min(original_balance + value, remaining_balance) = remaining_balance` nanoevers.
* 7 = 4 + 2 + 1 -> `reserve = remaining_balance - min(original_balance + value, remaining_balance)` nanoevers.

* 12 = 8 + 4 -> `reserve = original_balance - value` nanoevers.
* 13 = 8 + 4 + 1 -> `reserve = remaining_balance - (original_balance - value)` nanoevers.
* 14 = 8 + 4 + 2 -> `reserve = min(original_balance - value, remaining_balance)` nanoevers.
* 15 = 8 + 4 + 2 + 1 -> `reserve = remaining_balance - min(original_balance - value, remaining_balance)` nanoevers.

All other values of `flag` are invalid.

To make it clear, let's consider the order of `reserve` calculation:

1. if `flag` has bit `+8` then `value = -value`.
2. if `flag` has bit `+4` then `value += original_balance`.
3. Check `value >= 0`.
4. if `flag` has bit `+2` then `value = min(value, remaining_balance)`.
5. if `flag` has bit `+1` then `value = remaining_balance - value`.
6. `reserve = value`.
7. Check `0 <= reserve <= remaining_balance`.

Example:

```TVMSolidity
tvm.rawReserve(1 ever, 4 + 8);
```

See also: [23_rawReserve.sol](https://github.com/tonlabs/samples/blob/master/solidity/23_rawReserve.sol)

##### tvm.initCodeHash()

```TVMSolidity
tvm.initCodeHash() returns (uint256 hash)
```

Returns the initial code hash that contract had when it was deployed.

##### Hashing and cryptography

##### tvm.hash()

```TVMSolidity
tvm.hash(TvmCell cellTree) returns (uint256);
tvm.hash(string data) returns (uint256);
tvm.hash(bytes data) returns (uint256);
tvm.hash(TvmSlice data) returns (uint256);
```

Executes TVM instruction "HASHCU" or "HASHSU" ([TVM][1] - A.11.6. - F900).
It computes the representation hash of a given argument and returns
it as a 256-bit unsigned integer. For `string` and `bytes` it computes
hash of the tree of cells that contains data but not data itself.
See [sha256](#sha256) to count hash of data.

Example:

```TVMSolidity
uint256 hash = tvm.hash(TvmCell cellTree);
uint256 hash = tvm.hash(string);
uint256 hash = tvm.hash(bytes);
```

##### tvm.checkSign()

```TVMSolidity
(1)
tvm.checkSign(uint256 dataHash, uint256 SignHighPart, uint256 SignLowPart, uint256 pubkey) returns (bool);
(2)
tvm.checkSign(uint256 dataHash, TvmSlice signature, uint256 pubkey) returns (bool);
(3)
tvm.checkSign(TvmSlice data, TvmSlice signature, uint256 pubkey) returns (bool);
```

Executes TVM instruction "CHKSIGNU" ([TVM][1] - A.11.6. - F910) for variants 1 and 2.
This command checks the Ed25519-signature of the **dataHash** using public key **pubkey**.
Signature is represented by two uint256 **SignHighPart** and **SignLowPart** in the
first variant and by the slice **signature** in the second variant.
In the third variant executes TVM instruction "CHKSIGNS" ([TVM][1] - A.11.6. - F911).
This command checks Ed25519-signature of the **data** using public key **pubkey**.
Signature is represented by the slice **signature**.

Example:

```TVMSolidity
// option 1
uint256 dataHash;
uint256 SignHighPart;
uint256 SignLowPart;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(dataHash, SignHighPart, SignLowPart, pubkey);

// option 2
uint256 dataHash;
TvmSlice signature;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(dataHash, signature, pubkey);

// option 3
TvmSlice data;
TvmSlice signature;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(data, signature, pubkey);
```

##### Deploy contract from contract

##### tvm.insertPubkey()

```TVMSolidity
tvm.insertPubkey(TvmCell stateInit, uint256 pubkey) returns (TvmCell);
```

Inserts a public key into the `stateInit` data field. If the `stateInit` has wrong format then throws an exception.

##### tvm.buildStateInit()

```TVMSolidity
// 1)
tvm.buildStateInit(TvmCell code, TvmCell data) returns (TvmCell stateInit);
// 2)
tvm.buildStateInit(TvmCell code, TvmCell data, uint8 splitDepth) returns (TvmCell stateInit);
// 3)
tvm.buildStateInit({code: TvmCell code, data: TvmCell data, splitDepth: uint8 splitDepth,
    pubkey: uint256 pubkey, contr: contract Contract, varInit: {VarName0: varValue0, ...}});
```

Generates a `StateInit` ([TBLKCH][2] - 3.1.7.) from `code` and `data` `TvmCell`s.
Member `splitDepth` of the tree of cell `StateInit`:

1) is not set. Has no value.
2) is set. `0 <= splitDepth <= 31`
3) Arguments can also be set with names.
List of possible names:

* `code` (`TvmCell`) - defines the code field of the `StateInit`. Must be specified.
* `data` (`TvmCell`) - defines the data field of the `StateInit`. Conflicts with `pubkey` and
`varInit`. Can be omitted, in this case data field would be built from `pubkey` and `varInit`.
* `splitDepth` (`uint8`) - splitting depth. `0 <= splitDepth <= 31`. Can be omitted. By default,
it has no value.
* `pubkey` (`uint256`) - defines the public key of the new contract. Conflicts with `data`.
Can be omitted, default value is 0.
* `varInit` (`initializer list`) - used to set [static](#keyword-static) variables of the contract.
Conflicts with `data` and requires `contr` to be set. Can be omitted.
* `contr` (`contract`) - defines the contract whose `StateInit` is being built. Mandatory to be set if the
option `varInit` is specified.

Examples of this function usage:

```TvmSolidity
contract A {
    uint static var0;
    address static var1;
}

contract C {

    function f() public pure {
        TvmCell code;
        TvmCell data;
        uint8 depth;
        TvmCell stateInit = tvm.buildStateInit(code, data);
        stateInit = tvm.buildStateInit(code, data, depth);
    }

    function f1() public pure {
        TvmCell code;
        TvmCell data;
        uint8 depth;
        uint pubkey;
        uint var0;
        address var1;

        TvmCell stateInit1 = tvm.buildStateInit({code: code, data: data, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({code: code, splitDepth: depth, varInit: {var0: var0, var1: var1}, pubkey: pubkey, contr: A});
        stateInit1 = tvm.buildStateInit({varInit: {var0: var0, var1: var1}, pubkey: pubkey, contr: A, code: code, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({contr: A, varInit: {var0: var0, var1: var1}, pubkey: pubkey, code: code, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({contr: A, varInit: {var0: var0, var1: var1}, pubkey: pubkey, code: code});
        stateInit1 = tvm.buildStateInit({contr: A, varInit: {var0: var0, var1: var1}, code: code, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({pubkey: pubkey, code: code, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({code: code, splitDepth: depth});
        stateInit1 = tvm.buildStateInit({code: code});
    }
}
```

##### tvm.buildDataInit()

```TVMSolidity
tvm.buildDataInit({pubkey: uint256 pubkey, contr: contract Contract, varInit: {VarName0: varValue0, ...}});
```

Generates `data` field of the `StateInit` ([TBLKCH][2] - 3.1.7.). Parameters are the same as in
[tvm.buildStateInit()](#tvmbuildstateinit).

```TVMSolidity
// SimpleWallet.sol
contract SimpleWallet {
    uint static value;
    // ...
}

// Usage
TvmCell data = tvm.buildDataInit({
    contr: SimpleWallet,
    varInit: {mulRes: 12345},
    pubkey: 0x3f82435f2bd40915c28f56d3c2f07af4108931ae8bf1ca9403dcf77d96250827
});
TvmCell code = ...;
TvmCell stateInit = tvm.buildStateInit({code: code, data: data});

// Note, the code above can be simplified to:
TvmCell stateInit = tvm.buildStateInit({
    code: code,
    contr: SimpleWallet,
    varInit: {mulRes: 12345},
    pubkey: 0x3f82435f2bd40915c28f56d3c2f07af4108931ae8bf1ca9403dcf77d96250827
});
```

##### tvm.stateInitHash()

```TVMSolidity
tvm.stateInitHash(uint256 codeHash, uint256 dataHash, uint16 codeDepth, uint16 dataDepth) returns (uint256);
```

Calculates hash of the stateInit for given code and data specifications.

Example:

```TVMSolidity
TvmCell code = ...;
TvmCell data = ...;
uint codeHash = tvm.hash(code);
uint dataHash = tvm.hash(data);
uint16 codeDepth = code.depth();
uint16 dataDepth = data.depth();
uint256 hash = tvm.stateInitHash(codeHash, dataHash, codeDepth, dataDepth);
```

See also [internal doc](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/docs/internal/stateInit_hash.md) to read more about this
function mechanics.

##### Deploy via new

Either `code` or `stateInit` option must be set when you deploy a contract
from contract via keyword `new`. `stateInit` is a tree of cells that contains
original state of the contract. `stateInit` contains `data`, `code` and another members.
See also ([TBLKCH][2] - A.2.3.2) to read about `stateInit`.

Use `stateInit` option if you have the created account state (maybe offchain or
onchain) and use `code` if you want to create account state in the `new` expression.

**Note**: Address of the new account is calculated as a hash of the `stateInit`.
Constructor function parameters don't influence the address. See
[New contract address problem](#new-contract-address-problem).

[Step-by-step description how to deploy contracts from the contract here](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.md).  

Examples:

* [WalletProducer](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).
* [SelfDeployer](https://github.com/tonlabs/samples/blob/master/solidity/21_self_deploy.sol).

##### `stateInit` option usage

`stateInit` defines the origin state of the new account.

```TVMSolidity
TvmCell stateInit = ...;
address newWallet = new SimpleWallet{value: 1 ever, stateInit: stateInit}(arg0, arg1, ...);
```

##### `code` option usage

`code` option defines the code of the new contract.

```TVMSolidity
TvmCell code = ...;
address newWallet = new SimpleWallet{value: 1 ever, code: code}(arg0, arg1, ...);
```

The following options can only be used with the `code` option:

* `pubkey` (`uint256`) - defines the public key of the new contract.
* `varInit` (`initializer list`) - used to set [static](#keyword-static) variables of the new contract.
* `splitDepth` (`uint8`) - splitting depth. `0 <= splitDepth <= 31`. By default, it has no value.

Example of usage of these options:

```TVMSolidity
// file SimpleWallet.sol
...
contract SimpleWallet {
    address static m_owner;
    uint static m_value;
    ...
}

// file containing a contract that deploys a SimpleWallet
TvmCell code = ...;
address newWallet = new SimpleWallet{
    value: 1 ever,
    code: code,
    pubkey: 0xe8b1d839abe27b2abb9d4a2943a9143a9c7e2ae06799bd24dec1d7a8891ae5dd,
    splitDepth: 15,
    varInit: {m_owner: address(this), m_value: 15}
}(arg0, arg1, ...);
```

##### Other deploy options

The following options can be used with both `stateInit` and `code`:

* `value` (`uint128`) - funds attached to the outbound internal message, that creates new account.
This value must be set.
* `currencies` (`ExtraCurrencyCollection`) - currencies attached to the outbound internal message.
Defaults to an empty set.
* `bounce` (`bool`) - if it's set and deploy falls (only at the computing phase, not at the action
phase!) then funds will be returned. Otherwise, (flag isn't set or deploying terminated successfully)
the address accepts the funds. Defaults to `true`.
* `wid` (`uint8`) - workchain id of the new account address. Defaults to `0`.
* `flag` (`uint16`) - flag used to send the outbound internal message. Defaults to `0`.
Possible values of the `flag` are described here: [\<address\>.transfer()](#addresstransfer).

```TVMSolidity
TvmCell stateInit = ...;
address newWallet = new SimpleWallet{
    stateInit: stateInit,
    value: 1 ever,
    wid: -1,
    flag: 0
}(arg0, arg1, ...);
```

##### Deploy via \<address\>.transfer()

You can also deploy the contract via [\<address\>.transfer()](#addresstransfer).
Just set the option `stateInit`.

* [Example of usage](https://github.com/tonlabs/samples/blob/master/solidity/11_ContractDeployer.sol)
* [Step-by-step description how to deploy contracts from the contract here](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.md).

##### New contract address problem

Address of the new account is calculated as a hash of the `stateInit`.
Parameters of the constructor don't influence the address. The problem
is that hacker can deploy the contract with your `stateInit` before you
with malicious constructor parameters.

Let's consider how to protect against this problem:

1. Constructor is called by external message.
We must Check if we didn't forget to set the public key in the contract and the
inbound message is signed by that key. If hacker doesn't have your private
key then he can't sign message to call the constructor.
See [constructor of WalletProducer](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).
2. Constructor is called by internal message.
We should define static variable in the new contract that will contain
address of the creator. Address of the creator will be a part of the `stateInit`.
And in the constructor we must check address of the message sender.
See [function `deployWallet` how to deploy contract](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).  
See [constructor of SimpleWallet](https://github.com/tonlabs/samples/blob/master/solidity/17_SimpleWallet.sol).  
If some contract should deploy plenty of contracts (with some contract's
public key) then it's a good idea to declare static variable in the deployed
contract. This variable can contain some sequence number. It will allow
each new contact to have unique `stateInit`.
See [SimpleWallet](https://github.com/tonlabs/samples/blob/master/solidity/17_SimpleWallet.sol).  
**Note**: contract's public key (`tvm.pubkey()`) is a part of `stateInit`.

##### Misc functions from `tvm`

##### tvm.code()

```TVMSolidity
tvm.code() returns (TvmCell);
```

Returns contract's code.

See [SelfDeployer](https://github.com/tonlabs/samples/blob/master/solidity/21_self_deploy.sol).

##### tvm.codeSalt()

```TVMSolidity
tvm.codeSalt(TvmCell code) returns (optional(TvmCell) optSalt);
```

If **code** contains salt then **optSalt** contains one. Otherwise, **optSalt** doesn't contain any value.

##### tvm.setCodeSalt()

```TVMSolidity
tvm.setCodeSalt(TvmCell code, TvmCell salt) returns (TvmCell newCode);
```

Inserts **salt** into **code** and returns new code **newCode**.

##### tvm.pubkey()

```TVMSolidity
tvm.pubkey() returns (uint256);
```

Returns contract's public key, stored in contract data. If key is not set, function returns 0.

##### tvm.setPubkey()

```TVMSolidity
tvm.setPubkey(uint256 newPubkey);
```

Set new contract's public key. Contract's public key can be obtained from `tvm.pubkey`.

##### tvm.setCurrentCode()

```TVMSolidity
tvm.setCurrentCode(TvmCell newCode);
```

Changes this smart contract current code to that given by Cell **newCode**. Unlike [tvm.setcode()](#tvmsetcode)
this function changes code of the smart contract only for current TVM execution, but has no effect
after termination of the current run of the smart contract.

See example of how to use this function:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.resetStorage()

```TVMSolidity
tvm.resetStorage();
```

Resets all state variables to their default values.

##### tvm.functionId()

```TVMSolidity
// id of public function
tvm.functionId(functionName) returns (uint32);

// id of public constructor
tvm.functionId(ContractName) returns (uint32);
```

Returns the function id (uint32) of a public/external function or constructor.

Example:

```TVMSolidity
contract MyContract {
    constructor(uint a) public {
    }
        /*...*/
    }

    function f() public pure returns (uint) {
        /*...*/
    }

    function getConstructorID() public pure returns (uint32) {
        uint32 functionId = tvm.functionId(MyContract);
        return functionId;
    }

    function getFuncID() public pure returns (uint32) {
        uint32 functionId = tvm.functionId(f);
        return functionId;
    }
}
```

See example of how to use this function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

##### tvm.encodeBody()

```TVMSolidity
tvm.encodeBody(function, arg0, arg1, arg2, ...) returns (TvmCell);
tvm.encodeBody(function, callbackFunction, arg0, arg1, arg2, ...) returns (TvmCell);
tvm.encodeBody(contract, arg0, arg1, arg2, ...) returns (TvmCell);
```

Constructs a message body for the function call. Body can be used as a payload for  [\<address\>.transfer()](#addresstransfer).
If the **function** is `responsible`, **callbackFunction** must be set.

Example:

```TVMSolidity
contract Remote {
    constructor(uint x, uint y, uint z) public { /* */ }
    function func(uint256 num, int64 num2) public pure { /* */ }
    function getCost(uint256 num) public responsible pure returns (uint128) { /* */ }
}

// deploy the contract
TvmCell body = tvm.encodeBody(Remote, 100, 200, 300);
addr.transfer({value: 10 ever, body: body, stateInit: stateInit });

// call the function
TvmCell body = tvm.encodeBody(Remote.func, 123, -654);
addr.transfer({value: 10 ever, body: body});

// call the responsible function
TvmCell body = tvm.encodeBody(Remote.getCost, onGetCost, 105);
addr.transfer({value: 10 ever, body: body});
```

See also:

* [External function calls](#external-function-calls)
* [\<TvmSlice\>.loadFunctionParams()](#tvmsliceloadfunctionparams)
* [tvm.buildIntMsg()](#tvmbuildintmsg)

##### tvm.exit() and tvm.exit1()

```TVMSolidity
tvm.exit();
tvm.exit1();
```

Functions are used to save state variables and quickly terminate execution of the smart contract.
Exit codes are equal to zero and one for `tvm.exit` and `tvm.exit1` respectively.

Example:

```TVMSolidity
function g0(uint a) private {
    if (a == 0) {
        tvm.exit();
    }
    //...
}

function g1(uint a) private {
    if (a == 0) {
        tvm.exit1();
    }
    //...
}
```

##### tvm.buildExtMsg()

```TVMSolidity
tvm.buildExtMsg({
    dest: address,
    time: uint64,
    expire: uint32,
    call: {functionIdentifier [, list of function arguments]},
    sign: bool,
    pubkey: optional(uint256),
    callbackId: (uint32 | functionIdentifier),
    onErrorId: (uint32 | functionIdentifier),
    stateInit: TvmCell,
    signBoxHandle: optional(uint32),
    abiVer: uint8,
    flags: uint8
})
returns (TvmCell);
```

Function should be used only offchain and intended to be used only in debot contracts.
Allows creating an external inbound message, that calls the **func** function of the
contract on address **destination** with specified function arguments.

Mandatory parameters that are used to form a src field that is used for debots:

* `callbackId` - identifier of the callback function.
* `onErrorId` - identifier of the function that is called in case of an error.
* `signBoxHandle` - handle of the sign box entity, that engine will use to sign the message.

These parameters are stored in addr_extern and placed to the src field of the message.
Message is of type [ext_in_msg_info](https://github.com/ton-blockchain/ton/blob/24dc184a2ea67f9c47042b4104bbb4d82289fac1/crypto/block/block.tlb#L127)
and src address is of type [addr_extern](https://github.com/ton-blockchain/ton/blob/24dc184a2ea67f9c47042b4104bbb4d82289fac1/crypto/block/block.tlb#L101)
but stores special data:

* callback id - 32 bits;
* on error id - 32 bits;
* abi version - 8 bits; Can be specified manually and contain full abi version in little endian half bytes (e.g. version = "2.3" -> abiVer: 0x32)
* header mask - 3 bits in such order: time, expire, pubkey;
* optional value signBoxHandle - 1 bit (whether value exists) + \[32 bits\];
* control flags byte - 8 bits. 
  Currently used bits:
    1 - override time (dengine will replace time value with current time)
    2 - override exp (dengine will replace time value with actual expire value)
    4 - async call (dengine must send message and don't wait for the result)

Other function parameters define fields of the message:

* `time` - message creation timestamp. Used for replay attack protection, encoded as 64 bit Unix
time in milliseconds.
* `expire` - Unix time (in seconds, 32 bit) after that message should not be processed by contract.
* `pubkey` - public key from key pair used for signing the message body. This parameter is optional
and can be omitted.
* `sign` - constant bool flag that shows whether message should contain signature. If set to
**true**, message is generated with signature field filled with zeroes. This parameter is optional
and can be omitted (in this case is equal to **false**).

User can also attach stateInit to the message using `stateInit` parameter.

Function throws an exception with code 64 if function is called with wrong parameters (pubkey is set
and has value, but sign is false or omitted).

Example:

```TVMSolidity

interface Foo {
    function bar(uint a, uint b) external pure;
}

contract Test {

    TvmCell public m_cell;

    function generate0() public {
        tvm.accept();
        address addr = address.makeAddrStd(0, 0x0123456789012345678901234567890123456789012345678901234567890123);
        m_cell = tvm.buildExtMsg({callbackId: 0, onErrorId: 0, dest: addr, time: 0x123, expire: 0x12345, call: {Foo.bar, 111, 88}});
    }

    function generate1() public {
        tvm.accept();
        optional(uint) pubkey;
        address addr = address.makeAddrStd(0, 0x0123456789012345678901234567890123456789012345678901234567890123);
        m_cell = tvm.buildExtMsg({callbackId: 0, onErrorId: 0, dest: addr, time: 0x123, expire: 0x12345, call: {Foo.bar, 111, 88}, pubkey: pubkey});
    }

    function generate2() public {
        tvm.accept();
        optional(uint) pubkey;
        pubkey.set(0x95c06aa743d1f9000dd64b75498f106af4b7e7444234d7de67ea26988f6181df);
        address addr = address.makeAddrStd(0, 0x0123456789012345678901234567890123456789012345678901234567890123);
        optional(uint32) signBox;
        signBox.set(0x12333112);
        m_cell = tvm.buildExtMsg({callbackId: 0, onErrorId: 0, dest: addr, time: 0x1771c58ef9a, expire: 0x600741e4, call: {Foo.bar, 111, 88}, pubkey: pubkey, sign: true, signBoxHandle: signBox});
    }

}
```

External inbound message can also be built and sent with construction similar to remote contract
call. It requires suffix ".extMsg" and call options similar to `buildExtMsg` function call.
**Note**: this type of call should be used only offchain in debot contracts.

```TVMSolidity
interface Foo {
    function bar(uint a, uint b) external pure;
}

contract Test {

    function test7() public {
        address addr = address.makeAddrStd(0, 0x0123456789012345678901234567890123456789012345678901234567890123);
        Foo(addr).bar{expire: 0x12345, time: 0x123}(123, 45).extMsg;
        optional(uint) pubkey;
        optional(uint32) signBox;
        Foo(addr).bar{expire: 0x12345, time: 0x123, pubkey: pubkey}(123, 45).extMsg;
        Foo(addr).bar{expire: 0x12345, time: 0x123, pubkey: pubkey, sign: true}(123, 45).extMsg;
        pubkey.set(0x95c06aa743d1f9000dd64b75498f106af4b7e7444234d7de67ea26988f6181df);
        Foo(addr).bar{expire: 0x12345, time: 0x123, pubkey: pubkey, sign: true}(123, 45).extMsg;
        Foo(addr).bar{expire: 0x12345, time: 0x123, sign: true, signBoxHandle: signBox}(123, 45).extMsg;
        Foo(addr).bar{expire: 0x12345, time: 0x123, sign: true, signBoxHandle: signBox, abiVer: 0x32, flags: 0x07}(123, 45).extMsg;
    }
}
```

##### tvm.buildIntMsg()

```TVMSolidity
tvm.buildIntMsg({
    dest: address,
    value: uint128,
    call: {function, [callbackFunction,] arg0, arg1, arg2, ...},
    bounce: bool,
    currencies: ExtraCurrencyCollection
    stateInit: TvmCell
})
returns (TvmCell);
```

Generates an internal outbound message that contains a function call. The result `TvmCell` can be used to send a
message using [tvm.sendrawmsg()](#tvmsendrawmsg). If the `function` is `responsible` then
`callbackFunction` parameter must be set.

`dest`, `value` and `call` parameters are mandatory. Another parameters can be omitted. See
[\<address\>.transfer()](#addresstransfer) where these options and their default values are
described.

See also:

* sample [22_sender.sol](https://github.com/tonlabs/samples/blob/master/solidity/22_sender.sol)
* [tvm.encodeBody()](#tvmencodebody)

#### tvm.sendrawmsg()

```TVMSolidity
tvm.sendrawmsg(TvmCell msg, uint8 flag);
```

Send the internal/external message `msg` with `flag`. It's a wrapper for opcode
`SENDRAWMSG` ([TVM][1] - A.11.10).
Internal message `msg` can be generated by [tvm.buildIntMsg()](#tvmbuildintmsg).
Possible values of `flag` are described here: [\<address\>.transfer()](#addresstransfer).

**Note:** make sure that `msg` has a correct format and follows the [TL-B scheme][3] of `Message X`.
For example:

``` TVMSolidity
TvmCell msg = ...
tvm.sendrawmsg(msg, 2);
```

If the function is called by external message and `msg` has a wrong format (for example, the field
`init` of `Message X` is not valid) then the transaction will be replayed despite the usage of flag 2.
It will happen because the transaction will fail at the action phase.

#### **math** namespace

`T` is an integer, [variable integer](#varint-and-varuint) or fixed point type in the `math.*` functions where applicable.
Fixed point type is not applicable for `math.modpow2()`, `math.muldiv[r|c]()`, `math.muldivmod()` and `math.divmod()`.

##### math.min() math.max()

```TVMSolidity
math.min(T a, T b, ...) returns (T);
math.max(T a, T b, ...) returns (T);
```

Returns the minimal (maximal) value of the passed arguments. 

##### math.minmax()

```TVMSolidity
math.minmax(T a, T b) returns (T /*min*/, T /*max*/);
```

Returns minimal and maximal values of the passed arguments.

Example:

```TVMSolidity
(uint a, uint b) = math.minmax(20, 10); // (a, b) == (10, 20)
```

##### math.abs()

```TVMSolidity
math.abs(T val) returns (T);
```

Computes the absolute value of the given integer.

Example:

```TVMSolidity
int a = math.abs(-100); // a == 100
int b = -100;
int c = math.abs(b); // c == 100
```

##### math.modpow2()

```TVMSolidity
math.modpow2(T value, uint power) returns (T);
```

Computes the `value mod 2^power`. Note: `power` should be a constant integer.

Example:

```TVMSolidity
uint constant pow = 10;
uint val = 1026;
uint a = math.modpow2(21, 4); // a == 5
uint b = math.modpow2(val, pow); // b == 2
```

##### math.divr() math.divc()

```TVMSolidity
math.divc(T a, T b) returns (T);
math.divr(T a, T b) returns (T);
```

Returns result of the division of two integers. The return value is rounded. **ceiling** and **nearest** modes are used for `divc` and `divr`
respectively. See also: [Division and rounding](#division-and-rounding).

Example:

```TVMSolidity
int c = math.divc(10, 3); // c == 4
int c = math.divr(10, 3); // c == 3

fixed32x2 a = 0.25;
fixed32x2 res = math.divc(a, 2); // res == 0.13
```

##### math.muldiv() math.muldivr() math.muldivc()

```TVMSolidity
math.muldiv(T a, T b, T c) returns (T);
math.muldivr(T a, T b, T c) returns (T);
math.muldivc(T a, T b, T c) returns (T);
```

Multiplies two values and then divides the result by a third value. The return value is rounded. **floor**, **ceiling** and **nearest** modes are used for `muldiv`,
`muldivc` and `muldivr` respectively. See also: [Division and rounding](#division-and-rounding).

Example:

```TVMSolidity
uint res = math.muldiv(3, 7, 2); // res == 10
uint res = math.muldivr(3, 7, 2); // res == 11
uint res = math.muldivc(3, 7, 2); // res == 11
```

##### math.muldivmod()

```TVMSolidity
math.muldivmod(T a, T b, T c) returns (T /*result*/, T /*remainder*/);
```

This instruction multiplies first two arguments, divides the result by third argument and returns
the result and the remainder. Intermediate result is stored in the 514 bit buffer, and the final
result is rounded to the floor.

Example:

```TVMSolidity
uint a = 3;
uint b = 2;
uint c = 5;
(uint d, uint r) = math.muldivmod(a, b, c); // (d, r) == (1, 1)
int e = -1;
int f = 3;
int g = 2;
(int h, int p) = math.muldivmod(e, f, g); // (h, p) == (-2, 1)
```

##### math.divmod()

```TVMSolidity
math.divmod(T a, T b) returns (T /*result*/, T /*remainder*/);
```

This function divides the first number by the second and returns the result and the
remainder. Result is rounded to the floor.

Example:

```TVMSolidity
uint a = 11;
uint b = 3;
(uint d, uint r) = math.divmod(a, b); // (d, r) == (3, 2)

int e = -11;
int f = 3;
(int h, int p) = math.divmod(e, f); // (h, p) == (-3, 2)
```

##### math.sign()

```TVMSolidity
math.sign(T val) returns (int2);
```

Returns:
 * -1 if `val` is negative;
 * 0 if `val` is zero;
 * 1 if `val` is positive.

Example:

```TVMSolidity
int8 sign = math.sign(-100); // sign == -1
int8 sign = math.sign(100); // sign == 1
int8 sign = math.sign(0); // sign == 0
```

##### **tx** namespace

##### tx.logicaltime

```TVMSolidity
tx.logicaltime returns (uint64);
```

Returns the logical time of the current transaction.

##### tx.storageFee

It's an experimental feature and is available only in certain blockchain networks.

```TVMSolidity
tx.storageFee returns (uint120);
```

Returns the storage fee paid in the current transaction.

##### **block** namespace

##### block.timestamp

```TVMSolidity
block.timestamp returns (uint32);
```

Returns the current Unix time. Unix time is the same for the all transactions from one block. 

##### block.logicaltime

```TVMSolidity
block.logicaltime returns (uint64);
```

Returns the starting logical time of the current block.

##### **rnd** namespace

The pseudorandom number generator uses the random seed. The
initial value of the random seed before a smart contract execution in
TVM compatible blockchains is a hash of the smart contract address and the global
block random seed. If there are several runs of the same smart contract
inside a block, then all of these runs will have the same random seed.
This can be fixed, for example, by running `rnd.shuffle()` (without
parameters) each time before using the pseudorandom number generator.

##### rnd.next

```TVMSolidity
rnd.next([Type limit]) returns (Type);
```

Generates a new pseudo-random number.

1) Returns `uint256` number.
2) If the first argument `limit > 0` then function returns the value in the
range `0..limit-1`. Else if `limit < 0` then the returned value lies in range
`limit..-1`. Else if `limit == 0` then it returns `0`.

Example:

```TVMSolidity
// 1)
uint256 r0 = rnd.next(); // 0..2^256-1
// 2)
uint8 r1 = rnd.next(100);  // 0..99
int8 r2 = rnd.next(int8(100));  // 0..99
int8 r3 = rnd.next(int8(-100)); // -100..-1
```

##### rnd.getSeed

```TVMSolidity
rnd.getSeed() returns (uint256);
```

Returns the current random seed.

##### rnd.setSeed

```TVMSolidity
rnd.setSeed(uint256 x);
```

Sets the random seed to `x`.

##### rnd.shuffle

```TVMSolidity
(1)
rnd.shuffle(uint someNumber);
(2)
rnd.shuffle();
```

Randomizes the random seed.
(1) Mixes the random seed and `someNumber`. The result is set as the random seed.
(2) Mixes the random seed and the logical time of the current transaction.
The result is set as the random seed.

Example:

```TVMSolidity
// (1)
uint256 someNumber = ...;
rnd.shuffle(someNumber);
// (2)
rnd.shuffle();
```

#### abi namespace

##### abi.encode(), abi.decode()

```TVMSolidity
(1)
abi.encode(TypeA a, TypeB b, ...) returns (TvmCell /*cell*/);
(2)
abi.decode(TvmCell cell, (TypeA, TypeB, ...)) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

`abi.encode` creates `cell` from the values.
`abi.decode` decodes the `cell` and returns the values. Note: all types must be set in `abi.decode`.
Otherwise, `abi.decode` throws an exception.

Example:

```TVMSolidity
// pack the values to the cell
TvmCell cell = abi.encode(uint(1), uint(2), uint(3), uint(4));
// unpack the cell
(uint a, uint b, uint c, uint d) = abi.decode(cell, (uint, uint, uint, uint));
// a == 1
// b == 2
// c == 3
// d == 4
```

### **gosh** namespace

All `gosh.*` functions are experimental features and are available only in certain blockchain
networks.

#### gosh.diff and gosh.zipDiff


```TVMSolidity
(1)
gosh.diff(string oldText, string newText) returns (string patch)
(2)
gosh.zipDiff(bytes oldText, bytes newText) returns (bytes patch)
```
(1) Calculates [patch](https://en.wikipedia.org/wiki/Diff) between `oldText` and `newText`. Example:
(2) It's the same as `gosh.diff` but it calculates `patch` between compressed strings.


```TVMSolidity
string oldText = ...;
string newText = ...;
string patch = gosh.diff(oldText, newText);
```

#### gosh.applyPatch, gosh.applyPatchQ, gosh.applyZipPatch, gosh.applyZipPatchQ, gosh.applyZipBinPatch and gosh.applyZipBinPatchQ

```TVMSolidity
(1)
gosh.applyPatch(string oldText, string patch) returns (string newText)
gosh.applyPatchQ(string oldText, string patch) returns (optional(string) newText)
(2)
gosh.applyBinPatch(bytes oldText, bytes patch) returns (bytes newText)
gosh.applyBinPatchQ(bytes oldText, bytes patch) returns (optional(bytes) newText)
(3)
gosh.applyZipPatch(bytes oldText, bytes patch) returns (bytes newText)
gosh.applyZipPatchQ(bytes oldText, bytes patch) returns (optional(bytes) newText)
(4)
gosh.applyZipBinPatch(bytes oldText, bytes patch) returns (bytes newText)
gosh.applyZipBinPatchQ(bytes oldText, bytes patch) returns (optional(bytes) newText)
```

(1)
Applies `patch` to the `oldText`. If it's impossible (bad patch), `gosh.applyPatch` throws an exception with type check
error code (-8) but`gosh.applyPatchQ` returns `null`. Example:
(2)
These are the same as `gosh.applyPatch`/`gosh.applyPatchQ` but these functions are applied to binary arrays.
(3)
These are the same as `gosh.applyPatch`/`gosh.applyPatchQ` but these functions are applied to compressed strings.
(4)
These are the same as `gosh.applyPatch`/`gosh.applyPatchQ` but these functions are applied to compressed binary arrays.

```TVMSolidity
string oldText = ...;
string patch = ...;
string newText = gosh.applyPatch(oldText, patch);
```

#### gosh.zip and gosh.unzip

```TVMSolidity
gosh.zip(string text) returns (bytes zip)
gosh.unzip(bytes zip) returns (optional(string) text)
```

`gosh.zip` converts the `text` to compressed `bytes`. `gosh.unzip` reverts such compression.

#### Exponentiation

Exponentiation `**` is only available for unsigned types in the exponent. The resulting type of an
exponentiation is always equal to the type of the base. Please take care that it is large enough to
hold the result and prepare for potential assertion failures or wrapping behaviour.

Note that `0**0` throws an exception.

Example:

```TVMSolidity
uint b = 3;
uint32 p = 4;
uint res = b ** p; // res == 81
```

#### selfdestruct

```TVMSolidity
selfdestruct(address dest_addr);
```

Creates and sends the message that carries all the remaining balance
of the current smart contract and destroys the current account.

See example of how to use the `selfdestruct` function:

* [Kamikaze](https://github.com/tonlabs/samples/blob/master/solidity/8_Kamikaze.sol)

#### sha256

```TVMSolidity
// (1)
sha256(TvmSlice slice) returns (uint256)
// (2)
sha256(bytes b) returns (uint256)
// (3)
sha256(string str) returns (uint256)
```

1. Computes the SHA-256 hash. If the bit-length of `slice` is not divisible by eight, throws a cell
underflow [exception](#tvm-exception-codes). References of `slice` are not used to compute the hash. Only data bits located
in the root cell of `slice` are used.
2. Computes the SHA-256 hash only for the first 127 bytes. If `bytes.length > 127` then `b[128],
b[129], b[130] ...` elements are ignored.
3. Same as for `bytes`: only the first 127 bytes are taken into account.

See also [tvm.hash()](#tvmhash) to compute representation hash of the whole tree of cells.

#### gasToValue

```TVMSolidity
gasToValue(uint128 gas) returns (uint128 value)
gasToValue(uint128 gas, int8 wid) returns (uint128 value)
```

Returns worth of **gas** in workchain **wid**.
Throws an exception if **wid** is not equal to `0` or `-1`.
If `wid` is omitted than used the contract's `wid`.

#### valueToGas

```TVMSolidity
valueToGas(uint128 value) returns (uint128 gas)
valueToGas(uint128 value, int8 wid) returns (uint128 gas)
```

Counts how much **gas** could be bought on **value** nanoevers in workchain **wid**.
Throws an exception if **wid** is not equal to `0` or `-1`.
If `wid` is omitted than used the contract's `wid`.

#### gasleft

```TVMSolidity
gasleft() returns (uint64)
```

Returns the remaining gas. Supported only if `CapGasRemainingInsn` capability is set.

### TVM exception codes

| Name              | Code | Definition                                                                                                                                          |
|-------------------|:----:|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| Stack underflow   |  2   | Not enough arguments in the stack for a primitive                                                                                                   |
| Stack overflow    |  3   | More values have been stored on a stack than allowed by this version of TVM                                                                         |
| Integer overflow  |  4   | Integer does not fit into expected range (by default −2<sup>256</sup> ≤ x < 2<sup>256</sup>), or a division by zero has occurred                    |
| Range check error |  5   | Integer out of expected range                                                                                                                       |
| Invalid opcode    |  6   | Instruction or its immediate arguments cannot be decoded                                                                                            |
| Type check error  |  7   | An argument to a primitive is of incorrect value type                                                                                               |
| Cell overflow     |  8   | Error in one of the serialization primitives                                                                                                        |
| Cell underflow    |  9   | Deserialization error                                                                                                                               |
| Dictionary error  |  10  | Error while deserializing a dictionary object                                                                                                       |
| Unknown error     |  11  | Unknown error, may be thrown by user programs                                                                                                       |
| Fatal error       |  12  | Thrown by TVM in situations deemed impossible                                                                                                       |
| Out of gas        | -14  | Thrown by TVM when the remaining gas (g<sub>r</sub>) becomes negative. This exception cannot be caught and leads to an immediate termination of TVM |

See also: [TVM][1] - 4.5.7

### Solidity runtime errors

Smart-contract written in TVM Solidity can throw runtime errors while execution.

Solidity runtime error codes:
  * **40** - External inbound message has an invalid signature. See [tvm.pubkey()](#tvmpubkey) and [msg.pubkey()](#msgpubkey).
  * **50** - Array index or index of [\<mapping\>.at()](#mappingat) is out of range.
  * **51** - Contract's constructor has already been called.
  * **52** - Replay protection exception. See `timestamp` in [pragma AbiHeader](#pragma-abiheader).
  * **54** - `<array>.pop` call for an empty array.
  * **55** - See [tvm.insertPubkey()](#tvminsertpubkey).
  * **57** - External inbound message is expired. See `expire` in [pragma AbiHeader](#pragma-abiheader).
  * **58** - External inbound message has no signature but has public key. See `pubkey` in [pragma AbiHeader](#pragma-abiheader).
  * **60** - Inbound message has wrong function id. In the contract there are no functions with such function id and there is no fallback function that could handle the message. See [fallback](#fallback).
  * **61** - Deploying `StateInit` has no public key in `data` field.
  * **62** - Reserved for internal usage.
  * **63** - See [\<optional(T)\>.get()](#optionaltget).
  * **64** - `tvm.buildExtMSg()` call with wrong parameters. See [tvm.buildExtMsg()](#tvmbuildextmsg).
  * **66** - Convert an integer to a string with width less than number length. See [format()](#format).
  * **67** - See [gasToValue](#gastovalue) and [valueToGas](#valuetogas).
  * **68** - There is no config parameter 20 or 21.
  * **69** - Zero to the power of zero calculation (`0**0` in TVM Solidity style or `0^0`).
  * **70** - `string` method `substr` was called with substr longer than the whole string.
  * **71** - Function marked by `externalMsg` was called by internal message.
  * **72** - Function marked by `internalMsg` was called by external message.
  * **73** - The value can't be converted to enum type.
  * **74** - Await answer message has wrong source address.
  * **75** - Await answer message has wrong function id.
  * **76** - Public function was called before constructor.
  * **77** - It's impossible to convert `variant` type to target type. See [variant.toUint()](#varianttouint).
  * **78** - There's no private function with the function id.
  * **79** - You are deploying contract that uses [pragma upgrade func/oldsol](#pragma-upgrade-funcoldsol). Use the 
contract only for updating another contracts.

### Division and rounding

Let consider we have `x` and `y` and we want to divide `x` by `y`. Compute the quotient `q` and the
remainder `r` of the division of `x` by `y`: `x = y*q + r` where `|r| < |y|`.

In TVM there are 3 variants of rounding:

* **floor** - quotient `q` is rounded to −∞. `q = ⌊x/y⌋`, `r` has the same sign as `y`. This
rounding variant is used for operator `/`.
Example:

```TVMSolidity
int res = int(-2) / 3; // res == -1
int res = int(-5) / 10; // res == -1
int res = int(5) / 10; // res == 0
int res = int(15) / 10; // res == 1
```

* **ceiling** - quotient `q` is rounded to +∞. `q = ⌈x/y⌉`, `r` and `y` have opposite signs.
Example:

```TVMSolidity
int res = math.divc(-2, 3); // res == 0
int res = math.divc(-5, 10); // res == 0
int res = math.divc(5, 10); // res == 1
int res = math.divc(15, 10); // res == 2
```

* **nearest** - quotient `q` is rounded to the nearest number. `q = ⌊x/y + 1/2⌋` and `|r| ≤ |y|/2`.
Example:

```TVMSolidity
int res = math.divr(-2, 3); // res == -1
int res = math.divr(-5, 10); // res == 0
int res = math.divr(5, 10); // res == 1
int res = math.divr(15, 10); // res == 2
```

### Contract execution

Before executing any contract function special code is executed. In `*.code` file there are two special
functions: `main_internal` and `main_external` that run on internal and external messages
respectively. These functions initialize some internal global variables and call contract
function of special function like `receive`, `fallback`, `onBounce`, `onTickTock`, etc.

Before calling contract's function `main_external` does:

1. Checks the message signature. Let's consider how the signature is checked:
   - If signature exists and `pubkey` header isn't defined then `tvm.pubkey()` is used
   for checking.
   - If signature isn't exists and `pubkey` header isn't defined then signature isn't checked.
   - If signature exists, `pubkey` header is defined and `pubkey` isn't exists in the
   message then `tvm.pubkey()` is used for checking.
   - If signature exists, `pubkey` header is defined and `pubkey` exists in the
   message then `msg.pubkey()` is used for checking.
   - If signature isn't exists, `pubkey` header is defined and `pubkey` exists in the
   message then an [exception with code 58](#solidity-runtime-errors) is thrown.
2. Replay protection:
   - [*time* header](#pragma-abiheader) exists (`pragma AbiHeader notime` is not used) then the contract checks whether
   `oldTime` < `time` < `now * 1000 + 30 minutes`. If it's true then `oldTime` is updated by new `time`.
   Otherwise, an exception is thrown.
   - there is `afterSignatureCheck` (despite usage of `time`) then make your own replay protection.
3. Message expiration:
   - `expire` exists and there is no `afterSignatureCheck` then the contract checks whether
   `expire` > `now`.
   - there is `afterSignatureCheck` (despite usage of `expire`) then make your own check.

See also: [pragma AbiHeader](#pragma-abiheader), [afterSignatureCheck](#aftersignaturecheck).

### Gas optimization hints

Try to reduce count of `[]` operations for mappings and arrays. For example:

```TVMSolidity
Struct Point {
    uint x;
    uint y;
    uint z;
}

Point[] points;
```

Here we have 3 `[]` operations:

```TVMSolidity
points[0].x = 5;
points[0].y = 10;
points[0].z = -5;
```

We can use a temp variable:

```TVMSolidity
Point p = points[0];
p.x = 5;
p.y = 10;
p.z = -5;
points[0] = p;
```

[1]: https://test.ton.org/tvm.pdf        "TVM"
[2]: https://test.ton.org/tblkch.pdf     "TBLKCH"
[3]: https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb "TL-B scheme"
