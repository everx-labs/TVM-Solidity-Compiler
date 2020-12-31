# **TON Solidity API**

TON Solidity compiler expands Solidity language with different API
functions to facilitate TON contract development.

## Table of Contents

* [TON specific types](#ton-specific-types)
  * [TON units](#ton-units)
  * [TvmCell](#tvmcell)
    * [\<TvmCell\>.depth()](#tvmcelldepth)
    * [\<TvmCell\>.dataSize()](#tvmcelldatasize)
    * [\<TvmCell\>.dataSizeQ()](#tvmcelldatasizeq)
    * [\<TvmCell\>.toSlice()](#tvmcelltoslice)
  * [TvmSlice](#tvmslice)
    * [\<TvmSlice\>.size()](#tvmslicesize)
    * [\<TvmSlice\>.dataSize()](#tvmslicedatasize)
    * [\<TvmSlice\>.dataSizeQ()](#tvmslicedatasizeq)
    * [\<TvmSlice\>.bits()](#tvmslicebits)
    * [\<TvmSlice\>.refs()](#tvmslicerefs)
    * [\<TvmSlice\>.depth()](#tvmslicedepth)
    * [\<TvmSlice\>.decode()](#tvmslicedecode)
    * [\<TvmSlice\>.loadRef()](#tvmsliceloadref)
    * [\<TvmSlice\>.loadRefAsSlice()](#tvmsliceloadrefasslice)
    * [\<TvmSlice\>.loadSigned()](#tvmsliceloadsigned)
    * [\<TvmSlice\>.loadUnsigned()](#tvmsliceloadunsigned)
    * [\<TvmSlice\>.loadTons()](#tvmsliceloadtons)
    * [\<TvmSlice\>.decodeFunctionParams()](#tvmslicedecodefunctionparams)
  * [TvmBuilder](#tvmbuilder)
    * [\<TvmBuilder\>.toSlice()](#tvmbuildertoslice)
    * [\<TvmBuilder\>.toCell()](#tvmbuildertocell)
    * [\<TvmBuilder\>.bits()](#tvmbuilderbits)
    * [\<TvmBuilder\>.refs()](#tvmbuilderrefs)
    * [\<TvmBuilder\>.bitsAndRefs()](#tvmbuilderbitsandrefs)
    * [\<TvmBuilder\>.remBits()](#tvmbuilderrembits)
    * [\<TvmBuilder\>.remRefs()](#tvmbuilderremrefs)
    * [\<TvmBuilder\>.remBitsAndRefs()](#tvmbuilderrembitsandrefs)
    * [\<TvmBuilder\>.depth()](#tvmbuilderdepth)
    * [\<TvmBuilder\>.store()](#tvmbuilderstore)
    * [\<TvmBuilder\>.storeSigned()](#tvmbuilderstoresigned)
    * [\<TvmBuilder\>.storeUnsigned()](#tvmbuilderstoreunsigned)
    * [\<TvmBuilder\>.storeRef()](#tvmbuilderstoreref)
    * [\<TvmBuilder\>.storeTons()](#tvmbuilderstoretons)
  * [ExtraCurrencyCollection](#extracurrencycollection)
  * [optional(Type)](#optionaltype)
    * [constructing an optional](#constructing-an-optional)
    * [\<optional(Type)\>.hasValue()](#optionaltypehasvalue)
    * [\<optional(Type)\>.get()](#optionaltypeget)
    * [\<optional(Type)\>.set()](#optionaltypeset)
    * [\<optional(Type)\>.reset()](#optionaltypereset)
* [TON specific control structures](#ton-specific-control-structures)
  * [Range-based for loop](#range-based-for-loop)
  * [repeat](#repeat)
* [Changes and extensions in solidity types](#changes-and-extensions-in-solidity-types)
  * [struct](#struct)
    * [\<struct\>.unpack()](#structunpack)
  * [string](#string)
    * [\<string\>.byteLength()](#stringbytelength)
    * [\<string\>.substr()](#stringsubstr)
    * [string(int)](#stringint)
    * [hexstring()](#hexstring)
    * [format()](#format)
    * [stoi()](#stoi)
  * [bytes](#bytes)
    * [\<bytes\>.operator[]](#bytesoperator)
    * [\<bytes\>.length](#byteslength)
    * [\<bytes\>.toSlice](#bytestoslice)
    * [\<bytes\>.dataSize()](#bytesdatasize)
    * [\<bytes\>.dataSizeQ()](#bytesdatasizeq)
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
    * [\<mapping\>.operator[]](#mappingoperator)
    * [\<mapping\>.at()](#mappingat)
    * [\<mapping\>.min() and mapping.max()](#mappingmin-and-mappingmax)
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
    * [\<mapping\>.getReplace()](#mappinggetreplace)
  * [require, revert](#require-revert)
    * [require](#require)
    * [revert](#revert)
  * [Libraries](#libraries)
    * [Function call via library name](#function-call-via-library-name)
    * [Function call via object](#function-call-via-object)
* [Pragmas](#pragmas)
  * [pragma ignoreIntOverflow](#pragma-ignoreintoverflow)
  * [pragma AbiHeader](#pragma-abiheader)
  * [pragma msgValue](#pragma-msgvalue)
* [State variables](#state-variables)
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
  * [functionID()](#functionid)
* [Events and return](#events-and-return)
  * [extAddr](#extaddr)
  * [return](#return)
* [External function calls](#external-function-calls)
* [API functions and members](#api-functions-and-members)
  * [**msg** namespace](#msg-namespace)
    * [msg.value](#msgvalue)
    * [msg.currencies](#msgcurrencies)
    * [msg.pubkey()](#msgpubkey)
    * [msg.createdAt](#msgcreatedat)
    * [msg.data](#msgdata)
  * [**tvm** namespace](#tvm-namespace)
    * [TVM instructions](#tvm-instructions)
      * [tvm.accept()](#tvmaccept)
      * [tvm.commit()](#tvmcommit)
      * [tvm.log()](#tvmlog)
      * [tvm.setcode()](#tvmsetcode)
      * [tvm.transLT()](#tvmtranslt)
      * [tvm.configParam()](#tvmconfigparam)
      * [tvm.rawConfigParam()](#tvmrawconfigparam)
      * [tvm.rawReserve()](#tvmrawreserve)
    * [Hashing and cryptography](#hashing-and-cryptography)
      * [tvm.hash()](#tvmhash)
      * [tvm.checkSign()](#tvmchecksign)
    * [Deploy contract from contract](#deploy-contract-from-contract)
      * [tvm.insertPubkey()](#tvminsertpubkey)
      * [tvm.buildStateInit()](#tvmbuildstateinit)
      * [tvm.buildEmptyData()](#tvmbuildemptydata)
      * [tvm.deploy()](#tvmdeploy)
      * [Deploy via new](#deploy-via-new)
    * [Misc functions from `tvm`](#misc-functions-from-tvm)
      * [tvm.pubkey()](#tvmpubkey)
      * [tvm.setCurrentCode()](#tvmsetcurrentcode)
      * [tvm.resetStorage()](#tvmresetstorage)
      * [tvm.functionId()](#tvmfunctionid)
      * [tvm.encodeBody()](#tvmencodebody)
      * [tvm.exit() and tvm.exit1()](#tvmexit-and-tvmexit1)
  * [**math** namespace](#math-namespace)
    * [math.min() and math.max()](#mathmin-and-mathmax)
    * [math.minmax()](#mathminmax)
    * [math.abs()](#mathabs)
    * [math.modpow2()](#mathmodpow2)
    * [math.divr() math.divc()](#mathdivr-mathdivc)
    * [math.muldiv() math.muldivr() math.muldivc()](#mathmuldiv-mathmuldivr-mathmuldivc)
    * [math.muldivmod()](#mathmuldivmod)
  * [**tx** namespace](#tx-namespace)
    * [tx.timestamp](#txtimestamp)
  * [**block** namespace](#block-namespace)
    * [block.timestamp](#blocktimestamp)
  * [**rnd** namespace](#rnd-namespace)
    * [rnd.next](#rndnext)
    * [rnd.getSeed](#rndgetseed)
    * [rnd.setSeed](#rndsetseed)
    * [rnd.shuffle](#rndshuffle)
  * [selfdestruct](#selfdestruct)

## Detailed description

### TON specific types

TON Solidity compiler expands functionality of some existing types and adds several new TVM specific types: TvmCell, TvmSlice, TvmBuilder and ExtraCurrencyCollection. Full description of this types can be found in [TVM][1] and [TON Blockchain][2] specifications.

#### TON units

A literal number can take a suffix to specify a subdenomination of TON currency, where numbers without a postfix are assumed to be nanotons.

```TVMSolidity
require(1 nano == 1);
require(1 nanoton == 1);
require(1 nTon == 1);
require(1 ton == 1e9 nanoton);
require(1 Ton == 1e9 nanoton);
require(1 micro == 1e-6 ton);
require(1 microton == 1e-6 ton);
require(1 milli == 1e-3 ton);
require(1 milliton == 1e-3 ton);
require(1 kiloton == 1e3 ton);
require(1 kTon == 1e3 ton);
require(1 megaton == 1e6 ton);
require(1 MTon == 1e6 ton);
require(1 gigaton == 1e9 ton);
require(1 GTon == 1e9 ton);
```

#### TvmCell

TvmCell represents TVM type Cell. TON Solidity compiler defines the following functions to work with this type:

##### \<TvmCell\>.depth()

```TVMSolidity
<TvmCell>.depth() returns(uint64);
```

Returns the depth of TvmCell **c**. If **c** has no references, then **d** = 0;
otherwise **d** is one plus the maximum of depths of cells referred to from **c**.
If **c** is a Null instead of a Cell, returns zero.

#### \<TvmCell\>.dataSize()

```TVMSolidity
<TvmCell>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Returns the count of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If count of the distinct cells
exceeds `n+1` then a cell overflow exception (8) is thrown.  
This function is a wrapper for opcode "CDATASIZE" ([TVM][1] - A.11.7).

#### \<TvmCell\>.dataSizeQ()

```TVMSolidity
<TvmCell>.dataSizeQ(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Returns the count of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If count of the distinct cells
exceeds `n+1` then this function returns an `optional` that has no value.  
This function is a wrapper for opcode "CDATASIZEQ" ([TVM][1] - A.11.7).

##### \<TvmCell\>.toSlice()

```TVMSolidity
<TvmCell>.toSlice() returns (TvmSlice);
```

Converts the cell to a slice.

#### TvmSlice

TvmSlice represents TVM type Slice. TON Solidity compiler defines the following functions to work with this type:

##### \<TvmSlice\>.size()

```TVMSolidity
<TvmSlice>.size() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns number of data bits and references in the slice.

#### \<TvmSlice\>.dataSize()

```TVMSolidity
<TvmSlice>.dataSize(uint n) returns (uint /*cells*/, uint /*bits*/, uint /*refs*/);
```

Returns the count of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If count of the distinct cells
exceeds `n+1` then a cell overflow exception (8) is thrown.  
Note that the returned `count of distinct cells` does not take into
account the cell that contains the slice itself.  
This function is a wrapper for opcode "SDATASIZE" ([TVM][1] - A.11.7).

#### \<TvmSlice\>.dataSizeQ()

```TVMSolidity
<TvmSlice>.dataSize(uint n) returns (optional(uint /*cells*/, uint /*bits*/, uint /*refs*/));
```

Returns the count of distinct cells, data bits in the distinct cells and
cell references in the distinct cells. If count of the distinct cells
exceeds `n+1` then this function returns an `optional` that has no value.
Note that the returned `count of distinct cells` does not take into
account the cell that contains the slice itself.  
This function is a wrapper for opcode "SDATASIZEQ" ([TVM][1] - A.11.7).

##### \<TvmSlice\>.bits()

```TVMSolidity
<TvmSlice>.bits() returns (uint16);
```

Returns number of data bits in the slice.

##### \<TvmSlice\>.refs()

```TVMSolidity
<TvmSlice>.refs() returns (uint8);
```

Returns number of references in the slice.

##### \<TvmSlice\>.depth()

```TVMSolidity
<TvmSlice>.depth() returns (uint64);
```

Returns the depth of the slice. If slice has no references, then 0 is returned,
otherwise function result is one plus the maximum of depths of the cells referred to from the slice.

##### \<TvmSlice\>.decode()

```TVMSolidity
<TvmSlice>.decode(TypeA, TypeB, ...) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Loads given types from the slice.  
Example:

```TVMSolidity
TvmSlice slice = ...;
(uint8 a, uint16 b) = slice.decode(uint8, uint16);
(uint16 num0, uint32 num1, address addr) = slice.decode(uint16, uint32, address);
```

##### \<TvmSlice\>.loadRef()

```TVMSolidity
<TvmSlice>.loadRef() returns (TvmCell);
```

Loads a cell from the slice reference.

##### \<TvmSlice\>.loadRefAsSlice()

```TVMSolidity
<TvmSlice>.loadRefAsSlice() returns (TvmSlice);
```

Loads a cell from the slice reference and converts it into a slice.

##### \<TvmSlice\>.loadSigned()

```TVMSolidity
<TvmSlice>.loadSigned(uint16 bitSize) returns (int);
```

Loads a signed integer with the given **bitSize** from the slice.

##### \<TvmSlice\>.loadUnsigned()

```TVMSolidity
<TvmSlice>.loadSigned(uint16 bitSize) returns (uint);
```

Loads an unsigned integer with the given **bitSize** from the slice.

##### \<TvmSlice\>.loadTons()

```TVMSolidity
<TvmSlice>.loadTons() returns (uint128);
```

Loads (deserializes) **VarUInteger 16** and returns an unsigned 128-bit integer. See [TL-B scheme][3].

##### \<TvmSlice\>.decodeFunctionParams()

```TVMSolidity
// Decode public function parameters
<TvmSlice>.decodeFunctionParams(functionName) returns (TypeA /*a*/, TypeB /*b*/, ...);

// Decode constructor parameters
<TvmSlice>.decodeFunctionParams(ContractName) returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Decodes parameters of the function or constructor if
contract type is provided. It's very convenient if there are many params
and they don't fit in one cell. This function is usually used in
**[onBounce](#onbounce)** function.

See example of how to use **onBounce** function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### TvmBuilder

TvmBuilder represents TVM type Builder. TON Solidity compiler defines the following functions to work with this type:

##### \<TvmBuilder\>.toSlice()

```TVMSolidity
<TvmBuilder>.toSlice() returns (TvmSlice);
```

Converts the builder into a slice.

##### \<TvmBuilder\>.toCell()

```TVMSolidity
<TvmBuilder>.toCell() returns (TvmCell);
```

Converts the builder into a cell.

##### \<TvmBuilder\>.bits()

```TVMSolidity
<TvmBuilder>.bits() returns (uint16);
```

Returns the number of data bits already stored in the builder.

##### \<TvmBuilder\>.refs()

```TVMSolidity
<TvmBuilder>.refs() returns (uint8);
```

Returns the number of references already stored in the builder.

##### \<TvmBuilder\>.bitsAndRefs()

```TVMSolidity
<TvmBuilder>.bitsAndRefs() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns the number of data bits and references already stored in the builder.

##### \<TvmBuilder\>.remBits()

```TVMSolidity
<TvmBuilder>.remBits() returns (uint16);
```

Returns the number of data bits that can still be stored in the builder.

##### \<TvmBuilder\>.remRefs()

```TVMSolidity
<TvmBuilder>.remRefs() returns (uint8);
```

Returns the number of references that can still be stored in the builder.

##### \<TvmBuilder\>.remBitsAndRefs()

```TVMSolidity
<TvmBuilder>.remBitsAndRefs() returns (uint16 /*bits*/, uint8 /*refs*/);
```

Returns the number of data bits and references that can still be stored in the builder.

##### \<TvmBuilder\>.depth()

```TVMSolidity
<TvmBuilder>.depth() returns (uint64);
```

Returns the depth of the builder. If no cell references are stored
in the builder, then 0 is returned; otherwise function result is one plus the maximum of
depths of cells referred to from the builder.

##### \<TvmBuilder\>.store()

```TVMSolidity
<TvmBuilder>.store(/*list_of_variables*/);
```

Stores variables in the builder. Available types:

* integer
* address
* TvmSlice
* TvmBuilder
* TvmCell
* mapping
* struct
* array

Example:

```TVMSolidity
uint8 a = 11;
int16 b = 22;
TvmBuilder builder;
builder.store(a, b, uint(33));
```

##### \<TvmBuilder\>.storeSigned()

```TVMSolidity
<TvmBuilder>.storeSigned(int256 value, uint16 bitSize);
```

Stores a signed integer **value** with given **bitSize** in the builder.

##### \<TvmBuilder\>.storeUnsigned()

```TVMSolidity
<TvmBuilder>.storeUnsigned(uint256 value, uint16 bitSize);
```

Stores an unsigned integer **value** with given **bitSize** in the builder.

##### \<TvmBuilder\>.storeRef()

```TVMSolidity
<TvmBuilder>.storeRef(TvmBuilder refBuilder);
```

Converts the argument **refBuilder** into a cell and stores it in a reference of the builder.

##### \<TvmBuilder\>.storeTons()

```TVMSolidity
<TvmBuilder>.storeTons(uint128 value);
```

Stores (serializes) an integer **value** and stores it in the builder as
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

#### optional(Type)

The template optional type manages an optional contained value, i.e. a value that may or may not be present.

##### constructing an optional

There are many ways to set a value:

```TVMSolidity
optional(uint) opt;
opt.set(11); // just sets value
opt = 22; // just sets value, too
opt.get() = 33; // if 'opt' has value then set value. Otherwise throws an exception.

optional(uint) another = ...;
opt = another;
```

##### \<optional(Type)\>.hasValue()

```TVMSolidity
<optional(Type)>.hasValue() returns (bool);
```

Checks whether **opt** contains a value.

##### \<optional(Type)\>.get()

```TVMSolidity
<optional(Type)>.get() returns (Type);
```

Returns the contained value, if the optional contains one,
Otherwise, throws an exception.

##### \<optional(Type)\>.set()

```TVMSolidity
<optional(Type)>.set(Type value);
```

Replaces contents of the optional with the contents of other.

##### \<optional(Type)\>.reset()

```TVMSolidity
<optional(Type)>.reset();
```

Deletes contents of the optional.

### TON specific control structures

### Range-based for loop

Executes a for loop over a range. Used as a more readable equivalent to the traditional for loop operating over a range of values, such as all elements in a array or mapping.

```TVMSolidity
for ( range_declaration : range_expression ) loop_statement
```

`range_expression` is calculated only one time and a result is copied. And we iterate over the copy of the array or mapping.

```TVMSolidity
uint[] arr = ...;
uint sum = 0;
for (uint val : arr) { // iteration over array
    sum += val;
}


mapping(uint32 => uint) map = ...;
uint keySum = 0;
uint valueSum = 0;
for ((uint32 key, uint value) : map) { // iteration over mapping 
    keySum += key;
    valueSum += value;
}
```

Key or value can be omitted if you iterate over mapping:
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

```TVMSolidity
uint a = 0;
repeat(10) {
    a ++;
}
require(a == 10, 101);

// Despite a is changed in the cycle, code block will be repeated 10 times (10 is initial value of a)
repeat(a) {
    a += 2;
}
require(a == 30, 102);

a = 11;
repeat(a - 1) {
    a -= 1;
}
require(a == 1, 103);
```

Allows to repeat block of code several times.
A **repeat** loop evaluates the expression only one time.
This expression must have an unsigned integer type.

### Changes and extensions in solidity types

#### struct

##### \<struct\>.unpack()

```TVMSolidity
<struct>.unpack() returns (TypeA /*a*/, TypeB /*b*/, ...);
```

Unpacks all values stored in the struct.

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

#### bytes

##### \<bytes\>.operator[]

```TVMSolidity
<bytes>.operator[](uint8 index) returns (byte);
```

Returns a byte located at the **index** position.  
Warning: **index** must be in range 0 to 126 include.

Example:

```TVMSolidity
bytes byteArray = "abba";
int index = 0;
byte a0 = byteArray[index];
```

##### \<bytes\>.length

```TVMSolidity
<bytes>.length returns (uint)
```

Returns length of the byte array.  
Warning: if length of the array is greater than 127 then function returns 127.

##### \<bytes\>.toSlice

```TVMSolidity
<bytes>.toSlice() returns (TvmSlice);
```

Converts **bytes** to **TvmSlice**.  
Warning: if length of the array is greater than 127 then extra bytes are
stored in the first reference of the slice. Use [\<TvmSlice\>.loadRef()](#tvmsliceloadref) to load that extra bytes.

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

#### string

TON Solidity compiler expands **string** type with the following functions:

##### \<string\>.byteLength()

```TVMSolidity
<string>.byteLength() returns (uint8);
```

Returns byte length of the string data.  
Warning: if length of the string is greater than 127 then function returns 127.

##### \<string\>.substr()

```TVMSolidity
<string>.substr(uint8 from, uint8 count) returns (string);
```

Returns a substring starting from the byte with number **from** with byte length **count**.  
Warning: **from** must be in range 0 to 127 inclusive and **from + count** must be in range 1 to 127 inclusive.

#### string(int)

```TvmSolidity
string(int value) returns (string);
```

Converts an integer to a decimal string.  
Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
uint n = 123;
string b = string(n);
require(a == b, 101);
require("6465321365465" == string(6465321365465), 102);
require("-1113" == string(-1113));
```

#### hexstring()

```TvmSolidity
hexstring(Type value) returns (string);
```

Converts an integer or address into a hex string.  
Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
require(hexstring(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) == "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 101);
require(hexstring(255) == "FF", 102);
require(hexstring(-65535) == "-FFFF", 103);
require(hexstring(address.makeAddrStd(127,0x7fffffffffffffffffffffffffffffffffffffffffffffffff123456789abcde)) == "7F:7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE", 104);
```

#### format()

```TvmSolidity
format(string template, TypeA a, TypeB b, ...) returns (string);
```

Builds a string with arbitrary parameters. Empty placeholder {} can be filled with integer
(in decimal view) or address. The only specified format is {:x} to fill with integer in a hexadecimal view.  
**Note**\: total length of the string shouldn't exceed 127.  
Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
string str = format("Hello {} 0x{:x} {}  {}.{} tons", 123, 255, address.makeAddrStd(-33,0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE), 100500, 32);
require(str == "Hello 123 0xFF -21:7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE  100500.32 tons", 101);
require(format("Hello {}", 123) == "Hello 123", 102);
require(format("Hello 0x{:x}", 123) == "Hello 0x7B", 103);
```

#### stoi()

```TvmSolidity
stoi(string inputStr) returns (uint /*result*/, bool /*status*/);
```

Converts a string into an integer. String is meant to be number in decimal format, only
if string starts with '0x' it will be converted from a hexadecimal format. Function returns the integer, that can
be converted from **uint** to **int** and boolean status, which is false in case of illegal characters in the string.

Warning: this function consumes too much gas, that's why it's better not to use it onchain.
Example:

```TVMSolidity
uint res;
bool status;
(res, status) = stoi("123");
require(status, 111);
require(res == 123, 101);
string hexstr = "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE";
uint num = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF123456789ABCDE;
(res, status) = stoi(hexstr);
require(status, 112);
require(res == num, 102);
(res, status) = stoi("0xag");
require(!status, 116);
```

#### address

**address** represents different types of TVM addresses: **addr_none**, **addr_extern**, **addr_std** and **addr_var**. TON Solidity compiler expands **address** type with the following members and functions:

##### Object creating

##### constructor()

```TVMSolidity
uint address_value;
address addrStd = address(address_value);
```

This function constructs an **address** of type **addr_std** with zero workchain id and given address value.

##### address.makeAddrStd()

```TVMSolidity
int8 wid;
uint address;
address addrStd = address.makeAddrStd(wid, address);
```

This function constructs an **address** of type **addr_std** with given workchain id **wid** and value **address_value**.

##### address.makeAddrNone()

```TVMSolidity
address addrNone = address.makeAddrNone();
```

This function constructs an **address** of type **addr_none**.

##### address.makeAddrExtern()

```TVMSolidity
uint addrNumber;
uint bitCnt;
address addrExtern = address.makeAddrExtern(addrNumber, bitCnt);
```

This function constructs an **address** of type **addr_extern** with given **value** with **bitCnt** bit length.

##### Members

##### \<address\>.wid

```TVMSolidity
<address>.wid returns (int8);
```

Returns the workchain id of **addr_std** or **addr_var**. Throws "range check error" exception (error code equal to 5) for another address types.

##### \<address\>.value

```TVMSolidity
<address>.value returns (uint);
```

Returns the address value of **addr_std** or **addr_var** if **addr_var** has 256-bit address value. Throws "range check error" exception (error code equal to 5) for another address types.

##### \<address\>.balance

```TVMSolidity
address(this).balance returns (uint128);
```

Returns the balance of **this** contract in nanotons.

##### \<address\>.currencies

```TVMSolidity
address(this).currencies returns (ExtraCurrencyCollection);
```

Returns currencies on the balance of **this** contract.

##### Functions

##### \<address\>.getType()

```TVMSolidity
<address>.getType() returns (uint8);
```

Returns type of the **address**:  
0 - addr_none
1 - addr_extern
2 - addr_std

##### \<address\>.isStdZero()

```TVMSolidity
<address>.isStdZero() returns (bool);
```

Returns the result of comparison between this **address** with zero **address** of type **addr_std**.

##### \<address\>.isStdAddrWithoutAnyCast()

```TVMSolidity
<address>.isStdAddrWithoutAnyCast() returns (bool);
```

Check whether this **address** is of type **addr_std** without any cast.

##### \<address\>.isExternZero()

```TVMSolidity
<address>.isExternZero() returns (bool);
```

Returns the result of comparison between this **address** with zero **address** of type **addr_extern**.

##### \<address\>.isNone()

```TVMSolidity
<address>.isNone() returns (bool);
```

Check whether this **address** is of type **addr_none**.

##### \<address\>.unpack()

```TVMSolidity
<address>.unpack() returns (int8 /*wid*/, uint256 /*value*/);
```

Unpacks **addr_std** and returns workchain id **wid** and address **value**.

##### \<address\>.transfer()

```TVMSolidity
<address>.transfer(uint128 value, ExtraCurrencyCollection currencies, bool bounce, uint16 flag, TvmCell body);
```

Sends an internal outbound message to defined address. Function parameters:  

* `value` (`uint128`) - amount of nanotons sent to the address.
* `currencies` (`ExtraCurrencyCollection`) - additional currencies sent to the address. Defaults to an empty set.
* `bounce` (`bool`) - if it's set and transaction (generated by the internal outbound message) falls (only at computing phase, not at action phase!) then funds will be returned. Otherwise (flag isn't set or transaction terminated successfully) the address accepts the funds even if the account doesn't exist or is frozen. Defaults to `true`.
* `flag` (`uint16`) - sets flag which used to send the internal outbound message. See [TVM A.11.10][1] for more details. Defaults to `1`.
* `body` (`TvmCell`) -  body (payload) attached to the internal message. Defaults to an empty TvmCell.

All parameters can be omitted, except ``value``.

```TVMSolidity
address addr = ...;
uint128 value = ...;
bool bounce = ...;
uint16 flag = ...;
TvmCell body = ...;
ExtraCurrencyCollection c = ...;
addr.transfer(value);
addr.transfer(value, bounce);
addr.transfer(value, bounce, flag);
addr.transfer(value, bounce, flag, body);
addr.transfer(value, bounce, flag, body, c);
```

Another example:

```TVMSolidity
TvmCell cell = ...;
ExtraCurrencyCollection c = ...;
address destination = msg.sender;
destination.transfer({value: 1 ton, bounce: false, flag: 128, body: cell, currencies: c});
destination.transfer({value: 1 ton, bounce: false, flag: 128, body: cell});
```

See example of usage `address.transfer()`:

* [giver](https://github.com/tonlabs/samples/blob/master/solidity/7_Giver.sol)

#### mapping

See example of how to work with mappings:

* [database](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollector.sol)
* [client](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollectorClient.sol)

TON Solidity compiler expands **mapping** type with the following functions. In examples
below **\<map\>** defines the object of **mapping(KeyType => ValueType)** type.

Address, bytes, string, bool, contract, enum, fixed bytes, integer and struct types can
be used as **KeyType**. Struct type can be used as **KeyType** only if it contains only
integer, boolean, fixed bytes or enum types and fits ~1023 bit. Example of mapping which
has a struct as a **KeyType**:

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

Struct **KeyType** can be used to sort keys of the mapping in ascending order. In the example
above addresses in the mapping are sorted by their keys. `x` field of the Point struct
is used in comparison first, second is `y` and the last is `z`.

If `bytes`, `string` or `TvmCell` types are used as **KeyType** then `mapping` stores
only hashes of mapping keys. That's why for above types the `delMin`/`min`/`next` and
another mapping methods return `uint256` as key (not `bytes`/`string`/`TvmCell`).

##### \<mapping\>.operator[]

```TVMSolidity
<map>.operator[](KeyType index) returns (ValueType);
```

Returns the item of ValueType with **index** key or returns a default value
if key is not in the mapping.

##### \<mapping\>.at()

```TVMSolidity
<map>.operator[](KeyType index) returns (ValueType);
```

Returns the item of ValueType with **index** key. Throws an exception if key
is not in the mapping.

##### \<mapping\>.min() and mapping.max()

```TVMSolidity
<map>.min() returns (optional(KeyType, ValueType));
<map>.max() returns (optional(KeyType, ValueType));
```

Computes the minimal (maximal) key in the mapping and returns an optional
value containing that key and the associated value. If mapping is empty,
this function returns an empty optional.

##### \<mapping\>.next() and \<mapping\>.prev()

```TVMSolidity
<map>.next(KeyType key) returns (optional(KeyType, ValueType));
<map>.prev(KeyType key) returns (optional(KeyType, ValueType));
```

Computes the minimal (maximal) key in the mapping that is lexicographically
greater (less) than **key** and returns an optional value containing that
key and the associated value. Returns an empty optional if there is no such key.
If KeyType is an integer type, argument for this functions can not possibly fit KeyType.

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

Computes the minimal (maximal) key in the mapping that is lexicographically greater than
or equal to (less than or equal to) **key** and returns an optional value containing that
key and the associated value. Returns an empty optional if there is no such key.
If KeyType is an integer type, argument for this functions can not possibly fit KeyType.

##### \<mapping\>.delMin() and \<mapping\>.delMax()

```TVMSolidity
<map>.delMin() returns (optional(KeyType, ValueType));
<map>.delMax() returns (optional(KeyType, ValueType));
```

If mapping is not empty then this function computes the minimal (maximum) key of the mapping,
deletes that key and the associated value from the mapping and returns an optional value
containing that key and the associated value. Returns an empty optional if there is no such key.

##### \<mapping\>.fetch()

```TVMSolidity
<map>.fetch(KeyType key) returns (optional(ValueType));
```

Checks whether **key** presents in the mapping and returns an optional with the associated value.
Returns an empty optional if there is no such key.

##### \<mapping\>.exists()

```TVMSolidity
<map>.exists(KeyType key) returns (bool);
```

Returns a status flag whether **key** presents in the mapping.

##### \<mapping\>.empty()

```TVMSolidity
<map>.empty() returns (bool);
```

Returns a status flag whether the mapping is empty.

##### \<mapping\>.replace()

```TVMSolidity
<map>.replace(KeyType key, ValueType value) returns (bool);
```

Sets the value associated with **key** only if **key** presents in the mapping and
returns the success flag.

##### \<mapping\>.add()

```TVMSolidity
<map>.add(KeyType key, ValueType value) returns (bool);
```

Sets the value associated with **key** only if **key** does not present in the mapping.

##### \<mapping\>.getSet()

```TVMSolidity
<map>.getSet(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but also returns an optional with the
old value associated with the **key**, if presents. Otherwise returns an empty optional.

##### \<mapping\>.getAdd()

```TVMSolidity
<map>.getAdd(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but only if **key** does not present in the mapping.
Returns an optional with the old value without changing the dictionary if that value presents
in the mapping, otherwise returns an empty optional.

##### \<mapping\>.getReplace()

```TVMSolidity
<map>.getReplace(KeyType key, ValueType value) returns (optional(ValueType));
```

Sets the value associated with **key**, but only if **key** presents in the mapping.
On success, returns an optional with the old value associated with the **key**.
Otherwise, returns an empty optional.

#### require, revert

In case of exception state variables of the contract are reverted to the state before
[tvm.commit()](#tvmcommit) or to the state of the contract before it was called.  
Use error codes that are greater than 100 because another error codes can be
[reserved](https://docs.ton.dev/86757ecb2/p/3874d1-error-reference).  
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

Libraries are similar to contracts, but they cannot have state variables
and cannot inherit nor be inherited. Libraries can be seen as implicit
base contracts of the contracts that use them. They will not be
explicitly visible in the inheritance hierarchy, but calls to library
functions look just like calls to functions of explicit base contracts
(using qualified access like `LibName.func(a, b, c)`). There is also
another way to call library function: `obj.func(b, c)`.  
For now libraries are stored as a part of the code of the contact that
uses libraries. In future it can be changed.

##### Function call via library name

Example of using library in the manner `LibName.func(a, b, c)`:

```TVMSolidity
// file MathHelper.sol
pragma solidity >=0.6.0;

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
pragma solidity >=0.6.0;

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

In TON solidity **the arguments in a function passed by value not by
reference**. It's effective for numbers and even for huge arrays.
See ([TVM][1] - A.2.3.2).
**But if a library function is called like `obj.func(b, c)` then only the
first argument  `obj` is passed by reference.**  It's similar to
the `self` variable in Python.
The directive `using A for B;` can be used to attach library functions
(from the library `A`) to any type (`B`) in the context of the contract.
These functions will receive the object they are called for as their
first parameter.
The effect of `using A for *;` is that the functions from the library
`A` are attached to all types.

Example of using library in the manner `obj.func(b, c)`:

```TVMSolidity
// file ArrayHelper.sol
pragma solidity >=0.6.0;

library ArrayHelper {
    // Delete the value from `array` at position `index`
    function del(uint[] array, uint index) internal pure {
        for (uint i = index; i + 1 < array.length; ++i){
            array[i] = array[i + 1];
        }
        array.pop();
    }
}


// file MyContract.sol
pragma solidity >=0.6.0;

import "ArrayHelper.sol";

contract MyContract {
    // Attach library function `del` to type `uint[]`
    using ArrayHelper for uint[];

    uint[] array;

    constructor() public {
        array = [uint(100), 200, 300];
    }

    function deleteElement(uint index) public {
        // Library function call via object.
        // Note function library function `del` have 2 arguments.
        // array is passed by reference and index is passed by value
        array.del(index);
    }
}

```

### Pragmas

`pragma` keyword is used to enable certain compiler features or checks.
A pragma directive is always local to a source file, so you have to add
the pragma to all your files if you want enable it in your whole project.
If you import another file, the pragma from that file is not
automatically applied to the importing file.

#### pragma ignoreIntOverflow

```TVMSolidity
pragma ignoreIntOverflow;
```

Turns off binary operation result overflow check.

#### pragma AbiHeader

```TVMSolidity
pragma AbiHeader time;
pragma AbiHeader pubkey;
pragma AbiHeader expire;
```

Force message forming utility to fill an appropriate field(s) in the header of the message to be sent to this contract:

* **pubkey** - public key by which the message was signed;
* **time**   - local time at what message was created;
* **expire** - time at which message should be meant as expired.

**pubkey** field is mandatory for the contract to be able to check message signature which was generated with
public key that is different from what is stored in this contract data.
**time** and **expire** fields can be used for replay protection and if set they should be read in [afterSignatureCheck](#aftersignaturecheck) in case of not default replay protection.
To read more about this and ABI follow this [link](https://docs.ton.dev/86757ecb2/p/40ba94-abi-specification-v2).
Here is example of [message expiration time](https://docs.ton.dev/86757ecb2/p/88321a-message-expiration-time) usage.

#### pragma msgValue

```TVMSolidity
pragma msgValue <value>;
```

Allows to specify default value in nanotons attached to the
internal messages that contract sends to call another contract. If not
specified this value is set to 10 000 000 nanotons.

Example:

```TVMSolidity
pragma msgValue 123456789;
pragma msgValue 1e8;
pragma msgValue 10 ton;
pragma msgValue 10_000_000_123;
```

### State variables

#### Keyword `static`

Static state variables are used in generation of the contract origin state.
Such variables can be set while deploying contract from contract
(onchain) or by tvm-linker (offchain).

Example:

```TVMSolidity
contract C {
    uint static a; // ok
    // uint static b = 123; // error
}
```

See also:  
[`code` option usage](#code-option-usage)  
[New contract address problem](#new-contract-address-problem)

#### Keyword `public`

For each public state variable, a getter function is generated. Generated
function has the same name and return type as the public variable. This
function can be called only locally. Public state variables are useful,
because you don't need to write functions that return a particular state variable manually.

Example:

```TVMSolidity
contract C {
    uint public a;
    uint public static b; // it's ok. Variable is public and static.
}
```

### Special contract functions

#### receive

On plain value transfer **receive** function is called. See [\<address\>.transfer()](#addresstransfer). If there is no **receive** function in contract than the contract has an implicit empty **receive** function.

Example:

```TVMSolidity
contract Sink {
    uint counter = 0;
    receive() external payable {
        ++counter;
    }
}
```

##### fallback

**fallback** function is called when a body of an inbound message contains invalid function id.

Example:

```TVMSolidity
contract Contr {
    uint counter = 0;
    fallback() external {
        ++counter;
    }
}
```

#### onBounce

**onBounce** function is executed when contract receives an inbound
internal message that has bounced flag set. Parameter slice of onBounce
function contains truncated body of the message (it's truncated by the
network). If this function is not defined then contract does nothing
when receives a bounced inbound internal message.

```TVMSolidity
onBounce(TvmSlice body) external {
    /*...*/
}
```

More useful example of how to use **onBounce** function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### onTickTock

**onTickTock** function is executed on tick and tock transactions.
That transactions are automatically invoked for certain special accounts.
See ([TBLKCH][2] - 4.2.4.)
For tick transactions **isTock** is false, for tock transactions - true.

Prototype:

```TVMSolidity
onTickTock(bool isTock) external {
    /*...*/
}
```

#### onCodeUpgrade

**onCodeUpgrade** function can have arbitrary set of arguments and should be
executed after [tvm.setcode()](#tvmsetcode) function call. In this function
[tvm.resetStorage()](#tvmresetstorage) should be called if the set of state
variables is changed in the new version of the contract. This function implicitly
calls [tvm.commit()](#tvmcommit). After return from **onCodeUpgrade** TVM
execution is finished with exit code 0.

Prototype:

```TVMSolidity
function onCodeUpgrade() private {
    /*...*/
}
```

See example of how to upgrade code of the contract:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

#### afterSignatureCheck

Developer can define **afterSignatureCheck** function to create his own replay protection function instead of the default one.

Prototype:

```TVMSolidity
function afterSignatureCheck(TvmSlice body, TvmCell message) private inline returns (TvmSlice) {
    /*...*/
}
```

See example of how to define this function:

* [Custom replay protection](https://github.com/tonlabs/samples/blob/master/solidity/14_CustomReplayProtection.sol)

### Function specifiers

#### Function mutability: pure, view and default

Function mutability shows how this function treats state variables.
Possible value of the function mutability:

* `pure` - function can't read and assign state variables;
* `view` - function can read but can't assign state variables;
* default - function can read and assign state variables.

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

**inline** specifier instructs the compiler to insert a copy of the function
body into each place where the function is called.
Keyword can be used only for private and internal functions.

Example:

```TVMSolidity
// This function is called as usual function.
function getSum(uint a, uint b) public returns (uint) {
    return sum(a, b);
}

// Code of this function is inserted to the place of call.
function sum(uint a, uint b) private inline returns (uint) {
    return a + b;
}
```

#### functionID()

**functionID** keyword allows to set identifier of the function manually.
Each public function has unique 32-bit id. If `functionID` is not used
then function id is calculated as a hash of function signature.

Example:

```TVMSolidity
function functionName() public pure functionID(123) {
    /*...*/
}
 ```

### Events and return

#### extAddr

TON Solidity compiler allows to specify destination address of the message
sent via event emitting using suffix **extAddr**. If **extAddr** suffix is
not used, external address is set to **addr_none**.

Example:

```TVMSolidity
emit EventName(arguments).extAddr(address);
emit EventName(arguments);
```

#### return

Public or external functions (called by external message) send an
external message on return. Destination address of that message is
the source address of the inbound external message.

### External function calls

TON Solidity compiler allows to specify different parameters of the
outbound internal message which is sent via external function call.
`value`, `currencies`, `bounce` or `flag` options can be set
(NOT `body` option). See [\<address\>.transfer()](#addresstransfer) where this options are described.

Example:

```TVMSolidity
interface IContract {
    function f(uint a) external;
}

contract Caller {
    function callExt(address addr) public {
        IContract(addr).f{value: 10 ton}(123);
        IContract(addr).f{value: 10 ton, flag: 3}(123);
        IContract(addr).f{value: 10 ton, bounce: true}(123);
        IContract(addr).f{value: 1 micro, bounce: false, flag: 128}(123);
        ExtraCurrencyCollection cc;
        cc[12] = 1000;
        IContract(addr).f{value: 10 ton, currencies:cc}(123);
    }
}
```

### API functions and members

#### **msg** namespace

##### msg.value

```TVMSolidity
msg.value returns (uint128);
```

Balance of the inbound message in nanotons.

##### msg.currencies

```TVMSolidity
msg.currencies returns (ExtraCurrencyCollection);
```

Collections of arbitrary currencies contained in the balance of
the inbound message.

##### msg.pubkey()

```TVMSolidity
msg.pubkey() returns (uint256);
```

Returns sender's public key, obtained from the body if the external
inbound message. If message is not signed function returns 0. If
message is signed and message header ([pragma AbiHeader](#pragma-abiheader))
does not contain pubkey than `msg.pubkey()` is equal to `tvm.pubkey()`.

##### msg.createdAt

```TVMSolidity
msg.createdAt returns (uint32);
```

Returns a field **created_at** of the external inbound message.

##### msg.data

```TVMSolidity
msg.data returns (TvmSlice);
```

Returns a payload of the inbound message.

#### **tvm** namespace

##### TVM instructions

##### tvm.accept()

```TVMSolidity
tvm.accept();
```

Executes TVM instruction "ACCEPT" ([TVM][1] - A.11.2. - F800).
This instruction sets current gas limit to its maximal allowed value.
This action is required to process external messages, which bring no value.

See example of how to use this function:

* [accumulator](https://github.com/tonlabs/samples/blob/master/solidity/1_Accumulator.sol)

##### tvm.commit()

```TVMSolidity
tvm.commit();
```

Executes TVM instruction "COMMIT" ([TVM][1] - A.11.2. - F80F).
This instruction commits the current state of registers c4 and c5
so that the current execution is considered “successful” with the
saved values even if an exception is thrown later.

##### tvm.log()

```TVMSolidity
tvm.log(string log);
logtvm(string log);
```

Executes TVM instruction "PRINTSTR" ([TVM][1] - A.12.2. - FEFn01ssss).
This command may be ignored if --without-logstr flag is set in the
command line for the compiler.

**logtvm** is an alias for tvm.log(string).

##### tvm.setcode()

```TVMSolidity
tvm.setcode(TvmCell newCode);
```

Executes TVM instruction "SETCODE" ([TVM][1] - A.11.9. - FB04).
This command creates an output action that would change this smart contract
code to that given by Cell **newCode** (this change will take effect only
after the successful termination of the current run of the smart contract).

See example of how to use this function:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.transLT()

```TVMSolidity
tvm.transLT() returns (uint64);
```

Executes TVM instruction "LTIME" ([TVM][1] - A.11.4. - F825).
This command returns the logical time of the current transaction.

##### tvm.configParam()

```TVMSolidity
tvm.configParam(uint8 paramNumber) returns (TypeA a, TypeB b, ...);
```

Executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832).
This command returns the value of the global configuration parameter with
integer index paramNumber. Argument should be an integer literal.
Supported paramNumbers: 1, 15, 17, 34.

##### tvm.rawConfigParam()

```TVMSolidity
tvm.rawConfigParam(uint8 paramNumber) returns (TvmCell cell, bool status);
```

Executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832).
This command returns the value of the global configuration parameter with
integer index paramNumber as a cell and a boolean status.

##### tvm.rawReserve()

```TVMSolidity
tvm.rawReserve(uint value,[ExtraCurrencyCollection currency,] uint8 flag);
```

Executes TVM instruction "RAWRESERVE" and "RAWRESERVEX". See [TVM][1].
This command reserves some part of the contract balance.

Example:

```TVMSolidity
tvm.rawReserve(111, 0);

ExtraCurrencyCollection col;
tvm.rawReserve(10 ton, col, 1);

col[1] = 3;
tvm.rawReserve(12 * 1e9, col, 2);
```

##### Hashing and cryptography

##### tvm.hash()

```TVMSolidity
tvm.hash(TvmCell cellTree) returns (uint256);
tvm.hash(string data) returns (uint256);
tvm.hash(bytes data) returns (uint256);
```

Executes TVM instruction "HASHCU" ([TVM][1] - A.11.6. - F900).
It computes the representation hash of a given argument and returns
it as a 256-bit unsigned integer

Example:

```TVMSolidity
uint256 hash = tvm.hash(TvmCell cellTree);
uint256 hash = tvm.hash(string);
uint256 hash = tvm.hash(bytes);
```

##### tvm.checkSign()

```TVMSolidity
tvm.checkSign(uint256 hash, uint256 SignHighPart, uint256 SignLowPart, uint256 pubkey) returns (bool);
tvm.checkSign(uint256 hash, TvmSlice signature, uint256 pubkey) returns (bool);
tvm.checkSign(TvmSlice data, TvmSlice signature, uint256 pubkey) returns (bool);
```

Executes TVM instruction "CHKSIGNU" ([TVM][1] - A.11.6. - F910) for variants 1 and 2.
This command checks the Ed25519-signature of a **hash** using public key **pubkey**.
Signature is represented by two uint256 **SignHighPart** and **SignLowPart** in the
first variant and by a slice **signature** in the second variant.
In the third variant executes TVM instruction "CHKSIGNS" ([TVM][1] - A.11.6. - F911).
This command checks Ed25519-signature of **data** using public key **pubkey**.
Signature is represented by a slice **signature**.

Example:

```TVMSolidity
uint256 hash;
uint256 SignHighPart;
uint256 SignLowPart;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(hash, SignHighPart, SignLowPart, pubkey);  // 1 variant

uint256 hash;
TvmSlice signature;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(hash, signature, pubkey);  // 2 variant

TvmSlice data;
TvmSlice signature;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(hash, signature, pubkey);  // 3 variant
```

##### Deploy contract from contract

##### tvm.insertPubkey()

```TVMSolidity
tvm.insertPubkey(TvmCell stateInit, uint256 pubkey) returns (TvmCell);
```

Inserts a public key into stateInit data field.

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

Generates a `StateInit` ([TBLKCH][2] - 3.1.7.) from `code` and `data`.
Member `splitDepth` of the tree of cell `StateInit`:
1) is not set. Has no value.
2) is set. `0 <= splitDepth <= 31`
3) Also arguments can be set with names.
List of possible names:

* `code` (`TvmCell`) - defines the code field of the `StateInit`. Must be specified.
* `data` (`TvmCell`) - defines the data field of the `StateInit`. Conflicts with `pubkey` and `varInit`. Can be omitted, in this case data field would be build from `pubkey` and `varInit`.
* `splitDepth` (`uint8`) - splitting depth. `0 <= splitDepth <= 31`. Can be omitted. By default has no value.
* `pubkey` (`uint256`) - defines the public key of the new contract. Conflicts with `data`. Can be omitted, default value is 0.
* `varInit` (`initializer list`) - used to set [static](#keyword-static) variables of the contract which `StateInit` is built. Conflicts with `data` and requires `contr` to be set. Can be omitted.
* `contr` (`contract`) - defines the contract which `StateInit` is built. Mandatory to be set if the option `varInit` is specified.

Examples of usage of this function:

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

##### tvm.buildEmptyData()

```TVMSolidity
tvm.buildEmptyData(uint256 publicKey) returns (TvmCell);
```

Generates a persistent storage of the contract that contains only public
key. **data** can be used to generate StateInit ([TBLKCH][2] - 3.1.7.).

##### tvm.deploy()

```TVMSolidity
tvm.deploy(TvmCell stateInit, TvmCell payload, uint128 value, int8 wid) returns(address);
```

Deploys a new contract and returns the address of the deployed contract.
This function may be useful if you want to write a universal contract that
can deploy any contract. In another cases, use [Deploy via new](#deploy-via-new).
Arguments:

* `stateInit` - contract's StateInit.
* `payload` - encoded internal inbound message. This message should contain the function (constructor) id and encoded parameters of constructor.
* `value` - funds in nanotons that will be sent to the new contract address.
* `wid` - workchain id of the new contract address.

`payload` can be generated manually by tvm-linker tool.

See also:

* [Example of usage](https://github.com/tonlabs/samples/blob/master/solidity/11_ContractDeployer.sol)
* [Step-by-step description how to deploy contracts from the contract here](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.md).

##### Deploy via new

Either `code` or `stateInit` option must be set when you deploy a contract
from contract via keyword `new`. `stateInit` is a tree of cells that contains
original state of contract. `stateInit` contains `data`, `code` and another members.
See also ([TBLKCH][2] - A.2.3.2) to read about `stateInit`.

Use `stateInit` option if you have the created account state (maybe offchain or
onchain). And use `code` if want to create account state in `new` expression.

**Note**: Address of the new account is calculated as hash of the `stateInit`.
Constructor function parameters don't influence the address. See
[New contract address problem](#new-contract-address-problem).

[Step-by-step description how to deploy contracts from the contract here](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.md).  
Example: [WalletProducer](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).

##### `stateInit` option usage

`stateInit` defines the origin state of the new account.

```TVMSolidity
TvmCell stateInit = ...;
address newWallet = new SimpleWallet{value: 1 ton, stateInit: stateInit}(arg0, arg1, ...);
```

##### `code` option usage

`code` option defines the code of the new contract.

```TVMSolidity
TvmCell code = ...;
address newWallet = new SimpleWallet{value: 1 ton, code: code}(arg0, arg1, ...);
```

This options can be used only with `code` option:

* `pubkey` (`uint256`) - defines the public key of the new contract.
* `varInit` (`initializer list`) - used to set [static](#keyword-static) variables of the new contract.
* `splitDepth` (`uint8`) - splitting depth. `0 <= splitDepth <= 31`. By default has no value.

Example of using of these options:

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
    value: 1 ton,
    code: code,
    pubkey: 0xe8b1d839abe27b2abb9d4a2943a9143a9c7e2ae06799bd24dec1d7a8891ae5dd,
    splitDepth: 15
    varInit: {m_owner: address(this), m_value: 15}
}(arg0, arg1, ...);
```

##### Another deploy options

This options can be used with `stateInit` and `code`:

* `value` (`uint128`) - funds attached to the outbound internal message, that creates new account. This value must be set.
* `currencies` (`ExtraCurrencyCollection`) - currencies attached to the outbound internal message. Defaults to an empty set.
* `bounce` (`bool`) - if it's set and deploying falls (only at computing phase, not at action phase!) then funds will be returned. Otherwise (flag isn't set or deploying terminated successfully) the address accepts the funds. Defaults to `true`.
* `wid` (`uint8`) - workchain id of the new account address. Defaults to `0`.
* `flag` (`uint16`) - flag used to send the outbound internal message. Defaults to `1`. See opcode `SENDRAWMSG` ([TVM][1] - A.11.10).

```TVMSolidity
TvmCell stateInit = ...;
address newWallet = new SimpleWallet{
    stateInit: stateInit,
    value: 1 ton,
    wid: -1,
    flag: 0
}(arg0, arg1, ...);
```

##### New contract address problem

Address of the new account is calculated as hash of the `stateInit`.
Parameters of the constructor don't influence the address. The problem
is that hacker can deploy the contract with your `stateInit` before you
with malicious constructor parameters.

Let's consider how to protect against this problem:

1. Constructor is called by external message.  
We must check that we didn't forget to set public key in the contract and the
inbound message is signed by that key. If hacker doesn't have your private
key than he can't sign message to call the constructor.  
See [constructor of WalletProducer](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).
2. Constructor is called by internal message.  
We should define static variable in the new contract that will contain
address of the creator. Address of the creator will be a part of the `stateInit`.
And in the constructor we must check address of the message sender.  
See [function `deployWallet` how to deploy contract](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol).  
See [constructor of SimpleWallet](https://github.com/tonlabs/samples/blob/master/solidity/17_SimpleWallet.sol).  
If some contract should deploy plenty of contracts (with some contract's
public key) than it's a good idea to declare static variable in the deployed
contract. This variable can contain some sequence number. It will allow
each new contact to have unique `stateInit`.
See [SimpleWallet](https://github.com/tonlabs/samples/blob/master/solidity/17_SimpleWallet.sol).  
**Note**: contract's public key (`tvm.pubkey()`) is a part of `stateInit`.

##### Misc functions from `tvm`

##### tvm.pubkey()

```TVMSolidity
tvm.pubkey() returns (uint256);
```

Returns contract's public key, stored in contract data. If key is not set function returns 0.

##### tvm.setCurrentCode()

```TVMSolidity
tvm.setCurrentCode(TvmCell newCode);
```

Changes this smart contract current code to that given by Cell **newCode**.

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

This function returns a function id (uint32) for public/external function
or constructor.  

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
tvm.encodeBody(function_name, arg0, arg1, arg2, ...) returns (TvmCell);
```

Constructs a function call message body that can be used
as the payload for [\<address\>.transfer()](#addresstransfer).  

Example:

```TVMSolidity
interface Remote {
    function func(uint256 num, address a, int64 num2) public pure;
}

contract Caller {
    function test() public pure {
        TvmCell body = tvm.encodeBody(Remote.func, 123, address.makeAddrStd(22, 333), -654);
        msg.sender.transfer({value:1e10, body:body});
    }
}
```

##### tvm.exit() and tvm.exit1()

```TVMSolidity
tvm.exit();
tvm.exit1();
```

Functions are used to save state variables and to quickly terminate
execution of the smart contract.  
Exit codes are equal to zero and one for `tvm.exit` and `tvm.exit1`
respectively.

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

#### **math** namespace

##### math.min() and math.max()

```TVMSolidity
math.min(int a, int b, ...) returns (int);
math.max(int a, int b, ...) returns (int);
math.min(uint a, uint b, ...) returns (uint);
math.max(uint a, uint b, ...) returns (uint);
```

Returns the minimal (maximal) value of the passed arguments.

##### math.minmax()

```TVMSolidity
math.minmax(uint, uint) returns (uint /*min*/, uint /*max*/);
math.minmax(int, int) returns (int /*min*/, int /*max*/);
```

Returns min and max values of the passed arguments.

Example:

```TVMSolidity
(uint a, uint b) = math.minmax(20, 10); // (10, 20)
```

##### math.abs()

```TVMSolidity
math.abs(int val) returns (int);
```

Computes the absolute value of the given integer.

Example:

```TVMSolidity
int a = math.abs(-4123); // 4123
int b = -333;
int c = math.abs(b); // 333
```

##### math.modpow2()

```TVMSolidity
math.modpow2(uint value, uint power) returns (uint);
```

Computes the value modulo 2^**power**. Note that **power** should be a constant integer.

Example:

```TVMSolidity
uint constant pow = 12;
uint val = 12313;
uint a = math.modpow2(val, 10);
uint b = math.modpow2(val, pow);
```

##### math.divr() math.divc()

```TVMSolidity
math.divc(int a, int b) returns (int);
math.divr(int a, int b) returns (int);
```

Returns result of the division of two integers.
The return value is rounded.  
Round mode "nearest integer" is used for `divr`.  
Round mode "ceiling" is used for `divc`.

Example:

```TVMSolidity
int c = math.divc(10, 3); // 4
int c = math.divr(10, 3); // 3
```

##### math.muldiv() math.muldivr() math.muldivc()

```TVMSolidity
math.muldiv(int a, int b, int c) returns (int);
math.muldivr(int a, int b, int c) returns (int);
math.muldivc(int a, int b, int c) returns (int);
```

Multiplies two values and then divides the result by a third value.
The return value is rounded.  
Round mode "floor" is used for `muldiv`.  
Round mode "nearest integer" is used for `muldivr`.  
Round mode "ceiling" is used for `muldivc`.  
See also ([TVM][1] - cf. 1.5.6) about the round modes.

Example:

```TVMSolidity
require(math.muldiv(3, 7, 2) == 10);
require(math.muldivr(3, 7, 2) == 11);
require(math.muldivc(3, 7, 2) == 11);
```

##### math.muldivmod()

```TVMSolidity
math.muldivmod(uint a, uint b, uint c) returns (uint /*result*/, uint /*remainder*/);
```

Executes TVM instruction "MULDIVMOD" ([TVM][1] - A.5.2. - A98C).
This instruction multiplies first two arguments, divides the result
by third argument and returns the result and the remainder.
Intermediate result is stored in the 514 bit buffer, and the final result
is rounded to the floor.

Example:

```TVMSolidity
uint a = 3;
uint b = 2;
uint c = 5;
(uint d, uint r) = math.muldivmod(a, b, c);
int e = -1;
int f = 3;
int g = 2;
(int h, int p) = math.muldivmod(e, f, g);
```

##### **tx** namespace

##### tx.timestamp

```TVMSolidity
tx.timestamp returns (uint64);
```

Returns the logical time of the current transaction.

##### **block** namespace

##### block.timestamp

```TVMSolidity
block.timestamp returns (uint64);
```

Returns the starting logical time of the current block.

##### **rnd** namespace

The pseudorandom number generator uses the random seed. The
initial value of the random seed before a smart contract is executed in
TON Blockchain is a hash of the smart contract address and the global
block random seed. If there are several runs of the same smart contract
inside a block, then all of these runs will have the same random seed.
This can be fixed, for example, by running `rnd.shuffle()` (without
parameters) each time before using the pseudorandom number generator.

##### rnd.next

```TVMSolidity
rnd.next([Type mod]) returns (Type);
```

Generates a new pseudo-random number.  

1) Returns `uint256` number.
2) If the first argument `mod > 0` then function returns the value in the
range `0..mod-1`. Else if `mod < 0` then the returned value lies in range
`mod..-1`. Else if `mod == 0` than it returns `0`.

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
(1) Mixes the random seed and `someNumber`. The result is set as the
random seed.  
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

#### selfdestruct

```TVMSolidity
selfdestruct(address dest_addr);
```

Creates and sends the message that carries all the remaining balance
of the current smart contract and destroys the current account.

See example of how to use **selfdestruct** function:

* [Kamikaze](https://github.com/tonlabs/samples/blob/master/solidity/8_Kamikaze.sol)

[1]: https://ton.org/tvm.pdf        "TVM"
[2]: https://ton.org/tblkch.pdf     "TBLKCH"
[3]: https://github.com/ton-blockchain/ton/blob/master/crypto/block/block.tlb "TL-B scheme"
