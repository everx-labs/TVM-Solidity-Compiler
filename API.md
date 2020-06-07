# **TON Solidity API**

TON Solidity compiler expands Solidity language with different API functions to facilitate TON contract development.

## Table of Contents

* [TON specific types](#ton-specific-types)
  * [TvmCell](#tvmcell)
    * [TvmCell.toSlice()](#tvmcelltoslice)
  * [TvmSlice](#tvmslice)
    * [TvmSlice.size()](#tvmslicesize)
    * [TvmSlice.bits()](#tvmslicebits)
    * [TvmSlice.refs()](#tvmslicerefs)
    * [TvmSlice.decode()](#tvmslicedecode)
    * [TvmSlice.loadRef()](#tvmsliceloadref)
    * [TvmSlice.loadRefAsSlice()](#tvmsliceloadrefasslice)
    * [TvmSlice.loadSigned()](#tvmsliceloadsigned)
    * [TvmSlice.loadUnsigned()](#tvmsliceloadunsigned)
    * [TvmSlice.decodeFunctionParams()](#tvmslicedecodefunctionparams)
  * [TvmBuilder](#tvmbuilder)
    * [TvmBuilder.toSlice()](#tvmbuildertoslice)
    * [TvmBuilder.toCell()](#tvmbuildertocell)
    * [TvmBuilder.bits()](#tvmbuilderbits)
    * [TvmBuilder.refs()](#tvmbuilderrefs)
    * [TvmBuilder.bitsAndRefs()](#tvmbuilderbitsandrefs)
    * [TvmBuilder.remBits()](#tvmbuilderrembits)
    * [TvmBuilder.remRefs()](#tvmbuilderremrefs)
    * [TvmBuilder.remBitsAndRefs()](#tvmbuilderrembitsandrefs)
    * [TvmBuilder.store()](#tvmbuilderstore)
    * [TvmBuilder.storeSigned()](#tvmbuilderstoresigned)
    * [TvmBuilder.storeUnsigned()](#tvmbuilderstoreunsigned)
    * [TvmBuilder.storeRef()](#tvmbuilderstoreref)
  * Changes in
    * [string](#string)
      * [string.byteLength()](#stringbytelength)
      * [string.substr()](#stringsubstr)
    * [bytes](#bytes)
      * [operator[]](#operator)
      * [bytes.length](#byteslength)
    * [address](#address)
      * [Object creating](#object-creating)
        * [constructor()](#constructor)
        * [address.makeAddrStd()](#addressmakeaddrstd)
        * [address.makeAddrNone()](#addressmakeaddrnone)
        * [address.makeAddrExtern()](#addressmakeaddrextern)
      * [Members](#members)
        * [address.wid](#addresswid)
        * [address.value](#addressvalue)
        * [address.currencies](#addresscurrencies)
      * [Functions](#functions)
        * [address.getType()](#addressgettype)
        * [address.isStdZero()](#addressisstdzero)
        * [address.isExternZero()](#addressisexternzero)
        * [address.isNone()](#addressisnone)
        * [address.unpack()](#addressunpack)
        * [address.transfer()](#addresstransfer)
    * [mapping](#mapping)
      * [mapping.min() and mapping.max()](#mappingmin-and-mappingmax)
      * [mapping.next() and mapping.prev()](#mappingnext-and-mappingprev)
      * [mapping.nextOrEq() and mapping.prevOrEq()](#mappingnextoreq-and-mappingprevoreq)
      * [mapping.delMin() and mapping.delMax()](#mappingdelmin-and-mappingdelmax)
      * [mapping.fetch()](#mappingfetch)
      * [mapping.exists()](#mappingexists)
      * [mapping.empty()](#mappingempty)
      * [mapping.replace()](#mappingreplace)
      * [mapping.add()](#mappingadd)
      * [mapping.getSet()](#mappinggetset)
      * [mapping.getAdd()](#mappinggetadd)
      * [mapping.getReplace()](#mappinggetreplace)
  * [ExtraCurrencyCollection](#extracurrencycollection)
  * [require, revert](#require-revert)
    * [require](#require)
    * [revert](#revert)
* [Pragmas](#pragmas)
  * [ignoreIntOverflow](#ignoreintoverflow)
  * [AbiHeader](#abiheader)
* [Special contract functions](#special-contract-functions)
  * [receive](#receive)
  * [fallback](#fallback)
  * [onBounce](#onbounce)
  * [onCodeUpgrade](#oncodeupgrade)
  * [afterSignatureCheck](#aftersignaturecheck)
* [Function specifiers](#function-specifiers)
  * [functionID()](#functionid)
* [Events and return](#events-and-return)
  * [extAddr](#extaddr)
  * [return](#return)
* [API functions and members](#api-functions-and-members)
  * [**msg** namespace](#msg-namespace)
    * [msg.pubkey()](#msgpubkey)
    * [msg.createdAt](#msgcreatedat)
    * [msg.currencies](#msgcurrencies)
  * [**tvm** namespace](#tvm-namespace)
    * [TVM instructions](#tvm-instructions)
      * [tvm.accept()](#tvmaccept)
      * [tvm.commit()](#tvmcommit)
      * [tvm.log()](#tvmlog)
      * [tvm.setcode()](#tvmsetcode)
      * [tvm.cdatasize()](#tvmcdatasize)
      * [tvm.transLT()](#tvmtranslt)
      * [tvm.configParam()](#tvmconfigparam)
    * [Hashing and cryptography](#hashing-and-cryptography)
      * [tvm.hash()](#tvmhash)
      * [tvm.checkSign()](#tvmchecksign)
    * [Deploy contract from contract](#deploy-contract-from-contract)
      * [tvm.insertPubkey()](#tvminsertpubkey)
      * [tvm.buildStateInit()](#tvmbuildstateinit)
      * [tvm.deploy()](#tvmdeploy)
      * [tvm.deployAndCallConstructor()](#tvmdeployandcallconstructor)
      * [tvm.deployAndCallConstructorWithFlag()](#tvmdeployandcallconstructorwithflag)
      * [Deploy via new](#deploy-via-new)
    * [Others](#others)
      * [tvm.pubkey()](#tvmpubkey)
      * [tvm.setCurrentCode()](#tvmsetcurrentcode)
      * [tvm.resetStorage()](#tvmresetstorage)
      * [tvm.functionId()](#tvmfunctionid)
      * [tvm.encodeBody()](#tvmencodebody)
      * [tvm.min() and tvm.max()](#tvmmin-and-tvmmax)
  * [selfdestruct](#selfdestruct)

## Detailed description

### TON specific types

TON Solidity compiler expands functionality of some existing types and adds several new TVM specific types: TvmCell, TvmSlice, TvmBuilder and ExtraCurrencyCollection. Full description of this types can be found in [TVM][1] and [TON Blockchain][2] specifications.

#### TvmCell

TvmCell represents TVM type Cell. TON Solidity compiler defines the following functions to work with this type:

##### TvmCell.toSlice()

```TVMSolidity
TvmCell cell;
TvmSlice slice = cell.toSlice();
```

This function converts cell to slice.

#### TvmSlice

TvmSlice represents TVM type Slice. TON Solidity compiler defines the following functions to work with this type:

##### TvmSlice.size()

```TVMSolidity
TvmSlice slice;
(uint16 bits, uint8 refs) = slice.size();
```

This function returns number of data bits and references in the slice.

##### TvmSlice.bits()

```TVMSolidity
TvmSlice slice;
uint16 bits = slice.bits();
```

This function returns number of data bits in the slice.

##### TvmSlice.refs()

```TVMSolidity
TvmSlice slice;
uint8 refs = slice.refs();
```

This function returns number of references in the slice.

##### TvmSlice.decode()

```TVMSolidity
TvmSlice slice;
(TypeA a, TypeB b, ...) = slice.decode(TypeA, TypeB, ...);
```

This function loads given types from the slice.  
Example:

```TVMSolidity
TvmSlice slice;
(uint8 a, uint16 b) = slice.decode(uint8, uint16);
(uint16 num0, uint32 num1, address addr) = slice.decode(uint16, uint32, address);
```

##### TvmSlice.loadRef()

```TVMSolidity
TvmSlice slice;
TvmCell cell = slice.loadRef();
```

This function loads a cell from the slice reference.

##### TvmSlice.loadRefAsSlice()

```TVMSolidity
TvmSlice slice;
TvmSlice refSlice = slice.loadRefAsSlice();
```

This function loads a cell from the slice reference and converts it into a slice.

##### TvmSlice.loadSigned()

```TVMSolidity
TvmSlice slice;
uint16 bitSize;
int number = slice.loadSigned(bitSize)
```

This function loads a signed integer with the given **bitSize** from the slice.

##### TvmSlice.loadUnsigned()

```TVMSolidity
TvmSlice slice;
uint16 bitSize;
uint number = slice.loadUnsigned(bitSize);
```

This function loads an unsigned integer with the given **bitSize** from the slice.

##### TvmSlice.decodeFunctionParams()

```TVMSolidity
TvmSlice slice;
(TypeA a, TypeB b, ...) = slice.decodeFunctionParams(function_name);
```

This function decodes parameters of function with given name. It's very convenient if there are many params and they don't fit in one cell. This function is usually used in **[onBounce](#onbounce)** function.

See example of how to use **onBounce** function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### TvmBuilder

TvmBuilder represents TVM type Builder. TON Solidity compiler defines the following functions to work with this type:

##### TvmBuilder.toSlice()

```TVMSolidity
TvmBuilder builder;
TvmSlice slice = builder.toSlice();
```

This function converts the builder into a Slice.

##### TvmBuilder.toCell()

```TVMSolidity
TvmBuilder builder;
TvmCell cell = builder.toCell();
```

This function converts the builder into a Cell.

##### TvmBuilder.bits()

```TVMSolidity
TvmBuilder builder;
uint16 bits = builder.bits();
```

This function returns the number of data bits already stored in the builder.

##### TvmBuilder.refs()

```TVMSolidity
TvmBuilder builder;
uint8 refs = builder.refs();
```

This function returns the number of references already stored in the builder.

##### TvmBuilder.bitsAndRefs()

```TVMSolidity
TvmBuilder builder;
(uint16 bits, uint8 refs) = builder.bitsAndRefs();
```

This function returns the number of data bits and references already stored in the builder.

##### TvmBuilder.remBits()

```TVMSolidity
TvmBuilder builder;
uint16 remBits = builder.remBits();
```

This function returns the number of data bits that can still be stored in the builder.

##### TvmBuilder.remRefs()

```TVMSolidity
TvmBuilder builder;
uint8 refRefs = builder.remRefs();
```

This function returns the number of references that can still be stored in the builder.

##### TvmBuilder.remBitsAndRefs()

```TVMSolidity
TvmBuilder builder;
(uint16 remBits, uint8 remRefs) = builder.remBitsAndRefs();
```

This function returns the number of data bits and references that can still be stored in the builder.

##### TvmBuilder.store()

```TVMSolidity
TvmBuilder builder;
builder.store(/*list_of_variables*/);
```

This function stores variables in the builder. Available types:

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
uint8 a;
int16 b;
TvmBuilder builder;
builder.store(a, b, uint(123132));
```

##### TvmBuilder.storeSigned()

```TVMSolidity
TvmBuilder builder;
int256 value;
uint16 bitSize;
builder.storeSigned(value, bitSize);
```

This function stores a signed integer with given **bitSize** in the builder.

##### TvmBuilder.storeUnsigned()

```TVMSolidity
TvmBuilder builder;
uint256 value;
uint16 bitSize;
builder.storeUnsigned(value, bitSize);
```

This function stores an unsigned integer with given **bitSize** in the builder.

##### TvmBuilder.storeRef()

```TVMSolidity
TvmBuilder builder;
TvmBuilder builder2;
builder.storeRef(builder2)
```

This function converts the argument builder into a cell and stores it in a reference of the builder.

See example of how to work with TVM specific types:

* [Message_construction](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageSender.sol)
* [Message_parsing](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageReceiver.sol)

#### bytes

##### operator[]

```TVMSolidity
bytes byteArray = "abba";
int index = 0;
byte a0 = byteArray[index];
```

Operator **[]** returns a byte located at position **index**.  
Warring: **index** must be in range 0 to 203 include.

##### bytes.length

```TVMSolidity
bytes byteArray = "abba";
uint l = byteArray.length;
```

Member **length** returns length of byte array.  
Warring: if length of array is bigger than 204 than this function return 204.

#### string

TON Solidity compiler expands **string** type with the following functions:

##### string.byteLength()

```TVMSolidity
string str
uint8 len = str.byteLength();
```

This function returns byte length of the string data.  
Warring: if length of string is bigger than 204 than this function return 204.

##### string.substr()

```TVMSolidity
string str;
uint8 from;
uint8 count;
string substr = str.substr(from, count);
```

This function returns a substring starting from the byte with number **from** with byte length **count**.  
Warring: **from** must be in range 0 to 203 include and **from + count** must be in range 1 to 204 include.

#### address

**address** represents different types of TVM addresses: **addr_none**, **addr_extern** and **addr_std**. TON Solidity compiler expands **address** type with the following members and functions:

##### Object creating

##### constructor()

```TVMSolidity
uint address_value;
address addrStd = address(address_value);
```

This function constructs **address** of type **addr_std** with zero workchain id and given address value.

##### address.makeAddrStd()

```TVMSolidity
int8 wid;
uint address;
address addrStd = address.makeAddrStd(wid, address);
```

This function constructs **address** of type **addr_std** with given workchain id **wid** and value **address_value**.

##### address.makeAddrNone()

```TVMSolidity
address addrNone = address.makeAddrNone();
```

This function constructs **address** of type **addr_none**.

##### address.makeAddrExtern()

```TVMSolidity
uint addrNumber;
uint bitCnt;
address addrExtern = address.makeAddrExtern(addrNumber, bitCnt);
```

This function constructs **address** of type **addr_extern** with given **value** with **bitCnt** bit length.

##### Members

##### address.wid

```TVMSolidity
address addr;
int8 workchain_id = addr.wid;
```

This member of **address** type allows to obtain the workchain id of **addr_std**.

##### address.value

```TVMSolidity
address addr;
uint address_value = addr.value;
```

This member of **address** type allows to obtain the address value of **addr_std**.

##### address.currencies

```TVMSolidity
ExtraCurrencyCollection cur = address(this).currencies;
```

This member returns currencies on the balance of **this** contract.

##### Functions

##### address.getType()

```TVMSolidity
address addr;
uint8 address_type = addr.getType();
```

This function returns type of the **address**:  
0 - addr_none  
1 - addr_extern  
2 - addr_std

##### address.isStdZero()

```TVMSolidity
address addr;
bool status = addr.isStdZero();
```

This function compares **address** with zero **address** of type **addr_std**.

##### address.isExternZero()

```TVMSolidity
address addr;
bool status = addr.isExternZero();
```

This function compares **address** with zero **address** of type **addr_extern**.

##### address.isNone()

```TVMSolidity
address addr;
bool status = addr.isNone();
```

This function returns true for **address** of type **addr_none**, otherwise returns false.

##### address.unpack()

```TVMSolidity
address addr;
(int8 wid, uint256 value) = addr.unpack();
```

This function unpacks **addr_std** and returns workchain id **wid** and address **value**.

##### address.transfer()

```TVMSolidity
address addr;
uint128 value;
bool bounce;
uint16 sendrawmsg_flag;
TvmCell body;
ExtraCurrencyCollection c;
addr.transfer(value, bounce, sendrawmsg_flag, body, c);
```

Ths function is an overload of "address.transfer(uint128 value)" function. It allows to make a currency transfer with arbitrary settings. Unlike usual "address.transfer(uint128 value)" (which has bounce flag set to true) it can be used to send value to a non-existing address.  
Example:

```TVMSolidity
TvmCell cell = getCell();
ExtraCurrencyCollection c = getCurrency();
address destination = msg.sender;
destination.transfer({value:122, bounce:false, flag:128, body:cell, currencies:c});
destination.transfer({value:122, bounce:false, flag:128, body:cell});
destination.transfer({value:122, bounce:false, flag:128});
destination.transfer(10000, true, 1)
destination.transfer(10000, false, 128, cell)
```

Some named parameters can be omitted. If parameter is omitted than default value is used.  
Default values:  
``value`` = 0  
``bounce`` = true  
``flag`` = 1  
``body``  = empty TvmCell  
``currencies`` = empty ExtraCurrencyCollection

See example of how to use address.transfer():

* [giver](https://github.com/tonlabs/samples/blob/master/solidity/7_Giver.sol)

#### mapping

See example of how to work with mapping:

* [database](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollector.sol)
* [client](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollectorClient.sol)

TON Solidity compiler expands **mapping** type with the following functions.
In code examples below identifier **map** defines object of **mapping(KeyType => ValueType)** type.

##### mapping.min() and mapping.max()

```TVMSolidity
(KeyType key, ValueType value, bool haveValue) = map.min();
(KeyType key, ValueType value, bool haveValue) = map.max();
```

This function computes the minimal (maximal) key of mapping and returns that key, associated value and true for **haveValue**. If mapping is empty, this function returns default values and false for **haveValue**.

##### mapping.next() and mapping.prev()

```TVMSolidity
KeyType key;
(KeyType nextKey, ValueType nextValue, bool haveValue) = map.next(key);
(KeyType prevKey, ValueType prevValue, bool haveValue) = map.prev(key);
```

This function computes the minimal (maximal) key in mapping that is lexicographically greater (less) than **key** and returns that key, associated value and status flag.

##### mapping.nextOrEq() and mapping.prevOrEq()

```TVMSolidity
KeyType key;
(KeyType nextKey, ValueType nextValue, bool haveValue) = map.nextOrEq(key);
(KeyType prevKey, ValueType prevValue, bool haveValue) = map.prevOrEq(key);
```

This function computes the minimal (maximal) key in mapping that is lexicographically greater than or equal to (less than or equal to) **key** and returns that key, associated value and status flag.

##### mapping.delMin() and mapping.delMax()

```TVMSolidity
(KeyType firstKey, ValueType value) = map.delMin();
(KeyType lastKey, ValueType value) = map.delMax();
```

 If mapping is not empty than this function computes the minimal (maximum) key of mapping, deletes that key and associated value from the mapping and returns that key and associated value.
 Else exception is thrown.

##### mapping.fetch()

```TVMSolidity
KeyType key;
(bool exists, ValueType value) = map.fetch(key);
```

This function checks whether **key** presents in the mapping and returns status flag and associated value(if it exists).
If the value has struct type and key doesn't present in the mapping than value is NULL. Use value only in such constructions:

```TVMSolidity
(bool ok, MyStruct value) = map.fetch(key);
if (ok) {
    // use value here
}
```

##### mapping.exists()

```TVMSolidity
KeyType key;
bool exists = map.exists(key);
```

This function checks whether **key** presents in the mapping and returns status flag.

##### mapping.empty()

```TVMSolidity
bool isEmpty = map.empty();
```

This function checks whether the mapping is empty and returns status flag.

##### mapping.replace()

```TVMSolidity
KeyType key;
ValueType value;
bool success = map.replace(key, value);
```

This function sets the value associated with **key** only if the **key** is present in mapping.

##### mapping.add()

```TVMSolidity
KeyType key;
ValueType value;
bool success = map.add(key, value);
```

This function sets the value associated with **key** only if the **key** is not present in mapping.

##### mapping.getSet()

```TVMSolidity
KeyType key;
ValueType value;
(ValueType oldValue, bool haveOldValue) = map.getSet(key, value);
```

This function sets the **value** associated with **key**, but also
returns the **oldValue** associated with the **key**, if present. Else it returns default value.

##### mapping.getAdd()

```TVMSolidity
KeyType key;
ValueType value;
(ValueType oldValue, bool haveOldValue) = map.getAdd(key, value);
```

This function sets the **value** associated with **key**, but only if **key** is not present in mapping. Otherwise, just returns the **oldValue** without changing the dictionary.

##### mapping.getReplace()

```TVMSolidity
KeyType key;
ValueType value;
(ValueType oldValue, bool haveOldValue) = map.getReplace(key, value);
```

This function sets the **value** associated with **key**, but only if **key** is present in mapping. On success, returns the **oldValue** associated with the **key**. Otherwise, returns default value.

#### ExtraCurrencyCollection

ExtraCurrencyCollection represents TVM type ExtraCurrencyCollection. It has the same functions as **mapping(uint32 => uint256)**:

```TVMSolidity
ExtraCurrencyCollection curCol;
uint32 key;
uint256 value;
(uint32 minKey, uint256 minKeyValue, bool haveValue) = curCol.min();
(uint32 nextKey, uint256 nextValue, bool haveValue) = curCol.next(key);
(uint32 prevKey, uint256 prevValue, bool haveValue) = curCol.prev(key);
(uint32 firstKey, uint256 value) = curCol.delMin();
(uint32 firstKey, uint256 value) = curCol.delMax();
(bool exists, uint256 value) = curCol.fetch(key);
bool exists = curCol.exists(key);
bool isEmpty = curCol.empty();
bool success = curCol.replace(key, value);
bool success = curCol.add(key, value);
(uint32 oldValue, bool haveOldValue) = curCol.getSet(key, value);
(uint32 oldValue, bool haveOldValue) = curCol.getAdd(key, value);
(uint32 oldValue, bool haveOldValue) = curCol.getReplace(key, value);
uint256 value = curCol[index];
```

#### require, revert

On exception state variables of the contract are reverted to state before
[tvm.commit()](#tvmcommit) or to state before contract was called.

##### require

```TVMSolidity
uint a = 5;

require(a == 5); // ok 
require(a == 6); // throw exception 100
require(a == 6, 101); // throw exception 101
require(a == 6, 101, "a is not equal to six"); // throw exception 101 and string
require(a == 6, 101, a); // throw exception 101 and number a
```

The **require** function can be used to check for conditions and throw an exception if the condition is not met. The function takes condition and optional parameters: error code (unsigned integer) and any object.

##### revert

```TVMSolidity
uint a = 5;
revert(); // throw exception 100
revert(101); // throw exception 101
revert(102, "We have a some problem"); // throw exception 102 and string
revert(101, a); // throw exception 101 and number a
```

The **revert** function are used to trigger exceptions. The function takes an optional error code (unsigned integer) and some object.

### Pragmas

#### ignoreIntOverflow

```TVMSolidity
pragma ignoreIntOverflow;
```

This pragma turns off binary operation result overflow check.

#### AbiHeader

```TVMSolidity
pragma AbiHeader time;
pragma AbiHeader pubkey;
pragma AbiHeader expire;
```

This pragmas force message forming utility to fill an appropriate fields in the header of message to be sent to this contract:

* **pubkey** - public key by which the message was signed;
* **time**   - local time at what message was created;
* **expire** - time at which message should be meant as expired.

**pubkey** field is necessary for the contract to be able to check message signature which was generated with
public key that is different from what is stored in this contract data.  
**time** and **expire** fields can be used for replay protection and if set they should be read in [afterSignatureCheck](#aftersignaturecheck) in case of not default replay protection.  
To read more about this and ABI follow this [link](https://docs.ton.dev/86757ecb2/p/40ba94-abi-specification-v2).  
Here is example of [message expiration time](https://docs.ton.dev/86757ecb2/p/88321a-message-expiration-time) usage.

### Special contract functions

##### receive

```TVMSolidity
contract Sink {
    uint counter = 0;
    receive() external payable {
        ++counter;
    }
```

On plain value transfer **receive** function is
called. See [address.transfer()](#addresstransfer)  
If there is no **receive** function and **fallback** function
**fallback** function is called.  
If there are no **receive** and **fallback** functions, contract
does nothing on plain value transfer.  
If you don't want the contract to receive plain value transfers, define
 **receive** function and throw exception in that function.

##### fallback

```TVMSolidity
contract Contr {
    uint counter = 0;
    fallback() external {
        ++counter;
    }
```

**fallback** function is called when body of the inbound message has invalid
function id.  
If no **receive** function is defined and **fallback** function exists,
**fallback** function is called on plain value transfer. See [address.transfer()](#addresstransfer).


#### onBounce

```TVMSolidity
onBounce(TvmSlice slice) external override {
    /*...*/
}
```

**onBounce** function is executed if inbound internal message has bounced flag set. Parameter slice of onBounce function contains truncated body of the message (it's truncated by the network). If this function it not defined than on inbound internal message contract do nothing.

See example of how to use **onBounce** function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### onCodeUpgrade

```TVMSolidity
function onCodeUpgrade() private {
    /*...*/
}
```

**onCodeUpgrade** function can have arbitrary set of arguments and should be executed after [tvm.setcode()](#tvmsetcode) function call. In this function [tvm.resetStorage()](#tvmresetstorage) should be called if the set of state variables is changed in the new version of the contract. This function implicitly calls [tvm.commit()](#tvmcommit). After return from **onCodeUpgrade** TVM execution is finished with exit code 0.

See example of how to upgrade code:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

#### afterSignatureCheck

```TVMSolidity
function afterSignatureCheck(TvmSlice body, TvmCell message) private inline returns (TvmSlice) {
    /*...*/
}
```

Developer can define **afterSignatureCheck** function to create his own replay protection function instead of default one.

See example of how to define this function:

* [Custom replay protection](https://github.com/tonlabs/samples/blob/master/solidity/14_CustomReplayProtection.sol)

### Function specifiers

#### functionID()

```TVMSolidity
function functionName() public pure functionID(123) {
    /*...*/
}
 ```

This keyword allows to set identifier of the function manually.

### Events and return

#### extAddr

```TVMSolidity
emit EventName(arguments).extAddr(address);
emit EventName(arguments);
```

TON Solidity compiler allows to specify destination address of the message sent via event emitting using suffix **extAddr**. If **extAddr** suffix is not used, external address is set to **addr_none**.

#### return

```TVMSolidity
function f(uint n) public pure {
    return n <= 1? 1 : n * f(n - 1);
}
```

Public or external functions (called by external message) send an external message on return. Destination address of that message is the source address of the inbound external message.
For example, if function **f**  above was called with **n** = 5 by external message, only one external message is sent. Internal call of public or external function doesn't generate external message.

### API functions and members

#### **msg** namespace

##### msg.pubkey()

```TVMSolidity
uint256 pubkey = msg.pubkey();
```

This function returns sender's public key, obtained from the body if the external inbound message. If message is not signed function returns 0. If message is signed and message header ([AbiHeader](#abiheader)) does not contain pubkey than msg.pubkey() is equal to tvm.pubkey().

##### msg.createdAt

```TVMSolidity
uint32 created_at = msg.createdAt;
```

This member is a field **created_at** of the external inbound message.

##### msg.currencies

```TVMSolidity
ExtraCurrencyCollection c = msg.currencies;
```

This member is a field of the internal inbound message.

#### **tvm** namespace

##### TVM instructions

##### tvm.accept()

```TVMSolidity
tvm.accept()
```

This function executes TVM instruction "ACCEPT" ([TVM][1] - A.11.2. - F800). This instruction sets current gas limit to its maximal allowed value. This action is required to process external messages, which bring no value.

See example of how to use this function:

* [accumulator](https://github.com/tonlabs/samples/blob/master/solidity/1_Accumulator.sol)

##### tvm.commit()

```TVMSolidity
tvm.commit()
```

This function executes TVM instruction "COMMIT" ([TVM][1] - A.11.2. - F80F). This instruction commits the current state of registers c4 and c5 so that the current execution is considered “successful” with the saved values even if an exception is thrown later.

##### tvm.log()

```TVMSolidity
tvm.log(string)
logtvm(string)
```

This function executes TVM instruction "PRINTSTR" ([TVM][1] - A.12.2. - FEFn01ssss).
This command may be ignored if --without-logstr flag is presented in command line for compiler.

**logtvm** is an alias for tvm.log(string).

##### tvm.setcode()

```TVMSolidity
tvm.setcode(TvmCell newCode)
```

This function executes TVM instruction "SETCODE" ([TVM][1] - A.11.9. - FB04). This command creates an output action that would change this smart contract code to that given by Cell **newCode** (this change will take effect only after the successful termination of the current run of the smart contract).

See example of how to use this function:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.cdatasize()

```TVMSolidity
TvmCell cell;
uint totalNumOfCells;
(uint cells, uint bits, uint refs) = tvm.cdatasize(cell, totalNumOfCells);
```

This function executes TVM instruction "CDATASIZE" ([TVM][1] - A.11.7. - F941). This command recursively computes the count of distinct cells, data bits and cell references.

##### tvm.transLT()

```TVMSolidity
uint64 time = tvm.transLT();
```

This function executes TVM instruction "LTIME" ([TVM][1] - A.11.4. - F825). This command returns the logical time of the current transaction.

##### tvm.configParam()

```TVMSolidity
(TypeA a, TypeB b, ...) = tvm.configParam(uint8(paramNumber));
```

This function executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832). This command returns the value of the global configuration parameter with integer index paramNumber. Argument should be an integer literal. Supported paramNumbers: 1, 15, 17, 34.

##### Hashing and cryptography

##### tvm.hash()

```TVMSolidity
uint256 hash = tvm.hash(TvmCell cellTree);
uint256 hash = tvm.hash(string);
uint256 hash = tvm.hash(bytes);
```

This function executes TVM instruction "HASHCU" ([TVM][1] - A.11.6. - F900). It computes the representation hash of a given argument and returns it as a 256-bit unsigned integer

##### tvm.checkSign()

```TVMSolidity
uint256 hash;
uint256 SignHighPart;
uint256 SignLowPart;
uint256 pubkey;
bool signatureIsValid = tvm.checkSign(hash, SignHighPart, SignLowPart, pubkey);
```

This function executes TVM instruction "CHKSIGNU" ([TVM][1] - A.11.6. - F910). This command checks the Ed25519-signature of a **hash** using public key **pubKey**. Signature is represented by two uint256 **SignHighPart** and **SignLowPart**.

##### Deploy contract from contract

##### tvm.insertPubkey()

```TVMSolidity
TvmCell stateInit;
uint256 pubkey;
TvmCell stateInitWithKey = tvm.insertPubkey(stateInit, pubkey);
```

This function inserts a public key into contract's data field.

##### tvm.buildStateInit()

```TVMSolidity
TvmCell code;
TvmCell data;
TvmCell StateInit = tvm.buildStateInit(code, data);
```

This function generates a StateInit ([TBLKCH][2] - 3.1.7.) from code and data cells.

##### tvm.deploy()

```TVMSolidity
TvmCell stateInit;
address addr;
uint128 value;
TvmCell payload;
tvm.deploy(stateInit, addr, value, payload);
```

This function implements "Create smart contract by a smart contract" functionality. It generates and sends a constructor message to create a new contract.

Arguments:  
stateInit   - contract's StateInit;  
addr        - address of the contract;  
value       - amount of currency in nano tons that will be sent to the new contract address;  
payload     - encoded message of constructor call.

##### tvm.deployAndCallConstructor()

```TVMSolidity
TvmCell stateInit;
address addr;
uint128 value;
uint32 constructor_id;
tvm.deployAndCallConstructor(stateInit, addr, value, constructor_id[, <list_of_constructor_arguments>])
```

This function is equal to tvm.deploy() but it takes not body of a constructor call but builds it and then attaches to the constructor message.

Arguments:  
stateInit      - contract's StateInit;  
addr           - address of the contract;  
value          - amount of currency in nano tons that will be sent to the new contract address;  
constructor_id - identifier of constructor function;  
\<list_of_constructor_arguments\>.

##### tvm.deployAndCallConstructorWithFlag()

```TVMSolidity
TvmCell stateInit;
address addr;
uint128 value;
uint8 flag;
uint32 constructor_id;
tvm.deployAndCallConstructorWithFlag(stateInit, addr, value, flag, constructor_id[, <list_of_constructor_arguments>])
```

This function is equal to tvm.deployAndCallConstructor() but sends the message with an appropriate flag.

See example of how to deploy contract from contract:

* [Contract_deployer](https://github.com/tonlabs/samples/blob/master/solidity/11_ContractDeployer.sol)

##### Deploy via new

```TVMSolidity
TvmCell stateInitWithKey;
uint initialValue;
uint8 sendRawMsgFlag;
address newWallet = new SimpleWallet{stateInit:stateInitWithKey, value:initialValue, flag:sendRawMsgFlag}();
```

Developer can deploy contract from contract using **new** expression. Detailed description can be found in [doc](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.md).  
Example can be found in the samples repo:

* [Wallet_producer](https://github.com/tonlabs/samples/blob/master/solidity/17_ContractProducer.sol)

##### Others

##### tvm.pubkey()

```TVMSolidity
uint256 pubkey = tvm.pubkey();
```

This function returns contract's public key, stored in contract data. If key is not set function returns 0.

##### tvm.setCurrentCode()

```TVMSolidity
TvmCell newCode;
tvm.setCurrentCode(newCode);
```

This function changes this smart contract current code to that given by Cell **newCode**.

See example of how to use this function:

* [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
* [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.resetStorage()

```TVMSolidity
tvm.resetStorage();
```

This function resets all state variables to their default values.

##### tvm.functionId()

```TVMSolidity
uint32 funcID = tvm.functionId(function_name);
```

This function returns function id (uint32) for public/external function.  
Example:

```TVMSolidity
function f() public pure returns (uint) {
    /*...*/
}

function getFuncID() public pure returns (uint32) {
    uint32 functionId == tvm.functionId(f);
    return functionId;
}
```

See example of how to use this function:

* [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

##### tvm.encodeBody()

```TVMSolidity
TvmCell body = tvm.encodeBody(function_name, arg0, arg1, arg2, ...);
```

This function constructs a function call message body that can be used
as the [transfer](#addresstransfer) payload.  
Example:

```TVMSolidity
contract Remote {
    function func(uint256 num, address a, int64 num2) public pure {}
}

contract Caller {
    function test() public pure {
        TvmCell body = tvm.encodeBody(Remote.func, 123, address.makeAddrStd(22, 333), -654);
        msg.sender.transfer({value:1e10, body:body});
    }
}
```

##### tvm.min() and tvm.max()

```TVMSolidity
int min = tvm.min(int a, int b, ...);
uint max = tvm.min(uint a, uint b, ...);
int min = tvm.max(int a, int b, ...);
uint max = tvm.max(uint a, uint b, ...);
```

This function returns the minimal (maximal) value of the passed arguments.

##### selfdestruct

```TVMSolidity
address dest_addr = msg.sender;
selfdestruct(dest_addr);
```
Create and send the message that carry all the remaining balance
of the current smart contract and destroy the current account.

See example of how to use **selfdestruct** function:
* [Kamikaze](https://github.com/tonlabs/samples/blob/master/solidity/8_Kamikaze.sol)

[1]: https://ton.org/tvm.pdf        "TVM"
[2]: https://ton.org/tblkch.pdf     "TBLKCH"
