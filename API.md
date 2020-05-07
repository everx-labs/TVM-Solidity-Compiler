# **TON Solidity API**
TON Solidity compiler expands Solidity language with different API functions to facilitate TON contract development.

## Table of Contents
* [TON specific types](#ton-specific-types)
  * [TvmCell](#tvmcell)
  * [TvmSlice](#tvmslice)
  * [TvmBuilder](#tvmbuilder)
  * [ExtraCurrencyCollection](#extracurrencycollection)
  * Changes in [string](#string), [address](#address) and [mapping](#mapping)
* [Pragmas](#pragmas)
  * [ignoreIntOverflow](#ignoreintoverflow)
  * [AbiHeader](#abiheader)
* [Contract functions](#contract-functions)
  * [onBounce](#onbounce)
  * [onCodeUpgrade](#oncodeupgrade)
* [Function specificators](#function-specificators)
  * [functionID()](#functionid)
* [Events](#events)
  * [extAddr](#extaddr)
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
    * [Others](#others)
      * [tvm.pubkey()](#tvmpubkey)
      * [tvm.setCurrentCode()](#tvmsetcurrentcode)
      * [tvm.sendMsg()](#tvmsendmsg)
      * [tvm.resetStorage()](#tvmresetstorage)
      * [tvm.setExtDestAddr()](#tvmsetextdestaddr)
      * [tvm.functionId()](#tvmfunctionid)
      * [tvm.min()](#tvmmin)
      * [tvm.max()](#tvmmax)

## Detailed description

### TON specific types
TON Solidity compiler expands functionality of some existing types and adds several new TVM specific types: TvmCell, TvmSlice, TvmBuilder and ExtraCurrencyCollection. Full description of this types can be found in [TVM][1] and [TON Blockchain][2] specifications.

#### TvmCell
TvmCell represents TVM type Cell. TON Solidity compiler defines the following functions to work with this type:

##### 1. TvmCell.toSlice()
```
TvmCell.toSlice() -> returns (TvmSlice)
```
This function converts cell to slice.

#### TvmSlice
TvmSlice represents TVM type Slice. TON Solidity compiler defines the following functions to work with this type:

##### 1. TvmSlice.size()
```
TvmSlice.size() -> returns (uint16, uint8)
```
This function returns number of data bits and references in the slice.

##### 2. TvmSlice.bits()
```
TvmSlice.bits() -> returns (uint16)
```
This function returns number of data bits in the slice.

##### 3. TvmSlice.refs()
```
TvmSlice.refs() -> returns (uint8)
```
This function returns number of references in the slice.

##### 4.  TvmSlice.decode()
```
TvmSlice.decode(<list_of_types>) -> returns (<tuple_of_chosen_types>)
```
This function loads given types from the slice.  
Example:
```
(uint8 a, uint16 b) = slice.decode(uint8, uint16);
(uint16 num0, uint32 num1, address addr) = slice.decode(uint16, uint32, address);
```

##### 5. TvmSlice.loadRef()
```
TvmSlice.loadRef() -> returns (TvmCell)
```
This function loads a cell from the slice reference.

##### 6. TvmSlice.loadRefAsSlice()
```
TvmSlice.loadRefAsSlice() -> returns (TvmSlice)
```
This function loads a cell from the slice reference and converts it into a slice.

##### 7. TvmSlice.loadSigned()
```
TvmSlice.loadSigned(uint16 bitsize) -> returns (int)
```
This function loads a signed integer with the given bitsize from the slice.

##### 8. TvmSlice.loadUnsigned()
```
TvmSlice.loadUnsigned(uint16 bitsize) -> returns (uint)
```
This function loads an unsigned integer with the given bitsize from the slice.

##### 9. TvmSlice.decodeFunctionParams()
```
TvmSlice.decodeFunctionParams(function_name) -> returns (<list_of_function_params>)
```
This function decodes parameters of function with given name. It's very convenient if there are many params and they don't fit in one cell. This function is usually used in **[onBounce](#onbounce)** function.

See example of how to use **onBounce** function:
- [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### TvmBuilder
TvmBuilder represents TVM type Builder. TON Solidity compiler defines the following functions to work with this type:

##### 1. TvmBuilder.toSlice()
```
TvmBuilder.toSlice() -> returns (TvmSlice)
```
This function converts the builder into a Slice.

##### 2. TvmBuilder.toCell()
```
TvmBuilder.toCell() -> returns (TvmCell)
```
This function converts the builder into a Cell.

##### 3. TvmBuilder.bits()
```
TvmBuilder.bits() -> returns (uint16)
```
This function returns the number of data bits already stored in the builder.

##### 4. TvmBuilder.refs()
```
TvmBuilder.refs() -> returns (uint8)
```
This function returns the number of references already stored in the builder.

##### 5. TvmBuilder.bitsAndRefs()
```
TvmBuilder.bitsAndRefs() -> returns (uint16, uint8)
```
This function returns the number of data bits and references already stored in the builder.

##### 6. TvmBuilder.remBits()
```
TvmBuilder.remBits() -> returns (uint16)
```
This function returns the number of data bits that can still be stored in the builder.

##### 7. TvmBuilder.remRefs()
```
TvmBuilder.remRefs() -> returns (uint8)
```
This function returns the number of references that can still be stored in the builder.

##### 8. TvmBuilder.remBitsAndRefs()
```
TvmBuilder.remBitsAndRefs() -> returns (uint16, uint8)
```
This function returns the number of data bits and references that can still be stored in the builder.

##### 9. TvmBuilder.store()
```
TvmBuilder.store(<list_of_variables>)
```
This function stores variables in the builder. Available types:
- integer
- address
- TvmSlice
- TvmBuilder
- TvmCell
- mapping
- struct
- array

Example:
```
uint8 a;
int16 b;
TvmBuilder builder;
builder.store(a, b, uint(123132));
```

##### 10. TvmBuilder.storeSigned()
```
TvmBuilder.storeSigned(int256, uint16)
```
This function stores a signed integer with given bitsize in the builder.

##### 11. TvmBuilder.storeUnsigned()
```
TvmBuilder.storeUnsigned(uint256, uint8)
```
This function stores an unsigned integer with given bitsize in the builder.

##### 12. TvmBuilder.storeRef()
```
TvmBuilder.storeRef(TvmBuilder)
```
This function converts the argument builder to a cell and stores it in a reference of the builder.

See example of how to work with TVM specific types:
- [Message_construction](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageSender.sol)
- [Message_parsing](https://github.com/tonlabs/samples/blob/master/solidity/15_MessageReceiver.sol)


#### string
TON Solidity compiler expands **string** type with the following functions:

##### 1. string.byteLength()
```
string.byteLength() -> returns (uint8)
```
This function returns byte length of the string data.

##### 2. string.substr()
```
string.substr(uint8 from, uint8 count) -> returns (string)
```
This function returns a substring starting from the byte with number <from> with byte length <count>.

#### address
**address** represents different types of TVM addresses: **addr_none**, **addr_extern** and **addr_std**. TON Solidity compiler expands **address** type with the following members and functions:

##### Object creating

##### 1. constructor()
```
address addrStd = address(uint256 address_value);
```
This function constructs **address** of type **addr_std** with zero workchain id and given address value.

##### 2. address.makeAddrStd()
```
address addrStd = address.makeAddrStd(int8 wid, uint address);
```
This function constructs **address** of type **addr_std** with given workchain id *wid* and value *address_value*.

##### 3. address.makeAddrNone()
```
address addrNone = address.makeAddrNone();
```
This function constructs **address** of type **addr_none**.

##### 4. address.makeAddrExtern()
```
address addrExtern = address.makeAddrExtern(uint addrNumber, uint bitCnt);
```
This function constructs **address** of type **addr_extern** with given *value* with *bitCnt* bit length.

##### Members

##### 1. address.wid
```
address.wid -> returns (int8 workchain_id)
```
This member of **address** type allows to obtain the workchain id of **addr_std**.

##### 2. address.value
```
address.value -> returns (uint256 address_value)
```
This member of **address** type allows to obtain the address value of **addr_std**.

##### 3. address.currencies
```
address(this).currencies -> return ExtraCurrencyCollection
```
This member returns currencies on the balance of **this** contract.

##### Functions

##### 1. address.getType()
```
address.getType() -> (uint8 address_type)
```
This function returns type of the **address**:  
0 - addr_none  
1 - addr_extern  
2 - addr_std

##### 2. address.isStdZero()
```
address.isStdZero() -> returns (bool status)
```
This function compares **address** with zero **address** of type **addr_std**.

##### 3. address.isExternZero()
```
address.isExternZero() -> returns (bool status)
```
This function compares **address** with zero **address** of type **addr_extern**.

##### 4. address.isNone()
```
address.isNone() -> returns (bool status)
```
This function returns true for **address** of type **addr_none**, otherwise returns false.

##### 5. address.unpack()
```
address.unpack() -> returns (int8 wid, uint256 value)
```
This function unpacks **addr_std** and returns workchain id *wid* and address *value*.

##### 6. address.transfer()
```
address.transfer(uint128 grams_value, bool bounce, uint16 sendrawmsg_flag, TvmCell body, ExtraCurrencyCollection c)
```
Ths function is an overload of "address.transfer(uint128 value)" function. It allows to make a currency transfer with arbitrary settings. Unlike usual "address.transfer(uint128 value)" (which has bounce flag set to true) it can be used to send grams to a non-existing address.  
Example:
```
TvmCell cell = getCell();
ExtraCurrencyCollection c = getCurrence();
address destination = msg.sender;
destination.transfer({value:122, bounce:false, flag:128, body:cell, currencies:c});
destination.transfer({value:122, bounce:false, flag:128, body:cell});
destination.transfer({value:122, bounce:false, flag:128});
destination.transfer(10000, true, 1)
destination.transfer(10000, false, 128, cell)

```
Some named parameters can be omitted. If parameter is omitted than default value is used.  
Default values:  
value = 0  
bounce = true  
flag = 1  
body  = TvmCell  
currencies = ExtraCurrencyCollection

See example of how to use address.transfer():
- [giver](https://github.com/tonlabs/samples/blob/master/solidity/7_Giver.sol)

#### mapping
TON Solidity compiler expands **mapping** type with the following functions:

##### 1. mapping.min()
```
mapping.min() -> returns (KeyType key, ValueType value, bool have_value)
```
This function computes the minimal key of mapping and returns that key, associated value and true for *have_value*. If mapping is empty, this function returns default values and false for *have_value*.

##### 2. mapping.max()
```
mapping.max() -> returns (KeyType key, ValueType value, bool have_value)
```
This function computes the maximal key of mapping and returns that key, associated value and true for *have_value*. If mapping is empty, this function returns default values and false for *have_value*.

##### 3. mapping.next()
```
mapping.next(KeyType prev_key) -> returns (KeyType nextkey, ValueType next_value, bool have_value)
```
This function computes the minimal key in mapping that is lexicographically greater than *prev_key* and returns that key, associated value and status flag.

##### 4. mapping.prev()
```
mapping.prev(KeyType prev_key) -> returns (KeyType nextkey, ValueType next_value, bool have_value)
```
This function computes the maximal key in mapping that is lexicographically less than *prev_key* and returns that key, associated value and status flag.

##### 5. mapping.delMin()
```
mapping.delMin() -> returns (KeyType firstKey, ValueType value)
```
This function computes the minimal key of mapping, deletes that key and associated value from the mapping and returns that key and associated value.

##### 6. mapping.fetch()
```
mapping.fetch(KeyType key) -> returns (bool exists, ValueType value)
```
This function checks whether *key* presents in the mapping and returns status flag and associated value(if it exists).
If the value has struct type and key doesn't present in the mapping than value is NULL. Use value only in such constructions:
```
(bool ok, MyStruct value) = map.fetch(key);
if (ok) {
	// use value only after this check
}
```

##### 7. mapping.exists()
```
mapping.exists(KeyType key) -> returns (bool exists)
```
This function checks whether *key* presents in the mapping and returns status flag.

##### 8. mapping.empty()
```
mapping.empty() -> returns (bool isEmpty)
```
This function checks whether the mapping is empty and returns status flag.

See example of how to work with mapping:
- [database](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollector.sol)
- [client](https://github.com/tonlabs/samples/blob/master/solidity/13_BankCollectorClient.sol)


#### ExtraCurrencyCollection
ExtraCurrencyCollection represents TVM type ExtraCurrencyCollection. It has the same functions as **mapping(uint32 => uint256)**:
```
ExtraCurrencyCollection CurCol;
CurCol.min() -> returns (uint32 key, uint256 value, bool have_value)
CurCol.next(uint32 prev_key) -> returns (uint32 nextkey, uint256 next_value, bool have_value)
CurCol.delMin() -> returns (uint32 firstKey, uint256 value)
CurCol.fetch(uint32 key) -> returns (bool exists, uint256 value)
CurCol.exists(uint32 key) -> returns (bool exists)
CurCol.empty() -> returns (bool isEmpty)
CurCol[index] -> returns (uint256 value)
```

### Pragmas

#### ignoreIntOverflow
```
pragma ignoreIntOverflow;
```
This pragma turns off binary operation result overflow check.

#### AbiHeader
```
pragma AbiHeader time;
pragma AbiHeader pubkey
pragma AbiHeader expire;
```
This pragma can tells the compiler that message contains an approriate field in the header.  
See example of [message expiration time](https://docs.ton.dev/86757ecb2/p/88321a-message-expiration-time).

### Contract functions

#### onBounce
```
onBounce(TvmSlice slice) external override {
	/*...*/
}
```
**onBounce** function is executed if inbound internal message has bounced flag set. Parameter slice of onBounce function contains body of the message.

See example of how to use **onBounce** function:
- [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

#### onCodeUpgrade
```
function onCodeUpgrade() private {
	/*...*/
}
```
**onCodeUpgrade** function can have arbuitrary set of arguments and should be executed after [tvm.setcode()](#tvmsetcode) function call. In this function [tvm.resetStorage()](#tvmresetstorage) should be called if the set of state variables is changed in the new version of the contract. This function implicitly calls [tvm.commit()](#tvmcommit). After return from **onCodeUpgrade** TVM execution is finished with exit code 0.

See example of how to upgrade code:
- [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
- [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)


#### afterSignatureCheck
```
function afterSignatureCheck(TvmSlice body, TvmCell message) private inline returns (TvmSlice) {
	/*...*/
}
```
Developer can define **afterSignatureCheck** function to create his own replay protection function instead of default one.

See example of how to define this function:
- [Custom replay protection](https://github.com/tonlabs/samples/blob/master/solidity/14_CustomReplayProtection.sol)


### Function specificators

#### functionID()
 ```
function functionName() public pure functionID(123) {
	/*...*/
}
 ```
This keyword allows to set identificator of the function manually.

### Events

#### extAddr
```
emit EventName(arguments).extAddr(address)
```
TON Solidity compiler allows to specify destination address of the message sent via event emitting using suffix **extAddr**.

### API functions and members

#### **msg** namespace

##### msg.pubkey()
```
msg.pubkey() -> returns (uint256 pubkey)
```
This function returns sender's public key, obtained from the body if the external inbound message. If message is not signed function returns 0. If message is signed and message header ([AbiHeader](#abiheader)) does not contain pubkey than msg.pubkey() is equal to tvm.pubkey().

##### msg.createdAt
```
msg.createdAt -> returns (uint32 created_at)
```
This member is a field *created_at* of the external inbound message.

##### msg.currencies
```
msg.currencies -> returns ExtraCurrencyCollection
```
This member is a field *CurrencyCollection:other* of the external inbound message.


#### **tvm** namespace

##### TVM instructions

##### tvm.accept()
```
tvm.accept()
```
This function executes TVM instruction "ACCEPT" ([TVM][1] - A.11.2. - F800). This instruction sets current gas limit to its maximal allowed value. This action is required to process external messages, which bring no value.

See example of how to use this function:
- [accumulator](https://github.com/tonlabs/samples/blob/master/solidity/1_Accumulator.sol)

##### tvm.commit()
```
tvm.commit()
```
This function executes TVM instruction "COMMIT" ([TVM][1] - A.11.2. - F80F). This instruction commits the current state of registers c4 and c5 so that the current execution is considered “successful” with the saved values even if an exception is thrown later.

##### tvm.log()
```
tvm.log(string)
logtvm(string)
```
This function executes TVM instruction "PRINTSTR" ([TVM][1] - A.12.2. - FEFn01ssss).
This command may be ignored if --without-logstr flag is presented in command line for compiler.

**logtvm** is an alias for tvm.log(string).

##### tvm.setcode()
```
tvm.setcode(TvmCell newcode)
```
This function executes TVM instruction "SETCODE" ([TVM][1] - A.11.9. - FB04). This command creates an output action that would change this smart contract code to that given by Cell *newcode* (this change will take effect only after the successful termination of the current run of the smart contract).

See example of how to use this function:
- [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
- [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.cdatasize()
```
tvm.cdatasize(TvmCell cell, uint totalNumOfCells) -> returns (uint cells, uint bits, uint refs)
```
This function executes TVM instruction "CDATASIZE" ([TVM][1] - A.11.7. - F941). This command recursively computes the count of distinct cells, data bits and cell references.

##### tvm.transLT()
```
tvm.transLT()
```
This function executes TVM instruction "LTIME" ([TVM][1] - A.11.4. - F825). This command returns the logical time of the current transaction.

##### tvm.configParam()
```
tvm.configParam(uint8 paramNumber)
```
This function executes TVM instruction "CONFIGPARAM" ([TVM][1] - A.11.4. - F832). This command returns the value of the global configuration parameter with integer index paramNumber. Argument should be an integer literal. Supported paramNumbers: 1, 15, 17, 34.


##### Hashing and cryptography

##### tvm.hash()
```
tvm.hash(TvmCell cellTree) -> returns (uint256)
tvm.hash(string)           -> returns (uint256)
tvm.hash(bytes)            -> returns (uint256)
```
This function executes TVM instruction "HASHCU" ([TVM][1] - A.11.6. - F900). It computes the representation hash of a given argument and returns it as a 256-bit unsigned integer

##### tvm.checkSign()
```
tvm.checkSign(uint256 /*hash*/, uint256 /*SignHighPart*/, uint256 /*SignLowPart*/, uint256 /*pubkey*/) ->returns (bool)
```
This function executes TVM instruction "CHKSIGNU" ([TVM][1] - A.11.6. - F910). This command checks the Ed25519-signature of a hash using public key pubKey. Signature is represented by two uint256 SignHighPart and SignLowPart.


##### Deploy contract from contract

##### tvm.insertPubkey()
```
tvm.insertPubkey(TvmCell stateInit, uint256 pubkey) -> returns(TvmCell stateInitWithKey)
```
This function inserts a public key into contract's data field.

##### tvm.buildStateInit()
```
tvm.buildStateInit(TvmCell code, TvmCell data) -> returns (TvmCell StateInit)
```
This function generates a StateInit ([TBLKCH][2] - 3.1.7.) from code and data cells.

##### tvm.deploy()
```
tvm.deploy(TvmCell stateInit, address addr, uint128 grams, TvmCell payload)
```
This function implements "Create smart contract by a smart contract" functionality. It generates and sends a constructor message to create a new contract.

Arguments:  
stateInit   - contract's StateInit;  
addr        - address of the contract;  
gram        - amount of currency in nanograms that will be sent to the new contract address;  
payload     - encoded message of constructor call.

##### tvm.deployAndCallConstructor()
```
tvm.deployAndCallConstructor(TvmCell stateInit, address addr, uint128 grams, uint32 constructor_id, <list_of_constructor_arguments>)
```
This function is equal to tvm.deploy() but it takes not body of a constructor call but builds it and then attaches to the constructor message.

Arguments:  
stateInit      - contract's StateInit;  
addr           - address of the contract;  
grams          - amount of currency in nanograms that will be sent to the new contract address;  
constructor_id - identificator of constructor function;  
\<list_of_constructor_arguments\>.

##### tvm.deployAndCallConstructorWithFlag()
```
tvm.deployAndCallConstructorWithFlag(TvmCell my_contract, address addr, uint128 gram, uint8 flag, uint32 constructor_id, <list_of_constructor_arguments>)
```
This function is equal to tvm.deployAndCallConstructor() but sends the message with an appropriate flag.

See example of how to deploy contract from contract:
- [Contract_deployer](https://github.com/tonlabs/samples/blob/master/solidity/11_ContractDeployer.sol)

##### Others

##### tvm.pubkey()
```
tvm.pubkey() -> returns (uint256 pubkey)
```
This function returns contract's public key, stored in contract data. If key is not set function returns 0.

##### tvm.setCurrentCode()
```
tvm.setCurrentCode(TvmCell newcode)
```
This function changes this smart contract current code to that given by Cell *newcode*.

See example of how to use this function:
- [old contract](https://github.com/tonlabs/samples/blob/master/solidity/12_BadContract.sol)
- [new contract](https://github.com/tonlabs/samples/blob/master/solidity/12_NewVersion.sol)

##### tvm.sendMsg()
```
tvm.sendMsg(uint256 dest_addr_literal, uint32 funcID_literal, uint8 flag_literal, uint120 value = 10_000_000)
tvm.sendMsg(address dest_addr, uint32 funcID_literal, uint8 flag_literal, uint120 value = 10_000_000)
```
This function allows to call remote contract with address dest_addr function. Remote contract function is defined with identificator funcID_literal. Function generates msg and sends it via SENDRAWMSG instruction. If not passed value is set to 10000000 (10000 gas * 1000 gas price).

##### tvm.resetStorage()
```
tvm.resetStorage()
```
This function resets all state variables to their default values.

##### tvm.setExtDestAddr()
```
tvm.setExtDestAddr(address destAddr)
```
This function sets destination address for outbound external messages sent on emit or return command.

##### tvm.functionId()
```
tvm.functionId(function_name) -> returns (uint32)
```
This function returns function id (uint32) for public/external function.  
Example:
```
function f() public pure returns (uint) {
	/*...*/
}

function getFuncID() public pure returns (uint32) {
	uint32 functionId == tvm.functionId(f);
	return functionId;
}
```

See example of how to use this function:
- [onBounceHandler](https://github.com/tonlabs/samples/blob/master/solidity/16_onBounceHandler.sol)

##### tvm.min()
```
tvm.min(int a, int b, ...) -> returns (int)
tvm.min(uint a, uint b, ...) -> returns (uint)
```
This function returns the minimal value of the passed arguments.

##### tvm.max()
```
tvm.max(int a, int b, ...) -> returns (int)
tvm.max(uint a, uint b, ...) -> returns (uint)
```
This function returns the maximal value of the passed arguments.


[1]: https://ton.org/tvm.pdf        "TVM"
[2]: https://ton.org/tblkch.pdf     "TBLKCH"
