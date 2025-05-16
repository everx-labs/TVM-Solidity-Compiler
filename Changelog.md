### 0.78.0 (2024-05-16)

Supported `ton` features (`solc --tvm-version ton ...`):
   * Using opcode `INCOMINGVALUE` for [msg.value](API.md#msgvalue).
   * [tx.storageFees](API.md#txstoragefees).
   * [gasConsumed()](API.md#gasconsumed).
   * [Combined arithmetic operations](API.md#combined-arithmetic-operations).
   * [Hash functions](API.md#hash-functions).
   * [Storing the hash to a builder](API.md#storing-the-hash-to-a-builder)
   * [tvm.sendMsg()](API.md#tvmsendmsg).

Breaking changes:
 * Used another selector. To update old Solidity contracts use [pragma upgrade oldsol](API.md#pragma-upgrade-oldsol) and
function id 1666 for `onCodeUpgrade` in the new ones.
 * Now `_pubkey` in `*.abi.json` file  is `fixedbytes32`, not `uint256`. 
 * Functions [gasToValue](API.md#gastovalue) and [valueToGas](API.md#valuetogas) changed their signatures:
   * `gasToValue(coins gas, int8 wid)` -> `gasToValue(coins gas, bool isMasterchain)`
   * `valueToGas(coins gas, int8 wid)` -> `valueToGas(coins value, bool isMasterchain)`

Bugfixes:
 * Throw compilation error if public function has explicit functionID that is equal to functionID of another public
function that doesn't have explicit functionID.
 * Fixed compilation failure when free function was inline.
 * Fixed compilation failure when use construction `<optional(T)>.get() = (a, b, ...);`, 
e.g. `optional(uint256, uint32) data; data = (111, 222);`.

Compiler features:
 * `address_std` can be converted to `contract type`, and vice versa.  
 * Code analyzer: Print warning (for `solc --tvm-version ton`) if a message is sent after message with flag 128.

### 0.77.0 (2024-10-31)

Bugfixes:
 * Fixed wrong type conversion. For example: conversion from `stack(varuint)` to `stack(varint)` was supported. But now
it's an error.
 * Type checker: `abi.decodeData()` function could take any type for 2d parameter.
 * [format()](API.md#format) threw an exception for non `addr_std` address. See [issues #168](https://github.com/everx-labs/TVM-Solidity-Compiler/issues/168). 
Now [format()](API.md#format) can take address of any type. 
 * Fixed bug when you iterate over array of mapping. See [issues #169](https://github.com/everx-labs/TVM-Solidity-Compiler/issues/169).
 * Fixed bug when overloading inline function. See [issues #167](https://github.com/everx-labs/TVM-Solidity-Compiler/issues/167).
 * Fixed compilation failure on formatted output pattern. See [issues #166](https://github.com/everx-labs/TVM-Solidity-Compiler/issues/166).

Compiler features:
 * Supported [ABI 2.7](https://github.com/everx-labs/ever-abi/blob/master/CHANGELOG.md#version-270)
   * Supported [getters](API.md#getter).
   * Supported [address_std](API.md#address_std) type.
 * Supported encoding internal message to call constructor. See option 2 [abi.encodeIntMsg()](API.md#abiencodeintmsg).

Optimizations:
 * optimized decoding function parameters if the function marked as `externalMsg` or `internalMsg`.

Function [\<address\>.getType()](API.md#addressgettype) now returns  `uint4` instead of `uint8`.

### 0.76.0 (2024-07-18)

Compiler features:
 * Supported `bool` type for function [format](API.md#format).
 * Supported [\<TvmBuilder\>.storeQ()](API.md#tvmbuilderstoreq).

Fixed minor bugs.

### 0.75.0 (2024-06-04)

Breaking changes:
 * If the contract does not have constructor explicitly and does not have state variables with initialisation, then in `*.abi.json` file there is no `constructor` function and no `_constructorFlag` field. See [Deploy the contract with no constructor](API.md#deploy-the-contract-with-no-constructor) for more details.

Bugfixes:
 * Peephole optimizer generated invalid code. See [issue #153](https://github.com/everx-labs/TVM-Solidity-Compiler/issues/153).
 * Fixed segmentation fault that occurred in some cases of using incorrect types for `math.*` and some another functions.
 * Fixed minor bugs in TypeChecker (using bad type for mapping value).

Compiler features:
 * Supported `stateInit`option for [External function calls](./API.md#external-function-calls).
 * Supported [math.mulmod()](./API.md#mathmulmod).
 * Supported functions to work with `TvmSlice`'s:
   * [\<TvmSlice\>.bitEmpty() and \<TvmSlice\>.refEmpty()](./API.md#tvmsliceempty-tvmslicebitempty-and-tvmslicerefempty)
   * [\<TvmSlice\>.startsWith()](./API.md#tvmslicestartswith)
   * [\<TvmSlice\>.startsWithOne()](./API.md#tvmslicestartswithone)
 * Supported [operations on a pairing friendly BLS12-381 curve](./API.md#bls-namespace).
 * Supported conversion string literals to `TvmSlice`, e.g. `TvmSlice s = "0189abef_";`.

Usability:
 * Print function-candidates on compilation failure (for function overloading). 

Optimizations:
 * 14-bit numbers are used for function ids for private/internal functions for code optimizations instead of 32-bit.
   32-bit numbers are still used for `onCodeUpgrade` functions for back compatibility. 
 * peephole optimizations.

### 0.74.0 (2024-04-11)

Breaking changes:
 * Now `int` is alias for `int257`, not for `int256`. That's why `*loadInt*` functions return `int257` and `*storeInt*` functions take `int257`.
 * Deleted debots supporting.
 * Now [vector](API.md#vectort) can contain at least 255 elements.

Bugfixes:
 * Fixed minor bugs in TypeChecker.
 * Fixed compilation fail when you have private and public functions with the same name in the contract.
 * Fixed another minor bugs.

Compiler features:
 * Supported [quiet arithmetic](./API.md#quiet-arithmetic).
 * Supported [StringBuilder](./API.md#stringbuilder).
 * Supported [int257](API.md#integers).
 * Supported [\<vector(T)\>.last()](API.md#vectortlast).
 * Supported [stack(T)](API.md#stackt).
 * Supported unary operators (`++`, `--`, `-`, `delete` and `~`) for [varint and varuint](API.md#varint-and-varuint).
 * Supported [Free function call via object](API.md#free-function-call-via-object).

Other changes:
 * Renamed some types. Old types are available and marked as deprecated. Renaming:
   * `varInt` -> `varint` 
   * `varUint` -> `varuint` 
   * `varIntM` -> `varintM`
   * `varUintM` -> `varuintM`

### 0.73.0 (2024-02-12)

Update compiler frontend (from original version 0.8.17 to 0.8.24). Full changelog can be found [here](./compiler/Changelog.md).

Breaking changes:
 * Supported [ABI 2.4](https://github.com/everx-labs/ever-abi/blob/master/CHANGELOG.md#version-240).
 * Deleted `tvm.insertPubkey()` function.
 * Deleted `address.makeAddrNone()` function. Use [address.addrNone](./API.md#addressaddrnone).
 * Default value for `address` type was `address(0)` but now it is [address.addrNone](./API.md#addressaddrnone).
 * `byteN` in `*abi.json` file marked as `fixedbytesN`. See [ABI.md](https://github.com/everx-labs/ever-abi/blob/master/docs/ABI.md#fixedbytesn).
 * [abi.encodeStateInit()](./API.md#abiencodestateinit), [abi.encodeData()](./API.md#abiencodedata) functions and [Deploy via new](./API.md#deploy-via-new) generate contract's data in the new format. To deploy old contracts (with version < 0.72.0) from new ones (with version >= 0.72.0) use [abi.encodeOldDataInit()](./API.md#abiencodeolddatainit).
 * [format()](API.md#format) no longer throws an exception if formatted value exceeds the specified width. E.g. `format("{:1d}", 10) == "10"` now is correct, no exception.
 * Functions and constructions previously working with `uint128` value, now using `varUint16`:
   * [\<address\>.transfer()](API.md#addresstransfer)
   * [deploy via **new**](API.md#deploy-via-new)
   * [external function calls](API.md#external-function-calls)
   * [\<address\>.balance](API.md#addressbalance)
   * [msg.value](API.md#msgvalue)
   * [abi.encodeIntMsg()](API.md#abiencodeintmsg)
   * [return](API.md#return)
 * Control construction [repeat](API.md#repeat) takes `uint31` instead of `uint256`.
 * Supported [\<Integer\>.cast()](API.md#integercast) and changed conversion.

Bugfixes:
 * Fixed compiler assert fault that occurred in the case of using [user-defined value types](https://docs.soliditylang.org/en/latest/types.html#user-defined-value-types) as return parameter of public/external function.
 * Fixed segmentation fault of the compiler that could occur in some cases of using invalid rational number (too large or division by zero), e.g. `math.muldiv(2**500, 1, 1);`.
 * Now [\<string\>.substr()](API.md#stringsubstr) does not throw an exception if `pos + count > string.byteLength()`.
 * Fixed minor bugs in TypeChecker.

Compiler features:
 * Supported defining operators for a [user-defined type](API.md#user-defined-type).
 * Supported [keyword nostorage](API.md#keyword-nostorage).
 * Now you can use the keyword [coins](API.md#varint-and-varuint) as an alias for `varUint16`

Gas optimizations:
 * Optimized functions that work with strings: concatenation, substring, format etc.
 * Assorted stack optimizations.

Other changes:
 * Renamed some functions. Old functions are available and marked as deprecated. Renaming:
   * `tvm.buildDataInit()` -> `abi.encodeData()`
   * `<TvmSlice>.loadStateVars()` -> `abi.decodeData()`
   * `tvm.buildStateInit()` -> `abi.encodeStateInit()`
   * `tvm.stateInitHash()` -> `abi.stateInitHash()`
   * `tvm.codeSalt()` -> `abi.codeSalt()`
   * `tvm.setCodeSalt()` -> `abi.setCodeSalt()`
   * `tvm.functionId()` -> `abi.functionId()`
   * `tvm.buildExtMsg()` -> `abi.encodeExtMsg()`
   * `tvm.buildIntMsg()` -> `abi.encodeIntMsg()`
   * `tvm.encodeBody()` -> `abi.encodeBody()`
   * `<TvmSlice>.loadFunctionParams()` -> `abi.decodeFunctionParams()`
 * Marked functions `<TvmSlice>.loadTons()` and `<TvmBuilder>.storeTons()` as deprecated. Use `<TvmSlice>.load()` and `<TvmBuilder>.store()` that take `varUint16` type.

### 0.72.0 (2023-10-31)

Use [sold](https://github.com/everx-labs/TVM-Solidity-Compiler/tree/master/sold) to compile contracts. If you used `solc`+`tvm_linker`, then use `solc`+[asm](https://github.com/everx-labs/ever-assembler). Generated `*.code` files have some another format.

Breaking changes:
 * The conversion for integer type is only allowed when there is at most one change in sign, width or type-category (`int`, `address`, `bytesNN`, etc.). To perform multiple changes, use multiple conversions. See [Solidity v0.8.0 Breaking Changes](https://docs.soliditylang.org/en/v0.8.17/080-breaking-changes.html#new-restrictions). For example, to convert `int8 x;` to `uint` you can use at least two ways: 1) `uint(uint8(x))`, 2) `uint(int(x))`. 
 * Deleted `ExtraCurrencyCollection` type. Use `mapping(uint32 => varUint32)` instead of it.

Bugfixes:
 * Fixed bug when recursive calling of library function via object corrupted the stack.
 * Fixed minor bug that caused compilation failure.
 * Fixed bug when you use string literals for `bytesN constant`.

Compiler features:
 * Supported [unchecked blocks](API.md#unchecked-block).
 * Supported defining events in libraries.
 * Supported [require(bool condition, string text)](API.md#require).
 * Supported functions for working with exotic cells:
   * [\<TvmCell\>.exoticToSlice()](API.md#tvmcellexotictoslice)
   * [\<TvmCell\>.loadExoticCell() and \<TvmCell\>.loadExoticCellQ()](API.md#tvmcellloadexoticcell-and-tvmcellloadexoticcellq)
   * [\<TvmBuilder\>.toExoticCell()](API.md#tvmbuildertoexoticcell)
 * Supported command line option `--overwrite` that is used together with `--ast-compact-json -o` options.
 * Warning about storing too big structs via `<TvmBuilder>.store()`.
 * Change function signature `<TvmSlice>.skip(uint10 bits, uint2 refs)` -> `<TvmSlice>.skip(uint10 bits, uint3 refs)` to allow to skip 4 references.

### 0.71.0 (2023-07-20)

Bugfixes:
 * Fixed segmentation fault of the compiler that could occur in some cases of using `try-catch` (experimental feature).
 * Fixed compilation error in case of using operator `+` for constant strings.
 * Fixed segmentation fault of the compiler that could occur in the case of using `constructor` with `functionID()`.
 * Fixed an exception which could occur on contract upgrade in case of state variables number being reduced, yet still exceeding 16.

Breaking changes:
 * Now [msg.data](API.md#msgdata) returns the whole message instead of the message body. Use [msg.body](API.md#msgbody) to get the message body (payload).
 * `<address>.unpack` returns `(int32, uint256)` instead of `(int8, uint256)`.

Compiler features:
 * Supported [<optional(T)>.getOr()](API.md#optionaltgetor) and [<optional(T)>.getOrDefault()](API.md#optionaltgetordefault).
 * Supported creating empty cell via `TvmCell()`.
 * Supported `<TvmBuilder>.store()` for `varInt`/`varUint`.
 * Supported [msg.forwardFee](API.md#msgforwardfee).
 * Supported [msg.importFee](API.md#msgimportfee).
 * Supported [gasleft](API.md#gasleft).
 * Supported [\<mapping\>.getDel()](API.md#mappinggetdel).

Gas optimizations:
 * Assorted stack optimizations.

### 0.70.0 (2023-06-14)

Compiler features:
 * Supported [inline assembly](API.md#assembly). 
 * Supported overriding `onCodeUpgrade` function.
 * Supported [User-defined Value Types](https://docs.soliditylang.org/en/latest/types.html#user-defined-value-types). You can also use this type in public functions.
 * [Supported `type(T).min` and `type(T).max`](API.md#type-information).
 * New `<TvmBuilder>.store*()` functions:
   * `<TvmBuilder>.storeIntLE2()`
   * `<TvmBuilder>.storeIntLE4()`
   * `<TvmBuilder>.storeIntLE8()`
   * `<TvmBuilder>.storeUintLE2()`
   * `<TvmBuilder>.storeUintLE4()`
   * `<TvmBuilder>.storeUintLE8()`
 * New `<TvmSlice>.load*()` functions:
   * `<TvmSlice>.loadIntQ()`
   * `<TvmSlice>.loadUintQ()`
   * `<TvmSlice>.loadSliceQ()`
   * `<TvmSlice>.loadIntLE2()`
   * `<TvmSlice>.loadIntLE4()`
   * `<TvmSlice>.loadIntLE8()`
   * `<TvmSlice>.loadUintLE2()`
   * `<TvmSlice>.loadUintLE4()`
   * `<TvmSlice>.loadUintLE8()`
   * `<TvmSlice>.loadIntLE4Q()`
   * `<TvmSlice>.loadIntLE8Q()`
   * `<TvmSlice>.loadUintLE4Q()`
   * `<TvmSlice>.loadUintLE8Q()`
 * New `<TvmSlice>.preload*()` functions:
   * `<TvmSlice>.preload()`
   * `<TvmSlice>.preloadQ()`
   * `<TvmSlice>.preloadRef()`
   * `<TvmSlice>.preloadInt()`
   * `<TvmSlice>.preloadIntQ()`
   * `<TvmSlice>.preloadUint()`
   * `<TvmSlice>.preloadUintQ()`
   * `<TvmSlice>.preloadSlice()`
   * `<TvmSlice>.preloadSliceQ()`
   * `<TvmSlice>.preloadIntLE4()`
   * `<TvmSlice>.preloadIntLE8()`
   * `<TvmSlice>.preloadUintLE4()`
   * `<TvmSlice>.preloadUintLE8()`
   * `<TvmSlice>.preloadIntLE4Q()`
   * `<TvmSlice>.preloadIntLE8Q()`
   * `<TvmSlice>.preloadUintLE4Q()`
   * `<TvmSlice>.preloadUintLE8Q()`
 * Supported `variable integer` type in arguments of `math.*` functions.
 * Supported conversion `bytes`/`string` <=>  `TvmSlice`.

Gas optimizations:
 * Assorted stack optimizations.

Bugfixes:
 * Fixed compilation fail if you use `<TvmSlice>.loadSlice(l)` where `l` is constant and `l == 0` or `l > 256`.

Other changes:
 * Renamed some functions. Old functions are available and marked as deprecated. Renaming:
   * `<TvmSlice>.decode()` -> `<TvmSlice>.load()`
   * `<TvmSlice>.decodeQ()` -> `<TvmSlice>.loadQ()`
   * `<TvmSlice>.decodeFunctionParams()` -> `<TvmSlice>.loadFunctionParams()`
   * `<TvmSlice>.decodeStateVars()` -> `<TvmSlice>.loadStateVars()`
   * `<TvmSlice>.loadSigned()` -> `<TvmSlice>.loadInt()`
   * `<TvmSlice>.loadUnsigned()` -> `<TvmSlice>.loadUint()`
   * `<TvmBuilder>.storeSigned()` -> `<TvmSlice>.storeInt()`
   * `<TvmBuilder>.storeUnsigned()` -> `<TvmSlice>.storeUint()`
 * Improved `try-catch` (experimental feature). 
 * Now `tx.storageFee` returns `uint120` (not `uint64`). 
 * Renamed `tx.timestamp` to `tx.logicaltime`. `tx.timestamp` is available and marked as deprecated.

### 0.69.0 (2023-05-15)

Breaking changes:
 * `pragma AbiHeader time` is no longer allowed for use. Timestamp in header of external message is enabled by default and can be disabled with `pragma AbiHeader notime`.

Compiler features:
 * Supported generating code for The Open Network. Use command line option `--tvm-version ton`. We don't use `ZERO(SWAP|ROTR)IF[NOT][2]`, `COPYLEFT`, `INITCODEHASH`, `MYCODE`, `STORAGEFEE`, `LDCONT`, `STCONT` and another opcodes for ton.

Optimizations:
 * Improved constant analyzer that compile more optimal code for `require`/`revert` functions and another cases.

Bugfixes:
 * Fixed segmentation fault that could occur in some cases of using `variable integer` types.
 * Fixed segmentation fault that occurred in constructions `optional(T1, T2) x = null;` and some another cases. 

### 0.68.0 (2023-04-19)

Breaking changes:
 * `require()` and `revert()` take `uint16` exception code instead of `uint256`.
 * `tvm.rawConfigParam(...)` returns `optional(TvmCell)` instead of `(TvmCell, bool)`.
 * Exit status of `solc`/`sold` is equal to `0` and it prints `Compiler run successful, no output requested.` if the source contain no deployable contracts.

Compiler features:
 * Supported [free functions](https://docs.soliditylang.org/en/latest/contracts.html#functions).
 * Supported `<TvmSlice>.loadZeroes()`, `<TvmSlice>.loadOnes()` and `<TvmSlice>.loadSame()`.
 * Supported `<TvmBuilder>.storeSame()`.

Bugfixes:
 * Fixed a problem with lvalue expressions. The problem caused type check error.
 * Fixed the bug that caused an exception in decoding the parameters of public function.
 * Fixed compilation fail if you use `tvm.functionID(ContractName)` or `externalMsg`/`internalMsg`.
 * Fixed bug when AST (`solc --ast-compact-json`) was generated with ignoring options in return statements (`return {value: ..., flag: ..., bounce: ...}(...);`).
 * Fixed compilation fail when you have 2 files with the same names in your project.

sold:
 * Supported `--userdoc`/`--devdoc` options.
 * `--ast-compact-json` option outputs to stdout now.

### 0.67.0 (2023-03-16)

Update compiler frontend (from original version 0.6.3 to 0.8.17):
  * Breaking changes:
    * `block.timestamp` returns opcode `NOW` instead of opcode `BLOCKLT`. `block.logicaltime` returns opcode `BLOCKLT`.
    * Type `byte` is deleted. Use `bytes1`.
    * And another breaking changes from original compiler.
  * Files are imported according [this](https://docs.soliditylang.org/en/v0.8.17/path-resolution.html). Use cmd options `--base-path`, `--include-path` and another options.
  * Downloading files in import section from the Internet is forbidden. Use [remapping](https://docs.soliditylang.org/en/v0.8.17/path-resolution.html#import-remapping).
  * Update NatSpec, code analyzer, etc.

### 0.66.0 (2022-10-25)

Bugfixes:
 * Fixed problem with getting inherited function `afterSignatureCheck`.
 * Support overloading for function `onCodeUpgrade`.

Breaking changes:
 * Function `onCodeUpgrade` had function id = 2. Now, it has another id, but you can set `functionID(2)` in new
contracts  to upgrade old ones.

### 0.65.0 (2022-10-08)

Compiler features:
 * Supported `delete` and binary operators for `varUintN/varIntN` types.
 * Supported function references for library function, e.g. `function(int, int) internal returns (int) op = Math.add;`
 * Supported `gosh` functions: `applyBinPatch`/`applyBinPatchQ`/`applyZipBinPatch`/`applyZipBinPatchQ`.
 * Supported conversion `uint32` to private function, e.g. `function(uint, uint) internal pure returns (uint) fun = uint32(123456)`.
 * Support command line option `--private-function-ids` for printing private function ids.
 * Support `pragma upgrade func/oldsol;` to upgrade old contracts.

### 0.64.0 (2022-08-18)

Fixed build [sold](https://github.com/everx-labs/TVM-Solidity-Compiler/tree/master/sold) for Windows and macOS.

Compiler features:
 * Supported [ABI v2.3](https://github.com/everx-labs/ever-abi/blob/master/docs/ABI_2.3_spec.md).
 * Supported try-catch (experimental feature).
 * Supported type `variant`.

Typo:
 * Rename function `storeZeros` -> `storeZeroes`.

### 0.63.0 (2022-07-27)

Compiler features:
 * Supported operator `!=` for `bytes`.
 * Added `valueToGas`/`gasToValue` without `wid`.
 * Supported `tvm.buyGas()`.

Bugfixes:
 * Fixed compilation issue with inline arrays in inline arrays.
 * Fixed compilation failure caused by such expressions: `<struct>.libFunc`.
 * Fixed compilation issue with comparison `bytes` and string literals.
 * Fixed problem with wrong encoding/decoding structures with optional types in arrays.

### 0.62.0 (2022-07-07)

Compiler features:
 * Supported `<string>.toUpperCase()` and `<string>.toLowerCase()`.
 * Supported function overloading for private/internal functions.

Bugfixes:
 * Fixed mutability for public functions: it didn't respect function modifiers.
 * Fixed compilation issue with inline arrays, e.g. `byte[] barr = [byte('a'), 'A'];`.

Gas optimizations:
* Assorted stack optimizations.

### 0.61.2 (2022-06-15)

Bugfixes:
 * Optimizer: fix bug that caused generating bad code.

### 0.61.1 (2022-06-10)

Added `sold` sources.
Fix problem of using `mapping(string => ValueType) map` in loop `for ((uint k, ValueType v) : map)`.
Use `bytes` instead of `TvmCell` in `gosh` functions.

### 0.61.0 (2022-06-07)

Compiler features:
 * Supported experimental functions `gosh.diff`, `gosh.applyPatch`, `gosh.applyPatchQ`,
`gosh.zip`, `gosh.unzip`, `gosh.zipDiff`, `gosh.applyZipPatch` and `gosh.applyZipPatchQ`.
 * Supported experimental `tx.storageFee`.

Gas optimizations:
 * Use opcode `MYCODE` for Solidity function `tvm.code()`.
 * Assorted stack optimizations.

Solidity compilation driver:
 * Added new sold tool which combines Solidity compiler with linker library
into a standalone compiler driver able to produce tvc out of sol in one pass.

### 0.60.0 (2022-05-18)

Compiler features:
 * Supported `pragma copyleft <type>, <wallet_address>;`.
 * Supported explicit conversion from `bytesN` to `bytes`.
 * Supported `<TvmSlice>.decode` for `enum` type.
 * Supported `<TvmSlice>.decodeQ`.
 * Supported `<map>.keys()` and `<map>.values()`.

Bugfixes:
 * Fixed problem with fetching `varInt` from mappings that use `varInt` as value.
 * Fixed problem with control flow in  `do{ ...} while(...)` construction.

Breaking changes:
 * `value`, `bounce` and `flag` options in return statement of `responsible` function must be
explicitly defined.

Gas optimizations:
 * Peephole optimizations: use `PUSHPOW2`, `PUSHPOW2DEC`, `PUSHNEGPOW2` opcodes instead of
`PUSHINT` in same cases.

### 0.59.0 (2022-04-12)

Compiler features:
 * Supported `<TvmSlice>.decodeStateVars(...)`.
 * Added `tvm.initCodeHash`.

Bugfixes:
 * Fixed `rawReserve()` mutability to pure.
 * Fixed issue with explicit conversion in return statement.

Delete warnings about options `callbackId`, `onErrorId`.
Use `pragma ever-solidity` instead of `pragma ton-solidity`.

### 0.58.2 (2022-03-10)

Bugfixes:
 * Type checker: fix wrong conversion from `byte1[]` to `bytes`.

### 0.58.1 (2022-03-01)

Bugfixes:
 * Optimizer: fix bug that caused generating bad code.

### 0.58.0 (2022-02-28)

Bugfixes:
 * Peephole optimizer: fix bug that caused infinitive loops.
 * Type checker: fix bug that caused compilation fail if you define a recursive struct.

### 0.57.3 (2022-02-25)

Bugfixes:
 * Code size optimizer: fix bug that caused compilation error.
 * Fix bug that caused compilation error if some arguments of `store[Unsigned|signed]` were not
constants.

### 0.57.2 (2022-02-22)

Assorted peephole stack optimizations.

### 0.57.1 (2022-02-04)

Bugfixes:
 * Code optimizer: fix bug that caused swapping some opcodes.

### 0.57.0 (2022-02-01)

Compiler features:
 * Added `--include-path` option to support import from different folders.
 * Supported `msg.hasStateInit`.

Bugfix:
 * Fixed `<string>.substr()`, `bytes[x:y]` that threw an exception if the length is equal to `127`,
`2*127`, `3*127`..., `startIndex` is equal to `string`/`bytes` length and length of substring is
equal to zero.

### 0.56.0 (2022-01-24)

Code size optimization.
Assorted peephole stack optimizations.

Bugfix:
 * Fixed issue with using recursion in libraries that causes tvm-linker fault.

### 0.55.0 (2022-01-18)

Compiler features:
 * Supported `abi.encode()`/`abi.decode()`.
 * Supported keyword `emptyMap`.

Breaking changes:
 * Supported `mapping`'s in constructor of structures.

Bugfix:
 * Fixed issue with optional type that caused compilation fail.
 * Fixed issue with implicit conversion of optional types.
 * Fixed issue with optimizer.

### 0.54.0 (2021-12-30)

Bugfixes:
 * Fix issue when it was impossible to import 2 files from each other.

Breaking changes:
 * Output AST JSON as pure json without `====` separators or something else.
 * Deleted option "abiVer" from `tvm.buildExtMsg()` and sending external inbound message for debots.

### 0.53.0 (2021-12-09)

Compiler features:
 * Supported `varInt`/`varInt16`/`varInt32` and `varUint`/`varUint16`/`varUint32` types.
 * Supported `ever` units.

Gas optimizations:
 * Assorted stack optimizations.

### 0.52.0 (2021-11-15)

Compiler features:
 * Supported `continue`/`break`/`return` inside of `repeat(n) { /*...*/ }` block.
 * Supported `uintX` in steps of `1`: `uint1`, `uint2`, `uint3` ... `uint256`.
 * Supported `intX` in steps of `1`: `int1`, `int2`, `int3` ... `int256`.
 * Supported `tvm.setGasLimit(uint)`.
 * Supported constant arrays, e.g. `uint[] public constant fib = [uint(2), 3, 5, 8, 12, 20, 32];`.

Breaking changes:
 * Changed `stoi()` return type to `optional(int)`.
 * Output AST JSON to the file instead of standard output.

Gas optimizations:
 * Optimized gas consumption of `view` and `pure` functions.
 * Assorted stack optimizations.

### 0.51.0 (2021-10-15)

Compiler features:
 * Supported function identifier instead of uint32 for `callbackId` or `onErrorId` in expressions
`tvm.buildExtMsg(...)` and `ContractName.funcName().extMsg`.
 * Supported ABI 2.2.
 * Checked if `stateInit` is valid before deploying.
 * Supported tvm.buildDataInit().

Breaking changes:
 * Deleted `tvm.buildEmptyData()`. Use `tvm.buildDataInit()`.

Gas optimizations:
 * Assorted stack optimizations.

### 0.50.0 (2021-09-21)

Compiler features:
 * Added the option `stateInit` for `tvm.buildIntMsg` and `\<address\>.transfer()`.
 * Added function `tvm.stateInitHash()` to calculate stateInit hash based on code and data cells.
 * Added keyword `null`.
 * Supported concatenation `string` and `bytesN` types.

Breaking changes:
 * Deleted `\<TvmSlice\>.bitsAndRefs()`. Please, use `\<TvmSlice\>.size()`.
 * Renamed `\<TvmBuilder\>.bitsAndRefs()` -> `\<TvmSlice\>.size()`.
 * Deleted `tvm.deploy()`. Please, use `\<address\>.transfer({stateInit: ..., ...})`.
 * Changed `TvmSlice`, `TvmCell` and `TvmBuilder` `.depth()` method return type from `uint64` to `uint16`.

Gas optimizations:
 * Assorted stack optimizations.

### 0.49.0 (2021-08-25)

Compiler features:
 * Added string search methods: `<string>.find()` and `<string>.findLast()`.
 * Added `<TvmSlice\>.bitsAndRefs()`.
 * Added `bitSize()` and `uBitSize()` for computing bit length of an integer.

Gas optimizations:
 * Assorted stack optimizations.

### 0.48.0 (2021-08-04)

Compiler features:
 * Added `tvm.setData()` and `tvm.getData()` builtins for accessing raw content of `C4` register.
 * Added support of `bytes` to `bytesN` conversion.

Support ABI 2.1:
 * Field `fields` appeared in abi file. You can parse state variable. See `ever-cli decode account --help`.
 * Supported `string` type in abi.

Bugfixes:
 * Fixed an issue when a function can be called but constructor hasn't been called yet.

Breaking changes:
 * Syntax of creating and sending of an external inbound message changed from option `{extMsg: true}`
to suffix `.extMsg`.
 * Rename `_answer_id` to `answerId` for responsible functions.
 * Removed syntax `.value()` and `.flag()` for remote call customizing.

### 0.47.0 (2021-06-28)

Compiler features:
 * Added support for byte arrays slices ("bytes" type).
 * Added serialization to builder and deserialization from slice for "enum" type values.
 * Support synchronous calls with `.await` suffix.

Breaking changes:
 * Throw exception with code 72 on external message invocations for the functions with modifier `internalMsg`.

Bugfixes:
 * Fixed an issue with generating source files in JSON format.
 * Fixed conversion of integer to the fixed point type.

Gas optimizations:
 * Optimize away, if unused, deployed contract address returned by expression `new Contr{...}(...)`.
 * Assorted peephole stack optimizations.

### 0.46.0 (2021-06-18)

Compiler features:
 * Support `internalMsg`/`externalMsg` specifiers for public and external functions.
 * Support `msg.isInternal`, `msg.isExternal` and `msg.isTickTock`.
 * Support `storeOnes` and `storeZeroes` for `TvmBuilder`.

Breaking changes:
 * Change behavior of function `<string>.substr()`.
 * Use `{dest: ...}` instead of `.extAddr(...)` in emit statement to set destination address.

Gas optimizations.

### 0.45.0 (2021-05-30)

Compiler features:
 * Compiler version is added in the contract's code.
 * Decoding struct type from `TvmSlice`.
 * Added new container type `vector`.

Bugfixes:
 * Fixed segmentation fault when using format function
 * Fixed problem with comparisons `==` and  `!=` for `TvmCell` types.
 * Fixed bugs in TypeChecker.

Gas optimizations: encoding parameters to builders.

### 0.44.0 (2021-05-18)

Compiler features:
 * Support `<array>.empty()`, `<bytes>.empty()` and `<string>.empty()`.
 * Support self deploying of a contract (see sample #21).
 * Support `tvm.buildIntMsg()` to generate cell, that contains an internal message.
 * Support `tvm.sendrawmsg()` to send internal/external messages.
 * Support command line option `--function-ids` for printing function ids.

Bugfixes:
 * Public functions (that return public state variables) emited an external message (which is paid by
the contract). Now these functions emit external messages only on external messages.
 * Fix problem with generating inbound external messages (for debots) and responsible functions.
In message body there wasn't `_answer_id`. Now it's added.
 * TypeChecker checks `call` parameter of `tvm.buildExtMsg()`.
 * Fix problem (what caused underflow exception) with encoding/decoding of state variables.

### 0.43.0 (2021-05-12)

Compiler features:
 * Support mappings/arrays containing big structs (or another structs) as input/output parameters
of public/external functions.
 * Support `tvm.code()`, `tvm.codeSalt()` and `tvm.setCodeSalt`.

Some gas optimizations.

Changes:
 * `selfdestruct` appends 0 tons to the internal message (instead of 1000 nanotons).

### 0.42.0 (2021-04-19)

Breaking changes:
 * Calling a fallback function (instead of throwing error) in cases:
   1. Bit length of the internal input message is between 1 and 31.
   2. The message has zero bit size and not zero count of references.

Compiler features:
 * Added ability to import files via net link.

### 0.41.0 (2021-04-12)

Compiler features:
 * Support indices > 127 for `bytes`.
 * Support iteration over `bytes` using `for` loop. For example: `for (byte b : byteArray) { ... }`.

Bugfixes:
 * Fixed minor bugs in TypeChecker.
 * Fixed segmentation fault on using library functions.

Improvements:
 * '--tvm-optimize' option is now enabled by default.
 * Debot feature `signBoxHandle` is supported in `tvm.buildExtMsg()` and `extMsg` call.

### 0.40.0 (2021-04-06)

Compiler features:
 * Support `tvm.setPubkey(...);`
 * Support command line option `--debug` for generating debug info.

Some gas optimizations and bugfixes.

### 0.39.0 (2021-03-28)

Compiler Features:
 * Gas consumption optimizations
 * Support `sha256` for `string` and `bytes` types
 * Support applying twice the modifier with parameter(s) for the function.
 * Delete restriction about function id of public functions.

### 0.38.2 (2021-03-12)

Bugfixes:
 * Fixed minor bugs in TypeChecker.

### 0.38.1 (2021-03-11)

Bugfixes:
 * Fix contract execution fail on internal message if the function returns some value.

### 0.38 (2021-03-09)

Breaking changes:
 * Default value of parameter `flag` is equal to `0`. The parameter is used for `tvm.transfer` and
external function calls.
 * Introduce keyword `responsible` used to mark functions that can call a callback function.
 * Change behavior for command line option `-o [ --output-dir ] path/to/dir` and introduce option
`-f [ --file ] prefixName`.

Bugfixes:
 * Fixed minor bugs in TypeChecker that cause fails of the compiler.

Use another opcodes for working with `mapping(KeyType => TvmSlice) map;` So if you used it then be
sure that sum of bit length of `KeyType` and bit length of `TvmSlice` is less than 1023 bit.

Documentation:
 * Write about parameter `flag` used for `tvm.transfer` and external function calls. Add sample of
`flag` usage.

### 0.37 (2021-02-24)

Compiler Features:
 * Support fixed point numbers (`fixed` / `ufixed`).
 * Support functions to convert gas to tons (`gasToValue()`) and vice versa (`valueToGas()`).
 * Support `string` methods `byteLength()` and `substr()` to work with long strings (which are stored in more than one cell).
 * Add methods `skip()`, `compare()`, `hasNBits()`, `hasNRefs()` and `hasNBitsAndRefs()` for TvmSlice type.
 * Support proper `string` type comparison which works right with long strings.
 * Removed deprecated function `hexstring()` and explicit int to string conversion.

Bugfixes:
 * Fixed a bug with slices comparison.

### 0.36 (2021-02-04)

Assorted features requested by DeBot support:
 * `format` function now can create long strings (which does not fit one cell), can take string arguments and format integers width and fill settings.
 * Some additional parameters were added for `tvm.buildExtMsg()` and `extMsg` call to support DeBot external function calls and deploy.

### 0.35 (2021-02-01)

Compiler Features:
 * Support callback functions used for intercontract communication.
 * Added **string** type method `append` that allows to concatenate long strings.
 * Support function `sha256(TvmSlice slice) returns (uint256)`
 * Added api function `tvm.buildExtMsg()` to generate an external inbound message to call contract function.
 * Support constant variables of address type.

Breaking changes:
 * Use `pragma ton-solidity ...;` instead of `pragma solidity ...;` to restrict compiler version

Bugfixes:
 * Fixed an issue with modifier.
 * Fixed a function type issue.

### 0.34 (2020-12-30)

Compiler Features:
 * Struct type can be used as a key of a mapping. Some restrictions are imposed on such structs.
 * Support `bounce` and `currencies` options to deploy contact via new.
 * Support range-based for loop `for ( range_declaration : range_expression ) loop_statement` where `range_expression` is array or mapping type
 * Expanded api function `tvm.buildStateInit()` to generate a `StateInit` of the contract with arbitrary parameters.

Breaking changes:
* **fallback** function is not called on plain tons transfers if there is no receive function in the contract. In this case default **receive** function is called.

Bugfixes:
 * Fixed an issue with generating `*.abi.json` file.

### 0.33 (2020-11-18)

Compiler Features:
 * Support `varInit` and `pubkey` options in `new` expression.
 * Support optional type in complex lvalue expressions. e.g. `m[11].get()[22] = 33` where `m` is `mapping(uint => optional(uint[]))`.
* Support option `splitDepth` in `new` expression and in `tvm.buildStateInit(...)`.

Breaking changes:
 * `public` state variable don't create record in *.abi.json in section `data`. Use keyword `static` for that goal. For `public` state variable, getter function is automatically generated. That function can be called locally only.
* Change function interface for `tvm.deploy`

Bugfixes:
 * Code Generator: Fixed a runtime fail if `receive` or `fallback`
functions are declared as `pure` or `view`.
 * Type checker: Compiler failed during parsing wrong parameter names.

### 0.32 (2020-11-03)

Compiler Features:
 * Support some math function: `math.divc()` and `math.divr()`.
 * Support `tvm.exit()` and `tvm.exit1()`.
 * Support `bytes.toSlice()`.
 * Support `tvm.functionId` and `tvm.decodeFunctionParams` for contract constructor.
 * Support `mapping.at`.
 * Support api for *SmartContractInfo*: `tx.timestamp`, `block.timestamp`.
 * Support api for getting info about size of DAG: `cell.dataSize`, `slice.dataSize` and another.
 * Support api functions for converting integer or address to a string: `format()`, `string(int)` and `hexstring`.
 * Support api for pseudo-random number generator: `rnd.next()`,
`rnd.shuffle()`  and another.
 * Support function library calls via library name and object.
 * Support pragma to specify default message value: `pragma msgValue`.
 * Support api function for converting string into an integer `stoi()`.
 * Support control structure `repeat` which repeats block of code arbitrary amount of times.
 * Changed mapping api function `delMin` to return optional.

Some little gas optimizations.

### 0.31 (2020-09-16)

Breaking changes:
 * The `now` returns `uint32` value, not `uint256`
 * The `msg.value` returns `uint128` value, not `uint256`
 * Change round mode for `math.muldiv()`

Compiler Features:
 * Optimize runtime code (delete duplicate code and useless checks)
 * Add new math operations: `math.muldivr()` and `math.muldivc()`
 * Support setting workchain id for deploying contracts via `new`
 * Support `tvm.buildEmptyData(publicKey)` used for deploying contracts

Bugfixes:
 * fixed an issue with overridden functionID
 * fixed an issue with inline function

### 0.30 (2020-08-21)

APIs for common TVM-specific functionality:
 * Supported tick and tock transactions. See onTickTock function.
Bug fixes.


### 0.29 (2020-08-19)

APIs for common TVM-specific functionality:
 * Supported optional type
 * Added methods for optional type: set, hasValue, get

Changed APIs to handle mappings: fetch, min, max, prev, next and ect.


### 0.20 (2020-03-30)

APIs for common TVM-specific functionality:

 * Migrated frontend to 0.6.3
 * Native types support
   * TvmCell: toSlice()
   * TvmSlice: decode(), size(), loadRef(), loadRefAsSlice()
 * Native TVM operations as tvm member functions
   * Custom message builders: sendMsg(), sendRawMsg()
   * Account state access: setCode(), setCurrentCode(), commit(), resetStorage()
   * Misc: cdatasize(), transLT(), configParam()

 * Multiple stability fixes
 * Gas consumption optimizations

### 0.18 (2020-01-30)

APIs for common TVM-specific functionality:
 * Address constructors and accessors
 * Extended address format that includes workchain ID
   * Members for workchain ID and the actual address part
   * isZero() and unpack() functions
 * External addresses support

New APIs to handle mappings:
 * next(), exists(), min(), empty(), fetch(), delMin()

Optimizations:
 * Smarter parsing of inbound messages to build SmartContractInfo
 * Reduced gas consumption when working with smart-contract persistent memory

Enum support
