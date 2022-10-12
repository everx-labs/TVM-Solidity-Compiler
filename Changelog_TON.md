### 0.65.0 (2022-10-08)

Compiler features:
 * Supported `delete` and binary operators for `varUintN/varIntN` types.
 * Supported function references for library function, e.g. `function(int, int) internal returns (int) op = Math.add;`
 * Supported `gosh` functions: `applyBinPatch`/`applyBinPatchQ`/`applyZipBinPatch`/`applyZipBinPatchQ`. 
 * Supported conversion `uint32` to private function, e.g. `function(uint, uint) internal pure returns (uint) fun = uint32(123456)`.
 * Support command line option `--private-function-ids` for printing private function ids.
 * Support `pragma upgrade func/oldsol;` to upgrade old contracts.

### 0.64.0 (2022-08-18)

Fixed build [sold](https://github.com/tonlabs/TON-Solidity-Compiler/tree/master/sold) for Windows and macOS. 

Compiler features:
 * Supported [ABI v2.3](https://github.com/tonlabs/ton-labs-abi/blob/master/docs/ABI_2.3_spec.md).
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

Breaking change:
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

Breaking change:
 * Supported `mapping`'s in constructor of structures.

Bugfix:
 * Fixed issue with optional type that caused compilation fail.
 * Fixed issue with implicit conversion of optional types.
 * Fixed issue with optimizer.

### 0.54.0 (2021-12-30)

Bugfixes:
 * Fix issue when it was impossible to import 2 files from each other.

Breaking change:
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

Breaking change:
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

Breaking change:
 * Deleted `tvm.buildEmptyData()`. Use `tvm.buildDataInit()`.

Gas optimizations:
 * Assorted stack optimizations.

### 0.50.0 (2021-09-21)

Compiler features:
 * Added the option `stateInit` for `tvm.buildIntMsg` and `\<address\>.transfer()`.
 * Added function `tvm.stateInitHash()` to calculate stateInit hash based on code and data cells.
 * Added keyword `null`.
 * Supported concatenation `string` and `bytesN` types.

Breaking change:
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
 * Field `fields` appeared in abi file. You can parse state variable. See `tonos-cli decode account --help`.
 * Supported `string` type in abi.

Bugfixes:
 * Fixed an issue when a function can be called but constructor hasn't been called yet.

Breaking change:
 * Syntax of creating and sending of an external inbound message changed from option `{extMsg: true}`
to suffix `.extMsg`.
 * Rename `_answer_id` to `answerId` for responsible functions.
 * Removed syntax `.value()` and `.flag()` for remote call customizing.

### 0.47.0 (2021-06-28)

Compiler features:
 * Added support for byte arrays slices ("bytes" type).
 * Added serialization to builder and deserialization from slice for "enum" type values.
 * Support synchronous calls with `.await` suffix.

Breaking change:
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

Breaking change:
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

Breaking change:
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
 * Support ``varInit`` and ``pubkey`` options in ``new`` expression.
 * Support optional type in complex lvalue expressions. e.g. `m[11].get()[22] = 33` where `m` is `mapping(uint => optional(uint[]))`.
* Support option `splitDepth` in ``new`` expression and in `tvm.buildStateInit(...)`.

Breaking changes:
 * `public` state variable don't create record in *.abi.json in section `data`. Use keyword `static` for that goal. For `public` state variable, getter function is automatically generated. That function can be called locally only.
* Change function interface for `tvm.deploy`

Bugfixes:
 * Code Generator: Fixed a runtime fail if `receive` or `fallback`
functions are declared as `pure` or `view`.
 * Type checker: Compiler failed during parsing wrong parameter names.

### 0.32 (2020-11-03)

Compiler Features:
 * Support some math function: ``math.divc()`` and ``math.divr()``.
 * Support ``tvm.exit()`` and ``tvm.exit1()``.
 * Support ``bytes.toSlice()``.
 * Support ``tvm.functionId`` and ``tvm.decodeFunctionParams`` for contract constructor.
 * Support ``mapping.at``.
 * Support api for *SmartContractInfo*: ``tx.timestamp``, ``block.timestamp``.
 * Support api for getting info about size of DAG: ``cell.dataSize``, ``slice.dataSize`` and another.
 * Support api functions for converting integer or address to a string: ``format()``, ``string(int)`` and ``hexstring``.
 * Support api for pseudo-random number generator: ``rnd.next()``,
``rnd.shuffle()``  and another.
 * Support function library calls via library name and object.
 * Support pragma to specify default message value: ``pragma msgValue``.
 * Support api function for converting string into an integer ``stoi()``.
 * Support control structure ``repeat`` which repeats block of code arbitrary amount of times.
 * Changed mapping api function ``delMin`` to return optional.

Some little gas optimizations.

### 0.31 (2020-09-16)

Breaking changes:
 * The ``now`` returns ``uint32`` value, not ``uint256``
 * The ``msg.value`` returns ``uint128`` value, not ``uint256``
 * Change round mode for ``math.muldiv()``

Compiler Features:
 * Optimize runtime code (delete duplicate code and useless checks)
 * Add new math operations: ``math.muldivr()`` and ``math.muldivc()``
 * Support setting workchain id for deploying contracts via ``new``
 * Support ``tvm.buildEmptyData(publicKey)`` used for deploying contracts

Bugfixes:
 * fixed an issue with overridden functionID
 * fixed an issue with inline function

### 0.30 (2020-08-21)

APIs for common TON-specific functionality:
 * Supported tick and tock transactions. See onTickTock function.
Bug fixes.


### 0.29 (2020-08-19)

APIs for common TON-specific functionality:
 * Supported optional type
 * Added methods for optional type: set, hasValue, get 

Changed APIs to handle mappings: fetch, min, max, prev, next and ect.


### 0.20 (2020-03-30)

APIs for common TON-specific functionality:

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

APIs for common TON-specific functionality:
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
