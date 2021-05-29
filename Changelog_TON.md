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
