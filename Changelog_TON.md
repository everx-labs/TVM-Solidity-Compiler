### 0.34 (2020-12-21)

Compiler Features:
 * Struct type can be used as a key of a mapping. Some restrictions are imposed on such structs.
 * Support `bounce` and `currencies` options to deploy contact via new.
 * Support range-based for loop `for ( range_declaration : range_expression ) loop_statement` where `range_expression` is array or mapping type
 * Expanded api function `tvm.buildStateInit()` to generate a `StateInit` of the contract with arbitrary parameters.

Breaking changes:
* **fallback** function is not called on plain tons transfers if there is no receive function in the contract. In this case default **receive** function is called.

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
