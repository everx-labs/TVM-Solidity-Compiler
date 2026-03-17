<meta name="title" content="TVM-Solidity-Compiler">
<meta name="description" content="Solidity compiler for TVM">
<meta name='keywords' content='compiler, smart-contracts, blockchain, solidity, tvm, ton, tycho'>

# The TVM Solidity compiler

Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TVM compatible blockchains: [Tycho](https://github.com/broxus/tycho) and [TON](https://github.com/ton-blockchain/ton).

## TVM Solidity API reference

[API documentation is here](https://github.com/broxus/TVM-Solidity-Compiler/blob/master/API.md)

The compiler supports two versions of VM: [Tycho](https://github.com/broxus/tycho) and [TON](https://github.com/ton-blockchain/ton).
Use `sold --tvm-version tycho ...` or `sold --tvm-version ton ...` to select the desired vm.

## Build and Install

### Sold driver

We recommend using `sold` to compile smart-contracts. Documentation is available at [README.md](https://github.com/broxus/TVM-Solidity-Compiler/blob/master/sold/README.md).

### Building compiler

Original Instructions about how to build and install the Solidity compiler can be found in the [Solidity documentation](https://solidity.readthedocs.io/en/latest/installing-solidity.html#building-from-source).

#### Ubuntu Linux

```shell
git clone --recurse-submodules https://github.com/broxus/TVM-Solidity-Compiler
cd TVM-Solidity-Compiler
sh ./compiler/scripts/install_deps.sh
mkdir build
cd build
cmake ../compiler/ -DCMAKE_BUILD_TYPE=Release
cmake --build . --parallel
```

#### Windows 10

Install Visual Studio Build Tools 2019, Git bash, cmake.
Run Developer PowerShell for VS 2019

```shell
git clone --recurse-submodules https://github.com/broxus/TVM-Solidity-Compiler
cd TVM-Solidity-Compiler
compiler\scripts\install_deps.ps1
mkdir build
cd build
cmake -DBOOST_ROOT="..\compiler\deps\boost\" -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded ..\compiler
cmake --build . --config Release -- /m
```

## Links

 * [Code samples for TON](https://github.com/broxus/TVM-Solidity-Samples) in TVM Solidity
 * [Assembler and disassembler](https://github.com/broxus/tsol-asm)
 * [Code samples](https://github.com/everx-labs/samples/tree/master/solidity) in TVM Solidity
 * Example of usage `ever-cli` for working (deploying, calling etc.) with TVM compatible blockchains can be found there: [Write smart contract in Solidity](https://docs.ton.dev/86757ecb2/p/950f8a-write-smart-contract-in-solidity)
 * [Changelog](./Changelog.md)

## License
[GNU GENERAL PUBLIC LICENSE Version 3](./LICENSE)
