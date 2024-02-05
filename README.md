# The TVM Solidity compiler

[![GitHub](https://img.shields.io/github/license/tvmlabs/tvm-solidity-compiler?style=for-the-badge)](./LICENSE)

Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TVM compatible blockchains (Everscale, Venom, Gosh, TON). Please refer to upstream README.md for information on the language itself.

## TVM Solidity API reference

[API documentation is here](https://github.com/tvmlabs/tvm-solidity-compiler/blob/master/API.md)

## Build and Install

### Sold driver

We recommend using `sold` to compile smart-contracts. Documentation is available at [README.md](https://github.com/tvmlabs/tvm-solidity-compiler/blob/main/sold/README.md).

### Building compiler

Original Instructions about how to build and install the Solidity compiler can be found in the [Solidity documentation](https://solidity.readthedocs.io/en/latest/installing-solidity.html#building-from-source).

#### Ubuntu Linux

```shell
git clone https://github.com/tvmlabs/tvm-solidity-compiler
cd tvm-solidity-compiler
sh ./compiler/scripts/install_deps.sh
mkdir build
cd build
cmake ../compiler/ -DCMAKE_BUILD_TYPE=Release
cmake --build . -- -j8
```

#### Windows 10

Install Visual Studio Build Tools 2019, Git bash, cmake.
Run Developer PowerShell for VS 2019

```shell
git clone https://github.com/tvmlabs/tvm-solidity-compiler
cd tvm-solidity-compiler
compiler\scripts\install_deps.ps1
mkdir build
cd build
cmake -DBoost_DIR="..\compiler\deps\boost\lib\cmake\Boost-1.77.0" -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded ..\compiler
cmake --build . --config Release -- /m
```

## Links

* [Ever assembler and disassembler](https://github.com/tvmlabs/tvm-assembler)
* [Code samples](https://github.com/tvmlabs/samples/tree/master/solidity) in TVM Solidity
* [tvm-cli](https://github.com/tvmlabs/tvm-cli) command line interface for TVM compatible blockchains
* Example of usage `tvm-cli` for working (deploying, calling etc.) with TVM compatible blockchains can be found there: [Write smart contract in Solidity](https://docs.ton.dev/86757ecb2/p/950f8a-write-smart-contract-in-solidity)
* [Changelog](https://github.com/tvmlabs/tvm-solidity-compiler/blob/master/Changelog_TON.md)

## License

[GNU GENERAL PUBLIC LICENSE Version 3](./LICENSE)
