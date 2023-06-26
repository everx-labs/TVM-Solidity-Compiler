<meta name="title" content="TVM-Solidity-Compiler">
<meta name="description" content="Solidity compiler for TVM">
<meta name='keywords' content='compiler, smart-contracts, blockchain, solidity, tvm, everscale, everos, venom-blockchain, venom-developer-program'>

# the TVM Solidity compiler

[![GitHub](https://img.shields.io/github/license/tonlabs/TON-Solidity-Compiler?style=for-the-badge)](./LICENSE)
[![Everscale](https://custom-icon-badges.demolab.com/badge/-everscale-13173e?style=for-the-badge&logoColor=yellow&logo=everscale)](https://everscale.network/)


Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TVM compatible blockchains (Everscale, Venom, Gosh, TON). Please refer to upstream README.md for information on the language itself.

## TVM Solidity API reference

[API documentation is here](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/API.md)

## Developer program

<a href="https://github.com/venom-blockchain/developer-program">
 <img src="https://raw.githubusercontent.com/venom-blockchain/developer-program/main/vf-dev-program.png" alt="Logo" width="366.8" height="146.4">
</a>

## Build and Install

Original Instructions about how to build and install the Solidity compiler can be found in the [Solidity documentation](https://solidity.readthedocs.io/en/latest/installing-solidity.html#building-from-source).

### Sold driver

Documentation is available at [README.md](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/sold/README.md) (RECOMMENDED).

### Building compiler

#### Ubuntu Linux

```shell
git clone https://github.com/tonlabs/TON-Solidity-Compiler
cd TON-Solidity-Compiler
sh ./compiler/scripts/install_deps.sh
mkdir build
cd build
cmake ../compiler/ -DCMAKE_BUILD_TYPE=Release
cmake --build . -- -j8
```

Make other toolchain utilities aware of the language runtime library location via an environment variable: specify path to `stdlib_sol.tvm`.

```shell
sh ./compiler/scripts/install_lib_variable.sh
```

#### Windows 10

Install Visual Studio Build Tools 2019, Git bash, cmake.
Run Developer PowerShell for VS 2019

```shell
git clone https://github.com/tonlabs/TON-Solidity-Compiler
cd TON-Solidity-Compiler
compiler\scripts\install_deps.ps1
mkdir build
cd build
cmake -DBoost_DIR="..\compiler\deps\boost\lib\cmake\Boost-1.77.0" -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded ..\compiler
cmake --build . --config Release -- /m
```

To facilitate work with other toolchain utilities add path to `stdlib_sol.tvm` into environment variable `TVM_LINKER_LIB_PATH`.

## Links

 * [Code samples](https://github.com/tonlabs/samples/tree/master/solidity) in TVM Solidity
 * [TVM linker repository](https://github.com/tonlabs/TVM-linker)
 * [tonos-cli](https://github.com/tonlabs/tonos-cli) command line interface for TVM compatible blockchains
 * Example of usage `tonos-cli` for working (deploying, calling and etc.) with TVM compatible blockchains can be found there: [Write smart contract in Solidity](https://docs.ton.dev/86757ecb2/p/950f8a-write-smart-contract-in-solidity)
 * [Changelog](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/Changelog_TON.md)

## License
[GNU GENERAL PUBLIC LICENSE Version 3](./LICENSE)
