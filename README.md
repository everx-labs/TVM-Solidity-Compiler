# Sol2TVM Compiler

Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TON blockchain. Please refer to upstream README.md for information on the language itself.

##  TON Solidity API reference

[API documentation is here](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/API.md)

## Build and Install

Original Instructions about how to build and install the Solidity compiler can be found in the [Solidity documentation](https://solidity.readthedocs.io/en/latest/installing-solidity.html#building-from-source).

### Ubuntu Linux

```shell
git clone git@github.com:tonlabs/TON-Solidity-Compiler.git
cd TON-Solidity-Compiler
sh ./compiler/scripts/install_deps.sh
mkdir build
cd build
cmake ../compiler/ -DCMAKE_BUILD_TYPE=Release
cmake --build . -- -j8
```

Make other TON toolchain utilities aware of the language runtime library location via an environment variable: specify path to `stdlib_sol.tvm`.

```shell
sh ./compiler/scripts/install_lib_variable.sh
```

### Windows 10

Install Visual Studio, Git bash, cmake.
Run Visual Studio Developer Command Prompt

```shell
git clone https://github.com/tonlabs/TON-Solidity-Compiler
cd TON-Solidity-Compiler
cmake -P compiler\scripts\install_deps.cmake
mkdir build
cd build
cmake ..\compiler
cmake --build . --config Release -j 8
```

To facilitate work with other TON tools add path to stdlib_sol.tvm into environment variable TVM_LINKER_LIB_PATH.

## Links

Code samples in Solidity for TON can be found there: [https://github.com/tonlabs/samples/tree/master/solidity](https://github.com/tonlabs/samples/tree/master/solidity)

TVM linker repository: [https://github.com/tonlabs/TVM-linker](https://github.com/tonlabs/TVM-linker)

TON OS command line tool: [https://github.com/tonlabs/tonos-cli](https://github.com/tonlabs/tonos-cli)

Example of usage TON OS for working (deploying, calling and etc.) with TON blockchain can be found there: [Write smart contract in Solidity](https://docs.ton.dev/86757ecb2/p/950f8a-write-smart-contract-in-solidity)

Change log: [Changelog_TON.md](https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/Changelog_TON.md)
