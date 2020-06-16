# Sol2TVM Compiler

Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TON blockchain. Please refer to upstream README.md for information on the language itself.

## Build and Install

Original Instructions about how to build and install the Solidity compiler can be found in the [Solidity documentation](https://solidity.readthedocs.io/en/latest/installing-solidity.html#building-from-source).

### Ubuntu Linux

```shell
git clone git@github.com:tonlabs/TON-Solidity-Compiler.git
cd TON-Solidity-Compiler/compiler
sh ./scripts/install_deps.sh
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . -- -j8
```

To facilitate work with other TON tools add path to stdlib_sol.tvm into environment variables:

```shell
sh ./pub/compiler/scripts/install_lib_variable.sh
```

### Windows 10

Install Visual Studio, Git bash, cmake.
Run Visual Studio Developer Command Prompt

```shell
git clone https://github.com/tonlabs/TON-Solidity-Compiler
cd TON-Solidity-Compiler\compiler
cmake -P scripts\install_deps.cmake
mkdir build
cd build
cmake ..
cmake --build . --config Release -j 8
```

To facilitate work with other TON tools add path to stdlib_sol.tvm into environment variable TVM_LINKER_LIB_PATH.

## Usage

[Described in the samples repository](https://github.com/tonlabs/samples/tree/master/solidity)

TVM linker repository: https://github.com/tonlabs/TVM-linker

All other relevant binaries, if needed, can be found inside TON Labs Node SE distribution at https://ton.dev/.

## Recent changes

https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/compiler/Changelog_TON.md
