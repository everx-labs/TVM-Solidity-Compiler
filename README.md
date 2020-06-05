# Sol2TVM Compiler

Port of the Solidity smart-contract [compiler](https://github.com/ethereum/solidity) generating TVM bytecode for TON blockchain. Please refer to the upstream README.md for information on the language itself.

### Ubuntu Linux

```shell
git clone git@github.com:tonlabs/TON-Solidity-Compiler.git
cd TON-Solidity-Compiler/compiler
sh ./scripts/install_deps.sh
sh ./scripts/install_lib_variable.sh
mkdir build
cd build
cmake .. -DUSE_CVC4=OFF -DUSE_Z3=OFF -DTESTS=OFF -DCMAKE_BUILD_TYPE=Debug
make -j8
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
cmake .. -DUSE_CVC4=OFF -DUSE_Z3=OFF -DTESTS=OFF -DCMAKE_BUILD_TYPE=Debug
cmake --build . --config Release -j 8
```

To facilitate work with other TON tools add path to stdlib_sol.tvm into environment variable TVM_LINKER_LIB_PATH.

## Usage

[Step-by-step guide](https://docs.ton.dev/86757ecb2/p/950f8a-write-smart-contract-in-solidity)
[Described in the samples repository](https://github.com/tonlabs/samples/tree/master/solidity)

TVM linker repository: https://github.com/tonlabs/TVM-linker
TONOS-cli repostory: https://github.com/tonlabs/tonos-cli

## Recent changes

https://github.com/tonlabs/TON-Solidity-Compiler/blob/master/compiler/Changelog_TON.md
