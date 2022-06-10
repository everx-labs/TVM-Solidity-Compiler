# Experimental Sol2TVM Compiler Driver

This is a fork of the Solidity language [compiler](https://github.com/ethereum/solidity) targeting TVM.

## Build and Install

### Ubuntu Linux / MacOS

```shell
git clone git@github.com:tonlabs/sol2tvm.git
cd sol2tvm/pub/sold
sh ../compiler/scripts/install_deps.sh
cargo build --release
```

### Windows 10

Install Visual Studio Build Tools, Rust, Git, cmake.
Run Visual Studio Developer Command Prompt

```shell
git clone git@github.com:tonlabs/sol2tvm.git
cd sol2tvm\pub\sold
cargo build --release
```

## Usage

```shell
sold --help
```
