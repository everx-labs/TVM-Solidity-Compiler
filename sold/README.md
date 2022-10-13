# Experimental Solidity Compiler Driver

## Build and Install

### Ubuntu Linux / MacOS

```shell
sh ./compiler/scripts/install_deps.sh
cd ./sold
cargo build --release
```

### FreeBSD

```shell
pkg install boost-libs cmake rust-bindgen
cargo build --release
```

### Windows 11

Install Visual Studio Build Tools, Rust, Git, cmake.
Also install [LLVM-14.0.6-win64.exe](https://github.com/llvm/llvm-project/releases)
Run Developer PowerShell for VS 2022.

```shell
cmake -P compiler\scripts\install_deps.cmake
cd sold
cargo build --release
```

## Usage

Type `sold --help` for details.
