# The TVM Solidity Compiler Driver

## Build and Install

### Ubuntu Linux / MacOS

```shell
sh ./compiler/scripts/install_deps.sh
cd ./sold
cargo build --release
```

### FreeBSD

```shell
pkg install boost-libs cmake
cargo build --release
```

### Windows 11

Install Visual Studio Build Tools 2019, Git bash, cmake.
Run Developer PowerShell for VS 2019

```shell
compiler\scripts\install_deps.ps1
cd sold
cargo build --release
```

## Usage

Type `sold --help` for details.
