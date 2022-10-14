#!/usr/bin/env bash

set -o errexit

projectDir=$(cd "$(dirname "${0}")/.." && pwd)
# shellcheck source=script/util.sh
source "${projectDir}/script/util.sh" || source ./util.sh

usage() {
  println "POSIX-compliant bash script to manage toolchain for develop project"
  println "Usage: ${0} <option>"
  println "Options:"
  println "  -h this help"
  println "  -x enable debug mode (trace per command line in scripts)"
  println "  -c check requirements for environment"
  println "  -s setup environment ENV_TYPE=${ENV_TYPE}"
}

commonSetup() {
  info "setup common"
}

debianSetup() {
  info "setup for platform debian"
  sudo apt-get update
  info "Installing libs for build project"
  sudo apt-get install -qq -y \
      build-essential \
      cmake \
      g++ \
      gcc \
      libboost-all-dev \
      unzip \
      curl \
      libclang-dev
  commonSetup
}

macosSetup() {
  info "setup for platform macos"
  checkCommand brew "Requires a Homebrew install, see https://brew.sh"
  brew update
  brew install boost
  brew install cmake
  brew install curl
  if [ "$CI" = true ]; then
      brew upgrade cmake
  fi
  commonSetup
}

windowsSetup() {
  info "setup for platform windows"
  tryCommand make || choco install -y make
  tryCommand curl || choco install -y curl
  tryCommand cmake || choco install -y cmake
  cmake -P "${projectDir}/compiler/scripts/install_deps.cmake"
  commonSetup
}

checkRequirements() {
  tryCommand git && git --version
  tryCommand cargo && cargo --version
  tryCommand make && println "make $(make --version | grep Make | cut -d" " -f3)"
}

checkEnvironment() {
  printENV
  tryCommand bash && println "bash ${BASH_VERSION}"
  checkRequirements
}

setupEnvironment() {
  checkRequirements

  case "$OS_FAMILY" in
  debian) debianSetup ;;
  macos) macosSetup ;;
  windows) windowsSetup ;;
  *) notReady "setup toolchain" ;;
  esac
}

main() {
  if [ "$(id -u)" == "0" ]; then fatal "Not running as root"; fi
  if [ -z "$*" ]; then usage; fi

  cmd=
  while getopts ":hxsc" flag; do
    case "${flag}" in
    x) set -o xtrace ;;
    s) cmd=setupEnvironment ;;
    c) cmd=checkEnvironment ;;
    ?) usage ;;
    esac
  done

  ${cmd}
}

main "$*"
