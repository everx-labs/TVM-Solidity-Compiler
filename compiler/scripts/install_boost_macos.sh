#!/bin/bash

set -e

BOOST_VERSION=1.86.0
# This is the name of the directory created when extracting the tarball,
# and also the name we'll use for the installation directory under $HOME/.local/
BOOST_DIR_NAME="boost_${BOOST_VERSION//./_}" # e.g., boost_1_86_0
BOOST_ARCHIVE="${BOOST_DIR_NAME}.tar.gz"
BOOST_URL="https://archives.boost.io/release/${BOOST_VERSION}/source/${BOOST_ARCHIVE}"

# This is the final installation path that will be cached
INSTALL_PREFIX="$HOME/.local/${BOOST_DIR_NAME}"

set_github_env_vars() {
  if [ -n "$GITHUB_ENV" ]; then
    echo "BOOST_ROOT=${INSTALL_PREFIX}" >> "$GITHUB_ENV"
    echo "Exported BOOST_ROOT=${INSTALL_PREFIX} to GITHUB_ENV"

    # Prepend to CMAKE_PREFIX_PATH, preserving existing values if any
    # This reads the CMAKE_PREFIX_PATH from the current environment of the script
    if [ -n "${CMAKE_PREFIX_PATH}" ]; then
        echo "CMAKE_PREFIX_PATH=${INSTALL_PREFIX}:${CMAKE_PREFIX_PATH}" >> "$GITHUB_ENV"
    else
        echo "CMAKE_PREFIX_PATH=${INSTALL_PREFIX}" >> "$GITHUB_ENV"
    fi
    echo "Updated CMAKE_PREFIX_PATH in GITHUB_ENV (prepended ${INSTALL_PREFIX})"
  fi
}

print_local_setup_guidance() {
  echo ""
  echo "=== Boost ${BOOST_VERSION} is configured to use: ${INSTALL_PREFIX} ==="
  echo "For local use (if not in GitHub Actions), you may want to add these to your shell profile:"
  echo ""
  echo "export BOOST_ROOT=${INSTALL_PREFIX}"
  echo "export CPATH=\"${INSTALL_PREFIX}/include:\$CPATH\""
  echo "export LIBRARY_PATH=\"${INSTALL_PREFIX}/lib:\$LIBRARY_PATH\""
  echo "export LD_LIBRARY_PATH=\"${INSTALL_PREFIX}/lib:\$LD_LIBRARY_PATH\" # For Linux, DYLD_LIBRARY_PATH for macOS if needed"
  echo ""
}

# Check if Boost is already installed (e.g., from cache)
# A more robust check is to see if essential subdirectories like 'lib' and 'include' exist.
if [ -d "${INSTALL_PREFIX}/lib" ] && [ -d "${INSTALL_PREFIX}/include" ]; then
  echo "=== Boost ${BOOST_VERSION} found at ${INSTALL_PREFIX} (likely from cache) ==="
  set_github_env_vars
  print_local_setup_guidance
  exit 0
fi

echo "=== Boost ${BOOST_VERSION} not found or incomplete at ${INSTALL_PREFIX}. Proceeding with download and build. ==="

# Create and navigate to a temporary directory for downloading and building Boost source
# This keeps the source separate from the final INSTALL_PREFIX until 'b2 install'
TEMP_BOOST_SOURCE_PATH="/tmp/${BOOST_DIR_NAME}_source"
mkdir -p "${TEMP_BOOST_SOURCE_PATH}"
cd "${TEMP_BOOST_SOURCE_PATH}" # Now in /tmp/boost_1_86_0_source

echo "=== Downloading Boost ${BOOST_VERSION} to $(pwd) ==="
if [ ! -f "${BOOST_ARCHIVE}" ]; then
    # -L follows redirects, -S shows errors, -O uses remote filename
    curl -fLSO "${BOOST_URL}"
fi

# The archive extracts to a directory named $BOOST_DIR_NAME (e.g., boost_1_86_0)
# inside the current directory (TEMP_BOOST_SOURCE_PATH)
if [ ! -d "${BOOST_DIR_NAME}" ]; then
    echo "Extracting ${BOOST_ARCHIVE} in $(pwd)..."
    tar -xzf "${BOOST_ARCHIVE}"
    # Sanity check
    if [ ! -d "${BOOST_DIR_NAME}" ]; then
        echo "Error: Expected directory ${BOOST_DIR_NAME} not found in $(pwd) after extraction."
        exit 1
    fi
else
    echo "Source directory ${BOOST_DIR_NAME} already exists in $(pwd). Skipping re-extraction."
fi

cd "${BOOST_DIR_NAME}" # e.g., cd /tmp/boost_1_86_0_source/boost_1_86_0

echo "=== Bootstrapping Boost (source: $(pwd)) ==="
# Ensure INSTALL_PREFIX directory exists for the installation itself
mkdir -p "${INSTALL_PREFIX}"
# Bootstrap in the source directory, telling it where to install
./bootstrap.sh --prefix="${INSTALL_PREFIX}"

echo "=== Building and Installing Boost to ${INSTALL_PREFIX} (may take a while) ==="
NUM_CORES=$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)
echo "Using ${NUM_CORES} cores for building Boost."
# Install to the specified prefix
./b2 install -j${NUM_CORES} link=static runtime-link=static

# After successful installation, set environment variables
set_github_env_vars
print_local_setup_guidance

echo ""
echo "=== Boost ${BOOST_VERSION} successfully built and installed to: ${INSTALL_PREFIX} ==="
echo "Note: Temporary source files in ${TEMP_BOOST_SOURCE_PATH} are not automatically cleaned up by this script."
echo ""

exit 0
