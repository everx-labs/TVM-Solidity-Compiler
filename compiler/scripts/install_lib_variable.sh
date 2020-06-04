
if [ -z "${TVM_LINKER_LIB_PATH}" ]; then
    SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
    LIBPATH=$(dirname $(dirname $SCRIPTPATH))'/lib/stdlib_sol.tvm'
    SET_LIBPATH='export TVM_LINKER_LIB_PATH='$LIBPATH
    echo $SET_LIBPATH >> ~/.bashrc
fi