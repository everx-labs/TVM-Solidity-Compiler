pragma tvm-solidity >=0.50.0;
contract Require {
  function foo() public pure {
    require(false, 65536);
  }
}
