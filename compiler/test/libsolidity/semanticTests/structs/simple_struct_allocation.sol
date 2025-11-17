contract C {
  struct S {
    uint a;
  }

  function f() external externalMsg returns (uint) {
    S memory s = S(1);
    return s.a;
  }
}
// ----
// f() -> 1
