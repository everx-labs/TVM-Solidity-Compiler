pragma tvm-solidity >=0.5.0;

library Math {
	function mul(uint a, uint b) public returns (uint) {
		return a * b;
	}
}

contract C {
	function f(int19 funId, uint a, uint b) public pure returns (uint) {
		function(uint, uint) internal pure returns (uint) fun = funId;
		return fun(a, b);
	}

	function add(uint a, uint b) internal pure returns (uint) {
		return a + b;
	}

	function sub(uint a, uint b) internal pure returns (uint) {
		return a - b;
	}
}

