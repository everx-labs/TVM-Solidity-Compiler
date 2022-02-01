/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
 *
 * Licensed under the  terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License.
 *
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */
/**
 * @author TON Labs <connect@tonlabs.io>
 * @date 2021
 * Peephole optimizer
 */

#include <boost/format.hpp>

#include "TvmAst.hpp"
#include "TVMConstants.hpp"
#include "PeepholeOptimizer.hpp"
#include "TVMPusher.hpp"
#include "StackOpcodeSquasher.hpp"

using namespace std;
using namespace solidity::util;

namespace solidity::frontend {

struct Result {

	bool success{};
	int removeQty{};
	vector<Pointer<TvmAstNode>> commands{};

	explicit Result() : success(false) { }

	template <class ...Args>
	explicit Result(int remove,  Args... cmds) :
		success(true), removeQty(remove), commands{cmds...}
	{
	}

	explicit Result(int remove, vector<Pointer<TvmAstNode>> commands = {}) :
		success(true), removeQty(remove), commands{std::move(commands)}
	{
	}
};

class PrivatePeepholeOptimizer {
public:
	explicit PrivatePeepholeOptimizer(std::vector<Pointer<TvmAstNode>> instructions, bool _withUnpackOpaque) :
		m_instructions{std::move(instructions)},
		m_withUnpackOpaque{_withUnpackOpaque}
	{
	}
	vector<Pointer<TvmAstNode>> const &instructions() const { return m_instructions; }

	int nextCommandLine(int idx) const;
	Pointer<TvmAstNode> get(int idx) const;
	bool valid(int idx) const;
	void remove(int idx);
	void insert(int idx, const Pointer<TvmAstNode>& node);
	Result optimizeAt(int idx1) const;
	Result optimizeAt1(int idx1) const;
	Result optimizeAt2(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2) const;
	static Result optimizeAt3(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3);
	static Result optimizeAt4(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4);
	static Result optimizeAt5(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4, Pointer<TvmAstNode> const& cmd5);
	static Result optimizeAt6(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4, Pointer<TvmAstNode> const& cmd5, Pointer<TvmAstNode> const& cmd6);
	Result optimizeAtInf(int idx1) const;
	static bool hasRetOrJmp(TvmAstNode const* _node);

	void updateLinesAndIndex(int idx1, const Result& res);
	Result unsquash(bool _withUnpackOpaque, int idx1) const;
	Result squashPush(int idx1) const;
	bool optimize(const std::function<Result(int)> &f);

	static std::optional<std::pair<int, int>> isBLKDROP2(Pointer<TvmAstNode>const& node);
	static std::optional<int> isPUSH(Pointer<TvmAstNode> const& node);
	static bool isPUSHINT(Pointer<TvmAstNode> const& node);
	static bigint pushintValue(Pointer<TvmAstNode> const& node);
	static int fetchInt(Pointer<TvmAstNode> const& node);
	static bool isNIP(Pointer<TvmAstNode> const& node);
	static std::string arg(Pointer<TvmAstNode> const& node);
	static bool is(Pointer<TvmAstNode> const& node, const std::string& opcode);
	static std::optional<std::pair<int, int>> checkSimpleCommand(Pointer<TvmAstNode> const& node);
	static bool isSimpleCommand(Pointer<TvmAstNode> const& node, int take, int ret);
	static bool isAddOrSub(Pointer<TvmAstNode> const& node);
	static bool isCommutative(Pointer<TvmAstNode> const& node);
	static std::pair<int, int> getIndexes(std::string const& str);

	template<class ...Args>
	static bool isExc(Pointer<TvmAstNode> const& node, Args&&... cmd);
	static bool isRot(Pointer<TvmAstNode> const& node);
	static bool isConstAdd(Pointer<TvmAstNode> const& node);
	static int getAddNum(Pointer<TvmAstNode> const& node);
	static bool isStack(Pointer<TvmAstNode> const& node, Stack::Opcode op);
private:
	std::vector<Pointer<TvmAstNode>> m_instructions{};
	bool m_withUnpackOpaque{};
};

int PrivatePeepholeOptimizer::nextCommandLine(int idx) const {
	if (!valid(idx)) return -1;
	idx++;
	while (true) {
		if (!valid(idx))
			return -1;
		if (!isLoc(m_instructions[idx]))
			return idx;
		idx++;
	}
}

Pointer<TvmAstNode> PrivatePeepholeOptimizer::get(int idx) const {
	return valid(idx) ? m_instructions.at(idx) : nullptr;
}

bool PrivatePeepholeOptimizer::valid(int idx) const {
	return idx >= 0 && size_t(idx) < m_instructions.size();
}

void PrivatePeepholeOptimizer::remove(int idx) {
	m_instructions.erase(m_instructions.begin() + idx);
}

void PrivatePeepholeOptimizer::insert(int idx, const Pointer<TvmAstNode>& node) {
	m_instructions.insert(m_instructions.begin() + idx, node);
}

Result PrivatePeepholeOptimizer::optimizeAt(const int idx1) const {
	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	int idx4 = nextCommandLine(idx3);
	int idx5 = nextCommandLine(idx4);
	int idx6 = nextCommandLine(idx5);

	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);
	Pointer<TvmAstNode> const& cmd3 = get(idx3);
	Pointer<TvmAstNode> const& cmd4 = get(idx4);
	Pointer<TvmAstNode> const& cmd5 = get(idx5);
	Pointer<TvmAstNode> const& cmd6 = get(idx6);

	Result res;

	res = optimizeAt1(idx1);
	if (res.success) return res;

	if (!cmd2) return Result{};
	res = optimizeAt2(cmd1, cmd2);
	if (res.success) return res;

	// cmd2 has been checked already
	res = optimizeAtInf(idx1);
	if (res.success) return res;

	if (!cmd3) return Result{};
	res = optimizeAt3(cmd1, cmd2, cmd3);
	if (res.success) return res;

	if (!cmd4) return Result{};
	res = optimizeAt4(cmd1, cmd2, cmd3, cmd4);
	if (res.success) return res;

	if (!cmd5) return Result{};
	res = optimizeAt5(cmd1, cmd2, cmd3, cmd4, cmd5);
	if (res.success) return res;

	if (!cmd6) return Result{};
	res = optimizeAt6(cmd1, cmd2, cmd3, cmd4, cmd5, cmd6);
	if (res.success) return res;

	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt1(int idx1) const {
	int idx2 = nextCommandLine(idx1);
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	auto cmd1IfElse = to<TvmIfElse>(cmd1.get());
	auto cmd1GenOpcode = to<GenOpcode>(cmd1.get());
	auto cmd1Ret = to<TvmReturn>(cmd1.get());
	auto cmd1Sub = to<SubProgram>(cmd1.get());

	// delete last RET in block
	if (cmd1Ret && !cmd1Ret->withIf() && !cmd1Ret->withAlt() && idx2 == -1) {
		return Result{1};
	}
	if (cmd1GenOpcode && isIn(cmd1GenOpcode->fullOpcode(), "ADDCONST 0", "MULCONST 1")) {
		return Result{1};
	}
	if (cmd1GenOpcode && cmd1GenOpcode->fullOpcode() == "ADDCONST 1") {
		return Result{1, gen("INC")};
	}
	if (cmd1GenOpcode && cmd1GenOpcode->fullOpcode() == "ADDCONST -1") {
		return Result{1, gen("DEC")};
	}
	if (cmd1GenOpcode && cmd1GenOpcode->fullOpcode() == "MULCONST -1") {
		return Result{1, gen("NEGATE")};
	}
	// PUSHCONT {} IF/IFNOT => DROP
	if (
		cmd1IfElse && qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0 &&
		cmd1IfElse->falseBody() == nullptr && !cmd1IfElse->withJmp()
	) {
		return Result{1, makeDROP()};
	}
	// PUSHCONT {} IFJMP => IFRET
	// PUSHCONT {} IFNOTJMP => IFNOTRET
	if (cmd1IfElse && qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0 && cmd1IfElse->falseBody() == nullptr && cmd1IfElse->withJmp()) {
		if (cmd1IfElse->withNot())
			return Result{1, makeIFNOTRET()};
		return Result{1, makeIFRET()};
	}
	// PUSHCONT { THROW N } IF/IFJMP => THROWIF
	// PUSHCONT { THROW N } IFNOT/IFNOTJMP => THROWIFNOT
	if (cmd1IfElse && cmd1IfElse->falseBody() == nullptr) {
		std::vector<Pointer<TvmAstNode>> const& inst = cmd1IfElse->trueBody()->instructions();
		if (qtyWithoutLoc(inst) == 1) {
			Pointer<TvmAstNode> pos;
			for (const auto& x : inst) if (!to<Loc>(x.get())) pos = x;
			auto _throw = to<TvmException>(pos.get());
			if (_throw && _throw->opcode() == "THROW") {
				if (cmd1IfElse->withNot())
					return Result{1, makeTHROW("THROWIFNOT " + _throw->arg())};
				return Result{1, makeTHROW("THROWIF " + _throw->arg())};
			}
		}
	}

	// PUSH[REF]CONT { RETALT } IF[NOT][JMP] => IFRETALT
	if (cmd1IfElse && cmd1IfElse->falseBody() == nullptr) {
		std::vector<Pointer<TvmAstNode>> const& inst = cmd1IfElse->trueBody()->instructions();
		if (qtyWithoutLoc(inst) == 1) {
			Pointer<TvmAstNode> pos;
			for (const auto& x : inst) if (!to<Loc>(x.get())) pos = x;
			auto ret = to<TvmReturn>(pos.get());
			if (ret && !ret->withIf() && ret->withAlt()) {
				if (cmd1IfElse->withNot())
					return Result{1, makeIFNOTRET()};
				return Result{1, makeIFRETALT()};
			}
		}
	}

	// PUSH[REF]CONT { RETALT } JMP/CALLX => RETALT
	if (cmd1Sub) {
		std::vector<Pointer<TvmAstNode>> const& inst = cmd1Sub->block()->instructions();
		if (qtyWithoutLoc(inst) == 1) {
			Pointer<TvmAstNode> pos;
			for (const auto& x : inst) if (!to<Loc>(x.get())) pos = x;
			auto ret = to<TvmReturn>(pos.get());
			if (ret && !ret->withIf() && ret->withAlt()) {
				return Result{1, makeRETALT()};
			}
		}
	}

	// PUSHCONT {
	//  LDU 256
	//	ENDS
	// }
	// PUSHCONT {
	//	LDU 256
	//	ENDS
	// }
	// IFELSE
	// =>
	// DROP
	// PUSHCONT {
	//	LDU 256
	//	ENDS
	// }
	// CALLX
	if (cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& t = cmd1IfElse->trueBody()->instructions();
		std::vector<Pointer<TvmAstNode>> const& f = cmd1IfElse->falseBody()->instructions();
		if (t.size() == f.size()) {
			bool eq = true;
			int n = f.size();
			for (int i = 0; i < n; ++i) {
				eq &= *t.at(i) == *f.at(i);
			}
			if (eq) {
				auto subProg = createNode<SubProgram>(0, 0, false, cmd1IfElse->trueBody(), false);
				return Result{1, makeDROP(), subProg};
			}
		}
	}

	// PUSHCONT {
	//   ...
	//   Z
	// }
	// PUSHCONT {
	//   ...
	//   Z
	// }
	// IFELSE
	// =>
	// PUSHCONT {
	//   ...
	// }
	// PUSHCONT {
	//   ...
	// }
	// IFELSE
	// Z
	if (m_withUnpackOpaque && cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& t = cmd1IfElse->trueBody()->instructions();
		std::vector<Pointer<TvmAstNode>> const& f = cmd1IfElse->falseBody()->instructions();
		if (!t.empty() &&
			!f.empty() &&
			*t.back() == *f.back() &&
			!cmd1IfElse->withJmp() &&
			cmd1IfElse->trueBody()->type() == CodeBlock::Type::PUSHCONT &&
			cmd1IfElse->falseBody()->type() == CodeBlock::Type::PUSHCONT &&
			!hasRetOrJmp(cmd1IfElse->trueBody().get()) &&
			!hasRetOrJmp(cmd1IfElse->falseBody().get())
		) {
			auto tt = createNode<CodeBlock>(CodeBlock::Type::PUSHCONT, std::vector<Pointer<TvmAstNode>>(t.begin(), t.end() - 1));
			auto ff = createNode<CodeBlock>(CodeBlock::Type::PUSHCONT, std::vector<Pointer<TvmAstNode>>(f.begin(), f.end() - 1));
			auto ifElse2 = createNode<TvmIfElse>(cmd1IfElse->withNot(), false, tt, ff);
			return Result{1, ifElse2, t.back()};
		}
	}

	// PUSHCONT {
	//   ...
	// }
	// PUSHCONT {
	// }
	// IFELSE
	// =>
	// PUSHCONT {
	//   ...
	// }
	// IF
	if (cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& f = cmd1IfElse->falseBody()->instructions();
		if (qtyWithoutLoc(f) == 0 &&
			!cmd1IfElse->withJmp()
		) {
			auto ifElse2 = createNode<TvmIfElse>(cmd1IfElse->withNot(), false, cmd1IfElse->trueBody());
			return Result{1, ifElse2};
		}
	}

	// PUSHCONT {
	// }
	// PUSHCONT {
	//    ...
	// }
	// IFELSE
	// =>
	// PUSHCONT {
	//   ...
	// }
	// IFNOT
	if (cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& t = cmd1IfElse->trueBody()->instructions();
		if (qtyWithoutLoc(t) == 0 &&
			!cmd1IfElse->withJmp()
		) {
			auto ifElse2 = createNode<TvmIfElse>(!cmd1IfElse->withNot(), false, cmd1IfElse->falseBody());
			return Result{1, ifElse2};
		}
	}

	// PUSHCONT { TRUE }
	// PUSHCONT { ... }
	// WHILE
	// =>
	// PUSHCONT { ... }
	// AGAIN
	if (auto _while = to<While>(cmd1.get())) {
		std::vector<Pointer<TvmAstNode>> const& instr = _while->condition()->instructions();
		if (instr.size() == 1 && is(instr.at(0), "TRUE") && !_while->isInfinite()) {
			return Result{1, createNode<While>(true, _while->withBreakOrReturn(), _while->condition(), _while->body())};
		}
	}

	// PUSHCONT { here }
	// CALLX
	// =>
	// here
	if (cmd1Sub && !cmd1Sub->isJmp() && cmd1Sub->block()->type() == CodeBlock::Type::PUSHCONT) {
		bool ok = true;
		for (Pointer<TvmAstNode> const& cmd : cmd1Sub->block()->instructions()) {
			if (hasRetOrJmp(cmd.get())) {
				ok = false;
			}
		}
		if (ok) {
			return Result{1, cmd1Sub->block()->instructions()};
		}
	}

	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt2(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2) const {
	auto cmd1GenOp = to<GenOpcode>(cmd1.get());
	auto cmd1Glob = to<Glob>(cmd1.get());

	auto cmd2Cond = to<TvmCondition>(cmd2.get());
	auto cmd2Exc = to<TvmException>(cmd2.get());
	auto cmd2GenOpcode = to<GenOpcode>(cmd2.get());
	auto cmd2Glob = to<Glob>(cmd2.get());
	auto cmd2IfElse = to<TvmIfElse>(cmd2.get());
	auto cmd1Ret = to<TvmReturn>(cmd1.get());

	auto _isBLKDROP1 = isBLKDROP2(cmd1);
	auto _isBLKDROP2 = isBLKDROP2(cmd2);
	auto isBLKPUSH1 = isBLKPUSH(cmd1);
	auto isPUSH1 = isPUSH(cmd1);

	std::map<bigint, int> power2;
	for (int p2 = 2, p = 1; p2 <= 256; p2 *= 2, ++p) {
		power2[p2] = p;
	}

	if (isSWAP(cmd1)) {
		if (is(cmd2, "SUB")) return Result{2, gen("SUBR")};
		if (is(cmd2, "SUBR")) return Result{2, gen("SUB")};
		if (isNIP(cmd2)) return Result{2, makeDROP()};
		if (isCommutative(cmd2)) return Result{1};
		if (isDrop(cmd2)) {
			int n = isDrop(cmd2).value();
			if (n == 1) {
				return Result{2, makePOP(1)};
			} else {
				return Result{2, makeDROP(n)};
			}
		}
	if (cmd2GenOpcode &&
		boost::starts_with(cmd2GenOpcode->opcode(), "ST") &&
		boost::ends_with(cmd2GenOpcode->opcode(), "R") &&
		cmd2GenOpcode->take() == 2 &&
		cmd2GenOpcode->ret() == 1
	) {
			auto opcode = cmd2GenOpcode->opcode();
			opcode = opcode.substr(0, opcode.size() - 1);
			return Result{2, gen(opcode + " " + arg(cmd2))};
		}
	}
	if (isPUSHINT(cmd1)) {
		if (arg(cmd1) == "1") {
			if (is(cmd2, "ADD")) return Result{2, gen("INC")};
			if (is(cmd2, "SUB")) return Result{2, gen("DEC")};
		}
		bigint value = pushintValue(cmd1);
		if (-128 <= value && value <= 127) {
			if (is(cmd2, "ADD")) return Result{2, gen("ADDCONST " + toString(value))};
			if (is(cmd2, "MUL")) return Result{2, gen("MULCONST " + toString(value))};
		}
		if (-128 <= -value && -value <= 127) {
			if (is(cmd2, "SUB")) return Result{2, gen("ADDCONST " + toString(-value))};
		}
	}
	if ((cmd1Ret && !cmd1Ret->withIf()) || isExc(cmd1, "THROWANY", "THROW")) {
		// delete commands after non return opcode
		return Result{2, cmd1};
	}
	if (isPUSH1 && *isPUSH1 == 0 && isSWAP(cmd2)) {
		return Result{2, cmd1};
	}
	// POP Sn
	// DROP n-1
	// =>
	// BLKDROP2 n, 1
	if (isPOP(cmd1) && isDrop(cmd2) && isPOP(cmd1).value() == isDrop(cmd2).value() + 1) {
		int n = isPOP(cmd1).value();
		if (1 <= n && n <= 15) {
			return Result{2, makeBLKDROP2(n, 1)};
		}
	}
	// SWAP
	// POP S2
	//
	// BLKDROP2 1, 2
	if (isSWAP(cmd1) && isPOP(cmd2) && isPOP(cmd2).value() == 2) {
		return Result{2, makeBLKDROP2(1, 2)};
	}
	if ((isPUSH(cmd1) || isPureGen01(*cmd1)) && isDrop(cmd2)) {
		int qty = isDrop(cmd2).value();
		if (qty == 1) {
			return Result{2};
		} else {
			return Result{2, makeDROP(qty - 1)};
		}
	}

	// BLKPUSH N, index / DROP
	// BLKDROP N
	if (isBLKPUSH1 && isDrop(cmd2)) {
		auto [qty, index] = isBLKPUSH1.value();
		int diff = qty - isDrop(cmd2).value();
		if (diff == 0)
			return Result{2};
		if (diff < 0)
			return Result{2, makeDROP(-diff)};
		else
			return Result{2, makeBLKPUSH(diff, index)};
	}

	// PUSH S[n-1]
	// BLKDROP2 N, 1 / NIP
	// =>
	// DROP N-1
	if (isPUSH1 && _isBLKDROP2) {
		int index = *isPUSH1;
		auto [drop, rest] = _isBLKDROP2.value();

		if (drop == index + 1 && rest == 1) {
			if (drop == 1) {
				return Result{2};
			} else {
				return Result{2, makeDROP(drop - 1)};
			}
		}
	}
	// s01
	// BLKDROP2 N, M
	// =>
	// BLKDROP2 N, M-1
	// s01
	if (isPureGen01(*cmd1) && _isBLKDROP2) {
		auto [down, up] = _isBLKDROP2.value();
		return Result{2, makeBLKDROP2(down, up - 1), cmd1};
	}
	// BLKPUSH
	// BLKDROP2
	// =>
	// ???
	if (isBLKPUSH1 && _isBLKDROP2) {
		auto [qty, index] = isBLKPUSH1.value();
		auto [drop, rest] = _isBLKDROP2.value();

		// BLKPUSH  qty, qty-1
		// BLKDROP2 qty+X, qty
		// =>
		// BLKDROP2 X, qty
		if (qty == index + 1 && rest == qty) {
			if (drop == qty) {
				return Result{2};
			} else if (drop > qty) {
				return Result{2, makeBLKDROP2(drop - qty, qty)};
			}
		}

		// BLKPUSH   qty, index
		// BLKDROP2 drop, qty
		// =>
		// DROP X, qty
		if (qty == rest) {
			int lastIndex = index - qty + 1; // include
			if (lastIndex >= 0 && lastIndex + qty == drop) {
				// a b c d e f X Y |
				// X Y a b c d e f X Y | BLKPUSH
				// X Y | BLKDROP2
				int newDrop = lastIndex;
				return Result{2, makeDROP(newDrop)};
			}
		}
	}
	// DUP
	// BLKDROP2 n, 1
	// =>
	// BLKDROP2 n-1, 1
	// Same as prev
	if (isPUSH1 && *isPUSH1 == 0 &&
		_isBLKDROP2 && _isBLKDROP2.value().second == 1
	) {
		int n = _isBLKDROP2.value().first;
		if (n == 1) {
			return Result{2};
		} else {
			return Result{2, makeBLKDROP2(n - 1, 1)};
		}
	}

	// NIP
	// DROP n
	// =>
	// DROP n+1
	if (isNIP(cmd1) && isDrop(cmd2)) {
		return Result{2, makeDROP(isDrop(cmd2).value() + 1)};
	}
	// NOT THROWIFNOT/THROWIF N => THROWIF/THROWIFNOT N
	// NOT PUSHCONT {} IF/IFNOT => PUSHCONT {} IFNOT/IF
	if (is(cmd1, "NOT")) {
		if (isExc(cmd2, "THROWIF"))
			return Result{2, makeTHROW("THROWIFNOT " + cmd2Exc->arg())};
		if (isExc(cmd2, "THROWIFNOT"))
			return Result{2, makeTHROW("THROWIF " + cmd2Exc->arg())};
		if (cmd2IfElse)
			return Result{2, makeRevert(*cmd2IfElse)};
		if (cmd2Cond)
			return Result{2, makeRevertCond(*cmd2Cond)};
	}
	// EQINT 0 THROWIFNOT/THROWIF N => THROWIF/THROWIFNOT N
	// EQINT 0 PUSHCONT {} IF/IFNOT => PUSHCONT {} IFNOT/IF
	if (is(cmd1, "EQINT") && cmd1GenOp->arg() == "0") {
		if (isExc(cmd2, "THROWIF"))
			return Result{2, makeTHROW("THROWIFNOT " + cmd2Exc->arg())};
		if (isExc(cmd2, "THROWIFNOT"))
			return Result{2, makeTHROW("THROWIF " + cmd2Exc->arg())};
		if (cmd2IfElse)
			return Result{2, makeRevert(*cmd2IfElse)};
		if (cmd2Cond)
			return Result{2, makeRevertCond(*cmd2Cond)};
	}
	// NEQINT 0, THROWIF/THROWIFNOT N => THROWIF/THROWIFNOT N
	// NEQINT 0, PUSHCONT {} IF => PUSHCONT {} IF
	if (is(cmd1, "NEQINT") && cmd1GenOp->arg() == "0") {
		if (isExc(cmd2, "THROWIF"))
			return Result{2, makeTHROW("THROWIF " + cmd2Exc->arg())};
		if (isExc(cmd2, "THROWIFNOT"))
			return Result{2, makeTHROW("THROWIFNOT " + cmd2Exc->arg())};
		if (cmd2IfElse || cmd2Cond)
			return Result{2, cmd2};
	}

	// TRUE
	// PUSHCONT {} / PUSHREF {}
	// ...
	// IF / IFJMP / IFELSE / IFELSE_WITH_JMP
	if (is(cmd1, "TRUE") && cmd2IfElse && !cmd2IfElse->withNot()) {
		auto subProg = createNode<SubProgram>(0, 0, cmd2IfElse->withJmp(), cmd2IfElse->trueBody(), false);
		return Result{2, subProg};
	}

	// BLKSWAP  down, up
	// BLKDROP2 drop, rest where drop==up and rest==down
	// ...
	// DROP up
	if (isBLKSWAP(cmd1) && isBLKDROP2(cmd2)) {
		auto [down, up] = isBLKSWAP(cmd1).value();
		auto [drop, rest] = isBLKDROP2(cmd2).value();
		if (drop==up && rest==down)
			return Result{2, makeDROP(up)};
	}

	// BLKDROP2 drop, rest
	// BLKDROP rest + some
	// ...
	// BLKDROP drop + rest + some
	if (isBLKDROP2(cmd1) && isDrop(cmd2)) {
		auto [drop, rest] = isBLKDROP2(cmd1).value();
		int n = isDrop(cmd2).value();
		int some = n - rest;
		if (some >= 0)
			return Result{2, makeDROP(drop + rest + some)};
	}

	// BLKSWAP down, up
	// DROP down
	// =>
	// BLKDROP down, up
	if (isBLKSWAP(cmd1) && isDrop(cmd2)) {
		auto [down, up] = isBLKSWAP(cmd1).value();
		int n = isDrop(cmd2).value();
		if (n == down)
			return Result{2, makeBLKDROP2(down, up)};
	}

	if (isBLKSWAP(cmd1) && isBLKSWAP(cmd2)) {
		auto [down1, top1] = isBLKSWAP(cmd1).value();
		auto [down2, top2] = isBLKSWAP(cmd2).value();
		if (down1 + top1 == down2 + top2) {
			// BLKSWAP down1, top1 where down1 + top1 == n
			// BLKSWAP     1, n-1
			// ...
			// BLKSWAP down1+1, top1-1
			if (down2 == 1) {
				if (top1 == 1) {
					return Result{2};
				} else {
					return Result{2, makeBLKSWAP(down1 + 1, top1 - 1)};
				}
			}
			// BLKSWAP down1, top1  where down1 + top1 == n
			// BLKSWAP n-1,    1
			// ...
			// BLKSWAP down1-1, top1+1
			if (top2 == 1) {
				if (down1 == 1) {
					return Result{2};
				} else {
					return Result{2, makeBLKSWAP(down1 - 1, top1 + 1)};
				}
			}
		}
	}
	if (is(cmd1, "STSLICECONST") &&
		is(cmd2, "STSLICECONST")
	) {
		std::optional<std::string> slice = StrUtils::unitSlices(arg(cmd1), arg(cmd2));
		if (slice.has_value() && StrUtils::toBitString(*slice).length() <= TvmConst::MaxSTSLICECONST)
			return Result{2, gen("STSLICECONST " + *slice)};
		return Result{2, genPushSlice(*slice), gen("STSLICER")};
	}
	if (is(cmd1, "TUPLE") &&
		is(cmd2, "UNTUPLE") &&
		fetchInt(cmd1) == fetchInt(cmd2))
	{
		return Result{2};
	}
	if (is(cmd1, "UNTUPLE") &&
		is(cmd2, "TUPLE") &&
		fetchInt(cmd1) == fetchInt(cmd2))
	{
		return Result{2};
	}
	if (cmd1Glob && cmd1Glob->opcode() == Glob::Opcode::SetOrSetVar &&
		cmd2Glob && cmd2Glob->opcode() == Glob::Opcode::GetOrGetVar &&
		cmd1Glob->index() == cmd2Glob->index()
	) {
		return Result{2, makePUSH(0), makeSetGlob(cmd1Glob->index())};
	}
	if (isConstAdd(cmd1) && isConstAdd(cmd2)) {
		int final_add = getAddNum(cmd1) + getAddNum(cmd2);
		if (-128 <= final_add && final_add <= 127)
			return Result{2, gen("ADDCONST " + std::to_string(final_add))};
	}
	if ((is(cmd1, "INDEX_NOEXCEP") || is(cmd1, "INDEX_EXCEP")) && 0 <= strToInt(arg(cmd1)) && strToInt(arg(cmd1)) <= 3 &&
		(is(cmd2, "INDEX_NOEXCEP") || is(cmd2, "INDEX_EXCEP")) && 0 <= strToInt(arg(cmd2)) && strToInt(arg(cmd2)) <= 3) {
		return Result{2, gen("INDEX2 " + arg(cmd1) + ", " + arg(cmd2))};
	}
	if (is(cmd1, "INDEX2") &&
		(is(cmd2, "INDEX_NOEXCEP") || is(cmd2, "INDEX_EXCEP")) && 0 <= strToInt(arg(cmd2)) && strToInt(arg(cmd2)) <= 3
	) {
		auto [i, j] = getIndexes(arg(cmd1));
		if (0 <= i && i <= 3 &&
			0 <= j && j <= 3) {
			return Result{2, gen("INDEX3 " + toString(i) + ", " + toString(j) + ", " + arg(cmd2))};
		}
	}
	if (
		isPUSHINT(cmd1) && 1 <= pushintValue(cmd1) && pushintValue(cmd1) <= 256 &&
		(is(cmd2, "RSHIFT") || is(cmd2, "LSHIFT")) && arg(cmd2).empty()
	) {
		return Result{2, gen(cmd2GenOpcode->opcode() + " " + arg(cmd1))};
	}
	if (isPUSHINT(cmd1) &&
		(is(cmd2, "DIV") || is(cmd2, "MUL"))) {
		bigint val = pushintValue(cmd1);
		if (power2.count(val)) {
			const std::string& newOp = is(cmd2, "DIV") ? "RSHIFT" : "LSHIFT";
			return Result{2, gen(newOp + " " + toString(power2.at(val)))};
		}
	}
	if (isPUSHINT(cmd1) &&
		is(cmd2, "MOD")) {
		bigint val = pushintValue(cmd1);
		if (power2.count(val)) {
			return Result{2, gen("MODPOW2 " + toString(power2.at(val)))};
		}
	}
	if (isPUSHINT(cmd1)) {
		bigint val = pushintValue(cmd1);
		if (-128 <= val && val < 128) {
			if (is(cmd2, "NEQ"))
				return Result{2, gen("NEQINT " + toString(val))};
			if (is(cmd2, "EQUAL"))
				return Result{2, gen("EQINT " + toString(val))};
			if (is(cmd2, "GREATER"))
				return Result{2, gen("GTINT " + toString(val))};
			if (is(cmd2, "LESS"))
				return Result{2, gen("LESSINT " + toString(val))};
		}
		if (-128 <= val - 1 && val - 1 < 128 && is(cmd2, "GEQ"))
			return Result{2, gen("GTINT " + toString(val - 1))};
		if (-128 <= val + 1 && val + 1 < 128 && is(cmd2, "LEQ"))
			return Result{2, gen("LESSINT " + toString(val + 1))};
	}
	if (_isBLKDROP1 && _isBLKDROP2) {
		auto [drop1, rest1] = _isBLKDROP1.value();
		auto [drop2, rest2] = _isBLKDROP2.value();
		// BLKDROP2 drop0, rest
		// BLKDROP2 drop1, rest
		// =>
		// BLKDROP2 drop0+drop1, rest
		if (rest1 == rest2 && drop1 + drop2 <= 15) {
			return Result{2, makeBLKDROP2(drop1 + drop2, rest1)};
		}
		// BLKDROP2 drop1, rest1
		// BLKDROP2 drop2, rest2
		// =>
		// BLKDROP2 drop1+drop2, rest1
		if (rest1 == drop2 + rest2 && rest1 >= rest2) {
			return Result{2, makeBLKDROP2(drop1 + drop2, rest2)};
		}
	}

	// BLKSWAP bottom, top
	// BLKDROP bottom
	// =>
	// BLKDROP2 bottom, top
	if (isBLKSWAP(cmd1) &&
		isDrop(cmd2)
	) {
		auto [bottom, top] = isBLKSWAP(cmd1).value();
		int n = isDrop(cmd2).value();
		if (n == bottom) {
			return Result{2, makeBLKDROP2(n, top)};
		}
	}

	if (is(cmd1, "MUL") && is(cmd2, "RSHIFT") && arg(cmd2) != " ") {
		return Result{2, gen("MULRSHIFT " + arg(cmd2))};
	}

	if (is(cmd1, "NEWC") && is(cmd2, "ENDC")) {
		return Result{2, makePUSHREF()};
	}

	if (is(cmd1, "NOT") &&
		is(cmd2, "NOT")
	) {
		return Result{2};
	}
	if ((is(cmd1, "UFITS") && is(cmd2, "UFITS")) || (is(cmd1, "FITS") && is(cmd2, "FITS"))) {
		int bitSize = std::min(fetchInt(cmd1), fetchInt(cmd2));
		return Result{2, gen(cmd1GenOp->opcode() + " " + toString(bitSize))};
	}
	if ((is(cmd1, "TRUE") || is(cmd1, "FALSE")) &&
		is(cmd2, "STIR") && fetchInt(cmd2) == 1
	) {
		if (is(cmd1, "FALSE"))
			return Result{2, gen("STZERO")};
		return Result{2, gen("STONE")};
	}
	if (
		isPUSHINT(cmd1) && pushintValue(cmd1) == 0 &&
		is(cmd2, "STUR")
	) {
		return Result{2,
			gen("PUSHINT " + arg(cmd2)),
			gen("STZEROES")};
	}
	if (
		isPUSHINT(cmd1) &&
		is(cmd2, "STUR") && fetchInt(cmd2) <= 8 // TODO DELETE
	) {
		std::string s = StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd2));
		s = StrUtils::binaryStringToSlice(s);
		return Result{2, gen("STSLICECONST x" + s)};
	}

	if (
		is(cmd1, "ABS") &&
		is(cmd2, "UFITS") && fetchInt(cmd2) == 256
	) {
		return Result{2, gen("ABS")};
	}

	if (
		isPUSHINT(cmd1) && pushintValue(cmd1) == 1 &&
		is(cmd2, "STZEROES")
	) {
		return Result{2, gen("STZERO")};
	}

	// REVERSE N, 1
	// BLKSWAP N, 1
	// =>
	// REVERSE N+1, 0
	if (isREVERSE(cmd1) && isBLKSWAP(cmd2)) {
		auto [qty, index] = isREVERSE(cmd1).value();
		auto [bottom, top] = isBLKSWAP(cmd2).value();
		if (top == 1 && index == 1 && qty == bottom)
			return Result{2, makeREVERSE(qty + 1, 0)};
	}

	// REVERSE N+1, 0
	// BLKDROP N
	// =>
	// BLKDROP2 N, 1
	if (isREVERSE(cmd1) && isDrop(cmd2)) {
		auto [qty, index] = isREVERSE(cmd1).value();
		int n = isDrop(cmd2).value();
		if (n + 1 == qty && index == 0)
			return Result{2, makeBLKDROP2(n, 1)};
	}

	// ENDC
	// STREFR
	// =>
	// STBREFR
	if (
		is(cmd1, "ENDC") &&
		is(cmd2, "STREFR")
	) {
		return Result{2, gen("STBREFR")};
	}

	// s01
	// XCHG S1, S2
	// =>
	// SWAP
	// s01
	if (isPureGen01(*cmd1) &&
		isXCHG(cmd2, 1, 2)
	) {
		return Result{2, makeBLKSWAP(1, 1), cmd1};
	}

	// DUP
	// PUSHCONT {
	//   DROP
	//   TRUE
	// }
	// IF
	// =>
	//
	if (isPUSH1 && *isPUSH1 == 0) {
		if (auto lc = to<LogCircuit>(cmd2.get())) {
			if (lc->type() == LogCircuit::Type::AND && lc->body()->instructions().size() == 2) {
				auto cmd2_0 = lc->body()->instructions().at(0);
				auto cmd2_1 = lc->body()->instructions().at(1);
				auto _true = to<GenOpcode>(cmd2_1.get());
				if (isDrop(cmd2_0) == 1 && _true && _true->opcode() == "TRUE") {
					return Result{2};
				}
			}
		}
	}

	// TRUE
	// AND
	// =>
	//
	auto _true = to<GenOpcode>(cmd1.get());
	auto _and = to<GenOpcode>(cmd2.get());
	if (_true && _true->opcode() == "TRUE" &&
		_and && _and->opcode() == "AND") {
		return Result{2};
	}

	// NULL
	// ISNULL
	// =>
	// TRUE
	if (is(cmd1, "NULL") && is(cmd2, "ISNULL")) {
		return Result{2, gen("TRUE")};
	}

	// TRUE       / FALSE
	// THROWIFNOT / THROWIF
	// =>
	//
	if ((is(cmd1, "TRUE") && isExc(cmd2, "THROWIFNOT")) || (is(cmd1, "FALSE") && isExc(cmd2, "THROWIF"))) {
		return Result{2};
	}

	// PUSHINT N
	// ISNULL
	// =>
	// FALSE
	if (is(cmd1, "PUSHINT") && is(cmd2, "ISNULL")) {
		return Result{2, gen("FALSE")};
	}

	// TRUE    / FALSE
	// THROWIF / THROWIFNOT
	// =>
	//
	if ((is(cmd1, "TRUE") && isExc(cmd2, "THROWIF")) || (is(cmd1, "FALSE") && isExc(cmd2, "THROWIFNOT"))) {
		return Result{2, makeTHROW("THROW " + cmd2Exc->arg())};
	}

	if (cmd1GenOp && cmd1GenOp->isPure() &&
		std::make_pair(cmd1GenOp->take(), cmd1GenOp->ret()) == std::make_pair(1, 1) &&
		isDrop(cmd2)
	) {
		return Result{2, cmd2};
	}

	// BLKPUSH N, 0 / DUP
	// BLKPUSH Q, 0 / DUP
	// =>
	// BLKPUSH N+Q, 0
	if (isBLKPUSH(cmd1) && isBLKPUSH(cmd2)) {
		auto [qty0, index0] = isBLKPUSH(cmd1).value();
		auto [qty1, index1] = isBLKPUSH(cmd2).value();
		if (index0 == 0 && index1 == 0 && qty0 + qty1 <= 15)
			return Result{2, makeBLKPUSH(qty0 + qty1, 0)};
	}

	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt3(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3) {
	auto isPUSH1 = isPUSH(cmd1);
	auto isPUSH2 = isPUSH(cmd2);
	auto cmd3GenOpcode = to<GenOpcode>(cmd3.get());

	if (isNIP(cmd2) && isNIP(cmd3)) {
		if (isPUSH1 && *isPUSH1 == 1) {
			return Result{3, makeDROP()};
		}
		if (isSimpleCommand(cmd1, 0, 1)) {
			return Result{3, makeDROP(2), cmd1};
		}
	}

	// PUSHINT ?
	// NEWC
	// STU y
	// TODO add bool and STI
	if (
		isPUSHINT(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STU") && fetchInt(cmd3) <= 8 // TODO DELETE
	) {
		std::string s = StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd3));
		s = StrUtils::binaryStringToSlice(s);
		return Result{3, gen("NEWC"), gen("STSLICECONST x" + s)};
	}

	// NEW
	// s01
	// ST**R
	// =>
	// s01
	// NEW
	// ST**
	if (
		is(cmd1, "NEWC") &&
		isSimpleCommand(cmd2, 0, 1) &&
		cmd3GenOpcode &&
		boost::starts_with(cmd3GenOpcode->opcode(), "ST") && boost::ends_with(cmd3GenOpcode->opcode(), "R")
	) {
		const auto& opcode = cmd3GenOpcode->opcode();
		return Result{3,
					  cmd2,
					  gen("NEWC"),
					  gen(opcode.substr(0, opcode.size() - 1) + " " + arg(cmd3))};
	}
	// DUP
	// THROWIFNOT 507
	// DROP n
	if (isPUSH1 && *isPUSH1 == 0 &&
		isExc(cmd2, "THROWIFNOT", "THROWIF") &&
		isDrop(cmd3)
	) {
		int n = isDrop(cmd3).value();
		if (n == 1) {
			return Result{3, cmd2};
		} else {
			return Result{3, cmd2, makeDROP(n - 1)};
		}
	}

	if (isPUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3) == "0")
	{
		return Result{3,
							   gen("PUSHINT " + toString(pushintValue(cmd1) + 1)),
							   gen("STZEROES")};
	}
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "STSLICER") &&
		is(cmd3, "STSLICECONST")) {
		std::optional<std::string> slice = StrUtils::unitSlices(isPlainPushSlice(cmd1)->blob(), arg(cmd3));
		if (slice.has_value()) {
			return Result{3, genPushSlice(*slice), gen("STSLICER")};
		}
	}
	if (is(cmd1, "STSLICECONST") &&
		isPlainPushSlice(cmd2) &&
		is(cmd3, "STSLICER")) {
		std::optional<std::string> slice = StrUtils::unitSlices(arg(cmd1), isPlainPushSlice(cmd2)->blob());
		if (slice.has_value()) {
			return Result{3, genPushSlice(*slice), gen("STSLICER")};
		}
	}
	if (isPUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3).length() > 1) {
		std::string::size_type intValue = static_cast<std::string::size_type>(pushintValue(cmd1));
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(
			std::string(intValue, '0'),
			StrUtils::toBitString(arg(cmd3))
		);
		if (slice.has_value()) {
			return Result{3, genPushSlice(*slice), gen("STSLICER")};
		}
	}
	if (isPUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3) == "1"
	) {
		int lenA = fetchInt(cmd1);
		if (lenA <= 256) {
			return Result{3,
						  gen("PUSHINT 1"),
						  gen("STUR " + toString(lenA + 1))
			};
		}
	}
	if (
		is(cmd1, "STSLICECONST") && arg(cmd1) == "0" &&
		isPUSHINT(cmd2) && pushintValue(cmd2) == 0 &&
		is(cmd3, "STUR")
	) {
		int bitQty = fetchInt(cmd3);
		return Result{3,
					  gen("PUSHINT 0"),
					  gen("STUR " + toString(bitQty + 1))
		};
	}
	if (
		is(cmd1, "NEWC") &&
		is(cmd2, "STSLICECONST") && arg(cmd2).length() > 1 &&
		is(cmd3, "ENDC")
	) {
		return Result{3, makePUSHREF(arg(cmd2))};
	}

	// SWAP
	// s01
	// SWAP
	// =>
	// s01
	// ROT
	if (isSWAP(cmd1) &&
		isSimpleCommand(cmd2, 0, 1) &&
		isSWAP(cmd3)
	) {
		return Result{3, cmd2, makeROT()};
	}

	// ROT
	// s01
	// SWAP
	// =>
	// s01
	// BLKSWAP 1, 3
	if (isRot(cmd1) &&
		isSimpleCommand(cmd2, 0, 1) &&
		isSWAP(cmd3)
	) {
		return Result{3, cmd2, makeBLKSWAP(1, 3)};
	}
	// PUSHINT x
	// PUSH Si (i!=0) or gen(0,1)
	// CMP
	// =>
	// PUSH S(i-1) or gen(0,1)
	// CMP2
	if (isPUSHINT(cmd1) && ((isPUSH2 && *isPUSH2 != 0) || isPureGen01(*cmd2))) {
		auto newCmd2 = isPUSH(cmd2) ? makePUSH(*isPUSH2 - 1) : cmd2;
		bigint val = pushintValue(cmd1);
		if (-128 <= val && val < 128) {
			if (is(cmd3, "NEQ"))
				return Result{3, newCmd2, gen("NEQINT " + toString(val))};
			if (is(cmd3, "EQUAL"))
				return Result{3, newCmd2, gen("EQINT " + toString(val))};
			if (is(cmd3, "GREATER"))
				return Result{3, newCmd2, gen("LESSINT " + toString(val))};
			if (is(cmd3, "LESS"))
				return Result{3, newCmd2, gen("GTINT " + toString(val))};
		}
		if (-128 <= val + 1 && val + 1 < 128 && is(cmd3, "GEQ"))
			return Result{3, newCmd2, gen("LESSINT " + toString(val + 1))};
		if (-128 <= val - 1 && val - 1 < 128 && is(cmd3, "LEQ"))
			return Result{3, newCmd2, gen("GTINT " + toString(val - 1))};
	}
	if (isPUSHINT(cmd1) &&
		isPUSHINT(cmd2) &&
		cmd3GenOpcode && cmd3GenOpcode->opcode() == "MUL"
	) {
		bigint a = pushintValue(cmd1);
		bigint b = pushintValue(cmd2);
		bigint c = a * b;
		return Result{3, gen("PUSHINT " + toString(c))};
	}

	if (isPUSHINT(cmd1) &&
		isPUSHINT(cmd2) &&
		cmd3GenOpcode && cmd3GenOpcode->opcode() == "DIV"
	) {
		bigint a = pushintValue(cmd1);
		bigint b = pushintValue(cmd2);
		if (a >= 0 && b > 0) { // note in TVM  -9 / 2 == -5
			bigint c = a / b;
			return Result{3, gen("PUSHINT " + toString(c))};
		}
	}

	// TRUE
	// NEWC
	// STI 1
	if ((is(cmd1, "TRUE") || is(cmd1, "FALSE")) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STI") && arg(cmd3) == "1"
	) {
		if (is(cmd1, "TRUE"))
			return Result{3, gen("NEWC"), gen("STONE")};
		return Result{3, gen("NEWC"), gen("STZERO")};
	}

	if (
		isBLKSWAP(cmd1) &&
		isPureGen01(*cmd2) &&
		isBLKSWAP(cmd3)
	) {
		auto [bottom1, top1] = isBLKSWAP(cmd1).value();
		auto [bottom3, top3] = isBLKSWAP(cmd3).value();
		if (bottom1 == 1 && bottom3 == 1 && top3 == 1) {
			return Result{3, cmd2, makeBLKSWAP(bottom1, top1 + 1)};
		}
	}

	if (is(cmd1, "NULL") && isPUSH2 && *isPUSH2 == 0 && is(cmd3, "ISNULL")) {
		return Result{3, gen("NULL"), gen("TRUE")};
	}

	// gen(0, 1)
	// BLKPUSH N, 0
	// gen(0, 1)
	// =>
	// gen(0, 1)
	// BLKPUSH N+1, 0
	if (
		isPureGen01(*cmd1) &&
		isBLKPUSH(cmd2) &&
		isPureGen01(*cmd3) &&
		*cmd1 == *cmd3
	) {
		auto [qty, index] = isBLKPUSH(cmd2).value();
		if (index == 0 && qty + 1 <= 15) {
			return Result{3, cmd1, makeBLKPUSH(qty + 1, index)};
		}
	}

	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt4(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3, Pointer<TvmAstNode> const& cmd4) {
	if (isPUSHINT(cmd1) && isPUSHINT(cmd3)) {
		if (isAddOrSub(cmd2) && isAddOrSub(cmd4)) {
			bigint sum = 0;
			sum += (is(cmd2, "ADD") ? +1 : -1) * pushintValue(cmd1);
			sum += (is(cmd4, "ADD") ? +1 : -1) * pushintValue(cmd3);
			return Result{4, gen("PUSHINT " + toString(sum)), gen("ADD")};
		}
	}
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "STSLICECONST")
	) {
		std::optional<std::string> slice = StrUtils::unitSlices(isPlainPushSlice(cmd1)->blob(), arg(cmd4));
		if (slice.has_value()) {
			return Result{4,
						   genPushSlice(*slice),
						   gen("NEWC"),
						   gen("STSLICE")};
		}
	}
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICECONST") &&
		is(cmd4, "STSLICE")) {
		std::optional<std::string> slice = StrUtils::unitSlices(arg(cmd3), isPlainPushSlice(cmd1)->blob());
		if (slice.has_value()) {
			return Result{4,
						  genPushSlice(*slice),
						   gen("NEWC"),
						   gen("STSLICE")};
		}
	}
	// ADDCONST/INC/DEC
	// UFIT/FIT N
	// ADDCONST/INC/DEC
	// UFIT/FIT N
	// =>
	// ADDCONST
	// UFIT/FIT N
	if (isConstAdd(cmd1) && isConstAdd(cmd3)) {
		for (std::string fit : {"UFITS", "FITS"}) {
			if (is(cmd2, fit) && is(cmd4, fit) && arg(cmd2) == arg(cmd4)) {
				int final_add = getAddNum(cmd1) + getAddNum(cmd3);
				if (-128 <= final_add && final_add <= 127)
					return Result{4,
										   gen("ADDCONST " + std::to_string(final_add)),
										   gen(fit + " " + arg(cmd2))};
			}
		}
	}
	if (isPUSHINT(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICECONST") &&
		is(cmd4, "STU")) {
		std::string bitStr = StrUtils::toBitString(arg(cmd3)) +
			StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd4));
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{4,
			  genPushSlice(*slice),
				gen("NEWC"),
				gen("STSLICE")};
		}
	}
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		(is(cmd4, "STONE") || is(cmd4, "STZERO"))
	) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd1)->blob());
		bitStr += is(cmd4, "STONE") ? "1" : "0";
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{4,
						  genPushSlice(*slice),
						  gen("NEWC"),
						  gen("STSLICE")
			};
		}
	}
	if (isPUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		isPUSHINT(cmd3) &&
		is(cmd4, "STZEROES")
	) {
		int bitQty = fetchInt(cmd1) + fetchInt(cmd3);
		return Result{4,
					  gen("PUSHINT " + toString(bitQty)),
					  gen("STZEROES")
		};
	}

	if (isPUSHINT(cmd1) &&
		is(cmd2, "STUR") &&
		isPUSHINT(cmd3) &&
		is(cmd4, "STUR")
	) {
		bigint a = pushintValue(cmd1);
		int lenA = fetchInt(cmd2);
		bigint b = pushintValue(cmd3);
		int lenB = fetchInt(cmd4);
		if (lenA + lenB <= 256) {
			bigint c = (a << lenB) + b;
			return Result{4,
						  gen("PUSHINT " + toString(c)),
						  gen("STUR " + toString(lenA + lenB))
			};
		}
	}
	if (
		isPUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		isPUSHINT(cmd3) &&
		is(cmd4, "STUR")
	) {
		bigint bitQty = pushintValue(cmd1) + fetchInt(cmd4);
		if (bitQty <= 256) {
			return Result{4,
						  gen("PUSHINT " + arg(cmd3)),
						  gen("STUR " + toString(bitQty))
			};
		}
	}
	if (
		isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "ENDC")
	) {
		return Result{4, makePUSHREF(isPlainPushSlice(cmd1)->blob())};
	}
	if (isPUSHINT(cmd1) && isPUSHINT(cmd1) && pushintValue(cmd1) == 0 &&
		is(cmd2, "STUR") &&
		isPUSHINT(cmd3) && isPUSHINT(cmd3) && pushintValue(cmd3) == 0 &&
		is(cmd4, "STUR")
	) {
		int bitSize = fetchInt(cmd2) + fetchInt(cmd4);
		if (bitSize <= 256)
			return Result{4, gen("PUSHINT 0"), gen("STUR " + toString(bitSize))};
	}


	// PUSHSLICE xXXX
	// NEWC
	// STSLICE
	// STBREFR
	// =>
	// PUSHCELL
	// STREFR
	if (
		isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "STBREFR")
	) {
		return Result{4, makePUSHREF(isPlainPushSlice(cmd1)->blob()), gen("STREFR")};
	}

	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt5(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3, Pointer<TvmAstNode> const& cmd4,
											 Pointer<TvmAstNode> const& cmd5) {
	// PUSHSLICE A
	// PUSHSLICE B
	// NEWC
	// STSLICE
	// STSLICE
	// =>
	// PUSHSLICE BA
	// NEWC
	// STSLICE
	if (isPlainPushSlice(cmd1) &&
		isPlainPushSlice(cmd2) &&
		is(cmd3, "NEWC") &&
		is(cmd4, "STSLICE") &&
		is(cmd5, "STSLICE")
	) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd2)->blob()) +
							 StrUtils::toBitString(isPlainPushSlice(cmd1)->blob());
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{5,
						  genPushSlice(*slice),
						  gen("NEWC"),
						  gen("STSLICE")};
		}
	}

	// PUSHINT ?
	// PUSHSLICE ?
	// NEWC
	// STSLICE ?
	// STU ?
	if (isPUSHINT(cmd1) &&
		isPlainPushSlice(cmd2) &&
		is(cmd3, "NEWC") &&
		is(cmd4, "STSLICE") &&
		is(cmd5, "STU")
	) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd2)->blob()) +
			StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd5));
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{5,
					  genPushSlice(*slice),
					gen("NEWC"),
					gen("STSLICE")};
		}
	}
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		isPlainPushSlice(cmd4) &&
		is(cmd5, "STSLICER")) {
		std::optional<std::string> slice = StrUtils::unitSlices(isPlainPushSlice(cmd1)->blob(), isPlainPushSlice(cmd4)->blob());
		if (slice.has_value()) {
			return Result{5,
						  genPushSlice(*slice),
						   gen("NEWC"),
						   gen("STSLICE")};
		}
	}
	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAt6(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3, Pointer<TvmAstNode> const& cmd4,
											 Pointer<TvmAstNode> const& cmd5, Pointer<TvmAstNode> const& cmd6) {

	if (
		isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "NEWC") &&
		is(cmd5, "STSLICECONST") &&
		is(cmd6, "STB")
	) {
		std::string str1 = StrUtils::toBitString(isPlainPushSlice(cmd1)->blob());
		std::string str5 = StrUtils::toBitString(arg(cmd5));
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(str5, str1);
		if (slice.has_value()) {
			return Result{6,
						  genPushSlice(*slice),
						  gen("NEWC"),
						  gen("STSLICE")
			};
		}
	}
	return Result{};
}

Result PrivatePeepholeOptimizer::optimizeAtInf(int idx1) const {
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	int idx2 = nextCommandLine(idx1);

	// squash DROPs
	{
		int i = idx1, n = 0, total = 0;
		while (i != -1 && isDrop(get(i))) {
			n++;
			total += isDrop(get(i)).value();
			i = nextCommandLine(i);
		}
		if (n >= 2) {
			return Result{n, makeDROP(total)};
		}
	}

	// BLKSWAP N, 1
	// BLKSWAP N, 1
	// BLKSWAP N, 1
	// ...
	// BLKSWAP N, 1
	// =>
	//
	if (isBLKSWAP(cmd1)) {
		auto [n, top] = isBLKSWAP(cmd1).value();
		if (top == 1) {
			bool ok = true;
			for (int iter = 0; iter < n + 1; ++iter) {
				if (get(idx1 + iter) == nullptr) {
					ok = false;
					break;
				}
				auto c = get(idx1 + iter);
				ok &= isBLKSWAP(c) && std::make_pair(n, 1) == isBLKSWAP(c).value();
			}
			if (ok) {
				return Result{n + 1};
			}
		}
	}

	// POP N
	// POP N
	// ...
	// =>
	// BLKDROP2 N, N
	if (isPOP(cmd1)) {
		int n = isPOP(cmd1).value();
		if (n >= 2) {
			int index = idx1;
			bool ok = true;
			int i = 0;
			for (; ok && i < n && index != -1; ++i, index = nextCommandLine(index)) {
				auto cmd = get(index);
				ok &= isPOP(cmd) && isPOP(cmd).value() == n;
			}
			ok &= i == n;
			if (ok) {
				return Result{n, makeBLKDROP2(n, n)};
			}
		}
	}

	// REVERSE n, 0
	// POP
	// ...
	// POP
	// =>
	// POP
	// ..
	// POP
	if (isREVERSE(cmd1) && idx2 != -1) {
		auto [n, startIndex] = isREVERSE(cmd1).value();
		if (startIndex == 0) {
			vector<Pointer<TvmAstNode>> newCmds;
			int index = idx2;
			bool ok = true;
			int i = 0;
			for (; ok && i < n && index != -1; ++i, index = nextCommandLine(index)) {
				auto cmd = get(index);
				ok &= isPOP(cmd).has_value();
				newCmds.push_back(cmd);
			}
			ok &= i == n;
			if (ok) {
				std::reverse(newCmds.begin(), newCmds.end());
				std::set<int> uniqInds;
				int deltaSi = n - 1;
				for (i = 0; i < n; ++i) {
					int si = isPOP(newCmds[i]).value();
					int newSi = si + deltaSi;
					ok &= newSi >= n - i;
					if (!ok) {
						break;
					}
					newCmds[i] = makePOP(newSi);
					deltaSi -= 2;
					uniqInds.insert(newSi + i);
				}
				ok &= static_cast<int>(uniqInds.size()) == n;
				if (ok) {
					return Result{n + 1, newCmds};
				}
			}
		}
	}

	if (is(cmd1, "STONE") || is(cmd1, "STZERO")) {
		int qty = 0;
		int i = idx1;
		std::string bits;
		while (
			i != -1 &&
			qty + 1 < TvmConst::MaxSTSLICECONST &&
			(is(get(i), "STONE") || is(get(i), "STZERO"))
		) {
			bits += is(get(i), "STONE") ? "1" : "0";
			++qty;
			i = nextCommandLine(i);
		}
		if (qty >= 2) {
			std::optional<std::string> slice = StrUtils::unitBitStringToHex(bits, "");
			solAssert(slice.has_value(), "");
			return Result{qty, gen("STSLICECONST " + *slice)};
		}
	}

	// ROLL n
	// ROLL n+1
	// ROLL n+2
	// ...
	// =>
	// BLKSWAP qty, n
	// REVERSE qty, 0
	if (isBLKSWAP(cmd1)) {
		auto [down, up] = isBLKSWAP(cmd1).value();
		if (down == 1) {
			int i = nextCommandLine(idx1);
			int targetUp = up + 1;
			int qty = 1;
			while (true) {
				if (i == -1 || !isBLKSWAP(get(i)))
					break;
				auto [downi, upi] = isBLKSWAP(get(i)).value();
				if (downi != 1 || upi != targetUp)
					break;
				++targetUp;
				i = nextCommandLine(i);
				++qty;
			}
			if (qty >= 2) {
				return Result{qty, makeBLKSWAP(qty, up), makeREVERSE(qty, 0)};
			}
		}
	}

	// squash stack opcodes
	{
		StackState bestState{};
		int bestQty{};

		StackState state;
		int i = idx1;
		int qty = 0;
		while (true) {
			if (i == -1)
				break;
			auto stack = to<Stack>(get(i).get());
			if (!stack)
				break;
			if (!state.apply(*stack)) {
				break;
			}

			++qty;
			int steps = StackOpcodeSquasher::steps(state);
			if (steps != -1 && steps < qty) {
				bestState = state;
				bestQty = qty;
			}

			i = nextCommandLine(i);
		}

		if (bestQty != 0) {
			return Result{bestQty, StackOpcodeSquasher::recover(bestState)};
		}
	}

	// squash permutation of pure operations
	{
		// pure gen, get glob, push Si
		// BLKSWAP, REVERSE, XCHG
		std::deque<std::pair<Pointer<TvmAstNode>, int>> opcodes;
		bool removed = false;
		int cnt = 0;

		for (int ii = idx1; ii != -1; ii = nextCommandLine(ii)) {
			TvmAstNode const* op = get(ii).get();
			auto stack = to<Stack>(op);
			if (isPureGen01(*op)) {
				opcodes.emplace_front(get(ii), opcodes.size());
				++cnt;
			} else if (stack) {
				int i = stack->i();
				int j = stack->j();
				int n = opcodes.size();
				bool ok = true;
				switch (stack->opcode()) {
					case Stack::Opcode::BLKSWAP: {
						if (i + j <= n) {
							std::reverse(opcodes.begin() + j, opcodes.begin() + j + i);
							std::reverse(opcodes.begin() , opcodes.begin() + j);
							std::reverse(opcodes.begin(), opcodes.begin() + j + i);
							removed = true;
							++cnt;
						} else {
							ok = false;
						}
						break;
					}
					case Stack::Opcode::REVERSE: {
						if (j < n && j + i <= n) {
							std::reverse(opcodes.begin() + j, opcodes.begin() + j + i);
							removed = true;
							++cnt;
						} else {
							ok = false;
						}
						break;
					}
					case Stack::Opcode::XCHG: {
						if (i < n && j < n) {
							swap(opcodes[i], opcodes[j]);
							removed = true;
							++cnt;
						} else {
							ok = false;
						}
						break;
					}
					case Stack::Opcode::PUSH_S: {
						if (i >= n) {
							opcodes.emplace_front(get(ii), opcodes.size());
							++cnt;
						} else {
							ok = false;
						}
						break;
					}
					default:
						ok = false;
						break;
				}
				if (!ok) {
					break;
				}
			} else {
				break;
			}
		}

		if (removed) {
			vector<Pointer<TvmAstNode>> res;
			res.reserve(opcodes.size());
			for (auto const& [opcode, stackSize]  : opcodes | boost::adaptors::reversed) {
				if (isPUSH(opcode)) {
					int index = isPUSH(opcode).value();
					int newStackSize = res.size();
					int newIndex = index - stackSize + newStackSize;
					res.emplace_back(makePUSH(newIndex));
				} else {
					res.emplace_back(opcode);
				}
			}
			return Result{cnt, res};
		}
	}

	return Result{};
}

bool PrivatePeepholeOptimizer::hasRetOrJmp(TvmAstNode const* _node) {
	if (auto opaque = to<Opaque>(_node)) {
		for (Pointer<TvmAstNode> const& i : opaque->block()->instructions()) {
			if (hasRetOrJmp(i.get())) {
				return true;
			}
		}
	}
	if (auto cb = to<CodeBlock>(_node)) {
		for (Pointer<TvmAstNode> const& i : cb->instructions()) {
			if (hasRetOrJmp(i.get())) {
				return true;
			}
		}
	}
	if (to<ReturnOrBreakOrCont>(_node)) {
		return true;
	}
	if (to<TvmReturn>(_node)) {
		return true;
	}
	if (auto sub = to<SubProgram>(_node)) {
		if (sub->isJmp())
			return true;
	}
	if (auto isElse = to<TvmIfElse>(_node)) {
		if (isElse->withJmp())
			return true;
	}
	return false;
}

Result PrivatePeepholeOptimizer::unsquash(bool _withUnpackOpaque, const int idx1) const {
	auto c = get(idx1);
	auto stack = to<Stack>(c.get());
	if (isStack(c, Stack::Opcode::PUSH2_S)) {
		int si = stack->i();
		int sj = stack->j() + 1;
		return Result{1, makePUSH(si), makePUSH(sj)};
	}
	if (_withUnpackOpaque) {
		if (auto ret = to<ReturnOrBreakOrCont>(c.get())) {
			return Result{1, ret->body()->instructions()};
		}
		if (auto op = to<Opaque>(c.get())) {
			return Result{1, op->block()->instructions()};
		}
	}
	return Result{};
}

Result PrivatePeepholeOptimizer::squashPush(const int idx1) const {
	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);
	Pointer<TvmAstNode> const& cmd3 = get(idx3);
	auto cmd1Gen = to<Gen>(cmd1.get());
	auto cmd1PushCellOrSlice = to<PushCellOrSlice>(cmd1.get());

	if (isPUSH(cmd1) && isPUSH(cmd2)) {
		int i = idx1, n = 0;
		while (isPUSH(get(i)) && isPUSH(get(i)).value() == isPUSH(cmd1).value()) {
			n++;
			i = nextCommandLine(i);
		}
		if (n >= 2 && isPUSH(cmd1) <= 15) {
			return Result{n, makeBLKPUSH(n, isPUSH(cmd1).value())};
		}
	}

	if (isPUSH(cmd1) && isPUSH(cmd2) && isPUSH(cmd3)) {
		const int si = *isPUSH(cmd1);
		const int sj = *isPUSH(cmd2) - 1 == -1? si : *isPUSH(cmd2) - 1;
		const int sk = *isPUSH(cmd3) - 2 == -1? si : (
				*isPUSH(cmd3) - 2 == -2? sj : *isPUSH(cmd3) - 2
		);
		if (si <= 15 && sj <= 15 && sk <= 15) {
			return Result{3, makePUSH3(si, sj, sk)};
		}
	}
	if (isPUSH(cmd1) && isPUSH(cmd2)) {
		const int si = *isPUSH(cmd1);
		const int sj = *isPUSH(cmd2) - 1 == -1? si : *isPUSH(cmd2) - 1;
		if (si <= 15 && sj <= 15) {
			return Result{2, makePUSH2(si, sj)};
		}
	}

	// TODO delete
	// squash PUSHINT, NEWDICT, NILL, FALSE, TRUE, (PUSHINT 0 NEWDICT PAIR) etc
	if (isPureGen01(*cmd1)) {
		int i = idx1;
		int n = 0;
		while (true) {
			auto cmdI = to<Gen>(get(i).get());
			if  (cmdI && *cmd1Gen == *cmdI) {
				n++;
				i = nextCommandLine(i);
			} else {
				break;
			}
		}
		if (n >= 2) {
			return Result{n, cmd1, makeBLKPUSH(n - 1, 0)};
		}
	}

	// squash PUSHREF/PUSHREFSLICE/CELL
	if (cmd1PushCellOrSlice) {
		int i = idx1;
		int n = 0;
		while (true) {
			auto cmdI = to<PushCellOrSlice>(get(i).get());
			if  (cmdI && cmd1PushCellOrSlice->equal(*cmdI))
			{
				n++;
				i = nextCommandLine(i);
			} else {
				break;
			}
		}
		if (n >= 2) {
			return Result{n, cmd1, makeBLKPUSH(n - 1, 0)};
		}
	}

	// SWAP
	// OVER
	// =>
	// TUCK
	if (isXCHG_S0(cmd1) && isXCHG_S0(cmd1).value() == 1 &&
		isPUSH(cmd2) && *isPUSH(cmd2) == 1
	) {
		return Result{2, makeTUCK()};
	}

	// PUSH Si
	// SWAP
	// =>
	// PUXC Si, S-1
	if (isPUSH(cmd1) &&
		isXCHG_S0(cmd2) && isXCHG_S0(cmd2).value() == 1
	) {
		int i = *isPUSH(cmd1);
		if (0 <= i && i <= 15)
			return Result{2, makePUXC(i, -1)};
	}

	return Result{};
}

void PrivatePeepholeOptimizer::updateLinesAndIndex(int idx1, const Result& res) {
	solAssert(res.success, "");
	if (res.removeQty > 0) {
		solAssert(valid(idx1), "");
		solAssert(!isLoc(m_instructions.at(idx1)), "");
		int lastInx = idx1;
		for (int iter = 0; iter + 1 < res.removeQty; ++iter) {
			lastInx = nextCommandLine(lastInx);
			solAssert(valid(lastInx), "");
			solAssert(!isLoc(m_instructions.at(lastInx)), "");
		}

		Pointer<TvmAstNode> locLine;
		for (int i = idx1; i <= lastInx; ++i) {
			if (isLoc(m_instructions.at(i))) {
				locLine = m_instructions.at(i);
			}
		}

		// save opcodes after the peephole
		std::vector<Pointer<TvmAstNode>> codeTail(m_instructions.begin() + lastInx + 1, m_instructions.end());
		// delete the peephole and all after it
		m_instructions.erase(m_instructions.begin() + idx1, m_instructions.end());
		// insert new peephole
		for (Pointer<TvmAstNode> const& inst : res.commands) {
			m_instructions.emplace_back(inst);
		}
		// insert .loc if it presents
		if (locLine != nullptr) {
			m_instructions.push_back(locLine);
		}
		// insert the tail
		m_instructions.insert(m_instructions.end(), codeTail.begin(), codeTail.end());
	}
}

bool PrivatePeepholeOptimizer::optimize(const std::function<Result(int)> &f) {
	int idx1 = 0;
	while (idx1 < static_cast<int>(m_instructions.size()) && isLoc(m_instructions.at(idx1))) {
		++idx1;
	}

	bool didSomething = false;
	while (valid(idx1)) {
		solAssert(!isLoc(m_instructions.at(idx1)), "");
		Result res = f(idx1);
		if (res.success) {
			didSomething = true;
			updateLinesAndIndex(idx1, res);
			// step back to several commands
			idx1 = std::min<int>(idx1, m_instructions.size() - 1);
			idx1 = max(idx1, 0);
			int cnt = 10;
			while (cnt > 0 && idx1 > 0) {
				--idx1;
				if (!isLoc(m_instructions.at(idx1)))
					--cnt;
			}
			while (idx1 < static_cast<int>(m_instructions.size()) && isLoc(m_instructions.at(idx1))) {
				++idx1;
			}

//			Printer p{std::cout};
//			for (auto x : m_instructions) x->accept(p);
//			std::cout << std::endl;
		} else {
			idx1 = nextCommandLine(idx1);
		}
	}
	return didSomething;
}

std::optional<std::pair<int, int>> PrivatePeepholeOptimizer::isBLKDROP2(Pointer<TvmAstNode> const& node) {
	if (isStack(node, Stack::Opcode::BLKDROP2)) {
		auto stack = to<Stack>(node.get());
		return {{stack->i(), stack->j()}};
	}
	if (isStack(node, Stack::Opcode::POP_S)) {
		auto stack = to<Stack>(node.get());
		if (stack->i() == 1)
			return {{1, 1}};
	}
	return std::nullopt;
}

std::optional<int> PrivatePeepholeOptimizer::isPUSH(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (isStack(node, Stack::Opcode::PUSH_S)) {
		return stack->i();
	}
	if (isStack(node, Stack::Opcode::BLKPUSH)) {
		if (stack->i() == 1)
			return stack->j();
	}
	return {};
}

bigint PrivatePeepholeOptimizer::pushintValue(Pointer<TvmAstNode> const& node) {
	solAssert(isPUSHINT(node), "");
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return bigint{g->arg()};
}

int PrivatePeepholeOptimizer::fetchInt(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return strToInt(g->arg());
}

bool PrivatePeepholeOptimizer::isNIP(Pointer<TvmAstNode> const& node) {
	auto swap = dynamic_pointer_cast<Stack>(node);
	return
	(isPOP(node) && isPOP(node).value() == 1) ||
	(isBLKDROP2(node) && isBLKDROP2(node).value() == std::make_pair(1, 1));
}

bool PrivatePeepholeOptimizer::isPUSHINT(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	if (!g || g->opcode() != "PUSHINT")
		return false;
	int i = 0;
	for (char ch : g->arg()) {
		if (!(isdigit(ch) || (i == 0 && ch=='-'))) {
			return false; // e.g. PUSHINT $func_name$
		}
		++i;
	}
	return true;
}

std::string PrivatePeepholeOptimizer::arg(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	solAssert(g, "");
	return g->arg();
}

bool PrivatePeepholeOptimizer::is(Pointer<TvmAstNode> const& node, const std::string& opcode) {
	auto g = to<GenOpcode>(node.get());
	return g && g->opcode() == opcode;
}

std::pair<int, int> PrivatePeepholeOptimizer::getIndexes(std::string const& str) {
	size_t pos = str.find(',');
	solAssert(pos < str.size(), "");
	return {strToInt(str.substr(0, pos)), strToInt(str.substr(pos + 2))};
}

template<class ...Args>
bool PrivatePeepholeOptimizer::isExc(Pointer<TvmAstNode> const& node, Args&&... cmd) {
	auto cfi = to<TvmException>(node.get());
	return cfi && isIn(cfi->opcode(), std::forward<Args>(cmd)...);
}

bool PrivatePeepholeOptimizer::isRot(Pointer<TvmAstNode> const& node) {
	return isBLKSWAP(node) && std::make_pair(1, 2) == isBLKSWAP(node).value();
}

bool PrivatePeepholeOptimizer::isConstAdd(Pointer<TvmAstNode> const& node) {
	auto gen = to<GenOpcode>(node.get());
	return gen && isIn(gen->opcode(), "INC", "DEC", "ADDCONST");
}

int PrivatePeepholeOptimizer::getAddNum(Pointer<TvmAstNode> const& node) {
	solAssert(isConstAdd(node), "");
	auto gen = to<GenOpcode>(node.get());
	solAssert(gen, "");
	if (gen->opcode() == "INC") {
		return +1;
	}
	if (gen->opcode() == "DEC") {
		return -1;
	}
	if (gen->opcode() == "ADDCONST") {
		return strToInt(gen->arg());
	}
	solUnimplemented("");
}

bool PrivatePeepholeOptimizer::isStack(Pointer<TvmAstNode> const& node, Stack::Opcode op) {
	auto stack = to<Stack>(node.get());
	return stack && stack->opcode() == op;
}

std::optional<std::pair<int, int>> PrivatePeepholeOptimizer::checkSimpleCommand(Pointer<TvmAstNode> const& node) {
	if (auto gen = to<Gen>(node.get())) {
		return {{gen->take(), gen->ret()}};
	}
	if (isBLKSWAP(node)) {
		auto [bottom, top] = isBLKSWAP(node).value();
		int sum = bottom + top;
		return {{sum, sum}};
	}
	return std::nullopt;
}

bool PrivatePeepholeOptimizer::isSimpleCommand(Pointer<TvmAstNode> const& node, int take, int ret) {
	std::optional<std::pair<int, int>> opt = checkSimpleCommand(node);
	if (!opt) {
		return false;
	}
	return opt.value() == std::make_pair(take, ret);
}

bool PrivatePeepholeOptimizer::isAddOrSub(Pointer<TvmAstNode> const& node) {
	return is(node, "ADD") || is(node, "SUB");
}

bool PrivatePeepholeOptimizer::isCommutative(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return g && isIn(g->fullOpcode(),
					 "ADD",
					 "AND",
					 "EQUAL",
					 "MAX",
					 "MIN",
					 "MUL",
					 "NEQ",
					 "OR",
					 "SDEQ",
					 "XOR"
	);
}

void PeepholeOptimizer::endVisit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	PrivatePeepholeOptimizer optimizer{instructions, m_withUnpackOpaque};
	optimizer.optimize([&](int index){
		return optimizer.unsquash(m_withUnpackOpaque, index);
	});

	while (optimizer.optimize([&optimizer](int index){ return optimizer.optimizeAt(index); })) {}

	optimizer.optimize([&optimizer](int index){ return optimizer.squashPush(index);});
	_node.upd(optimizer.instructions());
}

} // end solidity::frontend
