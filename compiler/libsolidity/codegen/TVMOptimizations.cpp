/*
 * Copyright 2018-2020 TON DEV SOLUTIONS LTD.
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
 * @date 2020
 * TVM codegen driver
 */

#include <boost/algorithm/string/trim.hpp>
#include <boost/format.hpp>

#include "TvmAst.hpp"
#include "TvmAstVisitor.hpp"
#include "TVMConstants.hpp"
#include "TVMOptimizations.hpp"
#include "TVMOptimizations.hpp"
#include "TVMPusher.hpp"

namespace solidity::frontend {

struct Result {
	bool continue_{};
	int remove_{};
	vector<Pointer<TvmAstNode>> commands_;

	explicit Result(bool cont, int remove = 0, vector<Pointer<TvmAstNode>> commands = {}) :
			continue_(cont), remove_(remove), commands_{std::move(commands)} {

	}

	template <class ...Args>
	static Result Replace(int remove, Args... cmds) {
		return Result(true, remove, {cmds...});
	}

};

class TVMOptimizerPrivate {
public:
	TVMOptimizerPrivate(std::vector<Pointer<TvmAstNode>> instructions) : m_instructions{std::move(instructions)} {}
	vector<Pointer<TvmAstNode>> const &instructions() const { return m_instructions; }

	int next_command_line(int idx) const;
	Pointer<TvmAstNode> get(int idx) const;
	bool valid(int idx) const;
	void remove(int idx);
	void insert(int idx, Pointer<TvmAstNode> node);
	Result optimize_at(const int idx1) const;

	bool try_simulate(int i, int stack_size, int& remove_count, vector<Pointer<TvmAstNode>>& commands) const;
	bool updateLinesAndIndex(int& idx1, const Result& res);
	Result unsquash_push(const int idx1) const;
	Result squash_push(const int idx1) const;
	void optimize(const std::function<Result(int)> &f);

	std::optional<std::pair<int, int>> getBLKDROP2(Pointer<TvmAstNode> node) const;
	int qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> arr) const;
	bool is_SWAP(Pointer<TvmAstNode> node) const;
	bool is_PUSH(Pointer<TvmAstNode> node) const;
	int get_push_index(Pointer<TvmAstNode> node) const;
	bool is_PUSHINT(Pointer<TvmAstNode> node) const;
	bigint pushint_value(Pointer<TvmAstNode> node) const;
	int fetch_int(Pointer<TvmAstNode> node) const;
	bool is_drop_kind(Pointer<TvmAstNode> node) const;
	int get_drop_qty(Pointer<TvmAstNode> node) const;
	bool is_POP(Pointer<TvmAstNode> node) const;
	int get_pop_index(Pointer<TvmAstNode> node) const;
	bool is_NIP(Pointer<TvmAstNode> node) const;
	std::string arg(Pointer<TvmAstNode> node) const ;
	bool is(Pointer<TvmAstNode> node, std::string opcode) const;
	std::optional<std::pair<int, int>> check_simple_command(Pointer<TvmAstNode> node) const;
	bool isPureGen01OrGetGlob(Pointer<TvmAstNode> node) const;
	bool is_simple_command(Pointer<TvmAstNode> node, int take, int ret) const;
	bool is_add_or_sub(Pointer<TvmAstNode> node) const;
	bool is_commutative(Pointer<TvmAstNode> node) const;
	std::optional<std::pair<int, int>> isBLKSWAP(Pointer<TvmAstNode> node) const;
	std::optional<std::pair<int, int>> isREVERSE(Pointer<TvmAstNode> node) const;
	template<class ...Args>
	bool isCFI(Pointer<TvmAstNode> node, Args&&... cmd) const;
	bool isRot(Pointer<TvmAstNode> node) const;
	bool isRotRev(Pointer<TvmAstNode> node) const;
	bool is_const_add(Pointer<TvmAstNode> node) const;
	int get_add_num(Pointer<TvmAstNode> node) const;
	bool isStack(Pointer<TvmAstNode> node, Stack::Opcode op) const;
	//std::optional<std::pair> getBLKPUSH(Pointer<TvmAstNode> node, Stack::Opcode op) const;
private:
	std::vector<Pointer<TvmAstNode>> m_instructions{};
};

int TVMOptimizerPrivate::next_command_line(int idx) const {
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

Pointer<TvmAstNode> TVMOptimizerPrivate::get(int idx) const {
	return valid(idx) ? m_instructions.at(idx) : nullptr;
}

bool TVMOptimizerPrivate::valid(int idx) const {
	return idx >= 0 && size_t(idx) < m_instructions.size();
}

void TVMOptimizerPrivate::remove(int idx) {
	m_instructions.erase(m_instructions.begin() + idx);
}

void TVMOptimizerPrivate::insert(int idx, Pointer<TvmAstNode> node) {
	m_instructions.insert(m_instructions.begin() + idx, node);
}

Result TVMOptimizerPrivate::optimize_at(const int idx1) const {
	int idx2 = next_command_line(idx1);
	int idx3 = next_command_line(idx2);
	int idx4 = next_command_line(idx3);
	int idx5 = next_command_line(idx4);
	int idx6 = next_command_line(idx5);
	Pointer<TvmAstNode> cmd1 = get(idx1);
	Pointer<TvmAstNode> cmd2 = get(idx2);
	Pointer<TvmAstNode> cmd3 = get(idx3);
	Pointer<TvmAstNode> cmd4 = get(idx4);
	Pointer<TvmAstNode> cmd5 = get(idx5);
	Pointer<TvmAstNode> cmd6 = get(idx6);

	auto cmd1GenOp = to<GenOpcode>(cmd1.get());
	auto cmd1Glob = to<Glob>(cmd1.get());
	auto cmd1IfElse = to<TvmIfElse>(cmd1.get());
	auto cmd1Stack = to<Stack>(cmd1.get());

	auto cmd2CFI = to<ConFlowInst>(cmd2.get());
	auto cmd2GenOpcode = to<GenOpcode>(cmd2.get());
	auto cmd2Glob = to<Glob>(cmd2.get());
	auto cmd2IfElse = to<TvmIfElse>(cmd2.get());

	auto cmd3GenOpcode = to<GenOpcode>(cmd3.get());

	auto isBLKDROP1 = getBLKDROP2(cmd1);
	auto isBLKDROP2 = getBLKDROP2(cmd2);

	std::map<bigint, int> power2;
	for (int p2 = 2, p = 1; p2 <= 256; p2 *= 2, ++p) {
		power2[p2] = p;
	}

	if (is_SWAP(cmd1)) {
		if (is(cmd2, "SUB")) return Result::Replace(2, gen("SUBR"));
		if (is(cmd2, "SUBR")) return Result::Replace(2, gen("SUB"));
		if (is_SWAP(cmd2)) return Result::Replace(2);
		if (is_NIP(cmd2)) return Result::Replace(2, make_DROP());
		if (is_commutative(cmd2)) return Result::Replace(1);
		if (is_drop_kind(cmd2)) {
			int n = get_drop_qty(cmd2);
			if (n == 1) {
				return Result::Replace(2, make_POP(1));
			} else {
				return Result::Replace(2, make_DROP(n));
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
			return Result::Replace(2, gen(opcode + " " + arg(cmd2)));
		}
	}
	if (is_PUSHINT(cmd1) && is_PUSHINT(cmd3)) {
		// TODO: consider INC/DEC as well
		if (is_add_or_sub(cmd2) && is_add_or_sub(cmd4)) {
			bigint sum = 0;
			sum += (is(cmd2, "ADD") ? +1 : -1) * pushint_value(cmd1);
			sum += (is(cmd4, "ADD") ? +1 : -1) * pushint_value(cmd3);
			return Result::Replace(4, gen("PUSHINT " + toString(sum)), gen("ADD"));
		}
	}
	if (is_PUSHINT(cmd1)) {
		if (arg(cmd1) == "1") {
			if (is(cmd2, "ADD")) return Result::Replace(2, gen("INC"));
			if (is(cmd2, "SUB")) return Result::Replace(2, gen("DEC"));
		}
		bigint value = pushint_value(cmd1);
		if (-128 <= value && value <= 127) {
			if (is(cmd2, "ADD")) return Result::Replace(2, gen("ADDCONST " + toString(value)));
			if (is(cmd2, "MUL")) return Result::Replace(2, gen("MULCONST " + toString(value)));
		}
		if (-128 <= -value && -value <= 127) {
			if (is(cmd2, "SUB")) return Result::Replace(2, gen("ADDCONST " + toString(-value)));
		}
	}
	if (isCFI(cmd1, "RET") || isCFI(cmd1, "THROWANY") || isCFI(cmd1, "THROW")) {
		// delete commands after non return opcode
		if (cmd2) {
			return Result::Replace(2, cmd1);
		}
	}
	if (isCFI(cmd1, "RET") && cmd2 == nullptr) {
		return Result::Replace(1);
	}
	if (is_NIP(cmd2) && is_NIP(cmd3)) {
		if (is_PUSH(cmd1) && get_push_index(cmd1) == 1) {
			return Result::Replace(3, make_DROP());
		}
		if (is_simple_command(cmd1, 0, 1)) {
			return Result::Replace(3, make_DROP(2), cmd1);
		}
	}
	if (is_PUSHINT(cmd1) && is_PUSHINT(cmd2) && is_PUSHINT(cmd3)) {
		int i = idx1, n = 0;
		while (is_PUSHINT(get(i)) && pushint_value(get(i)) == 0) {
			n++;
			i = next_command_line(i);
		}
		if (n >= 3) {
			Result res = Result::Replace(n, gen("PUSHINT 0"));
			n--;
			while (n > 0) {
				int nn = std::min(15, n);
			 	res.commands_.push_back(make_BLKPUSH(nn, 0));
				n -= nn;
			}
			return res;
		}
	}
	if (is_PUSH(cmd1) && get_push_index(cmd1) == 0 && is_SWAP(cmd2)) {
		return Result::Replace(2, cmd1);
	}
	if (is_SWAP(cmd3)) {
		bool ok1 = is_simple_command(cmd1, 0, 1) || is_PUSH(cmd1);
		bool ok2 = is_simple_command(cmd2, 0, 1) || is_PUSH(cmd2);
		if (ok1 && ok2) {
			if (is_PUSH(cmd2) && get_push_index(cmd2) == 0)
				return Result::Replace(3, cmd1, cmd2);
			Pointer<TvmAstNode> s1 = is_PUSH(cmd2) ? make_PUSH(get_push_index(cmd2)-1) : cmd2;
			Pointer<TvmAstNode> s2 = is_PUSH(cmd1) ? make_PUSH(get_push_index(cmd1)+1) : cmd1;
			return Result::Replace(3, s1, s2);
		}
	}
	if (is_PUSH(cmd1) && is_PUSH(cmd2)) {
		int i = idx1, n = 0;
		while (is_PUSH(get(i)) && get_push_index(get(i)) == get_push_index(cmd1)) {
			n++;
			i = next_command_line(i);
		}
		if (n >= 2 && get_push_index(cmd1) <= 15) {
			if (n > 15) n = 15;
			return Result::Replace(n, make_BLKPUSH(n, get_push_index(cmd1)));
		}
	}
	// POP Sn
	// DROP n-1
	// =>
	// BLKDROP2 n, 1
	if (is_POP(cmd1) && is_drop_kind(cmd2) && get_pop_index(cmd1) == get_drop_qty(cmd2) + 1) {
		int n = get_pop_index(cmd1);
		if (1 <= n && n <= 15) {
			return Result::Replace(2, make_BLKDROP2(n, 1));
		}
	}

	if ((is_PUSH(cmd1) || isPureGen01OrGetGlob(cmd1)) && is_drop_kind(cmd2)) {
		int qty = get_drop_qty(cmd2);
		if (qty == 1) {
			return Result::Replace(2);
		} else {
			return Result::Replace(2, make_DROP(qty - 1));
		}
	}
	if (isBLKSWAP(cmd1) && is_drop_kind(cmd2)) {
		auto [down, top] = isBLKSWAP(cmd1).value();
		int n = get_drop_qty(cmd2);
		if (n == down) {
			return Result::Replace(2, make_BLKDROP2(down, top));
		}
	}
	if (isStack(cmd1, Stack::Opcode::BLKPUSH) && is_drop_kind(cmd2)) {
		int diff = cmd1Stack->i() - get_drop_qty(cmd2);
		if (diff == 0)
			return Result::Replace(2);
		if (diff < 0)
			return Result::Replace(2, make_DROP(-diff));
		else
			return Result::Replace(2, make_BLKPUSH(diff, cmd1Stack->i()));
	}

	// PUSH S[n-1]
	// BLKDROP2 N, 1 / NIP
	// =>
	// DROP N-1
	if (is_PUSH(cmd1) &&
		isBLKDROP2) {
		int index = get_push_index(cmd1);
		auto [drop, rest] = isBLKDROP2.value();

		if (drop == index + 1 && rest == 1) {
			if (drop == 1) {
				return Result::Replace(2);
			} else {
				return Result::Replace(2, make_DROP(drop - 1));
			}
		}
	}

	// gen(0,1)
	// BLKDROP2 N, 1
	// =>
	// DROP N
	// gen(0, 1)
	if (isPureGen01OrGetGlob(cmd1) && isBLKDROP2 && isBLKDROP2.value().second == 1) {
		int n = isBLKDROP2.value().first;
		return Result::Replace(2, make_DROP(n), cmd1);
	}

	// BLKPUSH
	// BLKDROP2
	// =>
	// ???
	if (isStack(cmd1, Stack::Opcode::BLKPUSH) && isBLKDROP2) {
		int qty = cmd1Stack->i();
		int index = cmd1Stack->j();
		auto [drop, rest] = isBLKDROP2.value();

		// BLKPUSH  qty, qty-1
		// BLKDROP2 qty+X, qty
		// =>
		// BLKDROP2 X, qty
		if (qty == index + 1 && rest == qty) {
			if (drop == qty) {
				return Result::Replace(2);
			} else if (drop > qty) {
				return Result::Replace(2, make_BLKDROP2(drop - qty, qty));
			}
		}

		// BLKPUSH   qty, index
		// BLKDROP2 drop, qty
		// =>
		// DROP X, qty
		if (qty == rest) {
			int lastIndex = index - qty;
			if (lastIndex + 1 + qty == drop) {
				// a b c d e f X Y |
				// X Y a b c d e f X Y | BLKPUSH
				// X Y | BLKDROP2
				int newDrop = lastIndex + 1;
				return Result::Replace(2, make_DROP(newDrop));
			}
		}
	}

	// DUP
	// BLKDROP2 n, 1
	// =>
	// BLKDROP2 n-1, 1
	// Same as prev
	if (is_PUSH(cmd1) && get_push_index(cmd1) == 0 &&
		isBLKDROP2 && isBLKDROP2.value().second == 1) {
		int n = isBLKDROP2.value().first;
		if (n == 1) {
			return Result::Replace(2);
		} else {
			return Result::Replace(2, make_BLKDROP2(n - 1, 1));
		}
	}

	// NIP
	// DROP n
	// =>
	// DROP n+1
	if (is_NIP(cmd1) && is_drop_kind(cmd2)) {
		return Result::Replace(2, make_DROP(get_drop_qty(cmd2) + 1));
	}

	// squash DROPs
	{
		int i = idx1, n = 0, total = 0;
		while (i != -1 && is_drop_kind(get(i))) {
			n++;
			total += get_drop_qty(get(i));
			i = next_command_line(i);
		}
		if (n >= 2) {
			return Result::Replace(n, make_DROP(total));
		}
	}
	// Try to remove unneeded DUP
	if (is_PUSH(cmd1) && get_push_index(cmd1) == 0) {
		vector<Pointer<TvmAstNode>> commands;
		int lines_to_remove = 1;
		if (try_simulate(idx2, 2, lines_to_remove, commands))
			return Result{true, lines_to_remove, commands};
	}
	// Try to remove unneeded PUSH S1
	if (is_PUSH(cmd1) && get_push_index(cmd1) == 1) {
		vector<Pointer<TvmAstNode>> commands{make_XCH_S(1)};
		int lines_to_remove = 1;
		if (try_simulate(idx2, 3, lines_to_remove, commands))
			if (lines_to_remove >= qtyWithoutLoc(commands))
				return Result{true, lines_to_remove, commands};
	}
	// Try to remove unneeded gen(pureOpcode, 0, 1)
	if (isPureGen01OrGetGlob(cmd1)) {
		vector<Pointer<TvmAstNode>> commands;
		int lines_to_remove = 1;
		if (try_simulate(idx2, 1, lines_to_remove, commands))
			return Result{true, lines_to_remove, commands};
	}
	// Try to remove unneeded SWAP
	if (is_SWAP(cmd1)) {
		vector<Pointer<TvmAstNode>> commands{make_DROP()};
		int lines_to_remove = 1;
		if (try_simulate(idx2, 2, lines_to_remove, commands))
			if (lines_to_remove >= qtyWithoutLoc(commands))
				return Result{true, lines_to_remove, commands};
	}
	// Check whether topmost stack elements can be dropped
	if (!is_drop_kind(cmd1)) {
		vector<Pointer<TvmAstNode>> commands{make_DROP()};
		int lines_to_remove = 0;
		if (try_simulate(idx1, 1, lines_to_remove, commands))
			if (lines_to_remove >= qtyWithoutLoc(commands))
				return Result{true, lines_to_remove, commands};
	}
	// Check whether the second stack element can be dropped
	// it's useless
	//if (is_NIP(cmd1)) {
	//	vector<Pointer<TvmAstNode>> commands{make_POP(1)};
	//	int lines_to_remove = 0;
	//	if (try_simulate(idx1, 2, lines_to_remove, commands))
	//		if (lines_to_remove > qtyWithoutLoc(commands))
	//			return Result{true, lines_to_remove, commands};
	//}

	// NEW
	// gen(0,1)
	// ST**R
	// =>
	// gen(0,1)
	// NEW
	// ST**
	if (
		is(cmd1, "NEWC") &&
		is_simple_command(cmd2, 0, 1) &&
		cmd3GenOpcode &&
		boost::starts_with(cmd3GenOpcode->opcode(), "ST") && boost::ends_with(cmd3GenOpcode->opcode(), "R")
	) {
		auto opcode = cmd3GenOpcode->opcode();
		return Result::Replace(3,
				cmd2,
				gen("NEWC"),
				gen(opcode.substr(0, opcode.size() - 1) + " " + arg(cmd3)));
	}
	// PUSHCONT {} IF/IFNOT => DROP
	if (
		cmd1IfElse && qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0 &&
		isIn(cmd1IfElse->type(), TvmIfElse::Type::IF, TvmIfElse::Type::IFNOT))
	{
		return Result::Replace(1, make_DROP());
	}
	// PUSHCONT {} IFJMP => IFRET
	// PUSHCONT {} IFNOTJMP => IFNOTRET
	if (cmd1IfElse && qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0) {
		if (cmd1IfElse->type() == TvmIfElse::Type::IFJMP)
			return Result::Replace(1, make_IFRET());
		if (cmd1IfElse->type() == TvmIfElse::Type::IFNOTJMP)
			return Result::Replace(1, make_IFNOTRET());
	}

	// DUP
	// THROWIFNOT 507
	// DROP n
	if (is_PUSH(cmd1) && get_push_index(cmd1) == 0 &&
		isCFI(cmd2, "THROWIFNOT", "THROWIF") &&
		is_drop_kind(cmd3)
	) {
		int n = get_drop_qty(cmd3);
		if (n == 1) {
			return Result::Replace(3, cmd2);
		} else {
			return Result::Replace(3, cmd2, make_DROP(n - 1));
		}
	}

	// PUSHCONT { THROW N } IF/IFJMP => THROWIF
	// PUSHCONT { THROW N } IFNOT/IFNOTJMP => THROWIFNOT
	if (cmd1IfElse) {
		std::vector<Pointer<TvmAstNode>> const& inst = cmd1IfElse->trueBody()->instructions();
		if (qtyWithoutLoc(inst) == 1) {
			Pointer<TvmAstNode> pos;
			for (auto x : inst) if (!to<Loc>(x.get())) pos = x;
			auto _throw = to<ConFlowInst>(pos.get());
			if (_throw && _throw->opcode() == "THROW") {
				if (isIn(cmd1IfElse->type(), TvmIfElse::Type::IF, TvmIfElse::Type::IFJMP))
					return Result::Replace(1, make_THROW("THROWIF " + _throw->arg()));
				if (isIn(cmd1IfElse->type(), TvmIfElse::Type::IFNOT, TvmIfElse::Type::IFNOTJMP))
					return Result::Replace(1, make_THROW("THROWIFNOT " + _throw->arg()));
			}
		}
	}
	// NOT THROWIFNOT N => THROWIF N
	// NOT THROWIF N => THROWIFNOT N
	if (is(cmd1, "NOT")) {
		if (isCFI(cmd2, "THROWIF"))
			return Result::Replace(2, make_THROW("THROWIFNOT " + cmd2CFI->arg()));
		if (isCFI(cmd2, "THROWIFNOT"))
			return Result::Replace(2, make_THROW("THROWIF " + cmd2CFI->arg()));
		if (cmd2IfElse)
			return Result::Replace(2, makeRevert(*cmd2IfElse));
	}
	// EQINT 0, THROWIFNOT N => THROWIF N
	// EQINT 0, THROWIF N => THROWIFNOT N
	if (is(cmd1, "EQINT") && cmd1GenOp->arg() == "0") {
		if (isCFI(cmd2, "THROWIF"))
			return Result::Replace(2, make_THROW("THROWIFNOT " + cmd2CFI->arg()));
		if (isCFI(cmd2, "THROWIFNOT"))
			return Result::Replace(2, make_THROW("THROWIF " + cmd2CFI->arg()));
		if (cmd2IfElse)
			return Result::Replace(2, makeRevert(*cmd2IfElse));
	}
	// NEQINT 0, THROWIF N => THROWIF N
	// NEQINT 0, THROWIFNOT N => THROWIFNOT N
	if (is(cmd1, "NEQINT") && cmd1GenOp->arg() == "0") {
		if (isCFI(cmd2, "THROWIF"))
			return Result::Replace(2, make_THROW("THROWIF " + cmd2CFI->arg()));
		if (isCFI(cmd2, "THROWIFNOT"))
			return Result::Replace(2, make_THROW("THROWIFNOT " + cmd2CFI->arg()));
		if (cmd2IfElse)
			return Result::Replace(2, cmd2);
	}

	// ROT
	// DROP
	// =>
	// BLKDROP 1, 2
	if (isRot(cmd1) && is_drop_kind(cmd2) && get_drop_qty(cmd2) == 1) {
		return Result::Replace(2, make_BLKDROP2(1, 2));
	}

	// ROTREV
	// DROP2
	// =>
	// BLKDROP 2, 1
	if (isRotRev(cmd1) && is_drop_kind(cmd2) && get_drop_qty(cmd2) == 2) {
		return Result::Replace(2, make_BLKDROP2(2, 1));
	}

	// ROT ROTREV =>
	// ROTREV ROT =>
	if (
		(isRot(cmd1) && isRotRev(cmd2)) ||
		(isRotRev(cmd1) && isRot(cmd2))
	) {
		return Result::Replace(2);
	}
	// ROT ROT ROT =>
	if (isRot(cmd1) && isRot(cmd2) && isRot(cmd3)) {
		return Result::Replace(3);
	}
	// ROTREV ROTREV ROTREV =>
	if (isRotRev(cmd1) && isRotRev(cmd2) && isRotRev(cmd3)) {
		return Result::Replace(3);
	}
	if (is_PUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3) == "0")
	{
		return Result::Replace(3,
							   gen("PUSHINT " + toString(pushint_value(cmd1) + 1)),
							   gen("STZEROES"));
	}
	if (is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "STSLICECONST")) {
		std::vector<std::string> opcodes = StackPusherHelper::unitSlices(arg(cmd1), arg(cmd4));
		if (opcodes.size() == 1) {
			return Result::Replace(4,
								   gen("PUSHSLICE " + opcodes[0]),
								   gen("NEWC"),
								   gen("STSLICE"));
		}
	}
	if (is(cmd1, "PUSHSLICE") &&
		is(cmd2, "STSLICER") &&
		is(cmd3, "STSLICECONST")) {
		std::vector<std::string> opcodes = StackPusherHelper::unitSlices(arg(cmd1), arg(cmd3));
		if (opcodes.size() == 1) {
			return Result::Replace(3,
								   gen("PUSHSLICE " + opcodes[0]),
								   gen("STSLICER"));
		}
	}
	if (is_PUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3).length() > 1) {
		std::string::size_type intValue = static_cast<std::string::size_type>(pushint_value(cmd1));
		std::vector<std::string> opcodes = StackPusherHelper::unitBitString(std::string(intValue, '0'),
											StackPusherHelper::toBitString(arg(cmd3)));
		if (opcodes.size() == 1) {
			return Result::Replace(3,
								   gen("PUSHSLICE " + opcodes[0]),
								   gen("STSLICER"));
		}
	}
	if (is(cmd1, "STSLICECONST") &&
		is(cmd2, "STSLICECONST")) {
		std::vector<std::string> opcodes = StackPusherHelper::unitSlices(arg(cmd1), arg(cmd2));
		if (opcodes.size() == 1 && StackPusherHelper::toBitString(opcodes[0]).length() <= TvmConst::MaxSTSLICECONST) {
			return Result::Replace(2, gen("STSLICECONST " + opcodes[0]));
		}
	}
	if (is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICECONST") &&
		is(cmd4, "STSLICE")) {
		std::vector<std::string> opcodes = StackPusherHelper::unitSlices(arg(cmd3), arg(cmd1));
		if (opcodes.size() == 1) {
			return Result::Replace(4,
								   gen("PUSHSLICE " + opcodes[0]),
								   gen("NEWC"),
								   gen("STSLICE"));
		}
	}
	if (is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "PUSHSLICE") &&
		is(cmd5, "STSLICER")) {
		std::vector<std::string> opcodes = StackPusherHelper::unitSlices(arg(cmd1), arg(cmd4));
		if (opcodes.size() == 1) {
			return Result::Replace(5,
								   gen("PUSHSLICE " + opcodes[0]),
								   gen("NEWC"),
								   gen("STSLICE"));
		}
	}
	if (is(cmd1, "TUPLE") &&
		is(cmd2, "UNTUPLE") &&
		fetch_int(cmd1) == fetch_int(cmd2)) {
		return Result::Replace(2);
	}
	if (is(cmd1, "PAIR") &&
		is(cmd2, "UNPAIR")) {
		return Result::Replace(2);
	}
	if (cmd1Glob && cmd1Glob->opcode() == Glob::Opcode::SetOrSetVar &&
		cmd2Glob && cmd2Glob->opcode() == Glob::Opcode::GetOrGetVar &&
		cmd1Glob->index() == cmd2Glob->index()) {
		return Result::Replace(2,
							   make_PUSH(0),
							   make_setGlob(cmd1Glob->index()));
	}
	if (is_const_add(cmd1) && is_const_add(cmd2)) {
		int final_add = get_add_num(cmd1) + get_add_num(cmd2);
		if (-128 <= final_add && final_add <= 127)
			return Result::Replace(2, gen("ADDCONST " + std::to_string(final_add)));
	}
	// ADDCONST/INC/DEC
	// UFIT/FIT N
	// ADDCONST/INC/DEC
	// UFIT/FIT N
	// =>
	// ADDCONST
	// UFIT/FIT N
	if (is_const_add(cmd1) && is_const_add(cmd3)) {
		for (std::string fit : {"UFITS", "FITS"}) {
			if (is(cmd2, fit) && is(cmd4, fit) && arg(cmd2) == arg(cmd4)) {
				int final_add = get_add_num(cmd1) + get_add_num(cmd3);
				if (-128 <= final_add && final_add <= 127)
					return Result::Replace(4,
										   gen("ADDCONST " + std::to_string(final_add)),
										   gen(fit + " " + arg(cmd2)));
			}
		}
	}
	if (is(cmd1, "INDEX") && 0 <= strToInt(arg(cmd1)) && strToInt(arg(cmd1)) <= 3 &&
		is(cmd2, "INDEX") && 0 <= strToInt(arg(cmd2)) && strToInt(arg(cmd2)) <= 3 &&
		is(cmd3, "INDEX") && 0 <= strToInt(arg(cmd3)) && strToInt(arg(cmd3)) <= 3) {
		return Result::Replace(3, gen("INDEX3 " + arg(cmd1) + ", " + arg(cmd2) + ", " + arg(cmd3)));
	}
	if (is(cmd1, "INDEX") && 0 <= strToInt(arg(cmd1)) && strToInt(arg(cmd1)) <= 3 &&
		is(cmd2, "INDEX") && 0 <= strToInt(arg(cmd2)) && strToInt(arg(cmd2)) <= 3) {
		return Result::Replace(2, gen("INDEX2 " + arg(cmd1) + ", " + arg(cmd2)));
	}
	if (
		is_PUSHINT(cmd1) && 1 <= pushint_value(cmd1) && pushint_value(cmd1) <= 256 &&
		(is(cmd2, "RSHIFT") || is(cmd2, "LSHIFT")) && arg(cmd2) == ""
	) {
		return Result::Replace(2, gen(cmd2GenOpcode->opcode() + " " + arg(cmd1)));
	}
	if (is_PUSHINT(cmd1) &&
		(is(cmd2, "DIV") || is(cmd2, "MUL"))) {
		bigint val = pushint_value(cmd1);
		if (power2.count(val)) {
			const std::string& newOp = is(cmd2, "DIV") ? "RSHIFT" : "LSHIFT";
			return Result::Replace(2, gen(newOp + " " + toString(power2.at(val))));
		}
	}
	if (is_PUSHINT(cmd1) &&
		is(cmd2, "MOD")) {
		bigint val = pushint_value(cmd1);
		if (power2.count(val)) {
			return Result::Replace(2, gen("MODPOW2 " + toString(power2.at(val))));
		}
	}
	if (is_PUSHINT(cmd1)) {
		bigint val = pushint_value(cmd1);
		if (-128 <= val && val < 128) {
			if (is(cmd2, "NEQ"))
				return Result::Replace(2, gen("NEQINT " + toString(val)));
			if (is(cmd2, "EQUAL"))
				return Result::Replace(2, gen("EQINT " + toString(val)));
			if (is(cmd2, "GREATER"))
				return Result::Replace(2, gen("GTINT " + toString(val)));
			if (is(cmd2, "LESS"))
				return Result::Replace(2, gen("LESSINT " + toString(val)));
		}
		if (-128 <= val - 1 && val - 1 < 128 && is(cmd2, "GEQ"))
			return Result::Replace(2, gen("GTINT " + toString(val - 1)));
		if (-128 <= val + 1 && val + 1 < 128 && is(cmd2, "LEQ"))
			return Result::Replace(2, gen("LESSINT " + toString(val + 1)));
	}
	if (is_PUSHINT(cmd1) && (is_PUSH(cmd2) || isPureGen01OrGetGlob(cmd2))) {
		bigint val = pushint_value(cmd1);
		if (-128 <= val && val < 128) {
			if (is(cmd3, "NEQ"))
				return Result::Replace(3, cmd2, gen("NEQINT " + toString(val)));
			if (is(cmd3, "EQUAL"))
				return Result::Replace(3, cmd2, gen("EQINT " + toString(val)));
			if (is(cmd3, "GREATER"))
				return Result::Replace(3, cmd2, gen("LESSINT " + toString(val)));
			if (is(cmd3, "LESS"))
				return Result::Replace(3, cmd2, gen("GTINT " + toString(val)));
		}
		if (-128 <= val + 1 && val + 1 < 128 && is(cmd3, "GEQ"))
			return Result::Replace(3, cmd2, gen("LESSINT " + toString(val + 1)));
		if (-128 <= val - 1 && val - 1 < 128 && is(cmd3, "LEQ"))
			return Result::Replace(3, cmd2, gen("GTINT " + toString(val - 1)));
	}

	// BLKSWAP N, 1
	// BLKSWAP N, 1
	// BLKSWAP N, 1
	// ...
	// BLKSWAP N, 1
	// =>
	//
	if (isStack(cmd1, Stack::Opcode::BLKSWAP)) {
		int n = cmd1Stack->i();
		bool ok = true;
		for (int iter = 0; iter < n + 1; ++iter) {
			if (get(idx1 + iter) == nullptr) {
				ok = false;
				break;
			}
			auto c = get(idx1 + iter);
			auto cStack = to<Stack>(c.get());
			ok &= isStack(c, Stack::Opcode::BLKSWAP) && cStack->i() == n && cStack->j() == 1;
		}
		if (ok) {
			return Result::Replace(n + 1);
		}
	}

	if (is_PUSHINT(cmd1) &&
		is_PUSHINT(cmd2) &&
		cmd3GenOpcode && cmd3GenOpcode->opcode() == "MUL"
	) {
		bigint a = pushint_value(cmd1);
		bigint b = pushint_value(cmd2);
		bigint c = a * b;
		return Result::Replace(3, gen("PUSHINT " + toString(c)));
	}

	if (is_PUSHINT(cmd1) &&
		is_PUSHINT(cmd2) &&
		cmd3GenOpcode && cmd3GenOpcode->opcode() == "DIV"
	) {
		bigint a = pushint_value(cmd1);
		bigint b = pushint_value(cmd2);
		if (a >= 0 && b > 0) { // note in TVM  -9 / 2 == -5, TODO handle these cases
			bigint c = a / b;
			return Result::Replace(3, gen("PUSHINT " + toString(c)));
		}
	}

	if (isBLKDROP1 && isBLKDROP2) {
		auto [drop1, rest1] = isBLKDROP1.value();
		auto [drop2, rest2] = isBLKDROP2.value();
		// BLKDROP2 drop0, rest
		// BLKDROP2 drop1, rest
		// =>
		// BLKDROP2 drop0+drop1, rest
		if (rest1 == rest2 && drop1 + drop2 <= 15) {
			return Result::Replace(2, make_BLKDROP2(drop1 + drop2, rest1));
		}
		// BLKDROP2 drop1, rest1
		// BLKDROP2 drop2, rest2
		// =>
		// BLKDROP2 drop1+drop2, rest1
		if (rest1 == drop2 + rest2 && rest1 >= rest2) {
			return Result::Replace(2, make_BLKDROP2(drop1 + drop2, rest2));
		}
	}

	// BLKSWAP bottom, top
	// BLKDROP bottom
	// =>
	// BLKDROP2 bottom, top
	if (isStack(cmd1, Stack::Opcode::BLKSWAP) &&
		is_drop_kind(cmd2)
	) {
		int a1 = cmd1Stack->i();
		int b1 = cmd1Stack->j();
		int a2 = get_drop_qty(cmd2);
		if (a1 == a2) {
			return Result::Replace(2, make_BLKDROP2(a1, b1));
		}
	}

	if (is(cmd1, "MUL") && is(cmd2, "RSHIFT") && arg(cmd2) != " ") {
		return Result::Replace(2, gen("MULRSHIFT " + arg(cmd2)));
	}

	if (is(cmd1, "NEWC") && is(cmd2, "ENDC")) {
		return Result::Replace(2, make_PUSHREF());
	}

	// POP N
	// POP N
	// ...
	// =>
	// BLKDROP2 N, N
	if (is_POP(cmd1)) {
		int n = get_pop_index(cmd1);
		if (n >= 2) {
			int index = idx1;
			bool ok = true;
			int i = 0;
			for (; ok && i < n && index != -1; ++i, index = next_command_line(index)) {
				auto cmd = get(index);
				ok &= is_POP(cmd) && get_pop_index(cmd) == n;
			}
			ok &= i == n;
			if (ok) {
				return Result::Replace(n, make_BLKDROP2(n, n));
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
	if (isREVERSE(cmd1)) {
		auto [n, startIndex] = isREVERSE(cmd1).value();
		if (startIndex == 0) {
			vector<Pointer<TvmAstNode>> newCmds;
			int index = idx2;
			bool ok = true;
			int i = 0;
			for (; ok && i < n && index != -1; ++i, index = next_command_line(index)) {
				auto cmd = get(index);
				ok &= is_POP(cmd);
				newCmds.push_back(cmd);
			}
			ok &= i == n;
			if (ok) {
				std::reverse(newCmds.begin(), newCmds.end());
				std::set<int> uniqInds;
				int deltaSi = n - 1;
				for (i = 0; i < n; ++i) {
					int si = get_pop_index(newCmds[i]);
					int newSi = si + deltaSi;
					newCmds[i] = make_POP(newSi);
					deltaSi -= 2;
					ok &= newSi >= n - i;
					uniqInds.insert(newSi + i);
				}
				ok &= static_cast<int>(uniqInds.size()) == n;
				if (ok) {
					return Result(true, n + 1, newCmds);
				}
			}
		}
	}

	if (is(cmd1, "NOT") &&
		is(cmd2, "NOT")
	) {
		return Result::Replace(2);
	}

	if (is_PUSHINT(cmd1) && is_PUSHINT(cmd1) && pushint_value(cmd1) == 0 &&
		is(cmd2, "STUR") &&
		is_PUSHINT(cmd3) && is_PUSHINT(cmd3) && pushint_value(cmd3) == 0 &&
		is(cmd4, "STUR")
	) {
		int bitSize = fetch_int(cmd2) + fetch_int(cmd4);
		if (bitSize <= 256)
			return Result::Replace(4, gen("PUSHINT 0"), gen("STUR " + toString(bitSize)));
	}

	if ((is(cmd1, "UFITS") && is(cmd2, "UFITS")) || (is(cmd1, "FITS") && is(cmd2, "FITS"))) {
		int bitSize = std::min(fetch_int(cmd1), fetch_int(cmd2));
		return Result::Replace(2, gen(cmd1GenOp->opcode() + " " + toString(bitSize)));
	}

	if (is_PUSHINT(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICECONST") &&
		is(cmd4, "STU")) {
		std::string bitStr = StackPusherHelper::toBitString(arg(cmd3));
		StackPusherHelper::addBinaryNumberToString(bitStr, pushint_value(cmd1), fetch_int(cmd4));
		std::vector<std::string> slices = StackPusherHelper::unitBitString(bitStr, "");
		if (slices.size() == 1) {
			return Result::Replace(4,
				gen("PUSHSLICE " + slices.at(0)),
				gen("NEWC"),
				gen("STSLICE"));
		}
	}

	if (is_PUSHINT(cmd1) &&
		is(cmd2, "PUSHSLICE") &&
		is(cmd3, "NEWC") &&
		is(cmd4, "STSLICE") &&
		is(cmd5, "STU")
	) {
		std::string bitStr = StackPusherHelper::toBitString(arg(cmd2));
		StackPusherHelper::addBinaryNumberToString(bitStr, pushint_value(cmd1), fetch_int(cmd5));
		std::vector<std::string> slices = StackPusherHelper::unitBitString(bitStr, "");
		if (slices.size() == 1) {
			return Result::Replace(5,
					gen("PUSHSLICE " + slices.at(0)),
					gen("NEWC"),
					gen("STSLICE"));
		}
	}

	if (is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		(is(cmd4, "STONE") || is(cmd4, "STZERO"))
	) {
		std::string bitStr = StackPusherHelper::toBitString(arg(cmd1));
		bitStr += is(cmd4, "STONE") ? "1" : "0";
		std::vector<std::string> slices = StackPusherHelper::unitBitString(bitStr, "");
		if (slices.size() == 1) {
			return Result::Replace(4,
					gen("PUSHSLICE " + slices.at(0)),
					gen("NEWC"),
					gen("STSLICE"));
		}
	}

	if (is_PUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is_PUSHINT(cmd3) &&
		is(cmd4, "STZEROES")
	) {
		int bitQty = fetch_int(cmd1) + fetch_int(cmd3);
		return Result::Replace(4,
				gen("PUSHINT " + toString(bitQty)),
				gen("STZEROES"));
	}

	if (is_PUSHINT(cmd1) &&
		is(cmd2, "STUR") &&
		is_PUSHINT(cmd3) &&
		is(cmd4, "STUR")
	) {
		bigint a = pushint_value(cmd1);
		int lenA = fetch_int(cmd2);
		bigint b = pushint_value(cmd3);
		int lenB = fetch_int(cmd4);
		if (lenA + lenB <= 256) {
			bigint c = (a << lenB) + b;
			return Result::Replace(4,
				gen("PUSHINT " + toString(c)),
				gen("STUR " + toString(lenA + lenB))
			);
		}
	}

	if (is_PUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is(cmd3, "STSLICECONST") && arg(cmd3) == "1"
	) {
		int lenA = fetch_int(cmd1);
		if (lenA <= 256) {
			return Result::Replace(3,
				gen("PUSHINT 1"),
				gen("STUR " + toString(lenA + 1))
			);
		}
	}

	if ((is(cmd1, "TRUE") || is(cmd1, "FALSE")) &&
		is(cmd2, "STIR") && fetch_int(cmd2) == 1
	) {
		if (is(cmd1, "FALSE"))
			return Result::Replace(2, gen("STZERO"));
		return Result::Replace(2, gen("STONE"));
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
			i = next_command_line(i);
		}
		if (qty >= 2) {
			std::vector<std::string> slices = StackPusherHelper::unitBitString(bits, "");
			solAssert(slices.size() == 1, "");
			return Result::Replace(qty, gen("STSLICECONST " + slices.at(0)));
		}

	}

	if (
		is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "NEWC") &&
		is(cmd5, "STSLICECONST") &&
		is(cmd6, "STB")
	) {
		std::string str1 = StackPusherHelper::toBitString(arg(cmd1));
		std::string str5 = StackPusherHelper::toBitString(arg(cmd5));
		std::vector<std::string> slices = StackPusherHelper::unitBitString(str5, str1);
		if (slices.size() == 1) {
			return Result::Replace(6,
				gen("PUSHSLICE " + slices.at(0)),
				gen("NEWC"),
				gen("STSLICE"));
		}
	}

	if (
		is(cmd1, "STSLICECONST") && arg(cmd1) == "0" &&
		is_PUSHINT(cmd2) && pushint_value(cmd2) == 0 &&
		is(cmd3, "STUR")
	) {
		int bitQty = fetch_int(cmd3);
		return Result::Replace(3,
			gen("PUSHINT 0"),
			gen("STUR " + toString(bitQty + 1)));
	}

	if (
		is_PUSHINT(cmd1) &&
		is(cmd2, "STZEROES") &&
		is_PUSHINT(cmd3) &&
		is(cmd4, "STUR")
	) {
		bigint bitQty = pushint_value(cmd1) + fetch_int(cmd4);
		if (bitQty <= 256) {
			return Result::Replace(4,
				gen("PUSHINT " + arg(cmd3)),
				gen("STUR " + toString(bitQty)));
		}
	}

	if (
		is_PUSHINT(cmd1) && pushint_value(cmd1) == 0 &&
		is(cmd2, "STUR")
	) {
		return Result::Replace(2,
			gen("PUSHINT " + arg(cmd2)),
			gen("STZEROES"));
	}

	if (
		is_PUSHINT(cmd1) &&
		is(cmd2, "STUR") && fetch_int(cmd2) <= 8
	) {
		std::string s;
		StackPusherHelper::addBinaryNumberToString(s, pushint_value(cmd1), fetch_int(cmd2));
		s = StackPusherHelper::binaryStringToSlice(s);
		return Result::Replace(2, gen("STSLICECONST x" + s));
	}

	if (
		is(cmd1, "ABS") &&
		is(cmd2, "UFITS") && fetch_int(cmd2) == 256
	) {
		return Result::Replace(2, gen("ABS"));
	}

	if (
		is_PUSHINT(cmd1) && pushint_value(cmd1) == 1 &&
		is(cmd2, "STZEROES")
	) {
		return Result::Replace(2, gen("STZERO"));
	}

	if (
		isStack(cmd1, Stack::Opcode::REVERSE) && cmd1Stack->i() == 2 && cmd1Stack->j() == 1 &&
		isRotRev(cmd2)
	) {
		return Result::Replace(2, make_XCH_S(2));
	}

	// REVERSE N, 1
	// BLKSWAP N, 1
	// =>
	// REVERSE N+1, 0
	if (isREVERSE(cmd1) && isBLKSWAP(cmd2)) {
		auto [qty, index] = isREVERSE(cmd1).value();
		auto [bottom, top] = isREVERSE(cmd1).value();
		if (top == 1 && index == 1 && qty == bottom)
			return Result::Replace(2, make_reverse(qty + 1, 0));
	}

	if (
		is(cmd1, "NEWC") &&
		is(cmd2, "STSLICECONST") && arg(cmd2).length() > 1 &&
		is(cmd3, "ENDC")
	) {
		return Result::Replace(3, make_PUSHREF(".blob " + arg(cmd2)));
	}

	if (
		is(cmd1, "PUSHSLICE") &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "ENDC")
	) {
		return Result::Replace(4, make_PUSHREF(".blob " + arg(cmd1)));
	}

	if (
		is(cmd1, "ENDC") &&
		is(cmd2, "STREFR")
	) {
		return Result::Replace(2, gen("STBREFR"));
	}

	// SWAP
	// s01
	// SWAP
	// =>
	// s01
	// ROT
	if (is_SWAP(cmd1) &&
		is_simple_command(cmd2, 0, 1) &&
		is_SWAP(cmd3)
	) {
		return Result::Replace(3, cmd2, makeROT());
	}

	// SWAP
	// ROT
	// =>
	// XCHG S2
	if (is_SWAP(cmd1) &&
		isRot(cmd2)
	) {
		return Result::Replace(2, make_XCH_S(2));
	}

	// ROT
	// SWAP
	// =>
	// XCHG S1, S2
	if (isRot(cmd1) &&
		is_SWAP(cmd2)
	) {
		return Result::Replace(2, make_XCH_S_S(1, 2));
	}

	// XCHG s2
	// SWAP
	// =>
	// ROTREV
	if (isStack(cmd1, Stack::Opcode::XCHG_S0) && cmd1Stack->i() == 2 &&
		is_SWAP(cmd2)
	) {
		return Result::Replace(2, makeROTREV());
	}

	// XCHG s2
	// ROTREV
	// =>
	// SWAP
	if (isStack(cmd1, Stack::Opcode::XCHG_S0) && cmd1Stack->i() == 2 &&
		isRotRev(cmd2)
	) {
		return Result::Replace(2, make_XCH_S(1));
	}

	// SWAP2
	// DROP2
	// =>
	// BLKDROP2 2 2
	if (isStack(cmd1, Stack::Opcode::SWAP2) &&
		is_drop_kind(cmd2) && get_drop_qty(cmd2) == 2
	) {
		return Result::Replace(2, make_BLKDROP2(2, 2));
	}


	return Result(false);
}

// chech whether
// * S[stack_size - 1] isn't used at all:
//   * S[stack_size - 1] isn't copied (PUSH Si or BLKPUSH)
//   * S[stack_size - 1] isn't set (POP Si, i >= 2)
//   * S[stack_size - 1] isn't used in simple operations
//   * and another operations
// * S[stack_size - 1] is removed by (DROP_N or NIP)
bool TVMOptimizerPrivate::try_simulate(int i, int stack_size, int& remove_count, vector<Pointer<TvmAstNode>>& commands) const {
	solAssert(stack_size >= 1, "");

	if (!valid(i))
		return false;

	bool first_time = true;
	int prev = -1;
	while (true) {
		if (first_time) {
			first_time = false;
		} else {
			remove_count++;
			prev = i;
			i = next_command_line(i);
		}

		auto addLoc = [&](Pointer<TvmAstNode> node) {
			if (prev != -1) {
				for (int index = prev + 1; index < i; ++index) {
					commands.emplace_back(m_instructions.at(index));
				}
			}
			commands.emplace_back(node);
		};

		if (!valid(i))
			return false;

		Pointer<TvmAstNode> c = get(i);
		Stack const* cStack = to<Stack>(c.get());
		if (is_PUSH(c)) {
			if (get_push_index(c) == stack_size - 1)
				return false;
			if (get_push_index(c) < stack_size - 1) {
				addLoc(c);
			} else if (get_push_index(c) > stack_size - 1) {
				solAssert(get_push_index(c) != 0, "");
				addLoc(make_PUSH(get_push_index(c) - 1));
			}
			++stack_size;
			continue;
		}
		if (is_POP(c) && get_pop_index(c) != 1) {
			solAssert(get_pop_index(c) != 0, "");
			if (stack_size == 1)
				return false;
			if (get_pop_index(c) == stack_size - 1)
				return false;
			if (get_pop_index(c) < stack_size - 1) {
				addLoc(c);
			} else if (get_pop_index(c) > stack_size - 1) {
				addLoc(make_POP(get_pop_index(c) - 1));
			}
			--stack_size;
			continue;
		}
		if (cStack) {
			// check whether the topmost elements are not touched
			if (isStack(c, Stack::Opcode::BLKPUSH)) {
				int si = cStack->j();
				if (si < stack_size - 1) {
					addLoc(c);
					stack_size += cStack->i();
					continue;
				}
			}
			// TODO: support other pushes not touching topmost element...
		}
		if (is_NIP(c)) {
			if (stack_size == 2) {
				remove_count++;
				break;
			}
			if (stack_size > 2) {
				stack_size--;
				addLoc(c);
				continue;
			}
			return false;
		}
		if (is_drop_kind(c)) {
			int n = get_drop_qty(c);
			if (n >= stack_size) {
				if (n > 1) {
					addLoc(make_DROP(n - 1));
				}
				remove_count++;
				break;
			} else {
				addLoc(c);
				stack_size -= n;
				continue;
			}
		}
		if (isStack(c, Stack::Opcode::BLKDROP2)) {
			int drop = cStack->i();
			int rest = cStack->j();
			if (stack_size > rest + drop) {
				addLoc(c);
				stack_size -= drop;
				continue;
			}
			if (stack_size <= rest) {
				if (rest == 1)
					addLoc(make_DROP(drop));
				else
					addLoc(make_BLKDROP2(drop, rest - 1));
				continue;
			}
			if (rest < stack_size && stack_size <= rest + drop) {
				if (drop > 1) {
					addLoc(make_BLKDROP2(drop - 1, rest));
				}
				remove_count++;
				break;
			}
		}
		if (auto optPair = check_simple_command(c)) {
			auto [take, ret] = optPair.value();
			if (stack_size <= take)
				return false;
			addLoc(c);
			stack_size += ret - take;
			continue;
		}
		return false;
	}
	return true;
}

Result TVMOptimizerPrivate::unsquash_push(const int idx1) const {
	auto c = get(idx1);
	auto stack = to<Stack>(c.get());
	if (isStack(c, Stack::Opcode::PUSH2_S)) {
		int si = stack->i();
		int sj = stack->j() + 1;
		return Result::Replace(1, make_PUSH(si), make_PUSH(sj));
	}
	return Result(false);
}

Result TVMOptimizerPrivate::squash_push(const int idx1) const {
	int idx2 = next_command_line(idx1);
	int idx3 = next_command_line(idx2);
	Pointer<TvmAstNode> cmd1 = get(idx1);
	Pointer<TvmAstNode> cmd2 = get(idx2);
	Pointer<TvmAstNode> cmd3 = get(idx3);
	if (is_PUSH(cmd1) && is_PUSH(cmd2) && is_PUSH(cmd3)) {
		const int si = get_push_index(cmd1);
		const int sj = get_push_index(cmd2) - 1 == -1? si : get_push_index(cmd2) - 1;
		const int sk = get_push_index(cmd3) - 2 == -1? si : (
				get_push_index(cmd3) - 2 == -2? sj : get_push_index(cmd3) - 2
		);
		if (si <= 15 && sj <= 15 && sk <= 15) {
			return Result::Replace(3, make_PUSH3(si, sj, sk));
		}
	}
	if (is_PUSH(cmd1) && is_PUSH(cmd2)) {
		if (get_push_index(cmd1) == 1 && get_push_index(cmd2) == 1) {
			return Result::Replace(2, make_DUP2());
		}
		const int si = get_push_index(cmd1);
		const int sj = get_push_index(cmd2) - 1 == -1? si : get_push_index(cmd2) - 1;
		if (si <= 15 && sj <= 15) {
			return Result::Replace(2, make_PUSH2(si, sj));
		}
	}
	if (isStack(cmd1, Stack::Opcode::BLKPUSH)) {
		auto blockPush = to<Stack>(cmd1.get());
		if (blockPush->i() == 2 && blockPush->j() == 1) {
			return Result::Replace(1, make_DUP2());
		}
		if (blockPush->i() == 2 && blockPush->j() == 3) {
			return Result::Replace(1, make_OVER2());
		}
	}

	return Result(false);
}


bool TVMOptimizerPrivate::updateLinesAndIndex(int& idx1, const Result& res) {
	if (res.remove_ > 0) {
		solAssert(valid(idx1), "");
		solAssert(!isLoc(m_instructions.at(idx1)), "");
		int lastInx = idx1;
		for (int iter = 0; iter + 1 < res.remove_; ++iter) {
			lastInx = next_command_line(lastInx);
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
		for (Pointer<TvmAstNode> const& inst : res.commands_) {
			m_instructions.emplace_back(inst);
		}
		// insert .loc if it presents
		if (locLine != nullptr) {
			m_instructions.push_back(locLine);
		}
		// insert the tail
		m_instructions.insert(m_instructions.end(), codeTail.begin(), codeTail.end());
	}

	if (res.continue_) {
		// step back to several commands
		int cnt = 10, i = idx1;
		while (cnt > 0) {
			i--;
			if (!valid(i)) break;
			if (!isLoc(m_instructions[i])) cnt--;
			idx1 = i;
		}
		return true;
	}

	return false;
}

void TVMOptimizerPrivate::optimize(const std::function<Result(int)> &f) {
	int idx1 = 0;
	while (valid(idx1)) {
		Result res = f(idx1);
		if (updateLinesAndIndex(idx1, res)) {
			continue;
		}
		idx1 = next_command_line(idx1);
	}
}

std::optional<std::pair<int, int>> TVMOptimizerPrivate::getBLKDROP2(Pointer<TvmAstNode> node) const {
	if (isStack(node, Stack::Opcode::BLKDROP2)) {
		auto stack = to<Stack>(node.get());
		return {{stack->i(), stack->j()}};
	}
	if (isStack(node, Stack::Opcode::POP_S)) {
		auto stack = to<Stack>(node.get());
		if (stack->i() == 1)
			return {{1, 1}};
	}
	return {};
}

int TVMOptimizerPrivate::qtyWithoutLoc(std::vector<Pointer<TvmAstNode>> arr) const{
	int qty{};
	for (Pointer<TvmAstNode> node : arr) {
		if (!to<Loc>(node.get())){
			++qty;
		}
	}
	return qty;
}

bool TVMOptimizerPrivate::is_SWAP(Pointer<TvmAstNode> node) const {
	auto swap = dynamic_pointer_cast<Stack>(node);
	return isStack(node, Stack::Opcode::XCHG_S0) && swap->i() == 1;
}

bool TVMOptimizerPrivate::is_PUSH(Pointer<TvmAstNode> node) const {
	return isStack(node, Stack::Opcode::PUSH_S);
}

bigint TVMOptimizerPrivate::pushint_value(Pointer<TvmAstNode> node) const {
	solAssert(is_PUSHINT(node), "");
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return bigint{g->arg()};
}


int TVMOptimizerPrivate::fetch_int(Pointer<TvmAstNode> node) const {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return strToInt(g->arg());
}

int TVMOptimizerPrivate::get_push_index(Pointer<TvmAstNode> node) const {
	solAssert(is_PUSH(node), "");
	auto push = dynamic_pointer_cast<Stack>(node);
	return push->i();
}

bool TVMOptimizerPrivate::is_drop_kind(Pointer<TvmAstNode> node) const {
	auto stack = dynamic_pointer_cast<Stack>(node);
	return stack && isIn(stack->opcode(), Stack::Opcode::DROP);
}

int TVMOptimizerPrivate::get_drop_qty(Pointer<TvmAstNode> node) const {
	solAssert(is_drop_kind(node), "");
	auto stack = dynamic_pointer_cast<Stack>(node);
	switch (stack->opcode()) {
		case Stack::Opcode::DROP:
			return stack->i();
		default:
			solUnimplemented("");
	}
	solUnimplemented("");
}

bool TVMOptimizerPrivate::is_POP(Pointer<TvmAstNode> node) const {
	return isStack(node, Stack::Opcode::POP_S);
}

int TVMOptimizerPrivate::get_pop_index(Pointer<TvmAstNode> node) const {
	solAssert(is_POP(node), "");
	auto pop = dynamic_pointer_cast<Stack>(node);
	return pop->i();
}

bool TVMOptimizerPrivate::is_NIP(Pointer<TvmAstNode> node) const {
	auto swap = dynamic_pointer_cast<Stack>(node);
	return isStack(node, Stack::Opcode::POP_S) && swap->i() == 1;
}

bool TVMOptimizerPrivate::is_PUSHINT(Pointer<TvmAstNode> node) const {
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

std::string TVMOptimizerPrivate::arg(Pointer<TvmAstNode> node) const {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	solAssert(g, "");
	return g->arg();
}

bool TVMOptimizerPrivate::is(Pointer<TvmAstNode> node, std::string opcode) const {
	auto g = to<GenOpcode>(node.get());
	return g && g->opcode() == opcode;
}

// down, top
std::optional<std::pair<int, int>> TVMOptimizerPrivate::isBLKSWAP(Pointer<TvmAstNode> node) const {
	auto stack = to<Stack>(node.get());
	if (stack) {
		switch (stack->opcode()) {
			case Stack::Opcode::ROT:
				return {{1, 2}};
			case Stack::Opcode::ROTREV:
				return {{2, 1}};
			case Stack::Opcode::SWAP2:
				return {{2, 2}};
			case Stack::Opcode::BLKSWAP:
				return {{stack->i(), stack->j()}};
			default:
				break;
		}
	}
	return {};
}

// qty, index
std::optional<std::pair<int, int>> TVMOptimizerPrivate::isREVERSE(Pointer<TvmAstNode> node) const {
	auto stack = to<Stack>(node.get());
	if (stack) {
		switch (stack->opcode()) {
			case Stack::Opcode::XCHG_S0:
				if (stack->i() == 1)
					return {{2, 0}};
				break;
			case Stack::Opcode::REVERSE:
				return {{stack->i(), stack->j()}};
			default:
				break;
		}
	}
	return {};
}

template<class ...Args>
bool TVMOptimizerPrivate::isCFI(Pointer<TvmAstNode> node, Args&&... cmd) const {
	auto cfi = to<ConFlowInst>(node.get());
	return cfi && isIn(cfi->opcode(), std::forward<Args>(cmd)...);
}

bool TVMOptimizerPrivate::isRot(Pointer<TvmAstNode> node) const {
	return isStack(node, Stack::Opcode::ROT);
}

bool TVMOptimizerPrivate::isRotRev(Pointer<TvmAstNode> node) const {
	return isStack(node, Stack::Opcode::ROTREV);
}

bool TVMOptimizerPrivate::is_const_add(Pointer<TvmAstNode> node) const {
	auto gen = to<GenOpcode>(node.get());
	return gen && isIn(gen->opcode(), "INC", "DEC", "ADDCONST");
}

int TVMOptimizerPrivate::get_add_num(Pointer<TvmAstNode> node) const {
	solAssert(is_const_add(node), "");
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

bool TVMOptimizerPrivate::isStack(Pointer<TvmAstNode> node, Stack::Opcode op) const {
	auto stack = to<Stack>(node.get());
	return stack && stack->opcode() == op;
}

std::optional<std::pair<int, int>> TVMOptimizerPrivate::check_simple_command(Pointer<TvmAstNode> node) const {
	if (auto gen = to<Gen>(node.get())) {
		return {{gen->take(), gen->ret()}};
	}
	if (auto gl = to<Glob>(node.get())) {
		if (gl->opcode() == Glob::Opcode::GetOrGetVar) {
			return {{0, 1}};
		}
		if (gl->opcode() == Glob::Opcode::SetOrSetVar) {
			return {{1, 0}};
		}
	}
	if (auto stack = to<Stack>(node.get())) {
		if (is_SWAP(node)) {
			return {{2, 2}};
		}
		if (isIn(stack->opcode(), Stack::Opcode::ROT, Stack::Opcode::ROTREV)) {
			return {{3, 3}};
		}
	}
	// TODO add another Stack operations ?
	return {};
}

bool TVMOptimizerPrivate::isPureGen01OrGetGlob(Pointer<TvmAstNode> node) const {
	if (auto gen = to<Gen>(node.get())) {
		return gen->isPure() && std::make_pair(gen->take(), gen->ret()) == std::make_pair(0, 1);
	}
	auto gl = to<Glob>(node.get());
	if (gl && gl->opcode() == Glob::Opcode::GetOrGetVar) {
		return true;
	}
	return false;
}

bool TVMOptimizerPrivate::is_simple_command(Pointer<TvmAstNode> node, int take, int ret) const {
	std::optional<std::pair<int, int>> opt = check_simple_command(node);
	if (!opt) {
		return false;
	}
	return opt.value() == std::make_pair(take, ret);
}

bool TVMOptimizerPrivate::is_add_or_sub(Pointer<TvmAstNode> node) const {
	return is(node, "ADD") || is(node, "SUB");
}

bool TVMOptimizerPrivate::is_commutative(Pointer<TvmAstNode> node) const {
	auto g = dynamic_pointer_cast<GenOpcode>(node);
	return g && isIn(g->fullOpcode(),
					 "ADD",
					 "AND",
					 "EQUAL",
					 "MAX",
					 "MIN"
					 "MUL",
					 "NEQ",
					 "OR",
					 "XOR"
	);
}

void TVMOptimizer::endVisit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	TVMOptimizerPrivate optimizer{instructions};
	optimizer.optimize([&optimizer](int index){ return optimizer.unsquash_push(index);});

	optimizer.optimize([&optimizer](int index){ return optimizer.optimize_at(index);});
	optimizer.optimize([&optimizer](int index){ return optimizer.optimize_at(index);});
	optimizer.optimize([&optimizer](int index){ return optimizer.optimize_at(index);});

	optimizer.optimize([&optimizer](int index){ return optimizer.squash_push(index);});

	_node.upd(optimizer.instructions());
}

} // end solidity::frontend
