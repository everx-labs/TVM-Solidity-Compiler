/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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
 * Peephole optimizer
 */

#include <boost/format.hpp>

#include <libsolidity/ast/TypeProvider.h>

#include "PeepholeOptimizer.hpp"
#include "StackOpcodeSquasher.hpp"
#include "TVM.hpp"
#include "TVMConstants.hpp"
#include "TVMPusher.hpp"
#include "TvmAst.hpp"

using namespace std;
using namespace solidity::util;

namespace solidity::frontend {

bool PeepholeOptimizer::withBlockPush = false;

struct Result {

	int removeQty{};
	vector<Pointer<TvmAstNode>> commands{};


	template <class ...Args>
	explicit Result(int remove,  Args... cmds) :
		removeQty(remove), commands{cmds...}
	{
	}

	explicit Result(int remove, vector<Pointer<TvmAstNode>> commands = {}) :
		removeQty(remove), commands{std::move(commands)}
	{
	}
};

class PrivatePeepholeOptimizer {
public:
	explicit PrivatePeepholeOptimizer(std::vector<Pointer<TvmAstNode>> instructions, bool _withUnpackOpaque, bool _optimizeSlice, bool _withTuck) :
		m_instructions{std::move(instructions)},
		m_withUnpackOpaque{_withUnpackOpaque},
		m_optimizeSlice{_optimizeSlice},
		m_withTuck{_withTuck}
	{
	}
	vector<Pointer<TvmAstNode>> const &instructions() const { return m_instructions; }

	int nextCommandLine(int idx) const;
	static int nextCommandLine(int idx, std::vector<Pointer<TvmAstNode>> const& instructions);
	Pointer<TvmAstNode> get(int idx) const;
	bool valid(int idx) const;
	void remove(int idx);
	void insert(int idx, const Pointer<TvmAstNode>& node);
	std::optional<Result> optimizeAt(int idx1) const;
	std::optional<Result> optimizeSlice(int idx1) const;
	static std::optional<Result> optimizeAt1(Pointer<TvmAstNode> const& cmd1, bool m_withUnpackOpaque);
	static std::optional<Result> optimizeAt2(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, bool _withTuck);
	static std::optional<Result> optimizeAt3(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3, bool m_withUnpackOpaque);
	static std::optional<Result> optimizeAt4(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4);
	static std::optional<Result> optimizeAt5(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4, Pointer<TvmAstNode> const& cmd5);
	static std::optional<Result> optimizeAt6(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, Pointer<TvmAstNode> const& cmd3,
					   Pointer<TvmAstNode> const& cmd4, Pointer<TvmAstNode> const& cmd5, Pointer<TvmAstNode> const& cmd6);
	std::optional<Result> optimizeAtInf(int idx1) const;
	static bool hasRetOrJmp(TvmAstNode const* _node);

	void updateLinesAndIndex(int idx1, const std::optional<Result>& res);
	std::optional<Result> unsquash(bool _withUnpackOpaque, int idx1) const;
	std::optional<Result> squashPush(int idx1) const;
	bool optimize(const std::function<std::optional<Result>(int)> &f);

	static std::optional<std::pair<int, int>> isBLKDROP2(Pointer<TvmAstNode>const& node);
	static bigint pushintValue(Pointer<TvmAstNode> const& node);
	static int fetchInt(Pointer<TvmAstNode> const& node);
	static bool isNIP(Pointer<TvmAstNode> const& node);
	static std::string arg(Pointer<TvmAstNode> const& node);
	template<class ...Args>
	static bool is(Pointer<TvmAstNode> const& node, Args&&... cmd);
	static bool isSimpleCommand(Pointer<TvmAstNode> const& node);
	static bool isAddOrSub(Pointer<TvmAstNode> const& node);
	static bool isCommutative(Pointer<TvmAstNode> const& node);
	static std::pair<int, int> getIndexes(std::string const& str);

	template<class ...Args>
	static bool isExc(Pointer<TvmAstNode> const& node, Args&&... cmd);
	static bool isConstAdd(Pointer<TvmAstNode> const& node);
	static int getAddNum(Pointer<TvmAstNode> const& node);
	static bool isStack(Pointer<TvmAstNode> const& node, Stack::Opcode op);
private:
	std::vector<Pointer<TvmAstNode>> m_instructions{};
	bool m_withUnpackOpaque{};
	bool m_optimizeSlice{};
	bool m_withTuck{};
};

int PrivatePeepholeOptimizer::nextCommandLine(int idx) const {
	if (idx == -1) {
		return -1;
	}
	return nextCommandLine(idx + 1, m_instructions);
}

int PrivatePeepholeOptimizer::nextCommandLine(int idx, std::vector<Pointer<TvmAstNode>> const& instructions) {
	solAssert(0 <= idx, "");
	int n = instructions.size();
	while (idx < n) {
		if (idx >= n)
			return -1;
		if (!isLoc(instructions[idx]))
			return idx;
		idx++;
	}
	return -1;
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

std::optional<Result> PrivatePeepholeOptimizer::optimizeSlice(int idx1) const {
	int idx2 = nextCommandLine(idx1);
	Pointer<TvmAstNode> const &cmd1 = get(idx1);
	Pointer<TvmAstNode> const &cmd2 = get(idx2);

	// PUSHSLICE xXXX
	// STSLICE
	if (isPlainPushSlice(cmd1) && is(cmd2, "STSLICER")) {
		std::string const& slice = isPlainPushSlice(cmd1)->blob();
		std::string const& binStr = StrUtils::toBitString(slice);
		int len = binStr.length();
		// PUSHINT len
		// STZERO
		if (all_of(binStr.begin(), binStr.end(), [](char ch) { return ch == '0'; })) {
			return Result{2, gen("PUSHINT " + toString(len)), gen("STZEROES")};
		}
		// PUSHINT N
		// STUR len
		bigint num = StrUtils::toBigint(binStr);
		int numLength = StrUtils::toBinString(num).length();
		if (numLength <= 255 && len > numLength) {
			return Result{2, gen("PUSHINT " + toString(num)), gen("STUR " + toString(len))};
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt(const int idx1) const {
	std::optional<Result> res;
	if (m_optimizeSlice) {
		res = optimizeSlice(idx1);
		if (res) return res;
	}

	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	int idx4 = nextCommandLine(idx3);
	int idx5 = nextCommandLine(idx4);
	int idx6 = nextCommandLine(idx5);

	Pointer<TvmAstNode> const &cmd1 = get(idx1);
	Pointer<TvmAstNode> const &cmd2 = get(idx2);
	Pointer<TvmAstNode> const &cmd3 = get(idx3);
	Pointer<TvmAstNode> const &cmd4 = get(idx4);
	Pointer<TvmAstNode> const &cmd5 = get(idx5);
	Pointer<TvmAstNode> const &cmd6 = get(idx6);

	res = optimizeAt1(cmd1, m_withUnpackOpaque);
	if (res) return res;

	res = optimizeAtInf(idx1);
	if (res) return res;

	if (!cmd2) return {};
	res = optimizeAt2(cmd1, cmd2, m_withTuck);
	if (res) return res;

	if (!cmd3) return {};
	res = optimizeAt3(cmd1, cmd2, cmd3, m_withUnpackOpaque);
	if (res) return res;

	if (!cmd4) return {};
	res = optimizeAt4(cmd1, cmd2, cmd3, cmd4);
	if (res) return res;

	if (!cmd5) return {};
	res = optimizeAt5(cmd1, cmd2, cmd3, cmd4, cmd5);
	if (res) return res;

	if (!cmd6) return {};
	res = optimizeAt6(cmd1, cmd2, cmd3, cmd4, cmd5, cmd6);
	if (res) return res;

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt1(Pointer<TvmAstNode> const& cmd1, bool m_withUnpackOpaque) {
	auto cmd1CodeBlock = to<CodeBlock>(cmd1.get());
	auto cmd1GenOpcode = to<StackOpcode>(cmd1.get());
	auto cmd1IfElse = to<TvmIfElse>(cmd1.get());
	auto cmd1Sub = to<SubProgram>(cmd1.get());

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
					return Result{1, makeIFNOTRETALT()};
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
	if (m_withUnpackOpaque && cmd1IfElse && cmd1IfElse->falseBody() != nullptr && cmd1IfElse->ret() == 0) {
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
			auto ifElse2 = createNode<TvmIfElse>(cmd1IfElse->withNot(), false, tt, ff, 0);
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
			auto ifElse2 = createNode<TvmIfElse>(cmd1IfElse->withNot(), false, cmd1IfElse->trueBody(), nullptr, 0);
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
			auto ifElse2 = createNode<TvmIfElse>(!cmd1IfElse->withNot(), false, cmd1IfElse->falseBody(), nullptr, 0);
			return Result{1, ifElse2};
		}
	}

	// PUSHCONT { genA }
	// PUSHCONT { genB }
	// IFELSE
	// =>
	// genA
	// genB
	// CONDSEL
	if (cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& t = cmd1IfElse->trueBody()->instructions();
		std::vector<Pointer<TvmAstNode>> const& f = cmd1IfElse->falseBody()->instructions();
		if (qtyWithoutLoc(t) == 1 && qtyWithoutLoc(f) == 1) {
			int ti = nextCommandLine(0, t);
			int fi = nextCommandLine(0, f);
			Pointer<TvmAstNode> a = t.at(ti);
			Pointer<TvmAstNode> b = f.at(fi);
			if ((isPureGen01(*a) && to<StackOpcode>(a.get())) ||
				to<Glob>(a.get()) ||
				to<PushCellOrSlice>(a.get()) ||
				isPUSH(a)
			) {
				Pointer<TvmAstNode> newB;
				if (to<StackOpcode>(b.get()) ||
					to<Glob>(b.get()) ||
					to<PushCellOrSlice>(b.get())
				) {
					newB = b;
				} else if (auto index = isPUSH(b)) {
					newB = makePUSH(*index + 2); // +2 because flag and value from first branch
				}
				if (newB)
					return Result{1, a, newB, gen("CONDSEL")};
			}
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

	// PUSHCONT {
	//    CALLREF {
	//       code
	//    }
	// }
	// =>
	// PUSHREF {
	//    code
	// }
	if (cmd1CodeBlock && cmd1CodeBlock->type() == CodeBlock::Type::PUSHCONT) {
		std::vector<Pointer<TvmAstNode>> const&  opcodes = cmd1CodeBlock->instructions();
		if (qtyWithoutLoc(opcodes) == 1) {
			int index = nextCommandLine(0, opcodes);
			TvmAstNode const* opcode = opcodes.at(index).get();
			if (auto sub = to<SubProgram>(opcode)) {
				return Result{1, createNode<CodeBlock>(sub->block()->type(), sub->block()->instructions())};
			}
		}
	}

	auto f = [](bool isZero, bool isSwap, std::vector<Pointer<TvmAstNode>> const& instructions) {
		if (instructions.size() != (isSwap ? 2 : 1)) {
			return false;
		}
		return *instructions.at(0) == *gen(isZero ? "PUSHINT 0" : "NULL") &&
			   (!isSwap || *instructions.at(1) == *makeXCH_S(1));
	};

	// PUSHCONT {
	//    PUSHINT 0 / NULL
	//    [SWAP]
	// }
	// IF[ELSE][NOT][JMP]
	// =>
	// ZERO/NULL SWAP/ROTR IF [NOT]
	// PUSHCONT { ... }
	// IF[ELSE][NOT][JMP]
	if (m_withUnpackOpaque && cmd1IfElse) {
		for (bool isZero : {true, false}) {
			if (isZero && *GlobalParams::g_tvmVersion == langutil::TVMVersion::ton()){
				// ignore ZERO SWAP/ROTR IF [NOT]
				continue;
			}
			for (bool isSwap : {true, false}) {
				for (bool trueBranch: {true, false}) {
					Pointer<CodeBlock> curBranch = trueBranch ? cmd1IfElse->trueBody() : cmd1IfElse->falseBody();
					if (!curBranch) {
						continue;
					}
					if (f(isZero, isSwap, curBranch->instructions())) {
						Pointer<AsymGen> align = getZeroOrNullAlignment(isZero, !isSwap,
																		!trueBranch || cmd1IfElse->withNot());
						solAssert(trueBranch ? true : !cmd1IfElse->withNot(), "");
						auto emptyBlock = createNode<CodeBlock>(curBranch->type());
						return Result{1,
									  align,
									  createNode<TvmIfElse>(cmd1IfElse->withNot(), cmd1IfElse->withJmp(),
															trueBranch ? emptyBlock : cmd1IfElse->trueBody(),
															trueBranch ? cmd1IfElse->falseBody() : emptyBlock,
															cmd1IfElse->ret())
						};
					}
				}
			}
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt2(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, bool _withTuck) {
	using namespace MathConsts;
	auto cmd1GenOp = to<StackOpcode>(cmd1.get());
	auto cmd1Glob = to<Glob>(cmd1.get());

	auto cmd2Exc = to<TvmException>(cmd2.get());
	auto cmd2GenOpcode = to<StackOpcode>(cmd2.get());
	auto cmd2Glob = to<Glob>(cmd2.get());
	auto cmd2IfElse = to<TvmIfElse>(cmd2.get());
	auto cmd1Ret = to<TvmReturn>(cmd1.get());

	auto _isBLKDROP1 = isBLKDROP2(cmd1);
	auto _isBLKDROP2 = isBLKDROP2(cmd2);
	auto isBLKPUSH1 = isBLKPUSH(cmd1);
	auto isPUSH1 = isPUSH(cmd1);

	if (isSWAP(cmd1)) {
		if (is(cmd2, "STU")) return Result{2, gen("STUR " + arg(cmd2))};
		if (is(cmd2, "STSLICE")) return Result{2, gen("STSLICER")};
		if (is(cmd2, "SUB")) return Result{2, gen("SUBR")};
		if (is(cmd2, "SUBR")) return Result{2, gen("SUB")};
		if (isCommutative(cmd2)) return Result{1};
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
	if (is(cmd1, "PUSHINT")) {
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
	// PUSH Si | gen01
	// DROP N
	//
	// DROP N-1
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
	if (PeepholeOptimizer::withBlockPush && isBLKPUSH1 && isDrop(cmd2)) {
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
	// PUSH SN
	// BLKDROP2 down, top
	// =>
	// BLKDROP2 down, top-1
	// PUSH S?
	if (_withTuck && isPUSH(cmd1) && _isBLKDROP2) {
		int n = *isPUSH(cmd1);
		auto [down, top] = _isBLKDROP2.value();
		if (n < top) {
			return Result{2, makeBLKDROP2(down, top - 1), cmd1};
		} else if (n >= down + top - 1) {
			return Result{2, makeBLKDROP2(down, top - 1), makePUSH(n - down)};
		}
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
			return Result{2, flipIfElse(*cmd2IfElse)};
	}
	// EQINT 0 THROWIFNOT/THROWIF N => THROWIF/THROWIFNOT N
	// EQINT 0 PUSHCONT {} IF/IFNOT => PUSHCONT {} IFNOT/IF
	if (is(cmd1, "EQINT") && cmd1GenOp->arg() == "0") {
		if (isExc(cmd2, "THROWIF"))
			return Result{2, makeTHROW("THROWIFNOT " + cmd2Exc->arg())};
		if (isExc(cmd2, "THROWIFNOT"))
			return Result{2, makeTHROW("THROWIF " + cmd2Exc->arg())};
		if (cmd2IfElse)
			return Result{2, flipIfElse(*cmd2IfElse)};
	}
	// NEQINT 0, THROWIF/THROWIFNOT N => THROWIF/THROWIFNOT N
	// NEQINT 0, PUSHCONT {} IF => PUSHCONT {} IF
	if (is(cmd1, "NEQINT") && cmd1GenOp->arg() == "0") {
		if (isExc(cmd2, "THROWIF"))
			return Result{2, makeTHROW("THROWIF " + cmd2Exc->arg())};
		if (isExc(cmd2, "THROWIFNOT"))
			return Result{2, makeTHROW("THROWIFNOT " + cmd2Exc->arg())};
		if (cmd2IfElse)
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
	// SETGLOB N
	// GETGLOB N
	//
	// DUP
	// SETGLOB N
	if (cmd1Glob && cmd1Glob->opcode() == Glob::Opcode::SetOrSetVar &&
		cmd2Glob && cmd2Glob->opcode() == Glob::Opcode::GetOrGetVar &&
		cmd1Glob->index() == cmd2Glob->index()
	) {
		return Result{2, makePUSH(0), makeSetGlob(cmd1Glob->index())};
	}
	// PUSHINT N
	// ADDCONST ? | INC | DEC
	//
	// PUSHINT (N+delta)
	if (is(cmd1, "PUSHINT") && isConstAdd(cmd2)) {
		bigint n = pushintValue(cmd1);
		bigint delta = getAddNum(cmd2);
		// TODO check overflow
		return Result{2, gen("PUSHINT " + toString(n + delta))};
	}
	// PUSHINT N
	// UFITS ? | FITS ?
	//
	// PUSHINT N
	if (is(cmd1, "PUSHINT") && (is(cmd2, "UFITS") || is(cmd2, "FITS"))) {
		bigint n = pushintValue(cmd1);
		int bits = fetchInt(cmd2);
		auto type = TypeProvider::integer(bits,
										  is(cmd2, "UFITS") ? IntegerType::Modifier::Unsigned :
										  						IntegerType::Modifier::Signed);
		if (type->minValue() <= n && n <= type->maxValue())
			return Result{2, gen("PUSHINT " + toString(n))};
	}
	if (isConstAdd(cmd1) && isConstAdd(cmd2)) {
		int final_add = getAddNum(cmd1) + getAddNum(cmd2);
		if (-128 <= final_add && final_add <= 127)
			return Result{2, gen("ADDCONST " + std::to_string(final_add))};
	}
	if ((is(cmd1, "INDEX_NOEXCEP") || is(cmd1, "INDEX_EXCEP")) && 0 <= fetchInt(cmd1) && fetchInt(cmd1) <= 3 &&
		(is(cmd2, "INDEX_NOEXCEP") || is(cmd2, "INDEX_EXCEP")) && 0 <= fetchInt(cmd2) && fetchInt(cmd2) <= 3) {
		return Result{2, gen("INDEX2 " + arg(cmd1) + ", " + arg(cmd2))};
	}
	if (is(cmd1, "INDEX2") &&
		(is(cmd2, "INDEX_NOEXCEP") || is(cmd2, "INDEX_EXCEP")) && 0 <= fetchInt(cmd2) && fetchInt(cmd2) <= 3
	) {
		auto [i, j] = getIndexes(arg(cmd1));
		if (0 <= i && i <= 3 &&
			0 <= j && j <= 3) {
			return Result{2, gen("INDEX3 " + toString(i) + ", " + toString(j) + ", " + arg(cmd2))};
		}
	}
	if (
		is(cmd1, "PUSHINT") && 1 <= pushintValue(cmd1) && pushintValue(cmd1) <= 256 &&
		(is(cmd2, "RSHIFT") || is(cmd2, "LSHIFT")) && arg(cmd2).empty()
	) {
		return Result{2, gen(cmd2GenOpcode->opcode() + " " + arg(cmd1))};
	}
	if (is(cmd1, "PUSHINT") &&
		(is(cmd2, "DIV") || is(cmd2, "MUL"))) {
		bigint val = pushintValue(cmd1);
		if (power2Exp().count(val)) {
			const std::string& newOp = is(cmd2, "DIV") ? "RSHIFT" : "LSHIFT";
			return Result{2, gen(newOp + " " + toString(power2Exp().at(val)))};
		}
	}
	// PUSHINT 2**N
	// MOD
	// =>
	// MODPOW2 N
	if (is(cmd1, "PUSHINT") && is(cmd2, "MOD")) {
		bigint val = pushintValue(cmd1);
		if (power2Exp().count(val)) {
			return Result{2, gen("MODPOW2 " + toString(power2Exp().at(val)))};
		}
	}
	// PUSHINT (2**N)-1
	// AND
	// =>
	// MODPOW2 N
	if (is(cmd1, "PUSHINT") && is(cmd2, "AND")) {
		bigint val = pushintValue(cmd1);
		if (power2DecExp().count(val)) {
			return Result{2, gen("MODPOW2 " + toString(power2DecExp().at(val)))};
		}
	}
	if (is(cmd1, "PUSHINT")) {
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

	if (is(cmd1, "NEWC") && is(cmd2, "ENDC")) {
		return Result{2, makePUSHREF()};
	}

	// LESS | LEQ    | GREATER | GEQ  | EQUAL | NEQ   | EQINT  | NEQINT | NOT | TRUE  | FALSE
	// NOT
	// =>
	// GEQ | GREATER | LEQ     | LESS | NEQ   | EQUAL | NEQINT | EQINT  |     | FALSE | TRUE
	if (is(cmd2, "NOT")) {
		if (is(cmd1, "LESS")) return Result{2, gen("GEQ")};
		if (is(cmd1, "LEQ")) return Result{2, gen("GREATER")};
		if (is(cmd1, "GREATER")) return Result{2, gen("LEQ")};
		if (is(cmd1, "GEQ")) return Result{2, gen("LESS")};
		if (is(cmd1, "EQUAL")) return Result{2, gen("NEQ")};
		if (is(cmd1, "NEQ")) return Result{2, gen("EQUAL")};

		if (is(cmd1, "LESSINT")) {  // !(x < value) => x >= value => x > value-1
			int value = fetchInt(cmd1);
			if (-128 <= value - 1 && value - 1 < 128)
				return Result{2, gen("GTINT " + toString(value - 1))};
		}
		if (is(cmd1, "GTINT")) {  // !(x > value) => x <= value => x < value+1
			int value = fetchInt(cmd1);
			if (-128 <= value + 1 && value + 1 < 128)
				return Result{2, gen("LESSINT " + toString(value + 1))};
		}
		if (is(cmd1, "EQINT")) return Result{2, gen("NEQINT " + arg(cmd1))};
		if (is(cmd1, "NEQINT")) return Result{2, gen("EQINT " + arg(cmd1))};

		if (is(cmd1, "NOT")) return Result{2};

		if (is(cmd1, "TRUE")) return Result{2, gen("FALSE")};
		if (is(cmd1, "FALSE")) return Result{2, gen("TRUE")};
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
		is(cmd1, "PUSHINT") && pushintValue(cmd1) == 0 &&
		is(cmd2, "STUR")
	) {
		return Result{2,
			gen("PUSHINT " + arg(cmd2)),
			gen("STZEROES")};
	}
	if (
		is(cmd1, "ABS") &&
		is(cmd2, "UFITS") && fetchInt(cmd2) == 256
	) {
		return Result{2, gen("ABS")};
	}

	if (
		is(cmd1, "PUSHINT") && pushintValue(cmd1) == 1 &&
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
				auto _true = to<StackOpcode>(cmd2_1.get());
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
	auto _true = to<StackOpcode>(cmd1.get());
	auto _and = to<StackOpcode>(cmd2.get());
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

	// pure gen(1, 1)
	// DROP N
	// =>
	// DROP N
	if (cmd1GenOp && cmd1GenOp->isPure() &&
		std::make_pair(cmd1GenOp->take(), cmd1GenOp->ret()) == std::make_pair(1, 1) &&
		isDrop(cmd2)
	) {
		return Result{2, cmd2};
	}

	// ABS
	// MODPOW2 256
	// =>
	// ABS
	if (is(cmd1, "ABS") &&
		cmd2GenOpcode && cmd2GenOpcode->opcode() == "MODPOW2" && cmd2GenOpcode->arg() == "256"
	) {
		return Result{2, gen("ABS")};
	}

	// MODPOW2 x
	// MODPOW2 y
	// =>
	// MODPOW2 min(x, y)
	if (is(cmd1, "MODPOW2") && is(cmd2, "MODPOW2")) {
		int x = fetchInt(cmd1);
		int y = fetchInt(cmd2);
		return Result{2, gen("MODPOW2 " + toString(std::min(x, y)))};
	}

	// BLKPUSH N, 0 / DUP
	// BLKPUSH Q, 0 / DUP
	// =>
	// BLKPUSH N+Q, 0
	if (PeepholeOptimizer::withBlockPush && isBLKPUSH(cmd1) && isBLKPUSH(cmd2)) {
		auto [qty0, index0] = isBLKPUSH(cmd1).value();
		auto [qty1, index1] = isBLKPUSH(cmd2).value();
		if (index0 == 0 && index1 == 0 && qty0 + qty1 <= 15)
			return Result{2, makeBLKPUSH(qty0 + qty1, 0)};
	}

	// LD[I|U] N / LDDICT / LDREF / LD[I|U]X N
	// DROP
	if ((is(cmd1, "LDU", "LDI", "LDREF", "LDDICT", "LDUX", "LDIX",
			"LDSLICE", "LDSLICEX")) &&
		isDrop(cmd2)
	) {
		// TODO add LD[I|U]LE[4|8]
		int n = isDrop(cmd2).value();
		Pointer<StackOpcode> newOpcode = gen("P" + cmd1GenOp->fullOpcode());
		if (n == 1) {
			return Result{2, newOpcode};
		} else {
			return Result{2, {newOpcode, makeDROP(n - 1)}};
		}
	}

	// 26 + 118 gas units
	// PLDREF
	// CTOS
	// =>
	// 118 + 18 gas units
	// LDREFRTOS
	// NIP
	if (is(cmd1, "PLDREF") && is(cmd2, "CTOS")) {
		return Result{2, gen("LDREFRTOS"), makeBLKDROP2(1, 1)};
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt3(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3, bool m_withUnpackOpaque) {
	auto isPUSH1 = isPUSH(cmd1);
	auto cmd1PushCellOrSlice = to<PushCellOrSlice>(cmd1.get());
	auto isPUSH2 = isPUSH(cmd2);
	auto cmd2PushCellOrSlice = to<PushCellOrSlice>(cmd2.get());
	auto cmd3GenOpcode = to<StackOpcode>(cmd3.get());
	auto cmd3SubProgram = to<SubProgram>(cmd3.get());

	// NEW
	// s01
	// ST**R
	// =>
	// s01
	// NEW
	// ST**
	if (
		is(cmd1, "NEWC") &&
		isSimpleCommand(cmd2) &&
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
	if (
		is(cmd1, "NEWC") &&
		is(cmd2, "STSLICECONST") && arg(cmd2).length() > 1 &&
		is(cmd3, "ENDC")
	) {
		return Result{3, makePUSHREF(arg(cmd2))};
	}

	// PUSHINT x
	// PUSH Si (i!=0) or gen(0,1)
	// CMP
	// =>
	// PUSH S(i-1) or gen(0,1)
	// CMP2
	if (is(cmd1, "PUSHINT") && ((isPUSH2 && *isPUSH2 != 0) || isPureGen01(*cmd2))) {
		auto newCmd2 = isPUSH2 ? makePUSH(*isPUSH2 - 1) : cmd2;
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
	// PUSHINT A
	// PUSHINT B
	// ADD | MUL
	//
	// PUSHINT A+B | PUSHINT A*B
	if (is(cmd1, "PUSHINT") &&
		is(cmd2, "PUSHINT") &&
		cmd3GenOpcode && isIn(cmd3GenOpcode->opcode(), "ADD", "MUL", "MAX")
	) {
		bigint a = pushintValue(cmd1);
		bigint b = pushintValue(cmd2);
		bigint c;
		if (cmd3GenOpcode->opcode() == "ADD")
			c = a + b;
		else if (cmd3GenOpcode->opcode() == "MUL")
			c = a * b;
		else if (cmd3GenOpcode->opcode() == "MAX")
			c = std::max(a, b);
		else
			solUnimplemented("");
		return Result{3, gen("PUSHINT " + toString(c))};
	}
	// PUSHINT A
	// PUSHINT B
	// DIV
	//
	// PUSHINT A/B
	if (is(cmd1, "PUSHINT") &&
		is(cmd2, "PUSHINT") &&
		cmd3GenOpcode && cmd3GenOpcode->opcode() == "DIV"
	) {
		bigint a = pushintValue(cmd1);
		bigint b = pushintValue(cmd2);
		if (a >= 0 && b > 0) { // note in TVM  -9 / 2 == -5
			bigint c = a / b;
			return Result{3, gen("PUSHINT " + toString(c))};
		}
	}

	// PUSHINT
	// PUSH SN / gen01
	// ADD / MUL
	//
	// PUSH S(N-1) / gen01
	// ADDCONST / MULCONST
	if (is(cmd1, "PUSHINT") &&
		(is(cmd3, "ADD") || is(cmd3, "MUL"))
	) {
		bigint val = pushintValue(cmd1);
		if (-128 <= val && val <= 127) {
			if ((isPureGen01(*cmd2) && to<StackOpcode>(cmd2.get())) ||
				to<Glob>(cmd2.get()) ||
				isPUSH(cmd2)
			) {
				Pointer<TvmAstNode> newCmd;
				if (to<StackOpcode>(cmd2.get()) ||
					to<Glob>(cmd2.get())
				) {
					newCmd = cmd2;
				} else if (auto index = isPUSH(cmd2); index.has_value() && *index > 0) {
					newCmd = makePUSH(*index - 1);
				}
				if (newCmd)
					return Result{3,  newCmd, gen((is(cmd3, "ADD") ? "ADDCONST " : "MULCONST ") + toString(val))};
			}
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
	if (PeepholeOptimizer::withBlockPush && // ?
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

	//            ; a b
	// SWAP       ; b a
	// gen(0, 1)  ; b a c
	// ROT        ; a c b
	// =>
	// gen(0,1)
	// SWAP
	if (isSWAP(cmd1)) {
		std::optional<std::pair<int, int>> rot = isBLKSWAP(cmd3);
		if (rot && *rot == std::make_pair(1, 2)) {
			if (isPureGen01(*cmd2)) {
				return Result{3, cmd2, makeXCH_S(1)};
			}
		}
	}


	if (cmd1PushCellOrSlice && cmd1PushCellOrSlice->type() == PushCellOrSlice::Type::PUSHREF &&
		cmd2PushCellOrSlice && cmd2PushCellOrSlice->type() == PushCellOrSlice::Type::PUSHREF
	) {
		if (cmd3SubProgram) {
			std::vector<Pointer<TvmAstNode>> const& instructions = cmd3SubProgram->block()->instructions();
			if (instructions.size() == 1 &&
				*instructions.at(0) == *createNode<StackOpcode>(".inline __concatenateStrings", 2, 1)) {
				string hexStr = cmd1PushCellOrSlice->chainBlob() + cmd2PushCellOrSlice->chainBlob();
				return Result{3, makePushCellOrSlice(hexStr, false)};
			}
		}
	}

	// TODO delete, fix in stackOpt
	// Note: breaking stack
	// DUP
	// IFREF { CALL $c7_to_c4$ / $upd_only_time_in_c4$ }
	// =>
	// IFREF { CALL $c7_to_c4$ / $upd_only_time_in_c4$ }
	if (m_withUnpackOpaque && isPUSH(cmd1)) {
		if (auto ifRef = to<TvmIfElse>(cmd2.get());
			ifRef && !ifRef->withJmp() && !ifRef->withNot() && ifRef->falseBody() == nullptr
		) {
			std::vector<Pointer<TvmAstNode>> const &cmds = ifRef->trueBody()->instructions();
			if (cmds.size() == 1) {
				if (auto gen = to<StackOpcode>(cmds.at(0).get())) {
					if (isIn(gen->fullOpcode(), ".inline c7_to_c4", ".inline upd_only_time_in_c4")) {
						return Result{2, cmd2};
					}
				}
			}
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt4(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
											 Pointer<TvmAstNode> const& cmd3, Pointer<TvmAstNode> const& cmd4) {
	auto cmd3Exc = to<TvmException>(cmd3.get());

	if (is(cmd1, "PUSHINT") && is(cmd3, "PUSHINT")) {
		if (isAddOrSub(cmd2) && isAddOrSub(cmd4)) {
			bigint sum = 0;
			sum += (is(cmd2, "ADD") ? +1 : -1) * pushintValue(cmd1);
			sum += (is(cmd4, "ADD") ? +1 : -1) * pushintValue(cmd3);
			// TODO DELETE
			return Result{4, gen("PUSHINT " + toString(sum)), gen("ADD")};
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
	// TODO if value on the top of the stack < 0
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

	if (is(cmd1, "PUSHINT") &&
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
	if (
		isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "ENDC")
	) {
		return Result{4, makePUSHREF(isPlainPushSlice(cmd1)->blob())};
	}
	if (is(cmd1, "PUSHINT") && pushintValue(cmd1) == 0 &&
		is(cmd2, "STUR") &&
		is(cmd3, "PUSHINT") && pushintValue(cmd3) == 0 &&
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

	// DUP
	// ISNULL
	// THROWIF 63
	// UNSINGLE
	// =>
	// UNSINGLE
	if (
		isPUSH(cmd1) && isPUSH(cmd1).value() == 0 &&
		is(cmd2, "ISNULL") &&
		cmd3Exc && cmd3Exc->opcode() == "THROWIF" && cmd3Exc->arg() == toString(TvmConst::RuntimeException::GetOptionalException) &&
		is(cmd4, "UNTUPLE") && fetchInt(cmd4) == 1
	) {
		return Result{4, cmd4};
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt5(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
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
	if (is(cmd1, "PUSHINT") &&
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
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt6(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2,
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
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAtInf(int idx1) const {
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	auto cmd1IfElse = to<TvmIfElse>(cmd1.get());
	auto cmd1Ret = to<TvmReturn>(cmd1.get());
	int idx2 = nextCommandLine(idx1);

	// delete last RET in block
	if (cmd1Ret && !cmd1Ret->withIf() && !cmd1Ret->withAlt() && idx2 == -1) {
		return Result{1};
	}

	// PUSHCONT {
	//    NULL
	//    SWAP
	// }
	// IFNOTJMP
	// =>
	// NULLROTRIFNOT
	// DROP
	if (cmd1IfElse && cmd1IfElse->falseBody() == nullptr && cmd1IfElse->withJmp() && cmd1IfElse->withNot() &&
		idx2 == -1) {
		std::vector<Pointer<TvmAstNode>> const& insts = cmd1IfElse->trueBody()->instructions();
		if (insts.size() == 2 && is(insts.at(0), "NULL") && isSWAP(insts.at(1))) {
			return Result{1, StackPusher::makeAsym("NULLROTRIFNOT"), makeDROP()};
		}
	}


	// squash ST* opcodes
	{
		// another stores?
		int i = idx1;
		std::string bitString;
		int opcodeQty = 0;
		int iterQty = 0;
		bool withBuilder = false;
		{
			int j = nextCommandLine(i);
			int k = nextCommandLine(j);
			Pointer<TvmAstNode> const& cmd2 = get(j);
			Pointer<TvmAstNode> const& cmd3 = get(k);

			if (k != -1 &&
				isPlainPushSlice(cmd1) &&
				is(cmd2, "NEWC") &&
				is(cmd3, "STSLICE")
			) {
				withBuilder = true;
				std::string hexSlice = isPlainPushSlice(cmd1)->blob();
				bitString += StrUtils::toBitString(hexSlice);
				++iterQty;
				opcodeQty += 3;
				i = nextCommandLine(k);
			}
			// PUSHINT ?
			// NEWC
			// STU y
			else if (
				is(cmd1, "PUSHINT") &&
				is(cmd2, "NEWC") &&
				is(cmd3, "STU")
			) {
				withBuilder = true;
				bitString += StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd3));
				++iterQty;
				opcodeQty += 3;
				i = nextCommandLine(k);
			}
		}

		while (i != -1) {
			Pointer<TvmAstNode> const& c1 = get(i);
			int j = nextCommandLine(i);
			Pointer<TvmAstNode> c2;
			if (j != -1) {
				c2 = get(j);
			}

			// TODO ADD LD[I|U]LE[4|8]
			if (is(c1, "STZERO")) {
				bitString += "0";
				++opcodeQty;
				i = j;
			} else if (is(c1, "STONE")) {
				bitString += "1";
				++opcodeQty;
				i = j;
			} else if (is(c1, "STSLICECONST")) {
				bitString += StrUtils::toBitString(arg(c1));
				++opcodeQty;
				i = j;
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STUR")) {
				bigint num = pushintValue(c1);
				int len = fetchInt(c2);
				bitString += StrUtils::toBitString(num, len);
				opcodeQty += 2;
				i = nextCommandLine(j);
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STIR")) {
				bigint num = pushintValue(c1);
				int len = fetchInt(c2);
				bitString += StrUtils::toBitString(num, len);
				opcodeQty += 2;
				i = nextCommandLine(j);
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STVARUINT16", "STGRAMS")) {
				bigint num = pushintValue(c1);
				bitString += StrUtils::tonsToBinaryString(num);
				opcodeQty += 2;
				i = nextCommandLine(j);
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STZEROES")) {
				int len = fetchInt(c1);
				bitString += std::string(len, '0');
				opcodeQty += 2;
				i = nextCommandLine(j);
			} else if (c2 && isPlainPushSlice(c1) && is(c2, "STSLICER")) {
				std::string hexSlice = isPlainPushSlice(c1)->blob();
				bitString += StrUtils::toBitString(hexSlice);
				opcodeQty += 2;
				i = nextCommandLine(j);
			} else {
				break;
			}
			++iterQty;
		}

		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitString, "");
		if (slice) {
			vector<Pointer<TvmAstNode>> opcodes;
			if (withBuilder) {
				opcodes.push_back(gen("NEWC"));
			}
			std::optional<Result> res;
			if (StrUtils::toBitString(*slice).length() <= TvmConst::MaxSTSLICECONST) {
				opcodes.push_back(gen("STSLICECONST " + *slice));
				res = Result{opcodeQty, opcodes};
			} else {
				opcodes.push_back(genPushSlice(*slice));
				opcodes.push_back(gen("STSLICER"));
				res = Result{opcodeQty, opcodes};
			}
			if (opcodeQty > int(res->commands.size()) || (opcodeQty == int(res->commands.size()) && iterQty >= 2)) {
				return res;
			}
		}
	}

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

	// BLKSWAP n, 1
	// POP n+1
	// ...
	// POP n+1 // n times
	// =>
	// BLKDROP2 n, n+1
	if (isBLKSWAP(cmd1) && idx2 != -1) {
		auto [n, up] = isBLKSWAP(cmd1).value();
		if (up == 1) {
			int index = idx2;
			bool ok = true;
			int i = 0;
			for (; ok && i < n && index != -1; ++i, index = nextCommandLine(index)) {
				auto cmd = get(index);
				ok &= isPOP(cmd).has_value() && isPOP(cmd) == n + 1;
			}
			ok &= i == n;
			if (ok) {
				return Result{n + 1, makeBLKDROP2(n, n + 1)};
			}
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
		struct BestResult {
			StackState bestState;
			int bestStartStackSize = 0;
			int bestOpcodeQty = 0;
		};
		std::optional<BestResult> bestResult;

		for (int startStackSize = 0; startStackSize <= StackState::MAX_STACK_DEPTH; ++startStackSize)
		{
			StackState state{startStackSize};
			int i = idx1;
			int gasCost = 0;
			int opcodeQty = 0;
			while (true) {
				if (i == -1)
					break;
				auto stack = to<Stack>(get(i).get());
				if (!stack)
					break;
				if (!state.apply(*stack)) {
					break;
				}

				++opcodeQty;
				gasCost += OpcodeUtils::gasCost(*stack);
				auto newGasCost = StackOpcodeSquasher::gasCost(startStackSize, state, m_withTuck);
				if (newGasCost.has_value() && newGasCost < gasCost) {
					bestResult = BestResult{state, startStackSize, opcodeQty};
				}

				i = nextCommandLine(i);
			}
		}

		if (bestResult.has_value()) {
			auto newOpcodes = StackOpcodeSquasher::recover(bestResult.value().bestStartStackSize,
														   bestResult.value().bestState,
														   m_withTuck);
			return Result{bestResult.value().bestOpcodeQty, newOpcodes};
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
					// TODO implement another cases
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

	return {};
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

std::optional<Result> PrivatePeepholeOptimizer::unsquash(bool _withUnpackOpaque, const int idx1) const {
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
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::squashPush(const int idx1) const {
	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);
	Pointer<TvmAstNode> const& cmd3 = get(idx3);
	auto cmd1Gen = to<Gen>(cmd1.get());
	auto cmd1PushCellOrSlice = to<PushCellOrSlice>(cmd1.get());

	if (PeepholeOptimizer::withBlockPush && isPUSH(cmd1) && isPUSH(cmd2)) {
		int i = idx1, n = 0;
		while (isPUSH(get(i)) && isPUSH(get(i)).value() == isPUSH(cmd1).value()) {
			n++;
			i = nextCommandLine(i);
		}
		if (n >= 2 && isPUSH(cmd1) <= 15) {
			return Result{n, makeBLKPUSH(n, isPUSH(cmd1).value())};
		}
	}

	if (PeepholeOptimizer::withBlockPush && isPUSH(cmd1) && isPUSH(cmd2) && isPUSH(cmd3)) {
		const int si = *isPUSH(cmd1);
		const int sj = *isPUSH(cmd2) - 1 == -1? si : *isPUSH(cmd2) - 1;
		const int sk = *isPUSH(cmd3) - 2 == -1? si : (
				*isPUSH(cmd3) - 2 == -2? sj : *isPUSH(cmd3) - 2
		);
		if (si <= 15 && sj <= 15 && sk <= 15) {
			return Result{3, makePUSH3(si, sj, sk)};
		}
	}

	if (PeepholeOptimizer::withBlockPush && isPUSH(cmd1) && isPUSH(cmd2)) {
		const int si = *isPUSH(cmd1);
		const int sj = *isPUSH(cmd2) - 1 == -1? si : *isPUSH(cmd2) - 1;
		if (si <= 15 && sj <= 15) {
			return Result{2, makePUSH2(si, sj)};
		}
	}

	// TODO delete
	// squash PUSHINT, NEWDICT, NILL, FALSE, TRUE, (PUSHINT 0 NEWDICT PAIR) etc
	if (PeepholeOptimizer::withBlockPush && isPureGen01(*cmd1)) {
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
	if (PeepholeOptimizer::withBlockPush && cmd1PushCellOrSlice) {
		int i = idx1;
		int n = 0;
		while (true) {
			auto cmdI = to<PushCellOrSlice>(get(i).get());
			if  (cmdI && *cmd1PushCellOrSlice == *cmdI)
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

	if (m_withTuck) {
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

		// XCHG Si
		// PUSH Sj
		// =>
		// XCPU Si, Sj
		if (m_optimizeSlice &&
			isXCHG_S0(cmd1) && cmd2 && isPUSH(cmd2)
		) {
			int i = isXCHG_S0(cmd1).value();
			int j = isPUSH(cmd2).value();
			if (
				0 <= i && i <= 15 &&
				0 <= j && j <= 15
			) {
				return Result{2, makeXCPU(i, j)};
			}
		}
	}

	return {};
}

void PrivatePeepholeOptimizer::updateLinesAndIndex(int idx1, const std::optional<Result>& res) {
	solAssert(res, "");
	if (res && res.value().removeQty > 0) {
		solAssert(valid(idx1), "");
		solAssert(!isLoc(m_instructions.at(idx1)), "");
		int lastInx = idx1;
		for (int iter = 0; iter + 1 < res.value().removeQty; ++iter) {
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
		for (Pointer<TvmAstNode> const& inst : res.value().commands) {
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

bool PrivatePeepholeOptimizer::optimize(const std::function<std::optional<Result>(int)> &f) {
	int idx1 = 0;
	while (idx1 < static_cast<int>(m_instructions.size()) && isLoc(m_instructions.at(idx1))) {
		++idx1;
	}

	bool didSomething = false;
	while (valid(idx1)) {
		solAssert(!isLoc(m_instructions.at(idx1)), "");
		std::optional<Result> res = f(idx1);
		if (res) {
			{
//				std::cout << ">>>A\n";
//				Printer p{std::cout};
//				for (const auto &x: m_instructions) x->accept(p);
//				std::cout << std::endl;
			}
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

//			std::cout << "<<<B\n";
//			Printer p{std::cout};
//			for (const auto& x : m_instructions) x->accept(p);
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

bigint PrivatePeepholeOptimizer::pushintValue(Pointer<TvmAstNode> const& node) {
	solAssert(is(node, "PUSHINT"), "");
	auto g = dynamic_pointer_cast<StackOpcode>(node);
	return bigint{g->arg()};
}

int PrivatePeepholeOptimizer::fetchInt(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<StackOpcode>(node);
	return strToInt(g->arg());
}

bool PrivatePeepholeOptimizer::isNIP(Pointer<TvmAstNode> const& node) {
	auto swap = dynamic_pointer_cast<Stack>(node);
	return
	(isPOP(node) && isPOP(node).value() == 1) ||
	(isBLKDROP2(node) && isBLKDROP2(node).value() == std::make_pair(1, 1));
}

std::string PrivatePeepholeOptimizer::arg(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<StackOpcode>(node);
	solAssert(g, "");
	return g->arg();
}

template<class ...Args>
bool PrivatePeepholeOptimizer::is(Pointer<TvmAstNode> const& node, Args&&... cmd) {
	auto g = to<StackOpcode>(node.get());
	return g && isIn(g->opcode(), std::forward<Args>(cmd)...);
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

bool PrivatePeepholeOptimizer::isConstAdd(Pointer<TvmAstNode> const& node) {
	auto gen = to<StackOpcode>(node.get());
	return gen && isIn(gen->opcode(), "INC", "DEC", "ADDCONST");
}

int PrivatePeepholeOptimizer::getAddNum(Pointer<TvmAstNode> const& node) {
	solAssert(isConstAdd(node), "");
	auto gen = to<StackOpcode>(node.get());
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


bool PrivatePeepholeOptimizer::isSimpleCommand(Pointer<TvmAstNode> const& node) {
	// See also isPureGen01
	auto gen = to<Gen>(node.get());
	return gen &&
		   (to<StackOpcode>(gen) || to<PushCellOrSlice>(gen) || to<Glob>(gen) || to<Opaque>(gen) || to<HardCode>(gen)) &&
		gen->take() == 0 && gen->ret() == 1;
}

bool PrivatePeepholeOptimizer::isAddOrSub(Pointer<TvmAstNode> const& node) {
	return is(node, "ADD") || is(node, "SUB");
}

bool PrivatePeepholeOptimizer::isCommutative(Pointer<TvmAstNode> const& node) {
	auto g = dynamic_pointer_cast<StackOpcode>(node);
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

bool PeepholeOptimizer::visit(CodeBlock &_node) {
	optimizeBlock(_node);
	return true;
}

void PeepholeOptimizer::endVisit(CodeBlock &_node) {
	optimizeBlock(_node);
}

bool PeepholeOptimizer::visit(Function &/*_node*/) {
	//if (_node.name() == "")
	//	_node.block()->accept(*this);
	//return false;
	return true;
}

void PeepholeOptimizer::optimizeBlock(CodeBlock &_node) const {
	{
		std::optional<Result> r = PrivatePeepholeOptimizer::optimizeAt1(_node.shared_from_this(), m_withUnpackOpaque);
		if (r && r.value().commands.size() == 1) {
			auto newBlock = to<CodeBlock>(r.value().commands.at(0).get());
			_node.upd(newBlock->instructions());
			_node.updType(newBlock->type());
		}
	}

	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	PrivatePeepholeOptimizer optimizer{instructions, m_withUnpackOpaque, m_optimizeSlice, m_withTuck};
	optimizer.optimize([&](int index){
		return optimizer.unsquash(m_withUnpackOpaque, index);
	});

	while (optimizer.optimize([&optimizer](int index){ return optimizer.optimizeAt(index); })) {}

	optimizer.optimize([&optimizer](int index){ return optimizer.squashPush(index);});
	_node.upd(optimizer.instructions());
}

} // end solidity::frontend
