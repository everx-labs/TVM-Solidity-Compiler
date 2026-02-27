/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
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

#include <libsolidity/codegen/Printer.hpp>
#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/optimizers/PeepholeOptimizer.hpp>
#include <libsolidity/codegen/optimizers/StackOpcodeSquasher.hpp>

using namespace solidity::util;

namespace solidity::frontend {

struct Result {
	int removeQty{};
	std::vector<Pointer<TvmAstNode>> commands{};


	template <class... Args>
	explicit Result(int remove, Args const&... cmds):
		removeQty(remove),
		commands{cmds...} {}

	explicit Result(int remove, std::vector<Pointer<TvmAstNode>> commands = {}):
		removeQty(remove),
		commands{std::move(commands)} {}
};

class PrivatePeepholeOptimizer {
public:
	explicit PrivatePeepholeOptimizer(std::vector<Pointer<TvmAstNode>> instructions, std::bitset<5> const _flags):
		m_instructions{std::move(instructions)},
		m_flags{_flags} {}
	std::vector<Pointer<TvmAstNode>> const& instructions() const { return m_instructions; }

	int nextCommandLine(int idx) const;
	static int nextCommandLine(int idx, std::vector<Pointer<TvmAstNode>> const& instructions);
	int qtyCmdAfter(int index) const;
	Pointer<TvmAstNode> get(int idx) const;
	bool valid(int idx) const;
	void remove(int idx);
	void insert(int idx, Pointer<TvmAstNode> const& node);
	std::optional<Result> optimizeAt(int idx1) const;
	std::optional<Result> useR(int idx1) const;
	std::optional<Result> optimizeSlice(int idx1) const;
	std::optional<Result> optimizeAt1(Pointer<TvmAstNode> const& cmd1) const;
	std::optional<Result> optimizeAt2(Pointer<TvmAstNode> const& cmd1, Pointer<TvmAstNode> const& cmd2, int idx2) const;
	std::optional<Result> optimizeAt3(
		Pointer<TvmAstNode> const& cmd1,
		Pointer<TvmAstNode> const& cmd2,
		Pointer<TvmAstNode> const& cmd3
	) const;

	static std::optional<Result> optimizeAt4(
		Pointer<TvmAstNode> const& cmd1,
		Pointer<TvmAstNode> const& cmd2,
		Pointer<TvmAstNode> const& cmd3,
		Pointer<TvmAstNode> const& cmd4
	);

	static std::optional<Result> optimizeAt5(
		Pointer<TvmAstNode> const& cmd1,
		Pointer<TvmAstNode> const& cmd2,
		Pointer<TvmAstNode> const& cmd3,
		Pointer<TvmAstNode> const& cmd4,
		Pointer<TvmAstNode> const& cmd5
	);

	static std::optional<Result> optimizeAt6(
		Pointer<TvmAstNode> const& cmd1,
		Pointer<TvmAstNode> const& cmd2,
		Pointer<TvmAstNode> const& cmd3,
		Pointer<TvmAstNode> const& cmd4,
		Pointer<TvmAstNode> const& cmd5,
		Pointer<TvmAstNode> const& cmd6
	);
	std::optional<Result> optimizeAtInf(int idx1) const;
	static bool hasRetOrJmp(TvmAstNode const* _node);

	void updateLinesAndIndex(int idx1, Result const& res);
	std::optional<Result> unsquash(bool _withUnpackOpaque, int idx1) const;
	std::optional<Result> squash(int idx1) const;
	bool optimize(std::function<std::optional<Result>(int)> const& f);

	static bigint pushintValue(Pointer<TvmAstNode> const& node);
	static int fetchInt(Pointer<TvmAstNode> const& node);
	static bool isNIP(Pointer<TvmAstNode> const& node);
	static bool isSimpleCommand(Pointer<TvmAstNode> const& node);
	static bool isAddOrSub(Pointer<TvmAstNode> const& node);
	static bool isCommutative(Pointer<TvmAstNode> const& node);
	static std::pair<int, int> getIndexes(std::string const& str);

	template <class... Args>
	static bool isExc(Pointer<TvmAstNode> const& node, Args&&... cmd);
	static bool isConstAdd(Pointer<TvmAstNode> const& node);
	static int getAddNum(Pointer<TvmAstNode> const& node);

private:
	std::vector<Pointer<TvmAstNode>> m_instructions{};
	std::bitset<5> const m_flags;
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
		if (!convertToLoc(instructions[idx].get()))
			return idx;
		idx++;
	}
	return -1;
}

int PrivatePeepholeOptimizer::qtyCmdAfter(int index) const {
	int qty = 0;
	index = nextCommandLine(index);
	while (index != -1) {
		++qty;
		index = nextCommandLine(index);
	}
	return qty;
}

Pointer<TvmAstNode> PrivatePeepholeOptimizer::get(int idx) const {
	return valid(idx) ? m_instructions.at(idx) : nullptr;
}

bool PrivatePeepholeOptimizer::valid(int idx) const {
	return idx >= 0 && static_cast<size_t>(idx) < m_instructions.size();
}

void PrivatePeepholeOptimizer::remove(int idx) { m_instructions.erase(m_instructions.begin() + idx); }

void PrivatePeepholeOptimizer::insert(int idx, Pointer<TvmAstNode> const& node) {
	m_instructions.insert(m_instructions.begin() + idx, node);
}

std::optional<Result> PrivatePeepholeOptimizer::useR(int idx1) const {
	int idx2 = nextCommandLine(idx1);

	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);

	if (isSWAP(cmd1)) {
		if (is(cmd2, "STU", "STI", "STSLICE", "STREF")) {
			auto const op = convertToStackGen(cmd2.get());
			return Result{2, gen(op->opcode() + "R " + op->arg())};
		}

		if (is(cmd2, "STBREFR"))
			return Result{2, gen("STBREF")};
	}
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeSlice(int idx1) const {
	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);
	Pointer<TvmAstNode> const& cmd3 = get(idx3);

	// PUSHSLICE xXXX   // firstCase
	// STSLICER
	// or
	// PUSHSLICE xXXX   // secondCase
	// NEWC
	// STSLICE
	bool const firstCase = cmd2 && is(cmd2, "STSLICER");
	bool const secondCase = cmd2 && is(cmd2, "NEWC") && cmd3 && is(cmd3, "STSLICE");
	if (isPlainPushSlice(cmd1) && (firstCase || secondCase)) {
		std::string const& slice = isPlainPushSlice(cmd1)->blob();
		std::string const& binStr = StrUtils::toBitString(slice);
		int sliceBits = binStr.length();
		// PUSHINT len
		// STZEROES
		// or
		// NEWC
		// PUSHINT len
		// STZEROES
		if (std::ranges::all_of(binStr, [](char ch) { return ch == '0'; })) {
			if (firstCase)
				return Result{2, gen("PUSHINT " + toString(sliceBits)), gen("STZEROES")};
			return Result{3, gen("NEWC"), gen("PUSHINT " + toString(sliceBits)), gen("STZEROES")};
		}

		std::optional<bigint> negNum = StrUtils::toNegBigint(binStr);
		int negNumLength = std::numeric_limits<int>::max() / 2;
		if (negNum)
			negNumLength = StrUtils::toBinString(-negNum.value()).length() +
						   5; // approximate length, opcode "PUSHINT" may contain more bits
		bigint num = StrUtils::toBigint(binStr);
		int numLength = StrUtils::toBinString(num).length() + 5;
		if (sliceBits <= 256 && sliceBits > std::min(numLength, negNumLength)) {
			if (firstCase) {
				if (numLength < negNumLength)
					// PUSHINT N
					// STUR sliceBits
					return Result{2, gen("PUSHINT " + toString(num)), gen("STUR " + toString(sliceBits))};
				// PUSHINT N
				// STIR sliceBits
				return Result{2, gen("PUSHINT " + toString(*negNum)), gen("STIR " + toString(sliceBits))};
			} else {
				if (numLength < negNumLength)
					// PUSHINT N
					// NEWC
					// STU sliceBits
					return Result{3, gen("PUSHINT " + toString(num)), gen("NEWC"), gen("STU " + toString(sliceBits))};
				else {
					// PUSHINT N
					// NEWC
					// STI sliceBits
					return Result{
						3,
						gen("PUSHINT " + toString(*negNum)),
						gen("NEWC"),
						gen("STI " + toString(sliceBits))
					};
				}
			}
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt(int const idx1) const {
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

	std::optional<Result> res = optimizeAt1(cmd1);
	if (res)
		return res;

	res = optimizeAtInf(idx1);
	if (res)
		return res;

	if (!cmd2)
		return {};
	res = optimizeAt2(cmd1, cmd2, idx2);
	if (res)
		return res;

	if (!cmd3)
		return {};
	res = optimizeAt3(cmd1, cmd2, cmd3);
	if (res)
		return res;

	if (!cmd4)
		return {};
	res = optimizeAt4(cmd1, cmd2, cmd3, cmd4);
	if (res)
		return res;

	if (!cmd5)
		return {};
	res = optimizeAt5(cmd1, cmd2, cmd3, cmd4, cmd5);
	if (res)
		return res;

	if (!cmd6)
		return {};
	res = optimizeAt6(cmd1, cmd2, cmd3, cmd4, cmd5, cmd6);
	if (res)
		return res;

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt1(Pointer<TvmAstNode> const& cmd1) const {
	auto cmd1CodeBlock = convertToCodeBlock(cmd1.get());
	auto cmd1GenOpcode = convertToStackGen(cmd1.get());
	auto cmd1IfElse = convertToTvmIfElse(cmd1.get());
	auto cmd1Sub = convertToSubProgram(cmd1.get());

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
	if (cmd1IfElse &&
		qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0 &&
		cmd1IfElse->falseBody() == nullptr &&
		!cmd1IfElse->withJmp()) {
		return Result{1, makeDROP()};
	}
	// PUSHCONT {} IFJMP => IFRET
	// PUSHCONT {} IFNOTJMP => IFNOTRET
	if (cmd1IfElse &&
		qtyWithoutLoc(cmd1IfElse->trueBody()->instructions()) == 0 &&
		cmd1IfElse->falseBody() == nullptr &&
		cmd1IfElse->withJmp()) {
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
			for (auto const& x: inst)
				if (!convertToLoc(x.get()))
					pos = x;
			auto _throw = convertToTvmException(pos.get());
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
			for (auto const& x: inst)
				if (!convertToLoc(x.get()))
					pos = x;
			auto ret = convertToTvmReturn(pos.get());
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
			for (auto const& x: inst)
				if (!convertToLoc(x.get()))
					pos = x;
			auto ret = convertToTvmReturn(pos.get());
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
				auto subProg = createNode<SubProgram>(false, cmd1IfElse->trueBody());
				return Result{1, makeDROP(), subProg};
			}
		}
	}

	// PUSHCONT {
	//   ...
	//   TAIL
	// }
	// PUSHCONT {
	//   ...
	//   TAIL
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
	// TAIL
	if (m_flags.test(static_cast<size_t>(OptFlags::UnpackOpaque)) &&
		cmd1IfElse &&
		cmd1IfElse->falseBody() != nullptr &&
		cmd1IfElse->ret() == 0) {
		std::vector<Pointer<TvmAstNode>> t = cmd1IfElse->trueBody()->instructions();
		std::vector<Pointer<TvmAstNode>> f = cmd1IfElse->falseBody()->instructions();
		trimLoc(t);
		trimLoc(f);
		if (!t.empty() &&
			!f.empty() &&
			*t.back() == *f.back() &&
			!cmd1IfElse->withJmp() &&
			cmd1IfElse->trueBody()->type() == CodeBlock::Type::PUSHCONT &&
			cmd1IfElse->falseBody()->type() == CodeBlock::Type::PUSHCONT &&
			!hasRetOrJmp(cmd1IfElse->trueBody().get()) &&
			!hasRetOrJmp(cmd1IfElse->falseBody().get())) {
			auto tt = createNode<
				CodeBlock>(CodeBlock::Type::PUSHCONT, std::vector<Pointer<TvmAstNode>>(t.begin(), t.end() - 1));
			auto ff = createNode<
				CodeBlock>(CodeBlock::Type::PUSHCONT, std::vector<Pointer<TvmAstNode>>(f.begin(), f.end() - 1));
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
		if (qtyWithoutLoc(f) == 0 && !cmd1IfElse->withJmp()) {
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
		if (qtyWithoutLoc(t) == 0 && !cmd1IfElse->withJmp()) {
			auto ifElse2 = createNode<TvmIfElse>(!cmd1IfElse->withNot(), false, cmd1IfElse->falseBody(), nullptr, 0);
			return Result{1, ifElse2};
		}
	}

	// PUSHCONT { a }
	// PUSHCONT { b }
	// IFELSE
	// =>
	// newA
	// newB
	// CONDSEL
	if (cmd1IfElse && cmd1IfElse->falseBody() != nullptr) {
		std::vector<Pointer<TvmAstNode>> const& t = cmd1IfElse->trueBody()->instructions();
		std::vector<Pointer<TvmAstNode>> const& f = cmd1IfElse->falseBody()->instructions();
		if (qtyWithoutLoc(t) == 1 && qtyWithoutLoc(f) == 1) {
			int ti = nextCommandLine(0, t);
			int fi = nextCommandLine(0, f);
			Pointer<TvmAstNode> a = t.at(ti);
			Pointer<TvmAstNode> b = f.at(fi);
			if ((isPureGen01(*a) && convertToStackGen(a.get())) ||
				convertToGlob(a.get()) ||
				convertToPushCellOrSlice(a.get()) ||
				isPUSH(a)) {
				Pointer<TvmAstNode> newA = a;
				if (auto index = isPUSH(a))
					newA = makePUSH(*index + 1); // +1 because condition flag

				Pointer<TvmAstNode> newB;
				if (convertToStackGen(b.get()) || convertToGlob(b.get()) || convertToPushCellOrSlice(b.get()))
					newB = b;
				else if (auto index = isPUSH(b))
					newB = makePUSH(*index + 2); // +2 because condition flag and value from first branch

				if (newB)
					return Result{1, newA, newB, gen("CONDSEL")};
			}
		}
	}

	// PUSHCONT { TRUE }
	// PUSHCONT { ... }
	// WHILE
	// =>
	// PUSHCONT { ... }
	// AGAIN
	if (auto _while = convertToWhile(cmd1.get())) {
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
		for (Pointer<TvmAstNode> const& cmd: cmd1Sub->block()->instructions()) {
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
		std::vector<Pointer<TvmAstNode>> const& opcodes = cmd1CodeBlock->instructions();
		if (qtyWithoutLoc(opcodes) == 1) {
			int index = nextCommandLine(0, opcodes);
			TvmAstNode const* opcode = opcodes.at(index).get();
			if (auto sub = convertToSubProgram(opcode)) {
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
	if (m_flags.test(static_cast<size_t>(OptFlags::UnpackOpaque)) && cmd1IfElse) {
		for (bool isZero: {true, false}) {
			if (isZero && *GlobalParams::g_tvmVersion == langutil::TVMVersion::ton()) {
				// ignore ZERO SWAP/ROTR IF [NOT]
				continue;
			}
			for (bool isSwap: {true, false}) {
				for (bool trueBranch: {true, false}) {
					Pointer<CodeBlock> curBranch = trueBranch ? cmd1IfElse->trueBody() : cmd1IfElse->falseBody();
					if (!curBranch) {
						continue;
					}
					if (f(isZero, isSwap, curBranch->instructions())) {
						Pointer<AsymGen> align =
							getZeroOrNullAlignment(isZero, !isSwap, !trueBranch || cmd1IfElse->withNot());
						solAssert(trueBranch ? true : !cmd1IfElse->withNot(), "");
						auto emptyBlock = createNode<CodeBlock>(curBranch->type());
						return Result{
							1,
							align,
							createNode<TvmIfElse>(
								cmd1IfElse->withNot(),
								cmd1IfElse->withJmp(),
								trueBranch ? emptyBlock : cmd1IfElse->trueBody(),
								trueBranch ? cmd1IfElse->falseBody() : emptyBlock,
								cmd1IfElse->ret()
							)
						};
					}
				}
			}
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt2(
	Pointer<TvmAstNode> const& cmd1,
	Pointer<TvmAstNode> const& cmd2,
	int idx2
) const {
	using namespace MathConsts;
	auto cmd1PushCellOrSlice = convertToPushCellOrSlice(cmd1.get());
	auto cmd1Gen = convertToGen(cmd1.get());
	auto cmd1GenOp = convertToStackGen(cmd1.get());
	auto cmd1Glob = convertToGlob(cmd1.get());
	auto cmd1Ret = convertToTvmReturn(cmd1.get());

	auto cmd2Exc = convertToTvmException(cmd2.get());
	auto cmd2GenOpcode = convertToStackGen(cmd2.get());
	auto cmd2Glob = convertToGlob(cmd2.get());
	auto cmd2IfElse = convertToTvmIfElse(cmd2.get());
	auto cmd2Sub = convertToSubProgram(cmd2.get());

	auto _isBLKDROP1 = isBLKDROP2(cmd1);
	auto _isBLKDROP2 = isBLKDROP2(cmd2);
	auto isBLKPUSH1 = isBLKPUSH(cmd1);
	auto isPUSH1 = isPUSH(cmd1);

	if (isSWAP(cmd1)) {
		if (is(cmd2, "SUB"))
			return Result{2, gen("SUBR")};
		if (is(cmd2, "SUBR"))
			return Result{2, gen("SUB")};
		if (isCommutative(cmd2))
			return Result{1};
	}
	if (is(cmd1, "PUSHINT")) {
		if (arg(cmd1) == "1") {
			if (is(cmd2, "ADD"))
				return Result{2, gen("INC")};
			if (is(cmd2, "SUB"))
				return Result{2, gen("DEC")};
		}
		bigint value = pushintValue(cmd1);
		if (-128 <= value && value <= 127) {
			if (is(cmd2, "ADD"))
				return Result{2, gen("ADDCONST " + toString(value))};
			if (is(cmd2, "MUL"))
				return Result{2, gen("MULCONST " + toString(value))};
		}
		if (-128 <= -value && -value <= 127) {
			if (is(cmd2, "SUB"))
				return Result{2, gen("ADDCONST " + toString(-value))};
		}
	}

	// stack opcode
	// THROW N
	// =>
	// THROW N
	if ((cmd1->category() == TvmAstNode::Category::Stack || (cmd1Gen && cmd1Gen->isPure())) && isExc(cmd2, "THROW")) {
		return Result{2, cmd2};
	}

	// [NOT]RET[ALT] / THROW[ANY]
	// cmd2
	// =>
	// [NOT]RET[ALT] / THROW[ANY]
	if ((cmd1Ret && !cmd1Ret->withIf()) || isExc(cmd1, "THROWANY", "THROW")) {
		// delete commands after non return opcode
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
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) && isBLKPUSH1 && isDrop(cmd2)) {
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
	// BLKDROP2 N, M   / NIP
	// =>
	// BLKDROP2 N, M-1 / DROP
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
	if (isPUSH(cmd1) && _isBLKDROP2) {
		int n = *isPUSH(cmd1);
		auto [down, top] = _isBLKDROP2.value();
		if (n + 2 <= top)
			return Result{2, makeBLKDROP2(down, top - 1), cmd1};
		else if (n >= down + top - 1)
			return Result{2, makeBLKDROP2(down, top - 1), makePUSH(n - down)};
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
	if (isPUSH1 && *isPUSH1 == 0 && _isBLKDROP2 && _isBLKDROP2.value().second == 1) {
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
		auto subProg = createNode<SubProgram>(false, cmd2IfElse->trueBody());
		int toRemove = 2;
		if (cmd2IfElse->withJmp()) {
			toRemove += qtyCmdAfter(idx2);
		}
		return Result{toRemove, subProg};
	}

	// BLKSWAP  down, up
	// BLKDROP2 drop, rest where drop==up and rest==down
	// ...
	// DROP up
	if (isBLKSWAP(cmd1) && isBLKDROP2(cmd2)) {
		auto [down, up] = isBLKSWAP(cmd1).value();
		auto [drop, rest] = isBLKDROP2(cmd2).value();
		if (drop == up && rest == down)
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
	if (is(cmd1, "TUPLE") && is(cmd2, "UNTUPLE") && fetchInt(cmd1) == fetchInt(cmd2)) {
		return Result{2};
	}
	if (is(cmd1, "UNTUPLE") && is(cmd2, "TUPLE") && fetchInt(cmd1) == fetchInt(cmd2)) {
		return Result{2};
	}
	// SETGLOB N
	// GETGLOB N
	//
	// DUP
	// SETGLOB N
	if (cmd1Glob &&
		cmd1Glob->opcode() == Glob::Opcode::SetOrSetVar &&
		cmd2Glob &&
		cmd2Glob->opcode() == Glob::Opcode::GetOrGetVar &&
		cmd1Glob->index() == cmd2Glob->index()) {
		return Result{2, makePUSH(0), makeSetGlob(cmd1Glob->index())};
	}
	// PUSHINT N
	// ADDCONST ? | INC | DEC
	//
	// PUSHINT (N+delta)
	if (is(cmd1, "PUSHINT") && isConstAdd(cmd2)) {
		bigint n = pushintValue(cmd1);
		bigint delta = getAddNum(cmd2);
		bigint sum = n + delta;
		if (isInRange257(sum))
			return Result{2, gen("PUSHINT " + toString(sum))};
	}
	// PUSHINT N
	// UFITS ? | FITS ?
	//
	// PUSHINT N
	if (is(cmd1, "PUSHINT") && is(cmd2, "UFITS", "FITS")) {
		bigint n = pushintValue(cmd1);
		int bits = fetchInt(cmd2);
		auto type = TypeProvider::
			integer(bits, is(cmd2, "UFITS") ? IntegerType::Modifier::Unsigned : IntegerType::Modifier::Signed);
		if (type->minValue() <= n && n <= type->maxValue())
			return Result{2, gen("PUSHINT " + toString(n))};
	}
	if (isConstAdd(cmd1) && isConstAdd(cmd2)) {
		int final_add = getAddNum(cmd1) + getAddNum(cmd2);
		if (-128 <= final_add && final_add <= 127)
			return Result{2, gen("ADDCONST " + std::to_string(final_add))};
	}
	if (is(cmd1, "INDEX_NOEXCEP", "INDEX_EXCEP") &&
		0 <= fetchInt(cmd1) &&
		fetchInt(cmd1) <= 3 &&
		is(cmd2, "INDEX_NOEXCEP", "INDEX_EXCEP") &&
		0 <= fetchInt(cmd2) &&
		fetchInt(cmd2) <= 3) {
		return Result{2, gen("INDEX2 " + arg(cmd1) + ", " + arg(cmd2))};
	}
	if (is(cmd1, "INDEX2") && is(cmd2, "INDEX_NOEXCEP", "INDEX_EXCEP") && 0 <= fetchInt(cmd2) && fetchInt(cmd2) <= 3) {
		auto [i, j] = getIndexes(arg(cmd1));
		if (0 <= i && i <= 3 && 0 <= j && j <= 3) {
			return Result{2, gen("INDEX3 " + toString(i) + ", " + toString(j) + ", " + arg(cmd2))};
		}
	}
	// PUSHINT N
	// RSHIFT / LSHIFT
	// =>
	// RSHIFT N / LSHIFT N
	if (is(cmd1, "PUSHINT") &&
		1 <= pushintValue(cmd1) &&
		pushintValue(cmd1) <= 256 &&
		is(cmd2, "RSHIFT", "LSHIFT") &&
		arg(cmd2).empty()) {
		return Result{2, gen(cmd2GenOpcode->opcode() + " " + arg(cmd1))};
	}
	// PUSHINT N
	// STUX[R] / STIX[R]
	// =>
	// [SWAP]
	// STU N / STI N
	if (is(cmd1, "PUSHINT") &&
		1 <= pushintValue(cmd1) &&
		pushintValue(cmd1) <= 256 &&
		is(cmd2, "STUX", "STIX", "STUXR", "STIXR")) {
		std::vector<Pointer<TvmAstNode>> commands;
		auto opcode = cmd2GenOpcode->opcode();
		if (opcode[opcode.length() - 1] == 'R') {
			commands.push_back(makeXCH_S(1));
		}
		auto newOpcode = opcode.substr(0, 3);
		commands.push_back(gen(newOpcode + " " + arg(cmd1)));
		return Result{2, commands};
	}
	// PUSHINT N
	// STUXR / STIXR
	// =>
	// STUR N / STIR N
	if (is(cmd1, "PUSHINT") && 1 <= pushintValue(cmd1) && pushintValue(cmd1) <= 256 && is(cmd2, "STUX", "STIX")) {
		return Result{2, gen(cmd2GenOpcode->opcode().substr(0, 3) + " " + arg(cmd1))};
	}
	// PUSHINT 2**N
	// DIV / MUL
	// =>
	// RSHIFT N / LSHIFT N
	if (is(cmd1, "PUSHINT") && is(cmd2, "DIV", "MUL")) {
		bigint val = pushintValue(cmd1);
		if (power2Exp().contains(val)) {
			std::string const& newOp = is(cmd2, "DIV") ? "RSHIFT" : "LSHIFT";
			int const n = power2Exp().at(val);
			if (n > 0)
				return Result{2, gen(newOp + " " + toString(n))};
		}
	}
	// PUSHINT 2**N
	// MOD
	// =>
	// MODPOW2 N
	if (is(cmd1, "PUSHINT") && is(cmd2, "MOD")) {
		bigint val = pushintValue(cmd1);
		if (power2Exp().contains(val)) {
			int power = power2Exp().at(val);
			if (power > 0)
				return Result{2, gen("MODPOW2 " + toString(power))};
		}
	}
	// PUSHINT (2**N)-1
	// AND
	// =>
	// MODPOW2 N
	if (is(cmd1, "PUSHINT") && is(cmd2, "AND")) {
		bigint val = pushintValue(cmd1);
		if (power2DecExp().contains(val)) {
			int power = power2DecExp().at(val);
			if (power > 0)
				return Result{2, gen("MODPOW2 " + toString(power))};
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
	if (isBLKSWAP(cmd1) && isDrop(cmd2)) {
		auto [bottom, top] = isBLKSWAP(cmd1).value();
		int n = isDrop(cmd2).value();
		if (n == bottom) {
			return Result{2, makeBLKDROP2(n, top)};
		}
	}

	// LESS | LEQ    | GREATER | GEQ  | EQUAL | NEQ   | EQINT  | NEQINT | NOT | TRUE  | FALSE
	// NOT
	// =>
	// GEQ | GREATER | LEQ     | LESS | NEQ   | EQUAL | NEQINT | EQINT  |     | FALSE | TRUE
	if (is(cmd2, "NOT")) {
		if (is(cmd1, "LESS"))
			return Result{2, gen("GEQ")};
		if (is(cmd1, "LEQ"))
			return Result{2, gen("GREATER")};
		if (is(cmd1, "GREATER"))
			return Result{2, gen("LEQ")};
		if (is(cmd1, "GEQ"))
			return Result{2, gen("LESS")};
		if (is(cmd1, "EQUAL"))
			return Result{2, gen("NEQ")};
		if (is(cmd1, "NEQ"))
			return Result{2, gen("EQUAL")};

		if (is(cmd1, "LESSINT")) { // !(x < value) => x >= value => x > value-1
			int value = fetchInt(cmd1);
			if (-128 <= value - 1 && value - 1 < 128)
				return Result{2, gen("GTINT " + toString(value - 1))};
		}
		if (is(cmd1, "GTINT")) { // !(x > value) => x <= value => x < value+1
			int value = fetchInt(cmd1);
			if (-128 <= value + 1 && value + 1 < 128)
				return Result{2, gen("LESSINT " + toString(value + 1))};
		}
		if (is(cmd1, "EQINT"))
			return Result{2, gen("NEQINT " + arg(cmd1))};
		if (is(cmd1, "NEQINT"))
			return Result{2, gen("EQINT " + arg(cmd1))};

		if (is(cmd1, "NOT"))
			return Result{2};

		if (is(cmd1, "TRUE"))
			return Result{2, gen("FALSE")};
		if (is(cmd1, "FALSE"))
			return Result{2, gen("TRUE")};
	}

	if ((is(cmd1, "UFITS") && is(cmd2, "UFITS")) || (is(cmd1, "FITS") && is(cmd2, "FITS"))) {
		int bitSize = std::min(fetchInt(cmd1), fetchInt(cmd2));
		return Result{2, gen(cmd1GenOp->opcode() + " " + toString(bitSize))};
	}
	if (is(cmd1, "TRUE", "FALSE") && is(cmd2, "STIR") && fetchInt(cmd2) == 1) {
		if (is(cmd1, "FALSE"))
			return Result{2, gen("STSLICECONST 0")};
		return Result{2, gen("STSLICECONST 1")};
	}
	if (is(cmd1, "ABS") && is(cmd2, "UFITS") && fetchInt(cmd2) == 256) {
		return Result{2, gen("ABS")};
	}

	if (is(cmd1, "PUSHINT") && pushintValue(cmd1) == 1 && is(cmd2, "STZEROES")) {
		return Result{2, gen("STSLICECONST 0")};
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

	// s01
	// XCHG S1, S2
	// =>
	// SWAP
	// s01
	if (isPureGen01(*cmd1) && isXCHG(cmd2, 1, 2)) {
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
		if (auto lc = convertToLogCircuit(cmd2.get())) {
			if (lc->type() == LogCircuit::Type::AND && lc->body()->instructions().size() == 2) {
				auto cmd2_0 = lc->body()->instructions().at(0);
				auto cmd2_1 = lc->body()->instructions().at(1);
				auto _true = convertToStackGen(cmd2_1.get());
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
	auto _true = convertToStackGen(cmd1.get());
	auto _and = convertToStackGen(cmd2.get());
	if (_true && _true->opcode() == "TRUE" && _and && _and->opcode() == "AND") {
		return Result{2};
	}

	// NULL
	// ISNULL
	// =>
	// TRUE
	if (is(cmd1, "NULL") && is(cmd2, "ISNULL")) {
		return Result{2, gen("TRUE")};
	}

	// PUSHINT N
	// ISNULL
	// =>
	// FALSE
	if (is(cmd1, "PUSHINT") && is(cmd2, "ISNULL")) {
		return Result{2, gen("FALSE")};
	}

	// TRUE       / FALSE
	// THROWIFNOT / THROWIF
	// =>
	//
	if ((is(cmd1, "TRUE") && isExc(cmd2, "THROWIFNOT")) || (is(cmd1, "FALSE") && isExc(cmd2, "THROWIF"))) {
		return Result{2};
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
	if (cmd1GenOp &&
		cmd1GenOp->isPure() &&
		std::make_pair(cmd1GenOp->take(), cmd1GenOp->ret()) == std::make_pair(1, 1) &&
		isDrop(cmd2)) {
		return Result{2, cmd2};
	}

	// ABS
	// MODPOW2 256
	// =>
	// ABS
	if (is(cmd1, "ABS") && cmd2GenOpcode && cmd2GenOpcode->opcode() == "MODPOW2" && cmd2GenOpcode->arg() == "256") {
		return Result{2, gen("ABS")};
	}

	// MODPOW2 x
	// MODPOW2 y
	// =>
	// MODPOW2 min(x, y)
	if (is(cmd1, "MODPOW2") && is(cmd2, "MODPOW2")) {
		int x = fetchInt(cmd1);
		int y = fetchInt(cmd2);
		int power = std::min(x, y);
		if (power > 0)
			return Result{2, gen("MODPOW2 " + toString(power))};
	}

	// BLKPUSH N, 0 / DUP
	// BLKPUSH Q, 0 / DUP
	// =>
	// BLKPUSH N+Q, 0
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) && isBLKPUSH(cmd1) && isBLKPUSH(cmd2)) {
		auto [qty0, index0] = isBLKPUSH(cmd1).value();
		auto [qty1, index1] = isBLKPUSH(cmd2).value();
		if (index0 == 0 && index1 == 0 && qty0 + qty1 <= 15)
			return Result{2, makeBLKPUSH(qty0 + qty1, 0)};
	}

	// LD[I|U] N / LDDICT / LDREF / LD[I|U]X N
	// DROP
	if (is(cmd1, "LDU", "LDI", "LDREF", "LDDICT", "LDUX", "LDIX", "LDSLICE", "LDSLICEX") && isDrop(cmd2)) {
		// TODO add LD[I|U]LE[4|8]
		int n = isDrop(cmd2).value();
		Pointer<StackGen> newOpcode = is(cmd1, "LDREF") ? gen("PLDREFIDX 0") : gen("P" + cmd1GenOp->fullOpcode());
		if (n == 1) {
			return Result{2, newOpcode};
		} else {
			return Result{2, {newOpcode, makeDROP(n - 1)}};
		}
	}

	// 26 + 118 gas units
	// PLDREFIDX 0
	// CTOS
	// =>
	// 118 + 18 gas units
	// LDREFRTOS
	// NIP
	if (is(cmd1, "PLDREFIDX") && arg(cmd1) == "0" && is(cmd2, "CTOS")) {
		return Result{2, gen("LDREFRTOS"), makeBLKDROP2(1, 1)};
	}

	// gen0M
	// PUSHCONT {
	//    here
	// }
	// CALLX
	// =>
	// PUSHCONT {
	//    gen0M
	//    here
	// }
	// CALLX
	if (cmd1Gen &&
		cmd1Gen->isPure() &&
		cmd1Gen->take() == 0 &&
		cmd1Gen->ret() == 1 &&
		cmd2Sub &&
		!cmd2Sub->isJmp() &&
		cmd2Sub->block()->type() == CodeBlock::Type::PUSHCONT) {
		std::vector<Pointer<TvmAstNode>> instructions;
		{
			instructions.push_back(cmd1);
			auto x = cmd2Sub->block()->instructions();
			instructions.insert(instructions.end(), x.begin(), x.end());
		}
		auto newBlock = createNode<CodeBlock>(cmd2Sub->block()->type(), instructions);
		auto subProg = createNode<SubProgram>(
			cmd2Sub->isJmp(), // false
			newBlock
		);
		return Result{2, subProg};
	}

	// TUPLE X
	// DROP N
	// =>
	// DROP N-1+X
	if (is(cmd1, "TUPLE") && isDrop(cmd2)) {
		int newDropN = isDrop(cmd2).value() - 1 + fetchInt(cmd1);
		return Result{2, makeDROP(newDropN)};
	}

	// PUSHREF { xxx }
	// CTOS
	// =>
	// PUSH[REF]SLICE { xxx }
	if (cmd1PushCellOrSlice && cmd1PushCellOrSlice->type() == CellOrSliceOperation::Type::PUSHREF && is(cmd2, "CTOS")) {
		if (cmd1PushCellOrSlice->child() == nullptr) {
			std::string data = cmd1PushCellOrSlice->blob();
			if (data.empty() || data == "x") {
				data = "x8_";
			}
			return Result{2, genPushSlice(data)};
		}
		return Result{
			2,
			createNode<CellOrSliceOperation>(
				CellOrSliceOperation::Type::PUSHREFSLICE,
				cmd1PushCellOrSlice->blob(),
				cmd1PushCellOrSlice->child()
			)
		};
	}

	// PUSHREF { XXX }
	// STREFR
	// =>
	// STREFCONST { XXX }
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) &&
		cmd1PushCellOrSlice &&
		cmd1PushCellOrSlice->type() == CellOrSliceOperation::Type::PUSHREF &&
		is(cmd2, "STREFR")) {
		return Result{
			2,
			createNode<CellOrSliceOperation>(
				CellOrSliceOperation::Type::STREFCONST,
				cmd1PushCellOrSlice->blob(),
				cmd1PushCellOrSlice->child()
			)
		};
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt3(
	Pointer<TvmAstNode> const& cmd1,
	Pointer<TvmAstNode> const& cmd2,
	Pointer<TvmAstNode> const& cmd3
) const {
	auto isPUSH1 = isPUSH(cmd1);
	auto cmd1PushCellOrSlice = convertToPushCellOrSlice(cmd1.get());
	auto isPUSH2 = isPUSH(cmd2);
	auto cmd2PushCellOrSlice = convertToPushCellOrSlice(cmd2.get());
	auto cmd3GenOpcode = convertToStackGen(cmd3.get());
	auto cmd3SubProgram = convertToSubProgram(cmd3.get());

	// DUP
	// THROWIFNOT 507
	// DROP n
	if (isPUSH1 && *isPUSH1 == 0 && isExc(cmd2, "THROWIFNOT", "THROWIF") && isDrop(cmd3)) {
		int n = isDrop(cmd3).value();
		if (n == 1)
			return Result{3, cmd2};
		return Result{3, cmd2, makeDROP(n - 1)};
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
	// ADD | MUL | MAX
	//
	// PUSHINT A+B | PUSHINT A*B | PUSHINT max(A*B)
	if (is(cmd1, "PUSHINT") &&
		is(cmd2, "PUSHINT") &&
		cmd3GenOpcode &&
		isIn(cmd3GenOpcode->opcode(), "ADD", "MUL", "MAX")) {
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
		if (isInRange257(c))
			return Result{3, gen("PUSHINT " + toString(c))};
	}
	// PUSHINT A
	// PUSHINT B
	// DIV
	//
	// PUSHINT A/B
	if (is(cmd1, "PUSHINT") && is(cmd2, "PUSHINT") && cmd3GenOpcode && cmd3GenOpcode->opcode() == "DIV") {
		bigint a = pushintValue(cmd1);
		bigint b = pushintValue(cmd2);
		if (a >= 0 && b > 0) { // note in TVM  -9 / 2 == -5
			bigint c = a / b;
			if (isInRange257(c))
				return Result{3, gen("PUSHINT " + toString(c))};
		}
	}

	// PUSHINT
	// PUSH SN / gen01
	// ADD / MUL
	//
	// PUSH S(N-1) / gen01
	// ADDCONST / MULCONST
	if (is(cmd1, "PUSHINT") && is(cmd3, "ADD", "MUL")) {
		bigint val = pushintValue(cmd1);
		if (-128 <= val && val <= 127) {
			if ((isPureGen01(*cmd2) && convertToStackGen(cmd2.get())) || convertToGlob(cmd2.get()) || isPUSH(cmd2)) {
				Pointer<TvmAstNode> newCmd;
				if (convertToStackGen(cmd2.get()) || convertToGlob(cmd2.get())) {
					newCmd = cmd2;
				} else if (auto index = isPUSH(cmd2); index.has_value() && *index > 0) {
					newCmd = makePUSH(*index - 1);
				}
				if (newCmd)
					return Result{3, newCmd, gen((is(cmd3, "ADD") ? "ADDCONST " : "MULCONST ") + toString(val))};
			}
		}
	}

	// TRUE
	// NEWC
	// STI 1
	if (is(cmd1, "TRUE", "FALSE") && is(cmd2, "NEWC") && is(cmd3, "STI") && arg(cmd3) == "1") {
		if (is(cmd1, "TRUE"))
			return Result{3, gen("NEWC"), gen("STSLICECONST 1")};
		return Result{3, gen("NEWC"), gen("STSLICECONST 0")};
	}

	if (isBLKSWAP(cmd1) && isPureGen01(*cmd2) && isBLKSWAP(cmd3)) {
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
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) && // ?
		isPureGen01(*cmd1) && isBLKPUSH(cmd2) && isPureGen01(*cmd3) && *cmd1 == *cmd3) {
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


	if (cmd1PushCellOrSlice &&
		cmd1PushCellOrSlice->type() == CellOrSliceOperation::Type::PUSHREF &&
		cmd2PushCellOrSlice &&
		cmd2PushCellOrSlice->type() == CellOrSliceOperation::Type::PUSHREF) {
		if (cmd3SubProgram) {
			std::vector<Pointer<TvmAstNode>> const& instructions = cmd3SubProgram->block()->instructions();
			if (instructions.size() == 1 && isInline(*instructions.at(0), "__concatenateStrings")) {
				std::string hexStr = cmd1PushCellOrSlice->chainBlob() + cmd2PushCellOrSlice->chainBlob();
				return Result{3, makePushCellOrSlice(hexStr, false)};
			}
		}
	}

	// PUSHINT N
	// PUSHINT 0
	// SPLIT
	// =>
	// LDSLICE N if A <= 256
	//
	// PUSHINT N otherwise
	// LDSLICEX
	if (is(cmd1, "PUSHINT") && is(cmd2, "PUSHINT") && pushintValue(cmd2) == 0 && is(cmd3, "SPLIT")) {
		bigint n = pushintValue(cmd1);
		if (n <= 256)
			return Result{3, gen("LDSLICE " + toString(n))};
		return Result{3, cmd1, gen("LDSLICEX")};
	}

	// PUSHINT N
	// PUSHINT 0
	// SSKIPFIRST
	// =>
	// PUSHINT N
	// SDSKIPFIRST
	if (is(cmd1, "PUSHINT") && is(cmd2, "PUSHINT") && pushintValue(cmd2) == 0 && is(cmd3, "SSKIPFIRST")) {
		bigint n = pushintValue(cmd1);
		return Result{3, cmd1, gen("SDSKIPFIRST")};
	}

	// ENDC
	// SWAP
	// STREF
	// =>
	// STBREFR
	if (is(cmd1, "ENDC") && isSWAP(cmd2) && is(cmd3, "STREF")) {
		return Result{3, gen("STBREFR")};
	}

	// PUSHINT N
	// PUSH Si / ROLL i
	// STU / STI
	// =>
	// PUSH Si
	// STSLICECONST
	if (is(cmd1, "PUSHINT") && is(cmd3, "STU", "STI")) {
		bool ok = false;
		int newSi;
		bool isFirstCase = false;
		if (isBLKSWAP(cmd2)) {
			isFirstCase = true;
			auto [down, up] = isBLKSWAP(cmd2).value();
			ok = down == 1 && up >= 2;
			newSi = up - 1;
		} else if (isPUSH2) {
			ok = *isPUSH2 > 0;
			newSi = *isPUSH2 - 1;
		}

		if (ok) {
			auto value = pushintValue(cmd1);
			int len = fetchInt(cmd3);
			auto binStr = StrUtils::toBitString(value, len, is(cmd3, "STI"));
			if (binStr) {
				auto slice = StrUtils::binaryStringToSlice(*binStr);
				if (binStr->length() <= TvmConst::MaxSTSLICECONST) {
					auto opcode = isFirstCase ? makeBLKSWAP(1, newSi) : makePUSH(newSi);
					return Result{3, opcode, gen("STSLICECONST x" + slice)};
				}
			}
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt4(
	Pointer<TvmAstNode> const& cmd1,
	Pointer<TvmAstNode> const& cmd2,
	Pointer<TvmAstNode> const& cmd3,
	Pointer<TvmAstNode> const& cmd4
) {
	auto cmd3Exc = convertToTvmException(cmd3.get());

	if (is(cmd1, "PUSHINT") && is(cmd3, "PUSHINT")) {
		if (isAddOrSub(cmd2) && isAddOrSub(cmd4)) {
			bigint sum = 0;
			sum += (is(cmd2, "ADD") ? +1 : -1) * pushintValue(cmd1);
			sum += (is(cmd4, "ADD") ? +1 : -1) * pushintValue(cmd3);
			// TODO DELETE
			if (isInRange257(sum))
				return Result{4, gen("PUSHINT " + toString(sum)), gen("ADD")};
		}
	}
	if (isPlainPushSlice(cmd1) && is(cmd2, "NEWC") && is(cmd3, "STSLICECONST") && is(cmd4, "STSLICE")) {
		std::optional<std::string> slice = StrUtils::unitSlices(arg(cmd3), isPlainPushSlice(cmd1)->blob());
		if (slice.has_value()) {
			return Result{4, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
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
		for (std::string fit: {"UFITS", "FITS"}) {
			if (is(cmd2, fit) && is(cmd4, fit) && arg(cmd2) == arg(cmd4)) {
				int final_add = getAddNum(cmd1) + getAddNum(cmd3);
				if (-128 <= final_add && final_add <= 127)
					return Result{4, gen("ADDCONST " + std::to_string(final_add)), gen(fit + " " + arg(cmd2))};
			}
		}
	}

	if (is(cmd1, "PUSHINT") && is(cmd2, "NEWC") && is(cmd3, "STSLICECONST") && is(cmd4, "STU", "STI")) {
		std::string bitStr = StrUtils::toBitString(arg(cmd3));
		if (auto x = StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd4), is(cmd4, "STI"))) {
			bitStr += x.value();
			std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
			if (slice.has_value())
				return Result{4, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
		}
	}

	// DUP
	// ISNULL
	// THROWIF 63
	// UNSINGLE
	// =>
	// UNSINGLE
	if (isPUSH(cmd1) &&
		isPUSH(cmd1).value() == 0 &&
		is(cmd2, "ISNULL") &&
		cmd3Exc &&
		cmd3Exc->opcode() == "THROWIF" &&
		cmd3Exc->arg() == toString(TvmConst::RuntimeException::GetOptionalException) &&
		is(cmd4, "UNTUPLE") &&
		fetchInt(cmd4) == 1) {
		return Result{4, cmd4};
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt5(
	Pointer<TvmAstNode> const& cmd1,
	Pointer<TvmAstNode> const& cmd2,
	Pointer<TvmAstNode> const& cmd3,
	Pointer<TvmAstNode> const& cmd4,
	Pointer<TvmAstNode> const& cmd5
) {
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
		is(cmd5, "STSLICE")) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd2)->blob()) +
							 StrUtils::toBitString(isPlainPushSlice(cmd1)->blob());
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{5, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
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
		is(cmd5, "STU", "STI")) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd2)->blob());
		if (auto v = StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd5), is(cmd5, "STI"))) {
			bitStr += v.value();
			std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
			if (slice.has_value()) {
				return Result{5, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
			}
		}
	}

	// NULL
	// PUSHSLICE ?
	// NEWC
	// STSLICE ?
	// STDICT
	if (is(cmd1, "NULL") && isPlainPushSlice(cmd2) && is(cmd3, "NEWC") && is(cmd4, "STSLICE") && is(cmd5, "STDICT")) {
		std::string bitStr = StrUtils::toBitString(isPlainPushSlice(cmd2)->blob()) + "0";
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitStr, "");
		if (slice.has_value()) {
			return Result{5, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
		}
	}

	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAt6(
	Pointer<TvmAstNode> const& cmd1,
	Pointer<TvmAstNode> const& cmd2,
	Pointer<TvmAstNode> const& cmd3,
	Pointer<TvmAstNode> const& cmd4,
	Pointer<TvmAstNode> const& cmd5,
	Pointer<TvmAstNode> const& cmd6
) {
	if (isPlainPushSlice(cmd1) &&
		is(cmd2, "NEWC") &&
		is(cmd3, "STSLICE") &&
		is(cmd4, "NEWC") &&
		is(cmd5, "STSLICECONST") &&
		is(cmd6, "STB")) {
		std::string str1 = StrUtils::toBitString(isPlainPushSlice(cmd1)->blob());
		std::string str5 = StrUtils::toBitString(arg(cmd5));
		std::optional<std::string> slice = StrUtils::unitBitStringToHex(str5, str1);
		if (slice.has_value()) {
			return Result{6, genPushSlice(*slice), gen("NEWC"), gen("STSLICE")};
		}
	}
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::optimizeAtInf(int idx1) const {
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	auto cmd1IfElse = convertToTvmIfElse(cmd1.get());
	auto cmd1Ret = convertToTvmReturn(cmd1.get());
	auto cmd1Sub = convertToSubProgram(cmd1.get());
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
	if (cmd1IfElse &&
		cmd1IfElse->falseBody() == nullptr &&
		cmd1IfElse->withJmp() &&
		cmd1IfElse->withNot() &&
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
		int removedOpcodeQty = 0;
		std::vector<Pointer<TvmAstNode>> takenOpcodes;
		bool withBuilder = false;
		{
			int j = nextCommandLine(i);
			int k = nextCommandLine(j);
			Pointer<TvmAstNode> const& cmd2 = get(j);
			Pointer<TvmAstNode> const& cmd3 = get(k);

			// PUSHSLICE ?
			// NEWC
			// STSLICE
			if (k != -1 && isPlainPushSlice(cmd1) && is(cmd2, "NEWC") && is(cmd3, "STSLICE")) {
				withBuilder = true;
				std::string hexSlice = isPlainPushSlice(cmd1)->blob();
				bitString += StrUtils::toBitString(hexSlice);
				removedOpcodeQty += 3;
				takenOpcodes.emplace_back(cmd1);
				takenOpcodes.emplace_back(cmd2);
				takenOpcodes.emplace_back(cmd3);
				i = nextCommandLine(k);
			}
			// PUSHINT ?
			// NEWC
			// STU y | STI y
			else if (is(cmd1, "PUSHINT") && is(cmd2, "NEWC") && is(cmd3, "STU", "STI")) {
				if (auto bitStr = StrUtils::toBitString(pushintValue(cmd1), fetchInt(cmd3), is(cmd3, "STI"))) {
					withBuilder = true;
					bitString += bitStr.value();
					removedOpcodeQty += 3;
					takenOpcodes.emplace_back(cmd1);
					takenOpcodes.emplace_back(cmd2);
					takenOpcodes.emplace_back(cmd3);
					i = nextCommandLine(k);
				}
			} else if (is(cmd1, "NEWC")) {
				withBuilder = true;
				removedOpcodeQty += 1;
				takenOpcodes.emplace_back(cmd1);
				i = j;
			}
		}

		while (i != -1) {
			int j = nextCommandLine(i);
			int k = nextCommandLine(j);
			Pointer<TvmAstNode> const& c1 = get(i);
			Pointer<TvmAstNode> c2 = get(j);
			Pointer<TvmAstNode> c3 = get(k);

			// TODO ADD LD[I|U]LE[4|8]
			int curOpcodeQty = 0;
			if (is(c1, "STSLICECONST")) {
				bitString += StrUtils::toBitString(arg(c1));
				curOpcodeQty = 1;
				i = j;
			} else if (c2 && c3 && is(c1, "PUSHINT") && isSWAP(c2) && (is(c3, "STU") || is(c3, "STI"))) {
				bigint num = pushintValue(c1);
				int len = fetchInt(c3);
				if (auto bitStr = StrUtils::toBitString(num, len, is(c3, "STI"))) {
					bitString += bitStr.value();
					curOpcodeQty = 3;
					i = nextCommandLine(k);
				} else
					break;
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STVARUINT16")) {
				bigint num = pushintValue(c1);
				bitString += StrUtils::tonsToBinaryString(num);
				curOpcodeQty = 2;
				i = nextCommandLine(j);
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STZEROES")) {
				int len = fetchInt(c1);
				bitString += std::string(len, '0');
				curOpcodeQty = 2;
				i = nextCommandLine(j);
			} else if (c2 && is(c1, "PUSHINT") && is(c2, "STONES")) {
				int len = fetchInt(c1);
				bitString += std::string(len, '1');
				curOpcodeQty = 2;
				i = nextCommandLine(j);
			} else if (c2 && c3 && isPlainPushSlice(c1) && isSWAP(c2) && is(c3, "STSLICE")) {
				std::string hexSlice = isPlainPushSlice(c1)->blob();
				bitString += StrUtils::toBitString(hexSlice);
				curOpcodeQty = 3;
				i = nextCommandLine(k);
			} else {
				break;
			}
			removedOpcodeQty += curOpcodeQty;
			takenOpcodes.emplace_back(c1);
			if (curOpcodeQty >= 2)
				takenOpcodes.emplace_back(c2);
		}

		std::optional<std::string> slice = StrUtils::unitBitStringToHex(bitString, "");
		if (slice) {
			std::vector<Pointer<TvmAstNode>> opcodes;
			std::optional<Result> res;
			if (StrUtils::toBitString(*slice).length() <= TvmConst::MaxSTSLICECONST) {
				if (withBuilder) {
					opcodes.push_back(gen("NEWC"));
				}
				opcodes.push_back(gen("STSLICECONST " + *slice));
				res = Result{removedOpcodeQty, opcodes};
			} else {
				if (withBuilder) {
					opcodes.push_back(genPushSlice(*slice));
					opcodes.push_back(gen("NEWC"));
					opcodes.push_back(gen("STSLICE"));
				} else {
					opcodes.push_back(genPushSlice(*slice));
					opcodes.push_back(makeXCH_S(1));
					opcodes.push_back(gen("STSLICE"));
				}
				res = Result{removedOpcodeQty, opcodes};
			}

			if (withBuilder && i != -1) {
				if (is(get(i), "ENDC"))
					return Result{removedOpcodeQty + 1, makePUSHREF(*slice)};

				// ...
				// STBREFR
				// =>
				// PUSHREF { xXXX }
				// SWAP
				// STREF
				if (is(get(i), "STBREFR")) {
					return Result{removedOpcodeQty + 1, makePUSHREF(*slice), makeXCH_S(1), gen("STREF")};
				}
			}

			if (removedOpcodeQty > static_cast<int>(res->commands.size())) {
				return res;
			}
			if (removedOpcodeQty == static_cast<int>(res->commands.size())) {
				bool eq = true;
				for (int j = 0; j < static_cast<int>(takenOpcodes.size()); ++j) {
					bool curEq = *takenOpcodes.at(j) == *res->commands.at(j);
					eq &= curEq;
				}
				if (!eq)
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
			std::vector<Pointer<TvmAstNode>> newCmds;
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
				std::ranges::reverse(newCmds);
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

		for (int startStackSize = 0; startStackSize <= StackState::MAX_STACK_DEPTH; ++startStackSize) {
			StackState state{startStackSize};
			int i = idx1;
			int gasCost = 0;
			int opcodeQty = 0;

			while (true) {
				if (i == -1)
					break;
				auto stack = convertToStack(get(i).get());
				if (!stack)
					break;
				if (!state.apply(*stack)) {
					break;
				}

				++opcodeQty;
				gasCost += OpcodeUtils::gasCost(*stack);
				auto newGasCost = StackOpcodeSquasher::
					gasCost(startStackSize, state, m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)));
				if (newGasCost.has_value() && newGasCost < gasCost) {
					bestResult = BestResult{state, startStackSize, opcodeQty};
				}

				i = nextCommandLine(i);
			}
		}

		if (bestResult.has_value()) {
			auto newOpcodes = StackOpcodeSquasher::recover(
				bestResult.value().bestStartStackSize,
				bestResult.value().bestState,
				m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes))
			);
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
			auto stack = convertToStack(op);
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
						std::reverse(opcodes.begin(), opcodes.begin() + j);
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
			std::vector<Pointer<TvmAstNode>> res;
			res.reserve(opcodes.size());
			for (auto const& [opcode, stackSize]: opcodes | std::views::reverse) {
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

	// PUSHCONT { here }
	// CALLX
	// =>
	// here
	if (cmd1Sub && !cmd1Sub->isJmp() && cmd1Sub->block()->type() == CodeBlock::Type::PUSHCONT) {
		if (idx2 == -1) {
			return Result{1, cmd1Sub->block()->instructions()};
		}
	}

	// PUSHCONT {
	//    ...
	//    PUSHCONT { A }
	//    IFJMP
	//    B
	// }
	// CALLX
	// =>
	// ...
	// PUSHCONT { A }
	// PUSHCONT { B }
	// IFELSE
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) &&
		cmd1Sub &&
		!cmd1Sub->isJmp() &&
		cmd1Sub->block()->type() == CodeBlock::Type::PUSHCONT) {
		bool ok = true;
		std::vector<Pointer<TvmAstNode>> newCmds;
		std::vector<Pointer<TvmAstNode>> bCmds;
		TvmIfElse const* gotIfElse = nullptr;
		for (auto const& cmd: cmd1Sub->block()->instructions()) {
			auto ifElse = convertToTvmIfElse(cmd.get());
			if (ifElse && ifElse->withJmp()) {
				if (ifElse->falseBody() != nullptr) {
					ok = false;
					break;
				}
				if (gotIfElse) {
					ok = false;
					break;
				}
				gotIfElse = ifElse;
				continue;
			}

			if (hasRetOrJmp(cmd.get())) {
				ok = false;
				break;
			}

			if (gotIfElse) {
				bCmds.push_back(cmd);
			} else {
				newCmds.push_back(cmd);
			}
		}
		if (ok) {
			auto trueBranch =
				createNode<CodeBlock>(gotIfElse->trueBody()->type(), gotIfElse->trueBody()->instructions());
			auto falseBranch = createNode<CodeBlock>(CodeBlock::Type::PUSHCONT, bCmds);
			if (gotIfElse->withNot()) {
				std::swap(trueBranch, falseBranch);
			}
			newCmds.push_back(createNode<TvmIfElse>(false, false, trueBranch, falseBranch, 0));
			return Result{1, newCmds};
		}
	}

	// PUSHCONT {
	//     A
	// }
	// PUSHCONT {
	//     B
	// }
	// IFELSE [JMP]
	// =>
	// PUSHCONT {
	//     A
	// }
	// IFJMP
	// B
	if (m_flags.test(static_cast<size_t>(OptFlags::UnpackIfElse)) &&
		cmd1IfElse &&
		(cmd1IfElse->withJmp() || idx2 == -1) && // jmp or it's last IFELSE in the block
		cmd1IfElse->falseBody() &&
		cmd1IfElse->falseBody()->type() == CodeBlock::Type::PUSHCONT) {
		std::vector<Pointer<TvmAstNode>> commands;
		commands.emplace_back(
			createNode<TvmIfElse>(cmd1IfElse->withNot(), true, cmd1IfElse->trueBody(), nullptr, cmd1IfElse->ret())
		);
		auto const& instructions = cmd1IfElse->falseBody()->instructions();
		std::ranges::copy(instructions, std::back_inserter(commands));
		return Result{1, commands};
	}

	// BLKDROP2 N, M / DROP
	// GEN ..
	// GEN ..
	// ...
	// BLKDROP2 N, M / DROP
	// =>
	// GEN ..
	// GEN ..
	// ...
	// BLKDROP2 NN, MM
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes))) {
		if (auto BLKDROP2OrDrop = isBLKDROP2OrDrop(cmd1)) {
			auto [drop, rest] = BLKDROP2OrDrop.value();
			int cmdQty = 1;
			int prevIndex = idx1;
			int nextIndex = nextCommandLine(idx1);
			std::vector<Pointer<TvmAstNode>> newCommands;
			while (nextIndex != -1) {
				auto gen = convertToGen(get(nextIndex).get());
				if (gen == nullptr)
					break;
				rest -= gen->take();
				if (rest < 0)
					break;
				rest += gen->ret();
				for (int i = prevIndex + 1; i < nextIndex; ++i) {
					newCommands.emplace_back(get(i));
				}
				newCommands.emplace_back(get(nextIndex));
				++cmdQty;
				prevIndex = nextIndex;
				nextIndex = nextCommandLine(nextIndex);
			}
			if (cmdQty >= 2 && nextIndex != -1) {
				if (auto lastDrop = isBLKDROP2OrDrop(get(nextIndex))) {
					auto [drop2, rest2] = lastDrop.value();
					if (rest2 <= rest && rest <= rest2 + drop2) {
						for (int i = prevIndex + 1; i < nextIndex; ++i) {
							newCommands.emplace_back(get(i));
						}
						newCommands.emplace_back(makeBLKDROP2(drop + drop2, rest2));
						++cmdQty;
						return Result{cmdQty, newCommands};
					}
				}
			}
		}
	}

	return {};
}

bool PrivatePeepholeOptimizer::hasRetOrJmp(TvmAstNode const* _node) {
	if (auto opaque = convertToOpaque(_node)) {
		return hasRetOrJmp(opaque->block().get());
	}
	if (auto cb = convertToCodeBlock(_node)) {
		for (Pointer<TvmAstNode> const& i: cb->instructions()) {
			if (hasRetOrJmp(i.get())) {
				return true;
			}
		}
	}
	if (ReturnOrBreakOrCont const* ret = convertToReturnOrBreakOrCont(_node)) {
		return hasRetOrJmp(ret->body().get());
	}
	if (convertToTvmReturn(_node)) {
		return true;
	}
	if (auto sub = convertToSubProgram(_node)) {
		return sub->isJmp();
	}
	if (auto isElse = convertToTvmIfElse(_node)) {
		return isElse->withJmp();
	}
	return false;
}

std::optional<Result> PrivatePeepholeOptimizer::unsquash(bool _withUnpackOpaque, int const idx1) const {
	auto c = get(idx1);
	if (_withUnpackOpaque) {
		if (auto ret = convertToReturnOrBreakOrCont(c.get())) {
			return Result{1, ret->body()->instructions()};
		}
		if (auto op = convertToOpaque(c.get())) {
			return Result{1, op->block()->instructions()};
		}
	}
	return {};
}

std::optional<Result> PrivatePeepholeOptimizer::squash(int const idx1) const {
	int idx2 = nextCommandLine(idx1);
	int idx3 = nextCommandLine(idx2);
	Pointer<TvmAstNode> const& cmd1 = get(idx1);
	Pointer<TvmAstNode> const& cmd2 = get(idx2);
	Pointer<TvmAstNode> const& cmd3 = get(idx3);
	auto cmd1Gen = convertToGen(cmd1.get());
	auto cmd1PushCellOrSlice = convertToPushCellOrSlice(cmd1.get());

	// PUSH Si
	// PUSH Si
	// =>
	// BLKPUSH N, i
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

	// PUSH Si
	// PUSH Sj
	// PUSH Sk
	// =>
	// PUSH3 Sa, Sb, Sc
	if (isPUSH(cmd1) && isPUSH(cmd2) && isPUSH(cmd3)) {
		int const si = *isPUSH(cmd1);
		int const sj = *isPUSH(cmd2) - 1 == -1 ? si : *isPUSH(cmd2) - 1;
		int const sk = *isPUSH(cmd3) - 2 == -1 ? si : (*isPUSH(cmd3) - 2 == -2 ? sj : *isPUSH(cmd3) - 2);
		if (si <= 15 && sj <= 15 && sk <= 15) {
			return Result{3, makePUSH3(si, sj, sk)};
		}
	}

	// PUSH Si
	// PUSH Sj
	// =>
	// PUSH2 Sa, Sb
	if (isPUSH(cmd1) && isPUSH(cmd2)) {
		int const si = *isPUSH(cmd1);
		int const sj = *isPUSH(cmd2) - 1 == -1 ? si : *isPUSH(cmd2) - 1;
		if (si <= 15 && sj <= 15) {
			return Result{2, makePUSH2(si, sj)};
		}
	}

	// PUSH Si
	// PUSH Sj
	// =>
	// PUSH2 Sa, Sb
	if (isPUSH(cmd1) && isPUSH(cmd2)) {
		int const si = *isPUSH(cmd1);
		int const sj = *isPUSH(cmd2) - 1 == -1 ? si : *isPUSH(cmd2) - 1;
		if (si <= 15 && sj <= 15) {
			return Result{2, makePUSH2(si, sj)};
		}
	}

	// PUSH2 Si, Sj
	// PUSH Sk
	// =>
	// PUSH3 Si, Sj, Sm
	if (isPUSH2(cmd1) && isPUSH(cmd2)) {
		auto const [si, sj] = *isPUSH2(cmd1);
		int const sk = *isPUSH(cmd2);
		int sm;
		if (sk == 0) {
			sm = sj;
		} else if (sk == 1) {
			sm = si;
		} else {
			sm = sk - 2;
		}
		if (sm <= 15) {
			return Result{2, makePUSH3(si, sj, sm)};
		}
	}

	// TODO delete?
	// squash PUSHINT, NULL, NILL, FALSE, TRUE, (PUSHINT 0 NILL PAIR) etc
	if (isPureGen01(*cmd1)) {
		int i = idx1;
		int n = 0;
		while (true) {
			auto cmdI = convertToGen(get(i).get());
			if (cmdI && *cmd1Gen == *cmdI) {
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
			auto cmdI = convertToPushCellOrSlice(get(i).get());
			if (cmdI && cmd1PushCellOrSlice->operator==(*cmdI)) {
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

	// PUSH Si
	// SWAP
	// =>
	// PUXC Si, S-1
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) &&
		isPUSH(cmd1) &&
		isXCHG_S0(cmd2) &&
		isXCHG_S0(cmd2).value() == 1) {
		int i = *isPUSH(cmd1);
		if (0 <= i && i <= 15)
			return Result{2, makePUXC(i, -1)};
	}

	// XCHG Si
	// PUSH Sj
	// =>
	// XCPU Si, Sj
	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)) && isXCHG_S0(cmd1) && cmd2 && isPUSH(cmd2)) {
		int i = isXCHG_S0(cmd1).value();
		int j = isPUSH(cmd2).value();
		if (0 <= i && i <= 15 && 0 <= j && j <= 15) {
			return Result{2, makeXCPU(i, j)};
		}
	}

	return {};
}

void PrivatePeepholeOptimizer::updateLinesAndIndex(int idx1, Result const& res) {
	if (res.removeQty > 0) {
		solAssert(valid(idx1), "");
		solAssert(!convertToLoc(m_instructions.at(idx1).get()), "");
		int lastInx = idx1;
		for (int iter = 0; iter + 1 < res.removeQty; ++iter) {
			lastInx = nextCommandLine(lastInx);
			solAssert(valid(lastInx), "");
			solAssert(!convertToLoc(m_instructions.at(lastInx).get()), "");
		}

		Pointer<TvmAstNode> locLine;
		for (int i = idx1; i <= lastInx; ++i) {
			if (convertToLoc(m_instructions.at(i).get())) {
				locLine = m_instructions.at(i);
			}
		}

		// [0 .. idx1-1] [idx1 .. lastInx] [lastInx + 1 .. ]
		int removeOpcodeQty = lastInx - idx1 + 1;
		int newOpcodeQty = res.commands.size() + (locLine == nullptr ? 0 : 1);
		int tailIndex = lastInx + 1;
		if (newOpcodeQty > removeOpcodeQty) {
			int delta = newOpcodeQty - removeOpcodeQty;
			m_instructions.insert(m_instructions.begin() + idx1, delta, nullptr);
			tailIndex += delta;
		}
		int index = idx1;
		for (Pointer<TvmAstNode> const& inst: res.commands) {
			m_instructions[index++] = inst;
		}
		// insert .loc if it presents
		if (locLine != nullptr) {
			m_instructions[index++] = locLine;
		}
		solAssert(index <= tailIndex, "");
		if (index != tailIndex) {
			for (std::size_t j = tailIndex; j < m_instructions.size(); ++j) {
				m_instructions[index++] = m_instructions[j];
			}
			m_instructions.resize(index);
		}
	}
}

bool PrivatePeepholeOptimizer::optimize(std::function<std::optional<Result>(int)> const& f) {
	int idx1 = 0;
	while (idx1 < static_cast<int>(m_instructions.size()) && convertToLoc(m_instructions.at(idx1).get())) {
		++idx1;
	}

	bool didSomething = false;
	while (valid(idx1)) {
		solAssert(!convertToLoc(m_instructions.at(idx1).get()), "");
		std::optional<Result> res = f(idx1);
		if (res) {
			// {
			// 	std::cout << ">>>A\n";
			// 	Printer p{std::cout};
			// 	for (auto const& x: m_instructions)
			// 		x->accept(p);
			// 	std::cout << std::endl;
			// }
			didSomething = true;
			updateLinesAndIndex(idx1, res.value());
			// step back to several commands
			idx1 = std::min<int>(idx1, m_instructions.size() - 1);
			idx1 = std::max(idx1, 0);
			int cnt = 10;
			while (cnt > 0 && idx1 > 0) {
				--idx1;
				if (!convertToLoc(m_instructions.at(idx1).get()))
					--cnt;
			}
			while (idx1 < static_cast<int>(m_instructions.size()) && convertToLoc(m_instructions.at(idx1).get())) {
				++idx1;
			}

			// std::cout << "<<<B\n";
			// Printer p{std::cout};
			// for (auto const& x: m_instructions)
			// 	x->accept(p);
			// std::cout << std::endl;
		} else {
			idx1 = nextCommandLine(idx1);
		}
	}
	return didSomething;
}

bigint PrivatePeepholeOptimizer::pushintValue(Pointer<TvmAstNode> const& node) {
	solAssert(is(node, "PUSHINT"), "");
	auto g = convertToStackGen(node.get());
	return bigint{g->arg()};
}

int PrivatePeepholeOptimizer::fetchInt(Pointer<TvmAstNode> const& node) {
	auto g = convertToStackGen(node.get());
	return strToInt(g->arg());
}

bool PrivatePeepholeOptimizer::isNIP(Pointer<TvmAstNode> const& node) {
	return (isPOP(node) && isPOP(node).value() == 1) ||
		   (isBLKDROP2(node) && isBLKDROP2(node).value() == std::make_pair(1, 1));
}

std::pair<int, int> PrivatePeepholeOptimizer::getIndexes(std::string const& str) {
	size_t pos = str.find(',');
	solAssert(pos < str.size(), "");
	return {strToInt(str.substr(0, pos)), strToInt(str.substr(pos + 2))};
}

template <class... Args>
bool PrivatePeepholeOptimizer::isExc(Pointer<TvmAstNode> const& node, Args&&... cmd) {
	auto cfi = convertToTvmException(node.get());
	return cfi && isIn(cfi->opcode(), std::forward<Args>(cmd)...);
}

bool PrivatePeepholeOptimizer::isConstAdd(Pointer<TvmAstNode> const& node) {
	auto gen = convertToStackGen(node.get());
	return gen && isIn(gen->opcode(), "INC", "DEC", "ADDCONST");
}

int PrivatePeepholeOptimizer::getAddNum(Pointer<TvmAstNode> const& node) {
	solAssert(isConstAdd(node), "");
	auto gen = convertToStackGen(node.get());
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

bool PrivatePeepholeOptimizer::isSimpleCommand(Pointer<TvmAstNode> const& node) {
	// See also isPureGen01
	auto gen = convertToGen(node.get());
	return gen &&
		   (convertToStackGen(gen) ||
			convertToPushCellOrSlice(gen) ||
			convertToGlob(gen) ||
			convertToOpaque(gen) ||
			convertToHardCode(gen)) &&
		   gen->take() == 0 &&
		   gen->ret() == 1;
}

bool PrivatePeepholeOptimizer::isAddOrSub(Pointer<TvmAstNode> const& node) {
	return is(node, "ADD") || is(node, "SUB");
}

bool PrivatePeepholeOptimizer::isCommutative(Pointer<TvmAstNode> const& node) {
	auto g = convertToStackGen(node.get());
	return g && isIn(g->fullOpcode(), "ADD", "AND", "EQUAL", "MAX", "MIN", "MUL", "NEQ", "OR", "SDEQ", "XOR");
}

bool PeepholeOptimizer::visit(CodeBlock& _node) {
	optimizeBlock(_node);
	return true;
}

bool PeepholeOptimizer::visit(Function& /*_node*/) {
	// return _node.name() == "f3_aaf05f3d_internal"; // for debug
	return true;
}

void PeepholeOptimizer::endVisit(CodeBlock& _node) { optimizeBlock(_node); }

void PeepholeOptimizer::optimizeBlock(CodeBlock& _node) const {
	{
		std::optional<Result> r = PrivatePeepholeOptimizer{{}, m_flags}.optimizeAt1(_node.shared_from_this());
		if (r && r.value().commands.size() == 1) {
			auto newBlock = convertToCodeBlock(r.value().commands.at(0).get());
			_node.changeInstructions(newBlock->instructions());
			_node.changeType(newBlock->type());
		}
	}

	std::vector<Pointer<TvmAstNode>> instructions = _node.instructions();

	PrivatePeepholeOptimizer optimizer{instructions, m_flags};
	optimizer.optimize([&](int index) {
		return optimizer.unsquash(m_flags.test(static_cast<size_t>(OptFlags::UnpackOpaque)), index);
	});
	if (m_flags.test(static_cast<size_t>(OptFlags::UseR)))
		while (optimizer.optimize([&optimizer](int index) { return optimizer.useR(index); })) {
		}

	while (optimizer.optimize([&optimizer](int index) { return optimizer.optimizeAt(index); })) {
	}

	if (m_flags.test(static_cast<size_t>(OptFlags::OptimizeSlice)))
		while (optimizer.optimize([&optimizer](int index) { return optimizer.optimizeSlice(index); })) {
		}

	if (m_flags.test(static_cast<size_t>(OptFlags::UseCompoundOpcodes)))
		optimizer.optimize([&optimizer](int index) { return optimizer.squash(index); });

	_node.changeInstructions(optimizer.instructions());
}

} // end solidity::frontend
