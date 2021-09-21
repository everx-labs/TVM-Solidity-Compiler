/*
 * Copyright 2018-2019 TON DEV SOLUTIONS LTD.
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
 * TVM Solidity abstract syntax tree.
 */

#include <string>
#include <unordered_map>

#include <boost/algorithm/string/trim.hpp>

#include "TvmAst.hpp"
#include "TvmAstVisitor.hpp"
#include "TVMCommons.hpp"
#include <liblangutil/Exceptions.h>

using namespace solidity::frontend;

void Loc::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

Stack::Stack(Stack::Opcode opcode, int i, int j, int k) : m_opcode{opcode}, m_i{i}, m_j{j}, m_k{k}
{
}

void Stack::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

Glob::Glob(Glob::Opcode opcode, int index) :
	Gen{isIn(opcode, Glob::Opcode::GetOrGetVar, Glob::Opcode::PUSHROOT, Glob::Opcode::PUSH_C3)},
	m_opcode{opcode},
	m_index{index}
{
}

void Glob::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

int Glob::take() const {
	switch (m_opcode) {
		case Opcode::GetOrGetVar:
		case Opcode::PUSHROOT:
		case Opcode::PUSH_C3:
		case Opcode::PUSH_C7:
			return 0;

		case Opcode::SetOrSetVar:
		case Opcode::POPROOT:
		case Opcode::POP_C3:
		case Opcode::POP_C7:
			return 1;
	}
	solUnimplemented("");
}

int Glob::ret() const {
	switch (m_opcode) {
		case Opcode::GetOrGetVar:
		case Opcode::PUSHROOT:
		case Opcode::PUSH_C3:
		case Opcode::PUSH_C7:
			return 1;

		case Opcode::SetOrSetVar:
		case Opcode::POPROOT:
		case Opcode::POP_C3:
		case Opcode::POP_C7:
			return 0;
	}
	solUnimplemented("");
}

void DeclRetFlag::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

void Opaque::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_block->accept(_visitor);
	}
}

void AsymGen::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

void HardCode::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

GenOpcode::GenOpcode(std::string opcode, int take, int ret, bool _isPure) : Gen{_isPure},  m_take{take}, m_ret{ret} {
	vector<string> lines = split(opcode, ';');
	solAssert(lines.size() <= 2, "");

	auto pos = lines.at(0).find(' ');
	m_opcode = boost::algorithm::trim_copy(lines.at(0).substr(0, pos));
	if (pos != std::string::npos)
		m_arg = boost::algorithm::trim_copy(lines.at(0).substr(pos + 1));

	if (lines.size() == 2) {
		m_comment = ";" + lines.at(1);
	}
}


void GenOpcode::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

std::string GenOpcode::fullOpcode() const {
	std::string ret = m_opcode;
	if (!m_arg.empty())
		ret += " " + m_arg;
	if (!m_comment.empty())
		ret += " " + m_comment;
	return ret;
}

void TvmReturn::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

void ReturnOrBreakOrCont::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
	}
}

void TvmException::accept(TvmAstVisitor& _visitor) {
	_visitor.visit(*this);
}

void PushCellOrSlice::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		if (m_child) {
			m_child->accept(_visitor);
		}
	}
}

bool PushCellOrSlice::equal(PushCellOrSlice const& another) const {
	PushCellOrSlice const* a = this;
	PushCellOrSlice const* b = &another;
	while (true) {
		if (a->m_blob != b->m_blob || a->m_type != b->m_type) {
			return false;
		}
		if (!a->m_child && !b->m_child) {
			return true;
		}
		if (!a->m_child || !b->m_child) {
			return false;
		}
		a = a->m_child.get();
		b = b->m_child.get();
	}
	solUnimplemented("");
}

std::string CodeBlock::toString(CodeBlock::Type t) {
	switch (t) {
		case CodeBlock::Type::PUSHCONT:
			return "PUSHCONT";
		case CodeBlock::Type::PUSHREFCONT:
			return "PUSHREFCONT";
		default:
			solUnimplemented("");
	}
	solUnimplemented("");
}

void CodeBlock::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		for (Pointer<TvmAstNode> node : m_instructions) {
			node->accept(_visitor);
		}
	}
	_visitor.endVisit(*this);
}

void SubProgram::accept(TvmAstVisitor &_visitor) {
	if (_visitor.visit(*this))
	{
		m_block->accept(_visitor);
	}
}

void TvmCondition::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		mTrueBody->accept(_visitor);
		mFalseBody->accept(_visitor);
	}
}

void LogCircuit::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
	}
}

void TvmIfElse::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_trueBody->accept(_visitor);
		if (m_falseBody) {
			m_falseBody->accept(_visitor);
		}
	}
}

void TvmRepeat::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
	}
}

void TvmUntil::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_body->accept(_visitor);
	}
}

void While::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_condition->accept(_visitor);
		body()->accept(_visitor);
	}
}

void Function::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		m_block->accept(_visitor);
	}
}

void Contract::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this))
	{
		for (Pointer<Function>& node : m_functions) {
			node->accept(_visitor);
		}
	}
}

std::vector<Pointer<Function>>& Contract::functions() {
	return m_functions;
}

namespace solidity::frontend {
Pointer<GenOpcode> gen(const std::string& cmd) {
	std::string op;
	std::string param;
	{
		std::istringstream iss(cmd);
		iss >> op >> param;
	}

	auto f = [&](const std::string& pattert) {
		return op == pattert;
	};

	auto dictReplaceOrAdd = [&]() {
		for (std::string key : {"", "I", "U"}) {
			for (std::string op : {"REPLACE", "ADD"}) {
				for (std::string suf : {"", "REF", "B"}) {
					std::string candidat = "DICT" + key + op + suf;
					if (candidat == cmd) {
						return true;
					}
				}
			}
		}
		return false;
	};

	auto dictSet = [&]() {
		for (std::string key : {"", "I", "U"}) {
			for (std::string suf : {"", "REF", "B"}) {
				std::string candidat = "DICT" + key + "SET" + suf;
				if (candidat == cmd) {
					return true;
				}
			}
		}
		return false;
	};

	struct OpcodeParams {
		int take{};
		int ret{};
		bool isPure{};

		OpcodeParams(int _take, int _ret, bool _isPure = false) :
			take{_take},
			ret{_ret},
			isPure{_isPure}
		{
		}
	};

	const static std::unordered_map<std::string, OpcodeParams> opcodes = {
		{"ACCEPT", {0, 0}},
		{"COMMIT", {0, 0}},
		{"PRINTSTR", {0, 0}},

		{"BLOCKLT", {0, 1, true}},
		{"FALSE", {0, 1, true}},
		{"GETPARAM", {0, 1, true}},
		{"LTIME", {0, 1, true}},
		{"MYADDR", {0, 1, true}},
		{"NEWC", {0, 1, true}},
		{"NEWDICT", {0, 1, true}},
		{"NIL", {0, 1, true}},
		{"NOW", {0, 1, true}},
		{"NULL", {0, 1, true}},
		{"PUSHINT", {0, 1, true}},
		{"PUSHPOW2DEC", {0, 1, true}},
		{"PUSHSLICE", {0, 1, true}},
		{"RANDSEED", {0, 1, true}},
		{"RANDU256", {0, 1}},
		{"TRUE", {0, 1, true}},

		{"ADDRAND", {1, 0}},
		{"ENDS", {1, 0}},
		{"SETCODE", {1, 0}},
		{"SETRAND", {1, 0}},

		{"ABS", {1, 1}},
		{"ADDCONST", {1, 1}},
		{"BBITS", {1, 1, true}},
		{"BDEPTH", {1, 1}},
		{"BINDUMP", {1, 1}},
		{"BITNOT", {1, 1}}, // pseudo opcode. Alias for NOT
		{"BITSIZE", {1, 1, true}},
		{"BLESS", {1, 1}},
		{"BREFS", {1, 1, true}},
		{"BREMBITS", {1, 1, true}},
		{"BREMREFS", {1, 1, true}},
		{"CDEPTH", {1, 1}},
		{"CTOS", {1, 1}},
		{"DEC", {1, 1}},
		{"DICTEMPTY", {1, 1, true}},
		{"ENDC", {1, 1}},
		{"EQINT", {1, 1, true}},
		{"FIRST", {1, 1}}, // TODO delete
		{"FITS", {1, 1}},
		{"GTINT", {1, 1, true}},
		{"HASHCU", {1, 1, true}},
		{"HASHSU", {1, 1, true}},
		{"HEXDUMP", {1, 1}},
		{"INC", {1, 1}},
		{"INDEX_EXCEP", {1, 1}},
		{"INDEX_NOEXCEP", {1, 1, true}},
		{"INDEX2", {1, 1}},
		{"INDEX3", {1, 1}},
		{"ISNEG", {1, 1, true}},
		{"ISNNEG", {1, 1, true}},
		{"ISNPOS", {1, 1, true}},
		{"ISNULL", {1, 1, true}},
		{"ISPOS", {1, 1, true}},
		{"ISZERO", {1, 1, true}},
		{"LAST", {1, 1}},
		{"LESSINT", {1, 1, true}},
		{"MODPOW2", {1, 1}},
		{"MULCONST", {1, 1}},
		{"NEGATE", {1, 1}},
		{"NEQINT", {1, 1, true}},
		{"NOT", {1, 1, true}}, // logical not
		{"PARSEMSGADDR", {1, 1}},
		{"PLDDICT", {1, 1}},
		{"PLDI", {1, 1}},
		{"PLDREF", {1, 1}},
		{"PLDREFIDX", {1, 1}},
		{"PLDU", {1, 1}},
		{"RAND", {1, 1}},
		{"SBITS", {1, 1, true}},
		{"SDEMPTY", {1, 1, true}},
		{"SDEPTH", {1, 1}},
		{"SECOND", {1, 1}}, // TODO delete
		{"SEMPTY", {1, 1, true}},
		{"SGN", {1, 1, true}},
		{"SHA256U", {1, 1, true}},
		{"SREFS", {1, 1, true}},
		{"STONE", {1, 1}},
		{"STRDUMP", {1, 1}},
		{"STSLICECONST", {1, 1}},
		{"STZERO", {1, 1}},
		{"THIRD", {1, 1}}, // TODO delete
		{"TLEN", {1, 1}},
		{"UBITSIZE", {1, 1}},
		{"UFITS", {1, 1}},

		{"BBITREFS", {1, 2, true}},
		{"BREMBITREFS", {1, 2, true}},
		{"LDDICT", {1, 2}},
		{"LDDICT", {1, 2}},
		{"LDGRAMS", {1, 2}},
		{"LDI", {1, 2}},
		{"LDMSGADDR", {1, 2}},
		{"LDOPTREF", {1, 2}},
		{"LDREF", {1, 2}},
		{"LDREFRTOS", {1, 2}},
		{"LDSLICE", {1, 2}},
		{"LDU", {1, 2}},
		{"LDVARUINT32", {1, 2}},
		{"REWRITESTDADDR", {1, 2}},
		{"SBITREFS", {1, 2, true}},
		{"TPOP", {1, 2}},
		{"UNPAIR", {1, 2}},

		{"RAWRESERVE", {2, 0}},
		{"SENDRAWMSG", {2, 0}},

		{"ADD", {2, 1}},
		{"AND", {2, 1, true}},
		{"CMP", {2, 1, true}},
		{"DIV", {2, 1}},
		{"DIVC", {2, 1}},
		{"DIVR", {2, 1}},
		{"EQUAL", {2, 1, true}},
		{"GEQ", {2, 1, true}},
		{"GREATER", {2, 1, true}},
		{"INDEXVAR", {2, 1}}, // only for vector
		{"LEQ", {2, 1, true}},
		{"LESS", {2, 1, true}},
		{"MAX", {2, 1, true}},
		{"MIN", {2, 1, true}},
		{"MOD", {2, 1}},
		{"MUL", {2, 1}},
		{"NEQ", {2, 1, true}},
		{"OR", {2, 1, true}},
		{"PAIR", {2, 1, true}},
		{"SCHKBITSQ", {2, 1, true}},
		{"SCHKREFSQ", {2, 1, true}},
		{"SDEQ", {2, 1, true}},
		{"SDLEXCMP", {2, 1}},
		{"SDSKIPFIRST", {2, 1}},
		{"SETINDEX", {2, 1}},
		{"SETINDEXQ", {2, 1}},
		{"STB", {2, 1}},
		{"STBR", {2, 1}},
		{"STBREF", {2, 1}},
		{"STBREFR", {2, 1}},
		{"STDICT", {2, 1}},
		{"STGRAMS", {2, 1}},
		{"STI", {2, 1}},
		{"STIR", {2, 1}},
		{"STONES", {2, 1}},
		{"STOPTREF", {2, 1}},
		{"STREF", {2, 1}},
		{"STREFR", {2, 1}},
		{"STSLICE", {2, 1}},
		{"STSLICER", {2, 1}},
		{"STU", {2, 1}},
		{"STUR", {2, 1}},
		{"STVARUINT32", {2, 1}},
		{"STZEROES", {2, 1}},
		{"SUB", {2, 1}},
		{"SUBR", {2, 1}},
		{"TPUSH", {2, 1}},
		{"XOR", {2, 1, true}},

		{"DIVMOD", {2, 2}},
		{"LDIX", {2, 2}},
		{"LDSLICEX", {2, 2}},
		{"LDUX", {2, 2}},
		{"MINMAX", {2, 2, true}},

		{"CDATASIZE", {2, 3}},
		{"SDATASIZE", {2, 3}},

		{"RAWRESERVEX", {3, 0}},

		{"CHKSIGNS", {3, 1}},
		{"CHKSIGNU", {3, 1}},
		{"MULDIV", {3, 1}},
		{"MULDIVC", {3, 1}},
		{"MULDIVR", {3, 1}},
		{"SCHKBITREFSQ", {3, 1}},
		{"SETINDEXVAR", {3, 1}},
		{"SETINDEXVARQ", {3, 1}},
		{"SSKIPFIRST", {3, 1}},
		{"STUX", {3, 1}},
		{"TRIPLE", {3, 1}},

		{"DICTDEL", {3, 2}},
		{"DICTIDEL", {3, 2}},
		{"DICTUDEL", {3, 2}},
		{"MULDIVMOD", {3, 2}},
		{"SPLIT", {3, 2}}
	};

	Pointer<GenOpcode> opcode;
	if (opcodes.count(op)) {
		OpcodeParams params = opcodes.at(op);
		opcode = createNode<GenOpcode>(cmd, params.take, params.ret, params.isPure);
	} else if (dictSet()) {
		opcode = createNode<GenOpcode>(cmd, 4, 1);
	} else if (dictReplaceOrAdd()) {
		opcode = createNode<GenOpcode>(cmd, 4, 2);
	} else if (f("TUPLE")) {
		int ret = boost::lexical_cast<int>(param);
		opcode = createNode<GenOpcode>(cmd, ret, 1);
	} else if (f("UNTUPLE")) {
		int ret = boost::lexical_cast<int>(param);
		opcode = createNode<GenOpcode>(cmd, 1, ret);
	} else if (f("LSHIFT") || f("RSHIFT")) {
		if (param.empty()) {
			opcode = createNode<GenOpcode>(cmd, 2, 1);
		} else {
			opcode = createNode<GenOpcode>(cmd, 1, 1);
		}
	} else if (f("MULRSHIFT")) {
		if (param.empty()) {
			opcode = createNode<GenOpcode>(cmd, 3, 1);
		} else {
			opcode = createNode<GenOpcode>(cmd, 2, 1);
		}
	} else {
		solUnimplemented("StackPusher::push: " + cmd);
	}
	solAssert(opcode != nullptr, "");
	return opcode;
}

Pointer<Stack> makeDROP(int cnt) {
	solAssert(cnt >= 1, "");
	return createNode<Stack>(Stack::Opcode::DROP, cnt);
}

Pointer<Stack> makePOP(int i) {
	solAssert(i >= 1 && i <= 255, "");
	return createNode<Stack>(Stack::Opcode::POP_S, i);
}

Pointer<Stack> makeBLKPUSH(int qty, int index) {
	solAssert(qty >= 1, "");
	solAssert(index >= 0 && index <= 15, "");
	if (qty == 1) return makePUSH(index);
	return createNode<Stack>(Stack::Opcode::BLKPUSH, qty, index);
}

Pointer<Stack> makePUSH(int i) {
	solAssert(0 <= i && i <= 255, "");
	return createNode<Stack>(Stack::Opcode::PUSH_S, i);
}

Pointer<Stack> makePUSH2(int i, int j) {
	solAssert(0 <= i && i <= 15, "");
	solAssert(0 <= j && j <= 15, "");
	return createNode<Stack>(Stack::Opcode::PUSH2_S, i, j);
}

Pointer<Stack> makePUSH3(int i, int j, int k) {
	solAssert(0 <= i && i <= 15, "");
	solAssert(0 <= j && j <= 15, "");
	solAssert(0 <= k && k <= 15, "");
	return createNode<Stack>(Stack::Opcode::PUSH3_S, i, j, k);
}

Pointer<TvmReturn> makeRET() {
	return createNode<TvmReturn>(TvmReturn::Type::RET);
}

Pointer<TvmReturn> makeIFRET() {
	return createNode<TvmReturn>(TvmReturn::Type::IFRET);
}

Pointer<TvmReturn> makeIFNOTRET() {
	return createNode<TvmReturn>(TvmReturn::Type::IFNOTRET);
}

Pointer<TvmException> makeTHROW(const std::string& cmd) {
	std::string op;
	std::string param;
	{
		std::istringstream iss(cmd);
		iss >> op >> param;
	}

	auto f = [&](const std::string& pattert) {
		return op == pattert;
	};

	Pointer<TvmException> opcode;
	if (f("THROW")) { opcode = createNode<TvmException>(cmd, 0, 0); }
	else if (f("THROWANY")) { opcode = createNode<TvmException>(cmd, 1, 0); } // ??? it returns 2 params
	else if (f("THROWANYIF")) { opcode = createNode<TvmException>(cmd, 2, 0); }
	else if (f("THROWANYIFNOT")) { opcode = createNode<TvmException>(cmd, 2, 0); }
	else if (f("THROWARG")) { opcode = createNode<TvmException>(cmd, 1, 0); } // ??? it returns 2 params
	else if (f("THROWARGANY")) { opcode = createNode<TvmException>(cmd, 2, 0); }
	else if (f("THROWARGANYIF")) { opcode = createNode<TvmException>(cmd, 3, 0); }
	else if (f("THROWARGANYIFNOT")) { opcode = createNode<TvmException>(cmd, 3, 0); }
	else if (f("THROWARGIF")) { opcode = createNode<TvmException>(cmd, 2, 0); }
	else if (f("THROWARGIFNOT")) { opcode = createNode<TvmException>(cmd, 2, 0); }
	else if (f("THROWIF")) { opcode = createNode<TvmException>(cmd, 1, 0); }
	else if (f("THROWIFNOT")) { opcode = createNode<TvmException>(cmd, 1, 0); }
	else  {
		solUnimplemented("");
	}
	return opcode;
}

Pointer<Stack> makeXCH_S(int i) {
	return makeXCH_S_S(0, i);
}

Pointer<Stack> makeXCH_S_S(int i, int j) {
	solAssert(i <= j, "");
	return createNode<Stack>(Stack::Opcode::XCHG, i, j);
}

Pointer<Glob> makeSetGlob(int i) {
	return createNode<Glob>(Glob::Opcode::SetOrSetVar, i);
}

Pointer<Stack> makeBLKDROP2(int droppedCount, int leftCount) {
	solAssert(1 <= droppedCount, "");
	solAssert(0 <= leftCount, "");
	if (leftCount == 1 && droppedCount == 1) {
		return createNode<Stack>(Stack::Opcode::POP_S, 1);
	}
	if (leftCount == 0) {
		return makeDROP(droppedCount);
	}
	return createNode<Stack>(Stack::Opcode::BLKDROP2, droppedCount, leftCount);
}

Pointer<PushCellOrSlice> makePUSHREF(std::string const& data) {
	return createNode<PushCellOrSlice>(PushCellOrSlice::Type::PUSHREF, data, nullptr);
}

Pointer<Stack> makeREVERSE(int i, int j) {
	solAssert(i >= 2, "");
	solAssert(j >= 0, "");
	return createNode<Stack>(Stack::Opcode::REVERSE, i, j);
}

Pointer<Stack> makeROT() {
	return createNode<Stack>(Stack::Opcode::BLKSWAP, 1, 2);
}

Pointer<Stack> makeROTREV() {
	return createNode<Stack>(Stack::Opcode::BLKSWAP, 2, 1);
}

Pointer<Stack> makeBLKSWAP(int down, int top) {
	return createNode<Stack>(Stack::Opcode::BLKSWAP, down, top);
}

Pointer<Stack> makeTUCK() {
	return createNode<Stack>(Stack::Opcode::TUCK);
}

Pointer<Stack> makePUXC(int i, int j) {
	return createNode<Stack>(Stack::Opcode::PUXC, i, j);
}

Pointer<TvmIfElse> makeRevert(TvmIfElse const& node) {
	switch (node.type()) {
		case TvmIfElse::Type::IF:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFNOT, node.trueBody());
		case TvmIfElse::Type::IFNOT:
			return createNode<TvmIfElse>(TvmIfElse::Type::IF, node.trueBody());
		case TvmIfElse::Type::IFJMP:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFNOTJMP, node.trueBody());
		case TvmIfElse::Type::IFNOTJMP:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFJMP, node.trueBody());
		case TvmIfElse::Type::IFELSE:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFELSE, node.falseBody(), node.trueBody());
		case TvmIfElse::Type::IFELSE_WITH_JMP:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFELSE_WITH_JMP, node.falseBody(), node.trueBody());
		case TvmIfElse::Type::IFREF:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFNOTREF, node.falseBody(), node.trueBody());
		case TvmIfElse::Type::IFNOTREF:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFREF, node.falseBody(), node.trueBody());
		case TvmIfElse::Type::IFJMPREF:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFNOTJMPREF, node.falseBody(), node.trueBody());
		case TvmIfElse::Type::IFNOTJMPREF:
			return createNode<TvmIfElse>(TvmIfElse::Type::IFJMPREF, node.falseBody(), node.trueBody());
	}
	solUnimplemented("");
}

Pointer<TvmCondition> makeRevertCond(TvmCondition const& node) {
	return createNode<TvmCondition>(node.falseBody(), node.trueBody(), node.ret());
}

bool isPureGen01OrGetGlob(TvmAstNode const& node) {
	if (auto gen = to<Gen>(&node)) {
		return gen->isPure() && std::make_pair(gen->take(), gen->ret()) == std::make_pair(0, 1);
	}
	auto gl = to<Glob>(&node);
	if (gl && gl->opcode() == Glob::Opcode::GetOrGetVar) {
		return true;
	}
	return false;
}

bool isSWAP(Pointer<TvmAstNode> const& node) {
	return isBLKSWAP(node) && isBLKSWAP(node).value() == std::make_pair(1, 1);
}

// down, top
std::optional<std::pair<int, int>> isBLKSWAP(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (stack) {
		int i = stack->i();
		int j = stack->j();
		switch (stack->opcode()) {
			case Stack::Opcode::BLKSWAP:
				return {{i, j}};
				case Stack::Opcode::XCHG: {
					if (i == 0 && j == 1)
						return {{1, 1}};
					break;
				}
				case Stack::Opcode::REVERSE: {
					if (i == 2 && j == 0)
						return {{1, 1}};
					break;
				}
				default:
					break;
		}
	}
	return {};
}

std::optional<int> isDrop(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (!stack)
		return {};
	switch (stack->opcode()) {
		case Stack::Opcode::DROP:
			return stack->i();
		default:
			return {};
	}
	solUnimplemented("");
}

std::optional<int> isPOP(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (stack) {
		switch (stack->opcode()) {
		case Stack::Opcode::POP_S:
			return {{stack->i()}};
		case Stack::Opcode::BLKDROP2:
			if (stack->i() == 1 && stack->j() == 1)
				return {{1}};
			break;
		default:
			break;
		}
	}
	return {};
}

bool isXCHG(Pointer<TvmAstNode> const& node, int i, int j) {
	auto cmd2Stack = to<Stack>(node.get());
	return cmd2Stack && cmd2Stack->opcode() == Stack::Opcode::XCHG &&
			cmd2Stack->i() == i &&
			cmd2Stack->j() == j;
}

std::optional<int> isXCHG_S0(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (stack) {
		int i = stack->i();
		int j = stack->j();
		switch (stack->opcode()) {
			case Stack::Opcode::XCHG:
				if (i == 0)
					return {j};
				break;
			case Stack::Opcode::BLKSWAP:
				if (i == 1 && j == 1)
					return {1};
				break;
			case Stack::Opcode::REVERSE:
				if (i == 2 && j == 0)
					return {1};
				if (i == 3 && j == 0)
					return {2};
				break;
			default:
				break;
		}
	}
	return {};
}

// qty, index
std::optional<std::pair<int, int>> isREVERSE(Pointer<TvmAstNode> const& node) {
	auto stack = to<Stack>(node.get());
	if (stack) {
		int i = stack->i();
		int j = stack->j();
		switch (stack->opcode()) {
			case Stack::Opcode::REVERSE:
				return {{i, j}};
			case Stack::Opcode::BLKSWAP:
				if (i == 1 && j == 1)
					return {{2, 0}};
				break;
			case Stack::Opcode::XCHG:
				if (i == 0 && j == 1)
					return {{2, 0}};
				if (i == 0 && j == 2)
					return {{3, 0}};
				break;
			default:
				break;
		}
	}
	return {};
}

} // end solidity::frontend