/*
 * Copyright (C) 2021-2026 EverX. All Rights Reserved.
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
 * TVM Solidity abstract syntax tree.
 */

#include <string>
#include <unordered_map>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>

#include <liblangutil/Exceptions.h>

#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>
#include <libsolidity/codegen/TvmAst.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>

using namespace solidity::frontend;

namespace {
bool eq(Pointer<TvmAstNode> const& a, Pointer<TvmAstNode> const& b) {
	if ((a == nullptr) ^ (b == nullptr)) {
		return false;
	}
	return a == nullptr || *a == *b;
}
}

void Loc::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool Loc::operator==(TvmAstNode const& node) const {
	auto n = convertToLoc(&node);
	return n && std::tie(m_file, m_line) == std::tie(n->m_file, n->m_line);
}

Stack::Stack(Stack::Opcode opcode, int i, int j, int k):
	m_opcode{opcode},
	m_i{i},
	m_j{j},
	m_k{k} {}

void Stack::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool Stack::operator==(TvmAstNode const& _node) const {
	auto st = convertToStack(&_node);
	return st && std::tie(m_opcode, m_i, m_j, m_k) == std::tie(st->m_opcode, st->m_i, st->m_j, st->m_k);
}

Glob::Glob(Glob::Opcode opcode, int index):
	Gen{isIn(opcode, Glob::Opcode::GetOrGetVar, Glob::Opcode::PUSHROOT, Glob::Opcode::PUSH_C3)},
	m_opcode{opcode},
	m_index{index} {}

void Glob::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool Glob::operator==(TvmAstNode const& node) const {
	auto g = convertToGlob(&node);
	return g && std::tie(m_opcode, m_index) == std::tie(g->m_opcode, g->m_index);
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

void DeclRetFlag::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool DeclRetFlag::operator==(TvmAstNode const& node) const {
	auto d = convertToDeclRetFlag(&node);
	return d;
}

void Opaque::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_block->accept(_visitor);
	}
}

bool Opaque::operator==(TvmAstNode const& _node) const {
	auto op = convertToOpaque(&_node);
	return op && std::tie(m_take, m_ret) == std::tie(op->m_take, op->m_ret) && m_block.get()->operator==(*op->m_block);
}

void AsymGen::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool AsymGen::operator==(TvmAstNode const& _node) const {
	auto a = convertToAsymGen(&_node);
	return a && opcode() == a->opcode();
}

AsymGen::AsymGen(std::string opcode):
	m_opcode(std::move(opcode)) {
	if (boost::starts_with(m_opcode, "ZERO"))
		solAssert(*GlobalParams::g_tvmVersion != langutil::TVMVersion::ton(), "");
}

void HardCode::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool HardCode::operator==(TvmAstNode const& _node) const {
	auto g = convertToHardCode(&_node);
	return g && std::tie(m_code, m_take, m_ret) == std::tie(g->m_code, g->m_take, g->m_ret);
}

StackGen::StackGen(std::string const& opcode, int take, int ret, bool _isPure):
	Gen{_isPure},
	m_take{take},
	m_ret{ret} {
	auto pos = opcode.find(' ');
	auto posComment = opcode.find(';');
	m_opcode = boost::algorithm::trim_copy(opcode.substr(0, pos));
	if (pos != std::string::npos && pos + 1 < posComment) {
		int n = posComment - (pos + 1);
		m_arg = boost::algorithm::trim_copy(opcode.substr(pos + 1, n));
	}
	if (posComment != std::string::npos) {
		m_comment = opcode.substr(posComment);
	}
}


void StackGen::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

std::string StackGen::fullOpcode() const {
	std::string ret = m_opcode;
	if (!m_arg.empty())
		ret += " " + m_arg;
	if (!m_comment.empty())
		ret += " " + m_comment;
	return ret;
}

bool StackGen::operator==(TvmAstNode const& _node) const {
	auto gen = convertToStackGen(&_node);
	if (gen) {
		if ((isIn(fullOpcode(), "TRUE", "PUSHINT -1") && isIn(gen->fullOpcode(), "TRUE", "PUSHINT -1")) ||
			(isIn(fullOpcode(), "FALSE", "PUSHINT 0") && isIn(gen->fullOpcode(), "FALSE", "PUSHINT 0"))) {
			return true;
		}
	}
	return gen && std::tie(m_opcode, m_arg) == std::tie(gen->m_opcode, gen->m_arg);
}

TvmReturn::TvmReturn(bool _withIf, bool _withNot, bool _withAlt):
	m_withIf{_withIf},
	m_withNot{_withNot},
	m_withAlt{_withAlt} {
	solAssert((m_withNot && m_withIf) || !m_withNot, "");
}

void TvmReturn::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool TvmReturn::operator==(TvmAstNode const& _node) const {
	auto t = convertToTvmReturn(&_node);
	return t && std::tie(m_withIf, m_withNot, m_withAlt) == std::tie(t->m_withIf, t->m_withNot, t->m_withAlt);
}

void ReturnOrBreakOrCont::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_body->accept(_visitor);
	}
}

bool ReturnOrBreakOrCont::operator==(TvmAstNode const& _node) const {
	auto r = convertToReturnOrBreakOrCont(&_node);
	return r && m_take == r->m_take && m_body.get()->operator==(*r->m_body);
}

void TvmException::accept(TvmAstVisitor& _visitor) { _visitor.visit(*this); }

bool TvmException::operator==(TvmAstNode const& _node) const {
	auto ex = convertToTvmException(&_node);
	return ex && std::tie(m_arg, m_any, m_if, m_not, m_param) ==
					 std::tie(ex->m_arg, ex->m_any, ex->m_if, ex->m_not, ex->m_param);
}

std::string TvmException::opcode() const {
	std::string str = "THROW";
	if (m_arg)
		str += "ARG";
	if (m_any)
		str += "ANY";
	if (m_if)
		str += "IF";
	if (m_not)
		str += "NOT";
	return str;
}

int TvmException::take() const {
	int res = 0;
	if (m_arg)
		++res;
	if (m_any)
		++res;
	if (m_if)
		++res;
	return res;
}

CellOrSliceOperation::CellOrSliceOperation(Type type, std::string blob, Pointer<CellOrSliceOperation> child):
	Gen{type != Type::STREFCONST},
	m_type{type},
	m_blob{std::move(blob)},
	m_child{std::move(child)} {
	switch (m_type) {
	case Type::PUSHREF_COMPUTE:
	case Type::PUSHREFSLICE_COMPUTE:
	case Type::PUSHREF:
	case Type::PUSHREFSLICE:
	case Type::CELL:
	case Type::PUSHSLICE:
		m_take = 0;
		m_ret = 1;
		break;
	case Type::STREFCONST:
		m_take = 2;
		m_ret = 1;
		break;
	}
}

void CellOrSliceOperation::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		if (m_child) {
			m_child->accept(_visitor);
		}
	}
}

bool CellOrSliceOperation::operator==(TvmAstNode const& _node) const {
	auto p = convertToPushCellOrSlice(&_node);
	if (p && std::tie(m_type, m_blob) == std::tie(p->m_type, p->m_blob)) {
		if ((m_child == nullptr) ^ (p->m_child == nullptr)) {
			return false;
		}
		if (m_child == nullptr) {
			// p->m_child == nullptr also
			return true;
		}
		return m_child.get()->operator==(*p->m_child);
	}
	return false;
}

bool CellOrSliceOperation::operator<(TvmAstNode const& _node) const {
	auto p = convertToPushCellOrSlice(&_node);
	if ((m_child == nullptr) ^ (p->m_child == nullptr)) {
		return m_child < p->m_child;
	}
	if (m_child == nullptr)
		return std::tie(m_type, m_blob) < std::tie(p->m_type, p->m_blob);
	return std::tie(m_type, m_blob, *m_child) < std::tie(p->m_type, p->m_blob, *p->m_child);
}

std::string CellOrSliceOperation::chainBlob() const {
	std::string s;
	CellOrSliceOperation const* p = this;
	while (p != nullptr) {
		if (p->blob().empty()) {
			solAssert(p->child() == nullptr, "");
		} else {
			solAssert(p->blob().at(0) == 'x');
			s += p->blob().substr(1);
		}
		p = p->child().get();
	}
	return s;
}

void CellOrSliceOperation::updToRef() { m_type = Type::PUSHREFSLICE; }

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
	if (_visitor.visit(*this)) {
		for (Pointer<TvmAstNode> const& node: m_instructions) {
			node->accept(_visitor);
		}
	}
	_visitor.endVisit(*this);
}

bool CodeBlock::operator==(TvmAstNode const& _node) const {
	auto c = convertToCodeBlock(&_node);
	if (c && m_type == c->m_type && m_instructions.size() == c->m_instructions.size()) {
		for (size_t i = 0; i < m_instructions.size(); ++i) {
			if (*m_instructions.at(i) != *c->m_instructions.at(i)) {
				return false;
			}
		}
		return true;
	}
	return false;
}

SubProgram::SubProgram(bool _isJmp, Pointer<CodeBlock> const& _block):
	m_isJmp{_isJmp},
	m_block{_block} {}


void SubProgram::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_block->accept(_visitor);
	}
}

bool SubProgram::operator==(TvmAstNode const& _node) const {
	auto s = convertToSubProgram(&_node);
	if (s == nullptr)
		return false;

	if (m_block == nullptr && s->m_block == nullptr) {
		return true;
	}
	if (m_block != nullptr && s->m_block != nullptr) {
		return m_block.get()->operator==(*s->m_block);
	}
	return false;
}

void LogCircuit::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_body->accept(_visitor);
	}
}

bool LogCircuit::operator==(TvmAstNode const& _node) const {
	auto l = convertToLogCircuit(&_node);
	return l && m_type == l->m_type && m_body.get()->operator==(*l->m_body);
}

TvmIfElse::TvmIfElse(
	bool _withNot,
	bool _withJmp,
	Pointer<CodeBlock> const& trueBody,
	Pointer<CodeBlock> const& falseBody,
	int ret
):
	m_withNot{_withNot},
	m_withJmp{_withJmp},
	m_trueBody(trueBody),
	m_falseBody(falseBody),
	m_ret{ret} {
	solAssert((m_withNot && falseBody == nullptr) || !m_withNot, "");
}

void TvmIfElse::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_trueBody->accept(_visitor);
		if (m_falseBody) {
			m_falseBody->accept(_visitor);
		}
	}
}

bool TvmIfElse::operator==(TvmAstNode const& _node) const {
	auto op = convertToTvmIfElse(&_node);
	return op &&
		   eq(m_trueBody, op->m_trueBody) &&
		   eq(op->m_falseBody, op->m_falseBody) &&
		   std::tie(m_withNot, m_withJmp, m_ret) == std::tie(op->m_withNot, op->m_withJmp, op->m_ret);
}

void TvmRepeat::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_body->accept(_visitor);
	}
}

void TvmUntil::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_body->accept(_visitor);
	}
}

void While::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_condition->accept(_visitor);
		body()->accept(_visitor);
	}
}

Function::Function(
	int take,
	int ret,
	std::string name,
	std::optional<uint32_t> _functionId,
	Pointer<CodeBlock> block,
	FunctionDefinition const* _function,
	bool canBeDelete
):
	m_take{take},
	m_ret{ret},
	m_name{std::move(name)},
	m_functionId{_functionId},
	m_block{std::move(block)},
	m_function{_function},
	m_canBeDelete{canBeDelete} {}

void Function::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		m_block->accept(_visitor);
	}
}

void Contract::accept(TvmAstVisitor& _visitor) {
	if (_visitor.visit(*this)) {
		for (Pointer<Function>& node: m_functions) {
			node->accept(_visitor);
		}
	}
}

namespace solidity::frontend {
Pointer<StackGen> gen(std::string const& cmd) {
	std::string op;
	std::string param;
	{
		std::istringstream iss(cmd);
		iss >> op >> param;
	}

	auto f = [&](std::string const& pattern) { return op == pattern; };

	auto dictReplaceOrAdd = [&] {
		for (std::string key: {"", "I", "U"}) {
			for (std::string oper: {"REPLACE", "ADD"}) {
				for (std::string suf: {"", "REF", "B"}) {
					std::string candidat = "DICT" + key + oper + suf;
					if (candidat == cmd) {
						return true;
					}
				}
			}
		}
		return false;
	};

	auto dictSet = [&] {
		for (std::string key: {"", "I", "U"}) {
			for (std::string suf: {"", "REF", "B"}) {
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

		OpcodeParams(int _take, int _ret, bool _isPure = false):
			take{_take},
			ret{_ret},
			isPure{_isPure} {}
	};

	static std::unordered_map<std::string, OpcodeParams> opcodes = {
		{"ACCEPT", {0, 0}},
		{"COMMIT", {0, 0}},
		{"PRINTSTR", {0, 0}},

		{"BLOCKLT", {0, 1, true}},
		{"BLS_G1_ZERO", {0, 1, true}},
		{"BLS_G2_ZERO", {0, 1, true}},
		{"BLS_PUSHR", {0, 1, true}},
		{"DUEPAYMENT", {0, 1, true}},
		{"FALSE", {0, 1, true}},
		{"GASCONSUMED", {0, 1, true}},
		{"GASREMAINING", {0, 1}},
		{"BALANCE", {0, 1, true}},
		{"GETPRECOMPILEDGAS", {0, 1, true}},
		{"GLOBALID", {0, 1, true}},
		{"INCOMINGVALUE", {0, 1, true}},
		{"INMSG_BOUNCE", {0, 1, true}},
		{"INMSG_BOUNCED", {0, 1, true}},
		{"INMSG_FWDFEE", {0, 1, true}},
		{"INMSG_LT", {0, 1, true}},
		{"INMSG_ORIGVALUE", {0, 1, true}},
		{"INMSG_SRC", {0, 1, true}},
		{"INMSG_STATEINIT", {0, 1, true}},
		{"INMSG_UTIME", {0, 1, true}},
		{"INMSG_VALUE", {0, 1, true}},
		{"INMSG_VALUEEXTRA", {0, 1, true}},
		{"LTIME", {0, 1, true}},
		{"MYADDR", {0, 1, true}},
		{"MYCODE", {0, 1, true}},
		{"NEWC", {0, 1, true}},
		{"NOW", {0, 1, true}},
		{"NULL", {0, 1, true}},
		{"PREVBLOCKSINFOTUPLE", {0, 1, true}},
		{"PREVKEYBLOCK", {0, 1, true}},
		{"PREVMCBLOCKS", {0, 1, true}},
		{"PREVMCBLOCKS_100", {0, 1, true}},
		{"PUSHINT", {0, 1, true}},
		{"PUSHNAN", {0, 1, true}},
		{"RANDSEED", {0, 1, true}},
		{"RANDU256", {0, 1}},
		{"RIST255_PUSHL", {0, 1, true}},
		{"STORAGEFEES", {0, 1, true}},
		{"TRUE", {0, 1, true}},
		{"UNPACKEDCONFIGTUPLE", {0, 1, true}},

		{"ADDRAND", {1, 0}},
		{"BUYGAS", {1, 0}},
		{"ENDS", {1, 0}},
		{"RIST255_VALIDATE", {1, 0}},
		{"SETCODE", {1, 0}},
		{"SETGASLIMIT", {1, 0}},
		{"SETRAND", {1, 0}},

		{"ABS", {1, 1}},
		{"ADDCONST", {1, 1}},
		{"BBITS", {1, 1, true}},
		{"BDEPTH", {1, 1}},
		{"BITNOT", {1, 1}}, // pseudo opcode. Alias for NOT
		{"BITSIZE", {1, 1, true}},
		{"BLESS", {1, 1}},
		{"BLS_G1_INGROUP", {1, 1}},
		{"BLS_G1_ISZERO", {1, 1}},
		{"BLS_G1_NEG", {1, 1}},
		{"BLS_G2_INGROUP", {1, 1}},
		{"BLS_G2_ISZERO", {1, 1}},
		{"BLS_G2_NEG", {1, 1}},
		{"BLS_MAP_TO_G1", {1, 1}},
		{"BLS_MAP_TO_G2", {1, 1}},
		{"BREFS", {1, 1, true}},
		{"BREMBITS", {1, 1, true}},
		{"BREMREFS", {1, 1, true}},
		{"BTOS", {1, 1, true}},
		{"CDEPTH", {1, 1, true}},
		{"CDEPTHI", {1, 1, true}},
		{"CHASHI", {1, 1, true}},
		{"CLEVEL", {1, 1, true}},
		{"CLEVELMASK", {1, 1, true}},
		{"CONFIGOPTPARAM", {1, 1, true}},
		{"CTOS", {1, 1}},
		{"DEC", {1, 1}},
		{"DICTEMPTY", {1, 1, true}},
		{"ENDC", {1, 1}},
		{"EQINT", {1, 1, true}},
		{"FITS", {1, 1}},
		{"GETEXTRABALANCE", {1, 1, true}},
		{"GTINT", {1, 1, true}},
		{"HASHBU", {1, 1, true}},
		{"HASHCU", {1, 1, true}},
		{"HASHSU", {1, 1, true}},
		{"INC", {1, 1}},
		{"INDEX2", {1, 1}},
		{"INDEX3", {1, 1}},
		{"INDEX_EXCEP", {1, 1}},
		{"INDEX_NOEXCEP", {1, 1, true}},
		{"ISNAN", {1, 1, true}},
		{"ISNEG", {1, 1, true}},
		{"ISNNEG", {1, 1, true}},
		{"ISNPOS", {1, 1, true}},
		{"ISNULL", {1, 1, true}},
		{"ISPOS", {1, 1, true}}, // TODO GTINT 0 and for another
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
		{"PLDILE4", {1, 1}},
		{"PLDILE8", {1, 1}},
		{"PLDREFIDX", {1, 1}},
		{"PLDSLICE", {1, 1}},
		{"PLDU", {1, 1}},
		{"PLDULE4", {1, 1}},
		{"PLDULE8", {1, 1}},
		{"POW2", {1, 1}},
		{"QBITNOT", {1, 1, true}}, // pseudo opcode. Alias for QNOT
		{"QDEC", {1, 1, true}},
		{"QFITS", {1, 1, true}},
		{"QINC", {1, 1, true}},
		{"QNEGATE", {1, 1, true}},
		{"QNOT", {1, 1, true}}, // logical not
		{"QSGN", {1, 1, true}},
		{"QUFITS", {1, 1, true}},
		{"RAND", {1, 1}},
		{"RIST255_MULBASE", {1, 1}},
		{"RIST255_QVALIDATE", {1, 1}},
		{"SBITS", {1, 1, true}},
		{"SDEMPTY", {1, 1, true}},
		{"SDEPTH", {1, 1}},
		{"SDFIRST", {1, 1, true}},
		{"SEMPTY", {1, 1, true}},
		{"SGN", {1, 1, true}},
		{"SHA256U", {1, 1, true}},
		{"SREFS", {1, 1, true}},
		{"SREMPTY", {1, 1, true}},
		{"STRDUMP", {1, 1}},
		{"STSLICECONST", {1, 1}},
		{"TLEN", {1, 1}},
		{"UBITSIZE", {1, 1}},
		{"UFITS", {1, 1}},
		{"XLOAD", {1, 1}},

		{"BBITREFS", {1, 2, true}},
		{"BREMBITREFS", {1, 2, true}},
		{"LDDICT", {1, 2}},
		{"LDI", {1, 2}},
		{"LDILE4", {1, 2}},
		{"LDILE8", {1, 2}},
		{"LDMSGADDR", {1, 2}},
		{"LDONES", {1, 2, true}},
		{"LDREF", {1, 2}},
		{"LDREFRTOS", {1, 2}},
		{"LDSLICE", {1, 2}},
		{"LDU", {1, 2}},
		{"LDULE4", {1, 2}},
		{"LDULE8", {1, 2}},
		{"LDVARINT16", {1, 2}},
		{"LDVARINT32", {1, 2}},
		{"LDVARUINT16", {1, 2}},
		{"LDVARUINT32", {1, 2}},
		{"LDZEROES", {1, 2, true}},
		{"REWRITESTDADDR", {1, 2}},
		{"SBITREFS", {1, 2, true}},
		{"TPOP", {1, 2}},
		{"XCTOS", {1, 2}},
		{"XLOADQ", {1, 2}},

		{"RAWRESERVE", {2, 0}},
		{"SENDRAWMSG", {2, 0}},

		{"ADD", {2, 1}},
		{"AND", {2, 1, true}},
		{"BLS_G1_ADD", {2, 1}},
		{"BLS_G1_MUL", {2, 1}},
		{"BLS_G1_SUB", {2, 1}},
		{"BLS_G2_ADD", {2, 1}},
		{"BLS_G2_MUL", {2, 1}},
		{"BLS_G2_SUB", {2, 1}},
		{"CDEPTHIX", {2, 1, true}},
		{"CHASHIX", {2, 1, true}},
		{"CMP", {2, 1, true}},
		{"DIV", {2, 1}},
		{"DIVC", {2, 1}},
		{"DIVR", {2, 1}},
		{"ENDXC", {2, 1}},
		{"EQUAL", {2, 1, true}},
		{"GEQ", {2, 1, true}},
		{"GETGASFEE", {2, 1}},
		{"GETGASFEESIMPLE", {2, 1, true}},
		{"GETORIGINALFWDFEE", {2, 1, true}},
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
		{"PLDIX", {2, 1}},
		{"PLDREFVAR", {2, 1}},
		{"PLDSLICEX", {2, 1}},
		{"PLDUX", {2, 1}},
		{"QADD", {2, 1, true}},
		{"QAND", {2, 1, true}},
		{"QDIV", {2, 1, true}},
		{"QDIVC", {2, 1, true}},
		{"QDIVR", {2, 1, true}},
		{"QEQUAL", {2, 1, true}},
		{"QGEQ", {2, 1, true}},
		{"QGREATER", {2, 1, true}},
		{"QLEQ", {2, 1, true}},
		{"QLESS", {2, 1, true}},
		{"QMAX", {2, 1, true}},
		{"QMIN", {2, 1, true}},
		{"QMOD", {2, 1, true}},
		{"QMUL", {2, 1, true}},
		{"QNEQ", {2, 1, true}},
		{"QOR", {2, 1, true}},
		{"QSUB", {2, 1, true}},
		{"QXOR", {2, 1, true}},
		{"QXOR", {2, 1, true}},
		{"RIST255_ADD", {2, 1}},
		{"RIST255_FROMHASH", {2, 1, true}},
		{"RIST255_MUL", {2, 1}},
		{"RIST255_SUB", {2, 1}},
		{"SCHKBITSQ", {2, 1, true}},
		{"SCHKREFSQ", {2, 1, true}},
		{"SDEQ", {2, 1, true}},
		{"SDLEXCMP", {2, 1}},
		{"SDPFXREV", {2, 1, true}},
		{"SDSKIPFIRST", {2, 1}},
		{"SENDMSG", {2, 1}},
		{"SETINDEX", {2, 1}},
		{"SETINDEXQ", {2, 1, true}},
		{"STB", {2, 1}},	 // CF13
		{"STBR", {2, 1}},	 // CF17
		{"STBREF", {2, 1}},	 // CF11
		{"STBREFR", {2, 1}}, // CD
		{"STDICT", {2, 1}},
		{"STI", {2, 1}},
		{"STILE4", {2, 1}},
		{"STILE8", {2, 1}},
		{"STIR", {2, 1}},
		{"STONES", {2, 1}},
		{"STREF", {2, 1}},	  // CC
		{"STREFR", {2, 1}},	  // CF14
		{"STSLICE", {2, 1}},  // CE
		{"STSLICER", {2, 1}}, // CF16
		{"STU", {2, 1}},
		{"STULE4", {2, 1}},
		{"STULE8", {2, 1}},
		{"STUR", {2, 1}},
		{"STVARINT16", {2, 1}},
		{"STVARINT32", {2, 1}},
		{"STVARUINT16", {2, 1}},
		{"STVARUINT32", {2, 1}},
		{"STZEROES", {2, 1}},
		{"SUB", {2, 1}},
		{"SUBR", {2, 1}}, // TODO add QSUBR ?
		{"TPUSH", {2, 1}},
		{"XOR", {2, 1, true}},

		{"DIVMOD", {2, 2}},
		{"LDIX", {2, 2}},
		{"LDSAME", {2, 2, true}},
		{"LDSLICEX", {2, 2}},
		{"LDUX", {2, 2}},
		{"MINMAX", {2, 2, true}},
		{"QDIVMOD", {2, 2, true}},
		{"QMINMAX", {2, 2, true}},

		{"CDATASIZE", {2, 3}},
		{"SDATASIZE", {2, 3}},

		{"RAWRESERVEX", {3, 0}},

		{"BLS_VERIFY", {3, 1}},
		{"CHKSIGNS", {3, 1}},
		{"CHKSIGNU", {3, 1}},
		{"CONDSEL", {3, 1}},
		{"GETFORWARDFEE", {3, 1}},
		{"GETFORWARDFEESIMPLE", {3, 1}},
		{"MULDIV", {3, 1}},
		{"MULDIVC", {3, 1}},
		{"MULDIVR", {3, 1}},
		{"MULMOD", {3, 1}},
		{"P256_CHKSIGNS", {3, 1}},
		{"P256_CHKSIGNU", {3, 1}},
		{"QMULDIV", {3, 1, true}},
		{"QMULDIVC", {3, 1, true}},
		{"QMULDIVR", {3, 1, true}},
		{"SCHKBITREFSQ", {3, 1, true}},
		{"SCUTFIRST", {3, 1}},
		{"SCUTLAST", {3, 1}},
		{"SETINDEXVAR", {3, 1}},
		{"SETINDEXVARQ", {3, 1, true}},
		{"SSKIPFIRST", {3, 1}},
		{"STIX", {3, 1}},
		{"STIXR", {3, 1}},
		{"STSAME", {3, 1}},
		{"STUX", {3, 1}},
		{"STUXR", {3, 1}},

		{"DICTDEL", {3, 2}},
		{"DICTIDEL", {3, 2}},
		{"DICTUDEL", {3, 2}},
		{"MULDIVMOD", {3, 2}},
		{"QMULDIVMOD", {3, 2, true}},
		{"SPLIT", {3, 2}},

		{"GETSTORAGEFEE", {4, 1, true}},
	};
	static bool isInit = false;
	if (!isInit) {
		isInit = true;
		auto const combArithOpers = tonCombinedArithmeticOperations();
		for (auto const& arith: combArithOpers) {
			opcodes.insert(
				{boost::to_upper_copy<std::string>(arith.name),
				 {static_cast<int>(arith.take), static_cast<int>(arith.ret)}}
			);
		}
	}

	Pointer<StackGen> opcode;
	if (opcodes.contains(op)) {
		OpcodeParams params = opcodes.at(op);
		opcode = createNode<StackGen>(cmd, params.take, params.ret, params.isPure);
	} else if (dictSet()) {
		opcode = createNode<StackGen>(cmd, 4, 1);
	} else if (dictReplaceOrAdd()) {
		opcode = createNode<StackGen>(cmd, 4, 2);
	} else if (f("TUPLE")) {
		int ret = boost::lexical_cast<int>(param);
		opcode = createNode<StackGen>(cmd, ret, 1);
	} else if (f("UNTUPLE")) {
		int ret = boost::lexical_cast<int>(param);
		opcode = createNode<StackGen>(cmd, 1, ret);
	} else if (f("UNPACKFIRST")) {
		int ret = boost::lexical_cast<int>(param);
		opcode = createNode<StackGen>(cmd, 1, ret);
	} else if (f("LSHIFT") || f("QLSHIFT") || f("RSHIFT") || f("QRSHIFT")) {
		if (param.empty())
			opcode = createNode<StackGen>(cmd, 2, 1);
		else
			opcode = createNode<StackGen>(cmd, 1, 1);
	} else if (f("MULRSHIFT")) {
		if (param.empty())
			opcode = createNode<StackGen>(cmd, 3, 1);
		else
			opcode = createNode<StackGen>(cmd, 2, 1);
	} else
		solUnimplemented("Unknown opcode: " + cmd);
	solAssert(opcode != nullptr, "");
	return opcode;
}

// TODO DELETE use makePushCellOrSlice
Pointer<CellOrSliceOperation> genPushSlice(std::string const& data) {
	if (StrUtils::toBitString(data).length() <= TvmConst::MaxPushSliceBitLength)
		return createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHSLICE, data, nullptr);
	return createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREFSLICE, data, nullptr);
}

Pointer<CellOrSliceOperation> makePushCellOrSlice(std::string const& hexStr, bool toSlice) {
	solAssert(hexStr.size() % 2 == 0, "");

	int const length = hexStr.size();
	constexpr int symbolQty =
		((TvmConst::CellBitLength / 8) * 8) / 4; // one symbol in string == 8 bit. Letter can't be divided into 2 cells
	CellOrSliceOperation::Type type =
		toSlice ? CellOrSliceOperation::Type::PUSHREFSLICE : CellOrSliceOperation::Type::PUSHREF;
	std::vector<std::pair<CellOrSliceOperation::Type, std::string>> data;
	int start = 0;
	do {
		std::string slice = hexStr.substr(start, std::min(symbolQty, length - start));
		data.emplace_back(type, "x" + slice);
		start += symbolQty;
		type = CellOrSliceOperation::Type::CELL;
	} while (start < length);

	Pointer<CellOrSliceOperation> cell;
	for (auto const& [t, d]: data | std::views::reverse) {
		cell = createNode<CellOrSliceOperation>(t, d, cell);
	}
	return cell;
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
	if (qty == 1)
		return makePUSH(index);
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

Pointer<TvmReturn> makeRET() { return createNode<TvmReturn>(false, false, false); }

Pointer<TvmReturn> makeRETALT() { return createNode<TvmReturn>(false, false, true); }

Pointer<TvmReturn> makeIFRETALT() { return createNode<TvmReturn>(true, false, true); }

Pointer<TvmReturn> makeIFRET() { return createNode<TvmReturn>(true, false, false); }

Pointer<TvmReturn> makeIFNOTRET() { return createNode<TvmReturn>(true, true, false); }

Pointer<TvmReturn> makeIFNOTRETALT() { return createNode<TvmReturn>(true, true, true); }

Pointer<TvmException> makeTHROW(std::string const& cmd) {
	std::string op;
	std::string param;
	{
		std::istringstream iss(cmd);
		iss >> op >> param;
	}

	auto skip = [](std::string& str, std::string const& pattern) -> bool {
		if (boost::starts_with(str, pattern)) {
			str = str.substr(pattern.size());
			return true;
		}
		return false;
	};


	solAssert(skip(op, "THROW"), "");
	bool _arg = skip(op, "ARG");
	bool _any = skip(op, "ANY");
	bool _if = skip(op, "IF");
	bool _not = skip(op, "NOT");
	solAssert(op.empty(), "");

	return createNode<TvmException>(_arg, _any, _if, _not, param);
}

Pointer<Stack> makeXCH_S(int i) { return makeXCH_S_S(0, i); }

Pointer<Stack> makeXCH_S_S(int i, int j) {
	solAssert(i <= j, "");
	return createNode<Stack>(Stack::Opcode::XCHG, i, j);
}

Pointer<Glob> makeGetGlob(int i) { return createNode<Glob>(Glob::Opcode::GetOrGetVar, i); }

Pointer<Glob> makeSetGlob(int i) { return createNode<Glob>(Glob::Opcode::SetOrSetVar, i); }

Pointer<Stack> makeBLKDROP2(int droppedCount, int leftCount) {
	solAssert(1 <= droppedCount, "");
	solAssert(0 <= leftCount, "");
	if (leftCount == 0) {
		return makeDROP(droppedCount);
	}
	return createNode<Stack>(Stack::Opcode::BLKDROP2, droppedCount, leftCount);
}

Pointer<CellOrSliceOperation> makePUSHREFSLICE(std::string const& data) {
	return createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREFSLICE, data, nullptr);
}

Pointer<CellOrSliceOperation> makePUSHREF(std::string const& data) {
	return createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREF, data, nullptr);
}

Pointer<Stack> makeREVERSE(int qty, int index) {
	solAssert(qty >= 2, "");
	solAssert(index >= 0, "");
	return createNode<Stack>(Stack::Opcode::REVERSE, qty, index);
}

Pointer<Stack> makeROT() { return createNode<Stack>(Stack::Opcode::BLKSWAP, 1, 2); }

Pointer<Stack> makeROTREV() { return createNode<Stack>(Stack::Opcode::BLKSWAP, 2, 1); }

Pointer<Stack> makeBLKSWAP(int down, int top) {
	solAssert(down >= 1 && top >= 1, "");
	return createNode<Stack>(Stack::Opcode::BLKSWAP, down, top);
}

Pointer<Stack> makePUXC(int i, int j) {
	solAssert(0 <= i && i <= 15, "");
	solAssert(-1 <= j && j <= 14, "");
	return createNode<Stack>(Stack::Opcode::PUXC, i, j);
}

Pointer<Stack> makeXCPU(int i, int j) {
	solAssert(0 <= i && i <= 15, "");
	solAssert(0 <= j && j <= 15, "");
	return createNode<Stack>(Stack::Opcode::XCPU, i, j);
}

Pointer<TvmIfElse> flipIfElse(TvmIfElse const& node) {
	if (node.falseBody() == nullptr) {
		return createNode<TvmIfElse>(!node.withNot(), node.withJmp(), node.trueBody(), node.falseBody(), node.ret());
	}
	return createNode<TvmIfElse>(node.withNot(), node.withJmp(), node.falseBody(), node.trueBody(), node.ret());
}

Stack const* convertToStack(TvmAstNode const* _node) {
	if (_node && _node->category() == TvmAstNode::Category::Stack) {
		return static_cast<Stack const*>(_node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

Gen const* convertToGen(TvmAstNode const* _node) {
	if (_node && isIn(
					 _node->category(),
					 TvmAstNode::Category::Glob,
					 TvmAstNode::Category::Opaque,
					 TvmAstNode::Category::HardCode,
					 TvmAstNode::Category::StackGen,
					 TvmAstNode::Category::PushCellOrSlice,
					 TvmAstNode::Category::TvmException
				 )) {
		return static_cast<Gen const*>(_node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

Loc const* convertToLoc(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::Loc) {
		return static_cast<Loc const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

StackGen const* convertToStackGen(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::StackGen) {
		return static_cast<StackGen const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

Glob const* convertToGlob(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::Glob) {
		return static_cast<Glob const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

TvmReturn const* convertToTvmReturn(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::TvmReturn) {
		return static_cast<TvmReturn const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

TvmException const* convertToTvmException(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::TvmException) {
		return static_cast<TvmException const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

CellOrSliceOperation const* convertToPushCellOrSlice(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::PushCellOrSlice) {
		return static_cast<CellOrSliceOperation const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

SubProgram const* convertToSubProgram(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::SubProgram) {
		return static_cast<SubProgram const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

CodeBlock const* convertToCodeBlock(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::CodeBlock) {
		return static_cast<CodeBlock const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

TvmIfElse const* convertToTvmIfElse(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::TvmIfElse) {
		return static_cast<TvmIfElse const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

Opaque const* convertToOpaque(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::Opaque) {
		return static_cast<Opaque const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

ReturnOrBreakOrCont const* convertToReturnOrBreakOrCont(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::ReturnOrBreakOrCont) {
		return static_cast<ReturnOrBreakOrCont const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

HardCode const* convertToHardCode(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::HardCode) {
		return static_cast<HardCode const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

While const* convertToWhile(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::While) {
		return static_cast<While const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

LogCircuit const* convertToLogCircuit(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::LogCircuit) {
		return static_cast<LogCircuit const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

DeclRetFlag const* convertToDeclRetFlag(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::DeclRetFlag) {
		return static_cast<DeclRetFlag const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

AsymGen const* convertToAsymGen(TvmAstNode const* node) {
	if (node && node->category() == TvmAstNode::Category::AsymGen) {
		return static_cast<AsymGen const*>(node); // NOLINT(*-pro-type-static-cast-downcast)
	}
	return nullptr;
}

std::string arg(Pointer<TvmAstNode> const& node) {
	auto g = convertToStackGen(node.get());
	solAssert(g, "");
	return g->arg();
}

bool isPureGen01(TvmAstNode const& node) {
	// See also isSimpleCommand
	auto gen = convertToGen(&node);
	return gen && gen->isPure() && gen->take() == 0 && gen->ret() == 1;
}

bool isInline(TvmAstNode const& node, std::string const& name) {
	StackGen const* stackGen = convertToStackGen(&node);
	return stackGen != nullptr && stackGen->opcode() == ".inline" && stackGen->arg() == name;
}

bool isSWAP(Pointer<TvmAstNode> const& node) {
	return isBLKSWAP(node) && isBLKSWAP(node).value() == std::make_pair(1, 1);
}

// down, top
std::optional<std::pair<int, int>> isBLKSWAP(Pointer<TvmAstNode> const& _node) {
	if (auto stack = convertToStack(_node.get())) {
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
	auto stack = convertToStack(node.get());
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

std::optional<std::pair<int, int>> isBLKDROP2(Pointer<TvmAstNode> const& node) {
	if (isStack(node, Stack::Opcode::BLKDROP2)) {
		auto stack = convertToStack(node.get());
		return {{stack->i(), stack->j()}};
	}
	if (isStack(node, Stack::Opcode::POP_S)) {
		auto stack = convertToStack(node.get());
		if (stack->i() == 1)
			return {{1, 1}};
	}
	return std::nullopt;
}

std::optional<std::pair<int, int>> isBLKDROP2OrDrop(Pointer<TvmAstNode> const& node) {
	if (isBLKDROP2(node)) {
		return isBLKDROP2(node).value();
	}
	if (isDrop(node))
		return {{isDrop(node).value(), 0}};
	return {};
}

bool isStack(Pointer<TvmAstNode> const& node, Stack::Opcode op) {
	auto stack = convertToStack(node.get());
	return stack && stack->opcode() == op;
}

std::optional<int> isPOP(Pointer<TvmAstNode> const& _node) {
	if (auto stack = convertToStack(_node.get())) {
		switch (stack->opcode()) {
		case Stack::Opcode::POP_S:
			return stack->i();
		case Stack::Opcode::BLKDROP2:
			if (stack->i() == 1 && stack->j() == 1)
				return 1;
			break;
		default:
			break;
		}
	}
	return {};
}

std::optional<int> isPUSH(Pointer<TvmAstNode> const& _node) {
	if (auto stack = convertToStack(_node.get())) {
		switch (stack->opcode()) {
		case Stack::Opcode::PUSH_S:
			return stack->i();
		case Stack::Opcode::BLKPUSH:
			if (stack->i() == 1)
				return stack->j();
			break;
		default:
			break;
		}
	}
	return {};
}

std::optional<std::pair<int, int>> isPUSH2(Pointer<TvmAstNode> const& _node) {
	if (auto stack = convertToStack(_node.get())) {
		switch (stack->opcode()) {
		case Stack::Opcode::PUSH2_S:
			return std::make_pair<int, int>(stack->i(), stack->j());
		case Stack::Opcode::BLKPUSH:
			if (stack->i() == 2) {
				if (stack->j() == 0)
					return std::make_pair<int, int>(stack->j(), stack->j());
				return std::make_pair<int, int>(stack->j(), stack->j() - 1);
			}
			break;
		default:
			break;
		}
	}
	return {};
}

std::optional<std::pair<int, int>> isBLKPUSH(Pointer<TvmAstNode> const& node) {
	if (auto stack = convertToStack(node.get())) {
		switch (stack->opcode()) {
		case Stack::Opcode::BLKPUSH:
			return {{stack->i(), stack->j()}};
		case Stack::Opcode::PUSH_S:
			if (stack->i() == 0)
				return {{1, 0}};
			break;
		// TODO add PUSH_S and PUSH2_S
		default:
			break;
		}
	}
	return {};
}

bool isXCHG(Pointer<TvmAstNode> const& node, int i, int j) {
	auto cmd2Stack = convertToStack(node.get());
	return cmd2Stack && cmd2Stack->opcode() == Stack::Opcode::XCHG && cmd2Stack->i() == i && cmd2Stack->j() == j;
}

std::optional<int> isXCHG_S0(Pointer<TvmAstNode> const& node) {
	auto stack = convertToStack(node.get());
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
	auto stack = convertToStack(node.get());
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

CellOrSliceOperation const* isPlainPushSlice(Pointer<TvmAstNode> const& node) {
	if (auto p = convertToPushCellOrSlice(node.get())) {
		if (p->child() == nullptr)
			return p;
	}
	return {};
}

int getRootBitSize(CellOrSliceOperation const& _node) {
	int size = StrUtils::toBitString(_node.blob()).length();
	return size;
}

Pointer<AsymGen> getZeroOrNullAlignment(bool isZero, bool isSwap, bool isNot) {
	std::string cmd;
	cmd += isZero ? "ZERO" : "NULL";
	cmd += isSwap ? "SWAP" : "ROTR";
	cmd += "IF";
	if (isNot)
		cmd += "NOT";
	return std::make_shared<AsymGen>(cmd);
}

int OpcodeUtils::gasCost(Stack const& opcode) {
	int i = opcode.i();
	int j = opcode.j();
	// int k = opcode.k();
	switch (opcode.opcode()) {
	case Stack::Opcode::POP_S:
		return 18;
	case Stack::Opcode::DROP: {
		int n = i;
		if (n == 1 || n == 2)
			return 18; // "DROP" "DROP2"
		if (n <= 15)
			return 26;	// BLKDROP
		return 18 + 18; // PUSHINT N + DROPX
	}
	case Stack::Opcode::BLKDROP2: {
		if (i > 15 || j > 15)
			solUnimplemented("");
		return 26;
	}
	case Stack::Opcode::BLKSWAP: {
		int bottom = i;
		int top = j;
		if (bottom == 1 && top == 1) {
			return 18; // SWAP
		}
		if (bottom == 1 && top == 2) {
			return 18; // "ROT";
		}
		if (bottom == 2 && top == 1) {
			return 18; // "ROTREV";
		}
		if (bottom == 2 && top == 2) {
			return 18; // "SWAP2";
		}
		if (1 <= bottom && bottom <= 16 && 1 <= top && top <= 16) {
			return 26; // "ROLL " "ROLLREV " "BLKSWAP"
		}
		solUnimplemented(""); // "ROLLX" "ROLLREVX" "BLKSWX"
	}
	case Stack::Opcode::BLKPUSH: {
		if ((i == 2 && j == 1) || (i == 2 && j == 3)) {
			return 18; // "DUP2" "OVER2"
		} else {
			if (i > 15)
				solAssert(j == 0, "");
			int rest = i;
			int cost = 0;
			while (rest > 0) {
				cost += 26; // "BLKPUSH "
				rest -= 15;
			}
			return cost;
		}
	}
	case Stack::Opcode::PUSH2_S:
		if ((i == 1 && j == 0) || (i == 3 && j == 2))
			return 18; // "DUP2" "OVER2"
		return 26;	   // "PUSH2"
	case Stack::Opcode::REVERSE:
		if ((i == 2 && j == 0) || (i == 3 && j == 0))
			return 18; // "SWAP" "XCHG S2"
		if (2 <= i && i <= 17 && 0 <= j && j <= 15)
			return 26; // "REVERSE"
		solUnimplemented("");
	case Stack::Opcode::XCHG:
		if (i == 0 || i == 1)
			return 18; // "XCHG Sj" "XCHG s1, Sj"
		return 26;	   // XCHG Si, Sj
	case Stack::Opcode::PUSH_S:
		return 18;
	case Stack::Opcode::XCHG3:
	case Stack::Opcode::XCHG2:
	case Stack::Opcode::XCPU:
	case Stack::Opcode::PUXC:
		return 26;
	case Stack::Opcode::PUSH3_S:
	case Stack::Opcode::XC2PU:
	case Stack::Opcode::XCPU2:
	case Stack::Opcode::PUXC2:
	case Stack::Opcode::XCPUXC:
	case Stack::Opcode::PUXCPU:
	case Stack::Opcode::PU2XC:
		return 34;
	}
	solUnimplemented("");
}

} // end solidity::frontend
