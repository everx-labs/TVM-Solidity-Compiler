/*
 * Copyright (C) 2021-2023 EverX. All Rights Reserved.
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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#include <ostream>
#include <memory>

#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <liblangutil/Exceptions.h>
#include "TVMCommons.hpp"

using namespace solidity::frontend;

bool Printer::visit(AsymGen &_node) {
	tabs();
	m_out << _node.opcode() << std::endl;
	return false;
}

bool Printer::visit(DeclRetFlag &/*_node*/) {
	tabs();
	m_out << "FALSE ; decl return flag" << std::endl;
	return false;
}

bool Printer::visit(Opaque &_node) {
	_node.block()->accept(*this);
	return false;
}

bool Printer::visit(HardCode &_node) {
	for (const std::string& s : _node.code()) {
		tabs();
		m_out << s << std::endl;
	}
	return false;
}


//bool Printer::visit(Loc &) { return false; }

bool Printer::visit(Loc &_node) {
	tabs();
	m_out << ".loc " << _node.file() << ", " << _node.line() << std::endl;
	return false;
}

bool Printer::visit(TvmReturn &_node) {
	tabs();
	if (_node.withIf()) {
		m_out << "IF";
	}
	if (_node.withNot()) {
		m_out << "NOT";
	}
	m_out << "RET";
	if (_node.withAlt()) {
		m_out << "ALT";
	}
	m_out << std::endl;
	return false;
}

bool Printer::visit(ReturnOrBreakOrCont &_node) {
	tabs();
	m_out << "; start return" << std::endl;
	_node.body()->accept(*this);
	tabs();
	m_out << "; end return" << std::endl;
	return false;
}

bool Printer::visit(TvmException &_node) {
	tabs();
	m_out << _node.opcode();
	if (!_node.arg().empty())
		m_out << " " << _node.arg();
	m_out << std::endl;
	return false;
}

bool Printer::visit(StackOpcode &_node) {
	tabs();
	if (_node.fullOpcode() == "BITNOT") m_out << "NOT";
	//else if (_node.fullOpcode() == "STVARUINT16") m_out << "STGRAMS";
	//else if (_node.fullOpcode() == "LDVARUINT16") m_out << "LDGRAMS";
	else if (_node.fullOpcode() == "TUPLE 1") m_out << "SINGLE";
	else if (_node.fullOpcode() == "TUPLE 2") m_out << "PAIR";
	else if (_node.fullOpcode() == "TUPLE 3") m_out << "TRIPLE";
	else if (_node.fullOpcode() == "UNTUPLE 1") m_out << "UNSINGLE";
	else if (_node.fullOpcode() == "UNTUPLE 2") m_out << "UNPAIR";
	else if (_node.fullOpcode() == "UNTUPLE 3") m_out << "UNTRIPLE";
	else if (_node.opcode() == "UNTUPLE") {
		int ret = boost::lexical_cast<int>(_node.arg());
		if (ret <= 15) {
			m_out << "UNTUPLE " << ret;
		} else {
			m_out << "PUSHINT " << ret << std::endl;
			tabs();
			m_out << "UNTUPLEVAR";
		}
	} else if (_node.opcode() == "UNPACKFIRST") {
		int ret = boost::lexical_cast<int>(_node.arg());
		if (ret <= 15) {
			m_out << "UNPACKFIRST " << ret;
		} else {
			m_out << "PUSHINT " << ret << std::endl;
			tabs();
			m_out << "UNPACKFIRSTVAR";
		}
	} else if (_node.fullOpcode() == "STSLICECONST x4_") m_out << "STZERO";
	else if (_node.fullOpcode() == "STSLICECONST xc_") m_out << "STONE";
	else if (isIn(_node.opcode(), "INDEX_EXCEP", "INDEX_NOEXCEP")) {
		int index = boost::lexical_cast<int>(_node.arg());
		if (index == 0) {
			m_out << "FIRST";
		} else if (index == 1) {
			m_out << "SECOND";
		} else if (index == 2) {
			m_out << "THIRD";
		} else if (index <= 15) {
			m_out << "INDEX " << index;
		} else {
			printPushInt(index);
			m_out << std::endl;
			tabs();
			m_out << "INDEXVAR";
		}
	} else if (_node.opcode() == "PUSHINT") {
		printPushInt(_node.arg(), _node.comment());
	} else {
		m_out << _node.fullOpcode();
	}
	m_out << std::endl;
	return false;
}

bool Printer::visit(PushCellOrSlice &_node) {
	tabs();
	switch (_node.type()) {
		case PushCellOrSlice::Type::PUSHREF_COMPUTE:
		case PushCellOrSlice::Type::PUSHREFSLICE_COMPUTE: {
			if (_node.type() == PushCellOrSlice::Type::PUSHREF_COMPUTE)
				m_out << "PUSHREF { " << std::endl;
			else
				m_out << "PUSHREFSLICE {" << std::endl;
			++m_tab;
			tabs();
			m_out << ".inline-computed-cell " << _node.blob() << ", 0" << std::endl;
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
			return false;
		}
		case PushCellOrSlice::Type::PUSHSLICE:
			m_out << "PUSHSLICE " << _node.blob() << std::endl;
			return false;
		case PushCellOrSlice::Type::PUSHREF:
			m_out << "PUSHREF {";
			break;
		case PushCellOrSlice::Type::PUSHREFSLICE:
			m_out << "PUSHREFSLICE {";
			break;
		case PushCellOrSlice::Type::CELL:
			m_out << ".cell {";
			break;
	}
	m_out << std::endl;

	++m_tab;
	if (!_node.blob().empty() && _node.blob() != "x") {
		tabs();
		m_out << ".blob " << _node.blob() << std::endl;
	}
	if (_node.child()) {
		_node.child()->accept(*this);
	}
	--m_tab;

	tabs();
	m_out << "}" << std::endl;
	return false;
}

bool Printer::visit(Glob &_node) {
	tabs();
	switch (_node.opcode()) {
		case Glob::Opcode::GetOrGetVar:
			if (1 <= _node.index() && _node.index() <= 31) {
				m_out << "GETGLOB " << _node.index();
			} else {
				printPushInt(_node.index());
				m_out << std::endl;
				tabs();
				m_out << "GETGLOBVAR";
			}
			break;
		case Glob::Opcode::SetOrSetVar:
			if (1 <= _node.index() && _node.index() <= 31) {
				m_out << "SETGLOB " << _node.index();
			} else {
				printPushInt(_node.index());
				m_out << std::endl;
				tabs();
				m_out << "SETGLOBVAR";
			}
			break;
		case Glob::Opcode::POPROOT:
			m_out << "POPROOT";
			break;
		case Glob::Opcode::PUSHROOT:
			m_out << "PUSHROOT";
			break;
		case Glob::Opcode::POP_C3:
			m_out << "POP C3";
			break;
		case Glob::Opcode::PUSH_C7:
			m_out << "PUSH C7";
			break;
		case Glob::Opcode::PUSH_C3:
			m_out << "PUSH C3";
			break;
		case Glob::Opcode::POP_C7:
			m_out << "POP C7";
			break;
	}
	m_out << std::endl;
	return false;
}

bool Printer::visit(Stack &_node) {
	tabs();
	int i = _node.i();
	int j = _node.j();
	int k = _node.k();
	auto printSS = [&](){
		m_out << " S" << i;
		if (j != -1) {
			m_out << ", S" << j;
			if (k != -1) {
				m_out << ", S" << k;
			}
		}
	};
	auto printIndexes = [&](){
		solAssert(i != -1, "");
		m_out << " " << i;
		if (j != -1) {
			m_out << ", " << j;
			solAssert(k==-1, "");
		}
	};

	auto drop = [&](int n){
		if (n == 1) {
			m_out << "DROP";
		} else if (n == 2) {
			m_out << "DROP2";
		} else if (n <= 15) {
			m_out << "BLKDROP";
			printIndexes();
		} else {
			printPushInt(n);
			m_out << std::endl;
			tabs();
			m_out << "DROPX";
		}
	};

	switch (_node.opcode()) {
	case Stack::Opcode::DROP: {
		drop(i);
		break;
	}
	case Stack::Opcode::PUSH_S:
		solAssert(j == -1, "");
		if (i == 0) {
			m_out << "DUP";
		} else if (i == 1) {
			m_out << "OVER";
		} else {
			m_out << "PUSH S" << i;
		}
		break;
	case Stack::Opcode::XCHG: {
		if (i == 0) {
			if (j == 1) {
				m_out << "SWAP";
			} else {
				m_out << "XCHG S" << j;
			}
		} else {
			m_out << "XCHG S" << i << ", S" << j;
		}
		break;
	}
	case Stack::Opcode::BLKDROP2:
		if (i > 15 || j > 15) {
			printPushInt(i);
			m_out << std::endl;
			tabs();
			printPushInt(j);
			m_out << std::endl;
			tabs();
			m_out << "BLKSWX" << std::endl;
			tabs();
			drop(i);
		} else {
			solAssert((i >= 2 && j >= 1) || (i >= 1 && j >= 2), "");
			m_out << "BLKDROP2";
			printIndexes();
		}
		break;
	case Stack::Opcode::PUSH2_S:
		if (i == 1 && j == 0)
			m_out << "DUP2";
		else if (i == 3 && j == 2)
			m_out << "OVER2";
		else {
			m_out << "PUSH2";
			printSS();
		}
		break;
	case Stack::Opcode::POP_S:
		if (i == 1) {
			m_out << "NIP";
		} else {
			m_out << "POP";
			printSS();
		}
		break;
	case Stack::Opcode::BLKSWAP: {
		int bottom = _node.i();
		int top = _node.j();
		if (bottom == 1 && top == 1) {
			m_out << "SWAP";
		} else if (bottom == 1 && top == 2) {
			m_out << "ROT";
		} else if (bottom == 2 && top == 1) {
			m_out << "ROTREV";
		} else if (bottom == 2 && top == 2) {
			m_out << "SWAP2";
		} else if (1 <= bottom && bottom <= 16 && 1 <= top && top <= 16) {
			if (bottom == 1) {
				m_out << "ROLL " << top;
			} else if (top == 1) {
				m_out << "ROLLREV " << bottom;
			} else {
				m_out << "BLKSWAP";
				printIndexes();
			}
		} else {
			if (bottom == 1) {
				printPushInt(top);
				m_out << std::endl;
				tabs();
				m_out << "ROLLX";
			} else if (top == 1) {
				printPushInt(bottom);
				m_out << std::endl;
				tabs();
				m_out << "ROLLREVX";
			} else {
				printPushInt(bottom);
				m_out << std::endl;
				tabs();
				printPushInt(top);
				m_out << std::endl;
				tabs();
				m_out << "BLKSWX";
			}
		}
		break;
	}
	case Stack::Opcode::REVERSE:
		solAssert(2 <= i, "");
		if (i == 2 && j == 0) {
			m_out << "SWAP";
		} else if (i == 3 && j == 0) {
			m_out << "XCHG S2";
		} else if (2 <= i && i <= 17 && 0 <= j && j <= 15) {
			m_out << "REVERSE";
			printIndexes();
		} else {
			printPushInt(i);
			m_out << std::endl;
			tabs();
			printPushInt(j);
			m_out << std::endl;
			tabs();
			m_out << "REVX";
		}
		break;
	case Stack::Opcode::BLKPUSH:
		if (i == 2 && j == 1) {
			m_out << "DUP2";
		} else if (i == 2 && j == 3) {
			m_out << "OVER2";
		} else {
			if (i > 15)
				solAssert(j == 0, "");
			int rest = i;
			bool first = true;
			while (rest > 0) {
				if (!first) {
					m_out << std::endl;
					tabs();
				}
				m_out << "BLKPUSH " << std::min(15, rest) << ", " << j;

				rest -= 15;
				first = false;
			}
		}
		break;
	case Stack::Opcode::PUSH3_S:
		m_out << "PUSH3";
		printSS();
		break;
	case Stack::Opcode::PUXC:
		m_out << "PUXC S" << i << ", S" << j;
		break;
	case Stack::Opcode::XCPU:
		if (i == 1 && j == 1)
			m_out << "TUCK";
		else
			m_out << "XCPU S" << i << ", S" << j;
		break;
	case Stack::Opcode::XC2PU:
		m_out << "XC2PU S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::XCHG2:
		m_out << "XCHG2 S" << i << ", S" << j;
		break;
	case Stack::Opcode::XCHG3:
		m_out << "XCHG3 S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::XCPU2:
		m_out << "XCPU2 S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::PUXC2:
		m_out << "PUXC2 S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::PUXCPU:
		m_out << "PUXCPU S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::XCPUXC:
		m_out << "XCPUXC S" << i << ", S" << j << ", S" << k;
		break;
	case Stack::Opcode::PU2XC:
		m_out << "PU2XC S" << i << ", S" << j << ", S" << k;
		break;
	}

	m_out << std::endl;
	return false;
}

bool Printer::visit(CodeBlock &_node) {
	switch (_node.type()) {
	case CodeBlock::Type::None:
		break;
	default:
		tabs();
		m_out << CodeBlock::toString(_node.type()) << " {" << std::endl;
		++m_tab;
		break;
	}

	for (Pointer<TvmAstNode> const& inst : _node.instructions()) {
		inst->accept(*this);
	}

	switch (_node.type()) {
		case CodeBlock::Type::None:
			break;
		default:
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
			break;
	}

	return false;
}

bool Printer::visit(SubProgram &_node) {
	switch (_node.block()->type()) {
	case CodeBlock::Type::None:
		solUnimplemented("");
	case CodeBlock::Type::PUSHCONT:
		_node.block()->accept(*this);

		tabs();
		if (_node.isJmp()) {
			m_out << "JMPX";
		} else {
			m_out << "CALLX";
		}
		m_out << std::endl;

		break;
	case CodeBlock::Type::PUSHREFCONT:
		tabs();
		if (_node.isJmp()) {
			m_out << "JMPREF {";
		} else {
			m_out << "CALLREF {";
		}
		m_out << std::endl;

		++m_tab;
		for (Pointer<TvmAstNode> const& i : _node.block()->instructions()) {
			i->accept(*this);
		}
		--m_tab;

		tabs();
		m_out << "}" << std::endl;
		break;
	}
	return false;
}

bool Printer::visit(LogCircuit &_node) {
	tabs();
	m_out << "PUSHCONT {" << std::endl;

	++m_tab;
	_node.body()->accept(*this);
	--m_tab;

	tabs();
	m_out << "}" << std::endl;

	tabs();
	switch (_node.type()) {
		case LogCircuit::Type::AND:
			m_out << "IF";
			break;
		case LogCircuit::Type::OR:
			m_out << "IFNOT";
			break;
	}
	m_out << std::endl;

	return false;
}

bool Printer::visit(TvmIfElse &_node) {
	if (_node.falseBody() == nullptr) {
	switch (_node.trueBody()->type()) {
		case CodeBlock::Type::None:
			solUnimplemented("");
			break;
		case CodeBlock::Type::PUSHCONT:
			_node.trueBody()->accept(*this);

			tabs();
			m_out << "IF";
			if (_node.withNot())
				m_out << "NOT";
			if (_node.withJmp())
				m_out << "JMP";
			m_out << std::endl;

			break;
		case CodeBlock::Type::PUSHREFCONT:
			tabs();
			m_out << "IF";
			if (_node.withNot())
				m_out << "NOT";
			if (_node.withJmp())
				m_out << "JMP";
			m_out << "REF {" << std::endl;

			++m_tab;
			for (Pointer<TvmAstNode> const& i : _node.trueBody()->instructions()) {
				i->accept(*this);
			}
			--m_tab;

			tabs();
			m_out << "}" << std::endl;
			break;
		}
	} else {
		if (_node.trueBody()->type() == CodeBlock::Type::PUSHREFCONT &&
			_node.falseBody()->type() == CodeBlock::Type::PUSHREFCONT
		) {
			tabs();
			m_out << "IFREFELSEREF" << std::endl;
			for (Pointer<CodeBlock> const& body : {_node.trueBody(), _node.falseBody()}) {
				tabs();
				m_out << "{" << std::endl;
				++m_tab;
				for (Pointer<TvmAstNode> const &n: body->instructions()) {
					n->accept(*this);
				}
				--m_tab;
				tabs();
				m_out << "}" << std::endl;
			}
		} else  if (_node.trueBody()->type() == CodeBlock::Type::PUSHREFCONT) {
			_node.falseBody()->accept(*this);
			tabs();
			m_out << "IFREFELSE {" << std::endl;
			++m_tab;
			for (Pointer<TvmAstNode> const& n : _node.trueBody()->instructions()) {
				n->accept(*this);
			}
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
		} else  if (_node.falseBody()->type() == CodeBlock::Type::PUSHREFCONT) {
			_node.trueBody()->accept(*this);
			tabs();
			m_out << "IFELSEREF {" << std::endl;
			++m_tab;
			for (Pointer<TvmAstNode> const& n : _node.falseBody()->instructions()) {
				n->accept(*this);
			}
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
		} else {
			_node.trueBody()->accept(*this);
			_node.falseBody()->accept(*this);
			if (_node.withNot())
				solUnimplemented("");

			tabs();
			m_out << "IFELSE" << std::endl;
		}
	}
	return false;
}

bool Printer::visit(TvmRepeat &_node) {
	_node.body()->accept(*this);
	tabs();
	if (_node.withBreakOrReturn()) {
		m_out << "REPEATBRK" << std::endl;
	} else {
		m_out << "REPEAT" << std::endl;
	}
	return false;
}

bool Printer::visit(TvmUntil &_node) {
	_node.body()->accept(*this);
	tabs();
	if (_node.withBreakOrReturn()) {
		m_out << "UNTILBRK" << std::endl;
	} else {
		m_out << "UNTIL" << std::endl;
	}
	return false;
}

bool Printer::visit(TryCatch &_node) {
	if (_node.saveAltC2()) {
		tabs();
		m_out << "SAVEALT C2" << std::endl;
	}
	_node.tryBody()->accept(*this);
	_node.catchBody()->accept(*this);
	tabs();
	m_out << "TRYKEEP" << std::endl;
	return false;
}

bool Printer::visit(While &_node) {
	if (!_node.isInfinite()) {
		_node.condition()->accept(*this);
	}
	_node.body()->accept(*this);
	tabs();
	if (_node.isInfinite()) {
		if (_node.withBreakOrReturn())
			m_out << "AGAINBRK" << std::endl;
		else
			m_out << "AGAIN" << std::endl;
	} else {
		if (_node.withBreakOrReturn())
			m_out << "WHILEBRK" << std::endl;
		else
			m_out << "WHILE" << std::endl;
	}
	return false;
}

bool Printer::visit(Contract &_node) {
	std::map<uint32_t, std::string> privFuncs = _node.privateFunctions();
	for (Pointer<Function> const& fun : _node.functions()) {
		if ((fun->type() == Function::FunctionType::Fragment && fun->functionId().has_value() && _node.saveAllFunction()) ||
			fun->type() == Function::FunctionType::OnCodeUpgrade
		) {
			std::string name = fun->name();
			uint32_t id = fun->functionId().value();
			if (privFuncs.count(id) == 0)
				privFuncs[id] = name;
			else
				solAssert(privFuncs[id] == name, "");
		}
	}
	bool hasOnTickTock = false;
	for (const Pointer<Function>& f : _node.functions()) {
		hasOnTickTock |= f->name() == "onTickTock";
		f->accept(*this);
	}

	if (!_node.isLib()) {
		if (!hasOnTickTock) {
			m_out << ".fragment onTickTock, {" << std::endl;
			m_out << "}" << std::endl;
			m_out << std::endl;
		}

		m_out << "; The code below forms a value of the StateInit type." << std::endl;
		m_out << ".blob x4_ ; split_depth = nothing" << std::endl;
		m_out << ".blob x4_ ; special = nothing" << std::endl;
		m_out << ".blob xc_ ; code = just" << std::endl;

		auto printCode = [&](){
			tabs(); m_out << ".cell { ; code cell" << std::endl;
			++m_tab;
			tabs(); m_out << "PUSHREFCONT {" << std::endl;
			tabs(); m_out << "	DICTPUSHCONST 32" << std::endl;
			tabs(); m_out << "	DICTUGETJMPZ" << std::endl;
			tabs(); m_out << "	THROW 78" << std::endl;

			tabs(); m_out << "	.code-dict-cell 32, {" << std::endl;
			m_tab += 2;
			for (auto const& [id, name] : privFuncs) {
				tabs(); m_out << "x" << std::setfill('0') << std::setw(8) << std::hex << id << " = " << name << "," << std::endl;
			}
			m_tab -= 2;
			tabs(); m_out << "	}" << std::endl;

			tabs(); m_out << "	.cell { ; version" << std::endl;
			tabs(); m_out << "		.blob x" << StrUtils::stringToHex(_node.version()) << " ; " << _node.version() << std::endl;
			tabs(); m_out << "	}" << std::endl;

			tabs(); m_out << "}" << std::endl;
			tabs(); m_out << "POPCTR c3" << std::endl;
			tabs(); m_out << "DUP" << std::endl;
			tabs(); m_out << "IFNOTJMPREF {" << std::endl;
			tabs(); m_out << "	.inline main_internal" << std::endl;
			tabs(); m_out << "}" << std::endl;
			tabs(); m_out << "DUP" << std::endl;
			tabs(); m_out << "EQINT -1" << std::endl;
			tabs(); m_out << "IFJMPREF {" << std::endl;
			tabs(); m_out << "	.inline main_external" << std::endl;
			tabs(); m_out << "}" << std::endl;
			tabs(); m_out << "DUP" << std::endl;
			tabs(); m_out << "EQINT -2" << std::endl;
			tabs(); m_out << "IFJMPREF {" << std::endl;
			tabs(); m_out << "	.inline onTickTock" << std::endl;
			tabs(); m_out << "}" << std::endl;
			tabs(); m_out << "THROW 11" << std::endl;
			--m_tab;
			tabs(); m_out << "}" << std::endl; // end code
		};
		if (_node.upgradeOldSolidity() || _node.upgradeFunc()) {
			int func_id = _node.upgradeFunc() ? 1666 : 2;
			m_out << ".cell { ; wrapper for code" << std::endl;
			++m_tab;
			tabs(); m_out << "PUSHINT " << func_id << std::endl;
			tabs(); m_out << "EQUAL" << std::endl;
			tabs(); m_out << "THROWIFNOT 79" << std::endl;
			tabs(); m_out << "PUSHREF" << std::endl;
			++m_tab;
			printCode();
			--m_tab;
			tabs(); m_out << "DUP" << std::endl;
			tabs(); m_out << "SETCODE" << std::endl;
			tabs(); m_out << "CTOS" << std::endl;
			tabs(); m_out << "PLDREF" << std::endl;
			tabs(); m_out << "CTOS" << std::endl;
			tabs(); m_out << "BLESS" << std::endl;
			tabs(); m_out << "POP C3" << std::endl;
			tabs(); m_out << "CALL 2" << std::endl;
			--m_tab;
			m_out << "}" << std::endl; // end code
		} else {
			printCode();
		}

		m_out << ".blob xc_ ; data = just" << std::endl;
		m_out << ".cell { " << std::endl;
		m_out << "	.inline-computed-cell default_data_cell, 0" << std::endl;
		m_out << "}" << std::endl;
		m_out << ".blob x4_ ; library = hme_empty" << std::endl;
	}
	return false;
}

bool Printer::visit(Function &_node) {
	std::string const& funName = _node.name();
	m_out << ".fragment " << funName << ", {" << std::endl;
	++m_tab;
	_node.block()->accept(*this);
	--m_tab;
	m_out << "}" << std::endl;
	m_out << std::endl;
	return false;
}

bool Printer::visitNode(TvmAstNode const&) {
	solUnimplemented("");
}

void Printer::tabs() {
	solAssert(m_tab >= 0, "");
	m_out << std::string(m_tab, '\t');
}

void Printer::printPushInt(std::string const& str, std::string const& comment) {
	std::map<bigint, int> const& power2Exp = MathConsts::power2Exp();
	std::map<bigint, int> const& power2DecExp = MathConsts::power2DecExp();
	std::map<bigint, int> const& power2NegExp = MathConsts::power2NegExp();

	bool didPrint = false;
	if (str.at(0) != '$') {
		bigint val = bigint{str};
		if (power2Exp.count(val) && power2Exp.at(val) >= 7) {
			m_out << "PUSHPOW2 " << power2Exp.at(val);
			didPrint = true;
		} else if (power2DecExp.count(val) && power2DecExp.at(val) >= 8) {
			m_out << "PUSHPOW2DEC " << power2DecExp.at(val);
			didPrint = true;
		} else if (power2NegExp.count(val) && power2NegExp.at(val) >= 8) {
			m_out << "PUSHNEGPOW2 " << power2NegExp.at(val);
			didPrint = true;
		}
	}
	if (!didPrint) {
		m_out << "PUSHINT " << str;
		if (!comment.empty()) {
			m_out << " " << comment;
		}
	}
}

void Printer::printPushInt(int i) {
	printPushInt(std::to_string(i));
}

bool LocSquasher::visit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> res0;

	{
		std::vector<Pointer<TvmAstNode>> a = _node.instructions();
		std::vector<Pointer<TvmAstNode>> b;
		if (!a.empty()) {
			b.push_back(a.front());
			for (size_t i = 1; i < a.size(); ++i) {
				if (!b.empty() && dynamic_cast<Loc const *>(b.back().get()) &&
					dynamic_cast<Loc const *>(a[i].get()))
					b.pop_back();
				b.push_back(a[i]);
			}
		}
		res0 = b;
	}


	std::vector<Pointer<TvmAstNode>> res;
	std::optional<Pointer<Loc>> lastLoc;
	for (const Pointer<TvmAstNode>& node : res0) {
		auto loc = std::dynamic_pointer_cast<Loc>(node);
		if (loc) {
			if (!lastLoc || std::make_pair(lastLoc.value()->file(), lastLoc.value()->line()) !=
							std::make_pair(loc->file(), loc->line())) {
				res.push_back(node);
			}
			//
			lastLoc = loc;
		} else {
			res.push_back(node);
		}
	}


	_node.upd(res);

	return true;
}

void DeleterAfterRet::endVisit(CodeBlock &_node) {
	bool didFind{};
	std::vector<Pointer<TvmAstNode>> newInstrs;
	for (Pointer<TvmAstNode> const& opcode : _node.instructions()) {
		auto ret = to<ReturnOrBreakOrCont>(opcode.get());
		auto ifElse = to<TvmIfElse>(opcode.get());
		bool ifElseWithJmp = ifElse && ifElse->falseBody() != nullptr && ifElse->withJmp();
		auto _throw = to<TvmException>(opcode.get());
		bool th = _throw && !_throw->withIf();
		if (!didFind && (ret || ifElseWithJmp || th)) {
			didFind = true;
			newInstrs.emplace_back(opcode);
		} else {
			if (!didFind || to<Loc>(opcode.get())) {
				newInstrs.emplace_back(opcode);
			}
		}
	}
	_node.upd(newInstrs);
}

bool DeleterCallX::visit(Function &_node) {
	Pointer<CodeBlock> const& block = _node.block();
	std::vector<Pointer<TvmAstNode>> const& inst = block->instructions();
	if (qtyWithoutLoc(inst) == 1) {
		std::vector<Pointer<TvmAstNode>> newCmds;
		for (Pointer<TvmAstNode> const& op : inst) {
			if (to<Loc>(op.get())) {
				newCmds.emplace_back(op);
			} else if (auto sub = to<SubProgram>(op.get()); sub) {
				newCmds.insert(newCmds.end(), sub->block()->instructions().begin(), sub->block()->instructions().end());
			} else {
				return false;
			}
		}
		block->upd(newCmds);
	}
	return false;
}

void LogCircuitExpander::endVisit(CodeBlock &_node) {
	std::vector<Pointer<TvmAstNode>> block;
	for (Pointer<TvmAstNode> const& opcode : _node.instructions()) {
		auto lc = to<LogCircuit>(opcode.get());
		if (lc) {
			m_stackSize = 1;
			m_newInst = {};
			bool isPure = true;
			std::vector<Pointer<TvmAstNode>> const &inst = lc->body()->instructions();
			for (size_t i = 0; i < inst.size(); ++i) {
				Pointer<TvmAstNode> op = inst.at(i);
				if (i == 0) {
					solAssert(isDrop(inst.at(i)).value() == 1, "");
					continue;
				}
				if (to<LogCircuit>(op.get()) && i + 1 != inst.size()) {
					isPure = false; // never happens
				}

				isPure &= isPureOperation(op);
			}
			if (isPure) {
				solAssert(m_stackSize == 2, "");
				Pointer<TvmAstNode> tail = m_newInst.back();
				bool hasTailLogCircuit = !m_newInst.empty() && to<LogCircuit>(m_newInst.back().get());
				if (hasTailLogCircuit) {
					if (to<LogCircuit>(m_newInst.back().get())->type() != lc->type()) {
						block.emplace_back(opcode);
						continue;
					}

					m_newInst.pop_back(); // DUP
					m_newInst.pop_back(); // LogCircuit
				}
				switch (lc->type()) {
					case LogCircuit::Type::AND:
						m_newInst.emplace_back(gen("AND"));
						break;
					case LogCircuit::Type::OR:
						m_newInst.emplace_back(gen("OR"));
						break;
				}
				if (hasTailLogCircuit) {
					m_newInst.emplace_back(makePUSH(0)); // DUP
					m_newInst.emplace_back(tail); // LogCircuit
				}

				block.pop_back(); // remove DUP opcode
				block.insert(block.end(), m_newInst.begin(), m_newInst.end());
				continue;
			}
		}
		block.emplace_back(opcode);
	}
	_node.upd(block);
}

bool LogCircuitExpander::isPureOperation(Pointer<TvmAstNode> const& op) {
	auto gen = to<Gen>(op.get());
	if (gen && gen->isPure()) {
		m_newInst.emplace_back(op);
		m_stackSize += -gen->take() + gen->ret();
		return true;
	}

	if (to<LogCircuit>(op.get())) {
		m_newInst.emplace_back(op);
		m_stackSize += -2 + 1;
		return true;
	}

	if (auto push = isPUSH(op)) {
		int index = *push;
		if (index + 1 < m_stackSize) {
			m_newInst.emplace_back(makePUSH(index));
		} else {
			m_newInst.emplace_back(makePUSH(index + 1));
		}
		++m_stackSize;
		return true;
	}

	return false;
}
