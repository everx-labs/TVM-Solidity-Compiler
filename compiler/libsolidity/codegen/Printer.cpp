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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#include <memory>
#include <ostream>

#include <liblangutil/Exceptions.h>
#include <libsolidity/codegen/Printer.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>

using namespace solidity::frontend;

bool Printer::visit(AsymGen& _node) {
	tabs();
	m_out << _node.opcode() << std::endl;
	return false;
}

bool Printer::visit(DeclRetFlag& /*_node*/) {
	tabs();
	m_out << "FALSE ; decl return flag" << std::endl;
	return false;
}

bool Printer::visit(Opaque& _node) {
	_node.block()->accept(*this);
	return false;
}

bool Printer::visit(HardCode& _node) {
	for (std::string const& s: _node.code()) {
		tabs();
		m_out << s << std::endl;
	}
	return false;
}

bool Printer::visit(Loc& _node) {
	tabs();
	m_out << ".loc " << _node.file() << ", " << _node.line() << std::endl;
	return false;
}

bool Printer::visit(TvmReturn& _node) {
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

bool Printer::visit(ReturnOrBreakOrCont& _node) {
	tabs();
	m_out << "; start return" << std::endl;
	_node.body()->accept(*this);
	tabs();
	m_out << "; end return" << std::endl;
	return false;
}

bool Printer::visit(TvmException& _node) {
	tabs();
	m_out << _node.opcode();
	if (!_node.arg().empty())
		m_out << " " << _node.arg();
	m_out << std::endl;
	return false;
}

bool Printer::visit(StackGen& _node) {
	tabs();
	if (_node.fullOpcode() == "BITNOT")
		m_out << "NOT";
	else if (_node.fullOpcode() == "QBITNOT")
		m_out << "QNOT";
	else if (_node.fullOpcode() == "PLDREFIDX 0")
		m_out << "PLDREF";
	else if (_node.fullOpcode() == "STVARUINT16")
		m_out << "STGRAMS";
	else if (_node.fullOpcode() == "LDVARUINT16")
		m_out << "LDGRAMS";
	else if (_node.fullOpcode() == "TUPLE 1")
		m_out << "SINGLE";
	else if (_node.fullOpcode() == "TUPLE 2")
		m_out << "PAIR";
	else if (_node.fullOpcode() == "TUPLE 3")
		m_out << "TRIPLE";
	else if (_node.fullOpcode() == "UNTUPLE 1")
		m_out << "UNSINGLE";
	else if (_node.fullOpcode() == "UNTUPLE 2")
		m_out << "UNPAIR";
	else if (_node.fullOpcode() == "UNTUPLE 3")
		m_out << "UNTRIPLE";
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
	} else if (_node.fullOpcode() == "STSLICECONST x4_")
		m_out << "STZERO";
	else if (_node.fullOpcode() == "STSLICECONST xc_")
		m_out << "STONE";
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

bool Printer::visit(CellOrSliceOperation& _node) {
	tabs();
	switch (_node.type()) {
	case CellOrSliceOperation::Type::PUSHREF_COMPUTE:
	case CellOrSliceOperation::Type::PUSHREFSLICE_COMPUTE: {
		if (_node.type() == CellOrSliceOperation::Type::PUSHREF_COMPUTE)
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
	case CellOrSliceOperation::Type::PUSHSLICE:
		m_out << "PUSHSLICE " << _node.blob() << std::endl;
		return false;
	case CellOrSliceOperation::Type::PUSHREF:
		m_out << "PUSHREF {";
		break;
	case CellOrSliceOperation::Type::PUSHREFSLICE:
		m_out << "PUSHREFSLICE {";
		break;
	case CellOrSliceOperation::Type::CELL:
		m_out << ".cell {";
		break;
	case CellOrSliceOperation::Type::STREFCONST:
		m_out << "STREFCONST {";
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

bool Printer::visit(Glob& _node) {
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

bool Printer::visit(Stack& _node) {
	tabs();
	int i = _node.i();
	int j = _node.j();
	int k = _node.k();
	auto printSS = [&] {
		m_out << " S" << i;
		if (j != -1) {
			m_out << ", S" << j;
			if (k != -1) {
				m_out << ", S" << k;
			}
		}
	};
	auto printIndexes = [&] {
		solAssert(i != -1, "");
		m_out << " " << i;
		if (j != -1) {
			m_out << ", " << j;
			solAssert(k == -1, "");
		}
	};

	auto drop = [&](int n) {
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
		if (i > 15 && j > 15) {
			printPushInt(i);
			m_out << std::endl;
			tabs();
			printPushInt(j);
			m_out << std::endl;
			tabs();
			m_out << "BLKSWX" << std::endl;
			tabs();
			drop(i);
		} else if (i > 15 || j <= 15) {
			bool isFirst = true;
			while (i > 0) {
				if (!isFirst)
					tabs();
				m_out << "BLKDROP2 " << std::min(15, i) << ", " << j;
				i -= 15;
				if (i > 0)
					m_out << std::endl;
				isFirst = false;
			}
		} else {
			// solAssert((i >= 2 && j >= 1) || (i >= 1 && j >= 2), "");
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

bool Printer::visit(CodeBlock& _node) {
	switch (_node.type()) {
	case CodeBlock::Type::None:
		break;
	default:
		tabs();
		m_out << CodeBlock::toString(_node.type()) << " {" << std::endl;
		++m_tab;
		break;
	}

	for (Pointer<TvmAstNode> const& inst: _node.instructions()) {
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

bool Printer::visit(SubProgram& _node) {
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
		for (Pointer<TvmAstNode> const& i: _node.block()->instructions()) {
			i->accept(*this);
		}
		--m_tab;

		tabs();
		m_out << "}";
		break;
	}
	m_out << std::endl;
	return false;
}

bool Printer::visit(LogCircuit& _node) {
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

bool Printer::visit(TvmIfElse& _node) {
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
			for (Pointer<TvmAstNode> const& i: _node.trueBody()->instructions()) {
				i->accept(*this);
			}
			--m_tab;

			tabs();
			m_out << "}" << std::endl;
			break;
		}
	} else {
		if (_node.trueBody()->type() == CodeBlock::Type::PUSHREFCONT &&
			_node.falseBody()->type() == CodeBlock::Type::PUSHREFCONT) {
			tabs();
			m_out << "IFREFELSEREF" << std::endl;
			for (Pointer<CodeBlock> const& body: {_node.trueBody(), _node.falseBody()}) {
				tabs();
				m_out << "{" << std::endl;
				++m_tab;
				for (Pointer<TvmAstNode> const& n: body->instructions()) {
					n->accept(*this);
				}
				--m_tab;
				tabs();
				m_out << "}" << std::endl;
			}
		} else if (_node.trueBody()->type() == CodeBlock::Type::PUSHREFCONT) {
			_node.falseBody()->accept(*this);
			tabs();
			m_out << "IFREFELSE {" << std::endl;
			++m_tab;
			for (Pointer<TvmAstNode> const& n: _node.trueBody()->instructions()) {
				n->accept(*this);
			}
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
		} else if (_node.falseBody()->type() == CodeBlock::Type::PUSHREFCONT) {
			_node.trueBody()->accept(*this);
			tabs();
			m_out << "IFELSEREF {" << std::endl;
			++m_tab;
			for (Pointer<TvmAstNode> const& n: _node.falseBody()->instructions()) {
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

bool Printer::visit(TvmRepeat& _node) {
	_node.body()->accept(*this);
	tabs();
	if (_node.withBreakOrReturn()) {
		m_out << "REPEATBRK" << std::endl;
	} else {
		m_out << "REPEAT" << std::endl;
	}
	return false;
}

bool Printer::visit(TvmUntil& _node) {
	_node.body()->accept(*this);
	tabs();
	if (_node.withBreakOrReturn()) {
		m_out << "UNTILBRK" << std::endl;
	} else {
		m_out << "UNTIL" << std::endl;
	}
	return false;
}

bool Printer::visit(While& _node) {
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

bool Printer::visit(Contract& _node) {
	if (m_func_name_to_debug.has_value()) {
		return true;
	}

	std::map<uint32_t, std::string> dictFunctions = _node.dictFunctions();
	for (Pointer<Function> const& fun: _node.functions()) {
		if (fun->functionDefinition() && fun->functionId().has_value() && _node.saveAllFunction()) {
			std::string name = fun->name();
			uint32_t id = fun->functionId().value();
			if (!dictFunctions.contains(id))
				dictFunctions[id] = name;
			else
				solAssert(dictFunctions[id] == name, "");
		}
	}
	bool hasOnTickTock = false;
	bool hasMainExternal = false;
	for (Pointer<Function> const& f: _node.functions()) {
		hasOnTickTock |= f->name() == "onTickTock";
		hasMainExternal |= f->name() == "main_external";
		f->accept(*this);
	}

	switch (_node.contractType()) {
	case Contract::ContractType::Contract: {
		m_out << "; The code below forms a value of the StateInit type." << std::endl;
		m_out << ".blob x4_ ; split_depth = nothing" << std::endl;
		m_out << ".blob x4_ ; special = nothing" << std::endl;
		m_out << ".blob xc_ ; code = just" << std::endl;

		auto printCode = [&] {
			tabs();
			m_out << "SETCP0" << std::endl;
			tabs();
			m_out << "DICTPUSHCONST 19, {" << std::endl;
			++m_tab;

			auto addToDict = [&](int id, std::string const& name) {
				auto binStr = StrUtils::toBitString(id, 19, true).value();
				auto const slice = StrUtils::binaryStringToSlice(binStr);
				tabs();
				m_out << "x" << slice << " = " << name << "," << std::endl;
			};

			addToDict(0, "main_internal");
			if (hasMainExternal)
				addToDict(-1, "main_external");
			if (hasOnTickTock)
				addToDict(-2, "onTickTock");
			for (auto const& [id, name]: _node.getters())
				addToDict(id, name);
			for (auto const& [id, name]: dictFunctions)
				addToDict(id, name);

			--m_tab;
			tabs();
			m_out << "}" << std::endl; // end DICTPUSHCONST 19, {

			tabs();
			m_out << "DICTIGETJMPZ" << std::endl;
			tabs();
			m_out << "THROW " + util::toString(TvmConst::RuntimeException::NoFunctionInTopSelector) << std::endl;
		};

		if (_node.upgradeOldSolidity()) {
			tabs();
			m_out << ".cell { ; wrapper for code" << std::endl;
			++m_tab;
			tabs();
			m_out << ".cell { ; wrapper for code" << std::endl;
			++m_tab;
			tabs();
			m_out << "MODPOW2 14" << std::endl;
			tabs();
			m_out << "PUSHREF {" << std::endl;
			++m_tab;
			printCode();
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
			tabs();
			m_out << "DUP" << std::endl;
			tabs();
			m_out << "SETCODE" << std::endl;
			tabs();
			m_out << "CTOS" << std::endl;
			tabs();
			m_out << "BLESS" << std::endl;
			tabs();
			m_out << "DUP" << std::endl;
			tabs();
			m_out << "POP C3" << std::endl;
			tabs();
			m_out << "CALLX" << std::endl;
			--m_tab;
			tabs();
			m_out << "}" << std::endl; // end code
			--m_tab;
			tabs();
			m_out << "}" << std::endl; // end code
		} else {
			tabs();
			m_out << ".cell { ; code cell" << std::endl;
			++m_tab;
			printCode();
			--m_tab;
			tabs();
			m_out << "}" << std::endl; // end code
		}

		m_out << ".blob xc_ ; data = just" << std::endl;
		m_out << ".cell { " << std::endl;
		m_out << "	.inline-computed-cell default_data_cell, 0" << std::endl;
		m_out << "}" << std::endl;
		m_out << ".blob x4_ ; library = hme_empty" << std::endl;
		break;
	}
	case Contract::ContractType::ContractLibrary: {
		tabs();
		m_out << "SETCP0" << std::endl;
		tabs();
		m_out << "DICTPUSHCONST 19, {" << std::endl;
		++m_tab;
		for (Pointer<Function> const& fun: _node.functions()) {
			FunctionDefinition const* def = fun->functionDefinition();
			if (def != nullptr && def->isPublic() && !def->isExternalMsg()) {
				auto binStr = StrUtils::toBitString(fun->functionId().value(), 19, false).value();
				auto const slice = StrUtils::binaryStringToSlice(binStr);
				tabs();
				m_out << "x" + slice + " = " + fun->name() + "," << std::endl;
			}
		}
		--m_tab;
		tabs();
		m_out << "}" << std::endl; // end DICTPUSHCONST 19, {
		tabs();
		m_out << "DICTUGETJMP" << std::endl;
		tabs();
		m_out << "THROW " + util::toString(TvmConst::RuntimeException::NoFunctionInContractLibrary) << std::endl;
		break;
	}
	case Contract::ContractType::StdLibrary: {
		// Do nothing
		break;
	}
	}

	return false;
}

bool Printer::visit(Function& _node) {
	if (!m_func_name_to_debug.has_value() || m_func_name_to_debug.value() == _node.name()) {
		std::string const& funName = _node.name();
		m_out << ".fragment " << funName << ", {" << std::endl;
		++m_tab;
		_node.block()->accept(*this);
		--m_tab;
		m_out << "}" << std::endl;
		m_out << std::endl;
	}
	return false;
}

bool Printer::visitNode(TvmAstNode const&) { solUnimplemented(""); }

void Printer::tabs() const {
	solAssert(m_tab >= 0, "");
	m_out << std::string(m_tab, '\t');
}

void Printer::printPushInt(std::string const& str, std::string const& comment) const {
	std::map<bigint, int> const& power2Exp = MathConsts::power2Exp();
	std::map<bigint, int> const& power2DecExp = MathConsts::power2DecExp();
	std::map<bigint, int> const& power2NegExp = MathConsts::power2NegExp();

	bool didPrint = false;
	if (str.at(0) != '$') {
		bigint val = bigint{str};
		if (power2Exp.contains(val) && power2Exp.at(val) >= 7) {
			m_out << "PUSHPOW2 " << power2Exp.at(val);
			didPrint = true;
		} else if (power2DecExp.contains(val) && power2DecExp.at(val) >= 8) {
			m_out << "PUSHPOW2DEC " << power2DecExp.at(val);
			didPrint = true;
		} else if (power2NegExp.contains(val) && power2NegExp.at(val) >= 8) {
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

void Printer::printPushInt(int i) const { printPushInt(std::to_string(i)); }
