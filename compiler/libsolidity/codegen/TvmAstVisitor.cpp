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
 * Visitor for TVM Solidity abstract syntax tree.
 */

#include <ostream>
#include <memory>

#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <liblangutil/Exceptions.h>

using namespace solidity::frontend;

bool Printer::visit(AsymGen &_node) {
	tabs();
	m_out << _node.opcode() << std::endl;
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

bool Printer::visit(ConFlowInst &_node) {
	tabs();
	m_out << _node.fullOpcode();
	endL();
	return false;
}

bool Printer::visit(GenOpcode &_node) {
	tabs();
	if (_node.fullOpcode() == "BITNOT") m_out << "NOT";
	else if (_node.fullOpcode() == "TUPLE 1") m_out << "SINGLE";
	else if (_node.fullOpcode() == "TUPLE 2") m_out << "PAIR";
	else if (_node.fullOpcode() == "TUPLE 3") m_out << "TRIPLE";
	else if (_node.fullOpcode() == "UNTUPLE 1") m_out << "UNSINGLE";
	else if (_node.fullOpcode() == "UNTUPLE 2") m_out << "UNPAIR";
	else if (_node.fullOpcode() == "UNTUPLE 3") m_out << "UNTRIPLE";
	else
		m_out << _node.fullOpcode();
	m_out << std::endl;
	return false;
}

bool Printer::visit(PushCellOrSlice &_node) {
	tabs();
	switch (_node.type()) {
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
	endL();

	++m_tab;
	if (!_node.blob().empty()) {
		tabs();
		m_out << _node.blob() << std::endl;
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
			if (_node.index() <= 31) {
				m_out << "GETGLOB " << _node.index();
			} else {
				m_out << "PUSHINT " << _node.index() << std::endl;
				tabs();
				m_out << "GETGLOBVAR";
			}
			break;
		case Glob::Opcode::SetOrSetVar:
			if (_node.index() <= 31) {
				m_out << "SETGLOB " << _node.index();
			} else {
				m_out << "PUSHINT " << _node.index() << std::endl;
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
		case Glob::Opcode::PUSH_C3:
			m_out << "PUSH C3";
			break;
	}
	endL();
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
	switch (_node.opcode()) {
		case Stack::Opcode::DROP: {
			int n = i;
			if (n == 1) {
				m_out << "DROP";
			} else if (n == 2) {
				m_out << "DROP2";
			} else if (n <= 15) {
				m_out << "BLKDROP";
				printIndexes();
			} else {
				m_out << "PUSHINT " + std::to_string(n) << std::endl;
				tabs();
				m_out << "DROPX";
			}
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
		case Stack::Opcode::XCHG_S0: {
			if (i == 1) {
				m_out << "SWAP";
			} else {
				m_out << "XCHG";
				printSS();
			}
			break;
		}
		case Stack::Opcode::ROT:
			m_out << "ROT";
			break;
		case Stack::Opcode::ROTREV:
			m_out << "ROTREV";
			break;
		case Stack::Opcode::BLKDROP2:
			m_out << "BLKDROP2";
			printIndexes();
			break;
		case Stack::Opcode::PUSH2_S:
			m_out << "PUSH2";
			printSS();
			break;
		case Stack::Opcode::POP_S:
			if (i == 1) {
				m_out << "NIP";
			} else {
				m_out << "POP";
				printSS();
			}
			break;
		case Stack::Opcode::BLKSWAP:
			m_out << "BLKSWAP";
			printIndexes();
			break;
		case Stack::Opcode::REVERSE:
			m_out << "REVERSE";
			printIndexes();
			break;
		case Stack::Opcode::DUP2:
			m_out << "DUP2";
			break;
		case Stack::Opcode::BLKSWX:
			m_out << "BLKSWX";
			break;
		case Stack::Opcode::REVX:
			m_out << "REVX";
			break;
		case Stack::Opcode::SWAP2:
			m_out << "SWAP2";
			break;
		case Stack::Opcode::BLKPUSH:
			m_out << "BLKPUSH";
			printIndexes();
			break;
		case Stack::Opcode::PUSH3_S:
			m_out << "PUSH3";
			printSS();
			break;
		case Stack::Opcode::OVER2:
			m_out << "OVER2";
			break;
		case Stack::Opcode::XCHG_S_S:
			m_out << "XCHG";
			printSS();
			break;
	}
	endL();
	return false;
}

bool Printer::visit(CodeBlock &_node) {
	switch (_node.type()) {
		case CodeBlock::Type::None:
			break;
		case CodeBlock::Type::CALLX: // TODO split
			tabs();
			m_out << "PUSHCONT {" << std::endl;
			++m_tab;
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
		case CodeBlock::Type::CALLX:
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
			tabs();
			m_out << "CALLX" << std::endl;
			break;
		default:
			--m_tab;
			tabs();
			m_out << "}" << std::endl;
			break;
	}
	return false;
}

bool Printer::visit(TvmIfElse &_node) {
	_node.trueBody()->accept(*this);
	if (_node.falseBody()) {
		_node.falseBody()->accept(*this);
	}

	tabs();
	switch (_node.type()) {
		case TvmIfElse::Type::IFELSE:
			m_out << "IFELSE";
			break;
		case TvmIfElse::Type::IF:
			m_out << "IF";
			break;
		case TvmIfElse::Type::IFNOT:
			m_out << "IFNOT";
			break;
		case TvmIfElse::Type::IFNOTJMP:
			m_out << "IFNOTJMP";
			break;
		case TvmIfElse::Type::IFJMP:
			m_out << "IFJMP";
			break;
		case TvmIfElse::Type::IFELSE_WITH_JMP:
			m_out << "CONDSEL" << std::endl;
			tabs();
			m_out << "JMPX";
			break;

	}
	m_out << std::endl;

	return false;
}

bool Printer::visit(RepeatOrUntil &_node) {
	_node.body()->accept(*this);
	tabs();
	switch (_node.type()) {
		case RepeatOrUntil::Type::Repeat:
			m_out << "REPEAT";
			break;
		case RepeatOrUntil::Type::Until:
			m_out << "UNTIL";
			break;
	}
	m_out << std::endl;
	return false;
}

bool Printer::visit(While &_node) {
	_node.condition()->accept(*this);
	_node.body()->accept(*this);
	tabs();
	m_out << "WHILE" << std::endl;
	return false;
}

bool Printer::visit(Contract &_node) {
	for (const std::string& pragma : _node.pragmas()) {
		m_out << pragma << std::endl;
		m_out << std::endl;
	}
	for (const Pointer<Function>& f : _node.functions()){
		f->accept(*this);
	}
	return false;
}

bool Printer::visit(Function &_node) {
	switch (_node.type()) {
		case Function::FunctionType::PrivateFunction:
			m_out << ".globl\t" << _node.name() << std::endl;
			m_out << ".type\t" << _node.name() << ", @function" << std::endl;
			break;
		case Function::FunctionType::Macro:
			m_out << ".macro " << _node.name() << std::endl;
			break;
		case Function::FunctionType::MainInternal:
			solAssert(_node.name() == "main_internal", "");
			m_out << ".internal-alias :main_internal, 0" << std::endl
				<< ".internal :main_internal" << std::endl;
			break;
		case Function::FunctionType::MainExternal:
			solAssert(_node.name() == "main_external", "");
			m_out << ".internal-alias :main_external, -1" << std::endl
				  << ".internal :main_external" << std::endl;
			break;
		case Function::FunctionType::OnCodeUpgrade:
			solAssert(_node.name() == "onCodeUpgrade", "");
			m_out << ".internal-alias :onCodeUpgrade, 2" << std::endl
				  << ".internal :onCodeUpgrade" << std::endl;
			break;
		case Function::FunctionType::OnTickTock:
			solAssert(_node.name() == "onTickTock", "");
			m_out << ".internal-alias :onTickTock, -2" << std::endl
				  << ".internal :onTickTock" << std::endl;
			break;
	}
	_node.block()->accept(*this);
	endL();
	return false;
}

bool Printer::visitNode(TvmAstNode const&) {
	solUnimplemented("");
}

void Printer::endL() {
	m_out << std::endl;
}

void Printer::tabs() {
	solAssert(m_tab >= 0, "");
	m_out << std::string(m_tab, '\t');
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