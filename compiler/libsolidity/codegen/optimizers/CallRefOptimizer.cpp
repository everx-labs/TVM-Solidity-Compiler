/*
 * Copyright (C) 2025-2026 EverX. All Rights Reserved.
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

#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <libsolidity/codegen/optimizers/CallRefOptimizer.hpp>


using namespace solidity::util;
using namespace solidity::frontend;

class CallRefToCallXOptimizerPrivate: public TvmAstVisitor {
public:
	bool visit(CodeBlock& _block) override;
	void inlineFunctionViaCallX(
		std::vector<Pointer<Function>>& functions,
		std::set<std::string> const& dictFunctions,
		bool saveAllFunction
	) const;

private:
	std::vector<std::pair<CodeBlock*, int>> m_blockAndQty;
};

bool CallRefToCallXOptimizerPrivate::visit(CodeBlock& _block) {
	if (_block.instructions().size() == 1 && is(_block.instructions().at(0), ".inline")) {
		bool isPushRef = _block.type() == CodeBlock::Type::PUSHREFCONT;
		bool has = false;
		for (auto& [block, qty]: m_blockAndQty) {
			if (block->operator==(_block)) {
				if (isPushRef) {
					++qty;
					block = &_block;
				}
				has = true;
				break;
			}
		}
		if (!has) {
			m_blockAndQty.emplace_back(&_block, isPushRef ? 1 : 0);
		}
	}
	return true;
}

void CallRefToCallXOptimizerPrivate::inlineFunctionViaCallX(
	std::vector<Pointer<Function>>& functions,
	std::set<std::string> const& dictFunctions,
	bool saveAllFunction
) const {
	for (auto const& [block, qty]: m_blockAndQty) {
		if (qty == 1) {
			std::string name = arg(block->instructions().at(0));
			for (size_t j = 0; j < functions.size(); j++) {
				if (functions[j]->name() == name) {
					block->changeType(CodeBlock::Type::PUSHCONT);

					if (dictFunctions.contains(name)) {
						std::vector<Pointer<TvmAstNode>> instructions;
						int take = functions[j]->take();
						int ret = functions[j]->ret();
						int id = ChainDataEncoder::toPrivateFunctionId(name);
						instructions.emplace_back(createNode<StackGen>("CALL " + toString(id), take, ret));
						block->changeInstructions(instructions);
					} else {
						block->changeType(CodeBlock::Type::PUSHCONT);
						block->changeInstructions(functions[j]->block()->instructions());
						if (!saveAllFunction) {
							functions.erase(functions.begin() + j);
						}
					}
				}
			}
		}
	}
}

void CallRefToCallXOptimizer::optimize(Pointer<Contract> const& contract) {
	if (contract->contractType() != Contract::ContractType::StdLibrary) {
		CallRefToCallXOptimizerPrivate callRefOptimizer;
		contract->accept(callRefOptimizer);
		std::vector<Pointer<Function>> functions = contract->functions();
		std::map<uint32_t, std::string> dictFunctions = contract->dictFunctions();
		std::set<std::string> dictFuncs;
		for (auto const& name: dictFunctions | std::views::values) {
			dictFuncs.emplace(name);
		}
		callRefOptimizer.inlineFunctionViaCallX(functions, dictFuncs, contract->saveAllFunction());
		contract->setFunctions(functions);
	}
}

class CodeSize: public TvmAstVisitor {
public:
	bool visit(AsymGen&) override {
		addDefaultSize();
		return false;
	}

	bool visit(DeclRetFlag&) override {
		addDefaultSize();
		return false;
	}

	bool visit(Opaque&) override { return true; }

	bool visit(HardCode&) override {
		setInf();
		return true;
	}

	bool visit(Loc&) override { return true; }

	bool visit(TvmReturn&) override {
		addDefaultSize();
		return false;
	}

	bool visit(ReturnOrBreakOrCont&) override { return true; }

	bool visit(TvmException&) override {
		addDefaultSize();
		return false;
	}

	bool visit(StackGen&) override {
		addDefaultSize();
		return false;
	}

	bool visit(CellOrSliceOperation& _node) override {
		switch (_node.type()) {
		case CellOrSliceOperation::Type::PUSHREF_COMPUTE:
		case CellOrSliceOperation::Type::PUSHREFSLICE_COMPUTE:
		case CellOrSliceOperation::Type::PUSHREF:
		case CellOrSliceOperation::Type::PUSHREFSLICE:
			addDefaultSize();
			break;
		case CellOrSliceOperation::Type::CELL:
			solUnimplemented("");
		case CellOrSliceOperation::Type::PUSHSLICE: {
			addDefaultSize();
			std::string bitString = StrUtils::toBitString(_node.blob());
			m_bits += bitString.size();
			break;
		}
		case CellOrSliceOperation::Type::STREFCONST:
			addDefaultSize();
			break;
		}
		return false;
	}

	bool visit(Glob&) override {
		addDefaultSize();
		return false;
	}

	bool visit(Stack&) override {
		addDefaultSize();
		return false;
	}

	bool visit(CodeBlock& _node) override {
		switch (_node.type()) {
		case CodeBlock::Type::None:
			return true;
		case CodeBlock::Type::PUSHCONT:
			addDefaultSize();
			return true;
		case CodeBlock::Type::PUSHREFCONT:
			addDefaultSize();
			return false;
		}
		solUnimplemented("");
	}

	bool visit(SubProgram& _node) override {
		switch (_node.block()->type()) {
		case CodeBlock::Type::None:
			return true;
		case CodeBlock::Type::PUSHCONT:
			addDefaultSize();
			return true;
		case CodeBlock::Type::PUSHREFCONT:
			addDefaultSize();
			return false;
		}
		solUnimplemented("");
	}

	bool visit(LogCircuit& _node) override {
		addDefaultSize(); // PUSHCONT
		addDefaultSize(); // IF
		_node.body()->accept(*this);
		return false;
	}

	bool visit(TvmIfElse&) override {
		addDefaultSize();
		return true;
	}

	bool visit(TvmRepeat&) override {
		addDefaultSize();
		return true;
	}

	bool visit(TvmUntil&) override {
		addDefaultSize();
		return true;
	}

	bool visit(While&) override {
		addDefaultSize();
		return true;
	}

	bool visit(Contract& _node) override { return visitNode(_node); }
	bool visit(Function& _node) override { return visitNode(_node); }

	int bits() const { return m_bits; }

protected:
	bool visitNode(TvmAstNode const&) override {
		m_bits = 100500;
		return false;
	}

private:
	void addDefaultSize() { m_bits += 16; }

	void setInf() { m_bits = 100500; }

private:
	int m_bits = 0;
};

class CallRefInlinerPrivate: public TvmAstVisitor {
public:
	explicit CallRefInlinerPrivate(Function* m_function):
		m_function(m_function) {}
	bool visit(CodeBlock& _block) override;
	int qty() const { return m_qty; }
	void changeTravelMode() { m_doReplace = true; }

private:
	Function* m_function;
	bool m_doReplace = false;
	int m_qty = 0;
};


bool CallRefInlinerPrivate::visit(CodeBlock& _block) {
	if (_block.type() == CodeBlock::Type::PUSHREFCONT &&
		_block.instructions().size() == 1 &&
		isInline(*_block.instructions().at(0), m_function->name())) {
		if (m_doReplace) {
			std::vector<Pointer<TvmAstNode>> const& instructions = m_function->block()->instructions();
			_block.changeType(CodeBlock::Type::PUSHCONT);
			_block.changeInstructions(instructions);
		} else {
			++m_qty;
		}
		return false;
	}

	std::vector<Pointer<TvmAstNode>> newInstructions;
	for (auto& inst: _block.instructions()) {
		bool isInlineFunction = isInline(*inst, m_function->name());

		if (m_doReplace) {
			if (isInlineFunction) {
				newInstructions += m_function->block()->instructions();
			} else {
				newInstructions.emplace_back(inst);
			}
		} else {
			if (isInlineFunction) {
				++m_qty;
			}
		}
	}

	if (m_doReplace) {
		_block.changeInstructions(newInstructions);
	}

	return true;
}

void CallRefInliner::optimize(Pointer<Contract> const& contract, std::vector<std::string> const& functionDag) {
	std::map<std::string, Function*> nameToFunc;
	for (auto const& func: contract->functions()) {
		solAssert(!nameToFunc.contains(func->name()), "");
		nameToFunc[func->name()] = func.get();
	}

	std::map<uint32_t, std::string> dictFunctions = contract->dictFunctions();
	std::set<std::string> dictFuncs;
	for (auto const& name: dictFunctions | std::views::values) {
		dictFuncs.emplace(name);
	}

	for (std::string const& functionName: functionDag) {
		if (nameToFunc.contains(functionName) && functionName != TvmConst::ON_BOUNCED_MESSAGE) {
			Function* func = nameToFunc.at(functionName);
			CallRefInlinerPrivate callRefInlinerPrivate{func};
			contract->accept(callRefInlinerPrivate);
			CodeSize codeSize;
			func->block()->accept(codeSize);
			int functionBitSize = codeSize.bits();
			int usedQty = callRefInlinerPrivate.qty();

			if (functionBitSize * usedQty <= TvmConst::CELL_PRICE_PS) {
				callRefInlinerPrivate.changeTravelMode();
				contract->accept(callRefInlinerPrivate);

				if (func->canBeDelete() &&
					!dictFuncs.contains(functionName) &&
					contract->contractType() != Contract::ContractType::StdLibrary) {
					std::vector<Pointer<Function>> functions = contract->functions();
					size_t deletedQty =
						std::erase_if(functions, [&](Pointer<Function> const& f) { return f->name() == functionName; });
					solAssert(deletedQty == 1, "");
					contract->setFunctions(functions);
				}
			}
		}
	}
}
