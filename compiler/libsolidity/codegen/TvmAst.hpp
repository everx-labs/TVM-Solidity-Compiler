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

#pragma once

#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include <liblangutil/Exceptions.h>
#include <libsolidity/ast/AST.h>

#include <boost/noncopyable.hpp>

#include <libsolidity/codegen/helpers/utils.hpp>

template <class T>
using Pointer = std::shared_ptr<T>;

namespace solidity::frontend {
class TvmAstVisitor;

template <class NodeType, typename... Args>
Pointer<NodeType> createNode(Args&&... _args) {
	return std::make_shared<NodeType>(std::forward<Args>(_args)...);
}

class TvmAstNode: boost::noncopyable, public std::enable_shared_from_this<TvmAstNode> {
public:
	enum class Category {
		Loc,
		Stack,
		Glob,
		DeclRetFlag,
		Opaque,
		AsymGen,
		HardCode,
		StackGen,
		TvmReturn,
		ReturnOrBreakOrCont,
		TvmException,
		PushCellOrSlice,
		CodeBlock,
		SubProgram,
		LogCircuit,
		TvmIfElse,
		TvmRepeat,
		TvmUntil,
		While,
		TryCatch,
		Function,
		Contract
	};
	virtual ~TvmAstNode() = default;
	virtual void accept(TvmAstVisitor& _visitor) = 0;
	virtual bool operator==(TvmAstNode const& _node) const = 0;
	virtual Category category() const = 0;
};

class Loc: public TvmAstNode {
public:
	explicit Loc(std::string _file, int _line):
		m_file{std::move(_file)},
		m_line{_line} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const& node) const override;
	std::string const& file() const { return m_file; }
	int line() const { return m_line; }
	Category category() const override { return Category::Loc; }

private:
	std::string m_file;
	int m_line;
};

class Stack: public TvmAstNode {
public:
	enum class Opcode {
		DROP,
		BLKDROP2, // BLKDROP2 1, 1
		POP_S,	  // POP_S 1

		BLKPUSH, // BLKPUSH 1, i | BLKPUSH 3, i         |  BLKPUSH 2, 1 (DUP2)  |  BLKPUSH 3, 1 (OVER2)
		PUSH2_S, //              |                      |  PUSH2 S1, S0         |  PUSH2 S3, S2
		PUSH3_S, //              | PUSH3 Si, Si-1, Si-2 |                       |
		PUSH_S,	 // PUSH Si      |                      |                       |

		BLKSWAP, //  BLKSWAP 1, 1  |
		REVERSE, //  REVERSE 2, 0  |  REVERSE 3, 0
		XCHG,	 //  XCHG S0 S1    |  XCHG S0 S2,

		//  Compound stack manipulation primitives
		XCHG3,
		XCHG2,
		XCPU, // XCPU s1,s1 == TUCK
		PUXC,
		XC2PU,
		XCPU2,
		PUXC2,
		XCPUXC,
		PUXCPU,
		PU2XC
	};
	explicit Stack(Opcode opcode, int i = -1, int j = -1, int k = -1);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const& _node) const override;
	Opcode opcode() const { return m_opcode; }
	Category category() const override { return Category::Stack; }
	int i() const { return m_i; }
	int j() const { return m_j; }
	int k() const { return m_k; }

private:
	Opcode m_opcode{};
	int m_i{-1};
	int m_j{-1};
	int m_k{-1};
};

// abstract
class Gen: public TvmAstNode {
public:
	explicit Gen(bool _isPure):
		m_isPure{_isPure} {}
	virtual int take() const = 0;
	virtual int ret() const = 0;
	bool isPure() const { return m_isPure; }

private:
	bool m_isPure{}; // it doesn't throw exception, has no side effects (doesn't change any GLOB vars)
};

class Glob: public Gen {
public:
	enum class Opcode {
		GetOrGetVar,
		SetOrSetVar,

		PUSHROOT,
		POPROOT,

		PUSH_C3,
		POP_C3,

		PUSH_C7,
		POP_C7,
	};
	explicit Glob(Opcode opcode, int index = -1);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Opcode opcode() const { return m_opcode; }
	Category category() const override { return Category::Glob; }
	int index() const { return m_index; }
	int take() const override;
	int ret() const override;

private:
	Opcode m_opcode{};
	int m_index{-1};
};

class CodeBlock;

class DeclRetFlag: public TvmAstNode {
public:
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Category category() const override { return Category::DeclRetFlag; }
};

class Opaque: public Gen {
public:
	explicit Opaque(Pointer<CodeBlock> _block, int take, int ret, bool isPure):
		Gen{isPure},
		m_block(std::move(_block)),
		m_take{take},
		m_ret{ret} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Pointer<CodeBlock> const& block() const { return m_block; }
	int take() const override { return m_take; }
	int ret() const override { return m_ret; }
	Category category() const override { return Category::Opaque; }

private:
	Pointer<CodeBlock> m_block;
	int m_take{};
	int m_ret{};
};

class AsymGen: public TvmAstNode {
public:
	explicit AsymGen(std::string opcode);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	std::string const& opcode() const { return m_opcode; }
	Category category() const override { return Category::AsymGen; }

private:
	std::string m_opcode;
};

class HardCode: public Gen {
public:
	explicit HardCode(std::vector<std::string> code, int take, int ret, bool _isPure):
		Gen{_isPure},
		m_code(std::move(code)),
		m_take{take},
		m_ret{ret} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	std::vector<std::string> const& code() const { return m_code; }
	int take() const override { return m_take; }
	int ret() const override { return m_ret; }
	Category category() const override { return Category::HardCode; }

private:
	std::vector<std::string> m_code;
	int m_take{};
	int m_ret{};
};

class StackGen: public Gen {
public:
	explicit StackGen(std::string const& opcode, int take, int ret, bool _isPure = false);
	void accept(TvmAstVisitor& _visitor) override;
	std::string fullOpcode() const;
	std::string const& opcode() const { return m_opcode; }
	std::string const& arg() const { return m_arg; }
	std::string const& comment() const { return m_comment; }
	int take() const override { return m_take; }
	int ret() const override { return m_ret; }
	bool operator==(TvmAstNode const& _node) const override;
	Category category() const override { return Category::StackGen; }

private:
	std::string m_opcode;
	std::string m_arg;
	std::string m_comment;
	int m_take{};
	int m_ret{};
};

class TvmReturn: public TvmAstNode {
public:
	TvmReturn(bool _withIf, bool _withNot, bool _withAlt);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	bool withIf() const { return m_withIf; }
	bool withNot() const { return m_withNot; }
	bool withAlt() const { return m_withAlt; }
	Category category() const override { return Category::TvmReturn; }

private:
	bool m_withIf{};
	bool m_withNot{};
	bool m_withAlt{};
};

class ReturnOrBreakOrCont: public TvmAstNode {
public:
	enum class Type {
		Return,
		Continue,
		Break
	};
	explicit ReturnOrBreakOrCont(Type type, int _take, Pointer<CodeBlock> const& body):
		m_type{type},
		m_take{_take},
		m_body{body} {}
	Type type() const { return m_type; }
	Pointer<CodeBlock> const& body() const { return m_body; }
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	int take() const { return m_take; }
	Category category() const override { return Category::ReturnOrBreakOrCont; }

private:
	Type m_type{};
	int m_take{};
	Pointer<CodeBlock> m_body;
};

class TvmException: public Gen {
public:
	explicit TvmException(bool _arg, bool _any, bool _if, bool _not, std::string _param):
		Gen{false},
		m_arg{_arg},
		m_any{_any},
		m_if{_if},
		m_not{_not},
		m_param{std::move(_param)} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	std::string opcode() const;
	std::string const& arg() const { return m_param; }
	int take() const override;
	int ret() const override { return 0; }
	bool withArg() const { return m_arg; }
	bool withAny() const { return m_any; }
	bool withIf() const { return m_if; }
	bool withNot() const { return m_not; }
	Category category() const override { return Category::TvmException; }

private:
	bool m_arg{};
	bool m_any{};
	bool m_if{};
	bool m_not{};
	std::string m_param;
};

class CellOrSliceOperation: public Gen {
public:
	enum class Type {
		PUSHREF_COMPUTE,
		PUSHREFSLICE_COMPUTE,
		PUSHREF,
		PUSHREFSLICE,
		CELL,
		PUSHSLICE,
		STREFCONST
	};
	CellOrSliceOperation(Type type, std::string blob, Pointer<CellOrSliceOperation> child);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	bool operator<(TvmAstNode const&) const;
	Category category() const override { return Category::PushCellOrSlice; }
	int take() const override { return m_take; }
	int ret() const override { return m_ret; }
	Type type() const { return m_type; }
	std::string const& blob() const { return m_blob; }
	std::string chainBlob() const;
	Pointer<CellOrSliceOperation> child() const { return m_child; }
	void updToRef();

private:
	int m_take;
	int m_ret;
	Type m_type;
	std::string m_blob;
	Pointer<CellOrSliceOperation> m_child;
};

class CodeBlock: public TvmAstNode {
public:
	enum class Type {
		None,
		PUSHCONT,
		PUSHREFCONT,
	};
	static std::string toString(Type t);
	explicit CodeBlock(Type type, std::vector<Pointer<TvmAstNode>> instructions = {}):
		m_type{type},
		m_instructions(std::move(instructions)) {
		for (Pointer<TvmAstNode> const& i: m_instructions) {
			solAssert(i != nullptr, "");
		}
	}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Type type() const { return m_type; }
	std::vector<Pointer<TvmAstNode>> const& instructions() const { return m_instructions; }
	void changeInstructions(std::vector<Pointer<TvmAstNode>> instructions) { m_instructions = std::move(instructions); }
	void changeType(Type type) { m_type = type; }
	Category category() const override { return Category::CodeBlock; }

private:
	Type m_type;
	std::vector<Pointer<TvmAstNode>> m_instructions;
};

class SubProgram: public TvmAstNode {
public:
	SubProgram(bool _isJmp, Pointer<CodeBlock> const& _block);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Pointer<CodeBlock> const& block() const { return m_block; }
	bool isJmp() const { return m_isJmp; }
	Category category() const override { return Category::SubProgram; }

private:
	bool m_isJmp{};
	Pointer<CodeBlock> m_block;
};

// Take one value from stack and return one
class LogCircuit: public Gen {
public:
	enum class Type {
		AND,
		OR
	};
	LogCircuit(Type type, Pointer<CodeBlock> const& body):
		Gen{false},
		m_type{type},
		m_body{body} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	Type type() const { return m_type; }
	Pointer<CodeBlock> const& body() const { return m_body; }
	Category category() const override { return Category::LogCircuit; }
	int take() const override { return 2; }
	int ret() const override { return 1; }

private:
	Type m_type{};
	Pointer<CodeBlock> m_body;
};

class TvmIfElse: public TvmAstNode {
public:
	TvmIfElse(
		bool _withNot,
		bool _withJmp,
		Pointer<CodeBlock> const& trueBody,
		Pointer<CodeBlock> const& falseBody,
		int ret
	);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override;
	bool withNot() const { return m_withNot; }
	bool withJmp() const { return m_withJmp; }
	Pointer<CodeBlock> const& trueBody() const { return m_trueBody; }
	Pointer<CodeBlock> const& falseBody() const { return m_falseBody; }
	int ret() const { return m_ret; }
	Category category() const override { return Category::TvmIfElse; }

private:
	bool m_withNot{};
	bool m_withJmp{};
	Pointer<CodeBlock> m_trueBody;
	Pointer<CodeBlock> m_falseBody; // nullptr for if-statement
	int m_ret{};
};

class TvmRepeat: public TvmAstNode {
public:
	explicit TvmRepeat(bool _withBreakOrReturn, Pointer<CodeBlock> const& body):
		m_withBreakOrReturn{_withBreakOrReturn},
		m_body(body) {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	bool withBreakOrReturn() const { return m_withBreakOrReturn; }
	Pointer<CodeBlock> const& body() const { return m_body; }
	Category category() const override { return Category::TvmRepeat; }

private:
	bool m_withBreakOrReturn{};
	Pointer<CodeBlock> m_body;
};

class TvmUntil: public TvmAstNode {
public:
	explicit TvmUntil(bool _withBreakOrReturn, Pointer<CodeBlock> const& body):
		m_withBreakOrReturn{_withBreakOrReturn},
		m_body(body) {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	bool withBreakOrReturn() const { return m_withBreakOrReturn; }
	Pointer<CodeBlock> const& body() const { return m_body; }
	Category category() const override { return Category::TvmUntil; }

private:
	bool m_withBreakOrReturn{};
	Pointer<CodeBlock> m_body;
};

class While: public TvmAstNode {
public:
	While(bool _infinite, bool _withBreakOrReturn, Pointer<CodeBlock> const& condition, Pointer<CodeBlock> const& body):
		m_infinite{_infinite},
		m_withBreakOrReturn{_withBreakOrReturn},
		m_condition{condition},
		m_body(body) {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	Pointer<CodeBlock> const& condition() const { return m_condition; }
	Pointer<CodeBlock> const& body() const { return m_body; }
	bool isInfinite() const { return m_infinite; }
	bool withBreakOrReturn() const { return m_withBreakOrReturn; }
	Category category() const override { return Category::While; }

private:
	bool m_infinite{};
	bool m_withBreakOrReturn{};
	Pointer<CodeBlock> m_condition;
	Pointer<CodeBlock> m_body;
};

class TryCatch: public TvmAstNode {
public:
	TryCatch(Pointer<CodeBlock> _tryBody, Pointer<CodeBlock> _catchBody, bool _saveAltC2):
		m_tryBody{std::move(_tryBody)},
		m_catchBody{std::move(_catchBody)},
		m_saveAltC2{_saveAltC2} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	Pointer<CodeBlock> const& tryBody() const { return m_tryBody; }
	Pointer<CodeBlock> const& catchBody() const { return m_catchBody; }
	bool saveAltC2() const { return m_saveAltC2; }
	Category category() const override { return Category::TryCatch; }

private:
	Pointer<CodeBlock> m_tryBody;
	Pointer<CodeBlock> m_catchBody;
	bool m_saveAltC2{};
};

class Function: public TvmAstNode {
public:
	Function(
		int take,
		int ret,
		std::string name,
		std::optional<uint32_t> _functionId,
		Pointer<CodeBlock> block,
		FunctionDefinition const* _function,
		bool canBeDelete
	);
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	int take() const { return m_take; }
	int ret() const { return m_ret; }
	std::string const& name() const { return m_name; }
	std::optional<uint32_t> functionId() const { return m_functionId; }
	FunctionDefinition const* functionDefinition() const { return m_function; }
	Pointer<CodeBlock> const& block() const { return m_block; }
	Category category() const override { return Category::Function; }
	bool canBeDelete() const { return m_canBeDelete; }

private:
	int m_take{};
	int m_ret{};
	std::string m_name;
	std::optional<uint32_t> m_functionId{};
	Pointer<CodeBlock> m_block;
	FunctionDefinition const* m_function{};
	bool m_canBeDelete{};
};

class Contract: public TvmAstNode {
public:
	enum class ContractType {
		Contract,
		ContractLibrary,
		StdLibrary
	};

	Contract(
		ContractType _contractType,
		bool _saveAllFunction,
		bool upgradeOldSolidity,
		std::string _version,
		std::vector<Pointer<Function>> _functions,
		std::map<uint32_t, std::string> _privateFunctions,
		std::map<uint32_t, std::string> _getters
	):
		m_contractType{_contractType},
		m_saveAllFunction{_saveAllFunction},
		m_upgradeOldSolidity{upgradeOldSolidity},
		m_version{std::move(_version)},
		m_functions{std::move(_functions)},
		m_dictFunctions{std::move(_privateFunctions)},
		m_getters{std::move(_getters)} {}
	void accept(TvmAstVisitor& _visitor) override;
	bool operator==(TvmAstNode const&) const override { return false; } // TODO
	ContractType contractType() const { return m_contractType; }
	bool saveAllFunction() const { return m_saveAllFunction; }
	bool upgradeOldSolidity() const { return m_upgradeOldSolidity; }
	std::string const& version() const { return m_version; }
	std::vector<Pointer<Function>> const& functions() const { return m_functions; }
	void setFunctions(std::vector<Pointer<Function>> const& _functions) { m_functions = _functions; }
	std::map<uint32_t, std::string> const& dictFunctions() const { return m_dictFunctions; }
	std::map<uint32_t, std::string> const& getters() const { return m_getters; }
	Category category() const override { return Category::Contract; }

private:
	ContractType m_contractType;
	bool m_saveAllFunction{};
	bool m_upgradeOldSolidity{};
	std::string m_version;
	std::vector<Pointer<Function>> m_functions{};
	std::map<uint32_t, std::string> m_dictFunctions{};
	std::map<uint32_t, std::string> m_getters{};
};

Pointer<StackGen> gen(std::string const& cmd);
Pointer<CellOrSliceOperation> genPushSlice(std::string const& data);
Pointer<CellOrSliceOperation> makePushCellOrSlice(std::string const& hexStr, bool toSlice);
Pointer<Stack> makeDROP(int cnt = 1);
Pointer<Stack> makePOP(int i);
Pointer<Stack> makeBLKPUSH(int qty, int index);
Pointer<Stack> makePUSH(int i);
Pointer<Stack> makePUSH2(int i, int j);
Pointer<Stack> makePUSH3(int i, int j, int k);
Pointer<TvmReturn> makeRET();
Pointer<TvmReturn> makeRETALT();
Pointer<TvmReturn> makeIFRETALT();
Pointer<TvmReturn> makeIFRET();
Pointer<TvmReturn> makeIFNOTRET();
Pointer<TvmReturn> makeIFNOTRETALT();
Pointer<TvmException> makeTHROW(std::string const& cmd);
Pointer<Stack> makeXCH_S(int i);
Pointer<Stack> makeXCH_S_S(int i, int j);
Pointer<Glob> makeGetGlob(int i);
Pointer<Glob> makeSetGlob(int i);
Pointer<Stack> makeBLKDROP2(int droppedCount, int leftCount);
Pointer<CellOrSliceOperation> makePUSHREF(std::string const& data = "");
Pointer<Stack> makeREVERSE(int qty, int index);
Pointer<Stack> makeROT();
Pointer<Stack> makeROTREV();
Pointer<Stack> makeBLKSWAP(int down, int top);
Pointer<Stack> makePUXC(int i, int j);
Pointer<Stack> makeXCPU(int i, int j);
Pointer<TvmIfElse> flipIfElse(TvmIfElse const& node);

Stack const* convertToStack(TvmAstNode const* _node);
Gen const* convertToGen(TvmAstNode const* _node);
Loc const* convertToLoc(TvmAstNode const* node);
StackGen const* convertToStackGen(TvmAstNode const* node);
Glob const* convertToGlob(TvmAstNode const* node);
TvmReturn const* convertToTvmReturn(TvmAstNode const* node);
TvmException const* convertToTvmException(TvmAstNode const* node);
CellOrSliceOperation const* convertToPushCellOrSlice(TvmAstNode const* node);
SubProgram const* convertToSubProgram(TvmAstNode const* node);
CodeBlock const* convertToCodeBlock(TvmAstNode const* node);
TvmIfElse const* convertToTvmIfElse(TvmAstNode const* node);
Opaque const* convertToOpaque(TvmAstNode const* node);
ReturnOrBreakOrCont const* convertToReturnOrBreakOrCont(TvmAstNode const* node);
HardCode const* convertToHardCode(TvmAstNode const* node);
While const* convertToWhile(TvmAstNode const* node);
LogCircuit const* convertToLogCircuit(TvmAstNode const* node);
DeclRetFlag const* convertToDeclRetFlag(TvmAstNode const* node);
AsymGen const* convertToAsymGen(TvmAstNode const* node);

template <class... Args>
bool is(Pointer<TvmAstNode> const& node, Args&&... cmd) {
	if (auto g = convertToStackGen(node.get())) {
		return isIn(g->opcode(), std::forward<Args>(cmd)...);
	}
	return false;
}
std::string arg(Pointer<TvmAstNode> const& node);
bool isPureGen01(TvmAstNode const& node);
bool isInline(TvmAstNode const& node, std::string const& name);
bool isSWAP(Pointer<TvmAstNode> const& node);
std::optional<std::pair<int, int>> isBLKSWAP(Pointer<TvmAstNode> const& _node);
std::optional<int> isDrop(Pointer<TvmAstNode> const& node);
std::optional<std::pair<int, int>> isBLKDROP2(Pointer<TvmAstNode> const& node);
std::optional<std::pair<int, int>> isBLKDROP2OrDrop(Pointer<TvmAstNode> const& node);
bool isStack(Pointer<TvmAstNode> const& node, Stack::Opcode op);
std::optional<int> isPOP(Pointer<TvmAstNode> const& _node);
std::optional<int> isPUSH(Pointer<TvmAstNode> const& _node);
std::optional<std::pair<int, int>> isPUSH2(Pointer<TvmAstNode> const& _node);
std::optional<std::pair<int, int>> isBLKPUSH(Pointer<TvmAstNode> const& node);
bool isXCHG(Pointer<TvmAstNode> const& node, int i, int j);
std::optional<int> isXCHG_S0(Pointer<TvmAstNode> const& node);
std::optional<std::pair<int, int>> isREVERSE(Pointer<TvmAstNode> const& node);
CellOrSliceOperation const* isPlainPushSlice(Pointer<TvmAstNode> const& node); // TODO DELETE, support long slices

int getRootBitSize(CellOrSliceOperation const& _node);

Pointer<AsymGen> getZeroOrNullAlignment(bool isZero, bool isSwap, bool isNot);

namespace OpcodeUtils {
int gasCost(Stack const& opcode);
}

} // end solidity::frontend
