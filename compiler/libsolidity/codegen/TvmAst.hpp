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

#pragma once

#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include <liblangutil/Exceptions.h>

#include <boost/noncopyable.hpp>

template <class T>
using Pointer = std::shared_ptr<T>;

namespace solidity::frontend
{
	class TvmAstVisitor;

	template <class NodeType, typename... Args>
	Pointer<NodeType> createNode(Args&& ... _args)
	{
		return std::make_shared<NodeType>(std::forward<Args>(_args)...);
	}

	class TvmAstNode : private boost::noncopyable {
	public:
		virtual ~TvmAstNode() {}
		virtual void accept(TvmAstVisitor& _visitor) = 0;
	};

	class Inst : public TvmAstNode {
	};

	class Loc : public Inst {
	public:
		explicit Loc(const std::string& _file, int _line) : m_file{_file}, m_line{_line} { }
		void accept(TvmAstVisitor& _visitor) override;
		std::string const& file() const { return m_file; }
		int line() const { return m_line; }
	private:
		std::string m_file;
		int m_line;
	};

	class Stack : public Inst {
	public:
		enum class Opcode {
			DROP,
			BLKDROP2,

			POP_S,

			PUSH2_S,
			PUSH3_S,
			PUSH_S,

			BLKPUSH,
			DUP2,
			OVER2,

			BLKSWAP,
			BLKSWX,
			SWAP2,

			ROT,
			ROTREV,

			REVERSE,
			REVX,

			XCHG_S0,
			XCHG_S_S,
		};
		explicit Stack(Opcode opcode, int i = -1, int j = -1, int k = -1);
		void accept(TvmAstVisitor& _visitor) override;
		Opcode opcode() const { return m_opcode; }
		int i() const { return m_i; }
		int j() const { return m_j; }
		int k() const { return m_k; }
	private:
		Opcode m_opcode{};
		int m_i{-1};
		int m_j{-1};
		int m_k{-1};
	};

	class Glob : public Inst {
	public:
		enum class Opcode {
			GetOrGetVar,
			SetOrSetVar,

			PUSHROOT,
			POPROOT,
			POP_C3,
			PUSH_C3
		};
		explicit Glob(Opcode opcode, int index = -1) : m_opcode{opcode}, m_index{index} {}
		void accept(TvmAstVisitor& _visitor) override;
		Opcode opcode() const { return m_opcode; }
		int index() const { return m_index; }
	private:
		Opcode m_opcode{};
		int m_index{-1};
	};

	// abstract
	class Gen : public Inst {
	public:
		explicit Gen(bool _isPure) : m_isPure{_isPure} {}
		virtual int take() const = 0;
		virtual int ret() const = 0;
		bool isPure() const { return m_isPure; }
	private:
		bool m_isPure{}; // it doesn't throw exception, have no side effects
	};

	class CodeBlock;

	class Opaque : public Gen {
	public:
		// TODO set for all Opaque correct pure
		explicit Opaque(Pointer<CodeBlock> _block, int take, int ret, bool isPure) :
			Gen{isPure}, m_block(std::move(_block)), m_take{take}, m_ret{ret} {}
		void accept(TvmAstVisitor& _visitor) override;
		Pointer<CodeBlock> const& block() const { return m_block; }
		int take() const override { return m_take; }
		int ret() const override { return m_ret; }
	private:
		Pointer<CodeBlock> m_block;
		int m_take{};
		int m_ret{};
	};

	class AsymGen : public Inst {
	public:
		explicit AsymGen(std::string opcode, int take, int retMin, int retMax) :
				m_opcode(std::move(opcode)), m_take{take}, m_retMin{retMin}, m_retMax{retMax} {}
		void accept(TvmAstVisitor& _visitor) override;
		std::string const& opcode() const { return m_opcode; }
		int take() const { return m_take; }
		int retMin() const { return m_retMin; }
		int retMax() const { return m_retMax; }
	private:
		std::string m_opcode;
		int m_take{};
		int m_retMin{};
		int m_retMax{};
	};

	class HardCode : public Gen {
	public:
		explicit HardCode(std::vector<std::string> code, int take, int ret) :
			Gen{false}, m_code(std::move(code)), m_take{take}, m_ret{ret} {}
		void accept(TvmAstVisitor& _visitor) override;
		std::vector<std::string> const& code() const { return m_code; }
		int take() const override { return m_take; }
		int ret() const override { return m_ret; }
	private:
		std::vector<std::string> m_code;
		int m_take{};
		int m_ret{};
	};

	class GenOpcode : public Gen {
	public:
		explicit GenOpcode(std::string opcode, int take, int ret, bool _isPure = false);
		void accept(TvmAstVisitor& _visitor) override;
		std::string fullOpcode() const;
		std::string const &opcode() const { return m_opcode; }
		std::string const &arg() const { return m_arg; }
		std::string const &comment() const { return m_comment; }
		int take() const override { return m_take; }
		int ret() const override { return m_ret; }
	private:
		std::string m_opcode;
		std::string m_arg;
		std::string m_comment;
		int m_take;
		int m_ret;
	};

	class ConFlowInst : public Inst {
	public:
		explicit ConFlowInst(std::string const _opcode, int _take, int _ret) :
			m_gen{_opcode, _take, _ret} {}
		void accept(TvmAstVisitor& _visitor) override;
		std::string const& opcode() const { return m_gen.opcode(); }
		std::string const& arg() const { return m_gen.arg(); }
		std::string fullOpcode() const { return m_gen.fullOpcode(); }
		int take() const { return m_gen.take(); }
		int ret() const { return m_gen.ret(); }
	private:
		GenOpcode m_gen;
	};

	class PushCellOrSlice : public Gen {
	public:
		enum class Type {
			PUSHREF,
			PUSHREFSLICE,
			CELL
		};
		PushCellOrSlice(Type type, std::string blob, Pointer<PushCellOrSlice> child) :
			Gen{true}, // we don't execute data
			m_type{type},
			m_blob{std::move(blob)},
			m_child{std::move(child)}
		{
		}
		void accept(TvmAstVisitor& _visitor) override;
		int take() const override { return 0; }
		int ret() const override { return 1; }
		Type type() const  { return m_type; }
		std::string const &blob() const { return m_blob; }
		Pointer<PushCellOrSlice> child() const { return m_child; };
	private:
		Type m_type;
		std::string m_blob;
		Pointer<PushCellOrSlice> m_child;
	};

	class CodeBlock : public Inst {
	public:
		enum class Type {
			None, // e.g. body of function without any PUSHCONT {}, CALLREF{} ect

			PUSHCONT,
			PUSHREF,
			PUSHREFCONT,

			// TODO split ?
			CALLREF,
			IFJMPREF,
			IFNOTJMPREF,
			IFNOTREF,
			IFREF,
			CALLX,
		};
		static std::string toString(Type t);
		CodeBlock(Type type, std::vector<Pointer<TvmAstNode>> instructions) :
			m_type{type}, m_instructions(std::move(instructions)) {
			for (const Pointer<TvmAstNode>& i : m_instructions) {
				solAssert(i != nullptr, "");
			}
		}
		void accept(TvmAstVisitor& _visitor) override;
		Type type() const { return m_type; }
		const std::vector<Pointer<TvmAstNode>> &instructions() const { return m_instructions; }
		void upd(std::vector<Pointer<TvmAstNode>> instructions) { m_instructions = instructions; }
	private:
		Type m_type;
		std::vector<Pointer<TvmAstNode>> m_instructions;
	};

	class TvmIfElse : public TvmAstNode {
	public:
		enum class Type {
			IF,
			IFNOT,

			IFJMP,
			IFNOTJMP,

			IFELSE,
			IFELSE_WITH_JMP,
		};
		TvmIfElse(Type type, Pointer<CodeBlock> const &mTrueBody, Pointer<CodeBlock> const &mFalseBody = nullptr) :
			m_type{type},
			m_trueBody(mTrueBody),
			m_falseBody(mFalseBody)
		{
		}
		void accept(TvmAstVisitor& _visitor) override;
		Type type() const { return m_type; }
		Pointer<CodeBlock> const& trueBody() const { return m_trueBody; }
		Pointer<CodeBlock> const& falseBody() const { return m_falseBody; }
	private:
		Type m_type;
		Pointer<CodeBlock> m_trueBody;
		Pointer<CodeBlock> m_falseBody; // nullptr for if-statement
	};

	class RepeatOrUntil : public TvmAstNode {
	public:
		enum class Type {
			Repeat,
			Until
		};
		RepeatOrUntil(Type type, Pointer<CodeBlock> const &body) : m_type{type}, m_body(body) { }
		void accept(TvmAstVisitor& _visitor) override;
		Type type() const {return m_type; }
		Pointer<CodeBlock> const& body() const { return m_body; }
	private:
		Type m_type;
		Pointer<CodeBlock> m_body;
	};

	class While : public TvmAstNode {
	public:
		While(Pointer<CodeBlock> const &condition, Pointer<CodeBlock> const &body) :
			m_condition{condition},
			m_body(body) { }
		void accept(TvmAstVisitor& _visitor) override;
		Pointer<CodeBlock> const& condition() const { return m_condition; }
		Pointer<CodeBlock> const& body() const { return m_body; }
	private:
		Pointer<CodeBlock> m_condition;
		Pointer<CodeBlock> m_body;
	};

	class Function : public TvmAstNode {
	public:
		enum class FunctionType {
			PrivateFunction,
			Macro,
			MainInternal,
			MainExternal,
			OnCodeUpgrade,
			OnTickTock
		};
		Function(std::string name, FunctionType type, Pointer<CodeBlock> block) :
			m_name(std::move(name)),
			m_type(type),
			m_block(std::move(block))
		{
		}
		void accept(TvmAstVisitor& _visitor) override;
		std::string const& name() const { return m_name; }
		FunctionType type() const { return m_type; }
		Pointer<CodeBlock> const& block() const { return m_block; }
	private:
		std::string m_name;
		FunctionType m_type;
		Pointer<CodeBlock> m_block;
	};

	class Contract : public TvmAstNode {
	public:
		explicit Contract(
			std::vector<std::string> pragmas,
			std::vector<Pointer<Function>> functions
		) :
			m_pragmas{std::move(pragmas)},
			m_functions{std::move(functions)}
		{
		}
		void accept(TvmAstVisitor& _visitor) override;
		std::vector<std::string> const &pragmas() const { return m_pragmas; }
		const std::vector<Pointer<Function>>& functions() const;
	private:
		std::vector<std::string> m_pragmas;
		std::vector<Pointer<Function>> m_functions;
	};

	Pointer<GenOpcode> gen(const std::string& cmd);
	Pointer<Stack> make_DROP(int cnt = 1);
	Pointer<Stack> make_POP(int i);
	Pointer<Stack> make_BLKPUSH(int qty, int index);
	Pointer<Stack> make_PUSH(int i);
	Pointer<Stack> make_DUP2();
	Pointer<Stack> make_PUSH2(int i, int j);
	Pointer<Stack> make_PUSH3(int i, int j, int k);
	Pointer<Stack> make_OVER2();
	Pointer<ConFlowInst> make_RET();
	Pointer<ConFlowInst> make_IFRET();
	Pointer<ConFlowInst> make_IFNOTRET();
	Pointer<ConFlowInst> make_THROW(const std::string& cmd);
	Pointer<Stack> make_XCH_S(int i);
	Pointer<Stack> make_XCH_S_S(int i, int j);
	Pointer<Glob> make_setGlob(int i);
	Pointer<Stack> make_BLKDROP2(int droppedCount, int leftCount);
	Pointer<PushCellOrSlice> make_PUSHREF(std::string const& data = "");
	Pointer<Stack> make_reverse(int i, int j);
	Pointer<Stack> makeROT();
	Pointer<Stack> makeROTREV();
	Pointer<TvmIfElse> makeRevert(TvmIfElse const& node);
}	// end solidity::frontend
