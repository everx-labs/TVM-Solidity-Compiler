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
 * @date 2019
 * Function call compiler for TVM
 */

#pragma once

#include "TVMCommons.hpp"

namespace dev {
namespace solidity {

class FunctionCallCompiler : public StackPusherHelper {
	IExpressionCompiler* const m_exprCompiler;
	
protected:
	void acceptExpr(const Expression* expr) {
		m_exprCompiler->acceptExpr(expr);
	}
	
public:
	FunctionCallCompiler(IStackPusher* pusher, IExpressionCompiler* exprCompiler, const TVMCompilerContext& ctx)
		: StackPusherHelper(pusher, &ctx)
		, m_exprCompiler(exprCompiler) {}
		
	void compile(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		auto expr = &_functionCall.expression();
		
		if (checkNewExpression(_functionCall)) return;
		if (checkTvmIntrinsic(_functionCall)) return;
		if (checkAddressThis(_functionCall)) return;
		if (checkSolidityUnits(_functionCall)) return;
		if (checkForIdentifier(_functionCall)) return;
		if (checkExplicitCasting(_functionCall)) return;

		// TODO: move to function
		if (auto ma = to<MemberAccess>(expr)) {
			for (const auto arg : arguments)
				acceptExpr(arg.get());
			auto category = getType(&ma->expression())->category();
			if (checkForSuper(*ma, category)) return;
			if (checkForTransfer(*ma, category)) return;
			if (checkForArrayPush(*ma, category)) return;
			if (checkForMemberAccessTypeType(*ma, category)) return;
			cast_error(*ma, "Unsupported function call");
		}
		
		solAssert(false, "#1036");
	}

protected:
	bool checkForSuper(MemberAccess const& _node, Type::Category) {
		// argument are on stack
		if (!isSuper(&_node.expression()))
			return false;
		push(0, ";; super");
		string fname = _node.memberName();
		auto super = getSuperContract(ctx().getContract(ctx().m_currentFunction->m_function),
												ctx().getContract(), fname);
		solAssert(super, "#1000");
		if (getFunction(super, fname)) {
			auto functionName = super->name() + "_" + fname;
			push( 0, ";; Super call " + functionName);
			if (auto ft = to<FunctionType>(getType(&_node))) {
				pushCall(functionName, ft);
				return true;
			}
		}
		solAssert(false, "");
	}

	bool checkForTransfer(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::Address)
			return false;
		if (_node.memberName() == "transfer") {
			// address.transfer(value)
			push(0, ";; transfer()");
			// stack: value
			acceptExpr(&_node.expression());
			// stack: value address
			push(+1, "NEWC"); // empty body
			pushPrivateFunctionCall(-3, "send_grams");
			return true;
		}
		return false;
	}

	bool checkForArrayPush(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::Array)
			return false;
		if (_node.memberName() == "push") {
			auto arrayType = to<ArrayType>(getType(&_node.expression()));
			if (arrayType == nullptr) {
				cast_error(_node.expression(), "Left hand side should be array type.");
			}
			auto arrayBaseType = arrayType->baseType().get();
			bool isInterger = isIntegralType(arrayBaseType);

			// Arguments are already on stack
			auto savedStackSize = getStack().size();
			// TODO: this code is hard to understand. Would it be better to move it to stdlib?
			// stack: value
			dumpStackSize();
			push(0, "LOGSTR array_push");
			if (isInterger) {
				push(-1 + 1, "NEWC STU " + toString(TypeInfo(arrayBaseType).numBits));
			}
			// stack: value-slice
			acceptExpr(&_node.expression());
			// stack: value-slice array
			push(+1, "DUP");
			// stack: value-slice array array
			pushInt(32);
			// • F48E — DICTUMAX (s n – x i −1 or 0).
			push(-2+3,	"DICTUMAX");
			// stack: value-slice array max_value max_idx -1
			push(+1,		"PUSHCONT { POP s1 INC}");
			push(+1,		"PUSHCONT { PUSHINT 0 }");
			push(-3-2+1,	"IFELSE");
			// stack: value-slice array (max_idx+1)
			push(0,		"SWAP");
			// stack: value-slice (max_idx+1) array
			if (isInterger) {
				pushInt(32);
				// stack: value-slice (max_idx+1) array 32
				push(-4 + 1, "DICTUSETB");
			} else {
				setDict(false, 32, false);
			}
			// stack: array'
			if (auto id = to<Identifier>(&_node.expression())) {
				assignVar(id);
			} else {
				cast_error(_node.expression(), "Left hand side should be identifier.");
			}
			dumpStackSize();
			getStack().ensureSize(savedStackSize-1, "array.push()");
			return true;
		}
		return false;
	}
	
	bool checkForMemberAccessTypeType(MemberAccess const& _node, Type::Category category) {
		if (category != Type::Category::TypeType)
			return false;
		if (auto identifier = to<Identifier>(&_node.expression())) {
			const auto& iname = identifier->name();
			if (auto funcitonType = to<FunctionType>(getType(&_node))) {
				// calling base contract method
				auto functionName = iname + "_" + _node.memberName();
				pushCall(functionName, funcitonType);
				return true;
			}
		}
		return false;
	}
	
	bool checkAddressThis(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		// compile  "address(this)"
		if (isAddressThis(&_functionCall)) {
			push(+1, "MYADDR");
			return true;
		}
		return false;
	}

	bool checkExplicitCasting(FunctionCall const& _functionCall) {
		auto etn = to<ElementaryTypeNameExpression>(&_functionCall.expression());
		if (etn == nullptr) {
			return false;
		}
		auto arguments = _functionCall.arguments();

		{
			Type::Category argCategory = arguments[0]->annotation().type->category();
			if (argCategory == Type::Category::Address && to<Literal>(arguments[0].get()) == nullptr) {
				cast_error(_functionCall, "Unsupported casting.");
			}
		}

		if (etn->typeName().token() == Token::BytesM) {
			if (arguments.size() != 1) {
				cast_error(_functionCall, "Unsupported casting.");
			}
			Type::Category argCategory = arguments[0]->annotation().type->category();
			if (argCategory == Type::Category::FixedBytes) {
				acceptExpr(arguments[0].get());
				int diff = 0;
				auto argType = to<FixedBytesType>(arguments[0]->annotation().type.get());
				diff = 8 * static_cast<int>(etn->typeName().firstNumber()) -
				       8 * static_cast<int>(argType->storageBytes());
				if (diff > 0) {
					push(0, "LSHIFT " + std::to_string(diff));
				} else if (diff < 0) {
					push(0, "RSHIFT " + std::to_string(-diff));
				}
			} else {
				cast_error(_functionCall, "Unsupported casting.");
			}
		} else if (etn->typeName().token() == Token::Address) {
			if (arguments.size() != 1) {
				cast_error(_functionCall, "Unsupported casting.");
			}
			Type::Category argCategory = arguments[0]->annotation().type->category();
			if (argCategory != Type::Category::Contract) {
				cast_error(_functionCall, "Unsupported casting.");
			}
			acceptExpr(arguments[0].get());
		} else {
			for (const auto &arg : arguments)
				acceptExpr(arg.get());
			acceptExpr(etn);
		}
		return true;
	}
	
	bool checkLocalFunctionCall(const Identifier* identifier) {
		string functionName = identifier->name();
		auto function = ctx().getLocalFunction(functionName);
		if (!function)
			return false;
		// Calling local function
		auto functionType = to<FunctionType>(getType(identifier));
		solAssert(functionType, "#209");
		if (ctx().m_inlinedFunctions.count(functionName) > 0) {
			// Inline function call
			CodeLines code = ctx().m_inlinedFunctions.at(functionName);
			pushCont(code, functionName);
			pushInlineCall(functionType);
		} else {
			pushCall(ctx().getFunctionInternalName(function), functionType);
		}
		return true;
	}

	bool checkSolidityUnits(FunctionCall const& _functionCall) {
		auto identifier = to<Identifier>(&_functionCall.expression());
		if (identifier == nullptr) {
			return false;
		}

		string iname = identifier->name();
		if (iname == "sha256") {
			for (const auto& arg : _functionCall.arguments()) {
				acceptExpr(arg.get());
			}
			push(0, "SHA256U");
			return true;
		}
		if (iname == "selfdestruct") {
			for (const auto& arg : _functionCall.arguments()) {
				acceptExpr(arg.get()); //  remote_addr
			}
			push(+1, "PUSHINT 1000"); // grams_value // TODO why not 0? Is here bug in local node?
			push(+1, "PUSHINT 0"); // bounce
			push(+1, "PUSHINT " + toString(TvmConst::SENDRAWMSG::CarryAllMoney)); // sendrawmsg_flag
			pushPrivateFunctionCall(-4, "accurate_transfer");
			return true;
		}
		return false;
	}

	bool checkForIdentifier(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		auto expr = &_functionCall.expression();
		auto identifier = to<Identifier>(expr);
		if (!identifier) 
			return false;
		string iname = identifier->name();
		// TODO: Refactor this ugly function
		for (const auto& arg : arguments) {
			acceptExpr(arg.get());
		}

		if (checkLocalFunctionCall(identifier)) {
		} else if (ctx().isContractName(iname)) {
			// convertion from address to Contract. Do nothing
			// TODO: move to function call level?
		} else if (ctx().isStruct(iname)) {
			// Setting up a structure with given members.
			// Members are already on stack
			// TODO: move it to upper level?
			if (!_functionCall.names().empty())
				cast_error(_functionCall, "Only unnamed variables inside struct initialization are supported.");
			setupStructure(iname);
		} else if (getStack().isParam(iname)) {
			// Local variable of functional type
			acceptExpr(expr);
			auto functionType = to<FunctionType>(identifier->annotation().type.get());
			int returnCnt = functionType->returnParameterTypes().size();
			int paramCnt = functionType->parameterTypes().size();
			push(-1 - paramCnt + returnCnt, "CALLX");
		} else {
			cast_error(*identifier, "Unknown identifier");
		}
		return true;
	}

	void setupStructure(string iname) {
		// Setting up a structure with given members.
		// Members are already on stack
		push( 0, ";; struct " + iname);
		auto members = ctx().getStructMembers(iname);
		push(+1, "NEWDICT");
		for (int i = members.size() - 1; i >= 0; i--) {
			VariableDeclaration const* member = members[i].get();
			push( 0, ";; member " + member->name());
			auto type = getType(member);
			if (isIntegralType(type)) {
				// x dict
				push( 0, "SWAP");
				// dict x
				intToBuilder(type);
				push( 0, "SWAP");
			}
			pushInt(i);
			push( 0, "SWAP");
			setDict(isIntegralType(type), 8, false);
		}
	}

	bool checkNewExpression(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		if (to<NewExpression>(&_functionCall.expression())) {
			// create new array
			// TODO: this code looks buggy. Do we check that it is array?
			Type const* resultType = getType(&_functionCall);
			push( 0, ";; new " + resultType->toString());

			Type const* arrayBaseType = nullptr;
			if (auto arrayType = to<ArrayType>(resultType)) {
				arrayBaseType = arrayType->baseType().get();
			}
			solAssert(arrayBaseType, string("Unsupported type: ") + typeid(*resultType).name());

			pushInt(0);
			intToBuilder(arrayBaseType);
			acceptExpr(arguments[0].get());
			// TODO: too complex code to read
			push(+1, "DUP");
			push(+1, "PUSHCONT {");
			push( 0, "\tDEC");
			push(+1, "\tNEWDICT");
			push(+1, "\tPUSHINT 32");
			push(-3, "\tDICTUSETB");
			dumpStackSize("\t");
			push(0, "}");
			push(+1, "PUSHCONT { DROP DROP NEWDICT }");
			push(-3,  "IFELSE");
			return true;
		}
		return false;
	}

	bool checkTvmIntrinsic(FunctionCall const& _functionCall) {
		IntrinsicsCompiler ic(getStackPusher(), m_exprCompiler, ctx());
		return ic.checkTvmIntrinsic(_functionCall);
	}

};
	
}	// solidity
}	// dev
