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
#include "TVMStructCompiler.hpp"

namespace dev {
namespace solidity {

class FunctionCallCompiler : public StackPusherHelper {
	IExpressionCompiler* const m_exprCompiler;
	bool m_isResultNeeded;
	
protected:
	void acceptExpr(const Expression* expr) {
		m_exprCompiler->acceptExpr(expr);
	}
	
public:
	FunctionCallCompiler(IStackPusher* pusher, IExpressionCompiler* exprCompiler, const TVMCompilerContext& ctx)
		: StackPusherHelper(pusher, &ctx)
		, m_exprCompiler(exprCompiler)
		, m_isResultNeeded{true}
	{

	}
		
	void compile(FunctionCall const& _functionCall, bool isResultNeeded = true) {
		m_isResultNeeded = isResultNeeded;
		
		if (checkNewExpression(_functionCall)) return;
		if (checkTvmIntrinsic(_functionCall)) return;
		if (checkAddressThis(_functionCall)) return;
		if (checkSolidityUnits(_functionCall)) return;
		if (_functionCall.annotation().kind == FunctionCallKind::StructConstructorCall) {
			auto const& type = dynamic_cast<TypeType const&>(*_functionCall.expression().annotation().type);
			auto const& structType = dynamic_cast<StructType const&>(*type.actualType());
			auto const& structDefinition = structType.structDefinition();
			StructCompiler structCompiler{this, &structDefinition};
			structCompiler.createStruct([this, &_functionCall](const int fieldIndex, const std::string& fieldName) {
				std::vector<ASTPointer<ASTString>> names = _functionCall.names();
				if (!names.empty()) {
					for (size_t i = 0; i < names.size(); ++i) {
						if (fieldName == *names[i].get()) {
							acceptExpr(_functionCall.arguments()[i].get());
							return;
						}
					}
					solAssert(false, "");
				}
				acceptExpr(_functionCall.arguments()[fieldIndex].get());
			});
			return ;
		}
		if (_functionCall.annotation().kind == FunctionCallKind::TypeConversion) {
			typeConversion(_functionCall);
			return;
		}
		if (checkForIdentifier(_functionCall)) return;


		// TODO: move to function
		auto arguments = _functionCall.arguments();
		auto expr = &_functionCall.expression();
		if (auto ma = to<MemberAccess>(expr)) {
			for (const auto& arg : arguments)
				acceptExpr(arg.get());
			auto category = getType(&ma->expression())->category();
			if (checkForSuper(*ma, category)) return;
			if (checkForTransfer(*ma, category)) return;
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
			pushPrivateFunctionOrMacroCall(-3, "send_internal_message_macro");
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

	void typeConversion(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		Type::Category argCategory = arguments[0]->annotation().type->category();
		auto acceptArg = [&arguments, this] () {
			for (const auto &arg : arguments)
				acceptExpr(arg.get());
			solAssert(arguments.size() == 1, "");
		};
		auto conversionToAddress = [&_functionCall, &argCategory, &acceptArg, &arguments, this](){
			switch (argCategory) {
				case Type::Category::Contract:
				case Type::Category::Address:
					acceptArg();
					break;
				case Type::Category::RationalNumber:
				case Type::Category::Integer: {
					auto literal = to<Literal>(arguments[0].get());
					if (literal) {
						literalToSliceAddress(literal);
					} else {
						acceptArg();
						pushPrivateFunctionOrMacroCall(-1 + 1, "make_std_address_with_zero_wid_macro");
					}
					break;
				}
				default:
					cast_error(_functionCall, "Unsupported casting.");
			}
		};

		if (auto etn = to<ElementaryTypeNameExpression>(&_functionCall.expression())) {
			auto defaultActionForCasting = [&acceptArg, &etn, this]() {
				acceptArg();
				acceptExpr(etn);
			};

			Type const *argType = arguments[0]->annotation().type.get();
			switch (etn->typeName().token()) {
				case Token::BytesM: {
					acceptArg();
					int diff = 0;
					if (argCategory == Type::Category::FixedBytes) {
						auto fixedBytesType = to<FixedBytesType>(arguments[0]->annotation().type.get());
						diff = 8 * static_cast<int>(etn->typeName().firstNumber()) -
						       8 * static_cast<int>(fixedBytesType->storageBytes());
					} else if (argCategory == Type::Category::Address) {
						pushPrivateFunctionOrMacroCall(-1 + 1, "address2uint_macro");
						diff = 8 * static_cast<int>(etn->typeName().firstNumber()) - static_cast<int>(256);
						if (diff < 0) {
							cast_warning(_functionCall,
							             "Such conversion can cause loss of data. Only first bytes are used in such assignment.");
						}
					} else {
						cast_error(_functionCall, "Unsupported casting.");
					}

					if (diff > 0) {
						push(0, "LSHIFT " + std::to_string(diff));
					} else if (diff < 0) {
						push(0, "RSHIFT " + std::to_string(-diff));
					}
					break;
				}
				case Token::Address: {
					conversionToAddress();
					break;
				}
				case Token::Int:
				case Token::UInt: {
					if (argCategory == Type::Category::Contract || argCategory == Type::Category::Address) {
						auto literal = to<Literal>(arguments[0].get());
						if (literal) {
							const u256 value = literal->annotation().type->literalValue(literal);
							push(+1, "PUSHINT " + toString(value));
						} else {
							acceptArg();
							pushPrivateFunctionOrMacroCall(-1 + 1, "address2uint_macro");
							cast_warning(_functionCall, "Workchain id is lost in this conversion.");
						}
					} else {
						defaultActionForCasting();
					}
					break;
				}
				case Token::Bytes:
				case Token::String: {
					if (isStringOrStringLiteralOrBytes(argType)) {
						acceptArg(); // nothing to do here
					} else {
						cast_error(_functionCall, "Unsupported casting.");
					}
					break;
				}
				default:
					defaultActionForCasting();
			}
		} else if (auto identifier = to<Identifier>(&_functionCall.expression())) {
			if (to<ContractDefinition>(identifier->annotation().referencedDeclaration)) {
				conversionToAddress();
			} else if (auto enumDef = to<EnumDefinition>(identifier->annotation().referencedDeclaration)) {
				
				if (auto e = to<UnaryOperation>(_functionCall.arguments()[0].get())) {
					auto literal = to<Literal>(&e->subExpression());
					if ((e->getOperator() == Token::Sub) && (literal)) {
						const auto* type = literal->annotation().type.get();
						dev::u256 value = type->literalValue(literal);
						if (value != 0)
							cast_error(_functionCall, "Argument for enum casting should not be negative.");
						pushInt(0);
						return;
					}
				}
				if (auto literal = to<Literal>(_functionCall.arguments()[0].get())) {
					const auto* type = literal->annotation().type.get();
					dev::u256 value = type->literalValue(literal);
					if (value >= enumDef->members().size())
						cast_error(_functionCall, "Argument for enum casting should not exceed enum limit.");
					push(+1, "PUSHINT " + toString(value));
					return;
				}
				
				acceptExpr(_functionCall.arguments()[0].get());
				push(+1, "DUP");
				pushInt(enumDef->members().size());
				push(-1, "GEQ");
				
				auto type = _functionCall.arguments()[0].get()->annotation().type.get();
				TypeInfo ti(type);
				if (!ti.isNumeric || ti.isSigned)
				{
					push(+1, "OVER");
					push(0, "ISNEG");
					push(-1, "OR");
				}
				push(-1, "THROWIF 5");
			} else {
				cast_error(_functionCall, "Unsupported type conversion");
			}
		} else {
			cast_error(_functionCall, "Unsupported type conversion");
		}
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
			auto &codeLines = ctx().m_inlinedFunctions.at(functionName);
			int nParams = functionType->parameterTypes().size();
			int nRetVals = functionType->returnParameterTypes().size();

			push(codeLines);
			push(-nParams + nRetVals, "");
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
			pushPrivateFunctionOrMacroCall(-4, "send_accurate_internal_message_macro");
			return true;
		}
		return false;
	}

	void dropFunctionResult(FunctionCall const& _functionCall) {
		Type const* returnType = _functionCall.annotation().type.get();
		switch(returnType->category()) {
			case Type::Category::Tuple: {
				drop(to<TupleType>(returnType)->components().size());
				break;
			}
			default:
				drop(1);
		}
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
			if (!m_isResultNeeded) {
				dropFunctionResult(_functionCall);
			}
		} else if (getStack().isParam(iname)) {
			// Local variable of functional type
			acceptExpr(expr);
			auto functionType = to<FunctionType>(identifier->annotation().type.get());
			int returnCnt = functionType->returnParameterTypes().size();
			int paramCnt = functionType->parameterTypes().size();
			push(-1 - paramCnt + returnCnt, "CALLX");
			if (!m_isResultNeeded) {
				dropFunctionResult(_functionCall);
			}
		} else {
			cast_error(*identifier, "Unknown identifier");
		}
		return true;
	}

	bool checkNewExpression(FunctionCall const& _functionCall) {
		auto arguments = _functionCall.arguments();
		if (to<NewExpression>(&_functionCall.expression())) {
			// create new array
			// TODO: this code looks buggy. Do we check that it is array?
			Type const* resultType = getType(&_functionCall);
			push( 0, ";; new " + resultType->toString());


			auto arrayType = to<ArrayType>(resultType);
			Type const* arrayBaseType = arrayType->baseType().get();

			int size = getStack().size();

			pushDefaultValue(arrayBaseType); // default
			prepareValueForDictOperations(arrayBaseType);
			acceptExpr(arguments[0].get()); // default size
			push(+1, "DUP");  // default size size
			{
				TVMStack stack;
				CodeLines code;
				StackPusherImpl pusher(stack, code);
				StackPusherHelper pusherHelper(&pusher, &ctx());
				pusherHelper.push(0, "DEC"); // default index
				pusherHelper.push(0, "NEWDICT"); // default index arr
				pusherHelper.setDict(getKeyTypeOfArray(), *arrayBaseType, _functionCall);

				pushCont(code);// default size size cont0
			}
			push(+1, "PUSHCONT { DROP DROP NEWDICT }"); // default size size cont0 cont1
			push(-5 + 1,  "IFELSE");

			solAssert(size + 1 == getStack().size(), "");
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
