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
 * Common TVM codegen routines, in particular, types, data structures, scope, stack manipulations, etc.
 */

#include "TVMCommons.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMCompiler.hpp"

void StackPusherHelper::encodeStruct(const StructType* structType, ASTNode const* node, EncodePosition& position) {
	// builder... builder struct
	const int saveStackSize0 = getStack().size() - 2;
	std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
	const int memberQty = members.size();
	untuple(memberQty); // builder... builder values...
	blockSwap(1, memberQty); // builder... values... builder
	for (int i = 0; i < memberQty; ++i) {
		encodeParameter(members[i]->type(), position, [&]() {
			const int index = getStack().size() - saveStackSize0 - 1 - i;
			pushS(index);
		}, node);
	}

	// builder... values... builder...
	const int builderQty = getStack().size() - saveStackSize0 - memberQty;
	dropUnder(builderQty, memberQty);
}

void StackPusherHelper::pushDefaultValue(Type const* type, bool isResultBuilder) {
	Type::Category cat = type->category();
	switch (cat) {
		case Type::Category::Address:
		case Type::Category::Contract:
			pushZeroAddress();
			if (isResultBuilder) {
				push(+1, "NEWC");
				push(-1, "STSLICE");
			}
			break;
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::Enum:
			push(+1, "PUSHINT 0");
			if (isResultBuilder) {
				push(+1, "NEWC");
				push(-1, storeIntegralOrAddress(type, false));
			}
			break;
		case Type::Category::Array:
			if (to<ArrayType>(type)->isByteArray()) {
				push(+1, "NEWC");
				if (!isResultBuilder) {
					push(0, "ENDC");
				}
				break;
			}
			if (!isResultBuilder) {
				pushInt(0);
				push(+1, "NEWDICT");
				push(-2 + 1, "PAIR");
			} else {
				push(+1, "NEWC");
				pushInt(33);
				push(-1, "STZEROES");
			}
			break;
		case Type::Category::Mapping:
			solAssert(!isResultBuilder, "");
			push(+1, "NEWDICT");
			break;
		case Type::Category::Struct: {
			auto structType = to<StructType>(type);
			StructCompiler structCompiler{this, structType};
			structCompiler.createDefaultStruct(isResultBuilder);
			break;
		}
		case Type::Category::TvmCell:
			push(+1, "NEWC");
			if (!isResultBuilder) {
				push(0, "ENDC");
			}
			break;
		case Type::Category::Function: {
			solAssert(!isResultBuilder, "");
			auto functionType = to<FunctionType>(type);
			StackPusherHelper pusherHelper(&ctx());
			pusherHelper.drop(functionType->parameterTypes().size());
			for (const TypePointer &param : functionType->returnParameterTypes()) {
				pusherHelper.pushDefaultValue(param);
			}
			pushCont(pusherHelper.code());
			break;
		}
		default:
			solAssert(false, "");
	}
}

void StackPusherHelper::getFromDict(Type const& keyType, Type const& valueType, ASTNode const& node,
									const DictOperation op,
									const bool resultAsSliceForStruct) {
	// stack: index dict
	const Type::Category valueCategory = valueType.category();
	prepareKeyForDictOperations(&keyType);
	int keyLength = lengthOfDictKey(&keyType);
	pushInt(keyLength); // stack: index dict nbits

	StackPusherHelper haveValue(&ctx()); // for Fetch
	haveValue.push(0, "SWAP");

	StackPusherHelper pusherMoveC7(&ctx()); // for MoveToC7

	auto pushContinuationWithDefaultDictValue = [&](){
		StackPusherHelper pusherHelper(&ctx());
		if (valueCategory == Type::Category::Struct) {
			if (resultAsSliceForStruct) {
				pusherHelper.pushDefaultValue(&valueType, true);
				pusherHelper.push(0, "ENDC");
				pusherHelper.push(0, "CTOS");
			} else {
				pusherHelper.pushDefaultValue(&valueType, false);
			}
		} else {
			pusherHelper.pushDefaultValue(&valueType);
		}
		pushCont(pusherHelper.code());
	};

	auto fetchValue = [&](){
		StackPusherHelper noValue(&ctx());
		noValue.pushDefaultValue(&valueType, false);

		push(0, "DUP");
		pushCont(haveValue.code());
		pushCont(noValue.code());
		push(-2, "IFELSE");
	};

	auto checkExist = [&](){
		StackPusherHelper deleteValue(&ctx());
		deleteValue.push(-1, "NIP"); // delete value

		push(0, "DUP");
		pushCont(deleteValue.code());
		push(-2, "IF");
	};

	std::string dictOpcode = "DICT" + typeToDictChar(&keyType);
	if (valueCategory == Type::Category::TvmCell) {
		push(-3 + 2, dictOpcode + "GETREF");
		switch (op) {
			case DictOperation::GetFromMapping:
				pushContinuationWithDefaultDictValue();
				push(-2, "IFNOT");
				break;
			case DictOperation::GetFromArray:
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				break;
			case DictOperation::Fetch:
				fetchValue();
				break;
			case DictOperation::Exist:
				checkExist();
				break;
		}
	} else if (valueCategory == Type::Category::Struct) {
		if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(&valueType))) {
			push(-3 + 2, dictOpcode + "GET");
			switch (op) {
				case DictOperation::GetFromMapping:
					if (resultAsSliceForStruct) {
						pushContinuationWithDefaultDictValue();
						push(-2, "IFNOT");
					} else {
						// if ok
						{
							startContinuation();
							StructCompiler sc{this, to<StructType>(&valueType)};
							sc.convertSliceToTuple();
							endContinuation();
						}
						// if fail
						{
							startContinuation();
							StructCompiler sc{this, to<StructType>(&valueType)};
							sc.createDefaultStruct(false);
							endContinuation();
						}
						push(-2, "IFELSE");
					}
					break;
				case DictOperation::GetFromArray:
					push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
					if (!resultAsSliceForStruct) {
						StructCompiler sc{this, to<StructType>(&valueType)};
						sc.convertSliceToTuple();
					}
					break;
				case DictOperation::Fetch: {
					StructCompiler sc{&haveValue, to<StructType>(&valueType)};
					sc.convertSliceToTuple();
					fetchValue();
					break;
				}
				case DictOperation::Exist:
					checkExist();
					break;
			}
		} else {
			push(-3 + 2, dictOpcode + "GETREF");
			switch (op) {
				case DictOperation::GetFromMapping: {
					StackPusherHelper pusherHelper(&ctx());
					pusherHelper.push(-1 + 1, "CTOS");
					if (!resultAsSliceForStruct) {
						StructCompiler sc{&pusherHelper, to<StructType>(&valueType)};
						sc.convertSliceToTuple();
					}
					pushCont(pusherHelper.code());
					pushContinuationWithDefaultDictValue();
					push(-3, "IFELSE");
					break;
				}
				case DictOperation::GetFromArray:
					push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
					push(-1 + 1, "CTOS");
					if (!resultAsSliceForStruct) {
						StructCompiler sc{this, to<StructType>(&valueType)};
						sc.convertSliceToTuple();
					}
					break;
				case DictOperation::Fetch: {
					haveValue.push(0, "CTOS");
					StructCompiler sc{&haveValue, to<StructType>(&valueType)};
					sc.convertSliceToTuple();
					fetchValue();
					break;
				}
				case DictOperation::Exist:
					checkExist();
					break;
			}
		}
	} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract) || isByteArrayOrString(&valueType)) {
		if (isByteArrayOrString(&valueType)) {
			push(-3 + 2, dictOpcode + "GETREF");
		} else {
			push(-3 + 2, dictOpcode + "GET");
		}

		switch (op) {
			case DictOperation::GetFromMapping:
				pushContinuationWithDefaultDictValue();
				push(-2, "IFNOT");
				break;
			case DictOperation::GetFromArray:
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				break;
			case DictOperation::Fetch: {
				fetchValue();
				break;
			}
			case DictOperation::Exist:
				checkExist();
				break;
		}
	} else if (isIntegralType(&valueType) || isUsualArray(&valueType) || valueCategory == Type::Category::Mapping) {
		push(-3 + 2, dictOpcode + "GET");
		switch (op) {
			case DictOperation::GetFromMapping: {
				StackPusherHelper pusherHelper(&ctx());

				pusherHelper.preload(&valueType);
				pushCont(pusherHelper.code());

				pushContinuationWithDefaultDictValue();
				push(-3, "IFELSE");
				break;
			}
			case DictOperation::GetFromArray:
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				preload(&valueType);
				break;
			case DictOperation::Fetch: {
				haveValue.preload(&valueType);
				fetchValue();
				break;
			}
			case DictOperation::Exist:
				checkExist();
				break;
		}
	} else {
		cast_error(node, "Unsupported value type: " + valueType.toString());
	}
}

void StackPusherHelper::pushLog(const std::string& str) {
	if (!TVMCompiler::g_without_logstr) {
		push(0, "PRINTSTR " + str);
	}
}
bool IExpressionCompiler::isWithoutLogstr()
{
	return TVMCompiler::g_without_logstr;
}
