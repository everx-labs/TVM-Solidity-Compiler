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

void StackPusherHelper::f(const StructDefinition &structDefinition, const string &pref,
		const std::map<std::string, int> &memberToStackSize,
		EncodePosition& position, ASTNode const* node) {
	for (const ASTPointer<VariableDeclaration> &member : structDefinition.members()) {
		Type const* type = member->type().get();
		if (isUsualStruct(type)) {
			auto structType = to<StructType>(type);
			f(structType->structDefinition(), pref + "@" + member->name(), memberToStackSize, position, node);
		} else {
			encodeParameter(type, position, [this, &memberToStackSize, &member, &pref](){
				int stackSize = getStack().size();
				const std::string memberName = pref + "@" + member->name();
				push(+1, "PUSH s" + toString(stackSize - memberToStackSize.at(memberName)));
			}, node);
		}
	}
}

void StackPusherHelper::encodeStruct(const StructType* structType, ASTNode const* node, EncodePosition& position) {
	// builder... builder struct
	const int saveStackSize0 = getStack().size() - 2;

	std::map<std::string, int> memberToStackSize;
	StructCompiler structCompiler{this, structType};
	structCompiler.expandStruct(memberToStackSize);

	// builder... builder data...
	const int saveStackSize1 = getStack().size();
	const int builderIndex = saveStackSize1 - (saveStackSize0 + 1);
	push(+1, "PUSH s" + toString(builderIndex)); // builder... builder data... builder

	const StructDefinition& structDefinition = structType->structDefinition();
	f(structDefinition, "", memberToStackSize, position, node);
	// builder data... builder...
	const int saveStackSize2 = getStack().size();
	dropUnder(saveStackSize2 - saveStackSize1, saveStackSize1 - saveStackSize0);
}

void StackPusherHelper::pushDefaultValue(Type const* type, bool isResultBuilder) {
	Type::Category cat = type->category();
	switch (cat) {
		case Type::Category::Address:
		case Type::Category::Contract:
			push(+1, "PUSHSLICE x8000000000000000000000000000000000000000000000000000000000000000001_");
			break;
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::Enum:
			push(+1, "PUSHINT 0");
			break;
		case Type::Category::Array:
			if (to<ArrayType>(type)->isByteArray()) {
				push(+1, "NEWC");
				push(0, "ENDC");
				break;
			}
			push(+1, "NEWC");
			pushInt(33);
			push(-1, "STZEROES");
			if (!isResultBuilder) {
				push(0, "ENDC");
				push(0, "CTOS");
			}
			break;
		case Type::Category::Mapping:
			push(+1, "NEWDICT");
			break;
		case Type::Category::Struct: {
			auto structType = to<StructType>(type);
			if (isTvmCell(structType)) {
				push(+1, "NEWC");
				push(0, "ENDC");
				break;
			}
			StructCompiler structCompiler{this, structType};
			structCompiler.createDefaultStruct(isResultBuilder);
			break;
		}
		case Type::Category::Function: {
			auto functionType = to<FunctionType>(type);
			StackPusherImpl2 pusher;
			StackPusherHelper pusherHelper(&pusher, &ctx());
			pusherHelper.drop(functionType->parameterTypes().size());
			for (const TypePointer &param : functionType->returnParameterTypes()) {
				pusherHelper.pushDefaultValue(param.get());
			}
			pushCont(pusher.codeLines());
			break;
		}
		default:
			solAssert(false, "");
	}
}

void StackPusherHelper::getFromDict(Type const& keyType, Type const& valueType, ASTNode const& node, const bool pushDefaultValue) {
	// stack: index dict
	const Type::Category valueCategory = valueType.category();
	prepareKeyForDictOperations(&keyType);
	int keyLength = lengthOfDictKey(&keyType);
	pushInt(keyLength); // stack: index dict nbits

	auto pushDefaultDictValue = [&](){
		StackPusherImpl2 pusher;
		StackPusherHelper pusherHelper(&pusher, &ctx());
		pusherHelper.pushDefaultValue(&valueType);
		pushCont(pusher.codeLines());
	};

	std::string dictOpcode = "DICT" + typeToDictChar(&keyType);
	if (isTvmCell(&valueType)) {
		push(-3 + 2, dictOpcode + "GETREF");
		if (pushDefaultValue) {
			pushDefaultDictValue();
			push(-2, "IFNOT");
		} else {
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
		}
	} else if (valueCategory == Type::Category::Mapping || isTvmCell(&valueType)) {
		push(-3 + 1, dictOpcode + "GETOPTREF");
	} else if (valueCategory == Type::Category::Struct) {
		if (StructCompiler::isCompatibleWithSDK(keyLength, to<StructType>(&valueType))) {
			push(-3 + 2, dictOpcode + "GET");
			if (pushDefaultValue) {
				pushDefaultDictValue();
				push(-2, "IFNOT");
			} else {
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
			}
		} else {
			push(-3 + 2, dictOpcode + "GETREF");
			if (pushDefaultValue) {
				StackPusherImpl2 pusher;
				StackPusherHelper pusherHelper(&pusher, &ctx());
				pusher.push(-1 + 1, "CTOS");
				pushCont(pusher.codeLines());

				pushDefaultDictValue();
				push(-3, "IFELSE");
			} else {
				push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				push(-1 + 1, "CTOS");
			}
		}
	} else if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract, Type::Category::Array)) {
		if (valueCategory == Type::Category::Array && to<ArrayType>(&valueType)->isByteArray()) {
			push(-3 + 2, dictOpcode + "GETREF");
		} else {
			push(-3 + 2, dictOpcode + "GET");
		}
		if (pushDefaultValue) {
			pushDefaultDictValue();
			push(-2, "IFNOT");
		} else {
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
		}
	} else if (isIntegralType(&valueType)) {
		push(-3 + 2, dictOpcode + "GET");
		if (pushDefaultValue) {
			StackPusherImpl2 pusher;
			StackPusherHelper pusherHelper(&pusher, &ctx());
			pusher.push(0, preloadIntergal(&valueType));
			pushCont(pusher.codeLines());

			pushDefaultDictValue();
			push(-3, "IFELSE");
		} else {
			push(-1, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
			push(0, preloadIntergal(&valueType));
		}
	} else {
		cast_error(node, "Unsupported value type: " + valueType.toString());
	}
}

std::vector<VariableDeclaration const *> StackPusherHelper::notConstantAndNotPublicStateVariables() {
	std::vector<VariableDeclaration const*> variableDeclarations;
	std::vector<ContractDefinition const*> mainChain = getContractsChain(ctx().getContract());
	for (ContractDefinition const* contract : mainChain) {
		for (VariableDeclaration const *variable: contract->stateVariables()) {
			if (!variable->isConstant() && !variable->isPublic()) {
				variableDeclarations.push_back(variable);
			}
		}
	}
	return variableDeclarations;
}

bool StackPusherHelper::isNotConstantAndNotPublicStateVariables(VariableDeclaration const* vd) {
	std::vector<VariableDeclaration const *> arr = notConstantAndNotPublicStateVariables();
	return std::any_of(arr.begin(), arr.end(), [&vd](VariableDeclaration const * element){
		return element->name() == vd->name();
	});
}


void StackPusherHelper::pushLog(const std::string& str) {
	if (TVMCompiler::g_with_logstr) {
		push(0, "PRINTSTR " + str);
	}
}