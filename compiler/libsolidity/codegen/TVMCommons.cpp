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

void StackPusherHelper::f(const StructDefinition &structDefinition, const string &pref,
		const std::map<std::string, int> &memberToStackSize,
		EncodePosition& position, ASTNode const* node) {
	for (const ASTPointer<VariableDeclaration> &member : structDefinition.members()) {
		Type const* type = member->type().get();
		if (type->category() == Type::Category::Struct) {
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

	const StructDefinition& structDefinition = structType->structDefinition();
	std::map<std::string, int> memberToStackSize;
	StructCompiler structCompiler{this, &structDefinition};
	structCompiler.expandStruct(memberToStackSize);

	// builder... builder data...
	const int saveStackSize1 = getStack().size();
	const int builderIndex = saveStackSize1 - (saveStackSize0 + 1);
	push(+1, "PUSH s" + toString(builderIndex)); // builder... builder data... builder

	f(structDefinition, "", memberToStackSize, position, node);
	// builder data... builder...
	const int saveStackSize2 = getStack().size();
	dropUnder(saveStackSize2 - saveStackSize1, saveStackSize1 - saveStackSize0);
}

void StackPusherHelper::pushDefaultValue(Type const* type) {
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
			[[fallthrough]];
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
			StructDefinition const* structDefinition = &structType->structDefinition();
			StructCompiler structCompiler{this, structDefinition};
			structCompiler.createDefaultStruct();
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

void StackPusherHelper::getFromDict(Type const& keyType, Type const& valueType, ASTNode const& node) {
	// stack: index dict
	const Type::Category valueCategory = valueType.category();
	prepareKeyForDictOperations(&keyType);
	if (isIn(keyType.category(), Type::Category::Address, Type::Category::Contract)) {
		pushInt(AddressInfo::maxBitLength());
	} else {
		pushInt(getKeyDictLength(&keyType)); // stack: index dict nbits
	}

	std::string dictOpcode = "DICT" + getKeyDict(&keyType);
	if (isIn(valueCategory, Type::Category::Mapping, Type::Category::Array) || isTvmCell(&valueType)) {
		push(-3 + 1, dictOpcode + "GETOPTREF");
		if (isStringOrStringLiteralOrBytes(&valueType) || isTvmCell(&valueType)) {
			push(+1, "DUP");
			push(-1 + 1, "ISNULL");
			{
				StackPusherImpl2 pusher;
				StackPusherHelper pusherHelper(&pusher, &ctx());
				pusherHelper.push(-1, "DROP");
				pusherHelper.pushDefaultValue(&valueType);
				pushCont(pusher.codeLines());
			}
			push(-2, "IF");
		}
		return;
	}

	push(-3 + 2, dictOpcode + "GET");
	if (isIn(valueCategory, Type::Category::Address, Type::Category::Contract, Type::Category::Function, Type::Category::Struct)) {
		StackPusherImpl2 pusher;
		StackPusherHelper pusherHelper(&pusher, &ctx());
		pusherHelper.pushDefaultValue(&valueType);
		pushCont(pusher.codeLines());
		push(-2, "IFNOT");
	} else if (isIntegralType(&valueType) || valueCategory == Type::Category::Enum) {
		{
			StackPusherImpl2 pusher;
			StackPusherHelper pusherHelper(&pusher, &ctx());
			pusher.push(+1, loadIntegralOrAddress(&valueType));
			pusher.push(-1, "ENDS");
			pushCont(pusher.codeLines());
		}
		{
			StackPusherImpl2 pusher;
			StackPusherHelper pusherHelper(&pusher, &ctx());
			pusherHelper.pushDefaultValue(&valueType);
			pushCont(pusher.codeLines());
		}
		push(-3, "IFELSE");
	} else {
		cast_error(node, "Unsupported value type: " + valueType.toString());
	}
}
