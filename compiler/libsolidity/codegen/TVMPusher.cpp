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
 */

#include "TVMCommons.hpp"
#include "TVMPusher.hpp"
#include "TVMContractCompiler.hpp"
#include "TVMExpressionCompiler.hpp"

using namespace solidity::frontend;

DictOperation::DictOperation(StackPusherHelper& pusher, Type const& keyType, Type const& valueType) :
		pusher{pusher},
		keyType{keyType},
		keyLength{lengthOfDictKey(&keyType)},
		valueType{valueType},
		valueCategory{valueType.category()} {
}

StackPusherHelper::StackPusherHelper(TVMCompilerContext *ctx, const int stackSize) :
		m_ctx(ctx),
		m_structCompiler{new StructCompiler{this,
											ctx->notConstantStateVariableTypes(),
											ctx->notConstantStateVariableNames(),
											256 + (m_ctx->storeTimestampInC4()? 64 : 0) + 1, // pubkey + timestamp + constructor_flag
											true}} {
	m_stack.change(stackSize);
}

void StackPusherHelper::pushLog() {
	push(0, "CTOS");
	push(0, "STRDUMP");
	drop();
}

StructCompiler &StackPusherHelper::structCompiler() {
	return *m_structCompiler;
}

void StackPusherHelper::generateC7ToT4Macro() {
	push(+1, ""); // fix stack, allocate builder
	generateMacro("c7_to_c4");
	pushLines(R"(
GETGLOB 2
NEWC
STU 256
)");
	if (ctx().storeTimestampInC4()) {
		pushLines(R"(
GETGLOB 3
STUR 64
)");
	}
	pushLines(R"(
GETGLOB 6
STUR 1
)");
	if (!ctx().notConstantStateVariables().empty()) {
		structCompiler().stateVarsToBuilder();
	}
	pushLines(R"(
ENDC
POP C4
)");
	push(0, " ");
}

bool StackPusherHelper::doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value) {
	int keyLength = lengthOfDictKey(key);
	return
		2 + // hml_long$10
		integerLog2(keyLength) +
		keyLength +
		maxBitLengthOfDictValue(value)
		<
		TvmConst::CellBitLength;
}

int StackPusherHelper::maxBitLengthOfDictValue(Type const* type) {
	switch (toDictValueType(type->category())) {
		case DictValueType::Enum:
		case DictValueType::Integer:
		case DictValueType::Bool:
		case DictValueType::FixedBytes:
		case DictValueType::FixedPoint: {
			TypeInfo ti{type};
			return ti.numBits;
		}

		case DictValueType::Address:
		case DictValueType::Contract:
			return AddressInfo::maxBitLength();

		case DictValueType::Array: {
			if (isStringOrStringLiteralOrBytes(type))
				solUnimplemented("");
			return 32 + 1;
		}

		case DictValueType::Mapping:
		case DictValueType::ExtraCurrencyCollection:
		case DictValueType::Optional:
			return 1;

		case DictValueType::VarInteger: {
			auto vi = to<VarInteger>(type);
			return integerLog2(vi->getNumber()) + 8 * vi->getNumber();
		}

		case DictValueType::TvmCell:
		case DictValueType::TvmSlice:
			solUnimplemented("");

		case DictValueType::Struct: {
			return StructCompiler::maxBitLength(to<StructType>(type));
		}

		case DictValueType::Function: {
			return 32;
		}
	}

	solUnimplemented("Unsupported " + type->toString());
}

DataType
StackPusherHelper::prepareValueForDictOperations(Type const *keyType, Type const *valueType, bool isValueBuilder) {
	// stack: value

	switch (toDictValueType(valueType->category())) {
		case DictValueType::TvmSlice: {
			return isValueBuilder ? DataType::Builder : DataType::Slice;
		}

		case DictValueType::Address:
		case DictValueType::Contract: {
			if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
				solAssert(!isValueBuilder, "");
				push(+1, "NEWC");
				push(-1, "STSLICE");
				push(0, "ENDC");
				return DataType::Cell;
			}
			return isValueBuilder ? DataType::Builder : DataType::Slice;
		}

		case DictValueType::Array: {
			if (isByteArrayOrString(valueType)) {
				if (isValueBuilder) {
					push(-1 + 1, "ENDC");
				}
				return DataType::Cell;
			}
			[[fallthrough]];
		}

		case DictValueType::Bool:
		case DictValueType::Enum:
		case DictValueType::ExtraCurrencyCollection:
		case DictValueType::FixedBytes:
		case DictValueType::FixedPoint:
		case DictValueType::Integer:
		case DictValueType::Mapping:
		case DictValueType::Optional:
		case DictValueType::VarInteger:
		case DictValueType::Function:
		{
			if (!isValueBuilder) {
				push(0, "NEWC");
				store(valueType, false, 0);
				push(+1, "");
			}
			if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
				push(0, "NEWC");
				push(0, "STBREF");
			}
			return DataType::Builder;
		}

		case DictValueType::Struct: {
			if (!isValueBuilder) {
				StructCompiler sc{this, to<StructType>(valueType)};
				sc.tupleToBuilder();
			}
			if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
				push(0, "ENDC");
				return DataType::Cell;
			}
			return DataType::Builder;
		}

		case DictValueType::TvmCell: {
			if (isValueBuilder) {
				 push(0, "ENDC");
			}
			return DataType::Cell;
		}
	}
	solUnimplemented("");
}

// delMin/delMax
// min/max
// fetch
// at/[] - for arrays and mappings
bool StackPusherHelper::doesDictStoreValueInRef(Type const* keyType, Type const* valueType) {
	switch (toDictValueType(valueType->category())) {
		case DictValueType::TvmCell:
			return true;

		case DictValueType::TvmSlice:
			return false;

		case DictValueType::Array: {
			if (isByteArrayOrString(valueType)) {
				return true;
			}
			return !doesFitInOneCellAndHaveNoStruct(keyType, valueType);
		}


		case DictValueType::Address:
		case DictValueType::Bool:
		case DictValueType::Contract:
		case DictValueType::Enum:
		case DictValueType::ExtraCurrencyCollection:
		case DictValueType::FixedBytes:
		case DictValueType::FixedPoint:
		case DictValueType::Integer:
		case DictValueType::Mapping:
		case DictValueType::Optional:
		case DictValueType::VarInteger:
		case DictValueType::Struct:
		case DictValueType::Function:
			return !doesFitInOneCellAndHaveNoStruct(keyType, valueType);
	}
	solUnimplemented("");
}

// false - value isn't in ref
// true - value is in ref
void StackPusherHelper::recoverKeyAndValueAfterDictOperation(
	Type const* keyType,
	Type const* valueType,
	bool haveKey,
	bool didUseOpcodeWithRef,
	const DecodeType& decodeType,
	bool resultAsSliceForStruct
)
{
	// stack: value [key]
	auto preloadValue = [&]() {
		if (haveKey) {
			if (keyType->category() == Type::Category::Struct) {
				StructCompiler sc{this, to<StructType>(keyType)};
				sc.convertSliceToTuple();
			}
			exchange(0, 1);
			// stack: key value
		}
		// stack: [key] value

		switch (toDictValueType(valueType->category())) {
			case DictValueType::Address:
			case DictValueType::Contract:
			case DictValueType::TvmSlice:
			{
				if (didUseOpcodeWithRef) {
					push(0, "CTOS");
				} else if (doesDictStoreValueInRef(keyType, valueType)) {
					push(0, "PLDREF");
					push(0, "CTOS");
				}
				break;
			}
			case DictValueType::Array:
				if (isByteArrayOrString(valueType)) {
					if (!didUseOpcodeWithRef) {
						push(0, "PLDREF");
					}
					break;
				}
				[[fallthrough]];
			case DictValueType::Bool:
			case DictValueType::Enum:
			case DictValueType::ExtraCurrencyCollection:
			case DictValueType::FixedBytes:
			case DictValueType::FixedPoint:
			case DictValueType::Integer:
			case DictValueType::Mapping:
			case DictValueType::Optional:
			case DictValueType::Struct:
			case DictValueType::VarInteger:
			case DictValueType::Function:
			{
				if (didUseOpcodeWithRef) {
					push(0, "CTOS");
				} else if (doesDictStoreValueInRef(keyType, valueType)) {
					push(0, "PLDREF");
					push(0, "CTOS");
				}
				uint32_t msk = Preload::UseCurrentSlice;
				if (resultAsSliceForStruct) msk |= Preload::ReturnStructAsSlice;
				preload(valueType, msk);
				break;
			}
			case DictValueType::TvmCell:
			{
				if (!didUseOpcodeWithRef) {
					push(0, "PLDREF");
				}
				break;
			}
		}
	};

	auto checkOnMappingOrOptional = [&]() {
		if (isIn(valueType->category(), Type::Category::Mapping, Type::Category::Optional)) {
			tuple(1);
		}
	};

	switch (decodeType) {
		case DecodeType::DecodeValue:
			preloadValue();
			break;
		case DecodeType::DecodeValueOrPushDefault: {
			startContinuation();
			preloadValue();
			endContinuation();

			bool hasEmptyPushCont = tryPollEmptyPushCont();

			startContinuation();
			bool isStructAndBuilder =
				valueType->category() == Type::Category::Struct &&
				resultAsSliceForStruct;
			pushDefaultValue(valueType, isStructAndBuilder);
			if (isStructAndBuilder) {
				push(0, "ENDC");
				push(0, "CTOS");
			}
			endContinuation(-1);

			if (hasEmptyPushCont)
				push(0, "IFNOT");
			else
				push(0, "IFELSE");
			break;
		}
		case DecodeType::DecodeValueOrPushNull: {
			push(0, "NULLSWAPIFNOT");

			startContinuation();
			preloadValue();
			if (haveKey) {
				tuple(2);
			} else {
				checkOnMappingOrOptional();
			}
			endContinuation();

			push(0, "IF");
			break;
		}
		case DecodeType::PushNullOrDecodeValue: {
			push(0, "NULLSWAPIF");

			startContinuation();
			preloadValue();
			checkOnMappingOrOptional();
			endContinuation();

			push(0, "IFNOT");
			break;
		}
	}
}

class DictSet : public DictOperation {
public:
	DictSet(
		StackPusherHelper& pusher,
		Type const &keyType,
		Type const &valueType,
		const DataType& dataType,
		StackPusherHelper::SetDictOperation operation
	) :
		DictOperation{pusher, keyType, valueType},
		dataType{dataType},
		operation{operation}
	{

	}

	void dictSet() {
		// stack: value key dict
		int keyLength = lengthOfDictKey(&keyType);
		pusher.pushInt(keyLength);
		// stack: value index dict keyBitLength
		opcode = "DICT" + typeToDictChar(&keyType);
		switch (operation) {
			case StackPusherHelper::SetDictOperation::Set:
				opcode += "SET";
				break;
			case StackPusherHelper::SetDictOperation::Replace:
				opcode += "REPLACE";
				break;
			case StackPusherHelper::SetDictOperation::Add:
				opcode += "ADD";
				break;
		}

		switch (dataType) {
			case DataType::Builder:
				opcode += "B";
				break;
			case DataType::Cell:
				opcode += "REF";
				break;
			case DataType::Slice:
				break;
		}

		switch (operation) {
			case StackPusherHelper::SetDictOperation::Set:
				pusher.push(-4 + 1, opcode);
				break;
			case StackPusherHelper::SetDictOperation::Replace:
			case StackPusherHelper::SetDictOperation::Add:
				pusher.push(-4 + 2, opcode);
				break;
		}
	}

private:
	const DataType& dataType;
	StackPusherHelper::SetDictOperation operation;
	std::string opcode;
};

void StackPusherHelper::setDict(Type const &keyType, Type const &valueType, const DataType& dataType, SetDictOperation operation) {
	DictSet d{*this, keyType, valueType, dataType, operation};
	d.dictSet();
}

void StackPusherHelper::tryPollLastRetOpcode() {
	if (m_code.lines.empty()) {
		return;
	}
	if (cmpLastCmd("RET")) {
		m_code.lines.pop_back();
	}
}

bool StackPusherHelper::tryPollConvertBuilderToSlice() {
	int n = m_code.lines.size();
	if (n >= 2 &&
		cmpLastCmd("CTOS") &&
		cmpLastCmd("ENDC", 1)
	)
	{
		m_code.lines.pop_back();
		m_code.lines.pop_back();
		return true;
	}
	return false;
}

bool StackPusherHelper::tryPollEmptyPushCont() {
	int n = m_code.lines.size();
	if (n >= 2 &&
		cmpLastCmd("PUSHCONT \\{", 1) &&
		cmpLastCmd("\\}")
	) {
		m_code.lines.pop_back();
		m_code.lines.pop_back();
		return true;
	}
	return false;
}

bool StackPusherHelper::cmpLastCmd(const std::string& cmd, int offset) {
	int n = m_code.lines.size() - 1 - offset;
	return n >= 0 &&
		std::regex_match(m_code.lines[n], std::regex("(\t*)" + cmd));
}

void StackPusherHelper::pollLastOpcode() {
	m_code.lines.pop_back();
}

bool StackPusherHelper::optimizeIf() {
	bool reverseOpcode = false;
	if (cmpLastCmd("NOT")) {
		while (cmpLastCmd("NOT")) {
			pollLastOpcode();
			reverseOpcode ^= true;
		}
	} else if (cmpLastCmd("EQINT 0")) {
		pollLastOpcode();
		reverseOpcode ^= true;
	} else if (cmpLastCmd("NEQINT 0")) {
		pollLastOpcode();
	}
	return reverseOpcode;
}

void StackPusherHelper::append(const CodeLines &oth) {
	m_code.append(oth);
}

void StackPusherHelper::addTabs(const int qty) {
	m_code.addTabs(qty);
}

void StackPusherHelper::subTabs(const int qty) {
	m_code.subTabs(qty);
}

void StackPusherHelper::pushCont(const CodeLines &cont, const string &comment) {
	if (comment.empty())
		push(0, "PUSHCONT {");
	else
		push(0, "PUSHCONT { ; " + comment);
	for (const auto& l : cont.lines)
		push(0, string("\t") + l);
	push(+1, "}"); // adjust stack // TODO delete +1. For ifelse it's a problem
}

void StackPusherHelper::generateGlobl(const string &fname, const bool isPublic) {
	push(0, ".globl\t" + fname);
	if (isPublic) {
		push(0, ".public\t" + fname);
	}
	push(0, ".type\t"  + fname + ", @function");
}

void StackPusherHelper::generateInternal(const string &fname, const int id) {
	push(0, ".internal-alias :" + fname + ",        " + toString(id));
	push(0, ".internal\t:" + fname);
}

void StackPusherHelper::generateMacro(const string &functionName) {
	push(0, ".macro " + functionName);
}

CodeLines StackPusherHelper::code() const {
	return m_code;
}

TVMCompilerContext &StackPusherHelper::ctx() {
	return *m_ctx;
}

void StackPusherHelper::push(int stackDiff, const string &cmd) {
	m_code.push(cmd);
	m_stack.change(stackDiff);
}

void StackPusherHelper::startContinuation(int deltaStack) {
	m_code.startContinuation();
	m_stack.change(deltaStack);
}

void StackPusherHelper::startIfRef(int deltaStack) {
	m_code.startIfRef();
	m_stack.change(deltaStack);
}

void StackPusherHelper::startIfNotRef(int deltaStack) {
	m_code.startIfNotRef();
	m_stack.change(deltaStack);
}

void StackPusherHelper::startCallRef(int deltaStack) {
	m_code.startCallRef();
	m_stack.change(deltaStack);
}

void StackPusherHelper::endContinuation(int deltaStack) {
	m_code.endContinuation();
	m_stack.change(deltaStack);
}

TVMStack &StackPusherHelper::getStack() {
	return m_stack;
}

void StackPusherHelper::pushLines(const std::string &lines) {
	std::istringstream stream{lines};
	std::string line;
	while (std::getline(stream, line)) {
		push(0, line);
	}
}

void StackPusherHelper::untuple(int n) {
	solAssert(0 <= n, "");
	if (n <= 15) {
		push(-1 + n, "UNTUPLE " + toString(n));
	} else {
		solAssert(n <= 255, "");
		pushInt(n);
		push(-2 + n, "UNTUPLEVAR");
	}
}

void StackPusherHelper::index(int index) {
	solAssert(0 <= index, "");
	if (index <= 15) {
		push(-1 + 1, "INDEX " + toString(index));
	} else {
		solAssert(index <= 254, "");
		pushInt(index);
		push(-2 + 1, "INDEXVAR");
	}
}

void StackPusherHelper::setIndex(int index) {
	solAssert(0 <= index, "");
	if (index <= 15) {
		push(-2 + 1, "SETINDEX " + toString(index));
	} else {
		solAssert(index <= 254, "");
		pushInt(index);
		push(-1 - 2 + 1, "SETINDEXVAR");
	}
}

void StackPusherHelper::setIndexQ(int index) {
	solAssert(0 <= index, "");
	if (index <= 15) {
		push(-2 + 1, "SETINDEXQ " + toString(index));
	} else {
		solAssert(index <= 254, "");
		pushInt(index);
		push(-1 - 2 + 1, "SETINDEXVARQ");
	}
}

void StackPusherHelper::tuple(int qty) {
	solAssert(0 <= qty, "");
	if (qty <= 15) {
		push(-qty + 1, "TUPLE " + toString(qty));
	} else {
		solAssert(qty <= 255, "");
		pushInt(qty);
		push(-1 - qty + 1, "TUPLEVAR");
	}
}

void StackPusherHelper::resetAllStateVars() {
	push(0, ";; set default state vars");
	for (VariableDeclaration const *variable: ctx().notConstantStateVariables()) {
		pushDefaultValue(variable->type());
		setGlob(variable);
	}
	push(0, ";; end set default state vars");
}

void StackPusherHelper::getGlob(VariableDeclaration const *vd) {
	const int index = ctx().getStateVarIndex(vd);
	getGlob(index);
}

void StackPusherHelper::getGlob(int index) {
	solAssert(index >= 0, "");
	if (index <= 31) {
		push(+1, "GETGLOB " + toString(index));
	} else {
		solAssert(index < 255, "");
		pushInt(index);
		push(-1 + 1, "GETGLOBVAR");
	}
}

void StackPusherHelper::setGlob(int index) {
	if (index <= 31) {
		push(-1, "SETGLOB " + toString(index));
	} else {
		solAssert(index < 255, "");
		pushInt(index);
		push(-1 - 1, "SETGLOBVAR");
	}
}

void StackPusherHelper::setGlob(VariableDeclaration const *vd) {
	const int index = ctx().getStateVarIndex(vd);
	solAssert(index >= 0, "");
	setGlob(index);
}

void StackPusherHelper::pushS(int i) {
	solAssert(i >= 0, "");
	if (i == 0) {
		push(+1, "DUP");
	} else {
		push(+1, "PUSH S" + toString(i));
	}
}

void StackPusherHelper::popS(int i) {
	solAssert(i >= 0, "");
	push(-1, "POP S" + toString(i));
}

void StackPusherHelper::pushInt(const bigint& i) {
	push(+1, "PUSHINT " + toString(i));
}

bool StackPusherHelper::fastLoad(const Type* type) {
	// slice
	switch (type->category()) {
		case Type::Category::Optional: {
			const int saveStakeSize = getStack().size();
			auto opt = to<OptionalType>(type);

			push(+1, "LDOPTREF"); // value slice
			exchange(0, 1); // slice value
			pushS(0); // slice value value
			push(-1 + 1, "ISNULL"); // slice value isNull
			push(-1, ""); // fix stack

			startContinuation();
			// slice value
			push(0, "CTOS"); // slice sliceValue
			// TODO add test
			preload(opt->valueType(), UseCurrentSlice); // slice value
			if (isIn(opt->valueType()->category(), Type::Category::Mapping, Type::Category::Optional)) {
				tuple(1);
			}
			endContinuation();

			push(0, "IFNOT");

			solAssert(saveStakeSize + 1 == getStack().size(), "");
			return false;
		}
		case Type::Category::TvmCell:
			push(-1 + 2, "LDREF");
			return true;
		case Type::Category::Struct: {
			push(+1, "LDREFRTOS");
			// slice structAsSlice
			auto st = to<StructType>(type);
			StructCompiler sc{this, st};
			sc.convertSliceToTuple();
			return false;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			push(-1 + 2, "LDMSGADDR");
			return true;
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedPoint:
		case Type::Category::FixedBytes: {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			string cmd = ti.isSigned ? "LDI " : "LDU ";
			push(-1 + 2, cmd + toString(ti.numBits));
			return true;
		}
		case Type::Category::Function: {
			push(-1 + 2, "LDU 32");
			return true;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				push(-1 + 2, "LDREF");
				return true;
			} else {
				push(-1 + 2, "LDU 32");
				push(-1 + 2, "LDDICT");
				push(0, "ROTREV");
				push(-2 + 1, "PAIR");
				return false;
			}
		}
		case Type::Category::Mapping:
			push(-1 + 2, "LDDICT");
			return true;
		default:
			solUnimplemented(type->toString());
	}
}

void StackPusherHelper::load(const Type *type, bool reverseOrder) {
	// slice
	bool directOrder = fastLoad(type);
	if (directOrder == reverseOrder) {
		exchange(0, 1);
	}
	// reverseOrder? slice member : member slice
}

void StackPusherHelper::preload(const Type *type, uint32_t mask) {
	const int stackSize = getStack().size();
	// on stack there is slice
	switch (type->category()) {
		case Type::Category::Optional: {
			auto opt = to<OptionalType>(type);

			pushS(0);
			push(-1 + 1, "PLDI 1"); // slice hasVal

			push(-1, ""); // fix stack

			// have value
			int savedStake0 = getStack().size();
			startContinuation();
			// stack: slice
			push(-1 + 1, "PLDREF");
			push(-1 + 1, "CTOS");
			preload(opt->valueType(),UseCurrentSlice);
			if (isIn(opt->valueType()->category(), Type::Category::Mapping, Type::Category::Optional)) {
				tuple(1);
			}
			endContinuation();
			getStack().ensureSize(savedStake0);

			// no value
			int savedStake1 = getStack().size();
			startContinuation();
			// stack: slice
			drop();
			push(+1, "NULL");
			endContinuation();
			getStack().ensureSize(savedStake1);

			push(0, "IFELSE");

			break;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			if (!(mask & IsAddressInEnd)) {
				push(-1 + 2, "LDMSGADDR");
				drop(1);
			}
			break;
		case Type::Category::TvmCell:
			push(0, "PLDREF");
			break;
		case Type::Category::Struct:
			if (!(mask & UseCurrentSlice)){
				push(-1 + 2, "LDREFRTOS");
				push(-1, "NIP");
			}
			if (!(mask & ReturnStructAsSlice)) {
				auto structType = to<StructType>(type);
				StructCompiler sc{this, structType};
				sc.convertSliceToTuple();
			}
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedPoint:
		case Type::Category::FixedBytes: {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			string cmd = ti.isSigned ? "PLDI " : "PLDU ";
			push(-1 + 1, cmd + toString(ti.numBits));
			break;
		}
		case Type::Category::Function: {
			push(-1 + 1, "PLDU 32");
			break;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				push(0, "PLDREF");
			} else {
				push(-1 + 2, "LDU 32");
				push(-1 + 1, "PLDDICT");
				push(-2 + 1, "PAIR");
				// stack: array
			}
			break;
		}
		case Type::Category::Mapping:
		case Type::Category::ExtraCurrencyCollection:
			push(-1 + 1, "PLDDICT");
			break;
		case Type::Category::VarInteger:
			push(0, "LDVARUINT32");
			push(0, "DROP");
			break;
		case Type::Category::Tuple: {
			const auto[types, names] = getTupleTypes(to<TupleType>(type));
			StructCompiler sc{this, types, names, 0, false};
			sc.convertSliceToTuple();
			break;
		}
		default:
			solUnimplemented("Decode isn't supported for " + type->toString(true));
	}
	getStack().ensureSize(stackSize);
}

void StackPusherHelper::store(
	const Type *type,
	bool reverse,
	uint32_t mask
) {
	// value   builder  -> reverse = false
	// builder value    -> reverse = true
	const int stackSize = getStack().size();
	int deltaStack = 1;
	switch (type->category()) {
		case Type::Category::Optional: {
			auto optType = to<OptionalType>(type);

			if (!reverse)
				exchange(0, 1);	// builder value
			pushS(0);	// builder value value
			push(-1 + 1, "ISNULL");	// builder value isnull
			push(-1 + 1, "NOT");	// builder value !isnull

			push(-1, ""); // fix stack
			getStack().ensureSize(stackSize);

			startContinuation();
			// builder value
			if (isIn(optType->valueType()->category(), Type::Category::Optional, Type::Category::Mapping)) {
				untuple(1);
			}
			push(+1, "NEWC"); // builder value builder
			store(optType->valueType(), false, 0); // builder builderWithValue
			exchange(0, 1); // builderWithValue builder
			stones(1);
			push(-1, "STBREF");	// builder
			endContinuation();
			push(+1, ""); // fix stack
			getStack().ensureSize(stackSize);

			startContinuation();
			// builder value
			drop(1); // builder
			stzeroes(1);
			endContinuation();
			push(+1, ""); // fix stack

			push(0, "IFELSE");
			push(-1, ""); // fix stack

			break;
		}
		case Type::Category::TvmCell:
			push(-1, reverse? "STREFR" : "STREF"); // builder
			break;
		case Type::Category::Struct: {
			auto structType = to<StructType>(type);
			if (mask & StoreFlag::StoreStructInOneCell) {
				if (!reverse)
					push(0, "SWAP");
				auto members = structType->structDefinition().members();
				untuple(members.size());
				this->reverse(members.size(), 0);
				blockSwap(1, members.size());
				for (const auto& member : members)
					store(member->type(), false, StackPusherHelper::StoreFlag::StoreStructInRef | StackPusherHelper::StoreFlag::StoreStructInOneCell);
			} else if (mask & StoreFlag::ValueIsBuilder) {
				if (mask & StoreFlag::StoreStructInRef)
					push(-1, reverse? "STBREFR" : "STBREF"); // builder
				else
					solUnimplemented("TODO");
			} else {
				if (!reverse) {
					push(0, "SWAP"); // builder struct
				}
				// builder struct
				StructCompiler sc{this, structType};
				sc.tupleToBuilder();
				if (mask & StoreFlag::StoreStructInRef)
					push(-1, "STBREFR"); // builder
				else {
					push(-1, "STBR"); // builder
					// TODO opt this: don't create builder in `tupleToBuilder` and don't append this.
				}
			}
			break;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
		case Type::Category::TvmSlice:
			push(-1, reverse? "STSLICER" : "STSLICE"); // builder slice-value
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
		case Type::Category::FixedPoint:
			if (mask & StoreFlag::ValueIsBuilder) {
				push(-1, reverse ? "STBR" : "STB");
			} else {
				push(-1, storeIntegralOrAddress(type, reverse));
			}
			break;
		case Type::Category::Function: {
			if (mask & StoreFlag::ValueIsBuilder) {
				push(-1, reverse ? "STBR" : "STB");
				solUnimplemented("");
			} else {
				push(-1, reverse ? "STUR 32" : "STU 32");
			}
			break;
		}
		case Type::Category::Mapping:
		case Type::Category::ExtraCurrencyCollection:
			if (reverse) {
				push(0, "SWAP"); // builder dict
			}
			// dict builder
			push(-1, "STDICT"); // builder
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				push(-1, reverse? "STREFR" : "STREF"); // builder
			} else {
				if (mask & StoreFlag::ValueIsBuilder) {
					solAssert(!(mask & StoreFlag::ArrayIsUntupled), "");
					push(-1, reverse ? "STBR" : "STB");
				} else {
					if (mask & StoreFlag::ArrayIsUntupled) {
						deltaStack = 2;
						solAssert(!reverse, "");
						// dict size builder
						push(-1, "STU 32");
						push(-1, "STDICT");
					} else {
						if (!reverse) {
							push(0, "SWAP"); // builder arr
						}
						push(-1 + 2, "UNPAIR"); // builder size dict
						push(0, "ROTREV"); // dict builder size
						push(-1, "STUR 32"); // dict builder'
						push(-1, "STDICT"); // builder''
					}
				}
			}
			break;
		}
		case Type::Category::TvmBuilder:
			push(-1, std::string("STB")  + (reverse ? "R " : ""));
			break;
		case Type::Category::Tuple: {
			if (!reverse)
				exchange(0, 1);	// builder value

			const auto[types, names] = getTupleTypes(to<TupleType>(type));
			StructCompiler sc{this, types, names, 0, false};
			sc.tupleToBuilder();
			push(-2 + 1, "STBR");
			break;
		}
		case Type::Category::VarInteger: {
			if (!reverse)
				exchange(0, 1);	// builder value

			push(-1, "STVARUINT32"); // builder
			break;
		}
		default: {
			solUnimplemented("Encode isn't supported for " + type->toString(true));
		}
	}

	getStack().ensureSize(stackSize - deltaStack);
}

void StackPusherHelper::pushZeroAddress() {
	push(+1, "PUSHSLICE x8000000000000000000000000000000000000000000000000000000000000000001_");
}

void StackPusherHelper::addBinaryNumberToString(std::string &s, u256 value, int bitlen) {
	for (int i = 0; i < bitlen; ++i) {
		s += value % 2 == 0? "0" : "1";
		value /= 2;
	}
	std::reverse(s.rbegin(), s.rbegin() + bitlen);
}

std::string StackPusherHelper::binaryStringToSlice(const std::string &_s) {
	std::string s = _s;
	bool haveCompletionTag = false;
	if (s.size() % 4 != 0) {
		haveCompletionTag = true;
		s += "1";
		s += std::string((4 - s.size() % 4) % 4, '0');
	}
	std::string ans;
	for (int i = 0; i < static_cast<int>(s.length()); i += 4) {
		int x = stoi(s.substr(i, 4), nullptr, 2);
		std::stringstream sstream;
		sstream << std::hex << x;
		ans += sstream.str();
	}
	if (haveCompletionTag) {
		ans += "_";
	}
	return ans;
}

std::string StackPusherHelper::tonsToBinaryString(Literal const *literal) {
	Type const* type = literal->annotation().type;
	u256 value = type->literalValue(literal);
	return tonsToBinaryString(value);
}

std::string StackPusherHelper::tonsToBinaryString(const u256& value) {
	return tonsToBinaryString(bigint(value));
}

std::string StackPusherHelper::tonsToBinaryString(bigint value) {
	std::string s;
	int len = 256;
	for (int i = 0; i < 256; ++i) {
		if (value == 0) {
			len = i;
			break;
		}
		s += value % 2 == 0? "0" : "1";
		value /= 2;
	}
	solAssert(len < 120, "Ton value should fit 120 bit");
	while (len % 8 != 0) {
		s += "0";
		len++;
	}
	std::reverse(s.rbegin(), s.rbegin() + len);
	len = len/8;
	std::string res;
	for (int i = 0; i < 4; ++i) {
		res += len % 2 == 0? "0" : "1";
		len /= 2;
	}
	std::reverse(res.rbegin(), res.rbegin() + 4);
	return res + s;
}

std::string StackPusherHelper::literalToSliceAddress(Literal const *literal, bool pushSlice) {
	Type const* type = literal->annotation().type;
	u256 value = type->literalValue(literal);
//		addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 address:bits256 = MsgAddressInt;
	std::string s;
	s += "10";
	s += "0";
	s += std::string(8, '0');
	addBinaryNumberToString(s, value);
	if (pushSlice)
		push(+1, "PUSHSLICE x" + binaryStringToSlice(s));
	return s;
}

bigint StackPusherHelper::pow10(int power) {
	bigint r = 1;
	for (int i = 1; i <= power; ++i) {
		r *= 10;
	}
	return r;
}

void StackPusherHelper::hardConvert(Type const *leftType, Type const *rightType) {
	// case opt(T) = T
	if (leftType->category() == Type::Category::Optional && *leftType != *rightType) {
		auto l = to<OptionalType>(leftType);
		hardConvert(l->valueType(), rightType);
		if (isIn(l->valueType()->category(), Type::Category::Mapping, Type::Category::Optional)) {
			tuple(1);
		}
		return;
	}



	bool impl = rightType->isImplicitlyConvertibleTo(*leftType);

	auto fixedPointFromFixedPoint = [this, impl](FixedPointType const* l, FixedPointType const* r) {
		int powerDiff = l->fractionalDigits() - r->fractionalDigits();
		if (powerDiff != 0) {
			if (powerDiff > 0) {
				pushInt(pow10(powerDiff));
				push(-2 + 1, "MUL");
			} else {
				pushInt(pow10(-powerDiff));
				push(-2 + 1, "DIV");
			}
		}
		if (!impl)
			checkFit(l);
	};

	auto integerFromFixedPoint = [this, impl](IntegerType const* l, FixedPointType const* r) {
		int powerDiff = r->fractionalDigits();
		if (powerDiff > 0) {
			pushInt(pow10(powerDiff));
			push(-2 + 1, "DIV");
		}
		if (!impl)
			checkFit(l);
	};

	auto integerFromInteger = [this, impl](IntegerType const* l, IntegerType const* /*r*/) {
		if (!impl)
			checkFit(l);
	};

	auto fixedPointFromInteger = [this, impl](FixedPointType const* l, IntegerType const* /*r*/) {
		int powerDiff = l->fractionalDigits();
		if (powerDiff > 0) {
			pushInt(pow10(powerDiff));
			push(-2 + 1, "MUL");
		}
		if (!impl)
			checkFit(l);
	};

	auto fixedBytesFromFixedBytes = [this](FixedBytesType const* l, FixedBytesType const* r) {
		int diff = 8 * (l->numBytes() - r->numBytes());
		if (diff > 0) {
			push(0, "LSHIFT " + std::to_string(diff));
		} else if (diff < 0) {
			push(0, "RSHIFT " + std::to_string(-diff));
		}
	};

	auto fixedBytesFromStringLiteral = [this](FixedBytesType const* l, StringLiteralType const* r) {
		size_t bytes = 0;
		u256 value = 0;
		for (char c : r->value()) {
			value = value * 256 + c;
			++bytes;
		}
		while (bytes < l->numBytes()) {
			value *= 256;
			++bytes;
		}
		drop(1); // delete old value
		push(+1, "PUSHINT " + toString(value));
	};

	auto fromFixedPoint = [&](FixedPointType const* r) {
		switch (leftType->category()) {
			case Type::Category::FixedPoint:
				fixedPointFromFixedPoint(to<FixedPointType>(leftType), r);
				break;
			case Type::Category::Integer:
				integerFromFixedPoint(to<IntegerType>(leftType), r);
				break;
			default:
				solUnimplemented("");
				break;
		}
	};

	auto fromInteger = [&](IntegerType const* r) {
		switch (leftType->category()) {
			case Type::Category::FixedPoint:
				fixedPointFromInteger(to<FixedPointType>(leftType), r);
				break;
			case Type::Category::Integer:
				integerFromInteger(to<IntegerType>(leftType), r);
				break;
			case Type::Category::FixedBytes:
				// nothing do here
				break;
			case Type::Category::Address:
				solUnimplemented("See FunctionCallCompiler::typeConversion");
				break;
			default:
				solUnimplemented(leftType->toString());
				break;
		}
	};
	
	
	
	
	switch (rightType->category()) {

		case Type::Category::RationalNumber: {
			Type const* mt = rightType->mobileType();
			if (mt->category() == Type::Category::Integer) {
				fromInteger(to<IntegerType>(mt));
			} else if (mt->category() == Type::Category::FixedPoint) {
				fromFixedPoint(to<FixedPointType>(mt));
			} else {
				solUnimplemented("");
			}
			break;
		}

		case Type::Category::FixedPoint: {
			fromFixedPoint(to<FixedPointType>(rightType));
			break;
		}

		case Type::Category::Integer: {
			fromInteger(to<IntegerType>(rightType));
			break;
		}

		case Type::Category::FixedBytes: {
			auto r = to<FixedBytesType>(rightType);
			switch (leftType->category()) {
				case Type::Category::FixedBytes:
					fixedBytesFromFixedBytes(to<FixedBytesType>(leftType), r);
					break;
				default:
					solUnimplemented("");
					break;
			}
			break;
		}


		case Type::Category::Array: {
			auto r = to<ArrayType>(rightType);
			if (!r->isByteArray()) {
				break;
			}
			// bytes or string
			switch (leftType->category()) {
				case Type::Category::Array:
					break;
				default:
					solUnimplemented("");
					break;
			}
			break;
		}

		case Type::Category::Address:
		case Type::Category::Bool:
		case Type::Category::Contract:
		case Type::Category::Enum:
		case Type::Category::ExtraCurrencyCollection:
		case Type::Category::Function:
		case Type::Category::Mapping:
		case Type::Category::Optional: // !!!
		case Type::Category::Struct:
		case Type::Category::TvmBuilder:
		case Type::Category::TvmCell:
		case Type::Category::TvmSlice:
			break;

		case Type::Category::Tuple:
			// opt(fixed32x5, fixed32x5) a;
//			 a.get() = (1, 2, 4);
			break;

		case Type::Category::StringLiteral: {
			auto r = to<StringLiteralType>(rightType);
			switch (leftType->category()) {
				case Type::Category::FixedBytes:
					fixedBytesFromStringLiteral(to<FixedBytesType>(leftType), r);
					break;
				case Type::Category::Array:
					break;
				default:
					solUnimplemented(leftType->toString());
					break;
			}
			break;
		}

		default:
			solUnimplemented(rightType->toString());
			break;
	}
}

void StackPusherHelper::checkFit(Type const *type) {
	switch (type->category()) {

		case Type::Category::Integer: {
			auto it = to<IntegerType>(type);
			if (it->isSigned()) {
				push(0, "FITS " + toString(it->numBits()));
			} else {
				push(0, "UFITS " + toString(it->numBits()));
			}
			break;
		}

		case Type::Category::FixedPoint: {
			auto fp = to<FixedPointType>(type);
			if (fp->isSigned()) {
				push(0, "FITS " + toString(fp->numBits()));
			} else {
				push(0, "UFITS " + toString(fp->numBits()));
			}
			break;
		}

		default:
			solUnimplemented("");
			break;
	}

}

void StackPusherHelper::push(const CodeLines &codeLines) {
	for (const std::string& s : codeLines.lines) {
		push(0, s);
	}
}

void StackPusherHelper::pushMacroCallInCallRef(int stackDelta, const string& fname) {
	startCallRef();
	push(stackDelta, "CALL $" + fname + "$");
	endContinuation();
}

void StackPusherHelper::pushPrivateFunctionOrMacroCall(const int stackDelta, const string &fname) {
	if (boost::ends_with(fname, "_macro")) {
		pushMacroCallInCallRef(stackDelta, fname);
	} else {
		push(stackDelta, "CALL $" + fname + "$");
	}
}

void StackPusherHelper::pushCall(const string &functionName, const FunctionType *ft) {
	int params  = ft->parameterTypes().size();
	int retVals = ft->returnParameterTypes().size();
	pushPrivateFunctionOrMacroCall(-params + retVals, functionName);
}

void StackPusherHelper::drop(int cnt) {
	solAssert(cnt >= 0, "");
	if (cnt == 0)
		return;

	if (cnt == 1) {
		push(-1, "DROP");
	} else if (cnt == 2) {
		push(-2, "DROP2");
	} else {
		if (cnt > 15) {
			pushInt(cnt);
			push(-(cnt + 1), "DROPX");
		} else {
			push(-cnt, "BLKDROP " + toString(cnt));
		}
	}
}

void StackPusherHelper::blockSwap(int m, int n) {
	solAssert(0 <= m, "");
	solAssert(0 <= n, "");
	if (m == 0 || n == 0) {
		return;
	}
	if (m == 1 && n == 1) {
		exchange(0, 1);
	} else if (m == 1 && n == 2) {
		push(0, "ROT");
	} else if (m == 2 && n == 1) {
		push(0, "ROTREV");
	} else if (m == 2 && n == 2) {
		push(0, "SWAP2");
	} else if (n <= 16 && m <= 16) {
		push(0, "BLKSWAP " + toString(m) + ", " + toString(n));
	} else {
		pushInt(m);
		pushInt(n);
		push(-2, "BLKSWX");
	}
}

void StackPusherHelper::reverse(int i, int j) {
	solAssert(i >= 2, "");
	solAssert(j >= 0, "");
	if (i == 2 && j == 0) {
		push(0, "SWAP");
	} else if (i == 3 && j == 0) {
		push(0, "XCHG s2");
	} else if (i - 2 <= 15 && j <= 15) {
		push(0, "REVERSE " + toString(i) + ", " + toString(j));
	} else {
		pushInt(i);
		pushInt(j);
		push(-2, "REVX");
	}
}

void StackPusherHelper::dropUnder(int leftCount, int droppedCount) {
	// drop dropCount elements that are situated under top leftCount elements
	solAssert(leftCount >= 0, "");
	solAssert(droppedCount >= 0, "");

	auto f = [this, leftCount, droppedCount](){
		if (droppedCount > 15 || leftCount > 15) {
			pushInt(droppedCount);
			pushInt(leftCount);
			push(-2, "BLKSWX");
			drop(droppedCount);
		} else {
			push(-droppedCount, "BLKDROP2 " + toString(droppedCount) + ", " + toString(leftCount));
		}
	};

	if (droppedCount == 0) {
		// nothing do
	} else if (leftCount == 0) {
		drop(droppedCount);
	} else if (droppedCount == 1) {
		if (leftCount == 1) {
			push(-1, "NIP");
		} else {
			f();
		}
	} else if (droppedCount == 2) {
		if (leftCount == 1) {
			push(-1, "NIP");
			push(-1, "NIP");
		} else {
			f();
		}
	} else {
		if (leftCount == 1) {
			exchange(0, droppedCount);
			drop(droppedCount);
		} else {
			f();
		}
	}
}

void StackPusherHelper::exchange(int i, int j) {
	solAssert(i <= j, "");
	solAssert(i >= 0, "");
	solAssert(j >= 1, "");
	if (i == 0 && j <= 255) {
		if (j == 1) {
			push(0, "SWAP");
		} else if (j <= 15) {
			push(0, "XCHG s" + toString(j));
		} else {
			push(0, "XCHG s0,s" + toString(j));
		}
	} else if (i == 1 && 2 <= j && j <= 15) {
		push(0, "XCHG s1,s" + toString(j));
	} else if (1 <= i && i < j && j <= 15) {
		push(0, "XCHG s" + toString(i) + ",s" + toString(j));
	} else if (j <= 255) {
		exchange(0, i);
		exchange(0, j);
		exchange(0, i);
	} else {
		solUnimplemented("");
	}
}

TypePointer StackPusherHelper::parseIndexType(Type const *type) {
	if (to<ArrayType>(type)) {
		return TypePointer(new IntegerType(32));
	}
	if (auto mappingType = to<MappingType>(type)) {
		return mappingType->keyType();
	}
	if (auto currencyType = to<ExtraCurrencyCollectionType>(type)) {
		return currencyType->keyType();
	}
	solUnimplemented("");
}

TypePointer StackPusherHelper::parseValueType(IndexAccess const &indexAccess) {
	if (auto currencyType = to<ExtraCurrencyCollectionType>(indexAccess.baseExpression().annotation().type)) {
		return currencyType->realValueType();
	}
	return indexAccess.annotation().type;
}

bool StackPusherHelper::tryAssignParam(Declaration const *name) {
	auto& stack = getStack();
	if (stack.isParam(name)) {
		int idx = stack.getOffset(name);
		solAssert(idx >= 0, "");
		if (idx == 0) {
			// nothing
		} else if (idx == 1) {
			push(-1, "NIP");
		} else {
			popS(idx);
		}
		return true;
	}
	return false;
}

void StackPusherHelper::prepareKeyForDictOperations(Type const *key, bool doIgnoreBytes) {
	// stack: key
	if (isStringOrStringLiteralOrBytes(key) || key->category() == Type::Category::TvmCell) {
		if (!doIgnoreBytes) {
			push(-1 + 1, "HASHCU");
		}
	} else if (key->category() == Type::Category::Struct) {
		StructCompiler sc{this, to<StructType>(key)};
		sc.tupleToBuilder();
		push(0, "ENDC");
		push(0, "CTOS");
	} else {
	}
}

int StackPusherHelper::int_msg_info(const std::set<int> &isParamOnStack, const std::map<int, std::string> &constParams) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//                 src:MsgAddress  dest:MsgAddressInt(#4)
	//                 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//                 created_lt:uint64  created_at:uint32
	//                 = CommonMsgInfoRelaxed;

	// currencies$_ grams:Grams other:ExtraCurrencyCollection = CurrencyCollection;

	const std::vector<int> zeroes {1, 1, 1,
									2, 2,
									4, 1, 4, 4,
									64, 32};
	std::string bitString = "0";
	int maxBitStringSize = 0;
	push(+1, "NEWC");
	for (int param = 0; param < static_cast<int>(zeroes.size()); ++param) {
		solAssert(constParams.count(param) == 0 || isParamOnStack.count(param) == 0, "");

		if (constParams.count(param) != 0) {
			bitString += constParams.at(param);
		} else if (isParamOnStack.count(param) == 0) {
			bitString += std::string(zeroes[param], '0');
			solAssert(param != TvmConst::int_msg_info::dest, "");
		} else {
			maxBitStringSize += bitString.size();
			appendToBuilder(bitString);
			bitString = "";
			switch (param) {
				case TvmConst::int_msg_info::bounce:
					push(-1, "STI 1");
					++maxBitStringSize;
					break;
				case TvmConst::int_msg_info::dest:
					push(-1, "STSLICE");
					maxBitStringSize += AddressInfo::maxBitLength();
					break;
				case TvmConst::int_msg_info::tons:
					exchange(0, 1);
					push(-1, "STGRAMS");
					maxBitStringSize += 4 + 16 * 8;
					// var_uint$_ {n:#} len:(#< n) value:(uint (len * 8)) = VarUInteger n;
					// nanograms$_ amount:(VarUInteger 16) = Grams;
					break;
				case TvmConst::int_msg_info::currency:
					push(-1, "STDICT");
					++maxBitStringSize;
					break;
				default:
					solUnimplemented("");
			}
		}
	}
	maxBitStringSize += bitString.size();
	appendToBuilder(bitString);
	bitString = "";
	return maxBitStringSize;
}

int StackPusherHelper::ext_msg_info(const set<int> &isParamOnStack, bool isOut = true) {
	// ext_in_msg_info$10 src:MsgAddressExt dest:MsgAddressInt
	// import_fee:Grams = CommonMsgInfo;
	//
	// ext_out_msg_info$11 src:MsgAddressInt dest:MsgAddressExt
	// created_lt:uint64 created_at:uint32 = CommonMsgInfo;

	std::vector<int> zeroes {2, 2};
	if (isOut) {
		zeroes.push_back(64);
		zeroes.push_back(32);
	} else {
		zeroes.push_back(4);
	}
	std::string bitString = isOut ? "11" : "10";
	int maxBitStringSize = 0;
	push(+1, "NEWC");
	for (int param = 0; param < static_cast<int>(zeroes.size()); ++param) {
		if (isParamOnStack.count(param) == 0) {
			bitString += std::string(zeroes.at(param), '0');
		} else {
			maxBitStringSize += bitString.size();
			appendToBuilder(bitString);
			bitString = "";
			if (param == TvmConst::ext_msg_info::dest) {
				push(-1, "STSLICE");
				maxBitStringSize += AddressInfo::maxBitLength();
			} else if (param == TvmConst::ext_msg_info::src) {
				push(-1, "STB");
				maxBitStringSize += TvmConst::ExtInboundSrcLength;
			} else {
				solUnimplemented("");
			}
		}
	}
	maxBitStringSize += bitString.size();
	appendToBuilder(bitString);
	bitString = "";
	return maxBitStringSize;
}


void StackPusherHelper::appendToBuilder(const std::string &bitString) {
	// stack: builder
	if (bitString.empty()) {
		return;
	}

	size_t count = std::count_if(bitString.begin(), bitString.end(), [](char c) { return c == '0'; });
	if (count == bitString.size()) {
		stzeroes(count);
	} else {
		const std::string hex = binaryStringToSlice(bitString);
		if (hex.length() * 4 <= 8 * 7 + 1) {
			push(0, "STSLICECONST x" + hex);
		} else {
			push(+1, "PUSHSLICE x" + binaryStringToSlice(bitString));
			push(-1, "STSLICER");
		}
	}
}

void StackPusherHelper::checkOptionalValue() {
	push(-1 + 1, "ISNULL");
	push(-1, "THROWIF " + toString(TvmConst::RuntimeException::GetOptionalException));
}

void StackPusherHelper::stzeroes(int qty) {
	if (qty > 0) {
		// builder
		if (qty == 1) {
			push(0, "STSLICECONST 0");
		} else {
			pushInt(qty); // builder qty
			push(-1, "STZEROES");
		}
	}
}

void StackPusherHelper::stones(int qty) {
	if (qty > 0) {
		// builder
		if (qty == 1) {
			push(0, "STSLICECONST 1");
		} else {
			pushInt(qty); // builder qty
			push(-1, "STONES");
		}
	}
}

void StackPusherHelper::sendrawmsg() {
	push(-2, "SENDRAWMSG");
}

void StackPusherHelper::sendIntMsg(const std::map<int, Expression const *> &exprs,
								   const std::map<int, std::string> &constParams,
								   const std::function<void(int)> &appendBody,
								   const std::function<void()> &pushSendrawmsgFlag) {
	std::set<int> isParamOnStack;
	for (auto &[param, expr] : exprs | boost::adaptors::reversed) {
		isParamOnStack.insert(param);
		TVMExpressionCompiler{*this}.compileNewExpr(expr);
	}
	sendMsg(isParamOnStack, constParams, appendBody, nullptr, pushSendrawmsgFlag);
}



void StackPusherHelper::prepareMsg(const std::set<int>& isParamOnStack,
								const std::map<int, std::string> &constParams,
								const std::function<void(int)> &appendBody,
								const std::function<void()> &appendStateInit,
								MsgType messageType) {
	int msgInfoSize = 0;
	switch (messageType) {
		case MsgType::Internal:
			msgInfoSize = int_msg_info(isParamOnStack, constParams);
			break;
		case MsgType::ExternalOut:
			msgInfoSize = ext_msg_info(isParamOnStack);
			break;
		case MsgType::ExternalIn:
			msgInfoSize = ext_msg_info(isParamOnStack, false);
			break;
	}
	// stack: builder

	if (appendStateInit) {
		// stack: values... builder
		appendToBuilder("1");
		appendStateInit();
		++msgInfoSize;
		// stack: builder-with-stateInit
	} else {
		appendToBuilder("0"); // there is no StateInit
	}

	++msgInfoSize;

	if (appendBody) {
		// stack: values... builder
		appendBody(msgInfoSize);
		// stack: builder-with-body
	} else {
		appendToBuilder("0"); // there is no body
	}

	// stack: builder'
	push(0, "ENDC"); // stack: cell
}

void StackPusherHelper::sendMsg(const std::set<int>& isParamOnStack,
								const std::map<int, std::string> &constParams,
								const std::function<void(int)> &appendBody,
								const std::function<void()> &appendStateInit,
								const std::function<void()> &pushSendrawmsgFlag,
								MsgType messageType) {
	prepareMsg(isParamOnStack, constParams, appendBody, appendStateInit, messageType);
	if (pushSendrawmsgFlag) {
		pushSendrawmsgFlag();
	} else {
		pushInt(TvmConst::SENDRAWMSG::DefaultFlag);
	}
	sendrawmsg();
}

int TVMStack::size() const {
	return m_size;
}

void TVMStack::change(int diff) {
	m_size += diff;
	solAssert(m_size >= 0, "");
}

bool TVMStack::isParam(Declaration const *name) const {
	return m_params.count(name) > 0;
}

void TVMStack::add(Declaration const *name, bool doAllocation) {
	solAssert(name != nullptr, "");
	solAssert(m_params.count(name) == 0, "");
	m_params[name] = doAllocation? m_size++ : m_size - 1;
}

int TVMStack::getOffset(Declaration const *name) const {
	solAssert(isParam(name), "");
	return getOffset(m_params.at(name));
}

int TVMStack::getOffset(int stackPos) const {
	return m_size - 1 - stackPos;
}

int TVMStack::getStackSize(Declaration const *name) const {
	return m_params.at(name);
}

void TVMStack::ensureSize(int savedStackSize, const string &location, const ASTNode* node) const {
	if (node != nullptr && savedStackSize != m_size) {
		cast_error(*node, string{} + "Stake size error: expected: " + toString(savedStackSize)
								   + " but real: " + toString(m_size) + " at " + location);
	}
	solAssert(savedStackSize == m_size, "stack: exp:" + toString(savedStackSize)
				+ " real: " + toString(m_size) + " at " + location);
}

string CodeLines::str(const string &indent) const {
	std::ostringstream o;
	for (const string& s : lines) {
		o << indent << s << endl;
	}
	return o.str();
}

void CodeLines::addTabs(const int qty) {
	tabQty += qty;
}

void CodeLines::subTabs(const int qty) {
	tabQty -= qty;
}

void CodeLines::startContinuation() {
	push("PUSHCONT {");
	++tabQty;
}

void CodeLines::startIfRef() {
	push("IFREF {");
	++tabQty;
}

void CodeLines::startIfNotRef() {
	push("IFNOTREF {");
	++tabQty;
}

void CodeLines::startCallRef() {
	push("CALLREF {");
	++tabQty;
}

void CodeLines::endContinuation() {
	--tabQty;
	push("}");
	solAssert(tabQty >= 0, "");
}

void CodeLines::push(const string &cmd) {
	if (cmd.empty() || cmd == "\n") {
		return;
	}

	// space means empty line
	if (cmd == " ")
		lines.emplace_back("");
	else {
		solAssert(tabQty >= 0, "");
		lines.push_back(std::string(tabQty, '\t') + cmd);
	}
}

void CodeLines::append(const CodeLines &oth) {
	for (const auto& s : oth.lines) {
		lines.push_back(std::string(tabQty, '\t') + s);
	}
}

void TVMCompilerContext::initMembers(ContractDefinition const *contract) {
	solAssert(!m_contract, "");
	m_contract = contract;

	for (ContractDefinition const* base : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const* f : base->definedFunctions()) {
			ignoreIntOverflow |= f->name() == "tvm_ignore_integer_overflow";
		}
	}

	ignoreIntOverflow |= m_pragmaHelper.haveIgnoreIntOverflow();
	for (VariableDeclaration const *variable: notConstantStateVariables()) {
		m_stateVarIndex[variable] = TvmConst::C7::FirstIndexForVariables + m_stateVarIndex.size();
	}
}

TVMCompilerContext::TVMCompilerContext(ContractDefinition const *contract,
									   PragmaDirectiveHelper const &pragmaHelper) : m_pragmaHelper{pragmaHelper} {
	initMembers(contract);
}

int TVMCompilerContext::getStateVarIndex(VariableDeclaration const *variable) const {
	return m_stateVarIndex.at(variable);
}

std::vector<VariableDeclaration const *> TVMCompilerContext::notConstantStateVariables() const {
	std::vector<VariableDeclaration const*> variableDeclarations;
	std::vector<ContractDefinition const*> mainChain = getContractsChain(getContract());
	for (ContractDefinition const* contract : mainChain) {
		for (VariableDeclaration const *variable: contract->stateVariables()) {
			if (!variable->isConstant()) {
				variableDeclarations.push_back(variable);
			}
		}
	}
	return variableDeclarations;
}

std::vector<Type const *> TVMCompilerContext::notConstantStateVariableTypes() const {
	std::vector<Type const *> types;
	for (VariableDeclaration const * var : notConstantStateVariables()) {
		types.emplace_back(var->type());
	}
	return types;
}

std::vector<std::string> TVMCompilerContext::notConstantStateVariableNames() const {
	std::vector<std::string> names;
	for (VariableDeclaration const * var : notConstantStateVariables()) {
		names.emplace_back(var->name());
	}
	return names;
}

PragmaDirectiveHelper const &TVMCompilerContext::pragmaHelper() const {
	return m_pragmaHelper;
}

bool TVMCompilerContext::haveTimeInAbiHeader() const {
	if (m_pragmaHelper.abiVersion() == 1) {
		return true;
	}
	if (m_pragmaHelper.abiVersion() == 2) {
		return m_pragmaHelper.haveTime() || afterSignatureCheck() == nullptr;
	}
	solUnimplemented("");
}

bool TVMCompilerContext::isStdlib() const {
	return m_contract->name() == "stdlib";
}

string TVMCompilerContext::getFunctionInternalName(FunctionDefinition const* _function) const {
	if (isStdlib()) {
		return _function->name();
	}
	if (_function->name() == "onCodeUpgrade") {
		return ":onCodeUpgrade";
	}
	if (_function->isFallback()) {
		return "fallback";
	}

	string name = _function->name() + "_internal";
	if (_function != getContractFunctions(getContract(), _function->name()).back()) {
		name = _function->annotation().contract->name() + "_" + _function->name();
	}
	return name;
}

string TVMCompilerContext::getFunctionExternalName(FunctionDefinition const *_function) {
	const string& fname = _function->name();
	solAssert(_function->isPublic(), "Internal error: expected public function: " + fname);
	if (_function->isConstructor()) {
		return "constructor";
	}
	if (_function->isFallback()) {
		return "fallback";
	}
	return fname;
}

const ContractDefinition *TVMCompilerContext::getContract() const {
	return m_contract;
}

bool TVMCompilerContext::ignoreIntegerOverflow() const {
	return ignoreIntOverflow;
}

FunctionDefinition const *TVMCompilerContext::afterSignatureCheck() const {
	for (FunctionDefinition const* f : m_contract->definedFunctions()) {
		if (f->name() == "afterSignatureCheck") {
			return f;
		}
	}
	return nullptr;
}

bool TVMCompilerContext::storeTimestampInC4() const {
	return haveTimeInAbiHeader() && afterSignatureCheck() == nullptr;
}

void TVMCompilerContext::addLib(FunctionDefinition const* f) {
	m_libFunctions.insert(f);
}

std::vector<std::pair<VariableDeclaration const*, int>> TVMCompilerContext::getStaticVariables() const {
	int shift = 0;
	std::vector<std::pair<VariableDeclaration const*, int>> res;
	for (VariableDeclaration const* v : notConstantStateVariables()) {
		if (v->isStatic()) {
			res.emplace_back(v, TvmConst::C4::PersistenceMembersStartIndex + shift++);
		}
	}
	return res;
}

void TVMCompilerContext::addInlineFunction(const std::string& name, const CodeLines& code) {
	solAssert(m_inlinedFunctions.count(name) == 0, "");
	m_inlinedFunctions[name] = code;
}

CodeLines TVMCompilerContext::getInlinedFunction(const std::string& name) {
	return m_inlinedFunctions.at(name);
}

void StackPusherHelper::pushNull() {
	push(+1, "NULL");
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
		case Type::Category::VarInteger:
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
		case Type::Category::ExtraCurrencyCollection:
			if (isResultBuilder) {
				push(+1, "NEWC");
				stzeroes(1);
			} else {
				push(+1, "NEWDICT");
			}
			break;
		case Type::Category::Struct: {
			auto structType = to<StructType>(type);
			StructCompiler structCompiler{this, structType};
			structCompiler.createDefaultStruct(isResultBuilder);
			break;
		}
		case Type::Category::TvmSlice:
			if (isResultBuilder) {
				push(+1, "NEWC");
			} else {
				push(+1, "PUSHSLICE x8_");
			}
			break;
		case Type::Category::TvmBuilder:
			push(+1, "NEWC");
			break;
		case Type::Category::TvmCell:
			push(+1, "NEWC");
			if (!isResultBuilder) {
				push(0, "ENDC");
			}
			break;
		case Type::Category::Function: {
			pushInt(TvmConst::FunctionId::DefaultValueForFunctionType);
			if (isResultBuilder) {
				solUnimplemented("TODO");
				push(+1, "NEWC");
				push(-1, "STU 32");
			}
			break;
		}
		case Type::Category::Optional:
			push(+1, "NULL");
			break;
		case Type::Category::FixedPoint:
			pushInt(0);
			break;
		default:
			solUnimplemented("");
	}
}

class GetFromDict : public DictOperation {
public:
	GetFromDict(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, const StackPusherHelper::GetDictOperation op,
				const bool resultAsSliceForStruct, const DataType& dataType) :
		DictOperation{pusher, keyType, valueType},
		op{op},
		resultAsSliceForStruct{resultAsSliceForStruct},
		dataType{dataType}
	{

	}

	void getDict() {
		pusher.pushInt(keyLength); // push keyLength on stack
		// if op == GetSetFromMapping than stack: value key dict keyLength
		// else                            stack: key dict keyLength

		const int saveStake = pusher.getStack().size();
		std::string opcode = "DICT" + typeToDictChar(&keyType);
		int stackDelta{};
		switch (op) {
			case StackPusherHelper::GetDictOperation::GetSetFromMapping:
			case StackPusherHelper::GetDictOperation::GetAddFromMapping:
			case StackPusherHelper::GetDictOperation::GetReplaceFromMapping: {
				stackDelta = -4 + 2;
				if (op == StackPusherHelper::GetDictOperation::GetSetFromMapping)
					opcode += "SETGET";
				else if (op == StackPusherHelper::GetDictOperation::GetAddFromMapping)
					opcode += "ADDGET";
				else if (op == StackPusherHelper::GetDictOperation::GetReplaceFromMapping)
					opcode += "REPLACEGET";
				else
					solUnimplemented("");

				bool didUseOpcodeWithRef = false;
				switch (dataType) {
					case DataType::Builder:
						opcode += "B";
						break;
					case DataType::Cell:
						didUseOpcodeWithRef = true;
						opcode += "REF";
						break;
					case DataType::Slice:
						break;
				}
				pusher.push(stackDelta, opcode);

				StackPusherHelper::DecodeType decodeType{};
				if (op == StackPusherHelper::GetDictOperation::GetAddFromMapping)
					decodeType = StackPusherHelper::DecodeType::PushNullOrDecodeValue;
				else if (
					op == StackPusherHelper::GetDictOperation::GetSetFromMapping ||
					op == StackPusherHelper::GetDictOperation::GetReplaceFromMapping
				)
					decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushNull;
				else
					solUnimplemented("");
				int ss = pusher.getStack().size();
				pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, didUseOpcodeWithRef, decodeType, resultAsSliceForStruct);
				solAssert(ss == pusher.getStack().size(), "");
				break;
			}

			case StackPusherHelper::GetDictOperation::Exist: {
				stackDelta = -3 + 1;
				opcode += "GET";
				pusher.push(stackDelta, opcode);
				checkExist();
				break;
			}
			case StackPusherHelper::GetDictOperation::Fetch:
			case StackPusherHelper::GetDictOperation::GetFromArray:
			case StackPusherHelper::GetDictOperation::GetFromMapping: {
				stackDelta = -3 + 1;
				opcode += "GET";
				bool isInRef = pusher.doesDictStoreValueInRef(&keyType, &valueType);
				if (isInRef) {
					opcode += "REF";
				}
				pusher.push(stackDelta, opcode);
				if (op == StackPusherHelper::GetDictOperation::GetFromArray) {
					pusher.push(0, "THROWIFNOT " + toString(TvmConst::RuntimeException::ArrayIndexOutOfRange));
				}
				StackPusherHelper::DecodeType decodeType{};
				if (op == StackPusherHelper::GetDictOperation::Fetch) {
					decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushNull;
				} else if (op == StackPusherHelper::GetDictOperation::GetFromArray) {
					decodeType = StackPusherHelper::DecodeType::DecodeValue;
				} else if (op == StackPusherHelper::GetDictOperation::GetFromMapping) {
					decodeType = StackPusherHelper::DecodeType::DecodeValueOrPushDefault;
				} else {
					solUnimplemented("");
				}
				pusher.recoverKeyAndValueAfterDictOperation(&keyType, &valueType, false, isInRef, decodeType, resultAsSliceForStruct);
				break;
			}
		}

		pusher.getStack().ensureSize(saveStake + stackDelta);
	}

private:
	void checkExist() {
		pusher.push(0, "DUP");
		pusher.startContinuation();
		pusher.push(0, "NIP");
		pusher.endContinuation();
		pusher.push(0, "IF");
	}

protected:
	const StackPusherHelper::GetDictOperation op{};
	const bool resultAsSliceForStruct{};
	const DataType dataType{};
};

void StackPusherHelper::getDict(
	Type const& keyType,
	Type const& valueType,
	const GetDictOperation op,
	const bool resultAsSliceForStruct,
	const DataType& dataType
) {
	GetFromDict d(*this, keyType, valueType, op, resultAsSliceForStruct, dataType);
	d.getDict();
}


void StackPusherHelper::switchSelector() {
	push(0, "PUSHINT 1");
	push(0, "CALL 1");
}

void StackPusherHelper::byteLengthOfCell()
{
	pushInt(0xFFFFFFFF);
	push(-2 + 3, "CDATASIZE");
	drop(1);
	dropUnder(1, 1);
	push(-1 + 1, "RSHIFT 3");
}


