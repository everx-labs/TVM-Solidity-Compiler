/*
 * Copyright (C) 2020-2026 EverX. All Rights Reserved.
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

#include <boost/range/adaptor/map.hpp>

#include <utility>

#include <libsolidity/ast/TypeProvider.h>

#include <libsolidity/codegen/DictOperations.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMExpressionCompiler.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>
#include <libsolidity/codegen/TVMStructCompiler.hpp>

using namespace solidity::frontend;
using namespace solidity::util;
using namespace solidity;

StackPusher::StackPusher(TVMCompilerContext* ctx, int const stackSize):
	m_ctx(ctx) {
	change(stackSize);
	m_instructions.emplace_back();
}

void StackPusher::pushLoc(std::string const& file, int line) {
	auto op = createNode<Loc>(file, line);
	m_instructions.back().emplace_back(op);
}

void StackPusher::pushString(std::string const& _str, bool toSlice) {
	std::string hexStr = StrUtils::stringToHex(_str); // 2 * len(_str) == len(hexStr). One symbol to 2 hex digits
	solAssert(hexStr.length() % 2 == 0, "");
	if (4 * hexStr.length() <= TvmConst::MaxPushSliceBitLength && toSlice) {
		pushSlice("x" + hexStr);
		return;
	}

	int const saveStackSize = stackSize();
	m_instructions.back().emplace_back(makePushCellOrSlice(hexStr, toSlice));
	change(0, 1);

	ensureSize(saveStackSize + 1, "");
}

void StackPusher::pushLog() {
	*this << "CTOS";
	*this << "STRDUMP";
	drop();
}

bool StackPusher::doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value) {
	int keyLength = dictKeyLength(key);
	return TvmConst::MAX_HASH_MAP_INFO_ABOUT_KEY + keyLength + ABITypeSize{value}.maxBits < TvmConst::CellBitLength;
}

DataType StackPusher::prepareValueForDictOperations(Type const* keyType, Type const* valueType) {
	// stack: value

	switch (toDictValueType(valueType->category())) {
	case DictValueType::TvmSlice: {
		return DataType::Slice;
	}

	case DictValueType::Address:
	case DictValueType::Contract: {
		if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
			*this << "NEWC";
			*this << "STSLICE";
			*this << "ENDC";
			return DataType::Cell;
		}
		return DataType::Slice;
	}

	case DictValueType::Array: {
		if (isByteArrayOrString(valueType)) {
			return DataType::Cell;
		}
		[[fallthrough]];
	}

	case DictValueType::Bool:
	case DictValueType::Enum:
	case DictValueType::FixedBytes:
	case DictValueType::FixedPoint:
	case DictValueType::Integer:
	case DictValueType::Mapping:
	case DictValueType::Optional:
	case DictValueType::VarInteger:
	case DictValueType::Function: {
		*this << "NEWC";
		store(valueType);
		if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
			*this << "ENDC";
			return DataType::Cell;
		}
		return DataType::Builder;
	}

	case DictValueType::Struct: {
		StructCompiler sc{this, to<StructType>(valueType)};
		sc.tupleToBuilder();
		if (!doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
			*this << "ENDC";
			return DataType::Cell;
		}
		return DataType::Builder;
	}

	case DictValueType::TvmCell: {
		return DataType::Cell;
	}
	}
	solUnimplemented("");
}

DataType StackPusher::pushDefaultValueForDict(Type const* keyType, Type const* valueType) {
	startOpaque();
	std::optional<DataType> value;
	switch (toDictValueType(valueType->category())) {
	case DictValueType::TvmSlice: {
		pushDefaultValue(valueType);
		value = DataType::Slice;
		break;
	}

	case DictValueType::Address:
	case DictValueType::Contract: {
		pushDefaultValue(valueType);
		value = prepareValueForDictOperations(keyType, valueType);
		break;
	}

	case DictValueType::Array: {
		if (isByteArrayOrString(valueType)) {
			pushDefaultValue(valueType);
			value = DataType::Cell;
		} else {
			*this << "NEWC";
			pushInt(33);
			*this << "STZEROES";
			value = DataType::Builder;
		}
		break;
	}

	case DictValueType::Mapping: {
		pushSlice("x4_");
		value = DataType::Slice;
		break;
	}

	case DictValueType::Bool:
	case DictValueType::Enum:
	case DictValueType::FixedBytes:
	case DictValueType::FixedPoint:
	case DictValueType::Integer:
	case DictValueType::Optional:
	case DictValueType::VarInteger:
	case DictValueType::Function: {
		pushDefaultValue(valueType);
		value = prepareValueForDictOperations(keyType, valueType);
		break;
	}

	case DictValueType::Struct: {
		StructCompiler sc{this, to<StructType>(valueType)};
		if (doesFitInOneCellAndHaveNoStruct(keyType, valueType)) {
			sc.createDefaultStructAsSlice();
			value = DataType::Slice;
		} else {
			sc.createDefaultStructAsCell();
			value = DataType::Cell;
		}
		break;
	}

	case DictValueType::TvmCell: {
		pushDefaultValue(valueType);
		value = DataType::Cell;
		break;
	}
	}
	endOpaque(0, 1, true);
	return value.value();
}

// delMin/delMax
// min/max
// fetch
// at/[] - for arrays and mappings
bool StackPusher::doesDictStoreValueInRef(Type const* keyType, Type const* valueType) {
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

void StackPusher::recoverKeyAndValueAfterDictOperation(
	Type const* keyType,
	Type const* valueType,
	bool hasKey,
	bool didUseOpcodeWithRef,
	DecodeType const& decodeType,
	bool saveOrigKeyAndNoTuple
) {
	bool const isValueStruct = valueType->category() == Type::Category::Struct;
	bool const pushRefCont = isValueStruct && !didUseOpcodeWithRef && !doesDictStoreValueInRef(keyType, valueType);

	// stack: value [key]
	auto preloadValue = [&] {
		if (hasKey) {
			// stack: value key
			if (saveOrigKeyAndNoTuple) {
				pushS(0); // stack: value key [key]
			}
			if (keyType->category() == Type::Category::Struct) {
				StructCompiler sc{this, to<StructType>(keyType)};
				sc.convertSliceToTuple();
				// stack: value slice Tuple
			}
			if (saveOrigKeyAndNoTuple)
				rot();
			else
				exchange(1);
			// stack: slice key value
		}
		// stack: [slice, key] value

		switch (toDictValueType(valueType->category())) {
		case DictValueType::Address:
		case DictValueType::Contract:
		case DictValueType::TvmSlice: {
			if (didUseOpcodeWithRef) {
				*this << "CTOS";
			} else if (doesDictStoreValueInRef(keyType, valueType)) {
				*this << "PLDREFIDX 0";
				*this << "CTOS";
			}
			break;
		}
		case DictValueType::Array:
			if (isByteArrayOrString(valueType)) {
				if (!didUseOpcodeWithRef) {
					*this << "PLDREFIDX 0";
				}
				break;
			}
			[[fallthrough]];
		case DictValueType::Bool:
		case DictValueType::Enum:
		case DictValueType::FixedBytes:
		case DictValueType::FixedPoint:
		case DictValueType::Integer:
		case DictValueType::Mapping:
		case DictValueType::Optional:
		case DictValueType::Struct:
		case DictValueType::VarInteger:
		case DictValueType::Function: {
			if (didUseOpcodeWithRef) {
				*this << "CTOS";
			} else if (doesDictStoreValueInRef(keyType, valueType)) {
				*this << "PLDREFIDX 0";
				*this << "CTOS";
			}
			preload(valueType);
			break;
		}
		case DictValueType::TvmCell: {
			if (!didUseOpcodeWithRef) {
				*this << "PLDREFIDX 0";
			}
			break;
		}
		}
	};

	auto checkOnMappingOrOptional = [&] {
		if (optValueAsTuple(valueType)) {
			makeTuple(1);
		}
	};

	switch (decodeType) {
	case DecodeType::DecodeValue:
		preloadValue();
		break;
	case DecodeType::DecodeValueOrPushDefault: {
		startContinuation();
		preloadValue();
		pushRefCont ? endContinuationFromRef() : endContinuation();

		bool hasEmptyPushCont = tryPollEmptyPushCont();
		startContinuation();
		pushDefaultValue(valueType);
		pushRefCont ? endContinuationFromRef() : endContinuation();

		if (hasEmptyPushCont)
			ifNot();
		else
			ifElse();
		break;
	}
	case DecodeType::DecodeValueOrPushNull: {
		if (!saveOrigKeyAndNoTuple) {
			pushAsym("NULLSWAPIFNOT");
		}

		startContinuation();
		preloadValue();
		if (hasKey) {
			if (!saveOrigKeyAndNoTuple) {
				makeTuple(2);
			}
		} else {
			checkOnMappingOrOptional();
		}
		isValueStruct ? endContinuationFromRef() : endContinuation();

		if (saveOrigKeyAndNoTuple) {
			startContinuation();
			pushNull();
			pushNull();
			pushNull();
			fixStack(-3); // fix stack
			endContinuation();

			ifElse();
		} else {
			_if();
		}

		break;
	}
	case DecodeType::PushNullOrDecodeValue: {
		pushAsym("NULLSWAPIF");

		startContinuation();
		preloadValue();
		checkOnMappingOrOptional();
		endContinuation();

		ifNot();
		break;
	}
	}
}

void StackPusher::setDict(
	Type const& keyType,
	Type const& valueType,
	DataType const& dataType,
	SetDictOperation operation
) {
	DictSet d{*this, keyType, valueType, dataType, operation};
	d.dictSet();
}

void StackPusher::pushInlineFunction(std::string const& name, int take, int ret) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), name), "");
	auto block = ctx().getInlinedFunction(name);
	solAssert(block->type() == CodeBlock::Type::None, "");
	for (Pointer<TvmAstNode> const& i: block->instructions())
		m_instructions.back().emplace_back(i);
	change(take, ret);
}

void StackPusher::pollLastRetOpcode() {
	std::vector<Pointer<TvmAstNode>>& opcodes = m_instructions.back();
	int offset = 0;
	int size = opcodes.size();
	while (offset < size && convertToLoc(opcodes.at(opcodes.size() - 1 - offset).get()))
		++offset;
	int begPos = size - 1 - offset;

	auto opcode = convertToReturnOrBreakOrCont(opcodes.at(begPos).get());
	solAssert(opcode, "");
	std::vector<Pointer<TvmAstNode>> instructions = opcode->body()->instructions();
	solAssert(!instructions.empty(), "");
	auto ret = convertToTvmReturn(instructions.back().get());
	solAssert(ret, "");
	solAssert(!ret->withIf() && !ret->withAlt(), "");
	instructions.pop_back();

	opcodes.erase(opcodes.begin() + begPos);
	opcodes.insert(opcodes.begin() + begPos, instructions.begin(), instructions.end());
}

bool StackPusher::tryPollEmptyPushCont() {
	std::vector<Pointer<TvmAstNode>>& opcodes = m_instructions.back();
	solAssert(opcodes.size() >= 2, "");
	auto block = convertToCodeBlock(opcodes.back().get());
	solAssert(block != nullptr, "");
	if (block->instructions().empty()) {
		opcodes.pop_back();
		return true;
	}
	return false;
}

TVMCompilerContext& StackPusher::ctx() const { return *m_ctx; }

void StackPusher::change(int delta) {
	solAssert(lockStack >= 0, "");
	if (lockStack == 0) {
		m_stack.change(delta);
	}
}

void StackPusher::change(int take, int ret) { change(-take + ret); }

int StackPusher::stackSize() const { return m_stack.size(); }

void StackPusher::ensureSize(int savedStackSize, std::string const& location, ASTNode const* node) const {
	if (lockStack == 0) {
		m_stack.ensureSize(savedStackSize, location, node);
	}
}

void StackPusher::startOpaque() {
	++lockStack;
	m_instructions.emplace_back();
}

void StackPusher::endOpaque(int take, int ret, bool isPure) {
	--lockStack;
	solAssert(m_instructions.size() >= 2, "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto bl = createNode<CodeBlock>(CodeBlock::Type::None, block);
	auto node = createNode<Opaque>(bl, take, ret, isPure);
	m_instructions.back().push_back(node);
	change(take, ret);
}

void StackPusher::declRetFlag() {
	m_instructions.back().push_back(createNode<DeclRetFlag>());
	change(0, 1);
}

Pointer<AsymGen> StackPusher::makeAsym(std::string const& cmd) {
	static std::set<std::string> asymOpcodes;
	if (asymOpcodes.empty()) {
		for (std::string type: {"", "I", "U"}) {
			for (std::string suf: {"", "REF"}) {
				for (std::string op: {"MIN", "MAX"}) {
					asymOpcodes.insert("DICT" + type + "REM" + op + suf);
					asymOpcodes.insert("DICT" + type + op + suf);
				}
			}

			for (std::string op: {"SETGET", "ADDGET", "REPLACEGET"})
				for (std::string suf: {"", "REF", "B"})
					asymOpcodes.insert("DICT" + type + op + suf);

			for (std::string op: {"DELGET"})
				for (std::string suf: {"", "REF"})
					asymOpcodes.insert("DICT" + type + op + suf);

			for (std::string suf: {"", "REF", "PREV", "PREVEQ", "NEXT", "NEXTEQ"})
				asymOpcodes.insert("DICT" + type + "GET" + suf);
		}

		for (std::string preload: {"", "P"})
			for (std::string type: {"I", "U"}) {
				for (std::string size: {"4", "8"})
					asymOpcodes.insert(preload + "LD" + type + "LE" + size + "Q");
				for (std::string x: {"", "X"})
					asymOpcodes.insert(preload + "LD" + type + x + "Q");
			}

		asymOpcodes.insert({
			"CDATASIZEQ",
			"CONFIGPARAM",
			"ECRECOVER",
			"HASHEXTA_BLAKE2B",
			"HASHEXTA_KECCAK256",
			"HASHEXTA_KECCAK512",
			"HASHEXTA_SHA256",
			"HASHEXTA_SHA512",
			"LDDICTQ",
			"LDMSGADDRQ",
			"LDSLICEQ",
			"LDSLICEXQ",
			"NULLROTRIFNOT",
			"NULLSWAPIF",
			"NULLSWAPIFNOT",
			"PLDSLICEQ",
			"PLDSLICEXQ",
			"RIST255_QADD",
			"RIST255_QMUL",
			"RIST255_QMULBASE",
			"RIST255_QSUB",
			"SDATASIZEQ",
			"SECP256K1_XONLY_PUBKEY_TWEAK_ADD",
			"SPLITQ",
			"STBQ",
			"STIQ",
			"STIQX",
			"STIXQ",
			"STREFQ",
			"STSLICEQ",
			"STUQ",
		});
	}

	std::istringstream iss(cmd);
	std::string baseCmd;
	iss >> baseCmd;
	solAssert(asymOpcodes.contains(baseCmd), "Unknown asym opcode: " + cmd);

	return createNode<AsymGen>(cmd);
}

void StackPusher::push(Pointer<Stack> const& opcode) { m_instructions.back().push_back(opcode); }

void StackPusher::push(Pointer<AsymGen> const& opcode) {
	// no stack changing
	solAssert(lockStack >= 0, "");
	m_instructions.back().push_back(opcode);
}

void StackPusher::push(Pointer<HardCode> const& opcode) {
	m_instructions.back().push_back(opcode);
	change(opcode->take(), opcode->ret());
}

void StackPusher::pushAsym(Pointer<AsymGen>&& node) { m_instructions.back().push_back(node); }

void StackPusher::pushAsym(std::string const& opcode) {
	solAssert(lockStack >= 1, "");
	Pointer<AsymGen> node = makeAsym(opcode);
	pushAsym(std::move(node));
}

StackPusher& StackPusher::operator<<(std::string const& opcode) {
	push(opcode);
	return *this;
}

void StackPusher::push(std::string const& cmd) {
	Pointer<StackGen> opcode = gen(cmd);
	change(opcode->take(), opcode->ret());
	m_instructions.back().push_back(opcode);
}

void StackPusher::fixStack(int stackDiff) { change(stackDiff); }

void StackPusher::pushCellOrSlice(Pointer<CellOrSliceOperation> const& opcode) {
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(opcode);
	change(0, 1);
}

void StackPusher::pushSlice(std::string const& data) {
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(genPushSlice(data));
	change(0, 1);
}

void StackPusher::pushPrivateFunctionId(FunctionDefinition const& funDef, bool isCalledByPoint) {
	auto const [funName, id] = ctx().functionInternalName(&funDef, isCalledByPoint);
	pushInt(id);
	ctx().callGraph().addDictFunction(id, funName);
}

void StackPusher::startContinuation() { m_instructions.emplace_back(); }

void StackPusher::endCont(CodeBlock::Type type) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(type, block);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(b);
}

void StackPusher::endContinuation() { endCont(CodeBlock::Type::PUSHCONT); }

void StackPusher::endContinuationFromRef() { endCont(CodeBlock::Type::PUSHREFCONT); }

void StackPusher::endRetOrBreakOrCont(ReturnOrBreakOrCont::Type type, int _take) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(CodeBlock::Type::None, block);
	auto r = createNode<ReturnOrBreakOrCont>(type, _take, b);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(r);
}

void StackPusher::endLogCircuit(LogCircuit::Type type) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(CodeBlock::Type::None, block);
	auto lc = createNode<LogCircuit>(type, b);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(lc);
}

void StackPusher::callRefOrCallX(bool _isJmp, CodeBlock::Type _blockType) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(_blockType, block);
	auto subProg = createNode<SubProgram>(_isJmp, b);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(subProg);
}

void StackPusher::pushRefContAndCallX() { callRefOrCallX(false, CodeBlock::Type::PUSHREFCONT); }

void StackPusher::pushContAndCallX() { callRefOrCallX(false, CodeBlock::Type::PUSHCONT); }

void StackPusher::ifElse(bool withJmp) {
	solAssert(m_instructions.back().size() >= 2, "");
	auto falseBlock = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(falseBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto trueBlock = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(false, withJmp, trueBlock, falseBlock, 0);
	m_instructions.back().push_back(b);
	if (withJmp) {
		ret();
	}
}

void StackPusher::pushConditional(int ret) {
	solAssert(m_instructions.back().size() >= 3, "");
	auto falseBlock = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(falseBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto trueBlock = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(false, false, trueBlock, falseBlock, ret);
	m_instructions.back().push_back(b);
	fixStack(ret);
}

void StackPusher::if_or_ifNot(bool _withNot, bool _withJmp) {
	solAssert(!m_instructions.back().empty(), "");
	auto trueBlock = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(_withNot, _withJmp, trueBlock, nullptr, 0);
	m_instructions.back().push_back(b);
}

void StackPusher::_if() { if_or_ifNot(false, false); }

void StackPusher::ifNot() { if_or_ifNot(true, false); }

void StackPusher::ifJmp() { if_or_ifNot(false, true); }

void StackPusher::ifNotJmp() { if_or_ifNot(true, true); }

void StackPusher::repeatOrUntil(bool withBreakOrReturn, bool isRepeat) {
	solAssert(!m_instructions.back().empty(), "");

	auto loopBody = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	m_instructions.back().pop_back();
	solAssert(loopBody != nullptr, "");

	Pointer<TvmAstNode> b;
	if (isRepeat) {
		b = createNode<TvmRepeat>(withBreakOrReturn, loopBody);
	} else {
		b = createNode<TvmUntil>(withBreakOrReturn, loopBody);
	}
	m_instructions.back().push_back(b);
}

void StackPusher::repeat(bool _withBreakOrReturn) { repeatOrUntil(_withBreakOrReturn, true); }

void StackPusher::until(bool withBreakOrReturn) { repeatOrUntil(withBreakOrReturn, false); }

void StackPusher::_while(bool _withBreakOrReturn) {
	solAssert(m_instructions.back().size() >= 2, "");
	auto body = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(body != nullptr, "");
	m_instructions.back().pop_back();
	auto condition = std::dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(condition != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<While>(false, _withBreakOrReturn, condition, body);
	m_instructions.back().push_back(b);
}

void StackPusher::ret() {
	auto opcode = makeRET();
	m_instructions.back().push_back(opcode);
}

void StackPusher::retAlt() {
	auto opcode = makeRETALT();
	m_instructions.back().push_back(opcode);
}

void StackPusher::ifRetAlt() {
	auto opcode = makeIFRETALT();
	m_instructions.back().push_back(opcode);
}

void StackPusher::ifret() {
	auto opcode = makeIFRET();
	m_instructions.back().push_back(opcode);
	change(1, 0);
}

void StackPusher::ifNotRet() {
	auto opcode = makeIFNOTRET();
	m_instructions.back().push_back(opcode);
	change(1, 0);
}

void StackPusher::_throw(std::string const& cmd) {
	auto opcode = makeTHROW(cmd);
	m_instructions.back().push_back(opcode);
	change(opcode->take(), 0);
}

TVMStack& StackPusher::getStack() {
	//	solUnimplemented("");
	return m_stack; // TODO delete
}

void StackPusher::untuple(int n) {
	solAssert(0 <= n && n <= 255, "");
	*this << "UNTUPLE " + toString(n);
}

void StackPusher::unpackFirst(int n) {
	solAssert(0 <= n && n <= 255, "");
	*this << "UNPACKFIRST " + toString(n);
}

void StackPusher::indexWithExcep(int index) {
	solAssert(0 <= index && index <= 254, "");
	*this << "INDEX_EXCEP " + toString(index);
}

void StackPusher::indexNoexcep(int index) {
	solAssert(0 <= index && index <= 254, "");
	*this << "INDEX_NOEXCEP " + toString(index);
}

void StackPusher::setIndex(int index) {
	solAssert(0 <= index, "");
	if (index <= 15) {
		*this << "SETINDEX " + toString(index);
	} else {
		solAssert(index <= 254, "");
		pushInt(index);
		*this << "SETINDEXVAR";
	}
}

void StackPusher::setIndexQ(int index) {
	solAssert(0 <= index, "");
	if (index <= 15) {
		*this << "SETINDEXQ " + toString(index);
	} else {
		solAssert(index <= 254, "");
		pushInt(index);
		*this << "SETINDEXVARQ";
	}
}

void StackPusher::makeTuple(int qty) {
	solAssert(0 <= qty, "");
	if (qty <= 15) {
		*this << "TUPLE " + toString(qty);
	} else {
		solAssert(qty <= 255, "");
		pushInt(qty);
		pushStackGenOpcode("TUPLEVAR", qty + 1, 1);
	}
}

void StackPusher::pushStackGenOpcode(std::string const& name, int take, int ret) {
	auto opcode = createNode<StackGen>(name, take, ret);
	m_instructions.back().push_back(opcode);
	change(take, ret);
}

void StackPusher::resetAllStateVars() {
	std::vector<VariableDeclaration const*> const usualStateVars = ctx().storageLayout().usualStateVariables();
	std::vector<VariableDeclaration const*> const unpackedStateVars = ctx().storageLayout().unpackedStateVariables();
	std::vector<Type const*> const allTypes =
		getTypesFromVarDecls(ctx().storageLayout().usualAndUnpackedStateVariables());
	std::vector<VariableDeclaration const*> const transientStateVariables = ctx().storageLayout().transientStateVars();

	auto getDefaultUnpackedStateVars = [&] {
		int const startSize = getStack().size();
		AbiV2Position position{ctx().storageLayout().getOffsetC4(), 0, allTypes};

		for (VariableDeclaration const* variable: unpackedStateVars | std::views::reverse)
			pushDefaultValue(variable->type());
		*this << "NEWC";

		position.skipTypes(getTypesFromVarDecls(usualStateVars));
		ChainDataEncoder encode{this};
		encode.encodeParameters(getTypesFromVarDecls(unpackedStateVars), position, false);
		*this << "BTOS";
		getStack().ensureSize(startSize + 1);
	};

	if (m_ctx->storageLayout().tooMuchStateVariables()) {
		pushC7();
		*this << "FALSE";
		setIndexQ(TvmConst::C7::FirstIndexForVariables);
		unpackFirst(TvmConst::C7::FirstIndexForVariables);
		for (VariableDeclaration const* variable: usualStateVars)
			pushDefaultValue(variable->type());
		if (!unpackedStateVars.empty())
			getDefaultUnpackedStateVars();
		for (VariableDeclaration const* variable: transientStateVariables)
			pushDefaultValue(variable->type());
		int const stateVarQty =
			usualStateVars.size() + (unpackedStateVars.empty() ? 0 : 1) + transientStateVariables.size();
		makeTuple(TvmConst::C7::FirstIndexForVariables + stateVarQty);
		popC7();
	} else {
		for (VariableDeclaration const* variable: usualStateVars)
			pushDefaultValue(variable->type());
		for (VariableDeclaration const* variable: transientStateVariables)
			pushDefaultValue(variable->type());

		for (VariableDeclaration const* variable: transientStateVariables | std::views::reverse)
			setGlob(variable);
		for (VariableDeclaration const* variable: usualStateVars | std::views::reverse)
			setGlob(variable);

		if (!unpackedStateVars.empty()) {
			getDefaultUnpackedStateVars();
			setGlob(ctx().storageLayout().getUnpackIndex());
		}
	}
}

void StackPusher::getGlob(VariableDeclaration const* vd) {
	int const index = ctx().storageLayout().getStateVarIndex(vd);
	getGlob(index);
}

void StackPusher::getGlob(int index) {
	solAssert(index >= 0, "");
	Pointer<TvmAstNode> opcode = makeGetGlob(index);
	change(+1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::pushRoot() {
	Pointer<TvmAstNode> opcode = createNode<Glob>(Glob::Opcode::PUSHROOT);
	change(+1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::popRoot() {
	Pointer<TvmAstNode> opcode = createNode<Glob>(Glob::Opcode::POPROOT);
	change(-1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::pushC3() {
	auto opcode = createNode<Glob>(Glob::Opcode::PUSH_C3);
	change(+1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::pushC7() {
	auto opcode = createNode<Glob>(Glob::Opcode::PUSH_C7);
	change(+1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::popC3() {
	auto opcode = createNode<Glob>(Glob::Opcode::POP_C3);
	change(-1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::popC7() {
	auto opcode = createNode<Glob>(Glob::Opcode::POP_C7);
	change(-1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::callx(int take, int ret) { pushStackGenOpcode("CALLX", take, ret); }

void StackPusher::call(uint32_t id, int take, int ret) { pushStackGenOpcode("CALL " + toString(id), take, ret); }

void StackPusher::setGlob(int index) {
	Pointer<TvmAstNode> opcode = makeSetGlob(index);
	change(-1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::setGlob(VariableDeclaration const* vd) {
	int const index = ctx().storageLayout().getStateVarIndex(vd);
	solAssert(index >= 0, "");
	setGlob(index);
}

void StackPusher::pushS(int i) {
	solAssert(i >= 0, "");
	m_instructions.back().push_back(makePUSH(i));
	change(+1);
}

void StackPusher::popS(int i) {
	solAssert(i >= 1, "");
	m_instructions.back().push_back(makePOP(i));
	change(-1);
}

void StackPusher::pushInt(bigint const& i) { *this << "PUSHINT " + toString(i); }

bool StackPusher::fastLoad(Type const* type) {
	// slice
	switch (type->category()) {
	case Type::Category::Optional: {
		auto optType = to<OptionalType>(type);
		auto optValueType = optType->valueType();
		auto array = to<ArrayType>(optType->valueType());
		if (optValueType->category() == Type::Category::TvmCell || (array && array->isByteArrayOrString())) {
			*this << "LDDICT";
		} else {
			startOpaque();
			int const saveStackSize = stackSize();
			auto opt = to<OptionalType>(type);

			auto f = [&](bool reverseOrder) {
				if (isSmallOptional(opt)) {
					load(opt->valueType(), reverseOrder);
				} else {
					*this << "LDREFRTOS";
					std::unique_ptr<StructCompiler> sc;
					if (auto st = to<StructType>(opt->valueType())) {
						sc = std::make_unique<StructCompiler>(this, st);
					} else if (auto tt = to<TupleType>(opt->valueType())) {
						sc = std::make_unique<StructCompiler>(this, tt);
					} else {
						solUnimplemented("");
					}
					sc->convertSliceToTuple();
					if (!reverseOrder) {
						exchange(1);
					}
				}
			};

			*this << "LDI 1"; // hasValue slice
			exchange(1);	  // slice hasValue
			fixStack(-1);	  // fix stack

			startContinuation();
			if (optValueAsTuple(opt->valueType())) {
				f(true);
				makeTuple(1);
				exchange(1);
			} else {
				f(false);
			}
			endContinuation();
			fixStack(-1); // fix stack
			if (!hasLock()) {
				solAssert(saveStackSize == stackSize(), "");
			}

			startContinuation();
			pushNull();
			exchange(1);
			endContinuation();
			fixStack(-1); // fix stack
			if (!hasLock()) {
				solAssert(saveStackSize == stackSize(), "");
			}

			ifElse();
			fixStack(+1); // fix stack
			if (!hasLock()) {
				solAssert(saveStackSize + 1 == stackSize(), "");
			}
			endOpaque(1, 2);
		}
		return true;
	}
	case Type::Category::Tuple: {
		auto tup = to<TupleType>(type);
		for (auto t: tup->components()) {
			load(t, false);
		}
		blockSwap(tup->components().size(), 1);
		makeTuple(tup->components().size());
		return false;
	}
	case Type::Category::TvmCell:
		*this << "LDREF";
		return true;
	case Type::Category::Struct: {
		auto st = to<StructType>(type);
		std::vector<ASTPointer<VariableDeclaration>> const& members = st->structDefinition().members();
		for (ASTPointer<VariableDeclaration> const& t: members) {
			load(t->type(), false);
		}
		blockSwap(members.size(), 1);
		makeTuple(members.size());
		exchange(1);
		return true;
	}
	case Type::Category::Address:
	case Type::Category::Contract:
	case Type::Category::AddressStd:
		*this << "LDMSGADDR";
		return true;
	case Type::Category::Enum:
	case Type::Category::Integer:
	case Type::Category::Bool:
	case Type::Category::FixedPoint:
	case Type::Category::FixedBytes: {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		if (ti.numBits == 257) {
			solAssert(ti.isSigned, "");
			pushInt(ti.numBits);
			*this << "LDIX";
		} else {
			std::string cmd = ti.isSigned ? "LDI " : "LDU ";
			*this << cmd + toString(ti.numBits);
		}
		return true;
	}
	case Type::Category::Function: {
		*this << "LDU 32";
		return true;
	}
	case Type::Category::Array: {
		auto arrayType = to<ArrayType>(type);
		if (arrayType->isByteArrayOrString()) {
			*this << "LDREF";
			return true;
		} else {
			*this << "LDU 32";
			*this << "LDDICT";
			rotRev();
			*this << "TUPLE 2";
			return false;
		}
	}
	case Type::Category::Mapping:
		*this << "LDDICT";
		return true;
	case Type::Category::VarInteger: {
		auto varint = to<VarIntegerType>(type);
		std::string cmd = "LDVAR";
		if (!varint->asIntegerType().isSigned())
			cmd += "U";
		cmd += "INT" + std::to_string(varint->n());
		*this << cmd;
		return true;
	}
	default:
		solUnimplemented(type->toString());
	}
	solUnimplemented("");
	// true  => value slice
	// false => slice value
}

void StackPusher::load(Type const* type, bool dataOnTop) {
	// slice
	bool directOrder = fastLoad(type);
	if (directOrder == dataOnTop) {
		exchange(1);
	}
	// dataOnTop? slice member : member slice
}

void StackPusher::preload(Type const* type) {
	int const stackSize = this->stackSize();
	// on stack there is slice
	switch (type->category()) {
	case Type::Category::Optional: {
		load(type, false);
		drop();
		break;
	}
	case Type::Category::Struct: {
		auto structType = to<StructType>(type);
		StructCompiler sc{this, structType};
		sc.convertSliceToTuple();
		break;
	}
	case Type::Category::Integer:
	case Type::Category::Enum:
	case Type::Category::Bool:
	case Type::Category::FixedPoint:
	case Type::Category::FixedBytes: {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		if (ti.numBits == 257) {
			solAssert(ti.isSigned, "");
			pushInt(ti.numBits);
			*this << "PLDIX";
		} else {
			std::string cmd = ti.isSigned ? "PLDI " : "PLDU ";
			*this << cmd + toString(ti.numBits);
		}
		break;
	}
	case Type::Category::Function: {
		*this << "PLDU 32";
		break;
	}
	case Type::Category::Array: {
		auto arrayType = to<ArrayType>(type);
		if (arrayType->isByteArrayOrString()) {
			*this << "PLDREFIDX 0";
		} else {
			*this << "LDU 32";
			*this << "PLDDICT";
			*this << "TUPLE 2";
			// stack: array
		}
		break;
	}
	case Type::Category::Mapping:
		*this << "PLDDICT";
		break;
	case Type::Category::VarInteger:
		load(type, false);
		drop();
		break;
	case Type::Category::Tuple: {
		auto const [types, names] = getTupleTypes(to<TupleType>(type));
		StructCompiler sc{this, types, names};
		sc.convertSliceToTuple();
		break;
	}
	default:
		solUnimplemented("Decode isn't supported for " + type->toString(true));
	}
	ensureSize(stackSize);
}

void StackPusher::loadQ(Type const* type) {
	// slice ->
	//    cell slice' true
	//    slice false

	auto decodeRef = [&] {
		pushS(0);
		*this << "SREFS";

		startContinuation();
		// slice
		load(type, false);
		*this << "TRUE";
		// cell slice' true
		endContinuation();

		startContinuation();
		*this << "FALSE";
		// slice false
		endContinuation();

		ifElse();
	};

	switch (type->category()) {
	case Type::Category::Optional: {
		solUnimplemented("TODO");
		break;
	}
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Contract:
		pushAsym("LDMSGADDRQ");
		break;
	case Type::Category::TvmCell:
		decodeRef();
		break;
	case Type::Category::Struct: {
		solUnimplemented("TODO");
		break;
	}
	case Type::Category::Integer:
	case Type::Category::Enum:
	case Type::Category::Bool:
	case Type::Category::FixedPoint:
	case Type::Category::FixedBytes: {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		std::string cmd = (ti.isSigned ? "LDIQ " : "LDUQ ") + toString(ti.numBits);
		pushAsym(cmd);
		break;
	}
	case Type::Category::Function: {
		solUnimplemented("TODO");
		break;
	}
	case Type::Category::Array: {
		auto arrayType = to<ArrayType>(type);
		if (arrayType->isByteArrayOrString()) {
			decodeRef();
		} else {
			pushAsym("LDUQ 32");

			startContinuation();
			pushAsym("LDDICTQ");
			{
				startContinuation(); // u32 dict s
				rotRev();
				this->makeTuple(2);
				blockSwap(1, 1);
				*this << "TRUE";
				endContinuation();

				startContinuation(); // u32 s
				popS(1);
				*this << "FALSE";
				endContinuation();
				ifElse();
			}
			endContinuation();

			startContinuation();
			*this << "FALSE";
			endContinuation();

			ifElse();
		}
		break;
	}
	case Type::Category::Mapping:
		pushAsym("LDDICTQ");
		break;
	case Type::Category::VarInteger: {
		solUnimplemented("TODO");
		break;
	}
	case Type::Category::Tuple: {
		solUnimplemented("TODO");
		break;
	}
	default:
		solUnimplemented("QDecode isn't supported for " + type->toString(true));
	}
}

void StackPusher::store(Type const* type) {
	// value builder
	int const stackSize = this->stackSize();
	int deltaStack = 1;
	switch (type->category()) {
	case Type::Category::Optional: {
		auto optType = to<OptionalType>(type);
		auto optValueType = optType->valueType();
		auto array = to<ArrayType>(optType->valueType());
		if (optValueType->category() == Type::Category::TvmCell || (array && array->isByteArrayOrString())) {
			*this << "STDICT";
		} else {
			startOpaque();
			exchange(1);	   // builder value
			pushS(0);		   // builder value value
			*this << "ISNULL"; // builder value isnull
			fixStack(-1);	   // fix stack
			ensureSize(stackSize);

			startContinuation();
			// builder value
			drop(1);	 // builder
			stzeroes(1); // builder'
			endContinuation();
			fixStack(+1); // fix stack
			ensureSize(stackSize);

			startContinuation();
			// builder value
			if (optValueAsTuple(optType->valueType())) {
				untuple(1);
			}
			// builder value
			if (isSmallOptional(optType)) {
				exchange(1);				 // value builder
				stones(1);					 // value builder'
				store(optType->valueType()); // builder''
			} else {
				// builder' value
				std::unique_ptr<StructCompiler> sc;
				if (optType->valueType()->category() == Type::Category::Tuple) {
					auto tup = to<TupleType>(optType->valueType());
					sc = std::make_unique<StructCompiler>(this, tup);
				} else if (optType->valueType()->category() == Type::Category::Struct) {
					auto st = to<StructType>(optType->valueType());
					sc = std::make_unique<StructCompiler>(this, st);
				} else {
					// TODO add test for struct
					solUnimplemented("");
				}
				sc->tupleToBuilder();
				*this << "STBREFR";
				stones(1); // builder'
			}
			endContinuation();
			fixStack(+1); // fix stack
			ensureSize(stackSize);

			ifElse();
			endOpaque(2, 1);
		}
		break;
	}
	case Type::Category::TvmCell:
		*this << "STREF"; // builder
		break;
	case Type::Category::Struct: {
		auto structType = to<StructType>(type);
		exchange(1);
		auto members = structType->structDefinition().members();
		untuple(members.size());
		this->reverse(members.size() + 1, 0);
		for (auto const& member: members)
			store(member->type());
		break;
	}
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Contract:
	case Type::Category::TvmSlice:
		*this << "STSLICE"; // builder slice-value
		break;
	case Type::Category::Integer:
	case Type::Category::Enum:
	case Type::Category::Bool:
	case Type::Category::FixedBytes:
	case Type::Category::FixedPoint: {
		TypeInfo ti(type);
		solAssert(ti.isNumeric, "");
		if (ti.numBits == 257) {
			solAssert(ti.isSigned, "");
			pushInt(ti.numBits);
			std::string cmd = "STIX";
			*this << cmd;
		} else {
			std::string cmd = ti.isSigned ? "STI" : "STU";
			cmd += " " + toString(ti.numBits);
			*this << cmd;
		}
		break;
	}
	case Type::Category::Function: {
		*this << "STU 32";
		break;
	}
	case Type::Category::Mapping:
		// dict builder
		*this << "STDICT"; // builder
		break;
	case Type::Category::Array: {
		auto arrayType = to<ArrayType>(type);
		if (arrayType->isByteArrayOrString()) {
			*this << "STREF"; // builder
		} else {
			exchange(1);		  // builder arr
			*this << "UNTUPLE 2"; // builder size dict
			exchange(2);		  // dict size builder
			*this << "STU 32";	  // dict builder'
			*this << "STDICT";	  // builder''
		}
		break;
	}
	case Type::Category::TvmBuilder:
		*this << "STB";
		break;
	case Type::Category::Tuple: {
		exchange(1); // builder value

		auto const [types, names] = getTupleTypes(to<TupleType>(type));
		StructCompiler sc{this, types, names};
		sc.tupleToBuilder();
		*this << "STBR";
		break;
	}
	case Type::Category::VarInteger: {
		exchange(1); // builder value

		auto varint = to<VarIntegerType>(type);
		std::string cmd = "STVAR";
		if (!varint->asIntegerType().isSigned())
			cmd += "U";
		cmd += "INT" + std::to_string(varint->n());
		*this << cmd;
		break;
	}
	default: {
		solUnimplemented("Encode isn't supported for " + type->toString(true));
	}
	}

	ensureSize(stackSize - deltaStack);
}

void StackPusher::storeQ(Type const* type) {
	// value builder
	startOpaque();
	switch (type->category()) {
	case Type::Category::Array: {
		auto arrayType = to<ArrayType>(type);
		solAssert(arrayType->isByteArrayOrString(), "");
		pushAsym("STREFQ");
		break;
	}
	case Type::Category::TvmCell:
		pushAsym("STREFQ");
		break;
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Contract:
	case Type::Category::TvmSlice:
		pushAsym("STSLICEQ");
		break;
	case Type::Category::Integer:
	case Type::Category::Enum:
	case Type::Category::Bool:
	case Type::Category::FixedBytes:
	case Type::Category::FixedPoint: {
		TypeInfo ti(type);
		solAssert(ti.isNumeric, "");
		if (ti.numBits == 257) {
			solAssert(ti.isSigned, "");
			pushInt(ti.numBits);
			std::string cmd = "STIXQ";
			pushAsym(cmd);
		} else {
			std::string cmd = ti.isSigned ? "STIQ" : "STUQ";
			cmd += " " + toString(ti.numBits);
			pushAsym(cmd);
		}
		break;
	}
	case Type::Category::TvmBuilder:
		pushAsym("STBQ");
		break;
	default:
		solUnimplemented("storeQ isn't supported for " + type->toString(true));
	}
	// value builder −1 or builder 0
	pushAsym("NULLROTRIFNOT");
	// value builder −1 or null builder 0
	dropUnder(1, 2); // builder −1 or builder 0
	endOpaque(2, 2);
	push("NOT");
}

void StackPusher::pushZeroAddress() {
	pushSlice("x8000000000000000000000000000000000000000000000000000000000000000001_");
}

void StackPusher::convert(Type const* leftType, Type const* rightType) {
	TypeConversion{*this}.convert(leftType, rightType);
}

void StackPusher::checkFit(Type const* type) {
	switch (type->category()) {
	case Type::Category::Integer: {
		auto it = to<IntegerType>(type);
		if (it->isSigned()) {
			if (it->numBits() != 257)
				*this << "FITS " + toString(it->numBits());
		} else
			*this << "UFITS " + toString(it->numBits());
		break;
	}
	case Type::Category::QInteger: {
		auto it2 = to<QIntegerType>(type);
		auto it = it2->asIntegerType();
		if (it->isSigned()) {
			if (it->numBits() != 257)
				*this << "QFITS " + toString(it->numBits());
		} else
			*this << "QUFITS " + toString(it->numBits());
		break;
	}
	case Type::Category::FixedPoint: {
		auto fp = to<FixedPointType>(type);
		if (fp->isSigned())
			*this << "FITS " + toString(fp->numBits());
		else
			*this << "UFITS " + toString(fp->numBits());
		break;
	}
	case Type::Category::VarInteger: {
		auto varint = to<VarIntegerType>(type);
		checkFit(&varint->asIntegerType());
		break;
	}
	case Type::Category::Enum: {
		auto enumType = to<EnumType>(type);
		int size = enumType->numberOfMembers();
		// TODO special case if size == 2**p
		pushS(0);
		pushInt(size);		// x x size
		*this << "LESS";	// x x<size
		pushS(1);			// x x<size x
		pushInt(-1);		// x x<size x -1
		*this << "GREATER"; // x x<size x>-1
		*this << "AND";		// x (x<size && x>-1)
		this->_throw("THROWIFNOT 4");
		break;
	}
	default:
		solUnimplemented(type->humanReadableName());
		break;
	}
}

void StackPusher::pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params) {
	for (ASTPointer<VariableDeclaration> const& variable: params) {
		getStack().add(variable.get(), true);
	}
}

void StackPusher::pushFragmentInCallRef(int take, int ret, std::string const& functionName) {
	startContinuation();
	pushFragment(take, ret, functionName);
	pushRefContAndCallX();
}

void StackPusher::pushCallOrCallRef(
	FunctionDefinition const* _functionDef,
	std::optional<std::pair<int, int>> const& deltaStack,
	bool const isCalledByPoint
) {
	auto [take, ret] =
		deltaStack.has_value()
			? deltaStack.value()
			: std::make_pair<int, int>(_functionDef->parameters().size(), _functionDef->returnParameters().size());

	std::string curFunctionName = ctx().currentFunctionName();
	auto const [functionName, id] = ctx().functionInternalName(_functionDef, isCalledByPoint);
	if (
		_functionDef->name() == "onCodeUpgrade" ||
		m_ctx->callGraph().tryToAddEdge(curFunctionName, functionName) // Does it have a loop?
	) {
		if (id < TvmConst::MaxCallN) {
			ctx().callGraph().addDictFunction(id, functionName);
			call(id, take, ret);
		} else {
			pushPrivateFunctionId(*_functionDef, isCalledByPoint);
			pushC3();
			callx(take + 2, ret);
		}
	} else {
		pushFragmentInCallRef(take, ret, functionName);
	}
}

void StackPusher::pushFragment(int take, int ret, std::string const& functionName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), functionName), "");
	pushStackGenOpcode(".inline " + functionName, take, ret);
}

void StackPusher::computeConstCell(std::string const& expName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), expName), "");
	pushCellOrSlice(createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREF_COMPUTE, expName, nullptr));
}

void StackPusher::computeConstSlice(std::string const& expName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), expName), "");
	pushCellOrSlice(
		createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREFSLICE_COMPUTE, expName, nullptr)
	);
}


void StackPusher::drop(int cnt) {
	solAssert(cnt >= 0, "");
	if (cnt >= 1) {
		auto opcode = makeDROP(cnt);
		fixStack(-cnt);
		m_instructions.back().push_back(opcode);
	}
}

void StackPusher::blockSwap(int down, int up) {
	solAssert(0 <= down, "");
	solAssert(0 <= up, "");
	if (down == 0 || up == 0)
		return;
	push(createNode<Stack>(Stack::Opcode::BLKSWAP, down, up));
}

void StackPusher::reverse(int qty, int startIndex) { push(makeREVERSE(qty, startIndex)); }

void StackPusher::dropUnder(int droppedCount, int leftCount) {
	// drop dropCount elements that are situated under top leftCount elements
	solAssert(leftCount >= 0, "");
	solAssert(droppedCount >= 0, "");

	if (droppedCount == 0) {
		// do nothing
	} else if (leftCount == 0) {
		drop(droppedCount);
	} else {
		push(createNode<Stack>(Stack::Opcode::BLKDROP2, droppedCount, leftCount));
		change(-droppedCount);
	}
}

void StackPusher::exchange(int i) {
	Pointer<Stack> opcode = makeXCH_S(i);
	push(opcode);
}

void StackPusher::exchange(int i, int j) {
	Pointer<Stack> opcode = makeXCH_S_S(i, j);
	push(opcode);
}

void StackPusher::rot() { push(makeROT()); }

void StackPusher::rotRev() { push(makeROTREV()); }

Type const* StackPusher::parseIndexType(Type const* type) {
	if (to<ArrayType>(type)) {
		return TypeProvider::uint(32);
	}
	if (auto mappingType = to<MappingType>(type)) {
		return mappingType->keyType();
	}
	solUnimplemented("");
}

void StackPusher::assignStackVariable(Declaration const* name) {
	auto& stack = getStack();
	int idx = stack.getOffset(name);
	solAssert(idx >= 0, "");
	if (idx == 0) {
		// nothing
	} else {
		popS(idx);
	}
}

void StackPusher::prepareKeyForDictOperations(Type const* key) {
	// stack: key
	if (key->category() == Type::Category::Struct) {
		StructCompiler sc{this, to<StructType>(key)};
		sc.tupleToBuilder();
		*this << "BTOS";
	}
}

std::pair<int, int> StackPusher::build_int_msg_info(
	std::set<int> const& isParamOnStack,
	std::map<int, std::string> const& constParams,
	bool isDestBuilder,
	std::function<void()> const& pushValue,
	std::function<void()> const& pushExtraFlags
) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//				 src:MsgAddress  dest:MsgAddressInt(#4)
	//				 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//				 created_lt:uint64  created_at:uint32
	//				 = CommonMsgInfoRelaxed;

	// currencies$_ grams:Grams other:ExtraCurrencyCollection = CurrencyCollection;

	static std::vector<int> const zeroes{1, 1, 1, 2, 2, 4, 1, 4, 4, 64, 32};
	int maxBits = 0;
	int maxRefs = 0;
	*this << "NEWC";
	this->stzeroes(1); // int_msg_info$0
	++maxBits;
	for (std::size_t param = 0; param < zeroes.size(); ++param) {
		solAssert(!constParams.contains(param) || !isParamOnStack.contains(param), "");

		if (param == 0) {
			solAssert(!constParams.contains(param) && !isParamOnStack.contains(param), "");
			// ihr_disabled:Bool
			this->stones(1);
			++maxBits;
		} else if (constParams.contains(param)) {
			std::string bitStr = constParams.at(param);
			appendToBuilder(bitStr);
			maxBits += bitStr.length();
		} else if (isParamOnStack.contains(param)) {
			switch (param) {
			case TvmConst::int_msg_info::bounce:
				*this << "STI 1";
				++maxBits;
				break;
			case TvmConst::int_msg_info::dest:
				if (isDestBuilder) {
					*this << "STB";
				} else {
					*this << "STSLICE";
				}
				maxBits += AddressInfo::stdAddrWithoutAnyCastLength();
				break;
			case TvmConst::int_msg_info::tons:
				solAssert(pushValue == nullptr, "");
				exchange(1);
				*this << "STVARUINT16";
				maxBits += VarUIntegerInfo::maxTonBitLength();
				break;
			case TvmConst::int_msg_info::currency:
				*this << "STDICT";
				++maxBits;
				++maxRefs;
				break;
			case TvmConst::int_msg_info::extra_flags:
				solAssert(pushExtraFlags == nullptr, "");
				exchange(1);
				*this << "STVARUINT16";
				maxBits += TvmConst::EXTRA_FLAG_SIZE;
				break;
			default:
				solUnimplemented("");
			}
		} else if (param == TvmConst::int_msg_info::tons) {
			pushValue();
			*this << "STVARUINT16";
			maxBits += VarUIntegerInfo::maxTonBitLength();
		} else if (pushExtraFlags && param == TvmConst::int_msg_info::extra_flags) {
			pushExtraFlags();
			*this << "STVARUINT16";
			maxBits += TvmConst::EXTRA_FLAG_SIZE;
		} else {
			int zeroQty = zeroes.at(param);
			std::string bitStr(zeroQty, '0');
			appendToBuilder(bitStr);
			maxBits += zeroQty;
			solAssert(param != TvmConst::int_msg_info::dest, "");
		}
	}
	return {maxBits, maxRefs};
}

int StackPusher::build_ext_msg_info(std::set<int> const& isParamOnStack) {
	// ext_out_msg_info$11 src:MsgAddressInt dest:MsgAddressExt
	// created_lt:uint64 created_at:uint32 = CommonMsgInfo;

	std::vector<int> zeroes{2, 2, 64, 32};
	*this << "NEWC";
	this->stones(2); // ext_out_msg_info$11
	int maxBitStringSize = 2;
	for (int param = 0; param < static_cast<int>(zeroes.size()); ++param) {
		if (!isParamOnStack.contains(param)) {
			int zeroQty = zeroes.at(param);
			maxBitStringSize += zeroQty;
			this->stzeroes(zeroQty);
		} else {
			if (param == TvmConst::ext_msg_info::dest) {
				*this << "STSLICE";
				maxBitStringSize += AddressInfo::externalAddressLength();
			} else {
				solUnimplemented("");
			}
		}
	}
	return maxBitStringSize;
}


void StackPusher::appendToBuilder(std::string const& bitString) {
	// stack: builder
	if (bitString.empty()) {
		return;
	}

	size_t count = std::ranges::count_if(bitString, [](char c) { return c == '0'; });
	if (count == bitString.size()) {
		stzeroes(count);
	} else {
		std::string const hex = StrUtils::binaryStringToSlice(bitString);
		if (hex.length() * 4 <= 8 * 7 + 1) {
			*this << "STSLICECONST x" + hex;
		} else {
			pushSlice("x" + StrUtils::binaryStringToSlice(bitString));
			this->blockSwap(1, 1);
			*this << "STSLICE";
		}
	}
}

void StackPusher::checkOptionalValue() {
	*this << "ISNULL";
	_throw("THROWIF " + toString(TvmConst::RuntimeException::GetOptionalException));
}

void StackPusher::stzeroes(int qty) {
	if (qty > 0) {
		// builder
		if (qty == 1) {
			*this << "STSLICECONST 0";
		} else {
			pushInt(qty); // builder qty
			*this << "STZEROES";
		}
	}
}

void StackPusher::stones(int qty) {
	if (qty > 0) {
		// builder
		if (qty == 1) {
			*this << "STSLICECONST 1";
		} else {
			pushInt(qty); // builder qty
			*this << "STONES";
		}
	}
}

void StackPusher::sendrawmsg() { *this << "SENDRAWMSG"; }

void StackPusher::pushParamsAndSendInternalMessage(
	std::map<int, Expression const*> const& exprs,
	std::map<int, std::string> const& constParams,
	std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
	std::function<void()> const& pushSendRawMsgFlag,
	std::function<std::pair<int, int>()> const& appendEitherStateInit,
	std::function<void()> const& pushValue,
	std::function<void()> const& pushExtraFlags
) {
	std::set<int> isParamOnStack;
	for (auto& [param, expr]: exprs | std::views::reverse) {
		isParamOnStack.insert(param);
		TVMExpressionCompiler{*this}.compileNewExpr(expr);
	}
	sendMessage(
		isParamOnStack,
		constParams,
		appendBody,
		appendEitherStateInit,
		pushSendRawMsgFlag,
		MsgType::Internal,
		false,
		pushValue,
		pushExtraFlags
	);
}

void StackPusher::prepareMessage(
	std::set<int> const& isParamOnStack,
	std::map<int, std::string> const& constParams,
	std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
	std::function<std::pair<int, int>()> const& appendEitherStateInit,
	MsgType messageType,
	bool isDestBuilder,
	std::function<void()> const& pushValue,
	std::function<void()> const& pushExtraFlags
) {
	int bitSizeBuilder = 0;
	int refSizeBuilder = 0;
	switch (messageType) {
	case MsgType::Internal:
		std::tie(bitSizeBuilder, refSizeBuilder) =
			build_int_msg_info(isParamOnStack, constParams, isDestBuilder, pushValue, pushExtraFlags);
		break;
	case MsgType::ExternalOut:
		bitSizeBuilder = build_ext_msg_info(isParamOnStack);
		break;
	}
	// stack: builder

	if (appendEitherStateInit) {
		// stack: values... builder
		appendToBuilder("1"); // Maybe = true
		++bitSizeBuilder;

		auto const [bits, refs] = appendEitherStateInit();
		bitSizeBuilder += bits;
		refSizeBuilder += refs; // Either StateInit ^StateInit
	} else {
		appendToBuilder("0"); // Maybe == false
		++bitSizeBuilder;
	}

	if (appendBody) {
		// stack: values... builder
		appendBody(bitSizeBuilder, refSizeBuilder);
		// stack: builder-with-body
	} else {
		appendToBuilder("0"); // there is no message body
	}

	// stack: builder'
	*this << "ENDC"; // stack: cell
}

void StackPusher::sendMessage(
	std::set<int> const& isParamOnStack,
	std::map<int, std::string> const& constParams,
	std::function<void(int bitSizeBuilder, int refSizeBuilder)> const& appendBody,
	std::function<std::pair<int, int>()> const& appendEitherStateInit,
	std::function<void()> const& pushSendRawMsgFlag,
	MsgType messageType,
	bool isDestBuilder,
	std::function<void()> const& pushValue,
	std::function<void()> const& pushExtraFlags
) {
	prepareMessage(
		isParamOnStack,
		constParams,
		appendBody,
		appendEitherStateInit,
		messageType,
		isDestBuilder,
		pushValue,
		pushExtraFlags
	);
	if (pushSendRawMsgFlag) {
		pushSendRawMsgFlag();
	} else {
		pushInt(TvmConst::SENDRAWMSG::DefaultFlag);
	}
	sendrawmsg();
}

int TVMStack::size() const { return m_size; }

void TVMStack::change(int diff) {
	if (diff != 0) {
		m_size += diff;
		solAssert(m_size >= 0, "TVMStack::change");
	}
}

void TVMStack::change(int take, int ret) {
	solAssert(take >= 0, "");
	solAssert(ret >= 0, "");
	change(-take + ret);
}

bool TVMStack::isParam(Declaration const* name) const { return getStackSize(name) != -1; }

void TVMStack::add(Declaration const* name, bool doAllocation) {
	solAssert(name != nullptr, "");
	if (doAllocation) {
		++m_size;
	}
	if (static_cast<int>(m_stackSize.size()) < m_size) {
		m_stackSize.resize(m_size);
	}
	m_stackSize.at(m_size - 1) = name;
}

int TVMStack::getOffset(Declaration const* name) const {
	solAssert(isParam(name), "");
	int stackSize = getStackSize(name);
	return getOffset(stackSize);
}

int TVMStack::getOffset(int stackSize) const { return m_size - 1 - stackSize; }

int TVMStack::getStackSize(Declaration const* name) const {
	int size = std::min<int>(m_stackSize.size(), m_size);
	for (int i = size - 1; i >= 0; --i) {
		if (m_stackSize.at(i) == name) {
			return i;
		}
	}
	return -1;
}

void TVMStack::ensureSize(int savedStackSize, std::string const& location, ASTNode const* node) const {
	if (node != nullptr && savedStackSize != m_size) {
		cast_error(
			*node,
			std::string{} +
				"Stack size error: expected: " +
				toString(savedStackSize) +
				" but real: " +
				toString(m_size) +
				" at " +
				location
		);
	}
	solAssert(
		savedStackSize == m_size,
		"stack: exp:" + toString(savedStackSize) + " real: " + toString(m_size) + " at " + location
	);
}

void TVMStack::takeLast(int n) {
	solAssert(m_size >= n, "");
	solAssert(static_cast<int>(m_stackSize.size()) >= m_size, "");
	m_stackSize.resize(m_size);
	m_stackSize = std::vector<Declaration const*>(m_stackSize.end() - n, m_stackSize.end());
	m_size = n;
	solAssert(static_cast<int>(m_stackSize.size()) == n, "");
}

InherHelper::InherHelper(ContractDefinition const* contract) {
	for (ContractDefinition const* c: contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const* _function: c->definedFunctions()) {
			std::set<CallableDeclaration const*> const& b = _function->annotation().baseFunctions;
			m_baseFunctions.insert(b.begin(), b.end());
		}
	}
}

bool InherHelper::isBaseFunction(CallableDeclaration const* d) const { return m_baseFunctions.contains(d); }

StorageLayout::StorageLayout(ContractDefinition const* contract):
	m_contract(contract) {
	auto const c4StateVars = usualStateVariables();
	for (VariableDeclaration const* variable: c4StateVars) {
		int index = TvmConst::C7::FirstIndexForVariables + m_stateVarIndex.size();
		m_stateVarIndex[variable] = index;
	}
	auto const& unpackedStateVars = unpackedStateVariables();
	for (VariableDeclaration const* variable: transientStateVars()) {
		int index = TvmConst::C7::FirstIndexForVariables + m_stateVarIndex.size() + (unpackedStateVars.empty() ? 0 : 1);
		m_stateVarIndex[variable] = index;
	}
}

int StorageLayout::getStateVarIndex(VariableDeclaration const* variable) const { return m_stateVarIndex.at(variable); }

std::vector<Type const*> StorageLayout::getC4Types() const {
	std::vector<Type const*> types;
	if (storePubkeyInC4())
		types.emplace_back(TypeProvider::uint256());
	if (storeTimestampInC4())
		types.emplace_back(TypeProvider::uint(64));
	if (hasConstructor())
		types.emplace_back(TypeProvider::boolean());

	auto declStateVars = usualAndUnpackedStateVariables();
	auto stateVarsTypes = getTypesFromVarDecls(declStateVars);
	types.insert(types.end(), stateVarsTypes.begin(), stateVarsTypes.end());
	return types;
}

std::vector<VariableDeclaration const*> StorageLayout::usualAndUnpackedStateVariables() const {
	auto stateVars = ::stateVariables(m_contract, StateVarType::Usual);
	auto unpacked = ::stateVariables(m_contract, StateVarType::Unpacked);
	stateVars.insert(stateVars.end(), unpacked.begin(), unpacked.end());
	return stateVars;
}

std::vector<VariableDeclaration const*> StorageLayout::usualStateVariables() const {
	return ::stateVariables(m_contract, StateVarType::Usual);
}

std::vector<VariableDeclaration const*> StorageLayout::unpackedStateVariables() const {
	return ::stateVariables(m_contract, StateVarType::Unpacked);
}

std::vector<VariableDeclaration const*> StorageLayout::transientStateVars() const {
	return ::stateVariables(m_contract, StateVarType::Transient);
}

bool StorageLayout::tooMuchStateVariables() const {
	return usualStateVariables().size() + (unpackedStateVariables().empty() ? 0 : 1) + transientStateVars().size() >=
		   TvmConst::C7::FirstIndexForVariables + 6;
}

FunctionDefinition const* StorageLayout::hasConstructor() const {
	for (ContractDefinition const* c: getContractsChain(m_contract)) {
		for (auto const f: c->definedFunctions()) {
			if (f->isConstructor())
				return f;
		}
	}
	return nullptr;
}

bool StorageLayout::storePubkeyInC4() const { return m_contract->externalMsgHeaders() != nullptr; }

bool StorageLayout::storeTimestampInC4() const {
	return m_contract->externalMsgHeaders() && m_contract->externalMsgHeaders()->hasTime();
}

int StorageLayout::getOffsetC4() const {
	return (storePubkeyInC4() ? 256 : 0) +								 // pubkey
		   (storeTimestampInC4() ? 64 : 0) + (hasConstructor() ? 1 : 0); // constructor flag
}

std::vector<std::pair<VariableDeclaration const*, int>> StorageLayout::getStaticVariables() const {
	int shift = 0;
	std::vector<std::pair<VariableDeclaration const*, int>> res;
	for (VariableDeclaration const* v: usualAndUnpackedStateVariables()) {
		if (v->isStatic()) {
			res.emplace_back(v, TvmConst::C4::PersistenceMembersStartIndex + shift++);
		}
	}
	return res;
}

int StorageLayout::getUnpackIndex() const {
	return TvmConst::C7::FirstIndexForVariables + usualStateVariables().size();
}

TVMCompilerContext::TVMCompilerContext(ContractDefinition const* contract, PragmaDirectiveHelper const& pragmaHelper):
	m_contract{contract},
	m_pragmaHelper{pragmaHelper},
	m_usage{*contract},
	m_inherHelper{contract},
	m_storageLayout{contract} {
	solAssert(m_contract, "");
	m_isUncheckedBlock.push(false);
	ignoreIntOverflow = m_pragmaHelper.hasIgnoreIntOverflow();
}
PragmaDirectiveHelper const& TVMCompilerContext::pragmaHelper() const { return m_pragmaHelper; }

bool TVMCompilerContext::isStdlib() const { return m_contract->name() == "stdlib"; }

std::pair<std::string, uint32_t>
TVMCompilerContext::functionInternalName(FunctionDefinition const* _function, bool calledByPoint) const {
	std::string functionName;
	if (isStdlib()) {
		functionName = _function->name();
	} else {
		std::string const hexName = _function->externalIdentifierHex();
		ContractDefinition const* contract = _function->annotation().contract;
		if (contract && contract->isLibrary())
			functionName = _function->annotation().contract->name() +
						   "_" +
						   (calledByPoint ? "with_obj_" : "") +
						   _function->name() +
						   "_" +
						   hexName;
		else if (_function->isFree())
			functionName = (calledByPoint ? "with_obj_" : "") + _function->name() + "_" + hexName + "_free_internal";
		else if (calledByPoint && isBaseFunction(_function) && _function->name() != "onCodeUpgrade")
			functionName = _function->annotation().contract->name() + "_" + _function->name() + "_" + hexName;
		else
			functionName = _function->name() + "_" + hexName + "_internal";
	}

	uint32_t id;
	if (_function->functionID().has_value())
		id = _function->functionID().value();
	else {
		id = ChainDataEncoder::toPrivateFunctionId(functionName);
	}
	return {functionName, id};
}

std::string TVMCompilerContext::getFunctionExternalName(FunctionDefinition const* _function) {
	std::string const& fname = _function->name();
	solAssert(_function->functionIsExternallyVisible(), "Internal error: expected public function: " + fname);
	if (_function->isConstructor()) {
		return "constructor";
	}
	if (_function->isFallback()) {
		return "fallback";
	}
	return fname;
}

ContractDefinition const* TVMCompilerContext::getContract() const { return m_contract; }

bool TVMCompilerContext::ignoreIntegerOverflow() const {
	solAssert(!m_isUncheckedBlock.empty(), "");
	return ignoreIntOverflow || m_isUncheckedBlock.top();
}

void TVMCompilerContext::addInlineFunction(std::string const& name, Pointer<CodeBlock> body) {
	solAssert(!m_inlinedFunctions.contains(name), "");
	m_inlinedFunctions[name] = std::move(body);
}

Pointer<CodeBlock> TVMCompilerContext::getInlinedFunction(std::string const& name) {
	return m_inlinedFunctions.at(name);
}

void TVMCompilerContext::addPublicFunction(
	FunctionDefinition const* function,
	uint32_t functionId,
	std::string const& functionName
) {
	if (function->isExternalMsg())
		addExternalMsgPublicFunction(functionId, functionName);
	else
		addInternalMsgPublicFunction(functionId, functionName);
}

void TVMCompilerContext::addExternalMsgPublicFunction(uint32_t functionId, std::string const& functionName) {
	m_extPublicFunctions.emplace_back(functionId, functionName);
}

void TVMCompilerContext::addInternalMsgPublicFunction(uint32_t functionId, std::string const& functionName) {
	m_intPublicFunctions.emplace_back(functionId, functionName);
}

std::vector<std::pair<uint32_t, std::string>> const& TVMCompilerContext::getExtPublicFunctions() {
	std::ranges::sort(m_extPublicFunctions);
	return m_extPublicFunctions;
}

std::vector<std::pair<uint32_t, std::string>> const& TVMCompilerContext::getIntPublicFunctions() {
	std::ranges::sort(m_intPublicFunctions);
	return m_intPublicFunctions;
}

bool TVMCompilerContext::isBaseFunction(CallableDeclaration const* d) const {
	solAssert(d->annotation().contract != nullptr, "");
	return m_inherHelper.isBaseFunction(d);
}

void StackPusher::pushEmptyArray() {
	pushInt(0);
	*this << "NULL";
	*this << "TUPLE 2";
}

void StackPusher::pushNull() { *this << "NULL"; }

void StackPusher::pushNaN() { *this << "PUSHNAN"; }

void StackPusher::pushEmptyCell() {
	pushCellOrSlice(createNode<CellOrSliceOperation>(CellOrSliceOperation::Type::PUSHREF, "", nullptr));
}

void StackPusher::pushDefaultValue(Type const* _type) {
	int returnValues = 1;
	startOpaque();
	Type::Category cat = _type->category();
	switch (cat) {
	case Type::Category::Address:
	case Type::Category::Contract:
	case Type::Category::AddressStd:
		pushSlice("x2_"); // addr_none$00 = MsgAddressExt;
		break;
	case Type::Category::Bool:
	case Type::Category::QBool:
	case Type::Category::FixedBytes:
	case Type::Category::Integer:
	case Type::Category::Enum:
	case Type::Category::VarInteger:
	case Type::Category::QInteger:
	case Type::Category::FixedPoint:
		*this << "PUSHINT 0";
		break;
	case Type::Category::Array:
	case Type::Category::TvmCell:
		if (cat == Type::Category::TvmCell || to<ArrayType>(_type)->isByteArrayOrString()) {
			pushEmptyCell();
			break;
		}
		pushEmptyArray();
		break;
	case Type::Category::Mapping:
		*this << "NULL";
		break;
	case Type::Category::Struct: {
		auto structType = to<StructType>(_type);
		StructCompiler structCompiler{this, structType};
		structCompiler.createDefaultStruct();
		break;
	}
	case Type::Category::TvmSlice:
		pushSlice("x8_");
		break;
	case Type::Category::TvmBuilder:
		*this << "NEWC";
		break;
	case Type::Category::Function: {
		pushInt(TvmConst::FunctionId::DefaultValueForFunctionType);
		break;
	}
	case Type::Category::Optional:
	case Type::Category::Variant:
		pushNull();
		break;
	case Type::Category::TvmVector:
		makeTuple(0);
		break;
	case Type::Category::TvmStack:
		pushNull();
		break;
	case Type::Category::UserDefinedValueType: {
		auto userDefValue = to<UserDefinedValueType>(_type);
		pushDefaultValue(&userDefValue->underlyingType());
		break;
	}
	case Type::Category::Tuple: {
		auto tuple = to<TupleType>(_type);
		for (Type const* comp: tuple->components())
			pushDefaultValue(comp);
		returnValues = tuple->components().size();
		break;
	}
	case Type::Category::StringBuilder: {
		pushFragment(0, 1, "__createStringBuilder");
		break;
	}
	default:
		solUnimplemented("");
	}
	endOpaque(0, returnValues, true);
}

void StackPusher::getDict(Type const& keyType, Type const& valueType, GetDictOperation const op) {
	GetFromDict d(*this, keyType, valueType, op, std::nullopt);
	d.getDict();
}

void StackPusher::getAndSetDict(
	Type const& keyType,
	Type const& valueType,
	GetDictOperation const op,
	DataType const inputValueType
) {
	GetFromDict d(*this, keyType, valueType, op, inputValueType);
	d.getDict();
}

void StackPusher::byteLengthOfCell() {
	pushInt(0xFFFFFFFF);
	*this << "CDATASIZE";
	drop(1);
	popS(1);
	*this << "RSHIFT 3";
}

void StackPusher::checkCtorCalled() {
	getGlob(TvmConst::C7::ConstructorFlag);
	_throw("THROWIFNOT " + toString(TvmConst::RuntimeException::CallThatWasBeforeCtorCall));
}

void StackPusher::checkIfCtorCalled(bool ifFlag) {
	startContinuation();
	checkCtorCalled();
	endContinuation();
	if (ifFlag) {
		ifJmp();
	} else {
		ifNotJmp();
	}
}

void StackPusher::add(StackPusher const& pusher) {
	solAssert(pusher.m_instructions.size() == 1, "");
	for (Pointer<TvmAstNode> const& op: pusher.m_instructions.back()) {
		m_instructions.back().emplace_back(op);
	}
}

void StackPusher::clear() {
	m_instructions.clear();
	m_instructions.emplace_back();
}

void StackPusher::takeLast(int n) { m_stack.takeLast(n); }

void TypeConversion::convert(Type const* leftType, Type const* rightType) {
	// TODO separate implicit conversion and explicit conversion

	// opt(opt(opt(opt(T)))) = T;
	// opt(opt(opt(opt(T0, T1, T2)))) = (T0, T1, T2);
	int lQty = optTypeQty(leftType);
	int rQty = optTypeQty(rightType);
	solAssert(lQty >= rQty, "");
	if (lQty > rQty) {
		auto l = to<OptionalType>(leftType);
		convert(l->valueType(), rightType);
		// optional(uint, uint) q = (1, 2);
		if (l->valueType()->category() == Type::Category::Tuple) {
			if (rightType->category() != Type::Category::Null) {
				auto tt = to<TupleType>(l->valueType());
				m_pusher.makeTuple(tt->components().size());
			}
			// optional([mapping|optional]) q = ...
		} else if (optValueAsTuple(l->valueType())) {
			m_pusher.makeTuple(1);
		}
		return;
	}

	switch (rightType->category()) {
	case Type::Category::RationalNumber:
		fromRational(leftType, to<RationalNumberType>(rightType));
		break;
	case Type::Category::FixedPoint:
		fromFixedPoint(leftType, to<FixedPointType>(rightType));
		break;
	case Type::Category::VarInteger:
		fromInteger(leftType, &to<VarIntegerType>(rightType)->asIntegerType());
		break;
	case Type::Category::Integer:
		fromInteger(leftType, to<IntegerType>(rightType));
		break;
	case Type::Category::FixedBytes:
		fromFixedBytesType(leftType, to<FixedBytesType>(rightType));
		break;
	case Type::Category::Array:
		fromArray(leftType, to<ArrayType>(rightType));
		break;
	case Type::Category::Optional:
		fromOptional(leftType, to<OptionalType>(rightType));
		break;
	case Type::Category::TvmSlice:
		fromSlice(leftType);
		break;
	case Type::Category::Tuple:
		fromTuple(leftType, to<TupleType>(rightType));
		break;
	case Type::Category::StringLiteral:
		fromStringLiteral(leftType, to<StringLiteralType>(rightType));
		break;
	case Type::Category::TvmCell:
		fromCell(leftType);
		break;
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Bool:
	case Type::Category::Contract:
	case Type::Category::EmptyMap:
	case Type::Category::Enum:
	case Type::Category::Function:
	case Type::Category::Mapping:
	case Type::Category::Null:
	case Type::Category::QBool:
	case Type::Category::QInteger:
	case Type::Category::Struct:
	case Type::Category::TVMNaN:
	case Type::Category::TvmBuilder:
	case Type::Category::TvmStack:
	case Type::Category::TvmVector:
	case Type::Category::UserDefinedValueType:
	case Type::Category::Variant:
		break;
	default:
		solUnimplemented(rightType->toString());
		break;
	}
}

void TypeConversion::integerToInteger(IntegerType const* leftType, IntegerType const* rightType) const {
	if (rightType->isImplicitlyConvertibleTo(*leftType))
		return;

	bigint x = (bigint(1) << leftType->numBits()) - 1;
	m_pusher << "PUSHINT " + toString(x) << "AND";

	if (leftType->isSigned()) {
		m_pusher.startOpaque();
		m_pusher.pushS(0);
		m_pusher.pushInt((bigint(1) << (leftType->numBits() - 1)) - 1);
		m_pusher << "GREATER";
		m_pusher.startContinuation();
		if (rightType->isSigned()) {
			m_pusher.pushInt(bigint(1) << leftType->numBits());
			m_pusher << "SUB";
		} else {
			m_pusher.pushInt(-(bigint(1) << leftType->numBits()));
			m_pusher << "ADD";
		}
		m_pusher.endContinuation();
		m_pusher._if();
		m_pusher.endOpaque(1, 1, true);
	}
}

void TypeConversion::fixedPointToInteger(IntegerType const* leftType, FixedPointType const* rightType) const {
	int powerDiff = rightType->fractionalDigits();
	if (powerDiff > 0) {
		m_pusher.pushInt(MathConsts::power10().at(powerDiff));
		m_pusher << "DIV";
	}
	integerToInteger(leftType, rightType->asIntegerType());
}

void TypeConversion::fixedPointToFixedPoint(FixedPointType const* leftType, FixedPointType const* rightType) const {
	int powerDiff = leftType->fractionalDigits() - rightType->fractionalDigits();
	if (powerDiff != 0) {
		if (powerDiff > 0) {
			m_pusher.pushInt(MathConsts::power10().at(powerDiff));
			m_pusher << "MUL"; // TODO use MULDIVMOD to avoid overflow
		} else {
			m_pusher.pushInt(MathConsts::power10().at(-powerDiff));
			m_pusher << "DIV";
		}
	}
	integerToInteger(leftType->asIntegerType(), rightType->asIntegerType());
}

void TypeConversion::integerToFixedPoint(FixedPointType const* leftType, IntegerType const* rightType) const {
	int powerDiff = leftType->fractionalDigits();
	if (powerDiff > 0) {
		m_pusher.pushInt(MathConsts::power10().at(powerDiff));
		m_pusher << "MUL"; // TODO use MULDIVMOD to avoid overflow
	}
	integerToInteger(leftType->asIntegerType(), rightType);
}

void TypeConversion::fixedBytesToFixedBytes(FixedBytesType const* leftType, FixedBytesType const* rightType) const {
	int diff = 8 * (leftType->numBytes() - rightType->numBytes());
	if (diff > 0) {
		m_pusher << "LSHIFT " + std::to_string(diff);
	} else if (diff < 0) {
		m_pusher << "RSHIFT " + std::to_string(-diff);
	}
}

auto TypeConversion::bytesToFixedBytes(FixedBytesType const* rightType) const -> void {
	size_t bits = rightType->numBytes() * 8;
	m_pusher.startOpaque();
	m_pusher << "CTOS"; // slice
	m_pusher.pushAsym("LDUQ " + std::to_string(bits));
	// data slice flag

	// if load succeeded drop slice
	m_pusher.startContinuation();
	m_pusher.drop(1);
	m_pusher.endContinuation();

	// if load failed load all available data
	m_pusher.startContinuation();
	// slice
	m_pusher.pushS(0);
	m_pusher << "SBITS";
	// slice slice_bits
	m_pusher.pushS(0);
	// slice slice_bits slice_bits
	m_pusher.rotRev();
	// slice_bits slice slice_bits
	m_pusher << "PLDUX";
	// slice_bits number
	m_pusher.blockSwap(1, 1);
	// number slice_bits
	m_pusher << "NEGATE";
	m_pusher.pushInt(bits);
	m_pusher << "ADD";
	m_pusher << "LSHIFT";
	// number with trailing zeros
	m_pusher.endContinuation();
	m_pusher.ifElse();
	m_pusher.endOpaque(1, 1);
}

void TypeConversion::stringLiteralToFixedBytes(
	FixedBytesType const* leftType,
	StringLiteralType const* rightType
) const {
	size_t bytes = 0;
	u256 value = 0;
	for (char c: rightType->value()) {
		auto x = static_cast<uint8_t>(c);
		value = value * 256 + x;
		++bytes;
	}
	while (bytes < leftType->numBytes()) {
		value *= 256;
		++bytes;
	}
	m_pusher.drop(1); // delete old value
	m_pusher << "PUSHINT " + toString(value);
}

void TypeConversion::fromFixedPoint(Type const* leftType, FixedPointType const* rightType) const {
	switch (leftType->category()) {
	case Type::Category::FixedPoint:
		fixedPointToFixedPoint(to<FixedPointType>(leftType), rightType);
		break;
	case Type::Category::Integer:
		fixedPointToInteger(to<IntegerType>(leftType), rightType);
		break;
	case Type::Category::VarInteger:
		fixedPointToInteger(&to<VarIntegerType>(leftType)->asIntegerType(), rightType);
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::convertIntegerToAddress(Type const* t) const {
	if (auto r = to<RationalNumberType>(t)) {
		m_pusher.drop();
		m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(StrUtils::literalToSliceAddress(r->value2())));
	} else {
		m_pusher << "NEWC";
		m_pusher
			<< "STSLICECONST x801_"; // addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 // 10 0  00000000 1 = 801
		m_pusher << "STU 256";		 // address:bits256
		m_pusher << "BTOS";
	}
}

void TypeConversion::convertIntegerToLibraryContinuation() const {
	convertIntegerToLibraryExoticCell();
	m_pusher << "CTOS";
	m_pusher << "BLESS";
}

void TypeConversion::convertIntegerToLibraryExoticCell() const {
	m_pusher << "PUSHINT 2"; // cell type: library
	m_pusher << "NEWC";
	m_pusher << "STU 8";
	m_pusher << "STU 256"; // library hash
	m_pusher << "TRUE";
	m_pusher << "ENDXC";
}

void TypeConversion::convertIntegerToEnum(EnumType const* leftType, IntegerType const* /*rightType*/) const {
	int const size = leftType->enumDefinition().members().size();
	m_pusher.pushInt(size);
	m_pusher << "MOD";
}

void TypeConversion::fromInteger(Type const* leftType, IntegerType const* rightType) const {
	switch (leftType->category()) {
	case Type::Category::FixedPoint:
		integerToFixedPoint(to<FixedPointType>(leftType), rightType);
		break;
	case Type::Category::Integer:
		integerToInteger(to<IntegerType>(leftType), rightType);
		break;
	case Type::Category::VarInteger:
		integerToInteger(&to<VarIntegerType>(leftType)->asIntegerType(), rightType);
		break;
	case Type::Category::QInteger:
		integerToInteger(to<QIntegerType>(leftType)->asIntegerType(), rightType);
		break;
	case Type::Category::Function: {
		m_pusher.ctx().setPragmaSaveAllFunctions();
		break;
	}
	case Type::Category::FixedBytes:
		// do nothing here
		break;
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Contract: {
		auto leftContract = to<ContractType>(leftType);
		if (leftContract && leftContract->contractDefinition().isContractLibrary()) {
			convertIntegerToLibraryContinuation();
		} else {
			convertIntegerToAddress(rightType);
		}
		break;
	}
	case Type::Category::Enum:
		convertIntegerToEnum(to<EnumType>(leftType), rightType);
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::fromRational(Type const* leftType, RationalNumberType const* rightType) const {
	switch (leftType->category()) {
	case Type::Category::FixedPoint: {
		auto fixedPointLeft = to<FixedPointType>(leftType);
		Type const* mob = rightType->mobileType();
		if (auto intRight = to<IntegerType>(mob)) {
			integerToFixedPoint(fixedPointLeft, intRight);
		} else {
			auto fixedRight = to<FixedPointType>(mob);
			solAssert(fixedRight, "");
			fixedPointToFixedPoint(fixedPointLeft, fixedRight);
		}
		break;
	}
	case Type::Category::Integer:
	case Type::Category::QInteger:
	case Type::Category::VarInteger:
		break;
	case Type::Category::Function: {
		m_pusher.ctx().setPragmaSaveAllFunctions();
		break;
	}
	case Type::Category::Enum:
	case Type::Category::FixedBytes:
		// do nothing here
		break;
	case Type::Category::Address:
	case Type::Category::AddressStd:
	case Type::Category::Contract:
		convertIntegerToAddress(rightType);
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::tupleFromTuple(TupleType const* leftType, TupleType const* rightType) {
	std::vector<Type const*> const& lc = leftType->components();
	std::vector<Type const*> const& rc = rightType->components();
	solAssert(lc.size() == rc.size(), "");
	int n = lc.size();
	for (int i = n - 1; 0 <= i; --i) {
		convert(lc.at(i), rc.at(i));
		if (n >= 2) {
			m_pusher.blockSwap(n - 1, 1);
		}
	}
}

void TypeConversion::fromFixedBytesType(Type const* leftType, FixedBytesType const* rightType) const {
	switch (leftType->category()) {
	case Type::Category::Address:
	case Type::Category::AddressStd:
		convertIntegerToAddress(rightType);
		break;
	case Type::Category::FixedBytes: {
		fixedBytesToFixedBytes(to<FixedBytesType>(leftType), rightType);
		break;
	}
	case Type::Category::Integer: {
		auto intType = to<IntegerType>(leftType);
		if (intType && !intType->isSigned() && (intType->numBits() >= 8 * rightType->numBytes()))
			break;
		solUnimplemented("");
		break;
	}
	case Type::Category::FixedPoint: {
		auto fixedPoint = to<FixedPointType>(leftType);
		integerToInteger(fixedPoint->asIntegerType(), TypeProvider::uint(8 * rightType->numBytes()));
		break;
	}
	case Type::Category::Array: {
		auto stringType = to<ArrayType>(leftType);
		solAssert(stringType->isByteArrayOrString(), "");
		m_pusher << "NEWC";
		m_pusher << "STU " + toString(8 * rightType->numBytes());
		m_pusher << "ENDC";
		break;
	}
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::fromArray(Type const* leftType, ArrayType const* rightType) const {
	auto r = to<ArrayType>(rightType);
	if (!r->isByteArrayOrString()) {
		return;
	}
	// bytes or string
	switch (leftType->category()) {
	case Type::Category::FixedBytes:
		bytesToFixedBytes(to<FixedBytesType>(leftType));
		break;
	case Type::Category::Array:
		break;
	case Type::Category::TvmSlice:
		m_pusher << "CTOS";
		break;
	default:
		solUnimplemented("");
		break;
	}
}

void TypeConversion::fromOptional(Type const* leftType, OptionalType const* rightType) {
	switch (leftType->category()) {
	case Type::Category::Optional: {
		auto l = to<OptionalType>(leftType);
		m_pusher.startOpaque();

		m_pusher.pushS(0);
		m_pusher << "ISNULL";
		m_pusher.fixStack(-1); // fix stack

		m_pusher.startContinuation();
		if (optValueAsTuple(l->valueType())) {
			m_pusher.untuple(1);
		} else if (auto tt = to<TupleType>(l->valueType())) {
			m_pusher.untuple(tt->components().size());
		}
		convert(l->valueType(), rightType->valueType());
		if (optValueAsTuple(l->valueType())) {
			m_pusher.makeTuple(1);
		} else if (auto tt = to<TupleType>(l->valueType())) {
			m_pusher.makeTuple(tt->components().size());
		}
		m_pusher.endContinuation();
		m_pusher.ifNot();

		m_pusher.endOpaque(1, 1, true);
		break;
	}
	default:
		break;
	}
}

void TypeConversion::fromSlice(Type const* leftType) const {
	switch (leftType->category()) {
	case Type::Category::TvmSlice:
		break;
	case Type::Category::Array: {
		auto arrType = to<ArrayType>(leftType);
		solAssert(arrType->isByteArrayOrString(), "");
		m_pusher << "NEWC"	  // s b
				 << "STSLICE" // b'
				 << "ENDC";	  // cell
		break;
	}
	default:
		solUnimplemented("");
	}
}

void TypeConversion::fromTuple(Type const* leftType, TupleType const* rightType) {
	switch (leftType->category()) {
	case Type::Category::Tuple:
		tupleFromTuple(to<TupleType>(leftType), rightType);
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::fromStringLiteral(Type const* leftType, StringLiteralType const* rightType) const {
	switch (leftType->category()) {
	case Type::Category::FixedBytes:
		stringLiteralToFixedBytes(to<FixedBytesType>(leftType), rightType);
		break;
	case Type::Category::Array:
		break;
	case Type::Category::TvmSlice: {
		m_pusher.drop();
		auto value = rightType->value();
		if (value == "" || value == "_") {
			value = "8_";
		}
		m_pusher.pushSlice("x" + value);
		break;
	}
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::fromCell(Type const* leftType) const {
	switch (leftType->category()) {
	case Type::Category::Contract: {
		auto contractType = to<ContractType>(leftType);
		solAssert(contractType->contractDefinition().isContractLibrary(), "");
		m_pusher << "CTOS";
		m_pusher << "BLESS";
		break;
	}
	case Type::Category::TvmCell:
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}
