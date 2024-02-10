/*
 * Copyright (C) 2020-2023 EverX. All Rights Reserved.
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

#include <libsolidity/ast/TypeProvider.h>

#include "DictOperations.hpp"
#include "TVMPusher.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMABI.hpp"
#include "TVMConstants.hpp"

#include <boost/range/adaptor/map.hpp>
#include <utility>

using namespace solidity::frontend;
using namespace solidity::util;
using namespace solidity;
using namespace std;

StackPusher::StackPusher(TVMCompilerContext *ctx, const int stackSize) :
	m_ctx(ctx)
{
	change(stackSize);
	m_instructions.emplace_back();
}

void StackPusher::pushLoc(const std::string& file, int line) {
	auto op = createNode<Loc>(file, line);
	m_instructions.back().emplace_back(op);
}

void StackPusher::pushString(const std::string& _str, bool toSlice) {
	std::string hexStr = StrUtils::stringToHex(_str); // 2 * len(_str) == len(hexStr). One symbol to 2 hex digits
	solAssert(hexStr.length() % 2 == 0, "");
	if (4 * hexStr.length() <= TvmConst::MaxPushSliceBitLength && toSlice) {
		pushSlice("x" + hexStr);
		return ;
	}

	const int saveStackSize = stackSize();
	m_instructions.back().emplace_back(makePushCellOrSlice(hexStr, toSlice));
	change(0, 1);

	ensureSize(saveStackSize + 1, "");
}

void StackPusher::pushLog() {
	*this << "CTOS";
	*this << "STRDUMP";
	drop();
}

// TODO move to function compiler
Pointer<Function> StackPusher::generateC7ToC4(bool forAwait) {
	const std::vector<Type const *>& memberTypes = m_ctx->c4StateVariableTypes();
	const int stateVarQty = memberTypes.size();
	if (ctx().tooMuchStateVariables()) {
		const int saveStack = stackSize();
		pushC7();
		*this << "FALSE";
		setIndexQ(stateVarQty + TvmConst::C7::FirstIndexForVariables);
		unpackFirst(stateVarQty + TvmConst::C7::FirstIndexForVariables);
		reverse(stateVarQty + TvmConst::C7::FirstIndexForVariables, 0);
		drop(TvmConst::C7::FirstIndexForVariables);
		solAssert(saveStack + stateVarQty == stackSize(), "");
	} else {
		for (int i = stateVarQty - 1; i >= 0; --i)
			getGlob(TvmConst::C7::FirstIndexForVariables + i);
	}
	if (ctx().storeTimestampInC4())
		getGlob(TvmConst::C7::ReplayProtTime);
	getGlob(TvmConst::C7::TvmPubkey);
	*this << "NEWC";
	*this << "STU 256";
	if (ctx().storeTimestampInC4()) {
		*this << "STU 64";
	}
	*this << "STONE"; // constructor flag
	if (forAwait) {
		*this << "STONE";         // remoteAddr stateVars... b
		blockSwap(1, stateVarQty + 1); // stateVars... b remoteAddr
		push(createNode<HardCode>(std::vector<std::string>{
				"NEWC",    // stateVars... b remoteAddr B
				"STSLICE", // stateVars... b B
				"PUSH c0",
				"PUSH c3",
				"PUSHCONT {",
				"	SETCONT c3",
				"	SETCONT c0",
				// 5SysVars... funcStack... stateVars... b B cont
				"	PUSHINT " + toString(stateVarQty),
				"	ADDCONST 2",
				"	PUSHINT 1",
				"	BLKSWX",
				// 5SysVars... funcStack... cont stateVars... b B
				"	DEPTH",
				"	ADDCONST -7", // 5 sys vars + 2 builders
				"	PUSHINT " + toString(stateVarQty), // and stateVars
				"	SUB",
				"	PUSHINT 2",
				"	PUSHINT " + toString(stateVarQty),
				"	ADD",
				"	BLKSWX",
				// 5SysVars... stateVars... b B funcStack... cont
				"	GETGLOB " + toString(TvmConst::C7::MsgPubkey),
				"	GETGLOB " + toString(TvmConst::C7::SenderAddress),
				"	GETGLOB " + toString(TvmConst::C7::AwaitAnswerId),
				// 5SysVars... stateVars... b B funcStack... cont msgPubKey senderAddr AwaitAnswerId
				"	BLKSWAP 1, 3",
				// 5SysVars... stateVars... b B funcStack... msgPubKey senderAddr AwaitAnswerId cont
				"	DEPTH",
				"	ADDCONST -8", // 5 sys vars + 2 builders + cont
				"	PUSHINT " + toString(stateVarQty), // and stateVars
				"	SUB",
				"	PUSHINT -1",
				"	SETCONTVARARGS",
				// 5SysVars... stateVars... b B cont
				"	SWAP",
				// 5SysVars... stateVars... b cont B
				"	STCONT",
				// 5SysVars... stateVars... b B
				"	ENDC",
				// 5SysVars... stateVars... b suspendedCodeCell
				"	STREFR",
				// 5SysVars... stateVars... b
		}, 0, 0, false));
	} else {
		if (m_ctx->usage().hasAwaitCall()) {
			*this << "STZERO";
		}
	}
	if (!memberTypes.empty()) {
		ChainDataEncoder encoder{this};
		DecodePositionAbiV2 position{m_ctx->getOffsetC4(), m_ctx->usage().hasAwaitCall() ? 1 : 0, memberTypes};
		encoder.encodeParameters(memberTypes, position);
	}

	if (forAwait) {
		push(createNode<HardCode>(std::vector<std::string>{
				// 5SysVars... b
				"	ENDC",
				"	POPROOT",
				"	THROW 0",
				"}",
				"CALLCC",
		}, 0, 0, false));
	} else {
		*this << "ENDC";
		popRoot();
	}
	Pointer<CodeBlock> block = getBlock();
	auto f = createNode<Function>(0, 0, (forAwait ? "c7_to_c4_for_await" : "c7_to_c4"), nullopt, Function::FunctionType::Fragment,
			block);
	return f;
}

bool StackPusher::doesFitInOneCellAndHaveNoStruct(Type const* key, Type const* value) {
	int keyLength = dictKeyLength(key);
	return
		TvmConst::MAX_HASH_MAP_INFO_ABOUT_KEY +
		keyLength +
		ABITypeSize{value}.maxBits
		<
		TvmConst::CellBitLength;
}

DataType
StackPusher::prepareValueForDictOperations(Type const *keyType, Type const *valueType) {
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
		case DictValueType::Function:
		{
			*this << "NEWC";
			store(valueType, false);
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
		case DictValueType::Function:
		{
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

// false - value isn't in ref
// true - value is in ref
void StackPusher::recoverKeyAndValueAfterDictOperation(
	Type const* keyType,
	Type const* valueType,
	bool haveKey,
	bool didUseOpcodeWithRef,
	const DecodeType& decodeType,
	bool saveOrigKeyAndNoTuple
)
{
	const bool isValueStruct = valueType->category() == Type::Category::Struct;
	const bool pushRefCont =
		isValueStruct &&
		!didUseOpcodeWithRef &&
		!doesDictStoreValueInRef(keyType, valueType);

	// stack: value [key]
	auto preloadValue = [&]() {
		if (haveKey) {
			// stack: value key
			if (saveOrigKeyAndNoTuple) {
				pushS(0); // stack: value key key
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
			case DictValueType::TvmSlice:
			{
				if (didUseOpcodeWithRef) {
					*this << "CTOS";
				} else if (doesDictStoreValueInRef(keyType, valueType)) {
					*this << "PLDREF";
					*this << "CTOS";
				}
				break;
			}
			case DictValueType::Array:
				if (isByteArrayOrString(valueType)) {
					if (!didUseOpcodeWithRef) {
						*this << "PLDREF";
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
			case DictValueType::Function:
			{
				bool pushCallRef = false;
				if (didUseOpcodeWithRef) {
					*this << "CTOS";
					pushCallRef = true;
				} else if (doesDictStoreValueInRef(keyType, valueType)) {
					*this << "PLDREF";
					*this << "CTOS";
					pushCallRef = true;
				}
				pushCallRef &= isValueStruct;
				if (pushCallRef) {
					startContinuation();
				}
				preload(valueType);
				if (pushCallRef) {
					pushRefContAndCallX(1, 1, false);
				}
				break;
			}
			case DictValueType::TvmCell:
			{
				if (!didUseOpcodeWithRef) {
					*this << "PLDREF";
				}
				break;
			}
		}
	};

	auto checkOnMappingOrOptional = [&]() {
		if (optValueAsTuple(valueType)) {
			makeTuple(1);
		}
	};

	switch (decodeType) {
		case DecodeType::DecodeValue:
			if (pushRefCont) {
				startContinuation();
			}
			preloadValue();
			if (pushRefCont) {
				pushRefContAndCallX(1, 1, false);
			}
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
			if (haveKey) {
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

void StackPusher::setDict(Type const &keyType, Type const &valueType, const DataType& dataType, SetDictOperation operation) {
	DictSet d{*this, keyType, valueType, dataType, operation};
	d.dictSet();
}

void StackPusher::pushInlineFunction(std::string const& name, int take, int ret) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), name), "");
	auto block = ctx().getInlinedFunction(name);
	solAssert(block->type() == CodeBlock::Type::None, "");
	for (Pointer<TvmAstNode> const& i : block->instructions())
		m_instructions.back().emplace_back(i);
	change(take, ret);
}

void StackPusher::pollLastRetOpcode() {
	std::vector<Pointer<TvmAstNode>>& opcodes = m_instructions.back();
	int offset = 0;
	int size = opcodes.size();
	while (offset < size && isLoc(opcodes.at(opcodes.size() - 1 - offset)))
		++offset;
	int begPos = size - 1 - offset;

	auto opcode = to<ReturnOrBreakOrCont>(opcodes.at(begPos).get());
	solAssert(opcode, "");
	vector<Pointer<TvmAstNode>> instructions = opcode->body()->instructions();
	solAssert(!instructions.empty(), "");
	auto ret = to<TvmReturn>(instructions.back().get());
	solAssert(ret, "");
	solAssert(!ret->withIf() && !ret->withAlt(), "");
	instructions.pop_back();

	opcodes.erase(opcodes.begin() + begPos);
	opcodes.insert(opcodes.begin() + begPos, instructions.begin(), instructions.end());
}

bool StackPusher::tryPollEmptyPushCont() {
	std::vector<Pointer<TvmAstNode>>& opcodes = m_instructions.back();
	solAssert(opcodes.size() >= 2, "");
	auto block = dynamic_pointer_cast<CodeBlock>(opcodes.back());
	solAssert(block != nullptr, "");
	if (block->instructions().empty()) {
		opcodes.pop_back();
		return true;
	}
	return false;
}

TVMCompilerContext &StackPusher::ctx() {
	return *m_ctx;
}

void StackPusher::change(int delta) {
	solAssert(lockStack >= 0, "");
	if (lockStack == 0) {
		m_stack.change(delta);
	}
}

void StackPusher::change(int take, int ret) {
	change(-take + ret);
}

int StackPusher::stackSize() const {
	return m_stack.size();
}

void StackPusher::ensureSize(int savedStackSize, const string &location, const ASTNode* node) {
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

Pointer<AsymGen>
StackPusher::makeAsym(const string& cmd) {
	static std::set<string> asymOpcodes;
	if (asymOpcodes.empty()) {
		for (std::string type : {"", "I", "U"}) {
			for (std::string suf : {"", "REF"}) {
				for (std::string op : {"MIN", "MAX"}) {
					asymOpcodes.insert("DICT" + type + "REM" + op + suf);
					asymOpcodes.insert("DICT" + type + op + suf);
				}
			}

			for (std::string op : {"SETGET", "ADDGET", "REPLACEGET"}) {
				for (std::string suf : {"", "REF", "B"})
					asymOpcodes.insert("DICT" + type + op + suf);
			}

			for (std::string op : {"DELGET"})
				for (std::string suf : {"", "REF"})
					asymOpcodes.insert("DICT" + type + op + suf);

			for (std::string suf : {"", "REF", "PREV", "PREVEQ", "NEXT", "NEXTEQ"})
				asymOpcodes.insert("DICT" + type + "GET" + suf);
		}

		for (std::string preload : {"", "P"})
			for (std::string type : {"I", "U"}) {
				for (std::string size : {"4", "8"})
					asymOpcodes.insert(preload + "LD" + type + "LE" + size + "Q");
				for (std::string x : {"", "X"})
					asymOpcodes.insert(preload + "LD" + type + x + "Q");
			}

		asymOpcodes.insert("CDATASIZEQ");
		asymOpcodes.insert("CONFIGPARAM");
		asymOpcodes.insert("LDDICTQ");
		asymOpcodes.insert("LDMSGADDRQ");
		asymOpcodes.insert("LDSLICEQ");
		asymOpcodes.insert("LDSLICEXQ");
		asymOpcodes.insert("NULLROTRIFNOT");
		asymOpcodes.insert("NULLSWAPIF");
		asymOpcodes.insert("NULLSWAPIFNOT");
		asymOpcodes.insert("PLDSLICEQ");
		asymOpcodes.insert("PLDSLICEXQ");
		asymOpcodes.insert("SDATASIZEQ");
		asymOpcodes.insert("SPLITQ");
	}

	istringstream iss(cmd);
	string baseCmd;
	iss >> baseCmd;
	solAssert(asymOpcodes.count(baseCmd) > 0, "Unknown asym opcode: " + cmd);

	return createNode<AsymGen>(cmd);
}

void StackPusher::push(const Pointer<Stack>& opcode) {
	m_instructions.back().push_back(opcode);
}

void StackPusher::push(const Pointer<AsymGen>& opcode) {
	// no stack changing
	solAssert(lockStack >= 0, "");
	m_instructions.back().push_back(opcode);
}

void StackPusher::push(const Pointer<HardCode>& opcode) {
	m_instructions.back().push_back(opcode);
	change(opcode->take(), opcode->ret());
}

void StackPusher::pushAsym(Pointer<AsymGen>&& node) {
	m_instructions.back().push_back(node);
}

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
	Pointer<StackOpcode> opcode = gen(cmd);
	change(opcode->take(), opcode->ret());
	m_instructions.back().push_back(opcode);
}

void StackPusher::fixStack(int stackDiff) {
	change(stackDiff);
}

void StackPusher::pushCellOrSlice(const Pointer<PushCellOrSlice>& opcode) {
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
	std::string funName = ctx().getFunctionInternalName(&funDef, isCalledByPoint);
	uint32_t id = funDef.functionID().has_value() ? funDef.functionID().value() : ChainDataEncoder::toHash256(funName);
	pushInt(id);
	ctx().callGraph().addPrivateFunction(id, funName);
}

void StackPusher::startContinuation() {
	m_instructions.emplace_back();
}

void StackPusher::endCont(CodeBlock::Type type) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(type, block);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(b);
}

void StackPusher::endContinuation() {
	endCont(CodeBlock::Type::PUSHCONT);
}

void StackPusher::endContinuationFromRef() {
	endCont(CodeBlock::Type::PUSHREFCONT);
}

void StackPusher::endRetOrBreakOrCont(int _take) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(CodeBlock::Type::None, block);
	auto r = createNode<ReturnOrBreakOrCont>(_take, b);
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

void StackPusher::callRefOrCallX(int take, int ret, bool _isJmp, CodeBlock::Type _blockType, bool isPure) {
	solAssert(!m_instructions.empty(), "");
	std::vector<Pointer<TvmAstNode>> block = m_instructions.back();
	m_instructions.pop_back();
	auto b = createNode<CodeBlock>(_blockType, block);
	auto subProg = createNode<SubProgram>(take, ret, _isJmp, b, isPure);
	solAssert(!m_instructions.empty(), "");
	m_instructions.back().push_back(subProg);
}

void StackPusher::pushRefContAndCallX(int take, int ret, bool isPure) {
	callRefOrCallX(take, ret, false, CodeBlock::Type::PUSHREFCONT, isPure);
}

void StackPusher::pushContAndCallX(int take, int ret, bool isPure) {
	callRefOrCallX(take, ret, false, CodeBlock::Type::PUSHCONT, isPure);
}

void StackPusher::ifElse(bool withJmp) {
	solAssert(m_instructions.back().size() >= 3, "");
	auto falseBlock = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(falseBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto trueBlock = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(false, withJmp, trueBlock, falseBlock, 0);
	m_instructions.back().push_back(b);
}

void StackPusher::pushConditional(int ret) {
	solAssert(m_instructions.back().size() >= 3, "");
	auto falseBlock = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(falseBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto trueBlock = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(false, false, trueBlock, falseBlock, ret);
	m_instructions.back().push_back(b);
	fixStack(ret);
}

void StackPusher::if_or_ifNot(bool _withNot, bool _withJmp) {
	solAssert(!m_instructions.back().empty(), "");
	auto trueBlock = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(trueBlock, "");
	m_instructions.back().pop_back();
	auto b = createNode<TvmIfElse>(_withNot, _withJmp, trueBlock, nullptr, 0);
	m_instructions.back().push_back(b);
}

void StackPusher::_if() {
	if_or_ifNot(false, false);
}

void StackPusher::ifNot() {
	if_or_ifNot(true, false);
}

void StackPusher::ifJmp() {
	if_or_ifNot(false, true);
}

void StackPusher::ifNotJmp() {
	if_or_ifNot(true, true);
}

void StackPusher::repeatOrUntil(bool withBreakOrReturn, bool isRepeat) {
	solAssert(!m_instructions.back().empty(), "");

	auto loopBody = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
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

void StackPusher::repeat(bool withBreakOrReturn) {
	repeatOrUntil(withBreakOrReturn, true);
}

void StackPusher::until(bool withBreakOrReturn) {
	repeatOrUntil(withBreakOrReturn, false);
}

void StackPusher::_while(bool _withBreakOrReturn) {
	solAssert(m_instructions.back().size() >= 2, "");
	auto body = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(body != nullptr, "");
	m_instructions.back().pop_back();
	auto condition = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(condition != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<While>(false, _withBreakOrReturn, condition, body);
	m_instructions.back().push_back(b);
}

void StackPusher::tryOpcode(bool saveAltC2) {
	solAssert(m_instructions.back().size() >= 2, "");
	auto catchBody = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(catchBody != nullptr, "");
	m_instructions.back().pop_back();
	auto tryBody = dynamic_pointer_cast<CodeBlock>(m_instructions.back().back());
	solAssert(tryBody != nullptr, "");
	m_instructions.back().pop_back();
	auto b = createNode<TryCatch>(tryBody, catchBody, saveAltC2);
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

void StackPusher::_throw(const std::string& cmd) {
	auto opcode = makeTHROW(cmd);
	m_instructions.back().push_back(opcode);
	change(opcode->take(), 0);
}

TVMStack &StackPusher::getStack() {
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
		auto opcode = createNode<StackOpcode>("TUPLEVAR", qty + 1, 1);
		m_instructions.back().push_back(opcode);
		change(qty + 1, 1);
	}
}

void StackPusher::resetAllStateVars() {
	std::vector<VariableDeclaration const *> const stateVariables = ctx().c4StateVariables();
	std::vector<VariableDeclaration const *> const nostorageStateVariables = ctx().nostorageStateVars();
	if (m_ctx->tooMuchStateVariables()) {
		pushC7();
		*this << "FALSE";
		setIndexQ(TvmConst::C7::FirstIndexForVariables);
		unpackFirst(TvmConst::C7::FirstIndexForVariables);
		for (VariableDeclaration const *variable: stateVariables)
			pushDefaultValue(variable->type());
		for (VariableDeclaration const *variable: nostorageStateVariables)
			pushDefaultValue(variable->type());
		const int stateVarQty = stateVariables.size() + nostorageStateVariables.size();
		makeTuple(TvmConst::C7::FirstIndexForVariables + stateVarQty);
		popC7();
	} else {
		for (VariableDeclaration const *variable: stateVariables)
			pushDefaultValue(variable->type());
		for (VariableDeclaration const *variable: nostorageStateVariables)
			pushDefaultValue(variable->type());
		for (VariableDeclaration const *variable: nostorageStateVariables | boost::adaptors::reversed)
			setGlob(variable);
		for (VariableDeclaration const *variable: stateVariables | boost::adaptors::reversed)
			setGlob(variable);
	}
}

void StackPusher::getGlob(VariableDeclaration const *vd) {
	const int index = ctx().getStateVarIndex(vd);
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

void StackPusher::execute(int take, int ret) {
	auto opcode = createNode<StackOpcode>("EXECUTE", take, ret);
	change(take, ret);
	m_instructions.back().push_back(opcode);
}

void StackPusher::setGlob(int index) {
	Pointer<TvmAstNode> opcode = makeSetGlob(index);
	change(-1);
	m_instructions.back().push_back(opcode);
}

void StackPusher::setGlob(VariableDeclaration const *vd) {
	const int index = ctx().getStateVarIndex(vd);
	solAssert(index >= 0, "");
	setGlob(index);
}

void StackPusher::pushS(int i) {
	solAssert(i >= 0, "");
	m_instructions.back().push_back(makePUSH(i));
	change(+1);
}

void StackPusher::pushS2(int i, int j) {
	solAssert(i >= 0 && j >= 0, "");
	m_instructions.back().push_back(makePUSH2(i, j));
	change(+2);
}

void StackPusher::popS(int i) {
	solAssert(i >= 1, "");
	m_instructions.back().push_back(makePOP(i));
	change(-1);
}

void StackPusher::pushInt(const bigint& i) {
	*this << "PUSHINT " + toString(i);
}

bool StackPusher::fastLoad(const Type* type) {
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
				const int saveStackSize = stackSize();
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
				exchange(1);  // slice hasValue
				fixStack(-1); // fix stack

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
			for (auto t : tup->components()) {
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
			for (const ASTPointer<VariableDeclaration>& t : members) {
				load(t->type(), false);
			}
			blockSwap(members.size(), 1);
			makeTuple(members.size());
			exchange(1);
			return true;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			*this << "LDMSGADDR";
			return true;
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedPoint:
		case Type::Category::FixedBytes: {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			string cmd = ti.isSigned ? "LDI " : "LDU ";
			*this << cmd + toString(ti.numBits);
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
			auto varInt = to<VarIntegerType>(type);
			std::string cmd = "LDVAR";
			if (!varInt->asIntegerType().isSigned()) cmd += "U";
			cmd += "INT" + std::to_string(varInt->n());
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

void StackPusher::load(const Type *type, bool reverseOrder) {
	// slice
	bool directOrder = fastLoad(type);
	if (directOrder == reverseOrder) {
		exchange(1);
	}
	// reverseOrder? slice member : member slice
}

void StackPusher::preload(const Type *type) {
	const int stackSize = this->stackSize();
	// on stack there is slice
	switch (type->category()) {
		case Type::Category::Optional: {
			load(type, false);
			drop();
			break;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			*this << "LDMSGADDR";
			drop(1);
			break;
		case Type::Category::TvmCell:
			*this << "PLDREF";
			break;
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
			string cmd = ti.isSigned ? "PLDI " : "PLDU ";
			*this << cmd + toString(ti.numBits);
			break;
		}
		case Type::Category::Function: {
			*this << "PLDU 32";
			break;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArrayOrString()) {
				*this << "PLDREF";
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
			const auto[types, names] = getTupleTypes(to<TupleType>(type));
			StructCompiler sc{this, types, names};
			sc.convertSliceToTuple();
			break;
		}
		default:
			solUnimplemented("Decode isn't supported for " + type->toString(true));
	}
	ensureSize(stackSize);
}

void StackPusher::loadQ(const Type *type) {
	// slice ->
	//    cell slice' true
	//    slice false

	auto decodeRef = [&](){
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
			string cmd = (ti.isSigned ? "LDIQ " : "LDUQ ") + toString(ti.numBits);
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
					dropUnder(1, 1);
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

void StackPusher::store(
	const Type *type,
	bool reverse
) {
	// value   builder  -> reverse = false
	// builder value	-> reverse = true
	const int stackSize = this->stackSize();
	int deltaStack = 1;
	switch (type->category()) {
		case Type::Category::Optional: {
			auto optType = to<OptionalType>(type);
			auto optValueType = optType->valueType();
			auto array = to<ArrayType>(optType->valueType());
			if (optValueType->category() == Type::Category::TvmCell || (array && array->isByteArrayOrString())) {
				if (reverse)
					exchange(1); // value builder
				*this << "STDICT";
			} else {
				startOpaque();
				if (!reverse)
					exchange(1);	// builder value
				pushS(0);	// builder value value
				*this << "ISNULL";	// builder value isnull
				fixStack(-1); // fix stack
				ensureSize(stackSize);

				startContinuation();
				// builder value
				drop(1); // builder
				stzeroes(1);  // builder'
				endContinuation();
				fixStack(+1); // fix stack
				ensureSize(stackSize);

				startContinuation();
				// builder value
				if (isIn(optType->valueType()->category(), Type::Category::Optional, Type::Category::Mapping)) {
					untuple(1);
				}
				// builder value
				if (isSmallOptional(optType)) {
					exchange(1); // value builder
					stones(1); // value builder'
					store(optType->valueType(), false); // builder''
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
			if (!reverse)
				exchange(1);
			auto members = structType->structDefinition().members();
			untuple(members.size());
			this->reverse(members.size() + 1, 0);
			for (const auto& member : members)
				store(member->type(), false);
			break;
		}
		case Type::Category::Address:
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
			string cmd = ti.isSigned? "STI" : "STU";
			if (reverse) cmd += "R";
			cmd += " " + toString(ti.numBits);
			*this << cmd;
			solAssert(ti.numBits != 267, "");
			break;
		}
		case Type::Category::Function: {
			*this << "STU 32";
			break;
		}
		case Type::Category::Mapping:
			if (reverse) {
				exchange(1); // builder dict
			}
			// dict builder
			*this << "STDICT"; // builder
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArrayOrString()) {
				*this << "STREF"; // builder
			} else {
				if (!reverse) {
					exchange(1); // builder arr
				}
				*this << "UNTUPLE 2"; // builder size dict
				exchange(2);// dict size builder
				*this << "STU 32"; // dict builder'
				*this << "STDICT"; // builder''
			}
			break;
		}
		case Type::Category::TvmBuilder:
			*this << std::string("STB")  + (reverse ? "R " : "");
			break;
		case Type::Category::Tuple: {
			if (!reverse)
				exchange(1);	// builder value

			const auto[types, names] = getTupleTypes(to<TupleType>(type));
			StructCompiler sc{this, types, names};
			sc.tupleToBuilder();
			*this << "STBR";
			break;
		}
		case Type::Category::VarInteger: {
			if (!reverse)
				exchange(1);	// builder value

			auto varInt = to<VarIntegerType>(type);
			std::string cmd = "STVAR";
			if (!varInt->asIntegerType().isSigned()) cmd += "U";
			cmd += "INT" + std::to_string(varInt->n());
			*this << cmd;
			break;
		}
		default: {
			solUnimplemented("Encode isn't supported for " + type->toString(true));
		}
	}

	ensureSize(stackSize - deltaStack);
}

void StackPusher::pushZeroAddress() {
	pushSlice("x8000000000000000000000000000000000000000000000000000000000000000001_");
}


void StackPusher::convert(Type const *leftType, Type const *rightType) {
	TypeConversion{*this}.convert(leftType, rightType);
}

void StackPusher::checkFit(Type const *type) {
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
	case Type::Category::FixedPoint: {
		auto fp = to<FixedPointType>(type);
		if (fp->isSigned())
			*this << "FITS " + toString(fp->numBits());
		else
			*this << "UFITS " + toString(fp->numBits());
		break;
	}
	case Type::Category::VarInteger: {
		auto varInt = to<VarIntegerType>(type);
		checkFit(&varInt->asIntegerType());
		break;
	}
	case Type::Category::Enum: {
		auto enumType = to<EnumType>(type);
		int size = enumType->numberOfMembers();
		// TODO special case if size == 2**p
		pushS(0);
		pushInt(size); // x x size
		*this << "LESS"; // x x<size
		pushS(1); // x x<size x
		pushInt(-1); // x x<size x -1
		*this << "GREATER"; // x x<size x>-1
		*this << "AND"; // x (x<size && x>-1)
		this->_throw("THROWIFNOT 4");
		break;
	}
	default:
		solUnimplemented(type->humanReadableName());
		break;
	}
}

void StackPusher::pushParameter(std::vector<ASTPointer<VariableDeclaration>> const& params) {
	for (const ASTPointer<VariableDeclaration>& variable: params) {
		getStack().add(variable.get(), true);
	}
}

void StackPusher::pushFragmentInCallRef(int take, int ret, const std::string &functionName) {
	startContinuation();
	pushFragment(take, ret, functionName);
	pushRefContAndCallX(take, ret, false);
}

void StackPusher::pushCallOrCallRef(
	FunctionDefinition const* _functionDef,
	const std::optional<std::pair<int, int>>& deltaStack,
	bool isCalledByPoint
) {
	auto [take, ret] = deltaStack.has_value() ?
		deltaStack.value() :
		std::make_pair<int, int>(_functionDef->parameters().size(), _functionDef->returnParameters().size());

	std::string curFunctionName = ctx().currentFunctionName();
	std::string functionName = ctx().getFunctionInternalName(_functionDef, isCalledByPoint);
	if (_functionDef->name() == "onCodeUpgrade" ||
		m_ctx->callGraph().tryToAddEdge(curFunctionName, functionName) // Does it have a loop?
	) {
		pushPrivateFunctionId(*_functionDef, isCalledByPoint);
		pushC3();
		execute(take + 2, ret);
	} else {
		pushFragmentInCallRef(take, ret, functionName);
	}
}

void StackPusher::pushFragment(int take, int ret, const std::string& functionName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), functionName), "");
	change(take, ret);
	auto opcode = createNode<StackOpcode>(".inline " + functionName, take, ret);
	m_instructions.back().push_back(opcode);
}

void StackPusher::computeConstCell(std::string const& expName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), expName), "");
	pushCellOrSlice(createNode<PushCellOrSlice>(PushCellOrSlice::Type::PUSHREF_COMPUTE, expName, nullptr));
}

void StackPusher::computeConstSlice(std::string const& expName) {
	solAssert(!ctx().callGraph().tryToAddEdge(ctx().currentFunctionName(), expName), "");
	pushCellOrSlice(createNode<PushCellOrSlice>(PushCellOrSlice::Type::PUSHREFSLICE_COMPUTE, expName, nullptr));
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
	if (down == 0 || up == 0) {
		return;
	}
	push(createNode<Stack>(Stack::Opcode::BLKSWAP, down, up));
}

void StackPusher::reverse(int i, int j) {
	push(makeREVERSE(i, j));
}

void StackPusher::dropUnder(int droppedCount, int leftCount) {
	// drop dropCount elements that are situated under top leftCount elements
	solAssert(leftCount >= 0, "");
	solAssert(droppedCount >= 0, "");

	if (droppedCount == 0) {
		// do nothing
	} else if (leftCount == 0) {
		drop(droppedCount);
	} else if (droppedCount == 1 && leftCount == 1) {
		popS(1);
	} else {
		push(createNode<Stack>(Stack::Opcode::BLKDROP2, droppedCount, leftCount));
		change(-droppedCount);
	}
}

void StackPusher::exchange(int i) {
	Pointer<Stack> opcode = makeXCH_S(i);
	push(opcode);
}

void StackPusher::rot() {
	push(makeROT());
}

void StackPusher::rotRev() {
	push(makeROTREV());
}

Type const* StackPusher::parseIndexType(Type const *type) {
	if (to<ArrayType>(type)) {
		return TypeProvider::uint(32);
	}
	if (auto mappingType = to<MappingType>(type)) {
		return mappingType->keyType();
	}
	solUnimplemented("");
}

void StackPusher::assignStackVariable(Declaration const *name) {
	auto& stack = getStack();
	int idx = stack.getOffset(name);
	solAssert(idx >= 0, "");
	if (idx == 0) {
		// nothing
	} else {
		popS(idx);
	}
}

void StackPusher::prepareKeyForDictOperations(Type const *key, bool doIgnoreBytes) {
	// stack: key
	if (isStringOrStringLiteralOrBytes(key) || key->category() == Type::Category::TvmCell) {
		if (!doIgnoreBytes) {
			*this << "HASHCU";
		}
	} else if (key->category() == Type::Category::Struct) {
		StructCompiler sc{this, to<StructType>(key)};
		sc.tupleToBuilder();
		*this << "ENDC";
		*this << "CTOS";
	}
}

int StackPusher::int_msg_info(const std::set<int> &isParamOnStack, const std::map<int, std::string> &constParams,
									bool isDestBuilder) {
	// int_msg_info$0  ihr_disabled:Bool  bounce:Bool(#1)  bounced:Bool
	//				 src:MsgAddress  dest:MsgAddressInt(#4)
	//				 value:CurrencyCollection(#5,#6)  ihr_fee:Grams  fwd_fee:Grams
	//				 created_lt:uint64  created_at:uint32
	//				 = CommonMsgInfoRelaxed;

	// currencies$_ grams:Grams other:ExtraCurrencyCollection = CurrencyCollection;

	static const std::vector<int> zeroes {1, 1, 1,
									2, 2,
									4, 1, 4, 4,
									64, 32};
	std::string bitString = "0";
	int maxBitStringSize = 0;
	*this << "NEWC";
	for (int param = 0; param < static_cast<int>(zeroes.size()); ++param) {
		solAssert(constParams.count(param) == 0 || isParamOnStack.count(param) == 0, "");

		if (constParams.count(param) != 0) {
			bitString += constParams.at(param);
			maxBitStringSize += constParams.at(param).length();
		} else if (isParamOnStack.count(param) == 0) {
			bitString += std::string(zeroes.at(param), '0');
			maxBitStringSize += zeroes.at(param);
			solAssert(param != TvmConst::int_msg_info::dest, "");
		} else {
			appendToBuilder(bitString);
			bitString = "";
			switch (param) {
				case TvmConst::int_msg_info::bounce:
					*this << "STI 1";
					++maxBitStringSize;
					break;
				case TvmConst::int_msg_info::dest:
					if (isDestBuilder) {
						*this << "STB";
					} else {
						*this << "STSLICE";
					}
					maxBitStringSize += AddressInfo::maxBitLength();
					break;
				case TvmConst::int_msg_info::tons:
					exchange(1);
					*this << "STGRAMS";
					maxBitStringSize += VarUIntegerInfo::maxTonBitLength();
					break;
				case TvmConst::int_msg_info::currency:
					*this << "STDICT";
					++maxBitStringSize;
					break;
				default:
					solUnimplemented("");
			}
		}
	}
	appendToBuilder(bitString);
	bitString = "";
	return maxBitStringSize;
}

int StackPusher::ext_msg_info(const set<int> &isParamOnStack, bool isOut = true) {
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
	*this << "NEWC";
	for (int param = 0; param < static_cast<int>(zeroes.size()); ++param) {
		if (isParamOnStack.count(param) == 0) {
			bitString += std::string(zeroes.at(param), '0');
		} else {
			maxBitStringSize += bitString.size();
			appendToBuilder(bitString);
			bitString = "";
			if (param == TvmConst::ext_msg_info::dest) {
				*this << "STSLICE";
				maxBitStringSize += AddressInfo::maxBitLength();
			} else if (param == TvmConst::ext_msg_info::src) {
				*this << "STB";
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


void StackPusher::appendToBuilder(const std::string &bitString) {
	// stack: builder
	if (bitString.empty()) {
		return;
	}

	size_t count = std::count_if(bitString.begin(), bitString.end(), [](char c) { return c == '0'; });
	if (count == bitString.size()) {
		stzeroes(count);
	} else {
		const std::string hex = StrUtils::binaryStringToSlice(bitString);
		if (hex.length() * 4 <= 8 * 7 + 1) {
			*this << "STSLICECONST x" + hex;
		} else {
			pushSlice("x" + StrUtils::binaryStringToSlice(bitString));
			*this << "STSLICER";
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

void StackPusher::sendrawmsg() {
	*this << "SENDRAWMSG";
}

void StackPusher::sendIntMsg(const std::map<int, Expression const *> &exprs,
								   const std::map<int, std::string> &constParams,
								   const std::function<void(int)> &appendBody,
								   const std::function<void()> &pushSendrawmsgFlag,
								   bool isAwait,
								   size_t callParamsOnStack,
								   const std::function<void()> &appendStateInit) {
	std::set<int> isParamOnStack;
	size_t pushedValCnt = 0;
	for (auto &[param, expr] : exprs | boost::adaptors::reversed) {
		isParamOnStack.insert(param);
		TVMExpressionCompiler{*this}.compileNewExpr(expr);
		if (param != TvmConst::int_msg_info::dest)
			++pushedValCnt;
		else if (isAwait) {
			pushS(0);
			++pushedValCnt;
			blockSwap(pushedValCnt + callParamsOnStack, 1);
		}
	}
	sendMsg(isParamOnStack, constParams, appendBody, appendStateInit, pushSendrawmsgFlag);
}



void StackPusher::prepareMsg(
	const std::set<int>& isParamOnStack,
	const std::map<int, std::string> &constParams,
	const std::function<void(int)> &appendBody,
	const std::function<void()> &appendStateInit,
	MsgType messageType,
	bool isDestBuilder
) {
	int msgInfoSize = 0;
	switch (messageType) {
		case MsgType::Internal:
			msgInfoSize = int_msg_info(isParamOnStack, constParams, isDestBuilder);
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
	*this << "ENDC"; // stack: cell
}

void StackPusher::sendMsg(const std::set<int>& isParamOnStack,
								const std::map<int, std::string> &constParams,
								const std::function<void(int)> &appendBody,
								const std::function<void()> &appendStateInit,
								const std::function<void()> &pushSendrawmsgFlag,
								MsgType messageType,
								bool isDestBuilder) {
	prepareMsg(isParamOnStack, constParams, appendBody, appendStateInit, messageType, isDestBuilder);
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

bool TVMStack::isParam(Declaration const *name) const {
	return getStackSize(name) != -1;
}

void TVMStack::add(Declaration const *name, bool doAllocation) {
	solAssert(name != nullptr, "");
	if (doAllocation) {
		++m_size;
	}
	if (static_cast<int>(m_stackSize.size()) < m_size) {
		m_stackSize.resize(m_size);
	}
	m_stackSize.at(m_size - 1) = name;
}

int TVMStack::getOffset(Declaration const *name) const {
	solAssert(isParam(name), "");
	int stackSize = getStackSize(name);
	return getOffset(stackSize);
}

int TVMStack::getOffset(int stackSize) const {
	return m_size - 1 - stackSize;
}

int TVMStack::getStackSize(Declaration const *name) const {
	for (int i = m_size - 1; i >= 0; --i) {
		if (i < static_cast<int>(m_stackSize.size()) && m_stackSize.at(i) == name) {
			return i;
		}
	}
	return -1;
}

void TVMStack::ensureSize(int savedStackSize, const string &location, const ASTNode* node) const {
	if (node != nullptr && savedStackSize != m_size) {
		cast_error(*node, string{} + "Stack size error: expected: " + toString(savedStackSize)
								   + " but real: " + toString(m_size) + " at " + location);
	}
	solAssert(savedStackSize == m_size, "stack: exp:" + toString(savedStackSize)
				+ " real: " + toString(m_size) + " at " + location);
}

void TVMStack::takeLast(int n) {
	solAssert(m_size >= n, "");
	solAssert(int(m_stackSize.size()) >= m_size, "");
	m_stackSize.resize(m_size);
	m_stackSize = vector<Declaration const*>(m_stackSize.end() - n, m_stackSize.end());
	m_size = n;
	solAssert(int(m_stackSize.size()) == n, "");
}

InherHelper::InherHelper(const ContractDefinition *contract) {
	for (ContractDefinition const* c : contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const *_function : c->definedFunctions()) {
			const std::set<CallableDeclaration const*>& b = _function->annotation().baseFunctions;
			m_baseFunctions.insert(b.begin(), b.end());
		}
	}
}

bool InherHelper::isBaseFunction(CallableDeclaration const* d) const {
	return m_baseFunctions.count(d) != 0;
}

bool FunctionCallGraph::tryToAddEdge(std::string const& _v, std::string const& _to) {
	m_graph[_v].insert(_to);
	m_graph[_to]; // creates default value if there is no such key
	for (const auto& k : m_graph | boost::adaptors::map_keys)
		m_color[k] = Color::White;
	m_order = {};
	bool hasLoop{};
	for (const auto& k : m_graph | boost::adaptors::map_keys)
		if (dfs(k)) {
			hasLoop = true;
			m_graph[_v].erase(_to);
			break;
		}
	return hasLoop;
}

std::vector<std::string> FunctionCallGraph::DAG() {
	for (const auto& k : m_graph | boost::adaptors::map_keys)
		m_color[k] = Color::White;
	m_order = {};
	for (const auto& k : m_graph | boost::adaptors::map_keys)
		dfs(k);
	return m_order;
}

bool FunctionCallGraph::dfs(std::string const& v) {
	if (m_color.at(v) == Color::Black)
		return false;
	if (m_color.at(v) == Color::Red)
		return true;

	// It's white
	m_color.at(v) = Color::Red;
	for (std::string const& _to : m_graph.at(v))
		if (dfs(_to))
			return true;
	m_order.emplace_back(v);

	m_color.at(v) = Color::Black;
	return false;
}

void TVMCompilerContext::initMembers(ContractDefinition const *contract) {
	solAssert(!m_contract, "");
	m_contract = contract;

	ignoreIntOverflow = m_pragmaHelper.hasIgnoreIntOverflow();
	auto const c4StateVars = c4StateVariables();
	for (VariableDeclaration const *variable : c4StateVars) {
		int index = TvmConst::C7::FirstIndexForVariables + m_stateVarIndex.size();
		m_stateVarIndex[variable] = index;
	}
	for (VariableDeclaration const *variable : nostorageStateVars()) {
		int index = TvmConst::C7::FirstIndexForVariables + m_stateVarIndex.size();
		m_stateVarIndex[variable] = index;
	}
}

TVMCompilerContext::TVMCompilerContext(ContractDefinition const *contract, PragmaDirectiveHelper const &pragmaHelper) :
	m_pragmaHelper{pragmaHelper},
	m_usage{*contract},
	m_inherHelper{contract}
{
	m_isUncheckedBlock.push(false);
	initMembers(contract);
}

int TVMCompilerContext::getStateVarIndex(VariableDeclaration const *variable) const {
	return m_stateVarIndex.at(variable);
}

std::vector<VariableDeclaration const *> TVMCompilerContext::c4StateVariables() const {
	return ::stateVariables(getContract(), false);
}

std::vector<VariableDeclaration const *> TVMCompilerContext::nostorageStateVars() const {
	return ::stateVariables(getContract(), true);
}

bool TVMCompilerContext::tooMuchStateVariables() const {
	return c4StateVariables().size() + nostorageStateVars().size() >= TvmConst::C7::FirstIndexForVariables + 6;
}

std::vector<Type const *> TVMCompilerContext::c4StateVariableTypes() const {
	std::vector<Type const *> types;
	for (VariableDeclaration const * var : c4StateVariables()) {
		types.emplace_back(var->type());
	}
	return types;
}

PragmaDirectiveHelper const &TVMCompilerContext::pragmaHelper() const {
	return m_pragmaHelper;
}

bool TVMCompilerContext::isStdlib() const {
	return m_contract->name() == "stdlib";
}

string TVMCompilerContext::getFunctionInternalName(FunctionDefinition const* _function, bool calledByPoint) const {
	if (isStdlib())
		return _function->name();

	std::string functionName;
	const std::string hexName = _function->externalIdentifierHex();
	ContractDefinition const* contract = _function->annotation().contract;
	if (contract && contract->isLibrary())
		functionName = getLibFunctionName(_function, calledByPoint);
	else if (calledByPoint && isBaseFunction(_function))
		functionName = _function->annotation().contract->name() + "_" + _function->name() + "_" + hexName;
	else if (_function->isFree())
		functionName = _function->name() + "_" + hexName + "_free_internal";
	else
		functionName = _function->name() + "_" + hexName + "_internal";
	return functionName;
}

string TVMCompilerContext::getLibFunctionName(FunctionDefinition const* _function, bool withObject) {
	std::string name = _function->annotation().contract->name() +
			(withObject ? "_with_obj_" : "_no_obj_") +
			_function->name() + "_" + _function->externalIdentifierHex();
	return name;
}

string TVMCompilerContext::getFunctionExternalName(FunctionDefinition const *_function) {
	const string& fname = _function->name();
	solAssert(_function->functionIsExternallyVisible(), "Internal error: expected public function: " + fname);
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
	solAssert(!m_isUncheckedBlock.empty(), "");
	return ignoreIntOverflow || m_isUncheckedBlock.top();
}

FunctionDefinition const *TVMCompilerContext::afterSignatureCheck() const {
	FunctionDefinition const *res = {};
	for (ContractDefinition const* c : m_contract->annotation().linearizedBaseContracts) {
		for (FunctionDefinition const *f: c->definedFunctions()) {
			if (f->name() == "afterSignatureCheck") {
				solAssert(res == nullptr, "");
				res = f;
			}
		}
	}
	return res;
}

bool TVMCompilerContext::storeTimestampInC4() const {
	return m_pragmaHelper.hasTime() && afterSignatureCheck() == nullptr;
}

int TVMCompilerContext::getOffsetC4() const {
	return
		256 + // pubkey
		(storeTimestampInC4() ? 64 : 0) +
		1 + // constructor flag
		(m_usage.hasAwaitCall() ? 1 : 0);
}

std::vector<std::pair<VariableDeclaration const*, int>> TVMCompilerContext::getStaticVariables() const {
	int shift = 0;
	std::vector<std::pair<VariableDeclaration const*, int>> res;
	for (VariableDeclaration const* v : c4StateVariables()) {
		if (v->isStatic()) {
			res.emplace_back(v, TvmConst::C4::PersistenceMembersStartIndex + shift++);
		}
	}
	return res;
}

void TVMCompilerContext::addInlineFunction(const std::string& name, Pointer<CodeBlock> body) {
	solAssert(m_inlinedFunctions.count(name) == 0, "");
	m_inlinedFunctions[name] = std::move(body);
}

Pointer<CodeBlock> TVMCompilerContext::getInlinedFunction(const std::string& name) {
	return m_inlinedFunctions.at(name);
}

void TVMCompilerContext::addPublicFunction(uint32_t functionId, const std::string& functionName) {
	m_publicFunctions.emplace_back(functionId, functionName);
}

const std::vector<std::pair<uint32_t, std::string>>& TVMCompilerContext::getPublicFunctions() {
	std::sort(m_publicFunctions.begin(), m_publicFunctions.end());
	return m_publicFunctions;
}

bool TVMCompilerContext::isBaseFunction(CallableDeclaration const* d) const {
	return m_inherHelper.isBaseFunction(d);
}

void StackPusher::pushEmptyArray() {
	pushInt(0);
	*this << "NEWDICT";
	*this << "TUPLE 2";
}

void StackPusher::pushNull() {
	*this << "NULL";
}

void StackPusher::pushEmptyCell() {
	pushCellOrSlice(createNode<PushCellOrSlice>(PushCellOrSlice::Type::PUSHREF, "", nullptr));
}

void StackPusher::pushDefaultValue(Type const* _type) {
	startOpaque();
	Type::Category cat = _type->category();
	switch (cat) {
		case Type::Category::Address:
		case Type::Category::Contract:
			pushSlice("x2_"); // addr_none$00 = MsgAddressExt;
			break;
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::VarInteger:
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
			*this << "NEWDICT";
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
		case Type::Category::UserDefinedValueType: {
			auto userDefValue = to<UserDefinedValueType>(_type);
			pushDefaultValue(&userDefValue->underlyingType());
			break;
		}
		case Type::Category::Tuple: {
			auto tuple = to<TupleType>(_type);
			for (Type const* comp : tuple->components()) {
				pushDefaultValue(comp);
			}
			break;
		}
		default:
			solUnimplemented("");
	}
	endOpaque(0, 1, true);
}

void StackPusher::getDict(
	Type const& keyType,
	Type const& valueType,
	const GetDictOperation op
) {
	GetFromDict d(*this, keyType, valueType, op, std::nullopt);
	d.getDict();
}

void StackPusher::getAndSetDict(
	const Type &keyType,
	const Type &valueType,
	const GetDictOperation op,
	const DataType inputValueType
) {
	GetFromDict d(*this, keyType, valueType, op, inputValueType);
	d.getDict();
}

void StackPusher::byteLengthOfCell() {
	pushInt(0xFFFFFFFF);
	*this << "CDATASIZE";
	drop(1);
	dropUnder(1, 1);
	*this << "RSHIFT 3";
}

void StackPusher::was_c4_to_c7_called() {
	getGlob(TvmConst::C7::TvmPubkey);
	*this << "ISNULL";
}

void StackPusher::checkCtorCalled() {
	getGlob(TvmConst::C7::ConstructorFlag);
	_throw("THROWIFNOT " + toString(TvmConst::RuntimeException::CallThatWasBeforeCtorCall));
}

void StackPusher::checkIfCtorCalled(bool ifFlag) {
	startContinuation();
	checkCtorCalled();
	endContinuationFromRef();
	if (ifFlag) {
		ifJmp();
	} else {
		ifNotJmp();
	}
}

void StackPusher::add(StackPusher const& pusher) {
	solAssert(pusher.m_instructions.size() == 1, "");
	for (const Pointer<TvmAstNode>& op : pusher.m_instructions.back()) {
		m_instructions.back().emplace_back(op);
	}
}

void StackPusher::clear() {
	m_instructions.clear();
	m_instructions.emplace_back();
}

void StackPusher::takeLast(int n) {
	m_stack.takeLast(n);
}

void TypeConversion::convert(Type const* leftType, Type const* rightType) {
	// opt(opt(opt(opt(T)))) = T;
	// opt(opt(opt(opt(T0, T1, T2)))) = (T0, T1, T2);
	int lQty = optTypeQty(leftType);
	int rQty = optTypeQty(rightType);
	solAssert(lQty >= rQty, "");
	if (lQty > rQty) {
		auto l = to<OptionalType>(leftType);
		convert(l->valueType(), rightType);
		// optional(uint, uint) q = (1, 2);
		if (l->valueType()->category() == Type::Category::Tuple){
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
		break ;
	case Type::Category::StringLiteral:
		fromStringLiteral(leftType, to<StringLiteralType>(rightType));
		break;
	case Type::Category::Address:
	case Type::Category::Bool:
	case Type::Category::Contract:
	case Type::Category::Enum:
	case Type::Category::Function:
	case Type::Category::Mapping:
	case Type::Category::TvmVector:
	case Type::Category::Struct:
	case Type::Category::TvmBuilder:
	case Type::Category::TvmCell:
	case Type::Category::Null:
	case Type::Category::EmpyMap:
	case Type::Category::UserDefinedValueType:
		break;
	default:
		solUnimplemented(rightType->toString());
		break;
	}
}

void TypeConversion::integerToInteger(IntegerType const* leftType, IntegerType const* rightType) {
	if (rightType->isImplicitlyConvertibleTo(*leftType) || leftType->numBits() == 257)
		return ;

	bool canConvert = leftType->numBits() > rightType->numBits() && leftType->isSigned() && !rightType->isSigned();
	if (canConvert)
		return ;

	bigint x = (bigint(1) << leftType->numBits()) - 1;
	m_pusher << "PUSHINT " + toString(x)
		  << "AND";

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

void TypeConversion::fixedPointToInteger(IntegerType const* leftType, FixedPointType const* rightType) {
	int powerDiff = rightType->fractionalDigits();
	if (powerDiff > 0) {
		m_pusher.pushInt(MathConsts::power10().at(powerDiff));
		m_pusher << "DIV";
	}
	integerToInteger(leftType, rightType->asIntegerType());
}

void TypeConversion::fixedPointToFixedPoint(FixedPointType const* leftType, FixedPointType const* rightType) {
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

void TypeConversion::integerToFixedPoint(FixedPointType const* leftType, IntegerType const* rightType) {
	int powerDiff = leftType->fractionalDigits();
	if (powerDiff > 0) {
		m_pusher.pushInt(MathConsts::power10().at(powerDiff));
		m_pusher << "MUL"; // TODO use MULDIVMOD to avoid overflow
	}
	integerToInteger(leftType->asIntegerType(), rightType);
}

void TypeConversion::fixedBytesToFixedBytes(FixedBytesType const* leftType, FixedBytesType const* rightType) {
	int diff = 8 * (leftType->numBytes() - rightType->numBytes());
	if (diff > 0) {
		m_pusher << "LSHIFT " + std::to_string(diff);
	} else if (diff < 0) {
		m_pusher << "RSHIFT " + std::to_string(-diff);
	}
}

void TypeConversion::bytesToFixedBytes(FixedBytesType const* rightType) {
	size_t bits = rightType->numBytes() * 8;
	m_pusher.startContinuation();
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
	m_pusher.pushRefContAndCallX(1, 1, false);
}

void TypeConversion::stringLiteralToFixedBytes(FixedBytesType const* leftType, StringLiteralType const* rightType) {
	size_t bytes = 0;
	u256 value = 0;
	for (char c : rightType->value()) {
		value = value * 256 + c;
		++bytes;
	}
	while (bytes < leftType->numBytes()) {
		value *= 256;
		++bytes;
	}
	m_pusher.drop(1); // delete old value
	m_pusher << "PUSHINT " + toString(value);
}

void TypeConversion::fromFixedPoint(Type const* leftType, FixedPointType const* rightType) {
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

void TypeConversion::convertIntegerToAddress(Type const* t) {
	if (auto r = to<RationalNumberType>(t)) {
		m_pusher.drop();
		m_pusher.pushSlice("x" + StrUtils::binaryStringToSlice(StrUtils::literalToSliceAddress(r->value2())));
	} else {
		m_pusher << "NEWC";
		m_pusher << "STSLICECONST x801_"; // addr_std$10 anycast:(Maybe Anycast) workchain_id:int8 // 10 0  00000000 1 = 801
		m_pusher << "STU 256"; // address:bits256
		m_pusher << "ENDC";
		m_pusher << "CTOS";
	}
}

void TypeConversion::convertIntegerToEnum(EnumType const* leftType, IntegerType const* /*rightType*/) {
	int const size = leftType->enumDefinition().members().size();
	m_pusher.pushInt(size);
	m_pusher << "MOD";
}

void TypeConversion::fromInteger(Type const* leftType, IntegerType const* rightType) {
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
	case Type::Category::Function: {
		m_pusher.ctx().setPragmaSaveAllFunctions();
		break;
	}
	case Type::Category::FixedBytes:
		// do nothing here
		break;
	case Type::Category::Address:
	case Type::Category::Contract:
		convertIntegerToAddress(rightType);
		break;
	case Type::Category::Enum:
		convertIntegerToEnum(to<EnumType>(leftType), rightType);
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}

void TypeConversion::fromRational(Type const* leftType, RationalNumberType const* rightType) {
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

void TypeConversion::fromFixedBytesType(Type const* leftType, FixedBytesType const* rightType) {
	switch (leftType->category()) {
	case Type::Category::Address: {
		convertIntegerToAddress(rightType);
		break;
	}
	case Type::Category::FixedBytes: {
		fixedBytesToFixedBytes(to<FixedBytesType>(leftType), rightType);
		break;
	}
	case Type::Category::Integer: {
		auto intType = to<IntegerType>(leftType);
		if (intType && !intType->isSigned() &&
			(intType->numBits() >= 8 * rightType->numBytes()))
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

void TypeConversion::fromArray(Type const* leftType, ArrayType const* rightType) {
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

void TypeConversion::fromSlice(Type const* leftType) {
	switch (leftType->category()) {
	case Type::Category::TvmSlice:
		break;
	case Type::Category::Array: {
		auto arrType = to<ArrayType>(leftType);
		solAssert(arrType->isByteArrayOrString(), "");
		m_pusher << "NEWC"    // s b
			  << "STSLICE" // b'
			  << "ENDC";   // cell
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

void TypeConversion::fromStringLiteral(Type const* leftType, StringLiteralType const* rightType) {
	switch (leftType->category()) {
	case Type::Category::FixedBytes:
		stringLiteralToFixedBytes(to<FixedBytesType>(leftType), rightType);
		break;
	case Type::Category::Array:
		break;
	default:
		solUnimplemented(leftType->toString());
		break;
	}
}
