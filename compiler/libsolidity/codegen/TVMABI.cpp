/*
 * Copyright (C) 2019-2026 EverX. All Rights Reserved.
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

#include <boost/algorithm/string.hpp>

#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/ast/TypeProvider.h>
#include <libsolutil/JSON.h>
#include <libsolutil/picosha2.h>

#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>
#include <libsolidity/codegen/TVMContractCompiler.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>

using namespace solidity::frontend;
using namespace solidity::util;
using namespace solidity::langutil;
using namespace solidity;

namespace {
static auto const SPACE = std::string(TVMABI::INDENT_SPACES, ' ');
}

Json TVMABI::generateFunctionIdsJson(ContractDefinition const& contract, PragmaDirectiveHelper const& pragmaHelper) {
	TVMCompilerContext ctx{&contract, pragmaHelper};
	StackPusher pusher{&ctx};
	std::vector<FunctionDefinition const*> publicFunctions = TVMABI::publicFunctions(contract);
	std::map<std::string, uint32_t> func2id;
	for (FunctionDefinition const* func: publicFunctions) {
		uint32_t functionID =
			ChainDataEncoder::calculateFunctionIDWithReason(func, ReasonOfOutboundMessage::RemoteCallInternal);
		std::string const name = TVMCompilerContext::getFunctionExternalName(func);
		func2id[name] = functionID;
	}
	if (!func2id.contains("constructor") && ctx.storageLayout().hasConstructor())
		func2id["constructor"] = ChainDataEncoder::calculateConstructorFunctionID();

	Json root;
	for (auto const& [func, functionID]: func2id) {
		root[func] = "0x" + StrUtils::intToHex(functionID);
	}
	return root;
}

Json TVMABI::generatePrivateFunctionIdsJson(
	ContractDefinition const& contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper,
	bool debugMode
) {
	if (!contract.canBeDeployed()) {
		fatal_error(contract.name() + " is not deployable. Can't print private function IDs.");
	}
	Json ids = Json::array();
	Pointer<Contract> codeContract =
		TVMContractCompiler::generateContractCode(&contract, _sourceUnits, pragmaHelper, debugMode);
	for (Pointer<Function> const& fun: codeContract->functions()) {
		FunctionDefinition const* def = fun->functionDefinition();
		if (fun->functionDefinition() != nullptr && fun->functionId()) {
			Json func;
			func["scope"] = def->isFree() ? "" : def->annotation().contract->name();
			func["sign"] = def->externalSignature();
			func["id"] = fun->functionId().value();
			ids.push_back(func);
		}
	}
	return ids;
}

Json TVMABI::generateABIJson(
	ContractDefinition const* contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const*> const& pragmaDirectives
) {
	PragmaDirectiveHelper pdh{pragmaDirectives};
	TVMCompilerContext ctx{contract, pdh};

	Json root;
	root["ABI version"] = 2;
	root["version"] = "2.7";

	// header
	{
		Json header = Json::array();

		// NOTE: order is important
		if (auto msgHeaders = contract->externalMsgHeaders()) {
			if (msgHeaders->hasPubkey())
				header.push_back("pubkey");
			if (msgHeaders->hasTime())
				header.push_back("time");
			if (msgHeaders->hasExpire())
				header.push_back("expire");
		}

		root["header"] = header;
	}

	// functions
	{
		std::set<std::string> used;
		Json functions = Json::array();
		std::vector<FunctionDefinition const*> const publicFunctions = TVMABI::publicFunctions(*contract);
		for (FunctionDefinition const* f: publicFunctions) {
			auto funcName = TVMCompilerContext::getFunctionExternalName(f);
			if (used.contains(funcName))
				continue;
			used.insert(funcName);
			functions.push_back(
				toJson(funcName, convertArray(f->parameters()), convertArray(f->returnParameters()), f)
			);
		}

		if (!used.contains("constructor") && ctx.storageLayout().hasConstructor()) {
			functions.push_back(toJson("constructor", {}, {}, nullptr));
		}

		root["functions"] = functions;
	}

	// events
	{
		std::vector<EventDefinition const*> events{};
		for (auto const& _event: contract->definedInterfaceEvents())
			events.push_back(_event);
		for (std::shared_ptr<SourceUnit> const& source: _sourceUnits)
			for (ASTPointer<ASTNode> const& node: source->nodes()) {
				if (auto eventDefinition = dynamic_cast<EventDefinition const*>(node.get()))
					events.push_back(eventDefinition);
				if (auto lib = dynamic_cast<ContractDefinition const*>(node.get()))
					if (lib->isLibrary())
						for (auto const& event: lib->definedInterfaceEvents())
							events.push_back(event);
			}

		Json eventAbi = Json::array();
		std::set<std::string> usedEvents;
		for (auto const& e: events) {
			std::string name = eventName(e);
			{
				std::string const fullName = eventName(e) + " " + e->functionType(true)->externalSignature();
				solAssert(!usedEvents.contains(fullName), "Event is duplicated: " + fullName);
				usedEvents.insert(fullName);
			}
			Json cur;
			cur["name"] = name;
			cur["inputs"] = encodeParams(convertArray(e->parameters()));
			eventAbi.push_back(cur);
		}
		root["events"] = eventAbi;
	}

	// fields
	{
		Json fields = Json::array();
		std::vector<std::pair<std::string, std::string>> offset;

		if (ctx.storageLayout().storePubkeyInC4())
			offset.emplace_back("_pubkey", "fixedbytes32");

		if (ctx.storageLayout().storeTimestampInC4())
			offset.emplace_back("_timestamp", "uint64");

		if (ctx.storageLayout().hasConstructor())
			offset.emplace_back("_constructorFlag", "bool");

		for (auto const& [name, type]: offset) {
			Json field;
			field["name"] = name;
			field["type"] = type;
			field["init"] = name == "_pubkey";
			fields.push_back(field);
		}

		std::vector<VariableDeclaration const*> stateVars = ctx.storageLayout().usualAndUnpackedStateVariables();
		std::set<std::string> usedNames;
		std::vector<std::tuple<std::string, Type const*, bool>> namesTypes;
		for (VariableDeclaration const* var: stateVars | std::views::reverse) {
			std::string name = var->name();
			if (usedNames.contains(name))
				name = var->annotation().contract->name() + "$" + var->name();
			solAssert(!usedNames.contains(name), "");
			usedNames.insert(name);
			namesTypes.emplace_back(name, var->type(), var->isStatic());
		}
		std::ranges::reverse(namesTypes);

		for (auto const& [name, type, isStatic]: namesTypes) {
			Json cur = setupNameTypeComponents(name, type);
			cur["init"] = isStatic;
			fields.push_back(cur);
		}
		root["fields"] = fields;
	}

	// getters
	{
		Json functions = Json::array();
		std::set<std::string> used;
		auto getters = TVMABI::getters(*contract);
		for (FunctionDefinition const* f: getters) {
			auto funcName = TVMCompilerContext::getFunctionExternalName(f);
			solAssert(!used.contains(funcName), "");
			used.insert(funcName);
			functions.push_back(
				toJson(funcName, convertArray(f->parameters()), convertArray(f->returnParameters()), f)
			);
		}
		root["getters"] = functions;
	}

	return root;
}

void TVMABI::generateABI(
	ContractDefinition const* contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const*> const& pragmaDirectives,
	std::ostream& out
) {
	Json root = generateABIJson(contract, _sourceUnits, pragmaDirectives);

	out << "{\n";
	out << SPACE << R"("ABI version": )" << root["ABI version"] << ",\n";
	if (root.contains("version")) {
		out << SPACE << R"("version": )" << root["version"] << ",\n";
	}

	if (root.contains("version")) {
		out << SPACE << R"("header": [)";
		for (unsigned i = 0; i < root["header"].size(); ++i) {
			out << root["header"][i];
			if (i + 1 != root["header"].size()) {
				out << ", ";
			}
		}
		out << "],\n";
	}

	for (auto const& member: {"functions", "getters", "events"}) {
		out << SPACE << "\"" << member << "\": [";
		if (!root[member].empty()) {
			out << "\n";
			print(root[member], out);
			out << SPACE;
		}
		out << "],\n";
	}

	out << SPACE << R"("fields": [)";
	if (!root["fields"].empty()) {
		out << "\n";
		printVariables(root["fields"], out, SPACE + SPACE);
		out << SPACE;
	}
	out << "]\n";

	out << "}\n";
}

std::vector<FunctionDefinition const*> TVMABI::publicFunctions(ContractDefinition const& contract) {
	std::vector<FunctionDefinition const*> publicFunctions;
	if (auto main_constr = contract.constructor(); main_constr != nullptr)
		publicFunctions.push_back(contract.constructor());

	for (auto c: contract.annotation().linearizedBaseContracts) {
		for (auto const& _function: c->definedFunctions()) {
			if (!_function->isConstructor() &&
				_function->isPublic() &&
				!_function->isReceive() &&
				!_function->isFallback() &&
				!_function->isOnBounce() &&
				!_function->isOnTickTock() &&
				_function->visibility() != Visibility::Getter)
				publicFunctions.push_back(_function);
		}
	}
	return publicFunctions;
}

std::vector<FunctionDefinition const*> TVMABI::getters(ContractDefinition const& contract) {
	std::vector<FunctionDefinition const*> getters;
	for (auto c: contract.annotation().linearizedBaseContracts) {
		for (auto const& _function: c->definedFunctions()) {
			if (!_function->isConstructor() && _function->visibility() == Visibility::Getter)
				getters.push_back(_function);
		}
	}
	return getters;
}

void TVMABI::printVariable(
	Json const& json,
	std::ostream& out,
	std::string const& indentation,
	bool hasIndentInFirstLine = true
) {
	std::vector<std::string> members;
	if (json.contains("init"))
		members.emplace_back("init");
	members.emplace_back("name");
	members.emplace_back("type");
	bool hasComponents = json.contains("components");
	if (hasComponents)
		members.emplace_back("components");

	if (hasIndentInFirstLine)
		out << indentation;
	out << "{";
	for (size_t i = 0; i < members.size(); ++i) {
		auto const& member = members[i];
		if (hasComponents)
			out << "\n" << SPACE << indentation;
		else
			out << " ";
		out << "\"" << member << "\": ";

		if (member == "components") {
			out << "[\n";
			printVariables(json[member], out, SPACE + SPACE + indentation);
			out << SPACE << indentation << "]\n";
		} else {
			out << json[member];
		}

		if (i + 1 != members.size())
			out << ",";
		else if (!hasComponents)
			out << " ";
	}
	if (hasComponents)
		out << indentation;
	out << "}";
}

void TVMABI::printVariables(Json const& json, std::ostream& out, std::string const& indentation) {
	for (unsigned f = 0; f < json.size(); ++f) {
		auto const& element = json[f];
		printVariable(element, out, indentation);
		if (f + 1 != json.size())
			out << ",";
		out << "\n";
	}
}

void TVMABI::print(Json const& json, std::ostream& out) {
	for (unsigned f = 0; f < json.size(); ++f) {
		auto const& function = json[f];
		out << SPACE << SPACE << "{\n";

		for (auto const& member: {"name", "id", "input_id", "output_id", "externalMsg"}) {
			if (function.contains(member))
				out << SPACE << SPACE << SPACE << "\"" << member << "\": " << function[member] << ",\n";
		}

		for (auto const& member: {"inputs", "outputs"}) {
			if (!function.contains(member))
				continue;

			out << SPACE << SPACE << SPACE << "\"" << member << "\": [";
			int size = function[member].size();
			if (size >= 2) {
				out << "\n";
				printVariables(function[member], out, SPACE + SPACE + SPACE + SPACE);
				out << SPACE << SPACE << SPACE;
			} else if (size == 1) {
				printVariable(function[member][0], out, SPACE + SPACE + SPACE, false);
			}
			out << "]";
			if (member == std::string("inputs") && function.contains("outputs"))
				out << ",";
			out << "\n";
		}

		out << SPACE << SPACE << "}";
		if (f + 1 != json.size())
			out << ",";
		out << "\n";
	}
}

Json TVMABI::toJson(
	std::string const& functionName,
	std::vector<VariableDeclaration const*> const& params,
	std::vector<VariableDeclaration const*> const& retParams,
	FunctionDefinition const* funcDef
) {
	Json function;

	function["name"] = functionName;

	Json inputs = encodeParams(params);
	if (funcDef != nullptr && funcDef->isResponsible()) {
		Json json;
		json["name"] = "answerId";
		json["type"] = "uint32";
		inputs.insert(inputs.begin(), json);
	}
	function["inputs"] = inputs;

	Json outputs = encodeParams(retParams);
	function["outputs"] = outputs;

	if (funcDef != nullptr) {
		uint32_t input_id;
		uint32_t output_id;

		if (funcDef->functionID().has_value()) {
			uint32_t id = funcDef->functionID().value();
			function["id"] = "0x" + StrUtils::intToHex(id);

			input_id = id;
			output_id = id;
		} else {
			input_id = ChainDataEncoder::
				calculateFunctionIDWithReason(funcDef, ReasonOfOutboundMessage::RemoteCallInternal, false);
			output_id = ChainDataEncoder::
				calculateFunctionIDWithReason(funcDef, ReasonOfOutboundMessage::FunctionReturnExternal, false);
		}

		function["input_id"] = "0x" + StrUtils::intToHex(input_id);
		if (funcDef->isExternalMsg()) {
			function["output_id"] = "0x" + StrUtils::intToHex(output_id);
		}

		function["externalMsg"] = funcDef->isExternalMsg();
	}

	return function;
}

Json TVMABI::encodeParams(std::vector<VariableDeclaration const*> const& params) {
	Json result = Json::array();
	size_t idx = 0;
	for (auto const& variable: params) {
		std::string name = variable->name();
		if (name.empty())
			name = "value" + toString(idx);
		Json json = setupNameTypeComponents(name, getType(variable));
		result.push_back(json);
		idx++;
	}
	return result;
}

Json TVMABI::setupNameTypeComponents(std::string const& name, Type const* type) {
	Json json;
	json["name"] = name;
	std::string typeName;
	std::optional<Json> components;

	TypeChecker typeChecker{*GlobalParams::g_tvmVersion, std::nullopt, *GlobalParams::g_errorReporter};
	SourceLocation tmpLoc;
	std::set<StructDefinition const*> tmpSet;
	if (to<StructType>(type) == nullptr && typeChecker.isBadAbiType(tmpLoc, type, tmpLoc, tmpSet, false)) {
		switch (type->category()) {
		case Type::Category::Mapping:
			typeName = "optional(cell)";
			break;
		case Type::Category::Function:
			typeName = "uint32";
			break;
		case Type::Category::FixedPoint: {
			TypeInfo ti{type};
			typeName = (ti.isSigned ? "int" : "uint") + toString(ti.numBits);
			break;
		}
		case Type::Category::Array: {
			typeName = "tuple";
			Json comp = Json::array();
			{
				Json obj;
				obj["name"] = name + "_length";
				obj["type"] = "uint32";
				comp.push_back(obj);
			}
			{
				Json obj;
				obj["name"] = name + "_dict";
				obj["type"] = "optional(cell)";
				comp.push_back(obj);
			}
			components = comp;
			break;
		}
		default:
			solUnimplemented("TODO: support for " + type->toString());
		}
	} else {
		Type::Category const category = type->category();
		TypeInfo ti(type);
		if (category == Type::Category::Address || category == Type::Category::Contract)
			typeName = "address";
		else if (category == Type::Category::AddressStd)
			typeName = "address_std";
		else if (category == Type::Category::VarInteger) {
			auto varint = to<VarIntegerType>(type);
			typeName = varint->toString(false);
		} else if (auto* fixedBytesType = to<FixedBytesType>(type))
			typeName = "fixedbytes" + toString(fixedBytesType->numBytes());
		else if (ti.isNumeric) {
			if (to<BoolType>(type)) {
				typeName = "bool";
			} else if (ti.isSigned) {
				typeName = "int" + toString(ti.numBits);
			} else {
				typeName = "uint" + toString(ti.numBits);
			}
		} else if (auto arrayType = to<ArrayType>(type)) {
			Type const* arrayBaseType = arrayType->baseType();
			if (arrayType->isByteArrayOrString()) {
				if (arrayType->isString()) {
					typeName = "string";
				} else {
					typeName = "bytes";
				}
			} else {
				Json obj = setupNameTypeComponents("arrayBaseType", arrayBaseType);
				typeName = obj["type"].get<std::string>() + "[]";
				if (obj.contains("components")) {
					components = obj["components"];
				}
			}
		} else if (auto st = to<StructType>(type)) {
			typeName = "tuple";
			components = setupStructComponents(st);
		} else if (category == Type::Category::TvmCell) {
			typeName = "cell";
		} else if (category == Type::Category::Mapping) {
			auto mapping = to<MappingType>(type);
			std::string key;
			std::string value;
			{
				Json obj = setupNameTypeComponents("keyType", mapping->keyType());
				key = obj["type"].get<std::string>();
				solAssert(!obj.contains("components"), "");
			}
			{
				Json obj = setupNameTypeComponents("valueType", mapping->valueType());
				value = obj["type"].get<std::string>();
				if (obj.contains("components")) {
					components = obj["components"];
				}
			}
			typeName = "map(" + key + "," + value + ")";
		} else if (auto opt = to<OptionalType>(type)) {
			if (auto tt = to<TupleType>(opt->valueType())) {
				typeName = "optional(tuple)";
				components = setupTupleComponents(tt);
			} else {
				Json obj = setupNameTypeComponents("valueType", opt->valueType());
				typeName = "optional(" + obj["type"].get<std::string>() + ")";
				if (obj.contains("components")) {
					components = obj["components"];
				}
			}
		} else if (auto userDefType = to<UserDefinedValueType>(type)) {
			Json obj = setupNameTypeComponents("", &userDefType->underlyingType());
			typeName = obj["type"].get<std::string>();
		} else {
			solUnimplemented("");
		}
	}

	solAssert(!typeName.empty(), "");
	json["type"] = typeName;
	if (components.has_value()) {
		json["components"] = components.value();
	}

	return json;
}

Json TVMABI::setupStructComponents(StructType const* type) {
	Json components = Json::array();
	StructDefinition const& structDefinition = type->structDefinition();
	auto const& members = structDefinition.members();
	for (auto const& member: members) {
		components.push_back(setupNameTypeComponents(member->name(), getType(member.get())));
	}
	return components;
}

Json TVMABI::setupTupleComponents(TupleType const* type) {
	Json components = Json::array();
	int i = 0;
	for (Type const* c: type->components()) {
		std::string name = "value" + toString(i++);
		components.push_back(setupNameTypeComponents(name, c));
	}
	return components;
}

void AbiPosition::unroll(std::vector<Type const*>& types, Type const* type) {
	if (type->category() == Type::Category::Struct) {
		auto members = to<StructType>(type)->structDefinition().members();
		for (auto const& m: members) {
			unroll(types, m->type());
		}
	} else if (type->category() == Type::Category::UserDefinedValueType) {
		auto userDefType = to<UserDefinedValueType>(type);
		unroll(types, &userDefType->underlyingType());
	} else {
		types.emplace_back(type);
	}
}

AbiV2Position::AbiV2Position(int const _bitOffset, int const _refOffset, std::vector<Type const*> const& _types) {
	for (auto const& type: _types) {
		unroll(m_types, type);
	}

	int n = m_types.size();
	m_doLoadNextCell = std::vector<bool>(n);
	std::vector<int> sufBits(n + 1);
	std::vector<int> sufRefs(n + 1);
	for (int i = n - 1; 0 <= i; --i) {
		ABITypeSize size{m_types.at(i)};
		sufBits[i] = sufBits[i + 1] + size.maxBits;
		sufRefs[i] = sufRefs[i + 1] + size.maxRefs;
	}
	int bits = m_rootBits = _bitOffset;
	int refs = m_rootRefs = _refOffset;
	bool isRootCell = true;
	for (int i = 0; i < n; ++i) {
		ABITypeSize size{m_types.at(i)};
		if (bits + sufBits[i] <= TvmConst::CellBitLength && refs + sufRefs[i] <= 4) {
			m_doLoadNextCell[i] = false;
			bits += size.maxBits;
			refs += size.maxRefs;
			solAssert(bits <= TvmConst::CellBitLength && refs <= 4, "");
		} else {
			bits += size.maxBits;
			refs += size.maxRefs;
			if (bits > TvmConst::CellBitLength || refs >= 4) {
				m_doLoadNextCell[i] = true;
				bits = size.maxBits;
				refs = size.maxRefs;
				isRootCell = false;
			}
			solAssert(bits <= TvmConst::CellBitLength && refs <= 3, "");
		}

		if (isRootCell) {
			m_rootBits += size.maxBits;
			m_rootRefs += size.maxRefs;
		}
	}
}

bool AbiV2Position::skipType(Type const* type) {
	int i = m_curTypeIndex;
	++m_curTypeIndex;
	solAssert(type->toString() == m_types.at(i)->toString(), "");
	return m_doLoadNextCell.at(i);
}

void AbiV2Position::skipTypes(std::vector<Type const*> const& _types) {
	for (auto const& type: _types) {
		std::vector<Type const*> curTypes;
		unroll(curTypes, type);
		for (auto const& curType: curTypes) {
			skipType(curType);
		}
	}
}

ChainDataDecoder::ChainDataDecoder(StackPusher* pusher):
	pusher{pusher} {}

int ChainDataDecoder::offsetExternalFunction(bool isResponsible) const {
	// external inbound message
	int maxUsed = TvmConst::Abi::MaxOptionalSignatureLength +
				  (pusher->ctx().getContract()->externalMsgHeaders()->hasPubkey() ? 1 + 256 : 0) +
				  (pusher->ctx().getContract()->externalMsgHeaders()->hasTime() ? 64 : 0) +
				  (pusher->ctx().getContract()->externalMsgHeaders()->hasExpire() ? 32 : 0) +
				  32 +						// functionID
				  (isResponsible ? 32 : 0); // callback function
	return maxUsed;
}

int ChainDataDecoder::offsetInternalFunction(bool isResponsible) { return 32 + (isResponsible ? 32 : 0); }

void ChainDataDecoder::decodePublicFunctionParameters(
	std::vector<Type const*> const& types,
	bool isResponsible,
	bool isInternal
) const {
	if (isInternal) {
		AbiV2Position position{offsetInternalFunction(isResponsible), 0, types};
		decodeParameters(types, position);
	} else {
		AbiV2Position position{offsetExternalFunction(isResponsible), 0, types};
		decodeParameters(types, position);
	}
	*pusher << "ENDS";
}

void ChainDataDecoder::decodeFunctionParameters(
	std::vector<Type const*> const& types,
	bool isResponsible,
	bool isExternalMsg
) const {
	if (isExternalMsg)
		decodePublicFunctionParameters(types, isResponsible, false);
	else
		decodePublicFunctionParameters(types, isResponsible, true);
}

void ChainDataDecoder::decodeData(
	int offset,
	int usedRefs,
	std::vector<Type const*> const& types,
	bool withENDS
) const {
	AbiV2Position position{offset, usedRefs, types};
	decodeParameters(types, position);
	if (!withENDS)
		*pusher << "ENDS";
}

void ChainDataDecoder::decodeParameters(std::vector<Type const*> const& types, AbiPosition& position) const {
	// slice is on stack
	solAssert(pusher->stackSize() >= 1, "");

	for (auto const& type: types) {
		auto savedStackSize = pusher->stackSize();
		decodeParameter(type, &position);
		pusher->ensureSize(savedStackSize + 1, "decodeParameter-2");
	}

	if (!pusher->hasLock())
		solAssert(static_cast<int>(types.size()) <= pusher->stackSize(), "");
}

void ChainDataDecoder::decodeParametersQ(std::vector<Type const*> const& types, AbiPosition& position) const {
	pusher->startOpaque();
	int ind = 0;
	for (auto const& type: types) {
		decodeParameterQ(type, &position, ind);
		++ind;
	}
	{
		int n = types.size();
		pusher->blockSwap(n, 1);
		if (n == 1) {
			if (optValueAsTuple(types.at(0))) {
				pusher->makeTuple(1);
			}
		} else {
			pusher->makeTuple(n);
		}
		pusher->blockSwap(1, 1);
	}
	pusher->endOpaque(1, 2);
}

void ChainDataDecoder::loadNextSlice() const {
	*pusher << "LDREF";
	*pusher << "ENDS"; // only ENDS
	*pusher << "CTOS";
}

void ChainDataDecoder::decodeParameter(
	Type const* type,
	AbiPosition* position,
	bool isFirstCall,
	bool loadForFirstCallIfNeeded
) const {
	Type::Category const category = type->category();
	if (auto structType = to<StructType>(type)) {
		ast_vec<VariableDeclaration> const& members = structType->structDefinition().members();
		for (ASTPointer<VariableDeclaration> const& m: members) {
			decodeParameter(m->type(), position, isFirstCall, loadForFirstCallIfNeeded);
			isFirstCall = false;
		}
		// members... slice
		int const memberQty = members.size();
		pusher->blockSwap(memberQty, 1); // slice members...
		pusher->makeTuple(memberQty);	 // slice struct
		pusher->exchange(1);			 // ... struct slice
	} else if (isIntegralType(type)) {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		bool doLoadNextCell = position->skipType(type);
		if (doLoadNextCell && ((isFirstCall && loadForFirstCallIfNeeded) || !isFirstCall))
			loadNextSlice();
		pusher->load(type, false);
		if (auto enumType = to<EnumType>(type)) {
			pusher->pushS(1);
			pusher->pushInt(enumType->enumDefinition().members().size());
			*pusher << "GEQ";
			pusher->_throw("THROWIF " + toString(TvmConst::RuntimeException::WrongValueOfEnum));
		}
	} else if (
		to<TvmCellType>(type) ||
		to<ArrayType>(type) ||
		to<MappingType>(type) ||
		to<OptionalType>(type) ||
		to<FunctionType>(type) ||
		to<VarIntegerType>(type) ||
		category == Type::Category::Address ||
		category == Type::Category::Contract ||
		category == Type::Category::AddressStd
	) {
		bool doLoadNextCell = position->skipType(type);
		if (doLoadNextCell && ((isFirstCall && loadForFirstCallIfNeeded) || !isFirstCall))
			loadNextSlice();
		pusher->load(type, false);
	} else if (auto userDefType = to<UserDefinedValueType>(type)) {
		decodeParameter(&userDefType->underlyingType(), position, isFirstCall, loadForFirstCallIfNeeded);
	} else {
		solUnimplemented("Unsupported parameter type for decoding: " + type->toString());
	}
}

void ChainDataDecoder::decodeParameterQ(Type const* type, AbiPosition* position, int ind) const {
	Type::Category const category = type->category();
	if (/*auto structType =*/to<StructType>(type)) {
		solUnimplemented("TODO");
	} else if (
		isIntegralType(type) ||
		to<TvmCellType>(type) ||
		to<ArrayType>(type) ||
		to<MappingType>(type) ||
		category == Type::Category::Address ||
		category == Type::Category::AddressStd ||
		category == Type::Category::Contract
	) {
		bool doLoadNextCell = position->skipType(type);
		if (doLoadNextCell)
			loadNextSlice();
		pusher->loadQ(type);
	} else {
		solUnimplemented("Unsupported parameter type for decoding: " + type->toString());
	}

	pusher->startContinuation();
	if (ind > 0) {
		pusher->dropUnder(ind, 1);
	}
	pusher->pushNull();
	pusher->blockSwap(1, 1);
	pusher->endContinuation();

	pusher->ifNotJmp();

	if (auto enumType = to<EnumType>(type)) {
		// val slice -1
		pusher->pushS(1);
		pusher->pushInt(enumType->enumDefinition().members().size());
		*pusher << "GEQ";
		pusher->_throw("THROWIF " + toString(TvmConst::RuntimeException::WrongValueOfEnum));
	}
}

uint32_t ChainDataEncoder::calculateConstructorFunctionID() {
	std::vector<VariableDeclaration const*> vect;
	return calculateFunctionID("constructor", {}, &vect) & 0x7FFFFFFFu;
}

std::pair<uint32_t, bool> ChainDataEncoder::calculateFunctionID(CallableDeclaration const* declaration) {
	auto functionDefinition = to<FunctionDefinition>(declaration);
	if (functionDefinition != nullptr && functionDefinition->functionID().has_value()) {
		return {functionDefinition->functionID().value(), true};
	}

	std::string name;
	if (functionDefinition != nullptr && functionDefinition->isConstructor())
		name = "constructor";
	else
		name = declaration->name();

	std::vector<VariableDeclaration const*> tmpRet;
	std::vector<VariableDeclaration const*>* ret = nullptr;
	if (declaration->returnParameterList()) {
		tmpRet = convertArray(declaration->returnParameters());
		ret = &tmpRet;
	}

	std::vector<Type const*> inputTypes = getTypesFromVarDecls(declaration->parameters());
	if (functionDefinition->isResponsible()) {
		inputTypes.insert(inputTypes.begin(), TypeProvider::uint(32));
	}
	uint32_t id = calculateFunctionID(name, inputTypes, ret);
	return {id, false};
}

uint32_t ChainDataEncoder::toHash256(std::string const& str) {
	bytes hash = picosha2::hash256(bytes(str.begin(), str.end()));
	uint32_t funcID = 0;
	for (size_t i = 0; i < 4; i++) {
		funcID <<= 8u;
		funcID += hash[i];
	}
	return funcID;
}

uint32_t ChainDataEncoder::toPrivateFunctionId(std::string const& str) {
	uint32_t funcID = toHash256(str);
	funcID &= TvmConst::MaxCallN - 1;
	return funcID;
}

uint32_t ChainDataEncoder::calculateFunctionID(
	std::string const& name,
	std::vector<Type const*> const& inputs,
	std::vector<VariableDeclaration const*> const* outputs
) {
	std::stringstream ss;
	ss << name << "(";
	bool comma = false;
	for (auto const& type: inputs) {
		std::string typestr = toStringForCalcFuncID(type);
		solAssert(!typestr.empty(), "Wrong type in remote function params.");
		if (comma)
			ss << ",";
		ss << typestr;
		comma = true;
	}
	ss << ")";
	comma = false;
	if (outputs) {
		ss << "(";
		for (auto const& output: *outputs) {
			std::string typestr = toStringForCalcFuncID(output->type());
			solAssert(!typestr.empty(), "Wrong type in remote function params.");
			if (comma)
				ss << ",";
			ss << typestr;
			comma = true;
		}
		ss << ")";
	}
	ss << "v2";

	return toHash256(ss.str());
}

uint32_t ChainDataEncoder::calculateFunctionIDWithReason(
	CallableDeclaration const* funcDef,
	ReasonOfOutboundMessage const& reason,
	bool isLib
) {
	std::vector<VariableDeclaration const*> outputs;
	std::vector<VariableDeclaration const*>* retParams = nullptr;
	if (funcDef->returnParameterList()) {
		outputs = convertArray(funcDef->returnParameters());
		retParams = &outputs;
	}
	std::optional<uint32_t> functionId;
	std::string name = funcDef->name();
	if (auto f = to<FunctionDefinition>(funcDef)) {
		functionId = f->functionID();
		if (f->isConstructor()) {
			name = "constructor";
		}
	}

	bool isResponsible{};
	if (auto fd = to<FunctionDefinition>(funcDef)) {
		isResponsible = fd->isResponsible();
	}

	std::vector<Type const*> input = getTypesFromVarDecls(funcDef->parameters());
	if (isLib) {
		input.erase(input.begin(), input.begin() + 1);
	}

	return calculateFunctionIDWithReason(name, input, retParams, reason, functionId, isResponsible);
}

uint32_t ChainDataEncoder::calculateFunctionIDWithReason(
	std::string const& name,
	std::vector<Type const*> inputs,
	std::vector<VariableDeclaration const*> const* outputs,
	ReasonOfOutboundMessage const& reason,
	std::optional<uint32_t> functionId,
	bool const isResponsible
) {
	if (isResponsible) {
		inputs.insert(inputs.begin(), TypeProvider::uint(32));
	}
	bool isManuallyOverridden = functionId.has_value();
	uint32_t funcID{};
	if (isManuallyOverridden) {
		funcID = functionId.value();
	} else {
		funcID = calculateFunctionID(name, inputs, outputs);
		switch (reason) {
		case ReasonOfOutboundMessage::FunctionReturnExternal:
			funcID |= 0x80000000;
			break;
		case ReasonOfOutboundMessage::EmitEventExternal:
		case ReasonOfOutboundMessage::RemoteCallInternal:
			funcID &= 0x7FFFFFFFu;
			break;
		}
	}
	return funcID;
}

// reversedArgs==False ? arg[0], arg[1], ..., arg[n-1], msgBuilder
// reversedArgs==True  ? arg[n-1], ..., arg[1], arg[0], msgBuilder
// Target: create and append to msgBuilder the message body
void ChainDataEncoder::createMsgBodyAndAppendToBuilder(
	std::vector<VariableDeclaration const*> const& params,
	std::variant<uint32_t, std::function<void()>> const& functionId,
	std::optional<uint32_t> const& callbackFunctionId,
	int const bitSizeBuilder,
	int const refSizeBuilder,
	bool reversedArgs
) const {
	int const saveStackSize = pusher->stackSize();

	std::vector<Type const*> types = getParams(params).first;
	int const callbackLength = callbackFunctionId.has_value() ? 32 : 0;
	auto position = std::make_unique<AbiV2Position>(32 + callbackLength, 0, types);

	// It's a child cell. Bit for body is in the root cell
	bool doAppendBody = bitSizeBuilder + 1 + position->rootBits() <= TvmConst::CellBitLength &&
						refSizeBuilder + position->rootRefs() <= 4;

	// body:(Either X ^X)/
	if (doAppendBody)
		pusher->stzeroes(1);
	else {
		pusher->stones(1);

		if (params.size() >= 2 && !reversedArgs) {
			pusher->reverse(params.size(), 1);
			reversedArgs = !reversedArgs;
		}

		pusher->blockSwap(params.size(), 1); // msgBuilder, arg[n-1], ..., arg[1], arg[0]
		*pusher << "NEWC";					 // msgBuilder, arg[n-1], ..., arg[1], arg[0], builder
	}

	// arg[n-1], ..., arg[1], arg[0], msgBuilder
	createMsgBody(params, functionId, callbackFunctionId, *position, reversedArgs);

	if (!doAppendBody) {
		// msgBuilder, builder
		*pusher << "STBREFR";
	}

	if (!pusher->hasLock())
		solAssert(saveStackSize == static_cast<int>(pusher->stackSize() + params.size()), "");
}

// arg[n-1], ..., arg[1], arg[0], msgBuilder
void ChainDataEncoder::createMsgBody(
	std::vector<VariableDeclaration const*> const& params,
	std::variant<uint32_t, std::function<void()>> const& functionId,
	std::optional<uint32_t> const& callbackFunctionId,
	AbiV2Position& position,
	bool const reversedArgs
) const {
	auto [types, nodes] = getParams(params);

	if (functionId.index() == 0) {
		pusher->pushInt(std::get<0>(functionId));
	} else {
		std::get<1>(functionId)();
	}
	pusher->blockSwap(1, 1);
	*pusher << "STU 32";

	if (callbackFunctionId.has_value()) {
		*pusher << "STSLICECONST x" + StrUtils::intToHex(callbackFunctionId.value());
	}

	if (params.size() >= 2 && !reversedArgs) {
		pusher->reverse(params.size(), 1);
	}

	encodeParameters(types, position, false);
}

// arg[n-1], ..., arg[1], arg[0], builder
// Target: create and append to `builder` the args
void ChainDataEncoder::encodeParameters(
	std::vector<Type const*> const& _types,
	AbiV2Position& position,
	bool hasUnpackedStateVars
) const {
	// builder must be located on the top of the stack
	int builderQty = 1;
	std::vector<Type const*> typesOnStack{_types.rbegin(), _types.rend()};
	while (!typesOnStack.empty()) {
		int const argQty = typesOnStack.size() + (hasUnpackedStateVars ? 1 : 0);
		Type const* type = typesOnStack.back();
		typesOnStack.pop_back();
		if (auto structType = to<StructType>(type)) {
			std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
			// struct builder
			pusher->exchange(1);					// builder struct
			pusher->untuple(members.size());		// builder, m0, m1, ..., m[len(n)-1]
			pusher->reverse(members.size() + 1, 0); // m[len(n)-1], ..., m1, m0, builder
			for (ASTPointer<VariableDeclaration> const& m: members | std::views::reverse) {
				typesOnStack.push_back(m->type());
			}
		} else if (auto userDefType = to<UserDefinedValueType>(type)) {
			typesOnStack.push_back(&userDefType->underlyingType());
		} else {
			bool doLoadNextCell = position.skipType(type);
			if (doLoadNextCell) {
				// arg[n-1], ..., arg[1], arg[0], builder
				pusher->blockSwap(argQty, 1);
				*pusher << "NEWC";
				++builderQty;
			}
			pusher->store(type);
		}
	}

	if (hasUnpackedStateVars)
		*pusher << "STSLICE";

	for (int i = 0; i + 1 < builderQty; ++i)
		*pusher << "STBREFR";
}

std::string ChainDataEncoder::toStringForCalcFuncID(Type const* type) {
	if (auto optType = to<OptionalType>(type)) {
		return "optional(" + toStringForCalcFuncID(optType->valueType()) + ")";
	}
	if (auto tupleType = to<TupleType>(type)) {
		std::string ret = "(";
		for (size_t i = 0; i < tupleType->components().size(); i++) {
			if (i != 0)
				ret += ",";
			ret += toStringForCalcFuncID(tupleType->components().at(i));
		}
		ret += ")";
		return ret;
	}
	if (auto structType = to<StructType>(type)) {
		std::string ret = "(";
		for (size_t i = 0; i < structType->structDefinition().members().size(); i++) {
			if (i != 0)
				ret += ",";
			ret += toStringForCalcFuncID(structType->structDefinition().members()[i]->type());
		}
		ret += ")";
		return ret;
	}
	if (auto arrayType = to<ArrayType>(type)) {
		if (!arrayType->isByteArrayOrString())
			return toStringForCalcFuncID(arrayType->baseType()) + "[]";
	} else if (auto mapping = to<MappingType>(type)) {
		std::string key = toStringForCalcFuncID(mapping->keyType());
		std::string value = toStringForCalcFuncID(mapping->valueType());
		return "map(" + key + "," + value + ")";
	}

	Json obj = TVMABI::setupNameTypeComponents("some", type);
	solAssert(!obj.contains("components"), "");
	std::string typeName = obj["type"].get<std::string>();
	return typeName;
}

UnpackedCoderDecoder::UnpackedCoderDecoder(
	StackPusher& pusher,
	int _offset,
	int _usedRefs,
	int _varOffset,
	std::vector<Type const*> const& _varTypes,
	std::vector<bool> const& _varNeeded
):
	pusher{pusher},
	offset{_offset},
	usedRefs{_usedRefs},
	varOffset{_varOffset},
	varTypes{_varTypes},
	varNeeded{_varNeeded} {
	std::unique_ptr<AbiV2Position> position = createPosition();

	neededVars = 0;
	types.reserve(position->size());
	varIndex = std::vector<int>(position->size(), -1);
	to = std::vector<int>(position->size(), -1);
	isNeededType = std::vector<bool>(position->size(), false);
	startIndexType = -1;
	lastIndexType = -1;
	for (int i = 0; i < static_cast<int>(varTypes.size()); ++i) {
		int startSize = types.size();
		if (i == varOffset) {
			startIndexType = startSize;
		}
		AbiPosition::unroll(types, varTypes.at(i));
		if (varNeeded.at(i)) {
			++neededVars;
			for (int j = startSize; j < static_cast<int>(types.size()); ++j) {
				lastIndexType = j;
				isNeededType[j] = true;
				varIndex[j] = i;
			}
		}
	}

	for (int i = lastIndexType; i >= 0; --i) {
		if (i == lastIndexType || position->getDoLoadNextCell(i + 1) || isNeededType[i] != isNeededType.at(i + 1)) {
			to[i] = i + 1;
		} else {
			// The next type is not in the new cell
			to[i] = to[i + 1];
		}
	}
}

std::unique_ptr<AbiV2Position> UnpackedCoderDecoder::createPosition() const {
	auto position = std::make_unique<AbiV2Position>(offset, usedRefs, varTypes);
	position->skipTypes(std::vector<Type const*>{varTypes.begin(), varTypes.begin() + varOffset});
	return position;
}

void UnpackedCoderDecoder::unpackedData() const {
	// slice is on stack
	int const startStackSize = pusher.stackSize();
	solAssert(startStackSize >= 1, "");

	std::unique_ptr<AbiV2Position> position = createPosition();

	if (position->getDoLoadNextCell(startIndexType)) {
		pusher << "LDREFRTOS";
		pusher.popS(1);
	}
	for (int i = startIndexType; i <= lastIndexType;) {
		if (int varInd = varIndex.at(i); varInd != -1 && varNeeded.at(varInd)) {
			int nextI = i + 1;
			while (nextI <= lastIndexType && varIndex.at(nextI) == varInd) {
				++nextI;
			}

			ChainDataDecoder decoder{&pusher};
			decoder.decodeParameter(varTypes.at(varInd), position.get(), true, false);

			if (nextI <= lastIndexType && position->getDoLoadNextCell(nextI)) {
				pusher << "LDREFRTOS";
				pusher.popS(1);
			}

			i = nextI;
		} else {
			skipTypesAndLoadCellIfNeeded(i, position);
			i = to.at(i);
		}
	}
	pusher.drop();

	pusher.getStack().ensureSize(startStackSize - 1 + neededVars);
}

void UnpackedCoderDecoder::packData(std::map<int, std::function<void()>> const& varIndexToPush) {
	// unpack data
	int const startStackSize = pusher.stackSize();
	std::unique_ptr<AbiV2Position> position = createPosition();
	std::vector<Type const*> typesOnStack;

	if (position->getDoLoadNextCell(startIndexType)) {
		pusher << "LDREFRTOS";
		pusher.popS(1);
	}
	for (int i = startIndexType; i <= lastIndexType;) {
		if (int const varInd = varIndex.at(i); varInd != -1 && varNeeded.at(varInd)) {
			int nextI = i + 1;
			while (nextI <= lastIndexType && isNeededType.at(nextI))
				++nextI;

			if (i + 1 == nextI && (nextI > lastIndexType || !position->getDoLoadNextCell(nextI))) {
				pusher.load(types.at(i), false);
				int curVarInd = varIndex.at(i);
				varIndexToPush.at(curVarInd)();
				pusher.popS(2);
				typesOnStack.emplace_back(varTypes.at(curVarInd));
			} else {
				for (int j = i; j < nextI;) {
					int toJ = to.at(j);
					skipTypesAndLoadCellIfNeeded(j, position);
					j = toJ;
				}

				int varQty = 0;
				for (int j = i; j < nextI; ++j) {
					if (j == i || varIndex.at(j - 1) != varIndex.at(j)) {
						int curVarInd = varIndex.at(j);
						varIndexToPush.at(curVarInd)();
						typesOnStack.emplace_back(varTypes.at(curVarInd));
						++varQty;
					}
				}
				pusher.blockSwap(1, varQty);
			}
			i = nextI;
		} else {
			int nextI = to.at(i);
			if (position->getDoLoadNextCell(nextI)) {
				auto refs = getFixedRef(i, nextI);
				if (refs.has_value() && refs.value() == 0) {
					pusher << "LDREFRTOS";
				} else {
					pusher.pushS(0);
					pusher << "SBITREFS";
					pusher << "DEC";
					pusher << "SPLIT";
					pusher << "LDREFRTOS";
					pusher.popS(1);
				}
				typesOnStack.emplace_back(TypeProvider::tvmslice());
				for (int j = i; j < nextI; ++j)
					position->skipTypes({types.at(j)});
			} else {
				for (int j = i; j < nextI;) {
					ABITypeSize typeSize{types.at(j)};
					if (typeSize.fixedSize) {
						int k = j;
						while (k < nextI && ABITypeSize{types.at(k)}.fixedSize) {
							position->skipTypes({types.at(k)});
							++k;
						}
						auto const size = isFixedSize(j, k);
						pusher.pushInt(size->bits);
						pusher.pushInt(size->refs);
						pusher << "SPLIT";
						typesOnStack.emplace_back(TypeProvider::tvmslice());
						j = k;
					} else {
						position->skipTypes({types.at(j)});
						pusher.load(types[j], false);
						typesOnStack.emplace_back(types.at(j));
						++j;
					}
				}
			}
			i = nextI;
		}
	}

	{
		int stackDelta = pusher.stackSize() - startStackSize;
		pusher.reverse(stackDelta + 1, 0);
		std::ranges::reverse(typesOnStack);
	}

	auto popTopType = [&](Type const* type) {
		solAssert(!typesOnStack.empty());
		solAssert(*type == *typesOnStack.back());
		typesOnStack.pop_back();
	};

	// pack data
	pusher << "NEWC";
	int builderQty = 1;

	for (int i = startIndexType; i <= lastIndexType;) {
		int varInd = varIndex.at(i);

		if (varInd != -1 && varNeeded.at(varInd)) {
			int nextI = i + 1;
			while (nextI <= lastIndexType && varIndex.at(nextI) == varInd)
				++nextI;

			for (int j = i; j < nextI; ++j) {
				if (position->getDoLoadNextCell(j)) {
					// b0 b1 b2 ... data... builder
					int stackDelta = pusher.stackSize() - startStackSize - builderQty + 1;
					pusher.blockSwap(stackDelta, 1);
					pusher << "NEWC";
					++builderQty;
				}

				if (*types.at(j) != *typesOnStack.back()) {
					auto currentType = typesOnStack.back();
					typesOnStack.pop_back();

					auto structType = ::to<StructType>(currentType);
					solAssert(structType);
					std::vector<ASTPointer<VariableDeclaration>> const& members =
						structType->structDefinition().members();

					// struct builder
					pusher.exchange(1);					   // builder struct
					pusher.untuple(members.size());		   // builder, m0, m1, ..., m[len(n)-1]
					pusher.reverse(members.size() + 1, 0); // m[len(n)-1], ..., m1, m0, builder

					for (ASTPointer<VariableDeclaration> const& m: members | std::views::reverse)
						typesOnStack.push_back(m->type());
				}

				pusher.store(types.at(j));
				popTopType(types.at(j));
			}

			i = nextI;
		} else {
			int nextI = to.at(i);

			if (position->getDoLoadNextCell(i) && position->getDoLoadNextCell(nextI)) {
				// b0 b1 b2 ... data... builder
				int stackDelta = pusher.stackSize() - startStackSize - builderQty + 1;
				pusher.blockSwap(stackDelta, 1);
				pusher << "NEWC";
				++builderQty;

				pusher << "STSLICE";
				popTopType(TypeProvider::tvmslice());
			} else if (!position->getDoLoadNextCell(i) && position->getDoLoadNextCell(nextI)) {
				pusher << "STSLICE";
				popTopType(TypeProvider::tvmslice());
			} else {
				if (position->getDoLoadNextCell(i)) {
					// b0 b1 b2 ... data... builder
					int stackDelta = pusher.stackSize() - startStackSize - builderQty + 1;
					pusher.blockSwap(stackDelta, 1);
					pusher << "NEWC";
					++builderQty;
				}

				for (int j = i; j < nextI;) {
					ABITypeSize typeSize{types.at(j)};
					if (typeSize.fixedSize) {
						int k = j;
						while (k < nextI && ABITypeSize{types.at(k)}.fixedSize) {
							++k;
						}
						pusher << "STSLICE";
						popTopType(TypeProvider::tvmslice());
						j = k;
					} else {
						pusher.store(types[j]);
						popTopType(types.at(j));
						++j;
					}
				}
			}

			i = nextI;
		}
	}
	pusher << "STSLICE"; // store tail

	for (int i = 0; i + 1 < builderQty; ++i) {
		pusher << "STBREFR";
	}

	pusher << "ENDC";
	pusher << "CTOS";

	solAssert(startStackSize == pusher.stackSize(), "startStackSize == pusher.stackSize()");
}

std::optional<UnpackedCoderDecoder::TypeSize> UnpackedCoderDecoder::isFixedSize(int i, int j) const {
	solAssert(i < j);
	int bits = 0;
	int refs = 0;
	for (int k = i; k < j; ++k) {
		ABITypeSize typeSize{types.at(k)};
		if (!typeSize.fixedSize)
			return {};
		bits += typeSize.maxBits;
		refs += typeSize.maxRefs;
	}
	return TypeSize{bits, refs};
}

std::optional<int> UnpackedCoderDecoder::getFixedRef(int i, int j) const {
	solAssert(i < j);
	int refs = 0;
	for (int k = i; k < j; ++k) {
		ABITypeSize typeSize{types.at(k)};
		if (!typeSize.fixedRefs)
			return {};
		refs += typeSize.maxRefs;
	}
	return refs;
}

void UnpackedCoderDecoder::skipTypes(
	int beginIndex,
	int endIndex,
	std::unique_ptr<AbiV2Position> const& position
) const {
	int dropQty = 0;
	for (int i = beginIndex; i < endIndex;) {
		ABITypeSize typeSize{types.at(i)};
		if (typeSize.fixedSize) {
			int j = i;
			while (j < endIndex && ABITypeSize{types.at(j)}.fixedSize) {
				position->skipTypes({types.at(j)});
				++j;
			}
			auto const size = isFixedSize(i, j);
			pusher.pushInt(size->bits);
			pusher.pushInt(size->refs);
			pusher << "SSKIPFIRST";
			i = j;
		} else {
			position->skipTypes({types.at(i)});
			pusher.load(types[i], false);
			++dropQty;
			++i;
		}
	}
	pusher.dropUnder(dropQty, 1);
}

void UnpackedCoderDecoder::skipTypesAndLoadCellIfNeeded(
	int index,
	std::unique_ptr<AbiV2Position> const& position
) const {
	int const nextIndex = to.at(index);
	if (nextIndex <= lastIndexType && position->getDoLoadNextCell(nextIndex)) {
		auto const refs = getFixedRef(index, nextIndex);
		if (refs.has_value()) {
			pusher << "PLDREFIDX " + toString(refs.value());
			pusher << "CTOS";
			for (int k = index; k < nextIndex; ++k)
				position->skipTypes({types.at(k)});
		} else if (index + 1 == nextIndex) {
			skipTypes(index, nextIndex, position);
			pusher << "PLDREFIDX 0";
			pusher << "CTOS";
		} else {
			// pusher.pushS(0);
			// pusher << "SREFS";
			// pusher << "DEC";
			// pusher << "PLDREFVAR";
			// pusher << "CTOS";

			pusher.pushInt(0);
			pusher.pushInt(1);
			pusher << "SCUTLAST";
			pusher << "LDREFRTOS";
			pusher.popS(1);
			for (int k = index; k < nextIndex; ++k)
				position->skipTypes({types.at(k)});
		}
	} else {
		skipTypes(index, nextIndex, position);
	}
}
