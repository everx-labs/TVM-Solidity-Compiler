/*
 * Copyright (C) 2019-2023 EverX. All Rights Reserved.
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

#include <libsolutil/picosha2.h>
#include <libsolidity/ast/TypeProvider.h>
#include <libsolidity/analysis/TypeChecker.h>

#include "TVMABI.hpp"
#include "TVMPusher.hpp"
#include "TVM.hpp"
#include "TVMConstants.hpp"
#include "TVMContractCompiler.hpp"

using namespace solidity::frontend;
using namespace std;
using namespace solidity::util;
using namespace solidity::langutil;

Json::Value TVMABI::generateFunctionIdsJson(
	ContractDefinition const& contract,
	PragmaDirectiveHelper const& pragmaHelper
) {
	TVMCompilerContext ctx{&contract, pragmaHelper};
	StackPusher pusher{&ctx};
	ChainDataEncoder encoder{&pusher};
	std::vector<const FunctionDefinition *> publicFunctions = TVMABI::publicFunctions(contract);
	std::map<std::string, uint32_t> map;
	for (FunctionDefinition const* func : publicFunctions) {
		uint32_t functionID = encoder.calculateFunctionIDWithReason(
			func,
			ReasonOfOutboundMessage::RemoteCallInternal
		);
		const std::string name = TVMCompilerContext::getFunctionExternalName(func);
		map[name] = functionID;
	}
	if (map.count("constructor") == 0) {
		map["constructor"] = encoder.calculateConstructorFunctionID();
	}
	for (VariableDeclaration const* vd : ctx.c4StateVariables()) {
		if (vd->isPublic()) {
			std::vector<VariableDeclaration const*> outputs = {vd};
			uint32_t functionId = encoder.calculateFunctionIDWithReason(
					vd->name(),
					{},
					&outputs,
					ReasonOfOutboundMessage::RemoteCallInternal,
					nullopt,
					false
			);
			map[vd->name()] = functionId;
		}
	}

	Json::Value root(Json::objectValue);
	for (const auto&[func, functionID] : map) {
		std::stringstream ss;
		ss << "0x" << std::hex << std::setfill('0') << std::setw(8) << functionID;
		root[func] = ss.str();
	}
	return root;
}

Json::Value
TVMABI::generatePrivateFunctionIdsJson(
	ContractDefinition const& contract,
	std::vector<std::shared_ptr<SourceUnit>> _sourceUnits,
	PragmaDirectiveHelper const& pragmaHelper
) {
	Json::Value ids{Json::arrayValue};
	Pointer<Contract> codeContract = TVMContractCompiler::generateContractCode(&contract, _sourceUnits, pragmaHelper);
	for (Pointer<Function> const& fun : codeContract->functions()) {
		FunctionDefinition const* def = fun->functionDefinition();
		if (fun->type() == Function::FunctionType::Fragment && fun->functionId()) {
			Json::Value func{Json::objectValue};
			func["scope"] = def->isFree() ? "" : def->annotation().contract->name();
			func["sign"] = def->externalSignature();
			func["id"] = fun->functionId().value();
			ids.append(func);
		}
	}
	return ids;
}

Json::Value TVMABI::generateABIJson(
	ContractDefinition const *contract,
	std::vector<std::shared_ptr<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const *> const &pragmaDirectives
) {
	PragmaDirectiveHelper pdh{pragmaDirectives};
	TVMCompilerContext ctx(contract, pdh);

	const std::vector<const FunctionDefinition *> publicFunctions = TVMABI::publicFunctions(*contract);
	std::vector<const EventDefinition *> events {};

	for (const auto &_event : contract->definedInterfaceEvents())
		events.push_back(_event);
	for (std::shared_ptr<SourceUnit> const& source: _sourceUnits)
		for (ASTPointer<ASTNode> const &node: source->nodes())
			if (auto lib = dynamic_cast<ContractDefinition const *>(node.get()))
				if (lib->isLibrary())
					for (const auto &event : lib->definedInterfaceEvents())
						events.push_back(event);

	std::set<std::string> used;

	Json::Value root(Json::objectValue);
	root["ABI version"] = 2;
	root["version"] = "2.4";

	// header
	{
		Json::Value header(Json::arrayValue);

		// NOTE: order is important
		if (pdh.hasPubkey()) header.append("pubkey");
		if (pdh.hasTime())   header.append("time");
		if (pdh.hasExpire()) header.append("expire");

		root["header"] = header;
	}

	// functions
	{
		Json::Value functions(Json::arrayValue);
		for (FunctionDefinition const* f : publicFunctions) {
			auto fname = TVMCompilerContext::getFunctionExternalName(f);
			if (used.count(fname)) {
				continue;
			}
			used.insert(fname);
			functions.append(toJson(fname, convertArray(f->parameters()), convertArray(f->returnParameters()), f));
		}

		if (used.count("constructor") == 0) {
			functions.append(toJson("constructor", {}, {}, nullptr));
		}

		// add public state variables to functions
		for (VariableDeclaration const* vd : ctx.c4StateVariables()) {
			if (vd->isPublic()) {
				functions.append(toJson(vd->name(), {}, {vd}));
			}
		}

		for (FunctionDefinition const* fd : ctx.usage().awaitFunctions()) {
			std::string name = "_await_" + fd->annotation().contract->name() + "_" + fd->name();
			functions.append(toJson(name, convertArray(fd->returnParameters()), {}));
		}


		root["functions"] = functions;
	}

	// events
	{
		Json::Value eventAbi(Json::arrayValue);
		std::set<std::string> usedEvents;
		for (const auto &e: events) {
			const auto &ename = e->name();
			solAssert(!ename.empty(), "Empty event name!");
			string name = eventName(e);
			if (usedEvents.count(name)) {
				solUnimplemented("Event name duplication!");
			}
			usedEvents.insert(name);
			Json::Value cur;
			cur["name"] = name;
			cur["inputs"] = encodeParams(convertArray(e->parameters()));
			eventAbi.append(cur);
		}
		root["events"] = eventAbi;
	}

	// fields
	{
		Json::Value fields(Json::arrayValue);
		std::vector<std::pair<std::string, std::string>> offset{{"_pubkey", "uint256"}};
		if (ctx.storeTimestampInC4()) {
			offset.emplace_back("_timestamp", "uint64");
		}
		offset.emplace_back("_constructorFlag", "bool");
		if (ctx.usage().hasAwaitCall()) {
			offset.emplace_back("_await", "optional(cell)");
		}

		for (const auto& [name, type] : offset) {
			Json::Value field(Json::objectValue);
			field["name"] = name;
			field["type"] = type;
			field["init"] = name == "_pubkey";
			fields.append(field);
		}

		std::vector<VariableDeclaration const *> stateVars = ctx.c4StateVariables();
		std::set<std::string> usedNames;
		std::vector<std::tuple<std::string, Type const*, bool>> namesTypes;
		for (VariableDeclaration const * var : stateVars | boost::adaptors::reversed) {
			std::string name = var->name();
			if (usedNames.count(name) != 0)
				name = var->annotation().contract->name() + "$" + var->name();
			solAssert(usedNames.count(name) == 0, "");
			usedNames.insert(name);
			namesTypes.emplace_back(name, var->type(), var->isStatic());
		}
		std::reverse(namesTypes.begin(), namesTypes.end());

		for (auto const&[name, type, isStatic] : namesTypes) {
			Json::Value cur = setupNameTypeComponents(name, type);
			cur["init"] = isStatic;
			fields.append(cur);
		}
		root["fields"] = fields;
	}

	return root;
}

void TVMABI::generateABI(
	ContractDefinition const *contract,
	std::vector<std::shared_ptr<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const *> const &pragmaDirectives,
	ostream *out
) {
	Json::Value root = generateABIJson(contract, _sourceUnits, pragmaDirectives);

//	Json::StreamWriterBuilder builder;
//	const std::string json_file = Json::writeString(builder, root);
//	*out << json_file << std::endl;


	*out << "{\n";
	*out << "\t" << R"("ABI version": )" << root["ABI version"].asString() << ",\n";
	if (root.isMember("version")) {
		*out << "\t" << R"("version": )" << root["version"] << ",\n";
	}

	if (root.isMember("version")) {
		*out << "\t" << R"("header": [)";
		for (unsigned i = 0; i < root["header"].size(); ++i) {
			*out << root["header"][i];
			if (i + 1 != root["header"].size()) {
				*out << ", ";
			}
		}
		*out << "],\n";
	}

	*out << "\t" << R"("functions": [)" << "\n";
	print(root["functions"], out);
	*out << "\t" << "],\n";

	*out << "\t" << R"("events": [)" << "\n";
	print(root["events"], out);

	if (root.isMember("fields")) {
		*out << "\t" << "],\n";
		*out << "\t" << R"("fields": [)" << "\n";
		printData(root["fields"], out);
		*out << "\t" << "]\n";
	} else {
		*out << "\t" << "]\n";
	}

	*out << "}" << endl;
}

std::vector<const FunctionDefinition *> TVMABI::publicFunctions(ContractDefinition const& contract) {
	std::vector<const FunctionDefinition *> publicFunctions;
	if (auto main_constr = contract.constructor(); main_constr != nullptr)
		publicFunctions.push_back(contract.constructor());

	for (auto c : contract.annotation().linearizedBaseContracts) {
		for (const auto &_function : c->definedFunctions()) {
			if (!_function->isConstructor() && _function->isPublic() &&
				!_function->isReceive() && !_function->isFallback() && !_function->isOnBounce() && !_function->isOnTickTock())
				publicFunctions.push_back(_function);
		}
	}
	return publicFunctions;
}

void TVMABI::printData(const Json::Value &json, std::ostream* out) {
	for (unsigned f = 0; f < json.size(); ++f) {
		const auto &element = json[f];
		*out << "\t\t";

		Json::StreamWriterBuilder builder;
		builder["indentation"] = "";
		std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
		writer->write(element, out);

		if (f + 1 != json.size())
			*out << ",";
		*out << std::endl;
	}
}

void TVMABI::print(const Json::Value &json, ostream *out) {
	for (unsigned f = 0; f < json.size(); ++f) {
		const auto& function = json[f];
		*out << "\t\t{\n";

		*out << "\t\t\t" << R"("name": )" << function["name"] << ",\n";

		if (function.isMember("id")) {
			*out << "\t\t\t" << R"("id": )" << function["id"] << ",\n";
		}

		*out << "\t\t\t" << R"("inputs": [)" << "\n";
		for (unsigned i = 0; i < function["inputs"].size(); ++i) {
			const auto& input = function["inputs"][i];
			Json::StreamWriterBuilder builder;
			builder["indentation"] = "";
			std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
			*out << "\t\t\t\t";
			writer->write(input, out);
			if (i + 1 == function["inputs"].size()) {
				*out << "\n";
			} else {
				*out << ",\n";
			}
		}
		*out << "\t\t\t" << "]" << ",\n";

		*out << "\t\t\t" << R"("outputs": [)" << "\n";
		for (unsigned o = 0; o < function["outputs"].size(); ++o) {
			const auto& output = function["outputs"][o];
			Json::StreamWriterBuilder builder;
			builder["indentation"] = "";
			std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
			*out << "\t\t\t\t";
			writer->write(output, out);
			if (o + 1 == function["outputs"].size()) {
				*out << "\n";
			} else {
				*out << ",\n";
			}
		}
		*out << "\t\t\t" << "]" << "\n";

		if (f + 1 == json.size())
			*out << "\t\t}\n";
		else
			*out << "\t\t},\n";
	}
}

Json::Value TVMABI::toJson(
	const string &fname,
	const std::vector<VariableDeclaration const*> &params,
	const std::vector<VariableDeclaration const*> &retParams,
	FunctionDefinition const* funcDef
) {
	Json::Value function;
	Json::Value inputs = encodeParams(params);
	if (funcDef != nullptr && funcDef->isResponsible()) {
		Json::Value json(Json::objectValue);
		json["name"] = "answerId";
		json["type"] = "uint32";
		inputs.insert(0, json);
	}
	Json::Value outputs = encodeParams(retParams);
	function["name"] = fname;
	if (funcDef && funcDef->functionID().has_value()) {
		std::ostringstream oss;
		oss << "0x" << std::hex << std::uppercase << funcDef->functionID().value();
		function["id"] = oss.str();
	}
	function["inputs"] = inputs;
	function["outputs"] = outputs;
	return function;
}

Json::Value TVMABI::encodeParams(const std::vector<VariableDeclaration const*> &params) {
	Json::Value result(Json::arrayValue);
	size_t idx = 0;
	for (const auto& variable: params) {
		string name = variable->name();
		if (name.empty()) name = "value" + toString(idx);
		Json::Value json = setupNameTypeComponents(name, getType(variable));
		result.append(json);
		idx++;
	}
	return result;
}

Json::Value TVMABI::setupNameTypeComponents(const string &name, const Type *type) {
	Json::Value json(Json::objectValue);
	json["name"] = name;
	std::string typeName;
	std::optional<Json::Value> components;

	TypeChecker typeChecker{*GlobalParams::g_errorReporter};
	SourceLocation tmpLoc;
	std::set<StructDefinition const*> tmpSet;
	if (
		to<StructType>(type) == nullptr &&
		typeChecker.isBadAbiType(tmpLoc, type, tmpLoc, tmpSet, false))
	{
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
				Json::Value comp(Json::arrayValue);
				{
					Json::Value obj(Json::objectValue);
					obj["name"] = name + "_length";
					obj["type"] = "uint32";
					comp.append(obj);
				}
				{
					Json::Value obj(Json::objectValue);
					obj["name"] = name + "_dict";
					obj["type"] = "optional(cell)";
					comp.append(obj);
				}
				components = comp;
				break;
			}
			default:
				solUnimplemented("TODO: support for " + type->toString());
		}
	} else {
		const Type::Category category = type->category();
		TypeInfo ti(type);
		if (category == Type::Category::Address || category == Type::Category::Contract) {
			typeName = "address";
		} else if (category == Type::Category::VarInteger) {
			auto varInt = to<VarIntegerType>(type);
			typeName = varInt->toString(false);
			boost::algorithm::to_lower(typeName);
		} else if (auto* fixedBytesType = to<FixedBytesType>(type)) {
			typeName = "fixedbytes" + toString(fixedBytesType->numBytes());
		} else if (ti.isNumeric) {
			if (to<BoolType>(type)) {
				typeName = "bool";
			} else if (ti.isSigned) {
				typeName = "int" + toString(ti.numBits);
			} else {
				typeName = "uint" + toString(ti.numBits);
			}
		} else if (auto arrayType = to<ArrayType>(type)) {
			Type const *arrayBaseType = arrayType->baseType();
			if (arrayType->isByteArrayOrString()) {
				if (arrayType->isString()) {
					typeName = "string";
				} else {
					typeName = "bytes";
				}
			} else {
				Json::Value obj = setupNameTypeComponents("arrayBaseType", arrayBaseType);
				typeName = obj["type"].asString() + "[]";
				if (obj.isMember("components")) {
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
				Json::Value obj = setupNameTypeComponents("keyType", mapping->keyType());
				key = obj["type"].asString();
				solAssert(!obj.isMember("components"), "");
			}
			{
				Json::Value obj = setupNameTypeComponents("valueType", mapping->valueType());
				value = obj["type"].asString();
				if (obj.isMember("components")) {
					components = obj["components"];
				}
			}
			typeName = "map(" + key + "," + value + ")";
		} else if (auto opt = to<OptionalType>(type)) {
			if (auto tt = to<TupleType>(opt->valueType())) {
				typeName = "optional(tuple)";
				components = setupTupleComponents(tt);
			} else {
				Json::Value obj = setupNameTypeComponents("valueType", opt->valueType());
				typeName = "optional(" + obj["type"].asString() + ")";
				if (obj.isMember("components")) {
					components = obj["components"];
				}
			}
		} else if (auto userDefType = to<UserDefinedValueType>(type)) {
			Json::Value obj = setupNameTypeComponents("", &userDefType->underlyingType());
			typeName = obj["type"].asString();
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

Json::Value TVMABI::setupStructComponents(const StructType *type) {
	Json::Value components(Json::arrayValue);
	const StructDefinition& structDefinition = type->structDefinition();
	const auto& members = structDefinition.members();
	for (const auto & member : members) {
		components.append(setupNameTypeComponents(member->name(), getType(member.get())));
	}
	return components;
}

Json::Value TVMABI::setupTupleComponents(const TupleType* type) {
	Json::Value components(Json::arrayValue);
	int i = 0;
	for (Type const* c : type->components()) {
		std::string name = "value" + toString(i++);
		components.append(setupNameTypeComponents(name, c));
	}
	return components;
}

DecodePositionAbiV2::DecodePositionAbiV2(int _bitOffset, int _refOffset, const std::vector<Type const *>& _types) {
	for (const auto & type : _types) {
		initTypes(type);
	}

	int bits = _bitOffset;
	int refs = _refOffset;
	int n = m_types.size();
	m_doLoadNextCell = std::vector<bool>(n);
	std::vector<int> sufBits(n + 1);
	std::vector<int> sufRefs(n + 1);
	for (int i = n - 1; 0 <= i; --i) {
		ABITypeSize size{m_types.at(i)};
		sufBits[i] = sufBits[i + 1] + size.maxBits;
		sufRefs[i] = sufRefs[i + 1] + size.maxRefs;
	}
	m_countOfCreatedBuilders = 0;
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
				++m_countOfCreatedBuilders;
			}
			solAssert(bits <= TvmConst::CellBitLength && refs <= 3, "");
		}
	}
}

void DecodePositionAbiV2::initTypes(Type const* type) {
	if (type->category() == Type::Category::Struct) {
		auto members = to<StructType>(type)->structDefinition().members();
		for (const auto &m : members) {
			initTypes(m->type());
		}
	} else if (type->category() == Type::Category::UserDefinedValueType) {
		auto userDefType = to<UserDefinedValueType>(type);
		initTypes(&userDefType->underlyingType());
	} else {
		m_types.push_back(type);
	}
}

bool DecodePositionAbiV2::loadNextCell(Type const *type) {
	int i = m_curTypeIndex;
	++m_curTypeIndex;
	solAssert(type->toString() == m_types[i]->toString(), "");
	ABITypeSize size{type};
	return m_doLoadNextCell.at(i);
}

int DecodePositionAbiV2::countOfCreatedBuilders() const {
	return m_countOfCreatedBuilders;
}

ChainDataDecoder::ChainDataDecoder(StackPusher *pusher) :
		pusher{pusher} {

}

int ChainDataDecoder::maxBits(bool isResponsible) {
	// external inbound message
	int maxUsed = TvmConst::Abi::MaxOptionalSignLength +
				  (pusher->ctx().pragmaHelper().hasPubkey()? 1 + 256 : 0) +
				  (pusher->ctx().pragmaHelper().hasTime()? 64 : 0) +
				  (pusher->ctx().pragmaHelper().hasExpire()? 32 : 0) +
				  32 + // functionID
				  (isResponsible ? 32 : 0); // callback function
	return maxUsed;
}

int ChainDataDecoder::minBits(bool isResponsible) {
	return 32 + (isResponsible ? 32 : 0);
}

void ChainDataDecoder::decodePublicFunctionParameters(
	const std::vector<Type const*>& types,
	bool isResponsible,
	bool isInternal
) {
	if (isInternal) {
		DecodePositionAbiV2 position{minBits(isResponsible), 0, types};
		decodeParameters(types, position);
	} else {
		DecodePositionAbiV2 position{maxBits(isResponsible), 0, types};
		decodeParameters(types, position);
	}
	*pusher << "ENDS";
}

void ChainDataDecoder::decodeFunctionParameters(const std::vector<Type const*>& types, bool isResponsible) {
	pusher->startOpaque();
	pusher->pushS(1);
	pusher->fixStack(-1); // fix stack

	pusher->startContinuation();
	decodePublicFunctionParameters(types, isResponsible, false);
	pusher->endContinuation();

	pusher->startContinuation();
	decodePublicFunctionParameters(types, isResponsible, true);
	pusher->endContinuation();

	pusher->ifElse();
	pusher->endOpaque(1, types.size());
}

void ChainDataDecoder::decodeData(int offset, int usedRefs, const std::vector<Type const*>& types) {
	DecodePositionAbiV2 position{offset, usedRefs, types};
	decodeParameters(types, position);
	*pusher << "ENDS";
}

void ChainDataDecoder::decodeParameters(
	const std::vector<Type const*>& types,
	DecodePosition& position
) {
	// slice are on stack
	solAssert(pusher->stackSize() >= 1, "");

	for (const auto & type : types) {
		auto savedStackSize = pusher->stackSize();
		decodeParameter(type, &position);
		pusher->ensureSize(savedStackSize + 1, "decodeParameter-2");
	}

	if (!pusher->hasLock())
		solAssert(static_cast<int>(types.size()) <= pusher->stackSize(), "");
}

void ChainDataDecoder::decodeParametersQ(
	const std::vector<Type const*>& types,
	DecodePosition& position
) {
	pusher->startContinuation();
	pusher->startOpaque();
	int ind = 0;
	for (const auto & type : types) {
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
	pusher->pushContAndCallX(1, 2, false);
}

void ChainDataDecoder::loadNextSlice() {
	*pusher << "LDREF";
	*pusher << "ENDS"; // only ENDS
	*pusher << "CTOS";
}

void ChainDataDecoder::loadNextSliceIfNeed(bool doLoadNextSlice) {
	if (doLoadNextSlice) {
		loadNextSlice();
	}
}

void ChainDataDecoder::decodeParameter(Type const* type, DecodePosition* position) {
	const Type::Category category = type->category();
	if (auto structType = to<StructType>(type)) {
		ast_vec<VariableDeclaration> const& members = structType->structDefinition().members();
		for (const ASTPointer<VariableDeclaration> &m : members) {
			decodeParameter(m->type(), position);
		}
		// members... slice
		const int memberQty = members.size();
		pusher->blockSwap(memberQty, 1); // slice members...
		pusher->makeTuple(memberQty); // slice struct
		pusher->exchange(1); // ... struct slice
	} else if (isIntegralType(type)) {
		TypeInfo ti{type};
		solAssert(ti.isNumeric, "");
		loadNextSliceIfNeed(position->loadNextCell(type));
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
		(category == Type::Category::Address || category == Type::Category::Contract)
	) {
		loadNextSliceIfNeed(position->loadNextCell(type));
		pusher->load(type, false);
	} else if (auto userDefType = to<UserDefinedValueType>(type)) {
		decodeParameter(&userDefType->underlyingType(), position);
	} else {
		solUnimplemented("Unsupported parameter type for decoding: " + type->toString());
	}
}

void ChainDataDecoder::decodeParameterQ(Type const* type, DecodePosition* position, int ind) {
	const Type::Category category = type->category();
	if (/*auto structType =*/ to<StructType>(type)) {
		solUnimplemented("TODO");
	} else if (
		isIntegralType(type) ||
		to<TvmCellType>(type) ||
		to<ArrayType>(type) ||
		to<MappingType>(type) ||
		(category == Type::Category::Address || category == Type::Category::Contract)
	) {
		loadNextSliceIfNeed(position->loadNextCell(type));
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

void ChainDataEncoder::createDefaultConstructorMsgBodyAndAppendToBuilder(const int bitSizeBuilder)
{
	uint32_t funcID = calculateConstructorFunctionID();
	std::stringstream ss;
	ss << "x" << std::hex << std::setfill('0') << std::setw(8) << funcID;

	if (bitSizeBuilder < (1023 - 32 - 1)) {
		pusher->stzeroes(1);
		*pusher << "STSLICECONST " + ss.str();
	} else {
		pusher->stones(1);
		*pusher << "NEWC";
		*pusher << "STSLICECONST " + ss.str();
		*pusher << "STBREFR";
	}
}

void ChainDataEncoder::createDefaultConstructorMessage2()
{
	uint32_t funcID = calculateConstructorFunctionID();
	std::stringstream ss;
	ss << "x" << std::hex << std::setfill('0') << std::setw(8) << funcID;
	*pusher << "STSLICECONST " + ss.str();
}

uint32_t ChainDataEncoder::calculateConstructorFunctionID() {
	std::vector<VariableDeclaration const*> vect;
	return calculateFunctionID("constructor", {}, &vect) & 0x7FFFFFFFu;
}

std::pair<uint32_t, bool> ChainDataEncoder::calculateFunctionID(const CallableDeclaration *declaration) {
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
	uint32_t id = calculateFunctionID(
			name,
			inputTypes,
			ret
	);
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

uint32_t ChainDataEncoder::calculateFunctionID(
		const std::string& name,
		const std::vector<Type const*>& inputs,
		const std::vector<VariableDeclaration const*> * outputs
) {
	std::stringstream ss;
	ss << name << "(";
	bool comma = false;
	for (const auto& type : inputs) {
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
		for (const auto& output : *outputs) {
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
		const CallableDeclaration *funcDef,
		const ReasonOfOutboundMessage &reason,
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
		const std::string& name,
		std::vector<Type const*> inputs,
		const std::vector<VariableDeclaration const*> *outputs,
		const ReasonOfOutboundMessage &reason,
		std::optional<uint32_t> functionId,
		const bool isResponsible
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
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		const int bitSizeBuilder,
		const bool reversedArgs
) {

	const int saveStackSize = pusher->stackSize();

	std::vector<Type const*> types = getParams(params).first;
	const int callbackLength = callbackFunctionId.has_value() ? 32 : 0;
	std::unique_ptr<DecodePositionAbiV2> position = std::make_unique<DecodePositionAbiV2>(bitSizeBuilder + 32 + callbackLength, 0, types);
	const bool doAppend = position->countOfCreatedBuilders() == 0;
	doAppend ? pusher->stzeroes(1) : pusher->stones(1);
	if (params.size() >= 2 && !reversedArgs) {
		pusher->reverse(params.size(), 1);
	}
	if (!doAppend) {
		position = std::make_unique<DecodePositionAbiV2>(32 + callbackLength, 0, types);
		pusher->blockSwap(params.size(), 1); // msgBuilder, arg[n-1], ..., arg[1], arg[0]
		*pusher << "NEWC"; // msgBuilder, arg[n-1], ..., arg[1], arg[0], builder
	}

	// arg[n-1], ..., arg[1], arg[0], msgBuilder
	createMsgBody(params, functionId, callbackFunctionId, *position);

	if (!doAppend) {
		// msgBuilder, builder
		*pusher << "STBREFR";
	}

	if (!pusher->hasLock())
		solAssert(saveStackSize == int(pusher->stackSize() + params.size()), "");

}

// arg[n-1], ..., arg[1], arg[0], msgBuilder
void ChainDataEncoder::createMsgBody(
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		DecodePositionAbiV2 &position
) {
	std::vector<Type const*> types;
	std::vector<ASTNode const*> nodes;
	std::tie(types, nodes) = getParams(params);

	if (functionId.index() == 0) {
		std::stringstream ss;
		ss << "x" << std::hex << std::setfill('0') << std::setw(8) << std::get<0>(functionId);
		*pusher << "STSLICECONST " + ss.str();
	} else {
		std::get<1>(functionId)();
		*pusher << "STUR 32";
	}

	if (callbackFunctionId.has_value()) {
		std::stringstream ss;
		ss << "x" << std::hex << std::setfill('0') << std::setw(8) << callbackFunctionId.value();
		*pusher << "STSLICECONST " + ss.str();
	}

	encodeParameters(types, position);
}

// arg[n-1], ..., arg[1], arg[0], builder
// Target: create and append to `builder` the args
void ChainDataEncoder::encodeParameters(
	const std::vector<Type const *> & _types,
	DecodePositionAbiV2 &position
) {
	// builder must be located on the top of stack
	std::vector<Type const *> typesOnStack{_types.rbegin(), _types.rend()};
	while (!typesOnStack.empty()) {
		const int argQty = typesOnStack.size();
		Type const* type = typesOnStack.back();
		typesOnStack.pop_back();
		if (auto structType = to<StructType>(type)) {
			std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
			// struct builder
			pusher->exchange(1); // builder struct
			pusher->untuple(members.size()); // builder, m0, m1, ..., m[len(n)-1]
			pusher->reverse(members.size() + 1, 0); // m[len(n)-1], ..., m1, m0, builder
			for (const ASTPointer<VariableDeclaration>& m : members | boost::adaptors::reversed) {
				typesOnStack.push_back(m->type());
			}
		} else if (auto userDefType = to<UserDefinedValueType>(type)) {
			typesOnStack.push_back(&userDefType->underlyingType());
		} else {
			if (position.loadNextCell(type)) {
				// arg[n-1], ..., arg[1], arg[0], builder
				pusher->blockSwap(argQty, 1);
				*pusher << "NEWC";
			}
			pusher->store(type, false);
		}
	}
	for (int idx = 0; idx < position.countOfCreatedBuilders(); idx++) {
		*pusher << "STBREFR";
	}
}

std::string ChainDataEncoder::toStringForCalcFuncID(Type const * type) {
	if (auto optType = to<OptionalType>(type)) {
		return "optional(" + toStringForCalcFuncID(optType->valueType()) + ")";
	} else if (auto tupleType = to<TupleType>(type)) {
		std::string ret = "(";
		for (size_t i = 0; i < tupleType->components().size(); i++) {
			if (i != 0) ret += ",";
			ret += toStringForCalcFuncID(tupleType->components().at(i));
		}
		ret += ")";
		return ret;
	} else if (auto structType = to<StructType>(type)) {
		std::string ret = "(";
		for (size_t i = 0; i < structType->structDefinition().members().size(); i++) {
			if (i != 0) ret += ",";
			ret += toStringForCalcFuncID(structType->structDefinition().members()[i]->type());
		}
		ret += ")";
		return ret;
	} else if (auto arrayType = to<ArrayType>(type)) {
		if (!arrayType->isByteArrayOrString())
			return toStringForCalcFuncID(arrayType->baseType()) + "[]";
	} else if (auto mapping = to<MappingType>(type)) {
		return "map(" + toStringForCalcFuncID(mapping->keyType()) + "," +
			   toStringForCalcFuncID(mapping->valueType()) + ")";
	}

	Json::Value obj = TVMABI::setupNameTypeComponents("some", type);
	solAssert(!obj.isMember("components"), "");
	std::string typeName = obj["type"].asString();
	return typeName;
}
