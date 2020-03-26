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
 * ABI generator and parser for TON
 */

#pragma once

#include "TVMCommons.hpp"

namespace solidity::frontend {

class TVMABI {
public:
	static void generateABI(ContractDefinition const* contract, const vector<ContractDefinition const*>& m_allContracts,
	                        std::vector<PragmaDirective const *> const& pragmaDirectives) {

		PragmaDirectiveHelper pdh{pragmaDirectives};
		TVMCompilerContext ctx(contract, m_allContracts, pdh);

		std::vector<const FunctionDefinition *> publicFunctions {};
		std::vector<const EventDefinition *> events {};

		if (auto main_constr = contract->constructor(); main_constr != nullptr)
			publicFunctions.push_back(contract->constructor());
		for (auto c : m_allContracts) {
			for (const auto &_function : c->definedFunctions()) {
				if (_function->isPublic() && !isTvmIntrinsic(_function->name()) && !_function->isConstructor() &&
					!_function->isReceive() && !_function->isFallback())
					publicFunctions.push_back(_function);
			}
		}

		for (const auto &_event : contract->interfaceEvents())
			events.push_back(_event);

		std::set<std::string> used;

		Json::Value root(Json::objectValue);
		root["ABI version"] = ctx.pragmaHelper().abiVersion();

		// header
		if (ctx.pragmaHelper().abiVersion() == 2) {
			Json::Value header(Json::arrayValue);
			for (const std::string &h : {"pubkey", "time", "expire"}) {
				if (std::get<0>(pdh.haveHeader(h)) || (h == "time" && ctx.haveTimeInAbiHeader())) {
					header.append(h);
				}
			}
			root["header"] = header;
		}

		// functions
		{
			Json::Value functions(Json::arrayValue);
			for (FunctionDefinition const* f : publicFunctions) {
				auto fname = TVMCompilerContext::getFunctionExternalName(f);
				// DBG("#### " << fname << " " << used.count(fname));
				if (used.count(fname)) {
					continue;
				}
				used.insert(fname);
				functions.append(processFunction(fname, f->parameters(), f->returnParameters(), f));
			}

			if (used.count("constructor") == 0) {
				auto v = ptr_vec<VariableDeclaration>();
				functions.append(processFunction("constructor", v, v, nullptr));
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
				if (usedEvents.count(ename)) {
					solAssert(false, "Event name duplication!");
				}
				usedEvents.insert(ename);
				Json::Value cur;
				cur["name"] = ename;
				cur["inputs"] = encodeParams(e->parameters());
				cur["outputs"] = Json::Value(); // TODO is it needed?
				eventAbi.append(cur);
			}
			root["events"] = eventAbi;
		}

		// data
		{
			int shift = 0;
			Json::Value data(Json::arrayValue);
			for (VariableDeclaration const* v : contract->stateVariables()) {
				if (v->isPublic()) {
					Json::Value cur;
					cur["key"] = TvmConst::C4::PersistenceMembersStartIndex + shift++;
					cur["name"] = v->name();
					cur["type"] = getParamTypeString(v->type(), *v);
					data.append(cur);
				}
			}
			root["data"] = data;
		}

//		Json::StreamWriterBuilder builder;
//		const std::string json_file = Json::writeString(builder, root);
//		std::cout << json_file << std::endl;

		cout << "{\n";
		cout << "\t" << R"("ABI version": )" << root["ABI version"] << ",\n";

		if (ctx.pragmaHelper().abiVersion() == 2) {
			cout << "\t" << R"("header": [)";
			for (unsigned i = 0; i < root["header"].size(); ++i) {
				cout << root["header"][i];
				if (i + 1 != root["header"].size()) {
					cout << ", ";
				}
			}
			cout << "],\n";
		}

		cout << "\t" << R"("functions": [)" << "\n";
		print(root["functions"]);
		cout << "\t" << "],\n";

		cout << "\t" << R"("data": [)" << "\n";
		printData(root["data"]);
		cout << "\t" << "],\n";

		cout << "\t" << R"("events": [)" << "\n";
		print(root["events"]);
		cout << "\t" << "]\n";

		cout << "}" << endl;
	}

private:
	static void printData(const Json::Value& json) {
		for (unsigned f = 0; f < json.size(); ++f) {
			const auto &element = json[f];
			cout << "\t\t";

			Json::StreamWriterBuilder builder;
			builder["indentation"] = "";
			std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
			writer->write(element, &std::cout);

			if (f + 1 != json.size())
				cout << ",";
			cout << std::endl;
		}
	}

	static void print(const Json::Value& json) {
		for (unsigned f = 0; f < json.size(); ++f) {
			const auto& function = json[f];
			cout << "\t\t{\n";

			cout << "\t\t\t" << R"("name": )" << function["name"] << ",\n";

			if (function.isMember("id")) {
				cout << "\t\t\t" << R"("id": )" << function["id"] << ",\n";
			}

			cout << "\t\t\t" << R"("inputs": [)" << "\n";
			for (unsigned i = 0; i < function["inputs"].size(); ++i) {
				const auto& input = function["inputs"][i];
				Json::StreamWriterBuilder builder;
				builder["indentation"] = "";
				std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
				cout << "\t\t\t\t";
				writer->write(input, &std::cout);
				if (i + 1 == function["inputs"].size()) {
					cout << "\n";
				} else {
					cout << ",\n";
				}
			}
			cout << "\t\t\t" << "]" << ",\n";

			cout << "\t\t\t" << R"("outputs": [)" << "\n";
			for (unsigned o = 0; o < function["outputs"].size(); ++o) {
				const auto& output = function["outputs"][o];
				Json::StreamWriterBuilder builder;
				builder["indentation"] = "";
				std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
				cout << "\t\t\t\t";
				writer->write(output, &std::cout);
				if (o + 1 == function["outputs"].size()) {
					cout << "\n";
				} else {
					cout << ",\n";
				}
			}
			cout << "\t\t\t" << "]" << "\n";

			if (f + 1 == json.size())
				cout << "\t\t}\n";
			else
				cout << "\t\t},\n";
		}
	}

	static Json::Value processFunction(
		const string& fname,
		const ptr_vec<VariableDeclaration>& params,
		const ptr_vec<VariableDeclaration>& retParams,
		FunctionDefinition const* funcDef = nullptr
	) {
		Json::Value function;
		Json::Value inputs  = encodeParams(params);
		Json::Value outputs = encodeParams(retParams);
		function["name"] = fname;
		if (funcDef && (funcDef->functioID() != 0)) {
			std::ostringstream oss;
			oss << "0x" << std::hex << std::uppercase << funcDef->functioID();
			function["id"] = oss.str();
		}
		function["inputs"] = inputs;
		function["outputs"] = outputs;
		return function;
	}

	static Json::Value encodeParams(const ptr_vec<VariableDeclaration>& params) {
		Json::Value result(Json::arrayValue);
		size_t idx = 0;
		for (const auto& variable: params) {
			string name = variable->name();
			if (name.empty()) name = "value" + toString(idx);
			Json::Value json = setupType(name, getType(variable.get()), *variable);
			result.append(json);
			idx++;
		}
		return result;
	};

	static string getParamTypeString(Type const* type, ASTNode const& node) {
		const Type::Category category = type->category();
		TypeInfo ti(type);
		if (category == Type::Category::Address || category == Type::Category::Contract) {
			return "address";
		} else if (ti.isNumeric) {
			if (to<BoolType>(type)) {
				return "bool";
			}
			if (ti.isSigned) {
				return "int" + toString(ti.numBits);
			} else {
				return "uint" + toString(ti.numBits);
			}
		} else if (auto arrayType = to<ArrayType>(type)) {
			Type const *arrayBaseType = arrayType->baseType();
			if (arrayType->isByteArray()) {
				return "bytes";
			}
			if (isIntegralType(arrayBaseType) || isAddressOrContractType(arrayBaseType) ||
			    to<StructType>(arrayBaseType) || to<ArrayType>(arrayBaseType)) {

				return getParamTypeString(arrayBaseType, node) + "[]";
			}
			cast_error(node, "Unsupported param type " + type->toString(true));
		} else if (to<StructType>(type)) {
			return "tuple";
		} else if (category == Type::Category::TvmCell) {
			return "cell";
		} else if (category == Type::Category::Mapping) {
			auto mapping = to<MappingType>(type);
			return "map(" + getParamTypeString(mapping->keyType(), node) + "," +
					getParamTypeString(mapping->valueType(), node) + ")";
		}
		cast_error(node, "Unsupported param type " + type->toString(true));
    }

	static Json::Value setupType(const string& name, const Type* type, ASTNode const& node) {
		Json::Value json(Json::objectValue);
		json["name"] = name;
		json["type"] = getParamTypeString(type, node);
		switch (type->category()) {
			case Type::Category::Struct:
				json["components"] = setupStructComponents(to<StructType>(type), node);
				break;
			case Type::Category::Array: {
				auto arrayType = to<ArrayType>(type);
				Type const *arrayBaseType = arrayType->baseType();
				if (arrayBaseType->category() == Type::Category::Struct) {
					json["components"] = setupStructComponents(to<StructType>(arrayBaseType), node);
				}
				break;
			}
			default:
				break;
		}
		return json;
	}

	static Json::Value setupStructComponents(const StructType* type, ASTNode const& node) {
		Json::Value components(Json::arrayValue);
		const StructDefinition& structDefinition = type->structDefinition();
		const auto& members = structDefinition.members();
		for (const auto & member : members) {
			components.append(setupType(member->name(), getType(member.get()), node));
		}
		return components;
	}

};

class DecodePosition : private boost::noncopyable {
public:
	enum Algo {JustLoad, NeedLoadNextCell, CheckBits, CheckRefs, Unknown};
	virtual ~DecodePosition() = default;
	virtual Algo updateStateAndGetLoadAlgo(Type const* type) = 0;
};

class DecodePositionAbiV1 : public DecodePosition {
	bool isPositionValid;
	int minRestSliceBits;
	int maxRestSliceBits;
	int minUsedRef;
	int maxUsedRef;
public:

	DecodePositionAbiV1() :
			isPositionValid{true},
			minRestSliceBits{TvmConst::CellBitLength - TvmConst::Message::functionIdLength - TvmConst::Message::timestampLength},
			maxRestSliceBits{TvmConst::CellBitLength - TvmConst::Message::functionIdLength},
			minUsedRef{0},
			maxUsedRef{1}
	{

	}

	Algo updateStateAndGetLoadAlgo(Type const* type) override {
		ABITypeSize size(type);
		solAssert(0 <= size.minRefs && size.minRefs <= 1, "");
		solAssert(0 <= size.maxRefs && size.maxRefs <= 1, "");
		solAssert(0 <= size.minBits && size.minBits <= size.maxBits, "");

		if (!isPositionValid) {
			return Unknown;
		}

		minRestSliceBits -= size.maxBits;
		maxRestSliceBits -= size.minBits;
		minUsedRef += size.minRefs;
		maxUsedRef += size.maxRefs;

		if (minRestSliceBits < 0 && maxRestSliceBits >= 0) {
			isPositionValid = false;
			return Unknown;
		}

		if (maxUsedRef == 4 && maxUsedRef != minUsedRef) {
			isPositionValid = false;
			return Unknown;
		}

		if (minRestSliceBits < 0 || maxUsedRef == 4) {
			minRestSliceBits = TvmConst::CellBitLength - size.maxBits;
			maxRestSliceBits = TvmConst::CellBitLength - size.minBits;
			minUsedRef = size.minRefs;
			maxUsedRef = size.maxRefs;
			return NeedLoadNextCell;
		}

		return JustLoad;
	}
};

class Position {
public:
	Position(int usedBits, int usedRefs) : usedBits{usedBits}, usedRefs{usedRefs} {

	}

	void update(const int bits, const int refs) {
		usedBits += bits;
		usedRefs += refs;
		if ((bits > 0 && usedBits > TvmConst::CellBitLength) || (refs > 0 && usedRefs >= 4)) {
			usedBits = bits;
			usedRefs = refs;
			++idCell;
		}
	}

	void loadRef() {
		++usedRefs;
		if (usedRefs == 4) {
			usedBits = 0;
			usedRefs = 1;
			++idCell;
		}
	}

	int cellNumber() const {
		return idCell;
	}

private:
	int idCell{};
	int usedBits{};
	int usedRefs{};
};

class DecodePositionAbiV2 : public DecodePosition {
	Position minPos;
	Position maxPos;

	std::vector<Type const*> types;
	int curTypeIndex = -1;
	int lastRefType = -1;
public:

	explicit DecodePositionAbiV2(int minBits, int maxBits,
	                             const ptr_vec<VariableDeclaration>& params) :
			minPos{minBits, 0},
			maxPos{maxBits, 0} {
		for (const auto & param : params) {
			initTypes(param.get());
		}
		for (int i = 0; i < static_cast<int>(types.size()); ++i) {
			Type const* t = types[i];
			if ((t->category() == Type::Category::Array && to<ArrayType>(t)->isByteArray()) ||
			    t->category() == Type::Category::TvmCell) {
				lastRefType = i;
			}
		}
	}

	void initTypes(VariableDeclaration const* variable) {
		if (variable->type()->category() == Type::Category::Struct) {
			auto members = to<StructType>(variable->type())->structDefinition().members();
			for (const auto &m : members) {
				initTypes(m.get());
			}
		} else {
			types.push_back(variable->type());
		}
	}

	Algo updateStateAndGetLoadAlgo(Type const* type) override {
		++curTypeIndex;
		solAssert(type->toString() == types[curTypeIndex]->toString(), "");
		ABITypeSize size{type};
		solAssert(0 <= size.minRefs && size.minRefs <= 1, "");
		solAssert(0 <= size.maxRefs && size.maxRefs <= 1, "");
		solAssert(0 <= size.minBits && size.minBits <= size.maxBits, "");

		if (curTypeIndex == lastRefType) {
			minPos.loadRef();
			maxPos.loadRef();
			if (curTypeIndex + 1 == static_cast<int>(types.size())) {
				return JustLoad;
			} else {
				return CheckBits;
			}
		}

		int prevMinCellNumber = minPos.cellNumber();
		int prevMaxCellNumber = maxPos.cellNumber();
		minPos.update(size.minBits, size.minRefs);
		maxPos.update(size.maxBits, size.maxRefs);

		if (prevMinCellNumber == minPos.cellNumber() && minPos.cellNumber() == maxPos.cellNumber()) {
			return JustLoad;
		}

		if (prevMinCellNumber == prevMaxCellNumber &&
		    prevMinCellNumber + 1 == minPos.cellNumber() &&
		    minPos.cellNumber() == maxPos.cellNumber()) {
			return NeedLoadNextCell;
		}

		if ((type->category() == Type::Category::Array && to<ArrayType>(type)->isByteArray()) ||
		    type->category() == Type::Category::TvmCell) {
			return CheckRefs;
		}

		return CheckBits;
	}
};

class DecodeFunctionParams : private boost::noncopyable {
public:
	explicit DecodeFunctionParams(StackPusherHelper *pusher) :
			pusher{pusher} {

	}

private:
	int maxBits() {
		int maxUsed = 1 + 512 + // signature
		              (pusher->ctx().pragmaHelper().havePubkey()? 1 + 256 : 0) +
		              (pusher->ctx().haveTimeInAbiHeader()? 64 : 0) +
		              (pusher->ctx().pragmaHelper().haveExpire()? 32 : 0) +
		              32; // functionID
		return maxUsed;
	}

	int minBits() {
		int minUsed = 1 + // signature
		              (pusher->ctx().pragmaHelper().havePubkey()? 1 : 0) +
		              (pusher->ctx().haveTimeInAbiHeader()? 64 : 0) +
		              (pusher->ctx().pragmaHelper().haveExpire()? 32 : 0) +
		              32; // functionID
		return minUsed;
	}

public:
	void decodeParameters(const ptr_vec<VariableDeclaration>& params) {
		std::unique_ptr<DecodePosition> position;
		switch (pusher->ctx().pragmaHelper().abiVersion()) {
			case 1:
				position = std::make_unique<DecodePositionAbiV1>();
				break;
			case 2: {
				position = std::make_unique<DecodePositionAbiV2>(minBits(), maxBits(), params);
				break;
			}
			default:
				solAssert(false, "");
		}

		pusher->push(+1, "; Decode input parameters"); // locate slice on stack
		for (const auto & variable : params) {
			pusher->push(0, "; Decode " + variable->name());
			auto savedStackSize = pusher->getStack().size();
			decodeParameter(variable.get(), position.get());
			pusher->getStack().ensureSize(savedStackSize + 1, "decodeParameter-2");
		}
		pusher->push(-1, "ENDS"); // only ENDS
		pusher->push(-(int)params.size(), ""); // fix stack
	}

private:

	void loadNextSlice() {
		pusher->push(0, ";; load next cell");
		pusher->push(0, "LDREF");
		pusher->push(0, "ENDS"); // only ENDS
		pusher->push(0, "CTOS");
	}

	void checkBitsAndLoadNextSlice() {
		pusher->pushLines(R"(DUP
SDEMPTY
PUSHCONT {
	LDREF
	ENDS
	CTOS
}
IF
)");
	}

	void checkRefsAndLoadNextSlice() {
		pusher->pushLines(R"(DUP
SREFS
EQINT 1
PUSHCONT {
	LDREF
	ENDS
	CTOS
}
IF
)");
	}

	void loadNextSliceIfNeed(const DecodePosition::Algo algo, VariableDeclaration const* variable, bool isRefType) {
		switch (algo) {
			case DecodePosition::JustLoad:
				break;
			case DecodePosition::NeedLoadNextCell:
				loadNextSlice();
				break;
			case DecodePosition::CheckBits:
				checkBitsAndLoadNextSlice();
				break;
			case DecodePosition::CheckRefs:
				checkRefsAndLoadNextSlice();
				break;
			case DecodePosition::Unknown:
				if (isRefType) {
					cast_error(*variable, "Too much refs types");
				}
				checkBitsAndLoadNextSlice();
				break;
		}
	}

	void loadq(const DecodePosition::Algo algo, const std::string& opcodeq, const std::string& opcode) {
		if (algo == DecodePosition::Algo::JustLoad) {
			pusher->push(+1, opcode);
		} else {
			pusher->push(+1, opcodeq);
			pusher->startContinuation();
			loadNextSlice();
			pusher->push(0, opcode);
			pusher->endContinuation();
			pusher->push(0, "IFNOT");
		}
	}

	void decodeParameter(VariableDeclaration const* variable, DecodePosition* position) {
		auto type = getType(variable);
		const Type::Category category = variable->type()->category();
		if (to<TvmCellType>(type)) {
			pusher->push(0, ";; decode TvmCell");
			loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type), variable, true);
			pusher->push(+1, "LDREF");
		} else if (auto structType = to<StructType>(type)) {
			ASTString const& structName = structType->structDefinition().name();
			pusher->push(0, ";; decode struct " + structName + " " + variable->name());
			std::vector<ASTPointer<VariableDeclaration>> const& members = structType->structDefinition().members();
			for (const ASTPointer<VariableDeclaration> &m : members) {
				pusher->push(0, ";; decode " + structName + "." + m->name());
				decodeParameter(m.get(), position);
			}
			pusher->push(0, ";; build struct " + structName + " ss:" + toString(pusher->getStack().size()));
			// members... slice
			const int memberQty = members.size();
			pusher->blockSwap(memberQty, 1); // slice members...
			pusher->tuple(memberQty); // slice struct
			pusher->push(0, "SWAP"); // ... struct slice
		} else if (category == Type::Category::Address || category == Type::Category::Contract) {
			DecodePosition::Algo algo = position->updateStateAndGetLoadAlgo(type);
			loadq(algo, "LDMSGADDRQ", "LDMSGADDR");
		} else if (isIntegralType(type)) {
			TypeInfo ti{type};
			solAssert(ti.isNumeric, "");
			DecodePosition::Algo algo = position->updateStateAndGetLoadAlgo(type);
			loadq(algo,
			      (ti.isSigned ? "LDIQ " : "LDUQ ") + toString(ti.numBits),
			      (ti.isSigned ? "LDI " : "LDU ") + toString(ti.numBits));
		} else if (auto arrayType = to<ArrayType>(type)) {
			if (arrayType->isByteArray()) {
				loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type), variable, true);
				pusher->push(+1, "LDREF");
			} else {
				loadNextSliceIfNeed(position->updateStateAndGetLoadAlgo(type), variable, false);
				auto baseStructType = to<StructType>(arrayType->baseType());
				if (baseStructType && !StructCompiler::isCompatibleWithSDK(TvmConst::ArrayKeyLength, baseStructType)) {
					cast_error(*variable, "Only arrays of plane little (<= 1023 bits) struct are supported");
				}
				pusher->loadArray();
			}
		} else if (to<MappingType>(type)) {
			DecodePosition::Algo algo = position->updateStateAndGetLoadAlgo(type);
			loadq(algo, "LDDICTQ", "LDDICT");
		} else {
			cast_error(*variable, "Unsupported parameter type for decoding: " + type->toString());
		}
	}

private:
	StackPusherHelper *pusher{};
};

}	// solidity::frontend
