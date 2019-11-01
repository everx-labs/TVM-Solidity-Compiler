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

namespace dev::solidity {
	
class TVMABI {
public:
	static void generateABI(ContractDefinition const* contract, const vector<ContractDefinition const*>& m_allContracts) {

		TVMCompilerContext ctx(contract, m_allContracts);


		std::vector<const FunctionDefinition *> publicFunctions;
		std::vector<const EventDefinition *> events;

		if (auto main_constr = contract->constructor(); main_constr != nullptr)
			publicFunctions.push_back(contract->constructor());
		for (auto c : m_allContracts) {
			for (const auto &_function : c->definedFunctions()) {
				if (_function->isPublic() && !isTvmIntrinsic(_function->name()) && !_function->isConstructor())
					publicFunctions.push_back(_function);
			}
		}

		for (const auto &_event : contract->interfaceEvents())
			events.push_back(_event);

		std::set<std::string> used;

		Json::Value root(Json::objectValue);
		root["ABI version"] = 1;

		{
			Json::Value functions(Json::arrayValue);
			for (auto f : publicFunctions) {
				auto fname = TVMCompilerContext::getFunctionExternalName(f);
				// DBG("#### " << fname << " " << used.count(fname));
				if (used.count(fname)) {
					continue;
				}
				used.insert(fname);
				bool isSigned = false;
				for (const auto &modifier : f->modifiers()) {
					// TODO: is this needed?
					if (modifier->name()->name() == "tvm_signed") {
						isSigned = true;
					}
				}
				functions.append(processFunction(fname, f->parameters(), f->returnParameters(), isSigned));
			}

			if (used.count("constructor") == 0) {
				auto v = ptr_vec<VariableDeclaration>();
				functions.append(processFunction("constructor", v, v, false));
			}

			root["functions"] = functions;
		}

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

		{
			Json::Value stateVariables(Json::arrayValue);
			for (ContractDefinition const *c : contract->annotation().linearizedBaseContracts) {
				for (VariableDeclaration const *variable: c->stateVariables()) {
					if (variable->visibility() == Declaration::Visibility::Public) {
						Json::Value var;
						var["name"] = variable->name();
						try {
							var["type"] = getParamTypeString(variable->type().get());
						} catch (...) {
							cerr << "Warning: " << "Unsupported param type " + variable->type()->toString() << endl;
							continue;
						}
						const int index = ctx.getMemberIdx(variable->name());
						solAssert(index != -1, "");
						var["key"] = index;
						stateVariables.append(var);
					}
				}
			}
			root["data"] = stateVariables;
		}


//		Json::StreamWriterBuilder builder;
//		const std::string json_file = Json::writeString(builder, root);
//		std::cout << json_file << std::endl;

		cout << "{\n";
		cout << "\t" << R"("ABI version": )" << root["ABI version"] << ",\n";

		cout << "\t" << R"("functions": [)" << "\n";
		print(root["functions"]);
		cout << "\t" << "],\n";

		cout << "\t" << R"("events": [)" << "\n";
		print(root["events"]);
		cout << "\t" << "],\n";

		cout << "\t" << R"("data": [)" << "\n";
		for (unsigned i = 0; i < root["data"].size(); ++i) {
			const auto& output = root["data"][i];
			Json::StreamWriterBuilder builder;
			builder["indentation"] = "";
			std::unique_ptr<Json::StreamWriter> writer(builder.newStreamWriter());
			cout << "\t\t";
			writer->write(output, &std::cout);
			if (i + 1 == root["data"].size()) {
				cout << "\n";
			} else {
				cout << ",\n";
			}
		}
		cout << "\t" << "]\n";

		cout << "}" << endl;
	}

private:
	static void print(const Json::Value& json) {
		for (unsigned f = 0; f < json.size(); ++f) {
			const auto& function = json[f];
			cout << "\t\t{\n";

			cout << "\t\t\t" << R"("name": )" << function["name"] << ",\n";

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
		bool isSigned
	) {
		Json::Value function;
		Json::Value inputs  = encodeParams(params);
		Json::Value outputs = encodeParams(retParams);
		function["name"] = fname;
		if (isSigned) {
			function["signed"] = true;
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
			Json::Value json;
			setupType(json, name, getType(variable.get()));
			result.append(json);
			idx++;
		}
		return result;
	};

	static string getParamTypeString(Type const* type) {
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
            Type const *arrayBaseType = arrayType->baseType().get();
			if (arrayType->isByteArray()) {
				return "bytes";
			}
            if (isIntegralType(arrayBaseType)) {
                return getParamTypeString(arrayBaseType) +  "[]";
            }
            // TODO: hack for correct abi compilation. To be fixed later
			if (to<FixedBytesType>(arrayBaseType) || to<StructType>(arrayBaseType)) {
				return "uint8[]";
			}
            solAssert(false, "Unsupported param type (not integral array) " + type->toString());
		} else if (to<StructType>(type)) {
			string sname = getStructName(type);
			if (sname == "TvmCell") 
				return "cell";
			return "tuple";
        }
        solAssert(false, "Unsupported param type " + type->toString());
		return "";
    }
	
	static void setupType(Json::Value& json, const string& name, const Type* type) {
		json["name"] = name;
		json["type"] = getParamTypeString(type);
		if (auto stype = to<StructType>(type)) {
			if (json["type"] == "tuple")
				setupStructComponents(json, stype);
		}
	}

	static void setupStructComponents(Json::Value& json, const StructType* type) {
		solAssert(type, "");
		Json::Value j;
		const StructDefinition& strDef = type->structDefinition();
		const auto& members = strDef.members();
		for (size_t i = 0; i < members.size(); i++) {
			int ii = int(i);
			setupType(j[ii], members[i]->name(), getType(members[i].get()));
		}
		json["components"] = j;
	}

};

}	// dev::solidity
