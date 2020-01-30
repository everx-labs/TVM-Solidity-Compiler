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

class FunctionIdFinder : public ASTConstVisitor {
public:
	bool visit(FunctionCall const& _node) override {
		auto ident = to<Identifier>(&_node.expression());
		if (ident && ident->name() == "tvm_methodID") {
			auto literal = to<Literal>(_node.arguments()[0].get());
			dev::u256 value = literal->annotation().type->literalValue(literal);
			std::ostringstream oss;
			oss << std::hex << std::showbase << value;
			function_id = oss.str();
		}
		return visitNode(_node);
	}

	const std::string& functionId() const { return function_id; }

private:
	std::string function_id;
};

class TVMABI {
public:
	static void generateABI(ContractDefinition const* contract, const vector<ContractDefinition const*>& m_allContracts) {

		TVMCompilerContext ctx(contract, m_allContracts);


		std::vector<const FunctionDefinition *> publicFunctions {};
		std::vector<const EventDefinition *> events {};

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
			for (FunctionDefinition const* f : publicFunctions) {
				auto fname = TVMCompilerContext::getFunctionExternalName(f);
				// DBG("#### " << fname << " " << used.count(fname));
				if (used.count(fname)) {
					continue;
				}
				used.insert(fname);
				functions.append(processFunction(fname, f->parameters(), f->returnParameters(), f->isImplemented()? &f->body() : nullptr));
			}

			if (used.count("constructor") == 0) {
				auto v = ptr_vec<VariableDeclaration>();
				functions.append(processFunction("constructor", v, v, nullptr));
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
		cout << "\t" << "]\n";

		cout << "}" << endl;
	}

private:
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
		Block const* body
	) {
		Json::Value function;
		Json::Value inputs  = encodeParams(params);
		Json::Value outputs = encodeParams(retParams);
		function["name"] = fname;
		if (body != nullptr) {
			FunctionIdFinder functionIdFinder;
			body->accept(functionIdFinder);
			if (!functionIdFinder.functionId().empty()) {
				function["id"] = functionIdFinder.functionId();
			}
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
            Type const *arrayBaseType = arrayType->baseType().get();
			if (arrayType->isByteArray()) {
				return "bytes";
			}
            if (isIntegralType(arrayBaseType) || isAddressType(arrayBaseType) ||
                to<StructType>(arrayBaseType) || to<ArrayType>(arrayBaseType)) {

                return getParamTypeString(arrayBaseType, node) +  "[]";
            }
            cast_error(node, "Unsupported param type " + type->toString(true));
		} else if (to<StructType>(type)) {
			if (isTvmCell(type))
				return "cell";
			return "tuple";
        }
		cast_error(node, "Unsupported param type " + type->toString(true));
    }
	
	static Json::Value setupType(const string& name, const Type* type, ASTNode const& node) {
		Json::Value json(Json::objectValue);
		json["name"] = name;
		json["type"] = getParamTypeString(type, node);
		switch (type->category()) {
			case Type::Category::Struct:
				if (!isTvmCell(type)) {
					json["components"] = setupStructComponents(to<StructType>(type), node);
				}
				break;
			case Type::Category::Array: {
				auto arrayType = to<ArrayType>(type);
				Type const *arrayBaseType = arrayType->baseType().get();
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

}	// dev::solidity
