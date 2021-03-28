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
#include "TVMPusher.hpp"
#include "TVMContractCompiler.hpp"


namespace solidity::frontend {

std::string functionName(FunctionDefinition const *_function) {
	if (_function->isConstructor()) {
		return _function->annotation().contract->name();
	}

	if (_function->isReceive()) {
		return "receive";
	}
	if (_function->isFallback()) {
		return "fallback";
	}
	if (_function->isOnBounce()) {
		return "onBounce";
	}
	return _function->name();
}

bool ends_with(const string &str, const string &suffix) {
	if (suffix.size() > str.size())
		return false;
	return 0 == str.compare(str.size()-suffix.size(), suffix.size(), suffix);
}

void cast_error(const ASTNode &node, const string &error_message) {
	TVMContractCompiler::g_errorReporter->fatalParserError(node.location(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

void cast_warning(const ASTNode &node, const string &error_message) {
	TVMContractCompiler::g_errorReporter->warning(node.location(), error_message);
}

void fatal_error(const string &error_message) {
	TVMContractCompiler::g_errorReporter->error(Error::Type::TypeError, SourceLocation(), error_message);
	BOOST_THROW_EXCEPTION(FatalError()); // never throw, just for [[noreturn]]
}

const ContractDefinition *
getSuperContract(const ContractDefinition *currentContract, const ContractDefinition *mainContract, const string &fname) {
	ContractDefinition const* prev = nullptr;
	for (auto c : getContractsChain(mainContract)) {
		if (c == currentContract)
			break;
		if (getFunction(c, fname))
			prev = c;
	}
	return prev;
}

const Type *getType(const VariableDeclaration *var) {
	return var->annotation().type;
}

bool isAddressOrContractType(const Type *type) {
	return to<AddressType>(type) || to<ContractType>(type);
}

bool isUsualArray(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && !arrayType->isByteArray();
}

bool isByteArrayOrString(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return arrayType && arrayType->isByteArray();
}

bool isString(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isString());
}

bool isSlice(const Type * type) {
	return to<TvmSliceType>(type) != nullptr;
}

int bitsForEnum(size_t val_count) {
	int bytes = 0;
	val_count--;
	while (true) {
		val_count >>= 8;
		++bytes;
		if (val_count == 0) {
			break;
		}
	}
	return 8 * bytes;
}

bool isTvmIntrinsic(const string &name) {
	return boost::starts_with(name, "tvm_");
}

const Type *getType(const Expression *expr) {
	return expr->annotation().type;
}

bool isIntegralType(const Type *type) {
	return TypeInfo(type).isNumeric;
}

bool isStringOrStringLiteralOrBytes(const Type *type) {
	auto arrayType = to<ArrayType>(type);
	return type->category() == Type::Category::StringLiteral || (arrayType && arrayType->isByteArray());
}

bool isRefType(Type const* t) {
	return (t->category() == Type::Category::Array && to<ArrayType>(t)->isByteArray()) ||
			t->category() == Type::Category::TvmCell;
}

std::string typeToDictChar(Type const *keyType) {
	TypeInfo ti(keyType);
	if (ti.isNumeric) {
		return ti.isSigned? "I" : "U";
	} else if (isStringOrStringLiteralOrBytes(keyType) || keyType->category() == Type::Category::TvmCell) {
		return "U";
	}
	return ""; // dict key is slice
}

int lengthOfDictKey(Type const *key) {
	if (isIn(key->category(), Type::Category::Address, Type::Category::Contract)) {
		return AddressInfo::stdAddrLength();
	}

	TypeInfo ti{key};
	if (ti.isNumeric) {
		return ti.numBits;
	}

	if (isStringOrStringLiteralOrBytes(key) || key->category() == Type::Category::TvmCell){
		return 256; // hash of tree of cells
	}

    auto structType = to<StructType>(key);
	if (structType) {
        int bitLength = 0;
        StructDefinition const &structDefinition = structType->structDefinition();
        for (const auto &member : structDefinition.members()) {
            TypeInfo ti2{member->type()};
            solAssert(ti2.isNumeric, "");
            bitLength += ti2.numBits;
        }
        return bitLength;
    }
	solUnimplemented("");
}

IntegerType getKeyTypeOfC4() {
	return IntegerType(TvmConst::C4::KeyLength);
}

IntegerType getKeyTypeOfArray() {
	return IntegerType(TvmConst::ArrayKeyLength);
}

std::tuple<Type const*, Type const*>
dictKeyValue(Type const* type) {
	Type const* keyType{};
	Type const* valueType{};
	if (auto mapType = to<MappingType>(type)) {
		keyType = mapType->keyType();
		valueType = mapType->valueType();
	} else if (auto ccType = to<ExtraCurrencyCollectionType>(type)) {
		keyType = ccType->keyType();
		valueType = ccType->realValueType();
	} else {
		solUnimplemented("");
	}
	return {keyType, valueType};
}

string storeIntegralOrAddress(const Type *type, bool reverse) {
	if (isAddressOrContractType(type))
		return reverse ? "STSLICER" : "STSLICE";
	auto ti = TypeInfo(type);
	if (ti.isNumeric) {
		string cmd = ti.isSigned? "STI" : "STU";
		if (reverse) cmd = cmd + "R";
		solAssert(cmd != "STU 267", "");
		return cmd + " " + toString(ti.numBits);
	}
	solUnimplemented("Unsupported param type " + type->toString());
}

vector<ContractDefinition const *> getContractsChain(ContractDefinition const *contract) {
	vector<FunctionDefinition const*> result;
	auto contracts = contract->annotation().linearizedBaseContracts;
	std::reverse(contracts.begin(), contracts.end());
	return contracts;
}

vector<std::pair<FunctionDefinition const *, ContractDefinition const *>>
getContractFunctionPairs(ContractDefinition const *contract) {
	vector<pair<FunctionDefinition const*, ContractDefinition const*>> result;
	for (ContractDefinition const* c : getContractsChain(contract)) {
		for (const auto f : c->definedFunctions())
			result.emplace_back(f, c);
	}
	return result;
}

const FunctionDefinition *getFunction(ContractDefinition const *contract, const string &functionName) {
	const FunctionDefinition* result = nullptr;
	for (const auto f : contract->definedFunctions()) {
		if (f->name() == functionName)
			return f;
	}
	return result;
}

bool isSuper(Expression const *expr) {
	if (auto identifier = to<Identifier>(expr)) {
		return identifier->name() == "super";
	}
	return false;
}

bool isMacro(const std::string &functionName) {
	return ends_with(functionName, "_macro");
}

bool isAddressThis(const FunctionCall *funCall) {
	if (!funCall)
		return false;
	auto arguments = funCall->arguments();
	if (auto etn = to<ElementaryTypeNameExpression>(&funCall->expression())) {
		if (etn->type().typeName().token() == Token::Address) {
			solAssert(!arguments.empty(), "");
			if (auto arg0 = to<Identifier>(arguments[0].get())) {
				if (arg0->name() == "this")
					return true;
			}
		}
	}
	return false;
}

vector<FunctionDefinition const *> getContractFunctions(ContractDefinition const *contract, const string &funcName) {
	vector<FunctionDefinition const*> result;
	for (auto &[functionDefinition, contractDefinition] : getContractFunctionPairs(contract)) {
		(void)contractDefinition;	// suppress unused variable error
		if (functionDefinition->isConstructor()) {
			continue;
		}
		if (functionName(functionDefinition) == funcName)
			result.push_back(functionDefinition);
	}
	return result;
}

CallableDeclaration const* getFunctionDeclarationOrConstructor(Expression const* expr) {
	auto f = to<FunctionType>(expr->annotation().type);
	if (f) {
		return to<CallableDeclaration>(&f->declaration());
	}
	auto tt = dynamic_cast<const TypeType*>(expr->annotation().type);
	solAssert(tt, "");
	auto contractType = dynamic_cast<const ContractType*>(tt->actualType());
	solAssert(contractType, "");
	return contractType->contractDefinition().constructor(); // null if no constructor
}

bool isEmptyFunction(FunctionDefinition const* f) {
	return f == nullptr || (f->modifiers().empty() && f->body().statements().empty());
}




std::vector<VariableDeclaration const*>
convertArray(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<VariableDeclaration const*>  ret;
	for (const auto& v : arr)
		ret.emplace_back(v.get());
	return ret;
}

std::vector<Type const*>
getTypesFromVarDecls(std::vector<ASTPointer<VariableDeclaration>> const& arr) {
	std::vector<Type const*>  ret;
	for (const auto& v : arr)
		ret.emplace_back(v->type());
	return ret;
}

std::pair<
	std::vector<Type const*>,
	std::vector<std::string>
>
getTupleTypes(TupleType const* tuple) {
	std::vector<std::string> names;
	std::vector<Type const*> types;
	int i = 0;
	for (const TypePointer& comp : tuple->components()) {
		types.emplace_back(comp);
		names.emplace_back(to_string(i));

		++i;
	}
	return {types, names};
}

DictValueType toDictValueType(const Type::Category& category) {
	switch (category) {
		case Type::Category::Address:
			return DictValueType::Address;
		case Type::Category::Array:
			return DictValueType::Array;
		case Type::Category::Bool:
			return DictValueType::Bool;
		case Type::Category::Contract:
			return DictValueType::Contract;
		case Type::Category::Enum:
			return DictValueType::Enum;
		case Type::Category::ExtraCurrencyCollection:
			return DictValueType::ExtraCurrencyCollection;
		case Type::Category::FixedBytes:
			return DictValueType::FixedBytes;
		case Type::Category::Integer:
			return DictValueType::Integer;
		case Type::Category::Mapping:
			return DictValueType::Mapping;
		case Type::Category::Optional:
			return DictValueType::Optional;
		case Type::Category::Struct:
			return DictValueType::Struct;
		case Type::Category::TvmCell:
			return DictValueType::TvmCell;
		case Type::Category::TvmSlice:
			return DictValueType::TvmSlice;
		case Type::Category::VarInteger:
			return DictValueType::VarInteger;
		case Type::Category::Function:
			return DictValueType::Function;
		case Type::Category::FixedPoint:
			return DictValueType::FixedPoint;
		default:
			solUnimplemented("");
	}
}

int integerLog2(int x) {
	return (x != 0)
		   ? static_cast<int>(std::ceil(std::log(x) / std::log(2) + 1e-7))
		   : 1;
}

std::string stringToBytes(const std::string& str) {
	std::string slice;
	for (char index : str) {
		std::stringstream ss;
		ss << std::hex << std::setfill('0') << std::setw(2)
			<< (static_cast<unsigned>(index) & 0xFFu);
		slice += ss.str();
	}
	return slice;
}

} // end namespace solidity::frontend
