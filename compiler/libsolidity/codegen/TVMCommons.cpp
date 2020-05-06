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

std::string ASTNode2String(const ASTNode &node, const string &error_messag, bool isWarning) {
	ErrorList		m_errors;
	ErrorReporter	m_errorReporter(m_errors);
	m_errorReporter.parserError(node.location(), error_messag);
	string message = SourceReferenceFormatter::formatExceptionInformation(
			*(m_errorReporter.errors())[0],
			isWarning ? "Warning" : "Error"
	);
	return message;
}

void cast_error(const ASTNode &node, const string &error_message) {
	cerr << ASTNode2String(node, error_message) << endl;
	std::exit(EXIT_FAILURE);
}

void cast_warning(const ASTNode &node, const string &error_message) {
	cerr << ASTNode2String(node, error_message, true) << endl;
}

void fatal_error(const string &error_message) {
	cerr << error_message << '\n';
	std::exit(EXIT_FAILURE);
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

vector<FunctionDefinition const *> getContractFunctions(ContractDefinition const *contract) {
	vector<FunctionDefinition const*> result;
	for (auto &[functionDefinition, contractDefinition] : getContractFunctionPairs(contract)) {
		(void)contractDefinition;	// suppress unused variable error
		if (functionDefinition->isConstructor())
			continue;
		const std::string funName = functionName(functionDefinition); // for fallback and receive name is empty
		if (isTvmIntrinsic(funName))
			continue;
		// TODO: not needed check?
		if (functionDefinition != getContractFunctions(contract, funName).back())
			continue;
		result.push_back(functionDefinition);
	}
	return result;
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
	return 0 == name.find("tvm_");
}

bool isFunctionForInlining(FunctionDefinition const *f) {
	return ends_with(f->name(), "_inline") || f->isInline() || f->isFallback() || f->isReceive() || f->isOnBounce();
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
	} else if (isStringOrStringLiteralOrBytes(keyType)) {
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

	if (isStringOrStringLiteralOrBytes(key)){
		return 256;
	}

	solAssert(false, "");
}

IntegerType getKeyTypeOfC4() {
	return IntegerType(TvmConst::C4::KeyLength);
}

IntegerType getKeyTypeOfArray() {
	return IntegerType(TvmConst::ArrayKeyLength);
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
	solAssert(false, "Unsupported param type " + type->toString());
}

bool isExpressionExactTypeKnown(Expression const *expr) {
	if (to<Literal>(expr)) return true;
	if (to<Identifier>(expr)) return true;
	if (to<FunctionCall>(expr)) return true;
	if (to<IndexAccess>(expr)) return true;
	if (to<MemberAccess>(expr)) return true;
	return false;
}

bool isNonNegative(Expression const *expr) {
	auto type = getType(expr);
	if (isExpressionExactTypeKnown(expr)) {
		if (auto type2 = to<RationalNumberType>(type)) {
			if (!type2->integerType()->isSigned())
				return true;
		}
		if (auto type2 = to<IntegerType>(type)) {
			if (!type2->isSigned())
				return true;
		}
	}
	if (auto binaryOp = to<BinaryOperation>(expr)) {
		if (isNonNegative(&binaryOp->leftExpression()) && isNonNegative(&binaryOp->rightExpression())) {
			switch (binaryOp->getOperator()) {
				case Token::Add:
				case Token::Mul:
					return true;
				default:
					break;
			}
		}
	}
	return false;
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

bool isAddressThis(const FunctionCall *fcall) {
	if (!fcall)
		return false;
	auto arguments = fcall->arguments();
	if (auto etn = to<ElementaryTypeNameExpression>(&fcall->expression())) {
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

} // end namespace solidity::frontend
