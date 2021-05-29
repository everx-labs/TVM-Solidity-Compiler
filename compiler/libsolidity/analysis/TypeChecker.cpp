/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @author Christian <c@ethdev.com>
 * @date 2015
 * Type analyzer and checker.
 */

#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTUtils.h>
#include <libsolidity/ast/TypeProvider.h>

#include <liblangutil/ErrorReporter.h>

#include <libsolutil/Algorithms.h>
#include <libsolutil/StringUtils.h>

#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <memory>
#include <vector>

#include "../codegen/TVMCommons.hpp"
#include "../codegen/TVMStructCompiler.hpp"
#include "../codegen/TVMConstants.hpp"


using namespace std;
using namespace solidity;
using namespace solidity::util;
using namespace solidity::langutil;
using namespace solidity::frontend;

namespace {
template<class T>
SourceLocation getSmallestCovering(const T &vect) {
	SourceLocation loc;
	for (const auto &x : vect) {
		loc = SourceLocation::smallestCovering(loc, x->location());
	}
	return loc;
}
}

bool TypeChecker::typeSupportedByOldABIEncoder(Type const& _type, bool _isLibraryCall)
{
	if (_isLibraryCall)
		return true;
	if (_type.category() == Type::Category::Struct)
		return false;
	if (_type.category() == Type::Category::Array)
	{
		auto const& arrayType = dynamic_cast<ArrayType const&>(_type);
		auto base = arrayType.baseType();
		if (!typeSupportedByOldABIEncoder(*base, _isLibraryCall) || (base->category() == Type::Category::Array && base->isDynamicallySized()))
			return false;
	}
	return true;
}

bool TypeChecker::checkTypeRequirements(ASTNode const& _contract)
{
	_contract.accept(*this);
	return Error::containsOnlyWarnings(m_errorReporter.errors());
}

TypePointer const& TypeChecker::type(Expression const& _expression) const
{
	solAssert(!!_expression.annotation().type, "Type requested but not present.");
	return _expression.annotation().type;
}

TypePointer const& TypeChecker::type(VariableDeclaration const& _variable) const
{
	solAssert(!!_variable.annotation().type, "Type requested but not present.");
	return _variable.annotation().type;
}

bool TypeChecker::visit(ContractDefinition const& _contract)
{
	m_scope = &_contract;

	ASTNode::listAccept(_contract.baseContracts(), *this);

	for (auto const& n: _contract.subNodes())
		n->accept(*this);

	return false;
}

void TypeChecker::checkDoubleStorageAssignment(Assignment const& _assignment)
{
	TupleType const& lhs = dynamic_cast<TupleType const&>(*type(_assignment.leftHandSide()));
	TupleType const& rhs = dynamic_cast<TupleType const&>(*type(_assignment.rightHandSide()));

	if (lhs.components().size() != rhs.components().size())
	{
		solAssert(m_errorReporter.hasErrors(), "");
		return;
	}
}

TypePointers TypeChecker::getReturnTypesForTVMConfig(FunctionCall const& _functionCall)
{
	vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 1)
		m_errorReporter.typeError(
			_functionCall.location(),
			"This function takes one argument, but " +
			toString(arguments.size()) +
			" were provided."
		);

	set<std::string> availableParams {"1","15","17","34"};
	auto paramNumberLiteral = dynamic_cast<const Literal *>(arguments[0].get());

	if (!paramNumberLiteral)
		m_errorReporter.typeError(
			_functionCall.location(),
			"This function takes only param number literal."
		);

	Type const* type = paramNumberLiteral->annotation().type;
	u256 value = type->literalValue(paramNumberLiteral);
	std::string paramNumber = value.str();

	if (!availableParams.count(paramNumber))
		m_errorReporter.typeError(
			_functionCall.location(),
			"Wrong config param number. Available numbers are: 1, 15, 17, 34."
		);
//	function tvm_config_param1() pure private returns (uint256, bool) { }
	if (paramNumber == "1")
		return TypePointers{TypeProvider::uint256(), TypeProvider::boolean()};

//	function tvm_config_param15() pure private returns (uint32, uint32, uint32, uint32, bool) { }
//	function tvm_config_param17() pure private returns (uint32, uint32, uint32, uint32, bool) { }
	if (paramNumber == "15" || paramNumber == "17")
		return TypePointers{TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
								  TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
								  TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
								  TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
								  TypeProvider::boolean()};

//    function tvm_config_param34() private pure returns (
//        uint8 /*constructor_id*/,
//        uint32 /*utime_since*/,
//        uint32 /*utime_until*/,
//        uint16 /*total*/,
//        uint16 /*main*/,
//        uint64 /*total_weight*/,
//        mapping(uint16 => TvmSlice) memory,
//        bool ok
//    ) { }
	if (paramNumber == "34") {
		auto ret = TypePointers{TypeProvider::integer(8, IntegerType::Modifier::Unsigned),
									  TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
									  TypeProvider::integer(32, IntegerType::Modifier::Unsigned),
									  TypeProvider::integer(16, IntegerType::Modifier::Unsigned),
									  TypeProvider::integer(16, IntegerType::Modifier::Unsigned),
									  TypeProvider::integer(64, IntegerType::Modifier::Unsigned),
									  TypeProvider::mapping(TypeProvider::integer(16, IntegerType::Modifier::Unsigned),
																TypeProvider::tvmslice()),
									  TypeProvider::boolean()};
		return ret;
	}
	solAssert(false, "Unsupported tvm.configParam argument.");
}

TypePointers TypeChecker::typeCheckABIDecodeAndRetrieveReturnType(FunctionCall const& _functionCall, bool _abiEncoderV2)
{
	vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 2)
		m_errorReporter.typeError(
			_functionCall.location(),
			"This function takes two arguments, but " +
			toString(arguments.size()) +
			" were provided."
		);

	if (arguments.size() >= 1)
		if (
			!type(*arguments.front())->isImplicitlyConvertibleTo(*TypeProvider::bytesMemory()) &&
			!type(*arguments.front())->isImplicitlyConvertibleTo(*TypeProvider::bytesCalldata())
		)
			m_errorReporter.typeError(
				arguments.front()->location(),
				"The first argument to \"abi.decode\" must be implicitly convertible to "
				"bytes memory or bytes calldata, but is of type " +
				type(*arguments.front())->toString() +
				"."
			);

	if (arguments.size() < 2)
		return {};

	// The following is a rather syntactic restriction, but we check it here anyway:
	// The second argument has to be a tuple expression containing type names.
	TupleExpression const* tupleExpression = dynamic_cast<TupleExpression const*>(arguments[1].get());
	if (!tupleExpression)
	{
		m_errorReporter.typeError(
			arguments[1]->location(),
			"The second argument to \"abi.decode\" has to be a tuple of types."
		);
		return {};
	}

	TypePointers components;
	for (auto const& typeArgument: tupleExpression->components())
	{
		solAssert(typeArgument, "");
		if (TypeType const* argTypeType = dynamic_cast<TypeType const*>(type(*typeArgument)))
		{
			TypePointer actualType = argTypeType->actualType();
			solAssert(actualType, "");
			// We force memory because the parser currently cannot handle
			// data locations. Furthermore, storage can be a little dangerous and
			// calldata is not really implemented anyway.
			actualType = TypeProvider::withLocationIfReference(actualType);
			if (!actualType->fullEncodingType(false, _abiEncoderV2, false))
				m_errorReporter.typeError(
					typeArgument->location(),
					"Decoding type " + actualType->toString(false) + " not supported."
				);
			components.push_back(actualType);
		}
		else
		{
			m_errorReporter.typeError(typeArgument->location(), "Argument has to be a type name.");
			components.push_back(TypeProvider::emptyTuple());
		}
	}
	return components;
}

void TypeChecker::typeCheckTVMBuildStateInit(
	FunctionCall const& _functionCall,
	const std::function<bool(const std::string&)>& hasName,
	const std::function<int(const std::string&)>& findName
) {
	bool hasNames = !_functionCall.names().empty();
	const vector<ASTPointer<const Expression>> &args = _functionCall.arguments();
	size_t argCnt = args.size();
	if (!hasNames && argCnt != 3 && argCnt != 2)
		m_errorReporter.typeError(
				_functionCall.location(),
				string("If parameters are set without names, only 2 or 3 arguments can be specified.")
		);

	if (hasNames) {
		bool hasCode = hasName("code");
		bool hasData = hasName("data");
		bool hasVarInit = hasName("varInit");
		bool hasPubkey = hasName("pubkey");
		bool hasContr = hasName("contr");

		if (!hasCode) {
			m_errorReporter.typeError(
					_functionCall.location(),
					string("Parameter \"code\" must be set.")
			);
		}
		if (hasData && (hasVarInit || hasPubkey)) {
			m_errorReporter.typeError(
					_functionCall.location(),
					string(R"(Parameter "data" can't be specified with "pubkey" or "varInit".)")
			);
		}
		if (hasVarInit != hasContr) {
			m_errorReporter.typeError(
					_functionCall.location(),
					string(R"(Parameter "varInit" requires parameter "contr" and there is no need in "contr" without "varInit".)")
			);
		}

		if (hasContr) {
			int contrInd = findName("contr");
			const ASTPointer<Expression const>& contrArg = args.at(contrInd);
			ContractType const* ct = getContractType(contrArg.get());
			if (ct == nullptr) {
				m_errorReporter.typeError(
						contrArg->location(),
						"Expected contract type."
				);
				return;
			}
			int varInitInd = findName("varInit");
			if (varInitInd == -1) {
				return;
			}
			auto list = dynamic_cast<InitializerList const *>(args.at(varInitInd).get());
			if (!list) {
				// It's checked in typeCheckFunctionCall function
				return;
			}
			checkInitList(list, ct);
		}
	}
}

void TypeChecker::typeCheckCallBack(FunctionType const* remoteFunction, Expression const& option) {
	auto calleeDefinition = dynamic_cast<FunctionDefinition const*>(&remoteFunction->declaration());
	if (calleeDefinition == nullptr) {
		m_errorReporter.typeError(
				option.location(),
				R"("callback" option can be used only for function type.)"
		);
	} else if (remoteFunction->returnParameterTypes().empty() || !calleeDefinition->isResponsible()) {
		m_errorReporter.typeError(
				option.location(),
				SecondarySourceLocation().append("Declaration of callee function", calleeDefinition->location()),
				R"("callback" option can be used only for responsible functions.)"
		);
	} else {
		FunctionDefinition const *callbackFunc = getFunctionDefinition(&option);
		if (callbackFunc == nullptr) {
			m_errorReporter.typeError(
					option.location(),
					"Expected function type but got " + option.annotation().type->toString() + "."
			);
		} else if (!callbackFunc->returnParameters().empty()) {
			m_errorReporter.typeError(
					option.location(),
					SecondarySourceLocation()
							.append("Declaration of the callback function:", callbackFunc->location()),
					R"(Callback function must return nothing.)"
			);
		} else {
			ContractDefinition const *funContract = callbackFunc->annotation().contract;
			if (!m_scope->derivesFrom(*funContract)) {
				m_errorReporter.typeError(
						option.location(),
						"Callback function should belong to this contract or any of base contracts."
				);
			} else {
				const ParameterList &callbackParams = callbackFunc->parameterList();
				const TypePointers& retTypes = remoteFunction->returnParameterTypes(); // check function without return
				if (callbackParams.parameters().size() != retTypes.size()) {
					m_errorReporter.typeError(
								option.location(),
							SecondarySourceLocation()
									.append("Declaration of the callee function:", calleeDefinition->location())
									.append("Declaration of the callback function:", callbackFunc->location()),
							R"(Count of return parameters of the callee function isn't equal to count of input arguments of the callback function.)"
					);
				} else {
					for (std::size_t p = 0; p < callbackParams.parameters().size(); ++p) {
						if (*callbackParams.parameters().at(p)->type() != *retTypes.at(p)) {
							m_errorReporter.typeError(
									option.location(),
									SecondarySourceLocation()
											.append("Parameter of the callee function:", calleeDefinition->returnParameters().at(p)->location())
											.append("Parameter of the callback function:", callbackFunc->parameters().at(p)->location()),
									toString(p + 1) + R"(th parameters of callback and callee functions have different types.)"
							);
						}
					}
				}
			}
		}
	}
}

TypePointers TypeChecker::checkSliceDecode(FunctionCall const& _functionCall)
{
	vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.empty())
		m_errorReporter.typeError(
			_functionCall.location(),
			"This function takes positive number of arguments."
		);

	TypePointers components;
	for (auto const& typeArgument: arguments) {
		solAssert(typeArgument, "");
		if (auto const* argTypeType = dynamic_cast<TypeType const*>(type(*typeArgument))) {
			TypePointer actualType = argTypeType->actualType();
			solAssert(actualType, "");
			components.push_back(actualType);

			switch (actualType->category()) {
				case Type::Category::Integer:
				case Type::Category::FixedBytes:
				case Type::Category::Bool:
				case Type::Category::FixedPoint:
				case Type::Category::Address:
				case Type::Category::Contract:
				case Type::Category::TvmCell:
				case Type::Category::Array:
				case Type::Category::Mapping:
				case Type::Category::ExtraCurrencyCollection:
				case Type::Category::Optional:
				case Type::Category::Struct:
					break;

				default:
					m_errorReporter.typeError(typeArgument->location(), "Unsupported type for decoding.");
			}
		} else {
			m_errorReporter.typeError(typeArgument->location(), "Argument has to be a type name.");
			components.push_back(TypeProvider::emptyTuple());
		}
	}
	return components;
}

TypePointers TypeChecker::typeCheckMetaTypeFunctionAndRetrieveReturnType(FunctionCall const& _functionCall)
{
	vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 1)
	{
		m_errorReporter.typeError(
			_functionCall.location(),
			"This function takes one argument, but " +
			toString(arguments.size()) +
			" were provided."
		);
		return {};
	}
	TypePointer firstArgType = type(*arguments.front());
	if (
		firstArgType->category() != Type::Category::TypeType ||
		dynamic_cast<TypeType const&>(*firstArgType).actualType()->category() != TypeType::Category::Contract
	)
	{
		m_errorReporter.typeError(
			arguments.front()->location(),
			"Invalid type for argument in function call. "
			"Contract type required, but " +
			type(*arguments.front())->toString(true) +
			" provided."
		);
		return {};
	}

	return {TypeProvider::meta(dynamic_cast<TypeType const&>(*firstArgType).actualType())};
}

void TypeChecker::endVisit(InheritanceSpecifier const& _inheritance)
{
	auto base = dynamic_cast<ContractDefinition const*>(&dereference(_inheritance.name()));
	solAssert(base, "Base contract not available.");

	if (m_scope->isInterface() && !base->isInterface())
		m_errorReporter.typeError(_inheritance.location(), "Interfaces can only inherit from other interfaces.");

	if (base->isLibrary())
		m_errorReporter.typeError(_inheritance.location(), "Libraries cannot be inherited from.");

	auto const& arguments = _inheritance.arguments();
	TypePointers parameterTypes;
	if (!base->isInterface())
		// Interfaces do not have constructors, so there are zero parameters.
		parameterTypes = ContractType(*base).newExpressionType()->parameterTypes();

	if (arguments)
	{
		if (parameterTypes.size() != arguments->size())
		{
			m_errorReporter.typeError(
				_inheritance.location(),
				"Wrong argument count for constructor call: " +
				toString(arguments->size()) +
				" arguments given but expected " +
				toString(parameterTypes.size()) +
				". Remove parentheses if you do not want to provide arguments here."
			);
		}
		for (size_t i = 0; i < std::min(arguments->size(), parameterTypes.size()); ++i)
		{
			BoolResult result = type(*(*arguments)[i])->isImplicitlyConvertibleTo(*parameterTypes[i]);
			if (!result)
				m_errorReporter.typeErrorConcatenateDescriptions(
					(*arguments)[i]->location(),
					"Invalid type for argument in constructor call. "
					"Invalid implicit conversion from " +
					type(*(*arguments)[i])->toString() +
					" to " +
					parameterTypes[i]->toString() +
					" requested.",
					result.message()
				);
		}
	}
}

void TypeChecker::endVisit(UsingForDirective const& _usingFor)
{
	ContractDefinition const* library = dynamic_cast<ContractDefinition const*>(
		_usingFor.libraryName().annotation().referencedDeclaration
	);
	if (!library || !library->isLibrary())
		m_errorReporter.fatalTypeError(_usingFor.libraryName().location(), "Library name expected.");
}

bool TypeChecker::visit(StructDefinition const& _struct)
{
	for (ASTPointer<VariableDeclaration> const& member: _struct.members())
		solAssert(type(*member)->canBeStored(), "Type cannot be used in struct.");

	// Check recursion, fatal error if detected.
	auto visitor = [&](StructDefinition const& _struct, CycleDetector<StructDefinition>& _cycleDetector, size_t _depth)
	{
		if (_depth >= 256)
			m_errorReporter.fatalDeclarationError(_struct.location(), "Struct definition exhausting cyclic dependency validator.");

		for (ASTPointer<VariableDeclaration> const& member: _struct.members())
		{
			Type const* memberType = type(*member);
			while (auto arrayType = dynamic_cast<ArrayType const*>(memberType))
			{
				if (arrayType->isDynamicallySized())
					break;
				memberType = arrayType->baseType();
			}
			if (auto structType = dynamic_cast<StructType const*>(memberType))
				if (_cycleDetector.run(structType->structDefinition()))
					return;
		}
	};
	if (CycleDetector<StructDefinition>(visitor).run(_struct) != nullptr)
		m_errorReporter.fatalTypeError(_struct.location(), "Recursive struct definition.");

	ASTNode::listAccept(_struct.members(), *this);
	m_structs.emplace(_struct.name(), TypeProvider::structType(_struct));
	return false;
}

bool TypeChecker::checkAbiType(
	VariableDeclaration const* origVar,
	Type const* curType,
	VariableDeclaration const* curVar,
	std::set<StructDefinition const*>& usedStructs
) {
	auto printError = [&](const std::string& message){
		if (origVar == curVar) {
			m_errorReporter.typeError(origVar->location(), message);
		} else {
			m_errorReporter.typeError(
				origVar->location(),
				SecondarySourceLocation().append("Another declaration is here:", curVar->location()),
				message
			);
		}
	};

	const Type::Category category = curType->category();
	switch (category) {
		case Type::Category::Mapping: {
			auto mappingType = to<MappingType>(curType);
			auto intKey = to<IntegerType>(mappingType->keyType());
			auto addrKey = to<AddressType>(mappingType->keyType());
			if (intKey == nullptr && addrKey == nullptr) {
				printError(
					"Key type of the mapping must be "
			   		"any of int<M>/uint<M> types with M from 8 to 256 or std address.");
				return true;
			}
			if (checkAbiType(origVar, mappingType->valueType(), curVar, usedStructs)) {
				return true;
			}
			break;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(curType);
			if (!arrayType->isByteArray()) {
				checkAbiType(origVar, arrayType->baseType(), curVar, usedStructs);
			}
			break;
		}
		case Type::Category::Struct: {
			auto valueStruct = to<StructType>(curType);
			StructDefinition const& structDefinition = valueStruct->structDefinition();
			if (usedStructs.count(&structDefinition)) {
				m_errorReporter.typeError(
					origVar->location(),
					SecondarySourceLocation().append("Recursive struct:", structDefinition.location()),
					"ABI doesn't support recursive types."
				);
				return true;
			}
			usedStructs.insert(&structDefinition);
			for (const ASTPointer<VariableDeclaration>& member : structDefinition.members()) {
				if (checkAbiType(origVar, member->type(), member.get(), usedStructs))
					return true;
			}
			usedStructs.erase(&structDefinition);
			break;
		}

		case Type::Category::Address:
		case Type::Category::Bool:
		case Type::Category::Contract:
		case Type::Category::Enum:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::TvmCell:
			break;

		default: {
			printError("ABI doesn't support " + curType->toString() + " type.");
			return true;
		}
	}
	return false;
}

bool TypeChecker::visit(FunctionDefinition const& _function)
{
	m_currentFunction = &_function;
	bool isLibraryFunction = _function.inContractKind() == ContractKind::Library;

	if (_function.isResponsible()) {
		if (_function.returnParameters().empty())
			m_errorReporter.typeError(_function.location(), "Responsible function must return at least one value.");
		if (!_function.isPublic())
			m_errorReporter.typeError(_function.location(), "Responsible function must have public or external visibility.");
	}

	if (_function.markedVirtual())
	{
		if (_function.annotation().contract->isInterface())
			m_errorReporter.warning(_function.location(), "Interface functions are implicitly \"virtual\"");
		if (_function.visibility() == Visibility::Private)
			m_errorReporter.typeError(_function.location(), "\"virtual\" and \"private\" cannot be used together.");
	}

	if (_function.isPublic() && !_function.isOnBounce())
	{
		for (const auto& params : {_function.parameters(), _function.returnParameters()}) {
			for (ASTPointer<VariableDeclaration> const &var : params) {
				std::set<StructDefinition const *> usedStructs;
				checkAbiType(var.get(), var->type(), var.get(), usedStructs);
				var->accept(*this);
			}
		}
	}
	set<Declaration const*> modifiers;
	for (ASTPointer<ModifierInvocation> const& modifier: _function.modifiers())
	{
		auto baseContracts = dynamic_cast<ContractDefinition const&>(*_function.scope()).annotation().linearizedBaseContracts;
		// Delete first base which is just the main contract itself
		baseContracts.erase(baseContracts.begin());

		visitManually(
			*modifier,
			_function.isConstructor() ? baseContracts : vector<ContractDefinition const*>()
		);
		Declaration const* decl = &dereference(*modifier->name());
		if (modifiers.count(decl))
		{
			if (dynamic_cast<ContractDefinition const*>(decl))
				m_errorReporter.declarationError(modifier->location(), "Base constructor already provided.");
		}
		else
			modifiers.insert(decl);
	}
	if (m_scope->isInterface())
	{
		if (_function.isImplemented())
			m_errorReporter.typeError(_function.location(), "Functions in interfaces cannot have an implementation.");

		if (_function.visibility() != Visibility::External)
			m_errorReporter.typeError(_function.location(), "Functions in interfaces must be declared external.");

		if (_function.isConstructor())
			m_errorReporter.typeError(_function.location(), "Constructor cannot be defined in interfaces.");
	}
	else if (m_scope->contractKind() == ContractKind::Library)
		if (_function.isConstructor())
			m_errorReporter.typeError(_function.location(), "Constructor cannot be defined in libraries.");
	if (_function.isImplemented())
		_function.body().accept(*this);
	else if (_function.isConstructor())
		m_errorReporter.typeError(_function.location(), "Constructor must be implemented if declared.");
	else if (isLibraryFunction)
		m_errorReporter.typeError(_function.location(), "Library functions must be implemented if declared.");
	else if (!_function.virtualSemantics())
		m_errorReporter.typeError(_function.location(), "Functions without implementation must be marked virtual.");


	if (_function.isFallback())
		typeCheckFallbackFunction(_function);
	else if (_function.isReceive())
		typeCheckReceiveFunction(_function);
	else if (_function.isConstructor())
		typeCheckConstructor(_function);
	else if (_function.isOnBounce())
		typeCheckOnBounce(_function);
	else if (_function.isOnTickTock())
		typeCheckOnTickTock(_function);

	return false;
}

void TypeChecker::endVisit(FunctionDefinition const& )
{
	m_currentFunction = nullptr;
}

bool TypeChecker::visit(VariableDeclaration const& _variable)
{
	if (_variable.typeName())
		_variable.typeName()->accept(*this);

	// type is filled either by ReferencesResolver directly from the type name or by
	// TypeChecker at the VariableDeclarationStatement level.
	TypePointer varType = _variable.annotation().type;
	solAssert(!!varType, "Variable type not provided.");

	if (auto contractType = dynamic_cast<ContractType const*>(varType))
		if (contractType->contractDefinition().isLibrary())
			m_errorReporter.typeError(_variable.location(), "The type of a variable cannot be a library.");
	if (_variable.value())
		expectType(*_variable.value(), *varType);
	if (_variable.isConstant())
	{
		if (!_variable.type()->isValueType())
		{
			bool allowed = false;
			if (auto arrayType = dynamic_cast<ArrayType const*>(_variable.type()))
				allowed = arrayType->isByteArray();
			if (!allowed)
				m_errorReporter.typeError(_variable.location(), "Constants of non-value type not yet implemented.");
		}

		if (!_variable.value())
			m_errorReporter.typeError(_variable.location(), "Uninitialized \"constant\" variable.");
		else if (!_variable.value()->annotation().isPure)
			m_errorReporter.typeError(
				_variable.value()->location(),
				"Initial value for constant variable has to be compile-time constant."
			);
	}
	if (!_variable.isStateVariable())
	{
	}
	else if (_variable.visibility() >= Visibility::Public)
	{
		FunctionType getter(_variable);
		if (!_variable.sourceUnit().annotation().experimentalFeatures.count(ExperimentalFeature::ABIEncoderV2))
		{
			vector<string> unsupportedTypes;
			for (auto const& param: getter.parameterTypes() + getter.returnParameterTypes())
				if (!typeSupportedByOldABIEncoder(*param, false /* isLibrary */))
					unsupportedTypes.emplace_back(param->toString());
			if (!unsupportedTypes.empty())
				m_errorReporter.typeError(_variable.location(),
					"The following types are only supported for getters in ABIEncoderV2: " +
					joinHumanReadable(unsupportedTypes) +
					". Either remove \"public\" or use \"pragma experimental ABIEncoderV2;\" to enable the feature."
				);
		}
		std::set<StructDefinition const*> usedStructs;
		checkAbiType(&_variable, _variable.type(), &_variable, usedStructs);
	}

	switch (varType->category())
	{
	case Type::Category::Array:
		break;
	case Type::Category::Mapping:
	{
		auto mapType = dynamic_cast<MappingType const*>(varType);
		switch (mapType->keyType()->category()) {
			case Type::Category::Address:
			case Type::Category::Array: // usual arrays (e.g. uint[]) are checked in another place
			case Type::Category::Bool:
			case Type::Category::Contract:
			case Type::Category::Enum:
			case Type::Category::FixedBytes:
			case Type::Category::Integer:
			case Type::Category::Struct: // length of struct is checked in another place
			case Type::Category::TvmCell:
			case Type::Category::FixedPoint:
				break;
			default:
				m_errorReporter.typeError(
					_variable.location(),
				  	"Type " + mapType->keyType()->toString() + " can't be used as mapping key. "
				  	"Allowed types: address, bytes, string, bool, contract, enum, fixed bytes, fixed-point number, integer and struct.");
				break;
		}
		break;
	}
	default:
		break;
	}

	if (_variable.isStatic() && _variable.value() != nullptr) {
		m_errorReporter.syntaxError(
			_variable.value()->location(),
			"Static variables can be initialized only during contract deployment.");
	}

	if (_variable.type()->category() == Type::Category::Function) {
		auto ft = dynamic_cast<FunctionType const*>(_variable.type());
		if (ft->kind() == FunctionType::Kind::External) {
			m_errorReporter.fatalTypeError(
				_variable.location(),
				"External functions are not supported yet.");
		}
	}

	return false;
}

void TypeChecker::visitManually(
	ModifierInvocation const& _modifier,
	vector<ContractDefinition const*> const& _bases
)
{
	std::vector<ASTPointer<Expression>> const& arguments =
		_modifier.arguments() ? *_modifier.arguments() : std::vector<ASTPointer<Expression>>();
	for (ASTPointer<Expression> const& argument: arguments)
		argument->accept(*this);

	_modifier.name()->accept(*this);

	auto const* declaration = &dereference(*_modifier.name());
	vector<ASTPointer<VariableDeclaration>> emptyParameterList;
	vector<ASTPointer<VariableDeclaration>> const* parameters = nullptr;
	if (auto modifierDecl = dynamic_cast<ModifierDefinition const*>(declaration))
		parameters = &modifierDecl->parameters();
	else
		// check parameters for Base constructors
		for (ContractDefinition const* base: _bases)
			if (declaration == base)
			{
				if (auto referencedConstructor = base->constructor())
					parameters = &referencedConstructor->parameters();
				else
					parameters = &emptyParameterList;
				break;
			}
	if (!parameters)
	{
		m_errorReporter.typeError(_modifier.location(), "Referenced declaration is neither modifier nor base class.");
		return;
	}
	if (parameters->size() != arguments.size())
	{
		m_errorReporter.typeError(
			_modifier.location(),
			"Wrong argument count for modifier invocation: " +
			toString(arguments.size()) +
			" arguments given but expected " +
			toString(parameters->size()) +
			"."
		);
		return;
	}
	for (size_t i = 0; i < arguments.size(); ++i)
	{
		BoolResult result = type(*arguments[i])->isImplicitlyConvertibleTo(*type(*(*parameters)[i]));
		if (!result)
			m_errorReporter.typeErrorConcatenateDescriptions(
				arguments[i]->location(),
				"Invalid type for argument in modifier invocation. "
				"Invalid implicit conversion from " +
				type(*arguments[i])->toString() +
				" to " +
				type(*(*parameters)[i])->toString() +
				" requested.",
				result.message()
			);
	}
}

bool TypeChecker::visit(EventDefinition const& _eventDef)
{
	solAssert(_eventDef.visibility() > Visibility::Internal, "");
	unsigned numIndexed = 0;
	for (ASTPointer<VariableDeclaration> const& var: _eventDef.parameters())
	{
		if (var->isIndexed())
			numIndexed++;
		if (!type(*var)->canLiveOutsideStorage())
			m_errorReporter.typeError(var->location(), "Type is required to live outside storage.");
		if (!type(*var)->interfaceType(false))
			m_errorReporter.typeError(var->location(), "Internal or recursive type is not allowed as event parameter type.");
//		if (
//			!_eventDef.sourceUnit().annotation().experimentalFeatures.count(ExperimentalFeature::ABIEncoderV2) &&
//			!typeSupportedByOldABIEncoder(*type(*var), false /* isLibrary */)
//		)
//			m_errorReporter.typeError(
//				var->location(),
//				"This type is only supported in ABIEncoderV2. "
//				"Use \"pragma experimental ABIEncoderV2;\" to enable the feature."
//			);
	}
	if (_eventDef.isAnonymous() && numIndexed > 4)
		m_errorReporter.typeError(_eventDef.location(), "More than 4 indexed arguments for anonymous event.");
	else if (!_eventDef.isAnonymous() && numIndexed > 3)
		m_errorReporter.typeError(_eventDef.location(), "More than 3 indexed arguments for event.");
	return false;
}

void TypeChecker::endVisit(FunctionTypeName const& _funType)
{
	FunctionType const& fun = dynamic_cast<FunctionType const&>(*_funType.annotation().type);
	if (fun.kind() == FunctionType::Kind::External)
		solAssert(fun.interfaceType(false), "External function type uses internal types.");
}

bool TypeChecker::visit(InlineAssembly const& /*_inlineAssembly*/)
{
	return true;
}

bool TypeChecker::visit(IfStatement const& _ifStatement)
{
	expectType(_ifStatement.condition(), *TypeProvider::boolean());
	_ifStatement.trueStatement().accept(*this);
	if (_ifStatement.falseStatement())
		_ifStatement.falseStatement()->accept(*this);
	return false;
}

void TypeChecker::endVisit(TryStatement const& _tryStatement)
{
	m_errorReporter.fatalTypeError(
		_tryStatement.location(),
		"Try-catch statement is not supported yet."
	);

	FunctionCall const* externalCall = dynamic_cast<FunctionCall const*>(&_tryStatement.externalCall());
	if (!externalCall || externalCall->annotation().kind != FunctionCallKind::FunctionCall)
	{
		m_errorReporter.typeError(
			_tryStatement.externalCall().location(),
			"Try can only be used with external function calls and contract creation calls."
		);
		return;
	}

	FunctionType const& functionType = dynamic_cast<FunctionType const&>(*externalCall->expression().annotation().type);
	if (
		functionType.kind() != FunctionType::Kind::External &&
		functionType.kind() != FunctionType::Kind::Creation &&
		functionType.kind() != FunctionType::Kind::DelegateCall
	)
	{
		m_errorReporter.typeError(
			_tryStatement.externalCall().location(),
			"Try can only be used with external function calls and contract creation calls."
		);
		return;
	}

	externalCall->annotation().tryCall = true;

	solAssert(_tryStatement.clauses().size() >= 2, "");
	solAssert(_tryStatement.clauses().front(), "");

	TryCatchClause const& successClause = *_tryStatement.clauses().front();
	if (successClause.parameters())
	{
		TypePointers returnTypes =
			m_evmVersion.supportsReturndata() ?
			functionType.returnParameterTypes() :
			functionType.returnParameterTypesWithoutDynamicTypes();
		std::vector<ASTPointer<VariableDeclaration>> const& parameters =
			successClause.parameters()->parameters();
		if (returnTypes.size() != parameters.size())
			m_errorReporter.typeError(
				successClause.location(),
				"Function returns " +
				to_string(functionType.returnParameterTypes().size()) +
				" values, but returns clause has " +
				to_string(parameters.size()) +
				" variables."
			);
		size_t len = min(returnTypes.size(), parameters.size());
		for (size_t i = 0; i < len; ++i)
		{
			solAssert(returnTypes[i], "");
			if (parameters[i] && *parameters[i]->annotation().type != *returnTypes[i])
				m_errorReporter.typeError(
					parameters[i]->location(),
					"Invalid type, expected " +
					returnTypes[i]->toString(false) +
					" but got " +
					parameters[i]->annotation().type->toString() +
					"."
				);
		}
	}

	TryCatchClause const* errorClause = nullptr;
	TryCatchClause const* lowLevelClause = nullptr;
	for (size_t i = 1; i < _tryStatement.clauses().size(); ++i)
	{
		TryCatchClause const& clause = *_tryStatement.clauses()[i];
		if (clause.errorName() == "")
		{
			if (lowLevelClause)
				m_errorReporter.typeError(
					clause.location(),
					SecondarySourceLocation{}.append("The first clause is here:", lowLevelClause->location()),
					"This try statement already has a low-level catch clause."
				);
			lowLevelClause = &clause;
			if (clause.parameters() && !clause.parameters()->parameters().empty())
			{
				if (
					clause.parameters()->parameters().size() != 1 ||
					*clause.parameters()->parameters().front()->type() != *TypeProvider::bytesMemory()
				)
					m_errorReporter.typeError(clause.location(), "Expected `catch (bytes memory ...) { ... }` or `catch { ... }`.");
				if (!m_evmVersion.supportsReturndata())
					m_errorReporter.typeError(
						clause.location(),
						"This catch clause type cannot be used on the selected EVM version (" +
						m_evmVersion.name() +
						"). You need at least a Byzantium-compatible EVM or use `catch { ... }`."
					);
			}
		}
		else if (clause.errorName() == "Error")
		{
			if (!m_evmVersion.supportsReturndata())
				m_errorReporter.typeError(
					clause.location(),
					"This catch clause type cannot be used on the selected EVM version (" +
					m_evmVersion.name() +
					"). You need at least a Byzantium-compatible EVM or use `catch { ... }`."
				);

			if (errorClause)
				m_errorReporter.typeError(
					clause.location(),
					SecondarySourceLocation{}.append("The first clause is here:", errorClause->location()),
					"This try statement already has an \"Error\" catch clause."
				);
			errorClause = &clause;
			if (
				!clause.parameters() ||
				clause.parameters()->parameters().size() != 1 ||
				*clause.parameters()->parameters().front()->type() != *TypeProvider::stringMemory()
			)
				m_errorReporter.typeError(clause.location(), "Expected `catch Error(string memory ...) { ... }`.");
		}
		else
			m_errorReporter.typeError(
				clause.location(),
				"Invalid catch clause name. Expected either `catch (...)` or `catch Error(...)`."
			);
	}
}

bool TypeChecker::visit(WhileStatement const& _whileStatement)
{
	switch (_whileStatement.loopType()) {
		case WhileStatement::LoopType::WHILE_DO:
		case WhileStatement::LoopType::DO_WHILE:
			expectType(_whileStatement.condition(), *TypeProvider::boolean());
			break;
		case WhileStatement::LoopType::REPEAT:
			expectType(_whileStatement.condition(), *TypeProvider::uint256());
			break;
	}

	_whileStatement.body().accept(*this);
	return false;
}

bool TypeChecker::visit(ForStatement const& _forStatement)
{
	if (_forStatement.initializationExpression())
		_forStatement.initializationExpression()->accept(*this);
	if (_forStatement.condition())
		expectType(*_forStatement.condition(), *TypeProvider::boolean());
	if (_forStatement.loopExpression())
		_forStatement.loopExpression()->accept(*this);
	_forStatement.body().accept(*this);
	return false;
}

bool TypeChecker::visit(ForEachStatement const& _forStatement)
{
	_forStatement.rangeDeclaration()->accept(*this);
	_forStatement.rangeExpression()->accept(*this);

	auto vars = dynamic_cast<VariableDeclarationStatement const*>(_forStatement.rangeDeclaration().get());
	if (vars == nullptr) {
		m_errorReporter.typeError(
			_forStatement.rangeDeclaration()->location(),
			"Expected variable declaration statement."
		);
	} else {
		auto mappingType = dynamic_cast<MappingType const *>(_forStatement.rangeExpression()->annotation().type);
		auto arrayType = dynamic_cast<ArrayType const *>(_forStatement.rangeExpression()->annotation().type);

		auto checkVarDeclaration = [&](VariableDeclaration const *vd, Type const *type) {
			if (vd == nullptr) { // for ((uint key, ) : map) {  }
				return;
			}
			BoolResult result = type->isImplicitlyConvertibleTo(*vd->type());
			if (!result) {
				auto errorMsg = "Type " +
								vd->type()->toString() +
								" is not implicitly convertible to expected type " +
								type->toString() + ".";
				m_errorReporter.typeError(vd->location(), errorMsg);
			}
		};

		if (mappingType) {
			if (vars->declarations().size() != 2) {
				m_errorReporter.typeError(
						vars->location(),
						"Expected two variables of type " +
						mappingType->keyType()->toString() +
						" and " +
						mappingType->valueType()->toString() +
						"."
				);
			} else {
				checkVarDeclaration(vars->declarations().at(0).get(), mappingType->keyType());
				checkVarDeclaration(vars->declarations().at(1).get(), mappingType->valueType());
			}
		} else if (arrayType) {
			if (vars->declarations().size() != 1) {
				m_errorReporter.typeError(
						vars->location(),
						"Too many declared variables. "
						"Expected one variable of type " + arrayType->baseType()->toString() + "."
				);
			} else {
				checkVarDeclaration(vars->declarations().at(0).get(), arrayType->baseType());
			}
		} else {
			m_errorReporter.typeError(
					_forStatement.rangeExpression()->location(),
					"Invalid range expression of type " +
					_forStatement.rangeExpression()->annotation().type->toString() + ". " +
					"Only array and mapping types are supported."
			);
		}
	}

	_forStatement.body().accept(*this);
	return false;
}

void TypeChecker::endVisit(Return const& _return)
{
	if (!_return.names().empty() && (!m_currentFunction->isPublic() ||  !m_currentFunction->isResponsible())) {
		SourceLocation loc = getSmallestCovering(_return.options());
		m_errorReporter.typeError(
			loc,
			R"(Options in return statement can be used only in responsible public/external functions.)"
		);
	} else {
		for (size_t i = 0; i < _return.names().size(); ++i) {
			_return.options().at(i)->accept(*this);
			const std::string &name = *_return.names().at(i);
			const std::map<std::string, Type const *> nameToType = {
					{"value",      TypeProvider::uint(128)},
					{"currencies", TypeProvider::extraCurrencyCollection()},
					{"bounce",     TypeProvider::boolean()},
					{"flag",       TypeProvider::uint(16)},
			};
			if (nameToType.count(name) == 0) {
				m_errorReporter.typeError(
						_return.options().at(i)->location(),
						"Unknown call option \"" +
						name +
						R"(". Valid options are "value", "currencies", "bounce", and "flag".)"
				);
			} else {
				expectType(*_return.options().at(i), *nameToType.at(name));
			}
		}
	}


	ParameterList const* params = _return.annotation().functionReturnParameters;
	if (!_return.expression())
	{
		if (params && !params->parameters().empty())
			m_errorReporter.typeError(_return.location(), "Return arguments required.");
		return;
	}
	if (!params)
	{
		m_errorReporter.typeError(_return.location(), "Return arguments not allowed.");
		return;
	}
	TypePointers returnTypes;
	for (auto const& var: params->parameters())
		returnTypes.push_back(type(*var));
	if (auto tupleType = dynamic_cast<TupleType const*>(type(*_return.expression())))
	{
		if (tupleType->components().size() != params->parameters().size())
			m_errorReporter.typeError(_return.location(), "Different number of arguments in return statement than in returns declaration.");
		else
		{
			BoolResult result = tupleType->isImplicitlyConvertibleTo(TupleType(returnTypes));
			if (!result)
				m_errorReporter.typeErrorConcatenateDescriptions(
					_return.expression()->location(),
					"Return argument type " +
					type(*_return.expression())->toString() +
					" is not implicitly convertible to expected type " +
					TupleType(returnTypes).toString(false) + ".",
					result.message()
				);
		}
	}
	else if (params->parameters().size() != 1)
		m_errorReporter.typeError(_return.location(), "Different number of arguments in return statement than in returns declaration.");
	else
	{
		TypePointer const& expected = type(*params->parameters().front());
		BoolResult result = type(*_return.expression())->isImplicitlyConvertibleTo(*expected);
		if (!result)
			m_errorReporter.typeErrorConcatenateDescriptions(
				_return.expression()->location(),
				"Return argument type " +
				type(*_return.expression())->toString() +
				" is not implicitly convertible to expected type (type of first return variable) " +
				expected->toString() + ".",
				result.message()
			);
	}
}

void TypeChecker::endVisit(EmitStatement const& _emit)
{
	if (
		_emit.eventCall().annotation().kind != FunctionCallKind::FunctionCall ||
		type(_emit.eventCall().expression())->category() != Type::Category::Function ||
		dynamic_cast<FunctionType const&>(*type(_emit.eventCall().expression())).kind() != FunctionType::Kind::Event
	)
		m_errorReporter.typeError(_emit.eventCall().expression().location(), "Expression has to be an event invocation.");
}

namespace
{
/**
 * @returns a suggested left-hand-side of a multi-variable declaration contairing
 * the variable declarations given in @a _decls.
 */
string createTupleDecl(vector<ASTPointer<VariableDeclaration>> const& _decls)
{
	vector<string> components;
	for (ASTPointer<VariableDeclaration> const& decl: _decls)
		if (decl)
		{
			solAssert(decl->annotation().type, "");
			components.emplace_back(decl->annotation().type->toString(false) + " " + decl->name());
		}
		else
			components.emplace_back();

	if (_decls.size() == 1)
		return components.front();
	else
		return "(" + boost::algorithm::join(components, ", ") + ")";
}

bool typeCanBeExpressed(vector<ASTPointer<VariableDeclaration>> const& decls)
{
	for (ASTPointer<VariableDeclaration> const& decl: decls)
	{
		// skip empty tuples (they can be expressed of course)
		if (!decl)
			continue;

		if (!decl->annotation().type)
			return false;

		if (auto functionType = dynamic_cast<FunctionType const*>(decl->annotation().type))
			if (
				functionType->kind() != FunctionType::Kind::Internal &&
				functionType->kind() != FunctionType::Kind::External
			)
				return false;
	}

	return true;
}
}

bool TypeChecker::visit(VariableDeclarationStatement const& _statement)
{
	if (!_statement.initialValue())
	{
		if (_statement.isInForLoop()){
			return false;
		}

		// No initial value is only permitted for single variables with specified type.
		if (_statement.declarations().size() != 1 || !_statement.declarations().front())
		{
			if (boost::algorithm::all_of_equal(_statement.declarations(), nullptr))
			{
				// The syntax checker has already generated an error for this case (empty LHS tuple).
				solAssert(m_errorReporter.hasErrors(), "");

				// It is okay to return here, as there are no named components on the
				// left-hand-side that could cause any damage later.
				return false;
			}
			else {
				// Bailing out *fatal* here, as those (untyped) vars may be used later, and diagnostics wouldn't be helpful then.
				m_errorReporter.fatalTypeError(_statement.location(), "Use of the \"var\" keyword is disallowed.");
			}
		}

		VariableDeclaration const& varDecl = *_statement.declarations().front();
		if (!varDecl.annotation().type)
			m_errorReporter.fatalTypeError(_statement.location(), "Use of the \"var\" keyword is disallowed.");

		varDecl.accept(*this);
		return false;
	}

	// Here we have an initial value and might have to derive some types before we can visit
	// the variable declaration(s).

	_statement.initialValue()->accept(*this);
	TypePointers valueTypes;
	if (auto tupleType = dynamic_cast<TupleType const*>(type(*_statement.initialValue())))
		valueTypes = tupleType->components();
	else
		valueTypes = TypePointers{type(*_statement.initialValue())};

	vector<ASTPointer<VariableDeclaration>> const& variables = _statement.declarations();
	if (variables.empty())
		// We already have an error for this in the SyntaxChecker.
		solAssert(m_errorReporter.hasErrors(), "");
	else if (valueTypes.size() != variables.size())
		m_errorReporter.typeError(
			_statement.location(),
			"Different number of components on the left hand side (" +
			toString(variables.size()) +
			") than on the right hand side (" +
			toString(valueTypes.size()) +
			")."
		);

	bool autoTypeDeductionNeeded = false;

	for (size_t i = 0; i < min(variables.size(), valueTypes.size()); ++i)
	{
		if (!variables[i])
			continue;
		VariableDeclaration const& var = *variables[i];
		solAssert(!var.value(), "Value has to be tied to statement.");
		TypePointer const& valueComponentType = valueTypes[i];
		solAssert(!!valueComponentType, "");
		if (!var.annotation().type)
		{
			autoTypeDeductionNeeded = true;

			// Infer type from value.
			solAssert(!var.typeName(), "");
			var.annotation().type = valueComponentType->mobileType();
			if (!var.annotation().type)
			{
				if (valueComponentType->category() == Type::Category::RationalNumber)
					m_errorReporter.fatalTypeError(
						_statement.initialValue()->location(),
						"Invalid rational " +
						valueComponentType->toString() +
						" (absolute value too large or division by zero)."
					);
				else
					solAssert(false, "");
			}
			else if (*var.annotation().type == *TypeProvider::emptyTuple())
				solAssert(false, "Cannot declare variable with void (empty tuple) type.");
			else if (valueComponentType->category() == Type::Category::RationalNumber)
			{
				string typeName = var.annotation().type->toString(true);
				string extension;
				if (auto type = dynamic_cast<IntegerType const*>(var.annotation().type))
				{
					unsigned numBits = type->numBits();
					bool isSigned = type->isSigned();
					solAssert(numBits > 0, "");
					string minValue;
					string maxValue;
					if (isSigned)
					{
						numBits--;
						minValue = "-" + bigint(bigint(1) << numBits).str();
					}
					else
						minValue = "0";
					maxValue = bigint((bigint(1) << numBits) - 1).str();
					extension = ", which can hold values between " + minValue + " and " + maxValue;
				}
				else
					solAssert(dynamic_cast<FixedPointType const*>(var.annotation().type), "Unknown type.");
			}

			var.accept(*this);
		}
		else
		{
			var.accept(*this);

			BoolResult result = valueComponentType->isImplicitlyConvertibleTo(*var.annotation().type);
			if (!result)
			{
				auto errorMsg = "Type " +
					valueComponentType->toString() +
					" is not implicitly convertible to expected type " +
					var.annotation().type->toString();
				if (
					valueComponentType->category() == Type::Category::RationalNumber &&
					dynamic_cast<RationalNumberType const&>(*valueComponentType).isFractional() &&
					valueComponentType->mobileType()
				)
				{
					if (var.annotation().type->operator==(*valueComponentType->mobileType()))
						m_errorReporter.typeError(
							_statement.location(),
							errorMsg + ", but it can be explicitly converted."
						);
					else
						m_errorReporter.typeError(
							_statement.location(),
							errorMsg +
							". Try converting to type " +
							valueComponentType->mobileType()->toString() +
							" or use an explicit conversion."
						);
				}
				else
					m_errorReporter.typeErrorConcatenateDescriptions(
						_statement.location(),
						errorMsg + ".",
						result.message()
					);
			}
		}
	}

	if (valueTypes.size() != variables.size())
	{
		solAssert(m_errorReporter.hasErrors(), "Should have errors!");
		for (auto const& var: variables)
			if (var && !var->annotation().type)
				BOOST_THROW_EXCEPTION(FatalError());
	}

	if (autoTypeDeductionNeeded)
	{
		if (!typeCanBeExpressed(variables))
			m_errorReporter.syntaxError(
				_statement.location(),
				"Use of the \"var\" keyword is disallowed. "
				"Type cannot be expressed in syntax."
			);
		else
			m_errorReporter.syntaxError(
				_statement.location(),
				"Use of the \"var\" keyword is disallowed. "
				"Use explicit declaration `" + createTupleDecl(variables) + " = ...´ instead."
			);
	}

	return false;
}

void TypeChecker::endVisit(ExpressionStatement const& _statement)
{
	if (type(_statement.expression())->category() == Type::Category::RationalNumber)
		if (!dynamic_cast<RationalNumberType const&>(*type(_statement.expression())).mobileType())
			m_errorReporter.typeError(_statement.expression().location(), "Invalid rational number.");

	if (auto call = dynamic_cast<FunctionCall const*>(&_statement.expression()))
	{
		if (auto callType = dynamic_cast<FunctionType const*>(type(call->expression())))
		{
			auto kind = callType->kind();
			if (
				kind == FunctionType::Kind::BareCall ||
				kind == FunctionType::Kind::BareCallCode ||
				kind == FunctionType::Kind::BareDelegateCall ||
				kind == FunctionType::Kind::BareStaticCall
			)
				m_errorReporter.warning(_statement.location(), "Return value of low-level calls not used.");
			else if (kind == FunctionType::Kind::Send)
				m_errorReporter.warning(_statement.location(), "Failure condition of 'send' ignored. Consider using 'transfer' instead.");
		}
	}
}

bool TypeChecker::visit(Conditional const& _conditional)
{
	expectType(_conditional.condition(), *TypeProvider::boolean());

	_conditional.trueExpression().accept(*this);
	_conditional.falseExpression().accept(*this);

	TypePointer trueType = type(_conditional.trueExpression())->mobileType();
	TypePointer falseType = type(_conditional.falseExpression())->mobileType();

	TypePointer commonType = nullptr;

	if (!trueType)
		m_errorReporter.typeError(_conditional.trueExpression().location(), "Invalid mobile type in true expression.");
	else
		commonType = trueType;

	if (!falseType)
		m_errorReporter.typeError(_conditional.falseExpression().location(), "Invalid mobile type in false expression.");
	else
		commonType = falseType;

	if (!trueType && !falseType)
		BOOST_THROW_EXCEPTION(FatalError());
	else if (trueType && falseType)
	{
		commonType = Type::commonType(trueType, falseType);

		if (!commonType)
		{
			m_errorReporter.typeError(
					_conditional.location(),
					"True expression's type " +
					trueType->toString() +
					" doesn't match false expression's type " +
					falseType->toString() +
					"."
					);
			// even we can't find a common type, we have to set a type here,
			// otherwise the upper statement will not be able to check the type.
			commonType = trueType;
		}
	}

	_conditional.annotation().type = commonType;
	_conditional.annotation().isPure =
		_conditional.condition().annotation().isPure &&
		_conditional.trueExpression().annotation().isPure &&
		_conditional.falseExpression().annotation().isPure;

	if (_conditional.annotation().lValueRequested)
		m_errorReporter.typeError(
				_conditional.location(),
				"Conditional expression as left value is not supported yet."
		);

	return false;
}

void TypeChecker::checkExpressionAssignment(Type const& _type, Expression const& _expression)
{
	if (auto const* tupleExpression = dynamic_cast<TupleExpression const*>(&_expression))
	{
		auto const* tupleType = dynamic_cast<TupleType const*>(&_type);
		auto const& types = tupleType ? tupleType->components() : vector<TypePointer> { &_type };

		solAssert(
			tupleExpression->components().size() == types.size() || m_errorReporter.hasErrors(),
			"Array sizes don't match or no errors generated."
		);

		for (size_t i = 0; i < min(tupleExpression->components().size(), types.size()); i++)
			if (types[i])
			{
				solAssert(!!tupleExpression->components()[i], "");
				checkExpressionAssignment(*types[i], *tupleExpression->components()[i]);
			}
	}
//	else if (_type.category() == Type::Category::Mapping)
//	{
//		bool isLocalOrReturn = false;
//		if (auto const* identifier = dynamic_cast<Identifier const*>(&_expression))
//			if (auto const *variableDeclaration = dynamic_cast<VariableDeclaration const*>(identifier->annotation().referencedDeclaration))
//				if (variableDeclaration->isLocalOrReturn())
//					isLocalOrReturn = true;
//		if (!isLocalOrReturn)
//			m_errorReporter.typeError(_expression.location(), "Mappings cannot be assigned to.");
//	}
}

bool TypeChecker::visit(Assignment const& _assignment)
{
	requireLValue(_assignment.leftHandSide());
	TypePointer t = type(_assignment.leftHandSide());
	_assignment.annotation().type = t;

	checkExpressionAssignment(*t, _assignment.leftHandSide());

	if (TupleType const* tupleType = dynamic_cast<TupleType const*>(t))
	{
		if (_assignment.assignmentOperator() != Token::Assign)
			m_errorReporter.typeError(
				_assignment.location(),
				"Compound assignment is not allowed for tuple types."
			);
		// Sequenced assignments of tuples is not valid, make the result a "void" type.
		_assignment.annotation().type = TypeProvider::emptyTuple();

		expectType(_assignment.rightHandSide(), *tupleType);

		// expectType does not cause fatal errors, so we have to check again here.
		if (dynamic_cast<TupleType const*>(type(_assignment.rightHandSide())))
			checkDoubleStorageAssignment(_assignment);
	}
	else if (_assignment.assignmentOperator() == Token::Assign)
		expectType(_assignment.rightHandSide(), *t);
	else
	{
		// compound assignment
		_assignment.rightHandSide().accept(*this);
		TypePointer resultType = t->binaryOperatorResult(
			TokenTraits::AssignmentToBinaryOp(_assignment.assignmentOperator()),
			type(_assignment.rightHandSide())
		);
		if (!resultType || *resultType != *t)
			m_errorReporter.typeError(
				_assignment.location(),
				"Operator " +
				string(TokenTraits::toString(_assignment.assignmentOperator())) +
				" not compatible with types " +
				t->toString() +
				" and " +
				type(_assignment.rightHandSide())->toString()
			);
	}
	return false;
}

bool TypeChecker::visit(TupleExpression const& _tuple)
{
	vector<ASTPointer<Expression>> const& components = _tuple.components();
	TypePointers types;

	if (_tuple.annotation().lValueRequested)
	{
		if (_tuple.isInlineArray())
			m_errorReporter.fatalTypeError(_tuple.location(), "Inline array type cannot be declared as LValue.");
		for (auto const& component: components)
			if (component)
			{
				requireLValue(*component);
				types.push_back(type(*component));
			}
			else
				types.push_back(TypePointer());
		if (components.size() == 1)
			_tuple.annotation().type = type(*components[0]);
		else
			_tuple.annotation().type = TypeProvider::tuple(move(types));
		// If some of the components are not LValues, the error is reported above.
		_tuple.annotation().isLValue = true;
	}
	else
	{
		bool isPure = true;
		TypePointer inlineArrayType = nullptr;

		for (size_t i = 0; i < components.size(); ++i)
		{
			if (!components[i])
				m_errorReporter.fatalTypeError(_tuple.location(), "Tuple component cannot be empty.");
			else if (components[i])
			{
				components[i]->accept(*this);
				types.push_back(type(*components[i]));

				if (types[i]->category() == Type::Category::Tuple)
					if (dynamic_cast<TupleType const&>(*types[i]).components().empty())
					{
						if (_tuple.isInlineArray())
							m_errorReporter.fatalTypeError(components[i]->location(), "Array component cannot be empty.");
						m_errorReporter.typeError(components[i]->location(), "Tuple component cannot be empty.");
					}

				// Note: code generation will visit each of the expression even if they are not assigned from.
				if (types[i]->category() == Type::Category::RationalNumber && components.size() > 1)
					if (!dynamic_cast<RationalNumberType const&>(*types[i]).mobileType())
						m_errorReporter.fatalTypeError(components[i]->location(), "Invalid rational number.");

				if (_tuple.isInlineArray())
				{
					solAssert(!!types[i], "Inline array cannot have empty components");

					if ((i == 0 || inlineArrayType) && !types[i]->mobileType())
						m_errorReporter.fatalTypeError(components[i]->location(), "Invalid mobile type.");

					if (i == 0)
						inlineArrayType = types[i]->mobileType();
					else if (inlineArrayType)
						inlineArrayType = Type::commonType(inlineArrayType, types[i]);
				}
				if (!components[i]->annotation().isPure)
					isPure = false;
			}
			else
				types.push_back(TypePointer());
		}
		_tuple.annotation().isPure = isPure;
		if (_tuple.isInlineArray())
		{
			if (!inlineArrayType)
				m_errorReporter.fatalTypeError(_tuple.location(), "Unable to deduce common type for array elements.");
			else if (!inlineArrayType->canLiveOutsideStorage())
				m_errorReporter.fatalTypeError(_tuple.location(), "Type " + inlineArrayType->toString() + " is only valid in storage.");

			_tuple.annotation().type = TypeProvider::array(inlineArrayType, types.size());
		}
		else
		{
			if (components.size() == 1)
				_tuple.annotation().type = type(*components[0]);
			else
				_tuple.annotation().type = TypeProvider::tuple(move(types));
		}

	}
	return false;
}

bool TypeChecker::visit(UnaryOperation const& _operation)
{
	// Inc, Dec, Add, Sub, Not, BitNot, Delete
	Token op = _operation.getOperator();
	bool const modifying = (op == Token::Inc || op == Token::Dec || op == Token::Delete);
	if (modifying)
		requireLValue(_operation.subExpression());
	else
		_operation.subExpression().accept(*this);
	TypePointer const& subExprType = type(_operation.subExpression());
	TypePointer t = type(_operation.subExpression())->unaryOperatorResult(op);
	if (!t)
	{
		m_errorReporter.typeError(
			_operation.location(),
			"Unary operator " +
			string(TokenTraits::toString(op)) +
			" cannot be applied to type " +
			subExprType->toString()
		);
		t = subExprType;
	}
	_operation.annotation().type = t;
	_operation.annotation().isPure = !modifying && _operation.subExpression().annotation().isPure;
	return false;
}

void TypeChecker::endVisit(BinaryOperation const& _operation)
{
	TypePointer const& leftType = type(_operation.leftExpression());
	TypePointer const& rightType = type(_operation.rightExpression());
	TypeResult result = leftType->binaryOperatorResult(_operation.getOperator(), rightType);
	TypePointer commonType = result.get();
	if (!commonType)
	{
		m_errorReporter.typeError(
			_operation.location(),
			"Operator " +
			string(TokenTraits::toString(_operation.getOperator())) +
			" not compatible with types " +
			leftType->toString() +
			" and " +
			rightType->toString() +
			(!result.message().empty() ? ". " + result.message() : "")
		);
		commonType = leftType;
	}
	_operation.annotation().commonType = commonType;
	_operation.annotation().type =
		TokenTraits::isCompareOp(_operation.getOperator()) ?
		TypeProvider::boolean() :
		commonType;
	_operation.annotation().isPure =
		_operation.leftExpression().annotation().isPure &&
		_operation.rightExpression().annotation().isPure;

	if (_operation.getOperator() == Token::Exp || _operation.getOperator() == Token::SHL)
	{
		string operation = _operation.getOperator() == Token::Exp ? "exponentiation" : "shift";
		if (
			leftType->category() == Type::Category::RationalNumber &&
			rightType->category() != Type::Category::RationalNumber
		)
			if ((
				commonType->category() == Type::Category::Integer &&
				dynamic_cast<IntegerType const&>(*commonType).numBits() != 256
			) || (
				commonType->category() == Type::Category::FixedPoint &&
				dynamic_cast<FixedPointType const&>(*commonType).numBits() != 256
			))
				m_errorReporter.warning(
					_operation.location(),
					"Result of " + operation + " has type " + commonType->toString() + " and thus "
					"might overflow. Silence this warning by converting the literal to the "
					"expected type."
				);
		if (
			commonType->category() == Type::Category::Integer &&
			rightType->category() == Type::Category::Integer &&
			dynamic_cast<IntegerType const&>(*commonType).numBits() <
			dynamic_cast<IntegerType const&>(*rightType).numBits()
		)
			m_errorReporter.warning(
				_operation.location(),
				"The result type of the " +
				operation +
				" operation is equal to the type of the first operand (" +
				commonType->toString() +
				") ignoring the (larger) type of the second operand (" +
				rightType->toString() +
				") which might be unexpected. Silence this warning by either converting "
				"the first or the second operand to the type of the other."
			);
	}
}

TypePointer TypeChecker::typeCheckTypeConversionAndRetrieveReturnType(
	FunctionCall const& _functionCall
)
{
	solAssert(_functionCall.annotation().kind == FunctionCallKind::TypeConversion, "");
	TypePointer const& expressionType = type(_functionCall.expression());

	vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	bool const isPositionalCall = _functionCall.names().empty();

	TypePointer resultType = dynamic_cast<TypeType const&>(*expressionType).actualType();
	if (arguments.size() != 1)
		m_errorReporter.typeError(
			_functionCall.location(),
			"Exactly one argument expected for explicit type conversion."
		);
	else if (!isPositionalCall)
		m_errorReporter.typeError(
			_functionCall.location(),
			"Type conversion cannot allow named arguments."
		);
	else
	{
		Type const* argType = type(*arguments.front());
		// Resulting data location is memory unless we are converting from a reference
		// type with a different data location.
		// (data location cannot yet be specified for type conversions)
		if (auto type = dynamic_cast<ReferenceType const*>(resultType))
			resultType = TypeProvider::withLocation(type, type->isPointer());
		if (argType->isExplicitlyConvertibleTo(*resultType))
		{
			if (dynamic_cast<ArrayType const*>(argType))
			{
				auto resultArrayType = dynamic_cast<ArrayType const*>(resultType);
				solAssert(!!resultArrayType, "");
			}
		}
		else
		{
			if (
				resultType->category() == Type::Category::Contract &&
				argType->category() == Type::Category::Address
			)
			{
				SecondarySourceLocation ssl;
				if (
					auto const* identifier = dynamic_cast<Identifier const*>(arguments.front().get())
				)
					if (
						auto const* variableDeclaration = dynamic_cast<VariableDeclaration const*>(
							identifier->annotation().referencedDeclaration
						)
					)
						ssl.append(
							"Did you mean to declare this variable as \"address payable\"?",
							variableDeclaration->location()
						);
				m_errorReporter.typeError(
					_functionCall.location(),
					ssl,
					"Explicit type conversion not allowed from non-payable \"address\" to \"" +
					resultType->toString() +
					"\", which has a payable fallback function."
				);
			}
			else if (
				auto const* functionType = dynamic_cast<FunctionType const*>(argType);
				functionType &&
				functionType->kind() == FunctionType::Kind::External &&
				resultType->category() == Type::Category::Address
			)
				m_errorReporter.typeError(
					_functionCall.location(),
					"Explicit type conversion not allowed from \"" +
					argType->toString() +
					"\" to \"" +
					resultType->toString() +
					"\". To obtain the address of the contract of the function, " +
					"you can use the .address member of the function."
				);
			else
				m_errorReporter.typeError(
					_functionCall.location(),
					"Explicit type conversion not allowed from \"" +
					argType->toString() +
					"\" to \"" +
					resultType->toString() +
					"\"."
				);
		}
		if (dynamic_cast<AddressType const*>(resultType)) {
			resultType = TypeProvider::address();
		}
	}
	return resultType;
}

void TypeChecker::typeCheckFunctionCall(
	FunctionCall const& _functionCall,
	FunctionTypePointer _functionType
)
{
	// Actual function call or struct constructor call.

	solAssert(!!_functionType, "");
	solAssert(_functionType->kind() != FunctionType::Kind::ABIDecode, "");

	if (_functionType->kind() == FunctionType::Kind::Declaration)
	{
		m_errorReporter.typeError(
			_functionCall.location(),
			"Cannot call function via contract type name."
		);
		return;
	}
	if (_functionType->kind() == FunctionType::Kind::Internal && _functionType->hasDeclaration())
		if (auto const* functionDefinition = dynamic_cast<FunctionDefinition const*>(&_functionType->declaration()))
			// functionDefinition->annotation().contract != m_scope ensures that this is a qualified access,
			// e.g. ``A.f();`` instead of a simple function call like ``f();`` (the latter is valid for unimplemented
			// functions).
			if (functionDefinition->annotation().contract != m_scope && !functionDefinition->isImplemented())
				m_errorReporter.typeError(
					_functionCall.location(),
					"Cannot call unimplemented base function."
				);

	// Check for unsupported use of bare static call
	if (
		_functionType->kind() == FunctionType::Kind::BareStaticCall &&
		!m_evmVersion.hasStaticCall()
	)
		m_errorReporter.typeError(
			_functionCall.location(),
			"\"staticcall\" is not supported by the VM version."
		);

	// Perform standard function call type checking
	typeCheckFunctionGeneralChecks(_functionCall, _functionType);
}

void TypeChecker::typeCheckFallbackFunction(FunctionDefinition const& _function)
{
	solAssert(_function.isFallback(), "");

	if (_function.inContractKind() == ContractKind::Library)
		m_errorReporter.typeError(_function.location(), "Libraries cannot have fallback functions.");
	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(_function.location(), "Fallback function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
	{
		if (_function.returnParameters().size() > 1 || *type(*_function.returnParameters().front()) != *TypeProvider::bytesMemory())
			m_errorReporter.typeError(_function.returnParameterList()->location(), "Fallback function can only have a single \"bytes memory\" return value.");
		else
			m_errorReporter.typeError(_function.returnParameterList()->location(), "Return values for fallback functions are not yet implemented.");
	}
	if (!_function.parameters().empty())
		m_errorReporter.typeError(_function.parameterList().location(), "Fallback function cannot take parameters.");
}

void TypeChecker::typeCheckReceiveFunction(FunctionDefinition const& _function)
{
	solAssert(_function.isReceive(), "");

	if (_function.inContractKind() == ContractKind::Library)
		m_errorReporter.typeError(_function.location(), "Libraries cannot have receive ether functions.");

	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(_function.location(), "Receive ether function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(_function.returnParameterList()->location(), "Receive ether function cannot return values.");
	if (!_function.parameters().empty())
		m_errorReporter.typeError(_function.parameterList().location(), "Receive ether function cannot take parameters.");
}


void TypeChecker::typeCheckConstructor(FunctionDefinition const& _function)
{
	solAssert(_function.isConstructor(), "");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(_function.returnParameterList()->location(), "Non-empty \"returns\" directive for constructor.");
	if (_function.stateMutability() != StateMutability::NonPayable)
		m_errorReporter.typeError(
			_function.location(),
			"Constructor must have default mutability, but it is \"" +
			stateMutabilityToString(_function.stateMutability()) +
			"\"."
		);
	if (_function.visibility() != Visibility::Public && _function.visibility() != Visibility::Internal)
		m_errorReporter.typeError(_function.location(), "Constructor must be public or internal.");
}

void TypeChecker::typeCheckOnBounce(const FunctionDefinition &_function) {
	solAssert(_function.isOnBounce(), "");

	if (_function.inContractKind() == ContractKind::Library)
		m_errorReporter.typeError(_function.location(), "Libraries cannot have onBounce functions.");

	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(_function.location(), "onBounce function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(_function.returnParameterList()->location(), "onBounce function cannot return values.");
	if (_function.parameters().size() != 1 || _function.parameters().at(0)->type()->category() != Type::Category::TvmSlice)
		m_errorReporter.typeError(_function.parameterList().location(), "onBounce function should take one parameter (TvmSlice body).");
}


void TypeChecker::typeCheckOnTickTock(const FunctionDefinition &_function) {
	if (_function.inContractKind() == ContractKind::Library)
		m_errorReporter.typeError(_function.location(), "Libraries cannot have onTickTock functions.");

	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(_function.location(), "onTickTock function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(_function.returnParameterList()->location(), "onTickTock function cannot return values.");
	if (_function.parameters().size() != 1 || _function.parameters().at(0)->type()->category() != Type::Category::Bool)
		m_errorReporter.typeError(_function.parameterList().location(), "onTickTock function should take one parameter (bool isTock).");
}

void TypeChecker::checkNeedCallback(FunctionType const* callee, ASTNode const& node) {
	if (callee->hasDeclaration()) {
		auto funcDef = dynamic_cast<FunctionDefinition const *>(&callee->declaration());
		if (funcDef && funcDef->isResponsible()) {
			m_errorReporter.typeError(
					node.location(),
					SecondarySourceLocation().append("Declaration is here:", funcDef->location()),
					R"("callback" option must be set because callee function is marked as responsible.)"
			);
		}
	}
}

void TypeChecker::typeCheckTvmEncodeArg(Type const* type, Expression const& node) {
	switch (type->category()) {
		case Type::Category::RationalNumber:
			m_errorReporter.typeError(
					node.location(),
					"Cannot perform encoding for a literal."
					" Please convert it to an explicit type first."
			);
			break;
		case Type::Category::Address:
		case Type::Category::Array:
		case Type::Category::Bool:
		case Type::Category::Contract:
		case Type::Category::ExtraCurrencyCollection:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::Mapping:
		case Type::Category::StringLiteral:
		case Type::Category::Struct:
		case Type::Category::TvmBuilder:
		case Type::Category::TvmCell:
		case Type::Category::TvmSlice:
			break;
		case Type::Category::Optional:
		{
			Type const *valueType = dynamic_cast<OptionalType const*>(type)->valueType();
			typeCheckTvmEncodeArg(valueType, node);
			break;
		}
		default:
			m_errorReporter.typeError(
					node.location(),
					"Encoding for a " + node.annotation().type->toString(true) + " isn't supported."
			);
	}
}

void TypeChecker::typeCheckTvmEncodeFunctions(FunctionCall const& _functionCall) {
	vector<ASTPointer<Expression const>> const &arguments = _functionCall.arguments();
	for (const auto & argument : arguments) {
		auto const &argType = type(*argument);
		typeCheckTvmEncodeArg(argType, *argument);
	}
}

void TypeChecker::typeCheckABIEncodeFunctions(
	FunctionCall const& _functionCall,
	FunctionTypePointer _functionType
)
{
	solAssert(!!_functionType, "");
	solAssert(
		_functionType->kind() == FunctionType::Kind::ABIEncode ||
		_functionType->kind() == FunctionType::Kind::ABIEncodePacked ||
		_functionType->kind() == FunctionType::Kind::ABIEncodeWithSelector ||
		_functionType->kind() == FunctionType::Kind::ABIEncodeWithSignature,
		"ABI function has unexpected FunctionType::Kind."
	);
	solAssert(_functionType->takesArbitraryParameters(), "ABI functions should be variadic.");

	bool const isPacked = _functionType->kind() == FunctionType::Kind::ABIEncodePacked;
	solAssert(_functionType->padArguments() != isPacked, "ABI function with unexpected padding");

	bool const abiEncoderV2 = m_scope->sourceUnit().annotation().experimentalFeatures.count(
		ExperimentalFeature::ABIEncoderV2
	);

	// Check for named arguments
	if (!_functionCall.names().empty())
	{
		m_errorReporter.typeError(
			_functionCall.location(),
			"Named arguments cannot be used for functions that take arbitrary parameters."
		);
		return;
	}

	// Perform standard function call type checking
	typeCheckFunctionGeneralChecks(_functionCall, _functionType);

	// Check additional arguments for variadic functions
	vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	for (size_t i = 0; i < arguments.size(); ++i)
	{
		auto const& argType = type(*arguments[i]);

		if (argType->category() == Type::Category::RationalNumber)
		{
			auto const& rationalType = dynamic_cast<RationalNumberType const&>(*argType);
			if (rationalType.isFractional())
			{
				m_errorReporter.typeError(
					arguments[i]->location(),
					"Fractional numbers cannot yet be encoded."
				);
				continue;
			}
			else if (!argType->mobileType())
			{
				m_errorReporter.typeError(
					arguments[i]->location(),
					"Invalid rational number (too large or division by zero)."
				);
				continue;
			}
			else if (isPacked)
			{
				m_errorReporter.typeError(
					arguments[i]->location(),
					"Cannot perform packed encoding for a literal."
					" Please convert it to an explicit type first."
				);
				continue;
			}
		}

		if (isPacked && !typeSupportedByOldABIEncoder(*argType, false /* isLibrary */))
		{
			m_errorReporter.typeError(
				arguments[i]->location(),
				"Type not supported in packed mode."
			);
			continue;
		}

		if (!argType->fullEncodingType(false, abiEncoderV2, !_functionType->padArguments()))
			m_errorReporter.typeError(
				arguments[i]->location(),
				"This type cannot be encoded."
			);
	}
}

FunctionDefinition const*
TypeChecker::getFunctionDefinition(Expression const* expr) {
	if (expr->annotation().type->category() != Type::Category::Function) {
		return nullptr;
	}
	auto identifier = dynamic_cast<Identifier const*>(expr);
	Declaration const* declaration{};
	if (identifier)
		declaration = identifier->annotation().referencedDeclaration;
	else if (auto member = dynamic_cast<MemberAccess const *>(expr)) {
		declaration = member->annotation().referencedDeclaration;
	}
	if (declaration == nullptr) {
		return nullptr;
	}
	return dynamic_cast<FunctionDefinition const*>(declaration);
}

std::pair<bool, FunctionDefinition const*>
TypeChecker::getConstructorDefinition(Expression const* expr) {
	auto contractType = getContractType(expr);
	if (contractType == nullptr) {
		return {};
	}
	FunctionDefinition const* constr = contractType->contractDefinition().constructor();
	if (constr == nullptr) {
		return {true, nullptr};
	}
	return {true, constr};
}

ContractType const* TypeChecker::getContractType(Expression const* expr) {
	if (expr->annotation().type->category() != Type::Category::TypeType) {
		return {};
	}
	auto tt = dynamic_cast<const TypeType*>(expr->annotation().type);
	auto contractType = dynamic_cast<const ContractType*>(tt->actualType());
	return contractType;
}

void TypeChecker::typeCheckFunctionGeneralChecks(
	FunctionCall const& _functionCall,
	FunctionTypePointer _functionType
)
{
	// Actual function call or struct constructor call.

	solAssert(!!_functionType, "");
	solAssert(_functionType->kind() != FunctionType::Kind::ABIDecode, "");

	bool const isPositionalCall = _functionCall.names().empty();
	bool const isVariadic = _functionType->takesArbitraryParameters();

	solAssert(
		!isVariadic || _functionCall.annotation().kind == FunctionCallKind::FunctionCall,
		"Struct constructor calls cannot be variadic."
	);

	TypePointers const& parameterTypes = _functionType->parameterTypes();
	vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	vector<ASTPointer<ASTString>> const& argumentNames = _functionCall.names();
	bool isFunctionWithDefaultValues = false;
	{
		auto ma = dynamic_cast<MemberAccess const*>(&_functionCall.expression());
		if (ma && ma->memberName() == "transfer" && dynamic_cast<AddressType const *>(ma->expression().annotation().type)) {
			isFunctionWithDefaultValues = true;
		}
		if (ma && dynamic_cast<MagicType const *>(ma->expression().annotation().type)) {
			if (ma->memberName() == "buildStateInit" ||
				ma->memberName() == "buildExtMsg" ||
				ma->memberName() == "buildIntMsg")
			isFunctionWithDefaultValues = true;
		}
	}

	// Check number of passed in arguments
	if (
		!isFunctionWithDefaultValues && (arguments.size() < parameterTypes.size() || (!isVariadic && arguments.size() > parameterTypes.size()))
	)
	{
		bool const isStructConstructorCall =
			_functionCall.annotation().kind == FunctionCallKind::StructConstructorCall;

		string msg;

		if (isVariadic)
			msg +=
				"Need at least " +
				toString(parameterTypes.size()) +
				" arguments for " +
				string(isStructConstructorCall ? "struct constructor" : "function call") +
				", but provided only " +
				toString(arguments.size()) +
				".";
		else
			msg +=
				"Wrong argument count for " +
				string(isStructConstructorCall ? "struct constructor" : "function call") +
				": " +
				toString(arguments.size()) +
				" arguments given but " +
				string(isVariadic ? "need at least " : "expected ") +
				toString(parameterTypes.size()) +
				".";

		// Extend error message in case we try to construct a struct with mapping member.
		if (isStructConstructorCall)
		{
			/// For error message: Struct members that were removed during conversion to memory.
			TypePointer const expressionType = type(_functionCall.expression());
			TypeType const& t = dynamic_cast<TypeType const&>(*expressionType);
			auto const& structType = dynamic_cast<StructType const&>(*t.actualType());
			set<string> membersRemovedForStructConstructor = structType.membersMissingInMemory();

			if (!membersRemovedForStructConstructor.empty())
			{
				msg += " Members that have to be skipped in memory:";
				for (auto const& member: membersRemovedForStructConstructor)
					msg += " " + member;
			}
		}
		else if (
			_functionType->kind() == FunctionType::Kind::BareCall ||
			_functionType->kind() == FunctionType::Kind::BareCallCode ||
			_functionType->kind() == FunctionType::Kind::BareDelegateCall ||
			_functionType->kind() == FunctionType::Kind::BareStaticCall
		)
		{
			if (arguments.empty())
				msg +=
					" This function requires a single bytes argument."
					" Use \"\" as argument to provide empty calldata.";
			else
				msg +=
					" This function requires a single bytes argument."
					" If all your arguments are value types, you can use"
					" abi.encode(...) to properly generate it.";
		}
		else if (
			_functionType->kind() == FunctionType::Kind::KECCAK256 ||
			_functionType->kind() == FunctionType::Kind::SHA256 ||
			_functionType->kind() == FunctionType::Kind::RIPEMD160
		)
			msg +=
				" This function requires a single bytes argument."
				" Use abi.encodePacked(...) to obtain the pre-0.5.0"
				" behaviour or abi.encode(...) to use ABI encoding.";
		m_errorReporter.typeError(_functionCall.location(), msg);
		return;
	}

	// Parameter to argument map
	std::vector<Expression const*> paramArgMap(parameterTypes.size());

	// Map parameters to arguments - trivially for positional calls, less so for named calls
	if (isPositionalCall) {
		if (isFunctionWithDefaultValues) {
			paramArgMap.resize(arguments.size());
		}
		for (size_t i = 0; i < paramArgMap.size(); ++i) {
			paramArgMap[i] = arguments.at(i).get();
		}
	} else {
		auto const &parameterNames = _functionType->parameterNames();

		solAssert(
				isFunctionWithDefaultValues || parameterNames.size() == argumentNames.size(),
				"Unexpected parameter length mismatch!"
		);

		// Check for duplicate argument names
		{
			bool duplication = false;
			for (size_t i = 0; i < argumentNames.size(); i++)
				for (size_t j = i + 1; j < argumentNames.size(); j++)
					if (*argumentNames[i] == *argumentNames[j]) {
						duplication = true;
						m_errorReporter.typeError(
								arguments[i]->location(),
								"Duplicate named argument \"" + *argumentNames[i] + "\"."
						);
					}
			if (duplication)
				return;
		}

		// map parameter names to argument names
		if (isFunctionWithDefaultValues) {
			for (size_t i = 0; i < argumentNames.size(); i++) {
				size_t j;
				for (j = 0; j < parameterNames.size(); j++)
					if (*argumentNames[i] == parameterNames[j])
						break;

				if (j < parameterNames.size())
					paramArgMap[j] = arguments[i].get();
				else
				{
					if (j < paramArgMap.size())
						paramArgMap[j] = nullptr;
					if (_functionType->kind() == FunctionType::Kind::TVMBuildStateInit && *argumentNames.at(i) == "contr"){
						// Do nothing.
						// It's checked in TypeChecker::visit(FunctionCall const& _functionCall)
					} else {
						m_errorReporter.typeError(
							_functionCall.location(),
							"Named argument \"" +
							*argumentNames[i] +
							"\" does not match function declaration."
						);
					}
				}
			}
		} else {
			bool not_all_mapped = false;

			for (size_t i = 0; i < paramArgMap.size(); i++)
			{
				size_t j;
				for (j = 0; j < argumentNames.size(); j++)
					if (parameterNames[i] == *argumentNames[j])
						break;

				if (j < argumentNames.size())
					paramArgMap[i] = arguments[j].get();
				else
				{
					paramArgMap[i] = nullptr;
					not_all_mapped = true;
					m_errorReporter.typeError(
						_functionCall.location(),
						"Named argument \"" +
						*argumentNames[i] +
						"\" does not match function declaration."
					);
				}
			}

			if (not_all_mapped)
				return;
		}
	}

	// Check for compatible types between arguments and parameters
	for (size_t i = 0; i < paramArgMap.size(); ++i)
	{
		if (paramArgMap[i] == nullptr && isFunctionWithDefaultValues) {
			continue;
		}
		solAssert(!!paramArgMap[i], "unmapped parameter");
		if (!type(*paramArgMap[i])->isImplicitlyConvertibleTo(*parameterTypes[i]))
		{
			string msg =
				"Invalid type for argument in function call. "
				"Invalid implicit conversion from " +
				type(*paramArgMap[i])->toString() +
				" to " +
				parameterTypes[i]->toString() +
				" requested.";
			if (
				_functionType->kind() == FunctionType::Kind::BareCall ||
				_functionType->kind() == FunctionType::Kind::BareCallCode ||
				_functionType->kind() == FunctionType::Kind::BareDelegateCall ||
				_functionType->kind() == FunctionType::Kind::BareStaticCall
			)
				msg +=
					" This function requires a single bytes argument."
					" If all your arguments are value types, you can"
					" use abi.encode(...) to properly generate it.";
			else if (
				_functionType->kind() == FunctionType::Kind::KECCAK256 ||
				_functionType->kind() == FunctionType::Kind::SHA256 ||
				_functionType->kind() == FunctionType::Kind::RIPEMD160
			)
				msg +=
					" This function requires a single bytes argument."
					" Use abi.encodePacked(...) to obtain the pre-0.5.0"
					" behaviour or abi.encode(...) to use ABI encoding.";
			m_errorReporter.typeError(paramArgMap[i]->location(), msg);
		}
	}
}

FunctionDefinition const*
TypeChecker::checkPubFunctionAndGetDefinition(Expression const& arg, bool printError) {
	FunctionDefinition const* funcDef = getFunctionDefinition(&arg);
	if (funcDef) {
		if (!funcDef->isPublic()) {
			m_errorReporter.fatalTypeError(
					arg.location(),
					SecondarySourceLocation().append("Declaration is here:", funcDef->location()),
					"Public/external function or contract type required, but \"" +
					Declaration::visibilityToString(funcDef->visibility()) +
					"\" function is provided."
			);
		}
	} else if (printError) {
		m_errorReporter.fatalTypeError(
			arg.location(),
			"Expected function type, but got " +
			toString(arg.annotation().type->toString()) +
			"."
		);
	}
	return funcDef;
}

FunctionDefinition const*
TypeChecker::checkPubFunctionOrContractTypeAndGetDefinition(Expression const& arg) {
	FunctionDefinition const* funcDef = checkPubFunctionAndGetDefinition(arg);
	if (funcDef) {
		return funcDef;
	}

	const auto &[isContract, constructorDef] = getConstructorDefinition(&arg);
	if (!isContract) {
		m_errorReporter.fatalTypeError(
			arg.location(),
			"Function or contract type required, but " +
			type(arg)->toString(true) +
			" provided."
		);
	}
	if (constructorDef && !constructorDef->isPublic()) {
		m_errorReporter.fatalTypeError(
			arg.location(),
			SecondarySourceLocation().append("Declaration is here:", constructorDef->location()),
			"Contract with public constructor required, but \"" +
					Declaration::visibilityToString(constructorDef->visibility()) +
			"\" constructor provided."
		);
	}
	return constructorDef;
}

void TypeChecker::checkInitList(InitializerList const *list, ContractType const *ct) {
	vector<tuple<VariableDeclaration const*, u256, unsigned>> vars = ct->stateVariables();
	for (size_t i = 0; i < list->names().size(); ++i) {
		const std::string name = *list->names().at(i);
		Type const* exprType = list->options().at(i)->annotation().type;
		size_t j;
		for (j = 0; j < vars.size(); ++j) {
			VariableDeclaration const* v = std::get<0>(vars.at(j));
			if (name == v->name()) {
				if (!v->isStatic()) {
					m_errorReporter.typeError(
							list->options().at(i)->location(),
							SecondarySourceLocation()
									.append("Declaration is here:", v->location()),
							"Initialization of a non-static variable."
					);
				} else if (!exprType->isImplicitlyConvertibleTo(*v->type())) {
					m_errorReporter.typeError(
						list->options().at(i)->location(),
						"Expected " + v->type()->toString() + " type" +
						" but got " + exprType->toString() + " type."
					);
				}
				break;
			}
		}
		if (j == vars.size()) {
			m_errorReporter.typeError(
				list->options().at(i)->location(),
				SecondarySourceLocation()
						.append("Contract is here:", ct->contractDefinition().location()),
				"Unknown state variable \"" + name + "\"."
			);
		}
	}
}

void TypeChecker::checkCallList(
	vector<ASTPointer<Expression const>> const& arguments,
	FunctionCall const& _functionCall,
	bool ignoreCallBack // for ext msg we set callbackFunctionId==0
) {
	if (arguments.empty()) {
		m_errorReporter.typeError(
				_functionCall.location(),
				"At least one argument of function or contract type is expected."
		);
		return;

	}
	FunctionDefinition const *functionDeclaration =
			checkPubFunctionOrContractTypeAndGetDefinition(*arguments.front().get());

	if (functionDeclaration != nullptr) {
		bool needCallback = !ignoreCallBack && functionDeclaration->isResponsible();
		int shift = needCallback ? 1 : 0;
		std::vector<ASTPointer<VariableDeclaration>> const &calleeParams = functionDeclaration->parameters();
		if (1 + shift + calleeParams.size() != arguments.size()) {
			m_errorReporter.typeError(
					_functionCall.location(),
					SecondarySourceLocation()
							.append("Declaration is here:", functionDeclaration->location()),
					"Wrong arguments count: " +
					toString(arguments.size()) +
					" arguments given but expected " +
					toString(1 + shift + calleeParams.size()) +
					" arguments: function/contract identifier" +
					(needCallback ? ", callback function identifier" : "") + " and " +
					toString(calleeParams.size()) + " function argument(s)."
			);
		} else {
			if (needCallback) {
				checkPubFunctionAndGetDefinition(*arguments.at(1).get(), true);
			}
			for (size_t i = 0; i < calleeParams.size(); i++)
				expectType(*arguments[1 + shift + i], *calleeParams[i]->annotation().type);
		}
	} else {
		if (arguments.size() >= 2) {
			// check default constructor
			auto tt = dynamic_cast<const TypeType *>(arguments.front()->annotation().type);
			auto contractType = dynamic_cast<const ContractType *>(tt->actualType());
			const auto &contractDefinition = contractType->contractDefinition();
			m_errorReporter.fatalTypeError(
					_functionCall.location(),
					SecondarySourceLocation().append("Declaration is here:",
													 contractDefinition.location()),
					"Wrong arguments count: " +
					toString(arguments.size()) +
					" arguments given but 0 expected. Default constructor has no parameters."
			);
		}
	}
}

bool TypeChecker::visit(FunctionCall const& _functionCall)
{
	vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	vector<ASTPointer<ASTString>> const &argumentNames = _functionCall.names();
	bool argumentsArePure = true;

	// We need to check arguments' type first as they will be needed for overload resolution.
	for (ASTPointer<Expression const> const& argument: arguments)
	{
		argument->accept(*this);
		if (!argument->annotation().isPure)
			argumentsArePure = false;
	}

	// Store argument types - and names if given - for overload resolution
	{
		FuncCallArguments funcCallArgs;

		funcCallArgs.names = _functionCall.names();

		for (ASTPointer<Expression const> const& argument: arguments)
			funcCallArgs.types.push_back(type(*argument));

		_functionCall.expression().annotation().arguments = std::move(funcCallArgs);
	}

	_functionCall.expression().accept(*this);

	Type const* expressionType = type(_functionCall.expression());

	// Determine function call kind and function type for this FunctionCall node
	FunctionCallAnnotation& funcCallAnno = _functionCall.annotation();
	FunctionTypePointer functionType = nullptr;

	// Determine and assign function call kind, lvalue, purity and function type for this FunctionCall node
	switch (expressionType->category())
	{
	case Type::Category::Function:
		functionType = dynamic_cast<FunctionType const*>(expressionType);
		funcCallAnno.kind = FunctionCallKind::FunctionCall;

		// Purity for function calls also depends upon the callee and its FunctionType
		funcCallAnno.isPure =
			argumentsArePure &&
			_functionCall.expression().annotation().isPure &&
			functionType &&
			functionType->isPure();

		if (
			functionType->kind() == FunctionType::Kind::ArrayPush ||
			functionType->kind() == FunctionType::Kind::ByteArrayPush
		)
			funcCallAnno.isLValue = functionType->parameterTypes().empty();
		else if (functionType->kind() == FunctionType::Kind::OptionalGet)
			funcCallAnno.isLValue = true;
		break;

	case Type::Category::TypeType:
	{
		// Determine type for type conversion or struct construction expressions
		TypePointer const& actualType = dynamic_cast<TypeType const&>(*expressionType).actualType();
		solAssert(!!actualType, "");

		if (actualType->category() == Type::Category::Struct)
		{
			functionType = dynamic_cast<StructType const&>(*actualType).constructorType();
			funcCallAnno.kind = FunctionCallKind::StructConstructorCall;
			funcCallAnno.isPure = argumentsArePure;
		}
		else
		{
			funcCallAnno.kind = FunctionCallKind::TypeConversion;
			funcCallAnno.isPure = argumentsArePure;
		}

		break;
	}

	default:
		m_errorReporter.typeError(_functionCall.location(), "Type is not callable");
		funcCallAnno.kind = FunctionCallKind::Unset;
		funcCallAnno.isPure = argumentsArePure;
		break;
	}

	auto checkArgNumAndIsInteger = [&](
		vector<ASTPointer<Expression const>> const& arguments,
		size_t arguments_cnt,
		const std::function<bool(size_t, size_t)>& cmpOperator,
		const std::string& errorMsg
	){
		if (!cmpOperator(arguments.size(), arguments_cnt)) {
			m_errorReporter.fatalTypeError(
				_functionCall.location(),
				errorMsg
			);
		}

		for (const auto & arg : arguments) {
			Type::Category cat = arg->annotation().type->mobileType()->category();
			if (cat != Type::Category::Integer) {
				m_errorReporter.fatalTypeError(
					arg->location(),
					"Expected an integer type."
				);
			}
		}
	};

	auto checkArgNumAndIsIntegerOrFixedPoint = [&](
		vector<ASTPointer<Expression const>> const& arguments,
		size_t arguments_cnt,
		const std::function<bool(size_t, size_t)>& cmpOperator,
		const std::string& errorMsg
	){
		if (!cmpOperator(arguments.size(), arguments_cnt)) {
			m_errorReporter.fatalTypeError(
				_functionCall.location(),
				errorMsg
			);
		}

		for (const auto & arg : arguments) {
			Type::Category cat = arg->annotation().type->mobileType()->category();
			if (cat != Type::Category::Integer && cat != Type::Category::FixedPoint) {
				m_errorReporter.fatalTypeError(
					arg->location(),
					"Expected integer or fixed point type."
				);
			}
		}
	};

	auto checkAllAreNotFractions = [&] (vector<ASTPointer<Expression const>> const& arguments) {
		bool areAllConstants = true;
		bool haveAnyFraction = false;
		SourceLocation loc = getSmallestCovering(arguments);
		for (const auto & arg : arguments) {
			auto r = dynamic_cast<RationalNumberType const*>(arg->annotation().type);
			areAllConstants &= r != nullptr;
			haveAnyFraction |= r != nullptr && r->isFractional();
		}
		if (areAllConstants && haveAnyFraction) {
			m_errorReporter.fatalTypeError(
				loc,
				"Cannot perform operation for constant literals. Please convert at least one function argument to an explicit type."
			);
		}
	};

	auto getCommonType = [&](vector<ASTPointer<Expression const>> const& arguments){
		TypePointer result = arguments.at(0)->annotation().type;
		for (std::size_t i = 1; i < arguments.size(); ++i) {
			TypePointer rightType = arguments.at(i)->annotation().type;
			result = Type::commonType(result, rightType);
			if (result == nullptr) {
				m_errorReporter.fatalTypeError(
						arguments.at(i)->location(),
						"All arguments must have signed or unsigned integer type at the same time."
				);
			}
		}
		return result;
	};

	auto findName = [&](const ASTString& optName) {
		auto it = std::find_if(argumentNames.begin(), argumentNames.end(),
							   [&](const ASTPointer<ASTString> &name) {
								   return *name == optName;
							   });
		return it == argumentNames.end()  ? -1 : it - argumentNames.begin();
	};

	auto hasName = [&](const ASTString& optName) {
		return findName(optName) != -1;
	};

	auto checkHaveNamedParams = [&]() {
		if (argumentNames.empty())
			m_errorReporter.fatalTypeError(
					_functionCall.location(),
					string("Function parameters should be specified with names.")
			);
	};


	// Determine return types
	// and target params arguments if function takes variable count of arguments or
	// takes fixed count of arguments but returned type depends on types of parameters
	switch (funcCallAnno.kind)
	{
	case FunctionCallKind::TypeConversion:
		funcCallAnno.type = typeCheckTypeConversionAndRetrieveReturnType(_functionCall);
		break;

	case FunctionCallKind::StructConstructorCall: // fall-through
	case FunctionCallKind::FunctionCall:
	{
		TypePointers paramTypes = functionType->parameterTypes();
		TypePointers returnTypes;
		auto checkArgConversion = [&]() {
			for (size_t i = 0; i < paramTypes.size(); ++i) {
				if (!arguments.at(i)->annotation().type->isImplicitlyConvertibleTo(*paramTypes.at(i))) {
					m_errorReporter.typeError(_functionCall.location(),
                    "Expected argument of type " + paramTypes.at(i)->canonicalName());
				}
			}
		};

		switch (functionType->kind())
		{
			case FunctionType::Kind::ABIDecode:
			{
				bool const abiEncoderV2 =
					m_scope->sourceUnit().annotation().experimentalFeatures.count(
						ExperimentalFeature::ABIEncoderV2
					);
				returnTypes = typeCheckABIDecodeAndRetrieveReturnType(_functionCall, abiEncoderV2);
				break;
			}
			case FunctionType::Kind::TVMConfigParam:
			{
				returnTypes = getReturnTypesForTVMConfig(_functionCall);
				break;
			}
			case FunctionType::Kind::TVMSliceDecode:
			{
				returnTypes = checkSliceDecode(_functionCall);
				break;
			}
			case FunctionType::Kind::DecodeFunctionParams:
			{
				if (arguments.size() != 1) {
					m_errorReporter.fatalTypeError(
						_functionCall.location(),
						string("Expected one argument.")
					);
				}
				FunctionDefinition const* functionDeclaration = checkPubFunctionOrContractTypeAndGetDefinition(*arguments.front().get());
				if (functionDeclaration != nullptr) { // if nullptr => default constructor
					if (functionDeclaration->isResponsible()) {
						returnTypes.push_back(TypeProvider::uint(32)); // callback function
					}
					for (const ASTPointer<VariableDeclaration> &vd : functionDeclaration->parameters()) {
						returnTypes.push_back(vd->type());
					}
				}
				break;
			}
			case FunctionType::Kind::RndNext:
			{
				checkArgNumAndIsInteger(arguments, 1, std::less_equal<>(), "Expected at most one argument.");
				if (arguments.empty()) {
					returnTypes.push_back(TypeProvider::uint256());
				} else {
					TypePointer result = arguments.at(0)->annotation().type->mobileType();
					paramTypes.push_back(result);
					returnTypes.push_back(result);
				}
				break;
			}
			case FunctionType::Kind::MathMin:
			case FunctionType::Kind::MathMax:
			{
				checkArgNumAndIsIntegerOrFixedPoint(arguments, 2, std::greater_equal<>(), "Expected at least two arguments.");
				TypePointer result = getCommonType(arguments);
				paramTypes = TypePointers(arguments.size(), result);
				returnTypes.push_back(result);
				break;
			}
			case FunctionType::Kind::MathMinMax:
			{
				checkArgNumAndIsIntegerOrFixedPoint(arguments, 2, std::equal_to<>(), "Expected two arguments.");
				TypePointer result = getCommonType(arguments);
				paramTypes = TypePointers(arguments.size(), result);
				returnTypes.push_back(result);
				returnTypes.push_back(result);
				break;
			}
			case FunctionType::Kind::MathDivR:
			case FunctionType::Kind::MathDivC:
			{
				checkArgNumAndIsIntegerOrFixedPoint(arguments, 2, std::equal_to<>(), "Expected two arguments.");
				checkAllAreNotFractions(arguments);
				TypePointer result = getCommonType(arguments);
				paramTypes = TypePointers(arguments.size(), result);
				returnTypes.push_back(result);
				break;
			}
			case FunctionType::Kind::MathDivMod:
			{
				checkArgNumAndIsInteger(arguments, 2, std::equal_to<>(), "Expected two arguments.");
				TypePointer result = getCommonType(arguments);
				paramTypes.push_back(result);
				paramTypes.push_back(result);
				returnTypes.push_back(result);
				returnTypes.push_back(result);
				break;
			}
			case FunctionType::Kind::MathMulDiv:
			case FunctionType::Kind::MathMulDivMod:
			{
				checkArgNumAndIsInteger(arguments, 3, std::equal_to<>(), "Expected three arguments.");
				TypePointer result = getCommonType(arguments);
				paramTypes.push_back(result);
				paramTypes.push_back(result);
				paramTypes.push_back(result);
				returnTypes.push_back(result);
				if (functionType->kind() == FunctionType::Kind::MathMulDivMod) {
					returnTypes.push_back(result);
				}
				break;
			}
			case FunctionType::Kind::MathAbs:
			{
				checkArgNumAndIsIntegerOrFixedPoint(arguments, 1, std::equal_to<>(), "Expected one argument.");
				TypePointer type = arguments[0]->annotation().type->mobileType();
				paramTypes.push_back(type);
				returnTypes.push_back(type->mobileType());
				break;
			}
			case FunctionType::Kind::MathModpow2:
			{
				checkArgNumAndIsInteger(arguments, 2, std::equal_to<>(), "Expected two arguments.");
				bool isConst = arguments[1]->annotation().isPure;
				if (!isConst) {
					m_errorReporter.fatalTypeError(
							arguments.at(1)->location(),
							"Expected a constant integer type but got " +
							arguments[1]->annotation().type->toString() + "."
					);
				}
				returnTypes.push_back(arguments.at(0)->annotation().type->mobileType());
				break;
			}
			case FunctionType::Kind::TVMBuilderStore:
				typeCheckTvmEncodeFunctions(_functionCall);
				break;
			case FunctionType::Kind::ABIEncode:
			case FunctionType::Kind::ABIEncodePacked:
			case FunctionType::Kind::ABIEncodeWithSelector:
			case FunctionType::Kind::ABIEncodeWithSignature:
			{
				typeCheckABIEncodeFunctions(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::MetaType:
				returnTypes = typeCheckMetaTypeFunctionAndRetrieveReturnType(_functionCall);
				break;
			case FunctionType::Kind::MappingGetNextKey:
			case FunctionType::Kind::MappingGetMinMax:
			case FunctionType::Kind::MappingDelMinOrMax:
			{

				auto memberAccess = dynamic_cast<const MemberAccess *>(&_functionCall.expression());
				auto mapType = dynamic_cast<const MappingType *>(memberAccess->expression().annotation().type);
				const Type * keyType;
				const Type * valueType;
				if (mapType == nullptr) {
					auto eccType = dynamic_cast<const ExtraCurrencyCollectionType *>(memberAccess->expression().annotation().type);
					keyType = eccType->keyType();
					valueType = eccType->valueType();
				} else {
					auto strOrBytesType = dynamic_cast<ArrayType const*>(mapType->keyType());
					if ((strOrBytesType != nullptr && strOrBytesType->isByteArray()) ||
						mapType->keyType()->category() == Type::Category::TvmCell
					) {
						keyType = TypeProvider::uint256();
					} else {
						keyType = mapType->keyType();
					}
					valueType = mapType->valueType();
				}
				if (functionType->kind() == FunctionType::Kind::MappingGetNextKey) {
					if (arguments.size() != 1) {
						m_errorReporter.typeError(_functionCall.location(), "Expected one argument.");
					} else {
						auto arg0Type = arguments[0]->annotation().type;
						if (keyType->category() == Type::Category::Integer) {
							auto category = arg0Type->category();
							if (category != Type::Category::Integer && category != Type::Category::RationalNumber) {
								m_errorReporter.fatalTypeError(
										_functionCall.location(),
										"Expected an integer type."
								);
							}
						} else if (!arg0Type->isImplicitlyConvertibleTo(*keyType)) {
							auto errorMsg = "Type " +
											arg0Type->toString() +
											" is not implicitly convertible to expected type " +
											keyType->toString() + ".";
							m_errorReporter.typeError(arguments[0]->location(), errorMsg);
						}
					}
					auto arrKey = dynamic_cast<ArrayType const*>(keyType);
					if (arrKey != nullptr && arrKey->isByteArray()) { // string or bytes
						paramTypes.push_back(TypeProvider::uint256());
					} else if (keyType->category() == Type::Category::Integer) {
						paramTypes.push_back(TypeProvider::integer(257, IntegerType::Modifier::Signed));
					} else {
						paramTypes.push_back(keyType);
					}
				} else {
					if (!arguments.empty()) {
						m_errorReporter.typeError(arguments[0]->location(), "Expected no arguments.");
					}
				}
				std::vector<Type const*> members = {keyType, valueType};
				returnTypes.push_back(TypeProvider::optional(TypeProvider::tuple(members)));
				break;
			}
			case FunctionType::Kind::Format: {
				auto cat = arguments[0]->annotation().type->category();
				if (cat != Type::Category::StringLiteral) {
					m_errorReporter.fatalTypeError(
							arguments[0]->location(),
							string("First parameter should be a literal string")
					);
				}
				auto lit = dynamic_cast<const Literal *>(arguments[0].get());
				std::string format = lit->value();
				size_t placeholdersCnt = 0;
				for (int i = 0; i < static_cast<int>(format.size()) - 1; i++) {
					if (format[i] == '{') {
						auto c = format[i+1];
						if (c == '}' || c == ':')
							placeholdersCnt++;
					}
				}
				if (arguments.size() != placeholdersCnt + 1) {
					m_errorReporter.fatalTypeError(
							_functionCall.location(),
							string("Number of arguments is not equal to the number of placeholders!")
					);
				}
				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::TVMBuildIntMsg: {
				checkHaveNamedParams();

				for (const std::string name : {"dest", "call", "value"}) {
					int index = findName(name);
					if (index == -1) {
						m_errorReporter.typeError(
								_functionCall.location(),
								string("Parameter \"" + name + "\" must be set.")
						);
					}
				}

				if (
					int index = findName("call");
					index != -1
				) {
					auto callList = dynamic_cast<CallList const*>(arguments.at(index).get());
					if (callList) {
						// now, we ignore constructor call, because in this case we must set stateInit
						checkPubFunctionAndGetDefinition(*callList->function().get(), true);

						std::vector<ASTPointer<Expression const>> params;
						params.push_back(callList->function());
						for (const ASTPointer<Expression const>& p : callList->arguments()) {
							params.push_back(p);
						}
						checkCallList(params, _functionCall, false);
					}
				}

				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::TVMBuildExtMsg: {
				checkHaveNamedParams();
				for (const std::string name: {"dest", "call", "callbackId", "abiVer", "onErrorId"}) {
					int index = findName(name);
					if (index == -1){
						m_errorReporter.fatalTypeError(
								_functionCall.location(),
								"Parameter \"" + name + "\" must be set."
						);
					}
				}

				if (
					int SignIndex = findName("sign");
					SignIndex != -1
				){
					auto ann = arguments[SignIndex]->annotation();
					if (ann.type->category() != Type::Category::Bool || !ann.isPure) {
						m_errorReporter.typeError(
								arguments[SignIndex]->location(),
								"\"sign\" parameter must have a constant boolean type."
						);
					}
				}

				if (
					int index = findName("call");
					index != -1
				) {
					auto callList = dynamic_cast<CallList const*>(arguments.at(index).get());
					if (callList) {
						std::vector<ASTPointer<Expression const>> params;
						params.push_back(callList->function());
						for (const ASTPointer<Expression const>& p : callList->arguments()) {
							params.push_back(p);
						}
						checkCallList(params, _functionCall, true);
					}
				}

				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::TVMBuildStateInit: {
				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = m_evmVersion.supportsReturndata() ?
							  functionType->returnParameterTypes() :
							  functionType->returnParameterTypesWithoutDynamicTypes();

				typeCheckTVMBuildStateInit(_functionCall, hasName, findName);
				break;
			}
			case FunctionType::Kind::TVMTransfer: {
				bool hasValue = false;
				if (!argumentNames.empty()) {
					hasValue = std::any_of(argumentNames.begin(), argumentNames.end(),
					  [](const ASTPointer<ASTString> &name) {
							return *name == "value";
					});
				} else {
					hasValue = !_functionCall.arguments().empty();
				}
				if (!hasValue) {
					m_errorReporter.fatalTypeError(
							_functionCall.location(),
							string("Parameter \"value\" must be set.")
					);
				}
				// parameter names are checked in function below
				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = m_evmVersion.supportsReturndata() ?
							  functionType->returnParameterTypes() :
							  functionType->returnParameterTypesWithoutDynamicTypes();
				break;
			}
			case FunctionType::Kind::TVMFunctionId: {
				if (arguments.size() != 1) {
					m_errorReporter.fatalTypeError(
						_functionCall.location(),
						"One argument of function type is expected."
					);
				}
				checkPubFunctionOrContractTypeAndGetDefinition(*arguments.front().get());
				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::TVMEncodeBody:
			{
				checkCallList(arguments, _functionCall, false);

				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::LogTVM: {
				if (arguments.size() != 1) {
					m_errorReporter.typeError(
						_functionCall.location(),
						"Expected one argument."
					);
				} else {
					Type const *type = arguments.at(0)->annotation().type;
					auto strLiteral = dynamic_cast<StringLiteralType const *>(type);
					auto toArray = dynamic_cast<ArrayType const *>(type);
					std::string errMsg;
					if (strLiteral) {
						if (strLiteral->value().size() > 127)
							errMsg = "String literal must not be longer than 127 characters.";
					} else if (!toArray || !toArray->isString()) {
						errMsg = "Expected a string literal, but got " + type->toString() + ".";
					}
					if (!errMsg.empty()) {
						m_errorReporter.typeError(arguments.at(0)->location(), errMsg);
					} else {
						typeCheckFunctionCall(_functionCall, functionType);
					}
				}
				returnTypes = functionType->returnParameterTypes();
				break;
			}
			case FunctionType::Kind::Require: {
				paramTypes.push_back(TypeProvider::boolean());
				if (arguments.empty()) {
					m_errorReporter.fatalTypeError(_functionCall.location(), "Expected at least one argument.");
				}
				if (arguments.size() >= 2) {
					paramTypes.push_back(TypeProvider::uint256());
				}
				if (arguments.size() >= 3) {
					paramTypes.push_back(arguments.at(2)->annotation().type->mobileType());
				}
				if (arguments.size() >= 4) {
					m_errorReporter.typeError(_functionCall.location(), "Expected at most 3 arguments.");
				}
				checkArgConversion();
				break;
			}
			case FunctionType::Kind::Revert: {
				if (!arguments.empty()) {
					paramTypes.push_back(TypeProvider::uint256());
				}
				if (arguments.size() >= 2) {
					paramTypes.push_back(arguments.at(1)->annotation().type->mobileType());
				}
				if (arguments.size() >= 3) {
					m_errorReporter.typeError(_functionCall.location(), "Expected at most 2 arguments.");
				}
				checkArgConversion();
				break;
			}
			case FunctionType::Kind::TVMDump: {
				if (arguments.size() != 1) {
					m_errorReporter.typeError(_functionCall.location(), "Expected one argument.");
				}
				auto type = arguments[0]->annotation().type->mobileType();
				auto cat = type->category();
				if (cat != Type::Category::Integer && cat !=Type::Category::TvmCell) {
					m_errorReporter.fatalTypeError(
							arguments[0]->location(),
							"Argument must have a TvmCell or integer type."
					);
				}
				paramTypes.push_back(type);
				break;
			}
			default:
			{
				typeCheckFunctionCall(_functionCall, functionType);
				returnTypes = functionType->returnParameterTypes();
				break;
			}
		}
		funcCallAnno.arguments->targetTypes = paramTypes;
		funcCallAnno.type = returnTypes.size() == 1 ?
			returnTypes.front() :
			TypeProvider::tuple(move(returnTypes));
		break;
	}

	case FunctionCallKind::Unset: // fall-through
	default:
		// for non-callables, ensure error reported and annotate node to void function
		solAssert(m_errorReporter.hasErrors(), "");
		funcCallAnno.kind = FunctionCallKind::FunctionCall;
		funcCallAnno.type = TypeProvider::emptyTuple();
		break;
	}

	if (functionType != nullptr &&
		dynamic_cast<FunctionCallOptions const*>(&_functionCall.expression()) == nullptr &&
		functionType->kind() == FunctionType::Kind::External)
	{
		checkNeedCallback(functionType, _functionCall);
	}

	return false;
}

bool TypeChecker::visit(FunctionCallOptions const& _functionCallOptions)
{
	const vector<ASTPointer<const Expression>> &options = _functionCallOptions.options();
	solAssert(options.size() == _functionCallOptions.names().size(), "Lengths of name & value arrays differ!");

	_functionCallOptions.expression().accept(*this);

	auto expressionFunctionType = dynamic_cast<FunctionType const*>(type(_functionCallOptions.expression()));
	if (!expressionFunctionType)
	{
		m_errorReporter.fatalTypeError(_functionCallOptions.location(), "Expected callable expression before call options.");
		return false;
	}

	int setBounce = -1;
	int setCurrencies = -1;
	int setFlag = -1;
	int setPubkey = -1;
	int setSplitDepth = -1;
	int setStateInit = -1;
	int setCode = -1;
	int setValue = -1;
	int setVarInit = -1;
	int setWid = -1;
	int setCallback = -1;
	int setSign = -1;
	int setExtMsg = -1;
	int setExpire = -1;
	int setTime = -1;
	int setAbi = -1;
	int setOnError = -1;
    int setSignHandler = -1;

	FunctionType::Kind kind = expressionFunctionType->kind();
	if (
		kind != FunctionType::Kind::Creation &&
		kind != FunctionType::Kind::External &&
		kind != FunctionType::Kind::BareCall &&
		kind != FunctionType::Kind::BareCallCode &&
		kind != FunctionType::Kind::BareDelegateCall &&
		kind != FunctionType::Kind::BareStaticCall
	)
	{
		m_errorReporter.fatalTypeError(
			_functionCallOptions.location(),
			"Function call options can only be set on external function calls or contract creations."
		);
		return false;
	}

	auto setCheckOption = [&](int& _option, string const&& _name, int index)
	{
		if (_option != -1)
			m_errorReporter.typeError(
				_functionCallOptions.location(),
				"Duplicate option \"" + std::move(_name) + "\"."
			);

		_option = index;
	};

	const bool isNewExpression = dynamic_cast<const NewExpression *>(&_functionCallOptions.expression()) != nullptr;
	auto names = _functionCallOptions.names();
	auto extMsg = find_if(names.begin(), names.end(), [](auto el){
		return *el == "extMsg";
	});
	const bool isExternalInboundMessage = extMsg != names.end();
	if (isExternalInboundMessage) {
		size_t index = extMsg - names.begin();
		options[index]->accept(*this);
		auto lit = dynamic_cast<const Literal *>(options[index].get());
		if (lit == nullptr || lit->token() != Token::TrueLiteral)
			m_errorReporter.typeError(
				_functionCallOptions.location(),
				R"(Option "extMsg" can be specified only with constant true bool value.)");
	}
	for (size_t i = 0; i < names.size(); ++i) {
		options[i]->accept(*this);
		string const &name = *(names[i]);

		std::vector<std::string> arr;
		auto fold = [&](){
			std::string s;
			for (size_t i = 0; i < arr.size(); ++i) {
				if (i + 1 == arr.size())
					s += " and ";
				else if (i > 0)
					s += ", ";
				s += arr[i];
			}
			return s;
		};

		if (isExternalInboundMessage) {
			arr = {"extMsg", "time", "expire", "call", "sign",  "pubkey", "abiVer", "callbackId", "onErrorId", "stateInit", "signBoxHandle"};
		} else if (isNewExpression) {
			arr = {"stateInit", "code", "data", "pubkey", "varInit", "splitDepth", "wid", "value", "currencies", "bounce", "flag"};
		} else {
			arr = {"value", "currencies", "bounce", "flag", "callback"};
		}

		// check option is appropriate
		if (std::find(arr.begin(), arr.end(), name) == arr.end()) {
			m_errorReporter.typeError(
					_functionCallOptions.location(),
					"Unknown option \"" + name + "\". " +
					 "Valid options are " + fold() + "."
			);
		// check type of option
		} else if (name == "extMsg") {
			expectType(*options[i], *TypeProvider::boolean());
			setCheckOption(setExtMsg, "extMsg", i);
		} else if (name == "sign") {
			expectType(*options[i], *TypeProvider::boolean());
			setCheckOption(setSign, "sign", i);
			if (!options[i]->annotation().isPure)
				m_errorReporter.typeError(
						_functionCallOptions.location(),
						R"(Option "sign" can be specified only with constant bool value.)");
		} else if (name == "pubkey") {
            if (isExternalInboundMessage)
                expectType(*options[i], *TypeProvider::optional(TypeProvider::uint256()));
            else
                expectType(*options[i], *TypeProvider::uint256());
            setCheckOption(setPubkey, "pubkey", i);
        } else if (name == "signBoxHandle") {
            expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(32)));
            setCheckOption(setSignHandler, "signBoxHandle", i);
		} else if (name == "abiVer") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(8)));
			setCheckOption(setAbi, "abiVer", i);
		} else if (name == "stateInit") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::tvmcell()));
			setCheckOption(setStateInit, "stateInit", i);
		} else if (name == "callbackId") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(32)));
			setCheckOption(setCallback, "callbackId", i);
		} else if (name == "onErrorId") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(32)));
			setCheckOption(setOnError, "onErrorId", i);
		} else if (name == "expire") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(32)));
			setCheckOption(setExpire, "expire", i);
		} else if (name == "time") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(64)));
			setCheckOption(setTime, "time", i);
		} else if (name == "bounce") {
			expectType(*options[i], *TypeProvider::boolean());
			setCheckOption(setBounce, "bounce", i);
		} else if (name == "flag") {
			expectType(*options[i], *TypeProvider::uint(16));
			setCheckOption(setFlag, "flag", i);
		} else if (name == "currencies") {
			expectType(*options[i], *TypeProvider::extraCurrencyCollection());
			setCheckOption(setCurrencies, "currencies", i);
		} else if (name == "wid") {
			expectType(*options[i], *TypeProvider::integer(8, IntegerType::Modifier::Signed));
			setCheckOption(setWid, "wid", i);
		} else if (name == "code") {
			expectType(*options[i], *TypeProvider::tvmcell());
			setCheckOption(setCode, "code", i);
		} else if (name == "varInit") {
			expectType(*options[i], *TypeProvider::initializerList());
			setCheckOption(setVarInit, "varInit", i);
		} else if (name == "value") {
			expectType(*options[i], *TypeProvider::uint(128));
			setCheckOption(setValue, "value", i);
		} else if (name == "splitDepth") {
			expectType(*options[i], *TypeProvider::uint(8));
			setCheckOption(setSplitDepth, "splitDepth", i);
		} else if (name == "callback") {
			typeCheckCallBack(expressionFunctionType, *options.at(i).get());
			setCheckOption(setCallback, "callback", i);
		} else {
			solUnimplemented("");
		}
	}

	if (isExternalInboundMessage && (setCallback == -1 || setAbi == -1 || setOnError == -1)) {
		m_errorReporter.typeError(
			_functionCallOptions.location(),
			R"("callbackId", "onErrorId" and "abiVer" options must be set.)"
		);
	}
	if (!isExternalInboundMessage && setCallback == -1 && !isNewExpression && !expressionFunctionType->returnParameterTypes().empty()) {
		checkNeedCallback(expressionFunctionType, _functionCallOptions);
	}

	if (isNewExpression) {
		if (setStateInit == -1 && setCode == -1) {
			m_errorReporter.typeError(_functionCallOptions.location(), R"(Either option "stateInit" or option "code" must be set.)");
		}
		if (setStateInit != -1 && setPubkey != -1) {
			m_errorReporter.declarationError(
					options.at(setPubkey)->location(),
					SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", options.at(setStateInit)->location()),
					R"(Option "pubkey" is not compatible with option "stateInit". Only with option "code".)"
			);
		}
		if (setStateInit != -1 && setVarInit != -1) {
			m_errorReporter.declarationError(
				options.at(setVarInit)->location(),
				SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", options.at(setStateInit)->location()),
				R"(Option "varInit" is not compatible with option "stateInit". Only with option "code".)"
			);
		}
		if (setValue == -1) {
			m_errorReporter.typeError(_functionCallOptions.location(), R"(Option "value" must be set.)");
		}
		if (setVarInit != -1) {
			auto newExpr = to<NewExpression>(&_functionCallOptions.expression());
			const TypePointer type = newExpr->typeName().annotation().type;
			auto ct = to<ContractType>(type);
			auto list = dynamic_cast<InitializerList const*>(options.at(setVarInit).get());
			checkInitList(list, ct);
		}
	}

	_functionCallOptions.annotation().type = expressionFunctionType->copyAndSetCallOptions(setValue);
	return false;
}

void TypeChecker::endVisit(NewExpression const& _newExpression)
{
	TypePointer type = _newExpression.typeName().annotation().type;
	solAssert(!!type, "Type name not resolved.");

	if (auto contractName = dynamic_cast<UserDefinedTypeName const*>(&_newExpression.typeName()))
	{
		auto contract = dynamic_cast<ContractDefinition const*>(&dereference(*contractName));

		if (!contract)
			m_errorReporter.fatalTypeError(_newExpression.location(), "Identifier is not a contract.");
		if (contract->isInterface())
			m_errorReporter.fatalTypeError(_newExpression.location(), "Cannot instantiate an interface.");
		if (!contract->constructorIsPublic())
			m_errorReporter.typeError(_newExpression.location(), "Contract with internal constructor cannot be created directly.");
		if (contract->abstract())
			m_errorReporter.typeError(_newExpression.location(), "Cannot instantiate an abstract contract.");

		solAssert(!!m_scope, "");
		m_scope->annotation().contractDependencies.insert(contract);
		solAssert(
			!contract->annotation().linearizedBaseContracts.empty(),
			"Linearized base contracts not yet available."
		);

		_newExpression.annotation().type = FunctionType::newExpressionType(*contract);
	}
	else if (type->category() == Type::Category::Array)
	{
		if (!type->canLiveOutsideStorage())
			m_errorReporter.fatalTypeError(
				_newExpression.typeName().location(),
				"Type cannot live outside storage."
			);
		if (!type->isDynamicallySized())
			m_errorReporter.typeError(
				_newExpression.typeName().location(),
				"Length has to be placed in parentheses after the array type for new expression."
			);
		type = TypeProvider::withLocationIfReference(type);
		_newExpression.annotation().type = TypeProvider::function(
			TypePointers{TypeProvider::uint256()},
			TypePointers{type},
			strings(1, ""),
			strings(1, ""),
			FunctionType::Kind::ObjectCreation,
			false,
			StateMutability::Pure
		);
		_newExpression.annotation().isPure = true;
	}
	else
		m_errorReporter.fatalTypeError(_newExpression.location(), "Contract or array type expected.");
}

bool TypeChecker::visit(MemberAccess const& _memberAccess)
{
	_memberAccess.expression().accept(*this);
	TypePointer exprType = type(_memberAccess.expression());
	ASTString const& memberName = _memberAccess.memberName();

	// Retrieve the types of the arguments if this is used to call a function.
	auto const& arguments = _memberAccess.annotation().arguments;
	MemberList::MemberMap possibleMembers = exprType->members(m_scope).membersByName(memberName);
	size_t const initialMemberCount = possibleMembers.size();
	if (initialMemberCount > 1 && arguments)
	{
		// do overload resolution
		for (auto it = possibleMembers.begin(); it != possibleMembers.end();)
			if (
				it->type->category() == Type::Category::Function &&
				!dynamic_cast<FunctionType const&>(*it->type).canTakeArguments(*arguments, exprType)
			)
				it = possibleMembers.erase(it);
			else
				++it;
	}

	auto& annotation = _memberAccess.annotation();

	if (possibleMembers.empty())
	{
		if (initialMemberCount == 0)
		{
			// Try to see if the member was removed because it is only available for storage types.
			auto storageType = TypeProvider::withLocationIfReference(
				exprType
			);
			if (!storageType->members(m_scope).membersByName(memberName).empty())
				m_errorReporter.fatalTypeError(
					_memberAccess.location(),
					"Member \"" + memberName + "\" is not available in " +
					exprType->toString() +
					" outside of storage."
				);
		}
		string errorMsg = "Member \"" + memberName + "\" not found or not visible "
				"after argument-dependent lookup in " + exprType->toString() + ".";

		if (auto const& funType = dynamic_cast<FunctionType const*>(exprType))
		{
			auto const& t = funType->returnParameterTypes();

			if (memberName == "value")
			{
				if (funType->kind() == FunctionType::Kind::Creation)
					errorMsg = "Constructor for " + t.front()->toString() + " must be payable for member \"value\" to be available.";
				else if (
					funType->kind() == FunctionType::Kind::DelegateCall ||
					funType->kind() == FunctionType::Kind::BareDelegateCall
				)
					errorMsg = "Member \"value\" is not allowed in delegated calls due to \"msg.value\" persisting.";
				else
					errorMsg = "Member \"value\" is only available for payable functions.";
			}
			else if (
				t.size() == 1 &&
				(t.front()->category() == Type::Category::Struct ||
				t.front()->category() == Type::Category::Contract)
			)
				errorMsg += " Did you intend to call the function?";
		}
		else if (exprType->category() == Type::Category::Contract)
		{
			for (auto const& addressMember: TypeProvider::address()->nativeMembers(nullptr))
				if (addressMember.name == memberName)
				{
					Identifier const* var = dynamic_cast<Identifier const*>(&_memberAccess.expression());
					string varName = var ? var->name() : "...";
					errorMsg += " Use \"address(" + varName + ")." + memberName + "\" to access this address member.";
					break;
				}
		}

		m_errorReporter.fatalTypeError(
			_memberAccess.location(),
			errorMsg
		);
	}
	else if (possibleMembers.size() > 1)
		m_errorReporter.fatalTypeError(
			_memberAccess.location(),
			"Member \"" + memberName + "\" not unique "
			"after argument-dependent lookup in " + exprType->toString() +
			(memberName == "value" ? " - did you forget the \"payable\" modifier?" : ".")
		);

	annotation.referencedDeclaration = possibleMembers.front().declaration;
	annotation.type = possibleMembers.front().type;

	if (auto funType = dynamic_cast<FunctionType const*>(annotation.type))
		solAssert(
			!funType->bound() || exprType->isImplicitlyConvertibleTo(*funType->selfType()),
			"Function \"" + memberName + "\" cannot be called on an object of type " +
			exprType->toString() + " (expected " + funType->selfType()->toString() + ")."
		);

	if (dynamic_cast<StructType const*>(exprType))
		annotation.isLValue = true;
	else if (exprType->category() == Type::Category::Array)
		annotation.isLValue = false;
	else if (exprType->category() == Type::Category::FixedBytes)
		annotation.isLValue = false;
	else if (TypeType const* typeType = dynamic_cast<decltype(typeType)>(exprType))
	{
		if (ContractType const* contractType = dynamic_cast<decltype(contractType)>(typeType->actualType()))
			annotation.isLValue = annotation.referencedDeclaration->isLValue();
	}

	// TODO some members might be pure, but for example `address(0x123).balance` is not pure
	// although every subexpression is, so leaving this limited for now.
	if (auto tt = dynamic_cast<TypeType const*>(exprType)) {
		if (tt->actualType()->category() == Type::Category::Enum) {
			annotation.isPure = true;
		} else if (tt->actualType()->category() == Type::Category::Address) {
			if (memberName == "makeAddrStd" || memberName == "makeAddrNone") {
				annotation.isPure = true;
			}
		}
	}
	if (auto magicType = dynamic_cast<MagicType const*>(exprType))
	{
		if (magicType->kind() == MagicType::Kind::ABI)
			annotation.isPure = true;
		else if (magicType->kind() == MagicType::Kind::MetaType && (
			memberName == "creationCode" || memberName == "runtimeCode"
		))
		{
			annotation.isPure = true;
			m_scope->annotation().contractDependencies.insert(
				&dynamic_cast<ContractType const&>(*magicType->typeArgument()).contractDefinition()
			);
			if (contractDependenciesAreCyclic(*m_scope))
				m_errorReporter.typeError(
					_memberAccess.location(),
					"Circular reference for contract code access."
				);
		}
		else if (magicType->kind() == MagicType::Kind::MetaType && memberName == "name")
			annotation.isPure = true;
	}

	return false;
}

bool TypeChecker::visit(IndexAccess const& _access)
{
	_access.baseExpression().accept(*this);
	TypePointer baseType = type(_access.baseExpression());
	TypePointer resultType = nullptr;
	bool isLValue = false;
	bool isPure = _access.baseExpression().annotation().isPure;
	Expression const* index = _access.indexExpression();
	switch (baseType->category())
	{
	case Type::Category::ArraySlice:
	{
		auto const& arrayType = dynamic_cast<ArraySliceType const&>(*baseType).arrayType();
		if (!arrayType.isDynamicallySized())
			m_errorReporter.typeError(_access.location(), "Index access is only implemented for slices of dynamic calldata arrays.");
		baseType = &arrayType;
		[[fallthrough]];
	}
	case Type::Category::Array:
	{
		ArrayType const& actualType = dynamic_cast<ArrayType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(_access.location(), "Index expression cannot be omitted.");
		else if (actualType.isString())
		{
			m_errorReporter.typeError(_access.location(), "Index access for string is not possible.");
			index->accept(*this);
		}
		else
		{
			expectType(*index, *TypeProvider::uint256());
			if (!m_errorReporter.hasErrors())
				if (auto numberType = dynamic_cast<RationalNumberType const*>(type(*index)))
				{
					solAssert(!numberType->isFractional(), "");
					if (!actualType.isDynamicallySized() && actualType.length() <= numberType->literalValue(nullptr))
						m_errorReporter.typeError(_access.location(), "Out of bounds array access.");
				}
		}
		resultType = actualType.baseType();
		isLValue = true;
		break;
	}
	case Type::Category::Mapping:
	{
		MappingType const& actualType = dynamic_cast<MappingType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(_access.location(), "Index expression cannot be omitted.");
		else
			expectType(*index, *actualType.keyType());
		resultType = actualType.valueType();
		isLValue = true;
		break;
	}
	case Type::Category::ExtraCurrencyCollection:
	{
		ExtraCurrencyCollectionType const& actualType = dynamic_cast<ExtraCurrencyCollectionType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(_access.location(), "Index expression cannot be omitted.");
		else
			expectType(*index, *actualType.keyType());
		resultType = actualType.valueType();
		isLValue = true;
		break;
	}
	case Type::Category::TypeType:
	{
		TypeType const& typeType = dynamic_cast<TypeType const&>(*baseType);
		if (dynamic_cast<ContractType const*>(typeType.actualType()))
			m_errorReporter.typeError(_access.location(), "Index access for contracts or libraries is not possible.");
		if (!index)
			resultType = TypeProvider::typeType(TypeProvider::array(typeType.actualType()));
		else
		{
			u256 length = 1;
			if (expectType(*index, *TypeProvider::uint256()))
			{
				if (auto indexValue = dynamic_cast<RationalNumberType const*>(type(*index)))
					length = indexValue->literalValue(nullptr);
				else
					m_errorReporter.fatalTypeError(index->location(), "Integer constant expected.");
			}
			else
				solAssert(m_errorReporter.hasErrors(), "Expected errors as expectType returned false");

			resultType = TypeProvider::typeType(TypeProvider::array(
				typeType.actualType(),
				length
			));
		}
		break;
	}
	case Type::Category::FixedBytes:
	{
		FixedBytesType const& bytesType = dynamic_cast<FixedBytesType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(_access.location(), "Index expression cannot be omitted.");
		else
		{
			if (!expectType(*index, *TypeProvider::uint256()))
				m_errorReporter.fatalTypeError(_access.location(), "Index expression cannot be represented as an unsigned integer.");
			if (auto integerType = dynamic_cast<RationalNumberType const*>(type(*index)))
				if (bytesType.numBytes() <= integerType->literalValue(nullptr))
					m_errorReporter.typeError(_access.location(), "Out of bounds array access.");
		}
		resultType = TypeProvider::fixedBytes(1);
		isLValue = false; // @todo this heavily depends on how it is embedded
		break;
	}
	case Type::Category::TvmTuple:
	{
		TvmTupleType const& actualType = dynamic_cast<TvmTupleType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(_access.location(), "Index expression cannot be omitted.");
		else
		{
			expectType(*index, *TypeProvider::uint256());
		}
		resultType = actualType.valueType();
		isLValue = false;
		break;
	}
	default:
		m_errorReporter.fatalTypeError(
			_access.baseExpression().location(),
			"Indexed expression has to be a type, mapping or array (is " + baseType->toString() + ")"
		);
	}
	_access.annotation().type = resultType;
	_access.annotation().isLValue = isLValue;
	if (index && !index->annotation().isPure)
		isPure = false;
	_access.annotation().isPure = isPure;

	return false;
}

bool TypeChecker::visit(IndexRangeAccess const& _access)
{
	_access.baseExpression().accept(*this);

	bool isLValue = false; // TODO: set this correctly when implementing slices for memory and storage arrays
	bool isPure = _access.baseExpression().annotation().isPure;

	if (Expression const* start = _access.startExpression())
	{
		expectType(*start, *TypeProvider::uint256());
		if (!start->annotation().isPure)
			isPure = false;
	}
	if (Expression const* end = _access.endExpression())
	{
		expectType(*end, *TypeProvider::uint256());
		if (!end->annotation().isPure)
			isPure = false;
	}

	TypePointer exprType = type(_access.baseExpression());
	if (exprType->category() == Type::Category::TypeType)
	{
		m_errorReporter.typeError(_access.location(), "Types cannot be sliced.");
		_access.annotation().type = exprType;
		return false;
	}

	ArrayType const* arrayType = nullptr;
	if (auto const* arraySlice = dynamic_cast<ArraySliceType const*>(exprType))
		arrayType = &arraySlice->arrayType();
	else if (!(arrayType = dynamic_cast<ArrayType const*>(exprType)))
		m_errorReporter.fatalTypeError(_access.location(), "Index range access is only possible for arrays and array slices.");


	if (!arrayType->isDynamicallySized())
		m_errorReporter.typeError(_access.location(), "Index range access is only supported for dynamic calldata arrays.");
	_access.annotation().type = TypeProvider::arraySlice(*arrayType);
	_access.annotation().isLValue = isLValue;
	_access.annotation().isPure = isPure;

	return false;
}

bool TypeChecker::visit(Identifier const& _identifier)
{
	IdentifierAnnotation& annotation = _identifier.annotation();
	if (!annotation.referencedDeclaration)
	{
		if (!annotation.arguments)
		{
			// The identifier should be a public state variable shadowing other functions
			vector<Declaration const*> candidates;

			for (Declaration const* declaration: annotation.overloadedDeclarations)
			{
				if (VariableDeclaration const* variableDeclaration = dynamic_cast<decltype(variableDeclaration)>(declaration))
					candidates.push_back(declaration);
			}
			if (candidates.empty())
				m_errorReporter.fatalTypeError(_identifier.location(), "No matching declaration found after variable lookup.");
			else if (candidates.size() == 1)
				annotation.referencedDeclaration = candidates.front();
			else
				m_errorReporter.fatalTypeError(_identifier.location(), "No unique declaration found after variable lookup.");
		}
		else if (annotation.overloadedDeclarations.empty())
			m_errorReporter.fatalTypeError(_identifier.location(), "No candidates for overload resolution found.");
		else if (annotation.overloadedDeclarations.size() == 1)
			annotation.referencedDeclaration = *annotation.overloadedDeclarations.begin();
		else
		{
			vector<Declaration const*> candidates;

			for (Declaration const* declaration: annotation.overloadedDeclarations)
			{
				FunctionTypePointer functionType = declaration->functionType(true);
				solAssert(!!functionType, "Requested type not present.");
				if (functionType->canTakeArguments(*annotation.arguments))
					candidates.push_back(declaration);
			}
			if (candidates.size() == 1)
				annotation.referencedDeclaration = candidates.front();
			else
			{
				SecondarySourceLocation ssl;

				for (Declaration const* declaration: annotation.overloadedDeclarations)
					if (!declaration->location().isValid())
					{
						// Try to re-construct function definition
						string description;
						for (auto const& param: declaration->functionType(true)->parameterTypes())
							description += (description.empty() ? "" : ", ") + param->toString(false);
						description = "function " + _identifier.name() + "(" + description + ")";

						ssl.append("Candidate: " + description, declaration->location());
					}
					else
						ssl.append("Candidate:", declaration->location());
				if (candidates.empty())
					m_errorReporter.fatalTypeError(_identifier.location(), ssl, "No matching declaration found after argument-dependent lookup.");
				else
					m_errorReporter.fatalTypeError(_identifier.location(), ssl, "No unique declaration found after argument-dependent lookup.");
			}
		}
	}
	solAssert(
		!!annotation.referencedDeclaration,
		"Referenced declaration is null after overload resolution."
	);
	annotation.isLValue = annotation.referencedDeclaration->isLValue();
	annotation.type = annotation.referencedDeclaration->type();
	solAssert(annotation.type, "Declaration referenced before type could be determined.");
	if (auto variableDeclaration = dynamic_cast<VariableDeclaration const*>(annotation.referencedDeclaration))
		annotation.isPure = annotation.isConstant = variableDeclaration->isConstant();
	else if (dynamic_cast<MagicVariableDeclaration const*>(annotation.referencedDeclaration))
	{
		if (dynamic_cast<FunctionType const*>(annotation.type))
			annotation.isPure = true;
	}
	else if (dynamic_cast<TypeType const*>(annotation.type))
		annotation.isPure = true;


	// Check for deprecated function names.
	// The check is done here for the case without an actual function call.
	if (FunctionType const* fType = dynamic_cast<FunctionType const*>(_identifier.annotation().type))
	{
		if (_identifier.name() == "sha3" && fType->kind() == FunctionType::Kind::KECCAK256)
			m_errorReporter.typeError(
				_identifier.location(),
				"\"sha3\" has been deprecated in favour of \"keccak256\"."
			);
		else if (_identifier.name() == "suicide" && fType->kind() == FunctionType::Kind::Selfdestruct)
			m_errorReporter.typeError(
				_identifier.location(),
				"\"suicide\" has been deprecated in favour of \"selfdestruct\"."
			);
	}

	return false;
}

void TypeChecker::endVisit(ElementaryTypeNameExpression const& _expr)
{
	_expr.annotation().type = TypeProvider::typeType(TypeProvider::fromElementaryTypeName(_expr.type().typeName()));
	_expr.annotation().isPure = true;
}

void TypeChecker::endVisit(MappingNameExpression const& _expr)
{
	_expr.annotation().type = TypeProvider::typeType(TypeProvider::mapping(
			_expr.type().keyType().annotation().type,
			_expr.type().valueType().annotation().type
			));
	_expr.annotation().isPure = true;
}

void TypeChecker::endVisit(OptionalNameExpression const& _expr)
{
	_expr.annotation().type = TypeProvider::typeType(_expr.type().annotation().type);
	_expr.annotation().isPure = true;
}

void TypeChecker::endVisit(InitializerList const& _expr) {
	_expr.annotation().type = TypeProvider::initializerList();
	_expr.annotation().isPure = false;
}

void TypeChecker::endVisit(CallList const& _expr) {
	_expr.annotation().type = TypeProvider::callList();
	_expr.annotation().isPure = false;
}

void TypeChecker::endVisit(Literal const& _literal)
{
	if (_literal.isHexNumber() && _literal.subDenomination() != Literal::SubDenomination::None)
		m_errorReporter.fatalTypeError(
			_literal.location(),
			"Hexadecimal numbers cannot be used with unit denominations. "
			"You can use an expression of the form \"0x1234 * 1 day\" instead."
		);

	if (_literal.subDenomination() == Literal::SubDenomination::Year)
		m_errorReporter.typeError(
			_literal.location(),
			"Using \"years\" as a unit denomination is deprecated."
		);

	if (!_literal.annotation().type)
		_literal.annotation().type = TypeProvider::forLiteral(_literal);

	if (!_literal.annotation().type)
		m_errorReporter.fatalTypeError(_literal.location(), "Invalid literal value.");

	_literal.annotation().isPure = true;
}

bool TypeChecker::visit(Mapping const& _mapping)
{
	if (auto const* keyType = dynamic_cast<UserDefinedTypeName const*>(&_mapping.keyType()))
	{
		if (auto const* contractType = dynamic_cast<ContractType const*>(keyType->annotation().type))
		{
			if (contractType->contractDefinition().isLibrary())
				m_errorReporter.typeError(
					keyType->location(),
					"Library types cannot be used as mapping keys."
				);
		}
		else if (
            keyType->annotation().type->category() != Type::Category::Enum &&
            keyType->annotation().type->category() != Type::Category::Struct
        )
			m_errorReporter.typeError(
				keyType->location(),
				"Only elementary types, contract types, structures (fitted in one cell) or enums are allowed as mapping keys."
			);
	}
	else
		solAssert(dynamic_cast<ElementaryTypeName const*>(&_mapping.keyType()), "");
	return true;
}


bool TypeChecker::contractDependenciesAreCyclic(
	ContractDefinition const& _contract,
	std::set<ContractDefinition const*> const& _seenContracts
) const
{
	// Naive depth-first search that remembers nodes already seen.
	if (_seenContracts.count(&_contract))
		return true;
	set<ContractDefinition const*> seen(_seenContracts);
	seen.insert(&_contract);
	for (auto const* c: _contract.annotation().contractDependencies)
		if (contractDependenciesAreCyclic(*c, seen))
			return true;
	return false;
}

Declaration const& TypeChecker::dereference(Identifier const& _identifier) const
{
	solAssert(!!_identifier.annotation().referencedDeclaration, "Declaration not stored.");
	return *_identifier.annotation().referencedDeclaration;
}

Declaration const& TypeChecker::dereference(UserDefinedTypeName const& _typeName) const
{
	solAssert(!!_typeName.annotation().referencedDeclaration, "Declaration not stored.");
	return *_typeName.annotation().referencedDeclaration;
}

bool TypeChecker::expectType(Expression const& _expression, Type const& _expectedType)
{
	_expression.accept(*this);

	BoolResult result = type(_expression)->isImplicitlyConvertibleTo(_expectedType);

	if (!result)
	{
		auto errorMsg = "Type " +
			type(_expression)->toString() +
			" is not implicitly convertible to expected type " +
			_expectedType.toString();
		if (
			type(_expression)->category() == Type::Category::RationalNumber &&
			dynamic_cast<RationalNumberType const*>(type(_expression))->isFractional() &&
			type(_expression)->mobileType()
		)
		{
			if (_expectedType.operator==(*type(_expression)->mobileType()))
				m_errorReporter.typeError(
					_expression.location(),
					errorMsg + ", but it can be explicitly converted."
				);
			else
				m_errorReporter.typeError(
					_expression.location(),
					errorMsg +
					". Try converting to type " +
					type(_expression)->mobileType()->toString() +
					" or use an explicit conversion."
				);
		}
		else
			m_errorReporter.typeError(_expression.location(), errorMsg + ".");
		return false;
	}
	return true;
}

void TypeChecker::requireLValue(Expression const& _expression)
{
	_expression.annotation().lValueRequested = true;
	_expression.accept(*this);

	if (_expression.annotation().isLValue)
		return;

	return m_errorReporter.typeError(_expression.location(), [&]() {
		if (_expression.annotation().isConstant)
			return "Cannot assign to a constant variable.";

		if (auto indexAccess = dynamic_cast<IndexAccess const*>(&_expression))
		{
			if (type(indexAccess->baseExpression())->category() == Type::Category::FixedBytes)
				return "Single bytes in fixed bytes arrays cannot be modified.";
		}

		if (auto memberAccess = dynamic_cast<MemberAccess const*>(&_expression))
		{
			if (dynamic_cast<StructType const*>(type(memberAccess->expression())))
			{
			}
			else if (dynamic_cast<ArrayType const*>(type(memberAccess->expression())))
				if (memberAccess->memberName() == "length")
					return "Member \"length\" is read-only and cannot be used to resize arrays.";
		}

		if (auto identifier = dynamic_cast<Identifier const*>(&_expression))
			if (auto varDecl = dynamic_cast<VariableDeclaration const*>(identifier->annotation().referencedDeclaration))
				if (varDecl->isExternalCallableParameter() && dynamic_cast<ReferenceType const*>(identifier->annotation().type))
					return "External function arguments of reference type are read-only.";

		return "Expression has to be an lvalue.";
	}());
}
