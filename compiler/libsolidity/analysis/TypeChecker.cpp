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
// SPDX-License-Identifier: GPL-3.0
/**
 * @author Christian <c@ethdev.com>
 * @date 2015
 * Type analyzer and checker.
 */

#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTUtils.h>
#include <libsolidity/ast/UserDefinableOperators.h>
#include <libsolidity/ast/TypeProvider.h>

#include <liblangutil/ErrorReporter.h>

#include <libsolutil/Algorithms.h>
#include <libsolutil/StringUtils.h>
#include <libsolutil/Views.h>
#include <libsolutil/Visitor.h>

#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include  <boost/core/ignore_unused.hpp>

#include <fmt/format.h>

#include <range/v3/algorithm/count_if.hpp>
#include <range/v3/view/drop_exactly.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/zip.hpp>

#include <memory>
#include <vector>

#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMConstants.hpp>

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

bool TypeChecker::checkTypeRequirements(SourceUnit const& _source)
{
	m_currentSourceUnit = &_source;
	_source.accept(*this);
	m_currentSourceUnit = nullptr;
	return !Error::containsErrors(m_errorReporter.errors());
}

Type const* TypeChecker::type(Expression const& _expression) const
{
	solAssert(!!_expression.annotation().type, "Type requested but not present.");
	return _expression.annotation().type;
}

Type const* TypeChecker::type(VariableDeclaration const& _variable) const
{
	solAssert(!!_variable.annotation().type, "Type requested but not present.");
	return _variable.annotation().type;
}

bool TypeChecker::visit(ContractDefinition const& _contract)
{
	m_currentContract = &_contract;

	ASTNode::listAccept(_contract.baseContracts(), *this);

	for (auto const& n: _contract.subNodes())
		n->accept(*this);

	m_currentContract = nullptr;

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
	std::vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 1)
		m_errorReporter.typeError(
			7750_error,
			_functionCall.location(),
			"This function takes one argument, but " +
			toString(arguments.size()) +
			" were provided."
		);

	std::set<std::string> availableParams {"1","15","17","34"};
	auto paramNumberLiteral = dynamic_cast<const Literal *>(arguments[0].get());

	if (!paramNumberLiteral)
		m_errorReporter.typeError(
			1961_error,
			_functionCall.location(),
			"This function takes only param number literal."
		);

	Type const* type = paramNumberLiteral->annotation().type;
	u256 value = type->literalValue(paramNumberLiteral);
	std::string paramNumber = value.str();

	if (!availableParams.count(paramNumber))
		m_errorReporter.typeError(
			2100_error,
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
									  TypeProvider::mapping(TypeProvider::integer(16, IntegerType::Modifier::Unsigned), "",
																TypeProvider::tvmslice(), ""),
									  TypeProvider::boolean()};
		return ret;
	}
	solAssert(false, "Unsupported tvm.configParam argument.");
}

TypePointers TypeChecker::typeCheckABIDecodeAndRetrieveReturnType(FunctionCall const& _functionCall, bool _abiEncoderV2)
{
	std::vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 2)
		m_errorReporter.typeError(
			5782_error,
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
				1956_error,
				arguments.front()->location(),
				"The first argument to \"abi.decode\" must be implicitly convertible to "
				"bytes memory or bytes calldata, but is of type " +
				type(*arguments.front())->humanReadableName() +
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
			6444_error,
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
			Type const* actualType = argTypeType->actualType();
			solAssert(actualType, "");
			// We force memory because the parser currently cannot handle
			// data locations. Furthermore, storage can be a little dangerous and
			// calldata is not really implemented anyway.
			if (!actualType->fullEncodingType(false, _abiEncoderV2, false))
				m_errorReporter.typeError(
					9611_error,
					typeArgument->location(),
					"Decoding type " + actualType->humanReadableName() + " not supported."
				);

			components.push_back(actualType);
		}
		else
		{
			m_errorReporter.typeError(1039_error, typeArgument->location(), "Argument has to be a type name.");
			components.push_back(TypeProvider::emptyTuple());
		}
	}
	return components;
}

void TypeChecker::typeCheckABIEncodeStateInit(
	FunctionCall const& _functionCall,
	const std::function<bool(const std::string&)>& hasName,
	const std::function<int(const std::string&)>& findName
) {
	bool hasNames = !_functionCall.names().empty();
	const std::vector<ASTPointer<const Expression>> &args = _functionCall.arguments();
	size_t argCnt = args.size();
	if (!hasNames && argCnt != 3 && argCnt != 2)
		m_errorReporter.typeError(
				6303_error,
				_functionCall.location(),
				std::string("If parameters are set without names, only 2 or 3 arguments can be specified.")
		);

	if (hasNames) {
		bool hasCode = hasName("code");
		bool hasData = hasName("data");
		bool hasVarInit = hasName("varInit");
		bool hasPubkey = hasName("pubkey");
		bool hasContr = hasName("contr");

		if (!hasCode)
			m_errorReporter.typeError(
				6128_error,
				_functionCall.location(),
				std::string("Parameter \"code\" must be set.")
			);
		if (hasData && (hasVarInit || hasPubkey))
			m_errorReporter.typeError(
				6578_error,
				_functionCall.location(),
				std::string(R"(Parameter "data" can't be specified with "pubkey" or "varInit".)")
			);
		if (!hasContr && !hasData)
			m_errorReporter.typeError(
				8123_error,
				_functionCall.location(),
				std::string(R"(Expected parameter "contr" or "data".)")
			);
		if (hasContr) {
			int contrInd = findName("contr");
			const ASTPointer<Expression const>& contrArg = args.at(contrInd);
			ContractType const* ct = getContractType(contrArg.get());
			if (ct == nullptr) {
				m_errorReporter.typeError(
					8286_error,
					contrArg->location(),
					"Expected contract type."
				);
				return ;
			}
			InitializerList const * list{};
			int varInitInd = findName("varInit");
			if (varInitInd != -1)
				list = dynamic_cast<InitializerList const *>(args.at(varInitInd).get());
			checkInitList(list, *ct, _functionCall.location());
		}
	}
}

void TypeChecker::typeCheckABIEncodeData(
	FunctionCall const& _functionCall,
	const std::function<bool(const std::string&)>& hasName,
	const std::function<int(const std::string&)>& findName
) {
	const std::vector<ASTPointer<const Expression>> &args = _functionCall.arguments();

	if (!hasName("contr")) {
		m_errorReporter.typeError(
			2957_error,
			_functionCall.location(),
			std::string(R"(Expected parameter "contr".)")
		);
		return ;
	}

	int contrInd = findName("contr");
	const ASTPointer<Expression const>& contrArg = args.at(contrInd);
	ContractType const* ct = getContractType(contrArg.get());
	if (ct == nullptr) {
		m_errorReporter.typeError(
			9417_error,
			contrArg->location(),
			"Expected contract type."
		);
		return;
	}

	InitializerList const * list{};
	int varInitInd = findName("varInit");
	if (varInitInd != -1) {
		list = dynamic_cast<InitializerList const *>(args.at(varInitInd).get());
	}
	checkInitList(list, *ct, _functionCall.location());
}

void TypeChecker::typeCheckCallBack(FunctionType const* remoteFunction, Expression const& option) {
	auto calleeDefinition = dynamic_cast<FunctionDefinition const*>(&remoteFunction->declaration());
	if (calleeDefinition == nullptr) {
		m_errorReporter.typeError(
				4220_error,
				option.location(),
				R"("callback" option can be used only for function type.)"
		);
		return ;
	}

	if (remoteFunction->returnParameterTypes().empty() || !calleeDefinition->isResponsible()) {
		m_errorReporter.typeError(
				5326_error,
				option.location(),
				SecondarySourceLocation().append("Declaration of callee function", calleeDefinition->location()),
				R"("callback" option can be used only for responsible functions.)"
		);
		return ;
	}

	FunctionDefinition const *callbackFunc = getFunctionDefinition(&option);
	if (callbackFunc == nullptr) {
		m_errorReporter.typeError(
				4191_error,
				option.location(),
				"Expected function type but got " + option.annotation().type->toString() + "."
		);
		return ;
	}

	if (!callbackFunc->returnParameters().empty()) {
		m_errorReporter.typeError(
				5801_error,
				option.location(),
				SecondarySourceLocation()
						.append("Declaration of the callback function:", callbackFunc->location()),
				R"(Callback function must return nothing.)"
		);
		return ;
	}
	checkRemoteAndCallBackFunctions(calleeDefinition, callbackFunc, option.location());
}

TypePointers TypeChecker::checkSliceDecode(std::vector<ASTPointer<Expression const>> const& arguments)
{
	TypePointers components;
	for (auto const& typeArgument: arguments) {
		solAssert(typeArgument, "");
		if (auto const* argTypeType = dynamic_cast<TypeType const*>(type(*typeArgument))) {
			Type const* actualType = argTypeType->actualType();
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
				case Type::Category::Optional:
				case Type::Category::Struct:
				case Type::Category::Enum:
				case Type::Category::VarInteger:
					break;

				default:
					m_errorReporter.typeError(9365_error, typeArgument->location(), "Unsupported type for decoding.");
			}
		} else {
			m_errorReporter.typeError(6318_error, typeArgument->location(), "Argument has to be a type name.");
			components.push_back(TypeProvider::emptyTuple());
		}
	}
	return components;
}

TypePointers TypeChecker::checkSliceDecodeQ(std::vector<ASTPointer<Expression const>> const& arguments)
{
	TypePointers components;
	for (auto const& typeArgument: arguments) {
		solAssert(typeArgument, "");
		if (auto const* argTypeType = dynamic_cast<TypeType const*>(type(*typeArgument))) {
			Type const* actualType = argTypeType->actualType();
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
				case Type::Category::Enum:
					break;

				default:
					m_errorReporter.typeError(7828_error, typeArgument->location(), "Unsupported type for decoding.");
			}
		} else {
			m_errorReporter.typeError(7213_error, typeArgument->location(), "Argument has to be a type name.");
			components.push_back(TypeProvider::emptyTuple());
		}
	}
	return components;
}

TypePointers TypeChecker::typeCheckMetaTypeFunctionAndRetrieveReturnType(FunctionCall const& _functionCall)
{
	std::vector<ASTPointer<Expression const>> arguments = _functionCall.arguments();
	if (arguments.size() != 1)
		m_errorReporter.fatalTypeError(
			8885_error,
			_functionCall.location(),
			"This function takes one argument, but " +
			toString(arguments.size()) +
			" were provided."
		);
	Type const* firstArgType = type(*arguments.front());

	bool wrongType = false;
	if (firstArgType->category() == Type::Category::TypeType)
	{
		TypeType const* typeTypePtr = dynamic_cast<TypeType const*>(firstArgType);
		Type::Category typeCategory = typeTypePtr->actualType()->category();
		if (auto const* contractType = dynamic_cast<ContractType const*>(typeTypePtr->actualType()))
			wrongType = contractType->isSuper();
		else if (
			typeCategory != Type::Category::Integer &&
			typeCategory != Type::Category::VarInteger &&
			typeCategory != Type::Category::Enum
		)
			wrongType = true;
	}
	else
		wrongType = true;

	if (wrongType)
		m_errorReporter.fatalTypeError(
			4259_error,
			arguments.front()->location(),
			"Invalid type for argument in the function call. "
			"An enum type, contract type or an integer type is required, but " +
			type(*arguments.front())->humanReadableName() + " provided."
		);

	return {TypeProvider::meta(dynamic_cast<TypeType const&>(*firstArgType).actualType())};
}

bool TypeChecker::visit(ImportDirective const&)
{
	return false;
}

void TypeChecker::endVisit(InheritanceSpecifier const& _inheritance)
{
	auto base = dynamic_cast<ContractDefinition const*>(&dereference(_inheritance.name()));
	solAssert(base, "Base contract not available.");
	solAssert(m_currentContract, "");

	if (m_currentContract->isInterface() && !base->isInterface())
		m_errorReporter.typeError(6536_error, _inheritance.location(), "Interfaces can only inherit from other interfaces.");

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
				7927_error,
				_inheritance.location(),
				"Wrong argument count for constructor call: " +
				toString(arguments->size()) +
				" arguments given but expected " +
				toString(parameterTypes.size()) +
				(arguments->size() == 0 ? ". Remove parentheses if you do not want to provide arguments here." : "")
			);
		}
		for (size_t i = 0; i < std::min(arguments->size(), parameterTypes.size()); ++i)
		{
			BoolResult result = type(*(*arguments)[i])->isImplicitlyConvertibleTo(*parameterTypes[i]);
			if (!result)
				m_errorReporter.typeErrorConcatenateDescriptions(
					9827_error,
					(*arguments)[i]->location(),
					"Invalid type for argument in constructor call. "
					"Invalid implicit conversion from " +
					type(*(*arguments)[i])->humanReadableName() +
					" to " +
					parameterTypes[i]->humanReadableName() +
					" requested.",
					result.message()
				);
		}
	}
}

void TypeChecker::endVisit(ModifierDefinition const& _modifier)
{
	if (auto const* contractDef = dynamic_cast<ContractDefinition const*>(_modifier.scope()))
	{
		if (_modifier.virtualSemantics() && contractDef->isLibrary())
			m_errorReporter.typeError(
				3275_error,
				_modifier.location(),
				"Modifiers in a library cannot be virtual."
			);

		if (contractDef->isInterface())
			m_errorReporter.typeError(
				6408_error,
				_modifier.location(),
				"Modifiers cannot be defined or declared in interfaces."
			);
	}

	if (!_modifier.isImplemented() && !_modifier.virtualSemantics())
		m_errorReporter.typeError(8063_error, _modifier.location(), "Modifiers without implementation must be marked virtual.");
}

bool TypeChecker::isBadAbiType(
	SourceLocation const& origVarLoc,
	Type const* curType,
	SourceLocation const& curVarLoc,
	std::set<StructDefinition const*>& usedStructs,
	bool doPrintErr
) {
	auto printError = [&](const std::string& message) {
		if (doPrintErr) {
			if (origVarLoc == curVarLoc) {
				m_errorReporter.typeError(2468_error, origVarLoc, message);
			} else {
				m_errorReporter.typeError(
						2321_error,
						origVarLoc,
						SecondarySourceLocation().append("Another declaration is here:", curVarLoc),
						message
				);
			}
		}
	};

	const Type::Category category = curType->category();
	switch (category) {
		case Type::Category::Optional: {
			auto optType = to<OptionalType>(curType);
			switch (optType->valueType()->category()) {
				case Type::Category::Tuple: {
					auto tup = to<TupleType>(optType->valueType());
					for (Type const* t : tup->components()) {
						if (isBadAbiType(origVarLoc, t, curVarLoc, usedStructs, doPrintErr)) {
							return true;
						}
					}
					break;
				}
				default:
					if (isBadAbiType(origVarLoc, optType->valueType(), curVarLoc, usedStructs, doPrintErr)) {
						return true;
					}
			}
			break;
		}
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
			if (isBadAbiType(origVarLoc, mappingType->valueType(), curVarLoc, usedStructs, doPrintErr)) {
				return true;
			}
			break;
		}
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(curType);
			if (!arrayType->isByteArray()) {
				if (isBadAbiType(origVarLoc, arrayType->baseType(), curVarLoc, usedStructs, doPrintErr)) {
					return true;
				}
			}
			break;
		}
		case Type::Category::Struct: {
			auto valueStruct = to<StructType>(curType);
			StructDefinition const& structDefinition = valueStruct->structDefinition();
			if (usedStructs.count(&structDefinition)) {
				if (doPrintErr) {
					m_errorReporter.typeError(
							1107_error,
							origVarLoc,
							SecondarySourceLocation().append("Recursive struct:", structDefinition.location()),
							"ABI doesn't support recursive types."
					);
				}
				return true;
			}
			usedStructs.insert(&structDefinition);
			for (const ASTPointer<VariableDeclaration>& member : structDefinition.members()) {
				if (isBadAbiType(origVarLoc, member->type(), member.get()->location(), usedStructs, doPrintErr))
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
		case Type::Category::VarInteger:
			break;

		case Type::Category::UserDefinedValueType: {
			auto userDefType = dynamic_cast<UserDefinedValueType const*>(curType);
			if (isBadAbiType(origVarLoc, &userDefType->underlyingType(), curVarLoc, usedStructs, doPrintErr)) {
				return true;
			}
			break;
		}

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

	if (_function.isResponsible()) {
		if (_function.returnParameters().empty())
			m_errorReporter.typeError(4438_error, _function.location(), "Responsible function must return at least one value.");
		if (!_function.isPublic())
			m_errorReporter.typeError(4403_error, _function.location(), "Responsible function must have public or external visibility.");
	}

	if (_function.markedVirtual())
	{
		if (_function.isFree())
			m_errorReporter.syntaxError(4493_error, _function.location(), "Free functions cannot be virtual.");
		else if (_function.isConstructor())
			m_errorReporter.typeError(7001_error, _function.location(), "Constructors cannot be virtual.");
		else if (_function.annotation().contract->isInterface())
			m_errorReporter.warning(5815_error, _function.location(), "Interface functions are implicitly \"virtual\"");
		else if (_function.visibility() == Visibility::Private)
			m_errorReporter.typeError(3942_error, _function.location(), "\"virtual\" and \"private\" cannot be used together.");
		else if (_function.libraryFunction())
			m_errorReporter.typeError(7801_error, _function.location(), "Library functions cannot be \"virtual\".");
	}
	if (_function.overrides() && _function.isFree())
		m_errorReporter.syntaxError(1750_error, _function.location(), "Free functions cannot override.");

	if (!_function.modifiers().empty() && _function.isFree())
		m_errorReporter.syntaxError(5811_error, _function.location(), "Free functions cannot have modifiers.");

	if (_function.isExternalMsg() || _function.isInternalMsg()) {
		if (_function.isExternalMsg() && _function.isInternalMsg()) {
			m_errorReporter.typeError(6672_error, _function.location(), R"("internalMsg" and "externalMsg" cannot be used together.)");
		}
		if (!_function.functionIsExternallyVisible()) {
			m_errorReporter.typeError(7446_error, _function.location(), R"(Private/internal function can't be marked as internalMsg/externalMsg.)");
		}
		if (_function.isReceive() || _function.isFallback() || _function.isOnBounce() || _function.isOnTickTock()) {
			m_errorReporter.typeError(1399_error, _function.location(), R"(receiver, fallback, onBounce and onTickTock functions can't be marked as internalMsg/externalMsg.)");
		}
	}

	std::vector<VariableDeclaration const*> internalParametersInConstructor;

	auto checkArgumentAndReturnParameter = [&](VariableDeclaration const& _var) {
		bool functionIsExternallyVisible =
			(!_function.isConstructor() && _function.isPublic()) ||
			(_function.isConstructor() && !m_currentContract->abstract());
		if (functionIsExternallyVisible)
		{
			auto iType = type(_var)->interfaceType(_function.libraryFunction());

			if (!iType)
			{
				std::string message = iType.message();
				solAssert(!message.empty(), "Expected detailed error message!");
				if (_function.isConstructor())
					message += " You can make the contract abstract to avoid this problem.";
				m_errorReporter.fatalTypeError(4103_error, _var.location(), message);
			}
			else if (
				!useABICoderV2() &&
				!typeSupportedByOldABIEncoder(*type(_var), _function.libraryFunction())
			)
			{
				std::string message =
					"This type is only supported in ABI coder v2. "
					"Use \"pragma abicoder v2;\" to enable the feature.";
				if (_function.isConstructor())
					message +=
						" Alternatively, make the contract abstract and supply the "
						"constructor arguments from a derived contract.";
				m_errorReporter.typeError(
					4957_error,
					_var.location(),
					message
				);
			}
		}
	};
	for (ASTPointer<VariableDeclaration> const& var: _function.parameters())
	{
		checkArgumentAndReturnParameter(*var);
		var->accept(*this);
	}
	for (ASTPointer<VariableDeclaration> const& var: _function.returnParameters())
	{
		checkArgumentAndReturnParameter(*var);
		var->accept(*this);
	}

	std::set<Declaration const*> modifiers;
	for (ASTPointer<ModifierInvocation> const& modifier: _function.modifiers())
	{
		std::vector<ContractDefinition const*> baseContracts;
		if (auto contract = dynamic_cast<ContractDefinition const*>(_function.scope()))
		{
			baseContracts = contract->annotation().linearizedBaseContracts;
			// Delete first base which is just the main contract itself
			baseContracts.erase(baseContracts.begin());
		}

		visitManually(
			*modifier,
			_function.isConstructor() ? baseContracts : std::vector<ContractDefinition const*>()
		);
		Declaration const* decl = &dereference(modifier->name());
		if (modifiers.count(decl))
		{
			if (dynamic_cast<ContractDefinition const*>(decl))
				m_errorReporter.declarationError(1697_error, modifier->location(), "Base constructor already provided.");
		}
		else
			modifiers.insert(decl);
	}

	solAssert(_function.isFree() == !m_currentContract, "");
	if (!m_currentContract)
	{
		solAssert(!_function.isConstructor(), "");
		solAssert(!_function.isFallback(), "");
		solAssert(!_function.isReceive(), "");
	}
	else if (m_currentContract->isInterface())
	{
		if (_function.isImplemented())
			m_errorReporter.typeError(4726_error, _function.location(), "Functions in interfaces cannot have an implementation.");

		if (_function.isConstructor())
			m_errorReporter.typeError(6482_error, _function.location(), "Constructor cannot be defined in interfaces.");
		else if (_function.visibility() != Visibility::External)
			m_errorReporter.typeError(1560_error, _function.location(), "Functions in interfaces must be declared external.");
	}
	else if (m_currentContract->contractKind() == ContractKind::Library)
		if (_function.isConstructor())
			m_errorReporter.typeError(7634_error, _function.location(), "Constructor cannot be defined in libraries.");

	if (_function.isImplemented())
		_function.body().accept(*this);
	else if (_function.isConstructor())
		m_errorReporter.typeError(5700_error, _function.location(), "Constructor must be implemented if declared.");
	else if (_function.libraryFunction())
		m_errorReporter.typeError(9231_error, _function.location(), "Library functions must be implemented if declared.");
	else if (!_function.virtualSemantics())
	{
		if (_function.isFree())
			solAssert(m_errorReporter.hasErrors(), "");
		else
			m_errorReporter.typeError(5424_error, _function.location(), "Functions without implementation must be marked virtual.");
	}


	if (_function.isFallback())
		typeCheckFallbackFunction(_function);
	else if (_function.isConstructor())
		typeCheckConstructor(_function);
	else if (_function.isOnBounce())
		typeCheckOnBounce(_function);
	else if (_function.isOnTickTock())
		typeCheckOnTickTock(_function);

	if (_function.functionIsExternallyVisible() && !_function.isOnBounce())
	{
		for (const auto& params : {_function.parameters(), _function.returnParameters()}) {
			for (ASTPointer<VariableDeclaration> const &var : params) {
				std::set<StructDefinition const *> usedStructs;
				isBadAbiType(var.get()->location(), var->type(), var.get()->location(), usedStructs, true);
//				var->accept(*this);
			}
		}
	}

	return false;
}

void TypeChecker::endVisit(FunctionDefinition const& )
{
	m_currentFunction = nullptr;
}

bool TypeChecker::visit(VariableDeclaration const& _variable)
{
	_variable.typeName().accept(*this);

	// type is filled either by ReferencesResolver directly from the type name or by
	// TypeChecker at the VariableDeclarationStatement level.
	Type const* varType = _variable.annotation().type;
	solAssert(!!varType, "Variable type not provided.");

	if (_variable.value())
	{
		if (_variable.isStateVariable() && varType->containsNestedMapping())
		{
			m_errorReporter.typeError(
				6280_error,
				_variable.location(),
				"Types in storage containing (nested) mappings cannot be assigned to."
			);
			_variable.value()->accept(*this);
		}
		else
			expectType(*_variable.value(), *varType);
	}
	if (_variable.isConstant())
	{
		if (!_variable.value())
			m_errorReporter.typeError(4266_error, _variable.location(), "Uninitialized \"constant\" variable.");
		else if (!*_variable.value()->annotation().isPure)
			m_errorReporter.typeError(
				8349_error,
				_variable.value()->location(),
				"Initial value for constant variable has to be compile-time constant."
			);
	}
	else if (_variable.immutable())
	{
		if (!_variable.type()->isValueType())
			m_errorReporter.typeError(6377_error, _variable.location(), "Immutable variables cannot have a non-value type.");
		if (
			auto const* functionType = dynamic_cast<FunctionType const*>(_variable.type());
			functionType && functionType->kind() == FunctionType::Kind::External
		)
			m_errorReporter.typeError(3366_error, _variable.location(), "Immutable variables of external function type are not yet supported.");
		solAssert(_variable.type()->sizeOnStack() == 1 || m_errorReporter.hasErrors(), "");
	}

	if (!_variable.isStateVariable())
	{
	}
	else if (_variable.visibility() >= Visibility::Public)
	{
		FunctionType getter(_variable);
		if (!useABICoderV2())
		{
			std::vector<std::string> unsupportedTypes;
			for (auto const& param: getter.parameterTypes() + getter.returnParameterTypes())
				if (!typeSupportedByOldABIEncoder(*param, false /* isLibrary */))
					unsupportedTypes.emplace_back(param->humanReadableName());
			if (!unsupportedTypes.empty())
				m_errorReporter.typeError(
					2763_error,
					_variable.location(),
					"The following types are only supported for getters in ABI coder v2: " +
					joinHumanReadable(unsupportedTypes) +
					". Either remove \"public\" or use \"pragma abicoder v2;\" to enable the feature."
				);
		}
		std::set<StructDefinition const*> usedStructs;
		isBadAbiType(_variable.location(), _variable.type(), _variable.location(), usedStructs, true);

		if (!getter.interfaceFunctionType())
		{
			solAssert(getter.returnParameterNames().size() == getter.returnParameterTypes().size());
			solAssert(getter.parameterNames().size() == getter.parameterTypes().size());
			if (getter.returnParameterTypes().empty() && getter.parameterTypes().empty())
				m_errorReporter.typeError(5359_error, _variable.location(), "The struct has all its members omitted, therefore the getter cannot return any values.");
			else
				m_errorReporter.typeError(6744_error, _variable.location(), "Internal or recursive type is not allowed for public state variables.");
		}
	}

	bool isStructMemberDeclaration = dynamic_cast<StructDefinition const*>(_variable.scope()) != nullptr;
	if (isStructMemberDeclaration)
		return false;

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
					5241_error,
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
			3505_error,
			_variable.value()->location(),
			"Static variables can be initialized only during contract deployment.");
	}

	if (_variable.type()->category() == Type::Category::Function) {
		auto ft = dynamic_cast<FunctionType const*>(_variable.type());
		if (ft->kind() == FunctionType::Kind::External) {
			m_errorReporter.fatalTypeError(
				7945_error,
				_variable.location(),
				"External functions are not supported yet.");
		}
	}

	return false;
}

void TypeChecker::endVisit(StructDefinition const& _struct)
{
	for (auto const& member: _struct.members())
		solAssert(
			member->annotation().type &&
			member->annotation().type->canBeStored(),
			"Type cannot be used in struct."
		);
}

void TypeChecker::visitManually(
	ModifierInvocation const& _modifier,
	std::vector<ContractDefinition const*> const& _bases
)
{
	std::vector<ASTPointer<Expression>> const& arguments =
		_modifier.arguments() ? *_modifier.arguments() : std::vector<ASTPointer<Expression>>();
	for (ASTPointer<Expression> const& argument: arguments)
		argument->accept(*this);

	_modifier.name().accept(*this);

	auto const* declaration = &dereference(_modifier.name());
	std::vector<ASTPointer<VariableDeclaration>> emptyParameterList;
	std::vector<ASTPointer<VariableDeclaration>> const* parameters = nullptr;
	if (auto modifierDecl = dynamic_cast<ModifierDefinition const*>(declaration))
	{
		parameters = &modifierDecl->parameters();
		if (auto const* modifierContract = dynamic_cast<ContractDefinition const*>(modifierDecl->scope()))
			if (m_currentContract)
			{
				if (!util::contains(m_currentContract->annotation().linearizedBaseContracts, modifierContract))
					m_errorReporter.typeError(
						9428_error,
						_modifier.location(),
						"Can only use modifiers defined in the current contract or in base contracts."
					);
			}
		if (
			*_modifier.name().annotation().requiredLookup == VirtualLookup::Static &&
			!modifierDecl->isImplemented()
		)
			m_errorReporter.typeError(
				1835_error,
				_modifier.location(),
				"Cannot call unimplemented modifier. The modifier has no implementation in the referenced contract. Refer to it by its unqualified name if you want to call the implementation from the most derived contract."
			);
	}
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
		m_errorReporter.typeError(4659_error, _modifier.location(), "Referenced declaration is neither modifier nor base class.");
		return;
	}
	if (parameters->size() != arguments.size())
	{
		m_errorReporter.typeError(
			2973_error,
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
				4649_error,
				arguments[i]->location(),
				"Invalid type for argument in modifier invocation. "
				"Invalid implicit conversion from " +
				type(*arguments[i])->humanReadableName() +
				" to " +
				type(*(*parameters)[i])->humanReadableName() +
				" requested.",
				result.message()
			);
	}
}

bool TypeChecker::visit(EventDefinition const& _eventDef)
{
	solAssert(_eventDef.visibility() > Visibility::Internal, "");
	checkErrorAndEventParameters(_eventDef);

	auto numIndexed = ranges::count_if(
		_eventDef.parameters(),
		[](ASTPointer<VariableDeclaration> const& var) { return var->isIndexed(); }
	);
	if (_eventDef.isAnonymous() && numIndexed > 4)
		m_errorReporter.typeError(8598_error, _eventDef.location(), "More than 4 indexed arguments for anonymous event.");
	else if (!_eventDef.isAnonymous() && numIndexed > 3)
		m_errorReporter.typeError(7249_error, _eventDef.location(), "More than 3 indexed arguments for event.");
	return true;
}

bool TypeChecker::visit(ErrorDefinition const& _errorDef)
{
	solAssert(_errorDef.visibility() > Visibility::Internal, "");
	checkErrorAndEventParameters(_errorDef);
	return true;
}

void TypeChecker::endVisit(FunctionTypeName const& _funType)
{
	FunctionType const& fun = dynamic_cast<FunctionType const&>(*_funType.annotation().type);
	if (fun.kind() == FunctionType::Kind::External)
	{
		for (auto const& t: _funType.parameterTypes() + _funType.returnParameterTypes())
		{
			solAssert(t->annotation().type, "Type not set for parameter.");
			if (!t->annotation().type->interfaceType(false).get())
				m_errorReporter.fatalTypeError(2582_error, t->location(), "Internal type cannot be used for external function type.");
		}
		solAssert(fun.interfaceType(false), "External function type uses internal types.");
	}
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
    TryCatchClause const& clause = _tryStatement.clause();
	if (clause.parameters() != nullptr) {
		std::vector<ASTPointer<VariableDeclaration>> const& errArgs = clause.parameters()->parameters();

		auto printError = [&](SourceLocation const& loc){
			m_errorReporter.typeError(
				8220_error,
				loc,
				"Expected `catch (variant value, uint16 number) { ... }`.");
		};
		if (errArgs.size() != 2) {
			printError(clause.location());
			return;
		}
		if (*errArgs.at(0)->type() != *TypeProvider::variant()) {
			printError(errArgs.at(0)->location());
		}
		if (*errArgs.at(1)->type() != *TypeProvider::uint(16)) {
			printError(errArgs.at(1)->location());
		}
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
			expectType(_whileStatement.condition(), *TypeProvider::integer(31, IntegerType::Modifier::Unsigned));
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

	auto vars = dynamic_cast<VariableDeclarationStatement const*>(_forStatement.rangeDeclaration());
	if (vars == nullptr) {
		m_errorReporter.typeError(
			9487_error,
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
				m_errorReporter.typeError(5397_error, vd->location(), errorMsg);
			}
		};

		if (mappingType) {
			if (vars->declarations().size() != 2) {
				m_errorReporter.typeError(
						6298_error,
						vars->location(),
						"Expected two variables of type " +
						mappingType->keyType()->toString() +
						" and " +
						mappingType->valueType()->toString() +
						"."
				);
			} else {
				checkVarDeclaration(vars->declarations().at(0).get(), mappingType->realKeyType());
				checkVarDeclaration(vars->declarations().at(1).get(), mappingType->valueType());
			}
		} else if (arrayType) {
			if (vars->declarations().size() != 1) {
				m_errorReporter.typeError(
						9207_error,
						vars->location(),
						"Too many declared variables. "
						"Expected one variable of type " + arrayType->baseType()->toString() + "."
				);
			} else {
				checkVarDeclaration(vars->declarations().at(0).get(), arrayType->baseType());
			}
		} else {
			m_errorReporter.typeError(
					4670_error,
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
			8755_error,
			loc,
			R"(Options in return statement can be used only in responsible public/external functions.)"
		);
	} else {
		for (size_t i = 0; i < _return.names().size(); ++i) {
			const std::string &name = *_return.names().at(i);
			const std::map<std::string, Type const *> nameToType = {
					{"value",      TypeProvider::coins()},
					{"currencies", TypeProvider::extraCurrencyCollection()},
					{"bounce",     TypeProvider::boolean()},
					{"flag",       TypeProvider::uint(16)},
			};
			if (nameToType.count(name) == 0) {
				m_errorReporter.typeError(
						3679_error,
						_return.options().at(i)->location(),
						"Unknown call option \"" +
						name +
						R"(". Valid options are "value", "currencies", "bounce", and "flag".)"
				);
			} else {
				Type const* expType = nameToType.at(name);
				expectType(*_return.options().at(i).get(), *expType, false);
			}
		}
	}


	ParameterList const* params = _return.annotation().functionReturnParameters;
	if (!_return.expression())
	{
		if (params && !params->parameters().empty())
			m_errorReporter.typeError(6777_error, _return.location(), "Return arguments required.");
		return;
	}
	if (!params)
	{
		m_errorReporter.typeError(7552_error, _return.location(), "Return arguments not allowed.");
		return;
	}
	TypePointers returnTypes;
	for (auto const& var: params->parameters())
		returnTypes.push_back(type(*var));
	if (auto tupleType = dynamic_cast<TupleType const*>(type(*_return.expression())))
	{
		if (tupleType->components().size() != params->parameters().size())
			m_errorReporter.typeError(5132_error, _return.location(), "Different number of arguments in return statement than in returns declaration.");
		else
		{
			BoolResult result = tupleType->isImplicitlyConvertibleTo(TupleType(returnTypes));
			if (!result)
				m_errorReporter.typeErrorConcatenateDescriptions(
					5992_error,
					_return.expression()->location(),
					"Return argument type " +
					type(*_return.expression())->humanReadableName() +
					" is not implicitly convertible to expected type " +
					TupleType(returnTypes).humanReadableName() + ".",
					result.message()
				);
		}
	}
	else if (params->parameters().size() != 1)
		m_errorReporter.typeError(8863_error, _return.location(), "Different number of arguments in return statement than in returns declaration.");
	else
	{
		Type const* expected = type(*params->parameters().front());
		BoolResult result = type(*_return.expression())->isImplicitlyConvertibleTo(*expected);
		if (!result)
			m_errorReporter.typeErrorConcatenateDescriptions(
				6359_error,
				_return.expression()->location(),
				"Return argument type " +
				type(*_return.expression())->humanReadableName() +
				" is not implicitly convertible to expected type (type of first return variable) " +
				expected->humanReadableName() + ".",
				result.message()
			);
	}
}

void TypeChecker::endVisit(EmitStatement const& _emit)
{
	if (
		*_emit.eventCall().annotation().kind != FunctionCallKind::FunctionCall ||
		type(_emit.eventCall().expression())->category() != Type::Category::Function ||
		dynamic_cast<FunctionType const&>(*type(_emit.eventCall().expression())).kind() != FunctionType::Kind::Event
	)
		m_errorReporter.typeError(9292_error, _emit.eventCall().expression().location(), "Expression has to be an event invocation.");

	const std::vector<ASTPointer<Expression>>& options = _emit.options();
	const std::vector<ASTPointer<ASTString>>& names = _emit.names();
	for (std::size_t i = 0; i < options.size(); ++i) {
		const std::string name = *names.at(i);
		Expression const* opt = options.at(i).get();
		if (name == "dest") {
			expectType(*opt, *TypeProvider::address(), false);
		} else {
			m_errorReporter.typeError(2900_error, _emit.location(), "Unknown option " + name + ". Only option \"dest\" is supported.");
		}
	}
}

void TypeChecker::endVisit(RevertStatement const& _revert)
{
	FunctionCall const& errorCall = _revert.errorCall();
	if (
		*errorCall.annotation().kind != FunctionCallKind::FunctionCall ||
		type(errorCall.expression())->category() != Type::Category::Function ||
		dynamic_cast<FunctionType const&>(*type(errorCall.expression())).kind() != FunctionType::Kind::Error
	)
		m_errorReporter.typeError(1885_error, errorCall.expression().location(), "Expression has to be an error.");
}

void TypeChecker::endVisit(ArrayTypeName const& _typeName)
{
	solAssert(
		_typeName.baseType().annotation().type &&
		_typeName.baseType().annotation().type->storageBytes() != 0,
		"Illegal base type of storage size zero for array."
	);
}

bool TypeChecker::visit(VariableDeclarationStatement const& _statement)
{
	if (!_statement.initialValue())
	{
		if (_statement.isInForLoop()){
			return false;
		}

		// No initial value is only permitted for single variables with specified type.
		// This usually already results in a parser error.
		if (_statement.declarations().size() != 1 || !_statement.declarations().front())
		{
			solAssert(m_errorReporter.hasErrors(), "");

			// It is okay to return here, as there are no named components on the
			// left-hand-side that could cause any damage later.
			return false;
		}

		VariableDeclaration const& varDecl = *_statement.declarations().front();
		solAssert(varDecl.annotation().type, "");

		varDecl.accept(*this);
		return false;
	}

	// Here we have an initial value and might have to derive some types before we can visit
	// the variable declaration(s).

	_statement.initialValue()->accept(*this);
	std::vector<ASTPointer<VariableDeclaration>> const& variables = _statement.declarations();
	TypePointers valueTypes;
	if (auto tupleType = dynamic_cast<TupleType const*>(type(*_statement.initialValue()))) {
		valueTypes = tupleType->components();
		if (valueTypes.size() != variables.size())
			valueTypes = TypePointers{tupleType};
	} else
		valueTypes = TypePointers{type(*_statement.initialValue())};

	if (variables.empty())
		// We already have an error for this in the SyntaxChecker.
		solAssert(m_errorReporter.hasErrors(), "");
	else if (valueTypes.size() != variables.size())
		m_errorReporter.typeError(
			7364_error,
			_statement.location(),
			"Different number of components on the left hand side (" +
			toString(variables.size()) +
			") than on the right hand side (" +
			toString(valueTypes.size()) +
			")."
		);

	for (size_t i = 0; i < std::min(variables.size(), valueTypes.size()); ++i)
	{
		if (!variables[i])
			continue;
		VariableDeclaration const& var = *variables[i];
		solAssert(!var.value(), "Value has to be tied to statement.");
		Type const* valueComponentType = valueTypes[i];
		solAssert(!!valueComponentType, "");
		solAssert(var.annotation().type, "");

		var.accept(*this);
		BoolResult result = valueComponentType->isImplicitlyConvertibleTo(*var.annotation().type);
		if (!result)
		{
			auto errorMsg = "Type " +
				valueComponentType->humanReadableName() +
				" is not implicitly convertible to expected type " +
				var.annotation().type->humanReadableName();
			if (
				valueComponentType->category() == Type::Category::RationalNumber &&
				dynamic_cast<RationalNumberType const&>(*valueComponentType).isFractional() &&
				valueComponentType->mobileType()
			)
			{
				if (var.annotation().type->operator==(*valueComponentType->mobileType()))
					m_errorReporter.typeError(
						5107_error,
						_statement.location(),
						errorMsg + ", but it can be explicitly converted."
					);
				else
					m_errorReporter.typeError(
						4486_error,
						_statement.location(),
						errorMsg +
						". Try converting to type " +
						valueComponentType->mobileType()->humanReadableName() +
						" or use an explicit conversion."
					);
			}
			else
				m_errorReporter.typeErrorConcatenateDescriptions(
					9574_error,
					_statement.location(),
					errorMsg + ".",
					result.message()
				);
		}
	}

	if (valueTypes.size() != variables.size())
	{
		solAssert(m_errorReporter.hasErrors(), "Should have errors!");
		for (auto const& var: variables)
			if (var && !var->annotation().type)
				BOOST_THROW_EXCEPTION(FatalError());
	}

	return false;
}

void TypeChecker::endVisit(ExpressionStatement const& _statement)
{
	if (type(_statement.expression())->category() == Type::Category::RationalNumber)
		if (!dynamic_cast<RationalNumberType const&>(*type(_statement.expression())).mobileType())
			m_errorReporter.typeError(3757_error, _statement.expression().location(), "Invalid rational number.");

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
				m_errorReporter.warning(9302_error, _statement.location(), "Return value of low-level calls not used.");
			else if (kind == FunctionType::Kind::Send)
				m_errorReporter.warning(5878_error, _statement.location(), "Failure condition of 'send' ignored. Consider using 'transfer' instead.");
		}
	}
}

bool TypeChecker::visit(Conditional const& _conditional)
{
	expectType(_conditional.condition(), *TypeProvider::boolean());

	_conditional.trueExpression().accept(*this);
	_conditional.falseExpression().accept(*this);

	Type const* trueType = type(_conditional.trueExpression())->mobileType();
	Type const* falseType = type(_conditional.falseExpression())->mobileType();

	Type const* commonType = nullptr;

	if (!trueType)
		m_errorReporter.typeError(9717_error, _conditional.trueExpression().location(), "Invalid mobile type in true expression.");
	else
		commonType = trueType;

	if (!falseType)
		m_errorReporter.typeError(3703_error, _conditional.falseExpression().location(), "Invalid mobile type in false expression.");
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
					1080_error,
					_conditional.location(),
					"True expression's type " +
					trueType->humanReadableName() +
					" does not match false expression's type " +
					falseType->humanReadableName() +
					"."
					);
			// even we can't find a common type, we have to set a type here,
			// otherwise the upper statement will not be able to check the type.
			commonType = trueType;
		}
	}

	_conditional.annotation().isConstant = false;
	_conditional.annotation().type = commonType;
	_conditional.annotation().isPure =
		*_conditional.condition().annotation().isPure &&
		*_conditional.trueExpression().annotation().isPure &&
		*_conditional.falseExpression().annotation().isPure;

	_conditional.annotation().isLValue = false;

	if (_conditional.annotation().willBeWrittenTo)
		m_errorReporter.typeError(
			2212_error,
			_conditional.location(),
			"Conditional expression as left value is not supported yet."
		);

	return false;
}

void TypeChecker::checkExpressionAssignment(Type const& _type, Expression const& _expression)
{
	if (auto const* tupleExpression = dynamic_cast<TupleExpression const*>(&_expression))
	{
		if (tupleExpression->components().empty())
			m_errorReporter.typeError(5547_error, _expression.location(), "Empty tuple on the left hand side.");

		auto const* tupleType = dynamic_cast<TupleType const*>(&_type);
		auto const& types = tupleType && tupleExpression->components().size() != 1 ? tupleType->components() : std::vector<Type const*> { &_type };

		solAssert(
			tupleExpression->components().size() == types.size() || m_errorReporter.hasErrors(),
			"Array sizes don't match and no errors generated."
		);

		for (size_t i = 0; i < std::min(tupleExpression->components().size(), types.size()); i++)
			if (types[i])
			{
				solAssert(!!tupleExpression->components()[i], "");
				checkExpressionAssignment(*types[i], *tupleExpression->components()[i]);
			}
	}
}

bool TypeChecker::visit(Assignment const& _assignment)
{
	requireLValue(
		_assignment.leftHandSide(),
		_assignment.assignmentOperator() == Token::Assign
	);
	Type const* t = type(_assignment.leftHandSide());
	_assignment.annotation().type = t;
	_assignment.annotation().isPure = false;
	_assignment.annotation().isLValue = false;
	_assignment.annotation().isConstant = false;

	checkExpressionAssignment(*t, _assignment.leftHandSide());

	if (TupleType const* tupleType = dynamic_cast<TupleType const*>(t))
	{
		if (_assignment.assignmentOperator() != Token::Assign)
			m_errorReporter.typeError(
				4289_error,
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
		Type const* resultType = t->binaryOperatorResult(
			TokenTraits::AssignmentToBinaryOp(_assignment.assignmentOperator()),
			type(_assignment.rightHandSide())
		);
		if (!resultType || *resultType != *t)
			m_errorReporter.typeError(
				7366_error,
				_assignment.location(),
				"Operator " +
				std::string(TokenTraits::friendlyName(_assignment.assignmentOperator())) +
				" not compatible with types " +
				t->humanReadableName() +
				" and " +
				type(_assignment.rightHandSide())->humanReadableName() +
				"."
			);
	}
	return false;
}

bool TypeChecker::visit(TupleExpression const& _tuple)
{
	_tuple.annotation().isConstant = false;
	std::vector<ASTPointer<Expression>> const& components = _tuple.components();
	TypePointers types;

	if (_tuple.annotation().willBeWrittenTo)
	{
		if (_tuple.isInlineArray())
			m_errorReporter.fatalTypeError(3025_error, _tuple.location(), "Inline array type cannot be declared as LValue.");
		for (auto const& component: components)
			if (component)
			{
				requireLValue(
					*component,
					_tuple.annotation().lValueOfOrdinaryAssignment
				);
				types.push_back(type(*component));
			}
			else
				types.push_back(nullptr);
		if (components.size() == 1)
			_tuple.annotation().type = type(*components[0]);
		else
			_tuple.annotation().type = TypeProvider::tuple(std::move(types));
		// If some of the components are not LValues, the error is reported above.
		_tuple.annotation().isLValue = true;
		_tuple.annotation().isPure = false;
	}
	else
	{
		bool isPure = true;
		Type const* inlineArrayType = nullptr;

		for (size_t i = 0; i < components.size(); ++i)
		{
			if (!components[i])
				m_errorReporter.fatalTypeError(8381_error, _tuple.location(), "Tuple component cannot be empty.");

			components[i]->accept(*this);
			types.push_back(type(*components[i]));

			if (types[i]->category() == Type::Category::Tuple)
				if (dynamic_cast<TupleType const&>(*types[i]).components().empty())
				{
					if (_tuple.isInlineArray())
						m_errorReporter.fatalTypeError(5604_error, components[i]->location(), "Array component cannot be empty.");
					m_errorReporter.typeError(6473_error, components[i]->location(), "Tuple component cannot be empty.");
				}

			// Note: code generation will visit each of the expression even if they are not assigned from.
			if (types[i]->category() == Type::Category::RationalNumber && components.size() > 1)
				if (!dynamic_cast<RationalNumberType const&>(*types[i]).mobileType())
					m_errorReporter.fatalTypeError(3390_error, components[i]->location(), "Invalid rational number.");

			if (_tuple.isInlineArray())
			{
				solAssert(!!types[i], "Inline array cannot have empty components");

				if ((i == 0 || inlineArrayType) && !types[i]->mobileType())
					m_errorReporter.fatalTypeError(9563_error, components[i]->location(), "Invalid mobile type.");

				if (i == 0)
					inlineArrayType = types[i]->mobileType();
				else if (inlineArrayType)
					inlineArrayType = Type::commonType(inlineArrayType, types[i]);
			}
			if (!*components[i]->annotation().isPure)
				isPure = false;
		}
		_tuple.annotation().isPure = isPure;
		if (_tuple.isInlineArray())
		{
			if (!inlineArrayType)
				m_errorReporter.fatalTypeError(6378_error, _tuple.location(), "Unable to deduce common type for array elements.");
			else if (!inlineArrayType->nameable())
				m_errorReporter.fatalTypeError(
					9656_error,
					_tuple.location(),
					"Unable to deduce nameable type for array elements. Try adding explicit type conversion for the first element."
				);
			else if (inlineArrayType->containsNestedMapping())
				m_errorReporter.fatalTypeError(
					1545_error,
					_tuple.location(),
					"Type " + inlineArrayType->humanReadableName() + " is only valid in storage."
				);

			_tuple.annotation().type = TypeProvider::array(inlineArrayType, types.size());
		}
		else
		{
			if (components.size() == 1)
				_tuple.annotation().type = type(*components[0]);
			else
				_tuple.annotation().type = TypeProvider::tuple(std::move(types));
		}

		_tuple.annotation().isLValue = false;
	}
	return false;
}

bool TypeChecker::visit(UnaryOperation const& _operation)
{
	// Inc, Dec, Add, Sub, Not, BitNot, Delete
	Token op = _operation.getOperator();
	bool const modifying = (op == Token::Inc || op == Token::Dec || op == Token::Delete);
	if (modifying)
		requireLValue(_operation.subExpression(), false);
	else
		_operation.subExpression().accept(*this);
	Type const* operandType = type(_operation.subExpression());

	// Check if the operator is built-in or user-defined.
	TypeResult builtinResult = operandType->unaryOperatorResult(op);
	std::set<FunctionDefinition const*, ASTNode::CompareByID> matchingDefinitions = operandType->operatorDefinitions(
		op,
		*currentDefinitionScope(),
		true // _unary
	);

	// Operator can't be both user-defined and built-in at the same time.
	solAssert(!builtinResult || matchingDefinitions.empty());

	// By default use the type we'd expect from correct code. This way we can continue analysis
	// of other expressions in a sensible way in case of a non-fatal error.
	Type const* resultType = operandType;

	FunctionDefinition const* operatorDefinition = nullptr;
	if (builtinResult)
		resultType = builtinResult;
	else if (!matchingDefinitions.empty())
	{
		// This is checked along with `using for` directive but the error is not fatal.
		if (matchingDefinitions.size() != 1)
			solAssert(m_errorReporter.hasErrors());

		operatorDefinition = *matchingDefinitions.begin();
	}
	else
	{
		std::string description = fmt::format(
			"Built-in unary operator {} cannot be applied to type {}.",
			TokenTraits::friendlyName(op),
			operandType->humanReadableName()
		);
		if (!builtinResult.message().empty())
			description += " " + builtinResult.message();
		if (operandType->typeDefinition() && util::contains(userDefinableOperators, op))
			description += " No matching user-defined operator found.";

		if (modifying)
			// Cannot just report the error, ignore the unary operator, and continue,
			// because the sub-expression was already processed with requireLValue()
			m_errorReporter.fatalTypeError(9767_error, _operation.location(), description);
		else
			m_errorReporter.typeError(4907_error, _operation.location(), description);
	}

	_operation.annotation().userDefinedFunction = operatorDefinition;

	if (operatorDefinition && !_operation.userDefinedFunctionType()->returnParameterTypes().empty())
		// Use the actual result type from operator definition. Ignore all values but the
		// first one - in valid code there will be only one anyway.
		resultType = _operation.userDefinedFunctionType()->returnParameterTypes()[0];
	_operation.annotation().type = resultType;
	_operation.annotation().isConstant = false;
	_operation.annotation().isPure =
		!modifying &&
		*_operation.subExpression().annotation().isPure &&
		(!_operation.userDefinedFunctionType() || _operation.userDefinedFunctionType()->isPure());
	_operation.annotation().isLValue = false;

	return false;
}

void TypeChecker::endVisit(BinaryOperation const& _operation)
{
	Type const* leftType = type(_operation.leftExpression());
	Type const* rightType = type(_operation.rightExpression());

	// Check if the operator is built-in or user-defined.
	TypeResult builtinResult = leftType->binaryOperatorResult(_operation.getOperator(), rightType);
	std::set<FunctionDefinition const*, ASTNode::CompareByID> matchingDefinitions = leftType->operatorDefinitions(
		_operation.getOperator(),
		*currentDefinitionScope(),
		false // _unary
	);

	// Operator can't be both user-defined and built-in at the same time.
	solAssert(!builtinResult || matchingDefinitions.empty());

	Type const* commonType = nullptr;
	FunctionDefinition const* operatorDefinition = nullptr;
	if (builtinResult)
		commonType = builtinResult.get();
	else if (!matchingDefinitions.empty())
	{
		// This is checked along with `using for` directive but the error is not fatal.
		if (matchingDefinitions.size() != 1)
			solAssert(m_errorReporter.hasErrors());

		operatorDefinition = *matchingDefinitions.begin();

		// Set common type to the type used in the `using for` directive.
		commonType = leftType;
	}
	else
	{
		std::string description = fmt::format(
			"Built-in binary operator {} cannot be applied to types {} and {}.",
			TokenTraits::friendlyName(_operation.getOperator()),
			leftType->humanReadableName(),
			rightType->humanReadableName()
		);
		if (!builtinResult.message().empty())
			description += " " + builtinResult.message();
		if (leftType->typeDefinition() && util::contains(userDefinableOperators, _operation.getOperator()))
			description += " No matching user-defined operator found.";

		m_errorReporter.typeError(2271_error, _operation.location(), description);

		// Set common type to something we'd expect from correct code just so that we can continue analysis.
		commonType = leftType;
	}

	_operation.annotation().commonType = commonType;
	_operation.annotation().userDefinedFunction = operatorDefinition;
	FunctionType const* userDefinedFunctionType = _operation.userDefinedFunctionType();

	// By default use the type we'd expect from correct code. This way we can continue analysis
	// of other expressions in a sensible way in case of a non-fatal error.
	Type const* resultType =
		TokenTraits::isCompareOp(_operation.getOperator()) ?
		TypeProvider::boolean() :
		commonType;

	if (operatorDefinition)
	{
		TypePointers const& parameterTypes = userDefinedFunctionType->parameterTypes();
		TypePointers const& returnParameterTypes = userDefinedFunctionType->returnParameterTypes();

		// operatorDefinitions() filters out definitions with non-matching first argument.
		solAssert(parameterTypes.size() == 2);
		solAssert(parameterTypes[0] && *leftType == *parameterTypes[0]);

		if (*rightType != *parameterTypes[0])
			m_errorReporter.typeError(
				5653_error,
				_operation.location(),
				fmt::format(
					"The type of the second operand of this user-defined binary operator {} "
					"does not match the type of the first operand, which is {}.",
					TokenTraits::friendlyName(_operation.getOperator()),
					parameterTypes[0]->humanReadableName()
				)
			);

		if (!returnParameterTypes.empty())
			// Use the actual result type from operator definition. Ignore all values but the
			// first one - in valid code there will be only one anyway.
			resultType = returnParameterTypes[0];
	}

	_operation.annotation().type = resultType;
	_operation.annotation().isPure =
		*_operation.leftExpression().annotation().isPure &&
		*_operation.rightExpression().annotation().isPure &&
		(!userDefinedFunctionType || userDefinedFunctionType->isPure());
	_operation.annotation().isLValue = false;
	_operation.annotation().isConstant = false;

	if (_operation.getOperator() == Token::Equal || _operation.getOperator() == Token::NotEqual)
	{
		auto const* leftFunction = dynamic_cast<FunctionType const*>(leftType);
		auto const* rightFunction = dynamic_cast<FunctionType const*>(rightType);
		if (
			leftFunction &&
			rightFunction &&
			leftFunction->kind() == FunctionType::Kind::Internal &&
			rightFunction->kind() == FunctionType::Kind::Internal
		)
		{
			m_errorReporter.warning(
				3075_error,
				_operation.location(),
				"Comparison of internal function pointers can yield unexpected results "
				"in the legacy pipeline with the optimizer enabled, and will be disallowed entirely "
				"in the next breaking release."
			);
		}
	}

	if (_operation.getOperator() == Token::Exp || _operation.getOperator() == Token::SHL)
	{
		std::string operation = _operation.getOperator() == Token::Exp ? "exponentiation" : "shift";
		if (
			leftType->category() == Type::Category::RationalNumber &&
			rightType->category() != Type::Category::RationalNumber
		)
		{
			// These rules are enforced by the binary operator, but assert them here too.
			if (auto type = dynamic_cast<IntegerType const*>(commonType))
				solAssert(type->numBits() == 256, "");
			if (auto type = dynamic_cast<FixedPointType const*>(commonType))
				solAssert(type->numBits() == 256, "");
		}
		if (
			commonType->category() == Type::Category::Integer &&
			rightType->category() == Type::Category::Integer &&
			dynamic_cast<IntegerType const&>(*commonType).numBits() <
			dynamic_cast<IntegerType const&>(*rightType).numBits()
		)
			m_errorReporter.warning(
				3149_error,
				_operation.location(),
				fmt::format(
					"The result type of the {} operation is equal to the type of the first operand ({}) "
					"ignoring the (larger) type of the second operand ({}) which might be unexpected. "
					"Silence this warning by either converting the first or the second operand to the type of the other.",
					operation,
					commonType->humanReadableName(),
					rightType->humanReadableName()
				)
			);
	}
}

Type const* TypeChecker::typeCheckTypeConversionAndRetrieveReturnType(
	FunctionCall const& _functionCall
)
{
	solAssert(*_functionCall.annotation().kind == FunctionCallKind::TypeConversion, "");
	Type const* expressionType = type(_functionCall.expression());

	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	bool const isPositionalCall = _functionCall.names().empty();

	Type const* resultType = dynamic_cast<TypeType const&>(*expressionType).actualType();
	if (arguments.empty() &&
			(resultType->category() == Type::Category::TvmCell ||
			 resultType->category() == Type::Category::TvmBuilder)
	) {
		// all right
	}
	else if (arguments.size() != 1)
		m_errorReporter.typeError(
			2558_error,
			_functionCall.location(),
			"Exactly one argument expected for explicit type conversion."
		);
	else if (!isPositionalCall)
		m_errorReporter.typeError(
			5153_error,
			_functionCall.location(),
			"Type conversion cannot allow named arguments."
		);
	else
	{
		Type const* argType = type(*arguments.front());
		// Resulting data location is memory unless we are converting from a reference
		// type with a different data location.
		// (data location cannot yet be specified for type conversions)
		BoolResult result = argType->isExplicitlyConvertibleTo(*resultType);
		if (result)
		{
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
					7398_error,
					_functionCall.location(),
					ssl,
					"Explicit type conversion not allowed from non-payable \"address\" to \"" +
					resultType->humanReadableName() +
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
					5030_error,
					_functionCall.location(),
					"Explicit type conversion not allowed from \"" +
					argType->humanReadableName() +
					"\" to \"" +
					resultType->humanReadableName() +
					"\". To obtain the address of the contract of the function, " +
					"you can use the .address member of the function."
				);
			else
				m_errorReporter.typeErrorConcatenateDescriptions(
					9640_error,
					_functionCall.location(),
					"Explicit type conversion not allowed from \"" +
					argType->humanReadableName() +
					"\" to \"" +
					resultType->humanReadableName() +
					"\".",
					result.message()
				);
		}
	}
	return resultType;
}

void TypeChecker::typeCheckFunctionCall(
	FunctionCall const& _functionCall,
	FunctionTypePointer _functionType,
	std::set<std::string> _ignoreOptions
)
{
	// Actual function call or struct constructor call.

	solAssert(!!_functionType, "");
	solAssert(_functionType->kind() != FunctionType::Kind::ABIDecode, "");

	if (_functionType->kind() == FunctionType::Kind::Declaration)
	{
		solAssert(_functionType->declaration().annotation().contract, "");
		if (
			m_currentContract &&
			m_currentContract->derivesFrom(*_functionType->declaration().annotation().contract) &&
			!dynamic_cast<FunctionDefinition const&>(_functionType->declaration()).isImplemented()
		)
			m_errorReporter.typeError(
				7501_error,
				_functionCall.location(),
				"Cannot call unimplemented base function."
			);
		else
			m_errorReporter.typeError(
				3419_error,
				_functionCall.location(),
				"Cannot call function via contract type name."
			);
		return;
	}

	// Check for unsupported use of bare static call
	if (
		_functionType->kind() == FunctionType::Kind::BareStaticCall &&
		!m_evmVersion.hasStaticCall()
	)
		m_errorReporter.typeError(
			5052_error,
			_functionCall.location(),
			"\"staticcall\" is not supported by the VM version."
		);

	// Perform standard function call type checking
	typeCheckFunctionGeneralChecks(_functionCall, _functionType, _ignoreOptions);
}

void TypeChecker::typeCheckFallbackFunction(FunctionDefinition const& _function)
{
	solAssert(_function.isFallback(), "");

	if (_function.libraryFunction())
		m_errorReporter.typeError(5982_error, _function.location(), "Libraries cannot have fallback functions.");
	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(1159_error, _function.location(), "Fallback function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(1624_error, _function.returnParameterList()->location(), "Fallback function cannot return parameters.");
	if (!_function.parameters().empty())
		m_errorReporter.typeError(1127_error, _function.parameterList().location(), "Fallback function cannot take parameters.");
}

void TypeChecker::typeCheckConstructor(FunctionDefinition const& _function)
{
	solAssert(_function.isConstructor(), "");
	if (_function.overrides())
		m_errorReporter.typeError(1209_error, _function.location(), "Constructors cannot override.");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(9712_error, _function.returnParameterList()->location(), "Non-empty \"returns\" directive for constructor.");
	if (_function.stateMutability() != StateMutability::NonPayable)
		m_errorReporter.typeError(
			1558_error,
			_function.location(),
			"Constructor must have default mutability, but it is \"" +
			stateMutabilityToString(_function.stateMutability()) +
			"\"."
		);
	if (!_function.noVisibilitySpecified())
	{
		auto const& contract = dynamic_cast<ContractDefinition const&>(*_function.scope());
		if (_function.visibility() != Visibility::Public && _function.visibility() != Visibility::Internal)
			m_errorReporter.typeError(9239_error, _function.location(), "Constructor cannot have visibility.");
		else if (_function.isPublic() && contract.abstract())
			m_errorReporter.declarationError(
				8295_error,
				_function.location(),
				"Abstract contracts cannot have public constructors. Remove the \"public\" keyword to fix this."
			);
		else if (!_function.isPublic() && !contract.abstract())
			m_errorReporter.declarationError(
				1845_error,
				_function.location(),
				"Non-abstract contracts cannot have internal constructors. Remove the \"internal\" keyword and make the contract abstract to fix this."
			);
		else
			m_errorReporter.warning(
				2462_error,
				_function.location(),
				"Visibility for constructor is ignored. If you want the contract to be non-deployable, making it \"abstract\" is sufficient."
			);
	}
}

void TypeChecker::typeCheckOnBounce(const FunctionDefinition &_function) {
	solAssert(_function.isOnBounce(), "");

	if (_function.libraryFunction())
		m_errorReporter.typeError(1510_error, _function.location(), "Libraries cannot have onBounce functions.");

	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(4869_error, _function.location(), "onBounce function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(3628_error, _function.returnParameterList()->location(), "onBounce function cannot return values.");
	if (_function.parameters().size() != 1 || _function.parameters().at(0)->type()->category() != Type::Category::TvmSlice)
		m_errorReporter.typeError(5297_error, _function.parameterList().location(), "onBounce function should take one parameter (TvmSlice body).");
}


void TypeChecker::typeCheckOnTickTock(const FunctionDefinition &_function) {
	if (_function.libraryFunction())
		m_errorReporter.typeError(3729_error, _function.location(), "Libraries cannot have onTickTock functions.");

	if (_function.visibility() != Visibility::External)
		m_errorReporter.typeError(5243_error, _function.location(), "onTickTock function must be defined as \"external\".");
	if (!_function.returnParameters().empty())
		m_errorReporter.typeError(7745_error, _function.returnParameterList()->location(), "onTickTock function cannot return values.");
	if (_function.parameters().size() != 1 || _function.parameters().at(0)->type()->category() != Type::Category::Bool)
		m_errorReporter.typeError(3050_error, _function.parameterList().location(), "onTickTock function should take one parameter (bool isTock).");
}

void TypeChecker::checkNeedCallback(FunctionType const* callee, ASTNode const& node) {
	if (callee->hasDeclaration()) {
		auto funcDef = dynamic_cast<FunctionDefinition const *>(&callee->declaration());
		if (funcDef && funcDef->isResponsible()) {
			m_errorReporter.typeError(
					9205_error,
					node.location(),
					SecondarySourceLocation().append("The declaration is here:", funcDef->location()),
					R"("callback" option must be set because callee function is marked as responsible.)"
			);
		}
	}
}

void TypeChecker::typeCheckTvmEncodeArg(Type const* type, Expression const& node) {
	switch (type->category()) {
		case Type::Category::RationalNumber:
			m_errorReporter.typeError(
					6332_error,
					node.location(),
					"Cannot perform encoding for a literal."
					" Please convert it to an explicit type first."
			);
			break;
		case Type::Category::Address:
		case Type::Category::Array:
		case Type::Category::Bool:
		case Type::Category::Contract:
		case Type::Category::Enum:
		case Type::Category::FixedBytes:
		case Type::Category::Integer:
		case Type::Category::Mapping:
		case Type::Category::StringLiteral:
		case Type::Category::Struct:
		case Type::Category::TvmBuilder:
		case Type::Category::TvmCell:
		case Type::Category::TvmSlice:
		case Type::Category::VarInteger:
			break;
		case Type::Category::Optional:
		{
			Type const *valueType = dynamic_cast<OptionalType const*>(type)->valueType();
			typeCheckTvmEncodeArg(valueType, node);
			break;
		}
		default:
			m_errorReporter.typeError(
					9100_error,
					node.location(),
					"Encoding for a " + node.annotation().type->toString(true) + " isn't supported."
			);
	}
}

void TypeChecker::typeCheckTvmEncodeFunctions(FunctionCall const& _functionCall) {
	std::vector<ASTPointer<Expression const>> const &arguments = _functionCall.arguments();
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
		_functionType->kind() == FunctionType::Kind::ABIEncodeCall ||
		_functionType->kind() == FunctionType::Kind::ABIEncodeWithSignature,
		"ABI function has unexpected FunctionType::Kind."
	);
	solAssert(_functionType->takesArbitraryParameters(), "ABI functions should be variadic.");

	bool const isPacked = _functionType->kind() == FunctionType::Kind::ABIEncodePacked;
	solAssert(_functionType->padArguments() != isPacked, "ABI function with unexpected padding");

	bool const abiEncoderV2 = useABICoderV2();

	// Check for named arguments
	if (!_functionCall.names().empty())
	{
		m_errorReporter.typeError(
			2627_error,
			_functionCall.location(),
			"Named arguments cannot be used for functions that take arbitrary parameters."
		);
		return;
	}

	// Perform standard function call type checking
	typeCheckFunctionGeneralChecks(_functionCall, _functionType);

	// No further generic checks needed as we do a precise check for ABIEncodeCall
	if (_functionType->kind() == FunctionType::Kind::ABIEncodeCall)
	{
		typeCheckABIEncodeCallFunction(_functionCall);
		return;
	}

	// Check additional arguments for variadic functions
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	for (size_t i = 0; i < arguments.size(); ++i)
	{
		auto const& argType = type(*arguments[i]);

		if (argType->category() == Type::Category::RationalNumber)
		{
			auto const& rationalType = dynamic_cast<RationalNumberType const&>(*argType);
			if (rationalType.isFractional())
			{
				m_errorReporter.typeError(
					6090_error,
					arguments[i]->location(),
					"Fractional numbers cannot yet be encoded."
				);
				continue;
			}
			else if (!argType->mobileType())
			{
				m_errorReporter.typeError(
					8009_error,
					arguments[i]->location(),
					"Invalid rational number (too large or division by zero)."
				);
				continue;
			}
			else if (isPacked)
			{
				m_errorReporter.typeError(
					7279_error,
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
				9578_error,
				arguments[i]->location(),
				"Type not supported in packed mode."
			);
			continue;
		}

		if (!argType->fullEncodingType(false, abiEncoderV2, !_functionType->padArguments()))
			m_errorReporter.typeError(
				2056_error,
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

void TypeChecker::typeCheckABIEncodeCallFunction(FunctionCall const& _functionCall)
{
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();

	// Expecting first argument to be the function pointer and second to be a tuple.
	if (arguments.size() != 2)
	{
		m_errorReporter.typeError(
			6219_error,
			_functionCall.location(),
			"Expected two arguments: a function pointer followed by a tuple."
		);
		return;
	}

	FunctionType const* externalFunctionType = nullptr;
	if (auto const functionPointerType = dynamic_cast<FunctionTypePointer>(type(*arguments.front())))
	{
		// this cannot be a library function, that is checked below
		externalFunctionType = functionPointerType->asExternallyCallableFunction(false);
		solAssert(externalFunctionType->kind() == functionPointerType->kind());
	}
	else
	{
		m_errorReporter.typeError(
			5511_error,
			arguments.front()->location(),
			"Expected first argument to be a function pointer, not \"" +
			type(*arguments.front())->humanReadableName() +
			"\"."
		);
		return;
	}

	if (
		externalFunctionType->kind() != FunctionType::Kind::External &&
		externalFunctionType->kind() != FunctionType::Kind::Declaration
	)
	{
		std::string msg = "Expected regular external function type, or external view on public function.";

		switch (externalFunctionType->kind())
		{
			case FunctionType::Kind::Internal:
				msg += " Provided internal function.";
				break;
			case FunctionType::Kind::DelegateCall:
				msg += " Cannot use library functions for abi.encodeCall.";
				break;
			case FunctionType::Kind::Creation:
				msg += " Provided creation function.";
				break;
			case FunctionType::Kind::Event:
				msg += " Cannot use events for abi.encodeCall.";
				break;
			case FunctionType::Kind::Error:
				msg += " Cannot use errors for abi.encodeCall.";
				break;
			default:
				msg += " Cannot use special function.";
		}

		SecondarySourceLocation ssl{};

		if (externalFunctionType->hasDeclaration())
		{
			ssl.append("Function is declared here:", externalFunctionType->declaration().location());
			if (
				externalFunctionType->declaration().visibility() == Visibility::Public &&
				externalFunctionType->declaration().scope() == m_currentContract
			)
				msg += " Did you forget to prefix \"this.\"?";
			else if (
				m_currentContract &&
				externalFunctionType->declaration().scope() != m_currentContract &&
				util::contains(
					m_currentContract->annotation().linearizedBaseContracts,
					externalFunctionType->declaration().scope()
				)
			)
				msg += " Functions from base contracts have to be external.";
		}

		m_errorReporter.typeError(3509_error, arguments[0]->location(), ssl, msg);
		return;
	}
	solAssert(!externalFunctionType->takesArbitraryParameters(), "Function must have fixed parameters.");
	// Tuples with only one component become that component
	std::vector<ASTPointer<Expression const>> callArguments;

	auto const* tupleType = dynamic_cast<TupleType const*>(type(*arguments[1]));
	if (tupleType)
	{
		if (TupleExpression const* argumentTuple = dynamic_cast<TupleExpression const*>(arguments[1].get()))
			callArguments = decltype(callArguments){argumentTuple->components().begin(), argumentTuple->components().end()};
		else
		{
			m_errorReporter.typeError(
				9062_error,
				arguments[1]->location(),
				"Expected an inline tuple, not an expression of a tuple type."
			);
			return;
		}
	}
	else
		callArguments.push_back(arguments[1]);

	if (externalFunctionType->parameterTypes().size() != callArguments.size())
	{
		if (tupleType)
			m_errorReporter.typeError(
				7788_error,
				_functionCall.location(),
				"Expected " +
				std::to_string(externalFunctionType->parameterTypes().size()) +
				" instead of " +
				std::to_string(callArguments.size()) +
				" components for the tuple parameter."
			);
		else
			m_errorReporter.typeError(
				7515_error,
				_functionCall.location(),
				"Expected a tuple with " +
				std::to_string(externalFunctionType->parameterTypes().size()) +
				" components instead of a single non-tuple parameter."
			);
	}

	// Use min() to check as much as we can before failing fatally
	size_t const numParameters = std::min(callArguments.size(), externalFunctionType->parameterTypes().size());

	for (size_t i = 0; i < numParameters; i++)
	{
		Type const& argType = *type(*callArguments[i]);
		BoolResult result = argType.isImplicitlyConvertibleTo(*externalFunctionType->parameterTypes()[i]);
		if (!result)
			m_errorReporter.typeError(
				5407_error,
				callArguments[i]->location(),
				"Cannot implicitly convert component at position " +
				std::to_string(i) +
				" from \"" +
				argType.humanReadableName() +
				"\" to \"" +
				externalFunctionType->parameterTypes()[i]->humanReadableName() +
				"\"" +
				(result.message().empty() ?  "." : ": " + result.message())
			);
	}
}


void TypeChecker::typeCheckStringConcatFunction(
	FunctionCall const& _functionCall,
	FunctionType const* _functionType
)
{
	solAssert(_functionType);
	solAssert(_functionType->kind() == FunctionType::Kind::StringConcat);
	solAssert(_functionCall.names().empty());

	typeCheckFunctionGeneralChecks(_functionCall, _functionType);

	for (std::shared_ptr<Expression const> const& argument: _functionCall.arguments())
	{
		Type const* argumentType = type(*argument);
		bool notConvertibleToString = !argumentType->isImplicitlyConvertibleTo(*TypeProvider::stringMemory());

		if (notConvertibleToString)
			m_errorReporter.typeError(
				9977_error,
				argument->location(),
				"Invalid type for argument in the string.concat function call. "
				"string type is required, but " +
				argumentType->identifier() + " provided."
			);
	}
}

void TypeChecker::typeCheckBytesConcatFunction(
	FunctionCall const& _functionCall,
	FunctionType const* _functionType
)
{
	solAssert(_functionType);
	solAssert(_functionType->kind() == FunctionType::Kind::BytesConcat);
	solAssert(_functionCall.names().empty());

	typeCheckFunctionGeneralChecks(_functionCall, _functionType);

	for (std::shared_ptr<Expression const> const& argument: _functionCall.arguments())
	{
		Type const* argumentType = type(*argument);
		bool notConvertibleToBytes =
			!argumentType->isImplicitlyConvertibleTo(*TypeProvider::fixedBytes(32)) &&
			!argumentType->isImplicitlyConvertibleTo(*TypeProvider::bytesMemory());
		bool numberLiteral = (dynamic_cast<RationalNumberType const*>(argumentType) != nullptr);

		if (notConvertibleToBytes || numberLiteral)
			m_errorReporter.typeError(
				8015_error,
				argument->location(),
				"Invalid type for argument in the bytes.concat function call. "
				"bytes or fixed bytes type is required, but " +
				argumentType->humanReadableName() + " provided."
			);
	}
}

void TypeChecker::typeCheckFunctionGeneralChecks(
	FunctionCall const& _functionCall,
	FunctionTypePointer _functionType,
	std::set<std::string> _ignoreOptions
)
{
	// Actual function call or struct constructor call.

	solAssert(!!_functionType, "");
	solAssert(_functionType->kind() != FunctionType::Kind::ABIDecode, "");

	bool const isPositionalCall = _functionCall.names().empty();
	bool const isVariadic = _functionType->takesArbitraryParameters();

	auto functionCallKind = *_functionCall.annotation().kind;

	solAssert(
		!isVariadic || functionCallKind == FunctionCallKind::FunctionCall,
		"Struct constructor calls cannot be variadic."
	);

	TypePointers const& parameterTypes = _functionType->parameterTypes();
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	std::vector<ASTPointer<ASTString>> const& argumentNames = _functionCall.names();
	bool isFunctionWithDefaultValues = false;
	{
		auto ma = dynamic_cast<MemberAccess const*>(&_functionCall.expression());
		if (ma && ma->memberName() == "transfer" && dynamic_cast<AddressType const *>(ma->expression().annotation().type)) {
			isFunctionWithDefaultValues = true;
		}
		if (ma && dynamic_cast<MagicType const *>(ma->expression().annotation().type)) {
			if (ma->memberName() == "encodeStateInit" ||
				ma->memberName() == "buildStateInit" ||
				ma->memberName() == "buildDataInit" ||
				ma->memberName() == "encodeData" ||
				ma->memberName() == "encodeOldDataInit" ||
				ma->memberName() == "buildExtMsg" ||
				ma->memberName() == "encodeExtMsg" ||
				ma->memberName() == "encodeIntMsg" ||
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
			functionCallKind == FunctionCallKind::StructConstructorCall;

		auto [errorId, description] = [&]() -> std::tuple<ErrorId, std::string> {
			std::string msg = isVariadic ?
				"Need at least " +
				toString(parameterTypes.size()) +
				" arguments for " +
				std::string(isStructConstructorCall ? "struct constructor" : "function call") +
				", but provided only " +
				toString(arguments.size()) +
				"."
				:
				"Wrong argument count for " +
				std::string(isStructConstructorCall ? "struct constructor" : "function call") +
				": " +
				toString(arguments.size()) +
				" arguments given but " +
				std::string(isVariadic ? "need at least " : "expected ") +
				toString(parameterTypes.size()) +
				".";

			if (isStructConstructorCall)
			{
				solAssert(!isVariadic, "");
				return { 9755_error, msg };
			}
			else if (
				_functionType->kind() == FunctionType::Kind::BareCall ||
				_functionType->kind() == FunctionType::Kind::BareCallCode ||
				_functionType->kind() == FunctionType::Kind::BareDelegateCall ||
				_functionType->kind() == FunctionType::Kind::BareStaticCall
			)
			{
				solAssert(!isVariadic, "");
				if (arguments.empty())
					return {
						6138_error,
						msg +
						" This function requires a single bytes argument."
						" Use \"\" as argument to provide empty calldata."
					};
				else
					return {
						8922_error,
						msg +
						" This function requires a single bytes argument."
						" If all your arguments are value types, you can use"
						" abi.encode(...) to properly generate it."
					};
			}
			else if (
				_functionType->kind() == FunctionType::Kind::KECCAK256 ||
				_functionType->kind() == FunctionType::Kind::SHA256 ||
				_functionType->kind() == FunctionType::Kind::RIPEMD160
			)
			{
				solAssert(!isVariadic, "");
				return {
					4323_error,
					msg +
					" This function requires a single bytes argument."
					" Use abi.encodePacked(...) to obtain the pre-0.5.0"
					" behaviour or abi.encode(...) to use ABI encoding."
				};
			}
			else
				return { isVariadic ? 9308_error : 6160_error, msg };
		}();

		m_errorReporter.typeError(errorId, _functionCall.location(), description);
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
							6995_error,
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
					if ((_functionType->kind() == FunctionType::Kind::ABIEncodeStateInit ||
							_functionType->kind() == FunctionType::Kind::ABIEncodeData)
							&& *argumentNames.at(i) == "contr")
					{
						// Do nothing.
						// It's checked in TypeChecker::visit(FunctionCall const& _functionCall)
					} else {
						m_errorReporter.typeError(
							8213_error,
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

			for (size_t i = 0; i < argumentNames.size(); i++)
			{
				size_t j;
				for (j = 0; j < parameterNames.size(); j++)
					if (parameterNames[j] == *argumentNames[i])
						break;

				if (j < parameterNames.size())
					paramArgMap[j] = arguments[i].get();
				else
				{
					not_all_mapped = true;
					m_errorReporter.typeError(
						4974_error,
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
		std::vector<std::string> const &parameterNames = _functionType->parameterNames();
		if (i >= parameterNames.size()) {
			m_errorReporter.fatalTypeError(3774_error, paramArgMap[i]->location(), "Too many arguments.");
		}
		if (!parameterNames.empty()) {
			std::string const &argName = parameterNames.at(i);
			if (_ignoreOptions.count(argName)) {
				continue;
			}
		}
		BoolResult result = type(*paramArgMap[i])->isImplicitlyConvertibleTo(*parameterTypes[i]);
		if (!result)
		{
			auto [errorId, description] = [&]() -> std::tuple<ErrorId, std::string> {
				std::string msg =
					"Invalid type for argument in function call. "
					"Invalid implicit conversion from " +
					type(*paramArgMap[i])->humanReadableName() +
					" to " +
					parameterTypes[i]->humanReadableName() +
					" requested.";
				if (!result.message().empty())
					msg += " " + result.message();
				if (
					_functionType->kind() == FunctionType::Kind::BareCall ||
					_functionType->kind() == FunctionType::Kind::BareCallCode ||
					_functionType->kind() == FunctionType::Kind::BareDelegateCall ||
					_functionType->kind() == FunctionType::Kind::BareStaticCall
				)
					return {
						8051_error,
						msg +
						" This function requires a single bytes argument."
						" If all your arguments are value types, you can"
						" use abi.encode(...) to properly generate it."
					};
				else if (
					_functionType->kind() == FunctionType::Kind::KECCAK256 ||
					_functionType->kind() == FunctionType::Kind::SHA256 ||
					_functionType->kind() == FunctionType::Kind::RIPEMD160
				)
					return {
						7556_error,
						msg +
						" This function requires a single bytes argument."
						" Use abi.encodePacked(...) to obtain the pre-0.5.0"
						" behaviour or abi.encode(...) to use ABI encoding."
					};
				else
					return { 9553_error, msg };
			}();
			m_errorReporter.typeError(errorId, paramArgMap[i]->location(), description);
		}
	}

	TypePointers const& returnParameterTypes = _functionType->returnParameterTypes();
	bool isLibraryCall = (_functionType->kind() == FunctionType::Kind::DelegateCall);
	bool callRequiresABIEncoding =
		// ABIEncode/ABIDecode calls not included because they should have been already validated
		// at this point and they have variadic arguments so they need special handling.
		_functionType->kind() == FunctionType::Kind::DelegateCall ||
		_functionType->kind() == FunctionType::Kind::External ||
		_functionType->kind() == FunctionType::Kind::Creation ||
		_functionType->kind() == FunctionType::Kind::Event ||
		_functionType->kind() == FunctionType::Kind::Error;

	if (callRequiresABIEncoding && !useABICoderV2())
	{
		solAssert(!isVariadic, "");
		solAssert(parameterTypes.size() == arguments.size(), "");
		solAssert(!_functionType->isBareCall(), "");
		solAssert(*_functionCall.annotation().kind == FunctionCallKind::FunctionCall, "");

		for (size_t i = 0; i < parameterTypes.size(); ++i)
		{
			solAssert(parameterTypes[i], "");

			if (!typeSupportedByOldABIEncoder(*parameterTypes[i], isLibraryCall))
				m_errorReporter.typeError(
					2443_error,
					paramArgMap[i]->location(),
					"The type of this parameter, " + parameterTypes[i]->humanReadableName() + ", "
					"is only supported in ABI coder v2. "
					"Use \"pragma abicoder v2;\" to enable the feature."
				);
		}

		for (size_t i = 0; i < returnParameterTypes.size(); ++i)
		{
			solAssert(returnParameterTypes[i], "");

			if (!typeSupportedByOldABIEncoder(*returnParameterTypes[i], isLibraryCall))
				m_errorReporter.typeError(
					2428_error,
					_functionCall.location(),
					"The type of return parameter " + toString(i + 1) + ", " + returnParameterTypes[i]->humanReadableName() + ", "
					"is only supported in ABI coder v2. "
					"Use \"pragma abicoder v2;\" to enable the feature."
				);
		}
	}

	auto functionCallOpt = dynamic_cast<const FunctionCallOptions *>(&_functionCall.expression());
	if (functionCallOpt) {
		bool isExternalInboundMessage = _functionCall.isExtMsg();

		std::vector<std::string> arr;
		auto fold = [&]() {
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
		const bool isNewExpression = dynamic_cast<const NewExpression *>(&functionCallOpt->expression()) != nullptr;

		if (isExternalInboundMessage) {
			arr = {"time", "expire", "call", "sign", "pubkey", "abiVer", "callbackId", "onErrorId", "stateInit","signBoxHandle", "flags"};
		} else if (isNewExpression) {
			arr = {"stateInit", "code", "pubkey", "varInit", "splitDepth", "wid", "value", "currencies", "bounce", "flag"};
		} else {
			arr = {"value", "currencies", "bounce", "flag", "callback"};
		}

		auto names = functionCallOpt->names();
		const std::vector<ASTPointer<const Expression>> &options = functionCallOpt->options();
		for (size_t i = 0; i < names.size(); ++i) {
			std::string const &name = *(names[i]);
			if (std::find(arr.begin(), arr.end(), name) == arr.end()) {
				m_errorReporter.typeError(
						4187_error,
						functionCallOpt->location(),
						"Unknown option \"" + name + "\". " +
						"Valid options are " + fold() + "."
				);
			}
			if (name == "pubkey") {
				if (isExternalInboundMessage)
					expectType(*options[i], *TypeProvider::optional(TypeProvider::uint256()));
				else
					expectType(*options[i], *TypeProvider::uint256());
			}
		}
		auto getId = [&](std::string const & param) {
			auto it = find_if(names.begin(), names.end(), [&](auto el){
				return *el == param;
			});
			return it == names.end() ? -1 : it - names.begin();
		};

		auto callback = getId("callback");
		if (callback != -1 && _functionCall.isAwait())
			m_errorReporter.typeError(
					8276_error,
					_functionCall.location(),
					R"("callback" option can't be set for await call.)"
					);
		auto callbackId = getId("callbackId");
		auto onErrorId = getId("onErrorId");
		auto expressionFunctionType = dynamic_cast<FunctionType const*>(type(functionCallOpt->expression()));
		if (!_functionCall.isExtMsg() && callback == -1 &&
				!expressionFunctionType->returnParameterTypes().empty() &&
				!_functionCall.isAwait()) {
			checkNeedCallback(expressionFunctionType, *functionCallOpt);
		}
		if (isExternalInboundMessage &&
		(callbackId == -1 || onErrorId == -1)) {
			m_errorReporter.typeError(
					3365_error,
					functionCallOpt->location(),
					R"("callbackId" and "onErrorId" options must be set.)"
			);
		}


	}

}

FunctionDefinition const*
TypeChecker::checkPubFunctionAndGetDefinition(Expression const& arg, bool printError) {
	FunctionDefinition const* funcDef = getFunctionDefinition(&arg);
	if (funcDef) {
		if (!funcDef->isPublic()) {
			m_errorReporter.fatalTypeError(
					6273_error,
					arg.location(),
					SecondarySourceLocation().append("The declaration is here:", funcDef->location()),
					"Public/external function or contract type required, but \"" +
					Declaration::visibilityToString(funcDef->visibility()) +
					"\" function is provided."
			);
		}
	} else if (printError) {
		m_errorReporter.fatalTypeError(
			4076_error,
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
			4974_error,
			arg.location(),
			"Function or contract type required, but " +
			type(arg)->toString(true) +
			" provided."
		);
	}
	return constructorDef;
}

void TypeChecker::checkInitList(InitializerList const* list, ContractType const& ct,
								langutil::SourceLocation const& _functionCallLocation
) {
	std::vector<VariableDeclaration const*> stateVariables;
	for (auto const &[v, _, ___] : ct.stateVariables()) {
		boost::ignore_unused(_);
		boost::ignore_unused(___);
		stateVariables.push_back(v);
	}

	if (list == nullptr) {
		for (VariableDeclaration const* v : stateVariables) {
			if (v->isStatic()) {
				m_errorReporter.typeError(
					8332_error,
					_functionCallLocation,
					SecondarySourceLocation()
						.append("The declaration of state static variable is here:", v->location()),
					"Expected \"varInit\" option. All static state variables should be defined."
				);
				break;
			}
		}
		return ;
	}

	std::map<std::string, size_t> usedNamedParams;
	for (size_t i = 0; i < list->names().size(); ++i) {
		const std::string name = *list->names().at(i);
		if (usedNamedParams.count(name) != 0) {
			size_t prevIndex = usedNamedParams.at(name);
			m_errorReporter.typeError(
				4019_error,
				list->nameLocations().at(i),
				SecondarySourceLocation()
						.append("Previous named argument is here:", list->nameLocations().at(prevIndex)),
				"Duplicate named argument \"" + name + "\"."
			);
		}
		usedNamedParams[name] = i;
		Type const* exprType = list->options().at(i)->annotation().type;
		size_t j;
		for (j = 0; j < stateVariables.size(); ++j) {
			VariableDeclaration const* v = stateVariables.at(j);
			if (name == v->name()) {
				if (!v->isStatic()) {
					m_errorReporter.typeError(
						6626_error,
						list->nameLocations().at(i),
						SecondarySourceLocation()
								.append("The declaration is here:", v->location()),
						"Initialization of a non-static variable."
					);
				} else if (!exprType->isImplicitlyConvertibleTo(*v->type())) {
					m_errorReporter.typeError(
						2937_error,
						list->options().at(i)->location(),
						"Expected " + v->type()->toString() + " type" +
						" but got " + exprType->toString() + " type."
					);
				}
				break;
			}
		}
		if (j == stateVariables.size()) {
			m_errorReporter.typeError(
				6711_error,
				list->nameLocations().at(i),
				SecondarySourceLocation()
						.append("The contract is here:", ct.contractDefinition().location()),
				"Unknown state variable \"" + name + "\"."
			);
		}
	}

	for (VariableDeclaration const* v : stateVariables) {
		if (v->isStatic() && usedNamedParams.count(v->name()) == 0) {
			m_errorReporter.typeError(
				6515_error,
				list->location(),
				SecondarySourceLocation()
						.append("The declaration of state static variable is here:", v->location()),
				"Expected named argument \"" + v->name() + "\" in the list. All static state variables should be defined."
			);
		}
	}
}

void TypeChecker::checkCallList(
	std::vector<Expression const *> const& arguments,
	FunctionCall const& _functionCall,
	bool ignoreCallBack // for ext msg we set callbackFunctionId==0
) {
	if (arguments.empty()) {
		m_errorReporter.typeError(
				5070_error,
				_functionCall.location(),
				"At least one argument of function or contract type is expected."
		);
		return;

	}
	FunctionDefinition const *functionDeclaration =
			checkPubFunctionOrContractTypeAndGetDefinition(*arguments.front());

	if (functionDeclaration != nullptr) {
		bool needCallback = !ignoreCallBack && functionDeclaration->isResponsible();
		int shift = needCallback ? 1 : 0;
		std::vector<ASTPointer<VariableDeclaration>> const &calleeParams = functionDeclaration->parameters();
		if (1 + shift + calleeParams.size() != arguments.size()) {
			m_errorReporter.typeError(
					5424_error,
					_functionCall.location(),
					SecondarySourceLocation()
							.append("The declaration is here:", functionDeclaration->location()),
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
				checkPubFunctionAndGetDefinition(*arguments.at(1), true);
			}
			for (size_t i = 0; i < calleeParams.size(); i++) {
				expectType(*arguments[1 + shift + i], *calleeParams[i]->annotation().type, false);
			}
		}
	} else {
		if (arguments.size() >= 2) {
			// check default constructor
			auto tt = dynamic_cast<const TypeType *>(arguments.front()->annotation().type);
			auto contractType = dynamic_cast<const ContractType *>(tt->actualType());
			const auto &contractDefinition = contractType->contractDefinition();
			m_errorReporter.fatalTypeError(
					2182_error,
					_functionCall.location(),
					SecondarySourceLocation().append("The declaration is here:",
													 contractDefinition.location()),
					"Wrong arguments count: " +
					toString(arguments.size()) +
					" arguments given but 0 expected. Default constructor has no parameters."
			);
		}
	}
}

void TypeChecker::checkBuildExtMsg(FunctionCall const& _functionCall) {
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	std::vector<ASTPointer<ASTString>> const &argumentNames = _functionCall.names();
	auto findName = [&](const ASTString& optName) {
		auto it = std::find_if(argumentNames.begin(), argumentNames.end(),
							   [&](const ASTPointer<ASTString> &name) {
								   return *name == optName;
							   });
		return it == argumentNames.end()  ? -1 : it - argumentNames.begin();
	};


	for (const std::string name: {"dest", "call", "callbackId", "onErrorId"}) {
		int index = findName(name);
		if (index == -1){
			m_errorReporter.fatalTypeError(
					2651_error,
					_functionCall.location(),
					"Parameter \"" + name + "\" must be set."
			);
		}
	}

	if (
		int SignIndex = findName("sign");
		SignIndex != -1
	){
		auto const& ann = arguments[SignIndex]->annotation();
		if (ann.type->category() != Type::Category::Bool || !*ann.isPure) {
			m_errorReporter.typeError(
					9588_error,
					arguments[SignIndex]->location(),
					"\"sign\" parameter must have a constant boolean type."
			);
		}
	}

	FunctionDefinition const *remoteFunction{};
	int indexCall = findName("call");
	if (indexCall != -1) {
		auto callList = dynamic_cast<CallList const*>(arguments.at(indexCall).get());
		if (callList) {
			std::vector<Expression const*> params;
			params.push_back(callList->function());
			for (const ASTPointer<Expression const>& p : callList->arguments()) {
				params.push_back(p.get());
			}
			checkCallList(params, _functionCall, true);
			remoteFunction = checkPubFunctionOrContractTypeAndGetDefinition(*callList->function());
		}
	}

	int callbackIndex = findName("callbackId");
	int errorIndex = findName("onErrorId");
	FunctionDefinition const* callBackFunction{};
	FunctionDefinition const* errorFunction{};
	for (std::string name : std::vector<std::string>{"callbackId", "onErrorId"}) {
		if (
			int nameIndex = findName(name);
			nameIndex != -1
		) {
			auto const& ann = arguments.at(nameIndex)->annotation();
			auto intType = dynamic_cast<IntegerType const*>(ann.type->mobileType());
			if (name == "callbackId") {
				callBackFunction = checkPubFunctionAndGetDefinition(*arguments.at(nameIndex));
			}
			if (name == "onErrorId") {
				errorFunction = checkPubFunctionAndGetDefinition(*arguments.at(nameIndex));
			}
			if (!(callBackFunction || (intType && !intType->isSigned() && intType->numBits() <= 32))) {
				m_errorReporter.typeError(
					1693_error,
					arguments[nameIndex]->location(),
					std::string{} +
					"Invalid type for argument in function call. " +
					"Expected functionID of uint32 type or just function name (ContractName.functionName or functionName)."
				);
			}
		}
	}


	if (callbackIndex != -1 && remoteFunction != nullptr && callBackFunction != nullptr) {
		checkRemoteAndCallBackFunctions(remoteFunction, callBackFunction, arguments.at(callbackIndex)->location());
	}

	if (errorFunction) {
		checkOnErrorId(errorFunction, arguments.at(errorIndex)->location());
	}
}

void TypeChecker::checkRemoteAndCallBackFunctions(
	FunctionDefinition const* calleeDefinition,
	FunctionDefinition const* callbackFunc,
	SourceLocation const& _location
) {
	ContractDefinition const *callbackContract = callbackFunc->annotation().contract;
	if (!m_currentContract->derivesFrom(*callbackContract)) {
		m_errorReporter.typeError(
			7940_error,
			_location,
			"Callback function should belong to this contract or any of base contracts."
		);
		return ;
	}

	const ParameterList &callbackParams = callbackFunc->parameterList();
	const TypePointers& retTypes = calleeDefinition->functionType({})->returnParameterTypes(); // check function without return
	if (callbackParams.parameters().size() != retTypes.size()) {
		m_errorReporter.typeError(
			2691_error,
			_location,
			SecondarySourceLocation()
				.append("Declaration of the callee function:", calleeDefinition->location())
				.append("Declaration of the callback function:", callbackFunc->location()),
			R"(Count of return parameters of the callee function isn't equal to count of input arguments of the callback function.)"
		);
		return ;
	}

	for (std::size_t p = 0; p < callbackParams.parameters().size(); ++p) {
		if (*callbackParams.parameters().at(p)->type() != *retTypes.at(p)) {
			m_errorReporter.typeError(
				2572_error,
				_location,
				SecondarySourceLocation()
						.append("Parameter of the callee function:", calleeDefinition->returnParameters().at(p)->location())
						.append("Parameter of the callback function:", callbackFunc->parameters().at(p)->location()),
				toString(p + 1) + R"(th parameters of callback and callee functions have different types.)"
			);
		}
	}
}

void TypeChecker::checkOnErrorId(FunctionDefinition const* errorFunction, langutil::SourceLocation const& _location) {
	auto badOnErrorFunction = [&]() {
		m_errorReporter.typeError(
				2992_error,
				_location,
				SecondarySourceLocation().append("Parameters of the error function:", errorFunction->location()),
				"Error function must take (uint32 sdkError, uint32 exitCode)."
		);
	};
	std::vector<ASTPointer<VariableDeclaration>> const& params = errorFunction->parameters();
	if (params.size() != 2) {
		badOnErrorFunction();
	}
	for (ASTPointer<VariableDeclaration> const& v : params) {
		auto intType = dynamic_cast<IntegerType const*>(v->type());
		if (!intType || intType->numBits() != 32) {
			badOnErrorFunction();
		}
	}
}

bool TypeChecker::visit(FunctionCall const& _functionCall)
{
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	std::vector<ASTPointer<ASTString>> const &argumentNames = _functionCall.names();
	bool argumentsArePure = true;

	// We need to check arguments' type first as they will be needed for overload resolution.
	for (ASTPointer<Expression const> const& argument: arguments)
	{
		argument->accept(*this);
		if (!*argument->annotation().isPure)
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
	funcCallAnno.isConstant = false;

	bool isLValue = false;

	for (auto const& argument : arguments)
	{
		auto const& argType = type(*argument);
		if (argType->category() == Type::Category::RationalNumber && !argType->mobileType())
		{
			m_errorReporter.fatalTypeError(
				8009_error,
				argument->location(),
				"Invalid rational number (too large or division by zero)."
				// See also TypeChecker::typeCheckABIEncodeFunctions
			);
		}
	}

	// Determine and assign function call kind, lvalue, purity and function type for this FunctionCall node
	switch (expressionType->category())
	{
	case Type::Category::Function:
		functionType = dynamic_cast<FunctionType const*>(expressionType);
		funcCallAnno.kind = FunctionCallKind::FunctionCall;

		if (auto memberAccess = dynamic_cast<MemberAccess const*>(&_functionCall.expression()))
		{
			if (dynamic_cast<FunctionDefinition const*>(memberAccess->annotation().referencedDeclaration))
				_functionCall.expression().annotation().calledDirectly = true;
		}
		else if (auto identifier = dynamic_cast<Identifier const*>(&_functionCall.expression()))
			if (dynamic_cast<FunctionDefinition const*>(identifier->annotation().referencedDeclaration))
				_functionCall.expression().annotation().calledDirectly = true;

		// Purity for function calls also depends upon the callee and its FunctionType
		funcCallAnno.isPure =
			argumentsArePure &&
			*_functionCall.expression().annotation().isPure &&
			functionType->isPure();

		if (
			functionType->kind() == FunctionType::Kind::ArrayPush ||
			functionType->kind() == FunctionType::Kind::ByteArrayPush
		)
			isLValue = functionType->parameterTypes().empty();
		else if (functionType->kind() == FunctionType::Kind::OptionalGet)
			isLValue = true;
		break;

	case Type::Category::TypeType:
	{
		// Determine type for type conversion or struct construction expressions
		Type const* actualType = dynamic_cast<TypeType const&>(*expressionType).actualType();
		solAssert(!!actualType, "");

		if (actualType->category() == Type::Category::Struct)
		{
			functionType = dynamic_cast<StructType const&>(*actualType).constructorType();
			funcCallAnno.kind = FunctionCallKind::StructConstructorCall;
		}
		else
		{
			if (auto const* contractType = dynamic_cast<ContractType const*>(actualType))
				if (contractType->isSuper())
					m_errorReporter.fatalTypeError(
						1744_error,
						_functionCall.location(),
						"Cannot convert to the super type."
					);
			funcCallAnno.kind = FunctionCallKind::TypeConversion;
		}

		funcCallAnno.isPure = argumentsArePure;

		break;
	}

	default:
		m_errorReporter.fatalTypeError(5704_error, _functionCall.location(), "This expression is not callable.");
		// Unreachable, because fatalTypeError throws. We don't set kind, but that's okay because the switch below
		// is never reached. And, even if it was, SetOnce would trigger an assertion violation and not UB.
		funcCallAnno.isPure = argumentsArePure;
		break;
	}

	funcCallAnno.isLValue = isLValue;

	auto checkArgNumAndIsInteger = [&](
		std::vector<ASTPointer<Expression const>> const& arguments,
		size_t arguments_cnt,
		const std::function<bool(size_t, size_t)>& cmpOperator,
		const std::string& errorMsg
	){
		if (!cmpOperator(arguments.size(), arguments_cnt)) {
			m_errorReporter.fatalTypeError(
				8309_error,
				_functionCall.location(),
				errorMsg
			);
		}

		for (const auto & arg : arguments) {
			auto t = arg->annotation().type;
			Type::Category cat = t->mobileType()->category();
			if (cat != Type::Category::Integer && cat != Type::Category::VarInteger) {
				m_errorReporter.fatalTypeError(
					4283_error,
					arg->location(),
					"Expected an integer or variable integer type."
				);
			}
		}
	};

	auto checkArgQtyAndIsIntOrVarIntOrFixedPoint = [&](
		std::vector<ASTPointer<Expression const>> const& arguments,
		size_t arguments_cnt,
		const std::function<bool(size_t, size_t)>& cmpOperator,
		const std::string& errorMsg
	){
		if (!cmpOperator(arguments.size(), arguments_cnt)) {
			m_errorReporter.fatalTypeError(
				1040_error,
				_functionCall.location(),
				errorMsg
			);
		}

		for (const auto & arg : arguments) {
			Type::Category cat = arg->annotation().type->mobileType()->category();
			if (cat != Type::Category::Integer && cat != Type::Category::FixedPoint && cat != Type::Category::VarInteger) {
				m_errorReporter.fatalTypeError(
					5943_error,
					arg->location(),
					"Expected integer, variable integer or fixed point type."
				);
			}
		}
	};

	auto checkAllAreNotFractions = [&] (std::vector<ASTPointer<Expression const>> const& arguments) {
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
				1164_error,
				loc,
				"Cannot perform operation for constant literals. Please convert at least one function argument to an explicit type."
			);
		}
	};

	auto getCommonType = [&](std::vector<ASTPointer<Expression const>> const& arguments){
		Type const* result = arguments.at(0)->annotation().type;
		for (std::size_t i = 1; i < arguments.size(); ++i) {
			Type const* rightType = arguments.at(i)->annotation().type;
			result = Type::commonType(result, rightType);
			if (result == nullptr) {
				m_errorReporter.fatalTypeError(
						2304_error,
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

	auto checkHasNamedParams = [&]() {
		if (argumentNames.empty())
			m_errorReporter.fatalTypeError(
				8461_error,
				_functionCall.location(),
				std::string("Function call arguments should be given by name.")
			);
	};

	auto checkAtLeastOneArg = [&]() {
		if (arguments.empty()) {
			m_errorReporter.fatalTypeError(5648_error, _functionCall.location(), "Expected at least one argument.");
		}
	};

	// Determine return types
	switch (*funcCallAnno.kind)
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
				const Type *givenType = arguments.at(i)->annotation().type;
				const Type *expType = paramTypes.at(i);
				if (!givenType->isImplicitlyConvertibleTo(*expType)) {
					m_errorReporter.typeError(1580_error, arguments.at(i)->location(),
				  		"Expected " + expType->canonicalName() + " type, but given " + givenType->canonicalName());
				}
			}
		};

		switch (functionType->kind())
		{
		case FunctionType::Kind::ABIDecode:
		{
			if (arguments.size() != 2) {
				m_errorReporter.fatalTypeError(1690_error, _functionCall.location(), "Expected two arguments.");
			}
			expectType(*arguments.at(0), *TypeProvider::tvmcell(), false);
			ASTPointer<Expression const> arg1 = arguments.at(1);
			ASTPointer<TupleExpression const> te = std::dynamic_pointer_cast<TupleExpression const>(arg1);
			std::vector<ASTPointer<Expression const>> args;
			if (te) {
				for (const ASTPointer<Expression>& e : te->components()) {
					args.emplace_back(e);
				}
			} else {
				args.emplace_back(arg1);
			}
			returnTypes = checkSliceDecode(args);
			break;
		}
		case FunctionType::Kind::TVMSliceLoad:
		case FunctionType::Kind::TVMSlicePreload:
		{
			checkAtLeastOneArg();
			returnTypes = checkSliceDecode(_functionCall.arguments());
			break;
		}
		case FunctionType::Kind::TVMSliceLoadQ:
		case FunctionType::Kind::TVMSlicePreloadQ:
		{
			checkAtLeastOneArg();
			TypePointers members = checkSliceDecode(_functionCall.arguments()); // TODO make for Q
			if (members.size() == 1)
				returnTypes = TypePointers{TypeProvider::optional(members.at(0))};
			else
				returnTypes = TypePointers{TypeProvider::optional(TypeProvider::tuple(members))};
			break;
		}
		case FunctionType::Kind::TVMConfigParam:
		{
			returnTypes = getReturnTypesForTVMConfig(_functionCall);
			break;
		}
		case FunctionType::Kind::ABIDecodeFunctionParams: {
			if (arguments.size() != 2) {
				m_errorReporter.fatalTypeError(
					1782_error,
					_functionCall.location(),
					std::string("Expected two arguments.")
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
			paramTypes.emplace_back(arguments.at(0)->annotation().type);
			paramTypes.emplace_back(arguments.at(1)->annotation().type);
			break;
		}
		case FunctionType::Kind::TVMSliceLoadFunctionParams:
		{
			if (arguments.size() != 1) {
				m_errorReporter.fatalTypeError(
					7016_error,
					_functionCall.location(),
					std::string("Expected one argument.")
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
		case FunctionType::Kind::ABIEncodePacked:
		case FunctionType::Kind::ABIEncodeWithSelector:
		case FunctionType::Kind::ABIEncodeWithSignature:
		case FunctionType::Kind::ABIEncodeCall:
		{
			typeCheckABIEncodeFunctions(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::MetaType:
			returnTypes = typeCheckMetaTypeFunctionAndRetrieveReturnType(_functionCall);
			break;
		case FunctionType::Kind::BytesConcat:
		{
			typeCheckBytesConcatFunction(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::StringConcat:
		{
			typeCheckStringConcatFunction(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::Wrap:
		case FunctionType::Kind::Unwrap:
		{
			typeCheckFunctionGeneralChecks(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::ABIDecodeData:
		{
			if (arguments.size() != 2) {
				m_errorReporter.fatalTypeError(
					3017_error,
					_functionCall.location(),
					std::string("Expected two arguments.")
				);
			}
			ContractType const* ct = getContractType(arguments.front().get());
			if (ct == nullptr) {
				m_errorReporter.fatalTypeError(
					8880_error,
					arguments.front()->location(),
					"Expected contract type."
				);
			}

			std::vector<VariableDeclaration const *> stateVars = ::stateVariables(&ct->contractDefinition(), false);
			returnTypes.push_back(TypeProvider::uint256()); // pubkey
			returnTypes.push_back(TypeProvider::uint(64)); // timestamp
			returnTypes.push_back(TypeProvider::boolean()); // constructor flag
			for (VariableDeclaration const * v : stateVars) {
				returnTypes.push_back(v->type());
			}
			paramTypes.emplace_back(arguments.at(0)->annotation().type);
			paramTypes.emplace_back(arguments.at(1)->annotation().type);
			break;
		}
		case FunctionType::Kind::TVMSliceLoadStateVars:
		{
			if (arguments.size() != 1) {
				m_errorReporter.fatalTypeError(
						5457_error,
						_functionCall.location(),
						std::string("Expected one argument.")
				);
			}
			ContractType const* ct = getContractType(arguments.front().get());
			if (ct == nullptr) {
				m_errorReporter.fatalTypeError(
						7161_error,
						arguments.front()->location(), // TODO move
						"Expected contract type."
				);
			}

			std::vector<VariableDeclaration const *> stateVars = ::stateVariables(&ct->contractDefinition(), false);
			returnTypes.push_back(TypeProvider::uint256()); // pubkey
			returnTypes.push_back(TypeProvider::uint(64)); // timestamp
			returnTypes.push_back(TypeProvider::boolean()); // constructor flag
			for (VariableDeclaration const * v : stateVars) {
				returnTypes.push_back(v->type());
			}
			break;
		}
		case FunctionType::Kind::RndNext:
		{
			checkArgNumAndIsInteger(arguments, 1, std::less_equal<>(), "Expected at most one argument.");
			if (arguments.empty()) {
				returnTypes.push_back(TypeProvider::uint256());
			} else {
				Type const* result = arguments.at(0)->annotation().type->mobileType();
				paramTypes.push_back(result);
				returnTypes.push_back(result);
			}
			break;
		}
		case FunctionType::Kind::MathMin:
		case FunctionType::Kind::MathMax:
		{
			checkArgQtyAndIsIntOrVarIntOrFixedPoint(arguments, 2, std::greater_equal<>(), "Expected at least two arguments.");
			Type const* result = getCommonType(arguments);
			paramTypes = TypePointers(arguments.size(), result);
			returnTypes.push_back(result);
			break;
		}
		case FunctionType::Kind::MathMinMax:
		{
			checkArgQtyAndIsIntOrVarIntOrFixedPoint(arguments, 2, std::equal_to<>(), "Expected two arguments.");
			Type const* result = getCommonType(arguments);
			paramTypes = TypePointers(arguments.size(), result);
			returnTypes.push_back(result);
			returnTypes.push_back(result);
			break;
		}
		case FunctionType::Kind::MathDivR:
		case FunctionType::Kind::MathDivC:
		{
			checkArgQtyAndIsIntOrVarIntOrFixedPoint(arguments, 2, std::equal_to<>(), "Expected two arguments.");
			checkAllAreNotFractions(arguments);
			Type const* result = getCommonType(arguments);
			paramTypes = TypePointers(arguments.size(), result);
			returnTypes.push_back(result);
			break;
		}
		case FunctionType::Kind::MathDivMod:
		{
			checkArgNumAndIsInteger(arguments, 2, std::equal_to<>(), "Expected two arguments.");
			Type const* result = getCommonType(arguments);
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
			Type const* result = getCommonType(arguments);
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
			checkArgQtyAndIsIntOrVarIntOrFixedPoint(arguments, 1, std::equal_to<>(), "Expected one argument.");
			Type const* argType = arguments.at(0)->annotation().type->mobileType();
			Type::Category cat = argType->category();
			paramTypes.push_back(argType);

			if (cat == Type::Category::Integer || cat == Type::Category::VarInteger) {
				IntegerType const* intType;
				if (cat == Type::Category::Integer)
					intType = dynamic_cast<IntegerType const*>(argType);
				else if (cat == Type::Category::VarInteger)
					intType = &dynamic_cast<VarIntegerType const*>(argType)->asIntegerType();
				else
					solUnimplemented("");
				if (!intType->isSigned())
					m_errorReporter.fatalTypeError(
						7695_error,
						arguments.at(0)->location(),
						"Expected signed integer type."
					);
				if (intType->numBits() == 1)
					m_errorReporter.fatalTypeError(
						7695_error,
						arguments.at(0)->location(),
						"Too low type."
					);
				returnTypes.push_back(TypeProvider::integer(intType->numBits() - 1, IntegerType::Modifier::Unsigned));
			} else if (cat == Type::Category::FixedPoint) {
				auto fixed = dynamic_cast<FixedPointType const*>(argType);
				if (!fixed->isSigned())
					m_errorReporter.fatalTypeError(
						7030_error,
						arguments.at(0)->location(),
						"Expected signed fixed-point type."
					);
				returnTypes.push_back(TypeProvider::fixedPoint(
					std::max<unsigned>(8, fixed->numBits() - 1),
					fixed->fractionalDigits(),
					FixedPointType::Modifier::Unsigned
				));
			}
			break;
		}
		case FunctionType::Kind::MathModpow2:
		{
			checkArgNumAndIsInteger(arguments, 2, std::equal_to<>(), "Expected two arguments.");
			bool isConst = *arguments[1]->annotation().isPure;
			if (!isConst) {
				m_errorReporter.fatalTypeError(
						8650_error,
						arguments.at(1)->location(),
						"Expected a constant integer type but got " +
						arguments[1]->annotation().type->toString() + "."
				);
			}
			returnTypes.push_back(arguments.at(0)->annotation().type->mobileType());
			break;
		}
		case FunctionType::Kind::ABIEncode:
		case FunctionType::Kind::TVMBuilderStore:
		{
			checkAtLeastOneArg();
			typeCheckTvmEncodeFunctions(_functionCall);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::MappingGetNextKey:
		case FunctionType::Kind::MappingGetMinMax:
		case FunctionType::Kind::MappingDelMinOrMax:
		{

			auto memberAccess = dynamic_cast<const MemberAccess *>(&_functionCall.expression());
			auto mapType = dynamic_cast<const MappingType *>(memberAccess->expression().annotation().type);
			Type const* keyType = mapType->realKeyType();
			Type const* valueType = mapType->valueType();
			if (functionType->kind() == FunctionType::Kind::MappingGetNextKey) {
				if (arguments.size() != 1) {
					m_errorReporter.typeError(1385_error, _functionCall.location(), "Expected one argument.");
				} else {
					auto arg0Type = arguments[0]->annotation().type;
					if (keyType->category() == Type::Category::Integer) {
						checkArgNumAndIsInteger(arguments, 1, std::equal_to<>(), "Expected one argument.");
					} else if (!arg0Type->isImplicitlyConvertibleTo(*keyType)) {
						auto errorMsg = "Type " +
										arg0Type->toString() +
										" is not implicitly convertible to expected type " +
										keyType->toString() + ".";
						m_errorReporter.typeError(3030_error, arguments[0]->location(), errorMsg);
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
					m_errorReporter.typeError(5538_error, arguments[0]->location(), "Expected no arguments.");
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
						1878_error,
						arguments[0]->location(),
						std::string("Expected string literal.")
				);
			}
			auto lit = dynamic_cast<const Literal *>(arguments[0].get());
			if (lit == nullptr) {
				m_errorReporter.fatalTypeError(
					4336_error,
					arguments[0]->location(),
					std::string("Expected string literal.")
				);
			}
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
					2689_error,
					_functionCall.location(),
					std::string("Number of arguments is not equal to the number of placeholders!")
				);
			}
			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::ABIBuildIntMsg: {
			checkHasNamedParams();

			for (const std::string name : {"dest", "call", "value"}) {
				int index = findName(name);
				if (index == -1) {
					m_errorReporter.typeError(
						3252_error,
						_functionCall.location(),
						std::string("Parameter \"" + name + "\" must be set.")
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
					checkPubFunctionAndGetDefinition(*callList->function(), true);

					std::vector<Expression const*> params;
					params.push_back(callList->function());
					for (const ASTPointer<Expression const> & p : callList->arguments()) {
						params.push_back(p.get());
					}
					checkCallList(params, _functionCall, false);
				}
			}

			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::ABIBuildExtMsg: {
			checkHasNamedParams();
			checkBuildExtMsg(_functionCall);

			typeCheckFunctionCall(_functionCall, functionType, {"callbackId", "onErrorId"});
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::ABIEncodeStateInit: {
			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = m_evmVersion.supportsReturndata() ?
						  functionType->returnParameterTypes() :
						  functionType->returnParameterTypesWithoutDynamicTypes();

			typeCheckABIEncodeStateInit(_functionCall, hasName, findName);
			break;
		}
		case FunctionType::Kind::ABIEncodeData: {
			checkHasNamedParams();
			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = m_evmVersion.supportsReturndata() ?
						  functionType->returnParameterTypes() :
						  functionType->returnParameterTypesWithoutDynamicTypes();
			typeCheckABIEncodeData(_functionCall, hasName, findName);
			break;
		}
		case FunctionType::Kind::AddressTransfer: {
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
					3221_error,
					_functionCall.location(),
					std::string("Parameter \"value\" must be set.")
				);
			}
			// parameter names are checked in function below
			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = m_evmVersion.supportsReturndata() ?
						  functionType->returnParameterTypes() :
						  functionType->returnParameterTypesWithoutDynamicTypes();
			break;
		}
		case FunctionType::Kind::ABIFunctionId: {
			if (arguments.size() != 1) {
				m_errorReporter.fatalTypeError(
					7354_error,
					_functionCall.location(),
					"One argument of function type is expected."
				);
			}
			checkPubFunctionOrContractTypeAndGetDefinition(*arguments.front().get());
			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::ABIEncodeBody:
		{
			std::vector<Expression const*> params;
			for (const ASTPointer<Expression const>& p : arguments) {
				params.push_back(p.get());
			}
			checkCallList(params, _functionCall, false);

			typeCheckFunctionCall(_functionCall, functionType);
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::LogTVM: {
			if (arguments.size() != 1) {
				m_errorReporter.typeError(
					8696_error,
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
					m_errorReporter.typeError(1045_error, arguments.at(0)->location(), errMsg);
				} else {
					typeCheckFunctionCall(_functionCall, functionType);
				}
			}
			returnTypes = functionType->returnParameterTypes();
			break;
		}
		case FunctionType::Kind::SHA256: {
			if (arguments.size() != 1) {
				m_errorReporter.fatalTypeError(3449_error, _functionCall.location(), "Expected one argument.");
			}

			Type const* argType = arguments.at(0)->annotation().type->mobileType();
			auto arrayType = dynamic_cast<ArrayType const *>(argType);
			if (!((arrayType && arrayType->isByteArrayOrString()) || dynamic_cast<TvmSliceType const*>(argType))) {
				m_errorReporter.fatalTypeError(7972_error, arguments.at(0)->location(), "Expected bytes, string or TvmSlice type.");
			}
			paramTypes.push_back(argType);
			returnTypes.emplace_back(TypeProvider::uint256());
			break;
		}
		case FunctionType::Kind::Require: {
			paramTypes.push_back(TypeProvider::boolean());
			checkAtLeastOneArg();
			if (arguments.size() >= 2) {
				Type const* type1 = arguments.at(1)->annotation().type;
				auto arr = dynamic_cast<ArrayType const*>(type1);
				if (dynamic_cast<StringLiteralType const*>(type1) || (arr && arr->isString()))
					paramTypes.push_back(TypeProvider::stringStorage());
				else
					paramTypes.push_back(TypeProvider::uint(16));
			}
			if (arguments.size() >= 3) {
				paramTypes.push_back(arguments.at(2)->annotation().type->mobileType());
			}
			if (arguments.size() >= 4) {
				m_errorReporter.typeError(7843_error, _functionCall.location(), "Expected at most 3 arguments.");
			}
			checkArgConversion();
			break;
		}
		case FunctionType::Kind::Revert: {
			if (!arguments.empty()) {
				paramTypes.push_back(TypeProvider::uint(16));
			}
			if (arguments.size() >= 2) {
				paramTypes.push_back(arguments.at(1)->annotation().type->mobileType());
			}
			if (arguments.size() >= 3) {
				m_errorReporter.typeError(1683_error, _functionCall.location(), "Expected at most 2 arguments.");
			}
			checkArgConversion();
			break;
		}
		case FunctionType::Kind::TVMDump: {
			if (arguments.size() != 1) {
				m_errorReporter.typeError(3797_error, _functionCall.location(), "Expected one argument.");
			}
			auto type = arguments[0]->annotation().type->mobileType();
			auto cat = type->category();
			if (cat != Type::Category::Integer && cat !=Type::Category::TvmCell) {
				m_errorReporter.fatalTypeError(
					8093_error,
					arguments[0]->location(),
					"Argument must have a TvmCell or integer type."
				);
			}
			paramTypes.push_back(type);
			break;
		}
		case FunctionType::Kind::TVMHash: {
			if (arguments.size() != 1) {
				m_errorReporter.typeError(1218_error, _functionCall.location(), "Expected one argument.");
			}
			auto type = arguments[0]->annotation().type->mobileType();
			auto cat = type->category();
			if (!isByteArrayOrString(type) && cat != Type::Category::TvmCell && cat != Type::Category::TvmSlice) {
				m_errorReporter.fatalTypeError(
					5802_error,
					arguments[0]->location(),
					"Expected string, bytes, TvmCell or TvmSlice types, but got " + type->toString() + " type."
				);
			}
			paramTypes.push_back(type);
			returnTypes.push_back(TypeProvider::uint256());
			break;
		}
		case FunctionType::Kind::IntCast: {
			if (arguments.size() != 1)
				m_errorReporter.fatalTypeError(5461_error, _functionCall.location(), "Expected one argument.");
			auto typeType = dynamic_cast<TypeType const*>(arguments.at(0)->annotation().type);
			auto actualType = typeType == nullptr ? nullptr : typeType->actualType();
			if (actualType == nullptr || actualType->category() != Type::Category::Integer)
				m_errorReporter.fatalTypeError(5974_error, arguments.at(0)->location(), "Expected integer type name.");
			auto const targetType = dynamic_cast<IntegerType const*>(actualType);
			auto ma = dynamic_cast<MemberAccess const*>(&_functionCall.expression());
			auto const fromType = dynamic_cast<IntegerType const*>(ma->expression().annotation().type);
			if (fromType->isSigned() != targetType->isSigned() && fromType->numBits() != targetType->numBits()) {
				m_errorReporter.typeError(
					7427_error,
					_functionCall.location(),
					"Type of integer and target type must have same sign or bit-size."
				);
			}
			returnTypes.push_back(actualType);
			break;
		}
		default:
		{
			typeCheckFunctionCall(_functionCall, functionType);
			if (functionType->kind() != FunctionType::Kind::External || _functionCall.isAwait()) {
				returnTypes = functionType->returnParameterTypes();
			}
			break;
		}
		}
		if (!funcCallAnno.arguments.has_value()) {
			// make leak sanitizer silent
			funcCallAnno.arguments = FuncCallArguments();
		}
		funcCallAnno.arguments->targetTypes = paramTypes;
		funcCallAnno.type = returnTypes.size() == 1 ?
			std::move(returnTypes.front()) :
			TypeProvider::tuple(std::move(returnTypes));

		break;
	}

	default:
		// for non-callables, ensure error reported and annotate node to void function
		solAssert(m_errorReporter.hasErrors(), "");
		funcCallAnno.kind = FunctionCallKind::FunctionCall;
		funcCallAnno.type = TypeProvider::emptyTuple();
		break;
	}

	if (functionType != nullptr &&
		dynamic_cast<FunctionCallOptions const*>(&_functionCall.expression()) == nullptr &&
		functionType->kind() == FunctionType::Kind::External && !_functionCall.isAwait())
	{
		checkNeedCallback(functionType, _functionCall);
	}

	if (_functionCall.isAwait()) {
		if (functionType == nullptr || functionType->kind() != FunctionType::Kind::External) {
			m_errorReporter.fatalTypeError(
				5451_error,
				_functionCall.location(),
				"\".await\" is supported only for external function calls."
			);
		}
		if (functionType) {
			auto fd = dynamic_cast<FunctionDefinition const*>(&functionType->declaration());
			if (fd && !fd->isResponsible()) {
				m_errorReporter.fatalTypeError(
					9054_error,
					_functionCall.location(),
					"\".await\" is supported only for responsible functions."
				);
			}
		}
	}

	return false;
}

bool TypeChecker::visit(FunctionCallOptions const& _functionCallOptions)
{
	const std::vector<ASTPointer<const Expression>> &options = _functionCallOptions.options();
	solAssert(options.size() == _functionCallOptions.names().size(), "Lengths of name & value arrays differ!");

	_functionCallOptions.expression().annotation().arguments = _functionCallOptions.annotation().arguments;

	_functionCallOptions.expression().accept(*this);

	_functionCallOptions.annotation().isPure = false;
	_functionCallOptions.annotation().isConstant = false;
	_functionCallOptions.annotation().isLValue = false;

	auto expressionFunctionType = dynamic_cast<FunctionType const*>(type(_functionCallOptions.expression()));
	if (!expressionFunctionType)
	{
		m_errorReporter.fatalTypeError(2622_error, _functionCallOptions.location(), "Expected callable expression before call options.");
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
	int setExpire = -1;
	int setTime = -1;
	int setOnError = -1;
    int setSignHandler = -1;
	int setAbiVer = -1;
	int setFlags = -1;

	FunctionType::Kind kind = expressionFunctionType->kind();

	bool isLib = false;
	if (expressionFunctionType->hasDeclaration()) {
		if (auto func = dynamic_cast<FunctionDefinition const *>(&expressionFunctionType->declaration())) {
			if (func->annotation().contract->isLibrary()) {
				isLib = true;
			}
		}
	}
	if (
		!isLib &&
		kind != FunctionType::Kind::Creation &&
		kind != FunctionType::Kind::External &&
		kind != FunctionType::Kind::BareCall &&
		kind != FunctionType::Kind::BareCallCode &&
		kind != FunctionType::Kind::BareDelegateCall &&
		kind != FunctionType::Kind::BareStaticCall
	)
	{
		m_errorReporter.fatalTypeError(
			2193_error,
			_functionCallOptions.location(),
			"Function call options can only be set on external function calls or contract creations."
		);
		return false;
	}

	auto setCheckOption = [&](int& _option, std::string const&& _name, int index)
	{
		if (_option != -1)
			m_errorReporter.typeError(
				9886_error,
				_functionCallOptions.location(),
				"Duplicate option \"" + std::move(_name) + "\"."
			);

		_option = index;
	};


	const bool isNewExpression = dynamic_cast<const NewExpression *>(&_functionCallOptions.expression()) != nullptr;
	auto names = _functionCallOptions.names();
	std::vector<std::string> arr;
	if (isNewExpression)
		arr = {"stateInit", "code", "pubkey", "varInit", "splitDepth", "wid", "value", "currencies", "bounce", "flag"};
	else
		arr = {"time", "expire", "call", "sign",  "pubkey", "abiVer", "callbackId", "onErrorId", "stateInit", "signBoxHandle", "value", "currencies", "bounce", "flag", "callback", "flags"};
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
	for (size_t i = 0; i < names.size(); ++i) {
		std::string const &name = *(names[i]);

		if (std::find(arr.begin(), arr.end(), name) == arr.end()) {
			m_errorReporter.typeError(
				7867_error,
				_functionCallOptions.location(),
				"Unknown option \"" + name + "\". " +
				"Valid options are " + fold() + "."
			);
			// check type of option
		} else if (name == "sign") {
			expectType(*options[i], *TypeProvider::boolean());
			setCheckOption(setSign, "sign", i);
			if (!*options[i]->annotation().isPure)
				m_errorReporter.typeError(
					5289_error,
					_functionCallOptions.location(),
					R"(Option "sign" can be specified only with constant bool value.)");
		} else if (name == "pubkey") {
			setCheckOption(setPubkey, "pubkey", i);
		} else if (name == "signBoxHandle") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(32)));
			setCheckOption(setSignHandler, "signBoxHandle", i);
		} else if (name == "abiVer") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(8)));
			setCheckOption(setAbiVer, "abiVer", i);
		} else if (name == "flags") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::uint(8)));
			setCheckOption(setFlags, "flags", i);
		} else if (name == "stateInit") {
			expectType(*options[i], *TypeProvider::optional(TypeProvider::tvmcell()));
			setCheckOption(setStateInit, "stateInit", i);
		} else if (name == "callbackId") {
			ASTPointer<const Expression> const& exp = options.at(i);
			exp->accept(*this);
			FunctionDefinition const* callBackFunction = checkPubFunctionAndGetDefinition(*exp);
			auto intType = dynamic_cast<IntegerType const*>(type(*exp.get())->mobileType());
			if (!(callBackFunction || (intType && !intType->isSigned() && intType->numBits() <= 32))) {
				m_errorReporter.typeError(
					3344_error,
					options.at(i)->location(),
					"Expected functionID of uint32 type or just function name (ContractName.functionName or functionName)."
				);
			}
			auto remoteFunction = dynamic_cast<FunctionDefinition const*>(&expressionFunctionType->declaration());
			if (callBackFunction) {
				checkRemoteAndCallBackFunctions(remoteFunction, callBackFunction, options.at(i)->location());
			}
			setCheckOption(setCallback, "callbackId", i);
		} else if (name == "onErrorId") {
			ASTPointer<const Expression> const& exp = options.at(i);
			exp->accept(*this);
			FunctionDefinition const* errorFunDef = checkPubFunctionAndGetDefinition(*exp);
			auto intType = dynamic_cast<IntegerType const*>(type(*exp.get())->mobileType());
			if (!(errorFunDef || (intType && !intType->isSigned() && intType->numBits() <= 32))) {
				m_errorReporter.typeError(
					7946_error,
					options.at(i)->location(),
					"Expected functionID of uint32 type or just function name (ContractName.functionName or functionName)."
				);
			}
			if (errorFunDef) {
				checkOnErrorId(errorFunDef, options.at(i)->location());
			}
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
			expectType(*options[i], *TypeProvider::coins());
			setCheckOption(setValue, "value", i);
		} else if (name == "splitDepth") {
			expectType(*options[i], *TypeProvider::uint(8));
			setCheckOption(setSplitDepth, "splitDepth", i);
		} else if (name == "callback") {
			options.at(i)->accept(*this);
			typeCheckCallBack(expressionFunctionType, *options.at(i).get());
			setCheckOption(setCallback, "callback", i);
		} else {
			solUnimplemented("");
		}
	}

	if (isNewExpression) {
		if (setStateInit == -1 && setCode == -1) {
			m_errorReporter.typeError(5391_error, _functionCallOptions.location(), R"(Either option "stateInit" or option "code" must be set.)");
		}
		if (setStateInit != -1 && setPubkey != -1) {
			m_errorReporter.declarationError(
				4939_error,
				options.at(setPubkey)->location(),
				SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", options.at(setStateInit)->location()),
				R"(Option "pubkey" is not compatible with option "stateInit". Only with option "code".)"
			);
		}
		if (setStateInit != -1 && setVarInit != -1) {
			m_errorReporter.declarationError(
				7888_error,
				options.at(setVarInit)->location(),
				SecondarySourceLocation().append(R"(Option "stateInit" is set here: )", options.at(setStateInit)->location()),
				R"(Option "varInit" is not compatible with option "stateInit". Only with option "code".)"
			);
		}
		if (setValue == -1)
			m_errorReporter.typeError(4337_error, _functionCallOptions.location(), R"(Option "value" must be set.)");
		if (setStateInit == -1) {
			auto newExpr = to<NewExpression>(&_functionCallOptions.expression());
			Type const* type = newExpr->typeName().annotation().type;
			auto ct = to<ContractType>(type);
			InitializerList const* list{};
			if (setVarInit != -1)
				list = dynamic_cast<InitializerList const*>(options.at(setVarInit).get());
			checkInitList(list, *ct, _functionCallOptions.location());
		}
	}

	_functionCallOptions.annotation().type = expressionFunctionType;
	return false;
}

void TypeChecker::endVisit(NewExpression const& _newExpression)
{
	Type const* type = _newExpression.typeName().annotation().type;
	solAssert(!!type, "Type name not resolved.");

	_newExpression.annotation().isConstant = false;
	_newExpression.annotation().isLValue = false;

	if (auto contractName = dynamic_cast<UserDefinedTypeName const*>(&_newExpression.typeName()))
	{
		auto contract = dynamic_cast<ContractDefinition const*>(&dereference(contractName->pathNode()));

		if (!contract)
			m_errorReporter.fatalTypeError(5540_error, _newExpression.location(), "Identifier is not a contract.");
		if (contract->isInterface())
			m_errorReporter.fatalTypeError(2971_error, _newExpression.location(), "Cannot instantiate an interface.");
		if (contract->abstract())
			m_errorReporter.typeError(4614_error, _newExpression.location(), "Cannot instantiate an abstract contract.");

		_newExpression.annotation().type = FunctionType::newExpressionType(*contract);
		_newExpression.annotation().isPure = false;
	}
	else if (type->category() == Type::Category::Array)
	{
		if (!type->isDynamicallySized())
			m_errorReporter.typeError(
				3904_error,
				_newExpression.typeName().location(),
				"Length has to be placed in parentheses after the array type for new expression."
			);
		_newExpression.annotation().type = TypeProvider::function(
			TypePointers{TypeProvider::uint256()},
			TypePointers{type},
			strings(1, ""),
			strings(1, ""),
			FunctionType::Kind::ObjectCreation,
			StateMutability::Pure
		);
		_newExpression.annotation().isPure = true;
	}
	else
	{
		_newExpression.annotation().isPure = false;
		m_errorReporter.fatalTypeError(8807_error, _newExpression.location(), "Contract or array type expected.");
	}
}

bool TypeChecker::visit(MemberAccess const& _memberAccess)
{
	_memberAccess.expression().accept(*this);
	Type const* exprType = type(_memberAccess.expression());
	ASTString const& memberName = _memberAccess.memberName();

	auto& annotation = _memberAccess.annotation();

	// Retrieve the types of the arguments if this is used to call a function.
	auto const& arguments = annotation.arguments;
	MemberList::MemberMap possibleMembers = exprType->members(currentDefinitionScope()).membersByName(memberName);
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

	annotation.isConstant = false;

	if (possibleMembers.empty())
	{
		if (to<MemberAccess>(&_memberAccess.expression()) != nullptr)
			if (memberName == "value" || memberName == "flag")
				m_errorReporter.fatalTypeError(
					9836_error,
					_memberAccess.location(),
					std::string("\".value()\" and \".flag()\" functionality is ") +
					"deprecated, use call options in {} instead."
				);

		if (initialMemberCount == 0 && !dynamic_cast<ArraySliceType const*>(exprType))
		{
			// Try to see if the member was removed because it is only available for storage types.
			auto storageType = exprType;
			if (!storageType->members(currentDefinitionScope()).membersByName(memberName).empty())
				m_errorReporter.fatalTypeError(
					4994_error,
					_memberAccess.location(),
					"Member \"" + memberName + "\" is not available in " +
					exprType->humanReadableName() +
					" outside of storage."
				);
		}

		auto [errorId, description] = [&]() -> std::tuple<ErrorId, std::string> {
			std::string errorMsg = "Member \"" + memberName + "\" not found or not visible "
				"after argument-dependent lookup in " + exprType->humanReadableName() + ".";

			if (auto const* funType = dynamic_cast<FunctionType const*>(exprType))
			{
				TypePointers const& t = funType->returnParameterTypes();

				if (memberName == "value")
				{
					if (funType->kind() == FunctionType::Kind::Creation)
						return {
							8827_error,
							"Constructor for " + t.front()->humanReadableName() + " must be payable for member \"value\" to be available."
						};
					else if (
						funType->kind() == FunctionType::Kind::DelegateCall ||
						funType->kind() == FunctionType::Kind::BareDelegateCall
					)
						return { 8477_error, "Member \"value\" is not allowed in delegated calls due to \"msg.value\" persisting." };
					else
						return { 8820_error, "Member \"value\" is only available for payable functions." };
				}
				else if (
					t.size() == 1 && (
						t.front()->category() == Type::Category::Struct ||
						t.front()->category() == Type::Category::Contract
					)
				)
					return { 6005_error, errorMsg + " Did you intend to call the function?" };
			}

			return { 9582_error, errorMsg };
		}();

		m_errorReporter.fatalTypeError(
			errorId,
			_memberAccess.location(),
			description
		);
	}
	else if (possibleMembers.size() > 1)
		m_errorReporter.fatalTypeError(
			6675_error,
			_memberAccess.location(),
			"Member \"" + memberName + "\" not unique "
			"after argument-dependent lookup in " + exprType->humanReadableName() +
			(memberName == "value" ? " - did you forget the \"payable\" modifier?" : ".")
		);

	annotation.referencedDeclaration = possibleMembers.front().declaration;
	annotation.type = possibleMembers.front().type;

	VirtualLookup requiredLookup = VirtualLookup::Static;

	if (auto funType = dynamic_cast<FunctionType const*>(annotation.type))
	{
		solAssert(
			!funType->hasBoundFirstArgument() || exprType->isImplicitlyConvertibleTo(*funType->selfType()),
			"Function \"" + memberName + "\" cannot be called on an object of type " +
			exprType->humanReadableName() + " (expected " + funType->selfType()->humanReadableName() + ")."
		);

		if (
			dynamic_cast<FunctionType const*>(exprType) &&
			!annotation.referencedDeclaration &&
			(memberName == "value" || memberName == "gas")
		)
			m_errorReporter.typeError(
				1621_error,
				_memberAccess.location(),
				"Using \"." + memberName + "(...)\" is deprecated. Use \"{" + memberName + ": ...}\" instead."
			);

		if (!funType->hasBoundFirstArgument())
			if (auto typeType = dynamic_cast<TypeType const*>(exprType))
			{
				auto contractType = dynamic_cast<ContractType const*>(typeType->actualType());
				if (contractType && contractType->isSuper())
					requiredLookup = VirtualLookup::Super;
			}
	}

	annotation.requiredLookup = requiredLookup;

	if (dynamic_cast<StructType const*>(exprType))
		annotation.isLValue = true;
	else if (exprType->category() == Type::Category::Array)
		annotation.isLValue = false;
	else if (exprType->category() == Type::Category::FixedBytes)
		annotation.isLValue = false;
	else if (TypeType const* typeType = dynamic_cast<decltype(typeType)>(exprType))
	{
		if (ContractType const* contractType = dynamic_cast<decltype(contractType)>(typeType->actualType()))
		{
			annotation.isLValue = annotation.referencedDeclaration->isLValue();
			if (
				auto const* functionType = dynamic_cast<FunctionType const*>(annotation.type);
				functionType &&
				functionType->kind() == FunctionType::Kind::Declaration
			)
				annotation.isPure = *_memberAccess.expression().annotation().isPure;
		}
		else
			annotation.isLValue = false;
	}
	else if (exprType->category() == Type::Category::Module)
	{
		annotation.isPure = *_memberAccess.expression().annotation().isPure;
		annotation.isLValue = false;
	}
	else
		annotation.isLValue = false;

	// TODO some members might be pure, but for example `address(0x123).balance` is not pure
	// although every subexpression is, so leaving this limited for now.
	if (auto tt = dynamic_cast<TypeType const*>(exprType))
	{
		if (
			tt->actualType()->category() == Type::Category::Enum ||
			tt->actualType()->category() == Type::Category::UserDefinedValueType
		)
			annotation.isPure = true;
		else if (tt->actualType()->category() == Type::Category::Address) {
			if (memberName == "makeAddrStd" || memberName == "addrNone" || memberName == "makeAddrExtern") {
				annotation.isPure = true;
			}
		}
	}
	if (
		auto const* functionType = dynamic_cast<FunctionType const*>(exprType);
		functionType &&
		functionType->hasDeclaration() &&
		dynamic_cast<FunctionDefinition const*>(&functionType->declaration()) &&
		memberName == "selector"
	)
		if (auto const* parentAccess = dynamic_cast<MemberAccess const*>(&_memberAccess.expression()))
		{
			bool isPure = *parentAccess->expression().annotation().isPure;
			if (auto const* exprInt = dynamic_cast<Identifier const*>(&parentAccess->expression()))
				if (exprInt->name() == "this" || exprInt->name() == "super")
					isPure = true;

			annotation.isPure = isPure;
		}
	if (
		auto const* varDecl = dynamic_cast<VariableDeclaration const*>(annotation.referencedDeclaration);
		!annotation.isPure.set() &&
		varDecl &&
		varDecl->isConstant()
	)
		annotation.isPure = true;

	if (auto magicType = dynamic_cast<MagicType const*>(exprType))
	{
		if (magicType->kind() == MagicType::Kind::ABI)
			annotation.isPure = true;
		else if (magicType->kind() == MagicType::Kind::MetaType && (
			memberName == "creationCode" || memberName == "runtimeCode"
		))
		{
			annotation.isPure = true;
			ContractType const& accessedContractType = dynamic_cast<ContractType const&>(*magicType->typeArgument());
			solAssert(!accessedContractType.isSuper(), "");
			if (
				memberName == "runtimeCode" &&
				!accessedContractType.immutableVariables().empty()
			)
				m_errorReporter.typeError(
					9274_error,
					_memberAccess.location(),
					"\"runtimeCode\" is not available for contracts containing immutable variables."
				);
		}
		else if (magicType->kind() == MagicType::Kind::MetaType && memberName == "name")
			annotation.isPure = true;
		else if (magicType->kind() == MagicType::Kind::MetaType && memberName == "interfaceId")
			annotation.isPure = true;
		else if (
			magicType->kind() == MagicType::Kind::MetaType &&
			(memberName == "min" ||	memberName == "max")
		)
			annotation.isPure = true;
		else if (magicType->kind() == MagicType::Kind::Block)
		{
			if (memberName == "chainid" && !m_evmVersion.hasChainID())
				m_errorReporter.typeError(
					3081_error,
					_memberAccess.location(),
					"\"chainid\" is not supported by the VM version."
				);
			else if (memberName == "basefee" && !m_evmVersion.hasBaseFee())
				m_errorReporter.typeError(
					5921_error,
					_memberAccess.location(),
					"\"basefee\" is not supported by the VM version."
				);
			else if (memberName == "blobbasefee" && !m_evmVersion.hasBlobBaseFee())
				m_errorReporter.typeError(
					1006_error,
					_memberAccess.location(),
					"\"blobbasefee\" is not supported by the VM version."
				);
			else if (memberName == "prevrandao" && !m_evmVersion.hasPrevRandao())
				m_errorReporter.warning(
					9432_error,
					_memberAccess.location(),
					"\"prevrandao\" is not supported by the VM version and will be treated as \"difficulty\"."
				);
			else if (memberName == "difficulty" && m_evmVersion.hasPrevRandao())
				m_errorReporter.warning(
					8417_error,
					_memberAccess.location(),
					"Since the VM version paris, \"difficulty\" was replaced by \"prevrandao\", which now returns a random number based on the beacon chain."
				);
		}
	}

	if (
		_memberAccess.expression().annotation().type->category() == Type::Category::Address &&
		memberName == "codehash" &&
		!m_evmVersion.hasExtCodeHash()
	)
		m_errorReporter.typeError(
			7598_error,
			_memberAccess.location(),
			"\"codehash\" is not supported by the VM version."
		);

	if (!annotation.isPure.set())
		annotation.isPure = false;

	return false;
}

bool TypeChecker::visit(IndexAccess const& _access)
{
	_access.annotation().isConstant = false;
	_access.baseExpression().accept(*this);
	Type const* baseType = type(_access.baseExpression());
	Type const* resultType = nullptr;
	bool isLValue = false;
	bool isPure = *_access.baseExpression().annotation().isPure;
	Expression const* index = _access.indexExpression();
	switch (baseType->category())
	{
	case Type::Category::ArraySlice:
	{
		auto const& arrayType = dynamic_cast<ArraySliceType const&>(*baseType).arrayType();
		if (!arrayType.isDynamicallySized())
			m_errorReporter.typeError(4802_error, _access.location(), "Index access is only implemented for slices of dynamic calldata arrays.");
		baseType = &arrayType;
		[[fallthrough]];
	}
	case Type::Category::Array:
	{
		ArrayType const& actualType = dynamic_cast<ArrayType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(9689_error, _access.location(), "Index expression cannot be omitted.");
		else if (actualType.isString())
		{
			m_errorReporter.typeError(9961_error, _access.location(), "Index access for string is not possible.");
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
						m_errorReporter.typeError(3383_error, _access.location(), "Out of bounds array access.");
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
			m_errorReporter.typeError(1267_error, _access.location(), "Index expression cannot be omitted.");
		else
			expectType(*index, *actualType.keyType());
		resultType = actualType.valueType();
		isLValue = true;
		break;
	}
	case Type::Category::TypeType:
	{
		TypeType const& typeType = dynamic_cast<TypeType const&>(*baseType);
		if (auto const* contractType = dynamic_cast<ContractType const*>(typeType.actualType()))
			if (contractType->contractDefinition().isLibrary())
				m_errorReporter.typeError(2876_error, _access.location(), "Index access for library types and arrays of libraries are not possible.");
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
					m_errorReporter.fatalTypeError(3940_error, index->location(), "Integer constant expected.");
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
			m_errorReporter.typeError(8830_error, _access.location(), "Index expression cannot be omitted.");
		else
		{
			if (!expectType(*index, *TypeProvider::uint256()))
				m_errorReporter.fatalTypeError(6318_error, _access.location(), "Index expression cannot be represented as an unsigned integer.");
			if (auto integerType = dynamic_cast<RationalNumberType const*>(type(*index)))
				if (bytesType.numBytes() <= integerType->literalValue(nullptr))
					m_errorReporter.typeError(1859_error, _access.location(), "Out of bounds array access.");
		}
		resultType = TypeProvider::fixedBytes(1);
		isLValue = false; // @todo this heavily depends on how it is embedded
		break;
	}
	case Type::Category::TvmVector:
	{
		TvmVectorType const& actualType = dynamic_cast<TvmVectorType const&>(*baseType);
		if (!index)
			m_errorReporter.typeError(5357_error, _access.location(), "Index expression cannot be omitted.");
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
			2614_error,
			_access.baseExpression().location(),
			"Indexed expression has to be a type, mapping or array (is " + baseType->humanReadableName() + ")"
		);
	}
	_access.annotation().type = resultType;
	_access.annotation().isLValue = isLValue;
	if (index && !*index->annotation().isPure)
		isPure = false;
	_access.annotation().isPure = isPure;

	return false;
}

bool TypeChecker::visit(IndexRangeAccess const& _access)
{
	_access.annotation().isConstant = false;
	_access.baseExpression().accept(*this);

	bool isLValue = false; // TODO: set this correctly when implementing slices for memory and storage arrays
	bool isPure = *_access.baseExpression().annotation().isPure;

	if (Expression const* start = _access.startExpression())
	{
		expectType(*start, *TypeProvider::uint256());
		if (!*start->annotation().isPure)
			isPure = false;
	}
	if (Expression const* end = _access.endExpression())
	{
		expectType(*end, *TypeProvider::uint256());
		if (!*end->annotation().isPure)
			isPure = false;
	}

	_access.annotation().isLValue = isLValue;
	_access.annotation().isPure = isPure;

	Type const* exprType = type(_access.baseExpression());
	if (exprType->category() == Type::Category::TypeType)
	{
		m_errorReporter.typeError(1760_error, _access.location(), "Types cannot be sliced.");
		_access.annotation().type = exprType;
		return false;
	}

	ArrayType const* arrayType = nullptr;
	if (auto const* arraySlice = dynamic_cast<ArraySliceType const*>(exprType))
		arrayType = &arraySlice->arrayType();
	else if (!(arrayType = dynamic_cast<ArrayType const*>(exprType)))
		m_errorReporter.fatalTypeError(4781_error, _access.location(), "Index range access is only possible for arrays and array slices.");

	if (!arrayType->isDynamicallySized())
		m_errorReporter.typeError(1227_error, _access.location(), "Index range access is only supported for dynamic calldata arrays.");
	_access.annotation().type = arrayType;
	return false;
}

std::vector<Declaration const*> TypeChecker::cleanOverloadedDeclarations(
	Identifier const& _identifier,
	std::vector<Declaration const*> const& _candidates
)
{
	solAssert(_candidates.size() > 1, "");
	std::vector<Declaration const*> uniqueDeclarations;

	for (Declaration const* declaration: _candidates)
	{
		solAssert(declaration, "");
		// the declaration is functionDefinition, eventDefinition or a VariableDeclaration while declarations > 1
		solAssert(
			dynamic_cast<FunctionDefinition const*>(declaration) ||
			dynamic_cast<EventDefinition const*>(declaration) ||
			dynamic_cast<VariableDeclaration const*>(declaration) ||
			dynamic_cast<MagicVariableDeclaration const*>(declaration),
			"Found overloading involving something not a function, event or a (magic) variable."
		);

		FunctionTypePointer functionType {declaration->functionType(false)};
		if (!functionType)
			functionType = declaration->functionType(true);
		solAssert(functionType, "Failed to determine the function type of the overloaded.");

		for (Type const* parameter: functionType->parameterTypes() + functionType->returnParameterTypes())
			if (!parameter)
				m_errorReporter.fatalDeclarationError(3893_error, _identifier.location(), "Function type can not be used in this context.");

		if (uniqueDeclarations.end() == find_if(
			uniqueDeclarations.begin(),
			uniqueDeclarations.end(),
			[&](Declaration const* d)
			{
				FunctionType const* newFunctionType = d->functionType(false);
				if (!newFunctionType)
					newFunctionType = d->functionType(true);
				return newFunctionType && functionType->hasEqualParameterTypes(*newFunctionType);
			}
		))
			uniqueDeclarations.push_back(declaration);
	}
	return uniqueDeclarations;
}

bool TypeChecker::visit(Identifier const& _identifier)
{
	IdentifierAnnotation& annotation = _identifier.annotation();

	if (!annotation.referencedDeclaration)
	{
		annotation.overloadedDeclarations = cleanOverloadedDeclarations(_identifier, annotation.candidateDeclarations);
		if (annotation.overloadedDeclarations.empty())
			m_errorReporter.fatalTypeError(7593_error, _identifier.location(), "No candidates for overload resolution found.");
		else if (annotation.overloadedDeclarations.size() == 1)
			annotation.referencedDeclaration = *annotation.overloadedDeclarations.begin();
		else if (!annotation.arguments)
		{
			// The identifier should be a public state variable shadowing other functions
			std::vector<Declaration const*> candidates;

			for (Declaration const* declaration: annotation.overloadedDeclarations)
			{
				if (VariableDeclaration const* variableDeclaration = dynamic_cast<decltype(variableDeclaration)>(declaration))
					candidates.push_back(declaration);
			}
			if (candidates.empty())
				m_errorReporter.fatalTypeError(2144_error, _identifier.location(), "No matching declaration found after variable lookup.");
			else if (candidates.size() == 1)
				annotation.referencedDeclaration = candidates.front();
			else
				m_errorReporter.fatalTypeError(7589_error, _identifier.location(), "No unique declaration found after variable lookup.");
		}
		else
		{
			std::vector<Declaration const*> candidates;

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
						std::string description;
						for (auto const& param: declaration->functionType(true)->parameterTypes())
							description += (description.empty() ? "" : ", ") + param->humanReadableName();
						description = "function " + _identifier.name() + "(" + description + ")";

						ssl.append("Candidate: " + description, declaration->location());
					}
					else
						ssl.append("Candidate:", declaration->location());
				if (candidates.empty())
					m_errorReporter.fatalTypeError(9322_error, _identifier.location(), ssl, "No matching declaration found after argument-dependent lookup.");
				else
					m_errorReporter.fatalTypeError(4487_error, _identifier.location(), ssl, "No unique declaration found after argument-dependent lookup.");
			}
		}
	}
	solAssert(
		!!annotation.referencedDeclaration,
		"Referenced declaration is null after overload resolution."
	);
	bool isConstant = false;
	annotation.isLValue = annotation.referencedDeclaration->isLValue();
	annotation.type = annotation.referencedDeclaration->type();
	solAssert(annotation.type, "Declaration referenced before type could be determined.");
	if (auto variableDeclaration = dynamic_cast<VariableDeclaration const*>(annotation.referencedDeclaration))
		annotation.isPure = isConstant = variableDeclaration->isConstant();
	else if (dynamic_cast<MagicVariableDeclaration const*>(annotation.referencedDeclaration))
		annotation.isPure = dynamic_cast<FunctionType const*>(annotation.type);
	else if (dynamic_cast<TypeType const*>(annotation.type))
		annotation.isPure = true;
	else if (dynamic_cast<ModuleType const*>(annotation.type))
		annotation.isPure = true;
	else
		annotation.isPure = false;

	annotation.isConstant = isConstant;

	annotation.requiredLookup =
		dynamic_cast<CallableDeclaration const*>(annotation.referencedDeclaration) ?
		VirtualLookup::Virtual : VirtualLookup::Static;

	// Check for deprecated function names.
	// The check is done here for the case without an actual function call.
	if (FunctionType const* fType = dynamic_cast<FunctionType const*>(_identifier.annotation().type))
	{
		if (_identifier.name() == "sha3" && fType->kind() == FunctionType::Kind::KECCAK256)
			m_errorReporter.typeError(
				3557_error,
				_identifier.location(),
				"\"sha3\" has been deprecated in favour of \"keccak256\"."
			);
	}

	if (
		MagicVariableDeclaration const* magicVar =
		dynamic_cast<MagicVariableDeclaration const*>(annotation.referencedDeclaration)
	)
		if (magicVar->type()->category() == Type::Category::Integer)
		{
			solAssert(_identifier.name() == "now", "");
			m_errorReporter.typeError(
				7359_error,
				_identifier.location(),
				"\"now\" has been deprecated. Use \"block.timestamp\" instead."
			);
		}

	return false;
}

void TypeChecker::endVisit(IdentifierPath const& _identifierPath)
{
	if (
		dynamic_cast<CallableDeclaration const*>(_identifierPath.annotation().referencedDeclaration) &&
		_identifierPath.path().size() == 1
	)
		_identifierPath.annotation().requiredLookup = VirtualLookup::Virtual;
	else
		_identifierPath.annotation().requiredLookup = VirtualLookup::Static;
}

void TypeChecker::endVisit(UserDefinedTypeName const& _userDefinedTypeName)
{
	if (!_userDefinedTypeName.annotation().type)
		_userDefinedTypeName.annotation().type = _userDefinedTypeName.pathNode().annotation().referencedDeclaration->type();
}

void TypeChecker::endVisit(ElementaryTypeNameExpression const& _expr)
{
	_expr.annotation().type = TypeProvider::typeType(TypeProvider::fromElementaryTypeName(_expr.type().typeName()));
	_expr.annotation().isPure = true;
	_expr.annotation().isLValue = false;
	_expr.annotation().isConstant = false;
}

void TypeChecker::endVisit(MappingNameExpression const& _expr)
{
	_expr.annotation().type = TypeProvider::typeType(TypeProvider::mapping(
		_expr.type().keyType().annotation().type,
		"",
		_expr.type().valueType().annotation().type,
		""
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
			5145_error,
			_literal.location(),
			"Hexadecimal numbers cannot be used with unit denominations. "
			"You can use an expression of the form \"0x1234 * 1 day\" instead."
		);

	if (_literal.subDenomination() == Literal::SubDenomination::Year)
		m_errorReporter.typeError(
			4820_error,
			_literal.location(),
			"Using \"years\" as a unit denomination is deprecated."
		);

	if (!_literal.annotation().type)
		_literal.annotation().type = TypeProvider::forLiteral(_literal);

	if (!_literal.annotation().type)
		m_errorReporter.fatalTypeError(2826_error, _literal.location(), "Invalid literal value.");

	_literal.annotation().isPure = true;
	_literal.annotation().isLValue = false;
	_literal.annotation().isConstant = false;
}

bool TypeChecker::visit(Mapping const& _mapping)
{
	if (auto const* keyType = dynamic_cast<UserDefinedTypeName const*>(&_mapping.keyType()))
	{
		if (auto const* contractType = dynamic_cast<ContractType const*>(keyType->annotation().type))
		{
			if (contractType->contractDefinition().isLibrary())
				m_errorReporter.typeError(
					3734_error,
					keyType->location(),
					"Library types cannot be used as mapping keys."
				);
		}
		else if (
            keyType->annotation().type->category() != Type::Category::Enum &&
            keyType->annotation().type->category() != Type::Category::Struct
        )
			m_errorReporter.typeError(
				5411_error,
				keyType->location(),
				"Only elementary types, contract types, structures (fitted in one cell) or enums are allowed as mapping keys."
			);
	}
	else
		solAssert(dynamic_cast<ElementaryTypeName const*>(&_mapping.keyType()), "");
	return true;
}

void TypeChecker::endVisit(UsingForDirective const& _usingFor)
{
	if (_usingFor.global())
	{
		if (m_currentContract || !_usingFor.typeName())
		{
			solAssert(m_errorReporter.hasErrors());
			return;
		}
		Type const* usingForType = _usingFor.typeName()->annotation().type;
		solAssert(usingForType);
		if (Declaration const* typeDefinition = usingForType->typeDefinition())
		{
			if (typeDefinition->scope() != m_currentSourceUnit)
				m_errorReporter.typeError(
					4117_error,
					_usingFor.location(),
					"Can only use \"global\" with types defined in the same source unit at file level."
				);
		}
		else
			m_errorReporter.typeError(
				8841_error,
				_usingFor.location(),
				"Can only use \"global\" with user-defined types."
			);
	}

	if (!_usingFor.usesBraces())
	{
		solAssert(_usingFor.functionsOrLibrary().size() == 1);
		ContractDefinition const* library = dynamic_cast<ContractDefinition const*>(
			_usingFor.functionsOrLibrary().front()->annotation().referencedDeclaration
		);
		solAssert(library && library->isLibrary());
		// No type checking for libraries
		return;
	}

	if (!_usingFor.typeName())
	{
		solAssert(m_errorReporter.hasErrors());
		return;
	}

	Type const* usingForType = _usingFor.typeName()->annotation().type;
	solAssert(usingForType);

	Type const* normalizedType = usingForType;
	solAssert(normalizedType);

	for (auto const& [path, operator_]: _usingFor.functionsAndOperators())
	{
		solAssert(path->annotation().referencedDeclaration);
		FunctionDefinition const& functionDefinition =
			dynamic_cast<FunctionDefinition const&>(*path->annotation().referencedDeclaration);

		FunctionType const* functionType = dynamic_cast<FunctionType const*>(
			functionDefinition.libraryFunction() ?
				functionDefinition.typeViaContractName() :
				functionDefinition.type()
			);

		solAssert(functionType);

		if (functionDefinition.parameters().empty())
			m_errorReporter.fatalTypeError(
				4731_error,
				path->location(),
				SecondarySourceLocation().append(
					"Function defined here:",
					functionDefinition.location()
				),
				fmt::format(
					"The function \"{}\" does not have any parameters, and therefore cannot be attached to the type \"{}\".",
					joinHumanReadable(path->path(), "."),
					normalizedType ? normalizedType->toString(true /* withoutDataLocation */) : "*"
				)
			);

		if (
			functionDefinition.visibility() == Visibility::Private &&
			functionDefinition.scope() != m_currentContract
		)
		{
			solAssert(functionDefinition.libraryFunction());
			m_errorReporter.typeError(
				6772_error,
				path->location(),
				SecondarySourceLocation().append(
					"Function defined here:",
					functionDefinition.location()
				),
				fmt::format(
					"Function \"{}\" is private and therefore cannot be attached"
					" to a type outside of the library where it is defined.",
					joinHumanReadable(path->path(), ".")
				)
			);
		}

		FunctionType const* functionTypeWithBoundFirstArgument = functionType->withBoundFirstArgument();
		solAssert(functionTypeWithBoundFirstArgument && functionTypeWithBoundFirstArgument->selfType(), "");
		BoolResult result = normalizedType->isImplicitlyConvertibleTo(
			*functionTypeWithBoundFirstArgument->selfType()
		);
		if (!result && !operator_)
			m_errorReporter.typeError(
				3100_error,
				path->location(),
				SecondarySourceLocation().append(
					"Function defined here:",
					functionDefinition.location()
				),
				fmt::format(
					"The function \"{}\" cannot be attached to the type \"{}\" because the type cannot "
					"be implicitly converted to the first argument of the function (\"{}\"){}",
					joinHumanReadable(path->path(), "."),
					usingForType->toString(true /* withoutDataLocation */),
					functionTypeWithBoundFirstArgument->selfType()->humanReadableName(),
					result.message().empty() ? "." : ": " +  result.message()
				)
			);
		else if (operator_.has_value())
		{
			if (!_usingFor.global())
				m_errorReporter.typeError(
					3320_error,
					path->location(),
					"Operators can only be defined in a global 'using for' directive."
				);

			if (
				functionType->stateMutability() != StateMutability::Pure ||
				!functionDefinition.isFree()
			)
				m_errorReporter.typeError(
					7775_error,
					path->location(),
					SecondarySourceLocation().append(
						"Function defined as non-pure here:",
						functionDefinition.location()
					),
					"Only pure free functions can be used to define operators."
				);

			solAssert(!functionType->hasBoundFirstArgument());
			TypePointers const& parameterTypes = functionType->parameterTypes();
			size_t const parameterCount = parameterTypes.size();
			if (usingForType->category() != Type::Category::UserDefinedValueType)
			{
				m_errorReporter.typeError(
					5332_error,
					path->location(),
					"Operators can only be implemented for user-defined value types."
				);
				continue;
			}
			solAssert(usingForType->typeDefinition());

			bool identicalFirstTwoParameters = (parameterCount < 2 || *parameterTypes.at(0) == *parameterTypes.at(1));
			bool isUnaryOnlyOperator = (!TokenTraits::isBinaryOp(operator_.value()) && TokenTraits::isUnaryOp(operator_.value()));
			bool isBinaryOnlyOperator = (TokenTraits::isBinaryOp(operator_.value()) && !TokenTraits::isUnaryOp(operator_.value()));
			bool firstParameterMatchesUsingFor = parameterCount == 0 || *usingForType == *parameterTypes.front();

			std::optional<std::string> wrongParametersMessage;
			if (isBinaryOnlyOperator && (parameterCount != 2 || !identicalFirstTwoParameters))
				wrongParametersMessage = fmt::format("two parameters of type {} and the same data location", usingForType->canonicalName());
			else if (isUnaryOnlyOperator && (parameterCount != 1 || !firstParameterMatchesUsingFor))
				wrongParametersMessage = fmt::format("exactly one parameter of type {}", usingForType->canonicalName());
			else if (parameterCount >= 3 || !firstParameterMatchesUsingFor || !identicalFirstTwoParameters)
				wrongParametersMessage = fmt::format("one or two parameters of type {} and the same data location", usingForType->canonicalName());

			if (wrongParametersMessage.has_value())
				m_errorReporter.typeError(
					1884_error,
					functionDefinition.parameterList().location(),
					SecondarySourceLocation().append(
						"Function was used to implement an operator here:",
						path->location()
					),
					fmt::format(
						"Wrong parameters in operator definition. "
						"The function \"{}\" needs to have {} to be used for the operator {}.",
						joinHumanReadable(path->path(), "."),
						wrongParametersMessage.value(),
						TokenTraits::friendlyName(operator_.value())
					)
				);

			// This case is separately validated for all attached functions and is a fatal error
			solAssert(parameterCount != 0);

			TypePointers const& returnParameterTypes = functionType->returnParameterTypes();
			size_t const returnParameterCount = returnParameterTypes.size();

			std::optional<std::string> wrongReturnParametersMessage;
			if (!TokenTraits::isCompareOp(operator_.value()) && operator_.value() != Token::Not)
			{
				if (returnParameterCount != 1 || *usingForType != *returnParameterTypes.front())
					wrongReturnParametersMessage = "exactly one value of type " + usingForType->canonicalName();
				else if (*returnParameterTypes.front() != *parameterTypes.front())
					wrongReturnParametersMessage = "a value of the same type and data location as its parameters";
			}
			else if (returnParameterCount != 1 || *returnParameterTypes.front() != *TypeProvider::boolean())
				wrongReturnParametersMessage = "exactly one value of type bool";

			solAssert(functionDefinition.returnParameterList());
			if (wrongReturnParametersMessage.has_value())
				m_errorReporter.typeError(
					7743_error,
					functionDefinition.returnParameterList()->location(),
					SecondarySourceLocation().append(
						"Function was used to implement an operator here:",
						path->location()
					),
					fmt::format(
						"Wrong return parameters in operator definition. "
						"The function \"{}\" needs to return {} to be used for the operator {}.",
						joinHumanReadable(path->path(), "."),
						wrongReturnParametersMessage.value(),
						TokenTraits::friendlyName(operator_.value())
					)
				);

			if (parameterCount != 1 && parameterCount != 2)
				solAssert(m_errorReporter.hasErrors());
			else
			{
				// TODO: This is pretty inefficient. For every operator binding we find, we're
				// traversing all bindings in all `using for` directives in the current scope.
				std::set<FunctionDefinition const*, ASTNode::CompareByID> matchingDefinitions = usingForType->operatorDefinitions(
					operator_.value(),
					*currentDefinitionScope(),
					parameterCount == 1 // _unary
				);

				if (matchingDefinitions.size() >= 2)
				{
					// TODO: We should point at other places that bind the operator rather than at
					// the definitions they bind.
					SecondarySourceLocation secondaryLocation;
					for (FunctionDefinition const* definition: matchingDefinitions)
						if (functionDefinition != *definition)
						secondaryLocation.append("Conflicting definition:", definition->location());

					m_errorReporter.typeError(
						4705_error,
						path->location(),
						secondaryLocation,
						fmt::format(
							"User-defined {} operator {} has more than one definition matching the operand type visible in the current scope.",
							parameterCount == 1 ? "unary" : "binary",
							TokenTraits::friendlyName(operator_.value())
						)
					);
				}
			}
		}
	}
}

void TypeChecker::checkErrorAndEventParameters(CallableDeclaration const& _callable)
{
	std::string kind = dynamic_cast<EventDefinition const*>(&_callable) ? "event" : "error";
	for (ASTPointer<VariableDeclaration> const& var: _callable.parameters())
	{
		if (!type(*var)->interfaceType(false))
			m_errorReporter.typeError(3417_error, var->location(), "Internal or recursive type is not allowed as " + kind + " parameter type.");
		if (
			!useABICoderV2() &&
			!typeSupportedByOldABIEncoder(*type(*var), false /* isLibrary */)
		)
			m_errorReporter.typeError(
				3061_error,
				var->location(),
				"This type is only supported in ABI coder v2. "
				"Use \"pragma abicoder v2;\" to enable the feature."
			);
	}
}

Declaration const& TypeChecker::dereference(Identifier const& _identifier) const
{
	solAssert(!!_identifier.annotation().referencedDeclaration, "Declaration not stored.");
	return *_identifier.annotation().referencedDeclaration;
}

Declaration const& TypeChecker::dereference(IdentifierPath const& _path) const
{
	solAssert(!!_path.annotation().referencedDeclaration, "Declaration not stored.");
	return *_path.annotation().referencedDeclaration;
}

bool TypeChecker::expectType(Expression const& _expression, Type const& _expectedType, bool acceptExpression)
{
	if (acceptExpression)
		_expression.accept(*this);
	BoolResult result = type(_expression)->isImplicitlyConvertibleTo(_expectedType);
	if (!result)
	{
		auto errorMsg = "Type " +
			type(_expression)->humanReadableName() +
			" is not implicitly convertible to expected type " +
			_expectedType.humanReadableName();
		if (
			type(_expression)->category() == Type::Category::RationalNumber &&
			dynamic_cast<RationalNumberType const*>(type(_expression))->isFractional() &&
			type(_expression)->mobileType()
		)
		{
			if (_expectedType.operator==(*type(_expression)->mobileType()))
				m_errorReporter.typeError(
					4426_error,
					_expression.location(),
					errorMsg + ", but it can be explicitly converted."
				);
			else
				m_errorReporter.typeErrorConcatenateDescriptions(
					2326_error,
					_expression.location(),
					errorMsg +
					". Try converting to type " +
					type(_expression)->mobileType()->humanReadableName() +
					" or use an explicit conversion.",
					result.message()
				);
		}
		else
			m_errorReporter.typeErrorConcatenateDescriptions(
				7407_error,
				_expression.location(),
				errorMsg + ".",
				result.message()
			);
		return false;
	}
	return true;
}

void TypeChecker::requireLValue(Expression const& _expression, bool _ordinaryAssignment)
{
	_expression.annotation().willBeWrittenTo = true;
	_expression.annotation().lValueOfOrdinaryAssignment = _ordinaryAssignment;
	_expression.accept(*this);

	if (*_expression.annotation().isLValue)
		return;

	auto [errorId, description] = [&]() -> std::tuple<ErrorId, std::string> {
		if (*_expression.annotation().isConstant)
			return { 6520_error, "Cannot assign to a constant variable." };

		if (auto indexAccess = dynamic_cast<IndexAccess const*>(&_expression))
		{
			if (type(indexAccess->baseExpression())->category() == Type::Category::FixedBytes)
				return { 4360_error, "Single bytes in fixed bytes arrays cannot be modified." };
		}

		if (auto memberAccess = dynamic_cast<MemberAccess const*>(&_expression))
		{
			if (dynamic_cast<StructType const*>(type(memberAccess->expression())))
			{
			}
			else if (dynamic_cast<ArrayType const*>(type(memberAccess->expression())))
				if (memberAccess->memberName() == "length")
					return { 7567_error, "Member \"length\" is read-only and cannot be used to resize arrays." };
		}

		return { 4247_error, "Expression has to be an lvalue." };
	}();

	m_errorReporter.typeError(errorId, _expression.location(), description);
}

bool TypeChecker::useABICoderV2() const
{
	solAssert(m_currentSourceUnit, "");
	if (m_currentContract)
		solAssert(m_currentSourceUnit == &m_currentContract->sourceUnit(), "");
	return *m_currentSourceUnit->annotation().useABICoderV2;

}
