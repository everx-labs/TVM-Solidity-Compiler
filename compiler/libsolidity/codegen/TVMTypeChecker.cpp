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


#include <boost/range/adaptor/reversed.hpp>

#include <libsolidity/ast/ASTForward.h>

#include "TVM.hpp"
#include "TVMCommons.hpp"
#include "TVMPusher.hpp"
#include "TVMConstants.hpp"
#include "TVMTypeChecker.hpp"

using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace std;

namespace {
	string isNotSupportedVM = " is not supported by the VM version. See \"--tvm-version\" command-line option.";
}

TVMTypeChecker::TVMTypeChecker(langutil::ErrorReporter& _errorReporter) :
	m_errorReporter{_errorReporter}
{

}

void TVMTypeChecker::checkOverrideAndOverload() {
	std::set<CallableDeclaration const*> overridedFunctions;
	std::set<CallableDeclaration const*> functions;
	std::map<uint32_t, CallableDeclaration const*> funcId2Decl;
	for (ContractDefinition const* cd : contractDefinition->annotation().linearizedBaseContracts | boost::adaptors::reversed) {
		for (FunctionDefinition const *f : cd->definedFunctions()) {

			if (f->functionID().has_value()) {
				uint32_t id = f->functionID().value();
				if (funcId2Decl.count(id) != 0) {
					CallableDeclaration const *f2 = funcId2Decl.at(id);
					std::set<CallableDeclaration const*> bf = getAllBaseFunctions(f);
					std::set<CallableDeclaration const*> bf2 = getAllBaseFunctions(f2);
					if (bf.count(f2) == 0 && bf2.count(f) == 0) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the function with the same function ID: ", funcId2Decl.at(id)->location()),
							"Two functions have the same functionID.");
					}
				} else {
					funcId2Decl[id] = f;
				}
			}

			if (f->isConstructor() || f->isReceive() || f->isFallback() || f->isOnTickTock()) {
				continue;
			}

			FunctionDefinitionAnnotation &annotation = f->annotation();
			if (!annotation.baseFunctions.empty()) {
				overridedFunctions.insert(f);
				for (CallableDeclaration const *base : annotation.baseFunctions) {
					auto baseFunction = to<FunctionDefinition>(base);
					overridedFunctions.insert(base);
					if ((!f->functionID().has_value() && baseFunction->functionID()) || (f->functionID().has_value() && !baseFunction->functionID())) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the base function: ", baseFunction->location()),
							"Both override and base functions should have functionID if it is defined for one of them.");
					} else if (f->functionID().has_value() && f->functionID() != baseFunction->functionID()) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the base function: ", baseFunction->location()),
							"Override function should have functionID = " + toString(baseFunction->functionID().value()) + ".");
					}

					if (baseFunction->isResponsible() != f->isResponsible()) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the base function: ", baseFunction->location()),
							"Both override and base functions should be marked as responsible or not");
					}

					if ((!f->functionID().has_value() && baseFunction->functionID()) || (f->functionID().has_value() && !baseFunction->functionID())) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the base function: ",
															 baseFunction->location()),
							"Both override and base functions should have functionID if it is defined for one of them.");
					}

					if ((f->isInternalMsg() ^ baseFunction->isInternalMsg()) || (f->isExternalMsg() ^ baseFunction->isExternalMsg())) {
						m_errorReporter.typeError(
							228_error,
							f->location(),
							SecondarySourceLocation().append("Declaration of the base function: ",
															 baseFunction->location()),
							"Both override and base functions should be marked as internalMsg or externalMsg.");
					}
				}
			}
			functions.insert(f);
		}
	}


	std::set<std::pair<CallableDeclaration const*, CallableDeclaration const*>> used{};
	for (CallableDeclaration const* f : functions) {
		if (!f->isPublic()) {
			continue;
		}
		if (overridedFunctions.count(f)) {
			continue;
		}
		for (CallableDeclaration const* ff : functions) {
			if (!ff->isPublic()) {
				continue;
			}
			if (overridedFunctions.count(ff) || f == ff) {
				continue;
			}
			if (f->name() == ff->name()) {
				if (used.count(std::make_pair(f, ff)) == 0) {
					m_errorReporter.typeError(
						228_error,
						f->location(),
						SecondarySourceLocation().append("Another overloaded function is here:", ff->location()),
						"Function overloading is not supported for public functions.");
					used.insert({f, ff});
					used.insert({ff, f});
				}
			}
		}
	}
}

void TVMTypeChecker::check_onCodeUpgrade(FunctionDefinition const& f) {
	const std::string s = "\nfunction onCodeUpgrade(...) (internal|private) { /*...*/ }";
	if (!f.returnParameters().empty()) {
		m_errorReporter.typeError(228_error, f.returnParameters().at(0)->location(), "Function mustn't return any parameters. Expected function signature:" + s);
	}
	if (f.isPublic()) {
		m_errorReporter.typeError(228_error, f.location(), "Bad function visibility. Expected function signature:" + s);
	}
}

bool TVMTypeChecker::visit(TryStatement const& _tryStatement) {
	if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
		m_errorReporter.typeError(228_error, _tryStatement.location(),
								  "\"try-catch\"" + isNotSupportedVM);
	}
	return true;
}

bool TVMTypeChecker::visit(VariableDeclaration const& _node) {
	if (_node.isStateVariable() && _node.type()->category() == Type::Category::TvmSlice) {
		m_errorReporter.typeError(228_error, _node.location(), "This type can't be used for state variables.");
	}
	return true;
}

bool TVMTypeChecker::visit(const Mapping &_mapping) {
	if (auto keyType = to<UserDefinedTypeName>(&_mapping.keyType())) {
		if (keyType->annotation().type->category() == Type::Category::Struct) {
			auto structType = to<StructType>(_mapping.keyType().annotation().type);
			int bitLength = 0;
			StructDefinition const& structDefinition = structType->structDefinition();
			for (const auto& member : structDefinition.members()) {
				TypeInfo ti {member->type()};
				if (!ti.isNumeric) {
					m_errorReporter.typeError(
						228_error,
						_mapping.keyType().location(),
						SecondarySourceLocation().append("Bad field: ", member->location()),
						"If struct type is used as a key type for mapping, then "
						"fields of the struct must have integer, boolean, fixed bytes or enum type"
					);
				}
				bitLength += ti.numBits;
			}
			if (bitLength > TvmConst::CellBitLength) {
				m_errorReporter.typeError(228_error, _mapping.keyType().location(), "If struct type is used as a key type for mapping, then "
											   "struct must fit in " + toString(TvmConst::CellBitLength) + " bits");
			}
		}
	}
	return true;
}

bool TVMTypeChecker::visit(const FunctionDefinition &f) {
	if (f.functionID().has_value()) {
		if (f.functionID().value() == 0) {
			m_errorReporter.typeError(228_error, f.location(), "functionID can't be equal to zero because this value is reserved for receive function.");
		}
		if (!f.functionIsExternallyVisible() && f.name() != "onCodeUpgrade") {
			m_errorReporter.typeError(228_error, f.location(), "Only public/external functions and function `onCodeUpgrade` can have functionID.");
		}
		if (f.isReceive() || f.isFallback() || f.isOnTickTock() || f.isOnBounce()) {
			m_errorReporter.typeError(228_error, f.location(), "functionID isn't supported for receive, fallback, onBounce and onTickTock functions.");
		}
	}

	if (f.isInline() && f.isPublic()) {
		m_errorReporter.typeError(228_error, f.location(), "Inline function should have private or internal visibility");
	}
	if (f.name() == "onCodeUpgrade") {
		check_onCodeUpgrade(f);
	}

	if (f.name() == "afterSignatureCheck") {
		const std::string s = "\nExpected follow format: \"function afterSignatureCheck(TvmSlice restOfMessageBody, TvmCell message) private inline returns (TvmSlice) { /*...*/ }\"";
		if (
			f.parameters().size() != 2 ||
			f.parameters().at(0)->type()->category() != Type::Category::TvmSlice ||
			f.parameters().at(1)->type()->category() != Type::Category::TvmCell
		) {
			m_errorReporter.typeError(228_error, f.location(),
									  "Unexpected function parameters." + s);
		}
		if (f.returnParameters().size() != 1 ||
			f.returnParameters().at(0)->type()->category() != Type::Category::TvmSlice) {
			m_errorReporter.typeError(228_error, f.location(), "Should return TvmSlice." + s);
		}
		if (f.visibility() != Visibility::Private) {
			m_errorReporter.typeError(228_error, f.location(), "Should be marked as private." + s);
		}
		if (!f.isInline()) {
			m_errorReporter.typeError(228_error, f.location(), "Should be marked as inline." + s);
		}
	}

	if (!f.isFree() && f.isInlineAssembly()) {
		m_errorReporter.typeError(228_error, f.location(), "Only free functions can be marked as \"assembly\".");
	}

	return true;
}

bool TVMTypeChecker::visit(IndexRangeAccess const& indexRangeAccess) {
	Type const *baseType = indexRangeAccess.baseExpression().annotation().type;
	auto baseArrayType = to<ArrayType>(baseType);
	if (baseType->category() != Type::Category::Array || !baseArrayType->isByteArrayOrString()) {
		m_errorReporter.typeError(228_error, indexRangeAccess.location(), "Index range access is available only for bytes.");
	}
	return true;
}

void TVMTypeChecker::checkDeprecation(FunctionCall const& _functionCall) {
	auto memberAccess = to<MemberAccess>(&_functionCall.expression());
	ASTString const& memberName = memberAccess ? memberAccess->memberName() : "";
	Type const* expressionType = _functionCall.expression().annotation().type;
	switch (expressionType->category()) {
	case Type::Category::Function: {
		auto functionType = to<FunctionType>(expressionType);
		switch (functionType->kind()) {
		case FunctionType::Kind::OptionalReset:
			m_errorReporter.warning(228_error, _functionCall.location(),
									"\"<optional(T)>.reset()\" is deprecated. Use \"delete <optional(T)>;\".");
			break;
		case FunctionType::Kind::TVMSliceLoad:
			if (memberName == "decode") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.decode()\" is deprecated. Use \"<TvmSlice>.load()\"");
			}
			break;
		case FunctionType::Kind::TVMSliceLoadQ:
			if (memberName == "decodeQ") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.decodeQ()\" is deprecated. Use \"<TvmSlice>.loadQ()\"");
			}
			break;
		case FunctionType::Kind::TVMSliceLoadFunctionParams:
			if (memberName == "decodeFunctionParams") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.decodeFunctionParams()\" is deprecated. Use \"<TvmSlice>.loadFunctionParams()\"");
			}
			break;
		case FunctionType::Kind::TVMSliceLoadStateVars:
			if (memberName == "decodeStateVars") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.decodeStateVars()\" is deprecated. Use \"<TvmSlice>.loadStateVars()\"");
			}
			break;
		case FunctionType::Kind::TVMSliceLoadUint:
			if (memberName == "loadUnsigned") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.loadUnsigned()\" is deprecated. Use \"<TvmSlice>.loadUint()\"");
			}
			break;
		case FunctionType::Kind::TVMSliceLoadInt:
			if (memberName == "loadSigned") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmSlice>.loadSigned()\" is deprecated. Use \"<TvmSlice>.loadInt()\"");
			}
			break;
		case FunctionType::Kind::TVMBuilderStoreUint:
			if (memberName == "storeUnsigned") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmBuilder>.storeUnsigned()\" is deprecated. Use \"<TvmBuilder>.storeUint()\"");
			}
			break;
		case FunctionType::Kind::TVMBuilderStoreInt:
			if (memberName == "storeSigned") {
				m_errorReporter.warning(228_error, _functionCall.location(),
										"\"<TvmBuilder>.storeSigned()\" is deprecated. Use \"<TvmBuilder>.storeInt()\"");
			}
			break;
		case FunctionType::Kind::ByteToSlice: {
			m_errorReporter.warning(228_error, _functionCall.location(),
									"\"<bytes>.toSlice()\" is deprecated. Use explicit conversion: \"TvmSlice(<bytes>)\"");
			break;
		}
		case FunctionType::Kind::StringToSlice: {
			m_errorReporter.warning(228_error, _functionCall.location(),
									"\"<string>.toSlice()\" is deprecated. Use explicit conversion: \"TvmSlice(<string>)\"");
			break;
		}
		default:
			break;
		}
		break;
	}
	default:
		break;
	}
}

void TVMTypeChecker::checkSupport(FunctionCall const& _functionCall) {
	Type const* expressionType = _functionCall.expression().annotation().type;
	switch (expressionType->category()) {
	case Type::Category::Function: {
		auto functionType = to<FunctionType>(expressionType);
		switch (functionType->kind()) {
		case FunctionType::Kind::GasLeft:
			if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
				m_errorReporter.typeError(228_error, _functionCall.location(),
										  "\"gasleft()\"" + isNotSupportedVM);
			}
			break;
		case FunctionType::Kind::TVMInitCodeHash:
			if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
				m_errorReporter.typeError(228_error, _functionCall.location(),
										  "\"tvm.initCodeHash()\"" + isNotSupportedVM);
			}
			break;
		case FunctionType::Kind::TVMCode:
			if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
				m_errorReporter.typeError(228_error, _functionCall.location(),
										  "\"tvm.code()\"" + isNotSupportedVM);
			}
			break;
		default:
			break;
		}
		break;
	}
	default:
		break;
	}

	if (_functionCall.isAwait() && *GlobalParams::g_tvmVersion == TVMVersion::ton()) {
		m_errorReporter.typeError(228_error, _functionCall.location(),
								  "\"*.await\"" + isNotSupportedVM);
	}
}

bool TVMTypeChecker::visit(FunctionCall const& _functionCall) {
	checkDeprecation(_functionCall);
	checkSupport(_functionCall);

	Type const* expressionType = _functionCall.expression().annotation().type;
	std::vector<ASTPointer<Expression const>> const& arguments = _functionCall.arguments();
	switch (expressionType->category()) {
	case Type::Category::Function: {
		auto functionType = to<FunctionType>(expressionType);

		if (functionType->hasDeclaration()) {
			auto fd = to<FunctionDefinition>(&functionType->declaration());
			if (fd && fd->name() == "onCodeUpgrade") {
				if (m_inherHelper->isBaseFunction(fd)) {
					m_errorReporter.typeError(
						228_error, _functionCall.location(),
						"It is forbidden to call base functions of \"onCodeUpgrade\".");
				}
			}
		}

		switch (functionType->kind()) {
		case FunctionType::Kind::TVMSliceLoadInt:
		case FunctionType::Kind::TVMSliceLoadIntQ:
		case FunctionType::Kind::TVMSliceLoadUint:
		case FunctionType::Kind::TVMSliceLoadUintQ:
		case FunctionType::Kind::TVMSlicePreLoadInt:
		case FunctionType::Kind::TVMSlicePreLoadIntQ:
		case FunctionType::Kind::TVMSlicePreLoadUint:
		case FunctionType::Kind::TVMSlicePreLoadUintQ: {
			const auto& value =  ExprUtils::constValue(*arguments.at(0));
			if (value.has_value()) {
				if (value > 256) {
					m_errorReporter.syntaxError(228_error, arguments.at(0)->location(),
											"Too big value. The value must be in the range 0 - 256.");
				}
			}
			break;
		}
		default:
			break;
		}
		break;
	}
	default:
		break;
	}

	return true;
}

bool TVMTypeChecker::visit(PragmaDirective const& _pragma) {
	if (!_pragma.literals().empty()) {
		if (_pragma.literals().at(0) == "copyleft" && *GlobalParams::g_tvmVersion == TVMVersion::ton()) {
			m_errorReporter.typeError(228_error, _pragma.location(),
									  "\"pragma copyleft ...\"" + isNotSupportedVM);
		}
	}
	return true;
}

bool TVMTypeChecker::visit(MemberAccess const& _memberAccess) {
	ASTString const& member = _memberAccess.memberName();
	Type const* exprType = _memberAccess.expression().annotation().type;
	switch (exprType->category()) {
	case Type::Category::Magic: {
		auto magicType = dynamic_cast<MagicType const*>(exprType);
		switch (magicType->kind()) {
		case MagicType::Kind::Transaction: {
			if (member == "timestamp") {
				m_errorReporter.warning(228_error, _memberAccess.location(),
										R"("tx.timestamp" is deprecated. Use "tx.logicaltime".)");
			}
			if (member == "storageFee") {
				if (*GlobalParams::g_tvmVersion == TVMVersion::ton()) {
					m_errorReporter.typeError(228_error, _memberAccess.location(),
											  "\"tx.storageFee\"" + isNotSupportedVM);
				}
			}
			break;
		}
		case MagicType::Kind::Gosh: {
			if (*GlobalParams::g_tvmVersion != TVMVersion::gosh()) {
				m_errorReporter.typeError(228_error, _memberAccess.location(),
										  "\"gosh." + member + "\"" + isNotSupportedVM);
			}
			break;
		}
		default:
			break;
		}
		break;
	}
	default:
		break;
	}
	return true;
}

bool TVMTypeChecker::visit(ContractDefinition const& cd) {
	contractDefinition = &cd;

	m_inherHelper = std::make_unique<InherHelper>(&cd);

	checkOverrideAndOverload();

	return true;
}

void TVMTypeChecker::endVisit(ContractDefinition const& ) {
	contractDefinition = nullptr;
	m_inherHelper = nullptr;
}
