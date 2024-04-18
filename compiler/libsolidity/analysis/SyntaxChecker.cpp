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

#include <libsolidity/analysis/SyntaxChecker.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ExperimentalFeatures.h>
#include <libsolidity/interface/Version.h>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/SemVerHandler.h>

#include <libsolutil/UTF8.h>

#include <string>

using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;
using namespace solidity::util;

bool SyntaxChecker::checkSyntax(ASTNode const& _astRoot)
{
	_astRoot.accept(*this);
	return !Error::containsErrors(m_errorReporter.errors());
}

bool SyntaxChecker::visit(SourceUnit const& _sourceUnit)
{
	m_versionPragma.reset();
	m_sourceUnit = &_sourceUnit;
	return true;
}

void SyntaxChecker::endVisit(SourceUnit const& _sourceUnit)
{
	if (!m_versionPragma.has_value())
	{
		std::string errorString("Source file does not specify required compiler version!");
		SemVerVersion recommendedVersion{std::string(VersionString)};
		if (!recommendedVersion.isPrerelease())
			errorString +=
				" Consider adding \"pragma tvm-solidity ^" +
				std::to_string(recommendedVersion.major()) +
				std::string(".") +
				std::to_string(recommendedVersion.minor()) +
				std::string(".") +
				std::to_string(recommendedVersion.patch()) +
				std::string(";\"");

		// when reporting the warning, print the source name only
		m_errorReporter.warning(3420_error, {-1, -1, _sourceUnit.location().sourceName}, errorString);
	}
	if (!m_sourceUnit->annotation().useABICoderV2.set())
		m_sourceUnit->annotation().useABICoderV2 = true;
	m_sourceUnit = nullptr;
}

bool SyntaxChecker::visit(PragmaDirective const& _pragma)
{
	solAssert(!_pragma.tokens().empty(), "");
	solAssert(_pragma.tokens().size() == _pragma.literals().size(), "");
	if (_pragma.tokens()[0] != Token::Identifier &&
		!(
			(_pragma.tokens().size() >= 3 && _pragma.tokens()[0] == Token::SubSmallTon && _pragma.tokens()[1] == Token::Sub && _pragma.tokens()[2] == Token::Identifier) ||
			(_pragma.tokens().size() >= 3 && _pragma.tokens()[0] == Token::SubSmallEver && _pragma.tokens()[1] == Token::Sub && _pragma.tokens()[2] == Token::Identifier)
		)
	)
		m_errorReporter.syntaxError(5226_error, _pragma.location(), "Invalid pragma \"" + _pragma.literals()[0] + "\"");
	else if (_pragma.literals()[0] == "experimental")
	{
		solAssert(m_sourceUnit, "");
		std::vector<std::string> literals(_pragma.literals().begin() + 1, _pragma.literals().end());
		m_errorReporter.warning(9089_error, _pragma.location(), "\"pragma experimental\" is not supported. Delete this.");
		if (literals.empty())
			m_errorReporter.syntaxError(
				9679_error,
				_pragma.location(),
				"Experimental feature name is missing."
			);
		else if (literals.size() > 1)
			m_errorReporter.syntaxError(
				6022_error,
				_pragma.location(),
				"Stray arguments."
			);
		else
		{
			std::string const literal = literals[0];
			if (literal.empty())
				m_errorReporter.syntaxError(3250_error, _pragma.location(), "Empty experimental feature name is invalid.");
			else if (!ExperimentalFeatureNames.count(literal))
				m_errorReporter.syntaxError(8491_error, _pragma.location(), "Unsupported experimental feature name.");
			else if (m_sourceUnit->annotation().experimentalFeatures.count(ExperimentalFeatureNames.at(literal)))
				m_errorReporter.syntaxError(1231_error, _pragma.location(), "Duplicate experimental feature name.");
			else
			{
				auto feature = ExperimentalFeatureNames.at(literal);
				m_sourceUnit->annotation().experimentalFeatures.insert(feature);
			}
		}
	}
	else if (_pragma.literals()[0] == "abicoder")
	{
		solAssert(m_sourceUnit, "");
		if (
			_pragma.literals().size() != 2 ||
			!std::set<std::string>{"v1", "v2"}.count(_pragma.literals()[1])
		)
			m_errorReporter.syntaxError(
				2745_error,
				_pragma.location(),
				"Expected either \"pragma abicoder v1\" or \"pragma abicoder v2\"."
			);
		else if (m_sourceUnit->annotation().useABICoderV2.set())
			m_errorReporter.syntaxError(
				3845_error,
				_pragma.location(),
				"ABI coder has already been selected for this source unit."
			);
		else
			m_sourceUnit->annotation().useABICoderV2 = (_pragma.literals()[1] == "v2");
	}
	else if (_pragma.literals()[0] == "solidity")
	{
		SemVerVersion recommendedVersion{std::string(VersionString)};
		std::string errorString =
				"It's deprecated."
				" Consider adding \"pragma tvm-solidity ^" +
				std::to_string(recommendedVersion.major()) +
				std::string(".") +
				std::to_string(recommendedVersion.minor()) +
				std::string(".") +
				std::to_string(recommendedVersion.patch()) +
				std::string(";\"") +
				" to set a version of the compiler.";
		m_errorReporter.warning(6413_error, _pragma.location(), errorString);
	}
	else if (_pragma.literals()[0] == "ever" || _pragma.literals()[0] == "ton" || _pragma.literals()[0] == "tvm")
	{
		if (m_versionPragma.has_value()) {
			m_errorReporter.fatalTypeError(
				8884_error,
				_pragma.location(),
				SecondarySourceLocation().append("Previous definition:", *m_versionPragma.value()),
				"Compiler version is defined more than once.");
		}
		try
		{
			std::vector<Token> tokens(_pragma.tokens().begin() + 3, _pragma.tokens().end());
			std::vector<std::string> literals(_pragma.literals().begin() + 3, _pragma.literals().end());
			SemVerMatchExpressionParser parser(tokens, literals);
			SemVerMatchExpression matchExpression = parser.parse();
			static SemVerVersion const currentVersion{std::string(VersionString)};
			solAssert(matchExpression.matches(currentVersion));
			m_versionPragma = &_pragma.location();
		}
		catch (SemVerError const&)
		{
			// An unparsable version pragma is an unrecoverable fatal error in the parser.
			solAssert(false);
		}
	}
	else if (_pragma.literals()[0] == "AbiHeader")
	{
		const std::string base = "Correct format: pragma AbiHeader [pubkey|expire|notime]";
		if (_pragma.literals().size() != 2) {
			auto err = "Empty pragma. " + base;
			m_errorReporter.syntaxError(1112_error, _pragma.location(), err);
		} else {
			auto literal = _pragma.literals()[1];
			if (literal != "pubkey" && literal != "expire" && literal != "notime") {
				auto err = "Unknown pragma \""+ literal + "\". " + base;
				if (literal == "time") {
					err += "\nNote: timestamp in header of external message "
							"is on by default, so delete this pragma.";
				}
				m_errorReporter.syntaxError(2632_error, _pragma.location(), err);
			}
		}
	}
	else if (_pragma.literals()[0] == "upgrade")
    {
        if (_pragma.literals().size() != 2 || (_pragma.literals()[1] != "func" && _pragma.literals()[1] != "oldsol")) {
            m_errorReporter.syntaxError(3323_error, _pragma.location(), R"(Unknown pragma. Use: "pragma upgrade func;" or "pragma upgrade oldsol;")");
        }
    }
	else if (_pragma.literals()[0] == "ignoreIntOverflow")
	{
		return true;
	}
	else if (_pragma.literals()[0] == "msgValue")
	{
		if (m_msgValuePragmaFound) {
			m_errorReporter.syntaxError(2995_error, _pragma.location(), "msgValue pragma shouldn't be specified more than once.");
		}
		if (_pragma.parameter().empty()) {
			m_errorReporter.syntaxError(6562_error, _pragma.location(), "Correct format: pragma msgValue <value_in_nanoevers>");
		}
		m_msgValuePragmaFound = true;
	}
	else if (_pragma.literals()[0] == "copyleft")
	{
		if (m_FirstCopyleft) {
			m_errorReporter.declarationError(
				1595_error,
				_pragma.location(),
				SecondarySourceLocation().append("The previous declaration is here:", *m_FirstCopyleft),
				"Pragma already defined."
			);
		}
		m_FirstCopyleft = &_pragma.location();
	}
	else
		m_errorReporter.syntaxError(4936_error, _pragma.location(), "Unknown pragma \"" + _pragma.literals()[0] + "\"");

	return true;
}

bool SyntaxChecker::visit(ModifierDefinition const&)
{
	m_placeholderFound = false;
	return true;
}

void SyntaxChecker::endVisit(ModifierDefinition const& _modifier)
{
	if (_modifier.isImplemented() && !m_placeholderFound)
		m_errorReporter.syntaxError(2883_error, _modifier.body().location(), "Modifier body does not contain '_'.");
	m_placeholderFound = false;
}

void SyntaxChecker::checkSingleStatementVariableDeclaration(ASTNode const& _statement)
{
	auto varDecl = dynamic_cast<VariableDeclarationStatement const*>(&_statement);
	if (varDecl)
		m_errorReporter.syntaxError(9079_error, _statement.location(), "Variable declarations can only be used inside blocks.");
}

bool SyntaxChecker::visit(IfStatement const& _ifStatement)
{
	checkSingleStatementVariableDeclaration(_ifStatement.trueStatement());
	if (Statement const* _statement = _ifStatement.falseStatement())
		checkSingleStatementVariableDeclaration(*_statement);
	return true;
}

bool SyntaxChecker::visit(WhileStatement const& _whileStatement)
{
	m_inLoopDepth++;
	checkSingleStatementVariableDeclaration(_whileStatement.body());
	return true;
}

void SyntaxChecker::endVisit(WhileStatement const&)
{
	m_inLoopDepth--;
}

bool SyntaxChecker::visit(ForStatement const& _forStatement)
{
	m_inLoopDepth++;
	checkSingleStatementVariableDeclaration(_forStatement.body());
	return true;
}

void SyntaxChecker::endVisit(ForStatement const&)
{
	m_inLoopDepth--;
}

bool SyntaxChecker::visit(ForEachStatement const& _forStatement)
{
	m_inLoopDepth++;
	checkSingleStatementVariableDeclaration(_forStatement.body());
	return true;
}

void SyntaxChecker::endVisit(ForEachStatement const&)
{
	m_inLoopDepth--;
}

bool SyntaxChecker::visit(Block const& _block)
{
	if (_block.unchecked())
	{
		if (m_uncheckedArithmetic)
			m_errorReporter.syntaxError(
				1941_error,
				_block.location(),
				"\"unchecked\" blocks cannot be nested."
			);

		m_uncheckedArithmetic = true;
	}
	return true;
}

void SyntaxChecker::endVisit(Block const& _block)
{
	if (_block.unchecked())
		m_uncheckedArithmetic = false;
}

bool SyntaxChecker::visit(Continue const& _continueStatement)
{
	if (m_inLoopDepth <= 0)
		// we're not in a for/while loop, report syntax error
		m_errorReporter.syntaxError(4123_error, _continueStatement.location(), "\"continue\" has to be in a \"for\" or \"while\" loop.");
	return true;
}

bool SyntaxChecker::visit(Break const& _breakStatement)
{
	if (m_inLoopDepth <= 0)
		// we're not in a for/while loop, report syntax error
		m_errorReporter.syntaxError(6102_error, _breakStatement.location(), "\"break\" has to be in a \"for\" or \"while\" loop.");
	return true;
}

bool SyntaxChecker::visit(Throw const& _throwStatement)
{
	m_errorReporter.syntaxError(
		4538_error,
		_throwStatement.location(),
		"\"throw\" is deprecated in favour of \"revert()\", \"require()\" and \"assert()\"."
	);

	return true;
}

bool SyntaxChecker::visit(Literal const& _literal)
{
	size_t invalidSequence;
	if ((_literal.token() == Token::UnicodeStringLiteral) && !validateUTF8(_literal.value(), invalidSequence))
		m_errorReporter.syntaxError(
			8452_error,
			_literal.location(),
			"Contains invalid UTF-8 sequence at position " + toString(invalidSequence) + "."
		);

	if (_literal.token() != Token::Number)
		return true;

	ASTString const& value = _literal.value();
	solAssert(!value.empty(), "");

	// Generic checks no matter what base this number literal is of:
	if (value.back() == '_')
	{
		m_errorReporter.syntaxError(2090_error, _literal.location(), "Invalid use of underscores in number literal. No trailing underscores allowed.");
		return true;
	}

	if (value.find("__") != ASTString::npos)
	{
		m_errorReporter.syntaxError(2990_error, _literal.location(), "Invalid use of underscores in number literal. Only one consecutive underscores between digits allowed.");
		return true;
	}

	if (!_literal.isHexNumber()) // decimal literal
	{
		if (value.find("._") != ASTString::npos)
			m_errorReporter.syntaxError(3891_error, _literal.location(), "Invalid use of underscores in number literal. No underscores in front of the fraction part allowed.");

		if (value.find("_.") != ASTString::npos)
			m_errorReporter.syntaxError(1023_error, _literal.location(), "Invalid use of underscores in number literal. No underscores in front of the fraction part allowed.");

		if (value.find("_e") != ASTString::npos)
			m_errorReporter.syntaxError(6415_error, _literal.location(), "Invalid use of underscores in number literal. No underscore at the end of the mantissa allowed.");

		if (value.find("e_") != ASTString::npos)
			m_errorReporter.syntaxError(6165_error, _literal.location(), "Invalid use of underscores in number literal. No underscore in front of exponent allowed.");
	}

	return true;
}

bool SyntaxChecker::visit(UnaryOperation const& _operation)
{
	solAssert(_operation.getOperator() != Token::Add);
	return true;
}

bool SyntaxChecker::visit(PlaceholderStatement const& _placeholder)
{
	if (m_uncheckedArithmetic)
		m_errorReporter.syntaxError(
			2573_error,
			_placeholder.location(),
			"The placeholder statement \"_\" cannot be used inside an \"unchecked\" block."
		);

	m_placeholderFound = true;
	return true;
}

bool SyntaxChecker::visit(ContractDefinition const& _contract)
{
	m_currentContractKind = _contract.contractKind();

	ASTString const& contractName = _contract.name();
	for (FunctionDefinition const* function: _contract.definedFunctions())
		if (function->name() == contractName)
			m_errorReporter.syntaxError(
				5796_error,
				function->location(),
				"Functions are not allowed to have the same name as the contract. "
				"If you intend this to be a constructor, use \"constructor(...) { ... }\" to define it."
			);
	return true;
}

void SyntaxChecker::endVisit(ContractDefinition const&)
{
	m_currentContractKind = std::nullopt;
}

bool SyntaxChecker::visit(UsingForDirective const& _usingFor)
{
	if (!_usingFor.usesBraces())
		solAssert(
			_usingFor.functionsAndOperators().size() == 1 &&
			!std::get<1>(_usingFor.functionsAndOperators().front())
		);

	if (!m_currentContractKind && !_usingFor.typeName())
		m_errorReporter.syntaxError(
			8118_error,
			_usingFor.location(),
			"The type has to be specified explicitly at file level (cannot use '*')."
		);
	else if (_usingFor.usesBraces() && !_usingFor.typeName())
		m_errorReporter.syntaxError(
			3349_error,
			_usingFor.location(),
			"The type has to be specified explicitly when attaching specific functions."
		);
	if (_usingFor.global() && !_usingFor.typeName())
		m_errorReporter.syntaxError(
			2854_error,
			_usingFor.location(),
			"Can only globally attach functions to specific types."
		);
	if (_usingFor.global() && m_currentContractKind)
		m_errorReporter.syntaxError(
			3367_error,
			_usingFor.location(),
			"\"global\" can only be used at file level."
		);
	if (m_currentContractKind == ContractKind::Interface)
		m_errorReporter.syntaxError(
			9088_error,
			_usingFor.location(),
			"The \"using for\" directive is not allowed inside interfaces."
		);

	return true;
}

bool SyntaxChecker::visit(FunctionDefinition const& _function)
{
	if (m_sourceUnit && m_sourceUnit->experimentalSolidity())
		// Handled in experimental::SyntaxRestrictor instead.
		return true;

	if (!_function.isFree() && _function.isInlineAssembly())
		m_errorReporter.fatalTypeError(7229_error, _function.location(), "Only free functions can be marked as \"assembly\".");

	if (!_function.isFree() && !_function.isConstructor() && _function.noVisibilitySpecified())
	{
		std::string suggestedVisibility =
			_function.isFallback() ||
			_function.isReceive() ||
			m_currentContractKind == ContractKind::Interface
		? "external" : "public";
		m_errorReporter.syntaxError(
			4937_error,
			_function.location(),
			"No visibility specified. Did you intend to add \"" + suggestedVisibility + "\"?"
		);
	}
	else if (_function.isFree())
	{
		if (!_function.noVisibilitySpecified())
			m_errorReporter.syntaxError(
				4126_error,
				_function.location(),
				"Free functions cannot have visibility."
			);
		if (!_function.isImplemented())
			m_errorReporter.typeError(4668_error, _function.location(), "Free functions must be implemented.");
	}

	if (m_currentContractKind == ContractKind::Interface && !_function.modifiers().empty())
		m_errorReporter.syntaxError(5842_error, _function.location(), "Functions in interfaces cannot have modifiers.");
	else if (!_function.isImplemented() && !_function.modifiers().empty())
		m_errorReporter.syntaxError(2668_error, _function.location(), "Functions without implementation cannot have modifiers.");

	return true;
}

bool SyntaxChecker::visit(FunctionTypeName const& _node)
{
	for (auto const& decl: _node.parameterTypeList()->parameters())
		if (!decl->name().empty())
			m_errorReporter.warning(6162_error, decl->location(), "Naming function type parameters is deprecated.");

	for (auto const& decl: _node.returnParameterTypeList()->parameters())
		if (!decl->name().empty())
			m_errorReporter.syntaxError(7304_error, decl->location(), "Return parameters in function types may not be named.");

	return true;
}

bool SyntaxChecker::visit(StructDefinition const& _struct)
{
	if (_struct.members().empty())
		m_errorReporter.syntaxError(5306_error, _struct.location(), "Defining empty structs is disallowed.");

	return true;
}

bool SyntaxChecker::visitNode(ASTNode const& _node)
{
	if (_node.experimentalSolidityOnly())
	{
		solAssert(m_sourceUnit);
		solAssert(m_sourceUnit->experimentalSolidity());
	}
	return ASTConstVisitor::visitNode(_node);
}
