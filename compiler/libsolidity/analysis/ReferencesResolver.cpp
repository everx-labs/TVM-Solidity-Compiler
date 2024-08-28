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
 * Component that resolves type names to types and annotates the AST accordingly.
 */

#include <libsolidity/analysis/ReferencesResolver.h>
#include <libsolidity/analysis/NameAndTypeResolver.h>
#include <libsolidity/ast/AST.h>

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/Exceptions.h>

#include <libsolutil/StringUtils.h>
#include <libsolutil/CommonData.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;


bool ReferencesResolver::resolve(ASTNode const& _root)
{
	auto errorWatcher = m_errorReporter.errorWatcher();
	_root.accept(*this);
	return errorWatcher.ok();
}

bool ReferencesResolver::visit(Block const& _block)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_block);
	return true;
}

void ReferencesResolver::endVisit(Block const& _block)
{
	if (!m_resolveInsideCode)
		return;

	m_resolver.setScope(_block.scope());
}

bool ReferencesResolver::visit(TryCatchClause const& _tryCatchClause)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_tryCatchClause);
	return true;
}

void ReferencesResolver::endVisit(TryCatchClause const& _tryCatchClause)
{
	if (!m_resolveInsideCode)
		return;

	m_resolver.setScope(_tryCatchClause.scope());
}

bool ReferencesResolver::visit(ForStatement const& _for)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_for);
	return true;
}

bool ReferencesResolver::visit(ForEachStatement const& _for)
{
	if (!m_resolveInsideCode)
		return false;
	m_resolver.setScope(&_for);
	return true;
}

void ReferencesResolver::endVisit(ForStatement const& _for)
{
	if (!m_resolveInsideCode)
		return;
	m_resolver.setScope(_for.scope());
}

void ReferencesResolver::endVisit(ForEachStatement const& _for)
{
	if (!m_resolveInsideCode)
		return;
	m_resolver.setScope(_for.scope());
}

void ReferencesResolver::endVisit(VariableDeclarationStatement const& _varDeclStatement)
{
	if (!m_resolveInsideCode)
		return;
	for (auto const& var: _varDeclStatement.declarations())
		if (var)
			m_resolver.activateVariable(var->name());
}

bool ReferencesResolver::visit(VariableDeclaration const& _varDecl)
{
	if (_varDecl.documentation())
		resolveInheritDoc(*_varDecl.documentation(), _varDecl.annotation());

	if (m_resolver.experimentalSolidity())
	{
		solAssert(!_varDecl.hasTypeName());
		if (_varDecl.typeExpression())
		{
			ScopedSaveAndRestore typeContext{m_typeContext, true};
			_varDecl.typeExpression()->accept(*this);
		}
		if (_varDecl.overrides())
			_varDecl.overrides()->accept(*this);
		if (_varDecl.value())
			_varDecl.value()->accept(*this);
		return false;
	}

	return true;
}

bool ReferencesResolver::visit(Identifier const& _identifier)
{
	auto declarations = m_resolver.nameFromCurrentScope(_identifier.name());
	if (declarations.empty())
	{
		if (m_resolver.experimentalSolidity() && m_typeContext)
			return false;
		std::string suggestions = m_resolver.similarNameSuggestions(_identifier.name());
		std::string errorMessage = "Undeclared identifier.";
		if (!suggestions.empty())
		{
			if ("\"" + _identifier.name() + "\"" == suggestions)
				errorMessage += " " + std::move(suggestions) + " is not (or not yet) visible at this point.";
			else
				errorMessage += " Did you mean " + std::move(suggestions) + "?";
		}
		m_errorReporter.declarationError(7576_error, _identifier.location(), errorMessage);
	}
	else if (declarations.size() == 1)
		_identifier.annotation().referencedDeclaration = declarations.front();
	else
		_identifier.annotation().candidateDeclarations = declarations;
	return false;
}

bool ReferencesResolver::visit(FunctionDefinition const& _functionDefinition)
{
	m_functionDefinitions.push_back(&_functionDefinition);

	if (_functionDefinition.documentation())
		resolveInheritDoc(*_functionDefinition.documentation(), _functionDefinition.annotation());

	return true;
}

void ReferencesResolver::endVisit(FunctionDefinition const&)
{
	solAssert(!m_functionDefinitions.empty(), "");
	m_functionDefinitions.pop_back();
}

bool ReferencesResolver::visit(ModifierDefinition const& _modifierDefinition)
{
	m_functionDefinitions.push_back(nullptr);

	if (_modifierDefinition.documentation())
		resolveInheritDoc(*_modifierDefinition.documentation(), _modifierDefinition.annotation());

	return true;
}

void ReferencesResolver::endVisit(ModifierDefinition const&)
{
	solAssert(!m_functionDefinitions.empty(), "");
	m_functionDefinitions.pop_back();
}

void ReferencesResolver::endVisit(IdentifierPath const& _path)
{
	// Note that library/functions names in "using {} for" directive are resolved separately in visit(UsingForDirective)
	std::vector<Declaration const*> declarations = m_resolver.pathFromCurrentScopeWithAllDeclarations(_path.path());
	if (declarations.empty())
	{
		m_errorReporter.fatalDeclarationError(7920_error, _path.location(), "Identifier not found or not unique.");
		return;
	}

	_path.annotation().referencedDeclaration = declarations.back();
	_path.annotation().pathDeclarations = std::move(declarations);
}

bool ReferencesResolver::visit(UsingForDirective const& _usingFor)
{
	for (ASTPointer<IdentifierPath> const& path: _usingFor.functionsOrLibrary())
	{
		// _includeInvisibles is enabled here because external library functions are marked invisible.
		// As unintended side-effects other invisible names (eg.: super, this) may be returned as well.
		// DeclarationTypeChecker should detect and report such situations.
		std::vector<Declaration const*> declarations = m_resolver.pathFromCurrentScopeWithAllDeclarations(path->path(), true /* _includeInvisibles */);
		if (declarations.empty())
		{
			std::string libraryOrFunctionNameErrorMessage =
				_usingFor.usesBraces() ?
				"Identifier is not a function name or not unique." :
				"Identifier is not a library name.";
			m_errorReporter.fatalDeclarationError(
				9589_error,
				path->location(),
				libraryOrFunctionNameErrorMessage
			);
			break;
		}

		path->annotation().referencedDeclaration = declarations.back();
		path->annotation().pathDeclarations = std::move(declarations);
	}

	if (_usingFor.typeName())
		_usingFor.typeName()->accept(*this);

	return false;
}

bool ReferencesResolver::visit(Return const& _return)
{
	solAssert(!m_functionDefinitions.empty(), "");
	_return.annotation().function = m_functionDefinitions.back();
	_return.annotation().functionReturnParameters = m_functionDefinitions.back() ? m_functionDefinitions.back()->returnParameterList().get() : nullptr;
	return true;
}

bool ReferencesResolver::visit(BinaryOperation const& _binaryOperation)
{
	if (m_resolver.experimentalSolidity())
	{
		_binaryOperation.leftExpression().accept(*this);
		if (_binaryOperation.getOperator() == Token::Colon)
		{
			ScopedSaveAndRestore typeContext(m_typeContext, !m_typeContext);
			_binaryOperation.rightExpression().accept(*this);
		}
		else
			_binaryOperation.rightExpression().accept(*this);
		return false;
	}
	else
		return ASTConstVisitor::visit(_binaryOperation);
}

void ReferencesResolver::resolveInheritDoc(StructuredDocumentation const& _documentation, StructurallyDocumentedAnnotation& _annotation)
{
	switch (_annotation.docTags.count("inheritdoc"))
	{
	case 0:
		break;
	case 1:
	{
		std::string const& name = _annotation.docTags.find("inheritdoc")->second.content;
		if (name.empty())
		{
			m_errorReporter.docstringParsingError(
				1933_error,
				_documentation.location(),
				"Expected contract name following documentation tag @inheritdoc."
			);
			return;
		}

		std::vector<std::string> path;
		boost::split(path, name, boost::is_any_of("."));
		if (any_of(path.begin(), path.end(), [](auto& _str) { return _str.empty(); }))
		{
			m_errorReporter.docstringParsingError(
				5967_error,
				_documentation.location(),
				"Documentation tag @inheritdoc reference \"" +
				name +
				"\" is malformed."
			);
			return;
		}
		Declaration const* result = m_resolver.pathFromCurrentScope(path);

		if (result == nullptr)
		{
			m_errorReporter.docstringParsingError(
				9397_error,
				_documentation.location(),
				"Documentation tag @inheritdoc references inexistent contract \"" +
				name +
				"\"."
			);
			return;
		}
		else
		{
			_annotation.inheritdocReference = dynamic_cast<ContractDefinition const*>(result);

			if (!_annotation.inheritdocReference)
				m_errorReporter.docstringParsingError(
					1430_error,
					_documentation.location(),
					"Documentation tag @inheritdoc reference \"" +
					name +
					"\" is not a contract."
				);
		}
		break;
	}
	default:
		m_errorReporter.docstringParsingError(
			5142_error,
			_documentation.location(),
			"Documentation tag @inheritdoc can only be given once."
		);
		break;
	}
}
