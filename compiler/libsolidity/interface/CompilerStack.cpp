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
 * @author Gav Wood <g@ethdev.com>
 * @date 2014
 * Full-stack compiler that converts a source code string to bytecode.
 */


#include <libsolidity/interface/CompilerStack.h>

#include <libsolidity/analysis/ControlFlowAnalyzer.h>
#include <libsolidity/analysis/ControlFlowGraph.h>
#include <libsolidity/analysis/ContractLevelChecker.h>
#include <libsolidity/analysis/DocStringAnalyser.h>
#include <libsolidity/analysis/GlobalContext.h>
#include <libsolidity/analysis/NameAndTypeResolver.h>
#include <libsolidity/analysis/PostTypeChecker.h>
#include <libsolidity/analysis/StaticAnalyzer.h>
#include <libsolidity/analysis/SyntaxChecker.h>
#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/analysis/ViewPureChecker.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>
#include <libsolidity/ast/ASTJsonImporter.h>
#include <libsolidity/interface/Natspec.h>
#include <libsolidity/interface/Version.h>
#include <libsolidity/parsing/Parser.h>

#include <liblangutil/Scanner.h>
#include <liblangutil/SemVerHandler.h>

#include <libsolutil/SwarmHash.h>
#include <libsolutil/IpfsHash.h>
#include <libsolutil/JSON.h>
#include <libsolutil/Keccak256.h>

#include <json/json.h>
#include <boost/algorithm/string.hpp>

#include <libsolidity/codegen/TVM.h>
#include <libsolidity/codegen/TVMTypeChecker.hpp>
#include <libsolidity/codegen/TVMAnalyzer.hpp>

using namespace std;
using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;

using solidity::util::errinfo_comment;
using solidity::util::toHex;

using solidity::util::h256;

static int g_compilerStackCounts = 0;


CompilerStack::CompilerStack(ReadCallback::Callback const& _readFile):
	m_readFile{_readFile},
	m_generateIR{false},
	m_generateEwasm{false},
	m_errorList{},
	m_errorReporter{m_errorList}
{
	// Because TypeProvider is currently a singleton API, we must ensure that
	// no more than one entity is actually using it at a time.
	solAssert(g_compilerStackCounts == 0, "You shall not have another CompilerStack aside me.");
	++g_compilerStackCounts;
}

CompilerStack::~CompilerStack()
{
	--g_compilerStackCounts;
	TypeProvider::reset();
}

std::optional<CompilerStack::Remapping> CompilerStack::parseRemapping(string const& _remapping)
{
	auto eq = find(_remapping.begin(), _remapping.end(), '=');
	if (eq == _remapping.end())
		return {};

	auto colon = find(_remapping.begin(), eq, ':');

	Remapping r;

	r.context = colon == eq ? string() : string(_remapping.begin(), colon);
	r.prefix = colon == eq ? string(_remapping.begin(), eq) : string(colon + 1, eq);
	r.target = string(eq + 1, _remapping.end());

	if (r.prefix.empty())
		return {};

	return r;
}

void CompilerStack::setRemappings(vector<Remapping> const& _remappings)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set remappings before parsing."));
	for (auto const& remapping: _remappings)
		solAssert(!remapping.prefix.empty(), "");
	m_remappings = _remappings;
}

void CompilerStack::setEVMVersion(langutil::EVMVersion _version)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set EVM version before parsing."));
	m_evmVersion = _version;
}

void CompilerStack::setLibraries(std::map<std::string, util::h160> const& _libraries)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set libraries before parsing."));
	m_libraries = _libraries;
}

void CompilerStack::setOptimiserSettings(bool _optimize, unsigned _runs)
{
	OptimiserSettings settings = _optimize ? OptimiserSettings::standard() : OptimiserSettings::minimal();
	settings.expectedExecutionsPerDeployment = _runs;
	setOptimiserSettings(std::move(settings));
}

void CompilerStack::setOptimiserSettings(OptimiserSettings _settings)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set optimiser settings before parsing."));
	m_optimiserSettings = std::move(_settings);
}

void CompilerStack::setRevertStringBehaviour(RevertStrings _revertStrings)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set revert string settings before parsing."));
	solUnimplementedAssert(_revertStrings != RevertStrings::VerboseDebug, "");
	m_revertStrings = _revertStrings;
}

void CompilerStack::useMetadataLiteralSources(bool _metadataLiteralSources)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set use literal sources before parsing."));
	m_metadataLiteralSources = _metadataLiteralSources;
}

void CompilerStack::setMetadataHash(MetadataHash _metadataHash)
{
	if (m_stackState >= ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set metadata hash before parsing."));
	m_metadataHash = _metadataHash;
}

void CompilerStack::reset(bool _keepSettings)
{
	m_stackState = Empty;
	m_hasError = false;
	m_sources.clear();
	if (!_keepSettings)
	{
		m_remappings.clear();
		m_libraries.clear();
		m_evmVersion = langutil::EVMVersion();
		m_generateIR = false;
		m_generateEwasm = false;
		m_revertStrings = RevertStrings::Default;
		m_optimiserSettings = OptimiserSettings::minimal();
		m_metadataLiteralSources = false;
		m_metadataHash = MetadataHash::IPFS;
	}
	m_globalContext.reset();
	m_scopes.clear();
	m_sourceOrder.clear();
	m_contracts.clear();
	m_errorReporter.clear();
	TypeProvider::reset();
}

void CompilerStack::setSources(StringMap _sources)
{
	if (m_stackState == SourcesSet)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Cannot change sources once set."));
	if (m_stackState != Empty)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must set sources before parsing."));
	for (auto source: _sources)
		m_sources[source.first].scanner = make_shared<Scanner>(CharStream(/*content*/std::move(source.second), /*name*/source.first));
	m_stackState = SourcesSet;
}

bool CompilerStack::parse()
{
	if (m_stackState != SourcesSet)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must call parse only after the SourcesSet state."));
	m_errorReporter.clear();

	Parser parser{m_errorReporter, m_evmVersion, m_parserErrorRecovery};

	vector<string> sourcesToParse;
	for (auto const& s: m_sources)
		sourcesToParse.push_back(s.first);
	for (size_t i = 0; i < sourcesToParse.size(); ++i)
	{
		string const& path = sourcesToParse[i];
		Source& source = m_sources[path];
		source.scanner->reset();
		source.ast = parser.parse(source.scanner);
		if (!source.ast)
			solAssert(!Error::containsOnlyWarnings(m_errorReporter.errors()), "Parser returned null but did not report error.");
		else
		{
			source.ast->annotation().path = path;
			std::string absPath;
			if (boost::filesystem::path(path).is_absolute())
				absPath = path;
			else
				absPath = boost::filesystem::canonical(path).string();
			for (auto const& newSource: loadMissingSources(*source.ast, absPath))
			{
				string const& newPath = newSource.first;
				string const& newContents = newSource.second;
				m_sources[newPath].scanner = make_shared<Scanner>(CharStream(newContents, newPath));
				sourcesToParse.push_back(newPath);
			}
		}
	}

	m_stackState = ParsingPerformed;
	if (!Error::containsOnlyWarnings(m_errorReporter.errors()))
		m_hasError = true;
	return !m_hasError;
}

void CompilerStack::importASTs(map<string, Json::Value> const& _sources)
{
	if (m_stackState != Empty)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must call importASTs only before the SourcesSet state."));
	m_sourceJsons = _sources;
	map<string, ASTPointer<SourceUnit>> reconstructedSources = ASTJsonImporter(m_evmVersion).jsonToSourceUnit(m_sourceJsons);
	for (auto& src: reconstructedSources)
	{
		string const& path = src.first;
		Source source;
		source.ast = src.second;
		string srcString = util::jsonCompactPrint(m_sourceJsons[src.first]);
		ASTPointer<Scanner> scanner = make_shared<Scanner>(langutil::CharStream(srcString, src.first));
		source.scanner = scanner;
		m_sources[path] = source;
	}
	m_stackState = ParsingPerformed;
	m_importedSources = true;
}

bool CompilerStack::analyze()
{
	if (m_stackState != ParsingPerformed || m_stackState >= AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Must call analyze only after parsing was performed."));
	resolveImports();

	bool noErrors = true;

	try
	{
		SyntaxChecker syntaxChecker(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !syntaxChecker.checkSyntax(*source->ast))
				noErrors = false;

		DocStringAnalyser docStringAnalyser(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !docStringAnalyser.analyseDocStrings(*source->ast))
				noErrors = false;

		m_globalContext = make_shared<GlobalContext>();
		NameAndTypeResolver resolver(*m_globalContext, m_evmVersion, m_scopes, m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !resolver.registerDeclarations(*source->ast))
				return false;

		map<string, SourceUnit const*> sourceUnitsByName;
		for (auto& source: m_sources)
			sourceUnitsByName[source.first] = source.second.ast.get();
		for (Source const* source: m_sourceOrder)
			if (source->ast && !resolver.performImports(*source->ast, sourceUnitsByName))
				return false;

		// This is the main name and type resolution loop. Needs to be run for every contract, because
		// the special variables "this" and "super" must be set appropriately.
		for (Source const* source: m_sourceOrder)
			if (source->ast)
				for (ASTPointer<ASTNode> const& node: source->ast->nodes())
				{
					if (!resolver.resolveNamesAndTypes(*node))
						return false;
					if (ContractDefinition* contract = dynamic_cast<ContractDefinition*>(node.get()))
					{
						// Note that we now reference contracts by their fully qualified names, and
						// thus contracts can only conflict if declared in the same source file. This
						// should already cause a double-declaration error elsewhere.
						if (m_contracts.find(contract->fullyQualifiedName()) == m_contracts.end())
							m_contracts[contract->fullyQualifiedName()].contract = contract;
						else
							solAssert(
								m_errorReporter.hasErrors(),
								"Contract already present (name clash?), but no error was reported."
							);
					}

				}

		// Next, we check inheritance, overrides, function collisions and other things at
		// contract or function level.
		// This also calculates whether a contract is abstract, which is needed by the
		// type checker.
		ContractLevelChecker contractLevelChecker(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast)
				for (ASTPointer<ASTNode> const& node: source->ast->nodes())
					if (ContractDefinition* contract = dynamic_cast<ContractDefinition*>(node.get()))
						if (!contractLevelChecker.check(*contract))
							noErrors = false;

		// New we run full type checks that go down to the expression level. This
		// cannot be done earlier, because we need cross-contract types and information
		// about whether a contract is abstract for the `new` expression.
		// This populates the `type` annotation for all expressions.
		//
		// Note: this does not resolve overloaded functions. In order to do that, types of arguments are needed,
		// which is only done one step later.
		TypeChecker typeChecker(m_evmVersion, m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast)
				for (ASTPointer<ASTNode> const& node: source->ast->nodes())
					if (ContractDefinition* contract = dynamic_cast<ContractDefinition*>(node.get()))
						if (!typeChecker.checkTypeRequirements(*contract))
							noErrors = false;

		if (noErrors)
		{
			// Checks that can only be done when all types of all AST nodes are known.
			PostTypeChecker postTypeChecker(m_errorReporter);
			for (Source const* source: m_sourceOrder)
				if (source->ast && !postTypeChecker.check(*source->ast))
					noErrors = false;
		}

		if (noErrors)
		{
			// Control flow graph generator and analyzer. It can check for issues such as
			// variable is used before it is assigned to.
			CFG cfg(m_errorReporter);
			for (Source const* source: m_sourceOrder)
				if (source->ast && !cfg.constructFlow(*source->ast))
					noErrors = false;

			if (noErrors)
			{
				ControlFlowAnalyzer controlFlowAnalyzer(cfg, m_errorReporter);
				for (Source const* source: m_sourceOrder)
					if (source->ast && !controlFlowAnalyzer.analyze(*source->ast))
						noErrors = false;
			}
		}

		if (noErrors)
		{
			// Checks for common mistakes. Only generates warnings.
			StaticAnalyzer staticAnalyzer(m_errorReporter);
			for (Source const* source: m_sourceOrder)
				if (source->ast && !staticAnalyzer.analyze(*source->ast))
					noErrors = false;
		}

		if (noErrors)
		{
			// Check for state mutability in every function.
			vector<ASTPointer<ASTNode>> ast;
			for (Source const* source: m_sourceOrder)
				if (source->ast)
					ast.push_back(source->ast);

			if (!ViewPureChecker(ast, m_errorReporter).check())
				noErrors = false;
		}

		if (noErrors) {
			//Checks for TVM specific issues.
			TVMAnalyzer tvmAnalyzer(m_errorReporter, m_structWarning);
			for (Source const* source: m_sourceOrder)
				if (source->ast && !tvmAnalyzer.analyze(*source->ast))
					noErrors = false;
		}

		if (noErrors)
		{

			for (Source const* source: m_sourceOrder) {

				std::vector<PragmaDirective const *> pragmaDirectives = getPragmaDirectives(source);
				TVMTypeChecker checker(m_errorReporter, pragmaDirectives);
				source->ast->accept(checker);
				if (m_errorReporter.hasErrors()) {
					noErrors = false;
				}
			}
		}
	}
	catch (FatalError const&)
	{
		if (m_errorReporter.errors().empty())
			throw; // Something is weird here, rather throw again.
		noErrors = false;
	}

	m_stackState = AnalysisPerformed;
	if (!noErrors)
		m_hasError = true;

	return !m_hasError;
}

bool CompilerStack::parseAndAnalyze()
{
	bool success = parse();
	if (success || m_parserErrorRecovery)
		success = analyze();
	return success;
}

bool CompilerStack::isRequestedSource(string const& _sourceName) const
{
	return
		m_requestedContractNames.empty() ||
		m_requestedContractNames.count("") ||
		m_requestedContractNames.count(_sourceName);
}

bool CompilerStack::isRequestedContract(ContractDefinition const& _contract) const
{
	/// In case nothing was specified in outputSelection.
	if (m_requestedContractNames.empty())
		return true;

	for (auto const& key: vector<string>{"", _contract.sourceUnitName()})
	{
		auto const& it = m_requestedContractNames.find(key);
		if (it != m_requestedContractNames.end())
			if (it->second.count(_contract.name()) || it->second.count(""))
				return true;
	}

	return false;
}

std::pair<bool, bool> CompilerStack::compile()
{
	bool didCompileSomething{};
	if (m_stackState < AnalysisPerformed)
		if (!parseAndAnalyze())
			return {false, didCompileSomething};

	if (m_hasError)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Called compile with errors."));

	ContractDefinition const *targetContract{};
	std::vector<PragmaDirective const *> targetPragmaDirectives;

	for (Source const* source: m_sourceOrder) {

		if (source->ast->annotation().path != m_inputFile) {
			continue;
		}


		std::vector<PragmaDirective const *> pragmaDirectives = getPragmaDirectives(source);
		for (auto pragma: pragmaDirectives) {
			if (pragma->parameter()) {
				TypeChecker typeChecker(m_evmVersion, m_errorReporter);
				typeChecker.checkTypeRequirements(*pragma->parameter().get());
			}
		}

		std::vector<ContractDefinition const *> contracts;
		for (ASTPointer<ASTNode> const &node: source->ast->nodes()) {
			if (auto contract = dynamic_cast<ContractDefinition const *>(node.get())) {
				contracts.push_back(contract);
			}
		}


		if (!m_mainContract.empty()) {
			for (ContractDefinition const *contract : contracts) {
				if (contract->name() == m_mainContract) {
					if (m_generateCode && !contract->canBeDeployed()) {
						m_errorReporter.typeError(
								contract->location(),
								"The desired contract isn't deployable (it has not public constructor or it's abstract or it's interface or it's library)."
						);
						return {false, didCompileSomething};
					}
					targetContract = contract;
					targetPragmaDirectives = pragmaDirectives;
				}
			}
			if (targetContract == nullptr) {
				m_errorReporter.typeError(
						SourceLocation(),
						"Source file doesn't contain the desired contract \"" + m_mainContract + "\"."
				);
				return {false, didCompileSomething};
			}
		} else {
			for (ContractDefinition const *contract : contracts) {
				if (m_generateAbi && !m_generateCode) {
					if (!contract->isLibrary()) {
						if (targetContract != nullptr) {
							m_errorReporter.typeError(
									targetContract->location(),
									SecondarySourceLocation().append("Previous contract:",
																	 contract->location()),
									"Source file contains at least two contracts/interfaces."
									" Consider adding the option --contract in compiler command line to select the desired contract/interface."
							);
							return {false, didCompileSomething};
						}
						targetContract = contract;
						targetPragmaDirectives = pragmaDirectives;
					}
				} else if (contract->canBeDeployed() && !contract->isLibrary()) {
					if (targetContract != nullptr) {
						m_errorReporter.typeError(
								targetContract->location(),
								SecondarySourceLocation().append("Previous deployable contract:",
																 contract->location()),
								"Source file contains at least two deployable contracts."
								" Consider adding the option --contract in compiler command line to select the desired contract."
						);
						return {false, didCompileSomething};
					}
					targetContract = contract;
					targetPragmaDirectives = pragmaDirectives;
				}
			}
		}
	}


	if (targetContract != nullptr) {
		try {
			TVMCompilerProceedContract(
				&m_errorReporter,
				*targetContract,
				&targetPragmaDirectives,
				m_generateAbi,
				m_generateCode,
				m_withOptimizations,
				m_doPrintInConsole,
				m_inputFile,
				m_folder,
				m_file_prefix
			);
			didCompileSomething = true;
		} catch (FatalError const &) {
			return {false, didCompileSomething};
		}
	}

	m_stackState = CompilationSuccessful;
	this->link();
	return {true, didCompileSomething};
}

void CompilerStack::link()
{
	solAssert(m_stackState >= CompilationSuccessful, "");
}

vector<string> CompilerStack::contractNames() const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Parsing was not successful."));
	vector<string> contractNames;
	for (auto const& contract: m_contracts)
		contractNames.push_back(contract.first);
	return contractNames;
}

string const CompilerStack::lastContractName() const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Parsing was not successful."));
	// try to find some user-supplied contract
	string contractName;
	for (auto const& it: m_sources)
		for (ASTPointer<ASTNode> const& node: it.second.ast->nodes())
			if (auto contract = dynamic_cast<ContractDefinition const*>(node.get()))
				contractName = contract->fullyQualifiedName();
	return contractName;
}

string const* CompilerStack::sourceMapping(string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	Contract const& c = contract(_contractName);
	return c.sourceMapping.get();
}

string const* CompilerStack::runtimeSourceMapping(string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	Contract const& c = contract(_contractName);
	return c.runtimeSourceMapping.get();
}

std::string const CompilerStack::filesystemFriendlyName(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("No compiled contracts found."));

	// Look up the contract (by its fully-qualified name)
	Contract const& matchContract = m_contracts.at(_contractName);
	// Check to see if it could collide on name
	for (auto const& contract: m_contracts)
	{
		if (contract.second.contract->name() == matchContract.contract->name() &&
				contract.second.contract != matchContract.contract)
		{
			// If it does, then return its fully-qualified name, made fs-friendly
			std::string friendlyName = boost::algorithm::replace_all_copy(_contractName, "/", "_");
			boost::algorithm::replace_all(friendlyName, ":", "_");
			boost::algorithm::replace_all(friendlyName, ".", "_");
			return friendlyName;
		}
	}
	// If no collision, return the contract's name
	return matchContract.contract->name();
}

string const& CompilerStack::yulIR(string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	return contract(_contractName).yulIR;
}

string const& CompilerStack::yulIROptimized(string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	return contract(_contractName).yulIROptimized;
}

string const& CompilerStack::ewasm(string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	return contract(_contractName).ewasm;
}

/// TODO: cache this string
string CompilerStack::assemblyString(string const& /*_contractName*/, StringMap /*_sourceCodes*/) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));
	return string();
}

/// TODO: cache the JSON
Json::Value CompilerStack::assemblyJSON(string const& /*_contractName*/, StringMap const& /*_sourceCodes*/) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));
	return Json::Value();
}

vector<string> CompilerStack::sourceNames() const
{
	vector<string> names;
	for (auto const& s: m_sources)
		names.push_back(s.first);
	return names;
}

map<string, unsigned> CompilerStack::sourceIndices() const
{
	map<string, unsigned> indices;
	unsigned index = 0;
	for (auto const& s: m_sources)
		indices[s.first] = index++;
	return indices;
}

Json::Value const& CompilerStack::natspecUser(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	return natspecUser(contract(_contractName));
}

Json::Value const& CompilerStack::natspecUser(Contract const& _contract) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	solAssert(_contract.contract, "");

	// caches the result
	if (!_contract.userDocumentation)
		_contract.userDocumentation = make_unique<Json::Value>(Natspec::userDocumentation(*_contract.contract));

	return *_contract.userDocumentation;
}

Json::Value const& CompilerStack::natspecDev(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	return natspecDev(contract(_contractName));
}

Json::Value const& CompilerStack::natspecDev(Contract const& _contract) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	solAssert(_contract.contract, "");

	// caches the result
	if (!_contract.devDocumentation)
		_contract.devDocumentation = make_unique<Json::Value>(Natspec::devDocumentation(*_contract.contract));

	return *_contract.devDocumentation;
}

Json::Value CompilerStack::methodIdentifiers(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	Json::Value methodIdentifiers(Json::objectValue);
	for (auto const& it: contractDefinition(_contractName).interfaceFunctions())
		methodIdentifiers[it.second->externalSignature()] = it.first.hex();
	return methodIdentifiers;
}

string const& CompilerStack::metadata(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	return metadata(contract(_contractName));
}

string const& CompilerStack::metadata(Contract const& _contract) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	solAssert(_contract.contract, "");

	// cache the result
	if (!_contract.metadata)
		_contract.metadata = make_unique<string>(createMetadata(_contract));

	return *_contract.metadata;
}

Scanner const& CompilerStack::scanner(string const& _sourceName) const
{
	if (m_stackState < SourcesSet)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("No sources set."));

	return *source(_sourceName).scanner;
}

SourceUnit const& CompilerStack::ast(string const& _sourceName) const
{
	if (m_stackState < ParsingPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Parsing not yet performed."));
	if (!source(_sourceName).ast && !m_parserErrorRecovery)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Parsing was not successful."));

	return *source(_sourceName).ast;
}

ContractDefinition const& CompilerStack::contractDefinition(string const& _contractName) const
{
	if (m_stackState < AnalysisPerformed)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	return *contract(_contractName).contract;
}

size_t CompilerStack::functionEntryPoint(
	std::string const& _contractName,
	FunctionDefinition const& /*_function*/
) const
{
	if (m_stackState != CompilationSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Compilation was not successful."));

	shared_ptr<Compiler> const& compiler = contract(_contractName).compiler;
	if (!compiler)
		return 0;
	return 0;
}

tuple<int, int, int, int> CompilerStack::positionFromSourceLocation(SourceLocation const& _sourceLocation) const
{
	int startLine;
	int startColumn;
	int endLine;
	int endColumn;
	tie(startLine, startColumn) = scanner(_sourceLocation.source->name()).translatePositionToLineColumn(_sourceLocation.start);
	tie(endLine, endColumn) = scanner(_sourceLocation.source->name()).translatePositionToLineColumn(_sourceLocation.end);

	return make_tuple(++startLine, ++startColumn, ++endLine, ++endColumn);
}

h256 const& CompilerStack::Source::keccak256() const
{
	if (keccak256HashCached == h256{})
		keccak256HashCached = util::keccak256(scanner->source());
	return keccak256HashCached;
}

h256 const& CompilerStack::Source::swarmHash() const
{
	if (swarmHashCached == h256{})
		swarmHashCached = util::bzzr1Hash(scanner->source());
	return swarmHashCached;
}

string const& CompilerStack::Source::ipfsUrl() const
{
	if (ipfsUrlCached.empty())
		if (scanner->source().size() < 1024 * 256)
			ipfsUrlCached = "dweb:/ipfs/" + util::ipfsHashBase58(scanner->source());
	return ipfsUrlCached;
}

StringMap CompilerStack::loadMissingSources(SourceUnit const& _ast, std::string const& _sourcePath)
{
	solAssert(m_stackState < ParsingPerformed, "");
	StringMap newSources;
	for (auto const& node: _ast.nodes())
		if (ImportDirective const* import = dynamic_cast<ImportDirective*>(node.get()))
		{
			solAssert(!import->path().empty(), "Import path cannot be empty.");

			if (!boost::filesystem::exists(import->path())) {
				m_errorReporter.parserError(
					import->location(),
					string("Source \"" + import->path() + "\" doesn't exist.")
				);
				continue;
			}
			string importPath = boost::filesystem::canonical(import->path(),
				boost::filesystem::path(_sourcePath).remove_filename()).string();

			// The current value of `path` is the absolute path as seen from this source file.
			// We first have to apply remappings before we can store the actual absolute path
			// as seen globally.
			importPath = applyRemapping(importPath, _sourcePath);
			import->annotation().absolutePath = importPath;
			if (m_sources.count(importPath) || newSources.count(importPath))
				continue;

			ReadCallback::Result result{false, string("File not supplied initially.")};
			if (m_readFile) {
				if (!boost::filesystem::path(importPath).is_absolute())
					importPath = (boost::filesystem::path(_sourcePath).remove_filename() / importPath).string();
				result = m_readFile(ReadCallback::kindString(ReadCallback::Kind::ReadFile), importPath);
			}
			if (result.success)
				newSources[importPath] = result.responseOrErrorMessage;
			else
			{
				m_errorReporter.parserError(
					import->location(),
					string("Source \"" + importPath + "\" not found: " + result.responseOrErrorMessage)
				);
				continue;
			}
		}
	return newSources;
}

string CompilerStack::applyRemapping(string const& _path, string const& _context)
{
	solAssert(m_stackState < ParsingPerformed, "");
	// Try to find the longest prefix match in all remappings that are active in the current context.
	auto isPrefixOf = [](string const& _a, string const& _b)
	{
		if (_a.length() > _b.length())
			return false;
		return std::equal(_a.begin(), _a.end(), _b.begin());
	};

	size_t longestPrefix = 0;
	size_t longestContext = 0;
	string bestMatchTarget;

	for (auto const& redir: m_remappings)
	{
		string context = util::sanitizePath(redir.context);
		string prefix = util::sanitizePath(redir.prefix);

		// Skip if current context is closer
		if (context.length() < longestContext)
			continue;
		// Skip if redir.context is not a prefix of _context
		if (!isPrefixOf(context, _context))
			continue;
		// Skip if we already have a closer prefix match.
		if (prefix.length() < longestPrefix && context.length() == longestContext)
			continue;
		// Skip if the prefix does not match.
		if (!isPrefixOf(prefix, _path))
			continue;

		longestContext = context.length();
		longestPrefix = prefix.length();
		bestMatchTarget = util::sanitizePath(redir.target);
	}
	string path = bestMatchTarget;
	path.append(_path.begin() + longestPrefix, _path.end());
	return path;
}

void CompilerStack::resolveImports()
{
	solAssert(m_stackState == ParsingPerformed, "");

	// topological sorting (depth first search) of the import graph, cutting potential cycles
	vector<Source const*> sourceOrder;
	set<Source const*> sourcesSeen;

	function<void(Source const*)> toposort = [&](Source const* _source)
	{
		if (sourcesSeen.count(_source))
			return;
		sourcesSeen.insert(_source);
		if (_source->ast)
			for (ASTPointer<ASTNode> const& node: _source->ast->nodes())
				if (ImportDirective const* import = dynamic_cast<ImportDirective*>(node.get()))
				{
					string const& path = import->annotation().absolutePath;
					solAssert(!path.empty(), "");
					solAssert(m_sources.count(path), "");
					import->annotation().sourceUnit = m_sources[path].ast.get();
					toposort(&m_sources[path]);
				}
		sourceOrder.push_back(_source);
	};

	for (auto const& sourcePair: m_sources)
		if (isRequestedSource(sourcePair.first))
			toposort(&sourcePair.second);

	swap(m_sourceOrder, sourceOrder);
}

namespace
{
bool onlySafeExperimentalFeaturesActivated(set<ExperimentalFeature> const& features)
{
	for (auto const feature: features)
		if (!ExperimentalFeatureWithoutWarning.count(feature))
			return false;
	return true;
}
}

void CompilerStack::compileContract(
	ContractDefinition const& _contract,
	map<ContractDefinition const*, shared_ptr<Compiler const>>& _otherCompilers
)
{
	solAssert(m_stackState >= AnalysisPerformed, "");
	if (m_hasError)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Called compile with errors."));

	for (auto const* dependency: _contract.annotation().contractDependencies)
		compileContract(*dependency, _otherCompilers);

	Contract& compiledContract = m_contracts.at(_contract.fullyQualifiedName());

	bytes cborEncodedMetadata = createCBORMetadata(
		metadata(compiledContract),
		!onlySafeExperimentalFeaturesActivated(_contract.sourceUnit().annotation().experimentalFeatures)
	);
}

void CompilerStack::generateIR(ContractDefinition const& _contract)
{
	solAssert(m_stackState >= AnalysisPerformed, "");
	if (m_hasError)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Called generateIR with errors."));

	if (!_contract.canBeDeployed())
		return;

	Contract& compiledContract = m_contracts.at(_contract.fullyQualifiedName());
	if (!compiledContract.yulIR.empty())
		return;

	for (auto const* dependency: _contract.annotation().contractDependencies)
		generateIR(*dependency);
}

void CompilerStack::generateEwasm(ContractDefinition const& _contract)
{
	solAssert(m_stackState >= AnalysisPerformed, "");
	if (m_hasError)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Called generateEwasm with errors."));

	Contract& compiledContract = m_contracts.at(_contract.fullyQualifiedName());
	solAssert(!compiledContract.yulIROptimized.empty(), "");
	if (!compiledContract.ewasm.empty())
		return;
}

CompilerStack::Contract const& CompilerStack::contract(string const& _contractName) const
{
	solAssert(m_stackState >= AnalysisPerformed, "");

	auto it = m_contracts.find(_contractName);
	if (it != m_contracts.end())
		return it->second;

	// To provide a measure of backward-compatibility, if a contract is not located by its
	// fully-qualified name, a lookup will be attempted purely on the contract's name to see
	// if anything will satisfy.
	if (_contractName.find(':') == string::npos)
	{
		for (auto const& contractEntry: m_contracts)
		{
			stringstream ss;
			ss.str(contractEntry.first);
			// All entries are <source>:<contract>
			string source;
			string foundName;
			getline(ss, source, ':');
			getline(ss, foundName, ':');
			if (foundName == _contractName)
				return contractEntry.second;
		}
	}

	// If we get here, both lookup methods failed.
	BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Contract \"" + _contractName + "\" not found."));
}

CompilerStack::Source const& CompilerStack::source(string const& _sourceName) const
{
	auto it = m_sources.find(_sourceName);
	if (it == m_sources.end()) {
		it = std::find_if(m_sources.begin(), m_sources.end(), [&_sourceName] (auto it) {
			return _sourceName == boost::filesystem::canonical(it.first).string();
		});
		if (it == m_sources.end())
			BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Given source file not found."));
	}

	return it->second;
}

string CompilerStack::createMetadata(Contract const& _contract) const
{
	Json::Value meta;
	meta["version"] = 1;
	meta["language"] = m_importedSources ? "SolidityAST" : "Solidity";
	meta["compiler"]["version"] = VersionStringStrict;

	/// All the source files (including self), which should be included in the metadata.
	set<string> referencedSources;
	referencedSources.insert(_contract.contract->sourceUnit().annotation().path);
	for (auto const sourceUnit: _contract.contract->sourceUnit().referencedSourceUnits(true))
		referencedSources.insert(sourceUnit->annotation().path);

	meta["sources"] = Json::objectValue;
	for (auto const& s: m_sources)
	{
		if (!referencedSources.count(s.first))
			continue;

		solAssert(s.second.scanner, "Scanner not available");
		meta["sources"][s.first]["keccak256"] = "0x" + toHex(s.second.keccak256().asBytes());
		if (m_metadataLiteralSources)
			meta["sources"][s.first]["content"] = s.second.scanner->source();
		else
		{
			meta["sources"][s.first]["urls"] = Json::arrayValue;
			meta["sources"][s.first]["urls"].append("bzz-raw://" + toHex(s.second.swarmHash().asBytes()));
			meta["sources"][s.first]["urls"].append(s.second.ipfsUrl());
		}
	}

	static_assert(sizeof(m_optimiserSettings.expectedExecutionsPerDeployment) <= sizeof(Json::LargestUInt), "Invalid word size.");
	solAssert(static_cast<Json::LargestUInt>(m_optimiserSettings.expectedExecutionsPerDeployment) < std::numeric_limits<Json::LargestUInt>::max(), "");
	meta["settings"]["optimizer"]["runs"] = Json::Value(Json::LargestUInt(m_optimiserSettings.expectedExecutionsPerDeployment));

	/// Backwards compatibility: If set to one of the default settings, do not provide details.
	OptimiserSettings settingsWithoutRuns = m_optimiserSettings;
	// reset to default
	settingsWithoutRuns.expectedExecutionsPerDeployment = OptimiserSettings::minimal().expectedExecutionsPerDeployment;
	if (settingsWithoutRuns == OptimiserSettings::minimal())
		meta["settings"]["optimizer"]["enabled"] = false;
	else if (settingsWithoutRuns == OptimiserSettings::standard())
		meta["settings"]["optimizer"]["enabled"] = true;
	else
	{
		Json::Value details{Json::objectValue};

		details["orderLiterals"] = m_optimiserSettings.runOrderLiterals;
		details["jumpdestRemover"] = m_optimiserSettings.runJumpdestRemover;
		details["peephole"] = m_optimiserSettings.runPeephole;
		details["deduplicate"] = m_optimiserSettings.runDeduplicate;
		details["cse"] = m_optimiserSettings.runCSE;
		details["constantOptimizer"] = m_optimiserSettings.runConstantOptimiser;
		details["yul"] = m_optimiserSettings.runYulOptimiser;
		if (m_optimiserSettings.runYulOptimiser)
		{
			details["yulDetails"] = Json::objectValue;
			details["yulDetails"]["stackAllocation"] = m_optimiserSettings.optimizeStackAllocation;
		}

		meta["settings"]["optimizer"]["details"] = std::move(details);
	}

	if (m_revertStrings != RevertStrings::Default)
		meta["settings"]["debug"]["revertStrings"] = revertStringsToString(m_revertStrings);

	if (m_metadataLiteralSources)
		meta["settings"]["metadata"]["useLiteralContent"] = true;

	static vector<string> hashes{"ipfs", "bzzr1", "none"};
	meta["settings"]["metadata"]["bytecodeHash"] = hashes.at(unsigned(m_metadataHash));

	meta["settings"]["evmVersion"] = m_evmVersion.name();
	meta["settings"]["compilationTarget"][_contract.contract->sourceUnitName()] =
		_contract.contract->annotation().canonicalName;

	meta["settings"]["remappings"] = Json::arrayValue;
	set<string> remappings;
	for (auto const& r: m_remappings)
		remappings.insert(r.context + ":" + r.prefix + "=" + r.target);
	for (auto const& r: remappings)
		meta["settings"]["remappings"].append(r);

	meta["settings"]["libraries"] = Json::objectValue;
	for (auto const& library: m_libraries)
		meta["settings"]["libraries"][library.first] = "0x" + toHex(library.second.asBytes());

	// meta["output"]["abi"] = contractABI(_contract);
	meta["output"]["userdoc"] = natspecUser(_contract);
	meta["output"]["devdoc"] = natspecDev(_contract);

	return util::jsonCompactPrint(meta);
}

class MetadataCBOREncoder
{
public:
	void pushBytes(string const& key, bytes const& value)
	{
		m_entryCount++;
		pushTextString(key);
		pushByteString(value);
	}

	void pushString(string const& key, string const& value)
	{
		m_entryCount++;
		pushTextString(key);
		pushTextString(value);
	}

	void pushBool(string const& key, bool value)
	{
		m_entryCount++;
		pushTextString(key);
		pushBool(value);
	}

	bytes serialise() const
	{
		unsigned size = m_data.size() + 1;
		solAssert(size <= 0xffff, "Metadata too large.");
		solAssert(m_entryCount <= 0x1f, "Too many map entries.");

		// CBOR fixed-length map
		bytes ret{static_cast<unsigned char>(0xa0 + m_entryCount)};
		// The already encoded key-value pairs
		ret += m_data;
		// 16-bit big endian length
		ret += util::toCompactBigEndian(size, 2);
		return ret;
	}

private:
	void pushTextString(string const& key)
	{
		unsigned length = key.size();
		if (length < 24)
		{
			m_data += bytes{static_cast<unsigned char>(0x60 + length)};
			m_data += key;
		}
		else if (length <= 256)
		{
			m_data += bytes{0x78, static_cast<unsigned char>(length)};
			m_data += key;
		}
		else
			solAssert(false, "Text string too large.");
	}
	void pushByteString(bytes const& key)
	{
		unsigned length = key.size();
		if (length < 24)
		{
			m_data += bytes{static_cast<unsigned char>(0x40 + length)};
			m_data += key;
		}
		else if (length <= 256)
		{
			m_data += bytes{0x58, static_cast<unsigned char>(length)};
			m_data += key;
		}
		else
			solAssert(false, "Byte string too large.");
	}
	void pushBool(bool value)
	{
		if (value)
			m_data += bytes{0xf5};
		else
			m_data += bytes{0xf4};
	}
	unsigned m_entryCount = 0;
	bytes m_data;
};

bytes CompilerStack::createCBORMetadata(string const& _metadata, bool _experimentalMode)
{
	MetadataCBOREncoder encoder;

	if (m_metadataHash == MetadataHash::IPFS)
	{
		solAssert(_metadata.length() < 1024 * 256, "Metadata too large.");
		encoder.pushBytes("ipfs", util::ipfsHash(_metadata));
	}
	else if (m_metadataHash == MetadataHash::Bzzr1)
		encoder.pushBytes("bzzr1", util::bzzr1Hash(_metadata).asBytes());
	else
		solAssert(m_metadataHash == MetadataHash::None, "Invalid metadata hash");

	if (_experimentalMode)
		encoder.pushBool("experimental", true);
	if (m_release)
		encoder.pushBytes("solc", VersionCompactBytes);
	else
		encoder.pushString("solc", VersionStringStrict);
	return encoder.serialise();
}


std::vector<PragmaDirective const *> CompilerStack::getPragmaDirectives(Source const* source) const {
	std::vector<PragmaDirective const *> pragmaDirectives;
	for (ASTPointer<ASTNode> const &node: source->ast->nodes()) {
		if (auto pragma = dynamic_cast<PragmaDirective const *>(node.get())) {
			pragmaDirectives.push_back(pragma);
		}
	}
	return pragmaDirectives;
}
