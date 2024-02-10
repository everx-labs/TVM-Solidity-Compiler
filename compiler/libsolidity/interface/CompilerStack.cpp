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
 * @author Gav Wood <g@ethdev.com>
 * @date 2014
 * Full-stack compiler that converts a source code string to bytecode.
 */


#include <libsolidity/interface/CompilerStack.h>
#include <libsolidity/interface/ImportRemapper.h>

#include <libsolidity/analysis/ControlFlowAnalyzer.h>
#include <libsolidity/analysis/ControlFlowGraph.h>
#include <libsolidity/analysis/ControlFlowRevertPruner.h>
#include <libsolidity/analysis/ContractLevelChecker.h>
#include <libsolidity/analysis/DeclarationTypeChecker.h>
#include <libsolidity/analysis/DocStringAnalyser.h>
#include <libsolidity/analysis/DocStringTagParser.h>
#include <libsolidity/analysis/GlobalContext.h>
#include <libsolidity/analysis/NameAndTypeResolver.h>
#include <libsolidity/analysis/PostTypeChecker.h>
#include <libsolidity/analysis/PostTypeContractLevelChecker.h>
#include <libsolidity/analysis/StaticAnalyzer.h>
#include <libsolidity/analysis/SyntaxChecker.h>
#include <libsolidity/analysis/Scoper.h>
#include <libsolidity/analysis/TypeChecker.h>
#include <libsolidity/analysis/ViewPureChecker.h>
#include <libsolidity/analysis/ImmutableValidator.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>
#include <libsolidity/ast/ASTJsonImporter.h>
#include <libsolidity/interface/Natspec.h>
#include <libsolidity/interface/Version.h>
#include <libsolidity/parsing/Parser.h>

#include <libsolidity/experimental/analysis/Analysis.h>
#include <libsolidity/experimental/analysis/FunctionDependencyAnalysis.h>

#include <libstdlib/stdlib.h>

#include <liblangutil/Scanner.h>
#include <liblangutil/SemVerHandler.h>
#include <liblangutil/SourceReferenceFormatter.h>

#include <libsolutil/SwarmHash.h>
#include <libsolutil/IpfsHash.h>
#include <libsolutil/JSON.h>

#include <libsolutil/Keccak256.h>

#include <boost/algorithm/string.hpp>

#include <libsolutil/Algorithms.h>
#include <libsolutil/FunctionSelector.h>

#include <json/json.h>

#include <boost/algorithm/string/replace.hpp>

#include <range/v3/algorithm/all_of.hpp>
#include <range/v3/view/concat.hpp>
#include <range/v3/view/map.hpp>

#include <fmt/format.h>

#include <utility>
#include <map>
#include <limits>
#include <string>

#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMTypeChecker.hpp>
#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TvmAstVisitor.hpp>
#include <libsolidity/codegen/TVMContractCompiler.hpp>

using namespace solidity;
using namespace solidity::langutil;
using namespace solidity::frontend;
using namespace solidity::stdlib;
using namespace solidity::yul;
using namespace std::string_literals;

using solidity::util::errinfo_comment;

using solidity::util::h256;

static int g_compilerStackCounts = 0;

using namespace solidity::langutil;

#include <stdlib.h>

CompilerStack::CompilerStack(ReadCallback::Callback _readFile):
	m_readFile{std::move(_readFile)},
	m_errorReporter{m_errorList}
{
	// Because TypeProvider is currently a singleton API, we must ensure that
	// no more than one entity is actually using it at a time.
	solAssert(g_compilerStackCounts == 0, "You shall not have another CompilerStack aside me.");
	++g_compilerStackCounts;
	GlobalParams::g_errorReporter = &m_errorReporter;
	GlobalParams::g_charStreamProvider = this;
}

CompilerStack::~CompilerStack()
{
	--g_compilerStackCounts;
	TypeProvider::reset();
}

void CompilerStack::createAndAssignCallGraphs()
{
	for (Source const* source: m_sourceOrder)
	{
		if (!source->ast)
			continue;

		for (ContractDefinition const* contract: ASTNode::filteredNodes<ContractDefinition>(source->ast->nodes()))
		{
			ContractDefinitionAnnotation& annotation =
				m_contracts.at(contract->fullyQualifiedName()).contract->annotation();

			annotation.creationCallGraph = std::make_unique<CallGraph>(
				FunctionCallGraphBuilder::buildCreationGraph(*contract)
			);
			annotation.deployedCallGraph = std::make_unique<CallGraph>(
				FunctionCallGraphBuilder::buildDeployedGraph(
					*contract,
					**annotation.creationCallGraph
				)
			);

			solAssert(annotation.contractDependencies.empty(), "contractDependencies expected to be empty?!");

			annotation.contractDependencies = annotation.creationCallGraph->get()->bytecodeDependency;

			for (auto const& [dependencyContract, referencee]: annotation.deployedCallGraph->get()->bytecodeDependency)
				annotation.contractDependencies.emplace(dependencyContract, referencee);
		}
	}
}

void CompilerStack::findAndReportCyclicContractDependencies()
{
	// Cycles we found, used to avoid duplicate reports for the same reference
	std::set<ASTNode const*, ASTNode::CompareByID> foundCycles;

	for (Source const* source: m_sourceOrder)
	{
		if (!source->ast)
			continue;

		for (ContractDefinition const* contractDefinition: ASTNode::filteredNodes<ContractDefinition>(source->ast->nodes()))
		{
			util::CycleDetector<ContractDefinition> cycleDetector{[&](
				ContractDefinition const& _contract,
				util::CycleDetector<ContractDefinition>& _cycleDetector,
				size_t _depth
			)
			{
				// No specific reason for exactly that number, just a limit we're unlikely to hit.
				if (_depth >= 256)
					m_errorReporter.fatalTypeError(
						7864_error,
						_contract.location(),
						"Contract dependencies exhausting cyclic dependency validator"
					);

				for (auto& [dependencyContract, referencee]: _contract.annotation().contractDependencies)
					if (_cycleDetector.run(*dependencyContract))
						return;
			}};

			ContractDefinition const* cycle = cycleDetector.run(*contractDefinition);

			if (!cycle)
				continue;

			ASTNode const* referencee = contractDefinition->annotation().contractDependencies.at(cycle);

			if (foundCycles.find(referencee) != foundCycles.end())
				continue;

			SecondarySourceLocation secondaryLocation{};
			secondaryLocation.append("Referenced contract is here:"s, cycle->location());

			m_errorReporter.typeError(
				7813_error,
				referencee->location(),
				secondaryLocation,
				"Circular reference to contract bytecode either via \"new\" or \"type(...).creationCode\" / \"type(...).runtimeCode\"."
			);

			foundCycles.emplace(referencee);
		}
	}
}

void CompilerStack::setRemappings(std::vector<ImportRemapper::Remapping> _remappings)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set remappings before parsing.");
	for (auto const& remapping: _remappings)
		solAssert(!remapping.prefix.empty(), "");
	m_importRemapper.setRemappings(std::move(_remappings));
}

void CompilerStack::setViaIR(bool _viaIR)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set viaIR before parsing.");
	m_viaIR = _viaIR;
}

void CompilerStack::setEVMVersion(langutil::EVMVersion _version)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set EVM version before parsing.");
	m_evmVersion = _version;
	// GlobalContext depends on evmVersion since the Cancun hardfork.
	// Therefore, we reset it whenever we set a new EVM version, ensuring that the context is never reused with a mismatched version.
	m_globalContext.reset();
}

void CompilerStack::setTVMVersion(langutil::TVMVersion _version)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set TVM version before parsing.");
	m_tvmVersion = _version;

	GlobalParams::g_tvmVersion = m_tvmVersion;
}

void CompilerStack::setLibraries(std::map<std::string, util::h160> const& _libraries)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set libraries before parsing.");
	m_libraries = _libraries;
}

void CompilerStack::setOptimiserSettings(bool _optimize, size_t _runs)
{
	OptimiserSettings settings = _optimize ? OptimiserSettings::standard() : OptimiserSettings::minimal();
	settings.expectedExecutionsPerDeployment = _runs;
	setOptimiserSettings(std::move(settings));
}

void CompilerStack::setOptimiserSettings(OptimiserSettings _settings)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set optimiser settings before parsing.");
	m_optimiserSettings = std::move(_settings);
}

void CompilerStack::setRevertStringBehaviour(RevertStrings _revertStrings)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set revert std::string settings before parsing.");
	solUnimplementedAssert(_revertStrings != RevertStrings::VerboseDebug);
	m_revertStrings = _revertStrings;
}

void CompilerStack::useMetadataLiteralSources(bool _metadataLiteralSources)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set use literal sources before parsing.");
	m_metadataLiteralSources = _metadataLiteralSources;
}

void CompilerStack::setMetadataHash(MetadataHash _metadataHash)
{
	if (m_stackState >= ParsedAndImported)
		solThrow(CompilerError, "Must set metadata hash before parsing.");
	m_metadataHash = _metadataHash;
}

void CompilerStack::reset(bool _keepSettings)
{
	m_stackState = Empty;
	m_sources.clear();
	if (!_keepSettings)
	{
		m_importRemapper.clear();
		m_libraries.clear();
		m_viaIR = false;
		m_evmVersion = langutil::EVMVersion();
		m_generateIR = false;
		m_revertStrings = RevertStrings::Default;
		m_optimiserSettings = OptimiserSettings::minimal();
		m_metadataLiteralSources = false;
		m_metadataFormat = defaultMetadataFormat();
		m_metadataHash = MetadataHash::IPFS;
		m_stopAfter = State::CompilationSuccessful;
	}
	m_experimentalAnalysis.reset();
	m_globalContext.reset();
	m_sourceOrder.clear();
	m_contracts.clear();
	m_errorReporter.clear();
	TypeProvider::reset();
}

void CompilerStack::setSources(StringMap _sources)
{
	if (m_stackState == SourcesSet)
		solThrow(CompilerError, "Cannot change sources once set.");
	if (m_stackState != Empty)
		solThrow(CompilerError, "Must set sources before parsing.");
	for (auto source: _sources)
		m_sources[source.first].charStream = std::make_unique<CharStream>(/*content*/std::move(source.second), /*name*/source.first);
	m_stackState = SourcesSet;
}

bool CompilerStack::parse()
{
	if (m_stackState != SourcesSet)
		solThrow(CompilerError, "Must call parse only after the SourcesSet state.");
	m_errorReporter.clear();

	Parser parser{m_errorReporter, m_evmVersion};

	std::vector<std::string> sourcesToParse;
	for (auto const& s: m_sources)
		sourcesToParse.push_back(s.first);

	for (size_t i = 0; i < sourcesToParse.size(); ++i)
	{
		std::string const& path = sourcesToParse[i];
		Source& source = m_sources[path];
		source.ast = parser.parse(*source.charStream);
		if (!source.ast)
			solAssert(Error::containsErrors(m_errorReporter.errors()), "Parser returned null but did not report error.");
		else
		{
			source.ast->annotation().path = path;
			for (auto const& import: ASTNode::filteredNodes<ImportDirective>(source.ast->nodes()))
			{
				solAssert(!import->path().empty(), "Import path cannot be empty.");
				// Check whether the import directive is for the standard library,
				// and if yes, add specified file to source units to be parsed.
				auto it = stdlib::sources.find(import->path());
				if (it != stdlib::sources.end())
				{
					auto [name, content] = *it;
					m_sources[name].charStream = std::make_unique<CharStream>(content, name);
					sourcesToParse.push_back(name);
				}

				// The current value of `path` is the absolute path as seen from this source file.
				// We first have to apply remappings before we can store the actual absolute path
				// as seen globally.
				import->annotation().absolutePath = applyRemapping(util::absolutePath(
					import->path(),
					path
				), path);
			}

			if (m_stopAfter >= ParsedAndImported)
				for (auto const& newSource: loadMissingSources(*source.ast))
				{
					std::string const& newPath = newSource.first;
					std::string const& newContents = newSource.second;
					m_sources[newPath].charStream = std::make_shared<CharStream>(newContents, newPath);
					sourcesToParse.push_back(newPath);
				}
		}
	}

	if (Error::containsErrors(m_errorReporter.errors()))
		return false;

	m_stackState = (m_stopAfter <= Parsed ? Parsed : ParsedAndImported);
	storeContractDefinitions();

	solAssert(!m_maxAstId.has_value());
	m_maxAstId = parser.maxID();

	return true;
}

void CompilerStack::importASTs(std::map<std::string, Json::Value> const& _sources)
{
	if (m_stackState != Empty)
		solThrow(CompilerError, "Must call importASTs only before the SourcesSet state.");
	std::map<std::string, ASTPointer<SourceUnit>> reconstructedSources = ASTJsonImporter(m_evmVersion).jsonToSourceUnit(_sources);
	for (auto& src: reconstructedSources)
	{
		solUnimplementedAssert(!src.second->experimentalSolidity());
		std::string const& path = src.first;
		Source source;
		source.ast = src.second;
		source.charStream = std::make_shared<CharStream>(
			util::jsonCompactPrint(_sources.at(src.first)),
			src.first,
			true // imported from AST
		);
		m_sources[path] = std::move(source);
	}
	m_stackState = ParsedAndImported;
	m_compilationSourceType = CompilationSourceType::SolidityAST;

	storeContractDefinitions();
}

bool CompilerStack::analyze()
{
	if (m_stackState != ParsedAndImported)
		solThrow(CompilerError, "Must call analyze only after parsing was successful.");

	if (!resolveImports())
		return false;

	for (Source const* source: m_sourceOrder)
		if (source->ast)
			Scoper::assignScopes(*source->ast);

	bool noErrors = true;

	try
	{
		SyntaxChecker syntaxChecker(m_errorReporter);
		bool experimentalSolidity = isExperimentalSolidity();
		for (Source const* source: m_sourceOrder)
			if (source->ast && !syntaxChecker.checkSyntax(*source->ast))
				noErrors = false;

		m_globalContext = std::make_shared<GlobalContext>(m_evmVersion);
		// We need to keep the same resolver during the whole process.
		NameAndTypeResolver resolver(*m_globalContext, m_evmVersion, m_errorReporter, experimentalSolidity);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !resolver.registerDeclarations(*source->ast))
				return false;

		std::map<std::string, SourceUnit const*> sourceUnitsByName;
		for (auto& source: m_sources)
			sourceUnitsByName[source.first] = source.second.ast.get();
		for (Source const* source: m_sourceOrder)
			if (source->ast && !resolver.performImports(*source->ast, sourceUnitsByName))
				return false;

		resolver.warnHomonymDeclarations();

		{
			DocStringTagParser docStringTagParser(m_errorReporter);
			for (Source const* source: m_sourceOrder)
				if (source->ast && !docStringTagParser.parseDocStrings(*source->ast))
					noErrors = false;
		}

		// Requires DocStringTagParser
		for (Source const* source: m_sourceOrder)
			if (source->ast && !resolver.resolveNamesAndTypes(*source->ast))
				return false;

		if (experimentalSolidity)
		{
			if (!analyzeExperimental())
				noErrors = false;
		}
		else if (!analyzeLegacy(noErrors))
			noErrors = false;
	}
	catch (FatalError const&)
	{
		if (m_errorReporter.errors().empty())
			throw; // Something is weird here, rather throw again.
		noErrors = false;
	}

	if (!noErrors)
		return false;

	m_stackState = AnalysisSuccessful;
	return true;
}


bool CompilerStack::analyzeLegacy(bool _noErrorsSoFar)
{
	bool noErrors = _noErrorsSoFar;

	DeclarationTypeChecker declarationTypeChecker(m_errorReporter, m_evmVersion);
	for (Source const* source: m_sourceOrder)
		if (source->ast && !declarationTypeChecker.check(*source->ast))
			return false;

	// Requires DeclarationTypeChecker to have run
	DocStringTagParser docStringTagParser(m_errorReporter);
	for (Source const* source: m_sourceOrder)
		if (source->ast && !docStringTagParser.validateDocStringsUsingTypes(*source->ast))
			noErrors = false;

	// Next, we check inheritance, overrides, function collisions and other things at
	// contract or function level.
	// This also calculates whether a contract is abstract, which is needed by the
	// type checker.
	ContractLevelChecker contractLevelChecker(m_errorReporter);

	for (Source const* source: m_sourceOrder)
		if (auto sourceAst = source->ast)
			noErrors = contractLevelChecker.check(*sourceAst);

	// Now we run full type checks that go down to the expression level. This
	// cannot be done earlier, because we need cross-contract types and information
	// about whether a contract is abstract for the `new` expression.
	// This populates the `type` annotation for all expressions.
	//
	// Note: this does not resolve overloaded functions. In order to do that, types of arguments are needed,
	// which is only done one step later.
	TypeChecker typeChecker(m_evmVersion, m_errorReporter);
	for (Source const* source: m_sourceOrder)
		if (source->ast && !typeChecker.checkTypeRequirements(*source->ast))
			noErrors = false;

	if (noErrors)
	{
		// Requires ContractLevelChecker and TypeChecker
		DocStringAnalyser docStringAnalyser(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !docStringAnalyser.analyseDocStrings(*source->ast))
				noErrors = false;
	}

	if (noErrors)
	{
		// Checks that can only be done when all types of all AST nodes are known.
		PostTypeChecker postTypeChecker(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !postTypeChecker.check(*source->ast))
				noErrors = false;
		if (!postTypeChecker.finalize())
			noErrors = false;
	}

	// Create & assign callgraphs and check for contract dependency cycles
	if (noErrors)
	{
		createAndAssignCallGraphs();
		annotateInternalFunctionIDs();
		//findAndReportCyclicContractDependencies();
	}

	if (noErrors)
		for (Source const* source: m_sourceOrder)
			if (source->ast && !PostTypeContractLevelChecker{m_errorReporter}.check(*source->ast))
				noErrors = false;

	// Check that immutable variables are never read in c'tors and assigned
	// exactly once
	if (noErrors)
		for (Source const* source: m_sourceOrder)
			if (source->ast)
				for (ASTPointer<ASTNode> const& node: source->ast->nodes())
					if (ContractDefinition* contract = dynamic_cast<ContractDefinition*>(node.get()))
						ImmutableValidator(m_errorReporter, *contract).analyze();

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
			ControlFlowRevertPruner pruner(cfg);
			pruner.run();

			ControlFlowAnalyzer controlFlowAnalyzer(cfg, m_errorReporter);
			if (!controlFlowAnalyzer.run())
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
		std::vector<ASTPointer<ASTNode>> ast;
		for (Source const* source: m_sourceOrder)
			if (source->ast)
				ast.push_back(source->ast);

		if (!ViewPureChecker(ast, m_errorReporter).check())
			noErrors = false;
	}

	if (noErrors) {
		// Check for TVM specific issues.
		TVMAnalyzer tvmAnalyzer(m_errorReporter);
		for (Source const* source: m_sourceOrder)
			if (source->ast && !tvmAnalyzer.analyze(*source->ast))
				noErrors = false;
	}

	if (noErrors)
	{
		// Check for TVM specific issues.
		// TODO merge TVMTypeChecker and TVMAnalyzer ?
		for (Source const* source: m_sourceOrder) {
			TVMTypeChecker checker(m_errorReporter);
			source->ast->accept(checker);
			if (m_errorReporter.hasErrors()) {
				noErrors = false;
			}
		}
	}

	return noErrors;
}

bool CompilerStack::analyzeExperimental()
{
	solAssert(!m_experimentalAnalysis);
	solAssert(m_maxAstId && *m_maxAstId >= 0);
	m_experimentalAnalysis = std::make_unique<experimental::Analysis>(m_errorReporter, static_cast<std::uint64_t>(*m_maxAstId));
	std::vector<std::shared_ptr<SourceUnit const>> sourceAsts;
	for (Source const* source: m_sourceOrder)
		if (source->ast)
			sourceAsts.emplace_back(source->ast);
	return m_experimentalAnalysis->check(sourceAsts);
}

bool CompilerStack::parseAndAnalyze(State _stopAfter)
{
	m_stopAfter = _stopAfter;

	bool success = parse();
	if (m_stackState >= m_stopAfter)
		return success;
	if (success)
		success = analyze();
	return success;
}

bool CompilerStack::isRequestedSource(std::string const& _sourceName) const
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

	for (auto const& key: std::vector<std::string>{"", _contract.sourceUnitName()})
	{
		auto const& it = m_requestedContractNames.find(key);
		if (it != m_requestedContractNames.end())
			if (it->second.count(_contract.name()) || it->second.count(""))
				return true;
	}

	return false;
}

std::pair<bool, bool> CompilerStack::compile(bool json)
{
	bool didCompileSomething{};
	if (m_stackState < AnalysisSuccessful)
		if (!parseAndAnalyze())
			return {false, didCompileSomething};

	if (m_hasError)
		solThrow(CompilerError, "Called compile with errors.");

	if (m_generateAbi || m_generateCode || m_doPrintFunctionIds || m_doPrivateFunctionIds) {
		ContractDefinition const *targetContract{};
		std::vector<PragmaDirective const *> targetPragmaDirectives;

		bool findSrc = false;
		for (Source const *source: m_sourceOrder) {
			std::string curSrcPath = *source->ast->annotation().path;
			if (curSrcPath != m_inputFile) {
				continue;
			}

			findSrc = true;
			std::vector<PragmaDirective const *> pragmaDirectives = getPragmaDirectives(source);

			std::vector<ContractDefinition const *> contracts;
			for (ASTPointer<ASTNode> const &node: source->ast->nodes()) {
				if (auto contract = dynamic_cast<ContractDefinition const *>(node.get())) {
					contracts.push_back(contract);
				}
			}


			for (ContractDefinition const *contract: contracts) {
				if (contract->isLibrary()) {
					continue;
				}

				if (!m_mainContract.empty()) {
					if (contract->name() == m_mainContract) {
						if (m_generateCode && !contract->canBeDeployed()) {
							m_errorReporter.typeError(
								3715_error,
								contract->location(),
								"The desired contract isn't deployable (it has not public constructor or it's abstract or it's interface or it's library)."
							);
							return {false, didCompileSomething};
						}
						targetContract = contract;
						targetPragmaDirectives = pragmaDirectives;
					}
				} else {
					if (m_generateAbi && !m_generateCode) {
						if (targetContract != nullptr) {
							m_errorReporter.typeError(
								4605_error,
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
					} else if (contract->canBeDeployed()) {
						if (targetContract != nullptr) {
							m_errorReporter.typeError(
								5205_error,
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
		if (!findSrc) {
			solAssert(findSrc, "Can't find src file");
		}

		if (!m_mainContract.empty() && targetContract == nullptr) {
			m_errorReporter.typeError(
				1468_error,
				SourceLocation(),
				"Source file doesn't contain the desired contract \"" + m_mainContract + "\"."
			);
			return {false, didCompileSomething};
		}

		if (targetContract != nullptr) {
			try {
				if (json) {
					std::vector<PragmaDirective const *> pragmaDirectives = getPragmaDirectives(&source(m_inputFile));
					PragmaDirectiveHelper pragmaHelper{pragmaDirectives};
					Contract const& c = contract(targetContract->name());
					if (m_generateAbi) {
						Json::Value abi = TVMABI::generateABIJson(targetContract, getSourceUnits(), pragmaDirectives);
						c.abi = std::make_unique<Json::Value>(abi);
					}
					if (m_generateCode) {
						Pointer<solidity::frontend::Contract> codeContract =
							TVMContractCompiler::generateContractCode(targetContract, getSourceUnits(), pragmaHelper);
						std::ostringstream out;
						Printer p{out};
						codeContract->accept(p);
						Json::Value code = Json::Value(out.str());
						c.code = std::make_unique<Json::Value>(code);
					}
					if (m_doPrintFunctionIds)
					{
						auto functionIds = TVMABI::generateFunctionIdsJson(*c.contract, pragmaHelper);
						c.functionIds = std::make_unique<Json::Value>(functionIds);
					}
					if (m_doPrivateFunctionIds)
					{
						auto functionIds = TVMABI::generatePrivateFunctionIdsJson(*c.contract, getSourceUnits(), pragmaHelper);
						c.privateFunctionIds = std::make_unique<Json::Value>(functionIds);
					}
				} else {
					TVMCompilerProceedContract(
						*targetContract,
						getSourceUnits(),
						&targetPragmaDirectives,
						m_generateAbi,
						m_generateCode,
						m_inputFile,
						m_folder,
						m_file_prefix,
						m_doPrintFunctionIds,
						m_doPrivateFunctionIds
					);
				}
				didCompileSomething = true;
			} catch (FatalError const &) {
				return {false, didCompileSomething};
			}
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

std::vector<std::string> CompilerStack::contractNames() const
{
	if (m_stackState < Parsed)
		solThrow(CompilerError, "Parsing was not successful.");
	std::vector<std::string> contractNames;
	for (auto const& contract: m_contracts)
		contractNames.push_back(contract.first);
	return contractNames;
}

std::string const* CompilerStack::sourceMapping(std::string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		solThrow(CompilerError, "Compilation was not successful.");

	Contract const& c = contract(_contractName);
	return &*c.sourceMapping;
}

std::string const* CompilerStack::runtimeSourceMapping(std::string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		solThrow(CompilerError, "Compilation was not successful.");

	Contract const& c = contract(_contractName);
	return &*c.runtimeSourceMapping;
}

std::string const CompilerStack::filesystemFriendlyName(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "No compiled contracts found.");

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

std::string const& CompilerStack::yulIR(std::string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		solThrow(CompilerError, "Compilation was not successful.");

	return contract(_contractName).yulIR;
}

std::string const& CompilerStack::yulIROptimized(std::string const& _contractName) const
{
	if (m_stackState != CompilationSuccessful)
		solThrow(CompilerError, "Compilation was not successful.");

	return contract(_contractName).yulIROptimized;
}


std::vector<std::string> CompilerStack::sourceNames() const
{
	return ranges::to<std::vector>(m_sources | ranges::views::keys);
}

std::map<std::string, unsigned> CompilerStack::sourceIndices() const
{
	std::map<std::string, unsigned> indices;
	unsigned index = 0;
	for (auto const& s: m_sources)
		indices[s.first] = index++;
	return indices;
}

std::string CompilerStack::contractSource(std::string const& _contractName) const {
	std::string sourceName = _contractName;
	std::string::size_type pos = _contractName.rfind(':');
	if (pos != std::string::npos) {
		sourceName = _contractName.substr(0, pos);
	}
	return sourceName;
}

Json::Value const& CompilerStack::contractABI(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		BOOST_THROW_EXCEPTION(CompilerError() << errinfo_comment("Analysis was not successful."));

	auto const &abi = contract(_contractName).abi;
	return abi ? *abi : Json::Value::null;
}

Json::Value const& CompilerStack::contractCode(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	auto const &code = contract(_contractName).code;
	return code ? *code : Json::Value::null;
}

Json::Value const& CompilerStack::functionIds(std::string const& _contractName) const
{
	std::string sourceName = contractSource(_contractName);
	Contract const &c = contract(_contractName);
	return c.functionIds ? *c.functionIds : Json::Value::null;
}

Json::Value const& CompilerStack::privateFunctionIds(std::string const& _contractName) const
{
	std::string sourceName = contractSource(_contractName);
	Contract const &c = contract(_contractName);
	return c.privateFunctionIds ? *c.privateFunctionIds : Json::Value::null;
}

Json::Value const& CompilerStack::natspecUser(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	return natspecUser(contract(_contractName));
}

Json::Value const& CompilerStack::natspecUser(Contract const& _contract) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	solAssert(_contract.contract, "");
	solUnimplementedAssert(!isExperimentalSolidity());

	return _contract.userDocumentation.init([&]{ return Natspec::userDocumentation(*_contract.contract); });
}

Json::Value const& CompilerStack::natspecDev(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	return natspecDev(contract(_contractName));
}

Json::Value const& CompilerStack::natspecDev(Contract const& _contract) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	solAssert(_contract.contract, "");

	solUnimplementedAssert(!isExperimentalSolidity());

	return _contract.devDocumentation.init([&]{ return Natspec::devDocumentation(*_contract.contract); });
}

Json::Value CompilerStack::interfaceSymbols(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	solUnimplementedAssert(!isExperimentalSolidity());

	Json::Value interfaceSymbols(Json::objectValue);
	// Always have a methods object
	interfaceSymbols["methods"] = Json::objectValue;

	for (auto const& it: contractDefinition(_contractName).interfaceFunctions())
		interfaceSymbols["methods"][it.second->externalSignature()] = it.first.hex();
	for (ErrorDefinition const* error: contractDefinition(_contractName).interfaceErrors())
	{
		std::string signature = error->functionType(true)->externalSignature();
		interfaceSymbols["errors"][signature] = util::toHex(toCompactBigEndian(util::selectorFromSignatureU32(signature), 4));
	}

	for (EventDefinition const* event: ranges::concat_view(
		contractDefinition(_contractName).definedInterfaceEvents(),
		contractDefinition(_contractName).usedInterfaceEvents()
	))
		if (!event->isAnonymous())
		{
			std::string signature = event->functionType(true)->externalSignature();
			interfaceSymbols["events"][signature] = toHex(u256(h256::Arith(util::keccak256(signature))));
		}

	return interfaceSymbols;
}

bytes CompilerStack::cborMetadata(std::string const& _contractName, bool _forIR) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	return createCBORMetadata(contract(_contractName), _forIR);
}

std::string const& CompilerStack::metadata(Contract const& _contract) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	solAssert(_contract.contract, "");

	solUnimplementedAssert(!isExperimentalSolidity());

	return _contract.metadata.init([&]{ return createMetadata(_contract, m_viaIR); });
}

CharStream const& CompilerStack::charStream(std::string const& _sourceName) const
{
	if (m_stackState < SourcesSet)
		solThrow(CompilerError, "No sources set.");

	solAssert(source(_sourceName).charStream, "");

	return *source(_sourceName).charStream;
}

SourceUnit const& CompilerStack::ast(std::string const& _sourceName) const
{
	if (m_stackState < Parsed)
		solThrow(CompilerError, "Parsing not yet performed.");
	if (!source(_sourceName).ast)
		solThrow(CompilerError, "Parsing was not successful.");

	solUnimplementedAssert(!isExperimentalSolidity());

	return *source(_sourceName).ast;
}

ContractDefinition const& CompilerStack::contractDefinition(std::string const& _contractName) const
{
	if (m_stackState < AnalysisSuccessful)
		solThrow(CompilerError, "Analysis was not successful.");

	return *contract(_contractName).contract;
}

size_t CompilerStack::functionEntryPoint(
	std::string const& _contractName,
	FunctionDefinition const& /*_function*/
) const
{
	if (m_stackState != CompilationSuccessful)
		solThrow(CompilerError, "Compilation was not successful.");

	std::shared_ptr<Compiler> const& compiler = contract(_contractName).compiler;
	if (!compiler)
		return 0;
	return 0;
}

h256 const& CompilerStack::Source::keccak256() const
{
	if (keccak256HashCached == h256{})
		keccak256HashCached = util::keccak256(charStream->source());
	return keccak256HashCached;
}

h256 const& CompilerStack::Source::swarmHash() const
{
	if (swarmHashCached == h256{})
		swarmHashCached = util::bzzr1Hash(charStream->source());
	return swarmHashCached;
}

std::string const& CompilerStack::Source::ipfsUrl() const
{
	if (ipfsUrlCached.empty())
		ipfsUrlCached = "dweb:/ipfs/" + util::ipfsHashBase58(charStream->source());
	return ipfsUrlCached;
}

StringMap CompilerStack::loadMissingSources(SourceUnit const& _ast)
{
	solAssert(m_stackState < ParsedAndImported, "");
	StringMap newSources;
	try
	{
		for (auto const& node: _ast.nodes())
			if (ImportDirective const* import = dynamic_cast<ImportDirective*>(node.get()))
			{
				std::string const& importPath = *import->annotation().absolutePath;

				if (m_sources.count(importPath) || newSources.count(importPath))
					continue;

				ReadCallback::Result result{false, std::string("File not supplied initially.")};
				if (m_readFile)
					result = m_readFile(ReadCallback::kindString(ReadCallback::Kind::ReadFile), importPath);

				if (result.success)
					newSources[importPath] = result.responseOrErrorMessage;
				else
				{
					m_errorReporter.parserError(
						6275_error,
						import->location(),
						std::string("Source \"" + importPath + "\" not found: " + result.responseOrErrorMessage)
					);
					continue;
				}
			}
	}
	catch (FatalError const&)
	{
		solAssert(m_errorReporter.hasErrors(), "");
	}
	return newSources;
}

std::string CompilerStack::applyRemapping(std::string const& _path, std::string const& _context)
{
	solAssert(m_stackState < ParsedAndImported, "");
	return m_importRemapper.apply(_path, _context);
}

bool CompilerStack::resolveImports()
{
	solAssert(m_stackState == ParsedAndImported, "");

	// topological sorting (depth first search) of the import graph, cutting potential cycles
	std::vector<Source const*> sourceOrder;
	std::set<Source const*> sourcesSeen;

	std::function<void(Source const*)> toposort = [&](Source const* _source)
	{
		if (sourcesSeen.count(_source))
			return;
		sourcesSeen.insert(_source);
		solAssert(_source->ast);
		for (ASTPointer<ASTNode> const& node: _source->ast->nodes())
			if (ImportDirective const* import = dynamic_cast<ImportDirective*>(node.get()))
			{
				std::string const& path = *import->annotation().absolutePath;
				solAssert(m_sources.count(path), "");
				import->annotation().sourceUnit = m_sources[path].ast.get();
				toposort(&m_sources[path]);
			}
		sourceOrder.push_back(_source);
	};

	std::vector<PragmaDirective const*> experimentalPragmaDirectives;
	for (auto const& sourcePair: m_sources)
	{
		if (isRequestedSource(sourcePair.first))
			toposort(&sourcePair.second);
		if (sourcePair.second.ast && sourcePair.second.ast->experimentalSolidity())
			for (ASTPointer<ASTNode> const& node: sourcePair.second.ast->nodes())
				if (PragmaDirective const* pragma = dynamic_cast<PragmaDirective*>(node.get()))
					if (pragma->literals().size() >=2 && pragma->literals()[0] == "experimental" && pragma->literals()[1] == "solidity")
					{
						experimentalPragmaDirectives.push_back(pragma);
						break;
					}
	}

	if (!experimentalPragmaDirectives.empty() && experimentalPragmaDirectives.size() != m_sources.size())
	{
		for (auto &&pragma: experimentalPragmaDirectives)
			m_errorReporter.parserError(
					2141_error,
					pragma->location(),
					"File declares \"pragma experimental solidity\". If you want to enable the experimental mode, all source units must include the pragma."
			);
		return false;
	}

	swap(m_sourceOrder, sourceOrder);
	return true;
}

void CompilerStack::storeContractDefinitions()
{
	for (auto const& pair: m_sources)
		if (pair.second.ast)
			for (
				ContractDefinition const* contract:
				ASTNode::filteredNodes<ContractDefinition>(pair.second.ast->nodes())
			)
			{
				std::string fullyQualifiedName = *pair.second.ast->annotation().path + ":" + contract->name();
				// Note that we now reference contracts by their fully qualified names, and
				// thus contracts can only conflict if declared in the same source file. This
				// should already cause a double-declaration error elsewhere.
				if (!m_contracts.count(fullyQualifiedName))
					m_contracts[fullyQualifiedName].contract = contract;
			}
}

void CompilerStack::annotateInternalFunctionIDs()
{
	for (Source const* source: m_sourceOrder)
	{
		if (!source->ast)
			continue;

		for (ContractDefinition const* contract: ASTNode::filteredNodes<ContractDefinition>(source->ast->nodes()))
		{
			uint64_t internalFunctionID = 1;
			ContractDefinitionAnnotation& annotation = contract->annotation();

			if (auto const* deployTimeInternalDispatch = util::valueOrNullptr((*annotation.deployedCallGraph)->edges, CallGraph::SpecialNode::InternalDispatch))
				for (auto const& node: *deployTimeInternalDispatch)
					if (auto const* callable = std::get_if<CallableDeclaration const*>(&node))
						if (auto const* function = dynamic_cast<FunctionDefinition const*>(*callable))
						{
							solAssert(contract->annotation().internalFunctionIDs.count(function) == 0);
							contract->annotation().internalFunctionIDs[function] = internalFunctionID++;
						}
			if (auto const* creationTimeInternalDispatch = util::valueOrNullptr((*annotation.creationCallGraph)->edges, CallGraph::SpecialNode::InternalDispatch))
				for (auto const& node: *creationTimeInternalDispatch)
					if (auto const* callable = std::get_if<CallableDeclaration const*>(&node))
						if (auto const* function = dynamic_cast<FunctionDefinition const*>(*callable))
							// Make sure the function already got an ID since it also occurs in the deploy-time internal dispatch.
							solAssert(contract->annotation().internalFunctionIDs.count(function) != 0);
		}
	}
}

namespace
{
bool onlySafeExperimentalFeaturesActivated(std::set<ExperimentalFeature> const& features)
{
	for (auto const feature: features)
		if (!ExperimentalFeatureWithoutWarning.count(feature))
			return false;
	return true;
}
}

CompilerStack::Contract const& CompilerStack::contract(std::string const& _contractName) const
{
	solAssert(m_stackState >= AnalysisSuccessful, "");

	auto it = m_contracts.find(_contractName);
	if (it != m_contracts.end())
		return it->second;

	// To provide a measure of backward-compatibility, if a contract is not located by its
	// fully-qualified name, a lookup will be attempted purely on the contract's name to see
	// if anything will satisfy.
	if (_contractName.find(':') == std::string::npos)
	{
		for (auto const& contractEntry: m_contracts)
		{
			std::stringstream ss;
			ss.str(contractEntry.first);
			// All entries are <source>:<contract>
			std::string source;
			std::string foundName;
			getline(ss, source, ':');
			getline(ss, foundName, ':');
			if (foundName == _contractName)
				return contractEntry.second;
		}
	}

	// If we get here, both lookup methods failed.
	solThrow(CompilerError, "Contract \"" + _contractName + "\" not found.");
}

CompilerStack::Source const& CompilerStack::source(std::string const& _sourceName) const
{
	auto it = m_sources.find(_sourceName);
	if (it == m_sources.end())
		solThrow(CompilerError, "Given source file not found: " + _sourceName);

	return it->second;
}

std::string CompilerStack::createMetadata(Contract const& _contract, bool _forIR) const
{
	Json::Value meta{Json::objectValue};
	meta["version"] = 1;
	std::string sourceType;
	switch (m_compilationSourceType)
	{
	case CompilationSourceType::Solidity:
		sourceType = "Solidity";
		break;
	case CompilationSourceType::SolidityAST:
		sourceType = "SolidityAST";
		break;
	}
	meta["language"] = sourceType;
	meta["compiler"]["version"] = VersionStringStrict;

	/// All the source files (including self), which should be included in the metadata.
	std::set<std::string> referencedSources;
	referencedSources.insert(*_contract.contract->sourceUnit().annotation().path);
	for (auto const sourceUnit: _contract.contract->sourceUnit().referencedSourceUnits(true))
		referencedSources.insert(*sourceUnit->annotation().path);

	meta["sources"] = Json::objectValue;
	for (auto const& s: m_sources)
	{
		if (!referencedSources.count(s.first))
			continue;

		solAssert(s.second.charStream, "Character stream not available");
		meta["sources"][s.first]["keccak256"] = "0x" + util::toHex(s.second.keccak256().asBytes());
		if (std::optional<std::string> licenseString = s.second.ast->licenseString())
			meta["sources"][s.first]["license"] = *licenseString;
		if (m_metadataLiteralSources)
			meta["sources"][s.first]["content"] = s.second.charStream->source();
		else
		{
			meta["sources"][s.first]["urls"] = Json::arrayValue;
			meta["sources"][s.first]["urls"].append("bzz-raw://" + util::toHex(s.second.swarmHash().asBytes()));
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
		details["inliner"] = m_optimiserSettings.runInliner;
		details["jumpdestRemover"] = m_optimiserSettings.runJumpdestRemover;
		details["peephole"] = m_optimiserSettings.runPeephole;
		details["deduplicate"] = m_optimiserSettings.runDeduplicate;
		details["cse"] = m_optimiserSettings.runCSE;
		details["constantOptimizer"] = m_optimiserSettings.runConstantOptimiser;
		details["simpleCounterForLoopUncheckedIncrement"] = m_optimiserSettings.simpleCounterForLoopUncheckedIncrement;
		details["yul"] = m_optimiserSettings.runYulOptimiser;
		if (m_optimiserSettings.runYulOptimiser)
		{
			details["yulDetails"] = Json::objectValue;
			details["yulDetails"]["stackAllocation"] = m_optimiserSettings.optimizeStackAllocation;
			details["yulDetails"]["optimizerSteps"] = m_optimiserSettings.yulOptimiserSteps + ":" + m_optimiserSettings.yulOptimiserCleanupSteps;
		}
		else
		{
			solAssert(m_optimiserSettings.optimizeStackAllocation == false);
			solAssert(m_optimiserSettings.yulOptimiserSteps == OptimiserSettings::DefaultYulOptimiserSteps);
			solAssert(m_optimiserSettings.yulOptimiserCleanupSteps == OptimiserSettings::DefaultYulOptimiserCleanupSteps);
		}

		meta["settings"]["optimizer"]["details"] = std::move(details);
	}

	if (m_revertStrings != RevertStrings::Default)
		meta["settings"]["debug"]["revertStrings"] = revertStringsToString(m_revertStrings);

	if (m_metadataFormat == MetadataFormat::NoMetadata)
		meta["settings"]["metadata"]["appendCBOR"] = false;

	if (m_metadataLiteralSources)
		meta["settings"]["metadata"]["useLiteralContent"] = true;

	static std::vector<std::string> hashes{"ipfs", "bzzr1", "none"};
	meta["settings"]["metadata"]["bytecodeHash"] = hashes.at(unsigned(m_metadataHash));

	if (_forIR)
		meta["settings"]["viaIR"] = _forIR;
	meta["settings"]["evmVersion"] = m_evmVersion.name();
	meta["settings"]["compilationTarget"][_contract.contract->sourceUnitName()] =
		*_contract.contract->annotation().canonicalName;

	meta["settings"]["remappings"] = Json::arrayValue;
	std::set<std::string> remappings;
	for (auto const& r: m_importRemapper.remappings())
		remappings.insert(r.context + ":" + r.prefix + "=" + r.target);
	for (auto const& r: remappings)
		meta["settings"]["remappings"].append(r);

	meta["settings"]["libraries"] = Json::objectValue;
	for (auto const& library: m_libraries)
		meta["settings"]["libraries"][library.first] = "0x" + util::toHex(library.second.asBytes());

	// TODO DELETE REVERT THIS?
	// meta["output"]["abi"] = contractABI(_contract);
	meta["output"]["userdoc"] = natspecUser(_contract);
	meta["output"]["devdoc"] = natspecDev(_contract);

	return util::jsonCompactPrint(meta);
}

class MetadataCBOREncoder
{
public:
	void pushBytes(std::string const& key, bytes const& value)
	{
		m_entryCount++;
		pushTextString(key);
		pushByteString(value);
	}

	void pushString(std::string const& key, std::string const& value)
	{
		m_entryCount++;
		pushTextString(key);
		pushTextString(value);
	}

	void pushBool(std::string const& key, bool value)
	{
		m_entryCount++;
		pushTextString(key);
		pushBool(value);
	}

	bytes serialise() const
	{
		size_t size = m_data.size() + 1;
		solAssert(size <= 0xffff, "Metadata too large.");
		solAssert(m_entryCount <= 0x1f, "Too many map entries.");

		// CBOR fixed-length map
		bytes ret{static_cast<unsigned char>(0xa0 + m_entryCount)};
		// The already encoded key-value pairs
		ret += m_data;
		// 16-bit big endian length
		ret += toCompactBigEndian(size, 2);
		return ret;
	}

private:
	void pushTextString(std::string const& key)
	{
		size_t length = key.size();
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
			solAssert(false, "Text std::string too large.");
	}
	void pushByteString(bytes const& key)
	{
		size_t length = key.size();
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
			solAssert(false, "Byte std::string too large.");
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

bytes CompilerStack::createCBORMetadata(Contract const& _contract, bool _forIR) const
{
	if (m_metadataFormat == MetadataFormat::NoMetadata)
		return bytes{};

	bool const experimentalMode = !onlySafeExperimentalFeaturesActivated(
		_contract.contract->sourceUnit().annotation().experimentalFeatures
	);

	std::string meta = (_forIR == m_viaIR ? metadata(_contract) : createMetadata(_contract, _forIR));

	MetadataCBOREncoder encoder;

	if (m_metadataHash == MetadataHash::IPFS)
		encoder.pushBytes("ipfs", util::ipfsHash(meta));
	else if (m_metadataHash == MetadataHash::Bzzr1)
		encoder.pushBytes("bzzr1", util::bzzr1Hash(meta).asBytes());
	else
		solAssert(m_metadataHash == MetadataHash::None, "Invalid metadata hash");

	if (experimentalMode)
		encoder.pushBool("experimental", true);
	if (m_metadataFormat == MetadataFormat::WithReleaseVersionTag)
		encoder.pushBytes("solc", VersionCompactBytes);
	else
	{
		solAssert(
			m_metadataFormat == MetadataFormat::WithPrereleaseVersionTag,
			"Invalid metadata format."
		);
		encoder.pushString("solc", VersionStringStrict);
	}
	return encoder.serialise();
}


std::vector<PragmaDirective const *> CompilerStack::getPragmaDirectives(Source const* source) const {
	std::vector<PragmaDirective const *> pragmaDirectives;
	for (ASTPointer<ASTNode> const &node: source->ast->nodes())
		if (auto pragma = dynamic_cast<PragmaDirective const *>(node.get()))
			pragmaDirectives.push_back(pragma);
	return pragmaDirectives;
}

std::vector<std::shared_ptr<SourceUnit>> CompilerStack::getSourceUnits() const {
	std::vector<std::shared_ptr<SourceUnit>> sourceUnits;
	for (Source const *source: m_sourceOrder) {
		sourceUnits.push_back(source->ast);
	}
	return sourceUnits;
}

bool CompilerStack::isExperimentalSolidity() const
{
	return
		!m_sourceOrder.empty() &&
		ranges::all_of(m_sourceOrder, [](auto const* _source) { return _source->ast->experimentalSolidity(); } )
	;
}

experimental::Analysis const& CompilerStack::experimentalAnalysis() const
{
	solAssert(!!m_experimentalAnalysis);
	return *m_experimentalAnalysis;
}
