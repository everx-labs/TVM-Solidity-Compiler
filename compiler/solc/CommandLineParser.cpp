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

#include <solc/CommandLineParser.h>

#include <solc/Exceptions.h>

#include <liblangutil/EVMVersion.h>
#include <liblangutil/TVMVersion.h>

#include <boost/algorithm/string.hpp>

#include <range/v3/view/transform.hpp>
#include <range/v3/view/filter.hpp>
#include <range/v3/range/conversion.hpp>

using namespace std;
using namespace solidity::langutil;

namespace po = boost::program_options;

namespace solidity::frontend
{

static string const g_strAllowPaths = "allow-paths";
static string const g_strBasePath = "base-path";
static string const g_strIncludePath = "include-path";
static string const g_strAssemble = "assemble";
static string const g_strCombinedJson = "combined-json";
static string const g_strErrorRecovery = "error-recovery";
static string const g_strEVM = "evm";
static string const g_strEVMVersion = "evm-version";
static string const g_strEwasm = "ewasm";
static string const g_strViaIR = "via-ir";
static string const g_strExperimentalViaIR = "experimental-via-ir";
static string const g_strGas = "gas";
static string const g_strHelp = "help";
static string const g_strImportAst = "import-ast";
static string const g_strInputFile = "input-file";
static string const g_strYul = "yul";
static string const g_strYulDialect = "yul-dialect";
static string const g_strIPFS = "ipfs";
static string const g_strLicense = "license";
static string const g_strLibraries = "libraries";
static string const g_strLink = "link";
static string const g_strLSP = "lsp";
static string const g_strMachine = "machine";
static string const g_strMetadataHash = "metadata-hash";
static string const g_strMetadataLiteral = "metadata-literal";
static string const g_strModelCheckerContracts = "model-checker-contracts";
static string const g_strModelCheckerDivModNoSlacks = "model-checker-div-mod-no-slacks";
static string const g_strModelCheckerEngine = "model-checker-engine";
static string const g_strModelCheckerInvariants = "model-checker-invariants";
static string const g_strModelCheckerShowUnproved = "model-checker-show-unproved";
static string const g_strModelCheckerSolvers = "model-checker-solvers";
static string const g_strModelCheckerTargets = "model-checker-targets";
static string const g_strModelCheckerTimeout = "model-checker-timeout";
static string const g_strNone = "none";
static string const g_strNoOptimizeYul = "no-optimize-yul";
static string const g_strOptimize = "optimize";
static string const g_strOptimizeYul = "optimize-yul";
static string const g_strYulOptimizations = "yul-optimizations";
static string const g_strOutputDir = "output-dir";
static string const g_strOverwrite = "overwrite";
static string const g_strRevertStrings = "revert-strings";
static string const g_strStopAfter = "stop-after";
static string const g_strParsing = "parsing";


static string const g_strContract = "contract";
static string const g_strOutputPrefix = "output-prefix";
static string const g_strAsm = "asm";
static string const g_strABI = "abi-json";
static string const g_strFunctionIds = "function-ids";
static string const g_strPrivateFunctionIds = "private-function-ids";
static string const g_strTVMVersion = "tvm-version";


/// Possible arguments to for --revert-strings
static set<string> const g_revertStringsArgs
{
	revertStringsToString(RevertStrings::Default),
	revertStringsToString(RevertStrings::Strip),
	revertStringsToString(RevertStrings::Debug),
	revertStringsToString(RevertStrings::VerboseDebug)
};

static string const g_strSources = "sources";
static string const g_strSourceList = "sourceList";
static string const g_strStandardJSON = "standard-json";
static string const g_strStrictAssembly = "strict-assembly";
static string const g_strSwarm = "swarm";
static string const g_strPrettyJson = "pretty-json";
static string const g_strJsonIndent = "json-indent";
static string const g_strVersion = "version";
static string const g_strIgnoreMissingFiles = "ignore-missing";
static string const g_strColor = "color";
static string const g_strNoColor = "no-color";
static string const g_strErrorIds = "error-codes";

/// Possible arguments to for --machine
static set<string> const g_machineArgs
{
	g_strEVM,
	g_strEwasm
};

/// Possible arguments to for --yul-dialect
static set<string> const g_yulDialectArgs
{
	g_strEVM,
	g_strEwasm
};

/// Possible arguments to for --metadata-hash
static set<string> const g_metadataHashArgs
{
	g_strIPFS,
	g_strSwarm,
	g_strNone
};

static map<InputMode, string> const g_inputModeName = {
	{InputMode::Help, "help"},
	{InputMode::License, "license"},
	{InputMode::Version, "version"},
	{InputMode::Compiler, "compiler"},
	{InputMode::CompilerWithASTImport, "compiler (AST import)"},
	{InputMode::Assembler, "assembler"},
	{InputMode::StandardJson, "standard JSON"},
	{InputMode::Linker, "linker"},
	{InputMode::LanguageServer, "language server (LSP)"},
};

void CommandLineParser::checkMutuallyExclusive(vector<string> const& _optionNames)
{
	if (countEnabledOptions(_optionNames) > 1)
	{
		solThrow(
			CommandLineValidationError,
			"The following options are mutually exclusive: " + joinOptionNames(_optionNames) + ". " +
			"Select at most one."
		);
	}
}

bool CompilerOutputs::operator==(CompilerOutputs const& _other) const noexcept
{
	for (bool CompilerOutputs::* member: componentMap() | ranges::views::values)
		if (this->*member != _other.*member)
			return false;
	return true;
}

ostream& operator<<(ostream& _out, CompilerOutputs const& _selection)
{
	vector<string> serializedSelection;
	for (auto&& [componentName, component]: CompilerOutputs::componentMap())
		if (_selection.*component)
			serializedSelection.push_back(CompilerOutputs::componentName(component));

	return _out << util::joinHumanReadable(serializedSelection, ",");
}

string const& CompilerOutputs::componentName(bool CompilerOutputs::* _component)
{
	solAssert(_component, "");

	// NOTE: Linear search is not optimal but it's simpler than getting pointers-to-members to work as map keys.
	for (auto const& [componentName, component]: CompilerOutputs::componentMap())
		if (component == _component)
			return componentName;

	solAssert(false, "");
}

bool CombinedJsonRequests::operator==(CombinedJsonRequests const& _other) const noexcept
{
	for (bool CombinedJsonRequests::* member: componentMap() | ranges::views::values)
		if (this->*member != _other.*member)
			return false;
	return true;
}


ostream& operator<<(ostream& _out, CombinedJsonRequests const& _requests)
{
	vector<string> serializedRequests;
	for (auto&& [componentName, component]: CombinedJsonRequests::componentMap())
		if (_requests.*component)
			serializedRequests.push_back(CombinedJsonRequests::componentName(component));

	return _out << util::joinHumanReadable(serializedRequests, ",");
}

string const& CombinedJsonRequests::componentName(bool CombinedJsonRequests::* _component)
{
	solAssert(_component, "");

	for (auto const& [componentName, component]: CombinedJsonRequests::componentMap())
		if (component == _component)
			return componentName;

	solAssert(false, "");
}

bool CommandLineOptions::operator==(CommandLineOptions const& _other) const noexcept
{
	return
		input.paths == _other.input.paths &&
		input.remappings == _other.input.remappings &&
		input.addStdin == _other.input.addStdin &&
		input.basePath == _other.input.basePath &&
		input.includePaths == _other.input.includePaths &&
		input.allowedDirectories == _other.input.allowedDirectories &&
		input.ignoreMissingFiles == _other.input.ignoreMissingFiles &&
		input.errorRecovery == _other.input.errorRecovery &&
		output.dir == _other.output.dir &&
		output.overwriteFiles == _other.output.overwriteFiles &&
		output.evmVersion == _other.output.evmVersion &&
		output.viaIR == _other.output.viaIR &&
		output.revertStrings == _other.output.revertStrings &&
		output.debugInfoSelection == _other.output.debugInfoSelection &&
		output.stopAfter == _other.output.stopAfter &&
		input.mode == _other.input.mode &&
		linker.libraries == _other.linker.libraries &&
		formatting.json == _other.formatting.json &&
		formatting.coloredOutput == _other.formatting.coloredOutput &&
		formatting.withErrorIds == _other.formatting.withErrorIds &&
		compiler.outputs == _other.compiler.outputs &&
		compiler.estimateGas == _other.compiler.estimateGas &&
		compiler.combinedJsonRequests == _other.compiler.combinedJsonRequests &&
		metadata.hash == _other.metadata.hash &&
		metadata.literalSources == _other.metadata.literalSources &&
		optimizer.enabled == _other.optimizer.enabled &&
		optimizer.expectedExecutionsPerDeployment == _other.optimizer.expectedExecutionsPerDeployment &&
		optimizer.noOptimizeYul == _other.optimizer.noOptimizeYul &&
		optimizer.yulSteps == _other.optimizer.yulSteps &&
		modelChecker.initialize == _other.modelChecker.initialize;
}

OptimiserSettings CommandLineOptions::optimiserSettings() const
{
	OptimiserSettings settings;

	if (optimizer.enabled)
		settings = OptimiserSettings::standard();
	else
		settings = OptimiserSettings::minimal();

	if (optimizer.noOptimizeYul)
		settings.runYulOptimiser = false;

	if (optimizer.expectedExecutionsPerDeployment.has_value())
		settings.expectedExecutionsPerDeployment = optimizer.expectedExecutionsPerDeployment.value();

	if (optimizer.yulSteps.has_value())
		settings.yulOptimiserSteps = optimizer.yulSteps.value();

	return settings;
}

void CommandLineParser::parse(int _argc, char const* const* _argv)
{
	parseArgs(_argc, _argv);
	processArgs();
}

void CommandLineParser::parseInputPathsAndRemappings()
{
	m_options.input.ignoreMissingFiles = (m_args.count(g_strIgnoreMissingFiles) > 0);

	if (m_args.count(g_strInputFile))
		for (string const& positionalArg: m_args[g_strInputFile].as<vector<string>>())
		{
			if (ImportRemapper::isRemapping(positionalArg))
			{
				optional<ImportRemapper::Remapping> remapping = ImportRemapper::parseRemapping(positionalArg);
				if (!remapping.has_value())
					solThrow(CommandLineValidationError, "Invalid remapping: \"" + positionalArg + "\".");

				if (m_options.input.mode == InputMode::StandardJson)
					solThrow(
						CommandLineValidationError,
						"Import remappings are not accepted on the command line in Standard JSON mode.\n"
						"Please put them under 'settings.remappings' in the JSON input."
					);

				if (!remapping->target.empty())
				{
					// If the target is a directory, whitelist it. Otherwise whitelist containing dir.
					// NOTE: /a/b/c/ is a directory while /a/b/c is not.
					boost::filesystem::path remappingDir = remapping->target;
					if (remappingDir.filename() != "..")
						// As an exception we'll treat /a/b/c/.. as a directory too. It would be
						// unintuitive to whitelist /a/b/c when the target is equivalent to /a/b/.
						remappingDir.remove_filename();
					m_options.input.allowedDirectories.insert(remappingDir.empty() ? "." : remappingDir);
				}

				m_options.input.remappings.emplace_back(std::move(remapping.value()));
			}
			else if (positionalArg == "-")
				m_options.input.addStdin = true;
			else {
				if (m_options.input.paths.empty())
					m_options.input.paths.insert(positionalArg);
				else
					solThrow(
						CommandLineValidationError,
						"Please specify a single file name for compiling on the command line."
					);
			}
		}

	if (m_options.input.mode == InputMode::StandardJson)
	{
		if (m_options.input.paths.size() > 1 || (m_options.input.paths.size() == 1 && m_options.input.addStdin))
			solThrow(
				CommandLineValidationError,
				"Too many input files for --" + g_strStandardJSON + ".\n"
				"Please either specify a single file name or provide its content on standard input."
			);
		else if (m_options.input.paths.size() == 0)
			// Standard JSON mode input used to be handled separately and zero files meant "read from stdin".
			// Keep it working that way for backwards-compatibility.
			m_options.input.addStdin = true;
	}
	else if (m_options.input.paths.size() == 0 && !m_options.input.addStdin)
		solThrow(
			CommandLineValidationError,
			"No input files given. If you wish to use the standard input please specify \"-\" explicitly."
		);
}

void CommandLineParser::parseLibraryOption(string const& _input)
{
	namespace fs = boost::filesystem;
	string data = _input;
	try
	{
		if (fs::is_regular_file(_input))
			data = util::readFileAsString(_input);
	}
	catch (fs::filesystem_error const&)
	{
		// Thrown e.g. if path is too long.
	}
	catch (util::FileNotFound const&)
	{
		// Should not happen if `fs::is_regular_file` is correct.
	}
	catch (util::NotAFile const&)
	{
		// Should not happen if `fs::is_regular_file` is correct.
	}

	vector<string> libraries;
	boost::split(libraries, data, boost::is_space() || boost::is_any_of(","), boost::token_compress_on);
	for (string const& lib: libraries)
		if (!lib.empty())
		{
			//search for equal sign or last colon in string as our binaries output placeholders in the form of file=Name or file:Name
			//so we need to search for `=` or `:` in the string
			auto separator = lib.rfind('=');
			bool isSeparatorEqualSign = true;
			if (separator == string::npos)
			{
				separator = lib.rfind(':');
				if (separator == string::npos)
					solThrow(
						CommandLineValidationError,
						"Equal sign separator missing in library address specifier \"" + lib + "\""
					);
				else
					isSeparatorEqualSign = false; // separator is colon
			}
			else
				if (lib.rfind('=') != lib.find('='))
					solThrow(
						CommandLineValidationError,
						"Only one equal sign \"=\" is allowed in the address string \"" + lib + "\"."
					);

			string libName(lib.begin(), lib.begin() + static_cast<ptrdiff_t>(separator));
			boost::trim(libName);
			if (m_options.linker.libraries.count(libName))
				solThrow(
					CommandLineValidationError,
					"Address specified more than once for library \"" + libName + "\"."
				);

			string addrString(lib.begin() + static_cast<ptrdiff_t>(separator) + 1, lib.end());
			boost::trim(addrString);
			if (addrString.empty())
				solThrow(
					CommandLineValidationError,
					"Empty address provided for library \"" + libName + "\".\n"
					"Note that there should not be any whitespace after the " +
					(isSeparatorEqualSign ? "equal sign" : "colon") + "."
				);

			if (addrString.substr(0, 2) == "0x")
				addrString = addrString.substr(2);
			else
				solThrow(
					CommandLineValidationError,
					"The address " + addrString + " is not prefixed with \"0x\".\n"
					"Note that the address must be prefixed with \"0x\"."
				);

			if (addrString.length() != 40)
				solThrow(
					CommandLineValidationError,
					"Invalid length for address for library \"" + libName + "\": " +
					to_string(addrString.length()) + " instead of 40 characters."
				);
			if (!util::passesAddressChecksum(addrString, false))
				solThrow(
					CommandLineValidationError,
					"Invalid checksum on address for library \"" + libName + "\": " + addrString + "\n"
					"The correct checksum is " + util::getChecksummedAddress(addrString)
				);
			bytes binAddr = util::fromHex(addrString);
			util::h160 address(binAddr, util::h160::AlignRight);
			if (binAddr.size() > 20 || address == util::h160())
				solThrow(
					CommandLineValidationError,
					"Invalid address for library \"" + libName + "\": " + addrString
				);
			m_options.linker.libraries[libName] = address;
		}
}

void CommandLineParser::parseOutputSelection()
{
	static auto outputSupported = [](InputMode _mode, string_view _outputName)
	{
		static set<string> const compilerModeOutputs = (
			CompilerOutputs::componentMap() |
			ranges::views::keys |
			ranges::to<set>()
		) - set<string>{CompilerOutputs::componentName(&CompilerOutputs::ewasmIR)};
		static set<string> const assemblerModeOutputs = {
			CompilerOutputs::componentName(&CompilerOutputs::binary),
			CompilerOutputs::componentName(&CompilerOutputs::irOptimized),
			CompilerOutputs::componentName(&CompilerOutputs::ewasm),
			CompilerOutputs::componentName(&CompilerOutputs::ewasmIR),
		};

		switch (_mode)
		{
		case InputMode::Help:
		case InputMode::License:
		case InputMode::Version:
		case InputMode::LanguageServer:
			solAssert(false);
		case InputMode::Compiler:
		case InputMode::CompilerWithASTImport:
			return util::contains(compilerModeOutputs, _outputName);
		case InputMode::Assembler:
			return util::contains(assemblerModeOutputs, _outputName);
		case InputMode::StandardJson:
		case InputMode::Linker:
			return false;
		}

		solAssert(false, "");
	};

	for (auto&& [optionName, outputComponent]: CompilerOutputs::componentMap())
		m_options.compiler.outputs.*outputComponent = (m_args.count(optionName) > 0);

	if (m_options.input.mode == InputMode::Assembler && m_options.compiler.outputs == CompilerOutputs{})
	{
		// In assembly mode keep the default outputs enabled for backwards-compatibility.
		// TODO: Remove this (must be done in a breaking release).
		m_options.compiler.outputs.binary = true;
		m_options.compiler.outputs.irOptimized = true;
		m_options.compiler.outputs.ewasm = true;
		m_options.compiler.outputs.ewasmIR = true;
	}

	vector<string> unsupportedOutputs;
	for (auto&& [optionName, outputComponent]: CompilerOutputs::componentMap())
		if (m_options.compiler.outputs.*outputComponent && !outputSupported(m_options.input.mode, optionName))
			unsupportedOutputs.push_back(optionName);

	if (!unsupportedOutputs.empty())
		solThrow(
			CommandLineValidationError,
			"The following outputs are not supported in " + g_inputModeName.at(m_options.input.mode) + " mode: " +
			joinOptionNames(unsupportedOutputs) + "."
		);
}

po::options_description CommandLineParser::optionsDescription()
{
	// Declare the supported options.
	po::options_description desc((R"(solc, the TVM Solidity commandline compiler.

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you
are welcome to redistribute it under certain conditions. See 'solc --)" + g_strLicense + R"('
for details.

Usage: solc [options] input_file
Compiles the given Solidity input file (or the standard input if none given or
"-" is used as a file name) and outputs the components specified in the options
at standard output or in files in the output directory, if specified.
Imports are automatically read from the filesystem, but it is also possible to
remap paths using the context:prefix=path syntax.
Example:
solc --asm --abi-json)" + R"( -o /tmp/solcoutput dapp=/usr/local/lib/dapp contract.sol

General Information)").c_str(),
		po::options_description::m_default_line_length,
		po::options_description::m_default_line_length - 23
	);
	desc.add_options()
		((g_strHelp + ",h").c_str(), "Show help message and exit.")
		((g_strVersion + ",v").c_str(), "Show version and exit.")
		(g_strLicense.c_str(), "Show licensing information and exit.")
	;

	po::options_description inputOptions("Input Options");
	inputOptions.add_options()
		(
			(g_strContract + ",c").c_str(),
			po::value<string>()->value_name("contractName"),
			"Contract to build if sources define more than one contract."
		)
		(
			g_strBasePath.c_str(),
			po::value<string>()->value_name("path"),
			"Use the given path as the root of the source tree instead of the root of the filesystem."
		)
		(
			(g_strIncludePath + ",i").c_str(),
			po::value<vector<string>>()->value_name("path"),
			"Make an additional source directory available to the default import callback. "
			"Use this option if you want to import contracts whose location is not fixed in relation "
			"to your main source tree, e.g. third-party libraries installed using a package manager. "
			"Can be used multiple times. "
			"Can only be used if base path has a non-empty value."
		)
		(
			g_strAllowPaths.c_str(),
			po::value<string>()->value_name("path(s)"),
			"Allow a given path for imports. A list of paths can be supplied by separating them with a comma."
		)
	;
	desc.add(inputOptions);

	po::options_description outputOptions("Output Options");
	outputOptions.add_options()
		(
			(g_strOutputPrefix + ",p").c_str(),
			po::value<string>()->value_name("prefix"),
			"Prefix for output files (by default, input file stem is used as prefix)"
		)
		(
			(g_strOutputDir + ",o").c_str(),
			po::value<string>()->value_name("path"),
			"Output directory (by default, current directory is used)"
		)
		(
			g_strTVMVersion.c_str(),
			po::value<string>()->value_name("version")->default_value(TVMVersion{}.name()),
			"Select desired TVM version. Either ever, ton, gosh."
		)
	;
	desc.add(outputOptions);

	po::options_description outputFormatting("Output Formatting");
	outputFormatting.add_options()
		(
			g_strPrettyJson.c_str(),
			"Output JSON in pretty format."
		)
		(
			g_strJsonIndent.c_str(),
			po::value<uint32_t>()->value_name("N")->default_value(util::JsonFormat::defaultIndent),
			"Indent pretty-printed JSON with N spaces. Enables '--pretty-json' automatically."
		)
		(
			g_strColor.c_str(),
			"Force colored output."
		)
		(
			g_strNoColor.c_str(),
			"Explicitly disable colored output, disabling terminal auto-detection."
		)
		(
			g_strErrorIds.c_str(),
			"Output error codes."
		)
	;
	desc.add(outputFormatting);

	po::options_description outputComponents("Output Components");
	outputComponents.add_options()
		(g_strAsm.c_str(), "Assembly of the contracts")
		(g_strABI.c_str(), "ABI specification of the contracts")
		(g_strFunctionIds.c_str(), "Print name and id for each public function.")
		(g_strPrivateFunctionIds.c_str(), "Print name and id for each private function.")
		(CompilerOutputs::componentName(&CompilerOutputs::astCompactJson).c_str(), "AST of all source files in a compact JSON format.")
		(CompilerOutputs::componentName(&CompilerOutputs::natspecUser).c_str(), "Natspec user documentation of all contracts.")
		(CompilerOutputs::componentName(&CompilerOutputs::natspecDev).c_str(), "Natspec developer documentation of all contracts.")
	;
	desc.add(outputComponents);

	desc.add_options()(g_strInputFile.c_str(), po::value<vector<string>>(), "input file");
	return desc;
}

po::positional_options_description CommandLineParser::positionalOptionsDescription()
{
	// All positional options should be interpreted as input files
	po::positional_options_description filesPositions;
	filesPositions.add(g_strInputFile.c_str(), -1);
	return filesPositions;
}

void CommandLineParser::parseArgs(int _argc, char const* const* _argv)
{
	po::options_description allOptions = optionsDescription();
	po::positional_options_description filesPositions = positionalOptionsDescription();

	// parse the compiler arguments
	try
	{
		po::command_line_parser cmdLineParser(_argc, _argv);
		cmdLineParser.style(po::command_line_style::default_style & (~po::command_line_style::allow_guessing));
		cmdLineParser.options(allOptions).positional(filesPositions);
		po::store(cmdLineParser.run(), m_args);
	}
	catch (po::error const& _exception)
	{
		solThrow(CommandLineValidationError, _exception.what());
	}

	po::notify(m_args);
}

void CommandLineParser::processArgs()
{
	checkMutuallyExclusive({
		g_strHelp,
		g_strLicense,
		g_strVersion,
		g_strStandardJSON,
		g_strLink,
		g_strAssemble,
		g_strStrictAssembly,
		g_strYul,
		g_strImportAst,
		g_strLSP
	});

	if (m_args.count(g_strHelp) > 0)
		m_options.input.mode = InputMode::Help;
	else if (m_args.count(g_strLicense) > 0)
		m_options.input.mode = InputMode::License;
	else if (m_args.count(g_strVersion) > 0)
		m_options.input.mode = InputMode::Version;
	else if (m_args.count(g_strStandardJSON) > 0)
		m_options.input.mode = InputMode::StandardJson;
	else if (m_args.count(g_strLSP))
		m_options.input.mode = InputMode::LanguageServer;
	else if (m_args.count(g_strAssemble) > 0 || m_args.count(g_strStrictAssembly) > 0 || m_args.count(g_strYul) > 0)
		m_options.input.mode = InputMode::Assembler;
	else if (m_args.count(g_strLink) > 0)
		m_options.input.mode = InputMode::Linker;
	else if (m_args.count(g_strImportAst) > 0)
		m_options.input.mode = InputMode::CompilerWithASTImport;
	else
		m_options.input.mode = InputMode::Compiler;

	if (
		m_options.input.mode == InputMode::Help ||
		m_options.input.mode == InputMode::License ||
		m_options.input.mode == InputMode::Version
	)
		return;

	map<string, set<InputMode>> validOptionInputModeCombinations = {
		// TODO: This should eventually contain all options.
		{g_strErrorRecovery, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strExperimentalViaIR, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strViaIR, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strMetadataLiteral, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strMetadataHash, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerShowUnproved, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerDivModNoSlacks, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerEngine, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerInvariants, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerSolvers, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerTimeout, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerContracts, {InputMode::Compiler, InputMode::CompilerWithASTImport}},
		{g_strModelCheckerTargets, {InputMode::Compiler, InputMode::CompilerWithASTImport}}
	};
	vector<string> invalidOptionsForCurrentInputMode;
	for (auto const& [optionName, inputModes]: validOptionInputModeCombinations)
	{
		if (
			m_args.count(optionName) > 0 &&
			inputModes.count(m_options.input.mode) == 0 &&
			!m_args[optionName].defaulted()
		)
			invalidOptionsForCurrentInputMode.push_back(optionName);
	}

	if (!invalidOptionsForCurrentInputMode.empty())
		solThrow(
			CommandLineValidationError,
			"The following options are not supported in the current input mode: " +
			joinOptionNames(invalidOptionsForCurrentInputMode)
		);

	if (m_options.input.mode == InputMode::LanguageServer)
		return;

	checkMutuallyExclusive({g_strColor, g_strNoColor});

	array<string, 9> const conflictingWithStopAfter{
		CompilerOutputs::componentName(&CompilerOutputs::binary),
		CompilerOutputs::componentName(&CompilerOutputs::ir),
		CompilerOutputs::componentName(&CompilerOutputs::irOptimized),
		CompilerOutputs::componentName(&CompilerOutputs::ewasm),
		CompilerOutputs::componentName(&CompilerOutputs::ewasmIR),
		g_strGas,
		CompilerOutputs::componentName(&CompilerOutputs::asmJson),
		CompilerOutputs::componentName(&CompilerOutputs::opcodes),
	};

	for (auto& option: conflictingWithStopAfter)
		checkMutuallyExclusive({g_strStopAfter, option});

	if (
		m_options.input.mode != InputMode::Compiler &&
		m_options.input.mode != InputMode::CompilerWithASTImport &&
		m_options.input.mode != InputMode::Assembler
	)
	{
		for (string const& option: {g_strOptimize, g_strNoOptimizeYul, g_strOptimizeYul, g_strYulOptimizations})
			if (m_args.count(option) > 0)
				solThrow(
					CommandLineValidationError,
					"Option --" + option + " is only valid in compiler and assembler modes."
				);
	}

	if (m_args.count(g_strColor) > 0)
		m_options.formatting.coloredOutput = true;
	else if (m_args.count(g_strNoColor) > 0)
		m_options.formatting.coloredOutput = false;

	m_options.formatting.withErrorIds = m_args.count(g_strErrorIds);

	if (m_args.count(g_strRevertStrings))
	{
		string revertStringsString = m_args[g_strRevertStrings].as<string>();
		std::optional<RevertStrings> revertStrings = revertStringsFromString(revertStringsString);
		if (!revertStrings)
			solThrow(
				CommandLineValidationError,
				"Invalid option for --" + g_strRevertStrings + ": " + revertStringsString
			);
		if (*revertStrings == RevertStrings::VerboseDebug)
			solThrow(
				CommandLineValidationError,
				"Only \"default\", \"strip\" and \"debug\" are implemented for --" + g_strRevertStrings + " for now."
			);
		m_options.output.revertStrings = *revertStrings;
	}

	parseCombinedJsonOption();

	if (m_args.count(g_strOutputDir))
		m_options.output.dir = m_args.at(g_strOutputDir).as<string>();

	m_options.output.overwriteFiles = (m_args.count(g_strOverwrite) > 0);

	if (m_args.count(g_strPrettyJson) > 0)
	{
		m_options.formatting.json.format = util::JsonFormat::Pretty;
	}
	if (!m_args[g_strJsonIndent].defaulted())
	{
		m_options.formatting.json.format = util::JsonFormat::Pretty;
		m_options.formatting.json.indent = m_args[g_strJsonIndent].as<uint32_t>();
	}

	parseOutputSelection();

	m_options.compiler.estimateGas = (m_args.count(g_strGas) > 0);

	if (m_args.count(g_strBasePath))
		m_options.input.basePath = m_args[g_strBasePath].as<string>();

	if (m_args.count(g_strIncludePath) > 0)
	{
		if (m_options.input.basePath.empty())
			solThrow(CommandLineValidationError, "--" + g_strIncludePath + " option requires a non-empty base path.");

		for (string const& includePath: m_args[g_strIncludePath].as<vector<string>>())
		{
			if (includePath.empty())
				solThrow(CommandLineValidationError, "Empty values are not allowed in --" + g_strIncludePath + ".");

			m_options.input.includePaths.push_back(includePath);
		}
	}

	if (m_args.count(g_strAllowPaths))
	{
		vector<string> paths;
		for (string const& allowedPath: boost::split(paths, m_args[g_strAllowPaths].as<string>(), boost::is_any_of(",")))
			if (!allowedPath.empty())
				m_options.input.allowedDirectories.insert(allowedPath);
	}

	if (m_args.count(g_strStopAfter))
	{
		if (m_args[g_strStopAfter].as<string>() != "parsing")
			solThrow(CommandLineValidationError, "Valid options for --" + g_strStopAfter + " are: \"parsing\".\n");
		else
			m_options.output.stopAfter = CompilerStack::State::Parsed;
	}

	parseInputPathsAndRemappings();

	if (m_options.input.mode == InputMode::StandardJson)
		return;

	if (m_args.count(g_strLibraries))
		for (string const& library: m_args[g_strLibraries].as<vector<string>>())
			parseLibraryOption(library);

	if (m_options.input.mode == InputMode::Linker)
		return;

	if (m_args.count(g_strEVMVersion))
	{
		string versionOptionStr = m_args[g_strEVMVersion].as<string>();
		std::optional<langutil::EVMVersion> versionOption = langutil::EVMVersion::fromString(versionOptionStr);
		if (!versionOption)
			solThrow(CommandLineValidationError, "Invalid option for --" + g_strEVMVersion + ": " + versionOptionStr);
		m_options.output.evmVersion = *versionOption;
	}

	m_options.optimizer.enabled = (m_args.count(g_strOptimize) > 0);
	m_options.optimizer.noOptimizeYul = (m_args.count(g_strNoOptimizeYul) > 0);

	if (m_args.count(g_strYulOptimizations))
	{
		OptimiserSettings optimiserSettings = m_options.optimiserSettings();
		if (!optimiserSettings.runYulOptimiser)
			solThrow(CommandLineValidationError, "--" + g_strYulOptimizations + " is invalid if Yul optimizer is disabled");

		m_options.optimizer.yulSteps = m_args[g_strYulOptimizations].as<string>();
	}

	if (m_options.input.mode == InputMode::Assembler)
	{
		vector<string> const nonAssemblyModeOptions = {
			// TODO: The list is not complete. Add more.
			g_strOutputDir,
			g_strGas,
			g_strCombinedJson,
			g_strOptimizeYul,
			g_strNoOptimizeYul,
		};
		if (countEnabledOptions(nonAssemblyModeOptions) >= 1)
		{
			auto optionEnabled = [&](string const& name){ return m_args.count(name) > 0; };
			auto enabledOptions = nonAssemblyModeOptions | ranges::views::filter(optionEnabled) | ranges::to_vector;

			string message = "The following options are invalid in assembly mode: " + joinOptionNames(enabledOptions) + ".";
			if (m_args.count(g_strOptimizeYul) || m_args.count(g_strNoOptimizeYul))
				message += " Optimization is disabled by default and can be enabled with --" + g_strOptimize + ".";

			solThrow(CommandLineValidationError, message);
		}

		// switch to assembly mode
		return;
	}
	else if (countEnabledOptions({g_strYulDialect, g_strMachine}) >= 1)
		solThrow(
			CommandLineValidationError,
			"--" + g_strYulDialect + " and --" + g_strMachine + " are only valid in assembly mode."
		);

	if (m_args.count(g_strMetadataHash))
	{
		string hashStr = m_args[g_strMetadataHash].as<string>();
		if (hashStr == g_strIPFS)
			m_options.metadata.hash = CompilerStack::MetadataHash::IPFS;
		else if (hashStr == g_strSwarm)
			m_options.metadata.hash = CompilerStack::MetadataHash::Bzzr1;
		else if (hashStr == g_strNone)
			m_options.metadata.hash = CompilerStack::MetadataHash::None;
		else
			solThrow(CommandLineValidationError, "Invalid option for --" + g_strMetadataHash + ": " + hashStr);
	}

	if (m_args.count(g_strModelCheckerSolvers))
	{
		string solversStr = m_args[g_strModelCheckerSolvers].as<string>();
		optional<smtutil::SMTSolverChoice> solvers = smtutil::SMTSolverChoice::fromString(solversStr);
		if (!solvers)
			solThrow(CommandLineValidationError, "Invalid option for --" + g_strModelCheckerSolvers + ": " + solversStr);
	}

	m_options.metadata.literalSources = (m_args.count(g_strMetadataLiteral) > 0);
	m_options.modelChecker.initialize =
		m_args.count(g_strModelCheckerContracts) ||
		m_args.count(g_strModelCheckerDivModNoSlacks) ||
		m_args.count(g_strModelCheckerEngine) ||
		m_args.count(g_strModelCheckerInvariants) ||
		m_args.count(g_strModelCheckerShowUnproved) ||
		m_args.count(g_strModelCheckerSolvers) ||
		m_args.count(g_strModelCheckerTargets) ||
		m_args.count(g_strModelCheckerTimeout);
	m_options.output.viaIR = (m_args.count(g_strExperimentalViaIR) > 0 || m_args.count(g_strViaIR) > 0);
	if (m_options.input.mode == InputMode::Compiler)
		m_options.input.errorRecovery = (m_args.count(g_strErrorRecovery) > 0);

	solAssert(m_options.input.mode == InputMode::Compiler || m_options.input.mode == InputMode::CompilerWithASTImport);


	if (m_args.count(g_strTVMVersion))
	{
		string versionOptionStr = m_args[g_strTVMVersion].as<string>();
		std::optional<langutil::TVMVersion> versionOption = langutil::TVMVersion::fromString(versionOptionStr);
		if (!versionOption)
			solThrow(CommandLineValidationError, "Invalid option for --" + g_strTVMVersion + ": " + versionOptionStr);
		m_options.tvmParams.tvmVersion = *versionOption;
	}

	if (m_args.count(g_strContract))
		m_options.tvmParams.mainContract = m_args[g_strContract].as<string>();
	if (m_args.count(g_strOutputPrefix))
		m_options.tvmParams.fileNamePrefix = m_args[g_strOutputPrefix].as<string>();
	if (m_args.count(g_strABI))
		m_options.tvmParams.abi = true;
	if (m_args.count(g_strAsm))
		m_options.tvmParams.code = true;
	if (m_args.count(g_strFunctionIds))
		m_options.tvmParams.printFunctionIds = true;
	if (m_args.count(g_strPrivateFunctionIds))
		m_options.tvmParams.printPrivateFunctionIds = true;

	if (
		!m_options.tvmParams.code &&
		!m_options.tvmParams.abi &&
		!m_options.tvmParams.printFunctionIds &&
		!m_options.tvmParams.printPrivateFunctionIds &&
		m_args.count("ast-compact-json") == 0 &&
		m_args.count("userdoc") == 0 &&
		m_args.count("devdoc") == 0
	) {
		// no params at all
		m_options.tvmParams.code = true;
		m_options.tvmParams.abi = true;
	}
}

void CommandLineParser::parseCombinedJsonOption()
{
	if (!m_args.count(g_strCombinedJson))
		return;

	set<string> requests;
	for (string const& item: boost::split(requests, m_args[g_strCombinedJson].as<string>(), boost::is_any_of(",")))
		if (CombinedJsonRequests::componentMap().count(item) == 0)
			solThrow(CommandLineValidationError, "Invalid option to --" + g_strCombinedJson + ": " + item);

	m_options.compiler.combinedJsonRequests = CombinedJsonRequests{};
	for (auto&& [componentName, component]: CombinedJsonRequests::componentMap())
		m_options.compiler.combinedJsonRequests.value().*component = (requests.count(componentName) > 0);
}

size_t CommandLineParser::countEnabledOptions(vector<string> const& _optionNames) const
{
	size_t count = 0;
	for (string const& _option: _optionNames)
		count += m_args.count(_option);

	return count;
}

string CommandLineParser::joinOptionNames(vector<string> const& _optionNames, string _separator)
{
	return util::joinHumanReadable(
		_optionNames | ranges::views::transform([](string const& _option){ return "--" + _option; }),
		_separator
	);
}

} // namespace solidity::frontend
