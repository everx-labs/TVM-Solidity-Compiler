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
 * @author Lefteris <lefteris@ethdev.com>
 * @author Gav Wood <g@ethdev.com>
 * @date 2014
 * Solidity command line interface.
 */
#include <solc/CommandLineInterface.h>

#include <solc/Exceptions.h>

#include "license.h"
#include "solidity/BuildInfo.h"

#include <libsolidity/interface/Version.h>
#include <libsolidity/ast/ASTJsonExporter.h>
#include <libsolidity/ast/ASTJsonImporter.h>
#include <libsolidity/analysis/NameAndTypeResolver.h>
#include <libsolidity/interface/CompilerStack.h>
#include <libsolidity/interface/StandardCompiler.h>
#include <libsolidity/interface/DebugSettings.h>
#include <libsolidity/interface/ImportRemapper.h>
#include <libsolidity/interface/StorageLayout.h>
#include <libsolidity/lsp/LanguageServer.h>
#include <libsolidity/lsp/Transport.h>


#include <liblangutil/Exceptions.h>
#include <liblangutil/SourceReferenceFormatter.h>

#include <libsmtutil/Exceptions.h>

#include <libsolutil/Common.h>
#include <libsolutil/CommonData.h>
#include <libsolutil/CommonIO.h>
#include <libsolutil/JSON.h>

#include <algorithm>
#include <fstream>
#include <memory>

#include <range/v3/view/map.hpp>

#include <boost/filesystem.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/algorithm/string.hpp>

#ifdef _WIN32 // windows
	#include <io.h>
	#define isatty _isatty
	#define fileno _fileno
#else // unix
	#include <unistd.h>
#endif

#include <fstream>

#if !defined(STDERR_FILENO)
	#define STDERR_FILENO 2
#endif

using namespace std::string_literals;
using namespace solidity;
using namespace solidity::util;
using namespace solidity::langutil;

namespace
{

std::set<frontend::InputMode> const CompilerInputModes{
	frontend::InputMode::Compiler,
	frontend::InputMode::CompilerWithASTImport,
};

} // anonymous namespace

namespace solidity::frontend
{

std::ostream& CommandLineInterface::sout(bool _markAsUsed)
{
	if (_markAsUsed)
		m_hasOutput = true;
	return m_sout;
}

std::ostream& CommandLineInterface::serr(bool _markAsUsed)
{
	if (_markAsUsed)
		m_hasOutput = true;
	return m_serr;
}

#define cin
#define cout
#define cerr

static bool needsHumanTargetedStdout(CommandLineOptions const& _options)
{
	if (_options.compiler.estimateGas)
		return true;
	if (!_options.output.dir.empty())
		return false;
	return
		_options.compiler.outputs.abi ||
		_options.compiler.outputs.asmJson ||
		_options.compiler.outputs.binary ||
		_options.compiler.outputs.binaryRuntime ||
		_options.compiler.outputs.metadata ||
		_options.compiler.outputs.natspecUser ||
		_options.compiler.outputs.natspecDev ||
		_options.compiler.outputs.opcodes ||
		_options.compiler.outputs.signatureHashes ||
		_options.compiler.outputs.storageLayout;
}

static bool coloredOutput(CommandLineOptions const& _options)
{
	return
		(!_options.formatting.coloredOutput.has_value() && isatty(STDERR_FILENO)) ||
		(_options.formatting.coloredOutput.has_value() && _options.formatting.coloredOutput.value());
}

void CommandLineInterface::handleNatspec(bool _natspecDev, std::string const& _contract)
{
	solAssert(CompilerInputModes.count(m_options.input.mode) == 1);

	bool enabled = false;
	std::string suffix;
	std::string title;

	if (_natspecDev)
	{
		enabled = m_options.compiler.outputs.natspecDev;
		suffix = ".docdev";
		title = "Developer Documentation";
	}
	else
	{
		enabled = m_options.compiler.outputs.natspecUser;
		suffix = ".docuser";
		title = "User Documentation";
	}

	if (enabled)
	{
		std::string output = jsonPrint(
			removeNullMembers(
				_natspecDev ?
				m_compiler->natspecDev(_contract) :
				m_compiler->natspecUser(_contract)
			),
			m_options.formatting.json
		);

		if (!m_options.output.dir.empty())
			createFile(m_compiler->filesystemFriendlyName(_contract) + suffix, output);
		else
		{
			sout() << title << std::endl;
			sout() << output << std::endl;
		}

	}
}

void CommandLineInterface::readInputFiles()
{
	solAssert(!m_standardJsonInput.has_value());

	if (m_options.input.noImportCallback)
		m_universalCallback.resetImportCallback();

	static std::set<frontend::InputMode> const noInputFiles{
		frontend::InputMode::Help,
		frontend::InputMode::License,
		frontend::InputMode::Version
	};

	if (noInputFiles.count(m_options.input.mode) == 1)
		return;

	m_fileReader.setBasePath(m_options.input.basePath);

	if (m_fileReader.basePath() != "")
	{
		if (!boost::filesystem::exists(m_fileReader.basePath()))
			solThrow(CommandLineValidationError, "Base path does not exist: \"" + m_fileReader.basePath().string() + '"');

		if (!boost::filesystem::is_directory(m_fileReader.basePath()))
			solThrow(CommandLineValidationError, "Base path is not a directory: \"" + m_fileReader.basePath().string() + '"');
	}

	for (boost::filesystem::path const& includePath: m_options.input.includePaths)
		m_fileReader.addIncludePath(includePath);

	for (boost::filesystem::path const& allowedDirectory: m_options.input.allowedDirectories)
		m_fileReader.allowDirectory(allowedDirectory);

	std::map<std::string, std::set<boost::filesystem::path>> collisions =
		m_fileReader.detectSourceUnitNameCollisions(m_options.input.paths);
	if (!collisions.empty())
	{
		auto pathToQuotedString = [](boost::filesystem::path const& _path){ return "\"" + _path.string() + "\""; };

		std::string message =
			"Source unit name collision detected. "
			"The specified values of base path and/or include paths would result in multiple "
			"input files being assigned the same source unit name:\n";

		for (auto const& [sourceUnitName, normalizedInputPaths]: collisions)
		{
			message += sourceUnitName + " matches: ";
			message += util::joinHumanReadable(normalizedInputPaths | ranges::views::transform(pathToQuotedString)) + "\n";
		}

		solThrow(CommandLineValidationError, message);
	}

	for (boost::filesystem::path const& infile: m_options.input.paths)
	{
		if (!boost::filesystem::exists(infile))
		{
			if (!m_options.input.ignoreMissingFiles)
				solThrow(CommandLineValidationError, '"' + infile.string() + "\" is not found.");
			else
				report(Error::Severity::Info, fmt::format("\"{}\" is not found. Skipping.", infile.string()));

			continue;
		}

		if (!boost::filesystem::is_regular_file(infile))
		{
			if (!m_options.input.ignoreMissingFiles)
				solThrow(CommandLineValidationError, '"' + infile.string() + "\" is not a valid file.");
			else
				report(Error::Severity::Info, fmt::format("\"{}\" is not a valid file. Skipping.", infile.string()));

			continue;
		}

		// NOTE: we ignore the FileNotFound exception as we manually check above
		std::string fileContent = readFileAsString(infile);
		if (m_options.input.mode == InputMode::StandardJson)
		{
			solAssert(!m_standardJsonInput.has_value());
			m_standardJsonInput = std::move(fileContent);
		}
		else
		{
			m_fileReader.addOrUpdateFile(infile, std::move(fileContent));
			m_fileReader.allowDirectory(boost::filesystem::canonical(infile).remove_filename());
		}
	}

	if (m_options.input.addStdin)
	{
		if (m_options.input.mode == InputMode::StandardJson)
		{
			solAssert(!m_standardJsonInput.has_value());
			m_standardJsonInput = readUntilEnd(m_sin);
		}
		else
			m_fileReader.setStdin(readUntilEnd(m_sin));
	}

	if (
		m_options.input.mode != InputMode::LanguageServer &&
		m_fileReader.sourceUnits().empty() &&
		!m_standardJsonInput.has_value()
	)
		solThrow(CommandLineValidationError, "All specified input files either do not exist or are not regular files.");
}

std::map<std::string, Json::Value> CommandLineInterface::parseAstFromInput()
{
	solAssert(m_options.input.mode == InputMode::CompilerWithASTImport);

	std::map<std::string, Json::Value> sourceJsons;
	std::map<std::string, std::string> tmpSources;

	for (SourceCode const& sourceCode: m_fileReader.sourceUnits() | ranges::views::values)
	{
		Json::Value ast;
		astAssert(jsonParseStrict(sourceCode, ast), "Input file could not be parsed to JSON");
		astAssert(ast.isMember("sources"), "Invalid Format for import-JSON: Must have 'sources'-object");

		for (auto& src: ast["sources"].getMemberNames())
		{
			std::string astKey = ast["sources"][src].isMember("ast") ? "ast" : "AST";

			astAssert(ast["sources"][src].isMember(astKey), "astkey is not member");
			astAssert(ast["sources"][src][astKey]["nodeType"].asString() == "SourceUnit",  "Top-level node should be a 'SourceUnit'");
			astAssert(sourceJsons.count(src) == 0, "All sources must have unique names");
			sourceJsons.emplace(src, std::move(ast["sources"][src][astKey]));
			tmpSources[src] = util::jsonCompactPrint(ast);
		}
	}

	m_fileReader.setSourceUnits(tmpSources);

	return sourceJsons;
}

void CommandLineInterface::createFile(std::string const& _fileName, std::string const& _data)
{
	namespace fs = boost::filesystem;

	solAssert(!m_options.output.dir.empty());

	// NOTE: create_directories() raises an exception if the path consists solely of '.' or '..'
	// (or equivalent such as './././.'). Paths like 'a/b/.' and 'a/b/..' are fine though.
	// The simplest workaround is to use an absolute path.
	fs::create_directories(fs::absolute(m_options.output.dir));

	std::string pathName = (m_options.output.dir / _fileName).string();
	if (fs::exists(pathName) && !m_options.output.overwriteFiles)
		solThrow(CommandLineOutputError, "Refusing to overwrite existing file \"" + pathName + "\" (use --overwrite to force).");

	std::ofstream outFile(pathName);
	outFile << _data;
	if (!outFile)
		solThrow(CommandLineOutputError, "Could not write to file \"" + pathName + "\".");
}

bool CommandLineInterface::run(int _argc, char const* const* _argv)
{
	try
	{
		if (!parseArguments(_argc, _argv))
			return false;

		readInputFiles();
		processInput();
		return true;
	}
	catch (CommandLineError const& _exception)
	{
		m_hasOutput = true;

		// There might be no message in the exception itself if the error output is bulky and has
		// already been printed to stderr (this happens e.g. for compiler errors).
		if (_exception.what() != ""s)
			report(Error::Severity::Error, _exception.what());

		return false;
	}
}

bool CommandLineInterface::parseArguments(int _argc, char const* const* _argv)
{
	CommandLineParser parser;

	if (isatty(fileno(stdin)) && _argc == 1)
	{
		// If the terminal is taking input from the user, provide more user-friendly output.
		CommandLineParser::printHelp(sout());

		// In this case we want to exit with an error but not display any error message.
		return false;
	}

	try
	{
		parser.parse(_argc, _argv);
	}
	catch (...)
	{
		// Even if the overall CLI parsing fails, the --color/--no-color options may have been
		// successfully parsed, and if so, should be taken into account when printing errors.
		// If no value is present, it's possible that --no-color is still there but parsing failed
		// due to other, unrecognized options so play it safe and disable color in that case.
		m_options.formatting.coloredOutput = parser.options().formatting.coloredOutput.value_or(false);
		throw;
	}
	m_options = parser.options();

	return true;
}

void CommandLineInterface::processInput()
{
	if (m_options.output.evmVersion < EVMVersion::constantinople())
		report(
			Error::Severity::Warning,
			"Support for EVM versions older than constantinople is deprecated and will be removed in the future."
		);

	switch (m_options.input.mode)
	{
	case InputMode::Help:
		CommandLineParser::printHelp(sout());
		break;
	case InputMode::License:
		printLicense();
		break;
	case InputMode::Version:
		printVersion();
		break;
	case InputMode::StandardJson:
	{
		solAssert(m_standardJsonInput.has_value());

		StandardCompiler compiler(m_universalCallback.callback(), m_options.formatting.json);
		sout() << compiler.compile(std::move(m_standardJsonInput.value())) << std::endl;
		m_standardJsonInput.reset();
		break;
	}
	case InputMode::LanguageServer:
		serveLSP();
		break;
	case InputMode::Assembler:
		solUnimplemented("");
		break;
	case InputMode::Linker:
		solUnimplemented("");
		break;
	case InputMode::Compiler:
	case InputMode::CompilerWithASTImport:
		compile();
		outputCompilationResults();
		break;
	case InputMode::EVMAssemblerJSON:
		solUnimplemented("");
		break;
	}
}

void CommandLineInterface::printVersion()
{
	sout() << "solc, the TVM solidity compiler commandline interface" << std::endl;
	sout() << "Version: " << solidity::frontend::VersionString << std::endl;
}

void CommandLineInterface::printLicense()
{
	sout() << otherLicenses << std::endl;
	// This is a static variable generated by cmake from LICENSE.txt
	sout() << licenseText << std::endl;
}


void CommandLineInterface::compile()
{
	solAssert(CompilerInputModes.count(m_options.input.mode) == 1);
	solAssert(!m_compiler);

	m_compiler = std::make_unique<CompilerStack>(m_universalCallback.callback());

	SourceReferenceFormatter formatter(serr(false), *m_compiler, coloredOutput(m_options), m_options.formatting.withErrorIds);

	try
	{
		if (m_options.metadata.literalSources)
			m_compiler->useMetadataLiteralSources(true);
		m_compiler->setMetadataFormat(m_options.metadata.format);
		m_compiler->setMetadataHash(m_options.metadata.hash);
		m_compiler->setRemappings(m_options.input.remappings);
		m_compiler->setLibraries(m_options.linker.libraries);
		m_compiler->setViaIR(m_options.output.viaIR);
		m_compiler->setEVMVersion(m_options.output.evmVersion);
		m_compiler->setRevertStringBehaviour(m_options.output.revertStrings);
		// TODO: Perhaps we should not compile unless requested
		m_compiler->enableIRGeneration(
			m_options.compiler.outputs.ir ||
			m_options.compiler.outputs.irOptimized ||
			m_options.compiler.outputs.irAstJson ||
			m_options.compiler.outputs.irOptimizedAstJson
		);
		m_compiler->enableEvmBytecodeGeneration(
			m_options.compiler.estimateGas ||
			m_options.compiler.outputs.asmJson ||
			m_options.compiler.outputs.opcodes ||
			m_options.compiler.outputs.binary ||
			m_options.compiler.outputs.binaryRuntime ||
			(m_options.compiler.combinedJsonRequests && (
				m_options.compiler.combinedJsonRequests->binary ||
				m_options.compiler.combinedJsonRequests->binaryRuntime ||
				m_options.compiler.combinedJsonRequests->opcodes ||
				m_options.compiler.combinedJsonRequests->generatedSources ||
				m_options.compiler.combinedJsonRequests->generatedSourcesRuntime ||
				m_options.compiler.combinedJsonRequests->srcMap ||
				m_options.compiler.combinedJsonRequests->srcMapRuntime ||
				m_options.compiler.combinedJsonRequests->funDebug ||
				m_options.compiler.combinedJsonRequests->funDebugRuntime
			))
		);

		m_compiler->setOptimiserSettings(m_options.optimiserSettings());

		if (m_options.input.mode == InputMode::CompilerWithASTImport)
		{
			try
			{
				m_compiler->importASTs(parseAstFromInput());

				if (!m_compiler->analyze())
				{
					formatter.printErrorInformation(m_compiler->errors());
					astAssert(false, "Analysis of the AST failed");
				}
			}
			catch (Exception const& _exc)
			{
				// FIXME: AST import is missing proper validations. This hack catches failing
				// assertions and presents them as if they were compiler errors.
				solThrow(CommandLineExecutionError, "Failed to import AST: "s + _exc.what());
			}
		}
		else
		{
			StringMap const& src = m_fileReader.sourceUnits();
			solAssert(src.size() == 1, "");
			m_compiler->setInputFile(src.begin()->first);
			m_compiler->setSources(m_fileReader.sourceUnits());
		}

		if (m_options.tvmParams.mainContract.has_value())
			m_compiler->setMainContract(m_options.tvmParams.mainContract.value());
		if (m_options.tvmParams.fileNamePrefix.has_value())
			m_compiler->setFileNamePrefix(m_options.tvmParams.fileNamePrefix.value());
		if (m_options.tvmParams.code)
			m_compiler->generateCode();
		if (m_options.tvmParams.abi)
			m_compiler->generateAbi();
		if (m_options.tvmParams.printFunctionIds)
			m_compiler->printFunctionIds();
		if (m_options.tvmParams.printPrivateFunctionIds)
			m_compiler->printPrivateFunctionIds();
		m_compiler->setOutputFolder(m_options.output.dir.string());
		m_compiler->setTVMVersion(m_options.tvmParams.tvmVersion);

		bool successful = true;
		bool didCompileSomething = false;
		std::tie(successful, didCompileSomething) = m_compiler->compile();
		m_hasOutput |= didCompileSomething;

		for (auto const& error: m_compiler->errors())
		{
			m_hasOutput = true;
			formatter.printErrorInformation(*error);
		}

		if (!successful)
			solThrow(CommandLineExecutionError, "");
	}
	catch (CompilerError const& _exception)
	{
		m_hasOutput = true;
		formatter.printExceptionInformation(
			_exception,
			Error::errorSeverity(Error::Type::CompilerError)
		);
		solThrow(CommandLineExecutionError, "");
	}
	catch (Error const& _error)
	{
		if (_error.type() == Error::Type::DocstringParsingError)
		{
			report(Error::Severity::Error, *boost::get_error_info<errinfo_comment>(_error));
			solThrow(CommandLineExecutionError, "Documentation parsing failed.");
		}
		else
		{
			m_hasOutput = true;
			formatter.printErrorInformation(_error);
			solThrow(CommandLineExecutionError, "");
		}
	}
}

void CommandLineInterface::handleAst()
{
	solAssert(CompilerInputModes.count(m_options.input.mode) == 1);

	if (!m_options.compiler.outputs.astCompactJson)
		return;

	std::vector<ASTNode const*> asts;
	for (auto const& sourceCode: m_fileReader.sourceUnits())
		asts.push_back(&m_compiler->ast(sourceCode.first));

	if (!m_options.output.dir.empty())
	{
		for (auto const& sourceCode: m_fileReader.sourceUnits())
		{
			std::stringstream data;
			std::string postfix = "";
			ASTJsonExporter(m_compiler->state(), m_compiler->sourceIndices()).print(data, m_compiler->ast(sourceCode.first), m_options.formatting.json);
			postfix += "_json";
			boost::filesystem::path path(sourceCode.first);
			createFile(path.filename().string() + postfix + ".ast", data.str());
		}
	}
	else
	{
		for (auto const& sourceCode: m_fileReader.sourceUnits())
		{
			ASTJsonExporter(m_compiler->state(), m_compiler->sourceIndices()).print(sout(), m_compiler->ast(sourceCode.first), m_options.formatting.json);
			sout() << std::endl;
		}
		m_hasOutput = true;
	}
}

void CommandLineInterface::serveLSP()
{
	lsp::StdioTransport transport;
	if (!lsp::LanguageServer{transport}.run())
		solThrow(CommandLineExecutionError, "LSP terminated abnormally.");
}

void CommandLineInterface::outputCompilationResults()
{
	solAssert(CompilerInputModes.count(m_options.input.mode) == 1);

	// do we need AST output?
	handleAst();

	CompilerOutputs astOutputSelection;
	astOutputSelection.astCompactJson = true;
	if (m_options.compiler.outputs != CompilerOutputs() && m_options.compiler.outputs != astOutputSelection)
	{
		// Currently AST is the only output allowed with --stop-after parsing. For all of the others
		// we can safely assume that full compilation was performed and successful.
		solAssert(m_options.output.stopAfter >= CompilerStack::State::CompilationSuccessful);

		for (std::string const& contract: m_compiler->contractNames())
		{
			if (needsHumanTargetedStdout(m_options))
				sout() << std::endl << "======= " << contract << " =======" << std::endl;

			handleNatspec(true, contract);
			handleNatspec(false, contract);
		} // end of contracts iteration
	}

	if (!m_hasOutput)
	{
		if (!m_options.output.dir.empty())
			sout() << "Compiler run successful. Artifact(s) can be found in directory " << m_options.output.dir << "." << std::endl;
		else if (m_compiler->contractNames().empty())
			sout() << "Compiler run successful. No contracts to compile." << std::endl;
		else
			sout() << "Compiler run successful. No output generated." << std::endl;
	}
}

void CommandLineInterface::report(langutil::Error::Severity _severity, std::string _message)
{
	SourceReferenceFormatter::printPrimaryMessage(
		serr(),
		_message,
		_severity,
		std::nullopt,
		coloredOutput(m_options),
		m_options.formatting.withErrorIds
	);
}

}
