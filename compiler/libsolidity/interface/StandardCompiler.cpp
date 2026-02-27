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
 * @author Alex Beregszaszi
 * @date 2016
 * Standard JSON compiler interface.
 */

#include <libsolidity/interface/StandardCompiler.h>
#include <libsolidity/interface/ImportRemapper.h>

#include <libsolidity/ast/ASTJsonExporter.h>

#include <libsmtutil/Exceptions.h>

#include <liblangutil/SourceReferenceFormatter.h>

#include <libsolutil/JSON.h>
#include <libsolutil/Keccak256.h>
#include <libsolutil/CommonData.h>

#include <boost/algorithm/string/predicate.hpp>

#include <algorithm>
#include <optional>

using namespace solidity;
using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace solidity::util;
using namespace std::string_literals;

namespace
{

Json formatError(
	Error::Type _type,
	std::string const& _component,
	std::string const& _message,
	std::string const& _formattedMessage = "",
	std::string const& _humanFormattedMessage = "",
	Json const& _sourceLocation = Json(),
	Json const& _secondarySourceLocation = Json()
)
{
	Json error;
	error["type"] = Error::formatErrorType(_type);
	error["component"] = _component;
	error["severity"] = Error::formatErrorSeverityLowercase(Error::errorSeverity(_type));
	error["message"] = _message;
	error["formattedMessage"] = (_formattedMessage.length() > 0) ? _formattedMessage : _message;
	error["humanFormattedMessage"] = (_humanFormattedMessage.length() > 0) ? _humanFormattedMessage : _message;
	if (_sourceLocation.is_object())
		error["sourceLocation"] = _sourceLocation;
	if (_secondarySourceLocation.is_array())
		error["secondarySourceLocations"] = _secondarySourceLocation;
	return error;
}

Json formatFatalError(Error::Type _type, std::string const& _message)
{
	Json output;
	output["errors"] = Json::array();
	output["errors"].emplace_back(formatError(_type, "general", _message));
	return output;
}

Json formatSourceLocation(SourceLocation const* location)
{
	if (!location || !location->sourceName)
		return Json();

	Json sourceLocation;
	sourceLocation["file"] = *location->sourceName;
	sourceLocation["start"] = location->start;
	sourceLocation["end"] = location->end;
	return sourceLocation;
}

Json formatSecondarySourceLocation(SecondarySourceLocation const* _secondaryLocation)
{
	if (!_secondaryLocation)
		return Json();

	Json secondarySourceLocation = Json::array();
	for (auto const& location: _secondaryLocation->infos)
	{
		Json msg = formatSourceLocation(&location.second);
		msg["message"] = location.first;
		secondarySourceLocation.emplace_back(msg);
	}
	return secondarySourceLocation;
}

Json formatErrorWithException(
	CharStreamProvider const& _charStreamProvider,
	util::Exception const& _exception,
	Error::Type _type,
	std::string const& _component,
	std::string const& _message,
	std::optional<ErrorId> _errorId = std::nullopt
)
{
	std::string message;
	// TODO: consider enabling color
	std::string formattedMessage = SourceReferenceFormatter::formatExceptionInformation(
		_exception,
		_type,
		_charStreamProvider,
		false // colored
	);

	std::string errText = Error::formatErrorSeverity(Error::errorSeverity(_type));
    std::string typeForHuman = boost::ends_with(errText, "Error") ? "Error" : errText;
	std::string humanFormattedMessage = SourceReferenceFormatter::formatExceptionInformation(_exception, _type, _charStreamProvider, true);

	if (std::string const* description = _exception.comment())
		message = ((_message.length() > 0) ? (_message + ": ") : "") + *description;
	else
		message = _message;

	Json error = formatError(
		_type,
		_component,
		message,
		formattedMessage,
		humanFormattedMessage,
		formatSourceLocation(boost::get_error_info<errinfo_sourceLocation>(_exception)),
		formatSecondarySourceLocation(boost::get_error_info<errinfo_secondarySourceLocation>(_exception))
	);

	if (_errorId)
		error["errorCode"] = std::to_string(_errorId.value().error);

	return error;
}

/// Returns true iff @a _hash (hex with 0x prefix) is the Keccak256 hash of the binary data in @a _content.
bool hashMatchesContent(std::string const& _hash, std::string const& _content)
{
	try
	{
		return util::h256(_hash) == util::keccak256(_content);
	}
	catch (util::BadHexCharacter const&)
	{
		return false;
	}
}

bool isArtifactRequested(Json const& _outputSelection, std::string const& _artifact, bool _wildcardMatchesExperimental)
{
	static std::set<std::string> experimental{"ir", "irAst", "irOptimized", "irOptimizedAst", "yulCFGJson", "ethdebug"};
	for (auto const& selectedArtifactJson: _outputSelection)
	{
		std::string const& selectedArtifact = selectedArtifactJson.get<std::string>();
		if (
			_artifact == selectedArtifact ||
			boost::algorithm::starts_with(_artifact, selectedArtifact + ".")
		)
		{
			if (_artifact.find("ethdebug") != std::string::npos)
				// only accept exact matches for ethdebug, e.g. evm.bytecode.ethdebug
				return selectedArtifact == _artifact;
			return true;
		}
		else if (selectedArtifact == "*")
		{
			// TODO: yulCFGJson is only experimental now, so it should not be matched by "*".
			if (_artifact == "yulCFGJson")
				return false;
			// TODO: everything ethdebug related is only experimental for now, so it should not be matched by "*".
			if (_artifact.find("ethdebug") != std::string::npos)
				return false;
			// "ir", "irOptimized" can only be matched by "*" if activated.
			if (experimental.count(_artifact) == 0 || _wildcardMatchesExperimental)
				return true;
		}
	}
	return false;
}

///
/// @a _outputSelection is a JSON object containing a two-level hashmap, where the first level is the filename,
/// the second level is the contract name and the value is an array of artifact names to be requested for that contract.
/// @a _file is the current file
/// @a _contract is the current contract
/// @a _artifact is the current artifact name
///
/// @returns true if the @a _outputSelection has a match for the requested target in the specific file / contract.
///
/// In @a _outputSelection the use of '*' as a wildcard is permitted.
///
/// @TODO optimise this. Perhaps flatten the structure upfront.
///
bool isArtifactRequested(Json const& _outputSelection, std::string const& _file, std::string const& _contract, std::string const& _artifact, bool _wildcardMatchesExperimental)
{
	if (!_outputSelection.is_object())
		return false;

	for (auto const& file: { _file, std::string("*") })
		if (_outputSelection.contains(file) && _outputSelection[file].is_object())
		{
			/// For SourceUnit-level targets (such as AST) only allow empty name, otherwise
			/// for Contract-level targets try both contract name and wildcard
			std::vector<std::string> contracts{ _contract };
			if (!_contract.empty())
				contracts.emplace_back("*");
			for (auto const& contract: contracts)
				if (
					_outputSelection[file].contains(contract) &&
					_outputSelection[file][contract].is_array() &&
					isArtifactRequested(_outputSelection[file][contract], _artifact, _wildcardMatchesExperimental)
				)
					return true;
		}

	return false;
}

/// @returns all artifact names of the EVM object, either for creation or deploy time.
std::vector<std::string> evmObjectComponents(std::string const& _objectKind)
{
	solAssert(_objectKind == "bytecode" || _objectKind == "deployedBytecode", "");
	std::vector<std::string> components{"", ".object", ".opcodes", ".sourceMap", ".functionDebugData", ".generatedSources", ".linkReferences", ".ethdebug"};
	if (_objectKind == "deployedBytecode")
		components.push_back(".immutableReferences");
	return util::applyMap(components, [&](auto const& _s) { return "evm." + _objectKind + _s; });
}

/// @returns true if any binary was requested, i.e. we actually have to perform compilation.
bool isBinaryRequested(Json const& _outputSelection)
{
	if (!_outputSelection.is_object())
		return false;

	// This does not include "evm.methodIdentifiers" on purpose!
	static std::vector<std::string> const outputsThatRequireBinaries = std::vector<std::string>{
		"*",
		"ir", "irAst", "irOptimized", "irOptimizedAst", "yulCFGJson",
		"evm.gasEstimates", "evm.legacyAssembly", "evm.assembly", "ethdebug"
	} + evmObjectComponents("bytecode") + evmObjectComponents("deployedBytecode");

	for (auto const& fileRequests: _outputSelection)
		for (auto const& requests: fileRequests)
			for (auto const& output: outputsThatRequireBinaries)
				if (isArtifactRequested(requests, output, false))
					return true;
	return false;
}

/// @returns true if EVM bytecode was requested, i.e. we have to run the old code generator.
bool isEvmBytecodeRequested(Json const& _outputSelection)
{
	if (!_outputSelection.is_object())
		return false;

	static std::vector<std::string> const outputsThatRequireEvmBinaries = std::vector<std::string>{
		"*",
		"evm.gasEstimates", "evm.legacyAssembly", "evm.assembly"
	} + evmObjectComponents("bytecode") + evmObjectComponents("deployedBytecode");

	for (auto const& fileRequests: _outputSelection)
		for (auto const& requests: fileRequests)
			for (auto const& output: outputsThatRequireEvmBinaries)
				if (isArtifactRequested(requests, output, false))
					return true;
	return false;
}

/// @returns true if ethdebug was requested.
bool isEthdebugRequested(Json const& _outputSelection)
{
	if (!_outputSelection.is_object())
		return false;

	for (auto const& fileRequests: _outputSelection)
		for (auto const& requests: fileRequests)
			for (auto const& request: requests)
				if (request == "evm.bytecode.ethdebug" || request == "evm.deployedBytecode.ethdebug")
					return true;

	return false;
}

/// @returns The set of selected contracts, along with their compiler pipeline configuration, based
/// on outputs requested in the JSON. Translates wildcards to the ones understood by CompilerStack.
/// Note that as an exception, '*' does not yet match "ir", "irAst", "irOptimized" or "irOptimizedAst".
CompilerStack::ContractSelection pipelineConfig(
	Json const& _jsonOutputSelection
)
{
	if (!_jsonOutputSelection.is_object())
		return {};

	CompilerStack::ContractSelection contractSelection;
	for (auto const& [sourceUnitName, jsonOutputSelectionForSource]: _jsonOutputSelection.items())
	{
		solAssert(jsonOutputSelectionForSource.is_object());
		for (auto const& [contractName, jsonOutputSelectionForContract]: jsonOutputSelectionForSource.items())
		{
			solAssert(jsonOutputSelectionForContract.is_array());
			CompilerStack::PipelineConfig pipelineForContract;
			for (Json const& request: jsonOutputSelectionForContract)
			{
				solAssert(request.is_string());
				pipelineForContract.irOptimization =
					pipelineForContract.irOptimization ||
					request == "irOptimized" ||
					request == "irOptimizedAst" ||
					request == "yulCFGJson";
				pipelineForContract.irCodegen =
					pipelineForContract.irCodegen ||
					pipelineForContract.irOptimization ||
					request == "ir" ||
					request == "irAst";
				pipelineForContract.bytecode = isEvmBytecodeRequested(_jsonOutputSelection);
			}
			std::string key = (sourceUnitName == "*") ? "" : sourceUnitName;
			std::string value = (contractName == "*") ? "" : contractName;
			contractSelection[key][value] = pipelineForContract;
		}
	}
	return contractSelection;
}

std::optional<Json> checkKeys(Json const& _input, std::set<std::string> const& _keys, std::string const& _name)
{
	if (!_input.empty() && !_input.is_object())
		return formatFatalError(Error::Type::JSONError, "\"" + _name + "\" must be an object");

	for (auto const& [member, _]: _input.items())
		if (!_keys.count(member))
			return formatFatalError(Error::Type::JSONError, "Unknown key \"" + member + "\"");

	return std::nullopt;
}

std::optional<Json> checkRootKeys(Json const& _input)
{
	static std::set<std::string> keys{"auxiliaryInput", "language", "settings", "sources"};
	return checkKeys(_input, keys, "root");
}

std::optional<Json> checkSourceKeys(Json const& _input, std::string const& _name)
{
	static std::set<std::string> keys{"content", "keccak256", "urls"};
	return checkKeys(_input, keys, "sources." + _name);
}

std::optional<Json> checkAuxiliaryInputKeys(Json const& _input)
{
	static std::set<std::string> keys{"smtlib2responses"};
	return checkKeys(_input, keys, "auxiliaryInput");
}

std::optional<Json> checkSettingsKeys(Json const& _input)
{
	static std::set<std::string> keys{"debug", "evmVersion", "eofVersion", "libraries", "metadata", "modelChecker", "optimizer", "outputSelection", "remappings", "stopAfter", "viaIR",
		"includePaths", "mainContract", "tvmVersion"};
	return checkKeys(_input, keys, "settings");
}

std::optional<Json> checkModelCheckerSettingsKeys(Json const& _input)
{
	static std::set<std::string> keys{"bmcLoopIterations", "contracts", "divModNoSlacks", "engine", "extCalls", "invariants", "printQuery", "showProvedSafe", "showUnproved", "showUnsupported", "solvers", "targets", "timeout"};
	return checkKeys(_input, keys, "modelChecker");
}

std::optional<Json> checkOptimizerKeys(Json const& _input)
{
	static std::set<std::string> keys{"details", "enabled", "runs"};
	return checkKeys(_input, keys, "settings.optimizer");
}

std::optional<Json> checkOptimizerDetailsKeys(Json const& _input)
{
	static std::set<std::string> keys{"peephole", "inliner", "jumpdestRemover", "orderLiterals", "deduplicate", "cse", "constantOptimizer", "yul", "yulDetails", "simpleCounterForLoopUncheckedIncrement"};
	return checkKeys(_input, keys, "settings.optimizer.details");
}

std::optional<Json> checkOptimizerDetail(Json const& _details, std::string const& _name, bool& _setting)
{
	if (_details.contains(_name))
	{
		if (!_details[_name].is_boolean())
			return formatFatalError(Error::Type::JSONError, "\"settings.optimizer.details." + _name + "\" must be Boolean");
		_setting = _details[_name].get<bool>();
	}
	return {};
}

std::optional<Json> checkMetadataKeys(Json const& _input)
{
	if (_input.is_object())
	{
		if (_input.contains("appendCBOR") && !_input["appendCBOR"].is_boolean())
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.appendCBOR\" must be Boolean");
		if (_input.contains("useLiteralContent") && !_input["useLiteralContent"].is_boolean())
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.useLiteralContent\" must be Boolean");

		static std::set<std::string> hashes{"ipfs", "bzzr1", "none"};
		if (_input.contains("bytecodeHash") && !hashes.count(_input["bytecodeHash"].get<std::string>()))
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.bytecodeHash\" must be \"ipfs\", \"bzzr1\" or \"none\"");
	}
	static std::set<std::string> keys{"appendCBOR", "useLiteralContent", "bytecodeHash"};
	return checkKeys(_input, keys, "settings.metadata");
}

std::optional<Json> checkOutputSelection(Json const& _outputSelection)
{
	if (!_outputSelection.empty() && !_outputSelection.is_object())
		return formatFatalError(Error::Type::JSONError, "\"settings.outputSelection\" must be an object");

	for (auto const& [sourceName, sourceVal]: _outputSelection.items())
	{
		if (!sourceVal.is_object())
			return formatFatalError(
				Error::Type::JSONError,
				"\"settings.outputSelection." + sourceName + "\" must be an object"
			);

		for (auto const& [contractName, contractVal]: sourceVal.items())
		{
			if (!contractVal.is_array())
				return formatFatalError(
					Error::Type::JSONError,
					"\"settings.outputSelection." +
					sourceName +
					"." +
					contractName +
					"\" must be a string array"
				);

			for (auto const& output: contractVal)
				if (!output.is_string())
					return formatFatalError(
						Error::Type::JSONError,
						"\"settings.outputSelection." +
						sourceName +
						"." +
						contractName +
						"\" must be a string array"
					);
		}
	}

	return std::nullopt;
}

/// Validates the optimizer settings and returns them in a parsed object.
/// On error returns the json-formatted error message.
std::variant<OptimiserSettings, Json> parseOptimizerSettings(std::string_view const _language, Json const& _jsonInput)
{
	if (auto result = checkOptimizerKeys(_jsonInput))
		return *result;

	OptimiserSettings settings = _language == "EVMAssembly" ? OptimiserSettings::none() : OptimiserSettings::minimal();

	if (_jsonInput.contains("enabled"))
	{
		if (!_jsonInput["enabled"].is_boolean())
			return formatFatalError(Error::Type::JSONError, "The \"enabled\" setting must be a Boolean.");

		if (_jsonInput["enabled"].get<bool>())
			settings = OptimiserSettings::standard();
	}

	if (_jsonInput.contains("runs"))
	{
		if (!_jsonInput["runs"].is_number_unsigned())
			return formatFatalError(Error::Type::JSONError, "The \"runs\" setting must be an unsigned number.");
		settings.expectedExecutionsPerDeployment = _jsonInput["runs"].get<size_t>();
	}

	if (_jsonInput.contains("details"))
	{
		Json const& details = _jsonInput["details"];
		if (auto result = checkOptimizerDetailsKeys(details))
			return *result;

		if (auto error = checkOptimizerDetail(details, "peephole", settings.runPeephole))
			return *error;
		if (auto error = checkOptimizerDetail(details, "inliner", settings.runInliner))
			return *error;
		if (auto error = checkOptimizerDetail(details, "jumpdestRemover", settings.runJumpdestRemover))
			return *error;
		if (auto error = checkOptimizerDetail(details, "orderLiterals", settings.runOrderLiterals))
			return *error;
		if (auto error = checkOptimizerDetail(details, "deduplicate", settings.runDeduplicate))
			return *error;
		if (auto error = checkOptimizerDetail(details, "cse", settings.runCSE))
			return *error;
		if (auto error = checkOptimizerDetail(details, "constantOptimizer", settings.runConstantOptimiser))
			return *error;
		if (auto error = checkOptimizerDetail(details, "yul", settings.runYulOptimiser))
			return *error;
		if (auto error = checkOptimizerDetail(details, "simpleCounterForLoopUncheckedIncrement", settings.simpleCounterForLoopUncheckedIncrement))
			return *error;
		settings.optimizeStackAllocation = settings.runYulOptimiser;
		if (details.contains("yulDetails"))
		{
			if (!settings.runYulOptimiser)
			{
				if (checkKeys(details["yulDetails"], {"optimizerSteps"}, "settings.optimizer.details.yulDetails"))
					return formatFatalError(Error::Type::JSONError, "Only optimizerSteps can be set in yulDetails when Yul optimizer is disabled.");
				return {std::move(settings)};
			}

			if (auto result = checkKeys(details["yulDetails"], {"stackAllocation", "optimizerSteps"}, "settings.optimizer.details.yulDetails"))
				return *result;
			if (auto error = checkOptimizerDetail(details["yulDetails"], "stackAllocation", settings.optimizeStackAllocation))
				return *error;
		}
	}
	return {std::move(settings)};
}

}

std::variant<StandardCompiler::InputsAndSettings, Json> StandardCompiler::parseInput(Json const& _input)
{
	InputsAndSettings ret;

	if (!_input.is_object())
		return formatFatalError(Error::Type::JSONError, "Input is not a JSON object.");

	if (auto result = checkRootKeys(_input))
		return *result;

	ret.language = _input.value<std::string>("language", "");

	Json const& sources = _input.value<Json>("sources", Json());

	if (!sources.is_object() && !sources.is_null())
		return formatFatalError(Error::Type::JSONError, "\"sources\" is not a JSON object.");

	if (sources.empty())
		return formatFatalError(Error::Type::JSONError, "No input sources specified.");

	ret.errors = Json::array();
	ret.sources = Json::object();

	if (ret.language == "Solidity" || ret.language == "Yul")
	{
		for (auto const& [sourceName, sourceValue]: sources.items())
		{
			std::string hash;

			if (auto result = checkSourceKeys(sourceValue, sourceName))
				return *result;

			if (sourceValue.contains("keccak256") && sourceValue["keccak256"].is_string())
				hash = sourceValue["keccak256"].get<std::string>();

			if (sourceValue.contains("content") && sourceValue["content"].is_string())
			{
				std::string content = sourceValue["content"].get<std::string>();
				if (!hash.empty() && !hashMatchesContent(hash, content))
					ret.errors.emplace_back(formatError(
						Error::Type::IOError,
						"general",
						"Mismatch between content and supplied hash for \"" + sourceName + "\""
					));
				else
					ret.sources[sourceName] = content;
			}
			else if (sourceValue["urls"].is_array())
			{
				if (!m_readFile)
					return formatFatalError(
						Error::Type::JSONError, "No import callback supplied, but URL is requested."
					);

				std::vector<std::string> failures;
				bool found = false;

				for (auto const& url: sourceValue["urls"])
				{
					if (!url.is_string())
						return formatFatalError(Error::Type::JSONError, "URL must be a string.");
					ReadCallback::Result result = m_readFile(ReadCallback::kindString(ReadCallback::Kind::ReadFile), url.get<std::string>());
					if (result.success)
					{
						if (!hash.empty() && !hashMatchesContent(hash, result.responseOrErrorMessage))
							ret.errors.emplace_back(formatError(
								Error::Type::IOError,
								"general",
								"Mismatch between content and supplied hash for \"" + sourceName + "\" at \"" + url.get<std::string>() + "\""
							));
						else
						{
							ret.sources[sourceName] = result.responseOrErrorMessage;
							found = true;
							break;
						}
					}
					else
						failures.push_back(
							"Cannot import url (\"" + url.get<std::string>() + "\"): " + result.responseOrErrorMessage
						);
				}

				for (auto const& failure: failures)
				{
					/// If the import succeeded, let mark all the others as warnings, otherwise all of them are errors.
					ret.errors.emplace_back(formatError(
						found ? Error::Type::Warning : Error::Type::IOError,
						"general",
						failure
					));
				}
			}
			else
				return formatFatalError(Error::Type::JSONError, "Invalid input source specified.");
		}
	}
	else if (ret.language == "SolidityAST")
	{
		for (auto const& [sourceName, sourceValue]: sources.items())
			ret.sources[sourceName] = util::jsonCompactPrint(sourceValue);
	}
	else if (ret.language == "EVMAssembly")
	{
		for (auto const& [sourceName, sourceValue]: sources.items())
		{
			solAssert(sources.contains(sourceName));
			if (
				!sourceValue.contains("assemblyJson") ||
				!sourceValue["assemblyJson"].is_object() ||
				sourceValue.size() != 1
			)
				return formatFatalError(
					Error::Type::JSONError,
					"Invalid input source specified. Expected exactly one object, named 'assemblyJson', inside $.sources." + sourceName
				);

			ret.jsonSources[sourceName] = sourceValue["assemblyJson"];
		}
		if (ret.jsonSources.size() != 1)
			return formatFatalError(
				Error::Type::JSONError,
				"EVMAssembly import only supports exactly one input file."
			);
	}
	Json const& auxInputs = _input.value("auxiliaryInput", Json::object());

	if (auto result = checkAuxiliaryInputKeys(auxInputs))
		return *result;

	if (!auxInputs.empty())
	{
		Json const& smtlib2Responses = auxInputs.value("smtlib2responses", Json::object());
		if (!smtlib2Responses.empty())
		{
			if (!smtlib2Responses.is_object())
				return formatFatalError(Error::Type::JSONError, "\"auxiliaryInput.smtlib2responses\" must be an object.");

			for (auto const& [hashString, response]: smtlib2Responses.items())
			{
				util::h256 hash;
				try
				{
					hash = util::h256(hashString);
				}
				catch (util::BadHexCharacter const&)
				{
					return formatFatalError(Error::Type::JSONError, "Invalid hex encoding of SMTLib2 auxiliary input.");
				}

				if (!response.is_string())
					return formatFatalError(
						Error::Type::JSONError,
						"\"smtlib2Responses." + hashString + "\" must be a string."
					);

				ret.smtLib2Responses[hash] = response.get<std::string>();
			}
		}
	}

	Json const& settings = _input.value("settings", Json::object());

	if (auto result = checkSettingsKeys(settings))
		return *result;

	if (settings.contains("includePaths"))
	{
		for (auto const& includePath: settings["includePaths"])
		{
			if (!includePath.is_string())
				return formatFatalError(Error::Type::JSONError, "Include path must be a string.");
			ret.includePaths.push_back(includePath.get<std::string>());
		}
	}

	if (settings.contains("mainContract"))
	{
		if (!settings["mainContract"].is_string())
			return formatFatalError(Error::Type::JSONError, "\"settings.mainContract\" must be a String.");
		ret.mainContract = settings["mainContract"].get<std::string>();
	}

	if (settings.contains("stopAfter"))
	{
		if (!settings["stopAfter"].is_string())
			return formatFatalError(Error::Type::JSONError, "\"settings.stopAfter\" must be a string.");

		if (settings["stopAfter"].get<std::string>() != "parsing")
			return formatFatalError(Error::Type::JSONError, "Invalid value for \"settings.stopAfter\". Only valid value is \"parsing\".");

		ret.stopAfter = CompilerStack::State::Parsed;
	}

	if (settings.contains("viaIR"))
	{
		if (!settings["viaIR"].is_boolean())
			return formatFatalError(Error::Type::JSONError, "\"settings.viaIR\" must be a Boolean.");
		ret.viaIR = settings["viaIR"].get<bool>();
	}

	if (settings.contains("tvmVersion"))
	{
		if (!settings["tvmVersion"].is_string())
			return formatFatalError(Error::Type::JSONError, "tvmVersion must be a string.");
		std::optional<langutil::TVMVersion> version = langutil::TVMVersion::fromString(settings["tvmVersion"].get<std::string>());
		if (!version)
			return formatFatalError(Error::Type::JSONError, "Invalid TVM version requested.");
		ret.tvmVersion = *version;
	}

	if (settings.contains("debug"))
	{
		if (auto result = checkKeys(settings["debug"], {"revertStrings", "debugInfo"}, "settings.debug"))
			return *result;

		if (settings["debug"].contains("revertStrings"))
		{
			if (!settings["debug"]["revertStrings"].is_string())
				return formatFatalError(Error::Type::JSONError, "settings.debug.revertStrings must be a string.");
			std::optional<RevertStrings> revertStrings = revertStringsFromString(settings["debug"]["revertStrings"].get<std::string>());
			if (!revertStrings)
				return formatFatalError(Error::Type::JSONError, "Invalid value for settings.debug.revertStrings.");
			if (*revertStrings == RevertStrings::VerboseDebug)
				return formatFatalError(
					Error::Type::UnimplementedFeatureError,
					"Only \"default\", \"strip\" and \"debug\" are implemented for settings.debug.revertStrings for now."
				);
			ret.revertStrings = *revertStrings;
		}

		if (settings["debug"].contains("debugInfo"))
		{
			if (!settings["debug"]["debugInfo"].is_array())
				return formatFatalError(Error::Type::JSONError, "settings.debug.debugInfo must be an array.");

			std::vector<std::string> components;
			for (Json const& arrayValue: settings["debug"]["debugInfo"])
				components.push_back(arrayValue.get<std::string>());

			std::optional<DebugInfoSelection> debugInfoSelection = DebugInfoSelection::fromComponents(
				components,
				true /* _acceptWildcards */
			);
			if (!debugInfoSelection.has_value())
				return formatFatalError(Error::Type::JSONError, "Invalid value in settings.debug.debugInfo.");

			if (debugInfoSelection->snippet && !debugInfoSelection->location)
				return formatFatalError(
					Error::Type::JSONError,
					"To use 'snippet' with settings.debug.debugInfo you must select also 'location'."
				);

			ret.debugInfoSelection = debugInfoSelection.value();
		}
	}

	if (settings.contains("remappings") && !settings["remappings"].is_array())
		return formatFatalError(Error::Type::JSONError, "\"settings.remappings\" must be an array of strings.");

	for (auto const& remapping: settings.value("remappings", Json::object()))
	{
		if (!remapping.is_string())
			return formatFatalError(Error::Type::JSONError, "\"settings.remappings\" must be an array of strings");
		if (auto r = ImportRemapper::parseRemapping(remapping.get<std::string>()))
			ret.remappings.emplace_back(std::move(*r));
		else
			return formatFatalError(Error::Type::JSONError, "Invalid remapping: \"" + remapping.get<std::string>() + "\"");
	}

	if (settings.contains("optimizer"))
	{
		auto optimiserSettings = parseOptimizerSettings(ret.language, settings["optimizer"]);
		if (std::holds_alternative<Json>(optimiserSettings))
			return std::get<Json>(std::move(optimiserSettings)); // was an error
		else
			ret.optimiserSettings = std::get<OptimiserSettings>(std::move(optimiserSettings));
	}
	else if (ret.language == "EVMAssembly")
		ret.optimiserSettings = OptimiserSettings::none();
	else
		ret.optimiserSettings = OptimiserSettings::minimal();

	Json const& jsonLibraries = settings.value("libraries", Json::object());
	if (!jsonLibraries.is_object())
		return formatFatalError(Error::Type::JSONError, "\"libraries\" is not a JSON object.");
	for (auto const& [sourceName, jsonSourceName]: jsonLibraries.items())
	{
		if (!jsonSourceName.is_object())
			return formatFatalError(Error::Type::JSONError, "Library entry is not a JSON object.");
		for (auto const& [library, libraryValue]: jsonSourceName.items())
		{
			if (!libraryValue.is_string())
				return formatFatalError(Error::Type::JSONError, "Library address must be a string.");
			std::string address = libraryValue.get<std::string>();

			if (!boost::starts_with(address, "0x"))
				return formatFatalError(
					Error::Type::JSONError,
					"Library address is not prefixed with \"0x\"."
				);

			if (address.length() != 42)
				return formatFatalError(
					Error::Type::JSONError,
					"Library address is of invalid length."
				);

			try
			{
				ret.libraries[sourceName + ":" + library] = util::h160(address);
			}
			catch (util::BadHexCharacter const&)
			{
				return formatFatalError(
					Error::Type::JSONError,
					"Invalid library address (\"" + address + "\") supplied."
				);
			}
		}
	}

	Json const& metadataSettings = settings.value("metadata", Json::object());

	if (auto result = checkMetadataKeys(metadataSettings))
		return *result;

	solAssert(CompilerStack::defaultMetadataFormat() != CompilerStack::MetadataFormat::NoMetadata, "");
	ret.metadataFormat =
		metadataSettings.value("appendCBOR", Json(true)) ?
		CompilerStack::defaultMetadataFormat() :
		CompilerStack::MetadataFormat::NoMetadata;

	ret.metadataLiteralSources =
		metadataSettings.contains("useLiteralContent") &&
		metadataSettings["useLiteralContent"].is_boolean() &&
		metadataSettings["useLiteralContent"].get<bool>();
	if (metadataSettings.contains("bytecodeHash"))
	{
		auto metadataHash = metadataSettings["bytecodeHash"].get<std::string>();
		ret.metadataHash =
			metadataHash == "ipfs" ?
			CompilerStack::MetadataHash::IPFS :
				metadataHash == "bzzr1" ?
				CompilerStack::MetadataHash::Bzzr1 :
				CompilerStack::MetadataHash::None;
		if (ret.metadataFormat == CompilerStack::MetadataFormat::NoMetadata && ret.metadataHash != CompilerStack::MetadataHash::None)
			return formatFatalError(
				Error::Type::JSONError,
				"When the parameter \"appendCBOR\" is set to false, the parameter \"bytecodeHash\" cannot be set to \"" +
				metadataHash +
				"\". The parameter \"bytecodeHash\" should either be skipped, or set to \"none\"."
			);
	}

	Json const& outputSelection = settings.value("outputSelection", Json::object());

	if (auto jsonError = checkOutputSelection(outputSelection))
		return *jsonError;

	ret.outputSelection = outputSelection;

	if (ret.stopAfter != CompilerStack::State::CompilationSuccessful && isBinaryRequested(ret.outputSelection))
		return formatFatalError(
			Error::Type::JSONError,
			"Requested output selection conflicts with \"settings.stopAfter\"."
		);

	Json const& modelCheckerSettings = settings.value("modelChecker", Json::object());

	if (auto result = checkModelCheckerSettingsKeys(modelCheckerSettings))
		return *result;

	if (modelCheckerSettings.contains("contracts"))
	{
		auto const& sources = modelCheckerSettings["contracts"];
		if (!sources.is_object() && !sources.is_null())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.contracts is not a JSON object.");

		std::map<std::string, std::set<std::string>> sourceContracts;
		for (auto const& [source, contracts]: sources.items())
		{
			if (source.empty())
				return formatFatalError(Error::Type::JSONError, "Source name cannot be empty.");

			if (!contracts.is_array())
				return formatFatalError(Error::Type::JSONError, "Source contracts must be an array.");

			for (auto const& contract: contracts)
			{
				if (!contract.is_string())
					return formatFatalError(Error::Type::JSONError, "Every contract in settings.modelChecker.contracts must be a string.");
				if (contract.get<std::string>().empty())
					return formatFatalError(Error::Type::JSONError, "Contract name cannot be empty.");
				sourceContracts[source].insert(contract.get<std::string>());
			}

			if (sourceContracts[source].empty())
				return formatFatalError(Error::Type::JSONError, "Source contracts must be a non-empty array.");
		}
	}

	if (modelCheckerSettings.contains("divModNoSlacks"))
	{
		auto const& divModNoSlacks = modelCheckerSettings["divModNoSlacks"];
		if (!divModNoSlacks.is_boolean())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.divModNoSlacks must be a Boolean.");
	}

	if (modelCheckerSettings.contains("engine"))
	{
		if (!modelCheckerSettings["engine"].is_string())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.engine must be a string.");
	}

	if (modelCheckerSettings.contains("showProvedSafe"))
	{
		auto const& showProvedSafe = modelCheckerSettings["showProvedSafe"];
		if (!showProvedSafe.is_boolean())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showProvedSafe must be a Boolean value.");
	}

	if (modelCheckerSettings.contains("showUnproved"))
	{
		auto const& showUnproved = modelCheckerSettings["showUnproved"];
		if (!showUnproved.is_boolean())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showUnproved must be a Boolean value.");
	}

	if (modelCheckerSettings.contains("showUnsupported"))
	{
		auto const& showUnsupported = modelCheckerSettings["showUnsupported"];
		if (!showUnsupported.is_boolean())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showUnsupported must be a Boolean value.");
	}

	if (modelCheckerSettings.contains("solvers"))
	{
		auto const& solversArray = modelCheckerSettings["solvers"];
		if (!solversArray.is_array())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.solvers must be an array.");

		smtutil::SMTSolverChoice solvers;
		for (auto const& s: solversArray)
		{
			if (!s.is_string())
				return formatFatalError(Error::Type::JSONError, "Every target in settings.modelChecker.solvers must be a string.");
			if (!solvers.setSolver(s.get<std::string>()))
				return formatFatalError(Error::Type::JSONError, "Invalid model checker solvers requested.");
		}
	}

	if ((ret.debugInfoSelection.has_value() && ret.debugInfoSelection->ethdebug) || isEthdebugRequested(ret.outputSelection))
	{
		if (ret.language != "Solidity" && ret.language != "Yul")
			return formatFatalError(Error::Type::FatalError, "'settings.debug.debugInfo' 'ethdebug' is only supported for languages 'Solidity' and 'Yul'.");
	}

	if (isEthdebugRequested(ret.outputSelection))
	{
		if (ret.language == "Solidity" && !ret.viaIR)
			return formatFatalError(Error::Type::FatalError, "'evm.bytecode.ethdebug' or 'evm.deployedBytecode.ethdebug' can only be selected as output, if 'viaIR' was set.");

		if (!ret.debugInfoSelection.has_value())
		{
			ret.debugInfoSelection = DebugInfoSelection::Default();
			ret.debugInfoSelection->enable("ethdebug");
		}
		else
		{
			if (!ret.debugInfoSelection->ethdebug && ret.language == "Solidity")
				return formatFatalError(Error::Type::FatalError, "'ethdebug' needs to be enabled in 'settings.debug.debugInfo', if 'evm.bytecode.ethdebug' or 'evm.deployedBytecode.ethdebug' was selected as output.");
		}
	}

	if (
		ret.debugInfoSelection.has_value() && ret.debugInfoSelection->ethdebug && (ret.language == "Solidity" || ret.language == "Yul") &&
		!pipelineConfig(ret.outputSelection)[""][""].irCodegen && !isEthdebugRequested(ret.outputSelection)
	)
		return formatFatalError(Error::Type::FatalError, "'settings.debug.debugInfo' can only include 'ethdebug', if output 'ir', 'irOptimized', 'evm.bytecode.ethdebug', or 'evm.deployedBytecode.ethdebug' was selected.");

	if (isEthdebugRequested(ret.outputSelection))
		if (ret.optimiserSettings.runYulOptimiser)
			solUnimplemented("Optimization is not yet supported with ethdebug.");

	return {std::move(ret)};
}

std::map<std::string, Json> StandardCompiler::parseAstFromInput(StringMap const& _sources)
{
	std::map<std::string, Json> sourceJsons;
	for (auto const& [sourceName, sourceCode]: _sources)
	{
		Json ast;
		astAssert(util::jsonParseStrict(sourceCode, ast), "Input file could not be parsed to JSON");
		std::string astKey = ast.contains("ast") ? "ast" : "AST";

		astAssert(ast.contains(astKey), "astkey is not member");
		astAssert(ast[astKey]["nodeType"].get<std::string>() == "SourceUnit", "Top-level node should be a 'SourceUnit'");
		astAssert(sourceJsons.count(sourceName) == 0, "All sources must have unique names");
		sourceJsons.emplace(sourceName, std::move(ast[astKey]));
	}
	return sourceJsons;
}

Json StandardCompiler::compileSolidity(StandardCompiler::InputsAndSettings _inputsAndSettings)
{
	solAssert(_inputsAndSettings.jsonSources.empty());

	CompilerStack compilerStack(m_readFile);

	StringMap sourceList = std::move(_inputsAndSettings.sources);
	compilerStack.setSources(sourceList);
	compilerStack.setViaIR(_inputsAndSettings.viaIR);
	compilerStack.setRemappings(std::move(_inputsAndSettings.remappings));
	compilerStack.setOptimiserSettings(std::move(_inputsAndSettings.optimiserSettings));
	compilerStack.setRevertStringBehaviour(_inputsAndSettings.revertStrings);
	compilerStack.setLibraries(_inputsAndSettings.libraries);
	compilerStack.useMetadataLiteralSources(_inputsAndSettings.metadataLiteralSources);
	compilerStack.setMetadataFormat(_inputsAndSettings.metadataFormat);
	compilerStack.setMetadataHash(_inputsAndSettings.metadataHash);
	compilerStack.selectContracts(pipelineConfig(_inputsAndSettings.outputSelection));

	Json errors = std::move(_inputsAndSettings.errors);

	bool const binariesRequested = isBinaryRequested(_inputsAndSettings.outputSelection);

	// TVM specific settings
	if (sourceList.size() != 1) {
		formatFatalError(Error::Type::JSONError, "Only one source is allowed.");
	}
	compilerStack.setInputFile(sourceList.begin()->first);
	compilerStack.setMainContract(_inputsAndSettings.mainContract);
	compilerStack.setTVMVersion(_inputsAndSettings.tvmVersion);
	compilerStack.generateAbi();
	if (binariesRequested)
		compilerStack.generateCode();
	compilerStack.printFunctionIds();
	compilerStack.printPrivateFunctionIds();

	try
	{
		if (_inputsAndSettings.language == "SolidityAST")
		{
			try
			{
				compilerStack.importASTs(parseAstFromInput(sourceList));
				if (!compilerStack.analyze())
					errors.emplace_back(formatError(Error::Type::FatalError, "general", "Analysis of the AST failed."));
				if (binariesRequested)
					compilerStack.compile();
			}
			catch (util::Exception const& _exc)
			{
				solThrow(util::Exception, "Failed to import AST: "s + _exc.what());
			}
		}
		else
		{
			compilerStack.compile(CompilerStack::State::CompilationSuccessful, true);

			for (auto const& error: compilerStack.errors())
				errors.emplace_back(formatErrorWithException(
					compilerStack,
					*error,
					error->type(),
					"general",
					"",
					error->errorId()
				));
		}
	}
	// NOTE: This includes langutil::StackTooDeepError.
	catch (CompilerError const& _exception)
	{
		errors.emplace_back(formatErrorWithException(
			compilerStack,
			_exception,
			Error::Type::CompilerError,
			"general",
			"Compiler error (" + _exception.lineInfo() + ")"
		));
	}
	catch (InternalCompilerError const&)
	{
		errors.emplace_back(formatError(
			Error::Type::InternalCompilerError,
			"general",
			"Internal compiler error:\n" + boost::current_exception_diagnostic_information()
		));
	}
	catch (UnimplementedFeatureError const& _exception)
	{
		// let StandardCompiler::compile handle this
		throw _exception;
	}
	catch (smtutil::SMTLogicError const&)
	{
		errors.emplace_back(formatError(
			Error::Type::SMTLogicException,
			"general",
			"SMT logic error:\n" + boost::current_exception_diagnostic_information()
		));
	}
	catch (...)
	{
		errors.emplace_back(formatError(
			Error::Type::Exception,
			"general",
			"Unknown exception during compilation: " + boost::current_exception_diagnostic_information()
		));
	}

	bool parsingSuccess = compilerStack.state() >= CompilerStack::State::Parsed;
	bool analysisSuccess = compilerStack.state() >= CompilerStack::State::AnalysisSuccessful;
	bool compilationSuccess = compilerStack.state() == CompilerStack::State::CompilationSuccessful;

	// If analysis fails, the artifacts inside CompilerStack are potentially incomplete and must not be returned.
	// Note that not completing analysis due to stopAfter does not count as a failure. It's neither failure nor success.
	bool analysisFailed = !analysisSuccess && _inputsAndSettings.stopAfter >= CompilerStack::State::AnalysisSuccessful;
	bool compilationFailed = !compilationSuccess && binariesRequested;
	if (compilationFailed || analysisFailed || !parsingSuccess)
		solAssert(!errors.empty(), "No error reported, but compilation failed.");

	Json output;

	if (errors.size() > 0)
		output["errors"] = std::move(errors);

	bool const wildcardMatchesExperimental = false;

	output["sources"] = Json::object();
	unsigned sourceIndex = 0;
	// NOTE: A case that will pass `parsingSuccess && !analysisFailed` but not `analysisSuccess` is
	// stopAfter: parsing with no parsing errors.
	if (parsingSuccess && !analysisFailed)
		for (std::string const& sourceName: compilerStack.sourceNames())
		{
			Json sourceResult;
			sourceResult["id"] = sourceIndex++;
			if (isArtifactRequested(_inputsAndSettings.outputSelection, sourceName, "", "ast", wildcardMatchesExperimental))
				sourceResult["ast"] = ASTJsonExporter(compilerStack.state(), compilerStack.sourceIndices()).toJson(compilerStack.ast(sourceName));
			output["sources"][sourceName] = sourceResult;
		}

	Json contractsOutput;
	for (std::string const& contractName: analysisSuccess ? compilerStack.mainContractName() : std::vector<std::string>())
	{
		size_t colon = contractName.rfind(':');
		solAssert(colon != std::string::npos, "");
		std::string file = contractName.substr(0, colon);
		std::string name = contractName.substr(colon + 1);

		Json contractData;
		if (compilationSuccess)
		{
			contractData["abi"] = compilerStack.contractABI(contractName);
			contractData["evm.assembly"] = compilerStack.contractCode(contractName);
			contractData["functionIds"] = compilerStack.functionIds(contractName);
			contractData["privateFunctionIds"] = compilerStack.privateFunctionIds(contractName);
			contractData["metadata"] = compilerStack.metadata(contractName);
			contractData["userdoc"] = compilerStack.natspecUser(contractName);
			contractData["devdoc"] = compilerStack.natspecDev(contractName);
		}

		if (!contractData.empty())
		{
			if (!contractsOutput.contains(file))
				contractsOutput[file] = Json::object();
			contractsOutput[file][name] = contractData;
		}
	}

	if (!contractsOutput.empty())
		output["contracts"] = contractsOutput;

	return output;
}

Json StandardCompiler::compile(Json const& _input) noexcept
{
	try
	{
		auto parsed = parseInput(_input);
		if (std::holds_alternative<Json>(parsed))
			return std::get<Json>(std::move(parsed));
		InputsAndSettings settings = std::get<InputsAndSettings>(std::move(parsed));
		if (settings.language == "Solidity")
			return compileSolidity(std::move(settings));
		else if (settings.language == "SolidityAST")
			return compileSolidity(std::move(settings));
		else
			return formatFatalError(Error::Type::JSONError, "Only \"Solidity\", \"Yul\", \"SolidityAST\" or \"EVMAssembly\" is supported as a language.");
	}
	catch (UnimplementedFeatureError const& _exception)
	{
		solAssert(_exception.comment(), "Unimplemented feature errors must include a message for the user");
		return formatFatalError(Error::Type::UnimplementedFeatureError, stringOrDefault(_exception.comment()));
	}
	catch (...)
	{
		return formatFatalError(
			Error::Type::InternalCompilerError,
			"Uncaught exception:\n" + boost::current_exception_diagnostic_information()
		);
	}
}

std::string StandardCompiler::compile(std::string const& _input) noexcept
{
	Json input;
	std::string errors;
	try
	{
		if (!util::jsonParseStrict(_input, input, &errors))
			return util::jsonPrint(formatFatalError(Error::Type::JSONError, errors), m_jsonPrintingFormat);
	}
	catch (...)
	{
		if (errors.empty())
			return "{\"errors\":[{\"type\":\"JSONError\",\"component\":\"general\",\"severity\":\"error\",\"message\":\"Error parsing input JSON.\"}]}";
		else
			return "{\"errors\":[{\"type\":\"JSONError\",\"component\":\"general\",\"severity\":\"error\",\"message\":\"Error parsing input JSON: " + errors + "\"}]}";
	}

//	std::cout << "Input: " << solidity::util::jsonPrettyPrint(input) << std::endl;
	Json output = compile(input);
//	std::cout << "Output: " << solidity::util::jsonPrettyPrint(output) << std::endl;

	try
	{
		return util::jsonPrint(output, m_jsonPrintingFormat);
	}
	catch (...)
	{
		return "{\"errors\":[{\"type\":\"JSONError\",\"component\":\"general\",\"severity\":\"error\",\"message\":\"Error writing output JSON.\"}]}";
	}
}
