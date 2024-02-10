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

#include <boost/algorithm/cxx11/any_of.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <algorithm>
#include <optional>

using namespace solidity;
using namespace solidity::yul;
using namespace solidity::frontend;
using namespace solidity::langutil;
using namespace std::string_literals;

namespace
{

Json::Value formatError(
	Error::Type _type,
	std::string const& _component,
	std::string const& _message,
	std::string const& _formattedMessage = "",
	std::string const& _humanFormattedMessage = "",
	Json::Value const& _sourceLocation = Json::Value(),
	Json::Value const& _secondarySourceLocation = Json::Value()
)
{
	Json::Value error{Json::objectValue};
	error["type"] = Error::formatErrorType(_type);
	error["component"] = _component;
	error["severity"] = Error::formatErrorSeverityLowercase(Error::errorSeverity(_type));
	error["message"] = _message;
	error["formattedMessage"] = (_formattedMessage.length() > 0) ? _formattedMessage : _message;
	error["humanFormattedMessage"] = (_humanFormattedMessage.length() > 0) ? _humanFormattedMessage : _message;
	if (_sourceLocation.isObject())
		error["sourceLocation"] = _sourceLocation;
	if (_secondarySourceLocation.isArray())
		error["secondarySourceLocations"] = _secondarySourceLocation;
	return error;
}

Json::Value formatFatalError(Error::Type _type, std::string const& _message)
{
	Json::Value output{Json::objectValue};
	output["errors"] = Json::arrayValue;
	output["errors"].append(formatError(_type, "general", _message));
	return output;
}

Json::Value formatSourceLocation(SourceLocation const* location)
{
	if (!location || !location->sourceName)
		return Json::nullValue;

	Json::Value sourceLocation{Json::objectValue};
	sourceLocation["file"] = *location->sourceName;
	sourceLocation["start"] = location->start;
	sourceLocation["end"] = location->end;
	return sourceLocation;
}

Json::Value formatSecondarySourceLocation(SecondarySourceLocation const* _secondaryLocation)
{
	if (!_secondaryLocation)
		return Json::nullValue;

	Json::Value secondarySourceLocation{Json::arrayValue};
	for (auto const& location: _secondaryLocation->infos)
	{
		Json::Value msg = formatSourceLocation(&location.second);
		msg["message"] = location.first;
		secondarySourceLocation.append(msg);
	}
	return secondarySourceLocation;
}

Json::Value formatErrorWithException(
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
		message = ((_message.length() > 0) ? (_message + ":") : "") + *description;
	else
		message = _message;

	Json::Value error = formatError(
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

std::map<std::string, std::set<std::string>> requestedContractNames(Json::Value const& _outputSelection)
{
	std::map<std::string, std::set<std::string>> contracts;
	for (auto const& sourceName: _outputSelection.getMemberNames())
	{
		std::string key = (sourceName == "*") ? "" : sourceName;
		for (auto const& contractName: _outputSelection[sourceName].getMemberNames())
		{
			std::string value = (contractName == "*") ? "" : contractName;
			contracts[key].insert(value);
		}
	}
	return contracts;
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

bool isArtifactRequested(Json::Value const& _outputSelection, std::string const& _artifact, bool _wildcardMatchesExperimental)
{
	static std::set<std::string> experimental{"ir", "irAst", "irOptimized", "irOptimizedAst"};
	for (auto const& selectedArtifactJson: _outputSelection)
	{
		std::string const& selectedArtifact = selectedArtifactJson.asString();
		if (
			_artifact == selectedArtifact ||
			boost::algorithm::starts_with(_artifact, selectedArtifact + ".")
		)
			return true;
		else if (selectedArtifact == "*")
		{
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
bool isArtifactRequested(Json::Value const& _outputSelection, std::string const& _file, std::string const& _contract, std::string const& _artifact, bool _wildcardMatchesExperimental)
{
	if (!_outputSelection.isObject())
		return false;

	for (auto const& file: { _file, std::string("*") })
		if (_outputSelection.isMember(file) && _outputSelection[file].isObject())
		{
			/// For SourceUnit-level targets (such as AST) only allow empty name, otherwise
			/// for Contract-level targets try both contract name and wildcard
			std::vector<std::string> contracts{ _contract };
			if (!_contract.empty())
				contracts.emplace_back("*");
			for (auto const& contract: contracts)
				if (
					_outputSelection[file].isMember(contract) &&
					_outputSelection[file][contract].isArray() &&
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
	std::vector<std::string> components{"", ".object", ".opcodes", ".sourceMap", ".functionDebugData", ".generatedSources", ".linkReferences"};
	if (_objectKind == "deployedBytecode")
		components.push_back(".immutableReferences");
	return util::applyMap(components, [&](auto const& _s) { return "evm." + _objectKind + _s; });
}

/// @returns true if any binary was requested, i.e. we actually have to perform compilation.
bool isBinaryRequested(Json::Value const& _outputSelection)
{
	if (!_outputSelection.isObject())
		return false;

	// This does not include "evm.methodIdentifiers" on purpose!
	static std::vector<std::string> const outputsThatRequireBinaries = std::vector<std::string>{
		"*",
		"assembly",
		"ir", "irAst", "irOptimized", "irOptimizedAst",
		"evm.gasEstimates", "evm.legacyAssembly", "evm.assembly"
	} + evmObjectComponents("bytecode") + evmObjectComponents("deployedBytecode");

	for (auto const& fileRequests: _outputSelection)
		for (auto const& requests: fileRequests)
			for (auto const& output: outputsThatRequireBinaries)
				if (isArtifactRequested(requests, output, false))
					return true;
	return false;
}

/// @returns true if EVM bytecode was requested, i.e. we have to run the old code generator.
bool isEvmBytecodeRequested(Json::Value const& _outputSelection)
{
	if (!_outputSelection.isObject())
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

/// @returns true if any Yul IR was requested. Note that as an exception, '*' does not
/// yet match "ir", "irAst", "irOptimized" or "irOptimizedAst"
bool isIRRequested(Json::Value const& _outputSelection)
{
	if (!_outputSelection.isObject())
		return false;

	for (auto const& fileRequests: _outputSelection)
		for (auto const& requests: fileRequests)
			for (auto const& request: requests)
				if (
					request == "ir" ||
					request == "irAst" ||
					request == "irOptimized" ||
					request == "irOptimizedAst"
				)
					return true;

	return false;
}

std::optional<Json::Value> checkKeys(Json::Value const& _input, std::set<std::string> const& _keys, std::string const& _name)
{
	if (!!_input && !_input.isObject())
		return formatFatalError(Error::Type::JSONError, "\"" + _name + "\" must be an object");

	for (auto const& member: _input.getMemberNames())
		if (!_keys.count(member))
			return formatFatalError(Error::Type::JSONError, "Unknown key \"" + member + "\"");

	return std::nullopt;
}

std::optional<Json::Value> checkRootKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"auxiliaryInput", "language", "settings", "sources"};
	return checkKeys(_input, keys, "root");
}

std::optional<Json::Value> checkSourceKeys(Json::Value const& _input, std::string const& _name)
{
	static std::set<std::string> keys{"content", "keccak256", "urls"};
	return checkKeys(_input, keys, "sources." + _name);
}

std::optional<Json::Value> checkAuxiliaryInputKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"smtlib2responses"};
	return checkKeys(_input, keys, "auxiliaryInput");
}

std::optional<Json::Value> checkSettingsKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"debug", "evmVersion", "libraries", "metadata", "modelChecker", "optimizer", "outputSelection", "remappings", "stopAfter", "viaIR",
									  "includePaths", "mainContract", "tvmVersion"};
	return checkKeys(_input, keys, "settings");
}

std::optional<Json::Value> checkModelCheckerSettingsKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"bmcLoopIterations", "contracts", "divModNoSlacks", "engine", "extCalls", "invariants", "printQuery", "showProvedSafe", "showUnproved", "showUnsupported", "solvers", "targets", "timeout"};
	return checkKeys(_input, keys, "modelChecker");
}

std::optional<Json::Value> checkOptimizerKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"details", "enabled", "runs"};
	return checkKeys(_input, keys, "settings.optimizer");
}

std::optional<Json::Value> checkOptimizerDetailsKeys(Json::Value const& _input)
{
	static std::set<std::string> keys{"peephole", "inliner", "jumpdestRemover", "orderLiterals", "deduplicate", "cse", "constantOptimizer", "yul", "yulDetails", "simpleCounterForLoopUncheckedIncrement"};
	return checkKeys(_input, keys, "settings.optimizer.details");
}

std::optional<Json::Value> checkOptimizerDetail(Json::Value const& _details, std::string const& _name, bool& _setting)
{
	if (_details.isMember(_name))
	{
		if (!_details[_name].isBool())
			return formatFatalError(Error::Type::JSONError, "\"settings.optimizer.details." + _name + "\" must be Boolean");
		_setting = _details[_name].asBool();
	}
	return {};
}

std::optional<Json::Value> checkMetadataKeys(Json::Value const& _input)
{
	if (_input.isObject())
	{
		if (_input.isMember("appendCBOR") && !_input["appendCBOR"].isBool())
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.appendCBOR\" must be Boolean");
		if (_input.isMember("useLiteralContent") && !_input["useLiteralContent"].isBool())
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.useLiteralContent\" must be Boolean");

		static std::set<std::string> hashes{"ipfs", "bzzr1", "none"};
		if (_input.isMember("bytecodeHash") && !hashes.count(_input["bytecodeHash"].asString()))
			return formatFatalError(Error::Type::JSONError, "\"settings.metadata.bytecodeHash\" must be \"ipfs\", \"bzzr1\" or \"none\"");
	}
	static std::set<std::string> keys{"appendCBOR", "useLiteralContent", "bytecodeHash"};
	return checkKeys(_input, keys, "settings.metadata");
}

std::optional<Json::Value> checkOutputSelection(Json::Value const& _outputSelection)
{
	if (!!_outputSelection && !_outputSelection.isObject())
		return formatFatalError(Error::Type::JSONError, "\"settings.outputSelection\" must be an object");

	for (auto const& sourceName: _outputSelection.getMemberNames())
	{
		auto const& sourceVal = _outputSelection[sourceName];

		if (!sourceVal.isObject())
			return formatFatalError(
				Error::Type::JSONError,
				"\"settings.outputSelection." + sourceName + "\" must be an object"
			);

		for (auto const& contractName: sourceVal.getMemberNames())
		{
			auto const& contractVal = sourceVal[contractName];

			if (!contractVal.isArray())
				return formatFatalError(
					Error::Type::JSONError,
					"\"settings.outputSelection." +
					sourceName +
					"." +
					contractName +
					"\" must be a string array"
				);

			for (auto const& output: contractVal)
				if (!output.isString())
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
std::variant<OptimiserSettings, Json::Value> parseOptimizerSettings(Json::Value const& _jsonInput)
{
	if (auto result = checkOptimizerKeys(_jsonInput))
		return *result;

	OptimiserSettings settings = OptimiserSettings::minimal();

	if (_jsonInput.isMember("enabled"))
	{
		if (!_jsonInput["enabled"].isBool())
			return formatFatalError(Error::Type::JSONError, "The \"enabled\" setting must be a Boolean.");

		if (_jsonInput["enabled"].asBool())
			settings = OptimiserSettings::standard();
	}

	if (_jsonInput.isMember("runs"))
	{
		if (!_jsonInput["runs"].isUInt())
			return formatFatalError(Error::Type::JSONError, "The \"runs\" setting must be an unsigned number.");
		settings.expectedExecutionsPerDeployment = _jsonInput["runs"].asUInt();
	}

	if (_jsonInput.isMember("details"))
	{
		Json::Value const& details = _jsonInput["details"];
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
		if (details.isMember("yulDetails"))
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

std::variant<StandardCompiler::InputsAndSettings, Json::Value> StandardCompiler::parseInput(Json::Value const& _input)
{
	InputsAndSettings ret;

	if (!_input.isObject())
		return formatFatalError(Error::Type::JSONError, "Input is not a JSON object.");

	if (auto result = checkRootKeys(_input))
		return *result;

	ret.language = _input["language"].asString();

	Json::Value const& sources = _input["sources"];

	if (!sources.isObject() && !sources.isNull())
		return formatFatalError(Error::Type::JSONError, "\"sources\" is not a JSON object.");

	if (sources.empty())
		return formatFatalError(Error::Type::JSONError, "No input sources specified.");

	ret.errors = Json::arrayValue;

	if (ret.language == "Solidity" || ret.language == "Yul")
	{
		for (auto const& sourceName: sources.getMemberNames())
		{
			std::string hash;

			if (auto result = checkSourceKeys(sources[sourceName], sourceName))
				return *result;

			if (sources[sourceName]["keccak256"].isString())
				hash = sources[sourceName]["keccak256"].asString();

			if (sources[sourceName]["content"].isString())
			{
				std::string content = sources[sourceName]["content"].asString();
				if (!hash.empty() && !hashMatchesContent(hash, content))
					ret.errors.append(formatError(
						Error::Type::IOError,
						"general",
						"Mismatch between content and supplied hash for \"" + sourceName + "\""
					));
				else
					ret.sources[sourceName] = content;
			}
			else if (sources[sourceName]["urls"].isArray())
			{
				if (!m_readFile)
					return formatFatalError(
						Error::Type::JSONError, "No import callback supplied, but URL is requested."
					);

				std::vector<std::string> failures;
				bool found = false;

				for (auto const& url: sources[sourceName]["urls"])
				{
					if (!url.isString())
						return formatFatalError(Error::Type::JSONError, "URL must be a string.");
					ReadCallback::Result result = m_readFile(ReadCallback::kindString(ReadCallback::Kind::ReadFile), url.asString());
					if (result.success)
					{
						if (!hash.empty() && !hashMatchesContent(hash, result.responseOrErrorMessage))
							ret.errors.append(formatError(
								Error::Type::IOError,
								"general",
								"Mismatch between content and supplied hash for \"" + sourceName + "\" at \"" + url.asString() + "\""
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
							"Cannot import url (\"" + url.asString() + "\"): " + result.responseOrErrorMessage
						);
				}

				for (auto const& failure: failures)
				{
					/// If the import succeeded, let mark all the others as warnings, otherwise all of them are errors.
					ret.errors.append(formatError(
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
		for (auto const& sourceName: sources.getMemberNames())
			ret.sources[sourceName] = util::jsonCompactPrint(sources[sourceName]);
	}
	else if (ret.language == "EVMAssembly")
	{
		for (std::string const& sourceName: sources.getMemberNames())
		{
			solAssert(sources.isMember(sourceName));
			if (
				!sources[sourceName].isMember("assemblyJson") ||
				!sources[sourceName]["assemblyJson"].isObject() ||
				sources[sourceName].size() != 1
			)
				return formatFatalError(
					Error::Type::JSONError,
					"Invalid input source specified. Expected exactly one object, named 'assemblyJson', inside $.sources." + sourceName
				);

			ret.jsonSources[sourceName] = sources[sourceName]["assemblyJson"];
		}
		if (ret.jsonSources.size() != 1)
			return formatFatalError(
				Error::Type::JSONError,
				"EVMAssembly import only supports exactly one input file."
			);
	}
	Json::Value const& auxInputs = _input["auxiliaryInput"];

	if (auto result = checkAuxiliaryInputKeys(auxInputs))
		return *result;

	if (!!auxInputs)
	{
		Json::Value const& smtlib2Responses = auxInputs["smtlib2responses"];
		if (!!smtlib2Responses)
		{
			if (!smtlib2Responses.isObject())
				return formatFatalError(Error::Type::JSONError, "\"auxiliaryInput.smtlib2responses\" must be an object.");

			for (auto const& hashString: smtlib2Responses.getMemberNames())
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

				if (!smtlib2Responses[hashString].isString())
					return formatFatalError(
						Error::Type::JSONError,
						"\"smtlib2Responses." + hashString + "\" must be a string."
					);

				ret.smtLib2Responses[hash] = smtlib2Responses[hashString].asString();
			}
		}
	}

	Json::Value const& settings = _input.get("settings", Json::Value());

	if (auto result = checkSettingsKeys(settings))
		return *result;

	if (settings.isMember("includePaths"))
	{
		for (auto const& includePath: settings["includePaths"])
		{
			if (!includePath.isString())
				return formatFatalError(Error::Type::JSONError, "Include path must be a string.");
			ret.includePaths.push_back(includePath.asString());
		}
	}

	if (settings.isMember("mainContract"))
	{
		if (!settings["mainContract"].isString())
			return formatFatalError(Error::Type::JSONError, "\"settings.mainContract\" must be a String.");
		ret.mainContract = settings["mainContract"].asString();
	}

	if (settings.isMember("stopAfter"))
	{
		if (!settings["stopAfter"].isString())
			return formatFatalError(Error::Type::JSONError, "\"settings.stopAfter\" must be a string.");

		if (settings["stopAfter"].asString() != "parsing")
			return formatFatalError(Error::Type::JSONError, "Invalid value for \"settings.stopAfter\". Only valid value is \"parsing\".");

		ret.stopAfter = CompilerStack::State::Parsed;
	}

	if (settings.isMember("viaIR"))
	{
		if (!settings["viaIR"].isBool())
			return formatFatalError(Error::Type::JSONError, "\"settings.viaIR\" must be a Boolean.");
		ret.viaIR = settings["viaIR"].asBool();
	}

	if (settings.isMember("evmVersion"))
	{
		if (!settings["evmVersion"].isString())
			return formatFatalError(Error::Type::JSONError, "evmVersion must be a string.");
		std::optional<langutil::EVMVersion> version = langutil::EVMVersion::fromString(settings["evmVersion"].asString());
		if (!version)
			return formatFatalError(Error::Type::JSONError, "Invalid EVM version requested.");
		if (version < EVMVersion::constantinople())
			ret.errors.append(formatError(
				Error::Type::Warning,
				"general",
				"Support for EVM versions older than constantinople is deprecated and will be removed in the future."
			));
		ret.evmVersion = *version;
	}

	if (settings.isMember("tvmVersion"))
	{
		if (!settings["tvmVersion"].isString())
			return formatFatalError(Error::Type::JSONError, "tvmVersion must be a string.");
		std::optional<langutil::TVMVersion> version = langutil::TVMVersion::fromString(settings["tvmVersion"].asString());
		if (!version)
			return formatFatalError(Error::Type::JSONError, "Invalid TVM version requested.");
		ret.tvmVersion = *version;
	}

	if (settings.isMember("debug"))
	{
		if (auto result = checkKeys(settings["debug"], {"revertStrings", "debugInfo"}, "settings.debug"))
			return *result;

		if (settings["debug"].isMember("revertStrings"))
		{
			if (!settings["debug"]["revertStrings"].isString())
				return formatFatalError(Error::Type::JSONError, "settings.debug.revertStrings must be a string.");
			std::optional<RevertStrings> revertStrings = revertStringsFromString(settings["debug"]["revertStrings"].asString());
			if (!revertStrings)
				return formatFatalError(Error::Type::JSONError, "Invalid value for settings.debug.revertStrings.");
			if (*revertStrings == RevertStrings::VerboseDebug)
				return formatFatalError(
					Error::Type::UnimplementedFeatureError,
					"Only \"default\", \"strip\" and \"debug\" are implemented for settings.debug.revertStrings for now."
				);
			ret.revertStrings = *revertStrings;
		}

		if (settings["debug"].isMember("debugInfo"))
		{
			if (!settings["debug"]["debugInfo"].isArray())
				return formatFatalError(Error::Type::JSONError, "settings.debug.debugInfo must be an array.");

			std::vector<std::string> components;
			for (Json::Value const& arrayValue: settings["debug"]["debugInfo"])
				components.push_back(arrayValue.asString());

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

	if (settings.isMember("remappings") && !settings["remappings"].isArray())
		return formatFatalError(Error::Type::JSONError, "\"settings.remappings\" must be an array of strings.");

	for (auto const& remapping: settings.get("remappings", Json::Value()))
	{
		if (!remapping.isString())
			return formatFatalError(Error::Type::JSONError, "\"settings.remappings\" must be an array of strings");
		if (auto r = ImportRemapper::parseRemapping(remapping.asString()))
			ret.remappings.emplace_back(std::move(*r));
		else
			return formatFatalError(Error::Type::JSONError, "Invalid remapping: \"" + remapping.asString() + "\"");
	}

	if (settings.isMember("optimizer"))
	{
		auto optimiserSettings = parseOptimizerSettings(settings["optimizer"]);
		if (std::holds_alternative<Json::Value>(optimiserSettings))
			return std::get<Json::Value>(std::move(optimiserSettings)); // was an error
		else
			ret.optimiserSettings = std::get<OptimiserSettings>(std::move(optimiserSettings));
	}

	Json::Value jsonLibraries = settings.get("libraries", Json::Value(Json::objectValue));
	if (!jsonLibraries.isObject())
		return formatFatalError(Error::Type::JSONError, "\"libraries\" is not a JSON object.");
	for (auto const& sourceName: jsonLibraries.getMemberNames())
	{
		auto const& jsonSourceName = jsonLibraries[sourceName];
		if (!jsonSourceName.isObject())
			return formatFatalError(Error::Type::JSONError, "Library entry is not a JSON object.");
		for (auto const& library: jsonSourceName.getMemberNames())
		{
			if (!jsonSourceName[library].isString())
				return formatFatalError(Error::Type::JSONError, "Library address must be a string.");
			std::string address = jsonSourceName[library].asString();

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

	Json::Value metadataSettings = settings.get("metadata", Json::Value());

	if (auto result = checkMetadataKeys(metadataSettings))
		return *result;

	solAssert(CompilerStack::defaultMetadataFormat() != CompilerStack::MetadataFormat::NoMetadata, "");
	ret.metadataFormat =
		metadataSettings.get("appendCBOR", Json::Value(true)).asBool() ?
		CompilerStack::defaultMetadataFormat() :
		CompilerStack::MetadataFormat::NoMetadata;

	ret.metadataLiteralSources = metadataSettings.get("useLiteralContent", Json::Value(false)).asBool();
	if (metadataSettings.isMember("bytecodeHash"))
	{
		auto metadataHash = metadataSettings["bytecodeHash"].asString();
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

	Json::Value outputSelection = settings.get("outputSelection", Json::Value());

	if (auto jsonError = checkOutputSelection(outputSelection))
		return *jsonError;

	ret.outputSelection = std::move(outputSelection);

	if (ret.stopAfter != CompilerStack::State::CompilationSuccessful && isBinaryRequested(ret.outputSelection))
		return formatFatalError(
			Error::Type::JSONError,
			"Requested output selection conflicts with \"settings.stopAfter\"."
		);

	Json::Value const& modelCheckerSettings = settings.get("modelChecker", Json::Value());

	if (auto result = checkModelCheckerSettingsKeys(modelCheckerSettings))
		return *result;

	if (modelCheckerSettings.isMember("contracts"))
	{
		auto const& sources = modelCheckerSettings["contracts"];
		if (!sources.isObject() && !sources.isNull())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.contracts is not a JSON object.");

		std::map<std::string, std::set<std::string>> sourceContracts;
		for (auto const& source: sources.getMemberNames())
		{
			if (source.empty())
				return formatFatalError(Error::Type::JSONError, "Source name cannot be empty.");

			auto const& contracts = sources[source];
			if (!contracts.isArray())
				return formatFatalError(Error::Type::JSONError, "Source contracts must be an array.");

			for (auto const& contract: contracts)
			{
				if (!contract.isString())
					return formatFatalError(Error::Type::JSONError, "Every contract in settings.modelChecker.contracts must be a string.");
				if (contract.asString().empty())
					return formatFatalError(Error::Type::JSONError, "Contract name cannot be empty.");
				sourceContracts[source].insert(contract.asString());
			}

			if (sourceContracts[source].empty())
				return formatFatalError(Error::Type::JSONError, "Source contracts must be a non-empty array.");
		}
	}

	if (modelCheckerSettings.isMember("divModNoSlacks"))
	{
		auto const& divModNoSlacks = modelCheckerSettings["divModNoSlacks"];
		if (!divModNoSlacks.isBool())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.divModNoSlacks must be a Boolean.");
	}

	if (modelCheckerSettings.isMember("engine"))
	{
		if (!modelCheckerSettings["engine"].isString())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.engine must be a string.");
	}

	if (modelCheckerSettings.isMember("showProvedSafe"))
	{
		auto const& showProvedSafe = modelCheckerSettings["showProvedSafe"];
		if (!showProvedSafe.isBool())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showProvedSafe must be a Boolean value.");
	}

	if (modelCheckerSettings.isMember("showUnproved"))
	{
		auto const& showUnproved = modelCheckerSettings["showUnproved"];
		if (!showUnproved.isBool())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showUnproved must be a Boolean value.");
	}

	if (modelCheckerSettings.isMember("showUnsupported"))
	{
		auto const& showUnsupported = modelCheckerSettings["showUnsupported"];
		if (!showUnsupported.isBool())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.showUnsupported must be a Boolean value.");
	}

	if (modelCheckerSettings.isMember("solvers"))
	{
		auto const& solversArray = modelCheckerSettings["solvers"];
		if (!solversArray.isArray())
			return formatFatalError(Error::Type::JSONError, "settings.modelChecker.solvers must be an array.");

		smtutil::SMTSolverChoice solvers;
		for (auto const& s: solversArray)
		{
			if (!s.isString())
				return formatFatalError(Error::Type::JSONError, "Every target in settings.modelChecker.solvers must be a string.");
			if (!solvers.setSolver(s.asString()))
				return formatFatalError(Error::Type::JSONError, "Invalid model checker solvers requested.");
		}

	}

	return {std::move(ret)};
}

std::map<std::string, Json::Value> StandardCompiler::parseAstFromInput(StringMap const& _sources)
{
	std::map<std::string, Json::Value> sourceJsons;
	for (auto const& [sourceName, sourceCode]: _sources)
	{
		Json::Value ast;
		astAssert(util::jsonParseStrict(sourceCode, ast), "Input file could not be parsed to JSON");
		std::string astKey = ast.isMember("ast") ? "ast" : "AST";

		astAssert(ast.isMember(astKey), "astkey is not member");
		astAssert(ast[astKey]["nodeType"].asString() == "SourceUnit", "Top-level node should be a 'SourceUnit'");
		astAssert(sourceJsons.count(sourceName) == 0, "All sources must have unique names");
		sourceJsons.emplace(sourceName, std::move(ast[astKey]));
	}
	return sourceJsons;
}

Json::Value StandardCompiler::compileSolidity(StandardCompiler::InputsAndSettings _inputsAndSettings)
{
	solAssert(_inputsAndSettings.jsonSources.empty());

	CompilerStack compilerStack(m_readFile);

	StringMap sourceList = std::move(_inputsAndSettings.sources);
	compilerStack.setSources(sourceList);
	// TODO DELETE: do we need EVMVersion and other stuff?
	compilerStack.setEVMVersion(_inputsAndSettings.evmVersion);
	compilerStack.setViaIR(_inputsAndSettings.viaIR);
	compilerStack.setEVMVersion(_inputsAndSettings.evmVersion);
	compilerStack.setRemappings(std::move(_inputsAndSettings.remappings));
	compilerStack.setOptimiserSettings(std::move(_inputsAndSettings.optimiserSettings));
	compilerStack.setRevertStringBehaviour(_inputsAndSettings.revertStrings);
	compilerStack.setLibraries(_inputsAndSettings.libraries);
	compilerStack.useMetadataLiteralSources(_inputsAndSettings.metadataLiteralSources);
	compilerStack.setMetadataFormat(_inputsAndSettings.metadataFormat);
	compilerStack.setMetadataHash(_inputsAndSettings.metadataHash);
	compilerStack.setRequestedContractNames(requestedContractNames(_inputsAndSettings.outputSelection));

	compilerStack.enableEvmBytecodeGeneration(isEvmBytecodeRequested(_inputsAndSettings.outputSelection));
	compilerStack.enableIRGeneration(isIRRequested(_inputsAndSettings.outputSelection));

	Json::Value errors = std::move(_inputsAndSettings.errors);

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
					errors.append(formatError(Error::Type::FatalError, "general", "Analysis of the AST failed."));
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
			compilerStack.compile(true);

			for (auto const& error: compilerStack.errors())
				errors.append(formatErrorWithException(
					compilerStack,
					*error,
					error->type(),
					"general",
					"",
					error->errorId()
				));
		}
	}
	/// This is only thrown in a very few locations.
	catch (Error const& _error)
	{
		errors.append(formatErrorWithException(
			compilerStack,
			_error,
			_error.type(),
			"general",
			"Uncaught error: "
		));
	}
	/// This should not be leaked from compile().
	catch (FatalError const& _exception)
	{
		errors.append(formatError(
			Error::Type::FatalError,
			"general",
			"Uncaught fatal error: " + boost::diagnostic_information(_exception)
		));
	}
	catch (CompilerError const& _exception)
	{
		errors.append(formatErrorWithException(
			compilerStack,
			_exception,
			Error::Type::CompilerError,
			"general",
			"Compiler error (" + _exception.lineInfo() + ")"
		));
	}
	catch (InternalCompilerError const& _exception)
	{
		errors.append(formatErrorWithException(
			compilerStack,
			_exception,
			Error::Type::InternalCompilerError,
			"general",
			"Internal compiler error (" + _exception.lineInfo() + ")"
		));
	}
	catch (UnimplementedFeatureError const& _exception)
	{
		errors.append(formatErrorWithException(
			compilerStack,
			_exception,
			Error::Type::UnimplementedFeatureError,
			"general",
			"Unimplemented feature (" + _exception.lineInfo() + ")"
		));
	}
	catch (util::Exception const& _exception)
	{
		errors.append(formatError(
			Error::Type::Exception,
			"general",
			"Exception during compilation: " + boost::diagnostic_information(_exception)
		));
	}
	catch (std::exception const& _exception)
	{
		errors.append(formatError(
			Error::Type::Exception,
			"general",
			"Unknown exception during compilation: " + boost::diagnostic_information(_exception)
		));
	}
	catch (...)
	{
		errors.append(formatError(
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

	/// Inconsistent state - stop here to receive error reports from users
	if (
		(compilationFailed || analysisFailed || !parsingSuccess) &&
		errors.empty()
	)
		return formatFatalError(Error::Type::InternalCompilerError, "No error reported, but compilation failed.");

	Json::Value output = Json::objectValue;

	if (errors.size() > 0)
		output["errors"] = std::move(errors);

	bool const wildcardMatchesExperimental = false;

	output["sources"] = Json::objectValue;
	unsigned sourceIndex = 0;
	// NOTE: A case that will pass `parsingSuccess && !analysisFailed` but not `analysisSuccess` is
	// stopAfter: parsing with no parsing errors.
	if (parsingSuccess && !analysisFailed)
		for (std::string const& sourceName: compilerStack.sourceNames())
		{
			Json::Value sourceResult = Json::objectValue;
			sourceResult["id"] = sourceIndex++;
			if (isArtifactRequested(_inputsAndSettings.outputSelection, sourceName, "", "ast", wildcardMatchesExperimental))
				sourceResult["ast"] = ASTJsonExporter(compilerStack.state(), compilerStack.sourceIndices()).toJson(compilerStack.ast(sourceName));
			output["sources"][sourceName] = sourceResult;
		}

	Json::Value contractsOutput = Json::objectValue;
	for (std::string const& contractName: analysisSuccess ? compilerStack.contractNames() : std::vector<std::string>())
	{
		size_t colon = contractName.rfind(':');
		solAssert(colon != std::string::npos, "");
		std::string file = contractName.substr(0, colon);
		std::string name = contractName.substr(colon + 1);

		// ABI, storage layout, documentation and metadata
		Json::Value contractData(Json::objectValue);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "abi", wildcardMatchesExperimental))
			contractData["abi"] = compilerStack.contractABI(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "assembly", wildcardMatchesExperimental))
			contractData["assembly"] = compilerStack.contractCode(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "showFunctionIds", wildcardMatchesExperimental))
			contractData["functionIds"] = compilerStack.functionIds(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "showPrivateFunctionIds", wildcardMatchesExperimental))
			contractData["privateFunctionIds"] = compilerStack.privateFunctionIds(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "metadata", wildcardMatchesExperimental))
			contractData["metadata"] = compilerStack.metadata(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "userdoc", wildcardMatchesExperimental))
			contractData["userdoc"] = compilerStack.natspecUser(contractName);
		if (isArtifactRequested(_inputsAndSettings.outputSelection, file, name, "devdoc", wildcardMatchesExperimental))
			contractData["devdoc"] = compilerStack.natspecDev(contractName);

		if (!contractData.empty())
		{
			if (!contractsOutput.isMember(file))
				contractsOutput[file] = Json::objectValue;
			contractsOutput[file][name] = contractData;
		}
	}
	if (!contractsOutput.empty())
		output["contracts"] = contractsOutput;

	return output;
}

Json::Value StandardCompiler::compile(Json::Value const& _input) noexcept
{
	try
	{
		auto parsed = parseInput(_input);
		if (std::holds_alternative<Json::Value>(parsed))
			return std::get<Json::Value>(std::move(parsed));
		InputsAndSettings settings = std::get<InputsAndSettings>(std::move(parsed));
		if (settings.language == "Solidity")
			return compileSolidity(std::move(settings));
		else if (settings.language == "SolidityAST")
			return compileSolidity(std::move(settings));
		else
			return formatFatalError(Error::Type::JSONError, "Only \"Solidity\", \"Yul\", \"SolidityAST\" or \"EVMAssembly\" is supported as a language.");
	}
	catch (Json::LogicError const& _exception)
	{
		return formatFatalError(Error::Type::InternalCompilerError, std::string("JSON logic exception: ") + _exception.what());
	}
	catch (Json::RuntimeError const& _exception)
	{
		return formatFatalError(Error::Type::InternalCompilerError, std::string("JSON runtime exception: ") + _exception.what());
	}
	catch (util::Exception const& _exception)
	{
		return formatFatalError(Error::Type::InternalCompilerError, "Internal exception in StandardCompiler::compile: " + boost::diagnostic_information(_exception));
	}
	catch (...)
	{
		return formatFatalError(Error::Type::InternalCompilerError, "Internal exception in StandardCompiler::compile: " +  boost::current_exception_diagnostic_information());
	}
}

std::string StandardCompiler::compile(std::string const& _input) noexcept
{
	Json::Value input;
	std::string errors;
	try
	{
		if (!util::jsonParseStrict(_input, input, &errors))
			return util::jsonPrint(formatFatalError(Error::Type::JSONError, errors), m_jsonPrintingFormat);
	}
	catch (...)
	{
		return "{\"errors\":[{\"type\":\"JSONError\",\"component\":\"general\",\"severity\":\"error\",\"message\":\"Error parsing input JSON.\"}]}";
	}

	// std::cout << "Input: " << input.toStyledString() << std::endl;
	Json::Value output = compile(input);
	// cout << "Output: " << output.toStyledString() << endl;

	try
	{
		return util::jsonPrint(output, m_jsonPrintingFormat);
	}
	catch (...)
	{
		return "{\"errors\":[{\"type\":\"JSONError\",\"component\":\"general\",\"severity\":\"error\",\"message\":\"Error writing output JSON.\"}]}";
	}
}
