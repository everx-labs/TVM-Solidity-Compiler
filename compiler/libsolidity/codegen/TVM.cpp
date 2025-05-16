/*
 * Copyright (C) 2019-2024 EverX. All Rights Reserved.
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
/**
 * TVM codegen driver
 */


#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMContractCompiler.hpp>

using namespace std;
using namespace solidity::frontend;

solidity::langutil::ErrorReporter* GlobalParams::g_errorReporter{};
solidity::langutil::CharStreamProvider* GlobalParams::g_charStreamProvider{};
solidity::util::SetOnce<solidity::langutil::TVMVersion> GlobalParams::g_tvmVersion{};

std::string getPathToFiles(
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix
) {
	std::string pathToFiles;

	if (filePrefix.empty()) {
		pathToFiles = boost::filesystem::path{solFileName}.stem().string();
	} else {
		pathToFiles = filePrefix;
		boost::filesystem::path p(filePrefix);
		if (filePrefix != p.filename()) {
			std::cerr << "Error: Option -p takes basename of output file(s)." << std::endl <<
						"\"" << filePrefix << "\" looks like a path. Use option -o to set an output directory.";
			std::exit(1);
		}
	}

	if (!outputFolder.empty()) {
		namespace fs = boost::filesystem;
		boost::system::error_code ec;
		fs::path dir = fs::weakly_canonical(outputFolder);
		fs::create_directories(dir, ec);
		if (ec) {
			std::cerr << "Problem with directory \"" + outputFolder + "\": " + ec.message();
			std::exit(1);
		}
		pathToFiles = (fs::path(dir) / pathToFiles).string();
	}
	return pathToFiles;
}

void TVMCompilerProceedContract(
	ContractDefinition const& _contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const *> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix,
	bool doPrintFunctionIds,
	bool doPrivateFunctionIds
) {
	std::string pathToFiles = getPathToFiles(solFileName, outputFolder, filePrefix);

	PragmaDirectiveHelper pragmaHelper{*pragmaDirectives};
	if (doPrintFunctionIds) {
		TVMContractCompiler::printFunctionIds(_contract, pragmaHelper);
	} else if (doPrivateFunctionIds) {
		TVMContractCompiler::printPrivateFunctionIds(_contract, _sourceUnits, pragmaHelper);
	} else {
		if (generateCode) {
			TVMContractCompiler::generateCodeAndSaveToFile(pathToFiles + ".code", _contract, _sourceUnits, pragmaHelper);
		}
		if (generateAbi) {
			TVMContractCompiler::generateABI(pathToFiles + ".abi.json", &_contract, _sourceUnits, *pragmaDirectives);
		}
	}

}
