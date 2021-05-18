/*
 * Copyright 2018-2019 TON DEV SOLUTIONS LTD.
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
 * @author TON Labs <connect@tonlabs.io>
 * @date 2019
 * TVM codegen driver
 */


#include "TVM.h"
#include "TVMContractCompiler.hpp"

using namespace solidity::frontend;

solidity::langutil::ErrorReporter* GlobalParams::g_errorReporter{};
bool GlobalParams::g_withOptimizations{};
bool GlobalParams::g_withDebugInfo{};

void TVMCompilerProceedContract(
    solidity::langutil::ErrorReporter* errorReporter,
	ContractDefinition const& _contract,
	std::vector<PragmaDirective const *> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	bool withOptimizations,
	bool withDebugInfo,
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix,
	bool doPrintFunctionIds
) {
    GlobalParams::g_errorReporter = errorReporter;
    GlobalParams::g_withDebugInfo = withDebugInfo;
    GlobalParams::g_withOptimizations = withOptimizations;

	std::string pathToFiles;

	if (filePrefix.empty()) {
		pathToFiles = boost::filesystem::path{solFileName}.stem().string();
	} else {
		pathToFiles = filePrefix;
		boost::filesystem::path p(filePrefix);
		if (filePrefix != p.filename()) {
			fatal_error(string{} + "Option -f takes basename of output file(s).\n" +
				"\"" + filePrefix + "\" looks like a path. Use option -o to set an output directory.");
		}
	}

	if (!outputFolder.empty()) {
		namespace fs = boost::filesystem;
		boost::system::error_code ec;
		fs::path dir = fs::weakly_canonical(outputFolder);
		fs::create_directories(dir, ec);
		if (ec) {
			errorReporter->fatalTypeError(_contract.location(), "Problem with directory \"" + outputFolder + "\": " + ec.message());
			return;
		}
		pathToFiles = (fs::path(dir) / pathToFiles).string();
    }

	PragmaDirectiveHelper pragmaHelper{*pragmaDirectives};
	if (doPrintFunctionIds) {
		TVMContractCompiler::printFunctionIds(_contract, pragmaHelper);
	} else {
		if (generateCode) {
			TVMContractCompiler::proceedContract(pathToFiles + ".code", _contract, pragmaHelper);
		}
		if (generateAbi) {
			TVMContractCompiler::generateABI(pathToFiles + ".abi.json", &_contract, *pragmaDirectives);
		}
	}

}