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

void TVMCompilerProceedContract(
	langutil::ErrorReporter* errorReporter,
	ContractDefinition const& _contract,
	std::vector<PragmaDirective const *> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	bool withOptimizations,
	bool doPrintInConsole,
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix
) {
	TVMContractCompiler::g_errorReporter = errorReporter;

	string pathToFiles;

	if (filePrefix.empty()) {
		pathToFiles = boost::filesystem::path{solFileName}.stem().string();
	} else {
		pathToFiles = filePrefix;
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
	if (generateCode) {
		TVMContractCompiler::proceedContract(doPrintInConsole ? "" : pathToFiles + ".code", _contract, pragmaHelper, withOptimizations);
	}
	if (generateAbi) {
		TVMContractCompiler::generateABI(doPrintInConsole ? "" : pathToFiles + ".abi.json", &_contract, *pragmaDirectives);
	}

}