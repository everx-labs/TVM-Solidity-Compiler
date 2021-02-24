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
	const std::string& outputFolder
) {
	TVMContractCompiler::g_errorReporter = errorReporter;

	string baseFileName;
	if (!outputFolder.empty()) {
		solUnimplemented("");
//		fileName = _contract.name()
//		TVMContractCompiler::m_outputToFile = true;
//		namespace fs = boost::filesystem;
//		fileName = (fs::path(outputFolder) / _contract.name()).string();
//	ensurePathExists();
//	boost::filesystem::path(fileName).stem().string()
    } else {
		if (boost::algorithm::ends_with(solFileName, ".sol")) {
			baseFileName = solFileName.substr(0, solFileName.size() - 4);
		} else {
			baseFileName = solFileName;
		}
	}

	PragmaDirectiveHelper pragmaHelper{*pragmaDirectives};
	if (generateCode) {
		TVMContractCompiler::proceedContract(doPrintInConsole ? "" : baseFileName + ".code", _contract, pragmaHelper, withOptimizations);
	}
	if (generateAbi) {
		TVMContractCompiler::generateABI(doPrintInConsole ? "" : baseFileName + ".abi.json", &_contract, *pragmaDirectives);
	}

}
