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
#include "TVMTypeChecker.hpp"

using namespace solidity::frontend;

void TVMCompilerProceedContract(langutil::ErrorReporter* errorReporter, ContractDefinition const& _contract,
								std::vector<PragmaDirective const *> const* pragmaDirectives) {
	if (!TVMContractCompiler::m_optionsEnabled)
		return;

	TVMContractCompiler::g_errorReporter = errorReporter;

	if (!TVMContractCompiler::m_outputFolder.empty()) {
		TVMContractCompiler::m_outputToFile = true;
		namespace fs = boost::filesystem;
		TVMContractCompiler::m_fileName = (fs::path(TVMContractCompiler::m_outputFolder) / _contract.name()).string();
    }

	TVMTypeChecker::check(&_contract, *pragmaDirectives);

	PragmaDirectiveHelper pragmaHelper{*pragmaDirectives};
	switch (TVMContractCompiler::m_tvmOption) {
		case TvmOption::Code:
			TVMContractCompiler::proceedContract(&_contract, pragmaHelper);
			break;
		case TvmOption::Abi:
			TVMContractCompiler::generateABI(&_contract, *pragmaDirectives);
			break;
		case TvmOption::DumpStorage:
			TVMContractCompiler::proceedDumpStorage(&_contract, pragmaHelper);
			break;
		case TvmOption::CodeAndAbi:
			TVMContractCompiler::m_outputToFile = true;
			TVMContractCompiler::proceedContract(&_contract, pragmaHelper);
			TVMContractCompiler::generateABI(&_contract, *pragmaDirectives);
			break;
	}
}

void TVMCompilerEnable(const TvmOption tvmOption, bool without_logstr, bool optimize) {
	TVMContractCompiler::m_optionsEnabled = true;
	TVMContractCompiler::m_tvmOption = tvmOption;
	TVMContractCompiler::g_without_logstr = without_logstr;
	TVMContractCompiler::g_disable_optimizer = !optimize;
}

void TVMSetFileName(std::string _fileName) {
	TVMContractCompiler::m_fileName = boost::filesystem::path(_fileName).stem().string();
}

bool TVMIsOutputProduced() {
	return TVMContractCompiler::m_outputProduced;
}
