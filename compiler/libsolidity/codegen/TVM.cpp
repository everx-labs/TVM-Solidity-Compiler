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

static string getLastContractName() {
	string name;
	for (auto c : TVMContractCompiler::m_allContracts) {
		if (c->canBeDeployed()) {
			name = c->name();
		}
	}
	return name;
}

void TVMCompilerProceedContract(ContractDefinition const& _contract,
								std::vector<PragmaDirective const *> const* pragmaDirectives) {
	if (!TVMContractCompiler::m_optionsEnabled)
		return;

	std::string mainContract = (TVMContractCompiler::m_mainContractName.empty()) ?
				getLastContractName() : TVMContractCompiler::m_mainContractName;

	if (TVMContractCompiler::m_outputFolder.empty()) {
		if (_contract.name() != mainContract)
			return;
	} else {
		if (_contract.abstract() || _contract.isInterface())
			return;
		TVMContractCompiler::m_outputToFile = true;
		namespace fs = boost::filesystem;
		TVMContractCompiler::m_fileName = (fs::path(TVMContractCompiler::m_outputFolder) / _contract.name()).string();
    }

	for (ContractDefinition const* c : TVMContractCompiler::m_allContracts) {
		TVMTypeChecker::check(c, *pragmaDirectives);
	}

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

void TVMSetAllContracts(const std::vector<ContractDefinition const*>& allContracts, std::string mainContract) {
	TVMContractCompiler::m_allContracts = allContracts;
	TVMContractCompiler::m_mainContractName = mainContract;
}

void TVMSetFileName(std::string _fileName) {
	TVMContractCompiler::m_fileName = boost::filesystem::path(_fileName).stem().string();
}

bool TVMIsOutputProduced() {
	return TVMContractCompiler::m_outputProduced;
}

void TVMAddWarning(const std::string& msg) {
	TVMContractCompiler::m_outputWarnings += msg;
}

const std::string& TVMGetWarning() {
	return TVMContractCompiler::m_outputWarnings;
}
