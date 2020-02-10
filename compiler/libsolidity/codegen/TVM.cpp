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


#include "TVMCommons.cpp"

#include "TVMFunctionCall.hpp"
#include "TVMExpressionCompiler.hpp"
#include "TVMABI.hpp"
#include "TVMCompiler.hpp"
#include "TVMIntrinsics.cpp"
#include "TVM.h"
#include "TVMStructCompiler.cpp"
#include "TVMTypeChecker.hpp"
#include "TVMInlineFunctionChecker.cpp"


bool TVMCompiler::m_optionsEnabled = false;
TvmOption TVMCompiler::m_tvmOption = TvmOption::Code;
bool TVMCompiler::m_dbg = false;
bool TVMCompiler::m_outputProduced = false;
bool TVMCompiler::g_without_logstr = false;
std::string TVMCompiler::m_outputWarnings;
std::vector<ContractDefinition const*> TVMCompiler::m_allContracts;

void TVMCompilerProceedContract(ContractDefinition const& _contract) {
	if (!TVMCompiler::m_optionsEnabled) 
		return;

	for (ContractDefinition const* c : TVMCompiler::m_allContracts) {
		TVMTypeChecker::check(c);
	}

	switch (TVMCompiler::m_tvmOption) {
		case TvmOption::Code:
			TVMCompiler::proceedContract(&_contract);
			break;
		case TvmOption::Abi:
			TVMCompiler::generateABI(&_contract);
			break;
		case TvmOption::DumpStorage:
			TVMCompiler::proceedDumpStorage(&_contract);
			break;
	}
}

void TVMCompilerEnable(const TvmOption tvmOption, bool dbg, bool without_logstr) {
	TVMCompiler::m_optionsEnabled = true;
	TVMCompiler::m_dbg = dbg;
	TVMCompiler::m_tvmOption = tvmOption;
	TVMCompiler::g_without_logstr = without_logstr;
}

void TVMSetAllContracts(const std::vector<ContractDefinition const*>& allContracts) {
	TVMCompiler::m_allContracts = allContracts;
}

bool TVMIsOutputProduced() {
	return TVMCompiler::m_outputProduced;
}

void TVMAddWarning(const std::string& msg) {
	TVMCompiler::m_outputWarnings += msg;
}

const std::string& TVMGetWarning() {
	return TVMCompiler::m_outputWarnings;
}