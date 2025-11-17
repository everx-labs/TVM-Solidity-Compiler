/*
 * Copyright (C) 2019-2025 EverX. All Rights Reserved.
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

using namespace solidity::frontend;

solidity::langutil::ErrorReporter* GlobalParams::g_errorReporter{};
solidity::langutil::CharStreamProvider* GlobalParams::g_charStreamProvider{};
solidity::util::SetOnce<solidity::langutil::TVMVersion> GlobalParams::g_tvmVersion{};

void TVMCompilerProceedContract(
	ContractDefinition const& _contract,
	std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
	std::vector<PragmaDirective const*> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	std::string const& outDirPathAndStem,
	bool doPrintFunctionIds,
	bool doPrivateFunctionIds,
	bool debugMode
) {
	PragmaDirectiveHelper pragmaHelper{*pragmaDirectives};
	if (doPrintFunctionIds) {
		TVMContractCompiler::printFunctionIds(outDirPathAndStem + ".ids", _contract, pragmaHelper);
	}
	if (doPrivateFunctionIds) {
		TVMContractCompiler::
			printPrivateFunctionIds(outDirPathAndStem + ".pids", _contract, _sourceUnits, pragmaHelper, debugMode);
	}
	if (generateCode) {
		TVMContractCompiler::
			generateCodeAndSaveToFile(outDirPathAndStem + ".code", _contract, _sourceUnits, pragmaHelper, debugMode);
	}
	if (generateAbi) {
		TVMContractCompiler::generateABI(outDirPathAndStem + ".abi.json", &_contract, _sourceUnits, *pragmaDirectives);
	}
}
