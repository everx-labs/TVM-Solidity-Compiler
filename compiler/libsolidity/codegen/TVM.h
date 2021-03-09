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

#pragma once

#include <vector>
#include <liblangutil/ErrorReporter.h>
#include <libsolidity/ast/ASTForward.h>


void TVMCompilerProceedContract(
	solidity::langutil::ErrorReporter* errorReporter,
	solidity::frontend::ContractDefinition const& _contract,
	std::vector<solidity::frontend::PragmaDirective const *> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	bool withOptimizations,
	bool doPrintInConsole,
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix
);