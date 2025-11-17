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

#pragma once

#include <liblangutil/CharStreamProvider.h>
#include <liblangutil/ErrorReporter.h>
#include <liblangutil/TVMVersion.h>
#include <libsolidity/ast/ASTForward.h>
#include <libsolutil/SetOnce.h>
#include <vector>

class GlobalParams {
public:
	static solidity::langutil::ErrorReporter* g_errorReporter;
	static solidity::langutil::CharStreamProvider* g_charStreamProvider;
	static solidity::util::SetOnce<solidity::langutil::TVMVersion> g_tvmVersion;
};

void TVMCompilerProceedContract(
	solidity::frontend::ContractDefinition const& _contract,
	std::vector<solidity::frontend::ASTPointer<solidity::frontend::SourceUnit>> const& _sourceUnits,
	std::vector<solidity::frontend::PragmaDirective const*> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	std::string const& outDirPathAndStem,
	bool doPrintFunctionIds,
	bool doPrivateFunctionIds,
	bool debugMode
);
