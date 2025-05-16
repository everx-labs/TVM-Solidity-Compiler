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

#pragma once

#include <vector>
#include <liblangutil/ErrorReporter.h>
#include <liblangutil/TVMVersion.h>
#include <libsolidity/ast/ASTForward.h>
#include <liblangutil/CharStreamProvider.h>
#include <libsolutil/SetOnce.h>

class GlobalParams {
public:
	static solidity::langutil::ErrorReporter* g_errorReporter;
	static solidity::langutil::CharStreamProvider* g_charStreamProvider;
	static solidity::util::SetOnce<solidity::langutil::TVMVersion> g_tvmVersion;
};

std::string getPathToFiles(
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix
);

void TVMCompilerProceedContract(
	solidity::frontend::ContractDefinition const& _contract,
	std::vector<solidity::frontend::ASTPointer<solidity::frontend::SourceUnit>> const& _sourceUnits,
	std::vector<solidity::frontend::PragmaDirective const *> const* pragmaDirectives,
	bool generateAbi,
	bool generateCode,
	const std::string& solFileName,
	const std::string& outputFolder,
	const std::string& filePrefix,
	bool doPrintFunctionIds,
	bool doPrivateFunctionIds
);
