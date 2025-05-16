/*
 * Copyright (C) 2020-2024 EverX. All Rights Reserved.
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
 * AST to TVM bytecode contract compiler
 */

#pragma once

#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>
#include <libsolidity/codegen/TvmAst.hpp>

namespace solidity::frontend {

class TVMCompilerContext;

class TVMContractCompiler: private boost::noncopyable {
public:
	static void printFunctionIds(ContractDefinition const& contract, PragmaDirectiveHelper const& pragmaHelper);
	static void printPrivateFunctionIds(
		ContractDefinition const& contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		PragmaDirectiveHelper const& pragmaHelper
	);
	static void generateABI(
		const std::string& fileName,
		ContractDefinition const* contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		std::vector<PragmaDirective const *> const& pragmaDirectives
	);
	static void generateCodeAndSaveToFile(
		std::string const& fileName,
		ContractDefinition const& contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		PragmaDirectiveHelper const &pragmaHelper
	);
	static Pointer<Contract> generateContractCode(
		ContractDefinition const* contract,
		std::vector<ASTPointer<SourceUnit>>const& _sourceUnits,
		PragmaDirectiveHelper const& pragmaHelper
	);
	static void optimizeCode(Pointer<Contract>& c);
private:
	static void fillInlineFunctions(TVMCompilerContext& ctx, ContractDefinition const* contract, std::vector<ASTPointer<SourceUnit>>const& _sourceUnits);
};

}	// end solidity::frontend
