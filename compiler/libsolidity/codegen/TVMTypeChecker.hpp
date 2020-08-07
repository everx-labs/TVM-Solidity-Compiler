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
 */

#pragma once

namespace solidity::frontend {

class TVMTypeChecker : private ASTConstVisitor {
private:
	explicit TVMTypeChecker(ContractDefinition const* contractDefinition,
							std::vector<PragmaDirective const *> const& pragmaDirectives);

public:
	static void check(ContractDefinition const* contractDefinition,
						std::vector<PragmaDirective const *> const& pragmaDirectives);

private:
	void checkPragma();
	void checkStateVariables();
	void checkOverrideAndOverload();
	void checkIntrinsic();
	void checkEncodeDecodeParams();
	void checkInlineFunctions();
	static void checkDecodeEncodeParams(FunctionDefinition const* f);
	static void checkDecodeEncodeParam(Type const* type, const ASTNode &node, int keyLength);
	static void checkTvmIntrinsic(FunctionDefinition const* f, ContractDefinition const* contractDefinition);
	void check_onCodeUpgrade();

    bool visit(Mapping const& _mapping) override;

private:
	ContractDefinition const* contractDefinition;
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};

}

