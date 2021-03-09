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

class TVMTypeChecker : public ASTConstVisitor {
public:
	explicit TVMTypeChecker(
		langutil::ErrorReporter& _errorReporter,
		std::vector<PragmaDirective const *> const& pragmaDirectives
	);

private:
	void checkPragma();
	void checkOverrideAndOverload();
	void checkTvmIntrinsic(FunctionDefinition const* f);
	void check_onCodeUpgrade(FunctionDefinition const& f);

    bool visit(Mapping const& _mapping) override;
    bool visit(FunctionDefinition const& fc) override;
    bool visit(ContractDefinition const& ) override;
    void endVisit(ContractDefinition const& ) override;

private:
	langutil::ErrorReporter& m_errorReporter;
	ContractDefinition const* contractDefinition{};
	std::vector<PragmaDirective const *> const& pragmaDirectives;
};

}

