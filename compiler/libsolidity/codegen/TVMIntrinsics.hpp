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
 * TVM intrinsics codegen routines
 */

#pragma once

#include <string>
#include <boost/algorithm/string.hpp>
#include <libsolidity/ast/Types.h>


namespace dev::solidity {

class IntrinsicsCompiler : public StackPusherHelper {
	IExpressionCompiler* const m_exprCompiler;

protected:
	void acceptExpr(const Expression* expr) {
		m_exprCompiler->acceptExpr(expr);
	}
	
public:
	IntrinsicsCompiler(IStackPusher* pusher, IExpressionCompiler* exprCompiler, const TVMCompilerContext& ctx)
		: StackPusherHelper(pusher, &ctx)
		, m_exprCompiler(exprCompiler) {}
		
	bool checkTvmIntrinsic(FunctionCall const& _functionCall);
};
	
} // end dev::solidity
