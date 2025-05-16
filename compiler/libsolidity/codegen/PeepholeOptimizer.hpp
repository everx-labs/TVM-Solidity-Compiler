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
 * Peephole optimizer
 */

#pragma once

#include <bitset>
#include <libsolidity/codegen/TvmAstVisitor.hpp>

namespace solidity::frontend {
enum OptFlags : unsigned {
	UnpackOpaque = 0,
	OptimizeSlice = 1,
	UseCompoundOpcodes = 2
};
class PeepholeOptimizer : public TvmAstVisitor {
public:
	explicit PeepholeOptimizer(std::bitset<3> const _flags)
		: m_flags{_flags } { }
	bool visit(CodeBlock &_node) override;
	bool visit(Function &_node) override;
	void endVisit(CodeBlock &_node) override;
private:
	void optimizeBlock(CodeBlock &_node) const;
private:
	std::bitset<3> m_flags;
};
} // end solidity::frontend

