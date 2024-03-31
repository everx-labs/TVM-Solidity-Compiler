/*
 * Copyright (C) 2019-2023 EverX. All Rights Reserved.
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

#include "TvmAstVisitor.hpp"

namespace solidity::frontend {
	class PeepholeOptimizer : public TvmAstVisitor {
	public:
		explicit PeepholeOptimizer(bool _withUnpackOpaque, bool _optimizeSlice, bool _withTuck)
			: m_withUnpackOpaque{_withUnpackOpaque}, m_optimizeSlice{_optimizeSlice}, m_withTuck{_withTuck} {}
		bool visit(Function &_node) override;
		bool visit(CodeBlock &_node) override;
		void endVisit(CodeBlock &_node) override;
	private:
		void optimizeBlock(CodeBlock &_node) const;
	private:
		bool m_withUnpackOpaque{};
		bool m_optimizeSlice{};
		bool m_withTuck{};
	public:
		static bool withBlockPush;
	};
} // end solidity::frontend

