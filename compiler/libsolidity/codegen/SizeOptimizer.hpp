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
 * Size optimizer
 */

#pragma once

#include <libsolidity/codegen/TvmAstVisitor.hpp>

namespace solidity::frontend {
	class SizeOptimizer : public TvmAstVisitor {
	public:
		void optimize(Pointer<Contract>& c);
	};
} // end solidity::frontend

