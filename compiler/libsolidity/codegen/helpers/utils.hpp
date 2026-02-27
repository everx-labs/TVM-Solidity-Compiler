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

#pragma once


namespace solidity::frontend {

template <typename T>
using ast_vec = std::vector<ASTPointer<T>>;

template <typename T1, typename T2>
T1 const* to(T2 const* ptr) {
	return dynamic_cast<T1 const*>(ptr);
}

template <typename T, typename... Args>
constexpr bool isIn(T const& v, Args... args) {
	return (... || (v == (args)));
}

constexpr uint64_t str2int(char const* str, int i = 0) {
	return !str[i] ? 5381 : (str2int(str, i + 1) * 33) ^ static_cast<uint64_t>(str[i]);
}


} // end solidity::frontend
