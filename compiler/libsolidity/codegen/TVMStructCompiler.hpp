/*
 * Copyright 2018-2021 TON DEV SOLUTIONS LTD.
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
 * Struct compiler for TVM
 */

#pragma once

#include <libsolidity/ast/ASTForward.h>
#include <libsolidity/ast/Types.h>
#include <boost/core/noncopyable.hpp>
#include "TVMCommons.hpp"

namespace solidity::frontend {

class StackPusher;

class StructCompiler : public boost::noncopyable {
public:
	StructCompiler(StackPusher *pusher, TupleType const* tuple);
	StructCompiler(StackPusher *pusher, StructType const* structType);
	StructCompiler(
		StackPusher *pusher,
		const std::vector<Type const*>& memberTypes,
		const std::vector<std::string>& memberNames
	);
	void createDefaultStruct();
	void createDefaultStructAsCell();
	void createDefaultStructAsSlice();
private:
	void createDefaultStruct(bool asSlice);
public:
	void pushMember(const std::string &memberName);
	void setMemberForTuple(const std::string &memberName);
	// MyStruct(1, 2, 4) or MyStruct({x: 1, y: 2, z: 4})
	void structConstructor(ast_vec<ASTString> const& names, const std::function<void(int, Type const*)>& pushParam);
	void tupleToBuilder();
	void convertSliceToTuple();

private:
	int getIndex(const std::string& name);

private:
	std::vector<std::string> m_names;
	std::vector<Type const*> m_types;
	StackPusher *pusher{};
}; // end StructCompiler
} // end solidity::frontend

