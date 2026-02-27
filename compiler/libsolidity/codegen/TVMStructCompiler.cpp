/*
 * Copyright (C) 2019-2026 EverX. All Rights Reserved.
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
 * Struct compiler for TVM
 */

#include <utility>

#include <libsolidity/codegen/TVMABI.hpp>
#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolidity/codegen/TVMPusher.hpp>
#include <libsolidity/codegen/TVMStructCompiler.hpp>

using namespace solidity::frontend;
using namespace solidity::util;


StructCompiler::StructCompiler(StackPusher* pusher, TupleType const* tuple):
	pusher{pusher} {
	int i = 0;
	for (auto t: tuple->components()) {
		m_names.push_back(toString(i++));
		m_types.push_back(t);
	}
}

namespace {
std::vector<Type const*> getTypesFrom(StructDefinition const* structDefinition) {
	std::vector<Type const*> result;
	for (ASTPointer<VariableDeclaration> const& vd: structDefinition->members()) {
		result.push_back(vd->type());
	}
	return result;
}

std::vector<std::string> getNamesFrom(StructDefinition const* structDefinition) {
	std::vector<std::string> result;
	for (ASTPointer<VariableDeclaration> const& vd: structDefinition->members()) {
		result.push_back(vd->name());
	}
	return result;
}
}

StructCompiler::StructCompiler(StackPusher* pusher, StructType const* structType):
	StructCompiler{
		pusher,
		getTypesFrom(&structType->structDefinition()),
		getNamesFrom(&structType->structDefinition())
	} {}

StructCompiler::StructCompiler(
	StackPusher* pusher,
	std::vector<Type const*> memberTypes,
	std::vector<std::string> memberNames
):
	m_names{std::move(memberNames)},
	m_types{std::move(memberTypes)},
	pusher{pusher} {}

void StructCompiler::createDefaultStruct() const {
	for (Type const* type: m_types) {
		pusher->pushDefaultValue(type);
	}
	pusher->makeTuple(m_types.size());
}

void StructCompiler::createDefaultStructAsCell() const { createDefaultStruct(false); }

void StructCompiler::createDefaultStructAsSlice() const { createDefaultStruct(true); }

void StructCompiler::createDefaultStruct(bool asSlice) const {
	std::string name = "default_tuple_builder";
	for (Type const* t: m_types) {
		name += "_" + t->identifier();
	}
	if (asSlice)
		pusher->computeConstSlice(name);
	else
		pusher->computeConstCell(name);
	pusher->ctx().addBuildTuple(name, m_types);
}

void StructCompiler::pushMember(std::string const& memberName) {
	// struct
	pusher->indexNoexcep(getIndex(memberName));
}

void StructCompiler::setMemberForTuple(std::string const& memberName) { pusher->setIndex(getIndex(memberName)); }

void StructCompiler::structConstructor(
	std::vector<ASTPointer<ASTString>> const& names,
	std::function<void(int, Type const*)> const& pushParam
) const {
	if (names.empty()) {
		int i{};
		for (Type const* type: m_types) {
			pushParam(i++, type);
		}
	} else {
		int i = 0;
		for (Type const* type: m_types) {
			std::string const& memberName = m_names.at(i);
			auto it =
				std::ranges::find_if(names, [&](ASTPointer<ASTString> const& name) { return memberName == *name; });
			int index = it - names.begin();
			pushParam(index, type);
			++i;
		}
	}

	pusher->makeTuple(m_types.size());
}

void StructCompiler::tupleToBuilder() const {
	// stack: tuple
	int const ss = pusher->stackSize();

	pusher->startOpaque();

	int const n = m_names.size();
	pusher->untuple(n);
	if (n >= 2) {
		pusher->reverse(n, 0);
	}
	*pusher << "NEWC";

	ChainDataEncoder encoder{pusher};
	AbiV2Position position{0, 0, m_types};
	encoder.encodeParameters(m_types, position, false);
	pusher->endOpaque(1, 1, false);
	// stack: builder
	int const curSize = pusher->stackSize();

	solAssert(ss == curSize, "");
}

void StructCompiler::convertSliceToTuple() const {
	int const ss = pusher->stackSize();

	ChainDataDecoder decoder{pusher};
	decoder.decodeData(0, 0, m_types, false);
	pusher->makeTuple(m_types.size());

	solAssert(ss == pusher->stackSize(), "");
}

int StructCompiler::getIndex(std::string const& name) {
	int index = std::ranges::find(m_names, name) - m_names.begin();
	solAssert(index != static_cast<int>(m_names.size()), "");
	return index;
}
