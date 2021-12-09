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

#include "TVMCommons.hpp"
#include "TVMPusher.hpp"
#include "TVMStructCompiler.hpp"
#include "TVMABI.hpp"

using namespace solidity::frontend;
using namespace solidity::util;


StructCompiler::StructCompiler(StackPusher *pusher, TupleType const* tuple) :
	pusher{pusher}
{
	int i = 0;
	for (auto t :tuple->components()) {
		m_names.push_back(toString(i++));
		m_types.push_back(t);
	}
}

std::vector<Type const*>
getTypesFrom(StructDefinition const *structDefinition)
{
	std::vector<Type const*> result;
	for (const ASTPointer<VariableDeclaration>& vd : structDefinition->members()) {
		result.push_back(vd->type());
	}
	return result;
}

std::vector<std::string>
getNamesFrom(StructDefinition const *structDefinition)
{
	std::vector<std::string> result;
	for (const ASTPointer<VariableDeclaration>& vd : structDefinition->members()) {
		result.push_back(vd->name());
	}
	return result;
}

StructCompiler::StructCompiler(StackPusher *pusher, StructType const *structType) :
	StructCompiler{pusher, getTypesFrom(&structType->structDefinition()), getNamesFrom(&structType->structDefinition())} {
}

StructCompiler::StructCompiler(
	StackPusher *pusher,
	const std::vector<Type const*>& m_types,
	const std::vector<std::string>& m_names
) :
	m_names{m_names},
	m_types{m_types},
	pusher{pusher}
{

}

void StructCompiler::createDefaultStruct() {
	for (Type const *type: m_types) {
		pusher->pushDefaultValue(type);
	}
	pusher->tuple(m_types.size());
}

void StructCompiler::createDefaultStructAsCell() {
	createDefaultStruct(false);
}

void StructCompiler::createDefaultStructAsSlice() {
	createDefaultStruct(true);
}

void StructCompiler::createDefaultStruct(bool asSlice) {
	std::string name = "default_tuple_builder";
	for (Type const* t : m_types) {
		name += "_" + t->identifier();
	}
	if (asSlice)
		pusher->compureConstSlice(name);
	else
		pusher->compureConstCell(name);
	pusher->ctx().addBuildTuple(name, m_types);
}

void StructCompiler::pushMember(const std::string &memberName) {
	// struct
	pusher->indexNoexcep(getIndex(memberName));
}

void StructCompiler::setMemberForTuple(const std::string &memberName) {
	pusher->setIndex(getIndex(memberName));
}

void StructCompiler::structConstructor(
	std::vector<ASTPointer<ASTString>> const& names,
	const std::function<void(int, Type const*)>& pushParam
) {
	if (names.empty()) {
		int i{};
		for (const Type *type : m_types) {
			if (type->category() == Type::Category::Mapping) {
				pusher->pushDefaultValue(type);
			} else {
				pushParam(i++, type);
			}
		}
	} else {
		int i = 0;
		for (const Type *type : m_types) {
			const std::string& memberName = m_names.at(i);
			auto it = std::find_if(names.begin(), names.end(), [&](const ASTPointer<ASTString>& name){
				return memberName == *name;
			});
			if (type->category() == Type::Category::Mapping) {
				solAssert(it == names.end(), "");
				pusher->pushDefaultValue(type);
			} else {
				int index = it - names.begin();
				pushParam(index, type);
			}
			++i;
		}
	}

	pusher->tuple(m_types.size());
}

void StructCompiler::tupleToBuilder() {
    // stack: tuple
	const int ss = pusher->stackSize();

	pusher->startContinuation();

	const int n = m_names.size();
	pusher->untuple(n);
	if (n >= 2) {
		pusher->reverse(n, 0);
	}
	pusher->push(+1, "NEWC");

	ChainDataEncoder encoder{pusher};
	DecodePositionAbiV2 position{0, 0, m_types};
	encoder.encodeParameters(m_types, position);
	pusher->pushRefContAndCallX(1, 1);
	// stack: builder
	const int curSize = pusher->stackSize();

	solAssert(ss == curSize, "");
}

void StructCompiler::convertSliceToTuple() {
	const int ss = pusher->stackSize();

	ChainDataDecoder decoder{pusher};
	decoder.decodeData(0, 0, m_types);
	pusher->tuple(m_types.size());

	solAssert(ss == pusher->stackSize(), "");
}

int StructCompiler::getIndex(const std::string& name) {
	int index = std::find(m_names.begin(), m_names.end(), name) - m_names.begin();
	solAssert(index != static_cast<int>(m_names.size()), "");
	return index;
}