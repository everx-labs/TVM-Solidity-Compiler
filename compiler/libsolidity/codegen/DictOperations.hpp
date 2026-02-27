/*
 * Copyright (C) 2021-2025 EverX. All Rights Reserved.
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

#include <libsolidity/ast/AST.h>

#include <libsolidity/codegen/TVMCommons.hpp>

namespace solidity::frontend {

class StackPusher;
class TVMExpressionCompiler;

class DictOperation {
public:
	DictOperation(StackPusher& pusher, Type const& keyType, Type const& valueType);

protected:
	StackPusher& pusher;
	Type const& keyType;
	int const keyLength{};
	Type const& valueType;
};


class DictMinMax: public DictOperation {
public:
	DictMinMax(StackPusher& pusher, Type const& keyType, Type const& valueType, bool isMin):
		DictOperation{pusher, keyType, valueType},
		isMin{isMin} {}

	void minOrMax(bool saveOrigKeyAndNoTuple = false);

private:
	bool const isMin{};
	std::string dictOpcode;
};

class DictPrevNext: public DictOperation {
public:
	DictPrevNext(StackPusher& pusher, Type const& keyType, Type const& valueType, std::string oper):
		DictOperation{pusher, keyType, valueType},
		oper{std::move(oper)} {}

	void prevNext(bool saveOrigKeyAndNoTuple = false) const;

private:
	std::string const oper;
};

class GetFromDict: public DictOperation {
public:
	GetFromDict(
		StackPusher& pusher,
		Type const& keyType,
		Type const& valueType,
		GetDictOperation const& op,
		std::optional<DataType> inputValueType
	);

	void getDict() const;

private:
	void checkExist() const;

protected:
	GetDictOperation const op{};
	std::optional<DataType> const inputValueType;
};

class DictSet: public DictOperation {
public:
	DictSet(
		StackPusher& pusher,
		Type const& keyType,
		Type const& valueType,
		DataType const& dataType,
		SetDictOperation operation
	);

	void dictSet();

private:
	DataType const& dataType;
	SetDictOperation operation;
	std::string opcode;
};

class DelMinOrMax: public DictOperation {
public:
	DelMinOrMax(
		StackPusher& pusher,
		Type const& keyType,
		Type const& valueType,
		bool isDelMin,
		MemberAccess const* memberAccess
	);
	void delMinOrMax();

private:
	bool const isDelMin{};
	MemberAccess const* memberAccess{};
	LValueInfo lValueInfo{};
	std::unique_ptr<TVMExpressionCompiler> ec{};
	int stackSize{};
	std::string opcode;
};

} // end solidity::frontend
