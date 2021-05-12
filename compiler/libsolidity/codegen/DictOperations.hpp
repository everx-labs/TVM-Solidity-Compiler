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
 * @date 2021
 */

#pragma once

#include <liblangutil/ErrorReporter.h>
#include <liblangutil/Exceptions.h>
#include <liblangutil/SourceReferenceFormatter.h>
#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/ASTVisitor.h>

#include "TVMCommons.hpp"

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace langutil;
using namespace solidity::util;


namespace solidity::frontend {

class StackPusherHelper;
class TVMExpressionCompiler;

class DictOperation {
public:
	DictOperation(StackPusherHelper& pusher, Type const& keyType, Type const& valueType);
protected:
	StackPusherHelper& pusher;
	Type const& keyType;
	const int keyLength{};
	Type const& valueType;
	const Type::Category valueCategory{};
};


class DictMinMax : public DictOperation {
public:
	DictMinMax(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, bool isMin) :
			DictOperation{pusher, keyType, valueType}, isMin{isMin} {

	}

	void minOrMax(bool saveOrigKeyAndNoTuple = false);

private:
	const bool isMin{};
	std::string dictOpcode;
};

class DictPrevNext : public DictOperation {
public:
	DictPrevNext(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, const std::string& oper) :
			DictOperation{pusher, keyType, valueType},
			oper{oper}
	{
	}

	void prevNext(bool saveOrigKeyAndNoTuple = false);

private:
	const std::string oper;
};

class GetFromDict : public DictOperation {
public:
	GetFromDict(StackPusherHelper& pusher, Type const& keyType, Type const& valueType, const GetDictOperation op,
				const DataType& dataType);

	void getDict();

private:
	void checkExist();

protected:
	const GetDictOperation op{};
	const DataType dataType{};
};

class DictSet : public DictOperation {
public:
	DictSet(
			StackPusherHelper& pusher,
			Type const &keyType,
			Type const &valueType,
			const DataType& dataType,
			SetDictOperation operation
	);

	void dictSet();

private:
	const DataType& dataType;
	SetDictOperation operation;
	std::string opcode;
};

class DelMinOrMax : public DictOperation {
public:
	DelMinOrMax(StackPusherHelper& pusher, Type const &keyType, Type const &valueType, bool isDelMin, MemberAccess const* memberAccess);
	void delMinOrMax();

private:
	const bool isDelMin{};
	MemberAccess const* memberAccess{};
	LValueInfo lValueInfo{};
	std::unique_ptr<TVMExpressionCompiler> ec{};
	int stackSize{};
	std::string opcode;
};

} // end solidity::frontend
