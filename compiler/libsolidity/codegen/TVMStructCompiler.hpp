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
 * Struct compiler for TVM
 */

#pragma once

#include <libsolidity/ast/ASTForward.h>
#include <libsolidity/ast/Types.h>
#include <boost/core/noncopyable.hpp>
#include "TVMCommons.hpp"

namespace solidity::frontend {

class StackPusherHelper;

class StructCompiler : public boost::noncopyable {
public:
	struct FieldSizeInfo {
		bool isBitFixed{false};
		bool isRefFixed{false};
		int maxBitLength{0};
		int maxRefLength{0};
		Type const* type{};

		FieldSizeInfo(bool isBitFixed, bool isRefFixed, int maxBitLength, int maxRefLength);
		explicit FieldSizeInfo(Type const* type);
		bool tryMerge(const FieldSizeInfo& fieldSizeInfo);
	};

	struct Field {
		FieldSizeInfo fieldSizeInfo;
		int memberIndex{0};
		Type const* type{};
		std::string name;

		Field(int memberIndex, Type const* type, std::string  name);
	};

	class Node {
	private:
		const int skipData{};
		std::vector<Field> fields;
		std::vector<int> children;
	public:
		explicit Node (int skipData);
		bool tryAddField(const Field &f, bool useAllRefs = false, bool withDeleteChild = false);
		void removeField(int index);
		bool tryAddChild(int child);
		void removeChildIfHave(int v);
		int maxBits() const;
		int maxRefs() const;
		const std::vector<Field>& getFields() const { return fields; }
		const std::vector<int>& getChildren() const { return children; }
	};

	struct PathToStructMember {
		std::vector<int> nodes;
		std::vector<int> childRef;
		std::vector<FieldSizeInfo> skippedFields;
		Field field;
		bool haveDataOrRefsAfterMember;

		PathToStructMember(
			const std::vector<int> &nodes,
			const std::vector<int> &childRef,
			std::vector<FieldSizeInfo> skippedFields,
			Field field,
			bool haveDataOrRefsAfterMember
		);
	};

private:
	std::vector<std::string> memberNames;
	std::vector<Type const*> memberTypes;
	std::vector<Node> nodes;
	std::map<std::string, Type const*> nameToType;
	std::map<std::string, PathToStructMember> paths; // paths[memberName]
	StackPusherHelper *pusher{};

public:
	StructCompiler(StackPusherHelper *pusher, StructType const* structType);
	StructCompiler(
		StackPusherHelper *pusher,
		const std::vector<Type const*>& memberTypes,
		std::vector<std::string> memberNames,
	   	int skipData,
	   	bool isC4
	);
	void createDefaultStruct(bool resultIsBuilder = false);
	void pushMember(const std::string &memberName, bool isStructTuple, bool returnStructAsSlice);
	void setMemberForTuple(const std::string &memberName);
	void structConstructor(ast_vec<ASTString> const& names, const std::function<void(int)>& pushParam);
	void tupleToBuilder();
	void stateVarsToBuilder();
	void expandStruct(const std::string &memberName, bool doPushMemberOnStack);
	void collectStruct(const std::string &memberName, bool isValueBuilder);
	void convertSliceToTuple();
	void sliceToStateVarsToC7();
	static int maxBitLength(StructType const* structType);
	static bool isCompatibleWithSDK(int keyLength, StructType const* structType);
private:
	bool isCompatibleWithSDK(int keyLength) const;
public:
	const std::vector<Node>& getNodes() { return nodes; }
private:
	void dfs(int v, std::vector<int> &nodePath, std::vector<int> &refPath);
	void createDefaultStructDfs(int v);
	void createStructDfs(int v, const std::map<std::string, int>& argStackSize);
	void stateVarsToBuilderDfs(const int v);
	void sliceToStateVarsToC7Dfs(int v);
	void skip(int bits, int refs);
	void skip(const FieldSizeInfo& si);
	void skip(const std::vector<FieldSizeInfo> &fieldInfo);
	void split(const FieldSizeInfo& fieldSizeInfo);
	void split(int bitQty, int refQty);
	void merge(const FieldSizeInfo& fieldSizeInfo);
}; // end StructCompiler
} // end solidity::frontend

