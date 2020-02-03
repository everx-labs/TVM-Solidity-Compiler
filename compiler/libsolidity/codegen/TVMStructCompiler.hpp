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

namespace dev::solidity {

class StackPusherHelper;

class StructCompiler {

public:
	struct FieldSizeInfo {
		bool isBitFixed{false};
		bool isRefFixed{false};
		int maxBitLength{0};
		int maxRefLength{0};
		Type const* type{};

		FieldSizeInfo(bool isBitFixed, bool isRefFixed, int maxBitLength, int maxRefLength);
		FieldSizeInfo(Type const* type, ASTNode const& node);
		bool tryMerge(const FieldSizeInfo& fieldSizeInfo);
	};

	struct Field {
		FieldSizeInfo fieldSizeInfo;
		int memberIndex{0};
		VariableDeclaration const* member{};

		Field(int memberIndex, VariableDeclaration const* member);
	};

	class Node {
		int maxBits{0};
		int maxRefs{0};
		int skipData;
		int skipRef;
	public:
		std::vector<Field> fields;
		std::vector<int> children;

		Node (int skipData, int skipRef);
		bool tryAddField(const Field &f, bool useAllRefs = false);
		void removeFirstField(int index);
		bool tryAddChild(int child);
		void removeChildIfHave(int v);
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
				bool haveDataOrRefsAfterMember);
	};

private:
	std::vector<VariableDeclaration const*> variableDeclarations;
	std::vector<Node> nodes;
	std::map<std::string, VariableDeclaration const*> nameToVariableDeclarations;
	std::map<std::string, PathToStructMember> paths; // paths[memberName]
	StackPusherHelper *pusher{};

public:
	StructCompiler(StackPusherHelper *pusher, StructType const* structType);
	StructCompiler(StackPusherHelper *pusher, std::vector<VariableDeclaration const*> variableDeclarations,
	               int skipData, int skipRef, bool isC4, StructType const *structType);
	void createDefaultStruct(bool resultIsBuilder = false);
	void pushMember(const std::string &memberName);
	void createStruct(const int argumentStackSize, std::vector<ASTPointer<ASTString>> const& names);
	void expandStruct(const std::string &memberName, bool doPushMemberOnStack);
	void collectStruct(const std::string &memberName, bool isValueBuilder, bool isResultBuilder);
	void expandStruct(std::map<std::string, int> &memberToStackSize);
	static bool isCompatibleWithSDK(int keyLength, StructType const* structType);
	const std::vector<Node>& getNodes() { return nodes; }
private:
	void dfs(int v, std::vector<int> &nodePath, std::vector<int> &refPath);
	void createDefaultStructDfs(int v);
	void createStructDfs(int v, const std::map<std::string, int>& argStackSize);
	void expandStructDfs(int v, const std::string& prefix, std::map<std::string, int> &memberToStackSize);
	void load(const VariableDeclaration *vd, bool reverseOrder);
	void preload(const VariableDeclaration *vd);
	void store(const VariableDeclaration *vd, bool reverse, bool isValueBuilder = false);
	void skip(int bits, int refs);
	void skip(const FieldSizeInfo& si);
	void skip(const std::vector<FieldSizeInfo> &fieldInfo);
	void split(const FieldSizeInfo& fieldSizeInfo);
	void split(int bitQty, int refQty);
	void merge(const FieldSizeInfo& fieldSizeInfo);
	static std::vector<VariableDeclaration const*> fVariableDeclarations(StructDefinition const* structDefinition);
}; // end StructCompiler
} // end dev::solidity

