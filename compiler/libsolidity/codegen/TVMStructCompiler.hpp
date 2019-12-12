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

#include <utility>
#include "TVMCommons.hpp"

namespace dev::solidity {

class StructCompiler {
private:

	struct Field {
		int bitLength{0};
		int refs{0};
		int memberIndex{0};
		ASTPointer <VariableDeclaration> member;

		explicit Field(const int memberIndex, const ASTPointer <VariableDeclaration> &member) :
				memberIndex{memberIndex},
				member{member} {

			switch (member->type()->category()) {
				case Type::Category::Integer:
				case Type::Category::Bool:
				case Type::Category::FixedBytes:
				case Type::Category::Enum: {
					TypeInfo ti{member->type().get()};
					bitLength = ti.numBits;
					refs = 0;
					break;
				}
				case Type::Category::Address:
				case Type::Category::Array:
				case Type::Category::Struct:
				case Type::Category::Mapping:
					bitLength = 0;
					refs = 1;
					break;
				default:
					cast_error(*member.get(), "Unsupported in struct");
			}
			solAssert((bitLength > 0 && refs == 0) || (bitLength == 0 && refs == 1), "");
		}
	};

	struct Node {
		int totalLength{0};
		int usedRefs{0};
		std::vector<Field> fields;
		std::vector<int> children;

		bool tryAddField(const Field &f) {
			if (!(totalLength + f.bitLength <= TvmConst::CellBitLength && usedRefs + f.refs <= 2)) {
				return false;
			}
			totalLength += f.bitLength;
			usedRefs += f.refs;
			fields.push_back(f);
			return true;
		}

		bool tryAddChild(int child) {
			if (usedRefs == 4) {
				return false;
			}
			++usedRefs;
			if (!children.empty() && children.back() == child) {
				solAssert(false, "");
			}
			children.push_back(child);

			return true;
		}
	};

	struct PathToStructMember {
		std::vector<int> nodes;
		std::vector<int> idRefs;
		int skippedBits;
		int skippedRefs;
		Field field;
		bool haveDataOrRefSuffix;

		PathToStructMember(
				const std::vector<int> &nodes,
				const std::vector<int> &idRefs,
				const int &skippedBits,
				const int &skippedRefs,
				Field field,
				const bool haveDataOrRefSuffix) :
				nodes{nodes}, idRefs{idRefs}, skippedBits{skippedBits}, skippedRefs{skippedRefs}, field{std::move(field)},
				haveDataOrRefSuffix{haveDataOrRefSuffix} {
			solAssert(nodes.size() == idRefs.size() + 1, "");
		}
	};

	std::vector<Node> nodes;
	std::map<std::string, ASTPointer < VariableDeclaration>> variableDeclarations;
	std::map<std::string, PathToStructMember> paths; // paths[memberName]
	StackPusherHelper *pusher{};

public:
	StructCompiler(StackPusherHelper *pusher, StructDefinition const *structDefinition) : pusher{pusher} {
		int parent = 0;
		nodes.emplace_back(Node());

		std::vector<ASTPointer<VariableDeclaration>> const &members = structDefinition->members();
		int memberIndex = 0;
		for (const auto &member : members) {
			const Field f{memberIndex, member};
			if (!nodes.back().tryAddField(f)) {
				nodes.emplace_back(Node());
				solAssert(nodes.back().tryAddField(f), "");

				int v = static_cast<int>(nodes.size()) - 1;
				if (!nodes[parent].tryAddChild(v)) {
					++parent;
					solAssert(nodes[parent].tryAddChild(v), "");
				}
			}
			variableDeclarations[member->name()] = member;
			++memberIndex;
		}

		std::vector<int> nodePath;
		std::vector<int> refPath;
		dfs(0, nodePath, refPath);
	}

	void createDefaultStruct() {
		createDefaultStructDfs(0);
		pusher->push(0, "ENDC");
		pusher->push(0, "CTOS");
	}

	void getMember(const std::string &memberName) {
		const PathToStructMember &path = paths.at(memberName);
		for (int idRef : path.idRefs) {
			pusher->push(0, "PLDREFIDX " + toString(idRef));
			pusher->push(0, "CTOS");
		}

		skipField(path.skippedBits, path.skippedRefs);
		preload(variableDeclarations[memberName].get());
	}

	void createStruct(const std::function<void(const int, const std::string &)> &pushOnStack) {
		createStructDfs(0, pushOnStack);
		pusher->push(0, "ENDC CTOS");
	}

	void expandStruct(const string &memberName, bool doPushMemberOnStack) {
		// slice-struct
		const PathToStructMember &path = paths.at(memberName);
		for (int i = 0; i < static_cast<int>(path.idRefs.size()); ++i) {
			int v = path.nodes[i];
			int idRef = path.idRefs[i];
			split(nodes[v].totalLength, idRef); // pref suf
			if (idRef + 1 == static_cast<int>(nodes[v].children.size())) {
				pusher->push(-1 + 1, "LDREFRTOS");
				pusher->push(-1 + 1, "NIP"); // pref child
			} else {
				pusher->push(+1, "LDREFRTOS"); // pref suf child
			}
		}

		// (pref, suf)... node
		if (path.skippedBits > 0 ||  path.skippedRefs > 0) {
			split(path.skippedBits, path.skippedRefs); // (pref, [suf])... node-pref node-suf
		}
		// (pref, [suf])... [node-pref] node-suf
		if (doPushMemberOnStack) {
			load(path.field.member.get()); // (pref, [suf])... [node-pref] member node-suf
			pusher->push(0, "SWAP"); // (pref, [suf])... [node-pref] node-suf member
		} else {
			skipField(path.field.bitLength, path.field.refs); // (pref, [suf])... [node-pref] node-suf
		}
	}

	void collectStruct(const string &memberName, bool isValueBuilder, bool isResultBuilder) {
		const PathToStructMember &path = paths.at(memberName);

		// (pref, [suf])... [node-pref] node-suf member
		if (path.skippedBits > 0 ||  path.skippedRefs > 0) {
			// (pref, [suf])... node-pref node-suf member
			pusher->push(0, "ROT"); // (pref, [suf])... node-suf member node-pref
			pusher->push(+1, "NEWC"); // (pref, [suf])... node-suf member node-pref builder
			pusher->push(-1, "STSLICE"); // (pref, [suf])... node-suf member builder
			store(path.field.member.get(), false, isValueBuilder); // (pref, suf)... node-suf builder
			pusher->push(-1, "STSLICE"); // (pref, [suf])... builder
		} else {
			// (pref, [suf])... node-suf member
			pusher->push(+1, "NEWC"); // (pref, [suf])... node-suf member builder
			store(path.field.member.get(), false, isValueBuilder); // (pref, [suf])... node-suf builder
			pusher->push(-1, "STSLICE"); // (pref, [suf])... builder
		}

		// (pref, [suf])... child
		for (int n = static_cast<int>(path.idRefs.size()), j = n - 1; j >= 0; --j) {
			const int v = path.nodes[j];
			const int idRef = path.idRefs[j];
			if (idRef + 1 == static_cast<int>(nodes[v].children.size())) {
				// (pref, [suf])... pref child
				pusher->push(0, "SWAP"); // child pref
				pusher->push(+1, "NEWC"); // child pref builder-node
				pusher->push(-1, "STSLICE"); // child builder-node
				pusher->push(-1, "STBREF");  // builder-node
			} else {
				// (pref, [suf])... pref suf child
				pusher->push(0, "ROT"); // suf builder pref
				pusher->push(+1, "NEWC"); // suf builder pref builder-node
				pusher->push(-1, "STSLICE"); // suf builder builder-node
				pusher->push(-1, "STBREF"); // suf builder-node
				pusher->push(-1, "STSLICE"); // builder-node
			}
		}

		if (!isResultBuilder) {
			pusher->push(0, "ENDC");
			pusher->push(0, "CTOS");
		}
	}

	void expandStruct(std::map<std::string, int> &memberToStackSize) {
		expandStructDfs(0, "", memberToStackSize);
	}

private:
	void dfs(int v, std::vector<int> &nodePath, std::vector<int> &refPath) {
		nodePath.push_back(v);
		int skippedBits = 0;
		int skippedRefs = 0;
		for (int i = 0, n = static_cast<int>(nodes[v].fields.size()); i < n; ++i) {
			const Field &f = nodes[v].fields[i];
			paths.emplace(f.member->name(), PathToStructMember{nodePath, refPath, skippedBits, skippedRefs, f,
			                                                   !nodes[v].children.empty() || i + 1 != n});

			skippedBits += f.bitLength;
			skippedRefs += f.refs;
		}

		for (const int to : nodes[v].children) {
			refPath.push_back(skippedRefs);
			dfs(to, nodePath, refPath);

			refPath.pop_back();
			++skippedRefs;
		}
		nodePath.pop_back();
	}

	void createDefaultStructDfs(int v) {
		pusher->push(+1, "NEWC");

		const std::vector<Field> &fields = nodes[v].fields;
		const int n = nodes[v].fields.size();
		for (int l = 0, r; l < n; l = r) {
			r = l + 1;
			const Field& f = fields[l];
			if (isPlane(f.member->annotation().type->category())) {
				solAssert(f.refs == 0, "");
				int size = fields[l].bitLength;
				while (r < n && isPlane(fields[r].member->annotation().type->category())) {
					solAssert(fields[r].refs == 0, "");
					size += fields[r].bitLength;
					++r;
				}
				pusher->pushInt(size);
				pusher->push(-2 + 1, "STZEROES");
			} else {
				auto structType = to<StructType>(f.member->type().get());
				if (structType && !isTvmCell(structType)) {
					StructCompiler structCompiler{pusher, &structType->structDefinition()};
					structCompiler.createDefaultStructDfs(0);
					store(f.member.get(), true, true);
				} else {
					pusher->pushDefaultValue(f.member->type().get());
					store(f.member.get(), true);
				}
			}
		}

		for (const int to : nodes[v].children) {
			createDefaultStructDfs(to);
			pusher->push(-1, "STBREFR");
		}
	}

	void createStructDfs(int v, const std::function<void(const int, const std::string &)> &pushOnStack) {
		pusher->push(+1, "NEWC");
		for (Field &f : nodes[v].fields) {
			pushOnStack(f.memberIndex, f.member->name());
			store(f.member.get(), true);
		}

		for (const int to : nodes[v].children) {
			createStructDfs(to, pushOnStack);
			pusher->push(-1, "STBREFR");
		}
	}

	void expandStructDfs(const int v, const std::string& prefix, std::map<std::string, int> &memberToStackSize) {
		// struct
		for (Field &f : nodes[v].fields) {
			load(f.member.get()); // field slice
			if (f.member->type()->category() != Type::Category::Struct) {
				const int stackSizeForMember = pusher->getStack().size() - 1;
				memberToStackSize[prefix + "@" + f.member->name()] = stackSizeForMember;
			} else {
				pusher->push(0, "SWAP"); // slice field
				const int stackSizeForSlice = pusher->getStack().size() - 1;
				auto structType = to<StructType>(f.member->type().get());
				StructCompiler structCompiler{pusher, &structType->structDefinition()};
				structCompiler.expandStructDfs(0, prefix + "@" + f.member->name(), memberToStackSize);

				const int newStackSize = pusher->getStack().size();
				pusher->push(+1, "PUSH s" + toString(newStackSize - stackSizeForSlice));
			}
		}
		// fields-and-trash... struct

		const int stackSizeForStruct = pusher->getStack().size();
		int idRef = 0;
		for (const int to : nodes[v].children) {
			const int newStackSize = pusher->getStack().size();
			pusher->push(+1, "PUSH s" + toString(newStackSize - stackSizeForStruct));
			pusher->push(0, "PLDREFIDX " + toString(idRef));
			pusher->push(0, "CTOS");
			expandStructDfs(to, prefix, memberToStackSize);

			++idRef;
		}
	}

	void load(const VariableDeclaration *vd) {
		// slice
		Type const *type = vd->type().get();
		TypeInfo ti{type};
		switch (vd->type()->category()) {
			case Type::Category::Struct:
				if (isTvmCell(type)) {
					pusher->push(0, "LDREF");
					pusher->push(0, "SWAP");
					break;
				}
				[[fallthrough]];
			case Type::Category::Address:
				pusher->push(+1, "LDREFRTOS");
				pusher->push(0, "SWAP");
				break;
			case Type::Category::Integer:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
				pusher->push(-1 + 2, loadIntegralOrAddress(type));
				break;
			case Type::Category::Array: {
				auto arrayType = to<ArrayType>(type);
				if (arrayType->isByteArray()) {
					pusher->push(-1 + 2, "LDREF");
				} else {
					loadDict();
				}
				break;
			}
			case Type::Category::Mapping:
				loadDict();
				break;
			default:
				solAssert(false, "");
		}
		// data slice
	}

	void preload(const VariableDeclaration *vd) {
		// on stack there is slice
		Type const *type = vd->type().get();
		TypeInfo ti{type};
		switch (vd->type()->category()) {
			case Type::Category::Struct:
				if (isTvmCell(type)) {
					pusher->push(0, "PLDREF");
					break;
				}
				[[fallthrough]];
			case Type::Category::Address:
				pusher->push(-1 + 2, "LDREFRTOS");
				pusher->push(-1, "NIP");
				break;
			case Type::Category::Integer:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
			case Type::Category::Enum:
				pusher->push(-1 + 1, preloadIntergalOrAddress(type));
				break;
			case Type::Category::Array: {
				auto arrayType = to<ArrayType>(type);
				if (arrayType->isByteArray()) {
					pusher->push(0, "PLDREF");
				} else {
					preloadDict();
				}
				break;
			}
			case Type::Category::Mapping:
				preloadDict();
				break;
			default:
				solAssert(false, "");
		}
	}

	void loadDict() {
		pusher->push(-1 + 2, "LDREFRTOS");
		pusher->push(0, "PLDDICT");
		pusher->push(0, "SWAP");
	}

	void preloadDict() {
		pusher->push(-1 + 1, "PLDREF");
		pusher->push(-1 + 1, "CTOS");
		pusher->push(-1 + 1, "PLDDICT");
	}

	void store(const VariableDeclaration *vd, bool reverse, bool isValueBuilder = false) {
		// value   builder
		// builder value    - reverse order
		Type const *type = vd->type().get();
		TypeInfo ti{type};
		switch (vd->type()->category()) {
			case Type::Category::Struct:
				if (isTvmCell(type)) {
					pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
					break;
				}
				[[fallthrough]];
			case Type::Category::Address:
			case Type::Category::Contract:
				if (!reverse) {
					pusher->push(0, "SWAP"); // builder slice-value
				}
				if (!isValueBuilder) {
					pusher->push(+1, "NEWC"); // builder slice-value builder
					pusher->push(-1, "STSLICE"); // builder builder-value
				}
				pusher->push(-1, "STBREFR"); // builder
				break;
			case Type::Category::Integer:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
			case Type::Category::Enum:
				pusher->push(-1, storeIntegralOrAddress(type, reverse));
				break;
			case Type::Category::Mapping:
				storeDict(reverse);
				break;
			case Type::Category::Array: {
				auto arrayType = to<ArrayType>(vd->type().get());
				if (arrayType->isByteArray()) {
					pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
				} else {
					storeDict(reverse);
				}
				break;
			}
			default:
				solAssert(false, "");
		}
	}

	void storeDict(bool reverse) {
		if (!reverse) {
			pusher->push(0, "SWAP"); // builder dict builder
		}
		pusher->push(+1, "NEWC"); // builder dict builder
		pusher->push(-1, "STDICT"); // builder builder-value
		pusher->push(-1, "STBREFR"); // builder
	}

	void skipField(int bits, int refs) {
		if (bits == 0 && refs == 0)
			return;

		if (refs == 0) {
			pusher->push(+1, "PUSHINT " + toString(bits));
			pusher->push(-2 + 1, "SDSKIPFIRST");
			return;
		}

		if (bits == 0 && refs == 1) {
			pusher->push(+1, "LDREF");
			pusher->push(-1, "NIP");
			return;
		}

		pusher->push(+1, "PUSHINT " + toString(bits));
		pusher->push(+1, "PUSHINT " + toString(refs));
		pusher->push(-3 + 1, "SSKIPFIRST");
	}

	void split(int bits, int refs) {
		// slice
		if (refs == 0) {
			solAssert(bits > 0, "");
			if (bits > 256){
				pusher->pushInt(bits);
				pusher->push(-2 + 2, "LDSLICEX");
			} else {
				pusher->push(-1 + 2, "LDSLICE " + toString(bits));
			}
		} else {
			pusher->pushInt(bits);
			pusher->pushInt(refs);
			pusher->push(-3 + 2, "SPLIT"); // value slice
		}
	}

	static bool isPlane(const Type::Category category) {
		switch (category) {
			case Type::Category::Integer:
			case Type::Category::Bool:
			case Type::Category::FixedBytes:
				return true;
			default:
				return false;
		}
	}

public:
	static bool isPlaneStruct(StructType const* structType) {
		int size = 0;
		StructDefinition const& structDefinition = structType->structDefinition();
		std::vector<ASTPointer<VariableDeclaration>> const& members = structDefinition.members();
		for (const ASTPointer<VariableDeclaration>& member : members) {
			if (!isPlane(member->annotation().type->category())) {
				return false;
			}
			TypeInfo ti{member->type().get()};
			size += ti.numBits;
		}
		return size <= TvmConst::CellBitLength;
	}
};
}

