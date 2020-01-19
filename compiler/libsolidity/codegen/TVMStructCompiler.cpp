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


#include "TVMStructCompiler.hpp"

StructCompiler::FieldSizeInfo::FieldSizeInfo(bool isBitFixed, bool isRefFixed, int maxBitLength, int maxRefLength)
	:
	isBitFixed{isBitFixed},
	isRefFixed{isRefFixed},
	maxBitLength{maxBitLength},
	maxRefLength{maxRefLength}
{

}

StructCompiler::FieldSizeInfo::FieldSizeInfo(Type const* type, ASTNode const& node) : type{type} {
	switch (type->category()) {
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedBytes: {
			TypeInfo ti{type};
			isBitFixed = true;
			isRefFixed = true;
			maxBitLength = ti.numBits;
			maxRefLength = 0;
			break;
		}
		case Type::Category::Address:
		case Type::Category::Contract: {
			isBitFixed = false;
			isRefFixed = true;
			maxBitLength = AddressInfo::maxBitLength();
			maxRefLength = 0;
			break;
		}
		case Type::Category::Array:
			if (isStringOrStringLiteralOrBytes(type)) {
				isBitFixed = true;
				isRefFixed = true;
				maxBitLength = 0;
				maxRefLength = 1;
				break;
			}
			isBitFixed = true;
			isRefFixed = false;
			maxBitLength = 32 + 1;
			maxRefLength = 1;
			break;
		case Type::Category::Mapping:
			isBitFixed = true;
			isRefFixed = false;
			maxBitLength = 1;
			maxRefLength = 1;
			break;
		case Type::Category::Struct: {
			// if isTvmCell(type) or usual structure
			isBitFixed = true;
			isRefFixed = true;
			maxBitLength = 0;
			maxRefLength = 1;
			break;
		}
		default:
			cast_error(node, "Unsupported in struct");
	}
}

bool StructCompiler::FieldSizeInfo::tryMerge(const StructCompiler::FieldSizeInfo &fieldSizeInfo) {
	if (!isBitFixed || !isRefFixed || !fieldSizeInfo.isBitFixed || !fieldSizeInfo.isRefFixed) {
		return false;
	}
	maxBitLength += fieldSizeInfo.maxBitLength;
	maxRefLength += fieldSizeInfo.maxRefLength;
	type = nullptr;
	return true;
}

StructCompiler::Field::Field(const int memberIndex, VariableDeclaration const* member) :
		fieldSizeInfo{ member->type().get(), *member},
		memberIndex{memberIndex},
		member{member} {
}

StructCompiler::Node::Node(const int skipData, const int skipRef) : skipData{skipData}, skipRef{skipRef} {
}

bool StructCompiler::Node::tryAddField(const StructCompiler::Field &f, bool useAllRefs) {
	const int refLimit = useAllRefs? 4 : 2;
	if (!(maxBits + skipData + f.fieldSizeInfo.maxBitLength <= TvmConst::CellBitLength &&
	      maxRefs + skipRef + f.fieldSizeInfo.maxRefLength <= refLimit)) {
		return false;
	}
	maxBits += f.fieldSizeInfo.maxBitLength;
	maxRefs += f.fieldSizeInfo.maxRefLength;
	fields.push_back(f);
	return true;
}

void StructCompiler::Node::removeFirstField(int index) {
	maxBits -= fields[index].fieldSizeInfo.maxBitLength;
	maxRefs -= fields[index].fieldSizeInfo.maxRefLength;
	fields.erase(fields.begin() + index);
}

bool StructCompiler::Node::tryAddChild(int child) {
	solAssert(maxRefs + skipRef <= 4, "");
	if (maxRefs + skipRef == 4) {
		return false;
	}
	++maxRefs;
	children.push_back(child);
	return true;
}

StructCompiler::PathToStructMember::PathToStructMember(const std::vector<int> &nodes, const std::vector<int> &childRef,
                                                       vector<FieldSizeInfo> skippedFields, StructCompiler::Field field,
                                                       const bool haveDataOrRefsAfterMember) :
		nodes{nodes}, childRef{childRef}, skippedFields{std::move(skippedFields)}, field{field},
		haveDataOrRefsAfterMember{haveDataOrRefsAfterMember} {
	solAssert(nodes.size() == childRef.size() + 1, "");
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, StructType const *structType) :
		StructCompiler{pusher, fVariableDeclarations(&structType->structDefinition()), 0, 0, false, structType} {
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, std::vector<VariableDeclaration const *> _variableDeclarations,
		const int skipData, const int skipRef, bool isC4, StructType const * structType)
		:
		variableDeclarations{std::move(_variableDeclarations)},
		pusher{pusher}
	{
	nodes.emplace_back(Node(skipData, skipRef));

	int memberIndex = 0;
	int parent = 0;
	for (const auto &member : variableDeclarations) {
		const Field f{memberIndex, member};
		bool isNodeFound = false;
		for (Node& node : nodes) {
			if (node.tryAddField(f)) {
				isNodeFound = true;
				break;
			}
		}

		if (!isNodeFound) {
			nodes.emplace_back(Node(0, 0));
			solAssert(nodes.back().tryAddField(f), "");

			int v = static_cast<int>(nodes.size()) - 1;
			if (!nodes[parent].tryAddChild(v)) {
				++parent;
				solAssert(nodes[parent].tryAddChild(v), "");
			}
		}

		nameToVariableDeclarations[member->name()] = member;
		++memberIndex;
	}

	for (int v = 1; v < static_cast<int>(nodes.size()); ++v) {
		for (int j = 0; j < static_cast<int>(nodes[v].fields.size()); ) {
			bool doMove = false;
			for (int v0 = 0; v0 < v; ++v0) {
				if (nodes[v0].tryAddField(nodes[v].fields[j], true)) {
					nodes[v].removeFirstField(j);
					doMove = true;
					break;
				}
			}
			if (!doMove) {
				++j;
			}
		}
	}

	if (isC4 || !isCompatibleWithSDK(TvmConst::ArrayKeyLength, structType)) {
		for (Node &node : nodes) {
			std::stable_sort(node.fields.begin(), node.fields.end(), [](const Field &a, const Field &b) {
				const bool l = a.fieldSizeInfo.isBitFixed && a.fieldSizeInfo.isRefFixed;
				const bool r = b.fieldSizeInfo.isBitFixed && b.fieldSizeInfo.isRefFixed;
				return l > r;
			});
		}
	}

	std::vector<int> nodePath;
	std::vector<int> refPath;
	dfs(0, nodePath, refPath);
}

void StructCompiler::createDefaultStruct(bool resultIsBuilder) {
	createDefaultStructDfs(0);
	if (!resultIsBuilder) {
		pusher->push(0, "ENDC");
		pusher->push(0, "CTOS");
	}
}

void StructCompiler::pushMember(const std::string &memberName) {
	// struct
	const PathToStructMember &path = paths.at(memberName);
	for (int childRef : path.childRef) {
		pusher->push(0, "PLDREFIDX " + toString(childRef));
		pusher->push(0, "CTOS");
	}

	// TODO optimize for load one ref: struct, bytes by PLDREFIDX
	const VariableDeclaration *vd = nameToVariableDeclarations.at(memberName);
	skip(path.skippedFields);
	preload(vd);
}

void StructCompiler::createStruct(const int argumentStackSize, std::vector<ASTPointer<ASTString>> const& names) {
	std::map<std::string, int> argStackSize{};
	if (!names.empty()) {
		int shift = 0;
		for (const ASTPointer<ASTString>& s : names) {
			argStackSize[*s.get()] = argumentStackSize + shift;
			++shift;
		}
	} else {
		int shift = 0;
		for (const VariableDeclaration* vd : variableDeclarations) {
			if (vd->type()->category() != Type::Category::Mapping) {
				argStackSize[vd->name()] = argumentStackSize + shift;
				++shift;
			}
		}
	}
	createStructDfs(0, argStackSize);
	pusher->push(0, "ENDC CTOS");
}


void StructCompiler::expandStruct(const std::string &memberName, bool doPushMemberOnStack) {
	// slice-struct
	const PathToStructMember &path = paths.at(memberName);
	for (int idRef : path.childRef) {
		if (idRef != 0) {
			split(0, idRef); // pref suf
		}
		// [pref] suf
		pusher->push(-1 + 2, "LDREFRTOS"); // [pref] suf child
	}

	// (suf, [pref])... [node-pref] node
	for (const FieldSizeInfo& fieldSizeInfo : path.skippedFields) {
		split(fieldSizeInfo); // (suf, [pref])... [node-pref...] node-suf
	}

	// (suf, [pref])... [node-pref...] node-suf
	if (doPushMemberOnStack) {
		if (path.haveDataOrRefsAfterMember) {
			load(path.field.member, true); // (suf, [pref])... [node-pref...] node-suf member
		} else {
			preload(path.field.member); // (suf, [pref])... [node-pref...] member
		}
		// (suf, [pref])... [node-pref...] [node-suf] member
	} else {
		if (path.haveDataOrRefsAfterMember) {
			skip(path.field.fieldSizeInfo); // (suf, [pref])... [node-pref...] node-suf
		} else {
			pusher->push(-1, "DROP"); // (suf, [pref])... [node-pref...]
		}
		// (suf, [pref])... [node-pref...] [node-suf]
	}
}

void StructCompiler::collectStruct(const std::string &memberName, bool isValueBuilder, bool isResultBuilder) {
	const PathToStructMember &path = paths.at(memberName);
	int nodePrefQty = static_cast<int>(path.skippedFields.size());
	// (suf, [pref])... [node-pref...] [node-suf] member
	if (nodePrefQty == 1) {
		// (suf, [pref])... node-pref [node-suf] member
		if (path.haveDataOrRefsAfterMember) {
			// (suf, [pref])... node-pref node-suf member
			pusher->push(0, "ROT"); // (suf, [pref])... node-suf member node-pref
		} else {
			// (suf, [pref])... node-pref member
			pusher->push(0, "SWAP"); // (suf, [pref])... member node-pref
		}
	} else if (nodePrefQty > 1) {
		// (suf, [pref])... node-pref... [node-suf] member
		if (path.haveDataOrRefsAfterMember) {
			pusher->push(0, "SWAP"); // (suf, [pref])... node-pref... member node-suf
		}
		// (suf, [pref])... node-pref... member [node-suf]
		pusher->reverse(nodePrefQty + 1 + (path.haveDataOrRefsAfterMember? 1 : 0), 0);
		// (suf, [pref])...  [node-suf] member ...node-pref
	}

	// (suf, [pref])...  [node-suf] member [...node-pref]
	pusher->push(+1, "NEWC"); // (suf, [pref])... [node-suf] member [...node-pref] builder
	for (int iter = 0; iter < nodePrefQty; ++iter) {
		pusher->push(-1, "STSLICE"); // (suf, [pref])... [node-suf] member [...node-pref] builder
	}
	// (suf, [pref])... [node-suf] member builder
	store(path.field.member, false, isValueBuilder); // (suf, [pref])... [node-suf] builder
	if (path.haveDataOrRefsAfterMember) {
		pusher->push(-1, "STSLICE"); // (suf, [pref])... builder
	}

	// ([pref], suf)... child
	for (int n = static_cast<int>(path.childRef.size()), j = n - 1; j >= 0; --j) {
		const int idRef = path.childRef[j];
		if (idRef == 0) {
			// ([pref], suf)... suf child
			pusher->push(+1, "NEWC"); // suf child builder-node
			pusher->push(-1, "STBREF");  // suf builder-node
			pusher->push(-1, "STSLICE"); // builder-node
		} else {
			// ([pref], suf)... pref suf child
			pusher->push(0, "ROT"); // suf child pref
			pusher->push(+1, "NEWC"); // suf child pref node-builder
			pusher->push(-1, "STSLICE"); // suf child node-builder
			pusher->push(-1, "STBREF");  // suf node-builder
			pusher->push(-1, "STSLICE");// node-builder
		}
	}

	if (!isResultBuilder) {
		pusher->push(0, "ENDC");
		pusher->push(0, "CTOS");
	}
}

void StructCompiler::expandStruct(std::map<std::string, int> &memberToStackSize) {
	expandStructDfs(0, "", memberToStackSize);
}

bool StructCompiler::isCompatibleWithSDK(int keyLength, StructType const *structType) {
	int size = 0;
	StructDefinition const& structDefinition = structType->structDefinition();
	std::vector<ASTPointer<VariableDeclaration>> const& members = structDefinition.members();
	for (const ASTPointer<VariableDeclaration>& member : members) {
		if (isIntegralType(member->type().get())) {
			TypeInfo ti{member->type().get()};
			size += ti.numBits;
		} else if (isAddressType(member->type().get())) {
			size += AddressInfo::maxBitLength();
		} else {
			size = 1024;
			break;
		}
	}
	return 2 + keyLength + size <= TvmConst::CellBitLength; // 2 is gotten from hml_long$10
}

void StructCompiler::dfs(int v, std::vector<int> &nodePath, std::vector<int> &refPath) {
	nodePath.push_back(v);
	std::vector<FieldSizeInfo> skippedField{};
	bool childRefInArray = nodes[v].children.empty();
	for (int i = 0, n = static_cast<int>(nodes[v].fields.size()); i < n; ++i) {
		const Field &f = nodes[v].fields[i];
		if (f.fieldSizeInfo.maxRefLength > 0 && !childRefInArray) {
			std::vector<FieldSizeInfo> newSkippedField{};
			FieldSizeInfo childRef{true, true, 0, static_cast<int>(nodes[v].children.size())};
			newSkippedField.push_back(childRef);
			for (int j = 0; j < i; ++j) {
				const Field &fj = nodes[v].fields[j];
				if (!newSkippedField.back().tryMerge(fj.fieldSizeInfo)) {
					newSkippedField.push_back(fj.fieldSizeInfo);
				}
			}
			skippedField = newSkippedField;
			childRefInArray = true;
		}
		paths.emplace(f.member->name(), PathToStructMember{nodePath, refPath, skippedField, f,
		                                                   i + 1 != n || (i + 1 == n && !childRefInArray)});

		if (skippedField.empty() || !skippedField.back().tryMerge(f.fieldSizeInfo)) {
			skippedField.push_back(f.fieldSizeInfo);
		}
	}

	int idRef = 0;
	for (const int to : nodes[v].children) {
		refPath.push_back(idRef);
		dfs(to, nodePath, refPath);

		refPath.pop_back();
		++idRef;
	}
	nodePath.pop_back();
}

void StructCompiler::createDefaultStructDfs(int v) {
	pusher->push(+1, "NEWC");

	for (const int to : nodes[v].children) {
		createDefaultStructDfs(to);
		pusher->push(-1, "STBREFR");
	}

	const std::vector<Field> &fields = nodes[v].fields;
	const int n = nodes[v].fields.size();
	for (int l = 0, r; l < n; l = r) {
		r = l + 1;
		const Field& f = fields[l];
		if (f.fieldSizeInfo.isBitFixed && f.fieldSizeInfo.maxRefLength == 0) {
			int size = fields[l].fieldSizeInfo.maxBitLength;
			while (r < n && fields[r].fieldSizeInfo.isBitFixed && fields[r].fieldSizeInfo.maxRefLength == 0) {
				size += fields[r].fieldSizeInfo.maxBitLength;
				++r;
			}
			pusher->pushInt(size);
			pusher->push(-2 + 1, "STZEROES");
		} else {
			auto structType = to<StructType>(f.member->type().get());
			if (structType && !isTvmCell(structType)) {
				StructCompiler structCompiler{pusher, structType};
				structCompiler.createDefaultStructDfs(0);
				store(f.member, true, true);
			} else {
				pusher->pushDefaultValue(f.member->type().get());
				store(f.member, true);
			}
		}
	}
}

void StructCompiler::createStructDfs(const int v, const std::map<std::string, int> &argStackSize) {
	pusher->push(+1, "NEWC");

	for (const int to : nodes[v].children) {
		createStructDfs(to, argStackSize);
		pusher->push(-1, "STBREFR");
	}

	for (const Field &f : nodes[v].fields) {
		if (f.member->type()->category() == Type::Category::Mapping) {
			pusher->pushDefaultValue(f.member->type().get());
		} else {
			const int curStackSize = pusher->getStack().size();
			const int index = curStackSize - 1 - argStackSize.at(f.member->name());
			pusher->push(+1, "PUSH s" + toString(index));
		}
		store(f.member, true);
	}
}

void
StructCompiler::expandStructDfs(const int v, const std::string &prefix, std::map<std::string, int> &memberToStackSize) {
	// struct

	const int stackSizeForStruct = pusher->getStack().size();
	if (!nodes[v].children.empty()) {
		split(0, nodes[v].children.size());
	}

	for (Field &f : nodes[v].fields) {
		if (isUsualStruct(f.member->type().get())) {
			load(f.member, true); // slice field
			const int stackSizeForSlice = pusher->getStack().size() - 1;
			auto structType = to<StructType>(f.member->type().get());
			StructCompiler structCompiler{pusher, structType};
			structCompiler.expandStructDfs(0, prefix + "@" + f.member->name(), memberToStackSize);

			const int newStackSize = pusher->getStack().size();
			pusher->push(+1, "PUSH s" + toString(newStackSize - stackSizeForSlice));
		} else {
			load(f.member, false); // field slice
			const int stackSizeForMember = pusher->getStack().size() - 1;
			memberToStackSize[prefix + "@" + f.member->name()] = stackSizeForMember;
		}
	}
	// fields-and-trash... struct


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

void StructCompiler::load(const VariableDeclaration *vd, bool reverseOrder) {
	// slice
	Type const *type = vd->type().get();
	switch (vd->type()->category()) {
		case Type::Category::Struct:
			if (isTvmCell(type)) {
				pusher->push(-1 + 2, "LDREF");
				if (reverseOrder) {
					pusher->push(0, "SWAP");
				}
				break;
			}
			pusher->push(+1, "LDREFRTOS");
			if (!reverseOrder) {
				pusher->push(0, "SWAP");
			}
			break;
		case Type::Category::Address:
		case Type::Category::Contract:
			pusher->push(-1 + 2, "LDMSGADDR");
			if (reverseOrder) {
				pusher->push(0, "SWAP");
			}
			break;
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			pusher->push(-1 + 2, loadIntegral(type));
			if (reverseOrder) {
				pusher->push(0, "SWAP");
			}
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				pusher->push(-1 + 2, "LDREF");
				if (reverseOrder) {
					pusher->push(0, "SWAP");
				}
			} else {
				pusher->loadArray();
				if (reverseOrder) {
					pusher->push(0, "SWAP");
				}
			}
			break;
		}
		case Type::Category::Mapping:
			pusher->push(-1 + 2, "LDDICT");
			if (reverseOrder) {
				pusher->push(0, "SWAP");
			}
			break;
		default:
			solAssert(false, "");
	}
	// reverseOrder? slice member : member slice
}

void StructCompiler::preload(const VariableDeclaration *vd) {
	// on stack there is slice
	Type const *type = vd->type().get();
	switch (vd->type()->category()) {
		case Type::Category::Address:
		case Type::Category::Contract:
			if (paths.at(vd->name()).haveDataOrRefsAfterMember) {
				pusher->push(-1 + 2, "LDMSGADDR");
				pusher->drop(1);
			}
			break;
		case Type::Category::Struct:
			if (isTvmCell(type)) {
				pusher->push(0, "PLDREF");
				break;
			}
			pusher->push(-1 + 2, "LDREFRTOS");
			pusher->push(-1, "NIP");
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			pusher->push(-1 + 1, preloadIntergal(type));
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				pusher->push(0, "PLDREF");
			} else {
				pusher->preLoadArray();
			}
			break;
		}
		case Type::Category::Mapping:
			pusher->push(-1 + 1, "PLDDICT");
			break;
		default:
			solAssert(false, "");
	}
}

void StructCompiler::store(const VariableDeclaration *vd, bool reverse, bool isValueBuilder) {
	// value   builder
	// builder value    - reverse order
	Type const *type = vd->type().get();
	switch (vd->type()->category()) {
		case Type::Category::Struct:
			if (isTvmCell(type)) {
				pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
				break;
			}
			if (!reverse) {
				pusher->push(0, "SWAP"); // builder slice-value
			}
			if (!isValueBuilder) {
				pusher->push(+1, "NEWC"); // builder slice-value builder
				pusher->push(-1, "STSLICE"); // builder builder-value
			}
			pusher->push(-1, "STBREFR"); // builder
			break;
		case Type::Category::Address:
		case Type::Category::Contract:
			pusher->push(-1, reverse? "STSLICER" : "STSLICE"); // builder slice-value
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			pusher->push(-1, storeIntegralOrAddress(type, reverse));
			break;
		case Type::Category::Mapping:
			if (reverse) {
				pusher->push(0, "SWAP"); // builder dict
			}
			// dict builder
			pusher->push(-1, "STDICT"); // builder
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(vd->type().get());
			if (arrayType->isByteArray()) {
				pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
			} else {
				if (isValueBuilder) {
					pusher->push(-1, reverse ? "STBR" : "STB");
				} else {
					pusher->push(-1, reverse ? "STSLICER" : "STSLICE");
				}
			}
			break;
		}
		default:
			solAssert(false, "");
	}
}

void StructCompiler::skip(int bits, int refs) {
	if (bits == 0 && refs == 0)
		return;

	if (refs == 0) {
		if (bits <= 256) {
			pusher->push(-1 + 2, "LDSLICE " + toString(bits));
			pusher->dropUnder(1, 1);
		} else {
			pusher->push(+1, "PUSHINT " + toString(bits));
			pusher->push(-2 + 1, "SDSKIPFIRST");
		}
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

void StructCompiler::skip(const StructCompiler::FieldSizeInfo &si) {
	if (si.isBitFixed && si.isRefFixed) {
		skip(si.maxBitLength, si.maxRefLength);
	} else {
		switch (si.type->category()) {
			case Type::Category::Address:
			case Type::Category::Contract:
				pusher->push(-1 + 2, "LDMSGADDR");
				pusher->dropUnder(1, 1);
				break;
			case Type::Category::Mapping:
				pusher->push(-1 + 1, "SKIPDICT");
				break;
			case Type::Category::Array:
				pusher->loadArray(); // array slice
				pusher->dropUnder(1, 1); // slice
				break;
			default:
				solAssert(false, "Unsupported skip");
		}
	}
}

void StructCompiler::skip(const vector<FieldSizeInfo> &fieldInfo) {
	for (const FieldSizeInfo& fi : fieldInfo) {
		skip(fi);
	}
}

void StructCompiler::split(const StructCompiler::FieldSizeInfo &fieldSizeInfo) {
	if (fieldSizeInfo.isBitFixed && fieldSizeInfo.isRefFixed) {
		split(fieldSizeInfo.maxBitLength, fieldSizeInfo.maxRefLength);
	} else {
		switch (fieldSizeInfo.type->category()) {
			case Type::Category::Address:
			case Type::Category::Contract:
				pusher->push(-1 + 2, "LDMSGADDR");
				break;
			case Type::Category::Mapping:
				pusher->push(-1 + 2, "LDDICTS");
				break;
			case Type::Category::Array:
				pusher->loadArray();
				break;
			default:
				solAssert(false, "Unsupported split");
		}
	}
}

void StructCompiler::split(int bitQty, int refQty) {
	// slice
	if (refQty == 0) {
		solAssert(bitQty > 0, "");
		if (bitQty > 256){
			pusher->pushInt(bitQty);
			pusher->push(-2 + 2, "LDSLICEX");
		} else {
			pusher->push(-1 + 2, "LDSLICE " + toString(bitQty));
		}
	} else {
		pusher->pushInt(bitQty);
		pusher->pushInt(refQty);
		pusher->push(-3 + 2, "SPLIT"); // value slice
	}
}

std::vector<VariableDeclaration const *> StructCompiler::fVariableDeclarations(StructDefinition const *structDefinition) {
	std::vector<VariableDeclaration const*> result;
	for (const ASTPointer<VariableDeclaration>& vd : structDefinition->members()) {
		result.push_back(vd.get());
	}
	return result;
}