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

#include "TVMCommons.hpp"
#include "TVMPusher.hpp"
#include "TVMStructCompiler.hpp"

using namespace solidity::frontend;

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
		case Type::Category::TvmCell:
		case Type::Category::Struct: {
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
		fieldSizeInfo{ member->type(), *member},
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

void StructCompiler::Node::removeChildIfHave(int v) {
	auto it = std::find(children.begin(), children.end(), v);
	if (it != children.end()) {
		--maxRefs;
		children.erase(it);
	}
}

StructCompiler::PathToStructMember::PathToStructMember(const std::vector<int> &nodes, const std::vector<int> &childRef,
                                                       vector<FieldSizeInfo> skippedFields, StructCompiler::Field field,
                                                       const bool haveDataOrRefsAfterMember) :
		nodes{nodes}, childRef{childRef}, skippedFields{std::move(skippedFields)}, field{field},
		haveDataOrRefsAfterMember{haveDataOrRefsAfterMember} {
	solAssert(nodes.size() == childRef.size() + 1, "");
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, StructType const *structType) :
		StructCompiler{pusher, fVariableDeclarations(&structType->structDefinition()), 0, 0, false} {
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, std::vector<VariableDeclaration const *> _variableDeclarations,
		const int skipData, const int skipRef, bool isC4)
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

	bool doSome {};
	do {
		doSome = false;
		for (int v = 1; v < static_cast<int>(nodes.size()); ++v) {
			for (int fieldInd = 0; fieldInd < static_cast<int>(nodes[v].fields.size());) {
				bool doMove = false;
				for (int v0 = 0; v0 < v; ++v0) {
					if (nodes[v0].tryAddField(nodes[v].fields[fieldInd], true)) {
						nodes[v].removeFirstField(fieldInd);
						doMove = true;
						doSome = true;
						break;
					}
				}
				if (!doMove) {
					++fieldInd;
				}
			}
		}
		if (nodes.size() >= 2 && nodes.back().fields.empty() && nodes.back().fields.empty()) {
			int v = static_cast<int>(nodes.size()) - 1;
			for (Node& node : nodes) {
				node.removeChildIfHave(v);
			}
			nodes.pop_back();

			doSome = true;
		}
	} while (doSome);

	if (isC4 || !isCompatibleWithSDK(TvmConst::ArrayKeyLength)) {
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
	if (resultIsBuilder) {
		createDefaultStructDfs(0);
	} else {
		for (VariableDeclaration const* var : variableDeclarations) {
			pusher->pushDefaultValue(var->type(), false);
		}
		pusher->tuple(variableDeclarations.size());
	}
}

void StructCompiler::pushMember(const std::string &memberName, bool isStructTuple, bool returnStructAsSlice) {
	// struct
	if (isStructTuple) {
		pusher->index(paths.at(memberName).field.memberIndex);
		solAssert(!returnStructAsSlice, "");
	} else {
		const PathToStructMember &path = paths.at(memberName);
		for (int childRef : path.childRef) {
			if (childRef == 0) {
				pusher->push(0, "PLDREF");
			} else {
				pusher->push(0, "PLDREFIDX " + toString(childRef));
			}
			pusher->push(0, "CTOS");
		}

		// TODO optimize for load one ref: struct, bytes by PLDREFIDX
		const VariableDeclaration *vd = nameToVariableDeclarations.at(memberName);
		skip(path.skippedFields);
		preload(vd, returnStructAsSlice);
	}
}

void StructCompiler::setMemberForTuple(const std::string &memberName) {
	pusher->set_index(paths.at(memberName).field.memberIndex);
}

void StructCompiler::structConstructor(std::vector<ASTPointer<ASTString>> const& names) {
	std::vector<std::string> order;
	if (names.empty()) {
		// where args are located
		int saveStackSize = pusher->getStack().size();
		for (const VariableDeclaration *vd : variableDeclarations) {
			if (vd->type()->category() != Type::Category::Mapping) {
				--saveStackSize;
			}
		}

		int i{};
		for (const VariableDeclaration *vd : variableDeclarations) {
			if (vd->type()->category() == Type::Category::Mapping) {
				pusher->pushDefaultValue(vd->type(), false);
				int topQty = pusher->getStack().size() - saveStackSize - 1 - i;
				pusher->blockSwap(topQty, 1);
			}
			order.push_back(vd->name());
			++i;
		}
	} else {
		for (const ASTPointer<ASTString> &n : names) {
			order.push_back(*n.get());
		}
		for (const VariableDeclaration *vd : variableDeclarations) {
			if (vd->type()->category() == Type::Category::Mapping) {
				auto it = std::find(order.begin(), order.end(), vd->name());
				solAssert(it == order.end(), "");
				pusher->pushDefaultValue(vd->type());
				order.push_back(vd->name());
			}
		}
	}

	solAssert(order.size() == variableDeclarations.size(), "");
	sortOnStack(order);
	const int argQty = static_cast<int>(variableDeclarations.size());
	pusher->tuple(argQty);
}

void StructCompiler::tupleToBuilder() {
	const int argumentStackSize = pusher->getStack().size() - 1;
	pusher->untuple(variableDeclarations.size());
	std::map<std::string, int> argStackSize{};
	int shift = 0;
	for (const VariableDeclaration* vd : variableDeclarations) {
		argStackSize[vd->name()] = argumentStackSize + shift;
		++shift;
	}
	createStructDfs(0, argStackSize);
	pusher->dropUnder(1, variableDeclarations.size());
}

void StructCompiler::stateVarsToBuilder() {
	stateVarsToBuilderDfs(0);
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
			preload(path.field.member, true); // (suf, [pref])... [node-pref...] member
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

void StructCompiler::collectStruct(const std::string &memberName, bool isValueBuilder) {
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
	for (const FieldSizeInfo& fieldSizeInfo : path.skippedFields) {
		merge(fieldSizeInfo); // (suf, [pref])... [node-suf] member [...node-pref] builder
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
}

void StructCompiler::convertSliceToTuple() {
	std::vector<std::string> names;
	convertSliceToTupleDfs(0, names);
	sortOnStack(names);
	pusher->tuple(names.size());
}

void StructCompiler::sliceToStateVarsToC7() {
	sliceToStateVarsToC7Dfs(0);
}

bool StructCompiler::isCompatibleWithSDK(int keyLength, StructType const *structType) {
	StructCompiler sc{nullptr, structType};
	return sc.isCompatibleWithSDK(keyLength);
}

bool StructCompiler::isCompatibleWithSDK(int keyLength) const {
	for (VariableDeclaration const* vd : variableDeclarations) {
		Type const *type = vd->type();
		if (type->category() == Type::Category::Struct) {
			return false;
		}
	}
	return nodes.size() == 1 &&
		2 + static_cast<int>(std::ceil(log2(keyLength + 1)+ 1e-5)) + keyLength + nodes[0].maxBitLength() <= TvmConst::CellBitLength; // 2 is gotten from hml_long$10
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

	auto onlyBitData = [](const Field& f) {
		return f.fieldSizeInfo.isBitFixed &&
					(f.fieldSizeInfo.maxRefLength == 0 ||
						isUsualArray(f.member->type()) ||
						f.member->type()->category() == Type::Category::Mapping);
	};

	const std::vector<Field> &fields = nodes[v].fields;
	const int n = nodes[v].fields.size();
	for (int l = 0, r; l < n; l = r) {
		r = l + 1;
		const Field& f = fields[l];
		if (onlyBitData(f)) {
			int size = fields[l].fieldSizeInfo.maxBitLength;
			while (r < n && onlyBitData(fields[r])) {
				size += fields[r].fieldSizeInfo.maxBitLength;
				++r;
			}
			pusher->pushInt(size);
			pusher->push(-2 + 1, "STZEROES");
		} else {
			auto structType = to<StructType>(f.member->type());
			if (structType) {
				StructCompiler structCompiler{pusher, structType};
				structCompiler.createDefaultStructDfs(0);
				store(f.member, true, true);
			} else {
				pusher->pushDefaultValue(f.member->type());
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
		const int curStackSize = pusher->getStack().size();
		const int index = curStackSize - 1 - argStackSize.at(f.member->name());
		pusher->pushS(index);
		store(f.member, true);
	}
}

void StructCompiler::stateVarsToBuilderDfs(const int v) {
	int cntValues = 0;
	for (const int to : nodes[v].children) {
		pusher->push(+1, "NEWC");
		++cntValues;
		stateVarsToBuilderDfs(to);
	}
	for (const Field &f : nodes[v].fields) {
		pusher->getGlob(f.member);
		if (isUsualArray(f.member->type())) {
			pusher->untuple(2); // size dict
			cntValues += 2;
		} else {
			cntValues++;
		}
	}

	if (cntValues + 1 >= 2) {
		pusher->reverse(cntValues + 1, 0);
	}
	for (const int to : nodes[v].children) {
		(void) to;
		pusher->push(-1, "STBREF");
	}
	for (const Field &f : nodes[v].fields) {
		store(f.member, false, false, true);
	}
}

void StructCompiler::convertSliceToTupleDfs(int v, std::vector<std::string>& names) {
	// struct

	for (const int to : nodes[v].children) {
		// members... struct
		int stackSizeForStruct = pusher->getStack().size();
		pusher->push(+1, "LDREFRTOS"); // members... struct child
		convertSliceToTupleDfs(to, names);
		// members... struct members...

		const int memberQty = pusher->getStack().size() - stackSizeForStruct;
		pusher->blockSwap(1, memberQty);
		// members... struct
	}

	// members... struct
	std::size_t iter = 0;
	for (Field &f : nodes[v].fields) {
		if (iter + 1 == nodes[v].fields.size()) {
			preload(f.member, false); // field
		} else {
			load(f.member, false); // field slice
		}
		names.push_back(f.member->name());
		++iter;
	}
}

void StructCompiler::sliceToStateVarsToC7Dfs(const int v) {
	// slice
	for (const int to : nodes[v].children) {
		pusher->push(+1, "LDREFRTOS"); // slice child[i]
		sliceToStateVarsToC7Dfs(to);
	}

	if (nodes[v].fields.empty()) {
		pusher->push(-1, "ENDS");
		return;
	}

	// slice
	std::vector<Field> fields;
	for (std::size_t i = 0; i < nodes[v].fields.size(); ++i) {
		Field &f = nodes[v].fields[i];
		if (i + 1 == nodes[v].fields.size()) {
			preload(f.member, false); // field
			pusher->setGlob(f.member);
		} else {
			bool directOrder = fastLoad(f.member);
			if (directOrder) { // value slice
				fields.push_back(f);
			} else { // slice value
				pusher->setGlob(f.member);
			}
		}
	}
	while (!fields.empty()) {
		pusher->setGlob(fields.back().member);
		fields.pop_back();
	}
}

void StructCompiler::load(const VariableDeclaration *vd, bool reverseOrder) {
	// slice
	bool directOrder = fastLoad(vd);
	if (directOrder == reverseOrder) {
		pusher->exchange(0, 1);
	}
	// reverseOrder? slice member : member slice
}

bool StructCompiler::fastLoad(const VariableDeclaration *vd) {
	Type const *type = vd->type();
	switch (vd->type()->category()) {
		case Type::Category::TvmCell:
			pusher->push(-1 + 2, "LDREF");
			return true;
		case Type::Category::Struct: {
			pusher->push(+1, "LDREFRTOS");
			// slice structAsSlice
			auto st = to<StructType>(type);
			StructCompiler sc{pusher, st};
			sc.convertSliceToTuple();
			return false;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			pusher->push(-1 + 2, "LDMSGADDR");
			return true;
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			pusher->load(type);
			return true;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(type);
			if (arrayType->isByteArray()) {
				pusher->push(-1 + 2, "LDREF");
				return true;
			} else {
				pusher->loadArray(false);
				return false;
			}
		}
		case Type::Category::Mapping:
			pusher->push(-1 + 2, "LDDICT");
			return true;
		default:
			solAssert(false, "");
	}
}

void StructCompiler::preload(const VariableDeclaration *vd, bool returnStructAsSlice) {
	// on stack there is slice
	Type const *type = vd->type();
	switch (vd->type()->category()) {
		case Type::Category::Address:
		case Type::Category::Contract:
			if (paths.at(vd->name()).haveDataOrRefsAfterMember) {
				pusher->push(-1 + 2, "LDMSGADDR");
				pusher->drop(1);
			}
			break;
		case Type::Category::TvmCell:
			pusher->push(0, "PLDREF");
			break;
		case Type::Category::Struct:
			pusher->push(-1 + 2, "LDREFRTOS");
			pusher->push(-1, "NIP");
			if (!returnStructAsSlice) {
				auto structType = to<StructType>(type);
				StructCompiler sc{pusher, structType};
				sc.convertSliceToTuple();
			}
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			pusher->preload(type);
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

void StructCompiler::store(const VariableDeclaration *vd, bool reverse, bool isValueBuilder, bool isArrayUntupled) {
	// value   builder
	// builder value    - reverse order
	Type const *type = vd->type();
	switch (vd->type()->category()) {
		case Type::Category::TvmCell:
			pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
			break;
		case Type::Category::Struct: {
			if (isValueBuilder) {
				pusher->push(-1, reverse? "STBREFR" : "STBREF"); // builder
			} else {
				if (!reverse) {
					pusher->push(0, "SWAP"); // builder struct
				}
				// builder struct
				auto st = to<StructType>(type);
				StructCompiler sc{pusher, st};
				sc.tupleToBuilder();
				pusher->push(-1, "STBREFR"); // builder
			}
			break;
		}
		case Type::Category::Address:
		case Type::Category::Contract:
			pusher->push(-1, reverse? "STSLICER" : "STSLICE"); // builder slice-value
			break;
		case Type::Category::Integer:
		case Type::Category::Enum:
		case Type::Category::Bool:
		case Type::Category::FixedBytes:
			if (isValueBuilder) {
				pusher->push(-1, reverse ? "STBR" : "STB");
			} else {
				pusher->push(-1, storeIntegralOrAddress(type, reverse));
			}
			break;
		case Type::Category::Mapping:
			if (reverse) {
				pusher->push(0, "SWAP"); // builder dict
			}
			// dict builder
			pusher->push(-1, "STDICT"); // builder
			break;
		case Type::Category::Array: {
			auto arrayType = to<ArrayType>(vd->type());
			if (arrayType->isByteArray()) {
				pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
			} else {
				if (isValueBuilder) {
					solAssert(!isArrayUntupled, "");
					pusher->push(-1, reverse ? "STBR" : "STB");
				} else {
					if (isArrayUntupled) {
						solAssert(!reverse, "");
						// dict size builder
						pusher->push(-1, "STU 32");
						pusher->push(-1, "STDICT");
					} else {
						if (!reverse) {
							pusher->push(0, "SWAP"); // builder arr
						}
						pusher->push(-1 + 2, "UNPAIR"); // builder size dict
						pusher->push(0, "ROTREV"); // dict builder size
						pusher->push(-1, "STUR 32"); // dict builder'
						pusher->push(-1, "STDICT"); // builder''
					}
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
				skip(32, 0);
				pusher->push(-1 + 1, "SKIPDICT");
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
	// slice
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
				solAssert(isUsualArray(fieldSizeInfo.type), "");
				pusher->push(-1 + 2, "LDSLICE 32"); // sliceSize slice
				pusher->push(-1 + 2, "LDDICTS"); // sliceSize dictSlice slice
				pusher->push(0, "ROTREV"); // slice sliceSize dictSlice
				pusher->push(-2 + 1, "PAIR"); // slice (sliceSize, dictSlice)
				pusher->push(0, "SWAP");// (sliceSize, dictSlice) slice
				break;
			default:
				solAssert(false, "Unsupported split");
		}
	}
	// value(slice or pair for array) restSlice
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

void StructCompiler::merge(const StructCompiler::FieldSizeInfo &fieldSizeInfo) {
	if (isUsualArray(fieldSizeInfo.type)) {
		// arr builder
		pusher->push(0, "SWAP"); // builder arr
		pusher->push(-1 + 2, "UNPAIR"); // builder sizeSlice dictSlice
		pusher->exchange(0, 2); // dictSlice sizeSlice builder
		pusher->push(-1, "STSLICE");
		pusher->push(-1, "STSLICE");
	} else {
		pusher->push(-1, "STSLICE");
	}
}

std::vector<VariableDeclaration const *> StructCompiler::fVariableDeclarations(StructDefinition const *structDefinition) {
	std::vector<VariableDeclaration const*> result;
	for (const ASTPointer<VariableDeclaration>& vd : structDefinition->members()) {
		result.push_back(vd.get());
	}
	return result;
}

void StructCompiler::sortOnStack(std::vector<std::string> &order) {
	const int n = static_cast<int>(order.size());
	for (int i = 0; i < n; ++i) {
		if (variableDeclarations[i]->name() != order[i]) {
			for (int j = i + 1; j < n; ++j) {
				if (variableDeclarations[i]->name() == order[j]) {
					pusher->exchange(n - 1 - j, n - 1 - i);
					swap(order[i], order[j]);
					break;
				}
				solAssert(j + 1 != n, "");
			}
		}
	}
}


