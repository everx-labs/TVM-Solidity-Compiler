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

#include <utility>

using namespace solidity::frontend;

StructCompiler::FieldSizeInfo::FieldSizeInfo(bool isBitFixed, bool isRefFixed, int maxBitLength, int maxRefLength)
	:
	isBitFixed{isBitFixed},
	isRefFixed{isRefFixed},
	maxBitLength{maxBitLength},
	maxRefLength{maxRefLength}
{

}

StructCompiler::FieldSizeInfo::FieldSizeInfo(Type const* type) : type{type} {
	switch (type->category()) {
		case Type::Category::Enum:
		case Type::Category::Integer:
		case Type::Category::Bool:
		case Type::Category::FixedPoint:
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
		case Type::Category::ExtraCurrencyCollection:
			isBitFixed = true;
			isRefFixed = false;
			maxBitLength = 1;
			maxRefLength = 1;
			break;
		case Type::Category::TvmCell:
		case Type::Category::TvmBuilder:
		case Type::Category::Struct: {
			isBitFixed = true;
			isRefFixed = true;
			maxBitLength = 0;
			maxRefLength = 1;
			break;
		}
		case Type::Category::Optional:
			isBitFixed = true;
			isRefFixed = false;
			maxBitLength = 1;
			maxRefLength = 1;
			break;
		case Type::Category::Function:
			isBitFixed = true;
			isRefFixed = true;
			maxBitLength = 32;
			maxRefLength = 0;
			break;
		default:
			solUnimplemented("Unsupported in struct");
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

StructCompiler::Field::Field(const int memberIndex, Type const* type, std::string name) :
		fieldSizeInfo{type},
		memberIndex{memberIndex},
		type{type},
		name{std::move(name)} {
}

StructCompiler::Node::Node(const int skipData) : skipData{skipData} {
}

bool StructCompiler::Node::tryAddField(const StructCompiler::Field &f, bool useAllRefs, bool withDeleteChild) {
	const int refLimit = useAllRefs? 4 : 2;
	if (withDeleteChild) {
		solAssert(maxRefs() >= 1, "");
	}
	if (!(maxBits() + skipData + f.fieldSizeInfo.maxBitLength <= TvmConst::CellBitLength &&
		 (maxRefs() + (withDeleteChild? - 1 : 0)) + f.fieldSizeInfo.maxRefLength <= refLimit)) {
		return false;
	}
	fields.push_back(f);
	return true;
}

void StructCompiler::Node::removeField(int index) {
	fields.erase(fields.begin() + index);
}

bool StructCompiler::Node::tryAddChild(int child) {
	solAssert(maxRefs() <= 4, "");
	if (maxRefs() == 4) {
		return false;
	}
	children.push_back(child);
	return true;
}

void StructCompiler::Node::removeChildIfHave(int v) {
	auto it = std::find(children.begin(), children.end(), v);
	if (it != children.end()) {
		children.erase(it);
	}
}

int StructCompiler::Node::maxBits() const {
	int b = 0;
	for (const Field& f : fields) {
		b += f.fieldSizeInfo.maxBitLength;
	}
	return b;
}

int StructCompiler::Node::maxRefs() const {
	int r = 0;
	for (const Field& f : fields) {
		r += f.fieldSizeInfo.maxRefLength;
	}
	r += children.size();
	return r;
}

StructCompiler::PathToStructMember::PathToStructMember(const std::vector<int> &nodes, const std::vector<int> &childRef,
													   vector<FieldSizeInfo> skippedFields, StructCompiler::Field field,
													   const bool haveDataOrRefsAfterMember) :
		nodes{nodes}, childRef{childRef}, skippedFields{std::move(skippedFields)}, field{std::move(field)},
		haveDataOrRefsAfterMember{haveDataOrRefsAfterMember} {
	solAssert(nodes.size() == childRef.size() + 1, "");
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

StructCompiler::StructCompiler(StackPusherHelper *pusher, StructType const *structType) :
		StructCompiler{pusher, getTypesFrom(&structType->structDefinition()), getNamesFrom(&structType->structDefinition()), 0, false} {
}

StructCompiler::StructCompiler(
	StackPusherHelper *pusher,
	const std::vector<Type const*>& memberTypes,
	std::vector<std::string> memberNames,
	const int skipData,
	bool isC4
) :
	memberNames{memberNames},
	memberTypes{memberTypes},
	pusher{pusher}
{

	int memberIndex = 0;
	int parent = 0;
	nodes.emplace_back(Node(skipData));
	for (const auto &type : memberTypes) {
		const Field f{memberIndex, type, memberNames.at(memberIndex)};
		bool ok = nodes.back().tryAddField(f);
		if (!ok) {
			nodes.emplace_back(Node(0));
			solAssert(nodes.back().tryAddField(f), "");

			int v = static_cast<int>(nodes.size()) - 1;
			if (!nodes[parent].tryAddChild(v)) {
				++parent;
				solAssert(nodes[parent].tryAddChild(v), "");
			}
		}

		nameToType[memberNames.at(memberIndex)] = type;
		++memberIndex;
	}

	bool doSome{};
	do {
		doSome = false;
		for (int v = 1; v < static_cast<int>(nodes.size()); ++v) {
			for (int fieldInd = 0;
				 !doSome && fieldInd < (isC4? static_cast<int>(nodes[v].getFields().size()) : std::min<int>(nodes[v].getFields().size(), 1));
				++fieldInd) {
				for (int v0 = isC4 ? 0 : v - 1; !doSome && v0 < v; ++v0) {
					if (nodes[v0].tryAddField(nodes[v].getFields()[fieldInd], true)) {
						nodes[v].removeField(fieldInd);
						doSome = true;
					} else {
						bool vIsChildOfV0 = std::find(nodes[v0].getChildren().begin(), nodes[v0].getChildren().end(), v) !=
											nodes[v0].getChildren().end();
						if (nodes[v].getFields().size() == 1 &&
							vIsChildOfV0 &&
							v + 1 == static_cast<int>(nodes.size()) &&
							nodes[v0].tryAddField(nodes[v].getFields()[fieldInd], true, true)) {
							nodes[v].removeField(fieldInd);
							doSome = true;
						}
					}
				}
			}
		}
		if (nodes.size() >= 2 && nodes.back().getFields().empty()) {
			int v = static_cast<int>(nodes.size()) - 1;
			for (Node &node : nodes) {
				node.removeChildIfHave(v);
			}
			nodes.pop_back();

			doSome = true;
		}
	} while (doSome);

	std::vector<int> nodePath;
	std::vector<int> refPath;
	dfs(0, nodePath, refPath);
}

void StructCompiler::createDefaultStruct(bool resultIsBuilder) {
	if (resultIsBuilder) {
		createDefaultStructDfs(0);
	} else {
		for (Type const* type : memberTypes) {
			pusher->pushDefaultValue(type, false);
		}
		pusher->tuple(memberTypes.size());
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
		const Type *type = nameToType.at(memberName);
		skip(path.skippedFields);
		pusher->preload(
			type,
			(returnStructAsSlice ? StackPusherHelper::Preload::ReturnStructAsSlice : 0) |
			(!paths.at(memberName).haveDataOrRefsAfterMember ? StackPusherHelper::Preload::IsAddressInEnd : 0)
		);
	}
}

void StructCompiler::setMemberForTuple(const std::string &memberName) {
	pusher->setIndex(paths.at(memberName).field.memberIndex);
}

void StructCompiler::structConstructor(
	std::vector<ASTPointer<ASTString>> const& names,
	const std::function<void(int, Type const*)>& pushParam
) {
	if (names.empty()) {
		int i{};
		for (const Type *type : memberTypes) {
			if (type->category() == Type::Category::Mapping) {
				pusher->pushDefaultValue(type, false);
			} else {
				pushParam(i++, type);
			}
		}
	} else {
		int i = 0;
		for (const Type *type : memberTypes) {
			const std::string& memberName = memberNames.at(i);
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

	pusher->tuple(memberTypes.size());
}

void StructCompiler::tupleToBuilder() {
	// stack: value
	const int argumentStackSize = pusher->getStack().size() - 1;
	pusher->untuple(memberNames.size());
	std::map<std::string, int> argStackSize{};
	int shift = 0;
	for (const std::string& name : memberNames) {
		argStackSize[name] = argumentStackSize + shift;
		++shift;
	}
	createStructDfs(0, argStackSize);
	pusher->dropUnder(1, memberNames.size());
	// stack: builder
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
			pusher->load(path.field.type, true); // (suf, [pref])... [node-pref...] node-suf member
		} else {
			// TODO add test
			// (suf, [pref])... [node-pref...] member
			pusher->preload(
				path.field.type,
				StackPusherHelper::Preload::ReturnStructAsSlice |
				(!paths.at(path.field.name).haveDataOrRefsAfterMember ? StackPusherHelper::Preload::IsAddressInEnd : 0)
			);
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
	pusher->store(path.field.type, false, isValueBuilder); // (suf, [pref])... [node-suf] builder
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
	std::deque<int> q;
	q.push_back(0);

	std::vector<int> stackSize(nodes.size(), -1);
	stackSize[0] = pusher->getStack().size();

	while (!q.empty()) {
		int v = q.front();
		q.pop_front();

		int currentStackSize = pusher->getStack().size();
		pusher->blockSwap(1, currentStackSize - stackSize.at(v));
		for (int to = v + 1; to < static_cast<int>(stackSize.size()); ++to) {
			--stackSize[to];
		}
		if (v != 0) {
			pusher->push(0, "CTOS");
		}

		for (const int to : nodes[v].getChildren()) {
			pusher->push(+1, "LDREF");
			stackSize[to] = pusher->getStack().size() - 1;
			q.push_back(to);
		}

		std::size_t iter = 0;
		for (const Field &f : nodes[v].getFields()) {
			if (iter + 1 == nodes[v].getFields().size()) {
				pusher->preload(f.type, !paths.at(f.name).haveDataOrRefsAfterMember ? StackPusherHelper::Preload::IsAddressInEnd : 0); // field // here we delete rest of slice
			} else {
				pusher->load(f.type, false); // field struct
			}
			++iter;
		}
	}
	pusher->tuple(memberNames.size());
}

void StructCompiler::sliceToStateVarsToC7() {
	sliceToStateVarsToC7Dfs(0);
}

int StructCompiler::maxBitLength(StructType const* structType) {
	StructCompiler sc{nullptr, structType};
	return sc.nodes.size() == 1? sc.nodes[0].maxBits() : TvmConst::CellBitLength + 1;
}

bool StructCompiler::doesFitInOneCellAndHaveNoStruct(int keyLength, StructType const *structType) {
	StructCompiler sc{nullptr, structType};
	return sc.doesFitInOneCellAndHaveNoStruct(keyLength);
}

bool StructCompiler::doesFitInOneCellAndHaveNoStruct(int keyLength) const {
	for (Type const *type : memberTypes) {
		if (type->category() == Type::Category::Struct) {
			return false;
		}
	}
	return nodes.size() == 1 &&
		2 + integerLog2(keyLength) + keyLength + nodes[0].maxBits() <= TvmConst::CellBitLength; // from hml_long$10
}


void StructCompiler::dfs(int v, std::vector<int> &nodePath, std::vector<int> &refPath) {
	nodePath.push_back(v);
	std::vector<FieldSizeInfo> skippedField{};
	bool childRefInArray = nodes[v].getChildren().empty();
	for (int i = 0, n = static_cast<int>(nodes[v].getFields().size()); i < n; ++i) {
		const Field &f = nodes[v].getFields()[i];
		if (f.fieldSizeInfo.maxRefLength > 0 && !childRefInArray) {
			std::vector<FieldSizeInfo> newSkippedField{};
			FieldSizeInfo childRef{true, true, 0, static_cast<int>(nodes[v].getChildren().size())};
			newSkippedField.push_back(childRef);
			for (int j = 0; j < i; ++j) {
				const Field &fj = nodes[v].getFields()[j];
				if (!newSkippedField.back().tryMerge(fj.fieldSizeInfo)) {
					newSkippedField.push_back(fj.fieldSizeInfo);
				}
			}
			skippedField = newSkippedField;
			childRefInArray = true;
		}
		paths.emplace(f.name, PathToStructMember{nodePath, refPath, skippedField, f,
														   i + 1 != n || (i + 1 == n && !childRefInArray)});

		if (skippedField.empty() || !skippedField.back().tryMerge(f.fieldSizeInfo)) {
			skippedField.push_back(f.fieldSizeInfo);
		}
	}

	int idRef = 0;
	for (const int to : nodes[v].getChildren()) {
		refPath.push_back(idRef);
		dfs(to, nodePath, refPath);

		refPath.pop_back();
		++idRef;
	}
	nodePath.pop_back();
}

void StructCompiler::createDefaultStructDfs(int v) {
	pusher->push(+1, "NEWC");

	for (const int to : nodes[v].getChildren()) {
		createDefaultStructDfs(to);
		pusher->push(-1, "STBREFR");
	}

	auto onlyBitData = [](const Field& f) {
		return f.fieldSizeInfo.isBitFixed &&
					(f.fieldSizeInfo.maxRefLength == 0 ||
						isUsualArray(f.type) ||
						f.type->category() == Type::Category::Mapping);
	};

	const std::vector<Field> &fields = nodes[v].getFields();
	const int n = nodes[v].getFields().size();
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
			auto structType = to<StructType>(f.type);
			if (structType) {
				StructCompiler structCompiler{pusher, structType};
				structCompiler.createDefaultStructDfs(0);
				pusher->store(f.type, true, StackPusherHelper::StoreFlag::ValueIsBuilder | StackPusherHelper::StoreFlag::StoreStructInRef);
			} else {
				pusher->pushDefaultValue(f.type);
				pusher->store(f.type, true, StackPusherHelper::StoreFlag::StoreStructInRef);
			}
		}
	}
}

void StructCompiler::createStructDfs(const int v, const std::map<std::string, int> &argStackSize) {
	pusher->push(+1, "NEWC");

	for (const int to : nodes[v].getChildren()) {
		createStructDfs(to, argStackSize);
		pusher->push(-1, "STBREFR");
	}

	for (const Field &f : nodes[v].getFields()) {
		const int curStackSize = pusher->getStack().size();
		const int index = curStackSize - 1 - argStackSize.at(f.name);
		pusher->pushS(index);
		pusher->store(f.type, true, StackPusherHelper::StoreFlag::StoreStructInRef);
	}
}

void StructCompiler::stateVarsToBuilderDfs(const int v) {
	int cntValues = 0;
	for (const int to : nodes[v].getChildren()) {
		pusher->push(+1, "NEWC");
		++cntValues;
		stateVarsToBuilderDfs(to);
	}
	for (const Field &f : nodes[v].getFields()) {
		pusher->getGlob(TvmConst::C7::FirstIndexForVariables +  + f.memberIndex);
		if (isUsualArray(f.type)) {
			pusher->untuple(2); // size dict
			cntValues += 2;
		} else {
			cntValues++;
		}
	}

	if (cntValues + 1 >= 2) {
		pusher->reverse(cntValues + 1, 0);
	}
	for (const int to : nodes[v].getChildren()) {
		(void) to;
		pusher->push(-1, "STBREF");
	}
	for (const Field &f : nodes[v].getFields()) {
		pusher->store(f.type, false, StackPusherHelper::StoreFlag::ArrayIsUntupled | StackPusherHelper::StoreFlag::StoreStructInRef);
	}
}

void StructCompiler::sliceToStateVarsToC7Dfs(const int v) {
	// slice
	for (const int to : nodes[v].getChildren()) {
		pusher->push(+1, "LDREFRTOS"); // slice child[i]
		sliceToStateVarsToC7Dfs(to);
	}

	if (nodes[v].getFields().empty()) {
		pusher->push(-1, "ENDS");
		return;
	}

	// slice
	std::vector<Field> fields;
	for (std::size_t i = 0; i < nodes[v].getFields().size(); ++i) {
		const Field &f = nodes[v].getFields()[i];
		if (i + 1 == nodes[v].getFields().size()) {
			pusher->preload(f.type, !paths.at(f.name).haveDataOrRefsAfterMember ? StackPusherHelper::Preload::IsAddressInEnd : 0); // field
			pusher->setGlob(TvmConst::C7::FirstIndexForVariables + f.memberIndex);
		} else {
			bool directOrder = pusher->fastLoad(f.type);
			if (directOrder) { // value slice
				fields.push_back(f);
			} else { // slice value
				pusher->setGlob(TvmConst::C7::FirstIndexForVariables +  + f.memberIndex);
			}
		}
	}
	while (!fields.empty()) {
		pusher->setGlob(TvmConst::C7::FirstIndexForVariables +  + fields.back().memberIndex);
		fields.pop_back();
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
				solUnimplemented("Unsupported skip");
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
				solUnimplemented("Unsupported split");
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
