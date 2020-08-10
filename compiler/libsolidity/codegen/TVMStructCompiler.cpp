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
		case Type::Category::ExtraCurrencyCollection:
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
		case Type::Category::Optional:
			isBitFixed = true;
			isRefFixed = false;
			maxBitLength = 1;
			maxRefLength = 1;
			break;
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
		nodes{nodes}, childRef{childRef}, skippedFields{std::move(skippedFields)}, field{field},
		haveDataOrRefsAfterMember{haveDataOrRefsAfterMember} {
	solAssert(nodes.size() == childRef.size() + 1, "");
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, StructType const *structType) :
		StructCompiler{pusher, fVariableDeclarations(&structType->structDefinition()), 0, false} {
}

StructCompiler::StructCompiler(StackPusherHelper *pusher, std::vector<VariableDeclaration const *> _variableDeclarations,
		const int skipData, bool isC4)
		:
		variableDeclarations{std::move(_variableDeclarations)},
		pusher{pusher}
	{

	int memberIndex = 0;
	int parent = 0;
	nodes.emplace_back(Node(skipData));
	for (const auto &member : variableDeclarations) {
		const Field f{memberIndex, member};
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

		nameToVariableDeclarations[member->name()] = member;
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

void StructCompiler::structConstructor(std::vector<ASTPointer<ASTString>> const& names,
										const std::function<void(int)>& pushParam) {
	if (names.empty()) {
		int i{};
		for (const VariableDeclaration *vd : variableDeclarations) {
			if (vd->type()->category() == Type::Category::Mapping) {
				pusher->pushDefaultValue(vd->type(), false);
			} else {
				pushParam(i++);
			}
		}
	} else {
		for (const VariableDeclaration *vd : variableDeclarations) {
			auto it = std::find_if(names.begin(), names.end(), [vd](const ASTPointer<ASTString>& name){
				return vd->name() == *name;
			});
			if (vd->type()->category() == Type::Category::Mapping) {
				solAssert(it == names.end(), "");
				pusher->pushDefaultValue(vd->type());
			} else {
				int index = it - names.begin();
				pushParam(index);
			}
		}
	}

	pusher->tuple(variableDeclarations.size());
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
				preload(f.member, false); // field // here we delete rest of slice
			} else {
				load(f.member, false); // field struct
			}
			++iter;
		}
	}
	pusher->tuple(variableDeclarations.size());
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
		2 + static_cast<int>(std::ceil(log2(keyLength + 1)+ 1e-5)) + keyLength + nodes[0].maxBits() <= TvmConst::CellBitLength; // 2 is gotten from hml_long$10
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
		paths.emplace(f.member->name(), PathToStructMember{nodePath, refPath, skippedField, f,
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
						isUsualArray(f.member->type()) ||
						f.member->type()->category() == Type::Category::Mapping);
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

	for (const int to : nodes[v].getChildren()) {
		createStructDfs(to, argStackSize);
		pusher->push(-1, "STBREFR");
	}

	for (const Field &f : nodes[v].getFields()) {
		const int curStackSize = pusher->getStack().size();
		const int index = curStackSize - 1 - argStackSize.at(f.member->name());
		pusher->pushS(index);
		store(f.member, true);
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
	for (const int to : nodes[v].getChildren()) {
		(void) to;
		pusher->push(-1, "STBREF");
	}
	for (const Field &f : nodes[v].getFields()) {
		store(f.member, false, false, true);
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

bool StructCompiler::fastLoad(const VariableDeclaration *vd, const Type * childType) {
	// slice
	Type const *type = (childType == nullptr) ? vd->type() : childType;
	switch (type->category()) {
		case Type::Category::Optional: {
            const int saveStakeSize = pusher->getStack().size();
			auto opt = to<OptionalType>(vd->type());

			pusher->push(+1, "LDOPTREF"); // value slice
			pusher->exchange(0, 1); // slice value
			pusher->pushS(0); // slice value value
			pusher->push(-1 + 1, "ISNULL"); // slice value isNull

			pusher->push(-1, ""); // fix stack

			pusher->startContinuation();
			// slice value
			pusher->push(0, "CTOS"); // slice sliceValue
			preload(vd, false, opt->valueType(), true); // slice value
			pusher->endContinuation();

			pusher->push(0, "IFNOT");

            solAssert(saveStakeSize + 1 == pusher->getStack().size(), "");
			return false;
		}
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

void StructCompiler::preload(const VariableDeclaration *vd, bool returnStructAsSlice, const Type* type,
                             bool useCurrentSlice) {
	const int stackSize = pusher->getStack().size();
	// on stack there is slice
	if (type == nullptr) {
		type = vd->type();
	}
	switch (type->category()) {
		case Type::Category::Optional: {
			auto opt = to<OptionalType>(type);

			pusher->pushS(0);
			pusher->push(-1 + 1, "PLDI 1"); // slice hasVal

			pusher->push(-1, ""); // fix stack

			// have value
			int savedStake0 = pusher->getStack().size();
			pusher->startContinuation();
			// stack: slice
			pusher->push(-1 + 1, "PLDREF");
			pusher->push(-1 + 1, "CTOS");
			preload(vd, returnStructAsSlice, opt->valueType(), true);
			pusher->endContinuation();
			pusher->getStack().ensureSize(savedStake0);

			// no value
			int savedStake1 = pusher->getStack().size();
			pusher->startContinuation();
			// stack: slice
			pusher->drop();
			pusher->push(+1, "NULL");
			pusher->endContinuation();
			pusher->getStack().ensureSize(savedStake1);

			pusher->push(0, "IFELSE");

			break;
		}
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
		    if (!useCurrentSlice){
                pusher->push(-1 + 2, "LDREFRTOS");
                pusher->push(-1, "NIP");
            }
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
		case Type::Category::ExtraCurrencyCollection:
			pusher->push(-1 + 1, "PLDDICT");
			break;
		default:
			cast_error(*vd, "Unsupported type in struct");
	}
	pusher->getStack().ensureSize(stackSize);
}

void StructCompiler::store(const VariableDeclaration *vd, bool reverse, bool isValueBuilder,
                           bool isArrayUntupled, const Type *childType, bool storeAsRefForStruct) {
	// value   builder  -> reverse = false
	// builder value    -> reverse = true
	const int stackSize = pusher->getStack().size();
	int deltaStack = 1;
	Type const *type = (childType == nullptr) ? vd->type() : childType;
	switch (type->category()) {
		case Type::Category::Optional: {
			auto optType = to<OptionalType>(type);

			if (!reverse)
				pusher->exchange(0, 1);	// builder value
			pusher->pushS(0);	// builder value value
			pusher->push(-1 + 1, "ISNULL");	// builder value isnull
			pusher->push(-1 + 1, "NOT");	// builder value !isnull

			pusher->push(-1, ""); // fix stack

			pusher->getStack().ensureSize(stackSize);
			pusher->startContinuation();
			// builder value
			pusher->push(+1, "NEWC"); // builder value builder
			store(vd, false, false, false, optType->valueType(), false); // builder builderWithValue
			pusher->exchange(0, 1); // builderWithValue builder
			pusher->stones(1);
			pusher->push(-1, "STBREF");	// builder
			pusher->endContinuation();
			pusher->push(+1, ""); // fix stack

			pusher->getStack().ensureSize(stackSize);
			pusher->startContinuation();
			// builder value
			pusher->drop(1); // builder
			pusher->stzeroes(1);
			pusher->endContinuation();
			pusher->push(+1, ""); // fix stack

			pusher->push(0, "IFELSE");
			pusher->push(-1, ""); // fix stack

			break;
		}
		case Type::Category::TvmCell:
			pusher->push(-1, reverse? "STREFR" : "STREF"); // builder
			break;
		case Type::Category::Struct: {
			if (isValueBuilder) {
			    if (storeAsRefForStruct)
				    pusher->push(-1, reverse? "STBREFR" : "STBREF"); // builder
                else
                    solAssert(false, "TODO");
			} else {
				if (!reverse) {
					pusher->push(0, "SWAP"); // builder struct
				}
				// builder struct
				auto st = to<StructType>(type);
				StructCompiler sc{pusher, st};
				sc.tupleToBuilder();
				if (storeAsRefForStruct)
				    pusher->push(-1, "STBREFR"); // builder
                else {
                    pusher->push(-1, "STBR"); // builder
                    // TODO opt this: don't create builder in `tupleToBuilder` and don't append this.
                }
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
		case Type::Category::ExtraCurrencyCollection:
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
						deltaStack = 2;
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
			cast_error(*vd, "Unsupported type in struct");
	}

	pusher->getStack().ensureSize(stackSize - deltaStack);
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

