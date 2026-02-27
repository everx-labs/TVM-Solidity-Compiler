/*
 * Copyright (C) 2019-2025 EverX. All Rights Reserved.
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
 * ABI generator and parser
 */

#pragma once

#include <libsolidity/codegen/TVMCommons.hpp>
#include <libsolutil/JSON.h>
#include <variant>

namespace solidity::frontend {

class StackPusher;

class TVMABI {
public:
	constexpr static std::size_t INDENT_SPACES = 4;

	static Json generateFunctionIdsJson(ContractDefinition const& contract, PragmaDirectiveHelper const& pragmaHelper);
	static Json generatePrivateFunctionIdsJson(
		ContractDefinition const& contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		PragmaDirectiveHelper const& pragmaHelper,
		bool debugMode
	);
	static void generateABI(
		ContractDefinition const* contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		std::vector<PragmaDirective const*> const& pragmaDirectives,
		std::ostream& out
	);
	static Json generateABIJson(
		ContractDefinition const* contract,
		std::vector<ASTPointer<SourceUnit>> const& _sourceUnits,
		std::vector<PragmaDirective const*> const& pragmaDirectives
	);

private:
	static std::vector<FunctionDefinition const*> publicFunctions(ContractDefinition const& contract);
	static std::vector<FunctionDefinition const*> getters(ContractDefinition const& contract);
	static void
	printVariable(Json const& json, std::ostream& out, std::string const& indentation, bool hasIndentInFirstLine);
	static void printVariables(Json const& json, std::ostream& out, std::string const& indentation);
	static void print(Json const& json, std::ostream& out);
	static Json toJson(
		std::string const& functionName,
		std::vector<VariableDeclaration const*> const& params,
		std::vector<VariableDeclaration const*> const& retParams,
		FunctionDefinition const* funcDef = nullptr
	);
	static Json encodeParams(std::vector<VariableDeclaration const*> const& params);

public:
	static Json setupNameTypeComponents(std::string const& name, Type const* type);

private:
	static Json setupStructComponents(StructType const* type);
	static Json setupTupleComponents(TupleType const* type);
};

class AbiPosition: boost::noncopyable {
public:
	virtual ~AbiPosition() = default;
	virtual bool skipType(Type const* type) = 0;
	static void unroll(std::vector<Type const*>& types, Type const* type);
};

class AbiV2Position: public AbiPosition {
public:
	AbiV2Position(int _bitOffset, int _refOffset, std::vector<Type const*> const& _types);
	bool skipType(Type const* type) override;
	void skipTypes(std::vector<Type const*> const& _types);
	bool getDoLoadNextCell(int index) const { return m_doLoadNextCell.at(index); }
	Type const* getType(int index) const { return m_types.at(index); }
	int size() const { return m_doLoadNextCell.size(); }
	int currentIndex() const { return m_curTypeIndex; }
	int rootBits() const { return m_rootBits; }
	int rootRefs() const { return m_rootRefs; }

private:
	int m_curTypeIndex{};
	std::vector<Type const*> m_types;
	// Do we load next cell before decoding current type?
	std::vector<bool> m_doLoadNextCell;
	int m_rootBits = 0;
	int m_rootRefs = 0;
};

class AbiPositionFromOneSlice: public AbiPosition {
public:
	bool skipType(Type const* /*type*/) override { return false; }
};

class ChainDataDecoder: boost::noncopyable {
public:
	explicit ChainDataDecoder(StackPusher* pusher);

private:
	int offsetExternalFunction(bool isResponsible) const;
	static int offsetInternalFunction(bool isResponsible);

public:
	void decodePublicFunctionParameters(
		std::vector<Type const*> const& types,
		bool isResponsible,
		bool isInternal
	) const;
	void decodeFunctionParameters(std::vector<Type const*> const& types, bool isResponsible, bool isExternalMsg) const;
	void decodeData(int offset, int usedRefs, std::vector<Type const*> const& types, bool withENDS) const;
	void decodeParameters(std::vector<Type const*> const& types, AbiPosition& position) const;
	void decodeParametersQ(std::vector<Type const*> const& types, AbiPosition& position) const;

private:
	void loadNextSlice() const;

public:
	void decodeParameter(
		Type const* type,
		AbiPosition* position,
		bool isFirstCall = true,
		bool loadForFirstCallIfNeeded = true
	) const;

private:
	void decodeParameterQ(Type const* type, AbiPosition* position, int ind) const;

	StackPusher* pusher{};
};


enum class ReasonOfOutboundMessage {
	EmitEventExternal,
	FunctionReturnExternal,
	RemoteCallInternal
};

class ChainDataEncoder: boost::noncopyable {
public:
	explicit ChainDataEncoder(StackPusher* pusher):
		pusher{pusher} {}

	// returns pair (functionID, is_manually_overridden)
	static uint32_t calculateConstructorFunctionID();
	static std::pair<uint32_t, bool> calculateFunctionID(CallableDeclaration const* declaration);
	static uint32_t toHash256(std::string const& str);
	static uint32_t toPrivateFunctionId(std::string const& str);
	static uint32_t calculateFunctionID(
		std::string const& name,
		std::vector<Type const*> const& inputs,
		std::vector<VariableDeclaration const*> const* outputs
	);
	static uint32_t calculateFunctionIDWithReason(
		CallableDeclaration const* funcDef,
		ReasonOfOutboundMessage const& reason,
		bool isLib = false
	);
	static uint32_t calculateFunctionIDWithReason(
		std::string const& name,
		std::vector<Type const*> inputs,
		std::vector<VariableDeclaration const*> const* outputs,
		ReasonOfOutboundMessage const& reason,
		std::optional<uint32_t> functionId,
		bool isResponsible
	);

	void createMsgBodyAndAppendToBuilder(
		std::vector<VariableDeclaration const*> const& params,
		std::variant<uint32_t, std::function<void()>> const& functionId,
		std::optional<uint32_t> const& callbackFunctionId,
		int bitSizeBuilder,
		int refSizeBuilder,
		bool reversedArgs = false
	) const;

	void createMsgBody(
		std::vector<VariableDeclaration const*> const& params,
		std::variant<uint32_t, std::function<void()>> const& functionId,
		std::optional<uint32_t> const& callbackFunctionId,
		AbiV2Position& position,
		bool const reversedArgs
	) const;

	void encodeParameters(
		std::vector<Type const*> const& _types,
		AbiV2Position& position,
		bool hasUnpackedStateVars
	) const;

private:
	static std::string toStringForCalcFuncID(Type const* type);

	StackPusher* pusher{};
};

class UnpackedCoderDecoder: boost::noncopyable {
public:
	explicit UnpackedCoderDecoder(
		StackPusher& pusher,
		int _offset,
		int _usedRefs,
		int _varOffset,
		std::vector<Type const*> const& _varTypes,
		std::vector<bool> const& _varNeeded
	);
	void unpackedData() const;

	void packData(std::map<int, std::function<void()>> const& varIndexToPush);

private:
	struct TypeSize {
		int bits;
		int refs;
	};
	std::optional<TypeSize> isFixedSize(int i, int j) const;
	std::optional<int> getFixedRef(int i, int j) const;
	std::unique_ptr<AbiV2Position> createPosition() const;
	void skipTypes(int beginIndex, int endIndex, std::unique_ptr<AbiV2Position> const& position) const;
	void skipTypesAndLoadCellIfNeeded(int index, std::unique_ptr<AbiV2Position> const& position) const;


	StackPusher& pusher;
	int const offset;
	int const usedRefs;
	int const varOffset;
	std::vector<Type const*> const& varTypes;
	std::vector<bool> const& varNeeded;

	static constexpr int INF = 1e9;
	int neededVars = 0;
	std::vector<Type const*> types;
	std::vector<int> varIndex;
	std::vector<int> to;
	std::vector<bool> isNeededType;

	int startIndexType = -1;
	int lastIndexType = -1;
};

} // solidity::frontend
