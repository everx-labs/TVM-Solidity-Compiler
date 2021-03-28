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
 * ABI generator and parser for TON
 */

#pragma once

#include <variant>

#include "TVMCommons.hpp"

namespace solidity::frontend {

class StackPusherHelper;

class TVMABI {
public:
	static void generateABI(ContractDefinition const* contract,
							std::vector<PragmaDirective const *> const& pragmaDirectives, std::ostream* out = &cout);
	static string getParamTypeString(Type const* type);
private:
	static void printData(const Json::Value& json, std::ostream* out);
	static void print(const Json::Value& json, std::ostream* out);
	static Json::Value processFunction(
		const string& fname,
		const std::vector<VariableDeclaration const*> &params,
		const std::vector<VariableDeclaration const*> &retParams,
		FunctionDefinition const* funcDef = nullptr
	);
	static Json::Value encodeParams(const std::vector<VariableDeclaration const*> &params);
	static Json::Value setupType(const string& name, const Type* type);
	static Json::Value setupComponents(Json::Value json, const Type* type);
	static Json::Value setupStructComponents(const StructType* type);
};

class DecodePosition : private boost::noncopyable {
public:
	enum Algo {JustLoad, LoadNextCell, CheckBits, CheckRefs, CheckBitsAndRefs, Unknown};
	virtual ~DecodePosition() = default;
	virtual Algo updateStateAndGetLoadAlgo(Type const* type) = 0;
};

class DecodePositionAbiV1 : public DecodePosition {
	bool isPositionValid;
	int minRestSliceBits;
	int maxRestSliceBits;
	int minUsedRef;
	int maxUsedRef;
public:
	DecodePositionAbiV1();
	Algo updateStateAndGetLoadAlgo(Type const* type) override;
};

class Position {
public:
	Position(int usedBits, int usedRefs);
	void update(const int bits, const int refs);
	void loadRef();
	int cellNumber() const;

private:
	int idCell{};
	int usedBits{};
	int usedRefs{};
};

class DecodePositionAbiV2 : public DecodePosition {
	Position minPos;
	Position maxPos;
	std::vector<Type const*> types;
	int curTypeIndex = -1;
	int lastRefType = -1;
public:
	explicit DecodePositionAbiV2(int minBits, int maxBits,
	                             const ast_vec<VariableDeclaration>& params);
	void initTypes(VariableDeclaration const* variable);
	Algo updateStateAndGetLoadAlgo(Type const* type) override;
};

class DecodeFunctionParams : private boost::noncopyable {
public:
	explicit DecodeFunctionParams(StackPusherHelper *pusher);
private:
	int maxBits(bool hasCallback);
	static int minBits(bool hasCallback);
public:
	void decodeParameters(const ast_vec<VariableDeclaration>& params, bool hasCallback);
private:
	void loadNextSlice();
	void checkBitsAndLoadNextSlice();
	void checkRefsAndLoadNextSlice();
	void checkBitsAndRefsAndLoadNextSlice();
	void loadNextSliceIfNeed(const DecodePosition::Algo algo, VariableDeclaration const* variable, bool isRefType);
	void loadq(const DecodePosition::Algo algo, const std::string& opcodeq, const std::string& opcode);
	void decodeParameter(VariableDeclaration const* variable, DecodePosition* position);
private:
	StackPusherHelper *pusher{};
};




enum class ReasonOfOutboundMessage {
	EmitEventExternal,
	FunctionReturnExternal,
	RemoteCallInternal
};

class EncodePosition : private boost::noncopyable {
	int restSliceBits{};
	int restFef{};
	int qtyOfCreatedBuilders{};
	std::vector<Type const *> types;
	int currentIndex{};
	std::vector<bool> isNeedNewCell;
	int lastRefType{};

public:
	explicit EncodePosition(int bits, const std::vector<Type const *> &types);
	bool needNewCell(Type const* type);
private:
	bool updateState(int i);
	void init(Type const* t);
public:
	int countOfCreatedBuilders() const;
};

class EncodeFunctionParams : private boost::noncopyable {
public:
	explicit EncodeFunctionParams(StackPusherHelper *pusher) : pusher{pusher} {}
	void createDefaultConstructorMsgBodyAndAppendToBuilder(int bitSizeBuilder);
	void createDefaultConstructorMessage2();

	// returns pair (functionID, is_manually_overridden)
	uint32_t calculateConstructorFunctionID();
	std::pair<uint32_t, bool> calculateFunctionID(const CallableDeclaration *declaration);
	uint32_t calculateFunctionID(
		const std::string& name,
		const std::vector<Type const*>& inputs,
		const std::vector<VariableDeclaration const*> *outputs
	);
	uint32_t calculateFunctionIDWithReason(const CallableDeclaration *funcDef, const ReasonOfOutboundMessage &reason);
	uint32_t calculateFunctionIDWithReason(
		const std::string& name,
		std::vector<Type const*> inputs,
		const std::vector<VariableDeclaration const*> *outputs,
		const ReasonOfOutboundMessage &reason,
		std::optional<uint32_t> functionId,
		bool isResponsible
	);

	void createMsgBodyAndAppendToBuilder(
		const std::function<void(size_t)>& pushParam,
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		const int bitSizeBuilder
	);

	void createMsgBody(
		const std::function<void(size_t)>& pushParam,
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		EncodePosition &position
	);

	void encodeParameters(const std::vector<Type const*>& types,
	                      const std::vector<ASTNode const*>& nodes,
	                      const std::function<void(size_t)>& pushParam,
	                      EncodePosition& position);

private:
	std::string getTypeString(Type const * type);
	void encodeParameter(Type const* type, EncodePosition& position, const std::function<void()>& pushParam, ASTNode const* node);
	void encodeStruct(const StructType* structType, ASTNode const* node, EncodePosition& position);

private:
	StackPusherHelper *pusher{};
};

}	// solidity::frontend
