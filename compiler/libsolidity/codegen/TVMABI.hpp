/*
 * Copyright 2018-2022 TON DEV SOLUTIONS LTD.
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

class StackPusher;

class TVMABI {
public:
	static void printFunctionIds(
		ContractDefinition const& contract,
		PragmaDirectiveHelper const& pragmaHelper
	 );
	static void generateABI(ContractDefinition const* contract,
							std::vector<PragmaDirective const *> const& pragmaDirectives, std::ostream* out = &std::cout);
private:
	static std::vector<const FunctionDefinition *> publicFunctions(ContractDefinition const& contract);
	static void printData(const Json::Value& json, std::ostream* out);
	static void print(const Json::Value& json, std::ostream* out);
	static Json::Value toJson(
		const std::string& fname,
		const std::vector<VariableDeclaration const*> &params,
		const std::vector<VariableDeclaration const*> &retParams,
		FunctionDefinition const* funcDef = nullptr
	);
	static Json::Value encodeParams(const std::vector<VariableDeclaration const*> &params);
public:
	static Json::Value setupNameTypeComponents(const std::string& name, const Type* type);
private:
	static Json::Value setupStructComponents(const StructType* type);
	static Json::Value setupTupleComponents(const TupleType* type);
};

class DecodePosition : private boost::noncopyable {
public:
	virtual ~DecodePosition() = default;
	virtual bool loadNextCell(Type const* type) = 0;
};

class DecodePositionAbiV2 : public DecodePosition {
public:
	DecodePositionAbiV2(int _bitOffset, int _refOffset, const std::vector<Type const *>& _types);
	bool loadNextCell(Type const* type) override;
	int countOfCreatedBuilders() const;
private:
	void initTypes(Type const* type);

private:
	int m_curTypeIndex{};
	std::vector<Type const*> m_types;
	std::vector<bool> m_doLoadNextCell;
	int m_countOfCreatedBuilders{};
};

class DecodePositionFromOneSlice : public DecodePosition {
public:
	bool loadNextCell(Type const* /*type*/) override {
		return false;
	}
};

class ChainDataDecoder : private boost::noncopyable {
public:
	explicit ChainDataDecoder(StackPusher *pusher);
private:
	int maxBits(bool hasCallback);
	static int minBits(bool hasCallback);
public:
	void decodePublicFunctionParameters(const std::vector<Type const*>& types, bool isResponsible, bool isInternal);
	void decodeFunctionParameters(const std::vector<Type const*>& types, bool isResponsible);
	void decodeData(int offset, int usedRefs, const std::vector<Type const*>& types);
	void decodeParameters(
		const std::vector<Type const*>& types,
		DecodePosition& position,
		const bool doDropSlice
	);
private:
	void loadNextSlice();
	void loadNextSliceIfNeed(bool doLoadNextSlice);
	void decodeParameter(Type const* type, DecodePosition* position);
private:
	StackPusher *pusher{};
};




enum class ReasonOfOutboundMessage {
	EmitEventExternal,
	FunctionReturnExternal,
	RemoteCallInternal
};

class ChainDataEncoder : private boost::noncopyable {
public:
	explicit ChainDataEncoder(StackPusher *pusher) : pusher{pusher} {}
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
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		int bitSizeBuilder,
		bool reversedArgs = false
	);

	void createMsgBody(
		const std::vector<VariableDeclaration const*> &params,
		const std::variant<uint32_t, std::function<void()>>& functionId,
		const std::optional<uint32_t>& callbackFunctionId,
		DecodePositionAbiV2 &position
	);

	void encodeParameters(
		const std::vector<Type const*>& types,
		DecodePositionAbiV2& position
	);

private:
	std::string toStringForCalcFuncID(Type const * type);

private:
	StackPusher *pusher{};
};

}	// solidity::frontend
