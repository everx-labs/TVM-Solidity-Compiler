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

#include "TVMPusher.hpp"

namespace solidity::frontend {

class TVMABI {
public:
	static void generateABI(ContractDefinition const* contract, const vector<ContractDefinition const*>& m_allContracts,
									std::vector<PragmaDirective const *> const& pragmaDirectives, std::ostream* out = &cout);
private:
	static void printData(const Json::Value& json, std::ostream* out);
	static void print(const Json::Value& json, std::ostream* out);
	static Json::Value processFunction(
		const string& fname,
		const ast_vec<VariableDeclaration>& params,
		const ast_vec<VariableDeclaration>& retParams,
		FunctionDefinition const* funcDef = nullptr
	);
	static Json::Value encodeParams(const ast_vec<VariableDeclaration>& params);
	static string getParamTypeString(Type const* type, ASTNode const& node);
	static Json::Value setupType(const string& name, const Type* type, ASTNode const& node);
	static Json::Value setupStructComponents(const StructType* type, ASTNode const& node);
};

class DecodePosition : private boost::noncopyable {
public:
	enum Algo {JustLoad, NeedLoadNextCell, CheckBits, CheckRefs, Unknown};
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
	int maxBits();
	int minBits();
public:
	void decodeParameters(const ast_vec<VariableDeclaration>& params);
private:
	void loadNextSlice();
	void checkBitsAndLoadNextSlice();
	void checkRefsAndLoadNextSlice();
	void loadNextSliceIfNeed(const DecodePosition::Algo algo, VariableDeclaration const* variable, bool isRefType);
	void loadq(const DecodePosition::Algo algo, const std::string& opcodeq, const std::string& opcode);
	void decodeParameter(VariableDeclaration const* variable, DecodePosition* position);
private:
	StackPusherHelper *pusher{};
};

}	// solidity::frontend
