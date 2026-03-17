/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0
/**
 * @author Christian <c@ethdev.com>
 * @author Gav Wood <g@ethdev.com>
 * @date 2014
 * Container of the (implicit and explicit) global objects.
 */

#include <libsolidity/analysis/GlobalContext.h>

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>
#include <libsolidity/ast/Types.h>
#include <memory>
#include <unordered_map>

namespace solidity::frontend
{

namespace
{

/// Magic variables get negative ids for easy differentiation
int magicVariableToID(std::string const& _name)
{
	static std::unordered_map<std::string, int> const magicVariables = {
		{"abi", -1},
		{"addmod", -2},
		{"assert", -3},
		{"block", -4},
		{"blockhash", -5},
		{"gasleft", -7},
		{"keccak256", -8},
		{"msg", -15},
		{"mulmod", -16},
		{"now", -17},
		{"require", -18},
		{"revert", -19},
		{"ripemd160", -20},
		{"selfdestruct", -21},
		{"sha256", -22},
		{"sha3", -23},
		{"suicide", -24},
		{"super", -25},
		{"tx", -26},
		{"type", -27},
		{"this", -28},
		{"blobhash", -29},
		{"bitSize", -62},
		{"uBitSize", -63},
		{"tvm", -101},
		{"logtvm", -102},
		{"math", -103},
		{"format", -104},
		{"rnd", -105},
		{"stoi", -106},
		{"bls", -108},
		{"gasConsumed", -109},
		{"sha256", -110},
		{"sha512", -111},
		{"blake2b", -112},
		{"keccak256", -113},
		{"keccak512", -114},
		{"rist255", -115},
		{"config", -116},
		{"secp256k1", -117},
		{"secp256r1", -118},
	};

	if (auto id = magicVariables.find(_name); id != magicVariables.end())
		return id->second;
	solAssert(false, "Unknown magic variable: \"" + _name + "\".");
}

inline std::vector<std::shared_ptr<MagicVariableDeclaration const>> constructMagicVariables()
{
	static auto const magicVarDecl = [](std::string const& _name, Type const* _type) {
		return std::make_shared<MagicVariableDeclaration>(magicVariableToID(_name), _name, _type);
	};

	std::vector<std::shared_ptr<MagicVariableDeclaration const>> magicVariableDeclarations = {
		magicVarDecl("abi", TypeProvider::magic(MagicType::Kind::ABI)),
		magicVarDecl("addmod", TypeProvider::function(strings{"uint256", "uint256", "uint256"}, strings{"uint256"}, FunctionType::Kind::AddMod, StateMutability::Pure)),
		magicVarDecl("assert", TypeProvider::function(strings{"bool"}, strings{}, FunctionType::Kind::Assert, StateMutability::Pure)),
		magicVarDecl("block", TypeProvider::magic(MagicType::Kind::Block)),
		magicVarDecl("gasleft", TypeProvider::function(strings(), {"uint64"}, FunctionType::Kind::GasLeft, StateMutability::Pure)),
		magicVarDecl("msg", TypeProvider::magic(MagicType::Kind::Message)),
		magicVarDecl("mulmod", TypeProvider::function(strings{"uint256", "uint256", "uint256"}, strings{"uint256"}, FunctionType::Kind::MulMod, StateMutability::Pure)),
		magicVarDecl("now", TypeProvider::uint(32)),
		magicVarDecl("require", TypeProvider::function(strings{"bool"}, strings{}, FunctionType::Kind::Require, StateMutability::Pure)),
		magicVarDecl("require", TypeProvider::function(strings{"bool", "uint16"}, strings{}, FunctionType::Kind::Require, StateMutability::Pure)),
		magicVarDecl("require", TypeProvider::function(strings{"bool", "uint16", "uint32"}, strings{}, FunctionType::Kind::Require, StateMutability::Pure)),
		magicVarDecl("revert", TypeProvider::function(strings{}, strings{}, FunctionType::Kind::Revert, StateMutability::Pure)),
		magicVarDecl("revert", TypeProvider::function(strings{"uint16"}, strings{}, FunctionType::Kind::Revert, StateMutability::Pure)),
		magicVarDecl("revert", TypeProvider::function(strings{"uint16", "uint32"}, strings{}, FunctionType::Kind::Revert, StateMutability::Pure)),
		magicVarDecl("selfdestruct", TypeProvider::function(strings{"address payable"}, strings{}, FunctionType::Kind::Selfdestruct)),
		magicVarDecl("format", TypeProvider::function(strings{}, strings{"string"}, FunctionType::Kind::Format, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("logtvm", TypeProvider::function(strings{"string"}, strings{}, FunctionType::Kind::LogTVM, StateMutability::Pure)),
		magicVarDecl("math", TypeProvider::magic(MagicType::Kind::Math)),
		magicVarDecl("rnd", TypeProvider::magic(MagicType::Kind::Rnd)),
		magicVarDecl("config", TypeProvider::magic(MagicType::Kind::Config)),
		magicVarDecl("secp256k1", TypeProvider::magic(MagicType::Kind::Secp256k1)),
		magicVarDecl("secp256r1", TypeProvider::magic(MagicType::Kind::Secp256r1)),
		magicVarDecl("stoi", TypeProvider::function(
			TypePointers{TypeProvider::stringStorage()},
			TypePointers{TypeProvider::optional(TypeProvider::integer(256, IntegerType::Modifier::Signed))},
			strings{std::string()},
			strings{std::string()},
			FunctionType::Kind::Stoi,
			StateMutability::Pure
		)),
		magicVarDecl("bls", TypeProvider::magic(MagicType::Kind::BLS)),
		magicVarDecl("rist255", TypeProvider::magic(MagicType::Kind::RIST255)),
		magicVarDecl("tvm", TypeProvider::magic(MagicType::Kind::TVM)),
		magicVarDecl("tx", TypeProvider::magic(MagicType::Kind::Transaction)),
		// Accepts a MagicType that can be any contract type or an Integer type and returns a
		// MagicType. The TypeChecker handles the correctness of the input and output types.
		magicVarDecl("type", TypeProvider::function(
			strings{},
			strings{},
			FunctionType::Kind::MetaType,
			StateMutability::Pure,
			FunctionType::Options::withArbitraryParameters()
		)),
		magicVarDecl("bitSize", TypeProvider::function({"int"}, {"uint16"}, FunctionType::Kind::BitSize, StateMutability::Pure)),
		magicVarDecl("uBitSize", TypeProvider::function({"uint"}, {"uint16"}, FunctionType::Kind::UBitSize, StateMutability::Pure)),
		magicVarDecl("gasConsumed", TypeProvider::function(strings(), {"uint59"}, FunctionType::Kind::GasConsumed, StateMutability::Pure)),

		magicVarDecl("sha256", TypeProvider::function({}, {}, FunctionType::Kind::SHA256, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("sha512", TypeProvider::function({}, {}, FunctionType::Kind::HashExt, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("blake2b", TypeProvider::function({}, {}, FunctionType::Kind::HashExt, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("keccak256", TypeProvider::function({}, {}, FunctionType::Kind::HashExt, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("keccak512", TypeProvider::function({}, {}, FunctionType::Kind::HashExt, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
	};

	return magicVariableDeclarations;
}

}

GlobalContext::GlobalContext():
	m_magicVariables{constructMagicVariables()}
{
}

void GlobalContext::setCurrentContract(ContractDefinition const& _contract)
{
	m_currentContract = &_contract;
}

std::vector<Declaration const*> GlobalContext::declarations() const
{
	std::vector<Declaration const*> declarations;
	declarations.reserve(m_magicVariables.size());
	for (ASTPointer<MagicVariableDeclaration const> const& variable: m_magicVariables)
		declarations.push_back(variable.get());
	return declarations;
}

MagicVariableDeclaration const* GlobalContext::currentThis() const
{
	if (!m_thisPointer[m_currentContract])
	{
		Type const* type = TypeProvider::emptyTuple();
		if (m_currentContract)
			type = TypeProvider::contract(*m_currentContract);
		m_thisPointer[m_currentContract] =
			std::make_shared<MagicVariableDeclaration>(magicVariableToID("this"), "this", type);
	}
	return m_thisPointer[m_currentContract].get();
}

MagicVariableDeclaration const* GlobalContext::currentSuper() const
{
	if (!m_superPointer[m_currentContract])
	{
		Type const* type = TypeProvider::emptyTuple();
		if (m_currentContract)
			type = TypeProvider::typeType(TypeProvider::contract(*m_currentContract, true));
		m_superPointer[m_currentContract] =
			std::make_shared<MagicVariableDeclaration>(magicVariableToID("super"), "super", type);
	}
	return m_superPointer[m_currentContract].get();
}

}
