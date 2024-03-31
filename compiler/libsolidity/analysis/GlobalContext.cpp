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

namespace solidity::frontend
{

namespace
{
/// Magic variables get negative ids for easy differentiation
int magicVariableToID(std::string const& _name)
{
	if (_name == "abi") return -1;
	else if (_name == "addmod") return -2;
	else if (_name == "assert") return -3;
	else if (_name == "block") return -4;
	else if (_name == "blockhash") return -5;
	else if (_name == "ecrecover") return -6;
	else if (_name == "gasleft") return -7;
	else if (_name == "format") return -105;
	else if (_name == "keccak256") return -8;
	else if (_name == "logtvm") return -102;
	else if (_name == "math") return -103;
	else if (_name == "rnd") return -105;
	else if (_name == "gosh") return -107;
	else if (_name == "msg") return -15;
	else if (_name == "mulmod") return -16;
	else if (_name == "now") return -17;
	else if (_name == "require") return -18;
	else if (_name == "revert") return -19;
	else if (_name == "ripemd160") return -20;
	else if (_name == "selfdestruct") return -21;
	else if (_name == "sha256") return -22;
	else if (_name == "sha3") return -23;
	else if (_name == "stoi") return -106;
	else if (_name == "super") return -25;
	else if (_name == "tvm") return -101;
	else if (_name == "tx") return -26;
	else if (_name == "type") return -27;
	else if (_name == "this") return -28;
	else if (_name == "blobhash") return -29;
	else if (_name == "gasToValue") return -60;
	else if (_name == "valueToGas") return -61;
	else if (_name == "bitSize") return -62;
	else if (_name == "uBitSize") return -63;
	else
		solAssert(false, "Unknown magic variable: \"" + _name + "\".");
}

inline std::vector<std::shared_ptr<MagicVariableDeclaration const>> constructMagicVariables(langutil::EVMVersion _evmVersion)
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
		magicVarDecl("require", TypeProvider::function(strings{}, strings{}, FunctionType::Kind::Require, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("revert", TypeProvider::function(strings(), strings(), FunctionType::Kind::Revert, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("selfdestruct", TypeProvider::function(strings{"address payable"}, strings{}, FunctionType::Kind::Selfdestruct)),
		magicVarDecl("sha256", TypeProvider::function({}, {}, FunctionType::Kind::SHA256, StateMutability::Pure)),

		magicVarDecl("format", TypeProvider::function(strings{}, strings{"string"}, FunctionType::Kind::Format, StateMutability::Pure, FunctionType::Options::withArbitraryParameters())),
		magicVarDecl("gosh", TypeProvider::magic(MagicType::Kind::Gosh)),
		magicVarDecl("logtvm", TypeProvider::function(strings{"string"}, strings{}, FunctionType::Kind::LogTVM, StateMutability::Pure)),
		magicVarDecl("math", TypeProvider::magic(MagicType::Kind::Math)),
		magicVarDecl("rnd", TypeProvider::magic(MagicType::Kind::Rnd)),
		magicVarDecl("stoi", TypeProvider::function(
				TypePointers{TypeProvider::stringStorage()},
				TypePointers{TypeProvider::optional(TypeProvider::integer(256, IntegerType::Modifier::Signed))},
				strings{std::string()},
				strings{std::string()},
				FunctionType::Kind::Stoi,
				StateMutability::Pure)
				),
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
		magicVarDecl("valueToGas", TypeProvider::function({"uint128", "int8"}, {"uint128"}, FunctionType::Kind::ValueToGas, StateMutability::Pure)),
		magicVarDecl("valueToGas", TypeProvider::function({"uint128"}, {"uint128"}, FunctionType::Kind::ValueToGas, StateMutability::Pure)),
		magicVarDecl("gasToValue", TypeProvider::function({"uint128", "int8"}, {"uint128"}, FunctionType::Kind::GasToValue, StateMutability::Pure)),
		magicVarDecl("gasToValue", TypeProvider::function({"uint128"}, {"uint128"}, FunctionType::Kind::GasToValue, StateMutability::Pure)),
		magicVarDecl("bitSize", TypeProvider::function({"int"}, {"uint16"}, FunctionType::Kind::BitSize, StateMutability::Pure)),
		magicVarDecl("uBitSize", TypeProvider::function({"uint"}, {"uint16"}, FunctionType::Kind::UBitSize, StateMutability::Pure)),
	};

	if (_evmVersion >= langutil::EVMVersion::cancun())
		magicVariableDeclarations.push_back(
			magicVarDecl("blobhash", TypeProvider::function(strings{"uint256"}, strings{"bytes32"}, FunctionType::Kind::BlobHash, StateMutability::View))
		);

	return magicVariableDeclarations;
}

}

GlobalContext::GlobalContext(langutil::EVMVersion _evmVersion):
	m_magicVariables{constructMagicVariables(_evmVersion)}
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
