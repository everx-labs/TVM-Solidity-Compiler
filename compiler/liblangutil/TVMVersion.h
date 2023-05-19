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
 * TVM versioning.
 */

#pragma once

#include <optional>
#include <string>

#include <boost/operators.hpp>


namespace solidity::langutil
{

/**
 * A version specifier of the TVM we want to compile to.
 */
class TVMVersion:
	boost::less_than_comparable<TVMVersion>,
	boost::equality_comparable<TVMVersion>
{
public:
	TVMVersion() = default;

	static TVMVersion ever() { return TVMVersion{Version::Ever}; }
	static TVMVersion ton() { return TVMVersion{Version::Ton}; }
	static TVMVersion gosh() { return TVMVersion{Version::Gosh}; }

	static std::optional<TVMVersion> fromString(std::string const& _version)
	{
		for (auto const& v: {ever(), ton(), gosh()})
			if (_version == v.name())
				return v;
		return std::nullopt;
	}

	bool operator==(TVMVersion const& _other) const { return m_version == _other.m_version; }
	bool operator<(TVMVersion const& _other) const { return m_version < _other.m_version; }

	std::string name() const
	{
		switch (m_version)
		{
		case Version::Ever: return "ever";
		case Version::Ton: return "ton";
		case Version::Gosh: return "gosh";
		}
		return "INVALID";
	}

private:
	enum class Version { Ever, Ton, Gosh };

	explicit TVMVersion(Version _version): m_version(_version) {}

	Version m_version = Version::Ever;
};

}
