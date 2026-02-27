/*
 * Copyright (C) 2021-2025 EverX. All Rights Reserved.
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

#include <libsolidity/codegen/optimizers/StackOptimizer.hpp>
#include <libsolidity/codegen/optimizers/StackOpcodeSquasher.hpp>
#include <libsolidity/codegen/TVM.hpp>
#include <libsolidity/codegen/Printer.hpp>

using namespace solidity;
using namespace solidity::frontend;

template <class ...Args>
void fff(int n, Args... cmds) {
	std::vector<Pointer<TvmAstNode>> instructions{cmds...};
	auto block = createNode<CodeBlock>(CodeBlock::Type::None, instructions);

	Function f{0, 0, "myFunction", std::nullopt, block, nullptr, false};

	StackOptimizer so{true};
	f.accept(so);

	Printer p{std::cout};
	f.accept(p);
	std::cout.flush();

	solAssert(int(f.block()->instructions().size()) == n, "Fail tests");
}

int main(int /*argc*/, char* /*argv*/[])
{
	GlobalParams::g_tvmVersion = langutil::TVMVersion{};

	fff(5,
		makeGetGlob(10),
		gen("PUSHINT 13"),
		gen("PUSHINT 14"),
		gen("PUSHINT 100"),
		makeSetGlob(11),
		makeROT()
	);

	fff(6,
		makeGetGlob(10),
		gen("PUSHINT 13"),
		gen("PUSHINT 14"),
		gen("PUSHINT 100"),
		makeSetGlob(10),
		makeROT()
	);

	fff(5,
		makeGetGlob(10),
		gen("PUSHINT 13"),
		gen("PUSHINT 14"),
		createNode<StackGen>("CALL $LOLA$", 0, 0),
		makeROT()
	);
	return 0;
}
