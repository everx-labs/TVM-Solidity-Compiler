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

#include <libsolidity/ast/AST.h>
#include <libsolidity/ast/TypeProvider.h>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace std;
using namespace solidity;
using namespace solidity::frontend;
using namespace solidity::util;

BoolType const TypeProvider::m_boolean{};
NullType const TypeProvider::m_nullType{};
TvmCellType const TypeProvider::m_tvmcell{};
TvmSliceType const TypeProvider::m_tvmslice{};
TvmBuilderType const TypeProvider::m_tvmbuilder{};

InaccessibleDynamicType const TypeProvider::m_inaccessibleDynamic{};

/// The string and bytes unique_ptrs are initialized when they are first used because
/// they rely on `byte` being available which we cannot guarantee in the static init context.
unique_ptr<ArrayType> TypeProvider::m_bytesStorage;
unique_ptr<ArrayType> TypeProvider::m_bytesMemory;
unique_ptr<ArrayType> TypeProvider::m_bytesCalldata;
unique_ptr<ArrayType> TypeProvider::m_stringStorage;
unique_ptr<ArrayType> TypeProvider::m_stringMemory;

TupleType const TypeProvider::m_emptyTuple{};
AddressType const TypeProvider::m_address{};
InitializerListType const TypeProvider::m_initializerList{};
CallListType const TypeProvider::m_callList{};

std::unique_ptr<IntegerType> const TypeProvider::m_int257 = make_unique<IntegerType>(257, IntegerType::Modifier::Signed);

//for i in range(1, 257):
//	print("{{make_unique<IntegerType>({}, IntegerType::Modifier::Signed)}},".format(i))
array<unique_ptr<IntegerType>, 256> const TypeProvider::m_intM{{
	{make_unique<IntegerType>(1, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(2, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(3, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(4, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(5, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(6, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(7, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(8, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(9, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(10, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(11, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(12, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(13, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(14, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(15, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(16, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(17, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(18, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(19, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(20, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(21, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(22, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(23, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(24, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(25, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(26, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(27, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(28, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(29, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(30, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(31, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(32, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(33, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(34, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(35, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(36, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(37, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(38, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(39, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(40, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(41, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(42, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(43, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(44, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(45, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(46, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(47, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(48, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(49, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(50, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(51, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(52, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(53, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(54, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(55, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(56, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(57, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(58, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(59, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(60, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(61, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(62, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(63, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(64, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(65, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(66, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(67, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(68, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(69, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(70, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(71, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(72, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(73, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(74, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(75, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(76, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(77, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(78, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(79, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(80, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(81, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(82, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(83, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(84, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(85, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(86, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(87, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(88, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(89, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(90, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(91, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(92, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(93, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(94, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(95, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(96, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(97, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(98, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(99, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(100, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(101, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(102, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(103, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(104, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(105, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(106, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(107, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(108, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(109, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(110, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(111, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(112, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(113, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(114, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(115, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(116, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(117, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(118, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(119, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(120, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(121, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(122, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(123, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(124, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(125, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(126, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(127, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(128, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(129, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(130, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(131, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(132, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(133, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(134, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(135, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(136, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(137, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(138, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(139, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(140, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(141, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(142, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(143, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(144, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(145, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(146, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(147, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(148, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(149, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(150, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(151, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(152, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(153, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(154, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(155, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(156, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(157, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(158, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(159, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(160, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(161, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(162, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(163, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(164, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(165, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(166, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(167, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(168, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(169, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(170, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(171, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(172, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(173, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(174, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(175, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(176, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(177, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(178, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(179, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(180, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(181, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(182, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(183, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(184, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(185, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(186, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(187, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(188, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(189, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(190, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(191, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(192, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(193, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(194, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(195, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(196, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(197, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(198, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(199, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(200, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(201, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(202, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(203, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(204, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(205, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(206, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(207, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(208, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(209, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(210, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(211, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(212, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(213, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(214, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(215, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(216, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(217, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(218, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(219, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(220, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(221, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(222, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(223, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(224, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(225, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(226, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(227, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(228, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(229, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(230, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(231, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(232, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(233, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(234, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(235, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(236, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(237, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(238, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(239, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(240, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(241, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(242, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(243, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(244, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(245, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(246, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(247, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(248, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(249, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(250, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(251, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(252, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(253, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(254, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(255, IntegerType::Modifier::Signed)},
	{make_unique<IntegerType>(256, IntegerType::Modifier::Signed)},
}};

//for i in range(1, 257):
//	print("{{make_unique<IntegerType>({}, IntegerType::Modifier::Unsigned)}},".format(i))
array<unique_ptr<IntegerType>, 256> const TypeProvider::m_uintM{{
	{make_unique<IntegerType>(1, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(2, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(3, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(4, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(5, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(6, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(7, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(8, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(9, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(10, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(11, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(12, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(13, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(14, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(15, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(16, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(17, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(18, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(19, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(20, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(21, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(22, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(23, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(24, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(25, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(26, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(27, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(28, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(29, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(30, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(31, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(32, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(33, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(34, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(35, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(36, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(37, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(38, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(39, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(40, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(41, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(42, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(43, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(44, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(45, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(46, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(47, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(48, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(49, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(50, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(51, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(52, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(53, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(54, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(55, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(56, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(57, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(58, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(59, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(60, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(61, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(62, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(63, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(64, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(65, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(66, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(67, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(68, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(69, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(70, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(71, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(72, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(73, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(74, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(75, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(76, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(77, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(78, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(79, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(80, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(81, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(82, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(83, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(84, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(85, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(86, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(87, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(88, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(89, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(90, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(91, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(92, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(93, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(94, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(95, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(96, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(97, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(98, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(99, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(100, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(101, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(102, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(103, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(104, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(105, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(106, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(107, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(108, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(109, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(110, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(111, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(112, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(113, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(114, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(115, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(116, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(117, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(118, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(119, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(120, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(121, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(122, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(123, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(124, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(125, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(126, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(127, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(128, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(129, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(130, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(131, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(132, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(133, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(134, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(135, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(136, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(137, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(138, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(139, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(140, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(141, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(142, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(143, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(144, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(145, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(146, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(147, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(148, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(149, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(150, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(151, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(152, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(153, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(154, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(155, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(156, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(157, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(158, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(159, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(160, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(161, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(162, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(163, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(164, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(165, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(166, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(167, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(168, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(169, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(170, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(171, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(172, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(173, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(174, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(175, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(176, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(177, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(178, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(179, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(180, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(181, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(182, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(183, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(184, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(185, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(186, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(187, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(188, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(189, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(190, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(191, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(192, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(193, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(194, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(195, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(196, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(197, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(198, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(199, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(200, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(201, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(202, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(203, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(204, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(205, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(206, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(207, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(208, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(209, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(210, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(211, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(212, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(213, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(214, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(215, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(216, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(217, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(218, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(219, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(220, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(221, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(222, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(223, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(224, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(225, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(226, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(227, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(228, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(229, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(230, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(231, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(232, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(233, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(234, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(235, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(236, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(237, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(238, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(239, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(240, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(241, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(242, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(243, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(244, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(245, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(246, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(247, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(248, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(249, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(250, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(251, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(252, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(253, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(254, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(255, IntegerType::Modifier::Unsigned)},
	{make_unique<IntegerType>(256, IntegerType::Modifier::Unsigned)},
}};

array<unique_ptr<FixedBytesType>, 32> const TypeProvider::m_bytesM{{
	{make_unique<FixedBytesType>(1)},
	{make_unique<FixedBytesType>(2)},
	{make_unique<FixedBytesType>(3)},
	{make_unique<FixedBytesType>(4)},
	{make_unique<FixedBytesType>(5)},
	{make_unique<FixedBytesType>(6)},
	{make_unique<FixedBytesType>(7)},
	{make_unique<FixedBytesType>(8)},
	{make_unique<FixedBytesType>(9)},
	{make_unique<FixedBytesType>(10)},
	{make_unique<FixedBytesType>(11)},
	{make_unique<FixedBytesType>(12)},
	{make_unique<FixedBytesType>(13)},
	{make_unique<FixedBytesType>(14)},
	{make_unique<FixedBytesType>(15)},
	{make_unique<FixedBytesType>(16)},
	{make_unique<FixedBytesType>(17)},
	{make_unique<FixedBytesType>(18)},
	{make_unique<FixedBytesType>(19)},
	{make_unique<FixedBytesType>(20)},
	{make_unique<FixedBytesType>(21)},
	{make_unique<FixedBytesType>(22)},
	{make_unique<FixedBytesType>(23)},
	{make_unique<FixedBytesType>(24)},
	{make_unique<FixedBytesType>(25)},
	{make_unique<FixedBytesType>(26)},
	{make_unique<FixedBytesType>(27)},
	{make_unique<FixedBytesType>(28)},
	{make_unique<FixedBytesType>(29)},
	{make_unique<FixedBytesType>(30)},
	{make_unique<FixedBytesType>(31)},
	{make_unique<FixedBytesType>(32)}
}};

array<unique_ptr<MagicType>, 7> const TypeProvider::m_magics{{
	{make_unique<MagicType>(MagicType::Kind::Block)},
	{make_unique<MagicType>(MagicType::Kind::Message)},
	{make_unique<MagicType>(MagicType::Kind::Transaction)},
	{make_unique<MagicType>(MagicType::Kind::ABI)},
	{make_unique<MagicType>(MagicType::Kind::TVM)},
	{make_unique<MagicType>(MagicType::Kind::Math)},
	{make_unique<MagicType>(MagicType::Kind::Rnd)}
	// MetaType is stored separately
}};

inline void clearCache(Type const& type)
{
	type.clearCache();
}

template <typename T>
inline void clearCache(unique_ptr<T> const& type)
{
	// Some lazy-initialized types might not exist yet.
	if (type)
		type->clearCache();
}

template <typename Container>
inline void clearCaches(Container& container)
{
	for (auto const& e: container)
		clearCache(e);
}

void TypeProvider::reset()
{
	clearCache(m_boolean);
	clearCache(m_inaccessibleDynamic);
	clearCache(m_bytesStorage);
	clearCache(m_bytesMemory);
	clearCache(m_bytesCalldata);
	clearCache(m_stringStorage);
	clearCache(m_stringMemory);
	clearCache(m_emptyTuple);
	clearCache(m_address);
	clearCaches(instance().m_intM);
	clearCaches(instance().m_uintM);
	clearCaches(instance().m_bytesM);
	clearCaches(instance().m_magics);

	instance().m_generalTypes.clear();
	instance().m_stringLiteralTypes.clear();
	instance().m_ufixedMxN.clear();
	instance().m_fixedMxN.clear();
	instance().m_varInterger.clear();
}

template <typename T, typename... Args>
inline T const* TypeProvider::createAndGet(Args&& ... _args)
{
	instance().m_generalTypes.emplace_back(make_unique<T>(std::forward<Args>(_args)...));
	return static_cast<T const*>(instance().m_generalTypes.back().get());
}

Type const* TypeProvider::fromElementaryTypeName(ElementaryTypeNameToken const& _type)
{
	solAssert(
		TokenTraits::isElementaryTypeName(_type.token()),
		"Expected an elementary type name but got " + _type.toString()
	);

	unsigned const m = _type.firstNumber();
	unsigned const n = _type.secondNumber();

	switch (_type.token())
	{
	case Token::VarUint:
		return varInteger(32, IntegerType::Modifier::Unsigned);
	case Token::VarUintM:
		return varInteger(m, IntegerType::Modifier::Unsigned);
	case Token::VarInt:
		return varInteger(32, IntegerType::Modifier::Signed);
	case Token::VarIntM:
		return varInteger(m, IntegerType::Modifier::Signed);
	case Token::IntM:
		return integer(m, IntegerType::Modifier::Signed);
	case Token::UIntM:
		return integer(m, IntegerType::Modifier::Unsigned);
	case Token::Byte:
		return byte();
	case Token::BytesM:
		return fixedBytes(m);
	case Token::FixedMxN:
		return fixedPoint(m, n, FixedPointType::Modifier::Signed);
	case Token::UFixedMxN:
		return fixedPoint(m, n, FixedPointType::Modifier::Unsigned);
	case Token::Int:
		return integer(256, IntegerType::Modifier::Signed);
	case Token::UInt:
		return integer(256, IntegerType::Modifier::Unsigned);
	case Token::Fixed:
		return fixedPoint(128, 18, FixedPointType::Modifier::Signed);
	case Token::UFixed:
		return fixedPoint(128, 18, FixedPointType::Modifier::Unsigned);
	case Token::Address:
		return address();
	case Token::Bool:
		return boolean();
	case Token::TvmCell:
		return tvmcell();
	case Token::TvmSlice:
		return tvmslice();
	case Token::TvmBuilder:
		return tvmbuilder();
	case Token::ExtraCurrencyCollection:
		return extraCurrencyCollection();
	case Token::Bytes:
		return bytesStorage();
	case Token::String:
		return stringStorage();
	default:
		solAssert(
			false,
			"Unable to convert elementary typename " + _type.toString() + " to type."
		);
	}
}

TypePointer TypeProvider::fromElementaryTypeName(string const& _name)
{
	vector<string> nameParts;
	boost::split(nameParts, _name, boost::is_any_of(" "));
	solAssert(nameParts.size() == 1 || nameParts.size() == 2, "Cannot parse elementary type: " + _name);

	Token token;
	unsigned short firstNum, secondNum;
	tie(token, firstNum, secondNum) = TokenTraits::fromIdentifierOrKeyword(nameParts[0]);

	auto t = fromElementaryTypeName(ElementaryTypeNameToken(token, firstNum, secondNum));
	if (auto* ref = dynamic_cast<ReferenceType const*>(t))
	{
		return withLocation(ref, true);
	}
	else if (t->category() == Type::Category::Address)
	{
		if (nameParts.size() == 2)
		{
			if (nameParts[1] == "payable")
				return address();
			else
				solAssert(false, "Invalid state mutability for address type: " + nameParts[1]);
		}
		return address();
	}
	else
	{
		solAssert(nameParts.size() == 1, "Storage location suffix only allowed for reference types");
		return t;
	}
}

ArrayType const* TypeProvider::bytesStorage()
{
	if (!m_bytesStorage)
		m_bytesStorage = make_unique<ArrayType>(false);
	return m_bytesStorage.get();
}

ArrayType const* TypeProvider::bytesMemory()
{
	if (!m_bytesMemory)
		m_bytesMemory = make_unique<ArrayType>(false);
	return m_bytesMemory.get();
}

ArrayType const* TypeProvider::bytesCalldata()
{
	if (!m_bytesCalldata)
		m_bytesCalldata = make_unique<ArrayType>(false);
	return m_bytesCalldata.get();
}

ArrayType const* TypeProvider::stringStorage()
{
	if (!m_stringStorage)
		m_stringStorage = make_unique<ArrayType>(true);
	return m_stringStorage.get();
}

ArrayType const* TypeProvider::stringMemory()
{
	if (!m_stringMemory)
		m_stringMemory = make_unique<ArrayType>(true);
	return m_stringMemory.get();
}

TypePointer TypeProvider::forLiteral(Literal const& _literal)
{
	switch (_literal.token())
	{
	case Token::TrueLiteral:
	case Token::FalseLiteral:
		return boolean();
	case Token::NullLiteral:
		return nullType();
	case Token::Number:
		return rationalNumber(_literal);
	case Token::StringLiteral:
	case Token::HexStringLiteral:
		return stringLiteral(_literal.value());
	default:
		return nullptr;
	}
}

RationalNumberType const* TypeProvider::rationalNumber(Literal const& _literal)
{
	solAssert(_literal.token() == Token::Number, "");
	std::tuple<bool, rational> validLiteral = RationalNumberType::isValidLiteral(_literal);
	if (std::get<0>(validLiteral))
	{
		TypePointer compatibleBytesType = nullptr;
		if (_literal.isHexNumber())
		{
			size_t const digitCount = _literal.valueWithoutUnderscores().length() - 2;
			if (digitCount % 2 == 0 && (digitCount / 2) <= 32)
				compatibleBytesType = fixedBytes(digitCount / 2);
		}

		return rationalNumber(std::get<1>(validLiteral), compatibleBytesType);
	}
	return nullptr;
}

StringLiteralType const* TypeProvider::stringLiteral(string const& literal)
{
	auto i = instance().m_stringLiteralTypes.find(literal);
	if (i != instance().m_stringLiteralTypes.end())
		return i->second.get();
	else
		return instance().m_stringLiteralTypes.emplace(literal, make_unique<StringLiteralType>(literal)).first->second.get();
}

VarInteger const* TypeProvider::varInteger(unsigned m, IntegerType::Modifier _modifier) {
	auto& map = instance().m_varInterger;
	auto i = map.find(make_pair(m, _modifier));
	if (i != map.end())
		return i->second.get();

	return map.emplace(
			make_pair(m, _modifier),
			make_unique<VarInteger>(m, _modifier)
	).first->second.get();

}

FixedPointType const* TypeProvider::fixedPoint(unsigned m, unsigned n, FixedPointType::Modifier _modifier)
{
	auto& map = _modifier == FixedPointType::Modifier::Unsigned ? instance().m_ufixedMxN : instance().m_fixedMxN;

	auto i = map.find(make_pair(m, n));
	if (i != map.end())
		return i->second.get();

	return map.emplace(
		make_pair(m, n),
		make_unique<FixedPointType>(m, n, _modifier)
	).first->second.get();
}

TupleType const* TypeProvider::tuple(vector<Type const*> members)
{
	if (members.empty())
		return &m_emptyTuple;

	return createAndGet<TupleType>(move(members));
}

ReferenceType const* TypeProvider::withLocation(ReferenceType const* _type, bool _isPointer)
{
	if (_type->isPointer() == _isPointer)
		return _type;

	instance().m_generalTypes.emplace_back(_type->copyForLocation(_isPointer));
	return static_cast<ReferenceType const*>(instance().m_generalTypes.back().get());
}

FunctionType const* TypeProvider::function(FunctionDefinition const& _function, FunctionType::Kind _kind)
{
	return createAndGet<FunctionType>(_function, _kind);
}

FunctionType const* TypeProvider::function(VariableDeclaration const& _varDecl)
{
	return createAndGet<FunctionType>(_varDecl);
}

FunctionType const* TypeProvider::function(EventDefinition const& _def)
{
	return createAndGet<FunctionType>(_def);
}

FunctionType const* TypeProvider::function(FunctionTypeName const& _typeName)
{
	return createAndGet<FunctionType>(_typeName);
}

FunctionType const* TypeProvider::function(
	strings const& _parameterTypes,
	strings const& _returnParameterTypes,
	FunctionType::Kind _kind,
	bool _arbitraryParameters,
	StateMutability _stateMutability
)
{
	return createAndGet<FunctionType>(
		_parameterTypes, _returnParameterTypes,
		_kind, _arbitraryParameters, _stateMutability
	);
}

FunctionType const* TypeProvider::function(
	TypePointers const& _parameterTypes,
	TypePointers const& _returnParameterTypes,
	strings _parameterNames,
	strings _returnParameterNames,
	FunctionType::Kind _kind,
	bool _arbitraryParameters,
	StateMutability _stateMutability,
	Declaration const* _declaration,
	bool _bound
)
{
	return createAndGet<FunctionType>(
		_parameterTypes,
		_returnParameterTypes,
		_parameterNames,
		_returnParameterNames,
		_kind,
		_arbitraryParameters,
		_stateMutability,
		_declaration,
		_bound
	);
}

RationalNumberType const* TypeProvider::rationalNumber(rational const& _value, Type const* _compatibleBytesType)
{
	return createAndGet<RationalNumberType>(_value, _compatibleBytesType);
}

ArrayType const* TypeProvider::array(bool _isString)
{
	return _isString ? stringStorage() : bytesStorage();
}

ArrayType const* TypeProvider::array(Type const* _baseType)
{
	return createAndGet<ArrayType>(_baseType);
}

ArrayType const* TypeProvider::array(Type const* _baseType, u256 const& _length)
{
	return createAndGet<ArrayType>(_baseType, _length);
}

ArraySliceType const* TypeProvider::arraySlice(ArrayType const& _arrayType)
{
	return createAndGet<ArraySliceType>(_arrayType);
}

ContractType const* TypeProvider::contract(ContractDefinition const& _contractDef, bool _isSuper)
{
	return createAndGet<ContractType>(_contractDef, _isSuper);
}

EnumType const* TypeProvider::enumType(EnumDefinition const& _enumDef)
{
	return createAndGet<EnumType>(_enumDef);
}

ModuleType const* TypeProvider::module(SourceUnit const& _source)
{
	return createAndGet<ModuleType>(_source);
}

TypeType const* TypeProvider::typeType(Type const* _actualType)
{
	return createAndGet<TypeType>(_actualType);
}

StructType const* TypeProvider::structType(StructDefinition const& _struct)
{
	return createAndGet<StructType>(_struct);
}

ModifierType const* TypeProvider::modifier(ModifierDefinition const& _def)
{
	return createAndGet<ModifierType>(_def);
}

MagicType const* TypeProvider::magic(MagicType::Kind _kind)
{
	solAssert(_kind != MagicType::Kind::MetaType, "MetaType is handled separately");
	return m_magics.at(static_cast<size_t>(_kind)).get();
}

MagicType const* TypeProvider::meta(Type const* _type)
{
	solAssert(_type && _type->category() == Type::Category::Contract, "Only contracts supported for now.");
	return createAndGet<MagicType>(_type);
}

MappingType const* TypeProvider::mapping(Type const* _keyType, Type const* _valueType)
{
	return createAndGet<MappingType>(_keyType, _valueType);
}

ExtraCurrencyCollectionType const *TypeProvider::extraCurrencyCollection()
{
	return createAndGet<ExtraCurrencyCollectionType>();
}

OptionalType const* TypeProvider::optional(Type const* _type)
{
	return createAndGet<OptionalType>(_type);
}

TvmVectorType const* TypeProvider::tvmtuple(Type const* _type)
{
	return createAndGet<TvmVectorType>(_type);
}
