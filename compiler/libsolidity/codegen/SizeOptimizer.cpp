/*
 * Copyright (C) 2022-2024 EverX. All Rights Reserved.
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
 * Size optimizer
 */

#include <boost/format.hpp>
#include <boost/range/adaptor/map.hpp>

#include <libsolidity/codegen/SizeOptimizer.hpp>

using namespace std;
using namespace solidity::util;
using namespace solidity::frontend;

struct SPtrNodeLess {
	bool operator()(const Pointer<PushCellOrSlice> &first, const Pointer<PushCellOrSlice> &second) const {
		return *first < *second;
	}
};

class SizeOptimizerPrivate : public TvmAstVisitor {
public:
	bool visit(PushCellOrSlice &_node) override;
	void upd();
private:
	std::map<Pointer<PushCellOrSlice>, std::vector<Pointer<PushCellOrSlice>>, SPtrNodeLess> m_qty;
};

bool SizeOptimizerPrivate::visit(PushCellOrSlice &_node) {
	if (_node.type() == PushCellOrSlice::Type::PUSHSLICE) {
		auto slice = dynamic_pointer_cast<PushCellOrSlice>(_node.shared_from_this());
		m_qty[slice].push_back(slice);
	}
	return false;
}

void SizeOptimizerPrivate::upd() {
	for (std::vector<Pointer<PushCellOrSlice>> & arr : m_qty | boost::adaptors::map_values) {
		int qty = arr.size();
		int bitSize = getRootBitSize(*arr.at(0));
		double oldSize = (12 + bitSize) * qty;
		double newSize = 8 * qty + bitSize;
		double coef = 1.7;
		if (oldSize >= 500 && oldSize >= coef * newSize) {
			for (Pointer<PushCellOrSlice> & node : arr) {
				node->updToRef();
			}
		}
	}
}

void SizeOptimizer::optimize(Pointer<Contract>& c){
	SizeOptimizerPrivate sp;
	c->accept(sp);
	sp.upd();
}
