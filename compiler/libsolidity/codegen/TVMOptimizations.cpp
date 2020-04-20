/*
 * Copyright 2018-2020 TON DEV SOLUTIONS LTD.
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
 * @date 2020
 * TVM codegen driver
 */

#include "TVMOptimizations.hpp"
#include <boost/format.hpp>

namespace solidity::frontend {

struct TVMOptimizer {
	vector<string>	lines_;

	static bool is_space(char ch) {
		return ch == ' ' || ch == '\t';
	}

	static bool is_comment_or_empty_line(const string& str) {
		for (auto ch : str) {
			if (is_space(ch))
				continue;
			return ch == ';';
		}
		// empty line
		return true;
	}

	struct Cmd {
		string prefix_, cmd_, rest_, comment_;

		bool is_simple_command_{false};
		int inputs_count_{0}, outputs_count_{0};

		explicit Cmd(const string& str) {
			int i = 0, n = str.size();
			while (i < n && is_space(str[i]))
				prefix_.push_back(str[i++]);
			while (i < n && !is_space(str[i]) && str[i] != ';')
				cmd_.push_back(str[i++]);
			while (i < n && is_space(str[i]))
				i++;
			while (i < n && str[i] != ';')
				rest_.push_back(str[i++]);
			analyze();
		}

		bool is(const string& cmd) const {
			return cmd_ == cmd;
		}

		string rest() const {
			return rest_;
		}

		string without_prefix() const {
			if (rest_.empty()) return cmd_;
			return cmd_ + " " + rest_;
		}

		// TODO: Function fetch_int return int. And don't check integer overflow.
		//		 What if there is big number. For example 128 unsigned int.
		//		 Maybe use using u256 = boost::multiprecision::number<boost::multiprecision::cpp_int_backend<256, 256, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void>>;
		//		 from libsolutil/Common.h or something else
		int fetch_int() const {
			return atoi(rest_.c_str());
		}

		int fetch_first_int() const {
			size_t i = rest_.find(',');
			solAssert(i != string::npos, "");
			return atoi(rest_.substr(0, i).c_str());
		}

		int fetch_second_int() const {
			size_t i = rest_.find(',');
			solAssert(i != string::npos, "");
			while (i < rest_.size() && is_space(rest_[i]))
				i++;
			solAssert(i != rest_.size(), "");
			return atoi(rest_.substr(i).c_str());
		}

		bool is_drop_kind() const {
			return get_drop_index() > 0;
		}

		int get_drop_index() const {
			if (is_DROP())
				return 1;
			if (is("DROP2"))
				return 2;
			if (is("BLKDROP"))
				return fetch_int();
			return 0;
		}

		int get_push_index() const {
			solAssert(is_PUSH(), "");
			if (is_DUP()) return 0;
			string s = rest();
			s.erase(s.begin()); // skipping char S
			return atoi(s.c_str());
		}

		std::pair<int, int> get_push2_indexes() const {
			solAssert(is("PUSH2"), "");
			std::string target = rest();
			std::smatch sm;
			std::regex re1("[S|s](\\d+),\\s*[S|s](\\d+)");
			std::regex_search(target, sm, re1);

			int si = std::stoi(sm[1]);
			int sj = std::stoi(sm[2]);
			return {si, sj};
		}

		int get_pop_index() const {
			solAssert(is_POP(), "");
			string s = rest();
			s.erase(s.begin());
			return atoi(s.c_str());
		}

		bool is_commutative() const {
			return is_ADD() || is_MUL() || is("AND") || is("OR") || is("XOR") ||
					is("EQUAL") || is("NEQ");
		}

		bool is_add_or_sub() const {
			return is_ADD() || is_SUB();
		}

		bool is_ADD() const 	{ 	return is("ADD"); 		}
		bool is_MUL() const 	{ 	return is("MUL");		}
		bool is_SUB() const 	{ 	return is("SUB"); 		}
		bool is_DROP() const 	{ 	return is("DROP"); 		}
		bool is_NIP() const 	{ 	return is("NIP"); 		}
		bool is_SWAP() const 	{ 	return is("SWAP"); 		}
		bool is_DUP() const 	{ 	return is("DUP"); 		}
		bool is_PUSH() const 	{ 	return is("PUSH") || is("DUP"); 		}
		bool is_PUSHINT() const { 	return is("PUSHINT"); 	}
		bool is_POP() const 	{ 	return is("POP"); 		}

		bool is_simple_command(int inp, int outp) const {
			return is_simple_command_ &&
				   inp == inputs_count_ &&
				   outp == outputs_count_;
		}

	private:
		void set_simple_command(int inp, int outp) {
			is_simple_command_ = true;
			inputs_count_  = inp;
			outputs_count_ = outp;
		}

		bool try_simple_command(const set<string>& s, int inp, int outp) {
			if (s.count(cmd_)) {
				set_simple_command(inp, outp);
				return true;
			}
			return false;
		}

		void analyze() {
			static const set<string> s01 {
				"PUSHINT", "GETGLOB", "PUSHSLICE", "TRUE", "FALSE", "ZERO",
				"NEWC", "NEWDICT",
			};
			static const set<string> s10 {
				"DROP", "SETGLOB", "ENDS", "THROWIF", "THROWIFNOT"
			};
			static const set<string> s11 {
				"FITS", "UFITS", "INC", "DEC", "EQINT", "NOT",
				"SHA256U", "HASHCU", "HASHSU", "CTOS", "INDEX",
				"FIRST", "SECOND", "THIRD", "PARSEMSGADDR", "SBITS"
			};
			static const set<string> s21 {
				"ADD", "MUL", "SUB", "SUBR", "DIV", "MOD",
				"OR", "AND", "EQ", "LESS", "NEQ",
				"SETINDEX", "PAIR", "PLDUX", "INDEXVAR"
			};
			static const set<string> s32 {
				"DICTUDEL", "DICTIDEL", "DICTDEL"
			};

			if (try_simple_command(s01, 0, 1)) return;
			if (try_simple_command(s10, 1, 0)) return;
			if (try_simple_command(s11, 1, 1)) return;
			if (try_simple_command(s21, 2, 1)) return;
			if (try_simple_command(s32, 3, 2)) return;

			if (is("TUPLE"))
				return set_simple_command(fetch_int(), 1);
			if (is("UNTUPLE"))
				return set_simple_command(1, fetch_int());
			if (is("UNPAIR"))
				return set_simple_command(1, 2);
			if (is("SETINDEXVAR"))
				return set_simple_command(3, 1);
		}

	};

	static string get_cmd(const string& str) {
		return Cmd(str).cmd_;
	}

	Cmd cmd(int idx) const {
		if (valid(idx))
			return Cmd(lines_[idx]);
		return Cmd("");
	}

	int next_command_line(int idx) const {
		if (!valid(idx)) return -1;
		idx++;
		while (true) {
			if (!valid(idx))
				return -1;
			if (!is_comment_or_empty_line(lines_[idx]))
				return idx;
			idx++;
		}
	}

	bool valid(int idx) const {
		return idx >= 0 && size_t(idx) < lines_.size();
	}

	void remove(int idx) {
		lines_.erase(lines_.begin() + idx);
	}

	void insert(int idx, const string& cmd, const string& pfx = "") {
		lines_.insert(lines_.begin() + idx, pfx + cmd);
	}

	bool is_cmd(int idx, const string& cmd) const {
		return valid(idx) && get_cmd(lines_[idx]) == cmd;
	}

	struct Result {
		bool continue_;
		int remove_ = 0;
		vector<string> commands_;

		Result(bool cont, int remove = 0, vector<string> commands = {}) :
			continue_(cont), remove_(remove), commands_{std::move(commands)} {

		}

		static Result Replace(int remove, const string& cmd, const string& cmd2 = "") {
			Result res(true, remove);
			if (!cmd.empty())  res.commands_.push_back(cmd);
			if (!cmd2.empty()) res.commands_.push_back(cmd2);
			return res;
		}

		static Result Comment(const string& cmd) {
			Result res(false);
			res.commands_.push_back(cmd);
			return res;
		}
	};

	Result optimize_at(const int idx1) const {
		int idx2 = next_command_line(idx1);
		int idx3 = next_command_line(idx2);
		int idx4 = next_command_line(idx3);
		int idx5 = next_command_line(idx4);
		Cmd cmd1 = cmd(idx1);
		Cmd cmd2 = cmd(idx2);
		Cmd cmd3 = cmd(idx3);
		Cmd cmd4 = cmd(idx4);
		Cmd cmd5 = cmd(idx5);
		// TODO: INC + UFITS256...
		if (cmd1.is_SWAP()) {
			if (cmd2.is_SUB())		return Result::Replace(2, "SUBR");
			if (cmd2.is("SUBR"))	return Result::Replace(2, "SUB");
			if (cmd2.is_SWAP())	return Result::Replace(2, "");
			if (cmd2.is_commutative())	return Result::Replace(1, "");
		}
		if (cmd1.is_PUSHINT() && cmd3.is_PUSHINT()) {
			// TODO: consider INC/DEC as well
			if (cmd2.is_add_or_sub() && cmd4.is_add_or_sub()) {
				int sum = 0;
				sum += (cmd2.is_ADD()? +1 : -1) * cmd1.fetch_int();
				sum += (cmd4.is_ADD()? +1 : -1) * cmd3.fetch_int();
				return Result::Replace(4, "PUSHINT " + toString(sum), "ADD");
			}
		}
		if (cmd1.is_PUSHINT()) {
			if (cmd1.rest() == "1") {
				if (cmd2.is_ADD()) return Result::Replace(2, "INC");
				if (cmd2.is_SUB()) return Result::Replace(2, "DEC");
			}
			int value = cmd1.fetch_int();
			if (-128 <= value && value <= 127) {
				if (cmd2.is_ADD()) return Result::Replace(2, "ADDCONST " + std::to_string(value));
				if (cmd2.is_MUL()) return Result::Replace(2, "MULCONST " + std::to_string(value));
			}
			if (-128 <= -value && -value <= 127) {
				if (cmd2.is_SUB()) return Result::Replace(2, "ADDCONST " + std::to_string(-value));
			}
		}
		if (cmd1.is("RET")) {
			// delete commands after RET
			if (cmd2.prefix_ == cmd1.prefix_)
				return Result::Replace(2, "RET");
		}
		if (cmd1.is("RET") && cmd2.is("}")) {
			return Result::Replace(2, "}");
		}
		if (cmd2.is_NIP() && cmd3.is_NIP()) {
			if (cmd1.is_PUSH() && cmd1.get_push_index() == 1) {
				return Result::Replace(3, "DROP");
			}
			if (cmd1.is_PUSHINT() || cmd1.is("GETGLOB")) {
				return Result::Replace(3, "DROP2", cmd1.without_prefix());
			}
			// return Result::Comment(";;;;;;;;;;;;;; NIP+NIP");
		}
		if (cmd1.is_NIP() && cmd2.is_NIP() && cmd3.is_NIP()) {
			int i = idx1, n = 0;
			while (cmd(i).is_NIP()) {
				n++;
				i = next_command_line(i);
			}
			if (n > 15) n = 15;
			return Result::Replace(n, "BLKSWAP " + toString(n) + ", 1", "BLKDROP " + toString(n));
		}
		if (cmd1.is_PUSH() && cmd2.is_PUSH()) {
			if (cmd4.is_NIP() && cmd5.is_NIP())
				return Result::Comment(";;;;;;;;;;;;;; PUSH+PUSH+NIP+NIP ");
		}
		if (cmd1.is_PUSHINT() && cmd2.is_PUSHINT() && cmd3.is_PUSHINT()) {
			int i = idx1, n = 0;
			while (cmd(i).is_PUSHINT() && cmd(i).fetch_int() == 0) {
				n++;
				i = next_command_line(i);
			}
			if (n >= 3) {
				Result res = Result::Replace(n, "PUSHINT 0");
				n--;
				while (n > 0) {
					int nn = std::min(15, n);
					res.commands_.push_back(make_BLKPUSH(nn, 0));
					n -= nn;
				}
				return res;
			}
		}
		if (cmd3.is_SWAP()) {
			bool ok1 = cmd1.is_simple_command(0, 1) || cmd1.is_PUSH();
			bool ok2 = cmd2.is_simple_command(0, 1) || cmd2.is_PUSH();
			if (ok1 && ok2) {
				if (cmd2.is_PUSH() && cmd2.get_push_index() == 0)
					return Result::Replace(3, cmd1.without_prefix(), cmd2.without_prefix());
				string s1 = cmd2.is_PUSH() ? make_PUSH(cmd2.get_push_index()-1) : cmd2.without_prefix();
				string s2 = cmd1.is_PUSH() ? make_PUSH(cmd1.get_push_index()+1) : cmd1.without_prefix();
				return Result::Replace(3, s1, s2);
			}
		}
		if (cmd1.is_PUSH() && cmd2.is_PUSH()) {
			int i = idx1, n = 0;
			while (cmd(i).is_PUSH() && cmd(i).get_push_index() == cmd1.get_push_index()) {
				n++;
				i = next_command_line(i);
			}
			if (n >= 2 && cmd1.get_push_index() <= 15) {
				if (n > 15) n = 15;
				return Result::Replace(n, make_BLKPUSH(n, cmd1.get_push_index()));
			}
		}
		if ((cmd1.is_PUSH() || cmd1.is_PUSHINT()) && cmd2.is_drop_kind()) {
			if (cmd2.is_DROP()) {
				return Result::Replace(2, "");
			} else {
				return Result::Replace(2, make_DROP(cmd2.get_drop_index()-1));
			}
		}
		if (cmd1.is("BLKPUSH") && cmd2.is_drop_kind()) {
			int diff = cmd1.fetch_first_int() - cmd2.get_drop_index();
			if (diff == 0)
				return Result::Replace(2, "");
			if (diff < 0)
				return Result::Replace(2, make_DROP(-diff));
			else
				return Result::Replace(2, make_BLKPUSH(diff, cmd1.fetch_second_int()));
		}
		if (cmd1.is_simple_command_ && cmd1.outputs_count_ == 1 && cmd2.is_drop_kind()) {
			int q = cmd1.inputs_count_ + cmd2.get_drop_index() - 1;
			solAssert(q >= 0, "");
			if (q == 0) return Result::Replace(2, "");
			return Result::Replace(2, make_DROP(q));
		}
		if (cmd1.is_simple_command_ && cmd1.inputs_count_ == 0 && cmd1.outputs_count_ == 1 && cmd2.is_NIP()) {
			return Result::Replace(2, make_DROP(1), cmd1.without_prefix());
		}
		if (cmd1.is_drop_kind() && cmd2.is_drop_kind()) {
			int i = idx1, n = 0, total = 0;
			while (cmd(i).is_drop_kind()) {
				n++;
				total += cmd(i).get_drop_index();
				i = next_command_line(i);
			}
			if (total > 1) {
				// TODO: consider (total > 15) case
				return Result::Replace(n, make_DROP(total));
			}
		}
		if ((cmd1.is_PUSH() && cmd1.get_push_index() <= 1) || cmd1.is_simple_command(0, 1)) {
			vector<int> lines;
			bool ok = true;
			int i = idx1, stack_size = 2;
			while (ok) {
				lines.push_back(i);
				i = next_command_line(i);
				if (!valid(i)) {
					ok = false;
					break;
				}
				Cmd c = cmd(i);
				// DBG(c.without_prefix() << " - " << stack_size);
				if (c.is_PUSH()) {
					if (c.get_push_index() + 1 == stack_size) {
						ok = false;
						break;
					}
					stack_size++;
					continue;
				}
				if (c.is_POP()) {
					if (c.get_pop_index() + 1 == stack_size) {
						ok = false;
						break;
					}
					stack_size--;
					continue;
				}
				if (c.is("BLKPUSH")) {
					if (c.fetch_second_int() + 1 < stack_size) {
						stack_size += c.fetch_first_int();
						continue;
					}
					ok = false;
					break;
				}
				if (c.is_NIP()) {
					if (stack_size == 2) {
						lines.push_back(i);
						break;
					}
					if (stack_size > 2) {
						stack_size--;
						continue;
					}
					ok = false;
					break;
				}
				if (c.is_DROP()) {
					if (stack_size == 1) {
						lines.push_back(i);
						break;
					}
					if (stack_size > 1) {
						stack_size--;
						continue;
					}
					ok = false;
					break;
				}
				if (c.is_simple_command_) {
					if (stack_size <= c.inputs_count_) {
						ok = false;
						break;
					}
					stack_size += c.outputs_count_ - c.inputs_count_;
					continue;
				}
				ok = false;
			}
			if (ok) {
				vector<string> commands;
				stack_size = 2;
				for (size_t ii = 0; ii < lines.size(); ii++) {
					auto c = cmd(lines[ii]);
					if (ii == 0) {
						if (c.is_PUSH())
							if (c.get_push_index() == 1) {
								commands.emplace_back("DROP");
								commands.emplace_back("DUP");
							}
						if (c.is_simple_command(0, 1)) {
							commands.emplace_back("DROP");
							commands.emplace_back(c.without_prefix());
						}
						continue;
					}
					if (ii + 1 == lines.size())
						continue;
					// DBG(c.without_prefix() << " - " << stack_size);
					if (c.is_PUSH()) {
						if (c.get_push_index() + 1 < stack_size) {
							commands.push_back(c.without_prefix());
						} else if (c.get_push_index() + 1 > stack_size) {
							commands.push_back(make_PUSH(c.get_push_index()-1));
						} else {
							solAssert(false, "");
						}
						stack_size++;
						continue;
					}
					if (c.is_POP()) {
						if (c.get_pop_index() + 1 < stack_size) {
							commands.push_back(c.without_prefix());
						} else if (c.get_pop_index() + 1 > stack_size) {
							commands.push_back(make_POP(c.get_pop_index()-1));
						} else {
							solAssert(false, "");
						}
						stack_size--;
						continue;
					}
					if (c.is("BLKPUSH")) {
						solAssert(c.fetch_second_int() + 1 < stack_size, "");
						commands.push_back(c.without_prefix());
						stack_size += c.fetch_first_int();
						continue;
					}
					if (c.is_simple_command_) {
						solAssert(stack_size > c.inputs_count_, "");
						stack_size += c.outputs_count_ - c.inputs_count_;
						commands.push_back(c.without_prefix());
						continue;
					}
					if (c.is_NIP()) {
						solAssert(stack_size > 2, "");
						stack_size--;
						commands.push_back(c.without_prefix());
						continue;
					}
					solAssert(false, "");
				}
				solAssert(stack_size == 2 || stack_size == 1, "");
				return Result{true, static_cast<int>(lines.size()), commands};
			}
		}
		return Result(false);
	}

	Result unsquash_push(const int idx1) const {
		Cmd cmd1 = cmd(idx1);
		if (cmd1.is("PUSH2")) {
			auto [si, sj] = cmd1.get_push2_indexes();
			return Result::Replace(1, make_PUSH(si), make_PUSH(sj + 1));
		}
		return Result(false);
	}

	Result squash_push(const int idx1) const {
		int idx2 = next_command_line(idx1);
		int idx3 = next_command_line(idx2);
		Cmd cmd1 = cmd(idx1);
		Cmd cmd2 = cmd(idx2);
		Cmd cmd3 = cmd(idx3);
		if (cmd1.is_PUSH() && cmd2.is_PUSH() && cmd3.is_PUSH()) {
			const int si = cmd1.get_push_index();
			const int sj = cmd2.get_push_index() - 1 == -1? si : cmd2.get_push_index() - 1;
			const int sk = cmd3.get_push_index() - 2 == -1? si : (
					cmd3.get_push_index() - 2 == -2? sj : cmd3.get_push_index() - 2
			);
			if (si <= 15 && sj <= 15 && sk <= 15) {
				const std::string newOpcode = (boost::format("PUSH3 S%d, S%d, S%d") % si % sj % sk).str();
				return Result::Replace(3, newOpcode);
			}
		}
		if (cmd1.is_PUSH() && cmd2.is_PUSH()) {
			const int si = cmd1.get_push_index();
			const int sj = cmd2.get_push_index() - 1 == -1? si : cmd2.get_push_index() - 1;
			if (si <= 15 && sj <= 15) {
				const std::string newOpcode = (boost::format("PUSH2 S%d, S%d") % si % sj).str();
				return Result::Replace(2, newOpcode);
			}
		}
		return Result(false);
	}

	static string make_DROP(int n) {
		solAssert(n > 0, "");
		if (n == 1) return "DROP";
		if (n == 2) return "DROP2";
		// TODO: use DROPX for n > 15
		return "BLKDROP " + toString(n);
	}

	static string make_PUSH(int n) {
		solAssert(n >= 0, "");
		if (n == 0) return "DUP";
		return "PUSH S" + toString(n);
	}

	static string make_POP(int n) {
		solAssert(n >= 0, "");
		if (n == 0) return "DROP";
		if (n == 1) return "NIP";
		return "POP S" + toString(n);
	}

	static string make_BLKPUSH(int n, int m) {
		solAssert(n > 0, "");
		solAssert(m >= 0 && m <= 15, "");
		if (n == 1) return make_PUSH(m);
		return "BLKPUSH " + toString(n) + ", " + toString(m);
	}

	bool updateLines(int& idx1, const Result& res) {
		deque<int> linesToRemove;
		for (int i = idx1; linesToRemove.size() < size_t(res.remove_); i = next_command_line(i)) {
			linesToRemove.push_front(i);
		}

		if (!res.commands_.empty()) {
			int idx2 = next_command_line(idx1);
			string pfx = Cmd(lines_[idx2]).prefix_;

			if (!linesToRemove.empty()) {
				// We have removed something, so add replacement commands.
				for (auto it = res.commands_.rbegin(); it != res.commands_.rend(); it++)
					insert(linesToRemove.front() + 1, *it, pfx);
			} else {
				// We add only a comment, check if it was not added before.
				solAssert(res.commands_.size() == 1, "");
				string cmd = res.commands_.front();
				if (lines_[idx1] != pfx + cmd) {
					if (lines_[idx1-1] != pfx + cmd) {
						insert(idx1, cmd, pfx);
						idx1++;
					}
				}
			}
		}

		for (int l : linesToRemove) {
			remove(l);
		}

		if (res.continue_) {
			// step back to several commands
			int cnt = 10, i = idx1;
			while (cnt > 0) {
				i--;
				if (!valid(i)) break;
				if (!is_comment_or_empty_line(lines_[i])) cnt--;
				idx1 = i;
			}
			return true;
		}

		return false;
	}

	void optimize(const std::function<Result(int)> &f) {
		int idx1 = 0;
		while (valid(idx1)) {
			Result res = f(idx1);
			if (updateLines(idx1, res)) {
				continue;
			}
			idx1 = next_command_line(idx1);
		}
	}
};

CodeLines optimize_code(const CodeLines& code0) {
	auto code = code0;
	TVMOptimizer optimizer{code.lines};
	optimizer.optimize([&optimizer](int index){ return optimizer.unsquash_push(index);});
	optimizer.optimize([&optimizer](int index){ return optimizer.optimize_at(index);});
	optimizer.optimize([&optimizer](int index){ return optimizer.optimize_at(index);});
	optimizer.optimize([&optimizer](int index){ return optimizer.squash_push(index);});
	code.lines = optimizer.lines_;
	return code;
}

void run_peephole_pass(const string& filename) {
	ifstream file(filename);
	string line;
	CodeLines code;
	while (getline(file, line)) {
		while (!line.empty() && (line.back() == '\n' || line.back() == '\r'))
			line.pop_back();
		code.push(line);
	}

	code = optimize_code(code);

	cout << code.str();
}

} // end solidity::frontend
