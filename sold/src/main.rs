/*
 * Copyright 2022 TON DEV SOLUTIONS LTD.
 *
 * Licensed under the SOFTWARE EVALUATION License (the "License"); you may not use
 * this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific TON DEV software governing permissions and
 * limitations under the License.
 */

use clap::Parser;
use sold_lib::{Args, VERSION, ERROR_MSG_NO_OUTPUT, build, solidity_version};

mod libsolc;

fn main() {
    VERSION.set(solidity_version()).unwrap();

    let args = Args::parse();
    if let Err(e) = build(args) {
        eprintln!("{}", e);
        if e.to_string() != ERROR_MSG_NO_OUTPUT {
            std::process::exit(1);
        }
    }
}
