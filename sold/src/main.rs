/*
 * Copyright (C) 2022-2025 EverX. All Rights Reserved.
 *
 * Licensed under the SOFTWARE EVALUATION License (the "License"); you may not use
 * this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */

mod abi_utils;
mod agrs;
mod libsolc;
mod solc_run;

use clap::{CommandFactory, Parser};
use ever_block::Status;

use crate::abi_utils::{
    decode_abi_param, decode_state_data, encode_body, encode_ext_message, encode_value,
    init_contract,
};
use crate::agrs::{Commands, DecodeSubcommands, EncodeSubcommands, SoldArgs, VERSION};
use crate::solc_run::{run_compile, solidity_version};

pub fn run_subcommand(args: SoldArgs) -> Status {
    match &args.subcommand {
        Commands::Init(init_args) => init_contract(
            init_args.input.as_str(),
            init_args.abi.as_str(),
            init_args.static_values.as_str(),
        ),
        Commands::Encode(encode_args) => match encode_args {
            EncodeSubcommands::Cell(encode_args) => {
                encode_value(encode_args.abi.as_str(), encode_args.input.as_str())
            }
            EncodeSubcommands::Body(encode_body_args) => encode_body(
                encode_body_args.abi.as_str(),
                encode_body_args.method.as_str(),
                encode_body_args.input.as_str(),
            ),
            EncodeSubcommands::Message(encode_message_args) => encode_ext_message(
                encode_message_args.sign.as_str(),
                encode_message_args.abi.as_str(),
                encode_message_args.time.as_str(),
                &encode_message_args.lifetime,
                encode_message_args.address.as_str(),
                encode_message_args.method.as_str(),
                encode_message_args.params.as_str(),
            ),
        },
        Commands::Decode(decode_subcommand) => match decode_subcommand {
            DecodeSubcommands::AbiParam(abi_param_args) => {
                decode_abi_param(abi_param_args.abi.as_str(), abi_param_args.input.as_str())
            }
            DecodeSubcommands::StateData(decode_state_data_args) => decode_state_data(
                decode_state_data_args.abi.as_str(),
                decode_state_data_args.input.as_str(),
            ),
        },
    }
}

fn main() {
    VERSION.set(solidity_version()).unwrap();

    let args: Vec<String> = std::env::args().collect();
    let arg1 = args[1].clone();
    let result = if args.len() == 1
        || (args.len() >= 2 && args[1] != "init" && args[1] != "encode" && args[1] != "decode")
    {
        let exit_code = run_compile(args);
        if arg1 == "--help" || arg1 == "-h" {
            println!();
            SoldArgs::command().print_help().unwrap();
        }
        exit_code
    } else {
        let sold_commands = SoldArgs::parse();
        run_subcommand(sold_commands)
    };

    if let Err(e) = result {
        let error_text = e.to_string();
        if !error_text.is_empty() {
            eprintln!("{error_text}");
        }
        std::process::exit(1);
    }
}
