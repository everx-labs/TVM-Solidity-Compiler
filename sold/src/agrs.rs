/*
 * Copyright (C) 2025 EverX. All Rights Reserved.
 *
 * Licensed under the SOFTWARE EVALUATION License (the "License"); you may not use
 * this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */

use clap::{Args, Parser, Subcommand};
use once_cell::sync::OnceCell;

pub static VERSION: OnceCell<String> = OnceCell::new();

#[derive(Parser, Debug)]
#[clap(author, about = "sold, the TVM Solidity commandline driver", long_about = None)]
#[clap(arg_required_else_help = true)]
#[clap(version = VERSION.get().unwrap().as_str())]
pub struct SoldArgs {
    #[command(subcommand)]
    pub subcommand: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Initialize the data of the stateInit
    Init(InitArgs),

    /// Encode body, message, etc.
    #[command(subcommand)]
    Encode(EncodeSubcommands),

    /// Decode contract state, message, etc.
    #[command(subcommand)]
    Decode(DecodeSubcommands),
}

#[derive(Args, Debug)]
pub struct InitArgs {
    /// Path to the boc file containing contract's StateInit
    #[clap(value_parser)]
    pub input: String,
    /// Initial data in json format
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub static_values: String,
    /// Path to the abi file
    #[clap(long, value_parser, value_names = &["PATH"])]
    pub abi: String,
}

#[derive(Subcommand, Debug)]
pub enum EncodeSubcommands {
    /// Encode value to cell. Supported a not empty array, map, cell, bytes and string.
    Cell(EncodeCellArgs),
    /// Encode the message body for an internal message.
    Body(EncodeBodyArgs),
    /// Encode the external message.
    Message(EncodeMessageArgs),
}

#[derive(Args, Debug)]
pub struct EncodeCellArgs {
    /// Abi for the value
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub abi: String,
    /// The value
    #[clap(value_parser)]
    pub input: String,
}

#[derive(Args, Debug)]
pub struct EncodeBodyArgs {
    /// Abi for value
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub abi: String,
    /// The function name
    #[clap(value_parser)]
    pub method: String,
    /// The value in JSON format
    #[clap(value_parser)]
    pub input: String,
}

#[derive(Args, Debug)]
pub struct EncodeMessageArgs {
    /// Signature
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub sign: String,
    /// Path to the abi file
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub abi: String,
    /// Timestamp for the message
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub time: String,
    /// Message lifetime
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub lifetime: Option<String>,
    /// Destination address
    #[clap(value_parser)]
    pub address: String,
    /// Function name
    #[clap(value_parser)]
    pub method: String,
    /// Function arguments in JSON format
    #[clap(value_parser)]
    pub params: String,
}

#[derive(Subcommand, Debug)]
pub enum DecodeSubcommands {
    /// Decode value to cell. Supported a not empty array, map, cell, bytes and string.
    AbiParam(DecodeAbiParamArgs),
    /// Decode all contract's state variables
    StateData(DecodeStateDataArgs),
}

#[derive(Args, Debug)]
pub struct DecodeAbiParamArgs {
    /// Abi for value
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub abi: String,
    /// The value
    #[clap(value_parser)]
    pub input: String,
}

#[derive(Args, Debug)]
pub struct DecodeStateDataArgs {
    /// Abi for value
    #[clap(long, value_parser, value_names = &["JSON"])]
    pub abi: String,
    /// base64 state's data or path to the file
    #[clap(value_parser)]
    pub input: String,
}
