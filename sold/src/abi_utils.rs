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

use std::fs::OpenOptions;

use anyhow::{bail, format_err};
use base64::{engine::general_purpose, Engine as _};
use ever_abi::contract::MAX_SUPPORTED_VERSION;
use ever_abi::token::{Cursor, Detokenizer, Tokenizer};
use ever_abi::{add_sign_to_function_call, ParamType, Token, TokenValue};
use ever_block::{
    boc, BuilderData, Cell, Deserializable, HashmapE, HashmapType, IBitstring, Serializable,
    SliceData,
};
use ever_block::{BocReader, Result, StateInit, Status};
use serde_json::json;

fn read_abi(abi_path: &str) -> Result<String> {
    std::fs::read_to_string(abi_path)
        .map_err(|e| format_err!("Invalid abi path \"{}\": {}", abi_path, e))
}

pub fn init_contract(state_init_path: &str, abi_path: &str, static_values: &str) -> Status {
    let abi_json = read_abi(abi_path)?;
    let new_data = ever_abi::encode_storage_fields(&abi_json, Some(static_values))?.into_cell()?;

    let mut state_init: StateInit;
    {
        let state_init_file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(state_init_path)
            .map_err(|e| format_err!("Unable to open {} file: {}", state_init_path, e))?;
        let cell = BocReader::new()
            .read(&mut &state_init_file)?
            .withdraw_single_root()?;
        state_init = StateInit::construct_from_cell(cell)
            .map_err(|e| format_err!("File {} is not valid stateInit: {}", state_init_path, e))?;
        state_init.set_data(new_data);
    }
    let cell_state_init: Cell = state_init.serialize()?;
    let hash = cell_state_init.repr_hash().as_hex_string();
    let new_state_init = boc::write_boc(&cell_state_init)?;

    std::fs::write(state_init_path, new_state_init)?;
    println!(
        r#"{{
    "state_init_hash": "{hash}"
}}
"#
    );

    Ok(())
}
pub fn encode_value(str_abi: &str, params: &str) -> Status {
    let value = serde_json::from_str(params)?;
    let param = serde_json::from_str::<ever_abi::Param>(str_abi)?;
    let token_value = Tokenizer::tokenize_parameter(&param.kind, &value, param.name.as_str())?;
    let abi_version = MAX_SUPPORTED_VERSION;

    let resulting_cell: Cell;

    match &token_value {
        TokenValue::Array(param_type, ref token_values) => {
            let hashmap_e =
                TokenValue::put_array_into_dictionary(param_type, token_values, &abi_version)?;
            resulting_cell = hashmap_e_to_cell(hashmap_e)?;
        }
        TokenValue::Map(key_type, value_type, values) => {
            let hashmap_e =
                TokenValue::map_token_to_hashmap_e(key_type, value_type, values, &abi_version)?;
            resulting_cell = hashmap_e_to_cell(hashmap_e)?;
        }
        TokenValue::Cell(cell) => {
            resulting_cell = cell.clone();
        }
        TokenValue::Bytes(bytes) => {
            resulting_cell = TokenValue::bytes_to_cells(bytes.as_ref(), &abi_version)?
        }
        TokenValue::String(bytes) => {
            resulting_cell = TokenValue::bytes_to_cells(bytes.as_ref(), &abi_version)?
        }
        _ => {
            bail!("Expected map, cell, array, string or bytes types")
        }
    }

    let cell_bytes = ever_block::write_boc(&resulting_cell)?;
    let ser_msg = json!({
        "cell_in_base64": general_purpose::STANDARD.encode(cell_bytes)
    });
    println!("{ser_msg:#}");

    Ok(())
}

pub fn encode_body(abi_path: &str, method: &str, method_args: &str) -> Status {
    let abi = read_abi(abi_path)?;
    let encoded_body =
        ever_abi::encode_function_call(abi.as_str(), method, None, method_args, true, None, None)?
            .into_cell()?;

    let cell_bytes = ever_block::write_boc(&encoded_body)?;
    let ser_msg = json!({
        "body": general_purpose::STANDARD.encode(cell_bytes)
    });

    println!("{ser_msg:#}");

    Ok(())
}

pub fn encode_ext_message(
    sign: &str,
    abi_path: &str,
    time: &str,
    lifetime: &Option<String>,
    address: &str,
    method: &str,
    params: &str,
) -> Status {
    let mut expired_at = 0;

    let mut js_headers = json!({
        "time": time
    });
    if let Some(lifetime_duration) = lifetime {
        let time_in_seconds = time.parse::<u64>()? / 1000;
        let duration = lifetime_duration.parse::<u64>()?;
        expired_at = time_in_seconds + duration;
        js_headers["expire"] = expired_at.into();
    }
    let headers = format!("{js_headers:#}");

    // eprintln!("headers: {}", headers);

    let abi = read_abi(abi_path)?;

    let (msg, data_to_sign) = ever_abi::prepare_function_call_for_sign(
        abi.as_str(),
        method,
        Some(headers.as_str()),
        params,
        Some(address),
    )?;

    // println!("sign: {}", sign);
    // println!("sign.len: {}", sign.len());

    let hex_sign = hex::decode(&sign[0..64])?;
    let x = hex_sign.as_slice();
    let sign_key = ever_block::ed25519_create_private_key(x)
        .map_err(|e| format_err!("Wrong ed25519 private key {}: {}", sign, e))?;

    let signature = sign_key.sign(&data_to_sign);

    let msg = SliceData::load_builder(msg)?;
    let signed_msg = add_sign_to_function_call(
        abi.as_str(),
        &signature,
        Some(&sign_key.verifying_key()),
        msg,
    )?
    .into_cell()?;

    let cell_bytes = ever_block::write_boc(&signed_msg)?;
    let ser_msg = json!({
        "message": general_purpose::STANDARD.encode(cell_bytes),
        "expire": expired_at,
    });

    println!("{ser_msg:#}");

    Ok(())
}

pub fn decode_abi_param(abi_fragment: &str, base64_cell: &str) -> Status {
    let data = general_purpose::STANDARD.decode(base64_cell)?;
    let cell = ever_block::read_single_root_boc(data)?;

    let param = serde_json::from_str::<ever_abi::Param>(abi_fragment)?;
    let slice = match param.kind {
        ParamType::Array(_) => SliceData::load_cell(cell)?,
        ParamType::Map(_, _) => {
            let mut builder = BuilderData::new();
            builder.append_bit_one()?;
            builder.checked_append_reference(cell.clone())?;
            SliceData::load_builder(builder)?
        }
        ParamType::Cell | ParamType::String | ParamType::Bytes => {
            SliceData::load_builder(cell.write_to_new_cell()?)?
        }
        _ => bail!("Only cell, map, bytes, string and array are supported"),
    };

    let abi_version = &MAX_SUPPORTED_VERSION;

    let (token_value, _cursor) = TokenValue::read_from(
        &param.kind,
        Cursor {
            used_bits: 0,
            used_refs: 0,
            slice,
        },
        true,
        abi_version,
        false,
    )?;

    let js_string = Detokenizer::detokenize(&[Token {
        name: param.name,
        value: token_value,
    }])?;
    println!("{}", js_string);

    Ok(())
}

pub fn decode_state_data(abi_path: &str, base64_data_or_path: &str) -> Status {
    let abi = read_abi(abi_path)?;

    let path = std::path::Path::new(base64_data_or_path);
    let data = if path.exists() {
        std::fs::read(base64_data_or_path)?
    } else {
        general_purpose::STANDARD.decode(base64_data_or_path)?
    };
    let cell = ever_block::read_single_root_boc(data)?;

    let fields = ever_abi::decode_storage_fields(abi.as_str(), SliceData::load_cell(cell)?, false)?;
    println!("{}", fields);
    Ok(())
}

pub fn hashmap_e_to_cell(hashmap_e: HashmapE) -> Result<Cell> {
    match hashmap_e.data() {
        None => bail!("Empty hashmap"),
        Some(data) => Ok(data.clone()),
    }
}
