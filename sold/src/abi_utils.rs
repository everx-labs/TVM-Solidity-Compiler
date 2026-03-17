/*
 * Copyright (C) 2025-2026 EverX. All Rights Reserved.
 *
 * Licensed under the SOFTWARE EVALUATION License (the "License"); you may not use
 * this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */

use anyhow::Result;
use anyhow::{bail, format_err};
use base64::{Engine as _, engine::general_purpose};
use ed25519_dalek::SecretKey;
use ed25519_dalek::SigningKey;
use serde_json::json;
use tsol_asm::Status;
use tycho_types::abi::{
    AbiType, AbiValue, AbiVersion, Contract, ContractInitData, NamedAbiType, NamedAbiValue,
    SerializeAbiValueParams, SerializeAbiValues,
};
use tycho_types::boc::Boc;
use tycho_types::cell::Store;
use tycho_types::models::{SignatureContext, StateInit, StdAddr, StdAddrFormat};
use tycho_types::prelude::{Cell, CellBuilder, CellFamily};

fn read_abi(abi_path: &str) -> Result<Contract> {
    let abi_json = std::fs::read_to_string(abi_path)
        .map_err(|e| format_err!("Invalid abi path \"{}\": {}", abi_path, e))?;
    Ok(serde_json::from_str::<Contract>(&abi_json)?)
}

pub fn load_named_abi_values(types: &[NamedAbiType], values: &str) -> Result<Vec<NamedAbiValue>> {
    NamedAbiValue::tuple_from_json_str(values, types)
        .map_err(|e| format_err!("Failed to decode \"{}\": {}", values, e))
}

pub fn init_contract(state_init_path: &str, abi_path: &str, static_values: &str) -> Status {
    // Create new `data` field of `StateInit`
    let contract = read_abi(abi_path)?;
    let init_fields: Vec<NamedAbiType> = contract
        .fields
        .iter()
        .filter(|&field| match &contract.init_data {
            ContractInitData::PlainFields(init_fields) => init_fields.contains(&field.name),
            ContractInitData::Dict(_) => {
                panic!()
            }
        })
        .cloned()
        .collect();
    let named_abi_values = load_named_abi_values(&init_fields, static_values)?;
    let new_data = contract.encode_init_data(None, &named_abi_values)?;

    // Update `data` field of `StateInit` structure
    let state_init = std::fs::read(state_init_path)?;
    let cell = Boc::decode(state_init)?;
    let mut state_init = cell.parse::<StateInit>()?;
    state_init.data = Some(new_data);

    // Build new StateInit
    let mut builder = CellBuilder::new();
    state_init.store_into(&mut builder, Cell::empty_context())?;
    let cell_state_init: Cell = builder.build()?;

    // Write new StateInit to file
    let new_state_init = Boc::encode(&cell_state_init);
    std::fs::write(state_init_path, new_state_init)?;

    let hash = hex::encode(cell_state_init.repr_hash().0);
    println!(
        r#"{{
    "state_init_hash": "{hash}"
}}
"#
    );

    Ok(())
}
pub fn encode_value(str_abi: &str, params: &str) -> Status {
    let param = serde_json::from_str::<NamedAbiType>(str_abi)?;

    let abi_value = AbiValue::from_json_str(params, &param.ty)?;

    let mut resulting_cell =
        AbiValue::tuple_to_cell(std::slice::from_ref(&abi_value), AbiVersion::LAST_VERSION)?;

    match abi_value {
        AbiValue::Array(_, _)
        | AbiValue::Map(_, _, _)
        | AbiValue::Bytes(_)
        | AbiValue::String(_) => {
            resulting_cell = resulting_cell.reference_cloned(0).unwrap();
        }
        _ => {}
    };

    let cell_bytes = Boc::encode(resulting_cell);
    let ser_msg = json!({
        "cell_in_base64": general_purpose::STANDARD.encode(cell_bytes)
    });
    println!("{ser_msg:#}");

    Ok(())
}

pub fn encode_body(abi_path: &str, method: &str, method_args: &str) -> Status {
    let contract = read_abi(abi_path)?;
    let function = contract
        .functions
        .get(method)
        .ok_or_else(|| format_err!("Method {} not found", method))?;

    let named_abi_values = load_named_abi_values(&function.inputs, method_args)?;

    let body = function.encode_internal_input(&named_abi_values)?.build()?;
    let cell_bytes = Boc::encode(body);

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
    let expired_at = if let Some(lifetime_duration) = lifetime {
        let time_in_seconds = time.parse::<u64>()? / 1000;
        let duration = lifetime_duration.parse::<u64>()?;
        time_in_seconds + duration
    } else {
        0
    };

    let abi = read_abi(abi_path)?;
    let function = abi
        .functions
        .get(method)
        .ok_or_else(|| format_err!("Method {} not found", method))?;
    let named_abi_params = load_named_abi_values(&function.inputs, params)?;

    let (addr, _) = StdAddr::from_str_ext(address, StdAddrFormat::any())?;
    let msg_time = time.parse::<u64>()?;
    let mut external_input = function
        .encode_external(&named_abi_params)
        .with_time(msg_time);
    if expired_at != 0 {
        external_input = external_input.with_expire_at(expired_at as u32);
    }
    let unsigned_ext_msg = external_input.build_message(&addr)?;

    let hex_sign = hex::decode(&sign[0..64])?;
    let x = SecretKey::try_from(hex_sign).unwrap();
    let sign = SigningKey::from_bytes(&x);

    let ext_message = unsigned_ext_msg.sign(&sign, SignatureContext::empty())?;
    let byte_body = Boc::encode(ext_message.body.1);
    let ser_msg = json!({
        "message": general_purpose::STANDARD.encode(byte_body),
        "expire": expired_at,
    });

    println!("{ser_msg:#}");

    Ok(())
}
pub fn decode_abi_param(abi_fragment: &str, base64_cell: &str) -> Status {
    let cell = Boc::decode_base64(base64_cell)?;
    let param = serde_json::from_str::<NamedAbiType>(abi_fragment)?;
    let cell2: Cell;
    let mut slice = match param.ty {
        AbiType::Array(_) => cell.as_slice()?,
        AbiType::Map(_, _) => {
            let mut builder = CellBuilder::new();
            builder.store_bit_one()?;
            builder.store_reference(cell.clone())?;
            cell2 = builder.build()?;
            cell2.as_slice()?
        }
        AbiType::Cell | AbiType::String | AbiType::Bytes => {
            let mut builder = CellBuilder::new();
            builder.store_reference(cell.clone())?;
            cell2 = builder.build()?;
            cell2.as_slice()?
        }
        _ => bail!("Only cell, map, bytes, string and array are supported"),
    };

    let abi_value = AbiValue::load(&param.ty, AbiVersion::LAST_VERSION, &mut slice)?;
    let output = serde_json::to_string(&SerializeAbiValues::with_params(
        &[NamedAbiValue {
            name: param.name.clone(),
            value: abi_value.clone(),
        }],
        SerializeAbiValueParams::default(),
    ))?;
    println!("{}", output);

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
    let cell_data = Boc::decode(data)?;

    let fields = abi.decode_fields(cell_data.as_slice()?)?;
    let output = serde_json::to_string(&SerializeAbiValues::with_params(
        &fields,
        SerializeAbiValueParams::default(),
    ))?;
    println!("{}", output);

    Ok(())
}
