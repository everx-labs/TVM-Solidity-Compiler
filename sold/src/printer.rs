/*
 * Copyright (C) 2019-2023 EverX. All Rights Reserved.
 *
 * Licensed under the SOFTWARE EVALUATION License (the "License"); you may not use
 * this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the  GNU General Public License for more details at: https://www.gnu.org/licenses/gpl-3.0.html
 */

use std::fs::File;
use std::io::Write;

use failure::format_err;
use serde::Serialize;

use ton_types::{Result, Status};

pub fn print_abi_json_canonically(out: &mut File, value: &serde_json::Value) -> Status {
    let root = value.as_object().ok_or_else(|| format_err!("ABI parsing failed"))?;
    writeln!(out, "{{")?;
    writeln!(out, "\t\"ABI version\": {},", root["ABI version"])?;
    if let Some(version) = root.get("version") {
        writeln!(out, "\t\"version\": {},", version)?;
    }
    if let Some(header) = root.get("header") {
        write!(out, "\t\"header\": [")?;
        let array = header.as_array().ok_or_else(|| format_err!("ABI parsing failed"))?;
        for i in 0..array.len() {
            write!(out, "{}", array[i])?;
            if i + 1 != array.len() {
                write!(out, ", ")?;
            }
        }
        writeln!(out, "],")?;
    }

    writeln!(out, "\t\"functions\": [")?;
    print(out, &root["functions"])?;
    writeln!(out, "\t],")?;

    writeln!(out, "\t\"events\": [")?;
    print(out, &root["events"])?;
    writeln!(out, "\t],")?;

    writeln!(out, "\t\"fields\": [")?;
    print_data(out, &root["fields"])?;
    writeln!(out, "\t]")?;

    writeln!(out, "}}")?;
    Ok(())
}

fn print_data(out: &mut File, value: &serde_json::Value) -> Status {
    let json = value.as_array().ok_or_else(|| format_err!("ABI parsing failed"))?;
    for f in 0..json.len() {
        write!(out, "\t\t")?;

        write!(out, "{}", to_string_pretty_no_indent(&json[f])?)?;

        if f + 1 != json.len() {
            write!(out, ",")?;
        }
        writeln!(out)?;
    }
    Ok(())
}

fn print_array(out: &mut File, array: &Vec<serde_json::Value>) -> Status {
    for i in 0..array.len() {
        write!(out, "\t\t\t\t")?;
        write!(out, "{}", to_string_pretty_no_indent(&array[i])?)?;
        if i + 1 == array.len() {
            writeln!(out)?;
        } else {
            writeln!(out, ",")?;
        }
    }
    Ok(())
}

fn print(out: &mut File, value: &serde_json::Value) -> Status {
    let json = value.as_array().ok_or_else(|| format_err!("ABI parsing failed"))?;
    for f in 0..json.len() {
        let function = json[f].as_object().ok_or_else(|| format_err!("ABI parsing failed"))?;
        writeln!(out, "\t\t{{")?;

        writeln!(out, "\t\t\t\"name\": {},", function["name"])?;

        if let Some(id) = function.get("id") {
            writeln!(out, "\t\t\t\"id\": {},", id)?;
        }

        writeln!(out, "\t\t\t\"inputs\": [")?;
        if let Some(inputs) = function.get("inputs") {
            let array = inputs.as_array().ok_or_else(|| format_err!("ABI parsing failed"))?;
            print_array(out, array)?;
        }
        writeln!(out, "\t\t\t],")?;

        writeln!(out, "\t\t\t\"outputs\": [")?;
        if let Some(outputs) = function.get("outputs") {
            let array = outputs.as_array().ok_or_else(|| format_err!("ABI parsing failed"))?;
            print_array(out, array)?;
        }
        writeln!(out, "\t\t\t]")?;

        if f + 1 == json.len() {
            writeln!(out, "\t\t}}")?;
        } else {
            writeln!(out, "\t\t}},")?;
        }
    }
    Ok(())
}

fn to_string_pretty_no_indent(value: &serde_json::Value) -> Result<String> {
    let buf = Vec::new();
    let formatter = serde_json::ser::CompactFormatter;
    let mut ser = serde_json::Serializer::with_formatter(buf, formatter);
    value.serialize(&mut ser)?;
    Ok(String::from_utf8(ser.into_inner())?)
}
