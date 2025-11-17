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
use std::ffi::CString;
use std::fs;
use std::fs::File;
use std::io::Write;

use anyhow::{bail, format_err};
use ever_assembler::{DbgInfo, Engine, Units};
use ever_block::Status;

use crate::libsolc;

static STDLIB: &str = include_str!("../../lib/stdlib.code");

pub fn solidity_version() -> String {
    unsafe {
        std::ffi::CStr::from_ptr(libsolc::solidity_version())
            .to_string_lossy()
            .into_owned()
    }
}

pub fn run_compile(rust_strings: Vec<String>) -> Status {
    // 1. Convert each Rust String to a CString and get raw pointers
    let c_strings: Vec<CString> = rust_strings
        .into_iter()
        .map(|s| CString::new(s).expect("String should not contain null bytes"))
        .collect();

    let raw_pointers: Vec<*const i8> = c_strings.iter().map(|cs| cs.as_ptr()).collect();

    let result = unsafe {
        libsolc::solc_main(
            c_strings.len() as std::os::raw::c_int,
            raw_pointers.as_ptr(),
        )
    };
    if result != 0 {
        // solc_main has already printed an error message => return error with empty message
        bail!("");
    }

    let output_prefix = {
        let name = unsafe { std::ffi::CStr::from_ptr(libsolc::get_out_stem()) };
        &name.to_string_lossy().into_owned()
    };
    let output_dir = {
        let name = unsafe { std::ffi::CStr::from_ptr(libsolc::get_out_dir()) };
        &name.to_string_lossy().into_owned()
    };

    let do_generate_tvc = unsafe { libsolc::do_generate_tvc() };

    if do_generate_tvc {
        let assembly_file_name = format!("{output_dir}/{output_prefix}.code");
        let assembly = fs::read_to_string(&assembly_file_name)?;
        let output_tvc = format!("{output_prefix}.tvc");

        let mut inputs = Vec::new();
        // Another lib can be added here instead of stdlib
        inputs.push((STDLIB.to_string(), String::from("stdlib.code")));
        inputs.push((assembly, format!("{output_dir}/{assembly_file_name}")));

        let mut engine = Engine::new("");
        let mut units = Units::new();
        for (input, filename) in inputs {
            engine.reset(filename);
            units = engine
                .compile_toplevel(&input)
                .map_err(|e| format_err!("{}", e))?;
        }
        let (builder_data, dbg_node) = units.finalize();
        let output = builder_data.into_cell()?;
        let dbg_map = DbgInfo::from(output.clone(), dbg_node);

        let output_filename = if output_dir == "." {
            output_tvc
        } else {
            format!("{output_dir}/{output_tvc}")
        };

        let bytes = ever_block::write_boc(&output)?;
        let mut file = File::create(output_filename)?;
        file.write_all(&bytes)?;

        let mut dbg_file = File::create(format!("{output_dir}/{output_prefix}.debug.json"))?;
        serde_json::to_writer_pretty(&mut dbg_file, &dbg_map)?;
        writeln!(dbg_file)?;
    }

    Ok(())
}
