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

use std::fs::File;
use std::io::{Read, Write};
use std::os::raw::{c_char, c_void};
use std::path::Path;

use clap::Parser;
use failure::{bail, format_err};

use ton_block::Serializable;
use ton_types::{BagOfCells, Result, Status};
use ton_utils::parser::{ParseEngine, ParseEngineInput};
use ton_utils::program::Program;

mod libsolc;
mod printer;

// Most of the work of locating an import is implemented in CompilerStack::loadMissingSources().
// This callback receives an already resolved path, and the only thing left to do is to read
// the file at the specified path.
unsafe extern "C" fn read_callback(
    _context: *mut c_void,
    kind: *const c_char,
    data: *const c_char,
    o_contents: *mut *mut c_char,
    o_error: *mut *mut c_char,
) {
    let kind = std::ffi::CStr::from_ptr(kind)
        .to_string_lossy()
        .into_owned();
    if kind != "source" {
        *o_error = make_error(format!("Unknown kind \"{}\"", kind));
        return
    }
    let filename = std::ffi::CStr::from_ptr(data)
        .to_string_lossy()
        .into_owned();
    let mut file = match File::open(&filename) {
        Ok(f) => f,
        Err(e) => {
            *o_error = make_error(format!("Failed to open file: {}", e));
            return
        }
    };
    let mut buf = vec![];
    let size = file.read_to_end(&mut buf).unwrap();
    let ptr = libsolc::solidity_alloc(size as u64);
    std::ptr::copy(buf.as_ptr(), ptr as *mut u8, size);
    *o_contents = ptr;
}

unsafe fn make_error(msg: String) -> *mut c_char {
    let ptr = libsolc::solidity_alloc(msg.len() as u64);
    std::ptr::copy(msg.as_ptr(), ptr as *mut u8, msg.len());
    ptr
}

fn solidity_version() -> String {
    unsafe {
        std::ffi::CStr::from_ptr(libsolc::solidity_version())
            .to_string_lossy()
            .into_owned()
    }
}

fn compile(args: &Args, input: &str) -> Result<serde_json::Value> {
    let include_paths = args.include_path.iter()
        .map(|x| format!("\"{}\"", x)).collect::<Vec<_>>()
        .join(", ");
    let show_function_ids = if args.function_ids {
        ", \"showFunctionIds\""
    } else {
        ""
    };
    let assembly = if args.abi_json || args.ast_json || args.ast_compact_json {
        ""
    } else {
        ", \"assembly\""
    };
    let force_remote_update = args.tvm_refresh_remote;
    let main_contract = args.contract.clone().unwrap_or_default();
    let input = serde_json::ser::to_string(input)?;
    let input = format!(r#"
        {{
            "language": "Solidity",
            "settings": {{
                "includePaths": [ {include_paths} ],
                "forceRemoteUpdate": {force_remote_update},
                "mainContract": "{main_contract}",
                "outputSelection": {{
                    {input}: {{
                        "*": [ "abi"{assembly}{show_function_ids} ],
                        "": [ "ast" ]
                    }}
                }}
            }},
            "sources": {{
                {input}: {{
                    "urls": [ {input} ]
                }}
            }}
        }}
    "#);
    let input_cstring = std::ffi::CString::new(input).expect("Failed to create CString");
    let output = unsafe {
        std::ffi::CStr::from_ptr(libsolc::solidity_compile(
            input_cstring.as_ptr(),
            Some(read_callback),
            std::ptr::null_mut(),
        ))
        .to_string_lossy()
        .into_owned()
    };
    let res = serde_json::from_str(output.as_str())?;
    Ok(res)
}

macro_rules! parse_error {
    () => {
        format_err!("Failed to parse compilation result")
    };
}

fn parse_comp_result(
    res: &serde_json::Value,
    input: &str,
    contract: Option<String>,
    compile: bool,
) -> Result<serde_json::Value> {
    let res = res.as_object().ok_or_else(|| parse_error!())?;

    if let Some(v) = res.get("errors") {
        let entries = v.as_array()
            .ok_or_else(|| parse_error!())?;
        let mut severe = false;
        for entry in entries {
            let entry = entry.as_object()
                .ok_or_else(|| parse_error!())?;
            let severity = entry.get("severity")
                .ok_or_else(|| parse_error!())?
                .as_str()
                .ok_or_else(|| parse_error!())?;
            if severity == "error" {
                severe = true;
            }
            let message = entry.get("humanFormattedMessage")
                .ok_or_else(|| parse_error!())?
                .as_str()
                .ok_or_else(|| parse_error!())?;
            let message = if atty::is(atty::Stream::Stderr) && !cfg!(target_family = "windows") {
                message.to_string()
            } else {
                let bytes = strip_ansi_escapes::strip(message)?;
                String::from_utf8(bytes)?
            };    
            eprint!("{}", message);
        }
        if severe {
            bail!("Compilation failed")
        }
    }

    let all = res
        .get("contracts")
        .ok_or_else(|| parse_error!())?
        .as_object()
        .ok_or_else(|| parse_error!())?
        .get(input)
        .ok_or_else(|| parse_error!())?
        .as_object()
        .ok_or_else(|| parse_error!())?;

    if let Some(ref contract) = contract {
        if !all.contains_key(contract) {
            Err(format_err!("Source file doesn't contain the desired contract \"{}\"", contract))
        } else {
            Ok(all.get(contract).unwrap().clone())
        }
    } else {
        let mut iter =
            all.iter().filter(|(_, v)| {
                if !compile {
                    true
                } else if let Some(v) = v.as_object() {
                    v.get("assembly").is_some()
                } else {
                    false
                }
            });
        let qualification = if compile { "deployable " } else { "" };
        let entry = iter.next();
        if let Some(entry) = entry {
            if iter.next().is_some() {
                Err(format_err!("Source file contains at least two {}contracts. Consider adding the option --contract in compiler command line to select the desired contract", qualification))
            } else {
                Ok(entry.1.clone())
            }
        } else {
            Err(format_err!("Source file contains no {}contracts", qualification))
        }
    }
}

static STDLIB: &[u8] = include_bytes!("../../lib/stdlib_sol.tvm");

fn build(args: Args) -> Status {
    let output_dir = args.output_dir.clone().unwrap_or_else(|| String::from("."));
    let output_path = Path::new(&output_dir);
    if !output_path.exists() {
        bail!("Output directory doesn't exist")
    }

    if let Some(ref output_prefix) = args.output_prefix {
        if output_prefix.contains(std::path::is_separator) {
            bail!("Invalid output prefix \"{}\". Use option -O to set output directory", output_prefix);
        }
    }

    let input_canonical = dunce::canonicalize(Path::new(&args.input))?;
    let input = input_canonical.as_os_str().to_str()
        .ok_or_else(|| format_err!("Failed to get canonical path"))?;

    let input = if cfg!(target_family = "windows") {
        // conform to boost::filesystem::canonical()
        // note the first slash: C:/Users\Dummy\work\tests\Test.sol
        input.replacen('\\', "/", 1)
    } else {
        input.to_owned()
    };

    let res = compile(&args, &input)?;
    let out = parse_comp_result(
        &res,
        &input,
        args.contract,
        !(args.abi_json || args.ast_json || args.ast_compact_json)
    )?;

    if args.function_ids {
        println!("{}", serde_json::to_string_pretty(&out["functionIds"])?);
        return Ok(())
    }

    let input_file_stem = input_canonical.file_stem()
        .ok_or_else(|| format_err!("Failed to extract file stem"))?
        .to_str()
        .ok_or_else(|| format_err!("Failed to get file stem"))?
        .to_string();
    let output_prefix = args.output_prefix.unwrap_or(input_file_stem);
    let output_tvc = format!("{}.tvc", output_prefix);

    if args.ast_json || args.ast_compact_json {
        let all = res.as_object()
            .ok_or_else(|| parse_error!())?
            .get("sources")
            .ok_or_else(|| parse_error!())?
            .as_object()
            .ok_or_else(|| parse_error!())?;

        let mut array = vec!();
        for (_, val) in all {
            let ast = val
                .as_object()
                .ok_or_else(|| parse_error!())?
                .get("ast")
                .ok_or_else(|| parse_error!())?;
            array.push(ast.clone());
        }

        let ast = serde_json::Value::Array(array);
        let ast_file_name = format!("{}.ast.json", output_prefix);
        let mut ast_file = File::create(output_path.join(&ast_file_name))?;

        if args.ast_json {
            serde_json::to_writer_pretty(&mut ast_file, &ast)?;
        } else {
            serde_json::to_writer(&mut ast_file, &ast)?;
        }
        writeln!(ast_file)?;
        return Ok(())
    }

    let abi = &out["abi"];
    let abi_file_name = format!("{}.abi.json", output_prefix);
    let mut abi_file = File::create(output_path.join(&abi_file_name))?;
    printer::print_abi_json_canonically(&mut abi_file, abi)?;
    if args.abi_json {
        return Ok(())
    }

    let assembly = out["assembly"]
        .as_str()
        .ok_or_else(|| parse_error!())?
        .to_owned();
    let assembly_file_name = format!("{}.code", output_prefix);
    let mut assembly_file = File::create(output_path.join(&assembly_file_name))?;
    assembly_file.write_all(assembly.as_bytes())?;

    let mut inputs = Vec::new();
    if let Some(lib) = args.lib {
        let lib_file = File::open(&lib)?;
        inputs.push(ParseEngineInput { buf: Box::new(lib_file), name: lib });
    } else {
        inputs.push(ParseEngineInput { buf: Box::new(STDLIB), name: String::from("stdlib_sol.tvm") });
    }
    inputs.push(ParseEngineInput { buf: Box::new(assembly.as_bytes()), name: format!("{}/{}", output_dir, assembly_file_name) });

    let mut prog = Program::new(ParseEngine::new_generic(inputs, Some(format!("{}", abi)))?);


    let output_filename = if output_dir == "." {
        output_tvc
    } else {
        format!("{}/{}", output_dir, output_tvc)
    };

    prog.compile_to_file_ex(
        -1,
        Some(&output_filename),
        None,
        false,
    )?;

    let mut dbg_file = File::create(format!("{}/{}.debug.json", output_dir, output_prefix))?;
    serde_json::to_writer_pretty(&mut dbg_file, &prog.dbgmap)?;
    writeln!(dbg_file)?;

    if let Some(params_data) = args.init {
        let mut state = ton_utils::program::load_from_file(&output_filename)?;
        let new_data = ton_abi::json_abi::update_contract_data(
            &serde_json::to_string(abi)?,
            &params_data,
            state.data.clone().unwrap_or_default().into(),
        )?;
        state.set_data(new_data.into_cell());

        let root_cell = state.write_to_new_cell()?.into_cell()?;
        let mut buffer = vec![];
        BagOfCells::with_root(&root_cell).write_to(&mut buffer, false)?;

        let mut file = std::fs::File::create(&output_filename)?;
        file.write_all(&buffer)?;
    }

    Ok(())
}

use once_cell::sync::OnceCell;
static VERSION: OnceCell<String> = OnceCell::new();

#[derive(Parser, Debug)]
#[clap(author, about, long_about = None)]
#[clap(version = VERSION.get().unwrap().as_str())]
struct Args {
    /// Source file name
    #[clap(value_parser)]
    input: String,
    /// Contract to build if sources define more than one contract
    #[clap(short, long, value_parser)]
    contract: Option<String>,
    /// Output directory (by default, current directory is used)
    #[clap(short('O'), long, value_parser)]
    output_dir: Option<String>,
    /// Output prefix (by default, input file stem is used as prefix)
    #[clap(short('P'), long, value_parser)]
    output_prefix: Option<String>,
    /// Include additional path to search for imports
    #[clap(short('I'), long, value_parser)]
    include_path: Vec<String>,
    /// Library to use instead of default
    #[clap(short('L'), long, value_parser)]
    lib: Option<String>,
    /// Execute constructor with provided parameters
    #[clap(short('p'), long, value_parser, hide = true)] // deprecated
    ctor_params: Option<String>,
    /// Set newly generated keypair
    #[clap(long, value_parser)]
    init: Option<String>,
    /// Print name and id for each public function
    #[clap(long, value_parser)]
    function_ids: bool,
    /// Get AST of all source files in JSON format
    #[clap(long, value_parser, conflicts_with = "ast-compact-json")]
    ast_json: bool,
    /// Get AST of all source files in compact JSON format
    #[clap(long, value_parser, conflicts_with = "ast-json")]
    ast_compact_json: bool,
    /// Get ABI without actually compiling
    #[clap(long, value_parser)]
    abi_json: bool,
    /// Force download and rewrite remote import files
    #[clap(long, value_parser)]
    tvm_refresh_remote: bool,
}

fn main() {
    VERSION.set(solidity_version()).unwrap();

    let args = Args::parse();
    if let Err(e) = build(args) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
