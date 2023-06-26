use std::fmt;
use std::fs::File;
use std::io::Write;
use std::os::raw::{c_char, c_void};
use std::path::Path;

use clap::{ValueEnum, Parser};
use failure::{bail, format_err};
use serde::Deserialize;

use ton_block::Serializable;
use ton_types::{Result, SliceData, Status};
use ton_utils::parser::{ParseEngine, ParseEngineInput};
use ton_utils::program::Program;

mod libsolc;
mod printer;

unsafe extern "C" fn read_callback(
    context: *mut c_void,
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
    let mut success = 0i32;
    let contents_ptr = libsolc::file_reader_read(context, data, &mut success);
    if success == 0 {
        *o_error = contents_ptr;
    } else {
        *o_contents = contents_ptr;
    }
}

unsafe fn make_error(msg: String) -> *mut c_char {
    let ptr = libsolc::solidity_alloc(msg.len() as u64);
    std::ptr::copy(msg.as_ptr(), ptr as *mut u8, msg.len());
    ptr
}

pub fn solidity_version() -> String {
    unsafe {
        std::ffi::CStr::from_ptr(libsolc::solidity_version())
            .to_string_lossy()
            .into_owned()
    }
}

fn to_cstr(s: &str) -> Result<std::ffi::CString> {
    std::ffi::CString::new(s).map_err(|e| format_err!("Failed to convert: {}", e))
}

fn compile(args: &Args, input: &str, remappings: Vec<String>) -> Result<(String, serde_json::Value)> {
    let file_reader = unsafe {
        let file_reader = libsolc::file_reader_new();
        if let Some(base_path) = args.base_path.clone() {
            libsolc::file_reader_set_base_path(file_reader, to_cstr(&base_path)?.as_ptr());
        }
        for include_path in args.include_path.clone() {
            libsolc::file_reader_add_include_path(file_reader, to_cstr(&include_path)?.as_ptr());
        }
        for allowed_path in args.allowed_path.clone() {
            libsolc::file_reader_allow_directory(file_reader, to_cstr(&allowed_path)?.as_ptr());
        }
        let input_content = std::fs::read_to_string(input)?;
        libsolc::file_reader_add_or_update_file(
            file_reader,
            to_cstr(input)?.as_ptr(),
            to_cstr(&input_content)?.as_ptr()
        );
        if let Some(path) = dunce::canonicalize(Path::new(&input))?.parent() {
            let path = path.to_str().ok_or_else(|| format_err!("Failed to convert path to string"))?;
            libsolc::file_reader_allow_directory(file_reader, to_cstr(path)?.as_ptr());
        }
        file_reader
    };
    let source_unit_name = {
        let name = unsafe {
            std::ffi::CStr::from_ptr(libsolc::file_reader_source_unit_name(
                file_reader, to_cstr(input)?.as_ptr()))
        };
        &name.to_string_lossy().into_owned()
    };
    let show_function_ids = if args.function_ids {
        ", \"showFunctionIds\""
    } else {
        ""
    };
    let show_private_function_ids = if args.private_function_ids {
        ", \"showPrivateFunctionIds\""
    } else {
        ""
    };
    let assembly = if args.abi_json || args.ast_compact_json || args.userdoc || args.devdoc {
        ""
    } else {
        ", \"assembly\""
    };
    let doc = if args.userdoc || args.devdoc {
        ", \"userdoc\", \"devdoc\""
    } else {
        ""
    };
    let tvm_version = match args.tvm_version {
        None => {
            "".to_string()
        }
        Some(version) => {
            format!(r#""tvmVersion": "{}","#, version)
        }
    };
    let main_contract = args.contract.clone().unwrap_or_default();
    let remappings = remappings_to_json_string(remappings);
    let input_json = format!(r#"
        {{
            "language": "Solidity",
            "settings": {{
                {tvm_version}
                "mainContract": "{main_contract}",
                "remappings": {remappings},
                "outputSelection": {{
                    "{source_unit_name}": {{
                        "*": [ "abi"{assembly}{show_function_ids}{show_private_function_ids}{doc} ],
                        "": [ "ast" ]
                    }}
                }}
            }},
            "sources": {{
                "{source_unit_name}": {{
                    "urls": [ "{source_unit_name}" ]
                }}
            }}
        }}
    "#);
    let output = unsafe {
        std::ffi::CStr::from_ptr(libsolc::solidity_compile(
            to_cstr(&input_json)?.as_ptr(),
            Some(read_callback),
            file_reader,
        ))
        .to_string_lossy()
        .into_owned()
    };
    let mut de = serde_json::Deserializer::from_str(&output);
    de.disable_recursion_limit(); // ast json part might be considerably nested
    let res = serde_json::Value::deserialize(&mut de)?;
    Ok((source_unit_name.clone(), res))
}

fn remappings_to_json_string(remappings: Vec<String>) -> String {
    let mut out = String::from("[ ");
    let len = remappings.len();
    for (i, r) in remappings.iter().enumerate() {
        out += &format!("\"{}\"", r);
        if i != len - 1 {
            out += ", ";
        }
    }
    out += " ]";
    out
}

macro_rules! parse_error {
    () => {
        format_err!("Failed to parse compilation result")
    };
}

pub static ERROR_MSG_NO_OUTPUT: &str = "Compiler run successful, no output requested.";

fn parse_comp_result(
    res: &serde_json::Value,
    source_unit_name: &str,
    contract: Option<String>,
    compile: bool,
) -> Result<serde_json::Value> {
    let res = res.as_object().ok_or_else(|| parse_error!())?;
    // println!("{}", serde_json::to_string_pretty(&res)?);

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
        .get(source_unit_name)
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
                    let assembly = v.get("assembly");
                    if let Some(assembly) = assembly {
                        !assembly.is_null()
                    } else {
                        false
                    }
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
            Err(format_err!("{}", ERROR_MSG_NO_OUTPUT))
        }
    }
}

static STDLIB: &[u8] = include_bytes!("../../lib/stdlib_sol.tvm");

fn parse_positional_args(args: Vec<String>) -> Result<(String, Vec<String>)> {
    let mut input = None;
    let mut remappings = vec!();
    for arg in args {
        if arg.contains('=') {
            remappings.push(arg);
        } else {
            if input.is_some() {
                bail!("Two or more inputs are given")
            }
            input = Some(arg)
        }
    }
    if let Some(input) = input {
        Ok((input, remappings))
    } else {
        bail!("No input files are given")
    }
}

pub fn build(args: Args) -> Status {
    if !args.include_path.is_empty() && args.base_path.is_none() {
        bail!("--include-path option requires a non-empty base path")
    }
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

    let (input, remappings) = parse_positional_args(args.input.clone())?;
    let input_canonical = dunce::canonicalize(Path::new(&input))?;

    let res = compile(&args, &input, remappings)?;
    let out = parse_comp_result(
        &res.1,
        &res.0,
        args.contract,
        !(args.abi_json || args.ast_compact_json || args.userdoc || args.devdoc )
    )?;

    if args.function_ids {
        println!("{}", serde_json::to_string_pretty(&out["functionIds"])?);
        return Ok(())
    }

    if args.private_function_ids {
        println!("{}", serde_json::to_string_pretty(&out["privateFunctionIds"])?);
        return Ok(())
    }

    let input_file_stem = input_canonical.file_stem()
        .ok_or_else(|| format_err!("Failed to extract file stem"))?
        .to_str()
        .ok_or_else(|| format_err!("Failed to get file stem"))?
        .to_string();
    let output_prefix = args.output_prefix.unwrap_or(input_file_stem);
    let output_tvc = format!("{}.tvc", output_prefix);

    if args.userdoc || args.devdoc {
        if args.devdoc {
            println!("Developer Documentation");
            println!("{}", serde_json::to_string_pretty(&out["devdoc"])?);
        }
        if args.userdoc {
            println!("User Documentation");
            println!("{}", serde_json::to_string_pretty(&out["userdoc"])?);
        }
        return Ok(())
    }

    if args.ast_compact_json {
        let all = res.1.as_object()
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
        assert_eq!(array.len(), 1);
        println!("{}", serde_json::to_string(&array[0])?);
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

    let mut prog = Program::new(ParseEngine::new_generic(inputs, Some(format!("{}", abi)))?)?;


    let output_filename = if output_dir == "." {
        output_tvc
    } else {
        format!("{}/{}", output_dir, output_tvc)
    };

    prog.set_silent(args.silent);
    prog.set_print_code(args.print_code);

    prog.compile_to_file_ex(
        -1,
        Some(&output_filename),
        None,
    )?;
    if !args.print_code {
        let mut dbg_file = File::create(format!("{}/{}.debug.json", output_dir, output_prefix))?;
        serde_json::to_writer_pretty(&mut dbg_file, &prog.dbgmap)?;
        writeln!(dbg_file)?;
    }

    if let Some(params_data) = args.init {
        let mut state = ton_utils::program::load_from_file(&output_filename)?;
        let new_data = ton_abi::json_abi::update_contract_data(
            &serde_json::to_string(abi)?,
            &params_data,
            SliceData::load_cell(state.data.clone().unwrap_or_default())?,
        )?;
        state.set_data(new_data.into_cell());

        let root_cell = state.write_to_new_cell()?.into_cell()?;
        let buffer = ton_types::write_boc(&root_cell)?;

        let mut file = File::create(&output_filename)?;
        file.write_all(&buffer)?;
    }

    Ok(())
}

use once_cell::sync::OnceCell;
pub static VERSION: OnceCell<String> = OnceCell::new();

#[derive(Copy, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum TvmVersion {
    Ton,
    Ever,
    Gosh,
}

impl fmt::Display for TvmVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TvmVersion::Ton => write!(f, "ton"),
            TvmVersion::Ever => write!(f, "ever"),
            TvmVersion::Gosh => write!(f, "gosh"),
        }
    }
}

#[derive(Parser, Debug)]
#[clap(author, about = "sold, the TVM Solidity commandline driver", long_about = None)]
#[clap(arg_required_else_help = true)]
#[clap(version = VERSION.get().unwrap().as_str())]
pub struct Args {
    /// Source file name or remappings in the form of context:prefix=target
    #[clap(value_parser)]
    pub input: Vec<String>,

    // Input Options:
    /// Contract to build if sources define more than one contract
    #[clap(short, long, value_parser, value_names = &["NAME"])]
    pub contract: Option<String>,
    /// Use the given path as the root of the source tree instead of the root of the filesystem
    #[clap(long, value_parser, value_names = &["PATH"])]
    pub base_path: Option<String>,
    /// Make an additional source directory available to the default import callback.
    /// Use this option if you want to import contracts whose location is not fixed in relation
    /// to your main source tree, e.g. third-party libraries installed using a package manager.
    /// Can be used multiple times.
    /// Can only be used if base path has a non-empty value.
    #[clap(short('i'), long, value_parser, value_names = &["PATH"])]
    pub include_path: Vec<String>,
    /// Allow a given path for imports. A list of paths can be supplied by separating them with a comma
    #[clap(long, value_parser, value_names = &["PATH"])]
    pub allowed_path: Vec<String>,
    /// Library to use instead of default
    #[clap(short('l'), long, value_parser, value_names = &["PATH"])]
    pub lib: Option<String>,

    // Output Options:
    /// Prefix for output files (by default, input file stem is used as prefix)
    #[clap(short('p'), long, value_parser, value_names = &["PREFIX"])]
    pub output_prefix: Option<String>,
    /// Output directory (by default, current directory is used)
    #[clap(short('o'), long, value_parser, value_names = &["PATH"])]
    pub output_dir: Option<String>,
    /// Select desired TVM version.
    #[clap(long, value_enum)]
    pub tvm_version: Option<TvmVersion>,

    //Output Components:
    /// Print the code cell to stdout
    #[clap(long("print-code"), value_parser)]
    pub print_code: bool,
    /// ABI specification of the contracts
    #[clap(long, value_parser)]
    pub abi_json: bool,
    /// Print name and id for each public function
    #[clap(long, value_parser)]
    pub function_ids: bool,
    /// Print name and id for each private function
    #[clap(long, value_parser)]
    pub private_function_ids: bool,
    /// AST of all source files in a compact JSON format
    #[clap(long, value_parser)]
    pub ast_compact_json: bool,
    /// Natspec user documentation of all contracts.
    #[clap(long, value_parser)]
    pub userdoc: bool,
    /// Natspec developer documentation of all contracts.
    #[clap(long, value_parser)]
    pub devdoc: bool,

    // TODO ?
    /// Set newly generated keypair
    #[clap(long, value_parser, value_names = &["FILENAME"])]
    pub init: Option<String>,
    /// Mute all notifications
    #[clap(long, value_parser)]
    pub silent: bool,
}
