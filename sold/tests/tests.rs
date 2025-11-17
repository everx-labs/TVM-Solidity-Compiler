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

use assert_cmd::cargo_bin_cmd;
use predicates::prelude::*;

type Status = Result<(), Box<dyn std::error::Error>>;

macro_rules! bin_name {
    () => {
        "sold"
    };
}

fn remove_all_outputs(name: &str) -> Status {
    std::fs::remove_file(format!("tests/{name}.abi.json"))?;
    std::fs::remove_file(format!("tests/{name}.code"))?;
    std::fs::remove_file(format!("tests/{name}.debug.json"))?;
    std::fs::remove_file(format!("tests/{name}.tvc"))?;
    Ok(())
}

#[test]
fn test_trivial() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Trivial.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success();

    remove_all_outputs("Trivial")?;
    Ok(())
}

#[test]
fn test_combined() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Combined.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success();

    remove_all_outputs("Combined")?;
    Ok(())
}

#[test]
fn test_multi() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Multi.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--contract")
        .arg("Contract1")
        .assert()
        .success();

    remove_all_outputs("Multi")?;
    Ok(())
}

#[test]
fn test_abi_json() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/AbiJson.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--abi-json")
        .arg("--contract")
        .arg("Contract")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "ABI was generated and saved to file ",
        ));

    std::fs::remove_file("tests/AbiJson.abi.json")?;
    Ok(())
}

#[test]
fn test_library() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Library.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Compiler run successful. Artifact(s) can be found in directory \"tests\".\n",
        ));

    Ok(())
}

#[test]
fn test_abstract() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Abstract.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--abi-json")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "ABI was generated and saved to file ",
        ));

    std::fs::remove_file("tests/Abstract.abi.json")?;
    Ok(())
}

#[test]
fn test_error_reporting() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/ErrorReporting.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Error: No matching declaration found after argument-dependent lookup.",
        ));

    Ok(())
}

#[test]
fn test_cycle() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/CycleA.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--base-path")
        .arg("tests")
        .assert()
        .success();

    remove_all_outputs("CycleA")?;
    Ok(())
}

#[test]
fn test_private_function_ids() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/FunctionId.sol")
        .arg("--private-function-ids")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Private function IDs were generated and saved to file ./FunctionId.pids",
        ));

    let real = std::fs::read_to_string("FunctionId.pids")?;
    let expected = r#"[
    {
        "id": 7460,
        "scope": "C",
        "sign": "f(int19,uint256,uint256)"
    },
    {
        "id": 7504,
        "scope": "C",
        "sign": "add(uint256,uint256)"
    },
    {
        "id": 10141,
        "scope": "C",
        "sign": "sub(uint256,uint256)"
    },
    {
        "id": 10143,
        "scope": "Math",
        "sign": "mul(uint256,uint256)"
    }
]
"#;
    assert_eq!(real, expected);

    std::fs::remove_file("FunctionId.pids")?;
    Ok(())
}

#[test]
fn test_remapping() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/ImportRemote.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("github.com/everx-labs/debots/=tests/remote/")
        .assert()
        .success();

    remove_all_outputs("ImportRemote")?;
    Ok(())
}

#[test]
fn test_userdoc_devdoc() -> Status {
    cargo_bin_cmd!(bin_name!())
        .arg("tests/Trivial.sol")
        .arg("--userdoc")
        .arg("--devdoc")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            r#"
======= tests/Trivial.sol:Trivial =======
Developer Documentation
{"kind":"dev","methods":{},"version":1}
User Documentation
{"kind":"user","methods":{},"version":1}
"#,
        ));
    Ok(())
}
