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

use assert_cmd::Command;
use predicates::prelude::*;

type Status = Result<(), Box<dyn std::error::Error>>;
const BIN_NAME: &str = "sold";

fn remove_all_outputs(name: &str) -> Status {
    std::fs::remove_file(format!("tests/{}.abi.json", name))?;
    std::fs::remove_file(format!("tests/{}.code", name))?;
    std::fs::remove_file(format!("tests/{}.debug.json", name))?;
    std::fs::remove_file(format!("tests/{}.tvc", name))?;
    Ok(())
}

#[test]
fn test_trivial() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Trivial.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success()
        .stdout(predicate::str::contains("Contract successfully compiled"));

    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Trivial.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--print_code")
        .assert()
        .success()
        .stdout(predicate::str::contains("code\":\"te6ccgECDAEAA"));

    remove_all_outputs("Trivial")?;
    Ok(())
}

#[test]
fn test_combined() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Combined.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success()
        .stdout(predicate::str::contains("Contract successfully compiled"));

    remove_all_outputs("Combined")?;
    Ok(())
}

#[test]
fn test_multi() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Multi.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--contract")
        .arg("Contract1")
        .assert()
        .success()
        .stdout(predicate::str::contains("Contract successfully compiled"));

    remove_all_outputs("Multi")?;
    Ok(())
}

#[test]
fn test_abi_json() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/AbiJson.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--abi-json")
        .arg("--contract")
        .arg("Contract")
        .assert()
        .success()
        .stdout(predicate::str::is_empty());

    std::fs::remove_file("tests/AbiJson.abi.json")?;
    Ok(())
}

#[test]
fn test_library() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Library.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Source file contains no deployable contracts",
        ));

    Ok(())
}

#[test]
fn test_abstract() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Abstract.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--abi-json")
        .assert()
        .success()
        .stdout(predicate::str::is_empty());

    std::fs::remove_file("tests/Abstract.abi.json")?;
    Ok(())
}

#[test]
fn test_error_reporting() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/ErrorReporting.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Compilation failed"));

    Ok(())
}

#[test]
fn test_cycle() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/CycleA.sol")
        .arg("--output-dir")
        .arg("tests")
        .assert()
        .success()
        .stdout(predicate::str::contains("Contract successfully compiled"));

    remove_all_outputs("CycleA")?;
    Ok(())
}

#[test]
fn test_init() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/Init.sol")
        .arg("--output-dir")
        .arg("tests")
        .arg("--init")
        .arg("{\"field1\":0,\"field2\":\"dummy\"}")
        .assert()
        .success()
        .stdout(predicate::str::contains("Contract successfully compiled"));

    let abi = std::fs::read_to_string("tests/Init.abi.json")?;
    assert!(abi.contains("ABI version"));

    remove_all_outputs("Init")?;
    Ok(())
}

#[test]
fn test_private_function_ids() -> Status {
    Command::cargo_bin(BIN_NAME)?
        .arg("tests/FunctionId.sol")
        .arg("--private-function-ids")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            r#"[
  {
    "id": 4199241165,
    "scope": "C",
    "sign": "f(uint32,uint256,uint256)"
  },
  {
    "id": 2254871888,
    "scope": "C",
    "sign": "add(uint256,uint256)"
  },
  {
    "id": 4034881437,
    "scope": "C",
    "sign": "sub(uint256,uint256)"
  },
  {
    "id": 4048818487,
    "scope": "Math",
    "sign": "mul(uint256,uint256)"
  }
"#,
        ));
    Ok(())
}
