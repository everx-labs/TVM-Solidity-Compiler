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
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=../compiler/");

    let profile = std::env::var("PROFILE").unwrap();
    let sol2tvm = cmake::Config::new("../compiler").build();

    for lib in ["solc", "solidity", "langutil", "solutil"] {
        if cfg!(any(windows)) {
            println!("cargo:rustc-link-search=native={}/build/lib{}/{}", sol2tvm.display(), lib, profile);
        } else {
            println!("cargo:rustc-link-search=native={}/build/lib{}", sol2tvm.display(), lib);
        }
        println!("cargo:rustc-link-lib=static={}", lib);
    }

    if cfg!(any(windows)) {
        let path = std::path::PathBuf::from("../compiler/deps/install/win64/lib");
        println!("cargo:rustc-link-search=native={}", std::fs::canonicalize(path).unwrap().display());
    }

    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-search=native=/opt/homebrew/lib");
        println!("cargo:rustc-link-search=native=/usr/local/lib");
    }

    println!("cargo:rustc-link-search=native={}/lib", sol2tvm.display());
    println!("cargo:rustc-link-search=native={}/build/deps/lib", sol2tvm.display());
    println!("cargo:rustc-link-lib=static=jsoncpp");
    if !cfg!(any(windows)) {
        println!("cargo:rustc-link-lib=static=boost_filesystem");
    }

    if let Some(cpp_stdlib) = get_cpp_stdlib() {
        println!("cargo:rustc-link-lib={}", cpp_stdlib);
    }

    let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
    bindgen::Builder::default()
        .header("../compiler/libsolc/libsolc.h")
        .generate()
        .expect("Failed to generate bindings")
        .write_to_file(out.join("bindings.rs"))
        .expect("Failed to write bindings");

    let mut git_branch = String::from("Unknown");
    let mut git_commit = String::from("Unknown");
    let mut commit_date = String::from("Unknown");
    let mut build_time = String::from("Unknown");

    let branch = Command::new("git")
        .args(&["rev-parse", "--abbrev-ref", "HEAD"])
        .output();

    if let Ok(branch) = branch {
        git_branch = String::from_utf8(branch.stdout).unwrap_or_else(|_| "Unknown".to_string());
    }

    let last = Command::new("git").args(&["rev-parse", "HEAD"]).output();
    if let Ok(last) = last {
        git_commit = String::from_utf8(last.stdout).unwrap_or_else(|_| "Unknown".to_string());
    }

    let time = Command::new("git")
        .args(&["log", "-1", "--date=iso", "--pretty=format:%cd"])
        .output();
    if let Ok(time) = time {
        commit_date = String::from_utf8(time.stdout).unwrap_or_else(|_| "Unknown".to_string());
    }

    let b_time = Command::new("date").args(&["+%Y-%m-%d %T %z"]).output();
    if let Ok(b_time) = b_time {
        build_time = String::from_utf8(b_time.stdout).unwrap_or_else(|_| "Unknown".to_string());
    }

    println!("cargo:rustc-env=BUILD_GIT_BRANCH={}", git_branch);
    println!("cargo:rustc-env=BUILD_GIT_COMMIT={}", git_commit);
    println!("cargo:rustc-env=BUILD_GIT_DATE={}", commit_date);
    println!("cargo:rustc-env=BUILD_TIME={}", build_time);
}

fn get_cpp_stdlib() -> Option<String> {
    std::env::var("TARGET").ok().and_then(|target| {
        if target.contains("msvc") {
            None
        } else if target.contains("darwin") {
            Some("c++".to_string())
        } else {
            Some("stdc++".to_string())
        }
    })
}
