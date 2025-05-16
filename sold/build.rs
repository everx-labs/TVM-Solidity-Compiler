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

fn absolute_path(path: &str) -> String {
    let path = std::path::PathBuf::from(path);
    let mut str = std::fs::canonicalize(path).unwrap().display().to_string();
    // eprintln!("We will get it from {}", str.to_string());
    if str.starts_with(r"\\?\") {
        // https://doc.rust-lang.org/std/fs/fn.canonicalize.html
        // https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
        str = str.get(4..).unwrap().to_string();
    }
    str
}

fn full_lib_name(lib_name: &str, boost_lib_dir: &str) -> String {
    let files = std::fs::read_dir(boost_lib_dir).unwrap();
    for file in files {
        let mut file_name = file.unwrap().file_name().to_str().unwrap().to_string();
        if file_name.starts_with(lib_name) {
            if file_name.ends_with(".lib") {
                file_name = file_name.get(0..file_name.len() - 4).unwrap().to_string();
            }
            return file_name.to_string();
        }
    }
    panic!();
}

// To debug this script use command:
// cargo build -vv
fn main() {
    println!("cargo:rerun-if-changed=../compiler/");

    let profile = std::env::var("PROFILE").unwrap();
    let sol2tvm = {
        let mut config = cmake::Config::new("../compiler");
        config.define("PEDANTIC", "OFF");
        if cfg!(target_os = "windows") {
            config
                .define("BOOST_ROOT", absolute_path("../compiler/deps/boost/"))
                .define("CMAKE_MSVC_RUNTIME_LIBRARY", "MultiThreaded");
        }
        config.build()
    };

    for lib in ["solc", "solidity", "langutil", "solutil"] {
        if cfg!(target_os = "windows") {
            println!(
                "cargo:rustc-link-search=native={}/build/lib{}/{}",
                sol2tvm.display(),
                lib,
                profile
            );
        } else {
            println!(
                "cargo:rustc-link-search=native={}/build/lib{}",
                sol2tvm.display(),
                lib
            );
        }
        println!("cargo:rustc-link-lib=static={}", lib);
    }

    println!("cargo:rustc-link-search=native={}/lib", sol2tvm.display());
    println!(
        "cargo:rustc-link-search=native={}/build/deps/lib",
        sol2tvm.display()
    );

    let boost_lib_dir = "../compiler/deps/boost/lib";
    if cfg!(target_os = "windows") {
        println!(
            "cargo:rustc-link-search=native={}",
            absolute_path(boost_lib_dir)
        );
    } else if cfg!(target_os = "macos") {
        match std::env::var("HOMEBREW") {
            Ok(homebrew) => {
                assert!(
                    std::path::PathBuf::from(homebrew.to_string()).exists(),
                    "Set correct $HOMEBREW!"
                );
                println!("cargo:rustc-link-search=native={}/lib", homebrew)
            }
            // use default path
            Err(_) => println!("cargo:rustc-link-search=native=/opt/homebrew/lib"),
        }
        println!("cargo:rustc-link-search=native=/usr/local/lib");
    } else if cfg!(target_os = "freebsd") {
        println!("cargo:rustc-link-search=native=/usr/local/lib");
    }

    println!("cargo:rustc-link-lib=static=jsoncpp");
    if cfg!(target_os = "linux") || cfg!(target_os = "macos") {
        // preferred way is to link statically to minimize exec deps
        println!("cargo:rustc-link-search=native=/usr/lib/x86_64-linux-gnu");
        println!("cargo:rustc-link-lib=static=boost_filesystem");
    } else if cfg!(target_os = "freebsd") {
        // libboost_filesystem.a from devel/boost-libs is compiled w/o -fPIC,
        // so link dynamically
        println!("cargo:rustc-link-lib=boost_filesystem");
    } else if cfg!(target_os = "windows") {
        let lib_name = full_lib_name("libboost_filesystem", &absolute_path(boost_lib_dir));
        println!("cargo:rustc-link-lib=static={}", lib_name);
    }

    if cfg!(target_os = "macos") || cfg!(target_os = "freebsd") {
        println!("cargo:rustc-link-lib=c++");
    } else if cfg!(target_os = "linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }

    // let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
    // bindgen::Builder::default()
    //     .header("../compiler/libsolc/libsolc.h")
    //     .generate()
    //     .expect("Failed to generate bindings")
    //     .write_to_file(out.join("bindings.rs"))
    //     .expect("Failed to write bindings");
}
