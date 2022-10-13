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

fn main() {
    println!("cargo:rerun-if-changed=../compiler/");

    let profile = std::env::var("PROFILE").unwrap();
    let sol2tvm = cmake::Config::new("../compiler").build();

    for lib in ["solc", "solidity", "langutil", "solutil"] {
        if cfg!(target_os = "windows") {
            println!("cargo:rustc-link-search=native={}/build/lib{}/{}", sol2tvm.display(), lib, profile);
        } else {
            println!("cargo:rustc-link-search=native={}/build/lib{}", sol2tvm.display(), lib);
        }
        println!("cargo:rustc-link-lib=static={}", lib);
    }

    println!("cargo:rustc-link-search=native={}/lib", sol2tvm.display());
    println!("cargo:rustc-link-search=native={}/build/deps/lib", sol2tvm.display());

    if cfg!(target_os = "windows") {
        let path = std::path::PathBuf::from("../compiler/deps/install/win64/lib");
        println!("cargo:rustc-link-search=native={}", std::fs::canonicalize(path).unwrap().display());
    } else if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-search=native=/opt/homebrew/lib");
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
    }

    if cfg!(target_os = "macos") || cfg!(target_os = "freebsd") {
        println!("cargo:rustc-link-lib=c++");
    } else if cfg!(target_os = "linux") {
        println!("cargo:rustc-link-lib=stdc++");
    }

    let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
    bindgen::Builder::default()
        .header("../compiler/libsolc/libsolc.h")
        .generate()
        .expect("Failed to generate bindings")
        .write_to_file(out.join("bindings.rs"))
        .expect("Failed to write bindings");
}
