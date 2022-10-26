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

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(deref_nullptr)]

// include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
#[cfg(target_os = "freebsd")]
include!("bindings-freebsd.rs");
#[cfg(target_os = "linux")]
include!("bindings-linux.rs");
#[cfg(target_os = "macos")]
include!("bindings-macos.rs");
#[cfg(target_os = "windows")]
include!("bindings-windows.rs");
