//! Runs `rustc --print cfg` to get built-in cfg flags.

use std::process::Command;

use anyhow::Result;
use paths::AbsPath;

use crate::{cfg_flag::CfgFlag, utf8_stdout};

pub(crate) fn get(cargo_toml: Option<&AbsPath>, target: Option<&str>) -> Vec<CfgFlag> {
    let _p = profile::span("rustc_cfg::get");
    let mut res = Vec::with_capacity(6 * 2 + 1);

    // Some nightly-only cfgs, which are required for stdlib
    res.push(CfgFlag::Atom("target_thread_local".into()));
    for &ty in ["8", "16", "32", "64", "cas", "ptr"].iter() {
        for &key in ["target_has_atomic", "target_has_atomic_load_store"].iter() {
            res.push(CfgFlag::KeyValue { key: key.to_string(), value: ty.into() });
        }
    }

    match get_rust_cfgs(cargo_toml, target) {
        Ok(rustc_cfgs) => res.extend(rustc_cfgs.lines().map(|it| it.parse().unwrap())),
        Err(e) => log::error!("failed to get rustc cfgs: {:#}", e),
    }

    res
}

fn get_rust_cfgs(cargo_toml: Option<&AbsPath>, target: Option<&str>) -> Result<String> {
    let cargo_rust_cfgs = match cargo_toml {
        Some(cargo_toml) => {
            let mut cargo_config = Command::new(toolchain::cargo());
            cargo_config
                .current_dir(cargo_toml.parent().unwrap())
                .args(&["-Z", "unstable-options", "rustc", "--print", "cfg"])
                .env("RUSTC_BOOTSTRAP", "1");
            if let Some(target) = target {
                cargo_config.args(&["--target", target]);
            }
            utf8_stdout(cargo_config).ok()
        }
        None => None,
    };
    match cargo_rust_cfgs {
        Some(stdout) => Ok(stdout),
        None => {
            // using unstable cargo features failed, fall back to using plain rustc
            let mut cmd = Command::new(toolchain::rustc());
            cmd.args(&["--print", "cfg", "-O"]);
            if let Some(target) = target {
                cmd.args(&["--target", target]);
            }
            utf8_stdout(cmd)
        }
    }
}
