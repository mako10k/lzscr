use assert_cmd::prelude::*;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn cli_cmd() -> Command {
    Command::new(assert_cmd::cargo::cargo_bin!("lzscr-cli"))
}

fn run_err_output() -> String {
    let code = "%{ { name: .Str, age: .Int } } { name: \"Alice\", age: \"30\" }".to_string();
    let mut stdlib = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    stdlib.pop();
    stdlib.pop();
    stdlib.push("stdlib");
    let stdlib = stdlib.to_string_lossy().to_string();

    let mut cmd = cli_cmd();
    cmd.args(["-e", &code, "--stdlib-dir", &stdlib]);
    let output = cmd.assert().failure().get_output().clone();
    String::from_utf8(output.stderr).expect("utf8")
}

fn read_golden(name: &str) -> String {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop();
    p.pop();
    p.push("goldens");
    p.push(format!("{}.golden", name));
    fs::read_to_string(p).expect("read golden")
}

#[test]
fn golden_record_field_error_cli() {
    let mut got = run_err_output();
    // Normalize absolute workspace stdlib path to portable ./stdlib for comparison
    let mut ws = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    ws.pop();
    ws.pop();
    ws.push("stdlib");
    if let Some(ws_str) = ws.to_str() {
        got = got.replace(ws_str, "./stdlib");
    }
    let want = read_golden("record_field_error_cli");
    assert_eq!(got.trim(), want.trim());
}
