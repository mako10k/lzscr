use assert_cmd::prelude::*;
use predicates::str::contains;
use std::process::Command;

#[test]
fn eval_add_prints_result() {
    let mut cmd = Command::cargo_bin("lzscr-cli").unwrap();
    cmd.args(["-e", "(~add 1 2)"]);
    cmd.assert().success().stdout(contains("3\n"));
}

#[test]
fn strict_effects_blocks_effect_outside_seq() {
    let mut cmd = Command::cargo_bin("lzscr-cli").unwrap();
    cmd.args(["-e", "!println \"x\"", "-s"]);
    // Runtime prints error to stderr via error formatting in CLI, process still exits 0 for now
    cmd.assert().failure();
}

#[test]
fn strict_effects_allows_effect_with_seq() {
    let mut cmd = Command::cargo_bin("lzscr-cli").unwrap();
    cmd.args(["-e", "(~seq () (!println \"x\"))", "-s"]);
    cmd.assert().success();
}
