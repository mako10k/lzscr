use assert_cmd::prelude::*;
use predicates::prelude::PredicateBooleanExt;
use predicates::str::contains;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use tempfile::NamedTempFile;

fn cli_cmd() -> Command {
    Command::new(assert_cmd::cargo::cargo_bin!("lzscr-cli"))
}

#[test]
fn eval_add_prints_result() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "(~add 1 2)"]);
    cmd.assert().success().stdout(contains("3\n"));
}

#[test]
fn strict_effects_blocks_effect_outside_seq() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "!println \"x\"", "-s"]);
    // Runtime prints error to stderr via error formatting in CLI, process still exits 0 for now
    cmd.assert().failure();
}

#[test]
fn strict_effects_allows_effect_with_seq() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "(~seq () (!println \"x\"))", "-s"]);
    cmd.assert().success();
}

#[test]
fn dump_coreir_text_outputs_seq() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "(~seq 1 (~add 2 3))", "--dump-coreir"]);
    // expect a textual (~seq ...) in the output
    cmd.assert().success().stdout(contains("(~seq 1 ((~add 2) 3))\n"));
}

#[test]
fn dump_coreir_json_outputs_term() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "(~seq 1 (~add 2 3))", "--dump-coreir-json"]);
    cmd.assert().success().stdout(contains("{\n").and(contains("\"Seq\"")));
}

#[test]
fn file_option_executes_program() {
    // Create a temporary file with top-level bindings and an expression
    let mut tmp = NamedTempFile::new().unwrap();
    // body: define ~x then use it; parentheses wrapping in CLI should form a let-group
    writeln!(tmp, "~x = 1; ~add ~x 2;").unwrap();
    let path = tmp.path();

    let mut cmd = cli_cmd();
    cmd.args(["--file", path.to_str().unwrap()]);
    cmd.assert().success().stdout(contains("3\n"));
}

#[test]
fn stdlib_list_helpers_via_cli() {
    let mut tmp = NamedTempFile::new().unwrap();
    writeln!(tmp, "~xs = [1, 2, 3];").unwrap();
    writeln!(tmp, "~mapped = (~map (\\~x -> (~x + 1)) ~xs);").unwrap();
    writeln!(tmp, "~filtered = (~filter (\\~x -> (~x > 1)) ~xs);").unwrap();
    writeln!(tmp, "~folded = (~foldl (\\~acc ~x -> (~acc + ~x)) 0 ~xs);").unwrap();
    writeln!(tmp, "(~length ~xs, ~mapped, ~filtered, ~folded)").unwrap();

    let stdlib_dir = workspace_stdlib_dir();

    let mut cmd = cli_cmd();
    cmd.args([
        "--file",
        tmp.path().to_str().unwrap(),
        "--stdlib-dir",
        stdlib_dir.to_str().unwrap(),
    ]);

    cmd.assert().success().stdout(contains("(3, [2, 3, 4], [2, 3], 6)\n"));
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

fn workspace_stdlib_dir() -> PathBuf {
    repo_root().join("stdlib")
}

fn run_sample(program: &str) {
    let sample = repo_root().join("tests").join(program);
    let mut cmd = cli_cmd();
    cmd.args(["--file", sample.to_str().unwrap(), "--no-stdlib"]);
    cmd.assert().success();
}

#[test]
fn prelude_basic_smoke() {
    let sample = repo_root().join("tests").join("prelude_basic.lzscr");
    let mut cmd = cli_cmd();
    cmd.args([
        "--file",
        sample.to_str().unwrap(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
    ]);
    let assert = cmd.assert().success();
    let stdout = String::from_utf8_lossy(&assert.get_output().stdout).into_owned();
    let expected_lines = [
        "length [] -> 0",
        "length [1,2,3] -> 3",
        "append [] [1] -> [1]",
        "append [1] [2,3] -> [1, 2, 3]",
        "reverse [1,2,3] -> [3, 2, 1]",
        "map (+1) [1,2] -> [2, 3]",
        "foldl sum [1..4] -> 10",
        "foldr product [1..4] -> 24",
        "split_char 'a,b,c' ',' -> [\"a\", \"b\", \"c\"]",
        "str_len 'abc' -> 3",
        "str_len == 3 -> .True",
        "prelude_basic: done",
    ];
    for needle in expected_lines {
        assert!(stdout.contains(needle), "stdout missing `{}`\nfull output:\n{}", needle, stdout);
    }
}

#[test]
fn regression_min_repro_executes() {
    run_sample("min_repro.lzscr");
}

#[test]
fn regression_hyp_nested_executes() {
    run_sample("hyp_nested.lzscr");
}
