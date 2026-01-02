use assert_cmd::prelude::*;
use predicates::prelude::PredicateBooleanExt;
use predicates::str::contains;
use std::fs;
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
fn dump_llvmir_outputs_basic_ir() {
    let mut cmd = cli_cmd();
    cmd.args(["-e", "1 + 2 * 3", "--dump-llvmir", "--no-stdlib"]);
    cmd.assert().success().stdout(
        contains("define i64 @main()")
            .and(contains("mul i64 2, 3"))
            .and(contains("add i64 1"))
            .and(contains("ret i64")),
    );
}

fn have_build_toolchain() -> bool {
    // Prefer clang for determinism; skip the test if clang isn't available.
    Command::new("clang").arg("--version").output().is_ok()
}

#[test]
fn build_exe_produces_runnable_binary_when_toolchain_available() {
    if !have_build_toolchain() {
        eprintln!("skip: clang not available");
        return;
    }

    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("lzscr_out");

    // First build should succeed
    let mut cmd = cli_cmd();
    cmd.args(["-e", "1 + 2 * 3", "--build-exe", out.to_str().unwrap(), "--no-stdlib"]);
    cmd.assert().success();

    // Second build without overwrite should fail
    let mut cmd = cli_cmd();
    cmd.args(["-e", "1 + 2 * 3", "--build-exe", out.to_str().unwrap(), "--no-stdlib"]);
    cmd.assert().failure().stderr(contains("output already exists"));

    // With overwrite flag it should succeed again
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "1 + 2 * 3",
        "--build-exe",
        out.to_str().unwrap(),
        "--build-exe-overwrite",
        "--no-stdlib",
    ]);
    cmd.assert().success();

    let st = Command::new(&out).status().unwrap();
    assert_eq!(st.code(), Some(7));
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

#[test]
fn effect_modules_blocked_in_pure_mode() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~require .effect .log)",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
    ]);

    cmd.assert().failure().stderr(contains("--stdlib-mode=allow-effects"));
}

#[test]
fn compat_module_blocked_in_pure_mode() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~require .compat .prelude_aliases)",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
    ]);

    cmd.assert().failure().stderr(contains("--stdlib-mode=allow-effects"));
}

#[test]
fn effect_modules_allowed_with_flag() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Log = (~require .effect .log); (~Scoped = ((~Log .with_fields_json_logger) (~Log .info_fields_json) [((~Log .field) \"session\" 9)]); (~chain ((~Log .tap_info) \"demo\" 42) (~Scoped \"stats\" [((~Log .field) \"count\" 2)]))))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);

    cmd.assert().success().stdout(
        contains("[INFO] demo: 42\n")
            .and(contains("[INFO] stats: {\"session\": 9, \"count\": 2}\n")),
    );
}

#[test]
fn compat_module_warns_with_flag() {
    let program = "(~Compat = (~require .compat .prelude_aliases); (~Compat .is_some (Some 1), (~Compat .map_option (\\~x -> (~x + 1)) (Some 2))))";
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program,
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);

    cmd.assert().success().stdout(
        contains("[WARN] stdlib.compat")
            .and(contains("~is_some is deprecated"))
            .and(contains("~map_option is deprecated"))
            .and(contains("(True, Some 3)")),
    );
}

#[test]
fn effect_fs_module_blocked_in_pure_mode() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~require .effect .fs)",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
    ]);

    cmd.assert().failure().stderr(contains("--stdlib-mode=allow-effects"));
}

#[test]
fn effect_fs_read_text_allowed_with_flag() {
    let mut tmp = NamedTempFile::new().unwrap();
    write!(tmp, "hello-fs").unwrap();
    let path_literal = format!("{:?}", tmp.path().to_str().unwrap());
    let program =
        format!("(~Fs = (~require .effect .fs); (~Fs .read_text_result {}))", path_literal);

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Ok hello-fs"));
}

#[test]
fn effect_fs_write_text_allowed_with_flag() {
    let tmp = NamedTempFile::new().unwrap();
    let path_literal = format!("{:?}", tmp.path().to_str().unwrap());
    let program = format!(
        "(~Fs = (~require .effect .fs); (~chain (~Fs .write_text_result {} \"payload\") (~Fs .read_text_result {})))",
        path_literal, path_literal
    );

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Ok payload"));
}

#[test]
fn effect_fs_append_text_allowed_with_flag() {
    let mut tmp = NamedTempFile::new().unwrap();
    write!(tmp, "seed").unwrap();
    tmp.flush().unwrap();
    let path_literal = format!("{:?}", tmp.path().to_str().unwrap());
    let program = format!(
        "(~Fs = (~require .effect .fs); (~chain (~Fs .append_text_result {} \"-tail\") (~Fs .read_text_result {})))",
        path_literal, path_literal
    );

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Ok seed-tail"));
}

#[test]
fn effect_fs_list_dir_allowed_with_flag() {
    let dir = tempfile::tempdir().unwrap();
    let file_a = dir.path().join("a.txt");
    let file_b = dir.path().join("b.log");
    fs::write(&file_a, "a").unwrap();
    fs::write(&file_b, "b").unwrap();
    let path_literal = format!("{:?}", dir.path().to_str().unwrap());
    let program =
        format!("(~Fs = (~require .effect .fs); (~Fs .list_dir_result {}))", path_literal);

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Ok").and(contains("a.txt")).and(contains("b.log")));
}

#[test]
fn effect_fs_remove_file_allowed_with_flag() {
    let tmp = NamedTempFile::new().unwrap();
    let path = tmp.path().to_path_buf();
    let path_literal = format!("{:?}", path.to_str().unwrap());
    let program = format!(
        "(~Fs = (~require .effect .fs); (~chain (~Fs .remove_file_result {}) (~Fs .read_text_result {})))",
        path_literal, path_literal
    );

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Err"));
}

#[test]
fn effect_fs_create_dir_allowed_with_flag() {
    let dir = tempfile::tempdir().unwrap();
    let new_dir = dir.path().join("nested");
    let path_literal = format!("{:?}", new_dir.to_str().unwrap());
    let dir_literal = format!("{:?}", dir.path().to_str().unwrap());
    let program = format!(
        "(~Fs = (~require .effect .fs); (~chain (~Fs .create_dir_result {}) (~Fs .list_dir_result {})))",
        path_literal, dir_literal
    );

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("Ok").and(contains("nested")));
}

#[test]
fn effect_fs_metadata_allowed_with_flag() {
    let mut tmp = NamedTempFile::new().unwrap();
    write!(tmp, "metal").unwrap();
    tmp.flush().unwrap();
    let path_literal = format!("{:?}", tmp.path().to_str().unwrap());
    let program =
        format!("(~Fs = (~require .effect .fs); (~Fs .metadata_result {}))", path_literal);

    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        program.as_str(),
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);

    cmd.assert().success().stdout(
        contains("Ok")
            .and(contains("size: 5"))
            .and(contains("is_dir: False"))
            .and(contains("is_file: True"))
            .and(contains("readonly: False"))
            .and(contains("modified_ms: (Some")),
    );
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
        "str_len == 3 -> True",
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

#[test]
fn effect_io_print_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Io = (~require .effect .io); (~Io .print \"hello\"))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("hello"));
}

#[test]
fn effect_io_println_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Io = (~require .effect .io); (~Io .println \"world\"))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("world\n"));
}

#[test]
fn effect_io_show_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Io = (~require .effect .io); (~Io .show 42))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("42\n"));
}

#[test]
fn effect_io_log_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Io = (~require .effect .io); (~Io .log \"value: \" 10))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("value: 10\n"));
}

#[test]
fn effect_log_emit_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Log = (~require .effect .log); (~Log .emit \"INFO\" \"started\"))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("[INFO] started\n"));
}

#[test]
fn effect_log_level_logger_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Log = (~require .effect .log); (~Log .info \"session\" 999))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("[INFO] session: 999\n"));
}

#[test]
fn effect_log_fields_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Log = (~require .effect .log); (~Log .info_fields \"req\" [(~Log .field \"id\" 1)]))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("[INFO] req: id=1\n"));
}

#[test]
fn effect_log_fields_json_smoke() {
    let mut cmd = cli_cmd();
    cmd.args([
        "-e",
        "(~Log = (~require .effect .log); (~Log .info_fields_json \"metrics\" [(~Log .field \"count\" 5)]))",
        "--stdlib-dir",
        workspace_stdlib_dir().to_str().unwrap(),
        "--stdlib-mode",
        "allow-effects",
    ]);
    cmd.assert().success().stdout(contains("[INFO] metrics: {\"count\": 5}\n"));
}
