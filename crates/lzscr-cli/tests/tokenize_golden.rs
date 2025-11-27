use assert_cmd::prelude::*;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

// Helper: run lzscr-cli with an inline program that imports stdlib/lex and tokenizes the given input string,
// pretty-prints tokens as "kind:text:span(off,len)" one per line. The program is constructed in lzscr syntax.
fn run_tokenize(input: &str) -> String {
    // lzscr program: use stdlib prelude and lex.lzscr from stdlib via (~require .lex)
    // Build code that maps tokens to a printable line format.
    let code = format!(
        "(\n  ~Lex = (~require .lex);\n  ~trim_ctor ~name = (~if ((~starts_with ~name \".\")) (~Str .slice ~name 1 ((~Str .len ~name) - 1)) ~name);\n  ~to_line ~t = (\n    ~k = (~t .kind);\n    ~x = (~t .text);\n    ~kind_text = (~trim_ctor (~to_str ~k));\n    (~Str .concat (~Str .concat ~kind_text \":\") ~x)\n  );\n  ~render ~xs = (\n    (\\[] -> \"\")\n    | \\(~h : ~t) -> (\n        ~line = (~to_line ~h);\n        ~rest = (~render ~t);\n        (~Str .concat (~Str .concat ~line \"\\n\") ~rest)\n      )\n  ) ~xs;\n  (~render (((~Lex .token) .tokenize) \"{}\"))\n)\n",
        input.replace('"', "\\\"")
    );
    // Compute absolute stdlib dir: <workspace_root>/stdlib
    let mut stdlib = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    stdlib.pop(); // crates
    stdlib.pop(); // workspace root
    stdlib.push("stdlib");
    let stdlib = stdlib.to_string_lossy().to_string();

    let mut cmd = Command::cargo_bin("lzscr-cli").unwrap();
    // Tokenization golden shouldn't depend on typechecker; skip it to avoid
    // transient stdlib type mismatches breaking lexing tests.
    cmd.args(["-e", &code, "--stdlib-dir", &stdlib, "--no-typecheck"]);
    let out = cmd.assert().success().get_output().stdout.clone();
    String::from_utf8(out).expect("utf8")
}

fn read_golden(name: &str) -> String {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // lzscr-cli
    p.pop(); // crates
    p.push("goldens");
    p.push(format!("{}.golden", name));
    fs::read_to_string(p).expect("read golden")
}

#[test]
#[ignore]
fn golden_ident_ops_numbers() {
    let input =
        "foo bar123 .member .,, 123 45.6 7e+8 9e-2 == != <= >= <- .< .<= .> .>= .+ .- .* ./ || ->";
    let got = run_tokenize(input);
    let want = read_golden("ident_ops_numbers");
    assert_eq!(got.trim(), want.trim());
}

#[test]
#[ignore]
fn golden_strings_chars_comments() {
    let input = "\"hi\\n\" 'x' {- block {- nested -} comment -} # line\nend";
    let got = run_tokenize(input);
    let want = read_golden("strings_chars_comments");
    assert_eq!(got.trim(), want.trim());
}

#[test]
#[ignore]
fn golden_numbers_radix_float() {
    let input = "0x1f 0xDEAD 0o77 0b101 3.14 0.5 2e10 6.02e+23 7e-4 1.5E-2";
    let got = run_tokenize(input);
    let want = read_golden("numbers_radix_float");
    assert_eq!(got.trim(), want.trim());
}
