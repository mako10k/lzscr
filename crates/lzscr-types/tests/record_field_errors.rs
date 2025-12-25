use lzscr_types::api::infer_program;
use std::fs;
use std::path::PathBuf;

#[test]
fn record_field_type_mismatch_reports_field_name() {
    // Annotate expected record type: { name: Str, age: Int }
    // Provide a record where age is a string -> should produce a record field mismatch
    let src = "%{ { name: .Str, age: .Int } } { name: \"Alice\", age: \"30\" }";
    let err = infer_program(src).expect_err("expected a type error for mismatched field");

    // Load golden substring and assert the error contains it.
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/lzscr-types -> crates
    p.pop(); // crates -> workspace root
    p.push("goldens");
    p.push("record_field_error.golden");
    let want = fs::read_to_string(p).expect("read golden");
    assert!(err.contains(want.trim()), "got error: {}", err);
}
