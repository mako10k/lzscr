use lzscr_types::api::infer_program;

fn pretty(r: Result<String, String>) -> String {
    r.unwrap_or_else(|e| format!("ERR: {e}"))
}

#[test]
fn effect_forbidden_outside_seq() {
    let src = "!println \"x\"";
    let got = infer_program(src);
    assert!(got.is_err(), "expected type error, got: {}", pretty(got));
    let err = got.err().unwrap();
    assert!(err.contains("effect not allowed"), "unexpected error: {err}");
}

#[test]
fn effect_allowed_in_seq_second_arg() {
    // (~seq () (!println "x")) should type as Unit
    let src = "(~seq () (!println \"x\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    assert_eq!(got.unwrap(), "Unit");
}

#[test]
fn fs_read_text_types_as_result() {
    let src = "(~seq () (!fs.read_text \"foo.txt\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(.Ok Str | .Err Str)" || ty == "(.Err Str | .Ok Str)", "unexpected type: {ty}");
}

#[test]
fn fs_write_text_types_as_result() {
    let src = "(~seq () (!fs.write_text \"foo.txt\" \"payload\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(
        ty == "(.Ok Unit | .Err Str)" || ty == "(.Err Str | .Ok Unit)",
        "unexpected type: {ty}"
    );
}

#[test]
fn fs_append_text_types_as_result() {
    let src = "(~seq () (!fs.append_text \"foo.txt\" \"payload\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(
        ty == "(.Ok Unit | .Err Str)" || ty == "(.Err Str | .Ok Unit)",
        "unexpected type: {ty}"
    );
}

#[test]
fn fs_list_dir_types_as_result() {
    let src = "(~seq () (!fs.list_dir \"foo\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(
        ty == "(.Ok [Str] | .Err Str)" || ty == "(.Err Str | .Ok [Str])",
        "unexpected type: {ty}"
    );
}
