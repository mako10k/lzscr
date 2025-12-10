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
    assert!(ty == "(Ok Str | Err Str)" || ty == "(Err Str | Ok Str)", "unexpected type: {ty}");
}

#[test]
fn fs_write_text_types_as_result() {
    let src = "(~seq () (!fs.write_text \"foo.txt\" \"payload\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(Ok Unit | Err Str)" || ty == "(Err Str | Ok Unit)", "unexpected type: {ty}");
}

#[test]
fn fs_append_text_types_as_result() {
    let src = "(~seq () (!fs.append_text \"foo.txt\" \"payload\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(Ok Unit | Err Str)" || ty == "(Err Str | Ok Unit)", "unexpected type: {ty}");
}

#[test]
fn fs_list_dir_types_as_result() {
    let src = "(~seq () (!fs.list_dir \"foo\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(Ok [Str] | Err Str)" || ty == "(Err Str | Ok [Str])", "unexpected type: {ty}");
}

#[test]
fn fs_remove_file_types_as_result() {
    let src = "(~seq () (!fs.remove_file \"foo.txt\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(Ok Unit | Err Str)" || ty == "(Err Str | Ok Unit)", "unexpected type: {ty}");
}

#[test]
fn fs_create_dir_types_as_result() {
    let src = "(~seq () (!fs.create_dir \"foo/bar\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty == "(Ok Unit | Err Str)" || ty == "(Err Str | Ok Unit)", "unexpected type: {ty}");
}

#[test]
fn fs_metadata_types_as_result() {
    let src = "(~seq () (!fs.metadata \"foo.txt\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    let ty = got.unwrap();
    assert!(ty.contains("is_dir: (False | True)"), "missing is_dir field: {ty}");
    assert!(ty.contains("is_file: (False | True)"), "missing is_file field: {ty}");
    let has_mod_some_first = ty.contains("modified_ms: (Some Int | None)");
    let has_mod_none_first = ty.contains("modified_ms: (None | Some Int)");
    assert!(has_mod_some_first || has_mod_none_first, "missing modified_ms option field: {ty}");
    assert!(ty.contains("readonly: (False | True)"), "missing readonly field: {ty}");
    assert!(ty.contains("size: Int"), "missing size field: {ty}");
    assert!(ty.contains("Ok") && ty.contains("Err"), "expected Result in type: {ty}");
}

#[test]
fn io_print_types_as_unit() {
    let src = "(~seq () (!print \"msg\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    assert_eq!(got.unwrap(), "Unit");
}

#[test]
fn io_println_types_as_unit() {
    let src = "(~seq () (!println \"msg\"))";
    let got = infer_program(src);
    assert!(got.is_ok(), "unexpected error: {}", pretty(got));
    assert_eq!(got.unwrap(), "Unit");
}
