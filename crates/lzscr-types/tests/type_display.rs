use lzscr_types::api::infer_program;

#[test]
fn id_function_type() {
    let t = infer_program("\\~x -> ~x").unwrap();
    assert_eq!(t, "%t0 -> %t0");
}

#[test]
fn list_int_type() {
    let t = infer_program("[1,2]").unwrap();
    assert_eq!(t, "[Int]");
}

#[test]
fn ctor_application_type() {
    let t = infer_program(".Foo 1").unwrap();
    assert_eq!(t, ".Foo Int");
}
