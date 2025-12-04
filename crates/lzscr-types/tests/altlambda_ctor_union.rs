//! Tests for AltLambda typing with Ctor-union strategy (currently ignored until implemented)

use lzscr_types::api::infer_program;

fn pretty(res: Result<String, String>) -> String {
    match res {
        Ok(s) => format!("OK: {s}"),
        Err(e) => format!("ERR: {e}"),
    }
}

#[test]
fn alt_union_two_ctors_same_return() {
    // (\(Foo ~x) -> 1) | (\(Bar ~y ~z) -> 2)
    // type: (.Foo(Int) | .Bar(Int,Int)) -> Int
    let src = "\\(Foo ~x) -> 1 | \\(Bar ~y ~z) -> 2";
    let got = pretty(infer_program(src));
    assert!(got.contains("-> .Int"), "{got}");
}

#[test]
fn alt_union_duplicate_tag_error() {
    // Same tag twice is an error regardless of arity
    let src = "\\(Foo ~x) -> 1 | \\(Foo ~y ~z) -> 2";
    let got = pretty(infer_program(src));
    assert!(got.contains("Foo") && got.contains("duplicate"), "{got}");
}

#[test]
fn alt_union_mixed_ctor_and_var_rejected() {
    // Mixing Ctor and non-Ctor branches rejected in MVP
    let src = "\\(Foo ~x) -> 1 | \\~y -> 2";
    let got = pretty(infer_program(src));
    assert!(got.contains("Ctor") && got.contains("mixed"), "{got}");
}

#[test]
fn alt_union_default_wildcard_allowed() {
    // Default wildcard does not extend the union; return types still unify
    let src = "\\(Foo ~x) -> 1 | \\_ -> 1";
    let got = pretty(infer_program(src));
    assert!(got.contains("-> .Int"), "{got}");
}
