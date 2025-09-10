use lzscr_parser::parse_expr;
use lzscr_types::api::{infer_ast_with_opts, infer_program, InferOptions};

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

#[test]
fn annotation_enforces_type_ok() {
    // %{Int} 1 should succeed and overall expression type is Int
    let t = infer_program("%{Int} 1").unwrap();
    assert_eq!(t, "Int");
}

#[test]
fn annotation_type_mismatch() {
    let err = infer_program("%{Str} 1").expect_err("expected mismatch");
    assert!(err.contains("type mismatch"));
    assert!(err.contains("Str") || err.contains("Int"));
}

#[test]
fn type_value_vs_annotation_disambiguation() {
    // Standalone: %{Int} is a type value -> its type prints as Type (legacy) and %{Type} in pretty form.
    // Then applying as function would be a separate parse; here just ensure parser sees a TypeVal when no trailing atom.
    let ast = parse_expr("%{Int}").unwrap();
    let legacy = infer_ast_with_opts(&ast, InferOptions { pretty: false }).unwrap();
    // The type of a type value literal is Type in legacy mode.
    assert_eq!(legacy, "Type");
}

#[test]
fn pretty_vs_legacy_variable_names() {
    // Lambda introducing a polymorphic var: \~x -> [~x]
    let src = "\\~x -> [~x]";
    let ast = parse_expr(src).unwrap();
    let legacy = infer_ast_with_opts(&ast, InferOptions { pretty: false }).unwrap();
    let pretty = infer_ast_with_opts(&ast, InferOptions { pretty: true }).unwrap();
    // Legacy should have %tN vars; pretty should wrap and rename to %a etc.
    assert!(legacy.contains("%t"));
    assert!(pretty.starts_with("%{"));
    assert!(pretty.contains("%a"));
}

#[test]
fn sumctor_pretty_and_legacy_display() {
    let src = "\\(.A ~x) -> ~x | \\(.B ~y ~z) -> ~z"; // (.A %t0 | .B(%t1, %t2)) -> %t2 (shape)
    let ast = parse_expr(src).unwrap();
    let legacy = infer_ast_with_opts(&ast, InferOptions { pretty: false }).unwrap();
    let pretty = infer_ast_with_opts(&ast, InferOptions { pretty: true }).unwrap();
    assert!(legacy.contains(".A") && legacy.contains(".B"));
    assert!(legacy.starts_with("(")); // union form wrapped in parens at top-level arg domain
    assert!(pretty.contains(".A") && pretty.contains(".B"));
    assert!(pretty.starts_with("%{"));
}

#[test]
fn list_bracket_type_sugar_in_annotation() {
    let src = "%{[Int]} [1,2,3]"; // Should type as [Int]
    let t = infer_program(src).unwrap();
    assert_eq!(t, "[Int]");
}

#[test]
fn hole_sharing_in_annotation() {
    // %{ %a -> [ %a ] } (\~x -> [~x]) using named var share vs holes: %{ ?x -> [?x] } (\~x -> [~x])
    let src_named = "%{ %a -> [%a] } (\\~x -> [~x])";
    let src_hole = "%{ ?x -> [?x] } (\\~x -> [~x])";
    let tn = infer_program(src_named).unwrap();
    let th = infer_program(src_hole).unwrap();
    // Both should be function types ending with list of same variable.
    assert!(tn.contains("->") && tn.contains("["));
    assert!(th.contains("->") && th.contains("["));
}
