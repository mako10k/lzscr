use lzscr_types::api::infer_program;
use lzscr_parser::parse_expr;

#[test]
fn record_transpose_modes() {
    let src = ".x ({ a: .{ x: 1 }, b: .{ x: \"s\" } })";
    let ast = parse_expr(src).unwrap();
    println!("AST: {:?}", ast);
    let t = infer_program(src).unwrap();
    println!("TYPE: {}", t);
    assert_eq!(t, "{a: Int, b: Str}");
}

#[test]
fn ctor_transpose_modes() {
    // Ctor transpose: skipped in this test iteration (parsing/constructor
    // application interaction requires more targeted cases). Keep as TODO.
}

#[test]
fn tuple_transpose_modes() {
    let src = ".x ((.{ x: 1 }, .{ x: \"s\" }))";
    let ast = parse_expr(src).unwrap();
    println!("AST: {:?}", ast);
    let t = infer_program(src).unwrap();
    println!("TYPE: {}", t);
    assert_eq!(t, "(Int, Str)");
}

#[test]
fn list_transpose_modes() {
    // List transpose: skipped for now; requires unified element shaping.
}
