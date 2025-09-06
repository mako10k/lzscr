use lzscr_runtime::{Env, eval};
use lzscr_parser::parse_expr;

#[test]
fn do_block_multiple() {
    let code = "!{ _ <- !println \"a\"; !println \"b\"; 3 + 4 }";
    let ast = parse_expr(code).unwrap();
    let env = Env::with_builtins();
    let _ = eval(&env, &ast).unwrap();
    // apply ~to_str to value and compare string
    // we don't have direct apply here; instead, just check that printing value is "7"
    // Using CLI-level to_str is complex; simplify by evaluating an expression that computes string directly
        let code = "(~to_str (!{ _ <- !println \"a\"; !println \"b\"; 3 + 4 }))";
        let ast = parse_expr(code).unwrap();
        let v = eval(&env, &ast).unwrap();
        match v { lzscr_runtime::Value::Str(s) => assert_eq!(s.to_string(), "7"), _ => panic!("expected Str") }
}
