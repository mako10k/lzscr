use lzscr_types::api::infer_program;

fn pretty(r: Result<String, String>) -> String {
    r.unwrap_or_else(|e| format!("ERR:{e}"))
}

#[test]
fn chain_allows_multiple_exprs() {
    // !{ !println "a"; !println "b"; 42 }
    let src = "(~chain (!println \"a\") (~chain (!println \"b\") 42))";
    let got = pretty(infer_program(src));
    assert_eq!(got, "Int");
}

#[test]
fn bind_types_like_apply() {
    // (~bind 1 (\x -> x)) : Int
    let src = "(~bind 1 (\\~x -> ~x))";
    let got = pretty(infer_program(src));
    assert_eq!(got, "Int");
}
