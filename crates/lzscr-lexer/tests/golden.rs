use lzscr_lexer::{lex_skip_comments, Tok};
use std::fs;
use std::path::PathBuf;

fn format_tokens(src: &str) -> String {
    let toks = lex_skip_comments(src);
    let mut out = String::new();
    for t in toks {
        let k = match t.tok {
            Tok::LBrace => "LBrace",
            Tok::RBrace => "RBrace",
            Tok::LParen => "LParen",
            Tok::RParen => "RParen",
            Tok::LBracket => "LBracket",
            Tok::RBracket => "RBracket",
            Tok::TypeOpen => "TypeOpen",
            Tok::Comma => "Comma",
            Tok::Semicolon => "Semicolon",
            Tok::Caret => "Caret",
            Tok::PipePipe => "PipePipe",
            Tok::Pipe => "Pipe",
            Tok::Colon => "Colon",
            Tok::Arrow => "Arrow",
            Tok::Eq => "Eq",
            Tok::At => "At",
            Tok::Bang => "Bang",
            Tok::Backslash => "Backslash",
            Tok::Question => "Question",
            Tok::Plus => "Plus",
            Tok::Minus => "Minus",
            Tok::Star => "Star",
            Tok::Slash => "Slash",
            Tok::DotPlus => "DotPlus",
            Tok::DotMinus => "DotMinus",
            Tok::DotStar => "DotStar",
            Tok::DotSlash => "DotSlash",
            Tok::LessEq => "LessEq",
            Tok::LeftArrow => "LeftArrow",
            Tok::Less => "Less",
            Tok::GreaterEq => "GreaterEq",
            Tok::Greater => "Greater",
            Tok::EqEq => "EqEq",
            Tok::BangEq => "BangEq",
            Tok::DotLessEq => "DotLessEq",
            Tok::DotLess => "DotLess",
            Tok::DotGreaterEq => "DotGreaterEq",
            Tok::DotGreater => "DotGreater",
            Tok::Tilde => "Tilde",
            Tok::Float(_) => "Float",
            Tok::Int(_) => "Int",
            Tok::Str(_) => "Str",
            Tok::Char(_) => "Char",
            Tok::Ident => "Ident",
            Tok::TyVar(_) => "TyVar",
            Tok::Member(_) => "Member",
            Tok::CommentLine | Tok::CommentBlock | Tok::_Whitespace => unreachable!(),
        };
        out.push_str(&format!("{}:{}\n", k, t.text));
    }
    out
}

fn read_golden(name: &str) -> String {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // lzscr-lexer
    p.pop(); // crates
    p.push("goldens");
    p.push(format!("{}.golden", name));
    fs::read_to_string(p).expect("read golden")
}

#[test]
fn golden_ident_ops_numbers() {
    let input =
        "foo bar123 .member .,, 123 45.6 7e+8 9e-2 == != <= >= <- .< .<= .> .>= .+ .- .* ./ || ->";
    let got = format_tokens(input);
    let want = read_golden("ident_ops_numbers");
    assert_eq!(got.trim(), want.trim());
}

#[test]
fn golden_strings_chars_comments() {
    let input = "\"hi\\n\" 'x' {- block {- nested -} comment -} # line\nend";
    let got = format_tokens(input);
    let want = read_golden("strings_chars_comments");
    assert_eq!(got.trim(), want.trim());
}

#[test]
fn golden_numbers_radix_float() {
    let input = "0x1f 0xDEAD 0o77 0b101 3.14 0.5 2e10 6.02e+23 7e-4 1.5E-2";
    let got = format_tokens(input);
    let want = read_golden("numbers_radix_float");
    assert_eq!(got.trim(), want.trim());
}
