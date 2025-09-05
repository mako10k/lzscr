use logos::{Lexer, Logos};
use lzscr_ast::span::Span;

#[derive(Debug, Logos, PartialEq, Clone)]
pub enum Tok {
    #[regex(r"[ \t\r\n]+", logos::skip)]

    #[regex(r"#[^\n]*", logos::skip)]
    CommentLine,

    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token(",")] Comma,
    #[token("->")] Arrow,
    #[token("\\")] Backslash,

    #[token("~")] Tilde,

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),

    #[regex(r#"\"([^"\\]|\\.)*\""#, parse_string)]
    Str(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
}

fn parse_string(lex: &mut Lexer<Tok>) -> Option<String> {
    let s = lex.slice();
    let inner = &s[1..s.len()-1];
    let mut out = String::new();
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(n) = chars.next() { out.push(n); } else { break; }
        } else { out.push(c); }
    }
    Some(out)
}

#[derive(Debug, Clone)]
pub struct Lexed<'a> {
    pub tok: Tok,
    pub span: Span,
    pub text: &'a str,
}

pub fn lex(input: &str) -> Vec<Lexed<'_>> {
    let mut out = Vec::new();
    let mut l = Tok::lexer(input);
    while let Some(res) = l.next() {
        let range = l.span();
        if let Ok(tok) = res {
            out.push(Lexed{ tok, span: Span::new(range.start, range.len()), text: &input[range.clone()] });
        }
    }
    out
}
