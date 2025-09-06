use logos::{Lexer, Logos};
use lzscr_ast::span::Span;

#[derive(Debug, Logos, PartialEq, Clone)]
pub enum Tok {
    // Whitespace is skipped
    #[regex(r"[ \t\r\n]+", logos::skip)]
    _Whitespace,

    // Comments are preserved in token stream
    #[regex(r"#[^\n]*")]
    CommentLine,
    #[regex(r"\{-", parse_block_comment)]
    CommentBlock,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    // Type annotation open: "%{" (close uses the regular RBrace '}')
    #[token("%{")]
    TypeOpen,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("^")]
    Caret,
    #[token("||")]
    PipePipe,
    #[token("|")]
    Pipe,
    #[token(":")]
    Colon,
    // Note: '==' must be matched before '=' to avoid ambiguity
    #[token("->")]
    Arrow,
    #[token("=")]
    Eq,
    #[token("@")]
    At,
    #[token("!")]
    Bang,
    #[token("\\")]
    Backslash,
    // For type holes like ?a
    #[token("?")]
    Question,
    // Infix operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    // Float-specific infix operators (distinct semantics)
    #[token(".+")]
    DotPlus,
    #[token(".-")]
    DotMinus,
    #[token(".*")]
    DotStar,
    #[token("./")]
    DotSlash,

    // Comparison operators (longer first)
    #[token("<=")]
    LessEq,
    #[token("<")]
    Less,
    #[token(">=")]
    GreaterEq,
    #[token(">")]
    Greater,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,

    // Float-specific comparison operators
    #[token(".<=")]
    DotLessEq,
    #[token(".<")]
    DotLess,
    #[token(".>=")]
    DotGreaterEq,
    #[token(".>")]
    DotGreater,

    #[token("~")]
    Tilde,

    // Float literals should be matched before Int to avoid consuming prefix digits
    #[regex(r"([0-9]+\.[0-9]*|\.[0-9]+)", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),

    #[regex(r#"\"([^"\\]|\\.)*\""#, parse_string)]
    Str(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // Type variable like 'a, 'foo (captures the name without leading apostrophe)
    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_]*", |lex| Some(lex.slice()[1..].to_string()))]
    TyVar(String),

    // Member-ish symbol like .println or .env
    #[regex(r"\.[a-zA-Z_][a-zA-Z0-9_]*", |lex| Some(lex.slice().to_string()))]
    Member(String),
}

fn parse_string(lex: &mut Lexer<Tok>) -> Option<String> {
    let s = lex.slice();
    let inner = &s[1..s.len() - 1];
    let mut out = String::new();
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(n) = chars.next() {
                out.push(n);
            } else {
                break;
            }
        } else {
            out.push(c);
        }
    }
    Some(out)
}

fn parse_block_comment(lex: &mut Lexer<Tok>) -> Option<()> {
    // We have just matched "{-"; consume with nesting until matching "-}"
    let s = lex.remainder();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    let mut depth = 1i32;
    while i + 1 < bytes.len() {
        if bytes[i] == b'{' && bytes[i + 1] == b'-' {
            depth += 1;
            i += 2;
            continue;
        }
        if bytes[i] == b'-' && bytes[i + 1] == b'}' {
            depth -= 1;
            i += 2;
            if depth == 0 {
                lex.bump(i);
                return Some(());
            }
            continue;
        }
        i += 1;
    }
    // Unterminated: consume the rest
    if i < bytes.len() {
        lex.bump(bytes.len());
    }
    Some(())
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
            out.push(Lexed {
                tok,
                span: Span::new(range.start, range.len()),
                text: &input[range.clone()],
            });
        }
    }
    out
}
