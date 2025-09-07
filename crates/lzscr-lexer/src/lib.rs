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
    #[token("<-")]
    LeftArrow,
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

    // Char literal: '\'' with escapes and unicode (e.g., '\n', '\\', '\'', '\u{1F600}')
    #[regex(r"'(?:[^'\\]|\\u\{[0-9a-fA-F]+\}|\\.)'", parse_char)]
    Char(i32),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // Type variable like %a (captures the name without the leading '%')
    #[regex(r"%[a-zA-Z_][a-zA-Z0-9_]*", |lex| Some(lex.slice()[1..].to_string()))]
    TyVar(String),

    // Member-ish symbol like .println or .env, and special tuple operator symbol ",." used in desugaring
    #[token(".,", |lex| Some(lex.slice().to_string()))]
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

fn parse_char(lex: &mut Lexer<Tok>) -> Option<i32> {
    let s = lex.slice();
    // s is like '\x' or '\\' or '\u{...}' wrapped in single quotes
    if s.len() < 2 { return None; }
    let inner = &s[1..s.len()-1];
    let mut chars = inner.chars();
    let c = match chars.next()? {
        '\\' => {
            match chars.next()? {
                '\'' => '\'' as u32,
                '"' => '"' as u32,
                'n' => '\n' as u32,
                'r' => '\r' as u32,
                't' => '\t' as u32,
                '0' => '\0' as u32,
                'u' => {
                    // expect {HEX+}
                    if chars.next()? != '{' { return None; }
                    let mut hex = String::new();
                    for ch in chars {
                        if ch == '}' { break; }
                        hex.push(ch);
                    }
                    let v = u32::from_str_radix(hex.trim(), 16).ok()?;
                    v
                }
                other => other as u32,
            }
        }
        other => other as u32,
    };
    Some(c as i32)
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
