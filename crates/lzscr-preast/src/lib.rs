use lzscr_ast::span::Span;
use lzscr_lexer::{lex, Tok};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum PreTokenKind {
    CommentLine(String),
    CommentBlock(String),
    Token(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PreToken {
    pub kind: PreTokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PreAst {
    pub items: Vec<PreToken>,
}

#[derive(Debug, Error)]
pub enum PreAstError {
    #[error("lex error: {0}")]
    Lex(String),
}

pub fn to_preast(src: &str) -> Result<PreAst, PreAstError> {
    let toks = lex(src);
    let mut items = Vec::new();
    for t in toks {
        match t.tok {
            Tok::CommentLine => {
                items.push(PreToken { kind: PreTokenKind::CommentLine(t.text.to_string()), span: t.span });
            }
            Tok::CommentBlock => {
                items.push(PreToken { kind: PreTokenKind::CommentBlock(t.text.to_string()), span: t.span });
            }
            _ => {
                items.push(PreToken { kind: PreTokenKind::Token(t.text.to_string()), span: t.span });
            }
        }
    }
    Ok(PreAst { items })
}

/// Render PRE-AST back to source with formatting rules:
/// - Preserve comments and token order
/// - Insert single spaces between tokens where needed
pub fn preast_to_source(pre: &PreAst) -> String {
    let mut out = String::new();
    let mut prev_is_op = false;
    for it in &pre.items {
        match &it.kind {
            PreTokenKind::CommentLine(s) => {
                if !out.ends_with('\n') && !out.is_empty() {
                    out.push('\n');
                }
                out.push_str(s);
                out.push('\n');
                prev_is_op = false;
            }
            PreTokenKind::CommentBlock(s) => {
                if needs_space_before(&out) { out.push(' '); }
                out.push_str(s);
                prev_is_op = false;
            }
            PreTokenKind::Token(s) => {
                let is_op = is_operator_token(s);
                if is_op {
                    if needs_space_before(&out) { out.push(' '); }
                    out.push_str(s);
                    out.push(' ');
                    prev_is_op = true;
                } else {
                    if prev_is_op {
                        // ensure space after operator
                        if !out.ends_with(' ') { out.push(' '); }
                    } else if needs_space_before(&out) && needs_space_before_token(s) {
                        out.push(' ');
                    }
                    out.push_str(s);
                    prev_is_op = false;
                }
            }
        }
    }
    out
}

fn needs_space_before(out: &str) -> bool {
    if out.is_empty() { return false; }
    let c = out.chars().last().unwrap();
    matches!(c, ')' | ']' | '}' | '"' | '_' | '.' | '0'..='9' | 'a'..='z' | 'A'..='Z')
}

fn needs_space_before_token(s: &str) -> bool {
    if s.is_empty() { return false; }
    let c = s.chars().next().unwrap();
    matches!(c, '(' | '[' | '{' | '"' | '_' | '.' | '0'..='9' | 'a'..='z' | 'A'..='Z')
}

fn is_operator_token(s: &str) -> bool {
    matches!(
        s,
        "+" | "-" | "*" | "/" | ".+" | ".-" | ".*" | "./" |
        "<" | "<=" | ">" | ">=" | "==" | "!=" |
        ".<" | ".<=" | ".>" | ".>=" |
        "||" | "|" | ":" | "->" | "=" | "@"
    )
}

// Placeholder: actual PRE-AST -> AST conversion will live in parser crate using this token stream.
