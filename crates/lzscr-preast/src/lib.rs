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
    preast_to_source_with_opts(pre, &FormatOpts::default())
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

#[derive(Debug, Clone, Copy)]
pub struct FormatOpts {
    pub indent: usize,
    pub max_width: usize,
}

impl Default for FormatOpts {
    fn default() -> Self {
        Self { indent: 2, max_width: 100 }
    }
}

pub fn preast_to_source_with_opts(pre: &PreAst, opts: &FormatOpts) -> String {
    // Simple formatter: attach line comments to previous token line if room, else own line.
    // Insert breaks after semicolons and before certain constructs; indent subsequent lines.
    let mut out = String::new();
    let mut col = 0usize;
    let mut indent_level = 0usize;
    let mut prev_is_op = false;
    let mut at_line_start = true;

    let push_str = |s: &str, col_ref: &mut usize, out_ref: &mut String| {
        out_ref.push_str(s);
        *col_ref += s.chars().count();
    };
    let newline = |indent_level: usize, col_ref: &mut usize, out_ref: &mut String| {
        out_ref.push('\n');
        for _ in 0..(indent_level * opts.indent) {
            out_ref.push(' ');
        }
        *col_ref = indent_level * opts.indent;
    };

    for it in &pre.items {
        match &it.kind {
            PreTokenKind::CommentLine(s) => {
                let trimmed = s.trim_end();
                if at_line_start {
                    // start of line: print as-is
                    push_str(trimmed, &mut col, &mut out);
                    out.push('\n');
                    for _ in 0..(indent_level * opts.indent) { out.push(' '); }
                    col = indent_level * opts.indent;
                } else {
                    // if fits on current line, attach; else put on its own line
                    let need = 1 + trimmed.chars().count();
                    if col + need <= opts.max_width {
                        if !out.ends_with(' ') { out.push(' '); col += 1; }
                        push_str(trimmed, &mut col, &mut out);
                        out.push('\n');
                        for _ in 0..(indent_level * opts.indent) { out.push(' '); }
                        col = indent_level * opts.indent;
                    } else {
                        newline(indent_level, &mut col, &mut out);
                        push_str(trimmed, &mut col, &mut out);
                        out.push('\n');
                        for _ in 0..(indent_level * opts.indent) { out.push(' '); }
                        col = indent_level * opts.indent;
                    }
                }
                prev_is_op = false;
                at_line_start = true;
            }
            PreTokenKind::CommentBlock(s) => {
                let trimmed = s;
                if needs_space_before(&out) { out.push(' '); col += 1; }
                push_str(trimmed, &mut col, &mut out);
                prev_is_op = false;
                at_line_start = false;
            }
            PreTokenKind::Token(s) => {
                let is_op = is_operator_token(s);
                let is_semi = s == ";";
                let is_lparen = s == "(";
                let is_rparen = s == ")";
                if is_op {
                    if needs_space_before(&out) { out.push(' '); col += 1; }
                    push_str(s, &mut col, &mut out);
                    out.push(' '); col += 1;
                    prev_is_op = true;
                    at_line_start = false;
                } else {
                    if prev_is_op {
                        if !out.ends_with(' ') { out.push(' '); col += 1; }
                    } else if needs_space_before(&out) && needs_space_before_token(s) {
                        out.push(' '); col += 1;
                    }
                    push_str(s, &mut col, &mut out);
                    prev_is_op = false;
                    at_line_start = false;
                }
                if is_lparen { indent_level += 1; }
                if is_rparen && indent_level > 0 { indent_level -= 1; }
                if is_semi {
                    // statement boundary: break line and indent next
                    newline(indent_level, &mut col, &mut out);
                    at_line_start = true;
                }
            }
        }
    }
    out
}

// Placeholder: actual PRE-AST -> AST conversion will live in parser crate using this token stream.
