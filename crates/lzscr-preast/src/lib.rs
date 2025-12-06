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
                items.push(PreToken {
                    kind: PreTokenKind::CommentLine(t.text.to_string()),
                    span: t.span,
                });
            }
            Tok::CommentBlock => {
                items.push(PreToken {
                    kind: PreTokenKind::CommentBlock(t.text.to_string()),
                    span: t.span,
                });
            }
            _ => {
                items
                    .push(PreToken { kind: PreTokenKind::Token(t.text.to_string()), span: t.span });
            }
        }
    }
    Ok(PreAst { items })
}

fn needs_space_before(out: &str) -> bool {
    if out.is_empty() {
        return false;
    }
    let c = out.chars().last().unwrap();
    // Insert space after most tokens, but NOT after . ~ ! (they bind tightly to following identifier)
    matches!(c, ')' | ']' | '}' | '"' | '_' | '0'..='9' | 'a'..='z' | 'A'..='Z')
}

fn needs_space_before_token(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let c = s.chars().next().unwrap();
    // Insert space before most tokens (including . ~ !)
    matches!(c, '(' | '[' | '{' | '"' | '_' | '.' | '~' | '!' | '0'..='9' | 'a'..='z' | 'A'..='Z')
}

fn is_operator_token(s: &str) -> bool {
    matches!(
        s,
        "+" | "-"
            | "*"
            | "/"
            | ".+"
            | ".-"
            | ".*"
            | "./"
            | "<"
            | "<="
            | ">"
            | ">="
            | "=="
            | "!="
            | ".<"
            | ".<="
            | ".>"
            | ".>="
            | "||"
            | "|"
            | "->"
            | "="
            | "@"
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

pub fn preast_to_source(pre: &PreAst) -> String {
    preast_to_source_with_opts(pre, &FormatOpts::default())
}

/// Render PRE-AST back to source with formatting rules:
/// - Preserve comments and token order
/// - Insert single spaces between tokens where needed
pub fn preast_to_source_with_opts(pre: &PreAst, opts: &FormatOpts) -> String {
    #[derive(Debug, Clone, Copy)]
    struct GroupFrame {
        _delim: char,
        broken: bool,
    }

    let mut out = String::new();
    let mut col = 0usize;
    let mut indent_level = 0usize;
    let mut group_stack: Vec<GroupFrame> = Vec::new();
    let mut prev_is_op = false;
    let mut at_line_start = true;

    let push_str = |s: &str, col_ref: &mut usize, out_ref: &mut String| {
        out_ref.push_str(s);
        *col_ref += s.chars().count();
    };
    let newline = |indent_level: usize,
                   col_ref: &mut usize,
                   out_ref: &mut String,
                   group_stack: &mut Vec<GroupFrame>| {
        if let Some(top) = group_stack.last_mut() {
            top.broken = true;
        }
        out_ref.push('\n');
        for _ in 0..(indent_level * opts.indent) {
            out_ref.push(' ');
        }
        *col_ref = indent_level * opts.indent;
    };

    let mut idx = 0usize;
    while idx < pre.items.len() {
        let it = &pre.items[idx];
        let next_token = pre.items.get(idx + 1);
        match &it.kind {
            PreTokenKind::CommentLine(s) => {
                let trimmed = s.trim_end();
                if at_line_start {
                    push_str(trimmed, &mut col, &mut out);
                    newline(indent_level, &mut col, &mut out, &mut group_stack);
                } else {
                    let need = 1 + trimmed.chars().count();
                    if col + need <= opts.max_width {
                        if !out.ends_with(' ') {
                            out.push(' ');
                            col += 1;
                        }
                        push_str(trimmed, &mut col, &mut out);
                        newline(indent_level, &mut col, &mut out, &mut group_stack);
                    } else {
                        newline(indent_level, &mut col, &mut out, &mut group_stack);
                        push_str(trimmed, &mut col, &mut out);
                        newline(indent_level, &mut col, &mut out, &mut group_stack);
                    }
                }
                prev_is_op = false;
                at_line_start = true;
            }
            PreTokenKind::CommentBlock(s) => {
                let trimmed = s;
                if needs_space_before(&out) {
                    out.push(' ');
                    col += 1;
                }
                push_str(trimmed, &mut col, &mut out);
                prev_is_op = false;
                at_line_start = false;
            }
            PreTokenKind::Token(s) => {
                let token_len = s.chars().count();
                let is_op = is_operator_token(s);
                let is_semi = s == ";";
                let is_lparen = s == "(";
                let is_rparen = s == ")";
                let is_lbrace = s == "{";
                let is_rbrace = s == "}";
                let is_lbracket = s == "[";
                let is_rbracket = s == "]";
                let in_group = !group_stack.is_empty();
                let is_closer = is_rbrace || is_rbracket || is_rparen;

                if is_closer {
                    if let Some(top) = group_stack.last() {
                        if top.broken && !at_line_start {
                            let target_indent = indent_level.saturating_sub(1);
                            newline(target_indent, &mut col, &mut out, &mut group_stack);
                            at_line_start = true;
                        }
                    }
                    if indent_level > 0 {
                        indent_level -= 1;
                    }
                    if at_line_start {
                        col = indent_level * opts.indent;
                    }
                }

                if !at_line_start
                    && indent_level > 0
                    && col + token_len + 1 > opts.max_width
                    && !is_closer
                {
                    newline(indent_level, &mut col, &mut out, &mut group_stack);
                    at_line_start = true;
                }

                if s == ":" {
                    if !at_line_start {
                        while out.ends_with(' ') {
                            out.pop();
                            col = col.saturating_sub(1);
                        }
                    }
                    push_str(":", &mut col, &mut out);
                    out.push(' ');
                    col += 1;
                    prev_is_op = false;
                    at_line_start = false;
                } else if s == "," {
                    if !at_line_start {
                        while out.ends_with(' ') {
                            out.pop();
                            col = col.saturating_sub(1);
                        }
                    }
                    push_str(",", &mut col, &mut out);
                    if in_group {
                        newline(indent_level, &mut col, &mut out, &mut group_stack);
                        at_line_start = true;
                    } else {
                        out.push(' ');
                        col += 1;
                        at_line_start = false;
                    }
                    prev_is_op = false;
                } else {
                    if is_op {
                        if !at_line_start && needs_space_before(&out) {
                            out.push(' ');
                            col += 1;
                        }
                        push_str(s, &mut col, &mut out);
                        out.push(' ');
                        col += 1;
                        prev_is_op = true;
                        at_line_start = false;
                    } else {
                        if prev_is_op {
                            if !out.ends_with(' ') {
                                out.push(' ');
                                col += 1;
                            }
                        } else if !at_line_start
                            && needs_space_before(&out)
                            && needs_space_before_token(s)
                            && !out.ends_with(' ')
                        {
                            out.push(' ');
                            col += 1;
                        }
                        push_str(s, &mut col, &mut out);
                        prev_is_op = false;
                        at_line_start = false;
                    }
                }

                if is_lparen || is_lbrace || is_lbracket {
                    group_stack
                        .push(GroupFrame { _delim: s.chars().next().unwrap(), broken: false });
                    indent_level += 1;
                    if is_lbrace {
                        if let Some(next) = next_token {
                            let is_next_rbrace =
                                matches!(&next.kind, PreTokenKind::Token(t) if t == "}");
                            if !is_next_rbrace {
                                newline(indent_level, &mut col, &mut out, &mut group_stack);
                                at_line_start = true;
                            }
                        }
                    }
                }
                if (is_rparen || is_rbrace || is_rbracket) && !group_stack.is_empty() {
                    group_stack.pop();
                }
                if is_semi {
                    newline(indent_level, &mut col, &mut out, &mut group_stack);
                    at_line_start = true;
                }
            }
        }
        idx += 1;
    }
    out
}
// Placeholder: actual PRE-AST -> AST conversion will live in parser crate using this token stream.
