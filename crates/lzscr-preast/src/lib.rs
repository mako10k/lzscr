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
    pub line: usize,
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
                    line: t.line,
                });
            }
            Tok::CommentBlock => {
                items.push(PreToken {
                    kind: PreTokenKind::CommentBlock(t.text.to_string()),
                    span: t.span,
                    line: t.line,
                });
            }
            _ => {
                items.push(PreToken {
                    kind: PreTokenKind::Token(t.text.to_string()),
                    span: t.span,
                    line: t.line,
                });
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
    let mut last_was_blank = false;
    let mut prev_line: Option<usize> = None;
    while idx < pre.items.len() {
        let it = &pre.items[idx];
        if prev_line.is_none() && it.line > 1 {
            if !last_was_blank {
                out.push('\n');
                let indent_spaces = indent_level * opts.indent;
                if indent_spaces == 0 {
                    col = 0;
                } else {
                    for _ in 0..indent_spaces {
                        out.push(' ');
                    }
                    col = indent_spaces;
                }
                at_line_start = true;
                prev_is_op = false;
                last_was_blank = true;
            }
        } else if let Some(pl) = prev_line {
            if it.line >= pl + 2 {
                if !at_line_start {
                    let indent_spaces = indent_level * opts.indent;
                    newline(indent_level, &mut col, &mut out, &mut group_stack);
                    if indent_spaces > 0 {
                        let new_len = out.len().saturating_sub(indent_spaces);
                        out.truncate(new_len);
                    }
                }
                out.push('\n');
                let indent_spaces = indent_level * opts.indent;
                if indent_spaces == 0 {
                    col = 0;
                } else {
                    for _ in 0..indent_spaces {
                        out.push(' ');
                    }
                    col = indent_spaces;
                }
                at_line_start = true;
                prev_is_op = false;
                last_was_blank = true;
            }
        }
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
                last_was_blank = false;
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
                last_was_blank = false;
            }
            PreTokenKind::Token(s) => {
                // Preserve whitespace-only tokens (including blank lines)
                if s.chars().all(|c| c.is_whitespace()) {
                    // If there are any newlines in this whitespace token, collapse
                    // consecutive blank-line runs to a single blank line.
                    let has_newline = s.chars().any(|c| c == '\n');
                    if has_newline {
                        if !last_was_blank {
                            out.push('\n');
                            let indent_spaces = indent_level * opts.indent;
                            if indent_spaces == 0 {
                                col = 0;
                            } else {
                                for _ in 0..indent_spaces {
                                    out.push(' ');
                                }
                                col = indent_spaces;
                            }
                            at_line_start = true;
                            prev_is_op = false;
                            last_was_blank = true;
                        }
                    }
                    idx += 1;
                    continue;
                }
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
                    indent_level = indent_level.saturating_sub(1);
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
                } else if is_op {
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
                    last_was_blank = false;
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
        prev_line = Some(it.line);
    }
    align_assignment_and_record_columns(out)
}
// Placeholder: actual PRE-AST -> AST conversion will live in parser crate using this token stream.

#[derive(Clone, Copy)]
enum AlignContextKind {
    Closure,
    Record,
}

#[derive(Clone, Copy)]
struct AlignEntry {
    line_idx: usize,
    byte_idx: usize,
    col: usize,
}

struct AlignContext {
    kind: AlignContextKind,
    entries: Vec<AlignEntry>,
}

fn align_assignment_and_record_columns(src: String) -> String {
    let mut lines: Vec<String> = src.split('\n').map(|s| s.to_string()).collect();
    let mut stack: Vec<AlignContext> = Vec::new();

    for line_idx in 0..lines.len() {
        let line = lines[line_idx].clone();
        let mut iter = line.char_indices().peekable();
        let mut char_col = 0usize;
        let mut last_non_ws: Option<char> = None;
        let mut in_string = false;
        let mut escape = false;

        while let Some((byte_idx, ch)) = iter.next() {
            if in_string {
                if escape {
                    escape = false;
                } else if ch == '\\' {
                    escape = true;
                } else if ch == '"' {
                    in_string = false;
                }
                char_col += 1;
                continue;
            }

            if ch == '"' {
                in_string = true;
                char_col += 1;
                continue;
            }

            if ch == '#' {
                break;
            }

            let curr_col = char_col;

            match ch {
                '{' => {
                    let kind = if matches!(last_non_ws, Some('!')) {
                        AlignContextKind::Closure
                    } else {
                        AlignContextKind::Record
                    };
                    stack.push(AlignContext { kind, entries: Vec::new() });
                }
                '}' => {
                    if let Some(ctx) = stack.pop() {
                        apply_alignment(&mut lines, ctx);
                    }
                }
                '=' => {
                    if let Some(ctx) = stack.last_mut() {
                        if matches!(ctx.kind, AlignContextKind::Closure)
                            && is_valid_assignment_eq(
                                &line,
                                byte_idx,
                                last_non_ws,
                                next_non_whitespace_char(&line, byte_idx + ch.len_utf8()),
                            )
                            && ctx.entries.iter().all(|e| e.line_idx != line_idx)
                        {
                            ctx.entries.push(AlignEntry { line_idx, byte_idx, col: curr_col });
                        }
                    }
                }
                ':' => {
                    if let Some(ctx) = stack.last_mut() {
                        if matches!(ctx.kind, AlignContextKind::Record)
                            && is_valid_record_colon(
                                &line,
                                byte_idx,
                                next_non_whitespace_char(&line, byte_idx + ch.len_utf8()),
                            )
                            && ctx.entries.iter().all(|e| e.line_idx != line_idx)
                        {
                            ctx.entries.push(AlignEntry { line_idx, byte_idx, col: curr_col });
                        }
                    }
                }
                _ => {}
            }

            if !ch.is_whitespace() {
                last_non_ws = Some(ch);
            }
            char_col = curr_col + 1;
        }
    }

    while let Some(ctx) = stack.pop() {
        apply_alignment(&mut lines, ctx);
    }

    lines.join("\n")
}

fn apply_alignment(lines: &mut [String], ctx: AlignContext) {
    if ctx.entries.len() < 2 {
        return;
    }
    let max_col = ctx.entries.iter().map(|e| e.col).max().unwrap_or(0);
    for entry in ctx.entries {
        if let Some(line) = lines.get_mut(entry.line_idx) {
            let diff = max_col.saturating_sub(entry.col);
            if diff == 0 {
                continue;
            }
            let (head, tail) = line.split_at(entry.byte_idx);
            let mut rebuilt = String::with_capacity(line.len() + diff);
            rebuilt.push_str(head);
            for _ in 0..diff {
                rebuilt.push(' ');
            }
            rebuilt.push_str(tail);
            *line = rebuilt;
        }
    }
}

fn is_valid_assignment_eq(
    line: &str,
    byte_idx: usize,
    prev_non_ws: Option<char>,
    next_non_ws: Option<char>,
) -> bool {
    if prev_non_ws.is_none() || !has_non_ws_after(line, byte_idx, 1) {
        return false;
    }
    if matches!(prev_non_ws, Some('=' | '<' | '>' | '!')) {
        return false;
    }
    if matches!(next_non_ws, Some('=' | '>')) {
        return false;
    }
    true
}

fn is_valid_record_colon(line: &str, byte_idx: usize, next_non_ws: Option<char>) -> bool {
    if !has_non_ws_before(line, byte_idx) || !has_non_ws_after(line, byte_idx, 1) {
        return false;
    }
    if matches!(next_non_ws, Some(':')) {
        return false;
    }
    true
}

fn has_non_ws_before(line: &str, idx: usize) -> bool {
    line[..idx].chars().any(|c| !c.is_whitespace())
}

fn has_non_ws_after(line: &str, idx: usize, len: usize) -> bool {
    let start = idx + len;
    if start >= line.len() {
        return false;
    }
    line[start..].chars().any(|c| !c.is_whitespace())
}

fn next_non_whitespace_char(line: &str, start: usize) -> Option<char> {
    if start >= line.len() {
        return None;
    }
    line[start..].chars().find(|c| !c.is_whitespace())
}
