use lzscr_ast::{ast::*, span::Span};
use lzscr_lexer::{lex, Tok};
// Reduce clippy::type-complexity by aliasing sum alternatives type
type SumAlts = Vec<(lzscr_ast::ast::Tag, Vec<TypeExpr>)>;
use lzscr_preast as preast;

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("parse error: {0}")]
    Generic(String),
    #[error("parse error: {msg} at ({span_offset}, {span_len})")]
    WithSpan { msg: String, span_offset: usize, span_len: usize },
    #[error(
        "parse error: {msg} at1 ({span1_offset}, {span1_len}); at2 ({span2_offset}, {span2_len})"
    )]
    WithSpan2 {
        msg: String,
        span1_offset: usize,
        span1_len: usize,
        span2_offset: usize,
        span2_len: usize,
    },
}

pub fn parse_expr(src: &str) -> Result<Expr, ParseError> {
    // Phase 0: PRE-AST (preserve comments, prepare token stream)
    let _pre = preast::to_preast(src).map_err(|e| ParseError::Generic(e.to_string()))?;
    // Minimal, direct token-based parser for a subset: ints, strings, refs, idents, lambda, apply, block
    let mut tokens = lex(src);
    // Drop comments for parsing; they are handled in PRE-AST/formatter layer
    tokens.retain(|t| !matches!(t.tok, Tok::CommentLine | Tok::CommentBlock));
    let mut i = 0usize;

    // line/col now supplied by lexer tokens; no local computation needed
    // Small token helpers
    fn peek<'a>(
        i: usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Option<&'a lzscr_lexer::Lexed<'a>> {
        toks.get(i)
    }
    fn bump<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Option<&'a lzscr_lexer::Lexed<'a>> {
        let t = toks.get(*i);
        if t.is_some() {
            *i += 1;
        }
        t
    }

    // Pattern parsing -----------------------------------------------------
    fn parse_pat_atom<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Pattern, ParseError> {
        let t =
            bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF in pattern".into()))?;
        let pat = match &t.tok {
            Tok::TypeOpen => {
                let start_off = t.span.offset;
                let mut tvars: Vec<String> = Vec::new();
                loop {
                    if let Some(nxt) = peek(*i, toks) {
                        if matches!(nxt.tok, Tok::RBrace) {
                            let _ = bump(i, toks);
                            break;
                        }
                    } else {
                        return Err(ParseError::WithSpan {
                            msg: "} expected to close %{ in pattern".into(),
                            span_offset: t.span.offset,
                            span_len: t.span.len,
                        });
                    }
                    let tv = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                        msg: "expected %a in %{ ... } pattern binder".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    })?;
                    match &tv.tok {
                        Tok::TyVar(name) => tvars.push(name.clone()),
                        _ => {
                            return Err(ParseError::WithSpan {
                                msg: "expected %a in %{ ... } pattern binder".into(),
                                span_offset: tv.span.offset,
                                span_len: tv.span.len,
                            });
                        }
                    }
                    let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                        msg: ", or } expected in %{ ... } pattern binder".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    })?;
                    match sep.tok {
                        Tok::Comma => continue,
                        Tok::RBrace => break,
                        _ => {
                            return Err(ParseError::WithSpan {
                                msg: ", or } expected in %{ ... } pattern binder".into(),
                                span_offset: sep.span.offset,
                                span_len: sep.span.len,
                            });
                        }
                    }
                }
                let inner = parse_pattern(i, toks)?;
                let end_off = inner.span.offset + inner.span.len;
                Pattern::new(
                    PatternKind::TypeBind { tvars, pat: Box::new(inner) },
                    Span::new(start_off, end_off - start_off),
                )
            }
            Tok::Tilde => {
                let id = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                    msg: "expected ident after ~ in pattern".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })?;
                match id.tok {
                    Tok::Ident => Pattern::new(
                        PatternKind::Var(id.text.to_string()),
                        Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset),
                    ),
                    _ => {
                        return Err(ParseError::WithSpan {
                            msg: "expected ident after ~ in pattern".into(),
                            span_offset: id.span.offset,
                            span_len: id.span.len,
                        })
                    }
                }
            }
            Tok::LParen => {
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RParen) {
                        let r = bump(i, toks).unwrap();
                        return Ok(Pattern::new(
                            PatternKind::Unit,
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        ));
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: ") expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
                let first = parse_pattern(i, toks)?;
                if let Some(n2) = peek(*i, toks) {
                    if matches!(n2.tok, Tok::Comma) {
                        let _ = bump(i, toks);
                        let mut items = vec![first];
                        loop {
                            let p2 = parse_pattern(i, toks)?;
                            items.push(p2);
                            let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: ") expected in tuple pattern".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RParen => break,
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected , or ) in tuple pattern".into(),
                                        span_offset: sep.span.offset,
                                        span_len: sep.span.len,
                                    })
                                }
                            }
                        }
                        let end = toks
                            .get(*i - 1)
                            .map(|r| r.span.offset + r.span.len)
                            .unwrap_or(t.span.offset + t.span.len);
                        Pattern::new(
                            PatternKind::Tuple(items),
                            Span::new(t.span.offset, end - t.span.offset),
                        )
                    } else if matches!(n2.tok, Tok::RParen) {
                        let rp = bump(i, toks).unwrap();
                        let mut p = first;
                        p.span =
                            Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                        p
                    } else {
                        return Err(ParseError::WithSpan {
                            msg: "expected , or ) in tuple/group pattern".into(),
                            span_offset: n2.span.offset,
                            span_len: n2.span.len,
                        });
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: ") expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
            }
            Tok::LBracket => {
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RBracket) {
                        let r = bump(i, toks).unwrap();
                        Pattern::new(
                            PatternKind::List(vec![]),
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        )
                    } else {
                        let mut items = Vec::new();
                        loop {
                            let p = parse_pattern(i, toks)?;
                            items.push(p);
                            let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: "] or , expected in list pattern".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RBracket => break,
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected , or ] in list pattern".into(),
                                        span_offset: sep.span.offset,
                                        span_len: sep.span.len,
                                    });
                                }
                            }
                        }
                        let end = toks
                            .get(*i - 1)
                            .map(|r| r.span.offset + r.span.len)
                            .unwrap_or(t.span.offset + t.span.len);
                        Pattern::new(
                            PatternKind::List(items),
                            Span::new(t.span.offset, end - t.span.offset),
                        )
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: "] expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
            }
            Tok::LBrace => {
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RBrace) {
                        let r = bump(i, toks).unwrap();
                        Pattern::new(
                            PatternKind::Record(vec![]),
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        )
                    } else {
                        let mut fields: Vec<lzscr_ast::ast::PatternRecordField> = Vec::new();
                        loop {
                            let k = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: "expected key in record pattern".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            let key = match &k.tok {
                                Tok::Ident => k.text.to_string(),
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected ident key in record pattern".into(),
                                        span_offset: k.span.offset,
                                        span_len: k.span.len,
                                    })
                                }
                            };
                            let key_span = k.span; // Phase 5: Capture field name span
                            let col = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: ": expected after key in record pattern".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            if !matches!(col.tok, Tok::Colon) {
                                return Err(ParseError::WithSpan {
                                    msg: ": expected after key in record pattern".into(),
                                    span_offset: col.span.offset,
                                    span_len: col.span.len,
                                });
                            }
                            let p = parse_pattern(i, toks)?;
                            fields.push(lzscr_ast::ast::PatternRecordField::new(key, key_span, p));
                            let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: ", or } expected in record pattern".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RBrace => break,
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected , or } in record pattern".into(),
                                        span_offset: sep.span.offset,
                                        span_len: sep.span.len,
                                    })
                                }
                            }
                        }
                        let end = toks
                            .get(*i - 1)
                            .map(|r| r.span.offset + r.span.len)
                            .unwrap_or(t.span.offset + t.span.len);
                        Pattern::new(
                            PatternKind::Record(fields),
                            Span::new(t.span.offset, end - t.span.offset),
                        )
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: "} expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
            }
            Tok::Int(n) => Pattern::new(PatternKind::Int(*n), t.span),
            Tok::Float(f) => Pattern::new(PatternKind::Float(*f), t.span),
            Tok::Str(s) => Pattern::new(PatternKind::Str(s.clone()), t.span),
            Tok::Char(c) => Pattern::new(PatternKind::Char(*c), t.span),
            Tok::Ident => {
                if t.text == "_" {
                    Pattern::new(PatternKind::Wildcard, t.span)
                } else {
                    let name = t.text.to_string();
                    let mut args = Vec::new();
                    loop {
                        let Some(nxt) = toks.get(*i) else { break };
                        match nxt.tok {
                            Tok::Arrow | Tok::RParen | Tok::Comma | Tok::Eq | Tok::Semicolon => {
                                break
                            }
                            Tok::Tilde
                            | Tok::Ident
                            | Tok::Member(_)
                            | Tok::LBracket
                            | Tok::LBrace
                            | Tok::LParen
                            | Tok::Int(_)
                            | Tok::Float(_)
                            | Tok::Str(_)
                            | Tok::Char(_) => {
                                let a = parse_pat_atom(i, toks)?;
                                args.push(a);
                            }
                            _ => break,
                        }
                    }
                    let end = if args.is_empty() {
                        t.span.offset + t.span.len
                    } else {
                        let last = args.last().unwrap();
                        last.span.offset + last.span.len
                    };
                    Pattern::new(
                        PatternKind::Ctor { name, args },
                        Span::new(t.span.offset, end - t.span.offset),
                    )
                }
            }
            Tok::Member(m) => Pattern::new(PatternKind::Symbol(m.clone()), t.span),
            _ => {
                return Err(ParseError::WithSpan {
                    msg: "unexpected token in pattern".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })
            }
        };
        Ok(pat)
    }

    fn parse_pattern<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Pattern, ParseError> {
        // baseline atom
        let mut left = parse_pat_atom(i, toks)?;
        // cons right-assoc
        loop {
            if let Some(nxt) = toks.get(*i) {
                if matches!(nxt.tok, Tok::Colon) {
                    let _ = bump(i, toks);
                    let right = parse_pattern(i, toks)?;
                    let sp = Span::new(
                        left.span.offset,
                        right.span.offset + right.span.len - left.span.offset,
                    );
                    left = Pattern::new(PatternKind::Cons(Box::new(left), Box::new(right)), sp);
                    continue;
                }
            }
            break;
        }
        // As pattern: '@' pat
        if let Some(nxt) = toks.get(*i) {
            if matches!(nxt.tok, Tok::At) {
                let _ = bump(i, toks);
                let right = parse_pattern(i, toks)?;
                let sp = Span::new(
                    left.span.offset,
                    right.span.offset + right.span.len - left.span.offset,
                );
                return Ok(Pattern::new(PatternKind::As(Box::new(left), Box::new(right)), sp));
            }
        }
        Ok(left)
    }

    // Variant for lambda/let parameter head: treat top-level spaces as parameter separators.
    // Key difference: when seeing a constructor token (e.g., `Foo`), do NOT greedily parse
    // subsequent atoms as its payload at this level. Users can still write complex payload
    // patterns by wrapping them in parentheses: `(Foo x y)`.
    fn parse_pat_atom_param<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Pattern, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("expected pattern".into()))?;
        let pat = match &t.tok {
            Tok::TypeOpen => {
                // Delegate to full pattern parser (supports %{...} type-binding)
                *i -= 1; // step back to include this token
                parse_pattern(i, toks)?
            }
            Tok::Tilde => {
                let id = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                    msg: "expected ident after ~ in pattern".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })?;
                match id.tok {
                    Tok::Ident => Pattern::new(
                        PatternKind::Var(id.text.to_string()),
                        Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset),
                    ),
                    _ => {
                        return Err(ParseError::WithSpan {
                            msg: "expected ident after ~ in pattern".into(),
                            span_offset: id.span.offset,
                            span_len: id.span.len,
                        })
                    }
                }
            }
            Tok::LParen => {
                // Delegate to the full pattern atom parser so tuple patterns like (~a, ~b) work.
                // Step back to include '(', then reuse parse_pat_atom which knows about commas inside.
                *i -= 1;
                return parse_pat_atom(i, toks);
            }
            Tok::LBracket => {
                // list literal in pattern param context: reuse existing logic via small loop
                if let Some(nxt) = toks.get(*i) {
                    if matches!(nxt.tok, Tok::RBracket) {
                        let r = bump(i, toks).unwrap();
                        Pattern::new(
                            PatternKind::List(vec![]),
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        )
                    } else {
                        // parse [p1, p2, ...]
                        let mut items = Vec::new();
                        loop {
                            let p = parse_pattern(i, toks)?;
                            items.push(p);
                            let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                                msg: "] or , expected".into(),
                                span_offset: t.span.offset,
                                span_len: t.span.len,
                            })?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RBracket => {
                                    break;
                                }
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected , or ] in list".into(),
                                        span_offset: sep.span.offset,
                                        span_len: sep.span.len,
                                    })
                                }
                            }
                        }
                        // compute span end
                        let end = toks
                            .get(*i - 1)
                            .map(|x| x.span.offset + x.span.len)
                            .unwrap_or(t.span.offset + t.span.len);
                        Pattern::new(
                            PatternKind::List(items),
                            Span::new(t.span.offset, end - t.span.offset),
                        )
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: "] expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
            }
            Tok::LBrace => {
                // record pattern: delegate to full parser inside braces
                if let Some(nxt) = toks.get(*i) {
                    if matches!(nxt.tok, Tok::RBrace) {
                        let r = bump(i, toks).unwrap();
                        Pattern::new(
                            PatternKind::Record(vec![]),
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        )
                    } else {
                        // Fall back to full parse by rewinding one token and using parse_pattern
                        // so record fields are handled; we can't easily duplicate record logic here.
                        // We reconstruct by stepping back and calling parse_pattern, then slicing span.
                        // Step back index
                        *i -= 1;
                        return parse_pattern(i, toks);
                    }
                } else {
                    return Err(ParseError::WithSpan {
                        msg: "} expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
            }
            Tok::Int(n) => Pattern::new(PatternKind::Int(*n), t.span),
            Tok::Float(f) => Pattern::new(PatternKind::Float(*f), t.span),
            Tok::Str(s) => Pattern::new(PatternKind::Str(s.clone()), t.span),
            Tok::Char(c) => Pattern::new(PatternKind::Char(*c), t.span),
            Tok::Ident => {
                if t.text == "_" {
                    Pattern::new(PatternKind::Wildcard, t.span)
                } else {
                    Pattern::new(
                        PatternKind::Ctor { name: t.text.to_string(), args: vec![] },
                        t.span,
                    )
                }
            }
            Tok::Member(m) => Pattern::new(PatternKind::Symbol(m.clone()), t.span),
            _ => {
                return Err(ParseError::WithSpan {
                    msg: "unexpected token in pattern".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })
            }
        };
        Ok(pat)
    }

    fn parse_pattern_param<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Pattern, ParseError> {
        // baseline atom with param-head semantics
        let mut left = parse_pat_atom_param(i, toks)?;
        // allow cons pattern chaining within a single parameter using ':'
        loop {
            if let Some(nxt) = toks.get(*i) {
                if matches!(nxt.tok, Tok::Colon) {
                    let _ = bump(i, toks);
                    // right side can be full pattern inside the same parameter only if parenthesized; to keep
                    // semantics simple in param context, continue using parse_pattern_param recursively.
                    let right = parse_pattern_param(i, toks)?;
                    let sp = Span::new(
                        left.span.offset,
                        right.span.offset + right.span.len - left.span.offset,
                    );
                    left = Pattern::new(PatternKind::Cons(Box::new(left), Box::new(right)), sp);
                    continue;
                }
            }
            break;
        }
        // As pattern: '@' pat
        if let Some(nxt) = toks.get(*i) {
            if matches!(nxt.tok, Tok::At) {
                let _ = bump(i, toks);
                let right = parse_pattern_param(i, toks)?;
                let sp = Span::new(
                    left.span.offset,
                    right.span.offset + right.span.len - left.span.offset,
                );
                return Ok(Pattern::new(PatternKind::As(Box::new(left), Box::new(right)), sp));
            }
        }
        Ok(left)
    }

    // Let-LHS sugar: ~name p1 p2 ... =
    fn try_parse_lhs_param_chain<'a>(
        j: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Option<(String, Vec<Pattern>, usize)> {
        let save = *j;
        let t0 = toks.get(*j)?;
        if !matches!(t0.tok, Tok::Tilde) {
            return None;
        }
        *j += 1;
        let id = toks.get(*j)?;
        if !matches!(id.tok, Tok::Ident) {
            *j = save;
            return None;
        }
        let name = id.text.to_string();
        *j += 1;
        // Pre-scan: ensure there is a top-level '=' ahead before any arrow/pipe/closer; otherwise not a let-head.
        {
            let mut k = *j;
            let mut depth = 0i32;
            let mut found_eq = false;
            while let Some(tok) = toks.get(k) {
                match tok.tok {
                    Tok::LParen | Tok::LBracket | Tok::LBrace => depth += 1,
                    Tok::RParen | Tok::RBracket | Tok::RBrace => {
                        if depth == 0 {
                            break;
                        }
                        depth -= 1;
                    }
                    Tok::Arrow | Tok::Pipe | Tok::PipePipe | Tok::Semicolon => {
                        if depth == 0 {
                            break;
                        }
                    }
                    Tok::Eq => {
                        if depth == 0 {
                            found_eq = true;
                            break;
                        }
                    }
                    _ => {}
                }
                k += 1;
            }
            if !found_eq {
                *j = save;
                return None;
            }
        }
        let mut params: Vec<Pattern> = Vec::new();
        loop {
            if let Some(nxt) = toks.get(*j) {
                if matches!(nxt.tok, Tok::Eq) {
                    break;
                }
            }
            let before = *j;
            if let Ok(p) = parse_pattern_param(j, toks) {
                params.push(p);
            } else {
                *j = before;
                break;
            }
        }
        if let Some(eq) = toks.get(*j) {
            if matches!(eq.tok, Tok::Eq) {
                return Some((name, params, *j));
            }
        }
        *j = save;
        None
    }

    fn collect_binders(p: &Pattern, names: &mut Vec<String>) {
        match &p.kind {
            PatternKind::Var(n) => names.push(n.clone()),
            PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                for x in xs {
                    collect_binders(x, names);
                }
            }
            PatternKind::Cons(h, t) => {
                collect_binders(h, names);
                collect_binders(t, names);
            }
            PatternKind::Record(fields) => {
                for f in fields {
                    collect_binders(&f.pattern, names);
                }
            }
            PatternKind::As(a, b) => {
                collect_binders(a, names);
                collect_binders(b, names);
            }
            PatternKind::Ctor { args, .. } => {
                for a in args {
                    collect_binders(a, names);
                }
            }
            PatternKind::TypeBind { pat, .. } => collect_binders(pat, names),
            _ => {}
        }
    }

    // Build nested lambdas from params and a body: \p1 p2 ... -> body
    fn nest_lambdas(params: &[Pattern], body: Expr) -> Expr {
        let mut acc = body;
        for p in params.iter().cloned().rev() {
            let span = Span::new(p.span.offset, acc.span.offset + acc.span.len - p.span.offset);
            acc = Expr::new(ExprKind::Lambda { param: p, body: Box::new(acc) }, span);
        }
        acc
    }

    // Global type parser used in type declarations and annotations
    fn parse_type_expr<'a>(
        j: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<TypeExpr, ParseError> {
        fn is_type_atom_start(tok: &Tok) -> bool {
            matches!(
                tok,
                Tok::Ident
                    | Tok::Member(_)
                    | Tok::TyVar(_)
                    | Tok::LBracket
                    | Tok::LParen
                    | Tok::LBrace
                    | Tok::Question
            )
        }

        fn ctor_from_name<'b>(
            j: &mut usize,
            toks: &'b [lzscr_lexer::Lexed<'b>],
            raw: &str,
            span: Span,
        ) -> Result<TypeExpr, ParseError> {
            let mut args = Vec::new();
            loop {
                let Some(nxt) = toks.get(*j) else { break };
                if !is_type_atom_start(&nxt.tok) {
                    break;
                }
                let arg = parse_type_expr_bp(j, toks, 6)?;
                args.push(arg);
            }
            let tag = raw;
            let is_member = tag.starts_with('.');
            let bare = tag.strip_prefix('.').unwrap_or(tag);
            if is_member {
                match bare {
                    "Unit" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Unit` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Unit);
                    }
                    "Int" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Int` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Int);
                    }
                    "Float" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Float` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Float);
                    }
                    "Bool" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Bool` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Bool);
                    }
                    "Str" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Str` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Str);
                    }
                    "Char" => {
                        if !args.is_empty() {
                            return Err(ParseError::WithSpan {
                                msg: "`Char` does not take type arguments".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::Char);
                    }
                    "List" => {
                        if args.len() != 1 {
                            return Err(ParseError::WithSpan {
                                msg: "`List` expects exactly one type argument".into(),
                                span_offset: span.offset,
                                span_len: span.len,
                            });
                        }
                        return Ok(TypeExpr::List(Box::new(args.into_iter().next().unwrap())));
                    }
                    "Tuple" => return Ok(TypeExpr::Tuple(args)),
                    _ => {
                        let tag_enum = lzscr_ast::ast::Tag::Name(bare.to_string());
                        return Ok(TypeExpr::Ctor { tag: tag_enum, args });
                    }
                }
            }
            // Non-member tags (no leading dot): accept builtins too.
            match bare {
                "Unit" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Unit` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Unit)
                }
                "Int" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Int` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Int)
                }
                "Float" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Float` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Float)
                }
                "Bool" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Bool` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Bool)
                }
                "Str" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Str` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Str)
                }
                "Char" => {
                    if !args.is_empty() {
                        return Err(ParseError::WithSpan {
                            msg: "`Char` does not take type arguments".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::Char)
                }
                "List" => {
                    if args.len() != 1 {
                        return Err(ParseError::WithSpan {
                            msg: "`List` expects exactly one type argument".into(),
                            span_offset: span.offset,
                            span_len: span.len,
                        });
                    }
                    Ok(TypeExpr::List(Box::new(args.into_iter().next().unwrap())))
                }
                "Tuple" => Ok(TypeExpr::Tuple(args)),
                _ => {
                    let tag_enum = lzscr_ast::ast::Tag::Name(bare.to_string());
                    Ok(TypeExpr::Ctor { tag: tag_enum, args })
                }
            }
        }

        fn parse_type_atom<'b>(
            j: &mut usize,
            toks: &'b [lzscr_lexer::Lexed<'b>],
        ) -> Result<TypeExpr, ParseError> {
            // Try sum-of-ctors before consuming a token: if the position starts a sum,
            // parse into TypeExpr::Sum and return early.
            let save_j = *j;
            if let Some(sum_alts) = try_parse_sum_alts(j, toks)? {
                return Ok(TypeExpr::Sum(sum_alts));
            }
            *j = save_j;

            let t = bump(j, toks).ok_or_else(|| ParseError::Generic("expected type".into()))?;
            Ok(match &t.tok {
                Tok::Member(name) => ctor_from_name(j, toks, name, t.span)?,
                Tok::TyVar(name) => TypeExpr::Var(name.clone()),
                Tok::Ident => ctor_from_name(j, toks, t.text, t.span)?,
                Tok::LBracket => {
                    let inner = parse_type_expr(j, toks)?;
                    let rb = bump(j, toks)
                        .ok_or_else(|| ParseError::Generic("] expected in type".into()))?;
                    if !matches!(rb.tok, Tok::RBracket) {
                        return Err(ParseError::Generic("] expected in type".into()));
                    }
                    TypeExpr::List(Box::new(inner))
                }
                Tok::LParen => {
                    if let Some(nxt) = toks.get(*j) {
                        if matches!(nxt.tok, Tok::RParen) {
                            let _ = bump(j, toks);
                            return Ok(TypeExpr::Tuple(vec![]));
                        }
                    }
                    // Attempt to parse a parenthesized sum-of-ctors: Tag Ty* ( '|' Tag Ty* )* )
                    let save_inner = *j;
                    if let Some(firsttok) = toks.get(*j) {
                        if matches!(firsttok.tok, Tok::Ident) {
                            let mut alts: Vec<(lzscr_ast::ast::Tag, Vec<TypeExpr>)> = Vec::new();
                            loop {
                                let tagtok = bump(j, toks).ok_or_else(|| {
                                    ParseError::Generic("expected tag in sum type".into())
                                })?;
                                let tag = match &tagtok.tok {
                                    Tok::Ident => {
                                        lzscr_ast::ast::Tag::Name(tagtok.text.to_string())
                                    }
                                    _ => {
                                        *j = save_inner;
                                        break;
                                    }
                                };
                                // parse zero-or-more type args for this alt
                                let mut args: Vec<TypeExpr> = Vec::new();
                                loop {
                                    if let Some(nxt2) = toks.get(*j) {
                                        if matches!(nxt2.tok, Tok::Pipe)
                                            || matches!(nxt2.tok, Tok::RParen)
                                        {
                                            break;
                                        }
                                    } else {
                                        *j = save_inner;
                                        return Err(ParseError::Generic(
                                            "expected ) to close sum type".into(),
                                        ));
                                    }
                                    if !is_type_atom_start(&toks.get(*j).unwrap().tok) {
                                        break;
                                    }
                                    let te = parse_type_expr_bp(j, toks, 6)?;
                                    args.push(te);
                                }
                                alts.push((tag, args));
                                // check separator
                                if let Some(nxt3) = toks.get(*j) {
                                    if matches!(nxt3.tok, Tok::Pipe) {
                                        let _ = bump(j, toks);
                                        continue;
                                    } else if matches!(nxt3.tok, Tok::RParen) {
                                        let _ = bump(j, toks);
                                        return Ok(TypeExpr::Sum(alts));
                                    } else {
                                        *j = save_inner;
                                        break;
                                    }
                                } else {
                                    *j = save_inner;
                                    return Err(ParseError::Generic(
                                        "expected ) to close sum type".into(),
                                    ));
                                }
                            }
                        }
                    }
                    // Fallback: normal tuple/group parsing
                    let first = parse_type_expr(j, toks)?;
                    let mut items = vec![first];
                    loop {
                        let Some(nxt) = toks.get(*j) else {
                            return Err(ParseError::Generic(") expected in type".into()));
                        };
                        match nxt.tok {
                            Tok::Comma => {
                                let _ = bump(j, toks);
                                let t2 = parse_type_expr(j, toks)?;
                                items.push(t2);
                            }
                            Tok::RParen => {
                                let _ = bump(j, toks);
                                break;
                            }
                            _ => {
                                return Err(ParseError::WithSpan {
                                    msg: "expected , or ) in type".into(),
                                    span_offset: nxt.span.offset,
                                    span_len: nxt.span.len,
                                })
                            }
                        }
                    }
                    if items.len() == 1 {
                        return Ok(items.into_iter().next().unwrap());
                    }
                    TypeExpr::Tuple(items)
                }
                Tok::LBrace => {
                    if let Some(nxt) = toks.get(*j) {
                        if matches!(nxt.tok, Tok::RBrace) {
                            let _ = bump(j, toks);
                            return Ok(TypeExpr::Record(vec![]));
                        }
                    }
                    let mut fields = Vec::new();
                    loop {
                        let k = bump(j, toks).ok_or_else(|| {
                            ParseError::Generic("expected key in type record".into())
                        })?;
                        let key = match &k.tok {
                            Tok::Ident => k.text.to_string(),
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected ident key in type record".into(),
                                ))
                            }
                        };
                        let key_span = k.span; // capture field name span
                        let col = bump(j, toks).ok_or_else(|| {
                            ParseError::Generic(": expected in type record".into())
                        })?;
                        if !matches!(col.tok, Tok::Colon) {
                            return Err(ParseError::Generic(": expected in type record".into()));
                        }
                        let tv = parse_type_expr(j, toks)?;
                        fields.push(lzscr_ast::ast::TypeExprRecordField::new(key, key_span, tv));
                        let sep = bump(j, toks).ok_or_else(|| {
                            ParseError::Generic("expected , or } in type record".into())
                        })?;
                        match sep.tok {
                            Tok::Comma => continue,
                            Tok::RBrace => break,
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected , or } in type record".into(),
                                ))
                            }
                        }
                    }
                    TypeExpr::Record(fields)
                }
                Tok::Question => {
                    let name = if let Some(nx) = toks.get(*j) {
                        if matches!(nx.tok, Tok::Ident) {
                            let n = nx.text.to_string();
                            let _ = bump(j, toks);
                            Some(n)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    TypeExpr::Hole(name)
                }
                _ => return Err(ParseError::Generic("unexpected token in type".into())),
            })
        }

        fn parse_type_expr_bp<'b>(
            j: &mut usize,
            toks: &'b [lzscr_lexer::Lexed<'b>],
            bp: u8,
        ) -> Result<TypeExpr, ParseError> {
            let mut lhs = parse_type_atom(j, toks)?;
            loop {
                let Some(nxt) = toks.get(*j) else { break };
                if matches!(nxt.tok, Tok::Arrow) {
                    if 5 < bp {
                        break;
                    }
                    let _ = bump(j, toks);
                    let rhs = parse_type_expr_bp(j, toks, 5)?;
                    lhs = TypeExpr::Fun(Box::new(lhs), Box::new(rhs));
                    continue;
                }
                break;
            }
            Ok(lhs)
        }
        parse_type_expr_bp(j, toks, 0)
    }

    // Shared helper to parse a sum-of-ctors form: .Tag Ty* ( '|' .Tag Ty* )*
    // Returns Some(alts) and leaves `j` at the closing `}` (does NOT consume it).
    fn try_parse_sum_alts<'b>(
        j: &mut usize,
        toks: &'b [lzscr_lexer::Lexed<'b>],
    ) -> Result<Option<SumAlts>, ParseError> {
        let save = *j;
        // Need at least one tag token
        let first = match toks.get(*j) {
            Some(t) => t,
            None => return Ok(None),
        };
        let tag = match &first.tok {
            Tok::Ident => lzscr_ast::ast::Tag::Name(first.text.to_string()),
            _ => return Ok(None),
        };
        // consume first tag
        *j += 1;
        // parse zero-or-more type args for the first alt
        let mut args: Vec<TypeExpr> = Vec::new();
        loop {
            if let Some(nxt) = toks.get(*j) {
                if matches!(nxt.tok, Tok::Pipe) || matches!(nxt.tok, Tok::RBrace) {
                    break;
                }
            } else {
                *j = save;
                return Ok(None);
            }
            let te = parse_type_expr(j, toks)?;
            args.push(te);
        }
        // If next token is not a Pipe, this is not a sum (single ctor-like), roll back.
        if let Some(nxt) = toks.get(*j) {
            if !matches!(nxt.tok, Tok::Pipe) {
                *j = save;
                return Ok(None);
            }
        } else {
            *j = save;
            return Ok(None);
        }

        // It's a sum: collect alternatives until RBrace (leave `j` at RBrace)
        let mut alts: SumAlts = Vec::new();
        alts.push((tag, args));
        loop {
            // consume the '|'
            if let Some(p) = toks.get(*j) {
                if matches!(p.tok, Tok::Pipe) {
                    *j += 1;
                } else {
                    // expected '|' or '}'
                    break;
                }
            } else {
                return Err(ParseError::Generic(
                    "expected alternative after '|' in sum type".into(),
                ));
            }
            // next must be tag
            let tagtok = toks
                .get(*j)
                .ok_or_else(|| ParseError::Generic("expected tag in sum type".into()))?;
            let tag = match &tagtok.tok {
                Tok::Ident => lzscr_ast::ast::Tag::Name(tagtok.text.to_string()),
                _ => return Err(ParseError::Generic("expected tag in sum type".into())),
            };
            *j += 1;
            let mut args: Vec<TypeExpr> = Vec::new();
            loop {
                if let Some(nxt) = toks.get(*j) {
                    if matches!(nxt.tok, Tok::Pipe) || matches!(nxt.tok, Tok::RBrace) {
                        break;
                    }
                } else {
                    return Err(ParseError::Generic("expected '}' to close sum type".into()));
                }
                let te = parse_type_expr(j, toks)?;
                args.push(te);
            }
            alts.push((tag, args));
            // if next is RBrace, stop (do not consume RBrace here)
            if let Some(nxt) = toks.get(*j) {
                if matches!(nxt.tok, Tok::RBrace) {
                    break;
                } else {
                    // otherwise loop continues expecting another '|'
                    continue;
                }
            } else {
                return Err(ParseError::Generic("expected '}' to close sum type".into()));
            }
        }
        // leave j at RBrace (do not consume)
        Ok(Some(alts))
    }

    // Try to parse a type declaration: %Name %a* = %{ .Tag Ty* ( '|' .Tag Ty* )* '}' ';'?
    // On success, returns Some((decl, new_index)); on failure to match, returns Ok(None) and does not consume.
    fn try_parse_type_decl<'a>(
        j: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Option<(TypeDecl, usize)>, ParseError> {
        let save = *j;
        // Support two head forms:
        // 1) %Name %a* = ...
        // 2) %{ .Name %a* } = ...
        let mut decl_name: Option<String> = None;
        let mut params: Vec<String> = Vec::new();
        let head_start = *j;
        if let Some(h) = toks.get(*j) {
            match &h.tok {
                Tok::TyVar(nm) => {
                    // Assign once; not overwritten later
                    if decl_name.is_none() {
                        decl_name = Some(nm.clone());
                    }
                    *j += 1;
                    while let Some(nx) = toks.get(*j) {
                        if let Tok::TyVar(pn) = &nx.tok {
                            params.push(pn.clone());
                            *j += 1;
                        } else {
                            break;
                        }
                    }
                }
                Tok::TypeOpen => {
                    // %{ .Name %a* }
                    *j += 1; // consume '%{'
                    let mut dotted = false;
                    if let Some(dot) = toks.get(*j) {
                        // Some lexers tokenize '.' as Tok::Dot or Tok::Member(_); accept both
                        if matches!(dot.tok, Tok::Dot) || matches!(dot.tok, Tok::Member(_)) {
                            dotted = true;
                            *j += 1;
                        }
                    }
                    let ident = toks.get(*j).ok_or_else(|| {
                        ParseError::Generic("expected type name inside %{ }".into())
                    })?;
                    let nm = match &ident.tok {
                        Tok::Ident => ident.text.to_string(),
                        Tok::TyVar(s) => s.clone(),
                        _ => {
                            *j = save;
                            return Ok(None);
                        }
                    };
                    *j += 1;
                    while let Some(nx) = toks.get(*j) {
                        if let Tok::TyVar(pn) = &nx.tok {
                            params.push(pn.clone());
                            *j += 1;
                        } else {
                            break;
                        }
                    }
                    // closing '}'
                    let rb = toks.get(*j).ok_or_else(|| {
                        ParseError::Generic("expected '}' to close type head".into())
                    })?;
                    if !matches!(rb.tok, Tok::RBrace) {
                        *j = save;
                        return Ok(None);
                    }
                    *j += 1;
                    if decl_name.is_none() {
                        decl_name = Some(if dotted { format!(".{}", nm) } else { nm });
                    }
                }
                _ => {
                    return Ok(None);
                }
            }
        } else {
            return Ok(None);
        }
        // Now expect '='
        if let Some(eq) = toks.get(*j) {
            if matches!(eq.tok, Tok::Eq) {
                *j += 1;
            } else {
                *j = save;
                return Ok(None);
            }
        } else {
            *j = save;
            return Ok(None);
        }
        if let Some(op) = toks.get(*j) {
            if matches!(op.tok, Tok::TypeOpen) {
                *j += 1;
            } else {
                *j = save;
                return Ok(None);
            }
        } else {
            *j = save;
            return Ok(None);
        }
        let mut alts: Vec<(lzscr_ast::ast::Tag, Vec<TypeExpr>)> = Vec::new();
        loop {
            let tagtok = match toks.get(*j) {
                Some(t) => t,
                None => {
                    *j = save;
                    return Ok(None);
                }
            };
            let tag = match &tagtok.tok {
                Tok::Ident => lzscr_ast::ast::Tag::Name(tagtok.text.to_string()),
                _ => {
                    *j = save;
                    return Ok(None);
                }
            };
            *j += 1;
            let mut args: Vec<TypeExpr> = Vec::new();
            loop {
                let Some(nxt) = toks.get(*j) else {
                    *j = save;
                    return Ok(None);
                };
                match nxt.tok {
                    Tok::Pipe | Tok::RBrace => break,
                    _ => {
                        let te = parse_type_expr(j, toks)?;
                        args.push(te);
                    }
                }
            }
            alts.push((tag, args));
            let sep = match toks.get(*j) {
                Some(s) => s,
                None => {
                    *j = save;
                    return Ok(None);
                }
            };
            match sep.tok {
                Tok::Pipe => {
                    *j += 1;
                    continue;
                }
                Tok::RBrace => {
                    *j += 1;
                    break;
                }
                _ => {
                    *j = save;
                    return Ok(None);
                }
            }
        }
        // optional ';'
        if let Some(semi) = toks.get(*j) {
            if matches!(semi.tok, Tok::Semicolon) {
                *j += 1;
            }
        }
        let start_off = toks.get(head_start).map(|t| t.span.offset).unwrap_or(0);
        let end_off = toks.get(*j - 1).map(|t| t.span.offset + t.span.len).unwrap_or(start_off);
        let span = Span::new(start_off, end_off - start_off);
        let name = decl_name.unwrap();
        Ok(Some((TypeDecl { name, params, body: TypeDefBody::Sum(alts), span }, *j)))
    }

    fn parse_atom<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Expr, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF".into()))?;
        Ok(match &t.tok {
            // Check for mode map syntax: .{ Mode: expr, ... }
            Tok::Dot => {
                // Bare dot; check if followed by {
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::LBrace) {
                        let _ = bump(i, toks); // consume '{'

                        // Parse mode map fields
                        if let Some(check_empty) = peek(*i, toks) {
                            if matches!(check_empty.tok, Tok::RBrace) {
                                let r = bump(i, toks).unwrap();
                                let span_all = Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                );
                                return Ok(Expr::new(ExprKind::ModeMap(vec![]), span_all));
                            }
                        }

                        let mut fields: Vec<lzscr_ast::ast::ExprRecordField> = Vec::new();
                        loop {
                            let ktok = bump(i, toks)
                                .ok_or_else(|| ParseError::Generic("expected mode name".into()))?;
                            let key = match &ktok.tok {
                                Tok::Ident => ktok.text.to_string(),
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected mode name (ident) in mode map".into(),
                                        span_offset: ktok.span.offset,
                                        span_len: ktok.span.len,
                                    })
                                }
                            };
                            let key_span = ktok.span;
                            let col = bump(i, toks).ok_or_else(|| {
                                ParseError::Generic("expected : after mode name".into())
                            })?;
                            if !matches!(col.tok, Tok::Colon) {
                                return Err(ParseError::WithSpan {
                                    msg: ": expected after mode name".into(),
                                    span_offset: col.span.offset,
                                    span_len: col.span.len,
                                });
                            }
                            let val = parse_expr_bp(i, toks, 0)?;
                            fields.push(lzscr_ast::ast::ExprRecordField::new(key, key_span, val));
                            let sep = bump(i, toks).ok_or_else(|| {
                                ParseError::Generic("expected , or } in mode map".into())
                            })?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RBrace => {
                                    let span_all = Span::new(
                                        t.span.offset,
                                        sep.span.offset + sep.span.len - t.span.offset,
                                    );
                                    return Ok(Expr::new(ExprKind::ModeMap(fields), span_all));
                                }
                                _ => {
                                    return Err(ParseError::WithSpan {
                                        msg: "expected , or } in mode map".into(),
                                        span_offset: sep.span.offset,
                                        span_len: sep.span.len,
                                    })
                                }
                            }
                        }
                    }
                }
                // Just a dot symbol
                Expr::new(ExprKind::Symbol(".".to_string()), t.span)
            }
            Tok::TypeOpen => {
                let mut j = *i;
                let ty = parse_type_expr(&mut j, toks)?;
                let rb = bump(&mut j, toks)
                    .ok_or_else(|| ParseError::Generic("} expected after type".into()))?;
                if !matches!(rb.tok, Tok::RBrace) {
                    return Err(ParseError::Generic("} expected after type".into()));
                }
                // lookahead for an expression atom to annotate; if next token begins an atom/lambda/ref/etc, parse it
                if let Some(nxt) = toks.get(j) {
                    match nxt.tok {
                        Tok::LParen
                        | Tok::LBracket
                        | Tok::LBrace
                        | Tok::Int(_)
                        | Tok::Float(_)
                        | Tok::Str(_)
                        | Tok::Tilde
                        | Tok::Backslash
                        | Tok::Ident
                        | Tok::Member(_)
                        | Tok::Bang
                        | Tok::Caret
                        | Tok::TypeOpen => {
                            // annotation applies to following atom/expression (parse full expr at top level bp)
                            *i = j;
                            let ex = parse_expr_bp(i, toks, 0)?;
                            let span = Span::new(
                                t.span.offset,
                                ex.span.offset + ex.span.len - t.span.offset,
                            );
                            return Ok(Expr::new(ExprKind::Annot { ty, expr: Box::new(ex) }, span));
                        }
                        _ => {}
                    }
                }
                // otherwise, type value
                *i = j;
                let span = Span::new(t.span.offset, rb.span.offset + rb.span.len - t.span.offset);
                Expr::new(ExprKind::TypeVal(ty), span)
            }
            Tok::LBracket => {
                // list literal: [ e1, e2, ... ]
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RBracket) {
                        let r = bump(i, toks).unwrap();
                        return Ok(Expr::new(
                            ExprKind::List(vec![]),
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        ));
                    }
                }
                let mut items = Vec::new();
                loop {
                    let e = parse_expr_bp(i, toks, 0)?;
                    items.push(e);
                    let sep = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                        msg: "] or , expected".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    })?;
                    match sep.tok {
                        Tok::Comma => continue,
                        Tok::RBracket => {
                            let span_all = Span::new(
                                t.span.offset,
                                sep.span.offset + sep.span.len - t.span.offset,
                            );
                            return Ok(Expr::new(ExprKind::List(items), span_all));
                        }
                        _ => {
                            return Err(ParseError::WithSpan {
                                msg: "expected , or ] in list".into(),
                                span_offset: sep.span.offset,
                                span_len: sep.span.len,
                            })
                        }
                    }
                }
            }
            // ^(Expr)
            Tok::Caret => {
                let lp = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                    msg: "expected ( after ^".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })?;
                if !matches!(lp.tok, Tok::LParen) {
                    return Err(ParseError::WithSpan {
                        msg: "expected ( after ^".into(),
                        span_offset: lp.span.offset,
                        span_len: lp.span.len,
                    });
                }
                let inner = parse_expr_bp(i, toks, 0)?;
                let rp = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                    msg: ") expected".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })?;
                if !matches!(rp.tok, Tok::RParen) {
                    return Err(ParseError::WithSpan {
                        msg: ") expected".into(),
                        span_offset: rp.span.offset,
                        span_len: rp.span.len,
                    });
                }
                let span = Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                Expr::new(ExprKind::Raise(Box::new(inner)), span)
            }
            Tok::Int(n) => Expr::new(ExprKind::Int(*n), t.span),
            Tok::Float(f) => Expr::new(ExprKind::Float(*f), t.span),
            Tok::Str(s) => Expr::new(ExprKind::Str(s.clone()), t.span),
            Tok::Char(c) => Expr::new(ExprKind::Char(*c), t.span),
            Tok::Member(name) => Expr::new(ExprKind::Symbol(name.clone()), t.span),
            Tok::Tilde => {
                // Use span-aware errors so the CLI can render a caret.
                let id = bump(i, toks).ok_or_else(|| ParseError::WithSpan {
                    msg: "expected ident after ~".into(),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })?;
                match id.tok {
                    Tok::Ident => Expr::new(
                        ExprKind::Ref(id.text.to_string()),
                        Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset),
                    ),
                    _ => {
                        return Err(ParseError::WithSpan {
                            msg: "expected ident after ~".into(),
                            span_offset: id.span.offset,
                            span_len: id.span.len,
                        })
                    }
                }
            }
            Tok::Bang => {
                // Two forms:
                // - !sym  =>  (~effects .sym)
                // - !{ do-notation } => desugar to chain/bind chaining
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::LBrace) {
                        let _l = bump(i, toks).unwrap(); // consume '{'
                                                         // Parse statements until '}'
                        enum DoStmt {
                            Bind(Pattern, Expr),
                            Expr(Expr),
                        }
                        let mut stmts: Vec<DoStmt> = Vec::new();
                        let mut final_expr: Option<Expr> = None;
                        loop {
                            let Some(cur) = peek(*i, toks) else {
                                return Err(ParseError::Generic("} expected".into()));
                            };
                            if matches!(cur.tok, Tok::RBrace) {
                                let r = bump(i, toks).unwrap();
                                let span_all = Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                );
                                // Desugar to nested ~chain/~bind
                                let mut acc = final_expr
                                    .ok_or_else(|| ParseError::Generic("empty do-block".into()))?;
                                // Base case for last expression: (~bind acc (\x -> x)) to run in effect-context and yield value
                                let x_pat =
                                    Pattern::new(PatternKind::Var("_x_do".into()), acc.span);
                                let id_lam_span = Span::new(
                                    acc.span.offset,
                                    acc.span.offset + acc.span.len - acc.span.offset,
                                );
                                let id_lam = Expr::new(
                                    ExprKind::Lambda {
                                        param: x_pat,
                                        body: Box::new(Expr::new(
                                            ExprKind::Ref("_x_do".into()),
                                            acc.span,
                                        )),
                                    },
                                    id_lam_span,
                                );
                                let bind_ref = Expr::new(ExprKind::Ref("bind".into()), t.span);
                                let app_b1 = Expr::new(
                                    ExprKind::Apply {
                                        func: Box::new(bind_ref),
                                        arg: Box::new(acc),
                                    },
                                    span_all,
                                );
                                acc = Expr::new(
                                    ExprKind::Apply {
                                        func: Box::new(app_b1),
                                        arg: Box::new(id_lam),
                                    },
                                    span_all,
                                );
                                for stmt in stmts.into_iter().rev() {
                                    match stmt {
                                        DoStmt::Expr(e) => {
                                            let chain_ref =
                                                Expr::new(ExprKind::Ref("chain".into()), t.span);
                                            let app_c1 = Expr::new(
                                                ExprKind::Apply {
                                                    func: Box::new(chain_ref),
                                                    arg: Box::new(e),
                                                },
                                                span_all,
                                            );
                                            acc = Expr::new(
                                                ExprKind::Apply {
                                                    func: Box::new(app_c1),
                                                    arg: Box::new(acc),
                                                },
                                                span_all,
                                            );
                                        }
                                        DoStmt::Bind(p, e) => {
                                            let bind_ref =
                                                Expr::new(ExprKind::Ref("bind".into()), t.span);
                                            let lam_span = Span::new(
                                                p.span.offset,
                                                acc.span.offset + acc.span.len - p.span.offset,
                                            );
                                            let lam = Expr::new(
                                                ExprKind::Lambda { param: p, body: Box::new(acc) },
                                                lam_span,
                                            );
                                            let app_b1 = Expr::new(
                                                ExprKind::Apply {
                                                    func: Box::new(bind_ref),
                                                    arg: Box::new(e),
                                                },
                                                span_all,
                                            );
                                            acc = Expr::new(
                                                ExprKind::Apply {
                                                    func: Box::new(app_b1),
                                                    arg: Box::new(lam),
                                                },
                                                span_all,
                                            );
                                        }
                                    }
                                }
                                return Ok(acc);
                            }
                            // Try to parse a pattern <- expr ;
                            let before = *i;
                            if let Ok(pat) = parse_pattern(i, toks) {
                                if let Some(arrow) = peek(*i, toks) {
                                    if matches!(arrow.tok, Tok::LeftArrow) {
                                        let _ = bump(i, toks); // consume <-
                                        let ex = parse_expr_bp(i, toks, 0)?;
                                        let semi = bump(i, toks).ok_or_else(|| {
                                            ParseError::Generic("; expected after <- expr".into())
                                        })?;
                                        if !matches!(semi.tok, Tok::Semicolon) {
                                            return Err(ParseError::Generic(
                                                "; expected after <- expr".into(),
                                            ));
                                        }
                                        stmts.push(DoStmt::Bind(pat, ex));
                                        continue;
                                    }
                                }
                                // not a '<-' after pattern: backtrack; treat as body expr
                                *i = before;
                            } else {
                                // parsing failed: restore position for expr parsing path
                                *i = before;
                            }
                            // Parse an expression statement; sequence if terminated by ';', else treat as final expr
                            let ex = parse_expr_bp(i, toks, 0)?;
                            if let Some(semi) = peek(*i, toks) {
                                if matches!(semi.tok, Tok::Semicolon) {
                                    let _ = bump(i, toks);
                                    stmts.push(DoStmt::Expr(ex));
                                    continue;
                                }
                            }
                            if final_expr.is_some() {
                                return Err(ParseError::Generic(
                                    "unexpected extra expression in do-block".into(),
                                ));
                            }
                            final_expr = Some(ex);
                        }
                    }
                }
                let nxt = bump(i, toks).ok_or_else(|| {
                    ParseError::Generic("expected ident or .member after !".into())
                })?;
                let func = Expr::new(
                    ExprKind::Ref("effects".into()),
                    Span::new(t.span.offset, t.span.len),
                );
                let member = match &nxt.tok {
                    Tok::Ident => Expr::new(ExprKind::Symbol(format!(".{}", nxt.text)), nxt.span),
                    Tok::Member(name) => Expr::new(ExprKind::Symbol(name.clone()), nxt.span),
                    _ => {
                        return Err(ParseError::Generic("expected ident or .member after !".into()))
                    }
                };
                let span_all =
                    Span::new(t.span.offset, nxt.span.offset + nxt.span.len - t.span.offset);
                Expr::new(ExprKind::Apply { func: Box::new(func), arg: Box::new(member) }, span_all)
            }
            Tok::Backslash => {
                // Support multi-parameter lambdas: \p1 p2 ... -> body
                // Parameter separation by spaces; constructor payloads must be grouped with parens.
                let mut params: Vec<Pattern> = Vec::new();
                // first parameter is required
                let first = parse_pattern_param(i, toks)?;
                params.push(first);
                // parse zero or more additional params until '->'
                loop {
                    match peek(*i, toks) {
                        Some(tok) if matches!(tok.tok, Tok::Arrow) => {
                            let _ = bump(i, toks); // consume '->'
                            break;
                        }
                        Some(_) => {
                            let before = *i;
                            if let Ok(p) = parse_pattern_param(i, toks) {
                                params.push(p);
                                continue;
                            } else {
                                let tok = peek(before, toks).unwrap();
                                return Err(ParseError::WithSpan {
                                    msg: "expected ->".into(),
                                    span_offset: tok.span.offset,
                                    span_len: tok.span.len,
                                });
                            }
                        }
                        None => {
                            let end = toks.last().map(|t| t.span.offset + t.span.len).unwrap_or(0);
                            return Err(ParseError::WithSpan {
                                msg: "expected ->".into(),
                                span_offset: end,
                                span_len: 1,
                            });
                        }
                    }
                }
                // Check duplicate binders across params
                let mut names: Vec<String> = Vec::new();
                for p in &params {
                    collect_binders(p, &mut names);
                }
                names.sort();
                if names.windows(2).any(|w| w[0] == w[1]) {
                    return Err(ParseError::Generic(
                        "duplicate binder in lambda parameter chain".into(),
                    ));
                }
                // Parse body with higher binding power than '|'
                let body = parse_expr_bp(i, toks, 2)?;
                // Build nested lambdas from params
                nest_lambdas(&params, body)
            }
            Tok::LParen => {
                // handle unit ()
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RParen) {
                        let r = bump(i, toks).unwrap();
                        return Ok(Expr::new(
                            ExprKind::Unit,
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                        ));
                    }
                }

                // Try LetGroup with only bindings (no %type decl yet). Fallback to group/tuple.
                let save_i = *i;
                let mut it = *i;
                let mut leading_types: Vec<TypeDecl> = Vec::new();
                let mut leading: Vec<(Pattern, Expr)> = Vec::new();
                loop {
                    let before = it;
                    let mut j = it;
                    // Do not try to parse let bindings if the group starts with a lambda.
                    // Skip over any number of leading '(' to find the first significant token.
                    {
                        let mut jj = j;
                        while let Some(h) = toks.get(jj) {
                            if matches!(h.tok, Tok::LParen) {
                                jj += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(h) = toks.get(jj) {
                            if matches!(h.tok, Tok::Backslash) {
                                it = before;
                                break;
                            }
                        }
                    }
                    if let Some((decl, new_j)) = try_parse_type_decl(&mut j, toks)? {
                        leading_types.push(decl);
                        it = new_j;
                        continue;
                    }
                    // Try param-chain: ~name pat* = expr ;
                    if let Some((fname, params, _)) = try_parse_lhs_param_chain(&mut j, toks) {
                        // check duplicate binders
                        let mut names: Vec<String> = Vec::new();
                        for p in &params {
                            collect_binders(p, &mut names);
                        }
                        names.sort();
                        if names.windows(2).any(|w| w[0] == w[1]) {
                            return Err(ParseError::Generic(
                                "duplicate binder in parameter chain on let LHS".into(),
                            ));
                        }
                        let _eq = bump(&mut j, toks).unwrap();
                        let ex = parse_expr_bp(&mut j, toks, 0)?;
                        // Expect ';' after binding; allow ')' as soft terminator without consuming
                        match toks.get(j) {
                            Some(tok) if matches!(tok.tok, Tok::Semicolon) => {
                                j += 1;
                            }
                            Some(tok) if matches!(tok.tok, Tok::RParen) => { /* soft end */ }
                            Some(tok) => {
                                return Err(ParseError::WithSpan {
                                    msg: "; expected after let binding".into(),
                                    span_offset: tok.span.offset,
                                    span_len: tok.span.len,
                                });
                            }
                            None => {
                                return Err(ParseError::WithSpan {
                                    msg: "; expected after let binding".into(),
                                    span_offset: t.span.offset,
                                    span_len: t.span.len,
                                });
                            }
                        }
                        let body = nest_lambdas(&params, ex);
                        let pat = Pattern::new(PatternKind::Var(fname), t.span);
                        leading.push((pat, body));
                        it = j;
                        continue;
                    }
                    // Plain: pat = expr ;
                    // If the group (possibly after leading '(') starts with a lambda, don't treat it as a binding head.
                    {
                        let mut kk = j;
                        while let Some(h) = toks.get(kk) {
                            if matches!(h.tok, Tok::LParen) {
                                kk += 1;
                            } else {
                                break;
                            }
                        }
                        if let Some(h) = toks.get(kk) {
                            if matches!(h.tok, Tok::Backslash) {
                                it = before;
                                break;
                            }
                        }
                    }
                    // Require a top-level '=' ahead before attempting to parse a plain pattern binding.
                    {
                        let mut kk = j;
                        let mut depth = 0i32;
                        let mut found_eq = false;
                        while let Some(tok) = toks.get(kk) {
                            match tok.tok {
                                Tok::LParen | Tok::LBracket | Tok::LBrace => depth += 1,
                                Tok::RParen | Tok::RBracket | Tok::RBrace => {
                                    if depth == 0 {
                                        break;
                                    }
                                    depth -= 1;
                                }
                                Tok::Eq => {
                                    if depth == 0 {
                                        found_eq = true;
                                        break;
                                    }
                                }
                                _ => {}
                            }
                            kk += 1;
                        }
                        if !found_eq {
                            it = before;
                            break;
                        }
                    }
                    if let Ok(p) = parse_pattern(&mut j, toks) {
                        if let Some(eq) = toks.get(j) {
                            if matches!(eq.tok, Tok::Eq) {
                                j += 1;
                                let ex = parse_expr_bp(&mut j, toks, 0)?;
                                match toks.get(j) {
                                    Some(tok) if matches!(tok.tok, Tok::Semicolon) => {
                                        j += 1;
                                    }
                                    Some(tok) if matches!(tok.tok, Tok::RParen) => { /* soft end */
                                    }
                                    Some(tok) => {
                                        return Err(ParseError::WithSpan {
                                            msg: "; expected after let binding".into(),
                                            span_offset: tok.span.offset,
                                            span_len: tok.span.len,
                                        });
                                    }
                                    None => {
                                        return Err(ParseError::WithSpan {
                                            msg: "; expected after let binding".into(),
                                            span_offset: t.span.offset,
                                            span_len: t.span.len,
                                        });
                                    }
                                }
                                leading.push((p, ex));
                                it = j;
                                continue;
                            }
                        }
                    }
                    it = before;
                    break;
                }
                // If we parsed one or more leading bindings/types and the next token is ')',
                // treat the body as Unit and finalize a LetGroup. This does not interfere with
                // the "value-first + trailing binds" form (leading=0), because we only trigger
                // when there is at least one leading entry and immediate ')'.
                if let Some(rp) = toks.get(it) {
                    if matches!(rp.tok, Tok::RParen)
                        && (!leading.is_empty() || !leading_types.is_empty())
                    {
                        *i = it + 1;
                        let unit = Expr::new(ExprKind::Unit, rp.span);
                        let span_all =
                            Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                        return Ok(Expr::new(
                            ExprKind::LetGroup {
                                type_decls: leading_types,
                                bindings: leading,
                                body: Box::new(unit),
                            },
                            span_all,
                        ));
                    }
                }
                // Body
                let mut j = it;
                let body_expr = parse_expr_bp(&mut j, toks, 0)?;
                // Optional trailing bindings
                let mut trailing_types: Vec<TypeDecl> = Vec::new();
                let mut trailing: Vec<(Pattern, Expr)> = Vec::new();
                if let Some(sep) = toks.get(j) {
                    if matches!(sep.tok, Tok::Semicolon) {
                        j += 1;
                        loop {
                            let before = j;
                            let mut k = j;
                            // If trailing section starts with a lambda (skipping '('), it's not a binding
                            {
                                let mut kk = k;
                                while let Some(h) = toks.get(kk) {
                                    if matches!(h.tok, Tok::LParen) {
                                        kk += 1;
                                    } else {
                                        break;
                                    }
                                }
                                if let Some(h) = toks.get(kk) {
                                    if matches!(h.tok, Tok::Backslash) {
                                        j = before;
                                        break;
                                    }
                                }
                            }
                            if let Some((decl2, new_k)) = try_parse_type_decl(&mut k, toks)? {
                                trailing_types.push(decl2);
                                j = new_k;
                                continue;
                            }
                            if let Some((fname, params, _)) =
                                try_parse_lhs_param_chain(&mut k, toks)
                            {
                                let mut names: Vec<String> = Vec::new();
                                for p in &params {
                                    collect_binders(p, &mut names);
                                }
                                names.sort();
                                if names.windows(2).any(|w| w[0] == w[1]) {
                                    return Err(ParseError::Generic(
                                        "duplicate binder in parameter chain on let LHS".into(),
                                    ));
                                }
                                let _eq = bump(&mut k, toks).unwrap();
                                let ex2 = parse_expr_bp(&mut k, toks, 0)?;
                                let semi2 = toks.get(k).ok_or_else(|| ParseError::WithSpan {
                                    msg: "; expected after let binding".into(),
                                    span_offset: t.span.offset,
                                    span_len: t.span.len,
                                })?;
                                if !matches!(semi2.tok, Tok::Semicolon) {
                                    return Err(ParseError::WithSpan {
                                        msg: "; expected after let binding".into(),
                                        span_offset: semi2.span.offset,
                                        span_len: semi2.span.len,
                                    });
                                }
                                k += 1;
                                let body2 = nest_lambdas(&params, ex2);
                                let pat2 = Pattern::new(PatternKind::Var(fname), t.span);
                                trailing.push((pat2, body2));
                                j = k;
                                continue;
                            }
                            if let Some(h) = toks.get(k) {
                                if matches!(h.tok, Tok::Backslash) {
                                    j = before;
                                    break;
                                }
                            }
                            // For trailing plain pattern binding, also require a top-level '=' ahead from current k.
                            let mut plain_binding_possible = false;
                            {
                                let mut kk = k;
                                let mut depth = 0i32;
                                while let Some(tok) = toks.get(kk) {
                                    match tok.tok {
                                        Tok::LParen | Tok::LBracket | Tok::LBrace => depth += 1,
                                        Tok::RParen | Tok::RBracket | Tok::RBrace => {
                                            if depth == 0 {
                                                break;
                                            }
                                            depth -= 1;
                                        }
                                        Tok::Eq => {
                                            if depth == 0 {
                                                plain_binding_possible = true;
                                                break;
                                            }
                                        }
                                        _ => {}
                                    }
                                    kk += 1;
                                }
                            }
                            if plain_binding_possible {
                                if let Ok(p2) = parse_pattern(&mut k, toks) {
                                    if let Some(eq) = toks.get(k) {
                                        if matches!(eq.tok, Tok::Eq) {
                                            k += 1;
                                            let ex2 = parse_expr_bp(&mut k, toks, 0)?;
                                            match toks.get(k) {
                                                Some(tok) if matches!(tok.tok, Tok::Semicolon) => {
                                                    k += 1;
                                                }
                                                Some(tok) if matches!(tok.tok, Tok::RParen) => { /* soft end */
                                                }
                                                Some(tok) => {
                                                    return Err(ParseError::WithSpan {
                                                        msg: "; expected after let binding".into(),
                                                        span_offset: tok.span.offset,
                                                        span_len: tok.span.len,
                                                    });
                                                }
                                                None => {
                                                    return Err(ParseError::WithSpan {
                                                        msg: "; expected after let binding".into(),
                                                        span_offset: t.span.offset,
                                                        span_len: t.span.len,
                                                    });
                                                }
                                            }
                                            trailing.push((p2, ex2));
                                            j = k;
                                            continue;
                                        }
                                    }
                                }
                            }
                            // Allow pure expression statements (effects) after we already have let entries
                            let has_let_entries = !leading.is_empty()
                                || !leading_types.is_empty()
                                || !trailing.is_empty()
                                || !trailing_types.is_empty();
                            if has_let_entries {
                                if let Ok(expr_stmt) = parse_expr_bp(&mut k, toks, 0) {
                                    match toks.get(k) {
                                        Some(tok) if matches!(tok.tok, Tok::Semicolon) => {
                                            k += 1;
                                        }
                                        Some(tok) if matches!(tok.tok, Tok::RParen) => {
                                            // soft end
                                        }
                                        Some(tok) => {
                                            return Err(ParseError::WithSpan {
                                                msg: "; expected after expression in let group"
                                                    .into(),
                                                span_offset: tok.span.offset,
                                                span_len: tok.span.len,
                                            });
                                        }
                                        None => {
                                            return Err(ParseError::WithSpan {
                                                msg: "; expected after expression in let group"
                                                    .into(),
                                                span_offset: t.span.offset,
                                                span_len: t.span.len,
                                            });
                                        }
                                    }
                                    let wild = Pattern::new(PatternKind::Wildcard, expr_stmt.span);
                                    trailing.push((wild, expr_stmt));
                                    j = k;
                                    continue;
                                }
                            }
                            j = before;
                            break;
                        }
                    }
                }
                // Commit if we have at least one binding or type decl and next token is ')'
                if let Some(rp) = toks.get(j) {
                    if matches!(rp.tok, Tok::RParen)
                        && (!leading.is_empty()
                            || !trailing.is_empty()
                            || !leading_types.is_empty()
                            || !trailing_types.is_empty())
                    {
                        *i = j + 1;
                        let mut all = leading;
                        all.extend(trailing);
                        let mut tys = leading_types;
                        tys.extend(trailing_types);
                        let span_all =
                            Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                        return Ok(Expr::new(
                            ExprKind::LetGroup {
                                type_decls: tys,
                                bindings: all,
                                body: Box::new(body_expr),
                            },
                            span_all,
                        ));
                    }
                }

                // Fallback: group/tuple
                *i = save_i;
                let first = parse_expr_bp(i, toks, 0)?;
                let mut items = vec![first];
                loop {
                    let Some(nxt) = peek(*i, toks) else {
                        return Err(ParseError::WithSpan {
                            msg: ") expected".into(),
                            span_offset: t.span.offset,
                            span_len: t.span.len,
                        });
                    };
                    match nxt.tok {
                        Tok::Comma => {
                            let _ = bump(i, toks);
                            let e = parse_expr_bp(i, toks, 0)?;
                            items.push(e);
                        }
                        Tok::RParen => {
                            let r = bump(i, toks).unwrap();
                            let span_all = Span::new(
                                t.span.offset,
                                r.span.offset + r.span.len - t.span.offset,
                            );
                            if items.len() == 1 {
                                return Ok(items.pop().unwrap());
                            }
                            // Build arity-specific tuple tag: ".," for 2, ".,," for 3, ...
                            let tag = {
                                let mut s = String::from(".");
                                let n_commas = if !items.is_empty() { items.len() - 1 } else { 0 };
                                for _ in 0..n_commas {
                                    s.push(',');
                                }
                                s
                            };
                            let mut tuple_expr = Expr::new(ExprKind::Symbol(tag), t.span);
                            for it in items {
                                let sp = Span::new(
                                    tuple_expr.span.offset,
                                    span_all.offset + span_all.len - tuple_expr.span.offset,
                                );
                                tuple_expr = Expr::new(
                                    ExprKind::Apply {
                                        func: Box::new(tuple_expr),
                                        arg: Box::new(it),
                                    },
                                    sp,
                                );
                            }
                            return Ok(tuple_expr);
                        }
                        _ => {
                            // 特別扱い: グループ/タプル内で ';' に遭遇 -> ユーザは LetGroup を意図した可能性
                            if matches!(nxt.tok, Tok::Semicolon) {
                                return Err(ParseError::WithSpan2 {
                                    msg: "unexpected ';' inside parenthesized expression (not a let-group). Did you forget a ')' before the ';'? To write a let-group use: (pat = expr; ... body)".into(),
                                    span1_offset: nxt.span.offset,
                                    span1_len: nxt.span.len,
                                    span2_offset: t.span.offset,
                                    span2_len: t.span.len,
                                });
                            }
                            // lookahead: 同レベルでこの後に対応する ')' が存在するか簡易チェック
                            let mut look = *i;
                            let mut depth2 = 0usize;
                            let mut found_same_level_rparen = false;
                            while let Some(tt) = peek(look, toks) {
                                match tt.tok {
                                    Tok::LParen => depth2 += 1,
                                    Tok::RParen => {
                                        if depth2 == 0 {
                                            found_same_level_rparen = true;
                                            break;
                                        } else {
                                            depth2 -= 1;
                                        }
                                    }
                                    _ => {}
                                }
                                look += 1;
                            }
                            if !found_same_level_rparen {
                                return Err(ParseError::WithSpan {
                                    msg: ") expected to close this '('".into(),
                                    span_offset: t.span.offset,
                                    span_len: t.span.len,
                                });
                            } else {
                                return Err(ParseError::WithSpan {
                                    msg: "expected , or )".into(),
                                    span_offset: nxt.span.offset,
                                    span_len: nxt.span.len,
                                });
                            }
                        }
                    }
                }
            }
            Tok::LBrace => {
                // Direct record literal to ExprKind::Record
                // Phase 5: Now captures field name spans for better diagnostics
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RBrace) {
                        let r = bump(i, toks).unwrap();
                        let span_all =
                            Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset);
                        return Ok(Expr::new(ExprKind::Record(vec![]), span_all));
                    }
                }
                let mut fields: Vec<lzscr_ast::ast::ExprRecordField> = Vec::new();
                loop {
                    let ktok =
                        bump(i, toks).ok_or_else(|| ParseError::Generic("expected key".into()))?;
                    let key = match &ktok.tok {
                        Tok::Ident => ktok.text.to_string(),
                        _ => return Err(ParseError::Generic("expected ident key".into())),
                    };
                    let key_span = ktok.span;
                    let col =
                        bump(i, toks).ok_or_else(|| ParseError::Generic("expected :".into()))?;
                    if !matches!(col.tok, Tok::Colon) {
                        return Err(ParseError::Generic(": expected".into()));
                    }
                    let val = parse_expr_bp(i, toks, 0)?;
                    fields.push(lzscr_ast::ast::ExprRecordField::new(key, key_span, val));
                    let sep = bump(i, toks)
                        .ok_or_else(|| ParseError::Generic("expected , or }".into()))?;
                    match sep.tok {
                        Tok::Comma => continue,
                        Tok::RBrace => {
                            let span_all = Span::new(
                                t.span.offset,
                                sep.span.offset + sep.span.len - t.span.offset,
                            );
                            return Ok(Expr::new(ExprKind::Record(fields), span_all));
                        }
                        _ => return Err(ParseError::Generic("expected , or }".into())),
                    }
                }
            }
            Tok::Ident => {
                if t.text == "_" {
                    return Err(ParseError::WithSpan {
                        msg: "'_' is only valid as a pattern wildcard".into(),
                        span_offset: t.span.offset,
                        span_len: t.span.len,
                    });
                }
                Expr::new(ExprKind::Symbol(t.text.to_string()), t.span)
            }
            _ => {
                return Err(ParseError::WithSpan {
                    msg: format!("unexpected token: {:?}", t.tok),
                    span_offset: t.span.offset,
                    span_len: t.span.len,
                })
            }
        })
    }

    fn parse_expr_bp<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
        bp: u8,
    ) -> Result<Expr, ParseError> {
        // Note: grouped/annotated lambdas are accepted downstream; syntactic checks are relaxed here.
        let mut lhs = parse_atom(i, toks)?;
        loop {
            let Some(nxt) = peek(*i, toks) else { break };
            // Special-case pipeline operator '|>' parsed as two tokens Pipe + Greater
            if let Tok::Pipe = nxt.tok {
                if let Some(n2) = peek(*i + 1, toks) {
                    if matches!(n2.tok, Tok::Greater) {
                        let _ = bump(i, toks); // consume '|'
                        let _ = bump(i, toks); // consume '>'
                                               // precedence slightly above '||' and '|'
                        let rhs = parse_expr_bp(i, toks, 3)?;
                        let span = Span::new(
                            lhs.span.offset,
                            rhs.span.offset + rhs.span.len - lhs.span.offset,
                        );
                        // desugar: lhs |> rhs  ==>  rhs lhs
                        lhs = Expr::new(
                            ExprKind::Apply { func: Box::new(rhs), arg: Box::new(lhs) },
                            span,
                        );
                        continue;
                    }
                }
            }
            // Pratt parser: handle infix precedence
            let (op_bp, op_kind) = match nxt.tok.clone() {
                // lowest precedence tier: catch (^|) handled specially below; '|' (alt-lambda) lower than '||' and '->'
                Tok::Pipe => (1, Some("|")),
                Tok::PipePipe => (2, Some("||")),
                Tok::Colon => (6, Some(":")),
                Tok::Plus => (10, Some("+")),
                Tok::Minus => (10, Some("-")),
                Tok::Star => (20, Some("*")),
                Tok::Slash => (20, Some("/")),
                Tok::DotPlus => (10, Some(".+")),
                Tok::DotMinus => (10, Some(".-")),
                Tok::DotStar => (20, Some(".*")),
                Tok::DotSlash => (20, Some("./")),
                Tok::Less => (5, Some("<")),
                Tok::LessEq => (5, Some("<=")),
                Tok::Greater => (5, Some(">")),
                Tok::GreaterEq => (5, Some(">=")),
                Tok::EqEq => (4, Some("==")),
                Tok::BangEq => (4, Some("!=")),
                Tok::DotLess => (5, Some(".<")),
                Tok::DotLessEq => (5, Some(".<=")),
                Tok::DotGreater => (5, Some(".>")),
                Tok::DotGreaterEq => (5, Some(".>=")),
                _ => (0, None),
            };
            if let Some(op) = op_kind {
                if op_bp < bp {
                    break;
                }
                // consume op
                let _ = bump(i, toks);
                // special case: '^|' should be parsed as a single operator token sequence '^' '|' when used infix
                if op == "||" {
                    // parse RHS normally
                    let rhs = parse_expr_bp(i, toks, op_bp + 1)?;
                    let span = Span::new(
                        lhs.span.offset,
                        rhs.span.offset + rhs.span.len - lhs.span.offset,
                    );
                    lhs = Expr::new(
                        ExprKind::OrElse { left: Box::new(lhs), right: Box::new(rhs) },
                        span,
                    );
                    continue;
                }
                if op == "|" {
                    // alt-lambda: syntactic validation is deferred; always build node
                    let rhs = parse_expr_bp(i, toks, op_bp)?; // right-assoc: same bp for RHS
                    let span = Span::new(
                        lhs.span.offset,
                        rhs.span.offset + rhs.span.len - lhs.span.offset,
                    );
                    lhs = Expr::new(
                        ExprKind::AltLambda { left: Box::new(lhs), right: Box::new(rhs) },
                        span,
                    );
                    continue;
                }
                if op == ":" {
                    // cons is right-associative; do not increase bp on RHS to keep right-assoc
                    let rhs = parse_expr_bp(i, toks, op_bp)?;
                    let span = Span::new(
                        lhs.span.offset,
                        rhs.span.offset + rhs.span.len - lhs.span.offset,
                    );
                    // desugar (h : t) => ((~cons h) t)
                    let callee = Expr::new(ExprKind::Ref("cons".into()), nxt.span);
                    let app1 = Expr::new(
                        ExprKind::Apply { func: Box::new(callee), arg: Box::new(lhs) },
                        span,
                    );
                    lhs = Expr::new(
                        ExprKind::Apply { func: Box::new(app1), arg: Box::new(rhs) },
                        span,
                    );
                    continue;
                }
                let rhs = parse_expr_bp(i, toks, op_bp + 1)?;
                // desugar: (a + b) -> ((~add a) b), etc.
                let callee = match op {
                    "+" => Expr::new(ExprKind::Ref("add".into()), nxt.span),
                    "-" => Expr::new(ExprKind::Ref("sub".into()), nxt.span),
                    "*" => Expr::new(ExprKind::Ref("mul".into()), nxt.span),
                    "/" => Expr::new(ExprKind::Ref("div".into()), nxt.span),
                    ".+" => Expr::new(ExprKind::Ref("fadd".into()), nxt.span),
                    ".-" => Expr::new(ExprKind::Ref("fsub".into()), nxt.span),
                    ".*" => Expr::new(ExprKind::Ref("fmul".into()), nxt.span),
                    "./" => Expr::new(ExprKind::Ref("fdiv".into()), nxt.span),
                    "<" => Expr::new(ExprKind::Ref("lt".into()), nxt.span),
                    "<=" => Expr::new(ExprKind::Ref("le".into()), nxt.span),
                    ">" => Expr::new(ExprKind::Ref("gt".into()), nxt.span),
                    ">=" => Expr::new(ExprKind::Ref("ge".into()), nxt.span),
                    "==" => Expr::new(ExprKind::Ref("eq".into()), nxt.span),
                    "!=" => Expr::new(ExprKind::Ref("ne".into()), nxt.span),
                    ".<" => Expr::new(ExprKind::Ref("flt".into()), nxt.span),
                    ".<=" => Expr::new(ExprKind::Ref("fle".into()), nxt.span),
                    ".>" => Expr::new(ExprKind::Ref("fgt".into()), nxt.span),
                    ".>=" => Expr::new(ExprKind::Ref("fge".into()), nxt.span),
                    _ => unreachable!(),
                };
                let span =
                    Span::new(lhs.span.offset, rhs.span.offset + rhs.span.len - lhs.span.offset);
                let app1 =
                    Expr::new(ExprKind::Apply { func: Box::new(callee), arg: Box::new(lhs) }, span);
                lhs = Expr::new(ExprKind::Apply { func: Box::new(app1), arg: Box::new(rhs) }, span);
                continue;
            }
            // parse '^|' as infix catch ONLY when the next token after '^' is '|'
            if let Tok::Caret = nxt.tok {
                if let Some(n2) = peek(*i + 1, toks) {
                    if matches!(n2.tok, Tok::Pipe) {
                        let _ = bump(i, toks); // consume '^'
                        let _ = bump(i, toks); // consume '|'
                        let rhs = parse_expr_bp(i, toks, 3)?; // slightly above '||' and '|'
                        let span = Span::new(
                            lhs.span.offset,
                            rhs.span.offset + rhs.span.len - lhs.span.offset,
                        );
                        lhs = Expr::new(
                            ExprKind::Catch { left: Box::new(lhs), right: Box::new(rhs) },
                            span,
                        );
                        continue;
                    }
                }
            }
            match nxt.tok {
                Tok::LParen
                | Tok::LBracket
                | Tok::Int(_)
                | Tok::Float(_)
                | Tok::Str(_)
                | Tok::Char(_)
                | Tok::Tilde
                | Tok::Backslash
                | Tok::Ident
                | Tok::Member(_)
                | Tok::Bang
                | Tok::Caret => {
                    // function application (left associative)
                    let arg = parse_atom(i, toks)?;
                    let span = Span::new(
                        lhs.span.offset,
                        arg.span.offset + arg.span.len - lhs.span.offset,
                    );
                    // (legacy boolean call sugar removed)
                    lhs = Expr::new(
                        ExprKind::Apply { func: Box::new(lhs), arg: Box::new(arg) },
                        span,
                    );
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    let expr = parse_expr_bp(&mut i, &tokens, 0)?;
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_literal_ident_key_ok() {
        let src = "{a:1, b:2}";
        let r = parse_expr(src);
        assert!(r.is_ok(), "expected Ok, got {:?}", r);
    }

    #[test]
    fn record_literal_string_key_rejected() {
        let src = "{\"a\":1}";
        let r = parse_expr(src);
        match r {
            Err(ParseError::Generic(msg)) => {
                assert!(msg.contains("expected ident key"), "unexpected msg: {}", msg)
            }
            other => panic!("expected Err(ParseError::Generic), got {:?}", other),
        }
    }

    #[test]
    fn record_pattern_string_key_rejected() {
        let src = "\\{ \"a\": ~x } -> ~x";
        let r = parse_expr(src);
        match r {
            Err(ParseError::Generic(msg)) | Err(ParseError::WithSpan { msg, .. }) => assert!(
                msg.contains("expected ident key in record pattern"),
                "unexpected msg: {}",
                msg
            ),
            other => panic!("expected Err(ParseError::Generic), got {:?}", other),
        }
    }

    #[test]
    fn list_literal_empty() {
        let src = "[]";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::List(xs) => assert!(xs.is_empty()),
            _ => panic!("expected List"),
        }
    }

    #[test]
    fn list_literal_basic() {
        let src = "[1, 2, 3]";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::List(xs) => assert_eq!(xs.len(), 3),
            _ => panic!("expected List"),
        }
    }

    #[test]
    fn cons_right_assoc() {
        // 1:2:[] should parse as 1:(2:[])
        let src = "1 : 2 : []";
        let r = parse_expr(src).unwrap();
        // We can't easily inspect without eval, but ensure it parses
        let _ = r;
    }

    #[test]
    fn type_annotation_parses() {
        let src = "%{.Int} 1";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::Annot { ty, expr } => {
                match ty {
                    TypeExpr::Int => {}
                    other => panic!("expected Int type, got {:?}", other),
                }
                matches!(expr.kind, ExprKind::Int(1));
            }
            other => panic!("expected Annot, got {:?}", other),
        }
    }

    #[test]
    fn type_value_parses() {
        let src = "%{.Int -> .Int}";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::TypeVal(ty) => match ty {
                TypeExpr::Fun(a, b) => {
                    matches!(*a, TypeExpr::Int);
                    matches!(*b, TypeExpr::Int);
                }
                other => panic!("expected Fun(Int, Int), got {:?}", other),
            },
            other => panic!("expected TypeVal, got {:?}", other),
        }
    }

    #[test]
    fn type_annotation_accepts_bare_constructors() {
        let src = "%{Foo} Foo";
        let expr = parse_expr(src).expect("bare ctor type annotation should parse");
        match expr.kind {
            ExprKind::Annot { ty, .. } => match ty {
                TypeExpr::Ctor { tag, args } => match tag {
                    lzscr_ast::ast::Tag::Name(s) => {
                        assert_eq!(s, "Foo");
                        assert!(args.is_empty());
                    }
                },
                other => panic!("expected ctor type, got {:?}", other),
            },
            other => panic!("expected annotation, got {:?}", other),
        }
    }

    #[test]
    fn type_annotation_accepts_dotted_constructors() {
        let src = "%{.Foo} Foo";
        let expr = parse_expr(src).expect("dotted ctor type annotation should parse");
        match expr.kind {
            ExprKind::Annot { ty, .. } => match ty {
                TypeExpr::Ctor { tag, args } => match tag {
                    lzscr_ast::ast::Tag::Name(s) => {
                        assert_eq!(s, "Foo");
                        assert!(args.is_empty());
                    }
                },
                other => panic!("expected ctor type, got {:?}", other),
            },
            other => panic!("expected annotation, got {:?}", other),
        }
    }

    #[test]
    fn type_decl_accepts_bare_constructors() {
        let src = "( %Maybe %a = %{ Just %a | Nothing }; 0 )";
        let expr = parse_expr(src).expect("bare ctor decl should parse");
        let ExprKind::LetGroup { type_decls, .. } = expr.kind else {
            panic!("expected let group with type decls");
        };
        assert_eq!(type_decls.len(), 1);
        let decl = &type_decls[0];
        let TypeDefBody::Sum(alts) = &decl.body;
        assert_eq!(alts.len(), 2);
        assert!(matches!(alts[0].0, lzscr_ast::ast::Tag::Name(ref s) if s == "Just"));
        assert!(matches!(alts[1].0, lzscr_ast::ast::Tag::Name(ref s) if s == "Nothing"));
    }

    #[test]
    fn type_decl_rejects_dotted_constructor_names() {
        let src = "( %Maybe %a = %{ .Just %a | .Nothing }; 0 )";
        let err = parse_expr(src).expect_err("dotted ctor in type decl should be rejected");
        let _ = err; // only assert failure
    }

    #[test]
    fn pattern_list_vars_parse() {
        // \[ ~x, ~y ] -> ~x
        let src = "\\[ ~x, ~y ] -> ~x";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::Lambda { param, .. } => match param.kind {
                PatternKind::List(items) => {
                    assert_eq!(items.len(), 2);
                    matches!(items[0].kind, PatternKind::Var(_));
                    matches!(items[1].kind, PatternKind::Var(_));
                }
                other => panic!("expected List pattern, got {:?}", other),
            },
            _ => panic!("expected Lambda"),
        }
    }

    #[test]
    fn pattern_bare_ctor_allowed() {
        let src = "\\(Foo ~x) -> ~x";
        let r = parse_expr(src).expect("constructor pattern should parse");
        match r.kind {
            ExprKind::Lambda { param, .. } => match param.kind {
                PatternKind::Ctor { name, args } => {
                    assert_eq!(name, "Foo");
                    assert_eq!(args.len(), 1);
                }
                other => panic!("expected ctor pattern, got {:?}", other),
            },
            other => panic!("expected Lambda, got {:?}", other),
        }
    }

    #[test]
    fn expr_bare_ctor_application() {
        let src = "Foo 1";
        let r = parse_expr(src).expect("bare constructor application should parse");
        match r.kind {
            ExprKind::Apply { func, arg } => {
                match func.kind {
                    ExprKind::Symbol(ref name) => assert_eq!(name, "Foo"),
                    other => panic!("expected ctor head, got {:?}", other),
                }
                match arg.kind {
                    ExprKind::Int(n) => assert_eq!(n, 1),
                    other => panic!("expected Int arg, got {:?}", other),
                }
            }
            other => panic!("expected Apply, got {:?}", other),
        }
    }

    #[test]
    fn expr_member_symbol_literal() {
        let src = ".println";
        let r = parse_expr(src).expect("member symbol literal should parse");
        match r.kind {
            ExprKind::Symbol(name) => assert_eq!(name, ".println"),
            other => panic!("expected symbol expression, got {:?}", other),
        }
    }

    #[test]
    fn pattern_cons_parse() {
        // \(~h : ~t) -> ~h
        let src = "\\( ~h : ~t ) -> ~h";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::Lambda { param, .. } => match param.kind {
                PatternKind::Cons(h, t) => {
                    matches!(h.kind, PatternKind::Var(_));
                    matches!(t.kind, PatternKind::Var(_));
                }
                other => panic!("expected Cons pattern, got {:?}", other),
            },
            _ => panic!("expected Lambda"),
        }
    }

    #[test]
    fn alt_lambda_requires_lambdas() {
        let src = "(\\~x -> ~x) | 1";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::AltLambda { left, right } => {
                matches!(left.kind, ExprKind::Lambda { .. });
                matches!(right.kind, ExprKind::Int(1));
            }
            other => panic!("expected AltLambda, got {:?}", other),
        }
    }

    #[test]
    fn alt_lambda_right_assoc() {
        let src = "\\~a -> ~a | \\~b -> ~b | \\~c -> ~c";
        let r = parse_expr(src).unwrap();
        // ensure it parses; structural checks covered in analyzer/runtime tests later
        let _ = r;
    }

    #[test]
    fn char_literal_basic_and_escape() {
        let r = parse_expr("'a'").unwrap();
        match r.kind {
            ExprKind::Char(c) => assert_eq!(c, 'a' as i32),
            _ => panic!("expected Char"),
        }
        let r2 = parse_expr("'\\n'").unwrap();
        match r2.kind {
            ExprKind::Char(c) => assert_eq!(c, '\n' as i32),
            _ => panic!("expected Char"),
        }
    }

    #[test]
    fn alt_lambda_lower_than_arrow() {
        let src = "\\~x -> ~x | \\~y -> ~y";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::AltLambda { left, right } => {
                matches!(left.kind, ExprKind::Lambda { .. });
                matches!(right.kind, ExprKind::Lambda { .. });
            }
            other => panic!("expected AltLambda at top, got {:?}", other),
        }
    }

    #[test]
    fn let_group_parse_basic() {
        // ( ~x = 1; ~x; ~y = 2; )
        let src = "( ~x = 1; ~x; ~y = 2; )";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::LetGroup { bindings, body, .. } => {
                assert_eq!(bindings.len(), 2);
                match body.kind {
                    ExprKind::Ref(n) => assert_eq!(n, "x"),
                    _ => panic!("expected body ~x"),
                }
            }
            other => panic!("expected LetGroup, got {:?}", other),
        }
    }

    #[test]
    fn group_without_bindings_is_plain_group_or_unit() {
        // Simple grouped body without bindings should not be a LetGroup
        let r = parse_expr("(1)").unwrap();
        match r.kind {
            ExprKind::Int(1) => {}
            _ => panic!("expected grouped Int to parse as Int (not LetGroup)"),
        }
    }

    #[test]
    fn letgroup_with_leading_bindings() {
        let r = parse_expr("(~x = 1; ~add ~x 2)").unwrap();
        match r.kind {
            ExprKind::LetGroup { .. } => {}
            _ => panic!("expected LetGroup with leading bindings"),
        }
    }

    #[test]
    fn letgroup_with_trailing_bindings() {
        let r = parse_expr("(42; ~x = 1;)").unwrap();
        match r.kind {
            ExprKind::LetGroup { .. } => {}
            _ => panic!("expected LetGroup with trailing bindings"),
        }
    }

    #[test]
    fn letgroup_value_first_then_trailing_bind() {
        let r = parse_expr("( 0; ~x = 1; )").unwrap();
        match r.kind {
            ExprKind::LetGroup { bindings, body, .. } => {
                assert_eq!(bindings.len(), 1);
                match body.kind {
                    ExprKind::Int(0) => {}
                    _ => panic!("expected body 0"),
                }
            }
            _ => panic!("expected LetGroup for value-first with trailing bind"),
        }
    }

    #[test]
    fn letgroup_trailing_effect_statements() {
        let src = "(~helper ~x = (~y = ~x; ~y;); ~helper 1; ~helper 2; ~helper 3;)";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::LetGroup { bindings, .. } => {
                assert_eq!(bindings.len(), 3);
                match &bindings[0].0.kind {
                    PatternKind::Var(name) => assert_eq!(name, "helper"),
                    other => panic!("expected helper binding, got {:?}", other),
                }
                assert!(matches!(bindings[1].0.kind, PatternKind::Wildcard));
                assert!(matches!(bindings[2].0.kind, PatternKind::Wildcard));
            }
            other => panic!("expected LetGroup, got {:?}", other),
        }
    }

    #[test]
    fn letgroup_still_errors_without_bindings() {
        assert!(parse_expr("(1; 2)").is_err());
    }

    #[test]
    fn letgroup_leading_only_no_body_unit() {
        let r = parse_expr("( ~x = 1; )").unwrap();
        match r.kind {
            ExprKind::LetGroup { bindings, body, .. } => {
                assert_eq!(bindings.len(), 1);
                match body.kind {
                    ExprKind::Unit => {}
                    _ => panic!("expected Unit body"),
                }
            }
            _ => panic!("expected LetGroup with Unit body when only leading binds"),
        }
    }

    #[test]
    fn mode_map_empty() {
        let src = ".{}";
        let r = parse_expr(src).expect("empty mode map should parse");
        match r.kind {
            ExprKind::ModeMap(fields) => assert!(fields.is_empty()),
            other => panic!("expected ModeMap, got {:?}", other),
        }
    }

    #[test]
    fn mode_map_single_mode() {
        let src = ".{ Pure: 1 }";
        let r = parse_expr(src).expect("mode map with single mode should parse");
        match r.kind {
            ExprKind::ModeMap(fields) => {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name, "Pure");
                match &fields[0].value.kind {
                    ExprKind::Int(1) => {}
                    other => panic!("expected Int(1), got {:?}", other),
                }
            }
            other => panic!("expected ModeMap, got {:?}", other),
        }
    }

    #[test]
    fn mode_map_multiple_modes() {
        let src = ".{ Pure: 1, Strict: 2, Lazy: 3 }";
        let r = parse_expr(src).expect("mode map with multiple modes should parse");
        match r.kind {
            ExprKind::ModeMap(fields) => {
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[0].name, "Pure");
                assert_eq!(fields[1].name, "Strict");
                assert_eq!(fields[2].name, "Lazy");
            }
            other => panic!("expected ModeMap, got {:?}", other),
        }
    }

    #[test]
    fn mode_map_with_complex_expressions() {
        let src = ".{ Pure: \\x -> x, Strict: (1 + 2) }";
        let r = parse_expr(src).expect("mode map with complex expressions should parse");
        match r.kind {
            ExprKind::ModeMap(fields) => {
                assert_eq!(fields.len(), 2);
                matches!(fields[0].value.kind, ExprKind::Lambda { .. });
            }
            other => panic!("expected ModeMap, got {:?}", other),
        }
    }
}
