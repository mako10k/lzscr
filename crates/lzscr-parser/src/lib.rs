use lzscr_ast::{ast::*, span::Span};
use lzscr_lexer::{lex, Tok};

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("parse error: {0}")]
    Generic(String),
}

pub fn parse_expr(src: &str) -> Result<Expr, ParseError> {
    // Minimal, direct token-based parser for a subset: ints, strings, refs, idents, lambda, apply, block
    let tokens = lex(src);
    let mut i = 0usize;

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
        *i += 1;
        t
    }

    // Shared pattern parser (used by lambda param and let-group bindings)
    fn parse_pattern<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Pattern, ParseError> {
        fn is_upper_initial(s: &str) -> bool {
            s.chars()
                .next()
                .map(|c| c.is_ascii_uppercase())
                .unwrap_or(false)
        }
        fn parse_pat_atom<'c>(
            i: &mut usize,
            toks: &'c [lzscr_lexer::Lexed<'c>],
        ) -> Result<Pattern, ParseError> {
            let t = bump(i, toks).ok_or_else(|| ParseError::Generic("expected pattern".into()))?;
            Ok(match &t.tok {
                Tok::LBracket => {
                    // [p1, p2, ...] or []
                    if let Some(nxt) = toks.get(*i) {
                        if matches!(nxt.tok, Tok::RBracket) {
                            let r = bump(i, toks).unwrap();
                            return Ok(Pattern::new(
                                PatternKind::List(vec![]),
                                Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                ),
                            ));
                        }
                    }
                    let mut items = Vec::new();
                    loop {
                        let p = parse_pattern(i, toks)?;
                        items.push(p);
                        let sep = bump(i, toks).ok_or_else(|| {
                            ParseError::Generic("] or , expected in list pattern".into())
                        })?;
                        match sep.tok {
                            Tok::Comma => continue,
                            Tok::RBracket => {
                                let sp = Span::new(
                                    t.span.offset,
                                    sep.span.offset + sep.span.len - t.span.offset,
                                );
                                break Pattern::new(PatternKind::List(items), sp);
                            }
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected , or ] in list pattern".into(),
                                ))
                            }
                        }
                    }
                }
                Tok::Tilde => {
                    let id = bump(i, toks).ok_or_else(|| {
                        ParseError::Generic("expected ident after ~ in pattern".into())
                    })?;
                    if !matches!(id.tok, Tok::Ident) {
                        return Err(ParseError::Generic(
                            "expected ident after ~ in pattern".into(),
                        ));
                    }
                    let span =
                        Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset);
                    Pattern::new(PatternKind::Var(id.text.to_string()), span)
                }
                Tok::Ident => {
                    let name = t.text.to_string();
                    if name == "_" {
                        Pattern::new(PatternKind::Wildcard, t.span)
                    } else if name == "true" {
                        Pattern::new(PatternKind::Bool(true), t.span)
                    } else if name == "false" {
                        Pattern::new(PatternKind::Bool(false), t.span)
                    } else {
                        // 裸の識別子は変数束縛としては使わない（~ident のみ）。Ctor は別経路で先読み処理。
                        return Err(ParseError::Generic(
                            "invalid bare identifier in pattern; use ~name or a constructor".into(),
                        ));
                    }
                }
                Tok::Member(m) => Pattern::new(
                    PatternKind::Ctor {
                        name: m.clone(),
                        args: vec![],
                    },
                    t.span,
                ),
                Tok::LParen => {
                    // () or tuple pattern or grouped pattern
                    if let Some(nxt) = toks.get(*i) {
                        if matches!(nxt.tok, Tok::RParen) {
                            let r = bump(i, toks).unwrap();
                            return Ok(Pattern::new(
                                PatternKind::Unit,
                                Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                ),
                            ));
                        }
                    }
                    // parse first
                    let first = parse_pattern(i, toks)?;
                    // comma-separated?
                    let mut items = vec![first];
                    loop {
                        let Some(nxt) = toks.get(*i) else {
                            return Err(ParseError::Generic(") expected".into()));
                        };
                        match nxt.tok {
                            Tok::Comma => {
                                let _ = bump(i, toks);
                                let p = parse_pattern(i, toks)?;
                                items.push(p);
                            }
                            Tok::RParen => {
                                let r = bump(i, toks).unwrap();
                                let span = Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                );
                                if items.len() == 1 {
                                    return Ok(items.pop().unwrap());
                                }
                                return Ok(Pattern::new(PatternKind::Tuple(items), span));
                            }
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected , or ) in pattern".into(),
                                ))
                            }
                        }
                    }
                }
                Tok::LBrace => {
                    // record pattern: { k: pat, ... } （キーは ident のみ）
                    // empty {}
                    if let Some(nxt) = toks.get(*i) {
                        if matches!(nxt.tok, Tok::RBrace) {
                            let r = bump(i, toks).unwrap();
                            return Ok(Pattern::new(
                                PatternKind::Record(vec![]),
                                Span::new(
                                    t.span.offset,
                                    r.span.offset + r.span.len - t.span.offset,
                                ),
                            ));
                        }
                    }
                    let mut fields: Vec<(String, Pattern)> = Vec::new();
                    loop {
                        // key (ident)
                        let k = bump(i, toks).ok_or_else(|| {
                            ParseError::Generic("expected key in record pattern".into())
                        })?;
                        let key = match &k.tok {
                            Tok::Ident => k.text.to_string(),
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected ident key in record pattern".into(),
                                ))
                            }
                        };
                        // ':'
                        let col = bump(i, toks).ok_or_else(|| {
                            ParseError::Generic("expected : in record pattern".into())
                        })?;
                        if !matches!(col.tok, Tok::Colon) {
                            return Err(ParseError::Generic(": expected in record pattern".into()));
                        }
                        // value pattern
                        let pv = parse_pattern(i, toks)?;
                        fields.push((key, pv));
                        let sep = bump(i, toks).ok_or_else(|| {
                            ParseError::Generic("expected , or } in record pattern".into())
                        })?;
                        match sep.tok {
                            Tok::Comma => continue,
                            Tok::RBrace => {
                                let span = Span::new(
                                    t.span.offset,
                                    sep.span.offset + sep.span.len - t.span.offset,
                                );
                                return Ok(Pattern::new(PatternKind::Record(fields), span));
                            }
                            _ => {
                                return Err(ParseError::Generic(
                                    "expected , or } in record pattern".into(),
                                ))
                            }
                        }
                    }
                }
                Tok::Int(n) => Pattern::new(PatternKind::Int(*n), t.span),
                Tok::Float(f) => Pattern::new(PatternKind::Float(*f), t.span),
                Tok::Str(s) => Pattern::new(PatternKind::Str(s.clone()), t.span),
                _ => return Err(ParseError::Generic("unexpected token in pattern".into())),
            })
        }
        // lookahead for ctor head: Ident with Uppercase initial or Member
        if let Some(head) = toks.get(*i) {
            match &head.tok {
                Tok::Ident if is_upper_initial(head.text) => {
                    // ctor with possible args
                    let h = bump(i, toks).unwrap();
                    let mut args = Vec::new();
                    loop {
                        let Some(nxt) = toks.get(*i) else { break };
                        match nxt.tok {
                            Tok::Arrow | Tok::RParen | Tok::Comma | Tok::Eq | Tok::Semicolon => {
                                break
                            }
                            Tok::Ident | Tok::Member(_) | Tok::LParen => {
                                let a = parse_pat_atom(i, toks)?;
                                args.push(a);
                            }
                            _ => break,
                        }
                    }
                    let end = if args.is_empty() {
                        h.span.offset + h.span.len
                    } else {
                        let last = args.last().unwrap();
                        last.span.offset + last.span.len
                    };
                    return Ok(Pattern::new(
                        PatternKind::Ctor {
                            name: h.text.to_string(),
                            args,
                        },
                        Span::new(h.span.offset, end - h.span.offset),
                    ));
                }
                Tok::Member(m) => {
                    let h = bump(i, toks).unwrap();
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
                            | Tok::LParen
                            | Tok::Int(_)
                            | Tok::Float(_)
                            | Tok::Str(_) => {
                                let a = parse_pat_atom(i, toks)?;
                                args.push(a);
                            }
                            _ => break,
                        }
                    }
                    let end = if args.is_empty() {
                        h.span.offset + h.span.len
                    } else {
                        let last = args.last().unwrap();
                        last.span.offset + last.span.len
                    };
                    return Ok(Pattern::new(
                        PatternKind::Ctor {
                            name: m.clone(),
                            args,
                        },
                        Span::new(h.span.offset, end - h.span.offset),
                    ));
                }
                _ => {}
            }
        }
        // Pratt-like for cons ':' (right-assoc) with optional '@' suffix for As
        let mut left = parse_pat_atom(i, toks)?;
        loop {
            if let Some(nxt) = toks.get(*i) {
                if matches!(nxt.tok, Tok::Colon) {
                    let _ = bump(i, toks);
                    let right = parse_pattern(i, toks)?; // right-assoc
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
        // As パターン: ('@' pat)?
        if let Some(nxt) = toks.get(*i) {
            if matches!(nxt.tok, Tok::At) {
                let _ = bump(i, toks);
                let right = parse_pattern(i, toks)?;
                let sp = Span::new(
                    left.span.offset,
                    right.span.offset + right.span.len - left.span.offset,
                );
                return Ok(Pattern::new(
                    PatternKind::As(Box::new(left), Box::new(right)),
                    sp,
                ));
            }
        }
        Ok(left)
    }

    fn parse_atom<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Expr, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF".into()))?;
        Ok(match &t.tok {
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
                    let sep = bump(i, toks).ok_or_else(|| ParseError::Generic("] or , expected".into()))?;
                    match sep.tok {
                        Tok::Comma => continue,
                        Tok::RBracket => {
                            let span_all = Span::new(t.span.offset, sep.span.offset + sep.span.len - t.span.offset);
                            return Ok(Expr::new(ExprKind::List(items), span_all));
                        }
                        _ => return Err(ParseError::Generic("expected , or ] in list".into())),
                    }
                }
            }
            // ^(Expr)
            Tok::Caret => {
                let lp = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ( after ^".into()))?;
                if !matches!(lp.tok, Tok::LParen) { return Err(ParseError::Generic("expected ( after ^".into())); }
                let inner = parse_expr_bp(i, toks, 0)?;
                let rp = bump(i, toks).ok_or_else(|| ParseError::Generic(") expected".into()))?;
                if !matches!(rp.tok, Tok::RParen) { return Err(ParseError::Generic(") expected".into())); }
                let span = Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                Expr::new(ExprKind::Raise(Box::new(inner)), span)
            }
            Tok::Int(n) => Expr::new(ExprKind::Int(*n), t.span),
            Tok::Float(f) => Expr::new(ExprKind::Float(*f), t.span),
            Tok::Str(s) => Expr::new(ExprKind::Str(s.clone()), t.span),
            Tok::Member(name) => Expr::new(ExprKind::Symbol(name.clone()), t.span),
            Tok::Tilde => {
                let id = bump(i, toks)
                    .ok_or_else(|| ParseError::Generic("expected ident after ~".into()))?;
                match id.tok {
                    Tok::Ident => Expr::new(
                        ExprKind::Ref(id.text.to_string()),
                        Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset),
                    ),
                    _ => return Err(ParseError::Generic("expected ident after ~".into())),
                }
            }
            Tok::Bang => {
                // !sym  =>  (~effects .sym)
                let id = bump(i, toks)
                    .ok_or_else(|| ParseError::Generic("expected ident after !".into()))?;
                if !matches!(id.tok, Tok::Ident) {
                    return Err(ParseError::Generic("expected ident after !".into()));
                }
                let func = Expr::new(
                    ExprKind::Ref("effects".into()),
                    Span::new(t.span.offset, t.span.len),
                );
                let member = Expr::new(ExprKind::Symbol(format!(".{}", id.text)), id.span);
                let span_all =
                    Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset);
                Expr::new(
                    ExprKind::Apply {
                        func: Box::new(func),
                        arg: Box::new(member),
                    },
                    span_all,
                )
            }
            Tok::Backslash => {
                // parse pattern parameter up to '->'
                let pat = parse_pattern(i, toks)?;
                let arr = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ->".into()))?;
                if !matches!(arr.tok, Tok::Arrow) {
                    return Err(ParseError::Generic("expected ->".into()));
                }
                let body = parse_expr_bp(i, toks, 0)?;
                let span = Span::new(
                    t.span.offset,
                    body.span.offset + body.span.len - t.span.offset,
                );
                Expr::new(
                    ExprKind::Lambda {
                        param: pat,
                        body: Box::new(body),
                    },
                    span,
                )
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

                // Try let-group first, with backtracking fallback to original tuple/group
                let save_i = *i;
                // Parse zero-or-more leading bindings: (Pat = Expr;)* using a lookahead index
                let mut it = *i;
                let mut leading: Vec<(Pattern, Expr)> = Vec::new();
                loop {
                    let before = it;
                    let mut j = it;
                    if let Ok(p) = parse_pattern(&mut j, toks) {
                        if let Some(eq) = toks.get(j) {
                            if matches!(eq.tok, Tok::Eq) {
                                j += 1; // consume '='
                                let ex = parse_expr_bp(&mut j, toks, 0)?;
                                let semi = toks.get(j).ok_or_else(|| {
                                    ParseError::Generic("; expected after let binding".into())
                                })?;
                                if !matches!(semi.tok, Tok::Semicolon) {
                                    return Err(ParseError::Generic(
                                        "; expected after let binding".into(),
                                    ));
                                }
                                j += 1; // consume ';'
                                leading.push((p, ex));
                                it = j;
                                continue;
                            }
                        }
                        // no '=' after a parsed pattern -> stop scanning leading bindings
                        it = before;
                        break;
                    } else {
                        it = before;
                        break;
                    }
                }
                // Parse candidate body expression
                let mut j = it;
                let body_expr = parse_expr_bp(&mut j, toks, 0)?;
                // Optional ';' followed by trailing bindings
                let mut trailing: Vec<(Pattern, Expr)> = Vec::new();
                if let Some(sep) = toks.get(j) {
                    if matches!(sep.tok, Tok::Semicolon) {
                        j += 1; // consume first ';'
                        loop {
                            let before = j;
                            let mut k = j;
                            if let Ok(p2) = parse_pattern(&mut k, toks) {
                                if let Some(eq) = toks.get(k) {
                                    if matches!(eq.tok, Tok::Eq) {
                                        k += 1; // '='
                                        let ex2 = parse_expr_bp(&mut k, toks, 0)?;
                                        let semi2 = toks.get(k).ok_or_else(|| {
                                            ParseError::Generic("; expected after let binding".into())
                                        })?;
                                        if !matches!(semi2.tok, Tok::Semicolon) {
                                            return Err(ParseError::Generic(
                                                "; expected after let binding".into(),
                                            ));
                                        }
                                        k += 1; // ';'
                                        trailing.push((p2, ex2));
                                        j = k;
                                        continue;
                                    }
                                }
                                // parsed a pattern but not a binding -> stop
                                j = before;
                                break;
                            } else {
                                j = before;
                                break;
                            }
                        }
                    }
                }
                // Expect ')'
                if let Some(rp) = toks.get(j) {
                    if matches!(rp.tok, Tok::RParen)
                        && (!leading.is_empty() || !trailing.is_empty())
                    {
                        // commit let-group
                        *i = j + 1;
                        let mut all = leading;
                        all.extend(trailing);
                        let span_all = Span::new(
                            t.span.offset,
                            rp.span.offset + rp.span.len - t.span.offset,
                        );
                        return Ok(Expr::new(
                            ExprKind::LetGroup {
                                bindings: all,
                                body: Box::new(body_expr),
                            },
                            span_all,
                        ));
                    }
                }
                // Fallback: parse as grouped expression or tuple
                *i = save_i;
                let first = parse_expr_bp(i, toks, 0)?;
                let mut items = vec![first];
                loop {
                    let Some(nxt) = peek(*i, toks) else {
                        return Err(ParseError::Generic(") expected".into()));
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
                            // desugar tuple: (a,b,...) => (., a) b ...
                            let mut tuple_expr = Expr::new(ExprKind::Symbol(".,".into()), t.span);
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
                        _ => return Err(ParseError::Generic("expected , or )".into())),
                    }
                }
            }
            Tok::LBrace => {
                // record literal: { k: v, ... } → (.Record (., (.KV "k" v) ...)) の糖衣（キーは ident のみ）
                // 空 {} はブロックの空ではないため特別扱い：Record 空にする
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RBrace) {
                        let r = bump(i, toks).unwrap();
                        let span_all = Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset);
                        // (.Record) 空適用
                        return Ok(Expr::new(ExprKind::Apply { func: Box::new(Expr::new(ExprKind::Ref("Record".into()), t.span)), arg: Box::new(Expr::new(ExprKind::Unit, r.span)) }, span_all));
                    }
                }
                let mut pairs: Vec<(String, Expr)> = Vec::new();
                loop {
                    // key
                    let k = bump(i, toks).ok_or_else(|| ParseError::Generic("expected key".into()))?;
                    let key = match &k.tok { Tok::Ident => k.text.to_string(), _ => return Err(ParseError::Generic("expected ident key".into())) };
                    // :
                    let col = bump(i, toks).ok_or_else(|| ParseError::Generic("expected :".into()))?;
                    if !matches!(col.tok, Tok::Colon) { return Err(ParseError::Generic(": expected".into())); }
                    // value
                    let val = parse_expr_bp(i, toks, 0)?;
                    pairs.push((key, val));
                    // , or }
                    let sep = bump(i, toks).ok_or_else(|| ParseError::Generic("expected , or }".into()))?;
                    match sep.tok {
                        Tok::Comma => continue,
                        Tok::RBrace => {
                            let span_all = Span::new(t.span.offset, sep.span.offset + sep.span.len - t.span.offset);
                            // (., (.KV "k" v) ...)
                            let mut tuple_expr = Expr::new(ExprKind::Symbol(".,".into()), t.span);
                            for (k, v) in pairs {
                                let kv = Expr::new(ExprKind::Apply { func: Box::new(Expr::new(ExprKind::Ref("KV".into()), t.span)), arg: Box::new(Expr::new(ExprKind::Str(k), t.span)) }, t.span);
                                let kv2 = Expr::new(ExprKind::Apply { func: Box::new(kv), arg: Box::new(v) }, t.span);
                                let sp = Span::new(tuple_expr.span.offset, span_all.offset + span_all.len - tuple_expr.span.offset);
                                tuple_expr = Expr::new(ExprKind::Apply { func: Box::new(tuple_expr), arg: Box::new(kv2) }, sp);
                            }
                            // (.Record (., ...))
                            let expr = Expr::new(ExprKind::Apply { func: Box::new(Expr::new(ExprKind::Ref("Record".into()), t.span)), arg: Box::new(tuple_expr) }, span_all);
                            return Ok(expr);
                        }
                        _ => return Err(ParseError::Generic("expected , or }".into())),
                    }
                }
            }
            Tok::Ident => {
                return Err(ParseError::Generic(format!(
                    "bare identifier '{}' cannot be used as a symbol; use ~{} for a ref or .{} for a symbol",
                    t.text, t.text, t.text
                )))
            }
            _ => {
                return Err(ParseError::Generic(format!(
                    "unexpected token: {:?}",
                    t.tok
                )))
            }
        })
    }

    fn parse_expr_bp<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
        bp: u8,
    ) -> Result<Expr, ParseError> {
        let mut lhs = parse_atom(i, toks)?;
        loop {
            let Some(nxt) = peek(*i, toks) else { break };
            // Pratt parser: handle infix precedence
            let (op_bp, op_kind) = match nxt.tok.clone() {
                // lowest precedence for catch and or-else
                Tok::Pipe => (1, Some("|")),
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
                if op == "|" {
                    // parse RHS normally
                    let rhs = parse_expr_bp(i, toks, op_bp + 1)?;
                    let span = Span::new(
                        lhs.span.offset,
                        rhs.span.offset + rhs.span.len - lhs.span.offset,
                    );
                    lhs = Expr::new(
                        ExprKind::OrElse {
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        },
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
                        ExprKind::Apply {
                            func: Box::new(callee),
                            arg: Box::new(lhs),
                        },
                        span,
                    );
                    lhs = Expr::new(
                        ExprKind::Apply {
                            func: Box::new(app1),
                            arg: Box::new(rhs),
                        },
                        span,
                    );
                    continue;
                }
                let rhs = parse_expr_bp(i, toks, op_bp + 1)?;
                // desugar: (a + b) → ((~add a) b) など
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
                let span = Span::new(
                    lhs.span.offset,
                    rhs.span.offset + rhs.span.len - lhs.span.offset,
                );
                let app1 = Expr::new(
                    ExprKind::Apply {
                        func: Box::new(callee),
                        arg: Box::new(lhs),
                    },
                    span,
                );
                lhs = Expr::new(
                    ExprKind::Apply {
                        func: Box::new(app1),
                        arg: Box::new(rhs),
                    },
                    span,
                );
                continue;
            }
            // parse '^|' as infix catch ONLY when the next token after '^' is '|'
            if let Tok::Caret = nxt.tok {
                if let Some(n2) = peek(*i + 1, toks) {
                    if matches!(n2.tok, Tok::Pipe) {
                        let _ = bump(i, toks); // consume '^'
                        let _ = bump(i, toks); // consume '|'
                        let rhs = parse_expr_bp(i, toks, 2)?; // precedence slightly above '|'
                        let span = Span::new(
                            lhs.span.offset,
                            rhs.span.offset + rhs.span.len - lhs.span.offset,
                        );
                        lhs = Expr::new(
                            ExprKind::Catch {
                                left: Box::new(lhs),
                                right: Box::new(rhs),
                            },
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
                    // sugar: true() / false()  =>  ~true / ~false
                    if let ExprKind::Symbol(name) = &lhs.kind {
                        if matches!(arg.kind, ExprKind::Unit) && (name == "true" || name == "false")
                        {
                            lhs = Expr::new(ExprKind::Ref(name.clone()), span);
                            continue;
                        }
                    }
                    lhs = Expr::new(
                        ExprKind::Apply {
                            func: Box::new(lhs),
                            arg: Box::new(arg),
                        },
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
            Err(ParseError::Generic(msg)) => assert!(
                msg.contains("expected ident key"),
                "unexpected msg: {}",
                msg
            ),
            other => panic!("expected Err(ParseError::Generic), got {:?}", other),
        }
    }

    #[test]
    fn record_pattern_string_key_rejected() {
        let src = "\\{ \"a\": ~x } -> ~x";
        let r = parse_expr(src);
        match r {
            Err(ParseError::Generic(msg)) => assert!(
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
    fn pattern_list_bare_ident_rejected() {
        let src = "\\[ x ] -> ~x";
        let r = parse_expr(src);
        match r {
            Err(ParseError::Generic(msg)) => assert!(
                msg.contains("invalid bare identifier in pattern")
                    || msg.contains("expected ident after ~ in pattern"),
                "unexpected msg: {}",
                msg
            ),
            other => panic!("expected Err, got {:?}", other),
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
    fn let_group_parse_basic() {
        // ( ~x = 1; ~x; ~y = 2; )
        let src = "( ~x = 1; ~x; ~y = 2; )";
        let r = parse_expr(src).unwrap();
        match r.kind {
            ExprKind::LetGroup { bindings, body } => {
                assert_eq!(bindings.len(), 2);
                match body.kind {
                    ExprKind::Ref(n) => assert_eq!(n, "x"),
                    _ => panic!("expected body ~x"),
                }
            }
            other => panic!("expected LetGroup, got {:?}", other),
        }
    }
}
