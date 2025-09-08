use lzscr_ast::{ast::*, span::Span};
use lzscr_lexer::{lex, Tok};
use lzscr_preast as preast;

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("parse error: {0}")]
    Generic(String),
    #[error("parse error: {msg} at ({span_offset}, {span_len})")]
    WithSpan { msg: String, span_offset: usize, span_len: usize },
}

pub fn parse_expr(src: &str) -> Result<Expr, ParseError> {
    // Phase 0: PRE-AST (preserve comments, prepare token stream)
    let _pre = preast::to_preast(src).map_err(|e| ParseError::Generic(e.to_string()))?;
    // Minimal, direct token-based parser for a subset: ints, strings, refs, idents, lambda, apply, block
    let mut tokens = lex(src);
    // Drop comments for parsing; they are handled in PRE-AST/formatter layer
    tokens.retain(|t| !matches!(t.tok, Tok::CommentLine | Tok::CommentBlock));
    let mut i = 0usize;
    // Small token helpers
    fn peek<'a>(i: usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Option<&'a lzscr_lexer::Lexed<'a>> {
        toks.get(i)
    }
    fn bump<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Option<&'a lzscr_lexer::Lexed<'a>> {
        let t = toks.get(*i);
        if t.is_some() { *i += 1; }
        t
    }

    // Pattern parsing -----------------------------------------------------
    fn parse_pat_atom<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Result<Pattern, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF in pattern".into()))?;
        let pat = match &t.tok {
            Tok::Tilde => {
                let id = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ident after ~ in pattern".into()))?;
                match id.tok {
                    Tok::Ident => Pattern::new(PatternKind::Var(id.text.to_string()), Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset)),
                    _ => return Err(ParseError::Generic("expected ident after ~ in pattern".into())),
                }
            }
            Tok::LParen => {
                // () or (p) or (p1, p2, ...)
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::RParen) {
                        let r = bump(i, toks).unwrap();
                        return Ok(Pattern::new(PatternKind::Unit, Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset)));
                    }
                } else {
                    return Err(ParseError::Generic(") expected".into()));
                }
                let first = parse_pattern(i, toks)?;
                if let Some(n2) = peek(*i, toks) {
                    if matches!(n2.tok, Tok::Comma) {
                        let _ = bump(i, toks); // consume ','
                        let mut items = vec![first];
                        loop {
                            let p2 = parse_pattern(i, toks)?;
                            items.push(p2);
                            let sep = bump(i, toks).ok_or_else(|| ParseError::Generic(") expected in tuple pattern".into()))?;
                            match sep.tok {
                                Tok::Comma => continue,
                                Tok::RParen => break,
                                _ => return Err(ParseError::Generic("expected , or ) in tuple pattern".into())),
                            }
                        }
                        let end = toks.get(*i - 1).map(|r| r.span.offset + r.span.len).unwrap_or(t.span.offset + t.span.len);
                        Pattern::new(PatternKind::Tuple(items), Span::new(t.span.offset, end - t.span.offset))
                    } else if matches!(n2.tok, Tok::RParen) {
                        let rp = bump(i, toks).unwrap();
                        let mut p = first;
                        p.span = Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                        p
                    } else {
                        return Err(ParseError::Generic("expected , or ) in tuple/group pattern".into()));
                    }
                } else {
                    return Err(ParseError::Generic(") expected".into()));
                }
            }
            Tok::LBracket => {
                // [p1, p2, ...]
                if let Some(nxt) = peek(*i, toks) { if matches!(nxt.tok, Tok::RBracket) {
                    let r = bump(i, toks).unwrap();
                    Pattern::new(PatternKind::List(vec![]), Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset))
                } else {
                    let mut items = Vec::new();
                    loop {
                        let p = parse_pattern(i, toks)?;
                        items.push(p);
                        let sep = bump(i, toks).ok_or_else(|| ParseError::Generic("] or , expected in list pattern".into()))?;
                        match sep.tok { Tok::Comma => continue, Tok::RBracket => break, _ => return Err(ParseError::Generic("expected , or ] in list pattern".into())) }
                    }
                    let end = toks.get(*i - 1).map(|r| r.span.offset + r.span.len).unwrap_or(t.span.offset + t.span.len);
                    Pattern::new(PatternKind::List(items), Span::new(t.span.offset, end - t.span.offset))
                }} else { return Err(ParseError::Generic("] expected".into())); }
            }
            Tok::LBrace => {
                // { k: p, ... }
                if let Some(nxt) = peek(*i, toks) { if matches!(nxt.tok, Tok::RBrace) {
                    let r = bump(i, toks).unwrap();
                    Pattern::new(PatternKind::Record(vec![]), Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset))
                } else {
                    let mut fields: Vec<(String, Pattern)> = Vec::new();
                    loop {
                        let k = bump(i, toks).ok_or_else(|| ParseError::Generic("expected key in record pattern".into()))?;
                        let key = match &k.tok { Tok::Ident => k.text.to_string(), _ => return Err(ParseError::Generic("expected ident key in record pattern".into())) };
                        let col = bump(i, toks).ok_or_else(|| ParseError::Generic(": expected after key in record pattern".into()))?;
                        if !matches!(col.tok, Tok::Colon) { return Err(ParseError::Generic(": expected after key in record pattern".into())); }
                        let p = parse_pattern(i, toks)?;
                        fields.push((key, p));
                        let sep = bump(i, toks).ok_or_else(|| ParseError::Generic(", or } expected in record pattern".into()))?;
                        match sep.tok { Tok::Comma => continue, Tok::RBrace => break, _ => return Err(ParseError::Generic("expected , or } in record pattern".into())) }
                    }
                    let end = toks.get(*i - 1).map(|r| r.span.offset + r.span.len).unwrap_or(t.span.offset + t.span.len);
                    Pattern::new(PatternKind::Record(fields), Span::new(t.span.offset, end - t.span.offset))
                }} else { return Err(ParseError::Generic("} expected".into())); }
            }
            Tok::Int(n) => Pattern::new(PatternKind::Int(*n), t.span),
            Tok::Float(f) => Pattern::new(PatternKind::Float(*f), t.span),
            Tok::Str(s) => Pattern::new(PatternKind::Str(s.clone()), t.span),
            Tok::Char(c) => Pattern::new(PatternKind::Char(*c), t.span),
            Tok::Ident => {
                if t.text == "true" || t.text == "false" {
                    Pattern::new(PatternKind::Bool(t.text == "true"), t.span)
                } else if t.text == "_" {
                    Pattern::new(PatternKind::Wildcard, t.span)
                } else {
                    let h = t;
                    let mut args = Vec::new();
                    loop {
                        let Some(nxt) = toks.get(*i) else { break };
                        match nxt.tok {
                            Tok::Arrow | Tok::RParen | Tok::Comma | Tok::Eq | Tok::Semicolon => break,
                            Tok::Tilde | Tok::Ident | Tok::Member(_) | Tok::LBracket | Tok::LBrace | Tok::LParen | Tok::Int(_) | Tok::Float(_) | Tok::Str(_) | Tok::Char(_) => {
                                let a = parse_pat_atom(i, toks)?; args.push(a);
                            }
                            _ => break,
                        }
                    }
                    let end = if args.is_empty() { h.span.offset + h.span.len } else { let last = args.last().unwrap(); last.span.offset + last.span.len };
                    Pattern::new(PatternKind::Ctor { name: h.text.to_string(), args }, Span::new(h.span.offset, end - h.span.offset))
                }
            }
            Tok::Member(m) => {
                let h = t;
                let mut args = Vec::new();
                loop {
                    let Some(nxt) = toks.get(*i) else { break };
                    match nxt.tok {
                        Tok::Arrow | Tok::RParen | Tok::Comma | Tok::Eq | Tok::Semicolon => break,
                        Tok::Tilde | Tok::Ident | Tok::Member(_) | Tok::LBracket | Tok::LBrace | Tok::LParen | Tok::Int(_) | Tok::Float(_) | Tok::Str(_) | Tok::Char(_) => {
                            let a = parse_pat_atom(i, toks)?; args.push(a);
                        }
                        _ => break,
                    }
                }
                let end = if args.is_empty() { h.span.offset + h.span.len } else { let last = args.last().unwrap(); last.span.offset + last.span.len };
                Pattern::new(PatternKind::Ctor { name: m.clone(), args }, Span::new(h.span.offset, end - h.span.offset))
            }
            _ => return Err(ParseError::Generic("unexpected token in pattern".into())),
        };
        Ok(pat)
    }

    fn parse_pattern<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Result<Pattern, ParseError> {
        // baseline atom
        let mut left = parse_pat_atom(i, toks)?;
        // cons right-assoc
        loop {
            if let Some(nxt) = toks.get(*i) { if matches!(nxt.tok, Tok::Colon) {
                let _ = bump(i, toks);
                let right = parse_pattern(i, toks)?;
                let sp = Span::new(left.span.offset, right.span.offset + right.span.len - left.span.offset);
                left = Pattern::new(PatternKind::Cons(Box::new(left), Box::new(right)), sp);
                continue;
            }}
            break;
        }
        // As pattern: '@' pat
        if let Some(nxt) = toks.get(*i) { if matches!(nxt.tok, Tok::At) {
            let _ = bump(i, toks);
            let right = parse_pattern(i, toks)?;
            let sp = Span::new(left.span.offset, right.span.offset + right.span.len - left.span.offset);
            return Ok(Pattern::new(PatternKind::As(Box::new(left), Box::new(right)), sp));
        }}
        Ok(left)
    }

    // Let-LHS sugar: ~name p1 p2 ... =
    fn try_parse_lhs_param_chain<'a>(j: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Option<(String, Vec<Pattern>, usize)> {
        let save = *j;
        let t0 = toks.get(*j)?;
        if !matches!(t0.tok, Tok::Tilde) { return None; }
        *j += 1;
        let id = toks.get(*j)?;
        if !matches!(id.tok, Tok::Ident) { *j = save; return None; }
        let name = id.text.to_string();
        *j += 1;
        let mut params: Vec<Pattern> = Vec::new();
        loop {
            if let Some(nxt) = toks.get(*j) { if matches!(nxt.tok, Tok::Eq) { break; } }
            let before = *j;
            if let Ok(p) = parse_pattern(j, toks) { params.push(p); } else { *j = before; break; }
        }
        if let Some(eq) = toks.get(*j) { if matches!(eq.tok, Tok::Eq) {
            return Some((name, params, *j));
        }}
        *j = save;
        None
    }

    fn collect_binders(p: &Pattern, names: &mut Vec<String>) {
        match &p.kind {
            PatternKind::Var(n) => names.push(n.clone()),
            PatternKind::Tuple(xs) | PatternKind::List(xs) => { for x in xs { collect_binders(x, names); } }
            PatternKind::Ctor { args, .. } => { for x in args { collect_binders(x, names); } }
            PatternKind::Record(fields) => { for (_k, v) in fields { collect_binders(v, names); } }
            PatternKind::Cons(a, b) | PatternKind::As(a, b) => { collect_binders(a, names); collect_binders(b, names); }
            PatternKind::TypeBind { pat, .. } => { collect_binders(pat, names); }
            _ => {}
        }
    }

    fn nest_lambdas(params: &[Pattern], body: Expr) -> Expr {
        let mut acc = body;
        for p in params.iter().rev() {
            let span = Span::new(p.span.offset, acc.span.offset + acc.span.len - p.span.offset);
            acc = Expr::new(ExprKind::Lambda { param: p.clone(), body: Box::new(acc) }, span);
        }
        acc
    }

    fn parse_atom<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Expr, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF".into()))?;
        Ok(match &t.tok {
            Tok::TypeOpen => {
                // %{ TypeExpr } [Expr?]
                fn parse_type<'b>(j: &mut usize, toks: &'b [lzscr_lexer::Lexed<'b>]) -> Result<TypeExpr, ParseError> {
                    // Pratt for -> (right-assoc), with atoms: Unit/Int/Float/Bool/Str/Ident/Member, tuple, list, record, holes
                    fn parse_type_atom<'c>(j: &mut usize, toks: &'c [lzscr_lexer::Lexed<'c>]) -> Result<TypeExpr, ParseError> {
                        let t = bump(j, toks).ok_or_else(|| ParseError::Generic("expected type".into()))?;
            Ok(match &t.tok {
        Tok::Ident => {
                                match t.text {
                                    "Unit" => TypeExpr::Unit,
                                    "Int" => TypeExpr::Int,
                                    "Float" => TypeExpr::Float,
                                    "Bool" => TypeExpr::Bool,
                    "Str" => TypeExpr::Str,
                    "Char" => TypeExpr::Char,
                                    other => TypeExpr::Ctor { tag: other.to_string(), args: vec![] },
                                }
                            },
                            Tok::TyVar(name) => TypeExpr::Var(name.clone()),
                            Tok::Member(name) => TypeExpr::Ctor { tag: name.clone(), args: vec![] },
                            Tok::LBracket => {
                                // [T]
                                let inner = parse_type(j, toks)?;
                                let rb = bump(j, toks).ok_or_else(|| ParseError::Generic("] expected in type".into()))?;
                                if !matches!(rb.tok, Tok::RBracket) { return Err(ParseError::Generic("] expected in type".into())); }
                                TypeExpr::List(Box::new(inner))
                            }
                            Tok::LParen => {
                                // () or (T1, T2, ...)
                                if let Some(nxt) = toks.get(*j) { if matches!(nxt.tok, Tok::RParen) {
                                    let _ = bump(j, toks);
                                    return Ok(TypeExpr::Tuple(vec![]));
                                }}
                                let first = parse_type(j, toks)?;
                                let mut items = vec![first];
                                loop {
                                    let Some(nxt) = toks.get(*j) else { return Err(ParseError::Generic(") expected in type".into())); };
                                    match nxt.tok {
                                        Tok::Comma => { let _ = bump(j, toks); let t2 = parse_type(j, toks)?; items.push(t2); }
                                        Tok::RParen => { let _ = bump(j, toks); break; }
                                        _ => return Err(ParseError::Generic("expected , or ) in type".into())),
                                    }
                                }
                                TypeExpr::Tuple(items)
                            }
                            Tok::LBrace => {
                                // { k: T, ... }
                                if let Some(nxt) = toks.get(*j) { if matches!(nxt.tok, Tok::RBrace) { let _= bump(j, toks); return Ok(TypeExpr::Record(vec![])); } }
                                let mut fields = Vec::new();
                                loop {
                                    let k = bump(j, toks).ok_or_else(|| ParseError::Generic("expected key in type record".into()))?;
                                    let key = match &k.tok { Tok::Ident => k.text.to_string(), _ => return Err(ParseError::Generic("expected ident key in type record".into())) };
                                    let col = bump(j, toks).ok_or_else(|| ParseError::Generic(": expected in type record".into()))?;
                                    if !matches!(col.tok, Tok::Colon) { return Err(ParseError::Generic(": expected in type record".into())); }
                                    let tv = parse_type(j, toks)?;
                                    fields.push((key, tv));
                                    let sep = bump(j, toks).ok_or_else(|| ParseError::Generic("expected , or } in type record".into()))?;
                                    match sep.tok { Tok::Comma => continue, Tok::RBrace => break, _ => return Err(ParseError::Generic("expected , or } in type record".into())), }
                                }
                                TypeExpr::Record(fields)
                            }
                            Tok::Question => {
                                // ? or ?name
                                let name = if let Some(nx) = toks.get(*j) { if matches!(nx.tok, Tok::Ident) { let n = nx.text.to_string(); let _ = bump(j, toks); Some(n) } else { None } } else { None };
                                TypeExpr::Hole(name)
                            }
                            _ => return Err(ParseError::Generic("unexpected token in type".into())),
                        })
                    }
                    fn parse_type_bp<'c>(j: &mut usize, toks: &'c [lzscr_lexer::Lexed<'c>], bp: u8) -> Result<TypeExpr, ParseError> {
                        let mut lhs = parse_type_atom(j, toks)?;
                        loop {
                            let Some(nxt) = toks.get(*j) else { break };
                            // only -> with right associativity
                            if matches!(nxt.tok, Tok::Arrow) {
                                if 5 < bp { break; }
                                let _ = bump(j, toks);
                                let rhs = parse_type_bp(j, toks, 5)?;
                                lhs = TypeExpr::Fun(Box::new(lhs), Box::new(rhs));
                                continue;
                            }
                            break;
                        }
                        Ok(lhs)
                    }
                    parse_type_bp(j, toks, 0)
                }
                let mut j = *i;
                let ty = parse_type(&mut j, toks)?;
                let rb = bump(&mut j, toks).ok_or_else(|| ParseError::Generic("} expected after type".into()))?;
                if !matches!(rb.tok, Tok::RBrace) { return Err(ParseError::Generic("} expected after type".into())); }
                // lookahead for an expression atom to annotate; if next token begins an atom/lambda/ref/etc, parse it
                if let Some(nxt) = toks.get(j) {
                    match nxt.tok {
                        Tok::LParen | Tok::LBracket | Tok::LBrace | Tok::Int(_) | Tok::Float(_) | Tok::Str(_) | Tok::Tilde | Tok::Backslash | Tok::Ident | Tok::Member(_) | Tok::Bang | Tok::Caret | Tok::TypeOpen => {
                            // annotation applies to following atom/expression (parse full expr at top level bp)
                            *i = j;
                            let ex = parse_expr_bp(i, toks, 0)?;
                            let span = Span::new(t.span.offset, ex.span.offset + ex.span.len - t.span.offset);
                            return Ok(Expr::new(ExprKind::Annot { ty, expr: Box::new(ex) }, span));
                        }
                        _ => {}
                    }
                }
                // otherwise, type value
                *i = j;
                let span = Span::new(t.span.offset, rb.span.offset + rb.span.len - t.span.offset);
                Expr::new(ExprKind::TypeVal(ty), span)
            },
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
            },
            // ^(Expr)
            Tok::Caret => {
                let lp = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ( after ^".into()))?;
                if !matches!(lp.tok, Tok::LParen) { return Err(ParseError::Generic("expected ( after ^".into())); }
                let inner = parse_expr_bp(i, toks, 0)?;
                let rp = bump(i, toks).ok_or_else(|| ParseError::Generic(") expected".into()))?;
                if !matches!(rp.tok, Tok::RParen) { return Err(ParseError::Generic(") expected".into())); }
                let span = Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                Expr::new(ExprKind::Raise(Box::new(inner)), span)
            },
            Tok::Int(n) => Expr::new(ExprKind::Int(*n), t.span),
            Tok::Float(f) => Expr::new(ExprKind::Float(*f), t.span),
            Tok::Str(s) => Expr::new(ExprKind::Str(s.clone()), t.span),
            Tok::Char(c) => Expr::new(ExprKind::Char(*c), t.span),
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
                // Two forms:
                // - !sym  =>  (~effects .sym)
                // - !{ do-notation } => desugar to chain/bind chaining
                if let Some(nxt) = peek(*i, toks) {
                    if matches!(nxt.tok, Tok::LBrace) {
                        let _l = bump(i, toks).unwrap(); // consume '{'
                        // Parse statements until '}'
                        enum DoStmt { Bind(Pattern, Expr), Expr(Expr) }
                        let mut stmts: Vec<DoStmt> = Vec::new();
                        let mut final_expr: Option<Expr> = None;
                        loop {
                            let Some(cur) = peek(*i, toks) else {
                                return Err(ParseError::Generic("} expected".into()));
                            };
                            if matches!(cur.tok, Tok::RBrace) {
                                let r = bump(i, toks).unwrap();
                                let span_all = Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset);
                                // Desugar to nested ~chain/~bind
                                let mut acc = final_expr.ok_or_else(|| ParseError::Generic("empty do-block".into()))?;
                                // Base case for last expression: (~bind acc (\x -> x)) to run in effect-context and yield value
                                let x_pat = Pattern::new(PatternKind::Var("_x_do".into()), acc.span);
                                let id_lam_span = Span::new(acc.span.offset, acc.span.offset + acc.span.len - acc.span.offset);
                                let id_lam = Expr::new(ExprKind::Lambda { param: x_pat, body: Box::new(Expr::new(ExprKind::Ref("_x_do".into()), acc.span)) }, id_lam_span);
                                let bind_ref = Expr::new(ExprKind::Ref("bind".into()), t.span);
                                let app_b1 = Expr::new(ExprKind::Apply { func: Box::new(bind_ref), arg: Box::new(acc) }, span_all);
                                acc = Expr::new(ExprKind::Apply { func: Box::new(app_b1), arg: Box::new(id_lam) }, span_all);
                                for stmt in stmts.into_iter().rev() {
                                    match stmt {
                                        DoStmt::Expr(e) => {
                                            let chain_ref = Expr::new(ExprKind::Ref("chain".into()), t.span);
                                            let app_c1 = Expr::new(ExprKind::Apply { func: Box::new(chain_ref), arg: Box::new(e) }, span_all);
                                            acc = Expr::new(ExprKind::Apply { func: Box::new(app_c1), arg: Box::new(acc) }, span_all);
                                        }
                                        DoStmt::Bind(p, e) => {
                                            let bind_ref = Expr::new(ExprKind::Ref("bind".into()), t.span);
                                            let lam_span = Span::new(p.span.offset, acc.span.offset + acc.span.len - p.span.offset);
                                            let lam = Expr::new(ExprKind::Lambda { param: p, body: Box::new(acc) }, lam_span);
                                            let app_b1 = Expr::new(ExprKind::Apply { func: Box::new(bind_ref), arg: Box::new(e) }, span_all);
                                            acc = Expr::new(ExprKind::Apply { func: Box::new(app_b1), arg: Box::new(lam) }, span_all);
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
                                        let semi = bump(i, toks).ok_or_else(|| ParseError::Generic("; expected after <- expr".into()))?;
                                        if !matches!(semi.tok, Tok::Semicolon) {
                                            return Err(ParseError::Generic("; expected after <- expr".into()));
                                        }
                                        stmts.push(DoStmt::Bind(pat, ex));
                                        continue;
                                    }
                                }
                                // not a '<-' after pattern: backtrack; treat as body expr
                                *i = before;
                            }
                            // Parse an expression statement; if followed by ';', it's a sequencing expr; otherwise it's the final expr
                            let ex = parse_expr_bp(i, toks, 0)?;
                            if let Some(semi) = peek(*i, toks) {
                                if matches!(semi.tok, Tok::Semicolon) {
                                    let _ = bump(i, toks);
                                    stmts.push(DoStmt::Expr(ex));
                                    continue;
                                }
                            }
                            if final_expr.is_some() {
                                return Err(ParseError::Generic("unexpected extra expression in do-block".into()));
                            }
                            final_expr = Some(ex);
                        }
                    }
                }
                let nxt = bump(i, toks)
                    .ok_or_else(|| ParseError::Generic("expected ident or .member after !".into()))?;
                let func = Expr::new(
                    ExprKind::Ref("effects".into()),
                    Span::new(t.span.offset, t.span.len),
                );
                let member = match &nxt.tok {
                    Tok::Ident => Expr::new(ExprKind::Symbol(format!(".{}", nxt.text)), nxt.span),
                    Tok::Member(name) => Expr::new(ExprKind::Symbol(name.clone()), nxt.span),
                    _ => return Err(ParseError::Generic("expected ident or .member after !".into())),
                };
                let span_all =
                    Span::new(t.span.offset, nxt.span.offset + nxt.span.len - t.span.offset);
                Expr::new(
                    ExprKind::Apply {
                        func: Box::new(func),
                        arg: Box::new(member),
                    },
                    span_all,
                )
            },
            Tok::Backslash => {
                // Support multi-parameter lambdas: \p1 p2 ... -> body
                // Collect one or more patterns until '->'
                let mut params: Vec<Pattern> = Vec::new();
                // first parameter is required
                let first = parse_pattern(i, toks)?;
                params.push(first);
                loop {
                    match peek(*i, toks) {
                        Some(tok) if matches!(tok.tok, Tok::Arrow) => {
                            let _ = bump(i, toks); // consume '->'
                            break;
                        }
                        Some(_) => {
                            // try to parse another parameter pattern
                            let before = *i;
                            if let Ok(p) = parse_pattern(i, toks) {
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
                            let end = toks
                                .last()
                                .map(|t| t.span.offset + t.span.len)
                                .unwrap_or(0);
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
                for p in &params { collect_binders(p, &mut names); }
                names.sort();
                if names.windows(2).any(|w| w[0] == w[1]) {
                    return Err(ParseError::Generic("duplicate binder in lambda parameter chain".into()));
                }
                // Parse body with higher binding power than '|'
                let body = parse_expr_bp(i, toks, 2)?;
                // Build nested lambdas from params
                let lam = nest_lambdas(&params, body);
                lam
            },
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
                let mut leading: Vec<(Pattern, Expr)> = Vec::new();
                loop {
                    let before = it;
                    let mut j = it;
                    // Try param-chain: ~name pat* = expr ;
                    if let Some((fname, params, _)) = try_parse_lhs_param_chain(&mut j, toks) {
                        // check duplicate binders
                        let mut names: Vec<String> = Vec::new();
                        for p in &params { collect_binders(p, &mut names); }
                        names.sort();
                        if names.windows(2).any(|w| w[0] == w[1]) {
                            return Err(ParseError::Generic("duplicate binder in parameter chain on let LHS".into()));
                        }
                        let _eq = bump(&mut j, toks).unwrap();
                        let ex = parse_expr_bp(&mut j, toks, 0)?;
                        let semi = toks.get(j).ok_or_else(|| ParseError::Generic("; expected after let binding".into()))?;
                        if !matches!(semi.tok, Tok::Semicolon) { return Err(ParseError::Generic("; expected after let binding".into())); }
                        j += 1;
                        let body = nest_lambdas(&params, ex);
                        let pat = Pattern::new(PatternKind::Var(fname), t.span);
                        leading.push((pat, body));
                        it = j;
                        continue;
                    }
                    // Plain: pat = expr ;
                    if let Ok(p) = parse_pattern(&mut j, toks) {
                        if let Some(eq) = toks.get(j) { if matches!(eq.tok, Tok::Eq) {
                            j += 1;
                            let ex = parse_expr_bp(&mut j, toks, 0)?;
                            let semi = toks.get(j).ok_or_else(|| ParseError::Generic("; expected after let binding".into()))?;
                            if !matches!(semi.tok, Tok::Semicolon) { return Err(ParseError::Generic("; expected after let binding".into())); }
                            j += 1;
                            leading.push((p, ex));
                            it = j;
                            continue;
                        }}
                    }
                    it = before;
                    break;
                }
                // Body
                let mut j = it;
                let body_expr = parse_expr_bp(&mut j, toks, 0)?;
                // Optional trailing bindings
                let mut trailing: Vec<(Pattern, Expr)> = Vec::new();
                if let Some(sep) = toks.get(j) {
                    if matches!(sep.tok, Tok::Semicolon) {
                        j += 1;
                        loop {
                            let before = j;
                            let mut k = j;
                            if let Some((fname, params, _)) = try_parse_lhs_param_chain(&mut k, toks) {
                                let mut names: Vec<String> = Vec::new();
                                for p in &params { collect_binders(p, &mut names); }
                                names.sort();
                                if names.windows(2).any(|w| w[0] == w[1]) {
                                    return Err(ParseError::Generic("duplicate binder in parameter chain on let LHS".into()));
                                }
                                let _eq = bump(&mut k, toks).unwrap();
                                let ex2 = parse_expr_bp(&mut k, toks, 0)?;
                                let semi2 = toks.get(k).ok_or_else(|| ParseError::Generic("; expected after let binding".into()))?;
                                if !matches!(semi2.tok, Tok::Semicolon) { return Err(ParseError::Generic("; expected after let binding".into())); }
                                k += 1;
                                let body2 = nest_lambdas(&params, ex2);
                                let pat2 = Pattern::new(PatternKind::Var(fname), t.span);
                                trailing.push((pat2, body2));
                                j = k;
                                continue;
                            }
                            if let Ok(p2) = parse_pattern(&mut k, toks) {
                                if let Some(eq) = toks.get(k) { if matches!(eq.tok, Tok::Eq) {
                                    k += 1;
                                    let ex2 = parse_expr_bp(&mut k, toks, 0)?;
                                    let semi2 = toks.get(k).ok_or_else(|| ParseError::Generic("; expected after let binding".into()))?;
                                    if !matches!(semi2.tok, Tok::Semicolon) { return Err(ParseError::Generic("; expected after let binding".into())); }
                                    k += 1;
                                    trailing.push((p2, ex2));
                                    j = k;
                                    continue;
                                }}
                            }
                            j = before;
                            break;
                        }
                    }
                }
                // Commit if we have at least one binding and next token is ')'
                if let Some(rp) = toks.get(j) {
                    if matches!(rp.tok, Tok::RParen) && (!leading.is_empty() || !trailing.is_empty()) {
                        *i = j + 1;
                        let mut all = leading;
                        all.extend(trailing);
                        let span_all = Span::new(t.span.offset, rp.span.offset + rp.span.len - t.span.offset);
                        return Ok(Expr::new(ExprKind::LetGroup { type_decls: vec![], bindings: all, body: Box::new(body_expr) }, span_all));
                    }
                }

                // Fallback: group/tuple
                *i = save_i;
                let first = parse_expr_bp(i, toks, 0)?;
                let mut items = vec![first];
                loop {
                    let Some(nxt) = peek(*i, toks) else { return Err(ParseError::Generic(") expected".into())); };
                    match nxt.tok {
                        Tok::Comma => { let _ = bump(i, toks); let e = parse_expr_bp(i, toks, 0)?; items.push(e); }
                        Tok::RParen => {
                            let r = bump(i, toks).unwrap();
                            let span_all = Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset);
                            if items.len() == 1 { return Ok(items.pop().unwrap()); }
                            let mut tuple_expr = Expr::new(ExprKind::Symbol(".,".into()), t.span);
                            for it in items {
                                let sp = Span::new(tuple_expr.span.offset, span_all.offset + span_all.len - tuple_expr.span.offset);
                                tuple_expr = Expr::new(ExprKind::Apply { func: Box::new(tuple_expr), arg: Box::new(it) }, sp);
                            }
                            return Ok(tuple_expr);
                        }
                        _ => return Err(ParseError::Generic("expected , or )".into())),
                    }
                }
            },
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
                    // Optional parameter chain before ':' :  key Pat* : Expr  (Pat* may be empty)
                    let mut params: Vec<Pattern> = Vec::new();
                    // Greedily parse patterns until ':'
                    loop {
                        let save = *i;
                        // Stop at ':'
                        if let Some(nx) = peek(*i, toks) { if matches!(nx.tok, Tok::Colon) { break; } }
                        // Try a pattern; if fails, stop (will error on ':' check below)
                        if let Ok(p) = parse_pattern(i, toks) { params.push(p); } else { *i = save; break; }
                    }
                    // ':'
                    let col = bump(i, toks).ok_or_else(|| ParseError::Generic("expected :".into()))?;
                    if !matches!(col.tok, Tok::Colon) { return Err(ParseError::Generic(": expected".into())); }
                    // value
                    let mut val = parse_expr_bp(i, toks, 0)?;
                    if !params.is_empty() {
                        // enforce duplicate-binder prohibition in field param chain
                        let mut names = Vec::new();
                        for p in &params { collect_binders(p, &mut names); }
                        names.sort();
                        if names.windows(2).any(|w| w[0] == w[1]) {
                            return Err(ParseError::Generic("duplicate binder in record field parameter chain".into()));
                        }
                        val = nest_lambdas(&params, val);
                    }
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
            },
            Tok::Ident => {
                return Err(ParseError::WithSpan { msg: format!(
                    "bare identifier '{}' cannot be used as a symbol; use ~{} for a ref or .{} for a symbol",
                    t.text, t.text, t.text
                ), span_offset: t.span.offset, span_len: t.span.len })
            },
            _ => {
                return Err(ParseError::WithSpan { msg: format!(
                    "unexpected token: {:?}",
                    t.tok
                ), span_offset: t.span.offset, span_len: t.span.len })
            },
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
                    // new alt-lambda: requires both sides to be lambdas syntactically
                    let rhs = parse_expr_bp(i, toks, op_bp)?; // right-assoc: same bp for RHS
                                                              // Validate shapes here; leave to analyzer/runtime if needed
                    match (&lhs.kind, &rhs.kind) {
                        (ExprKind::Lambda { .. }, ExprKind::Lambda { .. })
                        | (ExprKind::Lambda { .. }, ExprKind::AltLambda { .. })
                        | (ExprKind::AltLambda { .. }, ExprKind::Lambda { .. })
                        | (ExprKind::AltLambda { .. }, ExprKind::AltLambda { .. }) => {
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
                        _ => {
                            return Err(ParseError::Generic(
                                "'|' expects lambdas on both sides".into(),
                            ));
                        }
                    }
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
                    // sugar: true() / false()  =>  ~true / ~false
                    if let ExprKind::Symbol(name) = &lhs.kind {
                        if matches!(arg.kind, ExprKind::Unit) && (name == "true" || name == "false")
                        {
                            lhs = Expr::new(ExprKind::Ref(name.clone()), span);
                            continue;
                        }
                    }
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
    fn type_annotation_parses() {
        let src = "%{Int} 1";
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
        let src = "%{Int -> Int}";
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
    fn alt_lambda_requires_lambdas() {
        let src = "(\\~x -> ~x) | 1";
        let r = parse_expr(src);
        assert!(matches!(r, Err(ParseError::Generic(msg)) if msg.contains("expects lambdas")));
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
}
