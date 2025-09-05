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

    fn parse_atom<'a>(
        i: &mut usize,
        toks: &'a [lzscr_lexer::Lexed<'a>],
    ) -> Result<Expr, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF".into()))?;
        Ok(match &t.tok {
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
                let id =
                    bump(i, toks).ok_or_else(|| ParseError::Generic("expected param".into()))?;
                if !matches!(id.tok, Tok::Ident) {
                    return Err(ParseError::Generic("expected ident param".into()));
                }
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
                        param: id.text.to_string(),
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
                let inner = parse_expr_bp(i, toks, 0)?;
                let r = bump(i, toks).ok_or_else(|| ParseError::Generic("expected )".into()))?;
                if !matches!(r.tok, Tok::RParen) {
                    return Err(ParseError::Generic(") expected".into()));
                }
                inner
            }
            Tok::LBrace => {
                let inner = parse_expr_bp(i, toks, 0)?;
                let r = bump(i, toks).ok_or_else(|| ParseError::Generic("expected }".into()))?;
                if !matches!(r.tok, Tok::RBrace) {
                    return Err(ParseError::Generic("} expected".into()));
                }
                Expr::new(
                    ExprKind::Block(Box::new(inner.clone())),
                    Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset),
                )
            }
            Tok::Ident => Expr::new(ExprKind::Symbol(t.text.to_string()), t.span),
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
                if op_bp < bp { break; }
                // consume op
                let _ = bump(i, toks);
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
                let span = Span::new(lhs.span.offset, rhs.span.offset + rhs.span.len - lhs.span.offset);
                let app1 = Expr::new(ExprKind::Apply { func: Box::new(callee), arg: Box::new(lhs) }, span);
                lhs = Expr::new(ExprKind::Apply { func: Box::new(app1), arg: Box::new(rhs) }, span);
                continue;
            }
            match nxt.tok {
                Tok::LParen
                | Tok::Int(_)
                | Tok::Float(_)
                | Tok::Str(_)
                | Tok::Tilde
                | Tok::Backslash
                | Tok::Ident
                | Tok::Member(_)
                | Tok::Bang => {
                    // function application (left associative)
                    let arg = parse_atom(i, toks)?;
                    let span = Span::new(
                        lhs.span.offset,
                        arg.span.offset + arg.span.len - lhs.span.offset,
                    );
                    // sugar: true() / false()  =>  ~true / ~false
                    if let ExprKind::Symbol(name) = &lhs.kind {
                        if matches!(arg.kind, ExprKind::Unit)
                            && (name == "true" || name == "false")
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
