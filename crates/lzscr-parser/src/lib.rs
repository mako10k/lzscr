use lzscr_ast::{ast::*, span::Span};
use lzscr_lexer::{Tok, lex};

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("parse error: {0}")]
    Generic(String),
}

pub fn parse_expr(src: &str) -> Result<Expr, ParseError> {
    // Minimal, direct token-based parser for a subset: ints, strings, refs, idents, lambda, apply, block
    let tokens = lex(src);
    let mut i = 0usize;

    fn peek<'a>(i: usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Option<&'a lzscr_lexer::Lexed<'a>> { toks.get(i) }
    fn bump<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Option<&'a lzscr_lexer::Lexed<'a>> { let t = toks.get(*i); *i += 1; t }

    fn parse_atom<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>]) -> Result<Expr, ParseError> {
        let t = bump(i, toks).ok_or_else(|| ParseError::Generic("unexpected EOF".into()))?;
        Ok(match &t.tok {
            Tok::Int(n) => Expr::new(ExprKind::Int(*n), t.span),
            Tok::Str(s) => Expr::new(ExprKind::Str(s.clone()), t.span),
            Tok::Tilde => {
                let id = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ident after ~".into()))?;
                match id.tok { Tok::Ident => Expr::new(ExprKind::Ref(id.text.to_string()), Span::new(t.span.offset, id.span.offset + id.span.len - t.span.offset)), _ => return Err(ParseError::Generic("expected ident after ~".into())) }
            }
            Tok::Backslash => {
                let id = bump(i, toks).ok_or_else(|| ParseError::Generic("expected param".into()))?;
                if !matches!(id.tok, Tok::Ident) { return Err(ParseError::Generic("expected ident param".into())); }
                let arr = bump(i, toks).ok_or_else(|| ParseError::Generic("expected ->".into()))?;
                if !matches!(arr.tok, Tok::Arrow) { return Err(ParseError::Generic("expected ->".into())); }
                let body = parse_expr_bp(i, toks, 0)?;
                let span = Span::new(t.span.offset, body.span.offset + body.span.len - t.span.offset);
                Expr::new(ExprKind::Lambda{ param: id.text.to_string(), body: Box::new(body) }, span)
            }
            Tok::LParen => {
                // handle unit ()
                if let Some(nxt) = peek(*i, toks) { if matches!(nxt.tok, Tok::RParen) {
                    let r = bump(i, toks).unwrap();
                    return Ok(Expr::new(ExprKind::Unit, Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset)));
                } }
                let inner = parse_expr_bp(i, toks, 0)?;
                let r = bump(i, toks).ok_or_else(|| ParseError::Generic("expected )".into()))?;
                if !matches!(r.tok, Tok::RParen) { return Err(ParseError::Generic(") expected".into())); }
                inner
            }
            Tok::LBrace => {
                let inner = parse_expr_bp(i, toks, 0)?;
                let r = bump(i, toks).ok_or_else(|| ParseError::Generic("expected }".into()))?;
                if !matches!(r.tok, Tok::RBrace) { return Err(ParseError::Generic("} expected".into())); }
                Expr::new(ExprKind::Block(Box::new(inner.clone())), Span::new(t.span.offset, r.span.offset + r.span.len - t.span.offset))
            }
            Tok::Ident => Expr::new(ExprKind::Symbol(t.text.to_string()), t.span),
            _ => return Err(ParseError::Generic(format!("unexpected token: {:?}", t.tok)))
        })
    }

    fn parse_expr_bp<'a>(i: &mut usize, toks: &'a [lzscr_lexer::Lexed<'a>], _bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = parse_atom(i, toks)?;
        loop {
            let Some(nxt) = peek(*i, toks) else { break };
            match nxt.tok {
                Tok::LParen | Tok::Int(_) | Tok::Str(_) | Tok::Tilde | Tok::Backslash | Tok::Ident => {
                    // function application (left associative)
                    let arg = parse_atom(i, toks)?;
                    let span = Span::new(lhs.span.offset, arg.span.offset + arg.span.len - lhs.span.offset);
                    lhs = Expr::new(ExprKind::Apply{ func: Box::new(lhs), arg: Box::new(arg) }, span);
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    let expr = parse_expr_bp(&mut i, &tokens, 0)?;
    Ok(expr)
}
