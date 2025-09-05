use lzscr_ast::ast::*;
use serde::{Deserialize, Serialize};

// Core IR: deliberately small for PoC. Lambda-calculus-like with explicit refs and sequence.

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Ty {
    Unit,
    Int,
    Float,
    Bool,
    Str,
    Fun(Box<Ty>, Box<Ty>),
    Dyn, // unknown/placeholder at PoC stage
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Op {
    // Builtins and ops as symbolic names for now
    Ref(String),      // reference to name (pre-resolve)
    Symbol(String),   // bare symbol
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Unit,
    Lam { param: String, body: Box<Term> },
    App { func: Box<Term>, arg: Box<Term> },
    Seq { first: Box<Term>, second: Box<Term> },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Term {
    pub op: Op,
}

impl Term {
    pub fn new(op: Op) -> Self { Self { op } }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Module {
    pub body: Term,
}

pub fn lower_expr_to_core(e: &Expr) -> Term {
    match &e.kind {
        ExprKind::Unit => Term::new(Op::Unit),
        ExprKind::Int(n) => Term::new(Op::Int(*n)),
    ExprKind::Float(f) => Term::new(Op::Float(*f)),
        ExprKind::Str(s) => Term::new(Op::Str(s.clone())),
        ExprKind::Ref(n) => Term::new(Op::Ref(n.clone())),
        ExprKind::Symbol(s) => Term::new(Op::Symbol(s.clone())),
        ExprKind::Raise(inner) => {
            // 暫定: 表示用に Symbol("^(") + inner + Symbol(")") にせず、App で (~raise inner) 相当の形にすることも考えたが
            // PoC 段階なので単純に Symbol("RAISE") と引数の App に落とす
            let tag = Term::new(Op::Symbol("RAISE".into()));
            Term::new(Op::App { func: Box::new(tag), arg: Box::new(lower_expr_to_core(inner)) })
        }
        ExprKind::OrElse { left, right } => {
            // Symbol("OR") left right を App 連鎖に
            let or = Term::new(Op::Symbol("OR".into()));
            let app_l = Term::new(Op::App { func: Box::new(or), arg: Box::new(lower_expr_to_core(left)) });
            Term::new(Op::App { func: Box::new(app_l), arg: Box::new(lower_expr_to_core(right)) })
        }
        ExprKind::Catch { left, right } => {
            let cat = Term::new(Op::Symbol("CATCH".into()));
            let app_l = Term::new(Op::App { func: Box::new(cat), arg: Box::new(lower_expr_to_core(left)) });
            Term::new(Op::App { func: Box::new(app_l), arg: Box::new(lower_expr_to_core(right)) })
        }
        ExprKind::Lambda { param, body } => {
            fn print_pattern(p: &Pattern) -> String {
                match &p.kind {
                    PatternKind::Wildcard => "_".into(),
                    PatternKind::Var(n) => format!("~{}", n),
                    PatternKind::Unit => "()".into(),
                    PatternKind::Tuple(xs) => format!("({})", xs.iter().map(print_pattern).collect::<Vec<_>>().join(", ")),
                    PatternKind::Ctor { name, args } => {
                        if args.is_empty() { name.clone() } else { format!("{} {}", name, args.iter().map(print_pattern).collect::<Vec<_>>().join(" ")) }
                    }
                    PatternKind::Symbol(s) => s.clone(),
                    PatternKind::Int(n) => format!("{}", n),
                    PatternKind::Float(f) => format!("{}", f),
                    PatternKind::Str(s) => format!("\"{}\"", s.escape_default()),
                    PatternKind::Bool(b) => format!("{}", b),
                    PatternKind::As(a, b) => format!("{} @ {}", print_pattern(a), print_pattern(b)),
                }
            }
            let param_str = print_pattern(param);
            Term::new(Op::Lam { param: param_str, body: Box::new(lower_expr_to_core(body)) })
        }
        ExprKind::Apply { func, arg } => {
            // desugar (~seq a b) into Seq
            if let ExprKind::Apply { func: seq_ref_expr, arg: a_expr } = &func.kind {
                if let ExprKind::Ref(seq_name) = &seq_ref_expr.kind {
                    if seq_name == "seq" {
                        return Term::new(Op::Seq { first: Box::new(lower_expr_to_core(a_expr)), second: Box::new(lower_expr_to_core(arg)) });
                    }
                }
            }
            Term::new(Op::App { func: Box::new(lower_expr_to_core(func)), arg: Box::new(lower_expr_to_core(arg)) })
        }
        ExprKind::Block(inner) => lower_expr_to_core(inner),
    }
}

pub fn print_term(t: &Term) -> String {
    match &t.op {
        Op::Unit => "()".into(),
        Op::Int(n) => format!("{n}"),
    Op::Float(f) => format!("{}", f),
    Op::Bool(b) => format!("{}", b),
        Op::Str(s) => format!("\"{}\"", s.escape_default()),
        Op::Ref(n) => format!("~{n}"),
        Op::Symbol(s) => s.clone(),
    Op::Lam { param, body } => format!("\\{} -> {}", param, print_term(body)),
        Op::App { func, arg } => format!("({} {})", print_term(func), print_term(arg)),
        Op::Seq { first, second } => format!("(~seq {} {})", print_term(first), print_term(second)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lzscr_ast::span::Span;

    fn int_expr(n: i64) -> Expr { Expr::new(ExprKind::Int(n), Span::new(0,0)) }
    fn ref_expr(n: &str) -> Expr { Expr::new(ExprKind::Ref(n.into()), Span::new(0,0)) }
    fn apply(f: Expr, a: Expr) -> Expr { Expr::new(ExprKind::Apply{ func: Box::new(f), arg: Box::new(a) }, Span::new(0,0)) }

    #[test]
    fn lower_seq_special_form() {
        // (~seq 1 (~add 2 3)) -> Seq(Int(1), App(Ref(add), 2) 3)
        let seq = ref_expr("seq");
        let one = int_expr(1);
        let add = ref_expr("add");
        let two = int_expr(2);
        let three = int_expr(3);
        let add2 = apply(add, two);
        let add23 = apply(add2, three);
        let seq1 = apply(seq, one);
        let expr = apply(seq1, add23);
        let t = lower_expr_to_core(&expr);
        match t.op {
            Op::Seq { .. } => {}
            _ => panic!("expected Seq"),
        }
    }
}
