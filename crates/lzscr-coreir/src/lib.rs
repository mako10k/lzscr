use lzscr_ast::ast::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// Core IR: deliberately small for PoC. Lambda-calculus-like with explicit refs and sequence.

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Ty {
    Unit,
    Int,
    Float,
    Bool,
    Str,
    Char,
    Fun(Box<Ty>, Box<Ty>),
    Dyn, // unknown/placeholder at PoC stage
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Op {
    // Builtins and ops as symbolic names for now
    Ref(String),    // reference to name (pre-resolve)
    Symbol(String), // bare symbol
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(i32),
    Unit,
    Lam { param: String, body: Box<Term> },
    App { func: Box<Term>, arg: Box<Term> },
    Seq { first: Box<Term>, second: Box<Term> },
    Chain { first: Box<Term>, second: Box<Term> },
    Bind { value: Box<Term>, cont: Box<Term> },
    // Recursive let-group to reflect lazy, mutually recursive semantics
    LetRec { bindings: Vec<(String, Term)>, body: Box<Term> },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Term {
    pub op: Op,
}

impl Term {
    pub fn new(op: Op) -> Self {
        Self { op }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Module {
    pub body: Term,
}

pub fn lower_expr_to_core(e: &Expr) -> Term {
    fn print_type_expr(t: &TypeExpr) -> String {
        match t {
            TypeExpr::Unit => "Unit".into(),
            TypeExpr::Int => "Int".into(),
            TypeExpr::Float => "Float".into(),
            TypeExpr::Bool => "Bool".into(),
            TypeExpr::Str => "Str".into(),
            TypeExpr::Char => "Char".into(),
            TypeExpr::Var(a) => format!("%{}", a),
            TypeExpr::Hole(opt) => {
                if let Some(a) = opt {
                    format!("?{}", a)
                } else {
                    "?".into()
                }
            }
            TypeExpr::List(x) => format!("[{}]", print_type_expr(x)),
            TypeExpr::Tuple(xs) => {
                let inner = xs.iter().map(print_type_expr).collect::<Vec<_>>().join(", ");
                format!("({})", inner)
            }
            TypeExpr::Record(fields) => {
                let inner = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_type_expr(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            TypeExpr::Fun(a, b) => format!("{} -> {}", print_type_expr(a), print_type_expr(b)),
            TypeExpr::Ctor { tag, args } => {
                if args.is_empty() {
                    tag.clone()
                } else {
                    format!(
                        "{} {}",
                        tag,
                        args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                    )
                }
            }
        }
    }
    fn print_pattern(p: &Pattern) -> String {
        match &p.kind {
            PatternKind::Wildcard => "_".into(),
            PatternKind::Var(n) => format!("~{}", n),
            PatternKind::Unit => "()".into(),
            PatternKind::Tuple(xs) => {
                format!("({})", xs.iter().map(print_pattern).collect::<Vec<_>>().join(", "))
            }
            PatternKind::List(xs) => {
                format!("[{}]", xs.iter().map(print_pattern).collect::<Vec<_>>().join(", "))
            }
            PatternKind::TypeBind { pat, .. } => print_pattern(pat),
            PatternKind::Ctor { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    format!(
                        "{} {}",
                        name,
                        args.iter().map(print_pattern).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            PatternKind::Cons(h, t) => format!("{} : {}", print_pattern(h), print_pattern(t)),
            PatternKind::Symbol(s) => s.clone(),
            PatternKind::Int(n) => format!("{}", n),
            PatternKind::Float(f) => format!("{}", f),
            PatternKind::Str(s) => format!("\"{}\"", s.escape_default()),
            PatternKind::Char(c) => {
                let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
                let mut tmp = String::new();
                tmp.push(ch);
                format!("'{}'", tmp.escape_default())
            }
            PatternKind::Bool(b) => format!("{}", b),
            PatternKind::Record(fields) => {
                let inner = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_pattern(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            PatternKind::As(a, b) => format!("{} @ {}", print_pattern(a), print_pattern(b)),
        }
    }
    match &e.kind {
        ExprKind::Annot { ty: _, expr } => lower_expr_to_core(expr),
        ExprKind::TypeVal(ty) => Term::new(Op::Str(print_type_expr(ty))),
        ExprKind::Unit => Term::new(Op::Unit),
        ExprKind::Int(n) => Term::new(Op::Int(*n)),
        ExprKind::Float(f) => Term::new(Op::Float(*f)),
        ExprKind::Str(s) => Term::new(Op::Str(s.clone())),
        ExprKind::Char(c) => Term::new(Op::Char(*c)),
        ExprKind::Ref(n) => Term::new(Op::Ref(n.clone())),
        ExprKind::Symbol(s) => Term::new(Op::Symbol(s.clone())),
        ExprKind::LetGroup { bindings, body, .. } => {
            let bs: Vec<(String, Term)> =
                bindings.iter().map(|(p, ex)| (print_pattern(p), lower_expr_to_core(ex))).collect();
            Term::new(Op::LetRec { bindings: bs, body: Box::new(lower_expr_to_core(body)) })
        }
        ExprKind::List(xs) => {
            // Lower to foldr cons []
            let mut tail = Term::new(Op::Symbol("[]".into()));
            for x in xs.iter().rev() {
                let cons = Term::new(Op::Ref("cons".into()));
                let app1 = Term::new(Op::App {
                    func: Box::new(cons),
                    arg: Box::new(lower_expr_to_core(x)),
                });
                tail = Term::new(Op::App { func: Box::new(app1), arg: Box::new(tail) });
            }
            tail
        }
        ExprKind::Raise(inner) => {
            // 暫定: 表示用に Symbol("^(") + inner + Symbol(")") にせず、App で (~raise inner) 相当の形にすることも考えたが
            // PoC 段階なので単純に Symbol("RAISE") と引数の App に落とす
            let tag = Term::new(Op::Symbol("RAISE".into()));
            Term::new(Op::App { func: Box::new(tag), arg: Box::new(lower_expr_to_core(inner)) })
        }
        ExprKind::AltLambda { left, right } => {
            // Lower to \x -> ((~alt left) right) x
            let x = "x".to_string();
            let alt = Term::new(Op::Ref("alt".into()));
            let app1 =
                Term::new(Op::App { func: Box::new(alt), arg: Box::new(lower_expr_to_core(left)) });
            let app2 = Term::new(Op::App {
                func: Box::new(app1),
                arg: Box::new(lower_expr_to_core(right)),
            });
            let body = Term::new(Op::App {
                func: Box::new(app2),
                arg: Box::new(Term::new(Op::Ref(x.clone()))),
            });
            Term::new(Op::Lam { param: format!("~{}", x), body: Box::new(body) })
        }
        ExprKind::OrElse { left, right } => {
            // Symbol("OR") left right を App 連鎖に
            let or = Term::new(Op::Symbol("OR".into()));
            let app_l =
                Term::new(Op::App { func: Box::new(or), arg: Box::new(lower_expr_to_core(left)) });
            Term::new(Op::App { func: Box::new(app_l), arg: Box::new(lower_expr_to_core(right)) })
        }
        ExprKind::Catch { left, right } => {
            let cat = Term::new(Op::Symbol("CATCH".into()));
            let app_l =
                Term::new(Op::App { func: Box::new(cat), arg: Box::new(lower_expr_to_core(left)) });
            Term::new(Op::App { func: Box::new(app_l), arg: Box::new(lower_expr_to_core(right)) })
        }
        ExprKind::Lambda { param, body } => {
            let param_str = print_pattern(param);
            Term::new(Op::Lam { param: param_str, body: Box::new(lower_expr_to_core(body)) })
        }
        ExprKind::Apply { func, arg } => {
            // desugar (~seq a b) into Seq
            if let ExprKind::Apply { func: seq_ref_expr, arg: a_expr } = &func.kind {
                if let ExprKind::Ref(seq_name) = &seq_ref_expr.kind {
                    if seq_name == "seq" {
                        return Term::new(Op::Seq {
                            first: Box::new(lower_expr_to_core(a_expr)),
                            second: Box::new(lower_expr_to_core(arg)),
                        });
                    }
                }
            }
            // desugar (~chain a b) into Chain
            if let ExprKind::Apply { func: chain_ref_expr, arg: a_expr } = &func.kind {
                if let ExprKind::Ref(chain_name) = &chain_ref_expr.kind {
                    if chain_name == "chain" {
                        return Term::new(Op::Chain {
                            first: Box::new(lower_expr_to_core(a_expr)),
                            second: Box::new(lower_expr_to_core(arg)),
                        });
                    }
                }
            }
            // desugar (~bind e k) into Bind
            if let ExprKind::Apply { func: bind_ref_expr, arg: e_expr } = &func.kind {
                if let ExprKind::Ref(bind_name) = &bind_ref_expr.kind {
                    if bind_name == "bind" {
                        return Term::new(Op::Bind {
                            value: Box::new(lower_expr_to_core(e_expr)),
                            cont: Box::new(lower_expr_to_core(arg)),
                        });
                    }
                }
            }
            Term::new(Op::App {
                func: Box::new(lower_expr_to_core(func)),
                arg: Box::new(lower_expr_to_core(arg)),
            })
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
        Op::Char(c) => {
            let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
            let mut tmp = String::new();
            tmp.push(ch);
            format!("'{}'", tmp.escape_default())
        }
        Op::Ref(n) => format!("~{n}"),
        Op::Symbol(s) => s.clone(),
        Op::Lam { param, body } => format!("\\{} -> {}", param, print_term(body)),
        Op::App { func, arg } => format!("({} {})", print_term(func), print_term(arg)),
        Op::Seq { first, second } => format!("(~seq {} {})", print_term(first), print_term(second)),
        Op::Chain { first, second } => {
            format!("(~chain {} {})", print_term(first), print_term(second))
        }
        Op::Bind { value, cont } => format!("(~bind {} {})", print_term(value), print_term(cont)),
        Op::LetRec { bindings, body } => {
            let inner = bindings
                .iter()
                .map(|(p, e)| format!("{} = {}", p, print_term(e)))
                .collect::<Vec<_>>()
                .join("; ");
            format!("(letrec {{ {}; }} {})", inner, print_term(body))
        }
    }
}

// ---- Minimal IR evaluator (PoC) ----

#[derive(Debug, Clone, PartialEq)]
pub enum IrValue {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(i32),
    Fun { param: String, body: Term, env: HashMap<String, IrValue> },
    Builtin { name: String, args: Vec<IrValue> },
}

#[derive(thiserror::Error, Debug)]
pub enum IrEvalError {
    #[error("unbound ref: {0}")]
    Unbound(String),
    #[error("not a function: {0:?}")]
    NotFunction(IrValue),
    #[error("unsupported pattern in lambda parameter: {0}")]
    UnsupportedParam(String),
    #[error("arity error for builtin: {0}")]
    Arity(String),
}

fn is_simple_var_param(p: &str) -> Option<String> {
    // Accept param like "~x" only
    if let Some(rest) = p.strip_prefix('~') {
        if !rest.is_empty() && rest.chars().all(|c| c == '_' || c.is_alphanumeric()) {
            return Some(rest.to_string());
        }
    }
    None
}

fn eval_builtin(name: &str, args: &[IrValue]) -> Result<IrValue, IrEvalError> {
    match (name, args) {
        ("add", [IrValue::Int(a), IrValue::Int(b)]) => Ok(IrValue::Int(a + b)),
        ("sub", [IrValue::Int(a), IrValue::Int(b)]) => Ok(IrValue::Int(a - b)),
        ("mul", [IrValue::Int(a), IrValue::Int(b)]) => Ok(IrValue::Int(a * b)),
        ("div", [IrValue::Int(_), IrValue::Int(0)]) => {
            Err(IrEvalError::Arity("div by zero".into()))
        }
        ("div", [IrValue::Int(a), IrValue::Int(b)]) => Ok(IrValue::Int(a / b)),
        ("to_str", [v]) => Ok(IrValue::Str(match v {
            IrValue::Unit => "()".into(),
            IrValue::Int(n) => n.to_string(),
            IrValue::Float(f) => f.to_string(),
            IrValue::Bool(b) => b.to_string(),
            IrValue::Str(s) => s.clone(),
            IrValue::Char(c) => {
                let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
                let mut tmp = String::new();
                tmp.push(ch);
                format!("'{}'", tmp.escape_default())
            }
            IrValue::Fun { .. } => "<fun>".into(),
            IrValue::Builtin { .. } => "<fun>".into(),
        })),
        _ => Err(IrEvalError::Arity(name.into())),
    }
}

fn builtin_arity(name: &str) -> Option<usize> {
    match name {
        "add" | "sub" | "mul" | "div" => Some(2),
        "to_str" => Some(1),
        _ => None,
    }
}

fn eval_app(func: IrValue, arg: IrValue) -> Result<IrValue, IrEvalError> {
    match func {
        IrValue::Fun { param, body, mut env } => {
            env.insert(param, arg);
            eval_term_with_env(&body, &mut env)
        }
        IrValue::Builtin { name, mut args } => {
            args.push(arg);
            if let Some(k) = builtin_arity(&name) {
                if args.len() < k {
                    Ok(IrValue::Builtin { name, args })
                } else if args.len() == k {
                    eval_builtin(&name, &args)
                } else {
                    // support over-application: compute then keep applying remaining args
                    let (first, rest) = args.split_at(k);
                    let mut res = eval_builtin(&name, first)?;
                    for a in rest.iter().cloned() {
                        res = eval_app(res, a)?;
                    }
                    Ok(res)
                }
            } else {
                Err(IrEvalError::Unbound(name))
            }
        }
        other => Err(IrEvalError::NotFunction(other)),
    }
}

fn eval_term_with_env(
    t: &Term,
    env: &mut HashMap<String, IrValue>,
) -> Result<IrValue, IrEvalError> {
    match &t.op {
        Op::Unit => Ok(IrValue::Unit),
        Op::Int(n) => Ok(IrValue::Int(*n)),
        Op::Float(f) => Ok(IrValue::Float(*f)),
        Op::Bool(b) => Ok(IrValue::Bool(*b)),
        Op::Str(s) => Ok(IrValue::Str(s.clone())),
        Op::Char(c) => Ok(IrValue::Char(*c)),
        Op::Ref(n) => {
            if let Some(v) = env.get(n).cloned() {
                return Ok(v);
            }
            // Builtins
            if builtin_arity(n).is_some() {
                return Ok(IrValue::Builtin { name: n.clone(), args: vec![] });
            }
            Err(IrEvalError::Unbound(n.clone()))
        }
        Op::Symbol(s) => Ok(IrValue::Str(s.clone())), // minimal placeholder
        Op::Lam { param, body } => {
            let Some(vname) = is_simple_var_param(param) else {
                return Err(IrEvalError::UnsupportedParam(param.clone()));
            };
            Ok(IrValue::Fun { param: vname, body: (*body.clone()), env: env.clone() })
        }
        Op::App { func, arg } => {
            let f = eval_term_with_env(func, env)?;
            let a = eval_term_with_env(arg, env)?;
            eval_app(f, a)
        }
        Op::Seq { first, second } | Op::Chain { first, second } => {
            let _ = eval_term_with_env(first, env)?;
            eval_term_with_env(second, env)
        }
        Op::Bind { value, cont } => {
            let v = eval_term_with_env(value, env)?;
            let k = eval_term_with_env(cont, env)?;
            eval_app(k, v)
        }
        Op::LetRec { .. } => {
            // Not yet supported in PoC evaluator
            Err(IrEvalError::UnsupportedParam("letrec".into()))
        }
    }
}

pub fn eval_term(t: &Term) -> Result<IrValue, IrEvalError> {
    let mut env = HashMap::new();
    eval_term_with_env(t, &mut env)
}

pub fn print_ir_value(v: &IrValue) -> String {
    match v {
        IrValue::Unit => "()".into(),
        IrValue::Int(n) => n.to_string(),
        IrValue::Float(f) => f.to_string(),
        IrValue::Bool(b) => b.to_string(),
        IrValue::Str(s) => s.clone(),
        IrValue::Char(c) => {
            let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
            let mut tmp = String::new();
            tmp.push(ch);
            format!("'{}'", tmp.escape_default())
        }
        IrValue::Fun { .. } | IrValue::Builtin { .. } => "<fun>".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lzscr_ast::span::Span;

    fn int_expr(n: i64) -> Expr {
        Expr::new(ExprKind::Int(n), Span::new(0, 0))
    }
    fn ref_expr(n: &str) -> Expr {
        Expr::new(ExprKind::Ref(n.into()), Span::new(0, 0))
    }
    fn apply(f: Expr, a: Expr) -> Expr {
        Expr::new(ExprKind::Apply { func: Box::new(f), arg: Box::new(a) }, Span::new(0, 0))
    }

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

    #[test]
    fn lower_alt_lambda_to_alt_app_chain() {
        use lzscr_ast::span::Span;
        // (\~x -> ~x) | (\~y -> ~y)  ==>  \~x0 -> ((~alt (\~x -> ~x)) (\~y -> ~y)) ~x0
        let lam_l = Expr::new(
            ExprKind::Lambda {
                param: Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let lam_r = Expr::new(
            ExprKind::Lambda {
                param: Pattern::new(PatternKind::Var("y".into()), Span::new(0, 0)),
                body: Box::new(Expr::new(ExprKind::Ref("y".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let alt = Expr::new(
            ExprKind::AltLambda { left: Box::new(lam_l), right: Box::new(lam_r) },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&alt);
        match t.op {
            Op::Lam { param, body } => {
                assert!(param.starts_with("~"));
                // body must be App(App(App(Ref("alt"), left), right), Ref(param))
                if let Op::App { func, arg } = &body.op {
                    // last arg is the parameter ref
                    match &arg.op {
                        Op::Ref(_pn) => {}
                        other => panic!("expected Ref param, got {:?}", other),
                    }
                    // unroll the func side: ((Ref alt) left) right
                    if let Op::App { func: f2, arg: _right } = &func.op {
                        if let Op::App { func: f1, arg: _left } = &f2.op {
                            match &f1.op {
                                Op::Ref(name) => assert_eq!(name, "alt"),
                                other => panic!("expected Ref(alt), got {:?}", other),
                            }
                        } else {
                            panic!("expected App in f2.func");
                        }
                    } else {
                        panic!("expected App in body.func");
                    }
                } else {
                    panic!("expected App at body root");
                }
            }
            other => panic!("expected Lam at top, got {:?}", other),
        }
    }

    #[test]
    fn lower_let_group_seq() {
        // ( ~x = 1; ~x; )
        let e = Expr::new(
            ExprKind::LetGroup {
                type_decls: vec![],
                bindings: vec![(
                    Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                    Expr::new(ExprKind::Int(1), Span::new(0, 0)),
                )],
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&e);
        let s = print_term(&t);
        assert!(s.contains("letrec"));
    }

    #[test]
    fn lower_let_group_with_typedecls_ignored_in_coreir() {
        use lzscr_ast::span::Span;
        // LetGroup に % 型宣言が含まれていても、Core IR へのロワリングでは無視される（LetRec のみ生成）
        let opt_td = TypeDecl {
            name: "Option".into(),
            params: vec!["a".into()],
            body: TypeDefBody::Sum(vec![
                (".None".into(), vec![]),
                (".Some".into(), vec![TypeExpr::Var("a".into())]),
            ]),
            span: Span::new(0, 0),
        };

        let e = Expr::new(
            ExprKind::LetGroup {
                type_decls: vec![opt_td],
                bindings: vec![(
                    Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                    Expr::new(ExprKind::Int(1), Span::new(0, 0)),
                )],
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&e);
        let s = print_term(&t);
        assert!(s.contains("letrec"));
    }

    #[test]
    fn lower_chain_and_bind() {
        use lzscr_ast::span::Span;
        // (~chain 1 (~bind 2 (\~x -> ~x)))
        let chain_ref = Expr::new(ExprKind::Ref("chain".into()), Span::new(0, 0));
        let bind_ref = Expr::new(ExprKind::Ref("bind".into()), Span::new(0, 0));
        let one = Expr::new(ExprKind::Int(1), Span::new(0, 0));
        let two = Expr::new(ExprKind::Int(2), Span::new(0, 0));
        let lam = Expr::new(
            ExprKind::Lambda {
                param: Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let bind_2 = Expr::new(
            ExprKind::Apply {
                func: Box::new(Expr::new(
                    ExprKind::Apply { func: Box::new(bind_ref), arg: Box::new(two) },
                    Span::new(0, 0),
                )),
                arg: Box::new(lam),
            },
            Span::new(0, 0),
        );
        let chain_expr = Expr::new(
            ExprKind::Apply {
                func: Box::new(Expr::new(
                    ExprKind::Apply { func: Box::new(chain_ref), arg: Box::new(one) },
                    Span::new(0, 0),
                )),
                arg: Box::new(bind_2),
            },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&chain_expr);
        match &t.op {
            Op::Chain { first, second } => {
                match &first.op {
                    Op::Int(1) => {}
                    other => panic!("expected Int(1), got {:?}", other),
                }
                match &second.op {
                    Op::Bind { value, cont: _ } => match &value.op {
                        Op::Int(2) => {}
                        other => panic!("expected Int(2), got {:?}", other),
                    },
                    other => panic!("expected Bind, got {:?}", other),
                }
            }
            other => panic!("expected Chain root, got {:?}", other),
        }
        let s = print_term(&t);
        assert!(s.contains("~chain"));
        assert!(s.contains("~bind"));
    }

    #[test]
    fn eval_ir_add_and_bind() {
        use lzscr_ast::span::Span;
        // (~add 1 2) => 3
        let add = Expr::new(ExprKind::Ref("add".into()), Span::new(0, 0));
        let one = Expr::new(ExprKind::Int(1), Span::new(0, 0));
        let two = Expr::new(ExprKind::Int(2), Span::new(0, 0));
        let add1 =
            Expr::new(ExprKind::Apply { func: Box::new(add), arg: Box::new(one) }, Span::new(0, 0));
        let add12 = Expr::new(
            ExprKind::Apply { func: Box::new(add1), arg: Box::new(two) },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&add12);
        let v = eval_term(&t).expect("ir eval add");
        assert_eq!(print_ir_value(&v), "3");

        // (~bind 2 (\~x -> ~x)) => 2
        let bind = Expr::new(ExprKind::Ref("bind".into()), Span::new(0, 0));
        let two = Expr::new(ExprKind::Int(2), Span::new(0, 0));
        let id = Expr::new(
            ExprKind::Lambda {
                param: Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let b2 = Expr::new(
            ExprKind::Apply {
                func: Box::new(Expr::new(
                    ExprKind::Apply { func: Box::new(bind), arg: Box::new(two) },
                    Span::new(0, 0),
                )),
                arg: Box::new(id),
            },
            Span::new(0, 0),
        );
        let t2 = lower_expr_to_core(&b2);
        let v2 = eval_term(&t2).expect("ir eval bind");
        assert_eq!(print_ir_value(&v2), "2");
    }
}
