//! Core IR lowering and evaluation (PoC).
//!
//! - Lowering: stable for basic constructs; exceptions/alt/catch are evolving
//! - Evaluation: small-step PoC; not feature-complete
//!
//! Roadmap: firm up exception/alt/catch representation, improve evaluator coverage.
//! Source of truth: docs/ROADMAP.md

use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

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
    /// Legacy (older dumps): ambiguous symbol/constructor.
    ///
    /// New lowering prefers `Ctor` and `AtomSymbol`.
    Symbol(String),
    /// Constructor value (curried via application).
    Ctor(String),
    /// Atomic symbol value (used for ModeMap labels, `.select`, etc).
    AtomSymbol(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(i32),
    Unit,
    List { items: Vec<Term> },
    Record { fields: Vec<RecordFieldTerm> },
    ModeMap { fields: Vec<RecordFieldTerm> },
    /// ModeMap selection: `.select .M e`.
    ///
    /// `label` is the raw symbol string such as `.Int`.
    Select { label: String, label_span: Span, target: Box<Term> },
    Lam { param: Pattern, body: Box<Term> },
    App { func: Box<Term>, arg: Box<Term> },
    Seq { first: Box<Term>, second: Box<Term> },
    Chain { first: Box<Term>, second: Box<Term> },
    Bind { value: Box<Term>, cont: Box<Term> },
    /// Raise an exception with payload.
    ///
    /// Mirrors AST `^(e)`.
    Raise { payload: Box<Term> },
    /// Catch an exception: evaluate `left`; if it is raised, apply `right` to payload.
    ///
    /// Mirrors AST `(left ^| right)`.
    Catch { left: Box<Term>, right: Box<Term> },
    /// OrElse (discard LHS result): evaluate `left` (even if raised), then return `right`.
    ///
    /// Mirrors AST `(left || right)`.
    OrElse { left: Box<Term>, right: Box<Term> },
    /// Alternative lambdas (pattern-branching lambdas) chain.
    ///
    /// Mirrors AST `(left | right)`.
    ///
    /// Note: CoreIR evaluator does not implement pattern matching yet, so evaluation
    /// of this op is currently unsupported.
    Alt { left: Box<Term>, right: Box<Term> },
    // Recursive let-group to reflect lazy, mutually recursive semantics
    LetRec { bindings: Vec<(String, Term)>, body: Box<Term> },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RecordFieldTerm {
    pub name: String,
    pub name_span: Span,
    pub value: Term,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Term {
    pub op: Op,
}

impl Term {
    pub fn new(op: Op) -> Self {
        Self { op }
    }

    /// Convenience constructor for integer literal
    pub fn int(n: i64) -> Self {
        Self::new(Op::Int(n))
    }

    /// Convenience constructor for float literal
    pub fn float(f: f64) -> Self {
        Self::new(Op::Float(f))
    }

    /// Convenience constructor for string literal
    pub fn string<S: Into<String>>(s: S) -> Self {
        Self::new(Op::Str(s.into()))
    }

    /// Convenience constructor for variable reference
    pub fn var<S: Into<String>>(name: S) -> Self {
        Self::new(Op::Ref(name.into()))
    }

    /// Convenience constructor for lambda
    pub fn lambda(param: Pattern, body: Term) -> Self {
        Self::new(Op::Lam { param, body: Box::new(body) })
    }

    /// Convenience constructor for a simple variable lambda: `\~x -> body`.
    pub fn lambda_var<S: Into<String>>(name: S, body: Term) -> Self {
        Self::new(Op::Lam {
            param: Pattern::new(PatternKind::Var(name.into()), lzscr_ast::span::Span::new(0, 0)),
            body: Box::new(body),
        })
    }

    /// Convenience constructor for function application
    pub fn app(func: Term, arg: Term) -> Self {
        Self::new(Op::App { func: Box::new(func), arg: Box::new(arg) })
    }

    /// Convenience constructor for letrec
    pub fn letrec(bindings: Vec<(String, Term)>, body: Term) -> Self {
        Self::new(Op::LetRec { bindings, body: Box::new(body) })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Module {
    pub body: Term,
}

pub fn lower_expr_to_core(e: &Expr) -> Term {
    fn is_symbol(e: &Expr, sym: &str) -> bool {
        matches!(&e.kind, ExprKind::Symbol(s) if s == sym)
    }

    fn symbol_value(e: &Expr) -> Option<(&str, Span)> {
        match &e.kind {
            ExprKind::Symbol(s) => Some((s.as_str(), e.span)),
            _ => None,
        }
    }

    fn is_tuple_tag_symbol(s: &str) -> bool {
        // Matches runtime tuple tag encoding like `.,,` (dot then commas).
        let mut chars = s.chars();
        if chars.next() != Some('.') {
            return false;
        }
        let rest: Vec<char> = chars.collect();
        !rest.is_empty() && rest.iter().all(|c| *c == ',')
    }

    fn print_type_expr(t: &TypeExpr) -> String {
        fn dotted(name: &str) -> String {
            let mut s = String::from(".");
            s.push_str(name);
            s
        }
        match t {
            TypeExpr::Unit => dotted("Unit"),
            TypeExpr::Int => dotted("Int"),
            TypeExpr::Float => dotted("Float"),
            TypeExpr::Bool => dotted("Bool"),
            TypeExpr::Str => dotted("Str"),
            TypeExpr::Char => dotted("Char"),
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
                    .map(|f| format!("{}: {}", f.name, print_type_expr(&f.type_expr)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            TypeExpr::Fun(a, b) => format!("{} -> {}", print_type_expr(a), print_type_expr(b)),
            TypeExpr::Ctor { tag, args } => {
                let head = match tag {
                    lzscr_ast::ast::Tag::Name(s) => dotted(s),
                };
                if args.is_empty() {
                    head
                } else {
                    format!(
                        "{} {}",
                        head,
                        args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            TypeExpr::Sum(alts) => {
                let parts = alts
                    .iter()
                    .map(|(tag, args)| {
                        let head = match tag {
                            lzscr_ast::ast::Tag::Name(s) => dotted(s),
                        };
                        if args.is_empty() {
                            head
                        } else {
                            format!(
                                "{} {}",
                                head,
                                args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("%{{{}}}", parts)
            }
        }
    }
    // Pattern printing is shared between lowering and pretty-print.
    match &e.kind {
        ExprKind::Annot { ty: _, expr } => lower_expr_to_core(expr),
        ExprKind::TypeVal(ty) => Term::new(Op::Str(print_type_expr(ty))),
        ExprKind::Unit => Term::new(Op::Unit),
        ExprKind::Int(n) => Term::new(Op::Int(*n)),
        ExprKind::Float(f) => Term::new(Op::Float(*f)),
        ExprKind::Str(s) => Term::new(Op::Str(s.clone())),
        ExprKind::Char(c) => Term::new(Op::Char(*c)),
        ExprKind::Ref(n) => Term::new(Op::Ref(n.clone())),
        ExprKind::Symbol(s) => {
            // Disambiguate at lowering time:
            // - tuple tags like `.,,` are constructors
            // - dot-led names like `.Int` are atomic symbols
            // - everything else is a constructor name (e.g. `Some`)
            if is_tuple_tag_symbol(s) {
                Term::new(Op::Ctor(s.clone()))
            } else if s.starts_with('.') {
                Term::new(Op::AtomSymbol(s.clone()))
            } else {
                Term::new(Op::Ctor(s.clone()))
            }
        }
        ExprKind::Record(fields) => {
            Term::new(Op::Record {
                fields: fields
                    .iter()
                    .map(|f| RecordFieldTerm {
                        name: f.name.clone(),
                        name_span: f.name_span,
                        value: lower_expr_to_core(&f.value),
                    })
                    .collect(),
            })
        }
        ExprKind::ModeMap(fields) => {
            Term::new(Op::ModeMap {
                fields: fields
                    .iter()
                    .map(|f| RecordFieldTerm {
                        name: f.name.clone(),
                        name_span: f.name_span,
                        value: lower_expr_to_core(&f.value),
                    })
                    .collect(),
            })
        }
        ExprKind::LetGroup { bindings, body, .. } => {
            let bs: Vec<(String, Term)> =
                bindings.iter().map(|(p, ex)| (print_pattern(p), lower_expr_to_core(ex))).collect();
            Term::new(Op::LetRec { bindings: bs, body: Box::new(lower_expr_to_core(body)) })
        }
        ExprKind::List(xs) => {
            Term::new(Op::List { items: xs.iter().map(lower_expr_to_core).collect() })
        }
        ExprKind::Raise(inner) => {
            Term::new(Op::Raise { payload: Box::new(lower_expr_to_core(inner)) })
        }
        ExprKind::AltLambda { left, right } => {
            Term::new(Op::Alt {
                left: Box::new(lower_expr_to_core(left)),
                right: Box::new(lower_expr_to_core(right)),
            })
        }
        ExprKind::OrElse { left, right } => {
            Term::new(Op::OrElse {
                left: Box::new(lower_expr_to_core(left)),
                right: Box::new(lower_expr_to_core(right)),
            })
        }
        ExprKind::Catch { left, right } => {
            Term::new(Op::Catch {
                left: Box::new(lower_expr_to_core(left)),
                right: Box::new(lower_expr_to_core(right)),
            })
        }
        ExprKind::Lambda { param, body } => {
            Term::new(Op::Lam { param: param.clone(), body: Box::new(lower_expr_to_core(body)) })
        }
        ExprKind::Apply { func, arg } => {
            // Desugar selection: `(.select .M e)`.
            // Note: Sugar `.M e` is handled in later phases; CoreIR only recognizes the explicit `.select` form.
            if let ExprKind::Apply { func: f2, arg: a2 } = &func.kind {
                if is_symbol(f2, ".select") {
                    if let Some((label, label_span)) = symbol_value(a2) {
                        return Term::new(Op::Select {
                            label: label.to_string(),
                            label_span,
                            target: Box::new(lower_expr_to_core(arg)),
                        });
                    }
                }
            }
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
                format!("{} {}", name, args.iter().map(print_pattern).collect::<Vec<_>>().join(" "))
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
        PatternKind::Record(fields) => {
            let inner = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, print_pattern(&f.pattern)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", inner)
        }
        PatternKind::As(a, b) => format!("{} @ {}", print_pattern(a), print_pattern(b)),
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
        Op::Symbol(s) | Op::Ctor(s) | Op::AtomSymbol(s) => s.clone(),
        Op::List { items } => {
            format!("[{}]", items.iter().map(print_term).collect::<Vec<_>>().join(", "))
        }
        Op::Record { fields } => {
            let inner = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, print_term(&f.value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", inner)
        }
        Op::ModeMap { fields } => {
            let inner = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, print_term(&f.value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!(".{{{}}}", inner)
        }
        Op::Select { label, target, .. } => format!("(.select {} {})", label, print_term(target)),
        Op::Lam { param, body } => format!("\\{} -> {}", print_pattern(param), print_term(body)),
        Op::App { func, arg } => format!("({} {})", print_term(func), print_term(arg)),
        Op::Seq { first, second } => format!("(~seq {} {})", print_term(first), print_term(second)),
        Op::Chain { first, second } => {
            format!("(~chain {} {})", print_term(first), print_term(second))
        }
        Op::Bind { value, cont } => format!("(~bind {} {})", print_term(value), print_term(cont)),
        Op::Raise { payload } => format!("^({})", print_term(payload)),
        Op::Catch { left, right } => format!("({} ^| {})", print_term(left), print_term(right)),
        Op::OrElse { left, right } => format!("({} || {})", print_term(left), print_term(right)),
        Op::Alt { left, right } => format!("({} | {})", print_term(left), print_term(right)),
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
    Str(String),
    Char(i32),
    /// Constructor value, curried by application.
    ///
    /// Applying a constructor accumulates args: `Ctor(name, []) arg` => `Ctor(name, [arg])`.
    Ctor { name: String, args: Vec<IrValue> },
    /// Symbol value (distinct from constructors).
    Symbol(String),
    /// Concrete list value.
    List(Vec<IrValue>),
    /// Concrete tuple value.
    Tuple(Vec<IrValue>),
    /// Concrete record value.
    Record(BTreeMap<String, IrValue>),
    /// Concrete modemap value (record extension).
    ModeMap(BTreeMap<String, IrValue>),
    Raised(Box<IrValue>),
    Fun { param: Pattern, body: Term, env: HashMap<String, IrValue> },
    Alt { left: Box<IrValue>, right: Box<IrValue> },
    Builtin { name: String, args: Vec<IrValue> },
}

#[derive(thiserror::Error, Debug)]
pub enum IrEvalError {
    #[error("unbound ref: {0}")]
    Unbound(String),
    #[error("not a function: {0:?}")]
    NotFunction(Box<IrValue>),
    #[error("select target is not a ModeMap: {0:?}")]
    SelectNotModeMap(Box<IrValue>),
    #[error("mode label not found: {0}")]
    ModeLabelNotFound(String),
    #[error("unsupported pattern in lambda parameter: {0}")]
    UnsupportedParam(String),
    #[error("unsupported pattern: {0}")]
    UnsupportedPattern(String),
    #[error("arity error for builtin: {0}")]
    Arity(String),
    #[error("unsupported coreir op: {0}")]
    UnsupportedOp(String),
}

fn merge_bindings(
    mut a: HashMap<String, IrValue>,
    b: HashMap<String, IrValue>,
) -> Option<HashMap<String, IrValue>> {
    for (k, v) in b {
        if let Some(prev) = a.get(&k) {
            if prev != &v {
                return None;
            }
        }
        a.insert(k, v);
    }
    Some(a)
}

fn match_pattern(
    p: &Pattern,
    v: &IrValue,
) -> Result<Option<HashMap<String, IrValue>>, IrEvalError> {
    match &p.kind {
        PatternKind::Wildcard => Ok(Some(HashMap::new())),
        PatternKind::Var(name) => {
            let mut m = HashMap::new();
            m.insert(name.clone(), v.clone());
            Ok(Some(m))
        }
        PatternKind::Unit => Ok(matches!(v, IrValue::Unit).then(HashMap::new)),
        PatternKind::Int(n) => Ok(matches!(v, IrValue::Int(m) if m == n).then(HashMap::new)),
        PatternKind::Float(f) => Ok(matches!(v, IrValue::Float(g) if g == f).then(HashMap::new)),
        PatternKind::Str(s) => Ok(matches!(v, IrValue::Str(t) if t == s).then(HashMap::new)),
        PatternKind::Char(c) => Ok(matches!(v, IrValue::Char(d) if d == c).then(HashMap::new)),
        PatternKind::Symbol(s) => {
            Ok(matches!(v, IrValue::Symbol(t) if t == s).then(HashMap::new))
        }
        PatternKind::Ctor { name, args } => {
            let IrValue::Ctor { name: got, args: got_args } = v else {
                return Ok(None);
            };
            if got != name {
                return Ok(None);
            }
            if got_args.len() != args.len() {
                return Ok(None);
            }
            let mut acc = HashMap::new();
            for (p, v) in args.iter().zip(got_args.iter()) {
                let Some(b) = match_pattern(p, v)? else {
                    return Ok(None);
                };
                let Some(next) = merge_bindings(acc, b) else {
                    return Ok(None);
                };
                acc = next;
            }
            Ok(Some(acc))
        }
        PatternKind::Tuple(ps) => {
            let IrValue::Tuple(vs) = v else {
                return Ok(None);
            };
            if ps.len() != vs.len() {
                return Ok(None);
            }
            let mut acc = HashMap::new();
            for (p, v) in ps.iter().zip(vs.iter()) {
                let Some(b) = match_pattern(p, v)? else {
                    return Ok(None);
                };
                let Some(next) = merge_bindings(acc, b) else {
                    return Ok(None);
                };
                acc = next;
            }
            Ok(Some(acc))
        }
        PatternKind::List(ps) => {
            let IrValue::List(vs) = v else {
                return Ok(None);
            };
            if ps.len() != vs.len() {
                return Ok(None);
            }
            let mut acc = HashMap::new();
            for (p, v) in ps.iter().zip(vs.iter()) {
                let Some(b) = match_pattern(p, v)? else {
                    return Ok(None);
                };
                let Some(next) = merge_bindings(acc, b) else {
                    return Ok(None);
                };
                acc = next;
            }
            Ok(Some(acc))
        }
        PatternKind::Cons(h, t) => {
            let IrValue::List(vs) = v else {
                return Ok(None);
            };
            if vs.is_empty() {
                return Ok(None);
            }
            let head = &vs[0];
            let tail = IrValue::List(vs[1..].to_vec());
            let Some(bh) = match_pattern(h, head)? else {
                return Ok(None);
            };
            let Some(bt) = match_pattern(t, &tail)? else {
                return Ok(None);
            };
            Ok(merge_bindings(bh, bt))
        }
        PatternKind::Record(fields) => {
            let map = match v {
                IrValue::Record(map) | IrValue::ModeMap(map) => map,
                _ => return Ok(None),
            };
            let mut acc = HashMap::new();
            for f in fields {
                let Some(vf) = map.get(&f.name) else {
                    return Ok(None);
                };
                let Some(b) = match_pattern(&f.pattern, vf)? else {
                    return Ok(None);
                };
                let Some(next) = merge_bindings(acc, b) else {
                    return Ok(None);
                };
                acc = next;
            }
            Ok(Some(acc))
        }
        PatternKind::TypeBind { pat, .. } => match_pattern(pat, v),
        PatternKind::As(a, b) => {
            let Some(ba) = match_pattern(a, v)? else {
                return Ok(None);
            };
            let Some(bb) = match_pattern(b, v)? else {
                return Ok(None);
            };
            Ok(merge_bindings(ba, bb))
        }
    }
}

fn is_tuple_tag_symbol(s: &str) -> bool {
    // Matches runtime tuple tag encoding like `.,,` (dot then commas).
    let mut chars = s.chars();
    if chars.next() != Some('.') {
        return false;
    }
    let rest: Vec<char> = chars.collect();
    !rest.is_empty() && rest.iter().all(|c| *c == ',')
}

fn mode_label_key(label: &str) -> &str {
    label.strip_prefix('.').unwrap_or(label)
}

fn legacy_symbol_to_value(s: &str) -> IrValue {
    // Legacy behavior for Op::Symbol in older dumps.
    if s == "[]" {
        return IrValue::List(vec![]);
    }
    if s.starts_with('.') && !is_tuple_tag_symbol(s) {
        IrValue::Symbol(s.to_string())
    } else {
        IrValue::Ctor { name: s.to_string(), args: vec![] }
    }
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
            IrValue::Str(s) => s.clone(),
            IrValue::Char(c) => {
                let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
                let mut tmp = String::new();
                tmp.push(ch);
                format!("'{}'", tmp.escape_default())
            }
            IrValue::Ctor { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    format!(
                        "{} {}",
                        name,
                        args.iter().map(print_ir_value).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            IrValue::Symbol(s) => s.clone(),
            IrValue::List(xs) => {
                format!("[{}]", xs.iter().map(print_ir_value).collect::<Vec<_>>().join(", "))
            }
            IrValue::Tuple(xs) => {
                format!("({})", xs.iter().map(print_ir_value).collect::<Vec<_>>().join(", "))
            }
            IrValue::Record(map) => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_ir_value(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            IrValue::ModeMap(map) => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_ir_value(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(".{{{}}}", inner)
            }
            IrValue::Raised(inner) => format!("^({})", match &**inner {
                IrValue::Str(s) => s.clone(),
                other => print_ir_value(other),
            }),
            IrValue::Fun { .. } | IrValue::Alt { .. } | IrValue::Builtin { .. } => "<fun>".into(),
        })),
        _ => Err(IrEvalError::Arity(name.into())),
    }
}

fn ir_equal(a: &IrValue, b: &IrValue) -> bool {
    match (a, b) {
        (IrValue::Unit, IrValue::Unit) => true,
        (IrValue::Int(x), IrValue::Int(y)) => x == y,
        (IrValue::Float(x), IrValue::Float(y)) => x == y,
        (IrValue::Str(x), IrValue::Str(y)) => x == y,
        (IrValue::Char(x), IrValue::Char(y)) => x == y,
        (IrValue::Ctor { name: nx, args: ax }, IrValue::Ctor { name: ny, args: ay }) => {
            nx == ny && ax.len() == ay.len() && ax.iter().zip(ay.iter()).all(|(x, y)| ir_equal(x, y))
        }
        (IrValue::Symbol(x), IrValue::Symbol(y)) => x == y,
        (IrValue::List(x), IrValue::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| ir_equal(x, y))
        }
        (IrValue::Tuple(x), IrValue::Tuple(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| ir_equal(x, y))
        }
        (IrValue::Record(x), IrValue::Record(y)) => {
            x.len() == y.len() && x.iter().all(|(k, vx)| y.get(k).is_some_and(|vy| ir_equal(vx, vy)))
        }
        (IrValue::ModeMap(x), IrValue::ModeMap(y)) => {
            x.len() == y.len() && x.iter().all(|(k, vx)| y.get(k).is_some_and(|vy| ir_equal(vx, vy)))
        }
        (IrValue::Raised(x), IrValue::Raised(y)) => ir_equal(x, y),
        _ => false,
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
        IrValue::Raised(_) => Ok(func),
        IrValue::Ctor { name, mut args } => {
            args.push(arg);
            Ok(IrValue::Ctor { name, args })
        }
        IrValue::Fun { param, body, mut env } => {
            let Some(bindings) = match_pattern(&param, &arg)? else {
                return Ok(IrValue::Raised(Box::new(arg)));
            };
            for (k, v) in bindings {
                env.insert(k, v);
            }
            eval_term_with_env(&body, &mut env)
        }
        IrValue::Alt { left, right } => {
            let input = arg;
            match eval_app((*left).clone(), input.clone())? {
                IrValue::Raised(payload) => {
                    if ir_equal(payload.as_ref(), &input) {
                        eval_app((*right).clone(), input)
                    } else {
                        Ok(IrValue::Raised(payload))
                    }
                }
                other => Ok(other),
            }
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
        other => Err(IrEvalError::NotFunction(Box::new(other))),
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
        Op::Bool(b) => Ok(IrValue::Ctor {
            name: if *b { "True".into() } else { "False".into() },
            args: vec![],
        }),
        Op::Str(s) => Ok(IrValue::Str(s.clone())),
        Op::Char(c) => Ok(IrValue::Char(*c)),
        Op::List { items } => {
            let mut vs = Vec::with_capacity(items.len());
            for item in items {
                let v = eval_term_with_env(item, env)?;
                if let IrValue::Raised(_) = v {
                    return Ok(v);
                }
                vs.push(v);
            }
            Ok(IrValue::List(vs))
        }
        Op::Record { fields } => {
            let mut map = BTreeMap::new();
            for f in fields {
                let v = eval_term_with_env(&f.value, env)?;
                if let IrValue::Raised(_) = v {
                    return Ok(v);
                }
                map.insert(f.name.clone(), v);
            }
            Ok(IrValue::Record(map))
        }
        Op::ModeMap { fields } => {
            let mut map = BTreeMap::new();
            for f in fields {
                let v = eval_term_with_env(&f.value, env)?;
                if let IrValue::Raised(_) = v {
                    return Ok(v);
                }
                map.insert(f.name.clone(), v);
            }
            Ok(IrValue::ModeMap(map))
        }
        Op::Select { label, target, .. } => {
            let v = eval_term_with_env(target, env)?;
            if let IrValue::Raised(_) = v {
                return Ok(v);
            }
            let IrValue::ModeMap(map) = v else {
                return Err(IrEvalError::SelectNotModeMap(Box::new(v)));
            };
            let key = mode_label_key(label);
            map.get(key)
                .cloned()
                .ok_or_else(|| IrEvalError::ModeLabelNotFound(label.clone()))
        }
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
        Op::Symbol(s) => Ok(legacy_symbol_to_value(s)),
        Op::Ctor(s) => Ok(IrValue::Ctor { name: s.clone(), args: vec![] }),
        Op::AtomSymbol(s) => Ok(IrValue::Symbol(s.clone())),
        Op::Raise { payload } => {
            let v = eval_term_with_env(payload, env)?;
            Ok(IrValue::Raised(Box::new(v)))
        }
        Op::OrElse { left, right } => {
            // Spec: discard the result of LHS and return the result of RHS.
            // Discard LHS even if it is Raised; if RHS is Raised, the whole is Raised.
            let _ = eval_term_with_env(left, env)?;
            eval_term_with_env(right, env)
        }
        Op::Catch { left, right } => {
            let lv = eval_term_with_env(left, env)?;
            match lv {
                IrValue::Raised(payload) => {
                    let rv = eval_term_with_env(right, env)?;
                    eval_app(rv, *payload)
                }
                other => Ok(other),
            }
        }
        Op::Alt { left, right } => {
            let lv = eval_term_with_env(left, env)?;
            if let IrValue::Raised(_) = lv {
                return Ok(lv);
            }
            let rv = eval_term_with_env(right, env)?;
            if let IrValue::Raised(_) = rv {
                return Ok(rv);
            }
            Ok(IrValue::Alt { left: Box::new(lv), right: Box::new(rv) })
        }
        Op::Lam { param, body } => {
            // Keep full pattern for alt/catch semantics.
            Ok(IrValue::Fun { param: param.clone(), body: (*body.clone()), env: env.clone() })
        }
        Op::App { func, arg } => {
            let f = eval_term_with_env(func, env)?;
            let a = eval_term_with_env(arg, env)?;
            if let IrValue::Raised(_) = f {
                return Ok(f);
            }
            if let IrValue::Raised(_) = a {
                return Ok(a);
            }
            eval_app(f, a)
        }
        Op::Seq { first, second } | Op::Chain { first, second } => {
            let v1 = eval_term_with_env(first, env)?;
            if let IrValue::Raised(_) = v1 {
                return Ok(v1);
            }
            eval_term_with_env(second, env)
        }
        Op::Bind { value, cont } => {
            let v = eval_term_with_env(value, env)?;
            if let IrValue::Raised(_) = v {
                return Ok(v);
            }
            let k = eval_term_with_env(cont, env)?;
            if let IrValue::Raised(_) = k {
                return Ok(k);
            }
            eval_app(k, v)
        }
        Op::LetRec { bindings, body } => {
            // For letrec, we need to support mutual recursion by creating thunks
            // In a lazy language, each binding would be a thunk that closes over the extended environment.
            // For this PoC, we'll use a simpler approach: evaluate each binding in the extended environment.

            // Step 1: Add all binding names to the environment as placeholders (Unit for now)
            // This allows bindings to reference each other
            for (name, _) in bindings {
                env.insert(name.clone(), IrValue::Unit);
            }

            // Step 2: Evaluate each binding in the extended environment and update immediately
            // This allows later bindings to use earlier ones (left-to-right evaluation order)
            for (name, term) in bindings {
                let value = eval_term_with_env(term, env)?;
                if let IrValue::Raised(_) = value {
                    return Ok(value);
                }
                env.insert(name.clone(), value);
            }

            // Step 3: Evaluate the body in the extended environment
            eval_term_with_env(body, env)
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
        IrValue::Str(s) => s.clone(),
        IrValue::Char(c) => {
            let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
            let mut tmp = String::new();
            tmp.push(ch);
            format!("'{}'", tmp.escape_default())
        }
        IrValue::Ctor { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                format!("{} {}", name, args.iter().map(print_ir_value).collect::<Vec<_>>().join(" "))
            }
        }
        IrValue::Symbol(s) => s.clone(),
        IrValue::List(xs) => {
            format!("[{}]", xs.iter().map(print_ir_value).collect::<Vec<_>>().join(", "))
        }
        IrValue::Tuple(xs) => {
            format!("({})", xs.iter().map(print_ir_value).collect::<Vec<_>>().join(", "))
        }
        IrValue::Record(map) => {
            let inner = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, print_ir_value(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", inner)
        }
        IrValue::ModeMap(map) => {
            let inner = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, print_ir_value(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!(".{{{}}}", inner)
        }
        IrValue::Raised(inner) => format!("^({})", print_ir_value(inner)),
        IrValue::Fun { .. } | IrValue::Alt { .. } | IrValue::Builtin { .. } => "<fun>".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int_expr(n: i64) -> Expr {
        Expr::new(ExprKind::Int(n), Span::new(0, 0))
    }

    #[test]
    fn lower_list_is_explicit_op_and_eval_is_list_value() {
        let e = Expr::new(
            ExprKind::List(vec![int_expr(1), int_expr(2), int_expr(3)]),
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&e);
        match &t.op {
            Op::List { items } => {
                assert_eq!(items.len(), 3);
            }
            other => panic!("expected Op::List, got {:?}", other),
        }
        assert_eq!(print_term(&t), "[1, 2, 3]");
        let v = eval_term(&t).expect("eval list");
        assert_eq!(print_ir_value(&v), "[1, 2, 3]");
    }

    #[test]
    fn lower_record_and_modemap_are_explicit_ops_and_preserve_name_spans() {
        let a = ExprRecordField::new("a".into(), Span::new(10, 1), int_expr(1));
        let b = ExprRecordField::new("b".into(), Span::new(20, 1), int_expr(2));

        let rec = Expr::new(ExprKind::Record(vec![a.clone(), b.clone()]), Span::new(0, 0));
        let t = lower_expr_to_core(&rec);
        match &t.op {
            Op::Record { fields } => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name, "a");
                assert_eq!(fields[0].name_span, Span::new(10, 1));
                assert_eq!(fields[1].name, "b");
                assert_eq!(fields[1].name_span, Span::new(20, 1));
            }
            other => panic!("expected Op::Record, got {:?}", other),
        }
        assert_eq!(print_term(&t), "{a: 1, b: 2}");
        let v = eval_term(&t).expect("eval record");
        assert_eq!(print_ir_value(&v), "{a: 1, b: 2}");

        let mm = Expr::new(ExprKind::ModeMap(vec![a, b]), Span::new(0, 0));
        let t = lower_expr_to_core(&mm);
        match &t.op {
            Op::ModeMap { fields } => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name, "a");
                assert_eq!(fields[0].name_span, Span::new(10, 1));
                assert_eq!(fields[1].name, "b");
                assert_eq!(fields[1].name_span, Span::new(20, 1));
            }
            other => panic!("expected Op::ModeMap, got {:?}", other),
        }
        assert_eq!(print_term(&t), ".{a: 1, b: 2}");
        let v = eval_term(&t).expect("eval modemap");
        assert_eq!(print_ir_value(&v), ".{a: 1, b: 2}");
    }

    #[test]
    fn lower_select_is_explicit_op_and_eval_selects_modemap_arm() {
        let a = ExprRecordField::new("Int".into(), Span::new(10, 3), int_expr(1));
        let b = ExprRecordField::new("Str".into(), Span::new(20, 3), Expr::new(ExprKind::Str("x".into()), Span::new(0, 0)));
        let mm = Expr::new(ExprKind::ModeMap(vec![a, b]), Span::new(0, 0));

        let select = Expr::new(
            ExprKind::Apply {
                func: Box::new(Expr::new(
                    ExprKind::Apply {
                        func: Box::new(Expr::new(ExprKind::Symbol(".select".into()), Span::new(0, 0))),
                        arg: Box::new(Expr::new(ExprKind::Symbol(".Int".into()), Span::new(1, 4))),
                    },
                    Span::new(0, 0),
                )),
                arg: Box::new(mm),
            },
            Span::new(0, 0),
        );

        let t = lower_expr_to_core(&select);
        match &t.op {
            Op::Select { label, label_span, .. } => {
                assert_eq!(label, ".Int");
                assert_eq!(*label_span, Span::new(1, 4));
            }
            other => panic!("expected Op::Select, got {other:?}"),
        }
        assert!(print_term(&t).starts_with("(.select .Int "));
        let v = eval_term(&t).expect("eval select");
        assert_eq!(v, IrValue::Int(1));
    }

    #[test]
    fn eval_select_missing_label_is_error() {
        let mm = Expr::new(
            ExprKind::ModeMap(vec![ExprRecordField::new("Int".into(), Span::new(0, 0), int_expr(1))]),
            Span::new(0, 0),
        );
        let select = Expr::new(
            ExprKind::Apply {
                func: Box::new(Expr::new(
                    ExprKind::Apply {
                        func: Box::new(Expr::new(ExprKind::Symbol(".select".into()), Span::new(0, 0))),
                        arg: Box::new(Expr::new(ExprKind::Symbol(".Str".into()), Span::new(0, 0))),
                    },
                    Span::new(0, 0),
                )),
                arg: Box::new(mm),
            },
            Span::new(0, 0),
        );
        let t = lower_expr_to_core(&select);
        let err = eval_term(&t).expect_err("missing label");
        match err {
            IrEvalError::ModeLabelNotFound(s) => assert_eq!(s, ".Str"),
            other => panic!("expected ModeLabelNotFound, got {other:?}"),
        }
    }

    #[test]
    fn eval_ctor_pattern_match_success_and_mismatch_raises_input() {
        let pat = Pattern::new(
            PatternKind::Ctor {
                name: "Foo".into(),
                args: vec![Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0))],
            },
            Span::new(0, 0),
        );
        let lam = Term::lambda(pat, Term::var("x"));

        let foo_42 = Term::app(Term::new(Op::Ctor("Foo".into())), Term::int(42));
        let ok = Term::app(lam.clone(), foo_42);
        let v = eval_term(&ok).expect("eval ctor match");
        assert_eq!(v, IrValue::Int(42));

        let bar_1 = Term::app(Term::new(Op::Ctor("Bar".into())), Term::int(1));
        let bad = Term::app(lam, bar_1.clone());
        let v = eval_term(&bad).expect("eval ctor mismatch");
        match v {
            IrValue::Raised(payload) => assert_eq!(*payload, eval_term(&bar_1).unwrap()),
            other => panic!("expected Raised(arg), got {other:?}"),
        }
    }

    #[test]
    fn lower_raise_is_explicit_op() {
        let inner = int_expr(1);
        let e = Expr::new(ExprKind::Raise(Box::new(inner)), Span::new(0, 0));
        let t = lower_expr_to_core(&e);
        match &t.op {
            Op::Raise { payload } => match &payload.op {
                Op::Int(1) => {}
                other => panic!("expected payload Int(1), got {:?}", other),
            },
            other => panic!("expected Raise, got {:?}", other),
        }
        assert_eq!(print_term(&t), "^(1)");
    }

    #[test]
    fn lower_alt_lambda_is_explicit_op() {
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
        match &t.op {
            Op::Alt { left, right } => {
                match &left.op {
                    Op::Lam { .. } => {}
                    other => panic!("expected Lam (left), got {:?}", other),
                }
                match &right.op {
                    Op::Lam { .. } => {}
                    other => panic!("expected Lam (right), got {:?}", other),
                }
            }
            other => panic!("expected Alt, got {:?}", other),
        }
        let s = print_term(&t);
        assert!(s.contains(" | "));
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
        // Even if LetGroup contains % type declarations, lowering to Core IR ignores them (generates only LetRec)
        let opt_td = TypeDecl {
            name: "Option".into(),
            params: vec!["a".into()],
            body: TypeDefBody::Sum(vec![
                (lzscr_ast::ast::Tag::Name("None".into()), vec![]),
                (lzscr_ast::ast::Tag::Name("Some".into()), vec![TypeExpr::Var("a".into())]),
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

    #[test]
    fn eval_ir_letrec() {
        // Test LetRec evaluation: (letrec { ~x = 1; ~y = (~add ~x 2); } ~y) => 3
        let bindings = vec![
            ("x".into(), Term::new(Op::Int(1))),
            (
                "y".into(),
                Term::new(Op::App {
                    func: Box::new(Term::new(Op::App {
                        func: Box::new(Term::new(Op::Ref("add".into()))),
                        arg: Box::new(Term::new(Op::Ref("x".into()))),
                    })),
                    arg: Box::new(Term::new(Op::Int(2))),
                }),
            ),
        ];
        let body = Term::new(Op::Ref("y".into()));
        let letrec = Term::new(Op::LetRec { bindings, body: Box::new(body) });

        let result = eval_term(&letrec).expect("ir eval letrec");
        assert_eq!(print_ir_value(&result), "3");
    }

    #[test]
    fn eval_ir_letrec_function() {
        // Test LetRec with function: (letrec { ~f = \~x -> (~add ~x 1); } (~f 5)) => 6
        let bindings = vec![(
            "f".into(),
            Term::new(Op::Lam {
                param: Pattern::new(PatternKind::Var("x".into()), Span::new(0, 0)),
                body: Box::new(Term::new(Op::App {
                    func: Box::new(Term::new(Op::App {
                        func: Box::new(Term::new(Op::Ref("add".into()))),
                        arg: Box::new(Term::new(Op::Ref("x".into()))),
                    })),
                    arg: Box::new(Term::new(Op::Int(1))),
                })),
            }),
        )];
        let body = Term::new(Op::App {
            func: Box::new(Term::new(Op::Ref("f".into()))),
            arg: Box::new(Term::new(Op::Int(5))),
        });
        let letrec = Term::new(Op::LetRec { bindings, body: Box::new(body) });

        let result = eval_term(&letrec).expect("ir eval letrec with function");
        assert_eq!(print_ir_value(&result), "6");
    }

    #[test]
    fn eval_ir_letrec_nested() {
        // Test nested LetRec: (letrec { ~x = (letrec { ~y = 2; } ~y); } (~add ~x 3)) => 5
        let inner_bindings = vec![("y".into(), Term::new(Op::Int(2)))];
        let inner_body = Term::new(Op::Ref("y".into()));
        let inner_letrec =
            Term::new(Op::LetRec { bindings: inner_bindings, body: Box::new(inner_body) });

        let outer_bindings = vec![("x".into(), inner_letrec)];
        let outer_body = Term::new(Op::App {
            func: Box::new(Term::new(Op::App {
                func: Box::new(Term::new(Op::Ref("add".into()))),
                arg: Box::new(Term::new(Op::Ref("x".into()))),
            })),
            arg: Box::new(Term::new(Op::Int(3))),
        });
        let outer_letrec =
            Term::new(Op::LetRec { bindings: outer_bindings, body: Box::new(outer_body) });

        let result = eval_term(&outer_letrec).expect("ir eval nested letrec");
        assert_eq!(print_ir_value(&result), "5");
    }

    #[test]
    fn eval_ir_letrec_closure_capture() {
        // Test LetRec with closure capturing outer bindings:
        // (letrec { ~x = 10; ~f = \~y -> (~add ~x ~y); } (~f 5)) => 15
        let bindings = vec![
            ("x".into(), Term::new(Op::Int(10))),
            (
                "f".into(),
                Term::new(Op::Lam {
                    param: Pattern::new(PatternKind::Var("y".into()), Span::new(0, 0)),
                    body: Box::new(Term::new(Op::App {
                        func: Box::new(Term::new(Op::App {
                            func: Box::new(Term::new(Op::Ref("add".into()))),
                            arg: Box::new(Term::new(Op::Ref("x".into()))),
                        })),
                        arg: Box::new(Term::new(Op::Ref("y".into()))),
                    })),
                }),
            ),
        ];
        let body = Term::new(Op::App {
            func: Box::new(Term::new(Op::Ref("f".into()))),
            arg: Box::new(Term::new(Op::Int(5))),
        });
        let letrec = Term::new(Op::LetRec { bindings, body: Box::new(body) });

        let result = eval_term(&letrec).expect("ir eval letrec with closure");
        assert_eq!(print_ir_value(&result), "15");
    }

    #[test]
    fn eval_ir_seq_integration() {
        // Test Seq with LetRec: (~seq 1 (letrec { ~x = 2; } ~x)) => 2
        let bindings = vec![("x".into(), Term::new(Op::Int(2)))];
        let body = Term::new(Op::Ref("x".into()));
        let letrec = Term::new(Op::LetRec { bindings, body: Box::new(body) });

        let seq =
            Term::new(Op::Seq { first: Box::new(Term::new(Op::Int(1))), second: Box::new(letrec) });

        let result = eval_term(&seq).expect("ir eval seq with letrec");
        assert_eq!(print_ir_value(&result), "2");
    }

    #[test]
    fn eval_ir_chain_integration() {
        // Test Chain with LetRec operations
        let bindings = vec![("x".into(), Term::new(Op::Int(5)))];
        let body = Term::new(Op::Ref("x".into()));
        let letrec = Term::new(Op::LetRec { bindings, body: Box::new(body) });

        let chain = Term::new(Op::Chain {
            first: Box::new(letrec),
            second: Box::new(Term::new(Op::Int(10))),
        });

        let result = eval_term(&chain).expect("ir eval chain with letrec");
        assert_eq!(print_ir_value(&result), "10");
    }

    #[test]
    fn test_convenience_constructors() {
        // Test convenience constructors: let x = 5 in x + 3
        let bindings = vec![("x".into(), Term::int(5))];
        let body = Term::app(Term::app(Term::var("add"), Term::var("x")), Term::int(3));
        let expr = Term::letrec(bindings, body);

        let result = eval_term(&expr).expect("eval with convenience constructors");
        assert_eq!(print_ir_value(&result), "8");
    }

    #[test]
    fn test_convenience_lambda() {
        // Test lambda convenience: (\x -> x + 1) 5
        let lambda = Term::lambda_var(
            "x",
            Term::app(Term::app(Term::var("add"), Term::var("x")), Term::int(1)),
        );
        let expr = Term::app(lambda, Term::int(5));

        let result = eval_term(&expr).expect("eval lambda with convenience constructors");
        assert_eq!(print_ir_value(&result), "6");
    }

    #[test]
    fn eval_alt_fallback_on_pattern_mismatch_only() {
        // Left matches only 1; right matches anything.
        // Apply to 2 => left mismatches => ^(2) equals input => fallback => 20
        let left = Term::lambda(
            Pattern::new(PatternKind::Int(1), Span::new(0, 0)),
            Term::int(10),
        );
        let right = Term::lambda(Pattern::new(PatternKind::Wildcard, Span::new(0, 0)), Term::int(20));
        let alt = Term::new(Op::Alt { left: Box::new(left), right: Box::new(right) });
        let app = Term::app(alt, Term::int(2));
        let v = eval_term(&app).expect("eval alt");
        assert_eq!(print_ir_value(&v), "20");

        // If left raises a payload not equal to input, it should NOT fallback.
        let left2 = Term::lambda(
            Pattern::new(PatternKind::Wildcard, Span::new(0, 0)),
            Term::new(Op::Raise { payload: Box::new(Term::int(999)) }),
        );
        let right2 = Term::lambda(Pattern::new(PatternKind::Wildcard, Span::new(0, 0)), Term::int(1));
        let alt2 = Term::new(Op::Alt { left: Box::new(left2), right: Box::new(right2) });
        let app2 = Term::app(alt2, Term::int(2));
        let v2 = eval_term(&app2).expect("eval alt");
        assert_eq!(print_ir_value(&v2), "^(999)");
    }
}
