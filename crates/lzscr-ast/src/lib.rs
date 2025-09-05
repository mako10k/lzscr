pub mod span {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
    pub struct Span {
        pub offset: usize,
        pub len: usize,
    }
    impl Span {
        pub fn new(offset: usize, len: usize) -> Self {
            Self { offset, len }
        }
    }
}

pub mod ast {
    use crate::span::Span;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum PatternKind {
        Wildcard,                                  // _
        Var(String),                               // ~x（パース時は ~ident で生成）
        Unit,                                      // ()
        Tuple(Vec<Pattern>),                       // (p1, p2, ...)
        Ctor { name: String, args: Vec<Pattern> }, // Foo p1 p2 / .Foo p1 p2
        Symbol(String),                            // シンボル値に一致（foo, .bar など）
        Int(i64),
        Float(f64),
        Str(String),
        Bool(bool),
        Record(Vec<(String, Pattern)>), // { k: p, ... }
        As(Box<Pattern>, Box<Pattern>), // p1 @ p2
    // List patterns
    List(Vec<Pattern>),            // [p1, p2, ...]
    Cons(Box<Pattern>, Box<Pattern>), // h : t
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct Pattern {
        pub kind: PatternKind,
        pub span: Span,
    }
    impl Pattern {
        pub fn new(kind: PatternKind, span: Span) -> Self {
            Self { kind, span }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum ExprKind {
        Unit,
        Int(i64),
        Float(f64),
        Str(String),
        Ref(String),    // ~name
        Symbol(String), // bare symbol (constructor var candidate)
        Lambda { param: Pattern, body: Box<Expr> },
        Apply { func: Box<Expr>, arg: Box<Expr> },
        Block(Box<Expr>),
        // List literal: [e1, e2, ...]
        List(Vec<Expr>),
        // Let-group: (p1 = e1; ...; body; pN = eN; ...)
        // Bindings can mutually/recursively reference each other within the group scope.
        LetGroup {
            bindings: Vec<(Pattern, Expr)>,
            body: Box<Expr>,
        },
        // Exceptions / control
        Raise(Box<Expr>),                             // ^(Expr)
        OrElse { left: Box<Expr>, right: Box<Expr> }, // a | b
        Catch { left: Box<Expr>, right: Box<Expr> },  // a ^| b
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct Expr {
        pub kind: ExprKind,
        pub span: Span,
    }
    impl Expr {
        pub fn new(kind: ExprKind, span: Span) -> Self {
            Self { kind, span }
        }
    }
}

pub mod pretty {
    use crate::ast::*;
    fn print_pattern(p: &Pattern) -> String {
        match &p.kind {
            PatternKind::Wildcard => "_".into(),
            PatternKind::Var(n) => format!("~{}", n),
            PatternKind::Unit => "()".into(),
            PatternKind::Tuple(xs) => {
                format!(
                    "({})",
                    xs.iter().map(print_pattern).collect::<Vec<_>>().join(", ")
                )
            }
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
            PatternKind::Symbol(s) => s.clone(),
            PatternKind::Int(n) => format!("{}", n),
            PatternKind::Float(f) => format!("{}", f),
            PatternKind::Str(s) => format!("\"{}\"", s.escape_default()),
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
            PatternKind::List(xs) => {
                format!(
                    "[{}]",
                    xs.iter().map(print_pattern).collect::<Vec<_>>().join(", ")
                )
            }
            PatternKind::Cons(h, t) => format!("{} : {}", print_pattern(h), print_pattern(t)),
        }
    }
    pub fn print_expr(e: &Expr) -> String {
        match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("{n}"),
            ExprKind::Float(f) => format!("{}", f),
            ExprKind::Str(s) => format!("\"{}\"", s.escape_default()),
            ExprKind::Ref(n) => format!("~{n}"),
            ExprKind::Symbol(s) => s.clone(),
            ExprKind::Lambda { param, body } => {
                format!("\\{} -> {}", print_pattern(param), print_expr(body))
            }
            ExprKind::Apply { func, arg } => {
                format!("({} {})", print_expr(func), print_expr(arg))
            }
            ExprKind::Block(inner) => format!("{{ {} }}", print_expr(inner)),
            ExprKind::List(xs) => {
                format!(
                    "[{}]",
                    xs.iter().map(print_expr).collect::<Vec<_>>().join(", ")
                )
            }
            ExprKind::LetGroup { bindings, body } => {
                let bs = bindings
                    .iter()
                    .map(|(p, ex)| format!("{} = {};", print_pattern(p), print_expr(ex)))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({} {})", bs, print_expr(body))
            }
            ExprKind::Raise(inner) => format!("^({})", print_expr(inner)),
            ExprKind::OrElse { left, right } => {
                format!("({} | {})", print_expr(left), print_expr(right))
            }
            ExprKind::Catch { left, right } => {
                format!("({} ^| {})", print_expr(left), print_expr(right))
            }
        }
    }
}
