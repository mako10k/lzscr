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
    pub enum ExprKind {
        Unit,
        Int(i64),
        Str(String),
        Ref(String),    // ~name
        Symbol(String), // bare symbol (constructor var candidate)
        Lambda { param: String, body: Box<Expr> },
        Apply { func: Box<Expr>, arg: Box<Expr> },
        Block(Box<Expr>),
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
    pub fn print_expr(e: &Expr) -> String {
        match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("{n}"),
            ExprKind::Str(s) => format!("\"{}\"", s.escape_default()),
            ExprKind::Ref(n) => format!("~{n}"),
            ExprKind::Symbol(s) => s.clone(),
            ExprKind::Lambda { param, body } => format!("\\{param} -> {}", print_expr(body)),
            ExprKind::Apply { func, arg } => format!("({} {})", print_expr(func), print_expr(arg)),
            ExprKind::Block(inner) => format!("{{ {} }}", print_expr(inner)),
        }
    }
}
