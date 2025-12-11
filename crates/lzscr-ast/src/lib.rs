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

    /// Record field in a type expression with field name span tracking.
    /// Phase 5: Diagnostics Improvement - enables precise error reporting for type record fields.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct TypeExprRecordField {
        pub name: String,
        pub name_span: Span,
        pub type_expr: TypeExpr,
    }

    impl TypeExprRecordField {
        pub fn new(name: String, name_span: Span, type_expr: TypeExpr) -> Self {
            Self { name, name_span, type_expr }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum TypeExpr {
        Unit,
        Int,
        Float,
        Bool,
        Str,
        Char,
        List(Box<TypeExpr>),
        Tuple(Vec<TypeExpr>),
        Record(Vec<TypeExprRecordField>), // Phase 5: Now tracks field name spans
        // Sum type literal, e.g. `%{ A T1 | B T2 U }` used as a first-class type value
        Sum(Vec<(String, Vec<TypeExpr>)>),
        Fun(Box<TypeExpr>, Box<TypeExpr>),
        Ctor { tag: String, args: Vec<TypeExpr> },
        Var(String),          // %a etc. (only within %{...} syntax)
        Hole(Option<String>), // ? or ?a
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum TypeDefBody {
        // Sum type: .Tag T1 T2 | .Tag2 ...
        Sum(Vec<(String, Vec<TypeExpr>)>),
        // Future: alias/record etc. can be added
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct TypeDecl {
        pub name: String,        // type name (identifier without leading %)
        pub params: Vec<String>, // formal type variables (identifiers without %)
        pub body: TypeDefBody,
        pub span: Span,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum PatternKind {
        Wildcard,                                  // _
        Var(String),                               // ~x (produced as ~ident at parse time)
        Unit,                                      // ()
        Tuple(Vec<Pattern>),                       // (p1, p2, ...)
        Ctor { name: String, args: Vec<Pattern> }, // Foo p1 p2 / .Foo p1 p2
        Symbol(String),                            // match a symbol value (foo, .bar, ...)
        Int(i64),
        Float(f64),
        Str(String),
        Char(i32),
        Record(Vec<PatternRecordField>), // { k: p, ... } - Phase 5: Now tracks field name spans
        As(Box<Pattern>, Box<Pattern>),  // p1 @ p2
        // List patterns
        List(Vec<Pattern>),               // [p1, p2, ...]
        Cons(Box<Pattern>, Box<Pattern>), // h : t
        // Type variable binder for pattern scope: %{ 'a, ?x } p
        TypeBind { tvars: Vec<String>, pat: Box<Pattern> },
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

    /// Record field in an expression with field name span tracking.
    /// Phase 5: Diagnostics Improvement - enables precise error reporting for record fields.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct ExprRecordField {
        pub name: String,
        pub name_span: Span,
        pub value: Expr,
    }

    impl ExprRecordField {
        pub fn new(name: String, name_span: Span, value: Expr) -> Self {
            Self { name, name_span, value }
        }
    }

    /// Record field in a pattern with field name span tracking.
    /// Phase 5: Diagnostics Improvement - enables precise error reporting for pattern record fields.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct PatternRecordField {
        pub name: String,
        pub name_span: Span,
        pub pattern: Pattern,
    }

    impl PatternRecordField {
        pub fn new(name: String, name_span: Span, pattern: Pattern) -> Self {
            Self { name, name_span, pattern }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum ExprKind {
        Unit,
        Int(i64),
        Float(f64),
        Str(String),
        Char(i32),
        Ref(String),    // ~name
        Symbol(String), // bare symbol (constructor var candidate)
        // Record literal: { k1: e1, k2: e2, ... }
        // Phase 5: Now tracks field name spans for better diagnostics
        Record(Vec<ExprRecordField>),
        // Mode map: .{ Mode: expr, ... } for polymorphic function selection
        // Maps mode names (Pure, Strict, etc.) to implementations
        ModeMap(Vec<ExprRecordField>),
        // Type annotation: %{T} e (identity)
        Annot { ty: TypeExpr, expr: Box<Expr> },
        // First-class type value: %{T}
        TypeVal(TypeExpr),
        Lambda { param: Pattern, body: Box<Expr> },
        Apply { func: Box<Expr>, arg: Box<Expr> },
        Block(Box<Expr>),
        // List literal: [e1, e2, ...]
        List(Vec<Expr>),
        // Let-group: (p1 = e1; ...; body; pN = eN; ...)
        // Bindings can mutually/recursively reference each other within the group scope.
        // type_decls is the list of % type declarations within a Let group (merging both pre and post forms)
        LetGroup { type_decls: Vec<TypeDecl>, bindings: Vec<(Pattern, Expr)>, body: Box<Expr> },
        // Exceptions / control
        Raise(Box<Expr>), // ^(Expr)
        // Alternative lambda composition: (\p1 -> e1) | (\p2 -> e2)
        // Right-associative, lower precedence than || and ->
        AltLambda { left: Box<Expr>, right: Box<Expr> },
        OrElse { left: Box<Expr>, right: Box<Expr> }, // a || b
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
    pub fn print_pattern(p: &Pattern) -> String {
        match &p.kind {
            PatternKind::Wildcard => "_".into(),
            PatternKind::Var(n) => format!("~{}", n),
            PatternKind::Unit => "()".into(),
            PatternKind::Tuple(xs) => {
                format!("({})", xs.iter().map(print_pattern).collect::<Vec<_>>().join(", "))
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
            PatternKind::List(xs) => {
                format!("[{}]", xs.iter().map(print_pattern).collect::<Vec<_>>().join(", "))
            }
            PatternKind::Cons(h, t) => format!("{} : {}", print_pattern(h), print_pattern(t)),
            PatternKind::TypeBind { tvars, pat } => {
                let inner = tvars.iter().map(|s| format!("'{}", s)).collect::<Vec<_>>().join(", ");
                format!("%{{{}}} {}", inner, print_pattern(pat))
            }
        }
    }
    pub fn print_expr(e: &Expr) -> String {
        match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("{n}"),
            ExprKind::Float(f) => format!("{}", f),
            ExprKind::Str(s) => format!("\"{}\"", s.escape_default()),
            ExprKind::Char(c) => {
                let ch = char::from_u32(*c as u32).unwrap_or('\u{FFFD}');
                let mut tmp = String::new();
                tmp.push(ch);
                format!("'{}'", tmp.escape_default())
            }
            ExprKind::Ref(n) => format!("~{n}"),
            ExprKind::Symbol(s) => s.clone(),
            ExprKind::Record(fields) => {
                let inner = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, print_expr(&f.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {inner} }}")
            }
            ExprKind::ModeMap(fields) => {
                let inner = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, print_expr(&f.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(".{{ {inner} }}")
            }
            ExprKind::Annot { ty, expr } => {
                format!("%{{{}}} {}", print_type(ty), print_expr(expr))
            }
            ExprKind::TypeVal(ty) => {
                format!("%{{{}}}", print_type(ty))
            }
            ExprKind::Lambda { param, body } => {
                format!("\\{} -> {}", print_pattern(param), print_expr(body))
            }
            ExprKind::Apply { func, arg } => {
                format!("({} {})", print_expr(func), print_expr(arg))
            }
            ExprKind::Block(inner) => format!("{{ {} }}", print_expr(inner)),
            ExprKind::List(xs) => {
                format!("[{}]", xs.iter().map(print_expr).collect::<Vec<_>>().join(", "))
            }
            ExprKind::LetGroup { bindings, body, .. } => {
                // Show type declarations briefly (only the count)
                let bs = bindings
                    .iter()
                    .map(|(p, ex)| format!("{} = {};", print_pattern(p), print_expr(ex)))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({} {})", bs, print_expr(body))
            }
            ExprKind::Raise(inner) => format!("^({})", print_expr(inner)),
            ExprKind::AltLambda { left, right } => {
                format!("({} | {})", print_expr(left), print_expr(right))
            }
            ExprKind::OrElse { left, right } => {
                format!("({} || {})", print_expr(left), print_expr(right))
            }
            ExprKind::Catch { left, right } => {
                format!("({} ^| {})", print_expr(left), print_expr(right))
            }
        }
    }

    fn print_type(t: &TypeExpr) -> String {
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
            TypeExpr::List(x) => format!("[{}]", print_type(x)),
            TypeExpr::Tuple(xs) => {
                format!("({})", xs.iter().map(print_type).collect::<Vec<_>>().join(", "))
            }
            TypeExpr::Record(fields) => {
                let inner = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, print_type(&f.type_expr)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            TypeExpr::Fun(a, b) => format!("{} -> {}", print_type(a), print_type(b)),
            TypeExpr::Ctor { tag, args } => {
                let head = tag.clone();
                if args.is_empty() {
                    head
                } else {
                    format!(
                        "{} {}",
                        head,
                        args.iter().map(print_type).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            TypeExpr::Sum(alts) => {
                let parts = alts
                    .iter()
                    .map(|(tag, args)| {
                        if args.is_empty() {
                            tag.clone()
                        } else {
                            format!(
                                "{} {}",
                                tag,
                                args.iter().map(print_type).collect::<Vec<_>>().join(" ")
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                parts
            }
        }
    }
}
