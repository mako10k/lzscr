//! Runtime error types for lzscr evaluation.

#[derive(Debug, Clone)]
pub enum EvalError {
    TypeError,
    Unbound(String),
    UnknownEffect(String),
    EffectNotAllowed,
    NotFunc,
    NotApplicable(String), // Value cannot be applied (with type description)
    Traced { kind: Box<EvalError>, spans: Vec<lzscr_ast::span::Span> },
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::TypeError => write!(f, "type error"),
            EvalError::Unbound(n) => write!(f, "unbound variable: {n}"),
            EvalError::UnknownEffect(e) => write!(f, "unknown effect: {e}"),
            EvalError::EffectNotAllowed => write!(f, "effects not allowed in this context"),
            EvalError::NotFunc => write!(f, "not a function"),
            EvalError::NotApplicable(desc) => write!(f, "cannot apply argument to {desc}"),
            EvalError::Traced { kind, .. } => write!(f, "{kind}"),
        }
    }
}
