//! Thunk types for lazy evaluation in lzscr runtime.

use crate::{Env, Value};
use lzscr_ast::ast::{Expr, Pattern};

#[derive(Debug, Clone)]
pub enum ThunkState {
    Unevaluated,
    Evaluating,
    Evaluated(Box<Value>),
}

#[derive(Debug, Clone)]
pub enum ThunkKind {
    Expr { expr: Expr, env: std::rc::Rc<std::cell::RefCell<Env>> },
    Project { src: Box<Value>, pattern: Pattern, var: String },
}
