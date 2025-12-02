//! Type inference engine for lzscr.
//!
//! This module implements Hindley-Milner type inference with extensions for:
//! - AltLambda (pattern-based function chaining)
//! - Effects (IO operations with tracking)
//! - Named types and typedefs
//! - Record types with span tracking

pub(crate) mod context;
pub(crate) mod expr;
pub(crate) mod pattern;

// Re-exports (used by lib.rs and public API)
#[allow(unused_imports)]
pub(crate) use context::{DebugConfig, InferCtx, lookup_tyvar, pop_tyvars, push_tyvars_from_pattern};
pub(crate) use expr::infer_expr;
#[allow(unused_imports)]
pub(crate) use pattern::{PatInfo, infer_pattern};
