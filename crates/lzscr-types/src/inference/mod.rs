//! Type inference engine for lzscr.
//!
//! This module implements Hindley-Milner type inference with extensions for:
//! - AltLambda (pattern-based function chaining)
//! - Effects (IO operations with tracking)
//! - Named types and typedefs
//! - Record types with span tracking

pub(crate) mod context;
pub(crate) mod pattern;

// Re-exports
pub(crate) use context::{DebugConfig, InferCtx, lookup_tyvar, pop_tyvars, push_tyvars_from_pattern};
pub(crate) use pattern::{PatInfo, infer_pattern};

// Note: These re-exports are used by lib.rs (infer_expr and related functions)
