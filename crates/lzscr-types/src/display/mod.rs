//! Type display and pretty-printing functionality.
//!
//! This module provides multiple type display formats:
//! - `pretty`: Modern format with alphabetic variable names (%a, %b, ...)
//! - `legacy`: Backward-compatible format with raw variable IDs (%t0, %t1, ...)
//! - `user_pretty`: User-friendly format with normalization and %{ ... } wrapping

mod legacy;
mod pretty;
mod user_pretty;

pub(crate) use pretty::pp_type;
pub(crate) use user_pretty::{user_pretty_type, user_pretty_type_and_map};

// Legacy format is available but not exported by default
#[allow(unused_imports)]
use legacy::pp_type_legacy;

// Re-export normalize_type_and_map as an alias for backward compatibility
pub(crate) use user_pretty::user_pretty_type_and_map as normalize_type_and_map;
