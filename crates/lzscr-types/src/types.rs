//! Core type representations for lzscr type system.
//!
//! This module defines the fundamental types used throughout the type inference engine:
//! - `TvId`: Type variable identifier
//! - `Type`: The main type representation enum with 15 variants
//!
//! These types form the foundation for all other modules in lzscr-types.

use lzscr_ast::span::Span;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Type variable identifier.
///
/// Each type variable is assigned a unique ID during type inference.
/// Display format: `%t0`, `%t1`, etc. (raw form) or `%a`, `%b`, etc. (normalized form).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Copy)]
pub struct TvId(pub u32);

impl std::fmt::Display for TvId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

/// The main type representation for the lzscr type system.
///
/// Supports Hindley-Milner rank-1 type inference with additional features:
/// - Primitive types: `Unit`, `Int`, `Float`, `Str`, `Char`
/// - Type variables: `Var(TvId)` for unification
/// - Functions: `Fun(domain, codomain)`
/// - Collections: `List(element)`, `Tuple(elements)`
/// - Records: `Record(fields)` with optional span information
/// - Constructors: `Ctor { tag, payload }` for ADT-like values
/// - Named types: `Named { name, args }` for type aliases (desugared during unification)
/// - Unions: `SumCtor(variants)` for AltLambda argument/return types
///
/// # Examples
///
/// ```ignore
/// let int_type = Type::Int;
/// let list_int = Type::List(Box::new(Type::Int));
/// let fun_type = Type::fun(Type::Int, Type::Int);
/// ```
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    /// Unit type: `()`
    Unit,
    /// Integer type: `Int`
    Int,
    /// Floating-point type: `Float`
    Float,
    /// String type: `Str`
    Str,
    /// Character type: `Char`
    Char,
    /// First-class type value (experimental): `Type`
    Type,
    /// Type variable: used during unification
    Var(TvId),
    /// Function type: `a -> b`
    Fun(Box<Type>, Box<Type>),
    /// List type: `List a`
    List(Box<Type>),
    /// Tuple type: `(a, b, c, ...)`
    Tuple(Vec<Type>),
    /// Record type: `{ field1: Type1, field2: Type2, ... }`
    ///
    /// Each field maps to a tuple of (Type, optional Span for error reporting).
    /// Records are closed (exact field set required for unification).
    Record(BTreeMap<String, (Type, Option<Span>)>),
    /// Constructor type: `.Tag payload1 payload2 ...`
    ///
    /// Represents tagged values like `.Some x` or `.Ok value`.
    /// Payload is a vector to support multi-argument constructors.
    Ctor { tag: String, payload: Vec<Type> },
    /// Named type: syntactic sugar for type aliases
    ///
    /// Desugared to actual type during unification (e.g., via typedef lookup).
    Named { name: String, args: Vec<Type> },
    /// Sum of constructors: `(.Tag1 T1 | .Tag2 T2 | ...)`
    ///
    /// Used for AltLambda argument types (union of pattern constructors).
    /// Invariants:
    /// - Tags are sorted lexicographically
    /// - No duplicate tags
    /// - At least 2 variants (single variant collapses to `Ctor`)
    SumCtor(Vec<(String, Vec<Type>)>),
}

impl Type {
    /// Helper constructor for function types.
    ///
    /// Creates `Fun(Box::new(a), Box::new(b))` with less boilerplate.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let id_type = Type::fun(Type::Var(tv0), Type::Var(tv0));
    /// ```
    pub(crate) fn fun(a: Type, b: Type) -> Type {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Delegate to pp_type (defined in display module)
        // This is a forward declaration; actual implementation uses crate::display::pp_type
        write!(f, "{}", crate::pp_type(self))
    }
}
