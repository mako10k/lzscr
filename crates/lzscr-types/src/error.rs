//! Type error definitions and error suggestion helpers.
//!
//! This module defines:
//! - `TypeError`: All possible type inference errors with rich span information
//! - Error suggestion functions: `edit_distance()`, `find_similar_names()`, `format_field_path()`
//!
//! Errors support dual-span reporting for better diagnostics (e.g., expected vs actual).

use crate::diagnostic::{DiagnosticSpan, DualSpan};
use crate::types::{TvId, Type};

/// Type inference errors with detailed span information for diagnostics.
///
/// Most variants include span information (offset, len) for precise error reporting.
/// Some variants support dual-span errors (shows both expected and actual locations).
#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    /// Type mismatch (single-span)
    #[error("type mismatch: expected {expected} vs actual {actual} at ({span_offset},{span_len})")]
    Mismatch { expected: Type, actual: Type, span_offset: usize, span_len: usize },
    /// Record field type mismatch (single-span, legacy)
    #[error("record field type mismatch: field '{field}' expected {expected} vs actual {actual} at ({span_offset},{span_len})")]
    RecordFieldMismatch {
        field: String,
        expected: Type,
        actual: Type,
        span_offset: usize,
        span_len: usize,
    },
    /// Record field type mismatch (dual-span)
    #[error("record field '{field}' type mismatch: expected %{{expected}} vs actual %{{actual}}")]
    RecordFieldMismatchBoth {
        field: String,
        expected: Type,
        actual: Type,
        expected_span_offset: usize,
        expected_span_len: usize,
        actual_span_offset: usize,
        actual_span_len: usize,
    },
    /// Type mismatch with dual-span reporting (expected vs actual)
    #[error("type mismatch: expected {expected} vs actual {actual}")]
    MismatchBoth {
        expected: Type,
        actual: Type,
        expected_span_offset: usize,
        expected_span_len: usize,
        actual_span_offset: usize,
        actual_span_len: usize,
    },
    /// Type annotation mismatch
    #[error("type mismatch: expected {expected} vs actual {actual}")]
    AnnotMismatch {
        expected: Type,
        actual: Type,
        annot_span_offset: usize,
        annot_span_len: usize,
        expr_span_offset: usize,
        expr_span_len: usize,
    },
    /// Occurs check failure (infinite type detection)
    #[error("cannot construct infinite type: type variable {var_pretty} occurs within its own definition {pretty}")]
    Occurs {
        var: TvId,
        var_pretty: String, // normalized pretty name for the variable (e.g. %a)
        ty: Type,
        pretty: String, // normalized pretty name for the type (%{ ... })
        var_span_offset: usize,
        var_span_len: usize,
        ty_span_offset: usize,
        ty_span_len: usize,
    },
    /// Duplicate constructor tag in union
    #[error("constructor union duplicate tag: {tag} at ({span_offset},{span_len})")]
    DuplicateCtorTag { tag: String, span_offset: usize, span_len: usize },
    /// Mixed constructor and non-constructor patterns in AltLambda
    #[error("AltLambda branches mixed: expected all Ctor patterns or wildcard/default only at ({span_offset},{span_len})")]
    MixedAltBranches { span_offset: usize, span_len: usize },
    /// AltLambda constructor arity mismatch
    #[error("AltLambda arity mismatch: expected {expected} args but got {got}")]
    AltLambdaArityMismatch {
        expected: usize,
        got: usize,
        expected_span_offset: usize,
        expected_span_len: usize,
        actual_span_offset: usize,
        actual_span_len: usize,
    },
    /// Not a function type (applied to argument)
    #[error("not a function: {ty}")]
    NotFunction { ty: Type },
    /// Unbound variable reference with suggestions
    #[error("unbound reference: {name} at ({span_offset},{span_len})")]
    UnboundRef { name: String, span_offset: usize, span_len: usize, suggestions: Vec<String> },
    /// Effect used in non-effect context (strict-effects mode)
    #[error("effect not allowed at ({span_offset},{span_len})")]
    EffectNotAllowed { span_offset: usize, span_len: usize },
    /// Negative occurrence of recursive type (non-positive position)
    #[error("negative occurrence of recursive type {type_name} at ({span_offset},{span_len})")]
    NegativeOccurrence { type_name: String, span_offset: usize, span_len: usize },
    /// Invalid type declaration
    #[error("invalid type declaration: {msg} at ({span_offset},{span_len})")]
    InvalidTypeDecl { msg: String, span_offset: usize, span_len: usize },
}

impl TypeError {
    /// Convert this error to a DualSpan diagnostic if it has dual-span information.
    ///
    /// Returns `Some(DualSpan)` for errors with both expected and actual locations,
    /// `None` for single-span or non-span errors.
    pub fn as_dual_span(&self) -> Option<DualSpan> {
        match self {
            TypeError::MismatchBoth {
                expected_span_offset,
                expected_span_len,
                actual_span_offset,
                actual_span_len,
                ..
            }
            | TypeError::RecordFieldMismatchBoth {
                expected_span_offset,
                expected_span_len,
                actual_span_offset,
                actual_span_len,
                ..
            }
            | TypeError::AnnotMismatch {
                annot_span_offset: expected_span_offset,
                annot_span_len: expected_span_len,
                expr_span_offset: actual_span_offset,
                expr_span_len: actual_span_len,
                ..
            }
            | TypeError::AltLambdaArityMismatch {
                expected_span_offset,
                expected_span_len,
                actual_span_offset,
                actual_span_len,
                ..
            } => Some(
                DualSpan::from_offsets(
                    *expected_span_offset,
                    *expected_span_len,
                    *actual_span_offset,
                    *actual_span_len,
                )
                .with_labels("expected here", "actual here"),
            ),
            TypeError::Occurs {
                var_span_offset,
                var_span_len,
                ty_span_offset,
                ty_span_len,
                ..
            } => Some(
                DualSpan::from_offsets(
                    *var_span_offset,
                    *var_span_len,
                    *ty_span_offset,
                    *ty_span_len,
                )
                .with_labels("type variable defined here", "occurs inside here"),
            ),
            _ => None,
        }
    }

    /// Get the primary span for this error.
    ///
    /// Returns the most relevant span for single-span errors, or the actual/effect
    /// span for dual-span errors.
    pub fn primary_span(&self) -> Option<DiagnosticSpan> {
        match self {
            TypeError::Mismatch { span_offset, span_len, .. }
            | TypeError::RecordFieldMismatch { span_offset, span_len, .. }
            | TypeError::DuplicateCtorTag { span_offset, span_len, .. }
            | TypeError::MixedAltBranches { span_offset, span_len, .. }
            | TypeError::UnboundRef { span_offset, span_len, .. }
            | TypeError::EffectNotAllowed { span_offset, span_len, .. }
            | TypeError::NegativeOccurrence { span_offset, span_len, .. }
            | TypeError::InvalidTypeDecl { span_offset, span_len, .. } => {
                Some(DiagnosticSpan::new(*span_offset, *span_len))
            }
            TypeError::MismatchBoth { actual_span_offset, actual_span_len, .. }
            | TypeError::RecordFieldMismatchBoth { actual_span_offset, actual_span_len, .. }
            | TypeError::AnnotMismatch {
                expr_span_offset: actual_span_offset,
                expr_span_len: actual_span_len,
                ..
            }
            | TypeError::AltLambdaArityMismatch { actual_span_offset, actual_span_len, .. }
            | TypeError::Occurs {
                ty_span_offset: actual_span_offset,
                ty_span_len: actual_span_len,
                ..
            } => Some(DiagnosticSpan::new(*actual_span_offset, *actual_span_len)),
            TypeError::NotFunction { .. } => None,
        }
    }

    /// Check if this error supports dual-span reporting.
    pub fn has_dual_span(&self) -> bool {
        self.as_dual_span().is_some()
    }

    /// Get a user-friendly explanation for occurs check errors.
    ///
    /// Returns a detailed explanation of what went wrong and why the type cannot be constructed.
    pub fn occurs_explanation(&self) -> Option<String> {
        if let TypeError::Occurs { var_pretty, pretty, .. } = self {
            Some(format!(
                "The type variable {} would occur within its own definition.\n\
                 The inferred type would be: {} = {}\n\
                 This creates an infinite type (recursive definition without a fixpoint).\n\
                 \n\
                 Possible causes:\n\
                 - Missing type annotation on recursive function\n\
                 - Self-referential data structure without explicit type\n\
                 - Incorrect recursive call pattern",
                var_pretty, var_pretty, pretty
            ))
        } else {
            None
        }
    }

    /// Get fix-it hints for this error.
    ///
    /// Returns actionable suggestions based on the error type and context.
    pub fn fix_hints(&self) -> Vec<String> {
        match self {
            TypeError::Mismatch { expected, actual, .. }
            | TypeError::MismatchBoth { expected, actual, .. } => {
                suggest_fixes_for_mismatch(expected, actual)
            }
            TypeError::RecordFieldMismatch { field, expected, actual, .. }
            | TypeError::RecordFieldMismatchBoth { field, expected, actual, .. } => {
                suggest_fixes_for_record_field(field, expected, actual)
            }
            TypeError::UnboundRef { name, suggestions, .. } => {
                let mut hints = Vec::new();
                if !suggestions.is_empty() {
                    hints.push("Did you mean one of these?".to_string());
                    for suggestion in suggestions {
                        hints.push(format!("  - {}", suggestion));
                    }
                } else {
                    hints.push(format!("Variable '{}' is not defined in the current scope", name));
                    hints
                        .push("Check for typos or add the variable to the environment".to_string());
                }
                hints
            }
            TypeError::EffectNotAllowed { .. } => {
                vec![
                    "Effects require explicit sequencing".to_string(),
                    "Use ~seq or ~chain to enable effects:".to_string(),
                    "  (~seq () (!effect-call ...))".to_string(),
                    "or".to_string(),
                    "  (~chain (!effect-call ...) (\\~result -> ...))".to_string(),
                ]
            }
            TypeError::Occurs { .. } => {
                vec![
                    "Consider adding explicit type annotations".to_string(),
                    "For recursive functions, specify the return type".to_string(),
                    "For recursive data structures, use explicit type definitions".to_string(),
                ]
            }
            TypeError::MixedAltBranches { .. } => {
                vec![
                    "AltLambda requires consistent pattern style".to_string(),
                    "Either use:".to_string(),
                    "  - Constructor patterns: (\\SomeTag ... | OtherTag ... -> ...)".to_string(),
                    "or".to_string(),
                    "  - Wildcard/default only: (\\~x -> ... | \\_ -> ...)".to_string(),
                    "Cannot mix both styles in one AltLambda".to_string(),
                ]
            }
            TypeError::AltLambdaArityMismatch { expected, got, .. } => {
                vec![
                    format!("Constructor pattern expects {} arguments but got {}", expected, got),
                    "Check the constructor definition for the correct arity".to_string(),
                    "Ensure all branches have consistent argument counts".to_string(),
                ]
            }
            TypeError::AnnotMismatch { expected, actual, .. } => {
                let mut hints = suggest_fixes_for_mismatch(expected, actual);
                hints.insert(0, "Type annotation doesn't match inferred type".to_string());
                hints.push("Either fix the annotation or adjust the expression".to_string());
                hints
            }
            TypeError::NegativeOccurrence { type_name, .. } => {
                vec![
                    format!("Type '{}' occurs in a negative position", type_name),
                    "This breaks the positivity requirement for recursive types".to_string(),
                    "Recursive types must occur only in positive positions (e.g., as return types, not as function arguments)".to_string(),
                ]
            }
            _ => Vec::new(),
        }
    }
}

// ---------- Error Suggestion Helpers ----------

/// Compute Levenshtein edit distance between two strings.
///
/// Used for typo detection in variable name suggestions.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(edit_distance("kitten", "sitting"), 3);
/// assert_eq!(edit_distance("foo", "foo"), 0);
/// ```
pub fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row: Vec<usize> = vec![0; b_len + 1];

    for i in 1..=a_len {
        curr_row[0] = i;
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };
            curr_row[j] = std::cmp::min(
                std::cmp::min(curr_row[j - 1] + 1, prev_row[j] + 1),
                prev_row[j - 1] + cost,
            );
        }
        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Find similar variable names in the environment for typo suggestions.
///
/// Returns up to 3 suggestions sorted by edit distance (closest first).
/// Only suggests names within a reasonable edit distance (≤ min(3, len/2)).
///
/// # Arguments
///
/// * `target` - The unbound variable name
/// * `env` - The type environment to search
///
/// # Examples
///
/// ```ignore
/// let suggestions = find_similar_names("cound", &env);
/// // might return: ["count", "found", "bound"]
/// ```
pub fn find_similar_names(target: &str, env: &crate::TypeEnv) -> Vec<String> {
    let max_distance = std::cmp::min(3, target.len().div_ceil(2));
    let mut candidates: Vec<(String, usize)> = env
        .0
        .keys()
        .filter_map(|name| {
            let dist = edit_distance(target, name);
            if dist > 0 && dist <= max_distance {
                Some((name.clone(), dist))
            } else {
                None
            }
        })
        .collect();

    // Sort by distance first, then alphabetically
    candidates.sort_by(|a, b| a.1.cmp(&b.1).then(a.0.cmp(&b.0)));

    // Return top 3 suggestions
    candidates.into_iter().take(3).map(|(name, _)| name).collect()
}

/// Helper to format nested record field paths.
///
/// Used in error messages to show precise field location (e.g., "record.field.subfield").
#[inline]
pub fn format_field_path(parent: &str, child: &str) -> String {
    if child.is_empty() {
        parent.to_string()
    } else {
        format!("{parent}.{child}")
    }
}

/// Generate specific hints for record field errors.
///
/// Provides suggestions for missing fields, extra fields, or type mismatches.
pub fn suggest_fixes_for_record_field(_field: &str, expected: &Type, actual: &Type) -> Vec<String> {
    use crate::types::Type as T;
    let mut hints = Vec::new();

    // Extract field sets if both are records
    match (expected, actual) {
        (T::Record(expected_fields), T::Record(actual_fields)) => {
            let expected_keys: std::collections::HashSet<_> = expected_fields.keys().collect();
            let actual_keys: std::collections::HashSet<_> = actual_fields.keys().collect();

            // Find missing and extra fields
            let missing: Vec<_> = expected_keys.difference(&actual_keys).collect();
            let extra: Vec<_> = actual_keys.difference(&expected_keys).collect();

            if !missing.is_empty() {
                let missing_str =
                    missing.iter().map(|k| format!("'{}'", k)).collect::<Vec<_>>().join(", ");
                hints.push(format!("Missing field(s): {}", missing_str));
                hints.push("Add the missing field(s) to the record literal".to_string());
            }

            if !extra.is_empty() {
                let extra_str =
                    extra.iter().map(|k| format!("'{}'", k)).collect::<Vec<_>>().join(", ");
                hints.push(format!("Extra field(s): {}", extra_str));
                hints.push("Remove the extra field(s) from the record literal".to_string());
            }

            // Check for typos in field names
            if !missing.is_empty() && !extra.is_empty() {
                for m in &missing {
                    for e in &extra {
                        let dist = edit_distance(m, e);
                        if dist <= 2 {
                            hints.push(format!("Did you mean '{}' instead of '{}'?", m, e));
                        }
                    }
                }
            }
        }
        _ => {
            // Fallback for non-record types
            hints.extend(suggest_fixes_for_mismatch(expected, actual));
        }
    }

    hints
}

/// Generate specific fix-it hints based on type mismatch patterns.
///
/// Returns a list of actionable suggestions for common type errors.
pub fn suggest_fixes_for_mismatch(expected: &Type, actual: &Type) -> Vec<String> {
    use crate::types::Type as T;
    let mut hints = Vec::new();

    match (expected, actual) {
        // Int vs Float
        (T::Int, T::Float) => {
            hints.push("Use an integer literal (e.g., 42 instead of 42.0)".to_string());
            hints.push("Or convert with (~Int .from_float value)".to_string());
        }
        (T::Float, T::Int) => {
            hints.push("Use a float literal (e.g., 42.0 instead of 42)".to_string());
            hints.push("Or convert with (~Float .from_int value)".to_string());
        }
        // String vs Char
        (T::Str, T::Char) => {
            hints.push("Use a string literal (\"...\") instead of a character ('...')".to_string());
        }
        (T::Char, T::Str) => {
            hints.push("Use a character literal ('x') instead of a string (\"x\")".to_string());
        }
        // Effect type mismatch
        (T::Named { name: n1, .. }, _) if n1.starts_with("Effect") => {
            hints.push("Wrap the expression in an effect context:".to_string());
            hints.push("  (~seq () (!effect-call ...))".to_string());
        }
        (_, T::Named { name: n2, .. }) if n2.starts_with("Effect") => {
            hints.push("This produces an effect - use ~seq or ~chain to sequence it".to_string());
        }
        // List mismatch
        (T::List(expected_elem), T::List(actual_elem)) if expected_elem != actual_elem => {
            hints.push(format!(
                "List element type mismatch: expected [{}] but got [{}]",
                expected_elem, actual_elem
            ));
            hints.push("Ensure all list elements have the same type".to_string());
        }
        // Function arity hint
        (T::Fun(_, _), other) if !matches!(other, T::Fun(_, _)) => {
            hints.push("Expected a function but got a non-function value".to_string());
            hints.push("Wrap in a lambda if needed: (\\~arg -> ...)".to_string());
        }
        (other, T::Fun(_, _)) if !matches!(other, T::Fun(_, _)) => {
            hints.push("Got a function but expected a value".to_string());
            hints.push("Apply the function to its arguments".to_string());
        }
        // Tuple length mismatch
        (T::Tuple(expected_elems), T::Tuple(actual_elems))
            if expected_elems.len() != actual_elems.len() =>
        {
            hints.push(format!(
                "Tuple length mismatch: expected {} elements but got {}",
                expected_elems.len(),
                actual_elems.len()
            ));
        }
        // Record mismatch
        (T::Record(_), other) if !matches!(other, T::Record(_)) => {
            hints.push("Expected a record but got a non-record value".to_string());
            hints.push("Use record literal syntax: {{field1: value1, field2: value2}}".to_string());
        }
        // Generic advice
        _ => {
            hints.push("Consider adding an explicit type annotation:".to_string());
            hints.push("  (%{{expected-type}} expression)".to_string());
        }
    }

    hints
}
