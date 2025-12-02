//! Diagnostic span utilities for error reporting.
//!
//! This module provides structured span information and formatting utilities
//! for rich error diagnostics with dual-span support.

use lzscr_ast::span::Span;

/// Context level for diagnostic spans.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanContext {
    /// Primary error location (expected/cause)
    Primary,
    /// Secondary error location (actual/effect)
    Secondary,
    /// Additional note or context
    Note,
}

/// Structured diagnostic span with label and context.
///
/// Used for dual-span error reporting where we want to show
/// both the expected/cause location and actual/effect location.
#[derive(Debug, Clone)]
pub struct DiagnosticSpan {
    /// Byte offset in source
    pub offset: usize,
    /// Length in bytes
    pub len: usize,
    /// Optional label (e.g., "expected here", "actual here")
    pub label: Option<String>,
    /// Context level (primary/secondary/note)
    pub context: SpanContext,
}

impl DiagnosticSpan {
    /// Create a new diagnostic span.
    pub fn new(offset: usize, len: usize) -> Self {
        Self { offset, len, label: None, context: SpanContext::Primary }
    }

    /// Create from AST span.
    pub fn from_span(span: Span) -> Self {
        Self::new(span.offset, span.len)
    }

    /// Set label for this span.
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    /// Set context level.
    pub fn with_context(mut self, context: SpanContext) -> Self {
        self.context = context;
        self
    }

    /// Mark as primary (expected/cause) location.
    pub fn as_primary(self) -> Self {
        self.with_context(SpanContext::Primary)
    }

    /// Mark as secondary (actual/effect) location.
    pub fn as_secondary(self) -> Self {
        self.with_context(SpanContext::Secondary)
    }

    /// Mark as note/additional context.
    pub fn as_note(self) -> Self {
        self.with_context(SpanContext::Note)
    }
}

/// Pair of spans for dual-span error reporting.
#[derive(Debug, Clone)]
pub struct DualSpan {
    /// Primary span (expected/cause)
    pub primary: DiagnosticSpan,
    /// Secondary span (actual/effect)
    pub secondary: DiagnosticSpan,
}

impl DualSpan {
    /// Create a new dual span pair.
    pub fn new(primary: DiagnosticSpan, secondary: DiagnosticSpan) -> Self {
        Self { primary, secondary }
    }

    /// Create from raw offsets and lengths.
    pub fn from_offsets(
        primary_offset: usize,
        primary_len: usize,
        secondary_offset: usize,
        secondary_len: usize,
    ) -> Self {
        Self {
            primary: DiagnosticSpan::new(primary_offset, primary_len)
                .with_context(SpanContext::Primary),
            secondary: DiagnosticSpan::new(secondary_offset, secondary_len)
                .with_context(SpanContext::Secondary),
        }
    }

    /// Set labels for both spans.
    pub fn with_labels(mut self, primary_label: &str, secondary_label: &str) -> Self {
        self.primary.label = Some(primary_label.to_string());
        self.secondary.label = Some(secondary_label.to_string());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_span_builder() {
        let span = DiagnosticSpan::new(10, 5)
            .with_label("expected here")
            .as_primary();

        assert_eq!(span.offset, 10);
        assert_eq!(span.len, 5);
        assert_eq!(span.label, Some("expected here".to_string()));
        assert_eq!(span.context, SpanContext::Primary);
    }

    #[test]
    fn dual_span_creation() {
        let dual = DualSpan::from_offsets(10, 5, 20, 3)
            .with_labels("expected here", "actual here");

        assert_eq!(dual.primary.offset, 10);
        assert_eq!(dual.primary.len, 5);
        assert_eq!(dual.primary.label, Some("expected here".to_string()));

        assert_eq!(dual.secondary.offset, 20);
        assert_eq!(dual.secondary.len, 3);
        assert_eq!(dual.secondary.label, Some("actual here".to_string()));
    }
}
