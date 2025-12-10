use lzscr_preast::{
    preast_to_source, preast_to_source_with_opts, to_preast, FormatOpts as PreFormatOpts,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum FormatError {
    #[error("parse error: {0}")]
    Parse(String),
}

#[derive(Debug, Clone, Copy)]
pub struct FormatOptions {
    pub indent: usize,
    pub max_width: usize,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self { indent: 2, max_width: 100 }
    }
}

/// Format a source string by parsing and pretty-printing the AST.
/// This is a minimal formatter MVP that preserves semantics via round-trip.
pub fn format_source(src: &str) -> Result<String, FormatError> {
    // PRE-AST preserving comments; for now return normalized spacing around original tokens/comments
    let pre = to_preast(src).map_err(|e| FormatError::Parse(e.to_string()))?;
    Ok(preast_to_source(&pre))
}

/// Same as `format_source`, but with options.
pub fn format_source_with_options(src: &str, opts: FormatOptions) -> Result<String, FormatError> {
    let pre = to_preast(src).map_err(|e| FormatError::Parse(e.to_string()))?;
    let p = PreFormatOpts { indent: opts.indent, max_width: opts.max_width };
    Ok(preast_to_source_with_opts(&pre, &p))
}

/// Format "file-like" content. Since the language parser currently recognizes
/// LetGroup only within parentheses, we wrap the input to parse. The output will
/// omit the wrapping parentheses when the top-level is a LetGroup.
pub fn format_file_source(src: &str) -> Result<String, FormatError> {
    format_source(src)
}

/// File-like formatting with options.
pub fn format_file_source_with_options(
    src: &str,
    opts: FormatOptions,
) -> Result<String, FormatError> {
    format_source_with_options(src, opts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_add() {
        let src = "1 + 2 * 3";
        let out = format_source(src).unwrap();
        // expect normalized spacing and original infix preserved
        assert_eq!(out, "1 + 2 * 3");
    }

    #[test]
    fn format_file_does_not_indent_top_level() {
        let src = "let a = 1; let b = 2;";
        let out = format_file_source(src).unwrap();
        assert_eq!(out, "let a = 1;\nlet b = 2;\n");
    }

    #[test]
    fn formats_records_multiline() {
        let src = "{a:1,b:{c:2,d:3}}";
        let out = format_source(src).unwrap();
        assert_eq!(out, "{\n  a: 1,\n  b: {\n    c: 2,\n    d: 3\n  }\n}",);
    }

    #[test]
    fn aligns_assignments_inside_closure() {
        let src = "!{\n  short = 1;\n  much_longer_name = 22;\n  mid = 3;\n}";
        let out = format_source(src).unwrap();
        assert_eq!(
            out,
            "!{\n  short            = 1;\n  much_longer_name = 22;\n  mid              = 3;\n  }",
        );
    }

    #[test]
    fn aligns_record_fields() {
        let src = "{\n  short: 1,\n  much_longer_key: 22,\n  mid: 3\n}";
        let out = format_source(src).unwrap();
        assert_eq!(
            out,
            "{\n  short          : 1,\n  much_longer_key: 22,\n  mid            : 3\n}",
        );
    }
}
