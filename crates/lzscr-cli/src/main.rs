use clap::{Parser, ValueEnum};
use lzscr_analyzer::{
    analyze_ctor_arity, analyze_duplicates, analyze_shadowing, analyze_unbound_refs,
    analyze_unused_params, default_allowlist, AnalyzeOptions,
};
use lzscr_ast::ast::*;
use lzscr_coreir::{eval_term, lower_expr_to_core, print_ir_value, print_term};
use lzscr_lexer as _lexer_for_caret; // for re-lexing to correct line/col in secondary caret block
use lzscr_parser::parse_expr;
use lzscr_runtime::{eval, Env, Value};
use serde::Serialize;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::time::Instant;

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum StdlibMode {
    Pure,
    AllowEffects,
}

impl StdlibMode {
    fn allows_effects(self) -> bool {
        matches!(self, StdlibMode::AllowEffects)
    }
}

// Map span offsets to their originating source (filename and text), supporting modules.
struct SourceRegistry {
    primary_name: String,
    primary_text: String,
    next_base: usize,
    modules: Vec<RegisteredSource>,
    // Optional mapping for primary buffer segments back to original sources
    prelude_name: Option<String>,
    prelude_text: Option<String>,
    prelude_start: Option<usize>,
    user_name: Option<String>,
    user_text: Option<String>,
    user_start: Option<usize>,
}

struct RegisteredSource {
    name: String,
    text: String,
    base: usize,
}

impl SourceRegistry {
    fn new(primary_name: String, primary_text: String) -> Self {
        // Place module bases after the primary text, aligned to 4KB for simplicity
        let len = primary_text.len();
        let align = 4096usize;
        let next_base = ((len + align) / align) * align;
        Self {
            primary_name,
            primary_text,
            next_base,
            modules: Vec::new(),
            prelude_name: None,
            prelude_text: None,
            prelude_start: None,
            user_name: None,
            user_text: None,
            user_start: None,
        }
    }

    fn with_segments(
        primary_name: String,
        primary_text: String,
        prelude: Option<(String, String, usize)>,
        user: Option<(String, String, usize)>,
    ) -> Self {
        let mut s = Self::new(primary_name, primary_text);
        if let Some((name, text, start)) = prelude {
            s.prelude_name = Some(name);
            s.prelude_text = Some(text);
            s.prelude_start = Some(start);
        }
        if let Some((name, text, start)) = user {
            s.user_name = Some(name);
            s.user_text = Some(text);
            s.user_start = Some(start);
        }
        s
    }

    fn register_module(&mut self, name: String, text: String) -> usize {
        let base = self.next_base;
        // Advance next_base by aligned size of this module
        let align = 4096usize;
        let size = ((text.len() + align) / align) * align;
        self.next_base = base + size;
        self.modules.push(RegisteredSource { name, text, base });
        base
    }

    fn format_span_block(&self, offset: usize, len: usize) -> String {
        // Decide which source contains this offset
        if offset < self.primary_text.len() {
            // Map into prelude or user original sources if configured
            if let (Some(name), Some(text), Some(start)) =
                (&self.prelude_name, &self.prelude_text, &self.prelude_start)
            {
                let st = *start;
                let ed = st + text.len();
                if offset >= st && offset < ed {
                    let rel = offset - st;
                    return format_span_caret(text, name, rel, len);
                }
            }
            if let (Some(name), Some(text), Some(start)) =
                (&self.user_name, &self.user_text, &self.user_start)
            {
                let st = *start;
                let ed = st + text.len();
                if offset >= st && offset < ed {
                    let rel = offset - st;
                    return format_span_caret(text, name, rel, len);
                }
            }
            return format_span_caret(&self.primary_text, &self.primary_name, offset, len);
        }
        for m in &self.modules {
            if offset >= m.base && offset < m.base + m.text.len() {
                let rel = offset - m.base;
                return format_span_caret(&m.text, &m.name, rel, len);
            }
        }
        // Fallback to primary
        format_span_caret(
            &self.primary_text,
            &self.primary_name,
            offset.min(self.primary_text.len()),
            len,
        )
    }

    // Find the first non-comment, non-whitespace character offset starting from `offset`.
    // Treat lines whose first non-space is '#' as comments and skip them.
    fn first_non_comment_offset_from(&self, offset: usize) -> usize {
        // Helper to scan within a given text with base offset
        fn scan(text: &str, base: usize, rel: usize) -> usize {
            let bytes = text.as_bytes();
            let mut i = rel.min(text.len());
            // Move to start of current line
            while i > 0 && bytes[i - 1] != b'\n' {
                i -= 1;
            }
            let mut pos = i;
            while pos < text.len() {
                // Find end of this line
                let line_end = match text[pos..].find('\n') {
                    Some(k) => pos + k,
                    None => text.len(),
                };
                // Find first non-space/tab char in this line
                let mut j = pos;
                while j < line_end {
                    let b = bytes[j];
                    if b == b' ' || b == b'\t' || b == b'\r' {
                        j += 1;
                    } else {
                        break;
                    }
                }
                if j < line_end {
                    let b0 = bytes[j];
                    if b0 == b'#' {
                        // pure comment line; skip it
                    } else if b0 == b'(' {
                        // Skip a leading '(' (wrapper) and whitespace to point inside
                        let mut k = j + 1;
                        while k < line_end {
                            let b = bytes[k];
                            if b == b' ' || b == b'\t' || b == b'\r' {
                                k += 1;
                            } else {
                                break;
                            }
                        }
                        if k < line_end {
                            if bytes[k] == b'#' {
                                // Line is just "(  #..."; treat as comment wrapper, skip line
                            } else {
                                return base + k;
                            }
                        }
                    } else {
                        return base + j;
                    }
                }
                // Go to next line start
                pos = if line_end < text.len() { line_end + 1 } else { line_end };
            }
            // Fallback: no better position found
            base + rel
        }
        if offset < self.primary_text.len() {
            return scan(&self.primary_text, 0, offset);
        }
        for m in &self.modules {
            if offset >= m.base && offset < m.base + m.text.len() {
                let rel = offset - m.base;
                return scan(&m.text, m.base, rel);
            }
        }
        offset
    }

    // Heuristic: find the last top-level balanced { ... } block in the same source
    // as the provided offset (ignoring braces inside strings, chars, and comments).
    // Returns the absolute span (offset, len) of the block if found.
    fn last_top_level_brace_block_in_same_source(&self, offset: usize) -> Option<(usize, usize)> {
        // Helper to scan a text and return the last top-level {..} span
        fn scan(text: &str) -> Option<(usize, usize)> {
            let bytes = text.as_bytes();
            let mut i = 0usize;
            let mut depth = 0i32;
            let mut stack: Vec<usize> = Vec::new();
            let mut pairs: Vec<(usize, usize)> = Vec::new();
            // comment/string states
            let mut in_line_comment = false;
            let mut block_comment_depth = 0i32; // nestable {- -}
            let mut in_str = false;
            let mut in_char = false;
            while i < bytes.len() {
                let b = bytes[i];
                // Handle line comment
                if in_line_comment {
                    if b == b'\n' {
                        in_line_comment = false;
                    }
                    i += 1;
                    continue;
                }
                // Handle block comment
                if block_comment_depth > 0 {
                    if b == b'-' && i + 1 < bytes.len() && bytes[i + 1] == b'}' {
                        block_comment_depth -= 1;
                        i += 2;
                        continue;
                    }
                    if b == b'{' && i + 1 < bytes.len() && bytes[i + 1] == b'-' {
                        block_comment_depth += 1;
                        i += 2;
                        continue;
                    }
                    i += 1;
                    continue;
                }
                // Handle strings
                if in_str {
                    if b == b'\\' {
                        i = (i + 2).min(bytes.len());
                        continue;
                    }
                    if b == b'"' {
                        in_str = false;
                    }
                    i += 1;
                    continue;
                }
                if in_char {
                    if b == b'\\' {
                        i = (i + 2).min(bytes.len());
                        continue;
                    }
                    if b == b'\'' {
                        in_char = false;
                    }
                    i += 1;
                    continue;
                }
                // Detect comment/string/char starts
                if b == b'#' {
                    in_line_comment = true;
                    i += 1;
                    continue;
                }
                if b == b'{' && i + 1 < bytes.len() && bytes[i + 1] == b'-' {
                    block_comment_depth = 1;
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    in_str = true;
                    i += 1;
                    continue;
                }
                if b == b'\'' {
                    in_char = true;
                    i += 1;
                    continue;
                }
                // Track braces
                if b == b'{' {
                    depth += 1;
                    stack.push(i);
                } else if b == b'}' && depth > 0 {
                    depth -= 1;
                    if let Some(op) = stack.pop() {
                        pairs.push((op, i));
                    }
                }
                i += 1;
            }
            pairs.last().map(|(op, cl)| (*op, *cl))
        }
        // Decide which source this offset belongs to and scan there
        if offset < self.primary_text.len() {
            return scan(&self.primary_text).map(|(op, cl)| (op, cl - op + 1));
        }
        for m in &self.modules {
            if offset >= m.base && offset < m.base + m.text.len() {
                return scan(&m.text).map(|(op, cl)| (m.base + op, cl - op + 1));
            }
        }
        None
    }
} // end impl SourceRegistry

fn format_span_caret(src: &str, name: &str, offset: usize, len: usize) -> String {
    // Build line starts
    let mut starts = Vec::new();
    starts.push(0usize);
    for (i, ch) in src.char_indices() {
        if ch == '\n' {
            starts.push(i + 1);
        }
    }
    // Find line by binary search
    let mut line_idx = 0usize;
    for (i, &st) in starts.iter().enumerate() {
        if st <= offset {
            line_idx = i;
        } else {
            break;
        }
    }
    let line_start = *starts.get(line_idx).unwrap_or(&0);
    // Extract line text (until next newline)
    let line_end = src[line_start..].find('\n').map(|k| line_start + k).unwrap_or(src.len());
    let line_txt = &src[line_start..line_end];
    let col = offset.saturating_sub(line_start);
    let mut caret = String::new();
    for _ in 0..col {
        caret.push(' ');
    }
    if len > 0 {
        caret.push('^');
        for _ in 1..len {
            caret.push('~');
        }
    } else {
        caret.push('^');
    }
    format!("at {}:{}:{}\n    {}\n    {}", name, line_idx + 1, col + 1, line_txt, caret)
}

fn parse_ctor_arity_spec(spec: &str) -> (HashMap<String, usize>, Vec<String>) {
    let mut map = HashMap::new();
    let mut warnings = Vec::new();
    for raw in spec.split(',') {
        let item = raw.trim();
        if item.is_empty() {
            continue;
        }
        let Some((name_raw, n_raw)) = item.split_once('=') else {
            warnings.push(format!("ignored ctor-arity entry (missing '='): '{}'", item));
            continue;
        };
        let name = name_raw.trim();
        let n_str = n_raw.trim();
        match n_str.parse::<usize>() {
            Ok(k) => {
                if name.is_empty() {
                    warnings.push(format!("ignored ctor-arity entry (empty name): '{}'", item));
                } else {
                    map.insert(name.to_string(), k);
                }
            }
            Err(_) => warnings
                .push(format!("ignored ctor-arity entry (invalid number '{}'): '{}'", n_str, item)),
        }
    }
    (map, warnings)
}

#[derive(Parser, Debug)]
#[command(name = "lzscr", version, about = "LazyScript reimplementation (skeleton)")]
struct Opt {
    /// One-line program
    #[arg(short = 'e', long = "eval")]
    eval: Option<String>,

    /// Execute program from file
    #[arg(short = 'f', long = "file")]
    file: Option<PathBuf>,

    /// Enforce strict-effects (placeholder; runtime enforcement TBD)
    #[arg(short = 's', long = "strict-effects", default_value_t = false)]
    strict_effects: bool,

    /// Control stdlib visibility (pure vs allow-effects)
    #[arg(long = "stdlib-mode", value_enum, default_value_t = StdlibMode::Pure)]
    stdlib_mode: StdlibMode,

    /// Run static analysis instead of executing
    #[arg(long = "analyze", default_value_t = false)]
    analyze: bool,

    /// Dump Core IR (text)
    #[arg(long = "dump-coreir", default_value_t = false)]
    dump_coreir: bool,

    /// Dump Core IR as JSON
    #[arg(long = "dump-coreir-json", default_value_t = false)]
    dump_coreir_json: bool,

    /// Evaluate via Core IR evaluator (PoC)
    #[arg(long = "eval-coreir", default_value_t = false)]
    eval_coreir: bool,

    /// Disable static typechecking (inference) before execution
    #[arg(long = "no-typecheck", default_value_t = false)]
    no_typecheck: bool,

    /// Type output mode: pretty|legacy|json (when typechecking runs)
    #[arg(long = "types", default_value = "pretty")]
    types: String,

    /// 型デバッグ詳細レベル (0=無効)
    #[arg(long = "type-debug", default_value_t = 0)]
    type_debug: usize,

    /// 型デバッグ最大深さ
    #[arg(long = "type-debug-depth", default_value_t = 64)]
    type_debug_depth: usize,

    /// 型デバッグフラグ (カンマ区切り: unify,env,schemes) 例: --type-debug-flags unify,env
    #[arg(long = "type-debug-flags", default_value = "unify,schemes")]
    type_debug_flags: String,

    /// Declare constructor arities (e.g., Foo=2,Bar=0). Comma-separated.
    #[arg(long = "ctor-arity")]
    ctor_arity: Option<String>,

    /// Output format for --analyze: text|json
    #[arg(long = "format", default_value = "text")]
    format: String,

    /// Format code instead of executing (pretty-print)
    #[arg(long = "format-code", default_value_t = false)]
    format_code: bool,

    /// Formatter indent width (spaces)
    #[arg(long = "fmt-indent")]
    fmt_indent: Option<usize>,

    /// Formatter max line width
    #[arg(long = "fmt-width")]
    fmt_width: Option<usize>,

    /// Disable loading the standard library prelude
    #[arg(long = "no-stdlib", default_value_t = false)]
    no_stdlib: bool,

    /// Specify stdlib directory (default: ./stdlib)
    #[arg(long = "stdlib-dir")]
    stdlib_dir: Option<PathBuf>,

    /// Additional module search paths (colon-separated)
    #[arg(long = "module-path")]
    module_path: Option<String>,

    /// Duplicate detection: minimum subtree size (nodes)
    #[arg(long = "dup-min-size", default_value_t = 3)]
    dup_min_size: usize,

    /// Duplicate detection: minimum occurrences
    #[arg(long = "dup-min-count", default_value_t = 2)]
    dup_min_count: usize,

    /// Skip duplicate detection pass (useful for large files)
    #[arg(long = "no-dup", default_value_t = false)]
    no_dup: bool,

    /// Print timing of analysis phases and sizes to stderr
    #[arg(long = "analyze-trace", default_value_t = false)]
    analyze_trace: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::parse();
    // Select input source: -e or --file
    let (mut code, input_name, user_raw, user_wrapped_parens) = if let Some(c) = opt.eval {
        (c.clone(), "(eval)".to_string(), c, false)
    } else if let Some(ref p) = opt.file {
        let raw = fs::read_to_string(p)?;
        if opt.format_code {
            // For formatting, keep raw; formatter has its own file-aware handling
            (raw.clone(), p.display().to_string(), raw, false)
        } else {
            // Wrap in parens so top-level becomes a let-block when it contains bindings
            (format!("({})", raw), p.display().to_string(), raw, true)
        }
    } else {
        eprintln!("no input; try -e '...' or --file path");
        return Ok(());
    };

    // Preload stdlib (M1): prepend prelude as a let-group unless disabled or in formatting mode
    let stdlib_enabled = !opt.no_stdlib && !opt.format_code;
    // Compute stdlib dir (may be used by ~require search paths)
    let resolved_stdlib_dir = opt.stdlib_dir.clone().unwrap_or_else(|| PathBuf::from("stdlib"));
    // Preserve prelude text and path for diagnostics (span mapping before ~require expansion)
    let mut prelude_for_diag: Option<(String, String)> = None;
    if stdlib_enabled {
        // Resolve stdlib dir
        let prelude_path = resolved_stdlib_dir.join("prelude.lzscr");
        if let Ok(prelude_src) = fs::read_to_string(&prelude_path) {
            // Combine as a let-group: ( prelude ; user )
            // Nest defensively even if user code is already parenthesized
            code = format!("({}\n{} )", prelude_src, code);
            prelude_for_diag = Some((prelude_src, prelude_path.display().to_string()));
        } else {
            eprintln!(
                "warning: stdlib prelude not found at {} (use --stdlib-dir or --no-stdlib)",
                prelude_path.display()
            );
        }
    }

    {
        // Formatting mode: run formatter first and exit
        if opt.format_code {
            let from_file = opt.file.is_some();
            let fmt_opts = lzscr_format::FormatOptions {
                indent: opt.fmt_indent.unwrap_or(2),
                max_width: opt.fmt_width.unwrap_or(100),
            };
            let out = if from_file {
                lzscr_format::format_file_source_with_options(&code, fmt_opts)
            } else {
                lzscr_format::format_source_with_options(&code, fmt_opts)
            };
            match out {
                Ok(s) => {
                    println!("{}", s);
                    return Ok(());
                }
                Err(e) => {
                    eprintln!("format error: {}", e);
                    std::process::exit(2);
                }
            }
        }

        // Parse first to surface nice errors with caret; then expand ~require
        let t_req_start = Instant::now();
        let module_search_paths =
            build_module_search_paths(&resolved_stdlib_dir, opt.module_path.as_deref());
        let ast0 = match lzscr_parser::parse_expr(&code) {
            Ok(x) => x,
            Err(e) => {
                use lzscr_parser::ParseError;
                match e {
                    ParseError::WithSpan { msg, span_offset, span_len } => {
                        eprintln!("parse error: {}", msg);
                        // If message includes "Opening '(' at line X:Y (offset Z)" extract and show second caret block.
                        let mut open_paren_block: Option<String> = None;
                        // (legacy patterns ignored)
                        if let Some(idx) = msg.find("Opening '(' at line ") {
                            // pattern: Opening '(' at line L:C (offset O)
                            let tail = &msg[idx + "Opening '(' at line ".len()..];
                            // Extract L
                            // Parse numbers manually
                            let mut lchars = tail.chars();
                            let mut line_s = String::new();
                            for ch in lchars.by_ref() {
                                if ch.is_ascii_digit() {
                                    line_s.push(ch);
                                } else if ch == ':' {
                                    break;
                                } else {
                                    line_s.clear();
                                    break;
                                }
                            }
                            let mut col_s = String::new();
                            for ch in lchars.by_ref() {
                                if ch.is_ascii_digit() {
                                    col_s.push(ch);
                                } else if ch == ' ' {
                                    break;
                                } else {
                                    col_s.clear();
                                    break;
                                }
                            }
                            let mut off_s = String::new();
                            if let Some(pos_off) = tail.find("(offset ") {
                                let after = &tail[pos_off + "(offset ".len()..];
                                for ch in after.chars() {
                                    if ch.is_ascii_digit() {
                                        off_s.push(ch);
                                    } else {
                                        break;
                                    }
                                }
                            }
                            if let (Ok(_line_no), Ok(_col_no), Ok(off_no)) = (
                                line_s.parse::<usize>(),
                                col_s.parse::<usize>(),
                                off_s.parse::<usize>(),
                            ) {
                                // Build caret block for the opening paren (length 1)
                                let raw_block = format_span_caret(&code, &input_name, off_no, 1);
                                // Re-lex to get authoritative line/col from token (accounts for any wrapper adjustments later)
                                let toks = _lexer_for_caret::lex(&code);
                                if let Some(tok) = toks.iter().find(|t| t.span.offset == off_no) {
                                    let mut lines = raw_block.lines();
                                    if let Some(first) = lines.next() {
                                        // Replace header line with token's recorded line/col
                                        let _orig_header = first; // keep for potential future debugging
                                        let new_first =
                                            format!("at {}:{}:{}", input_name, tok.line, tok.col);
                                        let rest: String =
                                            lines.map(|l| format!("\n{}", l)).collect();
                                        open_paren_block = Some(format!("{}{}", new_first, rest));
                                    } else {
                                        open_paren_block = Some(raw_block);
                                    }
                                } else {
                                    open_paren_block = Some(raw_block);
                                }
                            }
                        }
                        // Try to attribute the span to prelude or user input for a better filename
                        if let Some((ref pre_src, ref pre_name)) = prelude_for_diag {
                            let pre_start = 1; // after opening '('
                            let pre_end = pre_start + pre_src.len(); // before inserted '\n'
                            if span_offset >= pre_start && span_offset < pre_end {
                                let rel = span_offset - pre_start;
                                eprintln!(
                                    "{}",
                                    format_span_caret(pre_src, pre_name, rel, span_len)
                                );
                            } else {
                                // Compute user segment start in the combined buffer
                                let mut user_start = 0usize;
                                // If prelude was inserted: '(' + prelude + '\n'
                                user_start += 1 + pre_src.len() + 1;
                                // If file input was wrapped earlier: additional '('
                                if user_wrapped_parens {
                                    user_start += 1;
                                }
                                let rel = span_offset.saturating_sub(user_start);
                                eprintln!(
                                    "{}",
                                    format_span_caret(&user_raw, &input_name, rel, span_len)
                                );
                            }
                        } else {
                            // No prelude; show against the current input
                            eprintln!(
                                "{}",
                                format_span_caret(&code, &input_name, span_offset, span_len)
                            );
                        }
                        if let Some(block) = open_paren_block {
                            eprintln!("\n{}", block);
                        }
                    }
                    ParseError::WithSpan2 {
                        msg,
                        span1_offset,
                        span1_len,
                        span2_offset,
                        span2_len,
                    } => {
                        eprintln!("parse error: {}", msg);
                        // Always print both spans with proper source segmentation handling (reuse logic for first span)
                        let print_block = |off: usize, len: usize| {
                            if let Some((ref pre_src, ref pre_name)) = prelude_for_diag {
                                let pre_start = 1;
                                let pre_end = pre_start + pre_src.len();
                                if off >= pre_start && off < pre_end {
                                    let rel = off - pre_start;
                                    eprintln!("{}", format_span_caret(pre_src, pre_name, rel, len));
                                    return;
                                } else {
                                    let mut user_start = 0usize;
                                    user_start += 1 + pre_src.len() + 1;
                                    if user_wrapped_parens {
                                        user_start += 1;
                                    }
                                    let rel = off.saturating_sub(user_start);
                                    eprintln!(
                                        "{}",
                                        format_span_caret(&user_raw, &input_name, rel, len)
                                    );
                                    return;
                                }
                            }
                            eprintln!("{}", format_span_caret(&code, &input_name, off, len));
                        };
                        print_block(span1_offset, span1_len);
                        print_block(span2_offset, span2_len);
                    }
                    other => {
                        eprintln!("parse error: {}", other);
                    }
                }
                std::process::exit(2);
            }
        };
        // Build source registry and expand requires while rebasing spans for modules
        // Build SourceRegistry with precise segment mapping back to prelude and original user file
        let (prelude_seg, user_seg) = {
            // Compute prelude segment if present
            let prelude_seg = prelude_for_diag.as_ref().map(|(pre_src, pre_name)| {
                // Combined buffer layout when prelude is enabled:
                //   '(' + prelude + '\n' + (optional '(') + user + (optional ')') + ' )'
                let prelude_start = 1usize; // after leading '('
                (pre_name.clone(), pre_src.clone(), prelude_start)
            });
            // Compute user segment start in the combined buffer
            let user_start = if let Some((pre_src, _)) = prelude_for_diag.as_ref() {
                let mut s = 1 + pre_src.len() + 1; // '(' + prelude + '\n'
                if user_wrapped_parens {
                    s += 1; // the extra '('
                }
                s
            } else {
                // No prelude; we may still have wrapped parens for file input
                if user_wrapped_parens {
                    1
                } else {
                    0
                }
            };
            let user_seg = Some((input_name.clone(), user_raw.clone(), user_start));
            (prelude_seg, user_seg)
        };
        let mut src_reg =
            SourceRegistry::with_segments(input_name.clone(), code.clone(), prelude_seg, user_seg);
        let ast = match expand_requires_in_expr(
            &ast0,
            &module_search_paths,
            &mut Vec::new(),
            &mut src_reg,
            opt.stdlib_mode,
        ) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("require error: {}", e);
                std::process::exit(2);
            }
        };
        if opt.analyze_trace {
            eprintln!("trace: require-expand+parse {} ms", t_req_start.elapsed().as_millis());
        }
        let ast_nodes = {
            fn count(e: &Expr) -> usize {
                use ExprKind::*;
                match &e.kind {
                    Unit | Int(_) | Float(_) | Str(_) | Char(_) | Ref(_) | Symbol(_)
                    | TypeVal(_) => 1,
                    Annot { expr, .. } => 1 + count(expr),
                    Lambda { body, .. } => 1 + count(body),
                    Apply { func, arg } => 1 + count(func) + count(arg),
                    Block(inner) => 1 + count(inner),
                    List(xs) => 1 + xs.iter().map(count).sum::<usize>(),
                    Record(fs) => 1 + fs.iter().map(|(_, v)| count(v)).sum::<usize>(),
                    LetGroup { bindings, body, .. } => {
                        1 + count(body) + bindings.iter().map(|(_, ex)| count(ex)).sum::<usize>()
                    }
                    Raise(inner) => 1 + count(inner),
                    OrElse { left, right } | AltLambda { left, right } | Catch { left, right } => {
                        1 + count(left) + count(right)
                    }
                }
            }
            count(&ast)
        };
        if opt.analyze_trace {
            eprintln!("trace: ast-nodes {}", ast_nodes);
        }
        // Core IR dump/eval modes take precedence over analyze/execute
        if opt.dump_coreir || opt.dump_coreir_json || opt.eval_coreir {
            let term = lower_expr_to_core(&ast);
            if opt.dump_coreir_json {
                println!("{}", serde_json::to_string_pretty(&term)?);
            } else if opt.dump_coreir {
                println!("{}", print_term(&term));
            } else if opt.eval_coreir {
                match eval_term(&term) {
                    Ok(v) => println!("{}", print_ir_value(&v)),
                    Err(e) => {
                        eprintln!("coreir eval error: {}", e);
                        std::process::exit(2);
                    }
                }
            }
            return Ok(());
        }
        if opt.analyze {
            #[derive(Serialize)]
            struct AnalyzeOut<'a> {
                duplicates: &'a [lzscr_analyzer::DupFinding],
                unbound_refs: &'a [lzscr_analyzer::UnboundRef],
                shadowing: &'a [lzscr_analyzer::Shadowing],
                unused_params: &'a [lzscr_analyzer::UnusedParam],
                unused_let: &'a [lzscr_analyzer::UnusedLet],
                let_collisions: &'a [lzscr_analyzer::LetCollision],
                ctor_arity: Vec<lzscr_analyzer::CtorArityIssue>,
            }
            // duplicates (optionally skipped)
            let t_dup_start = Instant::now();
            let dups = if opt.no_dup {
                Vec::new()
            } else {
                analyze_duplicates(
                    &ast,
                    AnalyzeOptions { min_size: opt.dup_min_size, min_count: opt.dup_min_count },
                )
            };
            if opt.analyze_trace {
                eprintln!(
                    "trace: duplicates {} ms ({} findings)",
                    t_dup_start.elapsed().as_millis(),
                    dups.len()
                );
            }
            // unbound refs
            let t_unb_start = Instant::now();
            let unb = analyze_unbound_refs(&ast, &default_allowlist());
            if opt.analyze_trace {
                eprintln!(
                    "trace: unbound-refs {} ms ({} findings)",
                    t_unb_start.elapsed().as_millis(),
                    unb.len()
                );
            }
            // shadowing
            let t_sh_start = Instant::now();
            let sh = analyze_shadowing(&ast);
            if opt.analyze_trace {
                eprintln!(
                    "trace: shadowing {} ms ({} findings)",
                    t_sh_start.elapsed().as_millis(),
                    sh.len()
                );
            }
            // unused params
            let t_up_start = Instant::now();
            let up = analyze_unused_params(&ast);
            if opt.analyze_trace {
                eprintln!(
                    "trace: unused-params {} ms ({} findings)",
                    t_up_start.elapsed().as_millis(),
                    up.len()
                );
            }
            // unused let bindings
            let t_ul_start = Instant::now();
            let ul = lzscr_analyzer::analyze_unused_let_bindings(&ast);
            if opt.analyze_trace {
                eprintln!(
                    "trace: unused-let {} ms ({} findings)",
                    t_ul_start.elapsed().as_millis(),
                    ul.len()
                );
            }
            // let binding name collisions
            let t_lc_start = Instant::now();
            let lc = lzscr_analyzer::analyze_let_collisions(&ast);
            if opt.analyze_trace {
                eprintln!(
                    "trace: let-collisions {} ms ({} findings)",
                    t_lc_start.elapsed().as_millis(),
                    lc.len()
                );
            }
            let arities = {
                let mut m = HashMap::new();
                if let Some(spec) = &opt.ctor_arity {
                    let (parsed, warns) = parse_ctor_arity_spec(spec);
                    for w in warns {
                        eprintln!("warning: {}", w);
                    }
                    m = parsed;
                }
                m
            };
            let t_ca_start = Instant::now();
            let ca = analyze_ctor_arity(&ast, &arities);
            if opt.analyze_trace {
                eprintln!(
                    "trace: ctor-arity {} ms ({} findings)",
                    t_ca_start.elapsed().as_millis(),
                    ca.len()
                );
            }
            if opt.format == "json" {
                let out = AnalyzeOut {
                    duplicates: &dups,
                    unbound_refs: &unb,
                    shadowing: &sh,
                    unused_params: &up,
                    unused_let: &ul,
                    let_collisions: &lc,
                    ctor_arity: ca,
                };
                println!("{}", serde_json::to_string_pretty(&out)?);
            } else {
                for f in &dups {
                    eprintln!(
                        "duplicate: size={} count={} span=({},{}) repr={}",
                        f.size, f.count, f.span.offset, f.span.len, f.repr
                    );
                    let block = src_reg.format_span_block(f.span.offset, f.span.len);
                    eprintln!("{}", block);
                }
                for u in &unb {
                    eprintln!(
                        "unbound-ref: name={} span=({},{})",
                        u.name, u.span.offset, u.span.len
                    );
                    let block = src_reg.format_span_block(u.span.offset, u.span.len);
                    eprintln!("{}", block);
                }
                for s in &sh {
                    eprintln!(
                        "shadowing: name={} lambda_span=({},{})",
                        s.name, s.lambda_span.offset, s.lambda_span.len
                    );
                    let block = src_reg.format_span_block(s.lambda_span.offset, s.lambda_span.len);
                    eprintln!("{}", block);
                }
                for u in &up {
                    eprintln!(
                        "unused-param: name={} lambda_span=({},{})",
                        u.name, u.lambda_span.offset, u.lambda_span.len
                    );
                    let block = src_reg.format_span_block(u.lambda_span.offset, u.lambda_span.len);
                    eprintln!("{}", block);
                }
                for u in &ul {
                    eprintln!(
                        "unused-let: name={} binding_span=({},{})",
                        u.name, u.binding_span.offset, u.binding_span.len
                    );
                    let block =
                        src_reg.format_span_block(u.binding_span.offset, u.binding_span.len);
                    eprintln!("{}", block);
                }
                for c in &lc {
                    eprintln!(
                        "let-collision: name={} group_span=({},{})",
                        c.name, c.group_span.offset, c.group_span.len
                    );
                    let block = src_reg.format_span_block(c.group_span.offset, c.group_span.len);
                    eprintln!("{}", block);
                }
                for c in &ca {
                    eprintln!(
                        "ctor-arity: name={} expected={} got={} span=({}, {}) kind={}",
                        c.name, c.expected, c.got, c.span.offset, c.span.len, c.kind
                    );
                    let block = src_reg.format_span_block(c.span.offset, c.span.len);
                    eprintln!("{}", block);
                }
            }
            return Ok(());
        }
        // Optional typechecking phase
        let mut inferred_type_pretty: Option<String> = None; // for --types pretty: print on same line with value later
        if !opt.no_typecheck {
            if opt.type_debug > 0 {
                // Parse flags
                let mut log_unify = false;
                let mut log_env = false;
                let mut log_schemes = false;
                for f in opt.type_debug_flags.split(',').map(|s| s.trim()).filter(|s| !s.is_empty())
                {
                    match f {
                        "unify" => log_unify = true,
                        "env" => log_env = true,
                        "schemes" => log_schemes = true,
                        "all" => {
                            log_unify = true;
                            log_env = true;
                            log_schemes = true;
                        }
                        _ => {}
                    }
                }
                match lzscr_types::api::infer_ast_debug_with(
                    &ast,
                    opt.type_debug,
                    opt.type_debug_depth,
                    log_unify,
                    log_env,
                    log_schemes,
                ) {
                    Ok((t, logs)) => {
                        if opt.types == "json" {
                            #[derive(Serialize)]
                            struct TypeOut {
                                ty: String,
                            }
                            println!(
                                "{}",
                                serde_json::to_string_pretty(&TypeOut { ty: t.clone() })?
                            );
                        } else if opt.types == "pretty" {
                            // Defer printing to combine with value on one line
                            inferred_type_pretty = Some(t.clone());
                        } else if opt.types == "legacy" {
                            // 再推論 (pretty=false) — オーバーヘッドは小さいので許容
                            match lzscr_types::api::infer_ast_with_opts(
                                &ast,
                                lzscr_types::api::InferOptions { pretty: false },
                            ) {
                                Ok(raw) => println!("{}", raw),
                                Err(e) => {
                                    eprintln!("type error: {}", e);
                                    std::process::exit(2);
                                }
                            }
                        }
                        for line in logs {
                            eprintln!("[type-debug] {}", line);
                        }
                    }
                    Err(e) => {
                        use lzscr_types::TypeError;
                        match e {
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
                            | TypeError::Occurs {
                                var_span_offset: expected_span_offset,
                                var_span_len: expected_span_len,
                                ty_span_offset: actual_span_offset,
                                ty_span_len: actual_span_len,
                                ..
                            }
                            | TypeError::AltLambdaArityMismatch {
                                expected_span_offset,
                                expected_span_len,
                                actual_span_offset,
                                actual_span_len,
                                ..
                            } => {
                                eprintln!("type error: {}", e);
                                let (eo, el) = (expected_span_offset, expected_span_len);
                                let (ao, al) = (actual_span_offset, actual_span_len);
                                let b1 = src_reg.format_span_block(eo, el);
                                let b2 = src_reg.format_span_block(ao, al);
                                eprintln!(
                                    "type variable defined here:\n{}\noccurs inside here:\n{}",
                                    b1, b2
                                );
                            }
                            TypeError::UnboundRef {
                                span_offset,
                                span_len,
                                ref suggestions,
                                ..
                            } => {
                                eprintln!("type error: {}", e);
                                let (adj_off, adj_len) = if span_len == 1 {
                                    if let Some((op, _len)) = src_reg
                                        .last_top_level_brace_block_in_same_source(span_offset)
                                    {
                                        (src_reg.first_non_comment_offset_from(op), 1)
                                    } else {
                                        (src_reg.first_non_comment_offset_from(span_offset), 1)
                                    }
                                } else {
                                    (span_offset, span_len)
                                };
                                let block = src_reg.format_span_block(adj_off, adj_len);
                                eprintln!("{}", block);
                                if !suggestions.is_empty() {
                                    eprintln!("  hint: did you mean one of these?");
                                    for suggestion in suggestions {
                                        eprintln!("    - {}", suggestion);
                                    }
                                }
                            }
                            TypeError::EffectNotAllowed { span_offset, span_len } => {
                                eprintln!("type error: {}", e);
                                let (adj_off, adj_len) = if span_len == 1 {
                                    if let Some((op, _len)) = src_reg
                                        .last_top_level_brace_block_in_same_source(span_offset)
                                    {
                                        (src_reg.first_non_comment_offset_from(op), 1)
                                    } else {
                                        (src_reg.first_non_comment_offset_from(span_offset), 1)
                                    }
                                } else {
                                    (span_offset, span_len)
                                };
                                let block = src_reg.format_span_block(adj_off, adj_len);
                                eprintln!("{}", block);
                                eprintln!("  hint: effects require explicit sequencing");
                                eprintln!("    Use ~seq or ~chain to enable effects:");
                                eprintln!("      (~seq () (!effect-call ...))");
                                eprintln!("    or");
                                eprintln!("      (~chain (!effect-call ...) (\\~result -> ...))");
                            }
                            TypeError::MixedAltBranches { span_offset, span_len } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                    eprintln!("  hint: AltLambda requires consistent pattern style");
                                    eprintln!("    Either use:");
                                    eprintln!("      - Constructor patterns: (\\SomeTag ... | OtherTag ... -> ...)");
                                    eprintln!("    or");
                                    eprintln!("      - Wildcard/default only: (\\~x -> ... | \\_ -> ...)");
                                    eprintln!("    Cannot mix both styles in one AltLambda.");
                                }
                                TypeError::Mismatch { span_offset, span_len, .. }
                            | TypeError::RecordFieldMismatch { span_offset, span_len, .. } => {
                                eprintln!("type error: {}", e);
                                let (adj_off, adj_len) = if span_len == 1 {
                                    if let Some((op, _len)) = src_reg
                                        .last_top_level_brace_block_in_same_source(span_offset)
                                    {
                                        (src_reg.first_non_comment_offset_from(op), 1)
                                    } else {
                                        (src_reg.first_non_comment_offset_from(span_offset), 1)
                                    }
                                } else {
                                    (span_offset, span_len)
                                };
                                let block = src_reg.format_span_block(adj_off, adj_len);
                                eprintln!("{}", block);
                                eprintln!("  hint: type mismatch may indicate ambiguous inference");
                                eprintln!("    Consider adding an explicit type annotation:");
                                eprintln!("      (%{{expected-type}} expression)");
                                eprintln!("    or adding type hints to function parameters/let bindings.");
                            }
                            TypeError::NegativeOccurrence { span_offset, span_len, .. }
                            | TypeError::InvalidTypeDecl { span_offset, span_len, .. }
                            | TypeError::DuplicateCtorTag { span_offset, span_len, .. } => {
                                eprintln!("type error: {}", e);
                                // Heuristic: if span len is 1 (generic), nudge caret to the first
                                // non-comment token at/after that offset for better UX.
                                let (adj_off, adj_len) = if span_len == 1 {
                                    // Prefer the last top-level { ... } block (e.g., module export record)
                                    if let Some((op, _len)) = src_reg
                                        .last_top_level_brace_block_in_same_source(span_offset)
                                    {
                                        (src_reg.first_non_comment_offset_from(op), 1)
                                    } else {
                                        (src_reg.first_non_comment_offset_from(span_offset), 1)
                                    }
                                } else {
                                    (span_offset, span_len)
                                };
                                let block = src_reg.format_span_block(adj_off, adj_len);
                                eprintln!("{}", block);
                            }
                            other => {
                                eprintln!("type error: {}", other);
                            }
                        }
                        std::process::exit(2);
                    }
                }
            } else {
                // 非デバッグ: 選択された表示モードで推論
                if opt.types == "legacy" {
                    match lzscr_types::api::infer_ast_with_opts(
                        &ast,
                        lzscr_types::api::InferOptions { pretty: false },
                    ) {
                        Ok(t) => println!("{}", t),
                        Err(e) => {
                            use lzscr_types::TypeError;
                            match e {
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
                                | TypeError::Occurs {
                                    var_span_offset: expected_span_offset,
                                    var_span_len: expected_span_len,
                                    ty_span_offset: actual_span_offset,
                                    ty_span_len: actual_span_len,
                                    ..
                                }
                                | TypeError::AltLambdaArityMismatch {
                                    expected_span_offset,
                                    expected_span_len,
                                    actual_span_offset,
                                    actual_span_len,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let (eo, el) = (expected_span_offset, expected_span_len);
                                    let (ao, al) = (actual_span_offset, actual_span_len);
                                    let b1 = src_reg.format_span_block(eo, el);
                                    let b2 = src_reg.format_span_block(ao, al);
                                    eprintln!(
                                        "type variable defined here:\n{}\noccurs inside here:\n{}",
                                        b1, b2
                                    );
                                }
                                TypeError::UnboundRef {
                                    span_offset,
                                    span_len,
                                    ref suggestions,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                    if !suggestions.is_empty() {
                                        eprintln!("  hint: did you mean one of these?");
                                        for suggestion in suggestions {
                                            eprintln!("    - {}", suggestion);
                                        }
                                    }
                                }
                                TypeError::EffectNotAllowed { span_offset, span_len } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                    eprintln!("  hint: effects require explicit sequencing");
                                    eprintln!("    Use ~seq or ~chain to enable effects:");
                                    eprintln!("      (~seq () (!effect-call ...))");
                                    eprintln!("    or");
                                    eprintln!("      (~chain (!effect-call ...) (\\\\~result -> ...))");
                                }                                | TypeError::NegativeOccurrence { span_offset, span_len, .. }
                                | TypeError::InvalidTypeDecl { span_offset, span_len, .. }
                                | TypeError::DuplicateCtorTag { span_offset, span_len, .. } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                }
                                TypeError::AnnotMismatch {
                                    annot_span_offset,
                                    annot_span_len,
                                    expr_span_offset,
                                    expr_span_len,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let b1 = src_reg
                                        .format_span_block(annot_span_offset, annot_span_len);
                                    let b2 =
                                        src_reg.format_span_block(expr_span_offset, expr_span_len);
                                    eprintln!("{}\n{}", b1, b2);
                                }
                                other => {
                                    eprintln!("type error: {}", other);
                                }
                            }
                            std::process::exit(2);
                        }
                    }
                } else {
                    match lzscr_types::api::infer_ast(&ast) {
                        Ok(t) => {
                            if opt.types == "json" {
                                #[derive(Serialize)]
                                struct TypeOut {
                                    ty: String,
                                }
                                println!(
                                    "{}",
                                    serde_json::to_string_pretty(&TypeOut { ty: t.clone() })?
                                );
                            } else if opt.types == "pretty" {
                                // Defer printing to combine with value on one line
                                inferred_type_pretty = Some(t.clone());
                            }
                        }
                        Err(e) => {
                            use lzscr_types::TypeError;
                            match e {
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
                                | TypeError::Occurs {
                                    var_span_offset: expected_span_offset,
                                    var_span_len: expected_span_len,
                                    ty_span_offset: actual_span_offset,
                                    ty_span_len: actual_span_len,
                                    ..
                                }
                                | TypeError::AltLambdaArityMismatch {
                                    expected_span_offset,
                                    expected_span_len,
                                    actual_span_offset,
                                    actual_span_len,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let (eo, el) = (expected_span_offset, expected_span_len);
                                    let (ao, al) = (actual_span_offset, actual_span_len);
                                    let b1 = src_reg.format_span_block(eo, el);
                                    let b2 = src_reg.format_span_block(ao, al);
                                    eprintln!(
                                        "type variable defined here:\n{}\noccurs inside here:\n{}",
                                        b1, b2
                                    );
                                }
                                TypeError::UnboundRef {
                                    span_offset,
                                    span_len,
                                    ref suggestions,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                    if !suggestions.is_empty() {
                                        eprintln!("  hint: did you mean one of these?");
                                        for suggestion in suggestions {
                                            eprintln!("    - {}", suggestion);
                                        }
                                    }
                                }
                                TypeError::EffectNotAllowed { span_offset, span_len } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                    eprintln!("  hint: effects require explicit sequencing");
                                    eprintln!("    Use ~seq or ~chain to enable effects:");
                                    eprintln!("      (~seq () (!effect-call ...))");
                                    eprintln!("    or");
                                    eprintln!("      (~chain (!effect-call ...) (\\\\~result -> ...))");
                                }                                | TypeError::NegativeOccurrence { span_offset, span_len, .. }
                                | TypeError::InvalidTypeDecl { span_offset, span_len, .. }
                                | TypeError::DuplicateCtorTag { span_offset, span_len, .. } => {
                                    eprintln!("type error: {}", e);
                                    let (adj_off, adj_len) = if span_len == 1 {
                                        if let Some((op, _len)) = src_reg
                                            .last_top_level_brace_block_in_same_source(span_offset)
                                        {
                                            (src_reg.first_non_comment_offset_from(op), 1)
                                        } else {
                                            (src_reg.first_non_comment_offset_from(span_offset), 1)
                                        }
                                    } else {
                                        (span_offset, span_len)
                                    };
                                    let block = src_reg.format_span_block(adj_off, adj_len);
                                    eprintln!("{}", block);
                                }
                                TypeError::AnnotMismatch {
                                    annot_span_offset,
                                    annot_span_len,
                                    expr_span_offset,
                                    expr_span_len,
                                    ..
                                } => {
                                    eprintln!("type error: {}", e);
                                    let b1 = src_reg
                                        .format_span_block(annot_span_offset, annot_span_len);
                                    let b2 =
                                        src_reg.format_span_block(expr_span_offset, expr_span_len);
                                    eprintln!("{}\n{}", b1, b2);
                                }
                                other => {
                                    eprintln!("type error: {}", other);
                                }
                            }
                            std::process::exit(2);
                        }
                    }
                    // end non-debug type inference output handling
                } // end else (non-debug type inference branch)
            } // end if opt.type_debug > 0
        } // end if !opt.no_typecheck
        let mut env = Env::with_builtins();
        if let Some(spec) = &opt.ctor_arity {
            let (parsed, warns) = parse_ctor_arity_spec(spec);
            for w in warns {
                eprintln!("warning: {}", w);
            }
            for (name, k) in parsed {
                env.declare_ctor_arity(name.trim(), k);
            }
        }
        if opt.strict_effects {
            env.strict_effects = true;
        }
        let val = match eval(&env, &ast) {
            Ok(v) => v,
            Err(e) => {
                // Pretty-print traced errors if available
                match e {
                    lzscr_runtime::EvalError::Traced { kind, spans } => {
                        eprintln!("runtime error: {kind}");
                        // Use source registry so spans from required modules show correct filenames
                        for (idx, sp) in spans.iter().enumerate() {
                            let block = src_reg.format_span_block(sp.offset, sp.len);
                            eprintln!("  {}", block.replace('\n', "\n  "));
                            if idx + 1 == spans.len() {
                                eprintln!("  (most recent call last)");
                            }
                        }
                        std::process::exit(2);
                    }
                    other => {
                        eprintln!("runtime error: {}", other);
                        std::process::exit(2);
                    }
                }
            }
        };
        fn val_to_string(env: &Env, v: &Value) -> String {
            fn char_literal_string(c: i32) -> String {
                let ch = char::from_u32(c as u32).unwrap_or('\u{FFFD}');
                let mut tmp = String::new();
                tmp.push(ch);
                format!("'{}'", tmp.escape_default())
            }
            match v {
                Value::Unit => "()".into(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Str(s) => s.to_string(),
                Value::Char(c) => char_literal_string(*c),
                Value::Symbol(id) => env.symbol_name(*id),
                Value::Raised(b) => format!("^({})", val_to_string(env, b)),
                Value::Thunk { .. } => "<thunk>".into(),
                Value::Ctor { name, args } => {
                    if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                        format!(
                            "({})",
                            args.iter()
                                .map(|x| val_to_string(env, x))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    } else if args.is_empty() {
                        name.clone()
                    } else {
                        format!(
                            "{}({})",
                            name,
                            args.iter()
                                .map(|x| val_to_string(env, x))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                }
                Value::List(xs) => format!(
                    "[{}]",
                    xs.iter().map(|x| val_to_string(env, x)).collect::<Vec<_>>().join(", ")
                ),
                Value::Tuple(xs) => format!(
                    "({})",
                    xs.iter().map(|x| val_to_string(env, x)).collect::<Vec<_>>().join(", ")
                ),
                Value::Record(map) => {
                    let inner = map
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, val_to_string(env, v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{{{}}}", inner)
                }
                Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
            }
        }
        let out = val_to_string(&env, &val);
        if opt.types == "pretty" {
            if let Some(tp) = inferred_type_pretty {
                println!("{} {}", tp, out);
            } else {
                println!("{out}");
            }
        } else {
            println!("{out}");
        }
        Ok(())
    }
} // end of main()

// ---------- ~require expansion ----------

use std::path::Path;
fn build_module_search_paths(stdlib_dir: &Path, module_path: Option<&str>) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    // 1) current directory
    paths.push(PathBuf::from("."));
    // 2) stdlib dir (may not exist)
    paths.push(stdlib_dir.to_path_buf());
    // 3) user-provided module-path (colon-separated)
    if let Some(spec) = module_path {
        for seg in spec.split(':') {
            if seg.is_empty() {
                continue;
            }
            paths.push(PathBuf::from(seg));
        }
    }
    paths
}

fn expand_requires_in_expr(
    e: &Expr,
    search_paths: &[PathBuf],
    stack: &mut Vec<String>,
    src_reg: &mut SourceRegistry,
    stdlib_mode: StdlibMode,
) -> Result<Expr, String> {
    // First, handle ~builtin path: expand to a Ref of a generated alias name.
    if let Some(alias) = match_builtin_call(e) {
        return Ok(Expr { kind: ExprKind::Ref(alias), span: e.span });
    }
    // Next, handle ~require expansion
    match match_require_call(e) {
        Some(Ok(segs)) => {
            if !stdlib_mode.allows_effects() && is_effect_namespace(&segs) {
                let dotted = segs.iter().map(|s| format!(".{}", s)).collect::<Vec<_>>().join(" ");
                return Err(format!(
                    "importing stdlib effect modules via `(~require {})` requires --stdlib-mode=allow-effects",
                    dotted
                ));
            }
            let rel = segs.join("/") + ".lzscr";
            let (path, content) = resolve_module_content(&rel, search_paths)?;
            let canon = match std::fs::canonicalize(&path) {
                Ok(p) => p.display().to_string(),
                Err(_) => path.display().to_string(),
            };
            if stack.contains(&canon) {
                return Err(format!(
                    "cyclic require detected: {} -> ... -> {}",
                    stack.first().cloned().unwrap_or_default(),
                    canon
                ));
            }
            stack.push(canon);
            // Wrap like file rule: ( <content> )
            let wrapped = format!("({})", content);
            let parsed = match parse_expr(&wrapped) {
                Ok(x) => x,
                Err(e) => {
                    use lzscr_parser::ParseError;
                    return Err(match e {
                        ParseError::WithSpan { msg, span_offset, span_len } => {
                            let // adjust for leading '('
                        adj_off = span_offset.saturating_sub(1);
                            let block = format_span_caret(&content, &rel, adj_off, span_len);
                            format!("parse error in required module '{}': {}\n{}", rel, msg, block)
                        }
                        other => format!("parse error in required module '{}': {}", rel, other),
                    });
                }
            };
            // Register module source and rebase spans so they map to this module's filename
            let base = src_reg.register_module(rel.clone(), content);
            let parsed_rebased = rebase_expr_spans_with_minus(&parsed, base, 1);
            let expanded = expand_requires_in_expr(
                &parsed_rebased,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?;
            stack.pop();
            return Ok(expanded);
        }
        Some(Err(msg)) => {
            return Err(msg);
        }
        _ => {}
    }
    // Otherwise, recurse children
    use ExprKind::*;
    let k = match &e.kind {
        Unit | Int(_) | Float(_) | Str(_) | Char(_) | Ref(_) | Symbol(_) | TypeVal(_) => {
            e.kind.clone()
        }
        Annot { ty, expr } => Annot {
            ty: ty.clone(),
            expr: Box::new(expand_requires_in_expr(
                expr,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
        },
        Lambda { param, body } => Lambda {
            param: param.clone(),
            body: Box::new(expand_requires_in_expr(
                body,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
        },
        Apply { func, arg } => Apply {
            func: Box::new(expand_requires_in_expr(
                func,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
            arg: Box::new(expand_requires_in_expr(arg, search_paths, stack, src_reg, stdlib_mode)?),
        },
        Block(inner) => Block(Box::new(expand_requires_in_expr(
            inner,
            search_paths,
            stack,
            src_reg,
            stdlib_mode,
        )?)),
        List(xs) => List(
            xs.iter()
                .map(|x| expand_requires_in_expr(x, search_paths, stack, src_reg, stdlib_mode))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Record(fs) => Record(
            fs.iter()
                .map(|(k, v)| {
                    Ok((
                        k.clone(),
                        expand_requires_in_expr(v, search_paths, stack, src_reg, stdlib_mode)?,
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?,
        ),
        LetGroup { bindings, body, .. } => {
            let mut new_bs = Vec::with_capacity(bindings.len());
            for (p, ex) in bindings.iter() {
                new_bs.push((
                    p.clone(),
                    expand_requires_in_expr(ex, search_paths, stack, src_reg, stdlib_mode)?,
                ));
            }
            // type_decls are excluded from require-expansion; keep them as-is
            if let LetGroup { type_decls, .. } = &e.kind {
                LetGroup {
                    type_decls: type_decls.clone(),
                    bindings: new_bs,
                    body: Box::new(expand_requires_in_expr(
                        body,
                        search_paths,
                        stack,
                        src_reg,
                        stdlib_mode,
                    )?),
                }
            } else {
                unreachable!()
            }
        }
        Raise(inner) => Raise(Box::new(expand_requires_in_expr(
            inner,
            search_paths,
            stack,
            src_reg,
            stdlib_mode,
        )?)),
        AltLambda { left, right } => AltLambda {
            left: Box::new(expand_requires_in_expr(
                left,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
            right: Box::new(expand_requires_in_expr(
                right,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
        },
        OrElse { left, right } => OrElse {
            left: Box::new(expand_requires_in_expr(
                left,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
            right: Box::new(expand_requires_in_expr(
                right,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
        },
        Catch { left, right } => Catch {
            left: Box::new(expand_requires_in_expr(
                left,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
            right: Box::new(expand_requires_in_expr(
                right,
                search_paths,
                stack,
                src_reg,
                stdlib_mode,
            )?),
        },
    };
    Ok(Expr { kind: k, span: e.span })
}

fn rebase_expr_spans_with_minus(e: &Expr, add: usize, minus: usize) -> Expr {
    use ExprKind::*;
    let map_expr = |x: &Expr| rebase_expr_spans_with_minus(x, add, minus);
    let map_box = |x: &Expr| Box::new(map_expr(x));
    let map_list = |xs: &Vec<Expr>| xs.iter().map(map_expr).collect::<Vec<_>>();
    let kind = match &e.kind {
        Unit => Unit,
        Int(n) => Int(*n),
        Float(f) => Float(*f),
        Str(s) => Str(s.clone()),
        Char(c) => Char(*c),
        Ref(n) => Ref(n.clone()),
        Symbol(s) => Symbol(s.clone()),
        Annot { ty, expr } => Annot { ty: ty.clone(), expr: map_box(expr) },
        TypeVal(t) => TypeVal(t.clone()),
        Lambda { param, body } => {
            Lambda { param: rebase_pattern_with_minus(param, add, minus), body: map_box(body) }
        }
        Apply { func, arg } => Apply { func: map_box(func), arg: map_box(arg) },
        Block(b) => Block(map_box(b)),
        List(xs) => List(map_list(xs)),
        Record(fields) => {
            let mut new = Vec::with_capacity(fields.len());
            for (k, v) in fields.iter() {
                new.push((k.clone(), map_expr(v)));
            }
            Record(new)
        }
        LetGroup { bindings, body, .. } => {
            let mut new_bs = Vec::with_capacity(bindings.len());
            for (p, ex) in bindings.iter() {
                new_bs.push((rebase_pattern_with_minus(p, add, minus), map_expr(ex)));
            }
            if let LetGroup { type_decls, .. } = &e.kind {
                LetGroup { type_decls: type_decls.clone(), bindings: new_bs, body: map_box(body) }
            } else {
                unreachable!()
            }
        }
        Raise(inner) => Raise(map_box(inner)),
        AltLambda { left, right } => AltLambda { left: map_box(left), right: map_box(right) },
        OrElse { left, right } => OrElse { left: map_box(left), right: map_box(right) },
        Catch { left, right } => Catch { left: map_box(left), right: map_box(right) },
    };
    let new_span = lzscr_ast::span::Span {
        offset: add + e.span.offset.saturating_sub(minus),
        len: e.span.len,
    };
    Expr { kind, span: new_span }
}

fn rebase_pattern_with_minus(p: &Pattern, add: usize, minus: usize) -> Pattern {
    let kind = match &p.kind {
        PatternKind::Wildcard => PatternKind::Wildcard,
        PatternKind::Var(s) => PatternKind::Var(s.clone()),
        PatternKind::Unit => PatternKind::Unit,
        PatternKind::Tuple(xs) => PatternKind::Tuple(
            xs.iter().map(|x| rebase_pattern_with_minus(x, add, minus)).collect(),
        ),
        PatternKind::Ctor { name, args } => PatternKind::Ctor {
            name: name.clone(),
            args: args.iter().map(|x| rebase_pattern_with_minus(x, add, minus)).collect(),
        },
        PatternKind::Symbol(s) => PatternKind::Symbol(s.clone()),
        PatternKind::Int(n) => PatternKind::Int(*n),
        PatternKind::Float(f) => PatternKind::Float(*f),
        PatternKind::Str(s) => PatternKind::Str(s.clone()),
        PatternKind::Char(c) => PatternKind::Char(*c),
        PatternKind::Record(fields) => {
            let mut new = Vec::with_capacity(fields.len());
            for (k, v) in fields.iter() {
                new.push((k.clone(), rebase_pattern_with_minus(v, add, minus)));
            }
            PatternKind::Record(new)
        }
        PatternKind::As(a, b) => PatternKind::As(
            Box::new(rebase_pattern_with_minus(a, add, minus)),
            Box::new(rebase_pattern_with_minus(b, add, minus)),
        ),
        PatternKind::List(xs) => {
            PatternKind::List(xs.iter().map(|x| rebase_pattern_with_minus(x, add, minus)).collect())
        }
        PatternKind::Cons(h, t) => PatternKind::Cons(
            Box::new(rebase_pattern_with_minus(h, add, minus)),
            Box::new(rebase_pattern_with_minus(t, add, minus)),
        ),
        PatternKind::TypeBind { tvars, pat } => PatternKind::TypeBind {
            tvars: tvars.clone(),
            pat: Box::new(rebase_pattern_with_minus(pat, add, minus)),
        },
    };
    let new_span = lzscr_ast::span::Span {
        offset: add + p.span.offset.saturating_sub(minus),
        len: p.span.len,
    };
    Pattern { kind, span: new_span }
}

fn match_require_call(e: &Expr) -> Option<Result<Vec<String>, String>> {
    // Recognize application chain ((~require .a) .b) .c
    fn collect_apply<'a>(mut cur: &'a Expr, out_args: &mut Vec<&'a Expr>) -> &'a Expr {
        loop {
            match &cur.kind {
                ExprKind::Apply { func, arg } => {
                    out_args.push(arg);
                    cur = func;
                }
                _ => break cur,
            }
        }
    }
    let mut args = Vec::new();
    let callee = collect_apply(e, &mut args);
    match &callee.kind {
        ExprKind::Ref(name) if name == "require" => {
            if args.is_empty() {
                return Some(Err("~require expects at least one .segment".into()));
            }
            let mut segs = Vec::with_capacity(args.len());
            for a in args.into_iter().rev() {
                match &a.kind {
                    ExprKind::Symbol(s) if s.starts_with('.') && s.len() > 1 => {
                        segs.push(s[1..].to_string());
                    }
                    other => {
                        return Some(Err(format!(
                            "~require expects only ctor-dot symbols (.name); got {}",
                            node_kind_name(other)
                        )));
                    }
                }
            }
            Some(Ok(segs))
        }
        _ => None,
    }
}

fn is_effect_namespace(segs: &[String]) -> bool {
    if segs.is_empty() {
        return false;
    }
    if segs[0] == "effect" || segs[0] == "compat" {
        return true;
    }
    segs.len() >= 2 && segs[0] == "stdlib" && (segs[1] == "effect" || segs[1] == "compat")
}

fn match_builtin_call(e: &Expr) -> Option<String> {
    // Recognize (~builtin .a .b .c) and turn into Ref("builtin_a_b_c")
    fn collect_apply<'a>(mut cur: &'a Expr, out_args: &mut Vec<&'a Expr>) -> &'a Expr {
        loop {
            match &cur.kind {
                ExprKind::Apply { func, arg } => {
                    out_args.push(arg);
                    cur = func;
                }
                _ => break cur,
            }
        }
    }
    let mut args = Vec::new();
    let callee = collect_apply(e, &mut args);
    match &callee.kind {
        ExprKind::Ref(name) if name == "builtin" => {
            if args.is_empty() {
                return Some("builtin".to_string());
            }
            let mut segs = Vec::with_capacity(args.len());
            for a in args.into_iter().rev() {
                match &a.kind {
                    ExprKind::Symbol(s) if s.starts_with('.') && s.len() > 1 => {
                        segs.push(s[1..].to_string());
                    }
                    _ => return None,
                }
            }
            let alias = format!("builtin_{}", segs.join("_"));
            Some(alias)
        }
        _ => None,
    }
}

fn node_kind_name(k: &ExprKind) -> &'static str {
    match k {
        ExprKind::Unit => "Unit",
        ExprKind::Int(_) => "Int",
        ExprKind::Float(_) => "Float",
        ExprKind::Str(_) => "Str",
        ExprKind::Char(_) => "Char",
        ExprKind::Ref(_) => "Ref",
        ExprKind::Symbol(_) => "Symbol",
        ExprKind::Annot { .. } => "Annot",
        ExprKind::TypeVal(_) => "TypeVal",
        ExprKind::Lambda { .. } => "Lambda",
        ExprKind::Apply { .. } => "Apply",
        ExprKind::Block(_) => "Block",
        ExprKind::List(_) => "List",
        ExprKind::Record(_) => "Record",
        ExprKind::LetGroup { .. } => "LetGroup",
        ExprKind::Raise(_) => "Raise",
        ExprKind::AltLambda { .. } => "AltLambda",
        ExprKind::OrElse { .. } => "OrElse",
        ExprKind::Catch { .. } => "Catch",
    }
}

fn resolve_module_content(
    rel_path: &str,
    search_paths: &[PathBuf],
) -> Result<(PathBuf, String), String> {
    for root in search_paths {
        let p = root.join(rel_path);
        if let Ok(s) = fs::read_to_string(&p) {
            return Ok((p, s));
        }
    }
    Err(format!(
        "module not found: {} (searched in: {})",
        rel_path,
        search_paths.iter().map(|p| p.display().to_string()).collect::<Vec<_>>().join(":")
    ))
}

// EOF
