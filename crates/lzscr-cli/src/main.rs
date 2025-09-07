use clap::Parser;
use lzscr_analyzer::{
    analyze_ctor_arity, analyze_duplicates, analyze_shadowing, analyze_unbound_refs,
    analyze_unused_params, default_allowlist, AnalyzeOptions,
};
use lzscr_coreir::{lower_expr_to_core, print_term, eval_term, print_ir_value};
use lzscr_parser::parse_expr;
use lzscr_ast::ast::*;
use lzscr_runtime::{eval, Env, Value};
use serde::Serialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;
use std::time::Instant;

fn parse_ctor_arity_spec(spec: &str) -> (HashMap<String, usize>, Vec<String>) {
    let mut map = HashMap::new();
    let mut warnings = Vec::new();
    for raw in spec.split(',') {
        let item = raw.trim();
        if item.is_empty() {
            continue;
        }
        let Some((name_raw, n_raw)) = item.split_once('=') else {
            warnings.push(format!(
                "ignored ctor-arity entry (missing '='): '{}'",
                item
            ));
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
            Err(_) => warnings.push(format!(
                "ignored ctor-arity entry (invalid number '{}'): '{}'",
                n_str, item
            )),
        }
    }
    (map, warnings)
}

#[derive(Parser, Debug)]
#[command(
    name = "lzscr",
    version,
    about = "LazyScript reimplementation (skeleton)"
)]
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

    /// Type output mode: pretty|json (when typechecking runs)
    #[arg(long = "types", default_value = "pretty")]
    types: String,

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
    let (mut code, _from_file) = if let Some(c) = opt.eval {
        (c, false)
    } else if let Some(ref p) = opt.file {
        let raw = fs::read_to_string(p)?;
        if opt.format_code {
            // For formatting, keep raw; formatter has its own file-aware handling
            (raw, true)
        } else {
            // Wrap in parens so top-level becomes a let-block when it contains bindings
            (format!("({})", raw), true)
        }
    } else {
        eprintln!("no input; try -e '...' or --file path");
        return Ok(());
    };

    // Preload stdlib (M1): prepend prelude as a let-group unless disabled or in formatting mode
    let stdlib_enabled = !opt.no_stdlib && !opt.format_code;
    // Compute stdlib dir (may be used by ~require search paths)
    let resolved_stdlib_dir = opt
        .stdlib_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from("stdlib"));
    if stdlib_enabled {
        // Resolve stdlib dir
        let prelude_path = resolved_stdlib_dir.join("prelude.lzscr");
        if let Ok(prelude_src) = fs::read_to_string(&prelude_path) {
            // Combine as a let-group: ( prelude ; user )
            // ユーザコードが既に括弧で包まれている場合でも安全側でネスト
            code = format!("({}\n{} )", prelude_src, code);
        } else {
            eprintln!("warning: stdlib prelude not found at {} (use --stdlib-dir or --no-stdlib)", prelude_path.display());
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

        // Expand ~require before parsing for subsequent phases (dump/analyze/exec)
        let t_req_start = Instant::now();
        let module_search_paths = build_module_search_paths(&resolved_stdlib_dir, opt.module_path.as_deref());
        let ast = match expand_requires_to_ast(&code, &module_search_paths) {
            Ok(expanded_ast) => expanded_ast,
            Err(e) => {
                eprintln!("require error: {}", e);
                std::process::exit(2);
            }
        };
        if opt.analyze_trace { eprintln!("trace: require-expand+parse {} ms", t_req_start.elapsed().as_millis()); }
        let ast_nodes = {
            fn count(e: &Expr) -> usize {
                use ExprKind::*;
                match &e.kind {
                    Unit | Int(_) | Float(_) | Str(_) | Char(_) | Ref(_) | Symbol(_) | TypeVal(_) => 1,
                    Annot { expr, .. } => 1 + count(expr),
                    Lambda { body, .. } => 1 + count(body),
                    Apply { func, arg } => 1 + count(func) + count(arg),
                    Block(inner) => 1 + count(inner),
                    List(xs) => 1 + xs.iter().map(count).sum::<usize>(),
                    LetGroup { bindings, body } => 1 + count(body) + bindings.iter().map(|(_, ex)| count(ex)).sum::<usize>(),
                    Raise(inner) => 1 + count(inner),
                    OrElse { left, right } | AltLambda { left, right } | Catch { left, right } => 1 + count(left) + count(right),
                }
            }
            count(&ast)
        };
        if opt.analyze_trace { eprintln!("trace: ast-nodes {}", ast_nodes); }
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
            return Ok(())
        }
    if opt.analyze {
            #[derive(Serialize)]
            struct AnalyzeOut<'a> {
                duplicates: &'a [lzscr_analyzer::DupFinding],
                unbound_refs: &'a [lzscr_analyzer::UnboundRef],
                shadowing: &'a [lzscr_analyzer::Shadowing],
                unused_params: &'a [lzscr_analyzer::UnusedParam],
                ctor_arity: Vec<lzscr_analyzer::CtorArityIssue>,
            }
            // duplicates (optionally skipped)
            let t_dup_start = Instant::now();
            let dups = if opt.no_dup {
                Vec::new()
            } else {
                analyze_duplicates(
                    &ast,
                    AnalyzeOptions {
                        min_size: opt.dup_min_size,
                        min_count: opt.dup_min_count,
                    },
                )
            };
            if opt.analyze_trace { eprintln!("trace: duplicates {} ms ({} findings)", t_dup_start.elapsed().as_millis(), dups.len()); }
            // unbound refs
            let t_unb_start = Instant::now();
            let unb = analyze_unbound_refs(&ast, &default_allowlist());
            if opt.analyze_trace { eprintln!("trace: unbound-refs {} ms ({} findings)", t_unb_start.elapsed().as_millis(), unb.len()); }
            // shadowing
            let t_sh_start = Instant::now();
            let sh = analyze_shadowing(&ast);
            if opt.analyze_trace { eprintln!("trace: shadowing {} ms ({} findings)", t_sh_start.elapsed().as_millis(), sh.len()); }
            // unused params
            let t_up_start = Instant::now();
            let up = analyze_unused_params(&ast);
            if opt.analyze_trace { eprintln!("trace: unused-params {} ms ({} findings)", t_up_start.elapsed().as_millis(), up.len()); }
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
            if opt.analyze_trace { eprintln!("trace: ctor-arity {} ms ({} findings)", t_ca_start.elapsed().as_millis(), ca.len()); }
            if opt.format == "json" {
                let out = AnalyzeOut {
                    duplicates: &dups,
                    unbound_refs: &unb,
                    shadowing: &sh,
                    unused_params: &up,
                    ctor_arity: ca,
                };
                println!("{}", serde_json::to_string_pretty(&out)?);
            } else {
                for f in &dups {
                    eprintln!(
                        "duplicate: size={} count={} span=({},{}) repr={}",
                        f.size, f.count, f.span.offset, f.span.len, f.repr
                    );
                }
                for u in &unb {
                    eprintln!(
                        "unbound-ref: name={} span=({},{})",
                        u.name, u.span.offset, u.span.len
                    );
                }
                for s in &sh {
                    eprintln!(
                        "shadowing: name={} lambda_span=({},{})",
                        s.name, s.lambda_span.offset, s.lambda_span.len
                    );
                }
                for u in &up {
                    eprintln!(
                        "unused-param: name={} lambda_span=({},{})",
                        u.name, u.lambda_span.offset, u.lambda_span.len
                    );
                }
                for c in &ca {
                    eprintln!(
                        "ctor-arity: name={} expected={} got={} span=({}, {}) kind={}",
                        c.name, c.expected, c.got, c.span.offset, c.span.len, c.kind
                    );
                }
            }
            return Ok(());
        }
    // Optional typechecking phase
        if !opt.no_typecheck {
            let ty_res = lzscr_types::api::infer_program(&code);
            match ty_res {
                Ok(t) => {
                    if opt.types == "json" {
                        #[derive(Serialize)]
                        struct TypeOut { ty: String }
                        println!("{}", serde_json::to_string_pretty(&TypeOut { ty: t })?);
                    } else if opt.types == "pretty" {
                        eprintln!("type: {t}");
                    }
                }
                Err(e) => {
                    eprintln!("type error: {e}");
                    std::process::exit(2);
                }
            }
        }
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
                        // Build line/col index once
                        let src = &code;
                        let lines: Vec<&str> = src.lines().collect();
                        let mut acc = 0usize;
                        let mut offsets: Vec<usize> = Vec::with_capacity(lines.len());
                        for ln in &lines { offsets.push(acc); acc += ln.len() + 1; }
                        for (idx, sp) in spans.iter().enumerate() {
                            // find line by binary search
                            let mut line_no = 0usize;
                            for (i, off) in offsets.iter().enumerate() { if *off <= sp.offset { line_no = i; } else { break; } }
                            let col = sp.offset - offsets.get(line_no).copied().unwrap_or(0);
                            let ltxt = lines.get(line_no).copied().unwrap_or("");
                            eprintln!("  at {}:{}:{}", line_no + 1, col + 1, sp.len);
                            eprintln!("    {}", ltxt);
                            let mut caret = String::new();
                            for _ in 0..col { caret.push(' '); }
                            if sp.len > 0 { caret.push('^'); for _ in 1..sp.len { caret.push('~'); } }
                            eprintln!("    {}", caret);
                            if idx + 1 == spans.len() { eprintln!("  (most recent call last)"); }
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
        Value::Bool(b) => b.to_string(),
        Value::Str(s) => s.to_string(),
        Value::Char(c) => char_literal_string(*c),
                Value::Symbol(id) => env.symbol_name(*id),
                Value::Raised(b) => format!("^({})", val_to_string(env, b)),
                Value::Thunk { .. } => "<thunk>".into(),
                Value::Ctor { name, args } => {
                    if name == ".," {
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
                    xs.iter()
                        .map(|x| val_to_string(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Value::Tuple(xs) => format!(
                    "({})",
                    xs.iter()
                        .map(|x| val_to_string(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
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
    println!("{out}");
    Ok(())
    }
}

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
            if seg.is_empty() { continue; }
            paths.push(PathBuf::from(seg));
        }
    }
    paths
}

fn expand_requires_to_ast(src: &str, search_paths: &[PathBuf]) -> Result<Expr, String> {
    let ast = parse_expr(src).map_err(|e| format!("parse error before require expansion: {e}"))?;
    let mut stack: Vec<String> = Vec::new();
    expand_requires_in_expr(&ast, search_paths, &mut stack)
}

fn expand_requires_in_expr(e: &Expr, search_paths: &[PathBuf], stack: &mut Vec<String>) -> Result<Expr, String> {
    // First, handle ~builtin path: expand to a Ref of a generated alias name.
    if let Some(alias) = match_builtin_call(e) {
        return Ok(Expr { kind: ExprKind::Ref(alias), span: e.span });
    }
    // Next, handle ~require expansion
    match match_require_call(e) {
        Some(Ok(segs)) => {
        let rel = segs.join("/") + ".lzscr";
        let (path, content) = resolve_module_content(&rel, search_paths)?;
        let canon = match std::fs::canonicalize(&path) {
            Ok(p) => p.display().to_string(),
            Err(_) => path.display().to_string(),
        };
        if stack.contains(&canon) {
            return Err(format!("cyclic require detected: {} -> ... -> {}", stack.first().cloned().unwrap_or_default(), canon));
        }
        stack.push(canon);
        // Wrap like file rule: ( <content> )
        let wrapped = format!("({})", content);
        let parsed = match parse_expr(&wrapped) {
            Ok(x) => x,
            Err(e) => {
                return Err(format!("parse error in required module '{}': {}", rel, e));
            }
        };
        let expanded = expand_requires_in_expr(&parsed, search_paths, stack)?;
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
        Unit | Int(_) | Float(_) | Str(_) | Char(_) | Ref(_) | Symbol(_) | TypeVal(_) => e.kind.clone(),
        Annot { ty, expr } => Annot { ty: ty.clone(), expr: Box::new(expand_requires_in_expr(expr, search_paths, stack)?) },
        Lambda { param, body } => Lambda { param: param.clone(), body: Box::new(expand_requires_in_expr(body, search_paths, stack)?) },
        Apply { func, arg } => Apply {
            func: Box::new(expand_requires_in_expr(func, search_paths, stack)?),
            arg: Box::new(expand_requires_in_expr(arg, search_paths, stack)?),
        },
        Block(inner) => Block(Box::new(expand_requires_in_expr(inner, search_paths, stack)?)),
        List(xs) => List(xs.iter().map(|x| expand_requires_in_expr(x, search_paths, stack)).collect::<Result<Vec<_>, _>>()?),
        LetGroup { bindings, body } => {
            let mut new_bs = Vec::with_capacity(bindings.len());
            for (p, ex) in bindings.iter() {
                new_bs.push((p.clone(), expand_requires_in_expr(ex, search_paths, stack)?));
            }
            LetGroup { bindings: new_bs, body: Box::new(expand_requires_in_expr(body, search_paths, stack)?) }
        }
        Raise(inner) => Raise(Box::new(expand_requires_in_expr(inner, search_paths, stack)?)),
        AltLambda { left, right } => AltLambda {
            left: Box::new(expand_requires_in_expr(left, search_paths, stack)?),
            right: Box::new(expand_requires_in_expr(right, search_paths, stack)?),
        },
        OrElse { left, right } => OrElse {
            left: Box::new(expand_requires_in_expr(left, search_paths, stack)?),
            right: Box::new(expand_requires_in_expr(right, search_paths, stack)?),
        },
        Catch { left, right } => Catch {
            left: Box::new(expand_requires_in_expr(left, search_paths, stack)?),
            right: Box::new(expand_requires_in_expr(right, search_paths, stack)?),
        },
    };
    Ok(Expr { kind: k, span: e.span })
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
            if args.is_empty() { return Some(Err("~require expects at least one .segment".into())); }
            let mut segs = Vec::with_capacity(args.len());
            for a in args.into_iter().rev() {
                match &a.kind {
                    ExprKind::Symbol(s) if s.starts_with('.') && s.len() > 1 => {
                        segs.push(s[1..].to_string());
                    }
                    other => {
                        return Some(Err(format!("~require expects only ctor-dot symbols (.name); got {}", node_kind_name(other))));
                    }
                }
            }
            Some(Ok(segs))
        }
        _ => None,
    }
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
            if args.is_empty() { return Some("builtin".to_string()); }
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
        ExprKind::LetGroup { .. } => "LetGroup",
        ExprKind::Raise(_) => "Raise",
        ExprKind::AltLambda { .. } => "AltLambda",
        ExprKind::OrElse { .. } => "OrElse",
        ExprKind::Catch { .. } => "Catch",
    }
}

fn resolve_module_content(rel_path: &str, search_paths: &[PathBuf]) -> Result<(PathBuf, String), String> {
    for root in search_paths {
        let p = root.join(rel_path);
        if let Ok(s) = fs::read_to_string(&p) {
            return Ok((p, s));
        }
    }
    Err(format!("module not found: {} (searched in: {})", rel_path, search_paths.iter().map(|p| p.display().to_string()).collect::<Vec<_>>().join(":")))
}
