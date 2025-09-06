use clap::Parser;
use lzscr_analyzer::{
    analyze_ctor_arity, analyze_duplicates, analyze_shadowing, analyze_unbound_refs,
    analyze_unused_params, default_allowlist, AnalyzeOptions,
};
use lzscr_coreir::{lower_expr_to_core, print_term};
use lzscr_parser::parse_expr;
use lzscr_runtime::{eval, Env, Value};
use serde::Serialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;

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

    /// Duplicate detection: minimum subtree size (nodes)
    #[arg(long = "dup-min-size", default_value_t = 3)]
    dup_min_size: usize,

    /// Duplicate detection: minimum occurrences
    #[arg(long = "dup-min-count", default_value_t = 2)]
    dup_min_count: usize,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::parse();
    // Select input source: -e or --file
    let (code, _from_file) = if let Some(c) = opt.eval {
        (c, false)
    } else if let Some(ref p) = opt.file {
        let raw = fs::read_to_string(&p)?;
        // Wrap in parens so top-level becomes a let-block when it contains bindings
        (format!("({})", raw), true)
    } else {
        eprintln!("no input; try -e '...' or --file path");
        return Ok(());
    };

    {
        let ast = parse_expr(&code).map_err(|e| format!("{}", e))?;
        // Core IR dump modes take precedence over analyze/execute
        if opt.dump_coreir || opt.dump_coreir_json {
            let term = lower_expr_to_core(&ast);
            if opt.dump_coreir_json {
                println!("{}", serde_json::to_string_pretty(&term)?);
            } else {
                println!("{}", print_term(&term));
            }
            return Ok(());
        }
        if opt.format_code {
            let from_file = opt.file.is_some();
            // If input was from --file, use file-aware formatting (drop outer parens)
            let out = if from_file {
                lzscr_format::format_file_source(&code)
            } else {
                lzscr_format::format_source(&code)
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
    if opt.analyze {
            #[derive(Serialize)]
            struct AnalyzeOut<'a> {
                duplicates: &'a [lzscr_analyzer::DupFinding],
                unbound_refs: &'a [lzscr_analyzer::UnboundRef],
                shadowing: &'a [lzscr_analyzer::Shadowing],
                unused_params: &'a [lzscr_analyzer::UnusedParam],
                ctor_arity: Vec<lzscr_analyzer::CtorArityIssue>,
            }
            // duplicates
            let dups = analyze_duplicates(
                &ast,
                AnalyzeOptions {
                    min_size: opt.dup_min_size,
                    min_count: opt.dup_min_count,
                },
            );
            // unbound refs
            let unb = analyze_unbound_refs(&ast, &default_allowlist());
            // shadowing
            let sh = analyze_shadowing(&ast);
            // unused params
            let up = analyze_unused_params(&ast);
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
            let ca = analyze_ctor_arity(&ast, &arities);
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
        let val = eval(&env, &ast).map_err(|e| format!("{e}"))?;
        fn val_to_string(env: &Env, v: &Value) -> String {
            match v {
                Value::Unit => "()".into(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Str(s) => s.clone(),
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
        return Ok(());
    }
}
