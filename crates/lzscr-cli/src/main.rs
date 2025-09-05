use clap::Parser;
use lzscr_analyzer::{
    analyze_duplicates, analyze_shadowing, analyze_unbound_refs, analyze_unused_params,
    default_allowlist, AnalyzeOptions,
};
use lzscr_parser::parse_expr;
use lzscr_runtime::{eval, Env, Value};
use serde::Serialize;
use lzscr_coreir::{lower_expr_to_core, print_term};

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

    /// Output format for --analyze: text|json
    #[arg(long = "format", default_value = "text")]
    format: String,

    /// Duplicate detection: minimum subtree size (nodes)
    #[arg(long = "dup-min-size", default_value_t = 3)]
    dup_min_size: usize,

    /// Duplicate detection: minimum occurrences
    #[arg(long = "dup-min-count", default_value_t = 2)]
    dup_min_count: usize,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::parse();
    if let Some(code) = opt.eval {
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
        if opt.analyze {
            #[derive(Serialize)]
            struct AnalyzeOut<'a> {
                duplicates: &'a [lzscr_analyzer::DupFinding],
                unbound_refs: &'a [lzscr_analyzer::UnboundRef],
                shadowing: &'a [lzscr_analyzer::Shadowing],
                unused_params: &'a [lzscr_analyzer::UnusedParam],
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
            if opt.format == "json" {
                let out = AnalyzeOut { duplicates: &dups, unbound_refs: &unb, shadowing: &sh, unused_params: &up };
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
            }
            return Ok(());
        }
        let mut env = Env::with_builtins();
        if opt.strict_effects {
            env.strict_effects = true;
        }
        let val = eval(&env, &ast).map_err(|e| format!("{e}"))?;
        fn val_to_string(v: &Value) -> String {
            match v {
                Value::Unit => "()".into(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Str(s) => s.clone(),
                Value::Symbol(s) => s.clone(),
                Value::Ctor { name, args } => {
                    if args.is_empty() {
                        name.clone()
                    } else {
                        format!("{}({})", name, args.iter().map(val_to_string).collect::<Vec<_>>().join(", "))
                    }
                }
                Value::List(xs) => format!("[{}]", xs.iter().map(val_to_string).collect::<Vec<_>>().join(", ")),
                Value::Tuple(xs) => format!("({})", xs.iter().map(val_to_string).collect::<Vec<_>>().join(", ")),
                Value::Record(map) => {
                    let inner = map.iter().map(|(k,v)| format!("{}: {}", k, val_to_string(v))).collect::<Vec<_>>().join(", ");
                    format!("{{{}}}", inner)
                }
                Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
            }
        }
        let out = val_to_string(&val);
        println!("{out}");
        return Ok(());
    }
    eprintln!("no input; try -e '...'");
    Ok(())
}
