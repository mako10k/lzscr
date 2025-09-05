use clap::Parser;
use lzscr_analyzer::{
    analyze_duplicates, analyze_shadowing, analyze_unbound_refs, analyze_unused_params,
    default_allowlist, AnalyzeOptions,
};
use lzscr_parser::parse_expr;
use lzscr_runtime::{eval, Env, Value};

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
        if opt.analyze {
            // duplicates
            let dups = analyze_duplicates(
                &ast,
                AnalyzeOptions {
                    min_size: opt.dup_min_size,
                    min_count: opt.dup_min_count,
                },
            );
            for f in &dups {
                eprintln!(
                    "duplicate: size={} count={} span=({},{}) repr={}",
                    f.size, f.count, f.span.offset, f.span.len, f.repr
                );
            }
            // unbound refs
            let unb = analyze_unbound_refs(&ast, &default_allowlist());
            for u in &unb {
                eprintln!(
                    "unbound-ref: name={} span=({},{})",
                    u.name, u.span.offset, u.span.len
                );
            }
            // shadowing
            let sh = analyze_shadowing(&ast);
            for s in &sh {
                eprintln!(
                    "shadowing: name={} lambda_span=({},{})",
                    s.name, s.lambda_span.offset, s.lambda_span.len
                );
            }
            // unused params
            let up = analyze_unused_params(&ast);
            for u in &up {
                eprintln!(
                    "unused-param: name={} lambda_span=({},{})",
                    u.name, u.lambda_span.offset, u.lambda_span.len
                );
            }
            return Ok(());
        }
        let mut env = Env::with_builtins();
        if opt.strict_effects {
            env.strict_effects = true;
        }
        let val = eval(&env, &ast).map_err(|e| format!("{e}"))?;
        let out = match val {
            Value::Unit => "()".to_string(),
            Value::Int(n) => n.to_string(),
            Value::Str(s) => s,
            Value::Symbol(s) => s,
            Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
        };
        println!("{out}");
        return Ok(());
    }
    eprintln!("no input; try -e '...'");
    Ok(())
}
