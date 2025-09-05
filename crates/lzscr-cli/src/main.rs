use clap::Parser;
use lzscr_parser::parse_expr;
use lzscr_runtime::{eval, Env, Value};

#[derive(Parser, Debug)]
#[command(name = "lzscr", version, about = "LazyScript reimplementation (skeleton)")]
struct Opt {
    /// One-line program
    #[arg(short = 'e', long = "eval")]
    eval: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::parse();
    if let Some(code) = opt.eval {
        let ast = parse_expr(&code).map_err(|e| format!("{}", e))?;
        let env = Env::with_builtins();
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
