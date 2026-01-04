use lzscr_coreir::{Op, Term};

#[derive(thiserror::Error, Debug)]
pub enum LlvmIrLowerError {
    #[error("{0}")]
    Message(String),
}

const SUPPORTED_I64_BUILTINS: [&str; 4] = ["add", "sub", "mul", "div"];

fn op_summary(op: &Op) -> String {
    match op {
        Op::Int(_) => "Int".into(),
        Op::Float(_) => "Float".into(),
        Op::Bool(_) => "Bool".into(),
        Op::Str(_) => "Str".into(),
        Op::Char(_) => "Char".into(),
        Op::Unit => "Unit".into(),
        Op::Ref(name) => format!("Ref({name})"),
        Op::Symbol(_) => "Symbol".into(),
        Op::Ctor(_) => "Ctor".into(),
        Op::AtomSymbol(_) => "AtomSymbol".into(),
        Op::List { .. } => "List".into(),
        Op::Tuple { .. } => "Tuple".into(),
        Op::Record { .. } => "Record".into(),
        Op::ModeMap { .. } => "ModeMap".into(),
        Op::Select { .. } => "Select".into(),
        Op::Lam { .. } => "Lam".into(),
        Op::App { .. } => "App".into(),
        Op::Seq { .. } => "Seq".into(),
        Op::Chain { .. } => "Chain".into(),
        Op::Bind { .. } => "Bind".into(),
        Op::Raise { .. } => "Raise".into(),
        Op::Catch { .. } => "Catch".into(),
        Op::OrElse { .. } => "OrElse".into(),
        Op::Alt { .. } => "Alt".into(),
        Op::LetRec { .. } => "LetRec".into(),
    }
}

fn unsupported(message: String) -> LlvmIrLowerError {
    LlvmIrLowerError::Message(message)
}

fn unsupported_with_hint(op: &Op, details: Option<String>) -> LlvmIrLowerError {
    let mut msg = format!(
        "LLVM lowering does not support this CoreIR op yet: {}",
        op_summary(op)
    );
    if let Some(d) = details {
        msg.push_str("\n");
        msg.push_str(&d);
    }
    msg.push_str("\nSupported subset: Int literals and saturated (~add|~sub|~mul|~div) over i64.");
    msg.push_str("\nHint: run lzscr-cli with --dump-coreir to inspect the lowered term.");
    unsupported(msg)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LlvmTy {
    I64,
}

#[derive(Debug)]
enum LlvmValue {
    ConstI64(i64),
    Reg { name: String, ty: LlvmTy },
}

impl LlvmValue {
    fn ty(&self) -> LlvmTy {
        match self {
            LlvmValue::ConstI64(_) => LlvmTy::I64,
            LlvmValue::Reg { ty, .. } => *ty,
        }
    }

    fn render(&self) -> String {
        match self {
            LlvmValue::ConstI64(n) => n.to_string(),
            LlvmValue::Reg { name, .. } => name.clone(),
        }
    }
}

#[derive(Default)]
struct Emitter {
    next_reg: usize,
    instrs: Vec<String>,
}

impl Emitter {
    fn fresh(&mut self) -> String {
        let name = format!("%{}", self.next_reg);
        self.next_reg += 1;
        name
    }

    fn emit_binop_i64(
        &mut self,
        op: &str,
        a: LlvmValue,
        b: LlvmValue,
    ) -> Result<LlvmValue, LlvmIrLowerError> {
        if a.ty() != LlvmTy::I64 || b.ty() != LlvmTy::I64 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for {op}: expected (i64, i64), got ({:?}, {:?})",
                a.ty(),
                b.ty()
            )));
        }
        let dst = self.fresh();
        self.instrs.push(format!("  {dst} = {op} i64 {}, {}", a.render(), b.render()));
        Ok(LlvmValue::Reg { name: dst, ty: LlvmTy::I64 })
    }
}

fn collect_apps<'a>(t: &'a Term) -> (&'a Term, Vec<&'a Term>) {
    let mut head = t;
    let mut args_rev: Vec<&'a Term> = vec![];
    while let Op::App { func, arg } = &head.op {
        args_rev.push(arg);
        head = func;
    }
    args_rev.reverse();
    (head, args_rev)
}

fn lower_expr_i64(em: &mut Emitter, t: &Term) -> Result<LlvmValue, LlvmIrLowerError> {
    match &t.op {
        Op::Int(n) => Ok(LlvmValue::ConstI64(*n)),
        Op::App { .. } => {
            let (head, args) = collect_apps(t);
            match (&head.op, args.as_slice()) {
                (Op::Ref(name), [a, b]) => {
                    let av = lower_expr_i64(em, a)?;
                    let bv = lower_expr_i64(em, b)?;
                    match name.as_str() {
                        "add" => em.emit_binop_i64("add", av, bv),
                        "sub" => em.emit_binop_i64("sub", av, bv),
                        "mul" => em.emit_binop_i64("mul", av, bv),
                        "div" => em.emit_binop_i64("sdiv", av, bv),
                        other => Err(unsupported(format!(
                            "LLVM lowering does not support builtin '{other}' yet. Supported builtins: {}",
                            SUPPORTED_I64_BUILTINS.join("|")
                        ))),
                    }
                }
                _ => Err(unsupported_with_hint(
                    &t.op,
                    Some(format!(
                        "Expected a fully-applied builtin call like (~add a b). Got head={} with {} arg(s).",
                        op_summary(&head.op),
                        args.len()
                    )),
                )),
            }
        }
        other => Err(unsupported_with_hint(other, None)),
    }
}

/// Lowers a CoreIR term into a minimal LLVM IR module.
///
/// Current scope (int-only PoC): supports `Int` literals and saturated applications of
/// `~add/~sub/~mul/~div` (as `Ref("add"|...)`) producing an `i64` `main` result.
pub fn lower_to_llvm_ir_text(term: &Term) -> Result<String, LlvmIrLowerError> {
    let mut em = Emitter::default();
    let v = lower_expr_i64(&mut em, term)?;
    if v.ty() != LlvmTy::I64 {
        return Err(unsupported("LLVM lowering produced a non-i64 result; only i64 is supported in this PoC.".into()));
    }

    let mut out = String::new();
    out.push_str("; lzscr-llvmir (PoC)\n");
    out.push_str("target triple = \"unknown-unknown-unknown\"\n\n");
    out.push_str("define i64 @main() {\n");
    out.push_str("entry:\n");
    for ins in em.instrs {
        out.push_str(&ins);
        out.push('\n');
    }
    out.push_str(&format!("  ret i64 {}\n", v.render()));
    out.push_str("}\n");
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn ref_(name: &str) -> Term {
        Term::new(Op::Ref(name.into()))
    }
    fn int_(n: i64) -> Term {
        Term::new(Op::Int(n))
    }
    fn app(f: Term, a: Term) -> Term {
        Term::new(Op::App { func: Box::new(f), arg: Box::new(a) })
    }

    #[test]
    fn lowers_simple_add_chain() {
        // ((~add 1) ((~mul 2) 3))
        let t = app(app(ref_("add"), int_(1)), app(app(ref_("mul"), int_(2)), int_(3)));
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = mul i64 2, 3
  %1 = add i64 1, %0
  ret i64 %1
}
"#;
        assert_eq!(ir, expected);
    }
}
