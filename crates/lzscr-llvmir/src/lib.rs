use lzscr_coreir::{Op, Term};
use std::collections::HashMap;

use lzscr_ast::ast::PatternKind;

#[derive(thiserror::Error, Debug)]
pub enum LlvmIrLowerError {
    #[error("{0}")]
    Message(String),
}

const SUPPORTED_I64_BUILTINS: [&str; 10] = [
    "add", "sub", "mul", "div", "eq", "ne", "lt", "le", "gt", "ge",
];

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
    msg.push_str(
        "\nSupported subset: Int literals, Bool literals (as i64 0/1), i64-only (~seq a b) and (~chain a b), i64-only inline lambda application ((\\~x -> body) arg ...) with ~x params only, and saturated (~add|~sub|~mul|~div|~eq|~ne|~lt|~le|~gt|~ge) over i64.",
    );
    msg.push_str("\nHint: run lzscr-cli with --dump-coreir to inspect the lowered term.");
    unsupported(msg)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LlvmTy {
    I64,
    I1,
}

#[derive(Debug, Clone)]
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

    fn emit_icmp_i64(
        &mut self,
        pred: &str,
        a: LlvmValue,
        b: LlvmValue,
    ) -> Result<LlvmValue, LlvmIrLowerError> {
        if a.ty() != LlvmTy::I64 || b.ty() != LlvmTy::I64 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for icmp {pred}: expected (i64, i64), got ({:?}, {:?})",
                a.ty(),
                b.ty()
            )));
        }
        let dst = self.fresh();
        self.instrs
            .push(format!("  {dst} = icmp {pred} i64 {}, {}", a.render(), b.render()));
        Ok(LlvmValue::Reg { name: dst, ty: LlvmTy::I1 })
    }

    fn emit_zext_i1_to_i64(&mut self, v: LlvmValue) -> Result<LlvmValue, LlvmIrLowerError> {
        if v.ty() != LlvmTy::I1 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for zext: expected i1, got {:?}",
                v.ty()
            )));
        }
        let dst = self.fresh();
        self.instrs
            .push(format!("  {dst} = zext i1 {} to i64", v.render()));
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

fn lower_expr_i64(
    em: &mut Emitter,
    env: &mut HashMap<String, LlvmValue>,
    t: &Term,
) -> Result<LlvmValue, LlvmIrLowerError> {
    match &t.op {
        Op::Int(n) => Ok(LlvmValue::ConstI64(*n)),
        Op::Bool(b) => Ok(LlvmValue::ConstI64(if *b { 1 } else { 0 })),
        Op::Ref(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| unsupported_with_hint(&t.op, Some(format!("Unbound reference '{name}'")))),
        Op::Seq { first, second } => {
            // Evaluate first for its effects (within the supported i64-only subset), then return second.
            let _ = lower_expr_i64(em, env, first)?;
            lower_expr_i64(em, env, second)
        }
        Op::Chain { first, second } => {
            // Evaluate first in effect context (within the supported i64-only subset), then return second.
            let _ = lower_expr_i64(em, env, first)?;
            lower_expr_i64(em, env, second)
        }
        Op::App { .. } => {
            let (head, args) = collect_apps(t);
            match (&head.op, args.as_slice()) {
                (Op::Ref(name), [a, b]) => {
                    let av = lower_expr_i64(em, env, a)?;
                    let bv = lower_expr_i64(em, env, b)?;
                    match name.as_str() {
                        "add" => em.emit_binop_i64("add", av, bv),
                        "sub" => em.emit_binop_i64("sub", av, bv),
                        "mul" => em.emit_binop_i64("mul", av, bv),
                        "div" => em.emit_binop_i64("sdiv", av, bv),
                        "eq" => {
                            let c = em.emit_icmp_i64("eq", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        "ne" => {
                            let c = em.emit_icmp_i64("ne", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        "lt" => {
                            let c = em.emit_icmp_i64("slt", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        "le" => {
                            let c = em.emit_icmp_i64("sle", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        "gt" => {
                            let c = em.emit_icmp_i64("sgt", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        "ge" => {
                            let c = em.emit_icmp_i64("sge", av, bv)?;
                            em.emit_zext_i1_to_i64(c)
                        }
                        other => Err(unsupported(format!(
                            "LLVM lowering does not support builtin '{other}' yet. Supported builtins: {}",
                            SUPPORTED_I64_BUILTINS.join("|")
                        ))),
                    }
                }
                (Op::Lam { .. }, _) if !args.is_empty() => {
                    // i64-only PoC: inline lambda applications (\~x -> body) a b ...
                    // This supports curried lambdas by consuming args and stepping into nested `Lam`s.
                    let mut func = head;
                    let mut restore: Vec<(String, Option<LlvmValue>)> = vec![];

                    for (idx, arg_term) in args.iter().enumerate() {
                        let Op::Lam { param, body } = &func.op else {
                            return Err(unsupported_with_hint(
                                &func.op,
                                Some(format!(
                                    "Expected a lambda during curried application at arg #{}, got {}",
                                    idx + 1,
                                    op_summary(&func.op)
                                )),
                            ));
                        };

                        let PatternKind::Var(name) = &param.kind else {
                            return Err(unsupported_with_hint(
                                &func.op,
                                Some("Only variable lambda params (~x) are supported in LLVM i64-only lowering.".into()),
                            ));
                        };

                        let arg_val = lower_expr_i64(em, env, arg_term)?;
                        let prev = env.insert(name.clone(), arg_val);
                        restore.push((name.clone(), prev));
                        func = body;
                    }

                    let result = lower_expr_i64(em, env, func);
                    for (name, prev) in restore.into_iter().rev() {
                        match prev {
                            Some(v) => {
                                env.insert(name, v);
                            }
                            None => {
                                env.remove(&name);
                            }
                        }
                    }
                    result
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
/// Current scope (int-only PoC): supports `Int` literals, Bool literals (as i64 0/1), i64-only `~seq` and `~chain`, and saturated
/// applications of `~add/~sub/~mul/~div/~eq/~ne/~lt/~le/~gt/~ge` (as `Ref("add"|...)`) producing an `i64` `main` result.
pub fn lower_to_llvm_ir_text(term: &Term) -> Result<String, LlvmIrLowerError> {
    let mut em = Emitter::default();
    let mut env: HashMap<String, LlvmValue> = HashMap::new();
    let v = lower_expr_i64(&mut em, &mut env, term)?;
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
    use lzscr_ast::ast::Pattern;
    use lzscr_ast::span::Span;
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

    fn lam_var(name: &str, body: Term) -> Term {
        let param = Pattern::new(PatternKind::Var(name.into()), Span::new(0, 0));
        Term::new(Op::Lam { param, body: Box::new(body) })
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

    #[test]
    fn lowers_seq_i64_only() {
        // (~seq ((~mul 2) 3) ((~add 10) 20))
        let first = app(app(ref_("mul"), int_(2)), int_(3));
        let second = app(app(ref_("add"), int_(10)), int_(20));
        let t = Term::new(Op::Seq { first: Box::new(first), second: Box::new(second) });
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = mul i64 2, 3
  %1 = add i64 10, 20
  ret i64 %1
}
"#;
        assert_eq!(ir, expected);
    }


    #[test]
    fn lowers_lt_to_icmp_zext() {
        // ((~lt 1) 2)
        let t = app(app(ref_("lt"), int_(1)), int_(2));
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp slt i64 1, 2
  %1 = zext i1 %0 to i64
  ret i64 %1
}
"#;
        assert_eq!(ir, expected);
    }

    #[test]
    fn lowers_eq_to_icmp_zext() {
        // ((~eq 42) 42)
        let t = app(app(ref_("eq"), int_(42)), int_(42));
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp eq i64 42, 42
  %1 = zext i1 %0 to i64
  ret i64 %1
}
"#;
        assert_eq!(ir, expected);
    }

    #[test]
    fn lowers_chain_i64_only() {
        // (~chain ((~mul 2) 3) ((~add 10) 20))
        let first = app(app(ref_("mul"), int_(2)), int_(3));
        let second = app(app(ref_("add"), int_(10)), int_(20));
        let t = Term::new(Op::Chain { first: Box::new(first), second: Box::new(second) });
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = mul i64 2, 3
  %1 = add i64 10, 20
  ret i64 %1
}
"#;
        assert_eq!(ir, expected);
    }

        #[test]
        fn lowers_inline_lambda_application_i64_only() {
                // ((\~x -> (~add ~x 1)) 5)
                let body = app(app(ref_("add"), ref_("x")), int_(1));
                let lam = lam_var("x", body);
                let t = app(lam, int_(5));
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = concat!(
                    r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
"#,
                    "  %0 = add i64 5, 1\n",
                    "  ret i64 %0\n",
                    "}\n",
                );
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_curried_lambda_application_i64_only() {
                // (((\~x -> (\~y -> (~add ~x ~y))) 2) 3)
                let inner = app(app(ref_("add"), ref_("x")), ref_("y"));
                let lam_y = lam_var("y", inner);
                let lam_x = lam_var("x", lam_y);
                let t = app(app(lam_x, int_(2)), int_(3));
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = concat!(
                    r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
"#,
                    "  %0 = add i64 2, 3\n",
                    "  ret i64 %0\n",
                    "}\n",
                );
                assert_eq!(ir, expected);
        }
}
