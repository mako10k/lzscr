use lzscr_coreir::{Op, Term};
use std::collections::{HashMap, HashSet};

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
        "\nSupported subset: Int literals, Bool literals (as i64 0/1), Ctor(True/False) (as i64 1/0), i64-only (if cond then else), i64-only (and/or) with short-circuit, i64-only not, i64-only (~seq a b) and (~chain a b), i64-only inline lambda application ((\\~x -> body) arg ...) with ~x params only, and saturated (~add|~sub|~mul|~div|~eq|~ne|~lt|~le|~gt|~ge) over i64.",
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
    next_block: usize,
    blocks: Vec<Block>,
    cur: usize,
}

#[derive(Default)]
struct Block {
    label: String,
    instrs: Vec<String>,
    terminator: Option<String>,
}

impl Emitter {
    fn new() -> Self {
        Self {
            next_reg: 0,
            next_block: 0,
            blocks: vec![Block { label: "entry".into(), ..Block::default() }],
            cur: 0,
        }
    }

    fn fresh_reg(&mut self) -> String {
        let name = format!("%{}", self.next_reg);
        self.next_reg += 1;
        name
    }

    fn fresh_block_label(&mut self, prefix: &str) -> String {
        let name = format!("{}{}", prefix, self.next_block);
        self.next_block += 1;
        name
    }

    fn cur_block_mut(&mut self) -> &mut Block {
        &mut self.blocks[self.cur]
    }

    fn emit_instr(&mut self, s: String) -> Result<(), LlvmIrLowerError> {
        if self.cur_block_mut().terminator.is_some() {
            return Err(unsupported("LLVM lowering internal error: instruction emitted after terminator".into()));
        }
        self.cur_block_mut().instrs.push(s);
        Ok(())
    }

    fn emit_terminator(&mut self, s: String) -> Result<(), LlvmIrLowerError> {
        if self.cur_block_mut().terminator.is_some() {
            return Err(unsupported("LLVM lowering internal error: terminator emitted twice".into()));
        }
        self.cur_block_mut().terminator = Some(s);
        Ok(())
    }

    fn start_block(&mut self, label: String) {
        self.blocks.push(Block { label, ..Block::default() });
        self.cur = self.blocks.len() - 1;
    }

    fn emit_ret_i64(&mut self, v: LlvmValue) -> Result<(), LlvmIrLowerError> {
        if v.ty() != LlvmTy::I64 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for ret: expected i64, got {:?}",
                v.ty()
            )));
        }
        self.emit_terminator(format!("  ret i64 {}", v.render()))
    }

    fn emit_br_uncond(&mut self, target: &str) -> Result<(), LlvmIrLowerError> {
        self.emit_terminator(format!("  br label %{}", target))
    }

    fn emit_br_cond(
        &mut self,
        cond_i1: LlvmValue,
        then_label: &str,
        else_label: &str,
    ) -> Result<(), LlvmIrLowerError> {
        if cond_i1.ty() != LlvmTy::I1 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for br: expected i1, got {:?}",
                cond_i1.ty()
            )));
        }
        self.emit_terminator(format!(
            "  br i1 {}, label %{}, label %{}",
            cond_i1.render(),
            then_label,
            else_label
        ))
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
        let dst = self.fresh_reg();
        self.emit_instr(format!("  {dst} = {op} i64 {}, {}", a.render(), b.render()))?;
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
        let dst = self.fresh_reg();
        self.emit_instr(format!("  {dst} = icmp {pred} i64 {}, {}", a.render(), b.render()))?;
        Ok(LlvmValue::Reg { name: dst, ty: LlvmTy::I1 })
    }

    fn emit_zext_i1_to_i64(&mut self, v: LlvmValue) -> Result<LlvmValue, LlvmIrLowerError> {
        if v.ty() != LlvmTy::I1 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for zext: expected i1, got {:?}",
                v.ty()
            )));
        }
        let dst = self.fresh_reg();
        self.emit_instr(format!("  {dst} = zext i1 {} to i64", v.render()))?;
        Ok(LlvmValue::Reg { name: dst, ty: LlvmTy::I64 })
    }

    fn emit_phi_i64(
        &mut self,
        then_label: &str,
        then_val: LlvmValue,
        else_label: &str,
        else_val: LlvmValue,
    ) -> Result<LlvmValue, LlvmIrLowerError> {
        if then_val.ty() != LlvmTy::I64 || else_val.ty() != LlvmTy::I64 {
            return Err(unsupported(format!(
                "LLVM lowering type mismatch for phi: expected (i64, i64), got ({:?}, {:?})",
                then_val.ty(),
                else_val.ty()
            )));
        }
        let dst = self.fresh_reg();
        self.emit_instr(format!(
            "  {dst} = phi i64 [ {}, %{} ], [ {}, %{} ]",
            then_val.render(),
            then_label,
            else_val.render(),
            else_label
        ))?;
        Ok(LlvmValue::Reg { name: dst, ty: LlvmTy::I64 })
    }

    fn render_function_body(&self) -> Result<String, LlvmIrLowerError> {
        let mut out = String::new();
        for b in &self.blocks {
            out.push_str(&format!("{}:\n", b.label));
            for ins in &b.instrs {
                out.push_str(ins);
                out.push('\n');
            }
            if let Some(t) = &b.terminator {
                out.push_str(t);
                out.push('\n');
            } else {
                return Err(unsupported(format!(
                    "LLVM lowering internal error: block '{}' has no terminator",
                    b.label
                )));
            }
        }
        Ok(out)
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

fn lower_cond_i1(
    em: &mut Emitter,
    env: &mut HashMap<String, LlvmValue>,
    cond: &Term,
) -> Result<LlvmValue, LlvmIrLowerError> {
    // Bool-like: in i64-only subset we treat i64 0 as false, non-zero as true.
    let cv = lower_expr_i64(em, env, cond)?;
    if cv.ty() != LlvmTy::I64 {
        return Err(unsupported(format!(
            "LLVM lowering produced a non-i64 condition; expected i64 Bool-like, got {:?}",
            cv.ty()
        )));
    }
    em.emit_icmp_i64("ne", cv, LlvmValue::ConstI64(0))
}

fn lower_expr_i64(
    em: &mut Emitter,
    env: &mut HashMap<String, LlvmValue>,
    t: &Term,
) -> Result<LlvmValue, LlvmIrLowerError> {
    match &t.op {
        Op::Int(n) => Ok(LlvmValue::ConstI64(*n)),
        Op::Bool(b) => Ok(LlvmValue::ConstI64(if *b { 1 } else { 0 })),
        Op::Ctor(name) if name == "True" => Ok(LlvmValue::ConstI64(1)),
        Op::Ctor(name) if name == "False" => Ok(LlvmValue::ConstI64(0)),
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
        Op::LetRec { bindings, body } => {
            // i64-only PoC: support a restricted, non-recursive subset of letrec.
            // Restriction: RHS must not reference any of the binding names (no self/mutual recursion).
            fn normalize_binder_name(name: &str) -> &str {
                name.strip_prefix('~').unwrap_or(name)
            }

            fn refs_forbidden(
                t: &Term,
                forbidden: &HashSet<String>,
                bound: &mut HashSet<String>,
            ) -> bool {
                match &t.op {
                    Op::Ref(name) => forbidden.contains(name) && !bound.contains(name),
                    Op::Lam { param, body } => {
                        let mut inserted: Option<String> = None;
                        if let PatternKind::Var(name) = &param.kind {
                            if bound.insert(name.clone()) {
                                inserted = Some(name.clone());
                            }
                        }
                        let hit = refs_forbidden(body, forbidden, bound);
                        if let Some(n) = inserted {
                            bound.remove(&n);
                        }
                        hit
                    }
                    Op::App { func, arg } => {
                        refs_forbidden(func, forbidden, bound) || refs_forbidden(arg, forbidden, bound)
                    }
                    Op::Seq { first, second }
                    | Op::Chain { first, second }
                    | Op::OrElse { left: first, right: second }
                    | Op::Alt { left: first, right: second }
                    | Op::Catch { left: first, right: second } => {
                        refs_forbidden(first, forbidden, bound) || refs_forbidden(second, forbidden, bound)
                    }
                    Op::Bind { value, cont } => {
                        refs_forbidden(value, forbidden, bound) || refs_forbidden(cont, forbidden, bound)
                    }
                    Op::Raise { payload } => refs_forbidden(payload, forbidden, bound),
                    Op::LetRec { bindings, body } => {
                        // Treat inner letrec binders as shadowing.
                        let inner_names: Vec<String> =
                            bindings.iter().map(|(n, _)| n.clone()).collect();
                        for n in &inner_names {
                            bound.insert(n.clone());
                            let nn = normalize_binder_name(n).to_string();
                            bound.insert(nn);
                        }
                        let mut hit = false;
                        for (_, rhs) in bindings {
                            hit |= refs_forbidden(rhs, forbidden, bound);
                        }
                        hit |= refs_forbidden(body, forbidden, bound);
                        for n in &inner_names {
                            bound.remove(n);
                            let nn = normalize_binder_name(n);
                            bound.remove(nn);
                        }
                        hit
                    }
                    Op::List { items } | Op::Tuple { items } => {
                        items.iter().any(|x| refs_forbidden(x, forbidden, bound))
                    }
                    Op::Record { fields } | Op::ModeMap { fields, .. } => fields
                        .iter()
                        .any(|f| refs_forbidden(&f.value, forbidden, bound)),
                    Op::Select { target, .. } => refs_forbidden(target, forbidden, bound),
                    _ => false,
                }
            }

            let mut forbidden: HashSet<String> = HashSet::new();
            for (n, _) in bindings {
                forbidden.insert(n.clone());
                forbidden.insert(normalize_binder_name(n).to_string());
            }
            for (name, rhs) in bindings {
                let mut bound: HashSet<String> = HashSet::new();
                if refs_forbidden(rhs, &forbidden, &mut bound) {
                    return Err(unsupported_with_hint(
                        &t.op,
                        Some(format!(
                            "LLVM i64-only lowering supports only non-recursive let bindings; RHS of '{}' must not reference any let-bound names.",
                            name
                        )),
                    ));
                }
            }

            let mut restore: Vec<(String, Option<LlvmValue>)> = vec![];
            for (name, rhs) in bindings {
                let v = lower_expr_i64(em, env, rhs)?;
                let normalized_str = normalize_binder_name(name);
                let normalized = normalized_str.to_string();
                let prev = env.insert(normalized.clone(), v.clone());
                restore.push((normalized, prev));
                if name.as_str() != normalized_str {
                    let prev = env.insert(name.clone(), v);
                    restore.push((name.clone(), prev));
                }
            }

            let result = lower_expr_i64(em, env, body);
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
        Op::App { .. } => {
            let (head, args) = collect_apps(t);
            match (&head.op, args.as_slice()) {
                (Op::Ctor(name), [cond, then_, else_]) if name == "if" => {
                    // i64-only PoC: lower (if cond then else) as br+phi.
                    let then_label = em.fresh_block_label("then");
                    let else_label = em.fresh_block_label("else");
                    let merge_label = em.fresh_block_label("merge");

                    let cond_i1 = lower_cond_i1(em, env, cond)?;
                    em.emit_br_cond(cond_i1, &then_label, &else_label)?;

                    em.start_block(then_label.clone());
                    let then_val = lower_expr_i64(em, env, then_)?;
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(else_label.clone());
                    let else_val = lower_expr_i64(em, env, else_)?;
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(merge_label.clone());
                    em.emit_phi_i64(&then_label, then_val, &else_label, else_val)
                }
                (Op::Ctor(name), [left, right]) if name == "and" => {
                    // i64-only Bool-like short-circuit: (and a b)
                    // Evaluate `a`; if falsy return 0, else evaluate `b` and return bool(b).
                    let then_label = em.fresh_block_label("then");
                    let else_label = em.fresh_block_label("else");
                    let merge_label = em.fresh_block_label("merge");

                    let a_i1 = lower_cond_i1(em, env, left)?;
                    em.emit_br_cond(a_i1, &then_label, &else_label)?;

                    em.start_block(then_label.clone());
                    let b_i1 = lower_cond_i1(em, env, right)?;
                    let b_i64 = em.emit_zext_i1_to_i64(b_i1)?;
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(else_label.clone());
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(merge_label.clone());
                    em.emit_phi_i64(&then_label, b_i64, &else_label, LlvmValue::ConstI64(0))
                }
                (Op::Ctor(name), [left, right]) if name == "or" => {
                    // i64-only Bool-like short-circuit: (or a b)
                    // Evaluate `a`; if truthy return 1, else evaluate `b` and return bool(b).
                    let then_label = em.fresh_block_label("then");
                    let else_label = em.fresh_block_label("else");
                    let merge_label = em.fresh_block_label("merge");

                    let a_i1 = lower_cond_i1(em, env, left)?;
                    em.emit_br_cond(a_i1, &then_label, &else_label)?;

                    em.start_block(then_label.clone());
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(else_label.clone());
                    let b_i1 = lower_cond_i1(em, env, right)?;
                    let b_i64 = em.emit_zext_i1_to_i64(b_i1)?;
                    em.emit_br_uncond(&merge_label)?;

                    em.start_block(merge_label.clone());
                    em.emit_phi_i64(&then_label, LlvmValue::ConstI64(1), &else_label, b_i64)
                }
                (Op::Ctor(name), [x]) if name == "not" => {
                    // Bool-like negation: (not x) where x is i64 truthy.
                    let xv = lower_expr_i64(em, env, x)?;
                    if xv.ty() != LlvmTy::I64 {
                        return Err(unsupported(format!(
                            "LLVM lowering type mismatch for not: expected i64, got {:?}",
                            xv.ty()
                        )));
                    }
                    let c = em.emit_icmp_i64("eq", xv, LlvmValue::ConstI64(0))?;
                    em.emit_zext_i1_to_i64(c)
                }
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
/// Current scope (int-only PoC): supports `Int` literals, Bool literals (as i64 0/1), constructor `True/False` (as i64 1/0), i64-only `if`, i64-only `and/or` (short-circuit), i64-only `not`, i64-only `~seq` and `~chain`, and saturated
/// applications of `~add/~sub/~mul/~div/~eq/~ne/~lt/~le/~gt/~ge` (as `Ref("add"|...)`) producing an `i64` `main` result.
pub fn lower_to_llvm_ir_text(term: &Term) -> Result<String, LlvmIrLowerError> {
    let mut em = Emitter::new();
    let mut env: HashMap<String, LlvmValue> = HashMap::new();
    let v = lower_expr_i64(&mut em, &mut env, term)?;
    if v.ty() != LlvmTy::I64 {
        return Err(unsupported("LLVM lowering produced a non-i64 result; only i64 is supported in this PoC.".into()));
    }

    em.emit_ret_i64(v)?;

    let mut out = String::new();
    out.push_str("; lzscr-llvmir (PoC)\n");
    out.push_str("target triple = \"unknown-unknown-unknown\"\n\n");
    out.push_str("define i64 @main() {\n");
    out.push_str(&em.render_function_body()?);
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
        fn lowers_if_true_br_phi_i64_only() {
                // CoreIR observed shape: (((if True) 1) 2)
                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), Term::new(Op::Ctor("True".into()))),
                                int_(1),
                        ),
                        int_(2),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 1, 0
  br i1 %0, label %then0, label %else1
then0:
  br label %merge2
else1:
  br label %merge2
merge2:
  %1 = phi i64 [ 1, %then0 ], [ 2, %else1 ]
  ret i64 %1
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_if_with_lt_condition_br_phi_i64_only() {
                // (if (1 < 2) 10 20) with `lt` producing i64 0/1, then converted to i1 via `icmp ne`.
                let cond = app(app(ref_("lt"), int_(1)), int_(2));
                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), cond),
                                int_(10),
                        ),
                        int_(20),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp slt i64 1, 2
  %1 = zext i1 %0 to i64
  %2 = icmp ne i64 %1, 0
  br i1 %2, label %then0, label %else1
then0:
  br label %merge2
else1:
  br label %merge2
merge2:
  %3 = phi i64 [ 10, %then0 ], [ 20, %else1 ]
  ret i64 %3
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_if_with_truthy_i64_condition_br_phi_i64_only() {
                // (if 42 1 2): in i64-only subset we treat non-zero as true.
                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), int_(42)),
                                int_(1),
                        ),
                        int_(2),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 42, 0
  br i1 %0, label %then0, label %else1
then0:
  br label %merge2
else1:
  br label %merge2
merge2:
  %1 = phi i64 [ 1, %then0 ], [ 2, %else1 ]
  ret i64 %1
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_if_with_zero_condition_br_phi_i64_only() {
                // (if 0 1 2): 0 is falsy (still lowered via icmp+br+phi, no constant-folding).
                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), int_(0)),
                                int_(1),
                        ),
                        int_(2),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 0, 0
  br i1 %0, label %then0, label %else1
then0:
  br label %merge2
else1:
  br label %merge2
merge2:
  %1 = phi i64 [ 1, %then0 ], [ 2, %else1 ]
  ret i64 %1
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_if_with_false_ctor_condition_br_phi_i64_only() {
                // (if False 1 2): constructor False is lowered as i64 0.
                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), Term::new(Op::Ctor("False".into()))),
                                int_(1),
                        ),
                        int_(2),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 0, 0
  br i1 %0, label %then0, label %else1
then0:
  br label %merge2
else1:
  br label %merge2
merge2:
  %1 = phi i64 [ 1, %then0 ], [ 2, %else1 ]
  ret i64 %1
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_if_with_seq_and_chain_in_branches_i64_only() {
                // (if True (~seq ((~mul 2) 3) ((~add 10) 20)) (~chain ((~mul 4) 5) ((~add 6) 7)))
                let then_first = app(app(ref_("mul"), int_(2)), int_(3));
                let then_second = app(app(ref_("add"), int_(10)), int_(20));
                let then_ = Term::new(Op::Seq { first: Box::new(then_first), second: Box::new(then_second) });

                let else_first = app(app(ref_("mul"), int_(4)), int_(5));
                let else_second = app(app(ref_("add"), int_(6)), int_(7));
                let else_ = Term::new(Op::Chain { first: Box::new(else_first), second: Box::new(else_second) });

                let t = app(
                        app(
                                app(Term::new(Op::Ctor("if".into())), Term::new(Op::Ctor("True".into()))),
                                then_,
                        ),
                        else_,
                );

                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 1, 0
  br i1 %0, label %then0, label %else1
then0:
  %1 = mul i64 2, 3
  %2 = add i64 10, 20
  br label %merge2
else1:
  %3 = mul i64 4, 5
  %4 = add i64 6, 7
  br label %merge2
merge2:
  %5 = phi i64 [ %2, %then0 ], [ %4, %else1 ]
  ret i64 %5
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_and_short_circuit_i64_only() {
                // (and 0 5) : short-circuit should route false branch to 0.
                let t = app(
                        app(Term::new(Op::Ctor("and".into())), int_(0)),
                        int_(5),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 0, 0
  br i1 %0, label %then0, label %else1
then0:
  %1 = icmp ne i64 5, 0
  %2 = zext i1 %1 to i64
  br label %merge2
else1:
  br label %merge2
merge2:
  %3 = phi i64 [ %2, %then0 ], [ 0, %else1 ]
  ret i64 %3
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_or_short_circuit_i64_only() {
                // (or 7 0) : short-circuit should route true branch to 1.
                let t = app(
                        app(Term::new(Op::Ctor("or".into())), int_(7)),
                        int_(0),
                );
                let ir = lower_to_llvm_ir_text(&t).expect("lower");
                let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = icmp ne i64 7, 0
  br i1 %0, label %then0, label %else1
then0:
  br label %merge2
else1:
  %1 = icmp ne i64 0, 0
  %2 = zext i1 %1 to i64
  br label %merge2
merge2:
  %3 = phi i64 [ 1, %then0 ], [ %2, %else1 ]
  ret i64 %3
}
"#;
                assert_eq!(ir, expected);
        }

        #[test]
        fn lowers_not_i64_only() {
            // (not 0) => 1
            let t = app(Term::new(Op::Ctor("not".into())), int_(0));
            let ir = lower_to_llvm_ir_text(&t).expect("lower");
            let expected = concat!(
                r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
"#,
                "  %0 = icmp eq i64 0, 0\n",
                "  %1 = zext i1 %0 to i64\n",
                "  ret i64 %1\n",
                "}\n",
            );
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

    #[test]
    fn lowers_letrec_non_recursive_i64_only() {
        // letrec { x = 5 } in (~add x 2)
        let body = app(app(ref_("add"), ref_("x")), int_(2));
        let t = Term::new(Op::LetRec { bindings: vec![("x".into(), int_(5))], body: Box::new(body) });
        let ir = lower_to_llvm_ir_text(&t).expect("lower");
        let expected = r#"; lzscr-llvmir (PoC)
target triple = "unknown-unknown-unknown"

define i64 @main() {
entry:
  %0 = add i64 5, 2
  ret i64 %0
}
"#;
        assert_eq!(ir, expected);
    }

    #[test]
    fn letrec_rejects_recursive_rhs_i64_only() {
        // letrec { x = x } in x
        let t = Term::new(Op::LetRec {
            bindings: vec![("x".into(), ref_("x"))],
            body: Box::new(ref_("x")),
        });
        let err = lower_to_llvm_ir_text(&t).unwrap_err();
        let msg = format!("{err}");
        assert!(msg.contains("non-recursive"), "error: {msg}");
    }
}
