//! lzscr-types: HM type inference, unification, schemes, diagnostics (occurs/mismatch).
//!
//! Status:
//! - Stable: unify/instantiate/generalize, occurs-check, basic pretty print
//! - Improving: dual-caret errors (MismatchBoth/RecordField…), occurs normalization
//!
//! Notes:
//! - apply(): single-pass structural substitution（安価）
//! - zonk(): 置換連鎖を固定点まで展開（最終出力/表示前に使用）
//! - ftv(): 自由型変数の収集（必要に応じて apply/zonk 後に）
//!
//! Source of truth: docs/ROADMAP.md
//!
//! Implements a minimal rank-1 HM inference over lzscr AST, including AltLambda
//! typing with Ctor-limited union (SumCtor) for lambda chains.
//!
//! # Module Structure (Refactored 2025-12-02)
//!
//! This crate has been split into multiple focused modules:
//! - `types`: Core type representations (`Type`, `TvId`)
//! - `error`: Type errors and suggestion helpers (`TypeError`, `edit_distance`)
//! - `builtins`: Built-in type constructors (bool, option, result, fs effects)
//! - `scheme`: Type schemes, substitutions, environments (Scheme, Subst, TypeEnv)
//! - `typeexpr`: Type expression conversion and typedef management
//! - `unification`: Type unification algorithm (Robinson's algorithm with extensions)
//! - Other modules to be extracted: inference, display

mod builtins;
mod display;
mod error;
mod scheme;
mod typeexpr;
mod types;
mod unification;

// Re-export core types
pub use builtins::{
    bool_sum_type, effect_signature, fs_effects_record_type, fs_metadata_record_type, option_type,
    result_list_str_type, result_metadata_type, result_str_str_type, result_unit_str_type,
};
pub use error::{find_similar_names, format_field_path, TypeError};
pub use scheme::{
    generalize, instantiate, normalize_tuples, zonk_type, Scheme, Subst, TvGen, TypeEnv, TypesApply,
};
pub use typeexpr::{
    build_typedefs_frame, build_typename_frame, conv_typeexpr_fresh, validate_typedecls_positive,
    TypeDefsFrame, TypeNameDef, TypeNameDefsFrame,
};
use typeexpr::{typedefs_lookup_ctor, typedefs_lookup_typename};
pub use types::{TvId, Type};
use display::{normalize_type_and_map, pp_type, user_pretty_type, user_pretty_type_and_map};
use unification::{ctx_unify, unify};

use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use std::collections::{BTreeMap, HashMap, HashSet};

// ---------- Inference Context and Pattern Matching ----------
// ---------- Pattern inference ----------

#[derive(Default)]
struct PatInfo {
    bindings: Vec<(String, Type)>,
    subst: Subst,
}

#[allow(clippy::result_large_err)]
fn infer_pattern(
    ctx: &mut InferCtx,
    pat: &Pattern,
    scrutinee: &Type,
) -> Result<PatInfo, TypeError> {
    match &pat.kind {
        PatternKind::Wildcard => Ok(PatInfo::default()),
        PatternKind::Var(n) => {
            Ok(PatInfo { bindings: vec![(n.clone(), scrutinee.clone())], subst: Subst::new() })
        }
        PatternKind::Unit => {
            ctx_unify(ctx, scrutinee, &Type::Unit).map(|s| PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::Int(_) => {
            ctx_unify(ctx, scrutinee, &Type::Int).map(|s| PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::Float(_) => {
            ctx_unify(ctx, scrutinee, &Type::Float).map(|s| PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::Str(_) => {
            ctx_unify(ctx, scrutinee, &Type::Str).map(|s| PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::Char(_) => {
            ctx_unify(ctx, scrutinee, &Type::Char).map(|s| PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::Tuple(xs) => {
            let mut s = Subst::new();
            let mut tys = Vec::with_capacity(xs.len());
            for _ in xs {
                tys.push(ctx.fresh_tv());
            }
            let tup = Type::Tuple(tys.clone());
            let s0 = ctx_unify(ctx, &scrutinee.apply(&s), &tup)?;
            s = s0.compose(s);
            let mut binds = vec![];
            for (p, t_elem) in xs.iter().zip(tys.into_iter()) {
                let pi = infer_pattern(ctx, p, &t_elem.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::List(items) => {
            let a = ctx.fresh_tv();
            let mut s = ctx_unify(ctx, scrutinee, &Type::List(Box::new(a.clone())))?;
            let mut binds = vec![];
            for p in items {
                let pi = infer_pattern(ctx, p, &a.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Cons(h, t) => {
            let a = ctx.fresh_tv();
            let s0 = ctx_unify(ctx, scrutinee, &Type::List(Box::new(a.clone())))?;
            let mut s = s0;
            let ph = infer_pattern(ctx, h, &a.apply(&s))?;
            s = ph.subst.compose(s);
            let pt = infer_pattern(ctx, t, &Type::List(Box::new(a.apply(&s))))?;
            s = pt.subst.compose(s);
            let mut binds = ph.bindings;
            binds.extend(pt.bindings);
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Record(fields) => {
            let mut want = BTreeMap::new();
            for (k, _) in fields {
                want.insert(k.clone(), ctx.fresh_tv());
            }
            let want_spanned =
                Type::Record(want.iter().map(|(k, v)| (k.clone(), (v.clone(), None))).collect());
            let s0 = ctx_unify(ctx, scrutinee, &want_spanned)?;
            let mut s = s0;
            let mut binds = vec![];
            for (k, p) in fields {
                let tfield = want.get(k).unwrap().apply(&s);
                let pi = infer_pattern(ctx, p, &tfield)?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Ctor { name, args } => {
            // Special-case tuple-like ctor tags '.,', '.,,', ... in patterns.
            // If written as a single tuple argument whose arity matches the tag, expand to that many args.
            let (eff_name, eff_args): (String, Vec<Pattern>) = if name.starts_with('.')
                && name.chars().skip(1).all(|c| c == ',')
                && args.len() == 1
            {
                if let PatternKind::Tuple(items) = &args[0].kind {
                    let total_arity = name.chars().skip(1).count() + 1;
                    if items.len() == total_arity {
                        (name.clone(), items.clone())
                    } else {
                        (name.clone(), args.clone())
                    }
                } else {
                    (name.clone(), args.clone())
                }
            } else {
                (name.clone(), args.clone())
            };

            let payload: Vec<Type> = if let Some(tmpls) = typedefs_lookup_ctor(ctx, &eff_name) {
                if tmpls.len() == eff_args.len() {
                    // Clone templates to drop immutable borrow of ctx before mutably borrowing ctx.tv
                    let tmpls_vec = tmpls.clone();
                    let mut out = Vec::with_capacity(tmpls_vec.len());
                    for te in tmpls_vec.iter() {
                        out.push(conv_typeexpr_fresh(&mut ctx.tv, te));
                    }
                    out
                } else {
                    (0..eff_args.len()).map(|_| ctx.fresh_tv()).collect()
                }
            } else {
                (0..eff_args.len()).map(|_| ctx.fresh_tv()).collect()
            };
            let want = Type::Ctor { tag: eff_name.clone(), payload: payload.clone() };
            let mut s = ctx_unify(ctx, scrutinee, &want)?;
            let mut binds = vec![];
            for (p, ta) in eff_args.iter().zip(payload.into_iter()) {
                let pi = infer_pattern(ctx, p, &ta.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Symbol(sym) => {
            let want = Type::Ctor { tag: sym.clone(), payload: vec![] };
            let s = ctx_unify(ctx, scrutinee, &want)?;
            Ok(PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::As(a, b) => {
            let pa = infer_pattern(ctx, a, scrutinee)?;
            let pb = infer_pattern(ctx, b, &scrutinee.apply(&pa.subst))?;
            let mut binds = pa.bindings;
            binds.extend(pb.bindings);
            Ok(PatInfo { bindings: binds, subst: pb.subst.compose(pa.subst) })
        }
        PatternKind::TypeBind { pat, .. } => {
            // Transparent for pattern typing; scoping handled by callers (lambda/let/alt-lambda)
            infer_pattern(ctx, pat, scrutinee)
        }
    }
}

// ---------- Inference ----------

struct InferCtx {
    tv: TvGen,
    env: TypeEnv,
    tyvars: Vec<HashMap<String, TvId>>, // stack of frames; lookup is from last to first
    typedefs: Vec<TypeDefsFrame>,       // stack of ctor templates from % declarations
    typedef_types: Vec<TypeNameDefsFrame>, // stack of named type defs (μ-types)
    debug: Option<std::rc::Rc<std::cell::RefCell<DebugConfig>>>, // optional debugging
    depth: usize,                       // current AST depth for logging
    tv_origins: HashMap<TvId, Span>,    // origin span for each generated type variable
    span_stack: Vec<Span>,              // expression span context stack
}

#[derive(Clone)]
struct DebugConfig {
    level: usize,
    max_depth: usize,
    logs: Vec<String>,
    log_unify: bool,
    log_env: bool,
    log_schemes: bool,
}

impl DebugConfig {
    fn log(&mut self, depth: usize, lvl: usize, msg: impl Into<String>) {
        if lvl <= self.level && depth <= self.max_depth {
            self.logs.push(format!("[d{depth} l{lvl}] {}", msg.into()));
        }
    }
    fn dump_env(&mut self, depth: usize, env: &TypeEnv) {
        if self.log_env && depth <= self.max_depth {
            let mut entries: Vec<_> = env.0.iter().collect();
            entries.sort_by(|a, b| a.0.cmp(b.0));
            for (k, sc) in entries.into_iter().take(64) {
                self.logs.push(format!("[d{depth} env] {} : {}", k, pp_type(&sc.ty)));
            }
        }
    }
    fn log_scheme(&mut self, depth: usize, name: &str, sc: &Scheme) {
        if self.log_schemes && depth <= self.max_depth {
            self.logs.push(format!("[d{depth} scheme] {} :: {}", name, pp_type(&sc.ty)));
        }
    }
}

fn lookup_tyvar(ctx: &InferCtx, name: &str) -> Option<TvId> {
    for frame in ctx.tyvars.iter().rev() {
        if let Some(id) = frame.get(name) {
            return Some(*id);
        }
    }
    None
}

impl InferCtx {
    fn push_span(&mut self, sp: Span) {
        self.span_stack.push(sp);
    }
    fn pop_span(&mut self) {
        self.span_stack.pop();
    }
    fn current_span(&self) -> Option<Span> {
        self.span_stack.last().copied()
    }
    fn fresh_tv(&mut self) -> Type {
        let t = self.tv.fresh();
        if let Type::Var(id) = t {
            if let Some(sp) = self.current_span() {
                self.tv_origins.insert(id, sp);
            }
        }
        t
    }
}

// (Removed SpanGuard due to borrow conflicts; using explicit push/pop in infer_expr)

fn push_tyvars_from_pattern<'a>(ctx: &mut InferCtx, p: &'a Pattern) -> (&'a Pattern, usize) {
    let mut cur = p;
    let mut pushed = 0usize;
    while let PatternKind::TypeBind { tvars, pat } = &cur.kind {
        let mut frame = HashMap::new();
        for nm in tvars {
            // '' (empty) represents anonymous '?', we still allocate a distinct id but not bound to a name for lookup
            if nm.is_empty() {
                continue;
            }
            let Type::Var(id) = ctx.tv.fresh() else { unreachable!() }; // keep raw fresh: type bind generics shouldn't inherit expr span
            frame.insert(nm.clone(), id);
        }
        ctx.tyvars.push(frame);
        pushed += 1;
        cur = pat;
    }
    (cur, pushed)
}

fn pop_tyvars(ctx: &mut InferCtx, n: usize) {
    for _ in 0..n {
        let _ = ctx.tyvars.pop();
    }
}

#[allow(clippy::result_large_err)]
fn infer_expr(
    ctx: &mut InferCtx,
    e: &Expr,
    allow_effects: bool,
) -> Result<(Type, Subst), TypeError> {
    // Enter logging
    if let Some(dbg) = &ctx.debug {
        dbg.borrow_mut().log(ctx.depth, 1, format!("enter {}", short_expr_kind(&e.kind)));
    }
    // Push current expression span so any fresh type variables generated within acquire an origin.
    ctx.push_span(e.span);
    ctx.depth += 1;
    // Helper: convert TypeExpr to Type using ctx.tyvars and a local holes table for '?'
    // Helper: try to find the span of a constructor symbol (e.g., .Some/.None) in an expression.
    fn find_ctor_symbol_span_in_expr(expr: &Expr) -> Option<Span> {
        match &expr.kind {
            ExprKind::Symbol(_) => Some(expr.span),
            ExprKind::Apply { func, .. } => find_ctor_symbol_span_in_expr(func),
            ExprKind::Block(b) => find_ctor_symbol_span_in_expr(b),
            ExprKind::Lambda { param, body } => {
                // Also check lambda params (multi-parameter lambdas are nested)
                find_ctor_symbol_span_in_pattern(param)
                    .or_else(|| find_ctor_symbol_span_in_expr(body))
            }
            _ => None,
        }
    }

    // Helper: pattern search for ctor/symbol head; returns a tight caret span at pattern start.
    fn find_ctor_symbol_span_in_pattern(p: &Pattern) -> Option<Span> {
        match &p.kind {
            PatternKind::Ctor { .. } | PatternKind::Symbol(_) => {
                // Point to the start of the pattern (at '.') with len=1 for a tight caret
                Some(Span::new(p.span.offset, 1))
            }
            PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                for x in xs {
                    if let Some(sp) = find_ctor_symbol_span_in_pattern(x) {
                        return Some(sp);
                    }
                }
                None
            }
            PatternKind::Record(fs) => {
                for (_k, v) in fs {
                    if let Some(sp) = find_ctor_symbol_span_in_pattern(v) {
                        return Some(sp);
                    }
                }
                None
            }
            PatternKind::Cons(a, b) | PatternKind::As(a, b) => {
                find_ctor_symbol_span_in_pattern(a).or_else(|| find_ctor_symbol_span_in_pattern(b))
            }
            PatternKind::TypeBind { pat, .. } => find_ctor_symbol_span_in_pattern(pat),
            _ => None,
        }
    }
    fn conv_typeexpr(ctx: &mut InferCtx, te: &TypeExpr, holes: &mut HashMap<String, TvId>) -> Type {
        match te {
            TypeExpr::Unit => Type::Unit,
            TypeExpr::Int => Type::Int,
            TypeExpr::Float => Type::Float,
            TypeExpr::Bool => bool_sum_type(),
            TypeExpr::Str => Type::Str,
            TypeExpr::Char => Type::Char,
            TypeExpr::List(t) => Type::List(Box::new(conv_typeexpr(ctx, t, holes))),
            TypeExpr::Tuple(xs) => {
                Type::Tuple(xs.iter().map(|t| conv_typeexpr(ctx, t, holes)).collect())
            }
            TypeExpr::Record(fs) => {
                let mut m = BTreeMap::new();
                for (k, v) in fs {
                    m.insert(k.clone(), conv_typeexpr(ctx, v, holes));
                }
                Type::Record(m.into_iter().map(|(k, v)| (k, (v, None))).collect())
            }
            TypeExpr::Fun(a, b) => {
                Type::fun(conv_typeexpr(ctx, a, holes), conv_typeexpr(ctx, b, holes))
            }
            TypeExpr::Ctor { tag, args } => {
                let conv_args =
                    args.iter().map(|t| conv_typeexpr(ctx, t, holes)).collect::<Vec<_>>();
                if typedefs_lookup_typename(ctx, tag).is_some() {
                    Type::Named { name: tag.clone(), args: conv_args }
                } else {
                    Type::Ctor { tag: tag.clone(), payload: conv_args }
                }
            }
            TypeExpr::Var(name) => {
                if let Some(id) = lookup_tyvar(ctx, name) {
                    Type::Var(id)
                } else {
                    // Unbound type var in annotation: treat as a shared hole keyed by the name
                    let key = format!("'{}", name);
                    let id = holes.entry(key).or_insert_with(|| {
                        let Type::Var(v) = ctx.fresh_tv() else { unreachable!() };
                        v
                    });
                    Type::Var(*id)
                }
            }
            TypeExpr::Hole(opt) => {
                if let Some(n) = opt.as_ref() {
                    let key = format!("?{}", n);
                    let id = holes.entry(key).or_insert_with(|| {
                        let Type::Var(v) = ctx.fresh_tv() else { unreachable!() };
                        v
                    });
                    Type::Var(*id)
                } else {
                    let Type::Var(v) = ctx.fresh_tv() else { unreachable!() };
                    Type::Var(v)
                }
            }
        }
    }

    let result = match &e.kind {
        ExprKind::Annot { ty, expr } => {
            let (got, s) = infer_expr(ctx, expr, allow_effects)?;
            let mut holes = HashMap::new();
            let want = conv_typeexpr(ctx, ty, &mut holes);
            match ctx_unify(ctx, &got.apply(&s), &want) {
                Ok(s2) => Ok((want.apply(&s2), s2.compose(s))),
                Err(TypeError::Mismatch { expected, actual, .. }) => {
                    let annot_len = expr.span.offset.saturating_sub(e.span.offset);
                    Err(TypeError::AnnotMismatch {
                        expected,
                        actual,
                        annot_span_offset: e.span.offset,
                        annot_span_len: annot_len,
                        expr_span_offset: expr.span.offset,
                        expr_span_len: expr.span.len,
                    })
                }
                Err(other) => Err(other),
            }
        }
        ExprKind::TypeVal(_ty) => Ok((Type::Type, Subst::new())),
        ExprKind::Unit => Ok((Type::Unit, Subst::new())),
        ExprKind::Int(_) => Ok((Type::Int, Subst::new())),
        ExprKind::Float(_) => Ok((Type::Float, Subst::new())),
        ExprKind::Str(_) => Ok((Type::Str, Subst::new())),
        ExprKind::Char(_) => Ok((Type::Char, Subst::new())),
        ExprKind::Ref(n) => {
            let s = ctx.env.get(n).ok_or_else(|| {
                let suggestions = find_similar_names(n, &ctx.env);
                TypeError::UnboundRef {
                    name: n.clone(),
                    span_offset: e.span.offset,
                    span_len: e.span.len,
                    suggestions,
                }
            })?;
            let inst = instantiate(&mut ctx.tv, &s);
            if let Some(dbg) = &ctx.debug {
                dbg.borrow_mut().log(ctx.depth, 1, format!("scheme {} => {}", n, pp_type(&s.ty)));
                dbg.borrow_mut().log(ctx.depth, 1, format!("inst {} => {}", n, pp_type(&inst)));
            }
            Ok((inst, Subst::new()))
        }
        ExprKind::Symbol(name) => {
            if name == ".True" || name == ".False" {
                Ok((Type::Ctor { tag: name.clone(), payload: vec![] }, Subst::new()))
            } else {
                // Treat other bare symbol as 0-arity ctor
                Ok((Type::Ctor { tag: name.clone(), payload: vec![] }, Subst::new()))
            }
        }
        ExprKind::Lambda { param, body } => {
            // Handle pattern-level type binders: push frames while inferring param and body
            let (p_core, pushed) = push_tyvars_from_pattern(ctx, param);
            let a = ctx.fresh_tv();
            let pi = infer_pattern(ctx, p_core, &a)?;
            let mut env2 = ctx.env.clone();
            for (n, t) in &pi.bindings {
                env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
            }
            let prev = std::mem::replace(&mut ctx.env, env2);
            let (bt, s_body) = infer_expr(ctx, body, allow_effects)?;
            ctx.env = prev;
            pop_tyvars(ctx, pushed);
            let ty = Type::fun(a.apply(&s_body).apply(&pi.subst), bt.apply(&s_body));
            Ok((ty, s_body.compose(pi.subst)))
        }
        ExprKind::Apply { func, arg } => {
            if let ExprKind::Ref(eff_ref) = &func.kind {
                if eff_ref == "effects" {
                    if let ExprKind::Symbol(sym) = &arg.kind {
                        if let Some(sig) = effect_signature(sym) {
                            return Ok((sig, Subst::new()));
                        }
                    }
                }
            }
            // Special-case: (~if cond then else)
            // Recognize nested application: (((if cond) then) else)
            if let ExprKind::Apply { func: f_then, arg: then_branch } = &func.kind {
                if let ExprKind::Apply { func: f_if, arg: cond } = &f_then.kind {
                    if let ExprKind::Ref(if_name) = &f_if.kind {
                        if if_name == "if" {
                            // Infer cond, then, else
                            let (tc, sc) = infer_expr(ctx, cond, allow_effects)?;
                            let (tt, st) = infer_expr(ctx, then_branch, allow_effects)?;
                            let (te, se) = infer_expr(ctx, arg, allow_effects)?;
                            // cond : Bool (union form)
                            let s0 = ctx_unify(ctx, &tc.apply(&st).apply(&sc), &bool_sum_type())?;
                            let s_acc = s0.compose(st).compose(sc);
                            // helper to merge ctor-like types into a finite union
                            fn is_ctor_like(t: &Type) -> bool {
                                matches!(t, Type::Ctor { .. } | Type::SumCtor(_))
                            }
                            fn merge_variants(
                                ctx: &mut InferCtx,
                                acc: &mut BTreeMap<String, Vec<Type>>,
                                t: &Type,
                            ) -> Result<Subst, TypeError> {
                                let mut s = Subst::new();
                                match t {
                                    Type::Ctor { tag, payload } => {
                                        if let Some(existing) = acc.get_mut(tag) {
                                            if existing.len() != payload.len() {
                                                return Err(TypeError::Mismatch {
                                                    expected: Type::Ctor {
                                                        tag: tag.clone(),
                                                        payload: existing.clone(),
                                                    },
                                                    actual: t.clone(),
                                                    span_offset: 0,
                                                    span_len: 0,
                                                });
                                            }
                                            for (x, y) in existing.iter_mut().zip(payload.iter()) {
                                                let s1 =
                                                    ctx_unify(ctx, &x.apply(&s), &y.apply(&s))?;
                                                *x = x.apply(&s1);
                                                s = s1.compose(s);
                                            }
                                        } else {
                                            acc.insert(tag.clone(), payload.clone());
                                        }
                                    }
                                    Type::SumCtor(vs) => {
                                        for (tag, payload) in vs {
                                            let tmp = Type::Ctor {
                                                tag: tag.clone(),
                                                payload: payload.clone(),
                                            };
                                            let s1 = merge_variants(ctx, acc, &tmp)?;
                                            s = s1.compose(s);
                                        }
                                    }
                                    _ => {}
                                }
                                Ok(s)
                            }
                            let mut acc: BTreeMap<String, Vec<Type>> = BTreeMap::new();
                            let t1 = tt.apply(&s_acc).apply(&se);
                            let t2 = te.apply(&s_acc).apply(&se);
                            if is_ctor_like(&t1) && is_ctor_like(&t2) {
                                let s1 = merge_variants(ctx, &mut acc, &t1)?
                                    .compose(merge_variants(ctx, &mut acc, &t2)?);
                                // Build union type from acc
                                let variants: Vec<(String, Vec<Type>)> = acc.into_iter().collect();
                                let ret = Type::SumCtor(variants);
                                let s = s1.compose(se).compose(s_acc);
                                return Ok((ret.apply(&s), s));
                            }
                            // Fallback to regular if typing: unify branch types to a common var
                            let r = ctx.fresh_tv();
                            let s1 = ctx_unify(ctx, &tt.apply(&se).apply(&s_acc), &r)?;
                            // 2回目の unify 失敗時に then/else のスパン（可能なら .Some/.None の記号位置）を添えて返す
                            let s2 =
                                match ctx_unify(ctx, &te.apply(&s1).apply(&se).apply(&s_acc), &r) {
                                    Ok(s) => s,
                                    Err(TypeError::Mismatch { expected, actual, .. }) => {
                                        let exp_sp = find_ctor_symbol_span_in_expr(then_branch)
                                            .unwrap_or(then_branch.span);
                                        let act_sp =
                                            find_ctor_symbol_span_in_expr(arg).unwrap_or(arg.span);
                                        return Err(TypeError::MismatchBoth {
                                            expected,
                                            actual,
                                            expected_span_offset: exp_sp.offset,
                                            expected_span_len: exp_sp.len,
                                            actual_span_offset: act_sp.offset,
                                            actual_span_len: act_sp.len,
                                        });
                                    }
                                    Err(e) => return Err(e),
                                };
                            let s = s2.compose(s1).compose(se).compose(s_acc);
                            return Ok((r.apply(&s), s));
                        }
                    }
                }
            }
            // Special-case: constructor application via symbol, e.g., (.Some x) or (., a) b or (.,, a) b c
            if let ExprKind::Symbol(tag) = &func.kind {
                if tag.starts_with('.') {
                    let (ta, sa) = infer_expr(ctx, arg, allow_effects)?;
                    // Tuple-like tags: leading '.' followed by one or more ','; total payload arity = (number of commas) + 1
                    if tag.chars().skip(1).all(|c| c == ',') {
                        let n_commas = tag.chars().skip(1).count();
                        let total_arity = n_commas + 1;
                        if total_arity == 0 {
                            // malformed, treat as unary below
                        } else {
                            // We have applied 1 arg now; remaining (total_arity-1) to collect
                            let mut payload = Vec::with_capacity(total_arity);
                            payload.push(ta.apply(&sa));
                            let mut params: Vec<Type> = Vec::new();
                            for _ in 1..total_arity {
                                params.push(ctx.fresh_tv());
                            }
                            payload.extend(params.iter().cloned());
                            let ret = Type::Ctor { tag: tag.clone(), payload };
                            // fold params into function type
                            let ty_fun =
                                params.iter().rev().fold(ret, |acc, p| Type::fun(p.clone(), acc));
                            return Ok((ty_fun, sa));
                        }
                    }
                    {
                        // Default: unary constructor
                        let ty = Type::Ctor { tag: tag.clone(), payload: vec![ta.apply(&sa)] };
                        return Ok((ty, sa));
                    }
                }
            }
            // Special-case: record field access via symbol application.
            // We represent ({a: f, ...} .a) as (~apply record (Symbol "a")),
            // so when arg is Symbol(name) we try to fetch that field's type.
            if let ExprKind::Symbol(field_name_raw) = &arg.kind {
                // Symbols used for record access are dot-prefixed (e.g., ".len"); strip leading dot.
                let field_name = if let Some(stripped) = field_name_raw.strip_prefix('.') {
                    stripped
                } else {
                    field_name_raw.as_str()
                };
                let (tr, sr) = infer_expr(ctx, func, allow_effects)?;
                match tr.apply(&sr) {
                    Type::Record(mut fs) => {
                        if let Some((fty, _sp)) = fs.remove(field_name) {
                            return Ok((fty, sr));
                        }
                        // If the field doesn't exist, fall back to normal application
                        // and let the later unification report a mismatch
                    }
                    _ => {
                        // Fall back to normal function application
                    }
                }
            }
            // Special-case: (~seq a b) allows effects in b only
            if let ExprKind::Apply { func: seq_ref_expr, arg: first } = &func.kind {
                if let ExprKind::Ref(seq_name) = &seq_ref_expr.kind {
                    if seq_name == "seq" {
                        let (_t1, s1) = infer_expr(ctx, first, false)?; // first must be pure
                        let (t2, s2) = infer_expr(ctx, arg, true)?; // second may perform effects
                        let s = s2.compose(s1);
                        return Ok((t2.apply(&s), s));
                    }
                    if seq_name == "chain" {
                        // (~chain a b): allow effects in both a and b; result type is type of b
                        let (_t1, s1) = infer_expr(ctx, first, true)?;
                        let (t2, s2) = infer_expr(ctx, arg, true)?;
                        let s = s2.compose(s1);
                        return Ok((t2.apply(&s), s));
                    }
                    if seq_name == "bind" {
                        // (~bind e k): allow effects in both e and k; unify k : te -> r
                        let (te, se) = infer_expr(ctx, first, true)?;
                        let (tk, sk) = infer_expr(ctx, arg, true)?;
                        let r = ctx.fresh_tv();
                        let s1 = unify(
                            &tk.apply(&sk).apply(&se),
                            &Type::fun(te.apply(&sk).apply(&se), r.clone()),
                        )?;
                        let s = s1.compose(sk).compose(se);
                        return Ok((r.apply(&s), s));
                    }
                }
            }
            let (tf, sf) = infer_expr(ctx, func, allow_effects)?;
            let (ta, sa) = infer_expr(ctx, arg, allow_effects)?;
            let r = ctx.fresh_tv();
            let lhs_fun = tf.apply(&sa).apply(&sf);
            let rhs_fun = Type::fun(ta.apply(&sa), r.clone());
            let s1 = match ctx_unify(ctx, &lhs_fun, &rhs_fun) {
                Ok(s) => s,
                Err(TypeError::Mismatch { expected, actual, span_offset, span_len })
                    if span_offset == 0 && span_len == 0 =>
                {
                    // 推論段階でまだ正確なスパンが無い単純な関数適用型不一致。
                    // 関数式と引数式の双方を指す二重ケアット版に格上げ。
                    return Err(TypeError::MismatchBoth {
                        expected,
                        actual,
                        expected_span_offset: func.span.offset,
                        expected_span_len: func.span.len.max(1),
                        actual_span_offset: arg.span.offset,
                        actual_span_len: arg.span.len.max(1),
                    });
                }
                Err(e) => return Err(e),
            };
            let s = s1.compose(sa).compose(sf);
            // If this is an effect application like ((~effects .sym) x), forbid unless allowed
            if !allow_effects {
                if let ExprKind::Apply { func: inner_f, arg: _ } = &func.kind {
                    if let ExprKind::Ref(eff_name) = &inner_f.kind {
                        if eff_name == "effects" {
                            return Err(TypeError::EffectNotAllowed {
                                span_offset: e.span.offset,
                                span_len: e.span.len,
                            });
                        }
                    }
                }
            }
            Ok((r.apply(&s), s))
        }
        ExprKind::Block(inner) => infer_expr(ctx, inner, allow_effects),
        ExprKind::List(items) => {
            let a = ctx.fresh_tv();
            let mut s = Subst::new();
            for it in items {
                let (ti, si) = infer_expr(ctx, it, allow_effects)?;
                let s1 = ctx_unify(ctx, &ti.apply(&s).apply(&si), &a.apply(&s).apply(&si))?;
                s = s1.compose(si).compose(s);
            }
            Ok((Type::List(Box::new(a.apply(&s))), s))
        }
        ExprKind::Record(fields) => {
            let mut subst = Subst::new();
            let mut map = BTreeMap::new();
            for (k, v) in fields {
                let (tv, sv) = infer_expr(ctx, v, allow_effects)?;
                subst = sv.compose(subst);
                map.insert(k.clone(), (tv.apply(&subst), Some(v.span)));
            }
            Ok((Type::Record(map), subst))
        }
        ExprKind::LetGroup { type_decls, bindings, body, .. } => {
            // Recursive let-group inference (Algorithm W style for letrec):
            // 1) Create monomorphic assumptions for each binder in all patterns using fresh type vars.
            // 2) Infer each RHS under env' = env0 + assumptions.
            // 3) Unify RHS type with pattern-scrutinee and update env' entries with generalized types.
            let env0 = ctx.env.clone();
            // Validate and push type declaration frames (constructors and named types)
            if !type_decls.is_empty() {
                validate_typedecls_positive(type_decls)?;
            }
            let pushed_typedefs = if !type_decls.is_empty() {
                let fr = build_typedefs_frame(type_decls);
                if !fr.is_empty() {
                    ctx.typedefs.push(fr);
                    true
                } else {
                    false
                }
            } else {
                false
            };
            let pushed_typenames = if !type_decls.is_empty() {
                let fr = build_typename_frame(type_decls);
                if !fr.is_empty() {
                    ctx.typedef_types.push(fr);
                    true
                } else {
                    false
                }
            } else {
                false
            };
            // Precompute pattern cores, pushed frames, scrutinee vars, and pattern infos
            struct PreBind {
                _p_core: Pattern,
                pushed: usize,
                a_scrut: Type,
                pat_info: PatInfo,
            }
            let mut pres: Vec<PreBind> = Vec::with_capacity(bindings.len());
            // Build assumptions env with placeholders
            let mut env_assume = env0.clone();
            for (p, _ex) in bindings.iter() {
                let (p_core, pushed) = push_tyvars_from_pattern(ctx, p);
                let a = ctx.fresh_tv();
                let pi = infer_pattern(ctx, p_core, &a)?;
                // Insert monomorphic placeholders for each bound name
                for (n, t) in &pi.bindings {
                    env_assume.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                }
                // Debug: log binding variable names extracted from pattern (helps diagnose missing 'append')
                if let Some(dbg) = &ctx.debug {
                    if !pi.bindings.is_empty() {
                        let names: Vec<String> =
                            pi.bindings.iter().map(|(n, _)| n.clone()).collect();
                        dbg.borrow_mut().log(ctx.depth, 1, format!("bind vars={:?}", names));
                    } else {
                        dbg.borrow_mut().log(ctx.depth, 1, "bind vars=[] (no pattern vars)");
                    }
                }
                pres.push(PreBind { _p_core: p_core.clone(), pushed, a_scrut: a, pat_info: pi });
                // Note: type-binder frames will be popped after RHS processing per-binding
            }
            // Now infer each RHS under env_assume and finalize bindings
            let mut env_final = env_assume.clone();
            for ((_, ex), pre) in bindings.iter().zip(pres.iter()) {
                let saved = std::mem::replace(&mut ctx.env, env_assume.clone());
                let (t_rhs, s_rhs) = infer_expr(ctx, ex, allow_effects)?;
                ctx.env = saved;
                // Unify scrutinee with RHS and compose with pattern subst
                let s = ctx_unify(ctx, &pre.a_scrut.apply(&s_rhs), &t_rhs.apply(&s_rhs))?;
                let subst_all = s.compose(s_rhs.clone()).compose(pre.pat_info.subst.clone());
                // Update env_final entries with generalized types against env0
                for (n, t) in pre.pat_info.bindings.iter() {
                    // Apply substitution then zonk to fully expand nested function arrows etc.
                    let applied = t.apply(&subst_all);
                    let zonked = zonk_type(&applied, &subst_all);
                    let sc = generalize(&env0, &zonked);
                    env_final.insert(n.clone(), sc.clone());
                    if let Some(dbg) = &ctx.debug {
                        dbg.borrow_mut().log_scheme(ctx.depth, n, &sc);
                    }
                }
                pop_tyvars(ctx, pre.pushed);
                // Also update env_assume so subsequent RHS can see refined types
                env_assume = env_final.clone();
                if let Some(dbg) = &ctx.debug {
                    dbg.borrow_mut().dump_env(ctx.depth, &env_assume);
                }
            }
            // Infer body under finalized environment
            let saved = std::mem::replace(&mut ctx.env, env_final);
            // If body inference produces a generic Mismatch without a precise span,
            // remap it to the inner body's span (peeling Block) so diagnostics
            // highlight the first real expression rather than the group/comment start.
            let res = infer_expr(ctx, body, allow_effects).map_err(|e| match e {
                // 既に有効なスパンが無い (0,0) 場合のみ、LetGroup の body スパンへ差し替える
                TypeError::Mismatch { expected, actual, span_offset, span_len }
                    if span_offset == 0 && span_len == 0 =>
                {
                    // Peel nested Block nodes to get a tighter span
                    let mut inner = body.as_ref();
                    while let ExprKind::Block(ref bx) = inner.kind {
                        inner = bx.as_ref();
                    }
                    TypeError::Mismatch {
                        expected,
                        actual,
                        span_offset: inner.span.offset,
                        span_len: 1, // point at body start with a single caret
                    }
                }
                TypeError::RecordFieldMismatch {
                    field,
                    expected,
                    actual,
                    span_offset,
                    span_len,
                } if span_offset == 0 && span_len == 0 => {
                    let mut inner = body.as_ref();
                    while let ExprKind::Block(ref bx) = inner.kind {
                        inner = bx.as_ref();
                    }
                    TypeError::RecordFieldMismatch {
                        field,
                        expected,
                        actual,
                        span_offset: inner.span.offset,
                        span_len: 1,
                    }
                }
                TypeError::RecordFieldMismatchBoth {
                    field,
                    expected,
                    actual,
                    expected_span_offset,
                    expected_span_len,
                    actual_span_offset,
                    actual_span_len,
                } if expected_span_offset == 0
                    && expected_span_len == 0
                    && actual_span_offset == 0
                    && actual_span_len == 0 =>
                {
                    let mut inner = body.as_ref();
                    while let ExprKind::Block(ref bx) = inner.kind {
                        inner = bx.as_ref();
                    }
                    TypeError::RecordFieldMismatchBoth {
                        field,
                        expected,
                        actual,
                        expected_span_offset: inner.span.offset,
                        expected_span_len: 1,
                        actual_span_offset: inner.span.offset,
                        actual_span_len: 1,
                    }
                }
                TypeError::Occurs {
                    var,
                    var_pretty: _,
                    ty,
                    pretty: _,
                    var_span_offset,
                    var_span_len,
                    ty_span_offset,
                    ty_span_len,
                } if var_span_offset == 0
                    && var_span_len == 0
                    && ty_span_offset == 0
                    && ty_span_len == 0 =>
                {
                    let mut inner = body.as_ref();
                    while let ExprKind::Block(ref bx) = inner.kind {
                        inner = bx.as_ref();
                    }
                    // Re-normalize to keep variable mapping consistent inside this error scope
                    let (norm, mapping) = user_pretty_type_and_map(&ty);
                    let vp = mapping.get(&var).cloned().unwrap_or_else(|| format!("{}", var));
                    TypeError::Occurs {
                        var,
                        var_pretty: vp,
                        ty: ty.clone(),
                        pretty: norm,
                        var_span_offset: inner.span.offset,
                        var_span_len: 1,
                        ty_span_offset: inner.span.offset,
                        ty_span_len: 1,
                    }
                }
                other => other,
            });
            if pushed_typenames {
                let _ = ctx.typedef_types.pop();
            }
            if pushed_typedefs {
                let _ = ctx.typedefs.pop();
            }
            ctx.env = saved;
            res
        }
        ExprKind::Raise(inner) => {
            let (_t, s) = infer_expr(ctx, inner, allow_effects)?;
            let r = ctx.fresh_tv();
            Ok((r, s))
        }
        ExprKind::OrElse { left, right } => {
            let (tl, sl) = infer_expr(ctx, left, allow_effects)?;
            let (tr, sr) = infer_expr(ctx, right, allow_effects)?;
            // On unification failure, return both sides' spans (.Some/.None 記号位置があれば優先) for better diagnostics
            let s1 = match ctx_unify(ctx, &tl.apply(&sr).apply(&sl), &tr.apply(&sr)) {
                Ok(s) => s,
                Err(TypeError::Mismatch { expected, actual, .. }) => {
                    let lsp = find_ctor_symbol_span_in_expr(left).unwrap_or(left.span);
                    let rsp = find_ctor_symbol_span_in_expr(right).unwrap_or(right.span);
                    return Err(TypeError::MismatchBoth {
                        expected,
                        actual,
                        expected_span_offset: lsp.offset,
                        expected_span_len: lsp.len,
                        actual_span_offset: rsp.offset,
                        actual_span_len: rsp.len,
                    });
                }
                Err(e) => return Err(e),
            };
            let s = s1.compose(sr).compose(sl);
            Ok((tr.apply(&s), s))
        }
        ExprKind::Catch { left, right } => {
            let (tl, sl) = infer_expr(ctx, left, allow_effects)?;
            let a = ctx.fresh_tv();
            let r = ctx.fresh_tv();
            let (tr, sr) = infer_expr(ctx, right, allow_effects)?;
            let s1 = ctx_unify(ctx, &tr.apply(&sr).apply(&sl), &Type::fun(a, r.clone()))?;
            let s2 = ctx_unify(ctx, &tl.apply(&s1).apply(&sr).apply(&sl), &r)?;
            let s = s2.compose(s1).compose(sr).compose(sl);
            Ok((r.apply(&s), s))
        }
        ExprKind::AltLambda { left, right } => {
            // 1) Flatten AltLambda into branches
            fn collect<'a>(e: &'a Expr, out: &mut Vec<&'a Expr>) {
                match &e.kind {
                    ExprKind::AltLambda { left, right } => {
                        collect(left, out);
                        collect(right, out);
                    }
                    _ => out.push(e),
                }
            }
            let mut branches: Vec<&Expr> = vec![];
            collect(left, &mut branches);
            collect(right, &mut branches);
            if branches.len() < 2 {
                return Err(TypeError::MixedAltBranches {
                    span_offset: left.span.offset,
                    span_len: right.span.offset + right.span.len - left.span.offset,
                });
            }

            // 2) Each branch must be a (possibly multi-arg) lambda; peel to (params[], body)
            #[derive(Clone)]
            struct LamView<'a> {
                params: Vec<&'a Pattern>,
                body: &'a Expr,
                span: Span,
            }
            fn peel_lambda_chain<'a>(e: &'a Expr) -> Option<LamView<'a>> {
                match &e.kind {
                    ExprKind::Lambda { param, body } => {
                        let mut params: Vec<&'a Pattern> = Vec::new();
                        params.push(param);
                        let mut next = body.as_ref();
                        while let ExprKind::Lambda { param, body } = &next.kind {
                            params.push(param);
                            next = body.as_ref();
                        }
                        Some(LamView { params, body: next, span: e.span })
                    }
                    _ => None,
                }
            }
            let mut views: Vec<LamView> = Vec::with_capacity(branches.len());
            for &b in &branches {
                if let Some(v) = peel_lambda_chain(b) {
                    views.push(v);
                } else {
                    return Err(TypeError::MixedAltBranches {
                        span_offset: b.span.offset,
                        span_len: b.span.len,
                    });
                }
            }
            let arity = views[0].params.len();
            let first_span = views[0].span;
            for v in &views {
                if v.params.len() != arity {
                    return Err(TypeError::AltLambdaArityMismatch {
                        expected: arity,
                        got: v.params.len(),
                        expected_span_offset: first_span.offset,
                        expected_span_len: first_span.len,
                        actual_span_offset: v.span.offset,
                        actual_span_len: v.span.len,
                    });
                }
            }

            // 3) For each parameter position i, classify patterns across branches (ctor vs structural)
            for i in 0..arity {
                let mut any_ctor = bool::default();
                let mut any_struct = bool::default();
                for v in &views {
                    match v.params[i].kind {
                        PatternKind::Ctor { .. } | PatternKind::Symbol(_) => any_ctor = true,
                        PatternKind::Wildcard => { /* wildcard allowed in either mode */ }
                        _ => any_struct = true,
                    }
                }
                if any_ctor && any_struct {
                    // pick span of first offending pattern
                    let p = views
                        .iter()
                        .map(|v| &v.params[i])
                        .find(|p| !matches!(p.kind, PatternKind::Wildcard))
                        .unwrap();
                    return Err(TypeError::MixedAltBranches {
                        span_offset: p.span.offset,
                        span_len: p.span.len,
                    });
                }
            }

            // Build param types vector t0..tn-1 and return type accumulator
            let mut s_all = Subst::new();
            let mut param_tys: Vec<Type> = (0..arity).map(|_| ctx.fresh_tv()).collect();
            use std::collections::BTreeMap;
            let mut ctor_payloads_per_param: Vec<BTreeMap<String, Vec<Type>>> =
                (0..arity).map(|_| BTreeMap::new()).collect();

            // Return type accumulation
            let mut ret_variants: Vec<(String, Vec<Type>)> = vec![];
            let mut ret_seen = HashSet::<String>::new();
            let mut ret_other: Option<(Type, Option<Span>)> = None;
            fn merge_ret(
                acc: &mut Vec<(String, Vec<Type>)>,
                seen: &mut HashSet<String>,
                t: &Type,
            ) -> bool {
                match t {
                    Type::Ctor { tag, payload } => {
                        if seen.insert(tag.clone()) {
                            acc.push((tag.clone(), payload.clone()));
                        }
                        true
                    }
                    Type::SumCtor(vs) => {
                        for (tg, ps) in vs {
                            if seen.insert(tg.clone()) {
                                acc.push((tg.clone(), ps.clone()));
                            }
                        }
                        true
                    }
                    _ => false,
                }
            }

            // For each parameter position, infer pattern types and extend env per branch
            // We process per branch: for i in 0..arity, infer_pattern with current param_tys[i]
            // Allow ctor-specific specialization by unifying scrutinee with ctor signature when pattern is ctor.
            for v in views.iter() {
                let mut env2 = ctx.env.clone();
                // Process all params first, build bindings, then infer body under extended env
                for (i, p) in v.params.iter().enumerate() {
                    let p_core;
                    let pushed;
                    {
                        let (c, k) = push_tyvars_from_pattern(ctx, p);
                        p_core = c;
                        pushed = k;
                    }
                    // Decide scrutinee for this pattern and update ctor maps
                    let (scrut, is_ctor_like) = match &p_core.kind {
                        PatternKind::Ctor { name, args } => {
                            let ent =
                                ctor_payloads_per_param[i].entry(name.clone()).or_insert_with(
                                    || (0..args.len()).map(|_| ctx.fresh_tv()).collect::<Vec<_>>(),
                                );
                            if ent.len() != args.len() {
                                return Err(TypeError::DuplicateCtorTag {
                                    tag: name.clone(),
                                    span_offset: p_core.span.offset,
                                    span_len: p_core.span.len,
                                });
                            }
                            (Type::Ctor { tag: name.clone(), payload: ent.clone() }, true)
                        }
                        PatternKind::Symbol(name) => {
                            let ent = ctor_payloads_per_param[i].entry(name.clone()).or_default();
                            if !ent.is_empty() {
                                return Err(TypeError::DuplicateCtorTag {
                                    tag: name.clone(),
                                    span_offset: p_core.span.offset,
                                    span_len: p_core.span.len,
                                });
                            }
                            (Type::Ctor { tag: name.clone(), payload: vec![] }, true)
                        }
                        _ => (param_tys[i].clone(), false),
                    };
                    // Infer pattern and merge
                    let pi = infer_pattern(ctx, p_core, &scrut.apply(&s_all))?;
                    for (n, t) in &pi.bindings {
                        env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                    }
                    s_all = pi.subst.compose(s_all);
                    // For ctor-like patterns, defer building union; do not force unify with scrut
                    if !is_ctor_like {
                        let s1 = ctx_unify(ctx, &param_tys[i].apply(&s_all), &scrut.apply(&s_all))?;
                        s_all = s1.compose(s_all);
                    }
                    pop_tyvars(ctx, pushed);
                }
                // Infer body
                let prev = std::mem::replace(&mut ctx.env, env2);
                let (tb, sb) = infer_expr(ctx, v.body, allow_effects)?;
                ctx.env = prev;
                let tbf = tb.apply(&sb);
                if !merge_ret(&mut ret_variants, &mut ret_seen, &tbf) {
                    match &mut ret_other {
                        None => {
                            ret_other = Some((tbf.clone(), find_ctor_symbol_span_in_expr(v.body)))
                        }
                        Some((existing, pref_span)) => {
                            let s1 = match unify(&existing.clone(), &tbf) {
                                Ok(s) => s,
                                Err(TypeError::Mismatch { expected, actual, .. }) => {
                                    let cur_sp = find_ctor_symbol_span_in_expr(v.body)
                                        .or(*pref_span)
                                        .unwrap_or(v.span);
                                    let ex_sp = pref_span.unwrap_or(v.span);
                                    return Err(TypeError::MismatchBoth {
                                        expected,
                                        actual,
                                        expected_span_offset: ex_sp.offset,
                                        expected_span_len: ex_sp.len,
                                        actual_span_offset: cur_sp.offset,
                                        actual_span_len: cur_sp.len,
                                    });
                                }
                                Err(e) => return Err(e),
                            };
                            *existing = existing.apply(&s1);
                            s_all = s1.compose(s_all);
                        }
                    }
                }
                s_all = sb.compose(s_all);
                // Update params with latest substitution
                for param_ty in param_tys.iter_mut() {
                    *param_ty = param_ty.apply(&s_all);
                }
            }

            // Finalize return type
            let ret_ty = if !ret_variants.is_empty() && ret_other.is_none() {
                if ret_variants.len() == 1 {
                    let (tag, payload) = ret_variants.pop().unwrap();
                    Type::Ctor { tag, payload }
                } else {
                    ret_variants.sort_by(|a, b| a.0.cmp(&b.0));
                    Type::SumCtor(ret_variants)
                }
            } else if let Some((t, _)) = ret_other {
                t
            } else {
                ctx.fresh_tv()
            };

            // Finalize parameter types: build union per position if ctor variants were collected
            for (param_ty, payloads) in param_tys.iter_mut().zip(ctor_payloads_per_param.iter()) {
                if payloads.is_empty() {
                    *param_ty = param_ty.apply(&s_all);
                } else if payloads.len() == 1 {
                    let (tag, payload) = payloads.iter().next().unwrap();
                    *param_ty = Type::Ctor { tag: tag.clone(), payload: payload.clone() };
                } else {
                    let mut variants: Vec<(String, Vec<Type>)> =
                        payloads.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    variants.sort_by(|a, b| a.0.cmp(&b.0));
                    *param_ty = Type::SumCtor(variants);
                }
            }

            // Compose final function type a1 -> a2 -> ... -> ret
            let fun_ty = param_tys
                .into_iter()
                .rev()
                .fold(ret_ty.apply(&s_all), |acc, a| Type::fun(a.apply(&s_all), acc));
            Ok((fun_ty, s_all))
        }
    };
    ctx.depth -= 1;
    if let Some(dbg) = &ctx.debug {
        if let Ok((ty, _)) = &result {
            dbg.borrow_mut().log(
                ctx.depth,
                1,
                format!("exit {} => {}", short_expr_kind(&e.kind), pp_type(ty)),
            );
        }
    }
    let out = result;
    ctx.pop_span();
    out
}

fn short_expr_kind(k: &ExprKind) -> &'static str {
    use ExprKind::*;
    match k {
        Unit => "Unit",
        Int(_) => "Int",
        Float(_) => "Float",
        Str(_) => "Str",
        Char(_) => "Char",
        Ref(_) => "Ref",
        Symbol(_) => "Symbol",
        Lambda { .. } => "Lambda",
        Apply { .. } => "Apply",
        Annot { .. } => "Annot",
        TypeVal(_) => "TypeVal",
        List(_) => "List",
        Record(_) => "Record",
        Block(_) => "Block",
        LetGroup { .. } => "LetGroup",
        Raise(_) => "Raise",
        OrElse { .. } => "OrElse",
        Catch { .. } => "Catch",
        AltLambda { .. } => "AltLambda",
    }
}

// ---------- Pretty Printing ----------
// pp_type: low-level, stable, developer-oriented printer.
//   - Raw type variables shown as %tN (internal ids) for debugging / legacy tests.
//   - No %{ } wrapper, no variable renaming, no cycle guard beyond recursion.
// user_pretty_type (below): user-facing normalized printer.
//   - Performs zonk beforehand at call sites.
//   - Deterministic renaming %a, %b, ... and wraps in "%{ ... }" for visual distinction.
//   - Keeps mapping (via user_pretty_type_and_map) for consistent occurs diagnostics.
// Guidelines:
//   * Use pp_type inside logs / debug output / legacy mode.
//   * Use user_pretty_type for CLI surface and error messages exposed to end users (pretty mode).
//   * Do not mix both in one diagnostic line to avoid confusing two naming domains.
// (Display functions moved to display/ module)

// ---------- Public API ----------

pub mod api {
    use super::*;
    use lzscr_parser::parse_expr;
    /// オプション指定で推論結果文字列のフォーマットを切り替えるための設定。
    #[derive(Debug, Clone, Copy, Default)]
    pub struct InferOptions {
        /// true ならユーザ向け pretty 表示 (%{ ... } + 型変数 %a,%b など)。
        /// false ならレガシー表示 (テスト互換: %tN 生の変数, ラッパ無し)。
        pub pretty: bool,
    }

    /// 下位互換 (レガシー) API: 既存テストが期待するフォーマット (pretty=false)。
    pub fn infer_program(src: &str) -> Result<String, String> {
        infer_program_with_opts(src, InferOptions { pretty: false })
    }

    /// 新 API: フォーマット指定付き。将来はこちらを安定化させる。
    pub fn infer_program_with_opts(src: &str, opts: InferOptions) -> Result<String, String> {
        let ast = parse_expr(src).map_err(|e| format!("parse error: {e}"))?;
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        match infer_expr(&mut ctx, &ast, false) {
            Ok((t, s)) => {
                let zonked = zonk_type(&t.apply(&s), &s);
                if opts.pretty {
                    Ok(user_pretty_type(&zonked))
                } else {
                    Ok(pp_type(&zonked))
                }
            }
            Err(e) => Err(format!("{e}")),
        }
    }

    // Prefer this from tools that already have an AST with precise spans (e.g., CLI after ~require expansion).
    #[allow(clippy::result_large_err)]
    pub fn infer_ast(ast: &Expr) -> Result<String, super::TypeError> {
        infer_ast_with_opts(ast, InferOptions { pretty: true })
    }

    /// オプション付き AST 推論。`infer_ast` は pretty=true の糖衣。
    #[allow(clippy::result_large_err)]
    pub fn infer_ast_with_opts(ast: &Expr, opts: InferOptions) -> Result<String, super::TypeError> {
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let zonked = zonk_type(&t.apply(&s), &s);
        if opts.pretty {
            Ok(user_pretty_type(&zonked))
        } else {
            Ok(pp_type(&zonked))
        }
    }

    // Debug variant returning (type, logs)
    #[allow(clippy::result_large_err)]
    pub fn infer_ast_debug(
        ast: &Expr,
        level: usize,
        max_depth: usize,
    ) -> Result<(String, Vec<String>), super::TypeError> {
        let dbg = DebugConfig {
            level,
            max_depth,
            logs: Vec::new(),
            log_unify: true,
            log_env: false,
            log_schemes: true,
        };
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        // Log initial environment keys only if env logging enabled
        if let Some(dbg) = &ctx.debug {
            if dbg.borrow().log_env {
                let mut keys: Vec<_> = ctx.env.0.keys().cloned().collect();
                keys.sort();
                let preview: Vec<_> = keys.into_iter().take(64).collect();
                dbg.borrow_mut().log(0, 1, format!("initial env keys={:?}", preview));
            }
        }
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let ty = user_pretty_type(&t.apply(&s));
        let logs = ctx.debug.as_ref().unwrap().borrow().logs.clone();
        Ok((ty, logs))
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_ast_debug_with(
        ast: &Expr,
        level: usize,
        max_depth: usize,
        log_unify: bool,
        log_env: bool,
        log_schemes: bool,
    ) -> Result<(String, Vec<String>), super::TypeError> {
        let dbg =
            DebugConfig { level, max_depth, logs: Vec::new(), log_unify, log_env, log_schemes };
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let ty = user_pretty_type(&t.apply(&s));
        let logs = ctx.debug.as_ref().unwrap().borrow().logs.clone();
        Ok((ty, logs))
    }

    fn prelude_env() -> TypeEnv {
        let mut env = TypeEnv::new();
        // Minimal builtins required by tests; most tests use pure lambdas.
        // Add 'alt' for completeness: forall a r. (a->r)->(a->r)->a->r
        let a = TvId(1000);
        let r = TvId(1001);
        let alt_ty = Type::fun(
            Type::fun(Type::Var(a), Type::Var(r)),
            Type::fun(Type::fun(Type::Var(a), Type::Var(r)), Type::fun(Type::Var(a), Type::Var(r))),
        );
        env.insert("alt".into(), Scheme { vars: vec![a, r], ty: alt_ty });
        // add : Int -> Int -> Int
        let add_ty = Type::fun(Type::Int, Type::fun(Type::Int, Type::Int));
        env.insert("add".into(), Scheme { vars: vec![], ty: add_ty });
        // Boolean ops (approximate): and/or : Bool -> Bool -> Bool ; not : Bool -> Bool
        // and/or : Bool -> Bool -> Bool
        let bool_t = bool_sum_type();
        env.insert(
            "and".into(),
            Scheme {
                vars: vec![],
                ty: Type::fun(bool_t.clone(), Type::fun(bool_t.clone(), bool_t.clone())),
            },
        );
        env.insert(
            "or".into(),
            Scheme {
                vars: vec![],
                ty: Type::fun(bool_t.clone(), Type::fun(bool_t.clone(), bool_t.clone())),
            },
        );
        env.insert(
            "not".into(),
            Scheme { vars: vec![], ty: Type::fun(bool_t.clone(), bool_t.clone()) },
        );
        // boolean values
        // seq : forall a b. a -> b -> b
        let a2 = TvId(1002);
        let b2 = TvId(1003);
        let seq_ty = Type::fun(Type::Var(a2), Type::fun(Type::Var(b2), Type::Var(b2)));
        env.insert("seq".into(), Scheme { vars: vec![a2, b2], ty: seq_ty });
        // chain : forall a b. a -> b -> b
        let a4 = TvId(1006);
        let b4 = TvId(1007);
        let chain_ty = Type::fun(Type::Var(a4), Type::fun(Type::Var(b4), Type::Var(b4)));
        env.insert("chain".into(), Scheme { vars: vec![a4, b4], ty: chain_ty });
        // bind : forall x r. x -> (x -> r) -> r
        let x5 = TvId(1008);
        let r5 = TvId(1009);
        let bind_ty = Type::fun(
            Type::Var(x5),
            Type::fun(Type::fun(Type::Var(x5), Type::Var(r5)), Type::Var(r5)),
        );
        env.insert("bind".into(), Scheme { vars: vec![x5, r5], ty: bind_ty });
        // effects : forall s a. s -> a -> Unit  (approximate; first arg is an effect symbol)
        let s3 = TvId(1004);
        let a3 = TvId(1005);
        let eff_ty = Type::fun(Type::Var(s3), Type::fun(Type::Var(a3), Type::Unit));
        env.insert("effects".into(), Scheme { vars: vec![s3, a3], ty: eff_ty });

        // Arithmetic and comparison commonly used in prelude
        env.insert(
            "add".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert(
            "sub".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert(
            "mul".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert("eq".into(), {
            let a = TvId(1010);
            Scheme {
                vars: vec![a],
                ty: Type::fun(Type::Var(a), Type::fun(Type::Var(a), bool_t.clone())),
            }
        });
        env.insert(
            "lt".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "le".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "gt".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "ge".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );

        // cons : forall a. a -> List a -> List a
        let a_cons = TvId(1012);
        let cons_ty = Type::fun(
            Type::Var(a_cons),
            Type::fun(
                Type::List(Box::new(Type::Var(a_cons))),
                Type::List(Box::new(Type::Var(a_cons))),
            ),
        );
        env.insert("cons".into(), Scheme { vars: vec![a_cons], ty: cons_ty });

        // Common list functions (if stdlib not already providing before top-level expression inference)
        // append : forall a. [a] -> [a] -> [a]
        if !env.0.contains_key("append") {
            let a_app = TvId(2000);
            let app_ty = Type::fun(
                Type::List(Box::new(Type::Var(a_app))),
                Type::fun(
                    Type::List(Box::new(Type::Var(a_app))),
                    Type::List(Box::new(Type::Var(a_app))),
                ),
            );
            env.insert("append".into(), Scheme { vars: vec![a_app], ty: app_ty });
        }
        // length : forall a. [a] -> Int
        if !env.0.contains_key("length") {
            let a_len = TvId(2001);
            let len_ty = Type::fun(Type::List(Box::new(Type::Var(a_len))), Type::Int);
            env.insert("length".into(), Scheme { vars: vec![a_len], ty: len_ty });
        }
        // map : forall a b. (a -> b) -> [a] -> [b]
        if !env.0.contains_key("map") {
            let a_map = TvId(2002);
            let b_map = TvId(2003);
            let map_ty = Type::fun(
                Type::fun(Type::Var(a_map), Type::Var(b_map)),
                Type::fun(
                    Type::List(Box::new(Type::Var(a_map))),
                    Type::List(Box::new(Type::Var(b_map))),
                ),
            );
            env.insert("map".into(), Scheme { vars: vec![a_map, b_map], ty: map_ty });
        }
        // reverse : forall a. [a] -> [a]
        if !env.0.contains_key("reverse") {
            let a_rev = TvId(2004);
            let rev_ty = Type::fun(
                Type::List(Box::new(Type::Var(a_rev))),
                Type::List(Box::new(Type::Var(a_rev))),
            );
            env.insert("reverse".into(), Scheme { vars: vec![a_rev], ty: rev_ty });
        }

        // to_str : forall a. a -> Str
        let a_ts = TvId(1013);
        let to_str_ty = Type::fun(Type::Var(a_ts), Type::Str);
        env.insert("to_str".into(), Scheme { vars: vec![a_ts], ty: to_str_ty });

        // if : forall a. Bool -> a -> a -> a   (Bool in union form)
        let a_if = TvId(1011);
        let if_ty = Type::fun(
            bool_t.clone(),
            Type::fun(Type::Var(a_if), Type::fun(Type::Var(a_if), Type::Var(a_if))),
        );
        env.insert("if".into(), Scheme { vars: vec![a_if], ty: if_ty });

        // Builtins record with nested namespaces used by stdlib/prelude.lzscr
        fn record(fields: Vec<(&str, Type)>) -> Type {
            let mut m = BTreeMap::new();
            for (k, v) in fields {
                m.insert(k.to_string(), v);
            }
            Type::Record(m.into_iter().map(|(k, v)| (k, (v, None))).collect())
        }
        // String namespace
        let string_ns = record(vec![
            ("len", Type::fun(Type::Str, Type::Int)),
            ("concat", Type::fun(Type::Str, Type::fun(Type::Str, Type::Str))),
            ("slice", Type::fun(Type::Str, Type::fun(Type::Int, Type::fun(Type::Int, Type::Str)))),
            // char_at : Str -> Int -> (.Some Char | .None)
            (
                "char_at",
                Type::fun(
                    Type::Str,
                    Type::fun(
                        Type::Int,
                        Type::SumCtor(vec![
                            (".Some".into(), vec![Type::Char]),
                            (".None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
        ]);
        // Math namespace (minimal for now)
        let math_ns = record(vec![("abs", Type::fun(Type::Int, Type::Int))]);
        // Char namespace
        let char_ns = record(vec![
            ("is_digit", Type::fun(Type::Char, bool_t.clone())),
            ("is_alpha", Type::fun(Type::Char, bool_t.clone())),
            ("is_alnum", Type::fun(Type::Char, bool_t.clone())),
            ("is_space", Type::fun(Type::Char, bool_t.clone())),
            // between : Char -> Char -> Char -> Bool-like (code point range using chars)
            (
                "between",
                Type::fun(Type::Char, Type::fun(Type::Char, Type::fun(Type::Char, bool_t.clone()))),
            ),
        ]);
        // Unicode namespace
        let unicode_ns = record(vec![
            ("to_int", Type::fun(Type::Char, Type::Int)),
            ("of_int", Type::fun(Type::Int, Type::Char)),
        ]);
        // Scan namespace
        let scan_state = record(vec![
            ("dummy", Type::Unit), // placeholder field; state is opaque
        ]);
        let scan_ns = record(vec![
            ("new", Type::fun(Type::Str, scan_state.clone())),
            // eof returns a symbol union used in AltLambda patterns: .True | .False
            (
                "eof",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![(".True".into(), vec![]), (".False".into(), vec![])]),
                ),
            ),
            ("pos", Type::fun(scan_state.clone(), Type::Int)),
            ("set_pos", Type::fun(scan_state.clone(), Type::fun(Type::Int, scan_state.clone()))),
            // peek : ScanState -> (.Some Char | .None)
            (
                "peek",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![
                        (".Some".into(), vec![Type::Char]),
                        (".None".into(), vec![]),
                    ]),
                ),
            ),
            // next : ScanState -> (.Some (., Char Scan) | .None)
            (
                "next",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![
                        (
                            ".Some".into(),
                            vec![Type::Ctor {
                                tag: ".,".into(),
                                payload: vec![Type::Char, scan_state.clone()],
                            }],
                        ),
                        (".None".into(), vec![]),
                    ]),
                ),
            ),
            // take_if : (Char -> Bool-like) -> ScanState -> (.Some (., Char Scan) | .None)
            (
                "take_if",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::SumCtor(vec![
                            (
                                ".Some".into(),
                                vec![Type::Ctor {
                                    tag: ".,".into(),
                                    payload: vec![Type::Char, scan_state.clone()],
                                }],
                            ),
                            (".None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
            // take_while : (Char -> Bool-like) -> ScanState -> (., Str Scan)
            (
                "take_while",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::Ctor {
                            tag: ".,".into(),
                            payload: vec![Type::Str, scan_state.clone()],
                        },
                    ),
                ),
            ),
            // take_while1 : (Char -> Bool-like) -> ScanState -> (.Some (., Str Scan) | .None)
            (
                "take_while1",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::SumCtor(vec![
                            (
                                ".Some".into(),
                                vec![Type::Ctor {
                                    tag: ".,".into(),
                                    payload: vec![Type::Str, scan_state.clone()],
                                }],
                            ),
                            (".None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
            (
                "slice_span",
                Type::fun(
                    scan_state.clone(),
                    Type::fun(Type::Int, Type::fun(Type::Int, Type::Str)),
                ),
            ),
        ]);
        let builtins_ty = record(vec![
            ("string", string_ns),
            ("math", math_ns),
            ("char", char_ns),
            ("unicode", unicode_ns),
            ("scan", scan_ns),
        ]);
        env.insert("Builtins".into(), Scheme { vars: vec![], ty: builtins_ty });
        env
    }
}

#[cfg(test)]
mod tests {
    use super::api::infer_program;

    #[test]
    fn infer_char_literal() {
        let t = infer_program("'x'").unwrap();
        assert_eq!(t, "Char");
    }
}
