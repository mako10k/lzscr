//! Expression type inference for lzscr.
//!
//! This module implements the main `infer_expr` function that performs
//! Hindley-Milner type inference on lzscr expressions.

use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::builtins::{bool_sum_type, effect_signature};
use crate::display::{pp_type, user_pretty_type_and_map};
use crate::error::{find_similar_names, TypeError};
use crate::inference::context::{lookup_tyvar, pop_tyvars, push_tyvars_from_pattern, InferCtx};
use crate::inference::pattern::{infer_pattern, PatInfo};
use crate::scheme::{generalize, instantiate, zonk_type, Scheme, Subst, TypesApply};
use crate::typeexpr::{
    build_typedefs_frame, build_typename_frame, typedefs_lookup_typename,
    validate_typedecls_positive,
};
use crate::types::{TvId, Type};
use crate::unification::{ctx_unify, unify};

/// Main expression type inference function.
#[allow(clippy::result_large_err)]
pub(crate) fn infer_expr(
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
                for f in fs {
                    if let Some(sp) = find_ctor_symbol_span_in_pattern(&f.pattern) {
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
                for f in fs {
                    m.insert(f.name.clone(), conv_typeexpr(ctx, &f.type_expr, holes));
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
            // Phase 5: Now uses field name spans for better diagnostics
            let mut subst = Subst::new();
            let mut map = BTreeMap::new();
            for f in fields {
                let (tv, sv) = infer_expr(ctx, &f.value, allow_effects)?;
                subst = sv.compose(subst);
                // Use field name span instead of value span for record field diagnostics
                map.insert(f.name.clone(), (tv.apply(&subst), Some(f.name_span)));
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

/// Get a short string description of an expression kind for debugging.
pub(crate) fn short_expr_kind(k: &ExprKind) -> &'static str {
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
