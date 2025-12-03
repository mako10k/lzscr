//! Pattern type inference for lzscr.
//!
//! This module provides pattern matching type inference, handling:
//! - Variable bindings
//! - Literals (Unit, Int, Float, Str, Char)
//! - Tuples and lists
//! - Records and constructors
//! - Pattern guards (As, TypeBind)

use lzscr_ast::ast::{Pattern, PatternKind};
use std::collections::BTreeMap;

use crate::error::TypeError;
use crate::inference::context::InferCtx;
use crate::scheme::{Subst, TypesApply};
use crate::typeexpr::{conv_typeexpr_fresh, typedefs_lookup_ctor};
use crate::types::Type;
use crate::unification::ctx_unify;

/// Pattern inference result containing variable bindings and substitution.
#[derive(Default)]
pub(crate) struct PatInfo {
    pub(crate) bindings: Vec<(String, Type)>,
    pub(crate) subst: Subst,
}

/// Infer the type of a pattern against a scrutinee type.
#[allow(clippy::result_large_err)]
pub(crate) fn infer_pattern(
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
            for f in fields {
                want.insert(f.name.clone(), ctx.fresh_tv());
            }
            let want_spanned =
                Type::Record(want.iter().map(|(k, v)| (k.clone(), (v.clone(), None))).collect());
            let s0 = ctx_unify(ctx, scrutinee, &want_spanned)?;
            let mut s = s0;
            let mut binds = vec![];
            for f in fields {
                let tfield = want.get(&f.name).unwrap().apply(&s);
                let pi = infer_pattern(ctx, &f.pattern, &tfield)?;
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
