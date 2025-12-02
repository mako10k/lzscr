//! Type unification for Hindley-Milner inference.
//!
//! This module implements Robinson's unification algorithm with support for:
//! - Basic types (Unit, Int, Float, Str, Char)
//! - Function types
//! - Lists, tuples, and records
//! - Constructor types (Ctor, SumCtor)
//! - Named types (μ-types) with one-step unfolding
//! - Occurs check with enriched error reporting

use std::collections::BTreeMap;

use crate::error::{format_field_path, TypeError};
use crate::scheme::{normalize_tuples, Subst, TypesApply};
use crate::typeexpr::instantiate_named_sum;
use crate::types::{Type, TvId};
use crate::{normalize_type_and_map, pp_type, user_pretty_type_and_map, InferCtx};

/// Check if a type variable occurs in a type (occurs check for preventing infinite types).
fn occurs(v: TvId, t: &Type) -> bool {
    t.ftv().contains(&v)
}

/// Unify two types, returning a substitution or a type error.
///
/// This is the core unification algorithm implementing Robinson's algorithm with extensions for:
/// - Record types with field-level error reporting
/// - Sum constructor types (tagged unions)
/// - Mixed Ctor/SumCtor unification
#[allow(clippy::result_large_err)]
pub(crate) fn unify(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Var(x), t) => bind(*x, t.clone()),
        (t, Type::Var(x)) => bind(*x, t.clone()),
        (Type::Unit, Type::Unit)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Str, Type::Str)
        | (Type::Char, Type::Char)
        | (Type::Type, Type::Type) => Ok(Subst::new()),
        (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
            let s1 = unify(a1, a2)?;
            let s2 = unify(&b1.apply(&s1), &b2.apply(&s1))?;
            Ok(s2.compose(s1))
        }
        (Type::List(x), Type::List(y)) => unify(x, y),
        (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => unify_slices(xs, ys),
        (Type::Record(rx), Type::Record(ry)) if rx.len() == ry.len() => {
            // Compare by keys first
            let mut s = Subst::new();
            for (k, (vx_ty, vx_sp)) in rx.iter() {
                let (vy_ty, vy_sp) = ry.get(k).ok_or_else(|| TypeError::Mismatch {
                    expected: Type::Record(rx.clone()),
                    actual: Type::Record(ry.clone()),
                    span_offset: 0,
                    span_len: 0,
                })?;
                match unify(&vx_ty.apply(&s), &vy_ty.apply(&s)) {
                    Ok(s1) => {
                        s = s1.compose(s);
                    }
                    Err(TypeError::Mismatch { expected, actual, .. }) => {
                        // If both sides have spans, emit dual-span variant.
                        match (vx_sp, vy_sp) {
                            (Some(es), Some(as_)) => {
                                return Err(TypeError::RecordFieldMismatchBoth {
                                    field: k.clone(),
                                    expected,
                                    actual,
                                    expected_span_offset: es.offset,
                                    expected_span_len: es.len,
                                    actual_span_offset: as_.offset,
                                    actual_span_len: as_.len,
                                });
                            }
                            _ => {
                                let use_span = vy_sp.or(*vx_sp);
                                let (span_offset, span_len) =
                                    use_span.map(|sp| (sp.offset, sp.len)).unwrap_or((0, 0));
                                return Err(TypeError::RecordFieldMismatch {
                                    field: k.clone(),
                                    expected,
                                    actual,
                                    span_offset,
                                    span_len,
                                });
                            }
                        }
                    }
                    Err(TypeError::RecordFieldMismatchBoth {
                        field,
                        expected,
                        actual,
                        expected_span_offset,
                        expected_span_len,
                        actual_span_offset,
                        actual_span_len,
                    }) => {
                        return Err(TypeError::RecordFieldMismatchBoth {
                            field: format_field_path(k, &field),
                            expected,
                            actual,
                            expected_span_offset,
                            expected_span_len,
                            actual_span_offset,
                            actual_span_len,
                        });
                    }
                    Err(TypeError::RecordFieldMismatch {
                        field,
                        expected,
                        actual,
                        span_offset,
                        span_len,
                    }) => {
                        return Err(TypeError::RecordFieldMismatch {
                            field: format_field_path(k, &field),
                            expected,
                            actual,
                            span_offset,
                            span_len,
                        });
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(s)
        }
        (Type::Ctor { tag: ta, payload: pa }, Type::Ctor { tag: tb, payload: pb })
            if ta == tb && pa.len() == pb.len() =>
        {
            unify_slices(pa, pb)
        }
        (Type::SumCtor(a), Type::SumCtor(b)) => {
            // Require exact same variant set (MVP). Unify payloads by tag.
            if a.len() != b.len() {
                return Err(TypeError::Mismatch {
                    expected: Type::SumCtor(a.clone()),
                    actual: Type::SumCtor(b.clone()),
                    span_offset: 0,
                    span_len: 0,
                });
            }
            let mut map_a: BTreeMap<&str, &Vec<Type>> = BTreeMap::new();
            for (t, ps) in a {
                map_a.insert(t.as_str(), ps);
            }
            let mut s = Subst::new();
            for (t, psb) in b {
                let psa = map_a.get(t.as_str()).ok_or_else(|| TypeError::Mismatch {
                    expected: Type::SumCtor(a.clone()),
                    actual: Type::SumCtor(b.clone()),
                    span_offset: 0,
                    span_len: 0,
                })?;
                if psa.len() != psb.len() {
                    return Err(TypeError::Mismatch {
                        expected: Type::SumCtor(a.clone()),
                        actual: Type::SumCtor(b.clone()),
                        span_offset: 0,
                        span_len: 0,
                    });
                }
                let s_payload = unify_slices(psa, psb)?; // compose after slice unify
                s = s_payload.compose(s);
            }
            Ok(s)
        }
        // Allow a single constructor to unify with a union that contains it
        (Type::Ctor { tag, payload }, Type::SumCtor(variants))
        | (Type::SumCtor(variants), Type::Ctor { tag, payload }) => {
            if let Some((_, ps_other)) = variants.iter().find(|(t, _)| t == tag) {
                if ps_other.len() != payload.len() {
                    return Err(TypeError::Mismatch {
                        expected: Type::SumCtor(variants.clone()),
                        actual: Type::Ctor { tag: tag.clone(), payload: payload.clone() },
                        span_offset: 0,
                        span_len: 0,
                    });
                }
                Ok(unify_slices(payload, ps_other)?)
            } else {
                Err(TypeError::Mismatch {
                    expected: a.clone(),
                    actual: b.clone(),
                    span_offset: 0,
                    span_len: 0,
                })
            }
        }
        _ => Err(TypeError::Mismatch {
            expected: a.clone(),
            actual: b.clone(),
            span_offset: 0,
            span_len: 0,
        }),
    }
}

/// Unify two equally-sized slices element-wise, composing substitutions left-to-right.
///
/// Used for Tuple, Ctor payloads, and SumCtor payload lists.
#[allow(clippy::result_large_err)] // TypeError variants intentionally carry rich span data.
fn unify_slices(xs: &[Type], ys: &[Type]) -> Result<Subst, TypeError> {
    debug_assert_eq!(xs.len(), ys.len());
    let mut s = Subst::new();
    for (x, y) in xs.iter().zip(ys.iter()) {
        let s1 = unify(&x.apply(&s), &y.apply(&s))?;
        s = s1.compose(s);
    }
    Ok(s)
}

/// Unification with awareness of Named types (μ-types): one-step unfold into SumCtor.
///
/// This function wraps `unify` with:
/// - Named type unfolding (converts `Type::Named` to `Type::SumCtor`)
/// - Tuple normalization
/// - Enhanced occurs check error reporting with span information from `InferCtx`
/// - Optional debug logging of unification attempts
#[allow(clippy::result_large_err)]
pub(crate) fn ctx_unify(ctx: &InferCtx, a: &Type, b: &Type) -> Result<Subst, TypeError> {
    if let Some(dbg) = &ctx.debug {
        if dbg.borrow().log_unify {
            dbg.borrow_mut().log(
                ctx.depth,
                2,
                format!("unify TRY {} ~ {}", pp_type(a), pp_type(b)),
            );
        }
    }
    // Normalize tuple-like constructors before actual unification
    let na = normalize_tuples(a);
    let nb = normalize_tuples(b);
    let res_raw = match (&na, &nb) {
        (Type::Named { name: na, args: aa }, _) => {
            let sa = instantiate_named_sum(ctx, na, aa)?;
            ctx_unify(ctx, &Type::SumCtor(sa), &nb)
        }
        (_, Type::Named { name: nb, args: ab }) => {
            let sb = instantiate_named_sum(ctx, nb, ab)?;
            ctx_unify(ctx, &na, &Type::SumCtor(sb))
        }
        _ => unify(&na, &nb),
    };
    // Post-process occurs errors to enrich spans from origins if missing.
    let res = res_raw.map_err(|e| match e {
        TypeError::Occurs {
            var,
            ty,
            var_pretty: _,
            pretty: _,
            var_span_offset,
            var_span_len,
            ty_span_offset,
            ty_span_len,
        } => {
            let (norm, mapping) = user_pretty_type_and_map(&ty);
            let vp = mapping.get(&var).cloned().unwrap_or_else(|| format!("{}", var));
            if (var_span_offset, var_span_len, ty_span_offset, ty_span_len) == (0, 0, 0, 0) {
                if let Some(spv) = ctx.tv_origins.get(&var) {
                    let mut ty_sp = None;
                    for tv in ty.ftv() {
                        if tv != var {
                            if let Some(sp) = ctx.tv_origins.get(&tv) {
                                ty_sp = Some(*sp);
                                break;
                            }
                        }
                    }
                    let ty_sp = ty_sp.unwrap_or(*spv);
                    TypeError::Occurs {
                        var,
                        var_pretty: vp,
                        ty: ty.clone(),
                        pretty: norm,
                        var_span_offset: spv.offset,
                        var_span_len: spv.len,
                        ty_span_offset: ty_sp.offset,
                        ty_span_len: ty_sp.len,
                    }
                } else {
                    TypeError::Occurs {
                        var,
                        var_pretty: vp,
                        ty: ty.clone(),
                        pretty: norm,
                        var_span_offset,
                        var_span_len,
                        ty_span_offset,
                        ty_span_len,
                    }
                }
            } else {
                TypeError::Occurs {
                    var,
                    var_pretty: vp,
                    ty: ty.clone(),
                    pretty: norm,
                    var_span_offset,
                    var_span_len,
                    ty_span_offset,
                    ty_span_len,
                }
            }
        }
        other => other,
    });
    if let Some(dbg) = &ctx.debug {
        if dbg.borrow().log_unify {
            match &res {
                Ok(s) => {
                    let subst_str = if s.0.is_empty() {
                        "<id>".to_string()
                    } else {
                        s.0.iter()
                            .map(|(TvId(id), t)| format!("%t{id}={}", pp_type(t)))
                            .collect::<Vec<_>>()
                            .join(", ")
                    };
                    dbg.borrow_mut().log(
                        ctx.depth,
                        2,
                        format!("unify OK {} ~ {} => {subst_str}", pp_type(a), pp_type(b)),
                    );
                }
                Err(e) => {
                    dbg.borrow_mut().log(
                        ctx.depth,
                        2,
                        format!("unify ERR {} ~ {} => {e}", pp_type(a), pp_type(b)),
                    );
                }
            }
        }
    }
    res
}

/// Bind a type variable to a type, performing the occurs check.
///
/// Returns a singleton substitution or an occurs error if the variable appears in the type.
#[allow(clippy::result_large_err)]
fn bind(v: TvId, t: Type) -> Result<Subst, TypeError> {
    match t {
        Type::Var(v2) if v == v2 => Ok(Subst::new()),
        _ if occurs(v, &t) => {
            let (norm, mapping) = normalize_type_and_map(&t);
            let vp = mapping.get(&v).cloned().unwrap_or_else(|| format!("{}", v));
            Err(TypeError::Occurs {
                var: v,
                var_pretty: vp,
                ty: t.clone(),
                pretty: norm,
                var_span_offset: 0,
                var_span_len: 0,
                ty_span_offset: 0,
                ty_span_len: 0,
            })
        }
        _ => Ok(Subst::singleton(v, t)),
    }
}
