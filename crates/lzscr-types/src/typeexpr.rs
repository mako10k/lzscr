//! Type expression conversion and type declaration management.
//!
//! This module provides:
//! - Type declaration frames (typedefs) for constructor and named type lookups
//! - Conversion from AST TypeExpr to runtime Type
//! - Positivity checking for recursive type declarations
//! - Named type (μ-types) instantiation with parameter substitution

use crate::{bool_sum_type, InferCtx, TvGen, Type, TypeError};
use lzscr_ast::ast::{TypeDecl, TypeDefBody, TypeExpr};
use std::collections::{BTreeMap, HashMap};

/// Frame mapping constructor tags to their payload type templates (AST TypeExpr).
pub type TypeDefsFrame = HashMap<String, Vec<TypeExpr>>;

/// Named type definition for μ-types (isorecursive unfolding).
#[derive(Clone)]
pub struct TypeNameDef {
    pub params: Vec<String>,
    pub alts: Vec<(String, Vec<TypeExpr>)>,
    pub span_offset: usize,
    pub span_len: usize,
}

/// Frame mapping type names to their definitions.
pub type TypeNameDefsFrame = HashMap<String, TypeNameDef>;

/// Build a constructor typedefs frame from type declarations.
///
/// Extracts all constructor tags and their payload templates.
pub fn build_typedefs_frame(decls: &[TypeDecl]) -> TypeDefsFrame {
    let mut m: TypeDefsFrame = HashMap::new();
    for d in decls {
        match &d.body {
            TypeDefBody::Sum(alts) => {
                for (tag, args) in alts {
                    m.insert(tag.clone(), args.clone());
                }
            }
        }
    }
    m
}

/// Build a typename frame from type declarations for named types.
///
/// Stores complete type definitions including parameters and alternatives.
pub fn build_typename_frame(decls: &[TypeDecl]) -> TypeNameDefsFrame {
    let mut m = HashMap::new();
    for d in decls {
        let TypeDefBody::Sum(alts) = &d.body;
        m.insert(
            d.name.clone(),
            TypeNameDef {
                params: d.params.clone(),
                alts: alts.clone(),
                span_offset: d.span.offset,
                span_len: d.span.len,
            },
        );
    }
    m
}

/// Look up a constructor's payload template in the typedefs stack.
///
/// Searches from innermost to outermost scope.
pub(crate) fn typedefs_lookup_ctor<'a>(ctx: &'a InferCtx, tag: &str) -> Option<&'a Vec<TypeExpr>> {
    for fr in ctx.typedefs.iter().rev() {
        if let Some(v) = fr.get(tag) {
            return Some(v);
        }
    }
    None
}

/// Look up a named type definition in the typename stack.
///
/// Searches from innermost to outermost scope.
pub(crate) fn typedefs_lookup_typename<'a>(ctx: &'a InferCtx, name: &str) -> Option<&'a TypeNameDef> {
    for fr in ctx.typedef_types.iter().rev() {
        if let Some(d) = fr.get(name) {
            return Some(d);
        }
    }
    None
}

/// Check positivity (no negative occurrence) for a type expression.
///
/// Recursive types must only appear in positive positions (not on the left of arrows).
/// Returns true if the type expression is valid.
fn check_positive_occurrence(te: &TypeExpr, target: &str, polarity: bool) -> bool {
    match te {
        TypeExpr::Unit
        | TypeExpr::Int
        | TypeExpr::Float
        | TypeExpr::Bool
        | TypeExpr::Str
        | TypeExpr::Char => true,
        TypeExpr::Var(_) => true,
        TypeExpr::Hole(_) => false, // holes are not allowed in type decls
        TypeExpr::List(t) => check_positive_occurrence(t, target, polarity),
        TypeExpr::Tuple(xs) => xs.iter().all(|t| check_positive_occurrence(t, target, polarity)),
        TypeExpr::Record(fs) => {
            fs.iter().all(|(_, t)| check_positive_occurrence(t, target, polarity))
        }
        TypeExpr::Fun(a, b) => {
            check_positive_occurrence(a, target, !polarity)
                && check_positive_occurrence(b, target, polarity)
        }
        TypeExpr::Ctor { tag, args } => {
            let self_occ_ok = if tag == target { polarity } else { true };
            self_occ_ok && args.iter().all(|t| check_positive_occurrence(t, target, polarity))
        }
    }
}

/// Validate that all type declarations have positive occurrences only.
///
/// Returns an error if any recursive type appears in a negative position.
#[allow(clippy::result_large_err)]
pub fn validate_typedecls_positive(decls: &[TypeDecl]) -> Result<(), TypeError> {
    for d in decls {
        let TypeDefBody::Sum(alts) = &d.body;
        for (_tag, payloads) in alts {
            for t in payloads {
                if !check_positive_occurrence(t, &d.name, true) {
                    return Err(TypeError::NegativeOccurrence {
                        type_name: d.name.clone(),
                        span_offset: d.span.offset,
                        span_len: d.span.len,
                    });
                }
            }
        }
    }
    Ok(())
}

/// Convert TypeExpr to Type with parameter substitution.
///
/// Used for instantiating named type definitions with concrete type arguments.
#[allow(clippy::result_large_err)]
pub(crate) fn conv_typeexpr_with_subst(
    ctx: &InferCtx,
    te: &TypeExpr,
    subst: &HashMap<String, Type>,
) -> Result<Type, TypeError> {
    Ok(match te {
        TypeExpr::Unit => Type::Unit,
        TypeExpr::Int => Type::Int,
        TypeExpr::Float => Type::Float,
        TypeExpr::Bool => bool_sum_type(),
        TypeExpr::Str => Type::Str,
        TypeExpr::Char => Type::Char,
        TypeExpr::List(t) => Type::List(Box::new(conv_typeexpr_with_subst(ctx, t, subst)?)),
        TypeExpr::Tuple(xs) => Type::Tuple(
            xs.iter()
                .map(|t| conv_typeexpr_with_subst(ctx, t, subst))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        TypeExpr::Record(fs) => {
            let mut m = BTreeMap::new();
            for (k, v) in fs {
                m.insert(k.clone(), (conv_typeexpr_with_subst(ctx, v, subst)?, None));
            }
            Type::Record(m)
        }
        TypeExpr::Fun(a, b) => Type::fun(
            conv_typeexpr_with_subst(ctx, a, subst)?,
            conv_typeexpr_with_subst(ctx, b, subst)?,
        ),
        TypeExpr::Ctor { tag, args } => {
            let conv_args = args
                .iter()
                .map(|t| conv_typeexpr_with_subst(ctx, t, subst))
                .collect::<Result<Vec<_>, _>>()?;
            if typedefs_lookup_typename(ctx, tag).is_some() {
                Type::Named { name: tag.clone(), args: conv_args }
            } else {
                Type::Ctor { tag: tag.clone(), payload: conv_args }
            }
        }
        TypeExpr::Var(nm) => match subst.get(nm) {
            Some(t) => t.clone(),
            None => {
                return Err(TypeError::InvalidTypeDecl {
                    msg: format!("unbound type variable '%{} in type declaration", nm),
                    span_offset: 0,
                    span_len: 0,
                })
            }
        },
        TypeExpr::Hole(_) => {
            return Err(TypeError::InvalidTypeDecl {
                msg: "holes are not allowed in type declarations".into(),
                span_offset: 0,
                span_len: 0,
            })
        }
    })
}

/// Convert TypeExpr to Type, generating fresh type variables for Var and Hole.
///
/// Used during type inference when dealing with type annotations.
pub fn conv_typeexpr_fresh(tv: &mut TvGen, te: &TypeExpr) -> Type {
    match te {
        TypeExpr::Unit => Type::Unit,
        TypeExpr::Int => Type::Int,
        TypeExpr::Float => Type::Float,
        TypeExpr::Bool => bool_sum_type(),
        TypeExpr::Str => Type::Str,
        TypeExpr::Char => Type::Char,
        TypeExpr::List(t) => Type::List(Box::new(conv_typeexpr_fresh(tv, t))),
        TypeExpr::Tuple(xs) => Type::Tuple(xs.iter().map(|t| conv_typeexpr_fresh(tv, t)).collect()),
        TypeExpr::Record(fs) => {
            let mut m = BTreeMap::new();
            for (k, v) in fs {
                m.insert(k.clone(), conv_typeexpr_fresh(tv, v));
            }
            Type::Record(m.into_iter().map(|(k, v)| (k, (v, None))).collect())
        }
        TypeExpr::Fun(a, b) => Type::fun(conv_typeexpr_fresh(tv, a), conv_typeexpr_fresh(tv, b)),
        TypeExpr::Ctor { tag, args } => {
            let payload = args.iter().map(|t| conv_typeexpr_fresh(tv, t)).collect();
            Type::Ctor { tag: tag.clone(), payload }
        }
        TypeExpr::Var(_) => tv.fresh(),
        TypeExpr::Hole(_) => tv.fresh(),
    }
}

/// Instantiate a named sum type with concrete type arguments.
///
/// Unfolds the named type definition, substituting parameters with arguments.
#[allow(clippy::result_large_err)]
pub(crate) fn instantiate_named_sum(
    ctx: &InferCtx,
    name: &str,
    args: &[Type],
) -> Result<Vec<(String, Vec<Type>)>, TypeError> {
    let def = typedefs_lookup_typename(ctx, name).ok_or_else(|| TypeError::InvalidTypeDecl {
        msg: format!("unknown type name {}", name),
        span_offset: 0,
        span_len: 0,
    })?;
    if def.params.len() != args.len() {
        return Err(TypeError::InvalidTypeDecl {
            msg: format!(
                "type {} arity mismatch: expected {}, got {}",
                name,
                def.params.len(),
                args.len()
            ),
            span_offset: def.span_offset,
            span_len: def.span_len,
        });
    }
    let mut map = HashMap::new();
    for (p, a) in def.params.iter().zip(args.iter()) {
        map.insert(p.clone(), a.clone());
    }
    let mut out = Vec::with_capacity(def.alts.len());
    for (tag, payload_tes) in &def.alts {
        let mut payload = Vec::with_capacity(payload_tes.len());
        for te in payload_tes {
            payload.push(conv_typeexpr_with_subst(ctx, te, &map)?);
        }
        out.push((tag.clone(), payload));
    }
    Ok(out)
}
