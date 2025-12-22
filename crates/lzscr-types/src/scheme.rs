//! Type schemes, substitutions, and type environments for HM inference.
//!
//! This module provides:
//! - `Scheme`: Polytype with universally quantified type variables
//! - `Subst`: Substitution mapping from type variables to types
//! - `TypeEnv`: Environment mapping identifiers to type schemes
//! - `instantiate`: Create fresh monomorphic instance of a scheme
//! - `generalize`: Abstract free type variables into a scheme
//! - `TypesApply` trait: Apply substitutions and compute free type variables
//! - `zonk_type`: Deeply resolve type variables (chase substitution chains)
//! - `normalize_tuples`: Canonicalize tuple constructor tags
//! - `TvGen`: Fresh type variable generator

use crate::{TvId, Type};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

/// Type variable generator: produces fresh type variables.
pub struct TvGen {
    next: u32,
}

impl TvGen {
    pub fn new() -> Self {
        Self { next: 0 }
    }

    pub fn fresh(&mut self) -> Type {
        let id = self.next;
        self.next += 1;
        Type::Var(TvId(id))
    }
}

impl Default for TvGen {
    fn default() -> Self {
        Self::new()
    }
}

/// A type scheme with universally quantified type variables.
///
/// Represents polytypes in the Hindley-Milner type system (e.g., `∀a. a -> a`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<TvId>,
    pub ty: Type,
}

/// Substitution: mapping from type variables to types.
///
/// Used during unification to record type variable bindings.
#[derive(Default, Debug, Clone)]
pub struct Subst(pub(crate) HashMap<TvId, Type>);

impl Subst {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn singleton(v: TvId, t: Type) -> Self {
        let mut m = HashMap::new();
        m.insert(v, t);
        Self(m)
    }

    pub fn compose(mut self, rhs: Subst) -> Subst {
        // apply rhs first, then self
        let mut out = Subst::new();
        for (k, v) in self.0.drain() {
            out.0.insert(k, v.apply(&rhs));
        }
        for (k, v) in rhs.0.into_iter() {
            out.0.insert(k, v);
        }
        out
    }
}

// -----------------------------------------------------------------------------
// Traversal helpers: zonk_type vs .apply() vs .ftv()
// -----------------------------------------------------------------------------
//  zonk_type:
//    - Fully normalises a type by repeatedly following substitution mappings.
//    - It aggressively chases Var chains (v -> T, where T may again contain Vars)
//      until it reaches a fixed point of concrete structure or an unbound Var.
//    - Use when producing a final, user-facing or serialization-stable form, or
//      right before pretty-printing for error messages where lingering indirections
//      (%tN bound to another %tM) would be confusing.
//
//  Type::apply / Scheme::apply / Subst::apply:
//    - Perform a single structural pass replacing any directly mapped Vars with
//      their images, WITHOUT recursively re-zonking what those images themselves
//      might reference (except through further recursive descent of structure).
//    - This preserves sharing opportunities and avoids quadratic blow‑ups during
//      repeated unification steps. Unifier composes substitutions instead of
//      eagerly normalising; later a zonk pass can collapse chains.
//
//  ftv (free type variables):
//    - Collects all unbound Vars reachable in the current (possibly still indirect)
//      representation. Because we do not zonk during collection, a Var that is
//      mapped inside the substitution but not yet structurally applied will not
//      appear spuriously (the calling site usually applies first where needed).
//
// Practical guideline:
//    * During inference/unify: prefer .apply() to keep cost low.
//    * When exporting a Scheme or reporting an error: zonk_type() then pass to
//      user_pretty_type (which internally performs its own normalisation step).
//    * If adding new traversals, keep them non-recursive over substitution chains
//      unless the intent is explicit finalisation (like zonk_type).
// -----------------------------------------------------------------------------

/// Deeply resolve (zonk) a type under a substitution: recursively chase Var chains.
///
/// Use this when producing final, user-facing types or before pretty-printing.
pub fn zonk_type(t: &Type, s: &Subst) -> Type {
    match t {
        Type::Var(v) => {
            if let Some(inner) = s.0.get(v) {
                zonk_type(inner, s)
            } else {
                Type::Var(*v)
            }
        }
        Type::Fun(a, b) => Type::fun(zonk_type(a, s), zonk_type(b, s)),
        Type::List(x) => Type::List(Box::new(zonk_type(x, s))),
        Type::Tuple(xs) => Type::Tuple(xs.iter().map(|x| zonk_type(x, s)).collect()),
        Type::Record(fs) => {
            let mut m = BTreeMap::new();
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (zonk_type(v, s), *sp));
            }
            Type::Record(m)
        }
        Type::ModeMap(fs, def, _kind) => {
            let mut m = BTreeMap::new();
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (zonk_type(v, s), *sp));
            }
            let d = def.as_ref().map(|b| Box::new(zonk_type(b, s)));
            Type::ModeMap(m, d, crate::types::ModeKind::Explicit)
        }
        Type::Ctor { tag, payload } => Type::Ctor {
            tag: tag.clone(),
            payload: payload.iter().map(|x| zonk_type(x, s)).collect(),
        },
        Type::Named { name, args } => {
            Type::Named { name: name.clone(), args: args.iter().map(|x| zonk_type(x, s)).collect() }
        }
        Type::SumCtor(vs) => Type::SumCtor(
            vs.iter()
                .map(|(n, ps)| (n.clone(), ps.iter().map(|x| zonk_type(x, s)).collect()))
                .collect(),
        ),
        primitive => primitive.clone(),
    }
}

/// Normalize tuple-like constructor tags (., .,, .,,, ...) into canonical Type::Tuple.
///
/// This canonicalization ensures consistent tuple representation.
pub fn normalize_tuples(t: &Type) -> Type {
    match t {
        Type::Ctor { tag, payload }
            if tag.starts_with('.') && tag.chars().skip(1).all(|c| c == ',') =>
        {
            // arity = payload.len(); treat as tuple of payload
            let items = payload.iter().map(normalize_tuples).collect();
            Type::Tuple(items)
        }
        Type::Fun(a, b) => Type::fun(normalize_tuples(a), normalize_tuples(b)),
        Type::List(x) => Type::List(Box::new(normalize_tuples(x))),
        Type::Tuple(xs) => Type::Tuple(xs.iter().map(normalize_tuples).collect()),
        Type::Record(fs) => {
            let mut m = BTreeMap::new();
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (normalize_tuples(v), *sp));
            }
            Type::Record(m)
        }
        Type::ModeMap(fs, def, kind) => {
            let mut m = BTreeMap::new();
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (normalize_tuples(v), *sp));
            }
            let d = def.as_ref().map(|b| Box::new(normalize_tuples(b)));
            Type::ModeMap(m, d, *kind)
        }
        Type::Ctor { tag, payload } => {
            Type::Ctor { tag: tag.clone(), payload: payload.iter().map(normalize_tuples).collect() }
        }
        Type::Named { name, args } => {
            Type::Named { name: name.clone(), args: args.iter().map(normalize_tuples).collect() }
        }
        Type::SumCtor(vs) => Type::SumCtor(
            vs.iter()
                .map(|(n, ps)| (n.clone(), ps.iter().map(normalize_tuples).collect()))
                .collect(),
        ),
        primitive => primitive.clone(),
    }
}

/// Trait for applying substitutions and computing free type variables.
pub trait TypesApply {
    fn apply(&self, s: &Subst) -> Self;
    fn ftv(&self) -> HashSet<TvId>;
}

impl TypesApply for Type {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Type::Var(v) => s.0.get(v).cloned().unwrap_or(Type::Var(*v)),
            Type::Fun(a, b) => Type::fun(a.apply(s), b.apply(s)),
            Type::List(x) => Type::List(Box::new(x.apply(s))),
            Type::Tuple(xs) => Type::Tuple(xs.iter().map(|t| t.apply(s)).collect()),
            Type::Record(fs) => {
                let mut m = BTreeMap::new();
                for (k, (v, sp)) in fs {
                    m.insert(k.clone(), (v.apply(s), *sp));
                }
                Type::Record(m)
            }
            Type::ModeMap(fs, def, kind) => {
                let mut m = BTreeMap::new();
                for (k, (v, sp)) in fs {
                    m.insert(k.clone(), (v.apply(s), *sp));
                }
                let d = def.as_ref().map(|b| Box::new(b.apply(s)));
                Type::ModeMap(m, d, *kind)
            }
            Type::Ctor { tag, payload } => Type::Ctor {
                tag: tag.clone(),
                payload: payload.iter().map(|t| t.apply(s)).collect(),
            },
            Type::Named { name, args } => {
                Type::Named { name: name.clone(), args: args.iter().map(|t| t.apply(s)).collect() }
            }
            Type::SumCtor(vs) => Type::SumCtor(
                vs.iter()
                    .map(|(n, ps)| (n.clone(), ps.iter().map(|t| t.apply(s)).collect()))
                    .collect(),
            ),
            t @ (Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char | Type::Type) => {
                t.clone()
            }
        }
    }

    fn ftv(&self) -> HashSet<TvId> {
        let mut s = HashSet::new();
        match self {
            Type::Var(v) => {
                s.insert(*v);
            }
            Type::Fun(a, b) => {
                s.extend(a.ftv());
                s.extend(b.ftv());
            }
            Type::List(t) => {
                s.extend(t.ftv());
            }
            Type::Tuple(ts) => {
                for t in ts {
                    s.extend(t.ftv());
                }
            }
            Type::Record(fs) => {
                for (v, _) in fs.values() {
                    s.extend(v.ftv());
                }
            }
            Type::ModeMap(fs, def, _kind) => {
                for (v, _) in fs.values() {
                    s.extend(v.ftv());
                }
                if let Some(d) = def {
                    s.extend(d.ftv());
                }
            }
            Type::Ctor { payload, .. } => {
                for t in payload {
                    s.extend(t.ftv());
                }
            }
            Type::Named { args, .. } => {
                for t in args {
                    s.extend(t.ftv());
                }
            }
            Type::SumCtor(vs) => {
                for (_, ps) in vs {
                    for t in ps {
                        s.extend(t.ftv());
                    }
                }
            }
            Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char | Type::Type => {}
        }
        s
    }
}

impl TypesApply for Scheme {
    fn apply(&self, s: &Subst) -> Self {
        // do not apply to bound vars
        let mut s2 = Subst::new();
        for (k, v) in s.0.iter() {
            if !self.vars.contains(k) {
                s2.0.insert(*k, v.clone());
            }
        }
        Scheme { vars: self.vars.clone(), ty: self.ty.apply(&s2) }
    }

    fn ftv(&self) -> HashSet<TvId> {
        let mut s = self.ty.ftv();
        for v in &self.vars {
            s.remove(v);
        }
        s
    }
}

/// Type environment: maps identifiers to type schemes.
#[derive(Default, Debug, Clone)]
pub struct TypeEnv(pub(crate) HashMap<String, Scheme>);

impl TypeEnv {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, n: String, s: Scheme) {
        self.0.insert(n, s);
    }

    pub fn get(&self, n: &str) -> Option<Scheme> {
        self.0.get(n).cloned()
    }
}

/// Instantiate a type scheme with fresh type variables.
///
/// Replaces all quantified variables with fresh ones, returning a monomorphic type.
pub fn instantiate(tv: &mut TvGen, s: &Scheme) -> Type {
    let mut m = HashMap::<TvId, Type>::new();
    for v in &s.vars {
        m.insert(*v, tv.fresh());
    }
    let sub = Subst(m);
    s.ty.apply(&sub)
}

/// Generalize a type into a scheme by quantifying over free type variables.
///
/// Variables free in the type but not in the environment become quantified.
pub fn generalize(env: &TypeEnv, t: &Type) -> Scheme {
    let env_ftv: HashSet<TvId> = env.0.values().flat_map(|s| s.ftv()).collect();
    let t_ftv = t.ftv();
    let vars: Vec<TvId> = t_ftv.difference(&env_ftv).cloned().collect();
    Scheme { vars, ty: t.clone() }
}
