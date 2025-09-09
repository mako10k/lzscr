//! lzscr-types: HM type inference (MVP)
//!
//! Implements a minimal rank-1 HM inference over lzscr AST, including AltLambda
//! typing with Ctor-limited union (SumCtor) for lambda chains.

use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

// ---------- Type Core ----------

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
pub struct TvId(pub u32);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Unit,
    Int,
    Float,
    Str,
    Char,
    Var(TvId),
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Record(BTreeMap<String, Type>),
    Ctor { tag: String, payload: Vec<Type> },
    // Named ADT (from % type declarations), e.g., Option a
    Named { name: String, args: Vec<Type> },
    // Limited union for constructor tags used by AltLambda chains.
    // Invariants (after construction):
    //  * Variants sorted lexicographically by tag
    //  * No duplicate tags
    //  * Only used when there are >= 2 variants (single variant collapses to Ctor form)
    SumCtor(Vec<(String, Vec<Type>)>),
}

impl Type {
    fn fun(a: Type, b: Type) -> Type {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

// Canonical Bool representation as a sum of two zero-arity constructors.
fn bool_sum_type() -> Type {
    Type::SumCtor(vec![(".False".into(), vec![]), (".True".into(), vec![])])
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<TvId>,
    pub ty: Type,
}

#[derive(Default, Debug, Clone)]
pub struct Subst(HashMap<TvId, Type>);

impl Subst {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn singleton(v: TvId, t: Type) -> Self {
        let mut m = HashMap::new();
        m.insert(v, t);
        Self(m)
    }
    fn compose(mut self, rhs: Subst) -> Subst {
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

// Deeply resolve (zonk) a type under a substitution: recursively chase Var chains.
fn zonk_type(t: &Type, s: &Subst) -> Type {
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
            for (k, v) in fs {
                m.insert(k.clone(), zonk_type(v, s));
            }
            Type::Record(m)
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

// Normalize tuple-like constructor tags (.,  .,,  .,,, ...) into canonical Type::Tuple.
fn normalize_tuples(t: &Type) -> Type {
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
            for (k, v) in fs {
                m.insert(k.clone(), normalize_tuples(v));
            }
            Type::Record(m)
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

trait TypesApply {
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
                for (k, v) in fs {
                    m.insert(k.clone(), v.apply(s));
                }
                Type::Record(m)
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
            t @ (Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char) => t.clone(),
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
                for v in fs.values() {
                    s.extend(v.ftv());
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
            Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char => {}
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

impl TypesApply for Subst {
    fn apply(&self, s: &Subst) -> Self {
        let mut out = Subst::new();
        for (k, v) in self.0.iter() {
            out.0.insert(*k, v.apply(s));
        }
        out
    }
    fn ftv(&self) -> HashSet<TvId> {
        let mut s = HashSet::new();
        for v in self.0.values() {
            s.extend(v.ftv());
        }
        s
    }
}

#[derive(Default)]
struct TvGen {
    next: u32,
}
impl TvGen {
    fn fresh(&mut self) -> Type {
        let id = self.next;
        self.next += 1;
        Type::Var(TvId(id))
    }
}

// ---------- TypeDef frames (ctor templates from % declarations) ----------

type TypeDefsFrame = HashMap<String, Vec<TypeExpr>>; // tag -> payload type templates (AST TypeExpr)

fn build_typedefs_frame(decls: &[TypeDecl]) -> TypeDefsFrame {
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

fn typedefs_lookup_ctor<'a>(ctx: &'a InferCtx, tag: &str) -> Option<&'a Vec<TypeExpr>> {
    for fr in ctx.typedefs.iter().rev() {
        if let Some(v) = fr.get(tag) {
            return Some(v);
        }
    }
    None
}

// Named type definitions (for μ-types via isorecursive unfolding)
#[derive(Clone)]
struct TypeNameDef {
    params: Vec<String>,
    alts: Vec<(String, Vec<TypeExpr>)>,
    span_offset: usize,
    span_len: usize,
}
type TypeNameDefsFrame = HashMap<String, TypeNameDef>; // name -> def

fn build_typename_frame(decls: &[TypeDecl]) -> TypeNameDefsFrame {
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

fn typedefs_lookup_typename<'a>(ctx: &'a InferCtx, name: &str) -> Option<&'a TypeNameDef> {
    for fr in ctx.typedef_types.iter().rev() {
        if let Some(d) = fr.get(name) {
            return Some(d);
        }
    }
    None
}

// ---------- Errors ----------

#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    #[error(
        "type mismatch: expected {expected:?} vs actual {actual:?} at ({span_offset},{span_len})"
    )]
    Mismatch { expected: Type, actual: Type, span_offset: usize, span_len: usize },
    #[error("occurs check failed: {var:?} in {ty:?}")]
    Occurs { var: TvId, ty: Type },
    #[error("constructor union duplicate tag: {tag}")]
    DuplicateCtorTag { tag: String },
    #[error("AltLambda branches mixed: expected all Ctor patterns or wildcard/default only at ({span_offset},{span_len})")]
    MixedAltBranches { span_offset: usize, span_len: usize },
    #[error("not a function: {ty:?}")]
    NotFunction { ty: Type },
    #[error("unbound reference: {name} at ({span_offset},{span_len})")]
    UnboundRef { name: String, span_offset: usize, span_len: usize },
    #[error("effect not allowed at ({span_offset},{span_len})")]
    EffectNotAllowed { span_offset: usize, span_len: usize },
    #[error("negative occurrence of recursive type {type_name} at ({span_offset},{span_len})")]
    NegativeOccurrence { type_name: String, span_offset: usize, span_len: usize },
    #[error("invalid type declaration: {msg} at ({span_offset},{span_len})")]
    InvalidTypeDecl { msg: String, span_offset: usize, span_len: usize },
}

// ---------- Unification ----------

fn occurs(v: TvId, t: &Type) -> bool {
    t.ftv().contains(&v)
}

#[allow(clippy::result_large_err)]
fn unify(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Var(x), t) => bind(*x, t.clone()),
        (t, Type::Var(x)) => bind(*x, t.clone()),
        (Type::Unit, Type::Unit)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Str, Type::Str)
        | (Type::Char, Type::Char) => Ok(Subst::new()),
        (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
            let s1 = unify(a1, a2)?;
            let s2 = unify(&b1.apply(&s1), &b2.apply(&s1))?;
            Ok(s2.compose(s1))
        }
        (Type::List(x), Type::List(y)) => unify(x, y),
        (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
            let mut s = Subst::new();
            let it = xs.iter().zip(ys.iter());
            for (x, y) in it {
                let s1 = unify(&x.apply(&s), &y.apply(&s))?;
                s = s1.compose(s);
            }
            Ok(s)
        }
        (Type::Record(rx), Type::Record(ry)) if rx.len() == ry.len() => {
            let mut s = Subst::new();
            for (k, vx) in rx.iter() {
                let vy = ry.get(k).ok_or_else(|| TypeError::Mismatch {
                    expected: Type::Record(rx.clone()),
                    actual: Type::Record(ry.clone()),
                    span_offset: 0,
                    span_len: 0,
                })?;
                let s1 = unify(&vx.apply(&s), &vy.apply(&s))?;
                s = s1.compose(s);
            }
            Ok(s)
        }
        (Type::Ctor { tag: ta, payload: pa }, Type::Ctor { tag: tb, payload: pb })
            if ta == tb && pa.len() == pb.len() =>
        {
            let mut s = Subst::new();
            for (x, y) in pa.iter().zip(pb.iter()) {
                let s1 = unify(&x.apply(&s), &y.apply(&s))?;
                s = s1.compose(s);
            }
            Ok(s)
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
                for (x, y) in psa.iter().zip(psb.iter()) {
                    let s1 = unify(&x.apply(&s), &y.apply(&s))?;
                    s = s1.compose(s);
                }
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
                let mut s = Subst::new();
                for (x, y) in payload.iter().zip(ps_other.iter()) {
                    let s1 = unify(&x.apply(&s), &y.apply(&s))?;
                    s = s1.compose(s);
                }
                Ok(s)
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

// Positivity (no negative occurrence) check for a type declaration body
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

#[allow(clippy::result_large_err)]
fn validate_typedecls_positive(decls: &[TypeDecl]) -> Result<(), TypeError> {
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

// Convert TypeExpr to Type for instantiating named type defs (substitute params with args)
#[allow(clippy::result_large_err)]
fn conv_typeexpr_with_subst(
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
                m.insert(k.clone(), conv_typeexpr_with_subst(ctx, v, subst)?);
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

#[allow(clippy::result_large_err)]
fn instantiate_named_sum(
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

// Unification with awareness of Named types (μ-types): one-step unfold into SumCtor
#[allow(clippy::result_large_err)]
fn ctx_unify(ctx: &InferCtx, a: &Type, b: &Type) -> Result<Subst, TypeError> {
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
    let res = match (&na, &nb) {
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

#[allow(clippy::result_large_err)]
fn bind(v: TvId, t: Type) -> Result<Subst, TypeError> {
    match t {
        Type::Var(v2) if v == v2 => Ok(Subst::new()),
        _ if occurs(v, &t) => Err(TypeError::Occurs { var: v, ty: t }),
        _ => Ok(Subst::singleton(v, t)),
    }
}

// ---------- Env / Generalize ----------

#[derive(Clone, Default)]
pub struct TypeEnv(HashMap<String, Scheme>);

impl TypeEnv {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn insert(&mut self, n: String, s: Scheme) {
        self.0.insert(n, s);
    }
    fn get(&self, n: &str) -> Option<Scheme> {
        self.0.get(n).cloned()
    }
}

fn instantiate(tv: &mut TvGen, s: &Scheme) -> Type {
    let mut m = HashMap::<TvId, Type>::new();
    for v in &s.vars {
        m.insert(*v, tv.fresh());
    }
    let sub = Subst(m);
    s.ty.apply(&sub)
}

fn generalize(env: &TypeEnv, t: &Type) -> Scheme {
    let env_ftv: HashSet<TvId> = env.0.values().flat_map(|s| s.ftv()).collect();
    let t_ftv = t.ftv();
    let vars: Vec<TvId> = t_ftv.difference(&env_ftv).cloned().collect();
    Scheme { vars, ty: t.clone() }
}

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
                tys.push(ctx.tv.fresh());
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
            let a = ctx.tv.fresh();
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
            let a = ctx.tv.fresh();
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
                want.insert(k.clone(), ctx.tv.fresh());
            }
            let s0 = ctx_unify(ctx, scrutinee, &Type::Record(want.clone()))?;
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
                    (0..eff_args.len()).map(|_| ctx.tv.fresh()).collect()
                }
            } else {
                (0..eff_args.len()).map(|_| ctx.tv.fresh()).collect()
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
            let Type::Var(id) = ctx.tv.fresh() else { unreachable!() };
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

// Convert an AST TypeExpr coming from a %type declaration payload into a fresh Type.
// All type variables are instantiated to fresh Type::Var; holes are treated as fresh as well.
fn conv_typeexpr_fresh(tv: &mut TvGen, te: &TypeExpr) -> Type {
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
            Type::Record(m)
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
    ctx.depth += 1;
    // Helper: convert TypeExpr to Type using ctx.tyvars and a local holes table for '?'
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
                Type::Record(m)
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
                        let Type::Var(v) = ctx.tv.fresh() else { unreachable!() };
                        v
                    });
                    Type::Var(*id)
                }
            }
            TypeExpr::Hole(opt) => {
                if let Some(n) = opt.as_ref() {
                    let key = format!("?{}", n);
                    let id = holes.entry(key).or_insert_with(|| {
                        let Type::Var(v) = ctx.tv.fresh() else { unreachable!() };
                        v
                    });
                    Type::Var(*id)
                } else {
                    let Type::Var(v) = ctx.tv.fresh() else { unreachable!() };
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
            let s2 = ctx_unify(ctx, &got.apply(&s), &want)?;
            Ok((want.apply(&s2), s2.compose(s)))
        }
        ExprKind::TypeVal(_ty) => Ok((Type::Str, Subst::new())),
        ExprKind::Unit => Ok((Type::Unit, Subst::new())),
        ExprKind::Int(_) => Ok((Type::Int, Subst::new())),
        ExprKind::Float(_) => Ok((Type::Float, Subst::new())),
        ExprKind::Str(_) => Ok((Type::Str, Subst::new())),
        ExprKind::Char(_) => Ok((Type::Char, Subst::new())),
        ExprKind::Ref(n) => {
            let s = ctx.env.get(n).ok_or_else(|| TypeError::UnboundRef {
                name: n.clone(),
                span_offset: e.span.offset,
                span_len: e.span.len,
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
            let a = ctx.tv.fresh();
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
                            let r = ctx.tv.fresh();
                            let s1 = ctx_unify(ctx, &tt.apply(&se).apply(&s_acc), &r)?;
                            let s2 = ctx_unify(ctx, &te.apply(&s1).apply(&se).apply(&s_acc), &r)?;
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
                                params.push(ctx.tv.fresh());
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
                        if let Some(fty) = fs.remove(field_name) {
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
                        let r = ctx.tv.fresh();
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
            let r = ctx.tv.fresh();
            let s1 =
                ctx_unify(ctx, &tf.apply(&sa).apply(&sf), &Type::fun(ta.apply(&sa), r.clone()))?;
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
            let a = ctx.tv.fresh();
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
                // Compose so far substitution into accumulated field types if needed
                subst = sv.compose(subst);
                map.insert(k.clone(), tv.apply(&subst));
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
                let a = ctx.tv.fresh();
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
            let res = infer_expr(ctx, body, allow_effects);
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
            let r = ctx.tv.fresh();
            Ok((r, s))
        }
        ExprKind::OrElse { left, right } => {
            let (tl, sl) = infer_expr(ctx, left, allow_effects)?;
            let (tr, sr) = infer_expr(ctx, right, allow_effects)?;
            let s1 = ctx_unify(ctx, &tl.apply(&sr).apply(&sl), &tr.apply(&sr))?;
            let s = s1.compose(sr).compose(sl);
            Ok((tr.apply(&s), s))
        }
        ExprKind::Catch { left, right } => {
            let (tl, sl) = infer_expr(ctx, left, allow_effects)?;
            let a = ctx.tv.fresh();
            let r = ctx.tv.fresh();
            let (tr, sr) = infer_expr(ctx, right, allow_effects)?;
            let s1 = ctx_unify(ctx, &tr.apply(&sr).apply(&sl), &Type::fun(a, r.clone()))?;
            let s2 = ctx_unify(ctx, &tl.apply(&s1).apply(&sr).apply(&sl), &r)?;
            let s = s2.compose(s1).compose(sr).compose(sl);
            Ok((r.apply(&s), s))
        }
        ExprKind::AltLambda { left, right } => {
            // --- Flatten nested AltLambda chain into a Vec of branch expressions ---
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

            // Extract (pattern, body) per branch ensuring each is a Lambda
            struct Br<'a> {
                pat: &'a Pattern,
                body: &'a Expr,
                span: Span,
            }
            let mut raw: Vec<Br> = vec![];
            for b in &branches {
                match &b.kind {
                    ExprKind::Lambda { param, body } => {
                        raw.push(Br { pat: param, body, span: b.span })
                    }
                    _ => {
                        return Err(TypeError::MixedAltBranches {
                            span_offset: b.span.offset,
                            span_len: b.span.len,
                        })
                    }
                }
            }

            // Peel type binders per branch (store pushed count to pop later)
            struct Peeled<'a> {
                core: &'a Pattern,
                pushed: usize,
                body: &'a Expr,
                span: Span,
            }
            let mut branches_p: Vec<Peeled> = vec![];
            for br in raw {
                let (core, pushed) = push_tyvars_from_pattern(ctx, br.pat);
                branches_p.push(Peeled { core, pushed, body: br.body, span: br.span });
            }

            // Classification pass: detect ctor-like vs structural vs wildcard
            let mut any_ctor = false;
            let mut any_structural = false; // non-ctor non-wildcard patterns
            let mut wildcard_pos: Option<usize> = None;
            for (i, br) in branches_p.iter().enumerate() {
                match br.core.kind {
                    PatternKind::Ctor { .. } | PatternKind::Symbol(_) => any_ctor = true,
                    PatternKind::Wildcard => {
                        wildcard_pos = Some(i);
                    }
                    _ => any_structural = true,
                }
            }
            // Reject mixing ctor-like with structural (except wildcard only branch) & wildcard not last
            if any_ctor && any_structural {
                let span_offset = branches_p.first().unwrap().span.offset;
                let last = branches_p.last().unwrap();
                let span_len = (last.span.offset + last.span.len).saturating_sub(span_offset);
                // Pop pushed tyvars before returning
                for br in branches_p.into_iter().rev() {
                    pop_tyvars(ctx, br.pushed);
                }
                return Err(TypeError::MixedAltBranches { span_offset, span_len });
            }
            if let Some(wpos) = wildcard_pos {
                if wpos != branches_p.len() - 1 {
                    let span_offset = branches_p[wpos].span.offset;
                    let span_len = branches_p[wpos].span.len;
                    for br in branches_p.into_iter().rev() {
                        pop_tyvars(ctx, br.pushed);
                    }
                    return Err(TypeError::MixedAltBranches { span_offset, span_len });
                }
            }

            // Accumulators
            let mut s_all = Subst::new();
            let mut param_ty = ctx.tv.fresh();

            // Constructor variants (unique tags) with shared unified payload vectors
            use std::collections::BTreeMap;
            let mut ctor_payloads: BTreeMap<String, Vec<Type>> = BTreeMap::new();

            // Return type accumulation (reuse previous helper logic)
            let mut ret_variants: Vec<(String, Vec<Type>)> = vec![];
            let mut ret_seen = HashSet::<String>::new();
            let mut ret_other: Option<Type> = None;
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

            // Helper to infer one branch
            for (idx, br) in branches_p.iter().enumerate() {
                let core = br.core;
                let body = br.body;
                match &core.kind {
                    PatternKind::Wildcard => {
                        let pi = infer_pattern(ctx, core, &param_ty.apply(&s_all))?;
                        let mut env2 = ctx.env.clone();
                        for (n, t) in &pi.bindings {
                            env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                        }
                        let prev = std::mem::replace(&mut ctx.env, env2);
                        let (tb, sb) = infer_expr(ctx, body, allow_effects)?;
                        ctx.env = prev;
                        let tb_final = tb.apply(&sb).apply(&pi.subst);
                        let tbf_applied = tb_final.clone();
                        if !merge_ret(&mut ret_variants, &mut ret_seen, &tbf_applied) {
                            match &mut ret_other {
                                None => ret_other = Some(tbf_applied),
                                Some(existing) => {
                                    let s1 = unify(&existing.clone(), &tbf_applied)?;
                                    *existing = existing.apply(&s1);
                                    s_all = s1.compose(s_all);
                                }
                            }
                        }
                        s_all = sb.compose(pi.subst).compose(s_all);
                        param_ty = param_ty.apply(&s_all);
                        // Wildcard must be last (already checked); remaining branches won't exist
                        if idx != branches_p.len() - 1 {
                            unreachable!();
                        }
                    }
                    PatternKind::Ctor { name, args } => {
                        // Reuse or create payload vector
                        let payload = if let Some(v) = ctor_payloads.get(name) {
                            v.clone()
                        } else {
                            let v: Vec<Type> = (0..args.len()).map(|_| ctx.tv.fresh()).collect();
                            ctor_payloads.insert(name.clone(), v.clone());
                            v
                        };
                        if payload.len() != args.len() {
                            // Same tag seen before with different arity -> treat as duplicate tag error
                            return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                        }
                        let scr = Type::Ctor { tag: name.clone(), payload: payload.clone() };
                        let pi = infer_pattern(ctx, core, &scr.apply(&s_all))?;
                        let mut env2 = ctx.env.clone();
                        for (n, t) in &pi.bindings {
                            env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                        }
                        let prev = std::mem::replace(&mut ctx.env, env2);
                        let (tb, sb) = infer_expr(ctx, body, allow_effects)?;
                        ctx.env = prev;
                        let tb_final = tb.apply(&sb).apply(&pi.subst);
                        let tbf_applied = tb_final.clone();
                        if !merge_ret(&mut ret_variants, &mut ret_seen, &tbf_applied) {
                            match &mut ret_other {
                                None => ret_other = Some(tbf_applied),
                                Some(existing) => {
                                    let s1 = unify(&existing.clone(), &tbf_applied)?;
                                    *existing = existing.apply(&s1);
                                    s_all = s1.compose(s_all);
                                }
                            }
                        }
                        s_all = sb.compose(pi.subst).compose(s_all);
                    }
                    PatternKind::Symbol(name) => {
                        let payload = ctor_payloads.entry(name.clone()).or_default().clone();
                        if !payload.is_empty() {
                            return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                        }
                        let scr = Type::Ctor { tag: name.clone(), payload }; // zero-arg
                        let pi = infer_pattern(ctx, core, &scr.apply(&s_all))?;
                        let mut env2 = ctx.env.clone();
                        for (n, t) in &pi.bindings {
                            env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                        }
                        let prev = std::mem::replace(&mut ctx.env, env2);
                        let (tb, sb) = infer_expr(ctx, body, allow_effects)?;
                        ctx.env = prev;
                        let tb_final = tb.apply(&sb).apply(&pi.subst);
                        let tbf_applied = tb_final.clone();
                        if !merge_ret(&mut ret_variants, &mut ret_seen, &tbf_applied) {
                            match &mut ret_other {
                                None => ret_other = Some(tbf_applied),
                                Some(existing) => {
                                    let s1 = unify(&existing.clone(), &tbf_applied)?;
                                    *existing = existing.apply(&s1);
                                    s_all = s1.compose(s_all);
                                }
                            }
                        }
                        s_all = sb.compose(pi.subst).compose(s_all);
                    }
                    // Structural (lists/tuples/records/cons/etc.) – only allowed if no ctor-like branches overall
                    _ => {
                        let pi = infer_pattern(ctx, core, &param_ty.apply(&s_all))?;
                        let mut env2 = ctx.env.clone();
                        for (n, t) in &pi.bindings {
                            env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() });
                        }
                        let prev = std::mem::replace(&mut ctx.env, env2);
                        let (tb, sb) = infer_expr(ctx, body, allow_effects)?;
                        ctx.env = prev;
                        let tb_final = tb.apply(&sb).apply(&pi.subst);
                        let tbf_applied = tb_final.clone();
                        if !merge_ret(&mut ret_variants, &mut ret_seen, &tbf_applied) {
                            match &mut ret_other {
                                None => ret_other = Some(tbf_applied),
                                Some(existing) => {
                                    let s1 = unify(&existing.clone(), &tbf_applied)?;
                                    *existing = existing.apply(&s1);
                                    s_all = s1.compose(s_all);
                                }
                            }
                        }
                        s_all = sb.compose(pi.subst).compose(s_all);
                        param_ty = param_ty.apply(&s_all);
                    }
                }
            }

            // Pop type variable frames pushed per branch (reverse order)
            for br in branches_p.iter().rev() {
                pop_tyvars(ctx, br.pushed);
            }

            // Finalize parameter type if ctor branches present
            if !ctor_payloads.is_empty() {
                if ctor_payloads.len() == 1 {
                    let (tag, payload) = ctor_payloads.into_iter().next().unwrap();
                    param_ty = Type::Ctor { tag, payload };
                } else {
                    let variants: Vec<(String, Vec<Type>)> = ctor_payloads.into_iter().collect();
                    param_ty = Type::SumCtor(variants);
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
            } else if let Some(t) = ret_other {
                t
            } else {
                ctx.tv.fresh()
            };

            Ok((Type::fun(param_ty.apply(&s_all), ret_ty.apply(&s_all)), s_all))
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
    result
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

fn pp_type(t: &Type) -> String {
    fn rename_var(id: i64) -> String {
        // Map 0->%t0 stays as %t0 for stability; optional enhancement: %a,%b,... cycling.
        // Keep current behavior for now (already %tN). Placeholder for future customization.
        format!("%t{id}")
    }
    match t {
        Type::Unit => "Unit".into(),
        Type::Int => "Int".into(),
        Type::Float => "Float".into(),
        Type::Str => "Str".into(),
        Type::Char => "Char".into(),
        Type::Var(TvId(i)) => rename_var(*i as i64),
        Type::List(a) => format!("[{}]", pp_type(a)),
        Type::Tuple(xs) => format!("({})", xs.iter().map(pp_type).collect::<Vec<_>>().join(", ")),
        Type::Record(fs) => {
            let mut items: Vec<_> =
                fs.iter().map(|(k, v)| format!("{}: {}", k, pp_type(v))).collect();
            items.sort();
            format!("{{{}}}", items.join(", "))
        }
        Type::Fun(a, b) => format!("{} -> {}", pp_atom(a), pp_type(b)),
        Type::Ctor { tag, payload } => {
            if payload.is_empty() {
                tag.clone()
            } else {
                format!("{} {}", tag, payload.iter().map(pp_atom).collect::<Vec<_>>().join(" "))
            }
        }
        Type::Named { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                format!("{} {}", name, args.iter().map(pp_atom).collect::<Vec<_>>().join(" "))
            }
        }
        Type::SumCtor(vs) => {
            let mut vs2 = vs.clone();
            vs2.sort_by(|a, b| a.0.cmp(&b.0));
            let inner = vs2
                .into_iter()
                .map(|(tag, ps)| match ps.len() {
                    0 => tag,
                    1 => format!("{} {}", tag, pp_atom(&ps[0])),
                    _ => {
                        let parts: Vec<String> = ps.into_iter().map(|ty| pp_type(&ty)).collect();
                        format!("{}({})", tag, parts.join(", "))
                    }
                })
                .collect::<Vec<_>>()
                .join(" | ");
            format!("({})", inner)
        }
    }
}

fn pp_atom(t: &Type) -> String {
    match t {
        Type::Fun(_, _) => format!("({})", pp_type(t)),
        _ => pp_type(t),
    }
}

// ---------- User Pretty (zonk + var rename + cycle guard) ----------

fn user_pretty_type(t: &Type) -> String {
    use std::collections::HashMap;
    // 1) Collect free vars in first-occurrence order
    fn collect_order(t: &Type, order: &mut Vec<TvId>, seen: &mut std::collections::HashSet<TvId>) {
        match t {
            Type::Var(v) => {
                if seen.insert(*v) {
                    order.push(*v);
                }
            }
            Type::Fun(a, b) => {
                collect_order(a, order, seen);
                collect_order(b, order, seen);
            }
            Type::List(x) => collect_order(x, order, seen),
            Type::Tuple(xs) => {
                for x in xs {
                    collect_order(x, order, seen);
                }
            }
            Type::Record(fs) => {
                for v in fs.values() {
                    collect_order(v, order, seen);
                }
            }
            Type::Ctor { payload, .. } => {
                for x in payload {
                    collect_order(x, order, seen);
                }
            }
            Type::Named { args, .. } => {
                for x in args {
                    collect_order(x, order, seen);
                }
            }
            Type::SumCtor(vs) => {
                for (_, ps) in vs {
                    for x in ps {
                        collect_order(x, order, seen);
                    }
                }
            }
            Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char => {}
        }
    }
    let mut order = Vec::new();
    collect_order(t, &mut order, &mut std::collections::HashSet::new());
    // 2) Build rename map
    fn name_of(i: usize) -> String {
        if i < 26 {
            format!("%{}", (b'a' + i as u8) as char)
        } else {
            format!("%{}{}", (b'a' + (i % 26) as u8) as char, i / 26)
        }
    }
    let mut m: HashMap<TvId, String> = HashMap::new();
    for (idx, v) in order.iter().enumerate() {
        m.insert(*v, name_of(idx));
    }
    // 3) Cycle-aware pretty (should not normally see cycles because occurs-check, but guard anyway)
    use std::ptr::addr_of;
    fn go(
        t: &Type,
        m: &HashMap<TvId, String>,
        seen: &mut HashMap<usize, String>,
        _out_defs: &mut Vec<String>,
    ) -> String {
        let id_addr = addr_of!(*t) as usize;
        if let Some(label) = seen.get(&id_addr) {
            return format!("@{}", label);
        }
        match t {
            Type::Var(v) => m.get(v).cloned().unwrap_or_else(|| "_".to_string()),
            Type::Unit => "Unit".into(),
            Type::Int => "Int".into(),
            Type::Float => "Float".into(),
            Type::Str => "Str".into(),
            Type::Char => "Char".into(),
            Type::List(x) => format!("[{}]", go(x, m, seen, _out_defs)),
            Type::Tuple(xs) => {
                let inner =
                    xs.iter().map(|x| go(x, m, seen, _out_defs)).collect::<Vec<_>>().join(", ");
                format!("({})", inner)
            }
            Type::Record(fs) => {
                let mut items: Vec<_> = fs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, go(v, m, seen, _out_defs)))
                    .collect();
                items.sort();
                format!("{{{}}}", items.join(", "))
            }
            Type::Fun(a, b) => {
                let pa = match **a {
                    Type::Fun(_, _) => format!("({})", go(a, m, seen, _out_defs)),
                    _ => go(a, m, seen, _out_defs),
                };
                let pb = go(b, m, seen, _out_defs);
                format!("{} -> {}", pa, pb)
            }
            Type::Ctor { tag, payload } => {
                if payload.is_empty() {
                    tag.clone()
                } else {
                    format!(
                        "{} {}",
                        tag,
                        payload
                            .iter()
                            .map(|x| {
                                let s = go(x, m, seen, _out_defs);
                                if matches!(x, Type::Fun(_, _)) {
                                    format!("({})", s)
                                } else {
                                    s
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                }
            }
            Type::Named { name, args } => {
                if args.is_empty() {
                    name.clone()
                } else {
                    format!(
                        "{} {}",
                        name,
                        args.iter()
                            .map(|x| {
                                let s = go(x, m, seen, _out_defs);
                                if matches!(x, Type::Fun(_, _)) {
                                    format!("({})", s)
                                } else {
                                    s
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                }
            }
            Type::SumCtor(vs) => {
                let inner = vs
                    .iter()
                    .map(|(tag, ps)| {
                        if ps.is_empty() {
                            tag.clone()
                        } else {
                            format!(
                                "{} {}",
                                tag,
                                ps.iter()
                                    .map(|x| {
                                        let s = go(x, m, seen, _out_defs);
                                        if matches!(x, Type::Fun(_, _)) {
                                            format!("({})", s)
                                        } else {
                                            s
                                        }
                                    })
                                    .collect::<Vec<_>>()
                                    .join(" ")
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("({})", inner)
            }
        }
    }
    let mut seen = HashMap::new();
    let mut out_defs = Vec::new();
    let core = go(t, &m, &mut seen, &mut out_defs);
    // 期待仕様: 外部表示は %{ ... } ラッピング
    format!("%{{{}}}", core)
}

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
            tv: TvGen { next: 0 },
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
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
            tv: TvGen { next: 0 },
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
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
            tv: TvGen { next: 0 },
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
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
            tv: TvGen { next: 0 },
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
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
            Type::Record(m)
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
