//! lzscr-types: HM type inference (MVP)
//!
//! Implements a minimal rank-1 HM inference over lzscr AST, including AltLambda
//! typing with Ctor-limited union (SumCtor) for lambda chains.

use lzscr_ast::ast::*;
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
    Bool,
    Str,
    Var(TvId),
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Record(BTreeMap<String, Type>),
    Ctor { tag: String, payload: Vec<Type> },
    // Limited union for constructor tags used by AltLambda chains
    SumCtor(Vec<(String, Vec<Type>)>),
}

impl Type {
    fn fun(a: Type, b: Type) -> Type { Type::Fun(Box::new(a), Box::new(b)) }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<TvId>,
    pub ty: Type,
}

#[derive(Default, Debug, Clone)]
pub struct Subst(HashMap<TvId, Type>);

impl Subst {
    fn new() -> Self { Self(HashMap::new()) }
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

trait TypesApply {
    fn apply(&self, s: &Subst) -> Self;
    fn ftv(&self) -> HashSet<TvId>;
}

impl TypesApply for Type {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Type::Var(v) => s.0.get(v).cloned().unwrap_or(Type::Var(*v)),
            Type::Fun(a, b) => Type::fun(a.apply(s), b.apply(s)),
            Type::List(t) => Type::List(Box::new(t.apply(s))),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| t.apply(s)).collect()),
            Type::Record(fs) => {
                let mut m = BTreeMap::new();
                for (k, v) in fs.iter() {
                    m.insert(k.clone(), v.apply(s));
                }
                Type::Record(m)
            }
            Type::Ctor { tag, payload } => Type::Ctor {
                tag: tag.clone(),
                payload: payload.iter().map(|t| t.apply(s)).collect(),
            },
            Type::SumCtor(vs) => Type::SumCtor(
                vs.iter()
                    .map(|(n, ps)| (n.clone(), ps.iter().map(|t| t.apply(s)).collect()))
                    .collect(),
            ),
            t @ (Type::Unit | Type::Int | Type::Float | Type::Bool | Type::Str) => t.clone(),
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
            Type::SumCtor(vs) => {
                for (_, ps) in vs {
                    for t in ps {
                        s.extend(t.ftv());
                    }
                }
            }
            Type::Unit | Type::Int | Type::Float | Type::Bool | Type::Str => {}
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
struct TvGen { next: u32 }
impl TvGen {
    fn fresh(&mut self) -> Type { let id = self.next; self.next += 1; Type::Var(TvId(id)) }
}

// ---------- Errors ----------

#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    #[error("type mismatch: expected {expected:?} vs actual {actual:?} at ({span_offset},{span_len})")]
    Mismatch { expected: Type, actual: Type, span_offset: usize, span_len: usize },
    #[error("occurs check failed: {var:?} in {ty:?}")]
    Occurs { var: TvId, ty: Type },
    #[error("constructor union duplicate tag: {tag}")]
    DuplicateCtorTag { tag: String },
    #[error("AltLambda branches mixed: expected all Ctor patterns or wildcard/default only")]
    MixedAltBranches,
    #[error("not a function: {ty:?}")]
    NotFunction { ty: Type },
    #[error("unbound reference: {name}")]
    UnboundRef { name: String },
}

// ---------- Unification ----------

fn occurs(v: TvId, t: &Type) -> bool { t.ftv().contains(&v) }

fn unify(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Var(x), t) => bind(*x, t.clone()),
        (t, Type::Var(x)) => bind(*x, t.clone()),
        (Type::Unit, Type::Unit)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::Str, Type::Str) => Ok(Subst::new()),
        (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
            let s1 = unify(a1, a2)?;
            let s2 = unify(&b1.apply(&s1), &b2.apply(&s1))?;
            Ok(s2.compose(s1))
        }
        (Type::List(x), Type::List(y)) => unify(x, y),
        (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
            let mut s = Subst::new();
            let mut it = xs.iter().zip(ys.iter());
            while let Some((x, y)) = it.next() {
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
            if a.len() != b.len() { return Err(TypeError::Mismatch { expected: Type::SumCtor(a.clone()), actual: Type::SumCtor(b.clone()), span_offset: 0, span_len: 0 }); }
            let mut map_a: BTreeMap<&str, &Vec<Type>> = BTreeMap::new();
            for (t, ps) in a { map_a.insert(t.as_str(), ps); }
            let mut s = Subst::new();
            for (t, psb) in b {
                let psa = map_a.get(t.as_str()).ok_or_else(|| TypeError::Mismatch { expected: Type::SumCtor(a.clone()), actual: Type::SumCtor(b.clone()), span_offset: 0, span_len: 0 })?;
                if psa.len() != psb.len() { return Err(TypeError::Mismatch { expected: Type::SumCtor(a.clone()), actual: Type::SumCtor(b.clone()), span_offset: 0, span_len: 0 }); }
                for (x, y) in psa.iter().zip(psb.iter()) {
                    let s1 = unify(&x.apply(&s), &y.apply(&s))?;
                    s = s1.compose(s);
                }
            }
            Ok(s)
        }
        _ => Err(TypeError::Mismatch { expected: a.clone(), actual: b.clone(), span_offset: 0, span_len: 0 }),
    }
}

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
    fn new() -> Self { Self(HashMap::new()) }
    fn insert(&mut self, n: String, s: Scheme) { self.0.insert(n, s); }
    fn get(&self, n: &str) -> Option<Scheme> { self.0.get(n).cloned() }
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

fn infer_pattern(tv: &mut TvGen, pat: &Pattern, scrutinee: &Type) -> Result<PatInfo, TypeError> {
    match &pat.kind {
        PatternKind::Wildcard => Ok(PatInfo::default()),
        PatternKind::Var(n) => Ok(PatInfo { bindings: vec![(n.clone(), scrutinee.clone())], subst: Subst::new() }),
        PatternKind::Unit => unify(scrutinee, &Type::Unit).map(|s| PatInfo { bindings: vec![], subst: s }),
        PatternKind::Int(_) => unify(scrutinee, &Type::Int).map(|s| PatInfo { bindings: vec![], subst: s }),
        PatternKind::Float(_) => unify(scrutinee, &Type::Float).map(|s| PatInfo { bindings: vec![], subst: s }),
        PatternKind::Str(_) => unify(scrutinee, &Type::Str).map(|s| PatInfo { bindings: vec![], subst: s }),
        PatternKind::Bool(_) => unify(scrutinee, &Type::Bool).map(|s| PatInfo { bindings: vec![], subst: s }),
        PatternKind::Tuple(xs) => {
            let mut s = Subst::new();
            let mut tys = Vec::with_capacity(xs.len());
            for _ in xs { tys.push(tv.fresh()); }
            let tup = Type::Tuple(tys.clone());
            let s0 = unify(&scrutinee.apply(&s), &tup)?; s = s0.compose(s);
            let mut binds = vec![];
            for (p, t_elem) in xs.iter().zip(tys.into_iter()) {
                let pi = infer_pattern(tv, p, &t_elem.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::List(items) => {
            let a = tv.fresh();
            let mut s = unify(scrutinee, &Type::List(Box::new(a.clone())))?;
            let mut binds = vec![];
            for p in items {
                let pi = infer_pattern(tv, p, &a.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Cons(h, t) => {
            let a = tv.fresh();
            let s0 = unify(scrutinee, &Type::List(Box::new(a.clone())))?;
            let mut s = s0;
            let ph = infer_pattern(tv, h, &a.apply(&s))?; s = ph.subst.compose(s);
            let pt = infer_pattern(tv, t, &Type::List(Box::new(a.apply(&s))))?; s = pt.subst.compose(s);
            let mut binds = ph.bindings; binds.extend(pt.bindings);
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Record(fields) => {
            let mut want = BTreeMap::new();
            for (k, _) in fields { want.insert(k.clone(), tv.fresh()); }
            let s0 = unify(scrutinee, &Type::Record(want.clone()))?;
            let mut s = s0;
            let mut binds = vec![];
            for (k, p) in fields {
                let tfield = want.get(k).unwrap().apply(&s);
                let pi = infer_pattern(tv, p, &tfield)?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Ctor { name, args } => {
            let payload: Vec<Type> = (0..args.len()).map(|_| tv.fresh()).collect();
            let want = Type::Ctor { tag: name.clone(), payload: payload.clone() };
            let mut s = unify(scrutinee, &want)?;
            let mut binds = vec![];
            for (p, ta) in args.iter().zip(payload.into_iter()) {
                let pi = infer_pattern(tv, p, &ta.apply(&s))?;
                s = pi.subst.compose(s);
                binds.extend(pi.bindings);
            }
            Ok(PatInfo { bindings: binds, subst: s })
        }
        PatternKind::Symbol(sym) => {
            // treat as ctor with zero args in patterns
            let want = Type::Ctor { tag: sym.clone(), payload: vec![] };
            let s = unify(scrutinee, &want)?;
            Ok(PatInfo { bindings: vec![], subst: s })
        }
        PatternKind::As(a, b) => {
            let pa = infer_pattern(tv, a, scrutinee)?;
            let pb = infer_pattern(tv, b, &scrutinee.apply(&pa.subst))?;
            let mut binds = pa.bindings; binds.extend(pb.bindings);
            Ok(PatInfo { bindings: binds, subst: pb.subst.compose(pa.subst) })
        }
    }
}

// ---------- Inference ----------

struct InferCtx {
    tv: TvGen,
    env: TypeEnv,
}

fn infer_expr(ctx: &mut InferCtx, e: &Expr) -> Result<(Type, Subst), TypeError> {
    match &e.kind {
        ExprKind::Unit => Ok((Type::Unit, Subst::new())),
        ExprKind::Int(_) => Ok((Type::Int, Subst::new())),
        ExprKind::Float(_) => Ok((Type::Float, Subst::new())),
        ExprKind::Str(_) => Ok((Type::Str, Subst::new())),
        ExprKind::Ref(n) => {
            let s = ctx.env.get(n).ok_or_else(|| TypeError::UnboundRef { name: n.clone() })?;
            Ok((instantiate(&mut ctx.tv, &s), Subst::new()))
        }
        ExprKind::Symbol(name) => {
            // Treat bare symbol as constructor variable; type is polymorphic function from payload to Ctor
            // arity unknown at type level; model as a type variable function: a1 -> a2 -> ... -> Ctor
            // MVP: assume arity 0 to allow usage as value; payload will be built via application elsewhere.
            Ok((Type::Ctor { tag: name.clone(), payload: vec![] }, Subst::new()))
        }
        ExprKind::Lambda { param, body } => {
            let a = ctx.tv.fresh();
            let pi = infer_pattern(&mut ctx.tv, param, &a)?;
            let mut env2 = ctx.env.clone();
            for (n, t) in &pi.bindings { env2.insert(n.clone(), Scheme { vars: vec![], ty: t.clone() }); }
            let prev = std::mem::replace(&mut ctx.env, env2);
            let (bt, s_body) = infer_expr(ctx, body)?;
            ctx.env = prev;
            let ty = Type::fun(a.apply(&s_body).apply(&pi.subst), bt.apply(&s_body));
            Ok((ty, s_body.compose(pi.subst)))
        }
        ExprKind::Apply { func, arg } => {
            let (tf, sf) = infer_expr(ctx, func)?;
            let (ta, sa) = infer_expr(ctx, arg)?;
            let r = ctx.tv.fresh();
            let s1 = unify(&tf.apply(&sa).apply(&sf), &Type::fun(ta.apply(&sa), r.clone()))?;
            let s = s1.compose(sa).compose(sf);
            Ok((r.apply(&s), s))
        }
        ExprKind::Block(inner) => infer_expr(ctx, inner),
        ExprKind::List(items) => {
            let a = ctx.tv.fresh();
            let mut s = Subst::new();
            for it in items {
                let (ti, si) = infer_expr(ctx, it)?;
                let s1 = unify(&ti.apply(&s), &a.apply(&s))?;
                s = s1.compose(si).compose(s);
            }
            Ok((Type::List(Box::new(a.apply(&s))), s))
        }
    ExprKind::LetGroup { bindings, body } => {
            // Simple non-recursive sequencing for MVP: evaluate bindings in order in extended env
            // Note: Full LetRec requires placeholder vars and mutual recursion handling.
            let env_prev = ctx.env.clone();
            let mut env = ctx.env.clone();
            for (p, ex) in bindings {
        // infer RHS under current extended env
        let saved = std::mem::replace(&mut ctx.env, env.clone());
        let (t_rhs, s_rhs) = infer_expr(ctx, ex)?;
        ctx.env = saved;
                let a = ctx.tv.fresh();
                // p : a  and unify a with t_rhs
                let pi = infer_pattern(&mut ctx.tv, p, &a.apply(&s_rhs))?;
                let s = unify(&a.apply(&pi.subst), &t_rhs.apply(&pi.subst))?;
                let subst_all = s.compose(pi.subst).compose(s_rhs);
                // bind variables
                for (n, t) in pi.bindings {
                    let sc = generalize(&env, &t.apply(&subst_all));
                    env.insert(n, sc);
                }
            }
            // body under env
            let _ = std::mem::replace(&mut ctx.env, env);
            let res = infer_expr(ctx, body);
            ctx.env = env_prev;
            res
        }
        ExprKind::Raise(inner) => {
            // model as bottom: type of inner unconstrained, result fresh var
            let (_t, s) = infer_expr(ctx, inner)?;
            let r = ctx.tv.fresh();
            Ok((r, s))
        }
        ExprKind::OrElse { left, right } => {
            let (tl, sl) = infer_expr(ctx, left)?;
            let (tr, sr) = infer_expr(ctx, right)?;
            let s1 = unify(&tl.apply(&sr).apply(&sl), &tr.apply(&sr))?;
            let s = s1.compose(sr).compose(sl);
            Ok((tr.apply(&s), s))
        }
        ExprKind::Catch { left, right } => {
            let (tl, sl) = infer_expr(ctx, left)?;
            // right: gamma -> r  (approximate) — MVP: treat as function from any to result type
            let a = ctx.tv.fresh();
            let r = ctx.tv.fresh();
            let (tr, sr) = infer_expr(ctx, right)?;
            let s1 = unify(&tr.apply(&sr).apply(&sl), &Type::fun(a, r.clone()))?;
            let s2 = unify(&tl.apply(&s1).apply(&sr).apply(&sl), &r)?;
            let s = s2.compose(s1).compose(sr).compose(sl);
            Ok((r.apply(&s), s))
        }
        ExprKind::AltLambda { left, right } => {
            // Special handling for Ctor-union when lambda patterns are constructor-only.
            // Pattern-check fast path: both sides must be Lambda.
            let (pl, bl) = match &left.kind {
                ExprKind::Lambda { param, body } => (param, body),
                _ => return Err(TypeError::MixedAltBranches),
            };
            let (pr, br) = match &right.kind {
                ExprKind::Lambda { param, body } => (param, body),
                _ => return Err(TypeError::MixedAltBranches),
            };

            // detect ctor-only or wildcard/default
            fn is_ctor_like(p: &PatternKind) -> bool {
                matches!(p, PatternKind::Ctor { .. } | PatternKind::Symbol(_))
            }
            let left_ctor = is_ctor_like(&pl.kind) || matches!(pl.kind, PatternKind::Wildcard);
            let right_ctor = is_ctor_like(&pr.kind) || matches!(pr.kind, PatternKind::Wildcard);
            if !(left_ctor && right_ctor) {
                return Err(TypeError::MixedAltBranches);
            }

            // Build union of ctor tags; wildcard does not add variants.
            let mut variants: Vec<(String, Vec<Type>)> = vec![];
            let mut seen = HashSet::<String>::new();
            let mut param_ty = ctx.tv.fresh(); // will be replaced by SumCtor if variants present

            let mut s_all = Subst::new();

            let mut branch_ret = ctx.tv.fresh();

            // process left branch
            match &pl.kind {
                PatternKind::Wildcard => {
                    let pi = infer_pattern(&mut ctx.tv, pl, &param_ty.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, bl)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    // consume s_all to silence assignment warning and propagate
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                PatternKind::Ctor { name, args } => {
                    let payload: Vec<Type> = (0..args.len()).map(|_| ctx.tv.fresh()).collect();
                    if !seen.insert(name.clone()) {
                        return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                    }
                    variants.push((name.clone(), payload.clone()));
                    let scr = Type::Ctor { tag: name.clone(), payload: payload.clone() };
                    let pi = infer_pattern(&mut ctx.tv, pl, &scr.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, bl)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                PatternKind::Symbol(name) => {
                    if !seen.insert(name.clone()) {
                        return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                    }
                    variants.push((name.clone(), vec![]));
                    let scr = Type::Ctor { tag: name.clone(), payload: vec![] };
                    let pi = infer_pattern(&mut ctx.tv, pl, &scr.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, bl)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                _ => return Err(TypeError::MixedAltBranches),
            }

            // process right branch
            match &pr.kind {
                PatternKind::Wildcard => {
                    let pi = infer_pattern(&mut ctx.tv, pr, &param_ty.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, br)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                PatternKind::Ctor { name, args } => {
                    let payload: Vec<Type> = (0..args.len()).map(|_| ctx.tv.fresh()).collect();
                    if !seen.insert(name.clone()) {
                        return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                    }
                    variants.push((name.clone(), payload.clone()));
                    let scr = Type::Ctor { tag: name.clone(), payload: payload.clone() };
                    let pi = infer_pattern(&mut ctx.tv, pr, &scr.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, br)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                PatternKind::Symbol(name) => {
                    if !seen.insert(name.clone()) {
                        return Err(TypeError::DuplicateCtorTag { tag: name.clone() });
                    }
                    variants.push((name.clone(), vec![]));
                    let scr = Type::Ctor { tag: name.clone(), payload: vec![] };
                    let pi = infer_pattern(&mut ctx.tv, pr, &scr.apply(&s_all))?;
                    let (tb, sb) = infer_expr(ctx, br)?;
                    let s1 = unify(&tb.apply(&sb).apply(&pi.subst), &branch_ret.apply(&sb).apply(&pi.subst))?;
                    s_all = s1.compose(sb).compose(pi.subst).compose(s_all);
                    param_ty = param_ty.apply(&s_all);
                    branch_ret = branch_ret.apply(&s_all);
                }
                _ => return Err(TypeError::MixedAltBranches),
            }

            // If we collected any ctor variants, parameter type is SumCtor of them; otherwise keep fresh param
            if !variants.is_empty() {
                param_ty = Type::SumCtor(variants);
            }
            Ok((Type::fun(param_ty.apply(&s_all), branch_ret.apply(&s_all)), s_all))
        }
    }
}

// ---------- Pretty Printing ----------

fn pp_type(t: &Type) -> String {
    match t {
        Type::Unit => "Unit".into(),
        Type::Int => "Int".into(),
        Type::Float => "Float".into(),
        Type::Bool => "Bool".into(),
        Type::Str => "Str".into(),
        Type::Var(TvId(i)) => format!("t{i}"),
        Type::List(a) => format!("List {}", pp_atom(a)),
        Type::Tuple(xs) => format!("({})", xs.iter().map(pp_type).collect::<Vec<_>>().join(", ")),
        Type::Record(fs) => {
            let mut items: Vec<_> = fs.iter().map(|(k, v)| format!("{}: {}", k, pp_type(v))).collect();
            items.sort();
            format!("{{{}}}", items.join(", "))
        }
        Type::Fun(a, b) => format!("{} -> {}", pp_atom(a), pp_type(b)),
        Type::Ctor { tag, payload } => {
            if payload.is_empty() { tag.clone() } else { format!("{}({})", tag, payload.iter().map(pp_type).collect::<Vec<_>>().join(", ")) }
        }
        Type::SumCtor(vs) => {
            let inner = vs
                .iter()
                .map(|(t, ps)| if ps.is_empty() { t.clone() } else { format!("{}({})", t, ps.iter().map(pp_type).collect::<Vec<_>>().join(", ")) })
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

// ---------- Public API ----------

pub mod api {
    use super::*;
    use lzscr_parser::parse_expr;

    pub fn infer_program(src: &str) -> Result<String, String> {
        let ast = parse_expr(src).map_err(|e| format!("parse error: {e}"))?;
        let mut ctx = InferCtx { tv: TvGen { next: 0 }, env: prelude_env() };
        match infer_expr(&mut ctx, &ast) {
            Ok((t, _s)) => Ok(pp_type(&t)),
            Err(e) => Err(format!("{e}")),
        }
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
        env.insert(
            "alt".into(),
            Scheme { vars: vec![a, r], ty: alt_ty },
        );
    // add : Int -> Int -> Int
    let add_ty = Type::fun(Type::Int, Type::fun(Type::Int, Type::Int));
    env.insert("add".into(), Scheme { vars: vec![], ty: add_ty });
    // seq : forall a b. a -> b -> b
    let a2 = TvId(1002);
    let b2 = TvId(1003);
    let seq_ty = Type::fun(Type::Var(a2), Type::fun(Type::Var(b2), Type::Var(b2)));
    env.insert("seq".into(), Scheme { vars: vec![a2, b2], ty: seq_ty });
    // effects : forall s a. s -> a -> Unit  (approximate; first arg is an effect symbol)
    let s3 = TvId(1004);
    let a3 = TvId(1005);
    let eff_ty = Type::fun(Type::Var(s3), Type::fun(Type::Var(a3), Type::Unit));
    env.insert("effects".into(), Scheme { vars: vec![s3, a3], ty: eff_ty });
        env
    }
}

