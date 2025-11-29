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

use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

// Core type representation (was accidentally removed during patching)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Copy)]
pub struct TvId(pub u32);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Unit,
    Int,
    Float,
    Str,
    Char,
    Type, // first-class type value
    Var(TvId),
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Record(BTreeMap<String, (Type, Option<Span>)>),
    Ctor { tag: String, payload: Vec<Type> },
    Named { name: String, args: Vec<Type> }, // syntactic sugar for unfolding during unify
    SumCtor(Vec<(String, Vec<Type>)>),       // union of constructors
}

impl Type {
    fn fun(a: Type, b: Type) -> Type {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

impl std::fmt::Display for TvId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pp_type(self))
    }
}
fn bool_sum_type() -> Type {
    Type::SumCtor(vec![(".False".into(), vec![]), (".True".into(), vec![])])
}

fn result_str_str_type() -> Type {
    Type::SumCtor(vec![(".Ok".into(), vec![Type::Str]), (".Err".into(), vec![Type::Str])])
}

fn result_unit_str_type() -> Type {
    Type::SumCtor(vec![(".Ok".into(), vec![Type::Unit]), (".Err".into(), vec![Type::Str])])
}

fn result_list_str_type() -> Type {
    Type::SumCtor(vec![
        (".Ok".into(), vec![Type::List(Box::new(Type::Str))]),
        (".Err".into(), vec![Type::Str]),
    ])
}

fn fs_metadata_record_type() -> Type {
    let mut fields = BTreeMap::new();
    fields.insert("size".into(), (Type::Int, None));
    fields.insert("is_dir".into(), (bool_sum_type(), None));
    Type::Record(fields)
}

fn result_metadata_type() -> Type {
    Type::SumCtor(vec![
        (".Ok".into(), vec![fs_metadata_record_type()]),
        (".Err".into(), vec![Type::Str]),
    ])
}

fn fs_effects_record_type() -> Type {
    let mut fields = BTreeMap::new();
    fields.insert("read_text".into(), (Type::fun(Type::Str, result_str_str_type()), None));
    fields.insert(
        "write_text".into(),
        (Type::fun(Type::Str, Type::fun(Type::Str, result_unit_str_type())), None),
    );
    fields.insert(
        "append_text".into(),
        (Type::fun(Type::Str, Type::fun(Type::Str, result_unit_str_type())), None),
    );
    fields.insert("list_dir".into(), (Type::fun(Type::Str, result_list_str_type()), None));
    fields.insert("remove_file".into(), (Type::fun(Type::Str, result_unit_str_type()), None));
    fields.insert("create_dir".into(), (Type::fun(Type::Str, result_unit_str_type()), None));
    fields.insert("metadata".into(), (Type::fun(Type::Str, result_metadata_type()), None));
    Type::Record(fields)
}

fn effect_signature(sym: &str) -> Option<Type> {
    match sym {
        ".fs" => Some(fs_effects_record_type()),
        _ => None,
    }
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
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (zonk_type(v, s), *sp));
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
            for (k, (v, sp)) in fs {
                m.insert(k.clone(), (normalize_tuples(v), *sp));
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
                for (k, (v, sp)) in fs {
                    m.insert(k.clone(), (v.apply(s), *sp));
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
    #[error("type mismatch: expected {expected} vs actual {actual} at ({span_offset},{span_len})")]
    Mismatch { expected: Type, actual: Type, span_offset: usize, span_len: usize },
    #[error("record field type mismatch: field '{field}' expected {expected} vs actual {actual} at ({span_offset},{span_len})")]
    RecordFieldMismatch {
        field: String,
        expected: Type,
        actual: Type,
        span_offset: usize,
        span_len: usize,
    },
    #[error("record field '{field}' type mismatch: expected %{expected} vs actual %{actual}")]
    RecordFieldMismatchBoth {
        field: String,
        expected: Type,
        actual: Type,
        expected_span_offset: usize,
        expected_span_len: usize,
        actual_span_offset: usize,
        actual_span_len: usize,
    },
    #[error("type mismatch: expected {expected} vs actual {actual}")]
    MismatchBoth {
        expected: Type,
        actual: Type,
        expected_span_offset: usize,
        expected_span_len: usize,
        actual_span_offset: usize,
        actual_span_len: usize,
    },
    #[error("type mismatch: expected {expected} vs actual {actual}")]
    AnnotMismatch {
        expected: Type,
        actual: Type,
        annot_span_offset: usize,
        annot_span_len: usize,
        expr_span_offset: usize,
        expr_span_len: usize,
    },
    #[error("cannot construct infinite type: {var_pretty} occurs in {pretty}")]
    Occurs {
        var: TvId,
        var_pretty: String, // normalized pretty name for the variable (e.g. %a)
        ty: Type,
        pretty: String, // normalized pretty name for the type (%{ ... })
        var_span_offset: usize,
        var_span_len: usize,
        ty_span_offset: usize,
        ty_span_len: usize,
    },
    #[error("constructor union duplicate tag: {tag} at ({span_offset},{span_len})")]
    DuplicateCtorTag { tag: String, span_offset: usize, span_len: usize },
    #[error("AltLambda branches mixed: expected all Ctor patterns or wildcard/default only at ({span_offset},{span_len})")]
    MixedAltBranches { span_offset: usize, span_len: usize },
    #[error("AltLambda arity mismatch: expected {expected} args but got {got} at ({span_offset},{span_len})")]
    AltLambdaArityMismatch { expected: usize, got: usize, span_offset: usize, span_len: usize },
    #[error("not a function: {ty}")]
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

#[inline]
fn format_field_path(parent: &str, child: &str) -> String {
    if child.is_empty() {
        parent.to_string()
    } else {
        format!("{parent}.{child}")
    }
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

// Small helper: unify two equally-sized slices element-wise, composing substitutions left-to-right.
// (Used for Tuple, Ctor payloads, and SumCtor payload lists.)
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
            for v in &views {
                if v.params.len() != arity {
                    return Err(TypeError::AltLambdaArityMismatch {
                        expected: arity,
                        got: v.params.len(),
                        span_offset: v.span.offset,
                        span_len: v.span.len,
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
        Type::Type => "Type".into(),
        Type::Var(TvId(i)) => rename_var(*i as i64),
        Type::List(a) => format!("[{}]", pp_type(a)),
        Type::Tuple(xs) => format!("({})", xs.iter().map(pp_type).collect::<Vec<_>>().join(", ")),
        Type::Record(fs) => {
            let mut items: Vec<_> =
                fs.iter().map(|(k, (v, _))| format!("{}: {}", k, pp_type(v))).collect();
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
                    collect_order(&v.0, order, seen);
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
            Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char | Type::Type => {}
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
            Type::Type => "Type".into(),
            Type::List(x) => format!("[{}]", go(x, m, seen, _out_defs)),
            Type::Tuple(xs) => {
                let inner =
                    xs.iter().map(|x| go(x, m, seen, _out_defs)).collect::<Vec<_>>().join(", ");
                format!("({})", inner)
            }
            Type::Record(fs) => {
                let mut items: Vec<_> = fs
                    .iter()
                    .map(|(k, (v, _))| format!("{}: {}", k, go(v, m, seen, _out_defs)))
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

// Normalize a type to user_pretty_type form AND return the mapping from TvId -> pretty name used.
// This lets callers pick out the pretty name for a specific variable (e.g. occurs variable) while
// using the same consistent normalization for the full type shown in the error.
// Internal normalization returning both pretty string and TvId->name mapping.
// (Legacy name normalize_type_and_map kept; new unified entry also exposed for future refactors.)
fn normalize_type_and_map(t: &Type) -> (String, HashMap<TvId, String>) {
    use std::collections::{HashMap, HashSet};
    fn collect_order(t: &Type, order: &mut Vec<TvId>, seen: &mut HashSet<TvId>) {
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
                    collect_order(&v.0, order, seen);
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
            Type::Unit | Type::Int | Type::Float | Type::Str | Type::Char | Type::Type => {}
        }
    }
    fn name_of(i: usize) -> String {
        if i < 26 {
            format!("%{}", (b'a' + i as u8) as char)
        } else {
            format!("%{}{}", (b'a' + (i % 26) as u8) as char, i / 26)
        }
    }
    let mut order = Vec::new();
    collect_order(t, &mut order, &mut HashSet::new());
    let mut mapping: HashMap<TvId, String> = HashMap::new();
    for (idx, v) in order.iter().enumerate() {
        mapping.insert(*v, name_of(idx));
    }
    use std::ptr::addr_of;
    fn go(t: &Type, m: &HashMap<TvId, String>, seen: &mut HashMap<usize, String>) -> String {
        let id_addr = addr_of!(*t) as usize;
        if let Some(l) = seen.get(&id_addr) {
            return format!("@{l}");
        }
        match t {
            Type::Var(v) => m.get(v).cloned().unwrap_or_else(|| "_".into()),
            Type::Unit => "Unit".into(),
            Type::Int => "Int".into(),
            Type::Float => "Float".into(),
            Type::Str => "Str".into(),
            Type::Char => "Char".into(),
            Type::Type => "Type".into(),
            Type::List(x) => format!("[{}]", go(x, m, seen)),
            Type::Tuple(xs) => {
                let inner = xs.iter().map(|x| go(x, m, seen)).collect::<Vec<_>>().join(", ");
                format!("({})", inner)
            }
            Type::Record(fs) => {
                let mut items: Vec<_> =
                    fs.iter().map(|(k, (v, _))| format!("{}: {}", k, go(v, m, seen))).collect();
                items.sort();
                format!("{{{}}}", items.join(", "))
            }
            Type::Fun(a, b) => {
                let pa = match **a {
                    Type::Fun(_, _) => format!("({})", go(a, m, seen)),
                    _ => go(a, m, seen),
                };
                let pb = go(b, m, seen);
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
                                let s = go(x, m, seen);
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
                                let s = go(x, m, seen);
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
                                        let s = go(x, m, seen);
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
    let core = go(t, &mapping, &mut seen);
    (format!("%{{{}}}", core), mapping)
}

fn user_pretty_type_and_map(t: &Type) -> (String, HashMap<TvId, String>) {
    normalize_type_and_map(t)
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
            tv: TvGen { next: 0 },
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
            tv: TvGen { next: 0 },
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
            tv: TvGen { next: 0 },
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
