//! User-friendly type display with cycle detection and variable normalization.
//!
//! This module provides the most polished type display format, wrapping
//! normalized types in `%{ ... }` syntax with consistent variable naming.

use std::collections::{HashMap, HashSet};
use std::ptr::addr_of;

use crate::types::{TvId, Type};


/// User-friendly type display with %{ ... } wrapping and normalized variable names.
pub(crate) fn user_pretty_type(t: &Type) -> String {
    normalize_type_and_map(t).0
}

/// Normalize a type to user_pretty_type form AND return the mapping from TvId -> pretty name used.
///
/// This lets callers pick out the pretty name for a specific variable (e.g. occurs variable) while
/// using the same consistent normalization for the full type shown in the error.
pub(crate) fn user_pretty_type_and_map(t: &Type) -> (String, HashMap<TvId, String>) {
    normalize_type_and_map(t)
}

/// Internal normalization returning both pretty string and TvId->name mapping.
fn normalize_type_and_map(t: &Type) -> (String, HashMap<TvId, String>) {
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

    fn go(t: &Type, m: &HashMap<TvId, String>, seen: &mut HashMap<usize, String>) -> String {
        let id_addr = addr_of!(*t) as usize;
        if let Some(label) = seen.get(&id_addr) {
            return format!("@{label}");
        }
        match t {
            Type::Var(v) => m.get(v).cloned().unwrap_or_else(|| "_".into()),
            Type::Unit => ".Unit".into(),
            Type::Int => ".Int".into(),
            Type::Float => ".Float".into(),
            Type::Str => ".Str".into(),
            Type::Char => ".Char".into(),
            Type::Type => ".Type".into(),
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
                let head = tag.clone();
                if payload.is_empty() {
                    head
                } else {
                    let args = payload
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
                        .join(" ");
                    format!("{} {}", head, args)
                }
            }
            Type::Named { name, args } => {
                let head = name.clone();
                if args.is_empty() {
                    head
                } else {
                    let args = args
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
                        .join(" ");
                    format!("{} {}", head, args)
                }
            }
            Type::SumCtor(vs) => {
                let mut vs2 = vs.clone();
                vs2.sort_by(|a, b| a.0.cmp(&b.0));
                let inner = vs2
                    .into_iter()
                    .map(|(tag, ps)| match ps.len() {
                        0 => tag,
                        1 => {
                            let arg = go(&ps[0], m, seen);
                            let arg = if matches!(ps[0], Type::Fun(_, _)) {
                                format!("({})", arg)
                            } else {
                                arg
                            };
                            format!("{} {}", tag, arg)
                        }
                        _ => {
                            let parts: Vec<String> =
                                ps.into_iter().map(|ty| go(&ty, m, seen)).collect();
                            format!("{}({})", tag, parts.join(", "))
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
