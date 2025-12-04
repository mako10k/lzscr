//! Legacy type display format (kept for backward compatibility).
//!
//! This format uses raw type variable IDs (%t0, %t1, etc.) instead of
//! alphabetic names. Used by existing tests that expect this format.

use crate::types::{TvId, Type};

/// Legacy type pretty-printing with raw variable IDs (%tN format).
#[allow(dead_code)]
pub(crate) fn pp_type_legacy(t: &Type) -> String {
    fn rename_var(id: i64) -> String {
        format!("%t{id}")
    }
    fn dotted(tag: &str) -> String {
        if tag.starts_with('.') {
            tag.to_string()
        } else {
            let mut s = String::from(".");
            s.push_str(tag);
            s
        }
    }
    match t {
        Type::Unit => dotted("Unit"),
        Type::Int => dotted("Int"),
        Type::Float => dotted("Float"),
        Type::Str => dotted("Str"),
        Type::Char => dotted("Char"),
        Type::Type => dotted("Type"),
        Type::Var(TvId(i)) => rename_var(*i as i64),
        Type::List(a) => format!("[{}]", pp_type_legacy(a)),
        Type::Tuple(xs) => {
            format!("({})", xs.iter().map(pp_type_legacy).collect::<Vec<_>>().join(", "))
        }
        Type::Record(fs) => {
            let mut items: Vec<_> =
                fs.iter().map(|(k, (v, _))| format!("{}: {}", k, pp_type_legacy(v))).collect();
            items.sort();
            format!("{{{}}}", items.join(", "))
        }
        Type::Fun(a, b) => format!("{} -> {}", pp_atom_legacy(a), pp_type_legacy(b)),
        Type::Ctor { tag, payload } => {
            let head = dotted(tag);
            if payload.is_empty() {
                head
            } else {
                format!(
                    "{} {}",
                    head,
                    payload.iter().map(pp_atom_legacy).collect::<Vec<_>>().join(" ")
                )
            }
        }
        Type::Named { name, args } => {
            let head = dotted(name);
            if args.is_empty() {
                head
            } else {
                format!(
                    "{} {}",
                    head,
                    args.iter().map(pp_atom_legacy).collect::<Vec<_>>().join(" ")
                )
            }
        }
        Type::SumCtor(vs) => {
            let mut vs2 = vs.clone();
            vs2.sort_by(|a, b| a.0.cmp(&b.0));
            let inner = vs2
                .into_iter()
                .map(|(tag, ps)| match ps.len() {
                    0 => dotted(&tag),
                    1 => format!("{} {}", dotted(&tag), pp_atom_legacy(&ps[0])),
                    _ => {
                        let parts: Vec<String> =
                            ps.into_iter().map(|ty| pp_type_legacy(&ty)).collect();
                        format!("{}({})", dotted(&tag), parts.join(", "))
                    }
                })
                .collect::<Vec<_>>()
                .join(" | ");
            format!("({})", inner)
        }
    }
}

#[allow(dead_code)]
fn pp_atom_legacy(t: &Type) -> String {
    match t {
        Type::Fun(_, _) => format!("({})", pp_type_legacy(t)),
        _ => pp_type_legacy(t),
    }
}
