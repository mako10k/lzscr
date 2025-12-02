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
    match t {
        Type::Unit => "Unit".into(),
        Type::Int => "Int".into(),
        Type::Float => "Float".into(),
        Type::Str => "Str".into(),
        Type::Char => "Char".into(),
        Type::Type => "Type".into(),
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
            if payload.is_empty() {
                tag.clone()
            } else {
                format!(
                    "{} {}",
                    tag,
                    payload.iter().map(pp_atom_legacy).collect::<Vec<_>>().join(" ")
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
                    0 => tag,
                    1 => format!("{} {}", tag, pp_atom_legacy(&ps[0])),
                    _ => {
                        let parts: Vec<String> =
                            ps.into_iter().map(|ty| pp_type_legacy(&ty)).collect();
                        format!("{}({})", tag, parts.join(", "))
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
