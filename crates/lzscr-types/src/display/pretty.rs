//! Pretty printing for types with variable renaming.
//!
//! This module provides the modern type display format with:
//! - Alphabetic variable naming (%a, %b, etc.)
//! - Proper precedence handling for function types
//! - Sorted output for records and sum constructors

use std::collections::HashMap;

use crate::types::{TvId, Type};

/// Pretty-print a type with alphabetic variable renaming.
pub(crate) fn pp_type(t: &Type) -> String {
    pp_type_with_renaming(t, &mut HashMap::new(), &mut 0)
}

fn pp_type_with_renaming(
    t: &Type,
    rename_map: &mut HashMap<TvId, String>,
    counter: &mut usize,
) -> String {
    fn get_or_create_name(
        tv: TvId,
        rename_map: &mut HashMap<TvId, String>,
        counter: &mut usize,
    ) -> String {
        if let Some(name) = rename_map.get(&tv) {
            return name.clone();
        }
        let name = if *counter < 26 {
            format!("%{}", (b'a' + *counter as u8) as char)
        } else {
            format!("%{}{}", (b'a' + (*counter % 26) as u8) as char, *counter / 26)
        };
        *counter += 1;
        rename_map.insert(tv, name.clone());
        name
    }

    match t {
        Type::Unit => "Unit".into(),
        Type::Int => "Int".into(),
        Type::Float => "Float".into(),
        Type::Str => "Str".into(),
        Type::Char => "Char".into(),
        Type::Type => "Type".into(),
        Type::Var(tv) => get_or_create_name(*tv, rename_map, counter),
        Type::List(a) => format!("[{}]", pp_type_with_renaming(a, rename_map, counter)),
        Type::Tuple(xs) => format!(
            "({})",
            xs.iter()
                .map(|x| pp_type_with_renaming(x, rename_map, counter))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Record(fs) => {
            let mut items: Vec<_> = fs
                .iter()
                .map(|(k, (v, _))| {
                    format!("{}: {}", k, pp_type_with_renaming(v, rename_map, counter))
                })
                .collect();
            items.sort();
            format!("{{{}}}", items.join(", "))
        }
        Type::Fun(a, b) => format!(
            "{} -> {}",
            pp_atom_with_renaming(a, rename_map, counter),
            pp_type_with_renaming(b, rename_map, counter)
        ),
        Type::Ctor { tag, payload } => {
            let head = tag.clone();
            if payload.is_empty() {
                head
            } else {
                format!(
                    "{} {}",
                    head,
                    payload
                        .iter()
                        .map(|x| pp_atom_with_renaming(x, rename_map, counter))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
        }
        Type::Named { name, args } => {
            let head = name.clone();
            if args.is_empty() {
                head
            } else {
                format!(
                    "{} {}",
                    head,
                    args.iter()
                        .map(|x| pp_atom_with_renaming(x, rename_map, counter))
                        .collect::<Vec<_>>()
                        .join(" ")
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
                    1 => format!(
                        "{} {}",
                        tag,
                        pp_atom_with_renaming(&ps[0], rename_map, counter)
                    ),
                    _ => {
                        let parts: Vec<String> = ps
                            .into_iter()
                            .map(|ty| pp_type_with_renaming(&ty, rename_map, counter))
                            .collect();
                        format!("{}({})", tag, parts.join(", "))
                    }
                })
                .collect::<Vec<_>>()
                .join(" | ");
            format!("({})", inner)
        }
    }
}

fn pp_atom_with_renaming(
    t: &Type,
    rename_map: &mut HashMap<TvId, String>,
    counter: &mut usize,
) -> String {
    match t {
        Type::Fun(_, _) => format!("({})", pp_type_with_renaming(t, rename_map, counter)),
        _ => pp_type_with_renaming(t, rename_map, counter),
    }
}
