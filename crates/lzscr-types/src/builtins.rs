//! Built-in type constructors for LazyScript standard types.
//!
//! This module provides type constructors for:
//! - Boolean union type (`.False` | `.True`)
//! - Option type (`.Some` | `.None`)
//! - Result variants (`.Ok` | `.Err`)
//! - Filesystem effects record type (`.fs`)
//! - Effect signature lookup

use crate::Type;
use std::collections::BTreeMap;

/// Construct the boolean sum type: `.False | .True`.
///
/// Used for standard library boolean operations.
pub fn bool_sum_type() -> Type {
    Type::SumCtor(vec![(".False".into(), vec![]), (".True".into(), vec![])])
}

/// Construct the option type: `.Some(T) | .None`.
///
/// Generic over the inner type parameter.
///
/// # Examples
///
/// ```ignore
/// let opt_int = option_type(Type::Int);
/// // => .Some(Int) | .None
/// ```
pub fn option_type(inner: Type) -> Type {
    Type::SumCtor(vec![(".Some".into(), vec![inner]), (".None".into(), vec![])])
}

/// Construct a Result type variant: `.Ok(Str) | .Err(Str)`.
///
/// Used for string-based error handling.
pub fn result_str_str_type() -> Type {
    Type::SumCtor(vec![(".Ok".into(), vec![Type::Str]), (".Err".into(), vec![Type::Str])])
}

/// Construct a Result type variant: `.Ok(Unit) | .Err(Str)`.
///
/// Used for side-effect operations that return unit on success.
pub fn result_unit_str_type() -> Type {
    Type::SumCtor(vec![(".Ok".into(), vec![Type::Unit]), (".Err".into(), vec![Type::Str])])
}

/// Construct a Result type variant: `.Ok(List<Str>) | .Err(Str)`.
///
/// Used for operations returning lists of strings (e.g., directory listings).
pub fn result_list_str_type() -> Type {
    Type::SumCtor(vec![
        (".Ok".into(), vec![Type::List(Box::new(Type::Str))]),
        (".Err".into(), vec![Type::Str]),
    ])
}

/// Construct the filesystem metadata record type.
///
/// Fields:
/// - `is_dir`: Bool
/// - `is_file`: Bool
/// - `modified_ms`: Option<Int>
/// - `readonly`: Bool
/// - `size`: Int
pub fn fs_metadata_record_type() -> Type {
    let mut fields = BTreeMap::new();
    fields.insert("is_dir".into(), (bool_sum_type(), None));
    fields.insert("is_file".into(), (bool_sum_type(), None));
    fields.insert("modified_ms".into(), (option_type(Type::Int), None));
    fields.insert("readonly".into(), (bool_sum_type(), None));
    fields.insert("size".into(), (Type::Int, None));
    Type::Record(fields)
}

/// Construct a Result type variant: `.Ok(Metadata) | .Err(Str)`.
///
/// Used for filesystem metadata operations.
pub fn result_metadata_type() -> Type {
    Type::SumCtor(vec![
        (".Ok".into(), vec![fs_metadata_record_type()]),
        (".Err".into(), vec![Type::Str]),
    ])
}

/// Construct the filesystem effects record type (`.fs`).
///
/// Fields:
/// - `read_text`: Str -> Result<Str, Str>
/// - `write_text`: Str -> Str -> Result<Unit, Str>
/// - `append_text`: Str -> Str -> Result<Unit, Str>
/// - `list_dir`: Str -> Result<List<Str>, Str>
/// - `remove_file`: Str -> Result<Unit, Str>
/// - `create_dir`: Str -> Result<Unit, Str>
/// - `metadata`: Str -> Result<Metadata, Str>
pub fn fs_effects_record_type() -> Type {
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

/// Look up the type signature for an effect symbol.
///
/// Currently supports:
/// - `.fs`: filesystem effects record
///
/// Returns `None` for unrecognized effect symbols.
pub fn effect_signature(sym: &str) -> Option<Type> {
    match sym {
        ".fs" => Some(fs_effects_record_type()),
        _ => None,
    }
}
