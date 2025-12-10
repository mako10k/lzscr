use crate::force_value;
use crate::{Env, EvalError, Value};
pub fn to_str_like(env: &Env, v: &Value) -> String {
    // Force a thunk here if possible before printing
    let vv: Value = match v {
        Value::Thunk { .. } => match force_value(env, v) {
            Ok(val) => val,
            Err(_) => return "<thunk>".into(),
        },
        _ => v.clone(),
    };
    match &vv {
        Value::Unit => "()".into(),
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Str(s) => s.to_string(),
        Value::Char(c) => char_literal_string(*c),
        Value::Symbol(id) => env.symbol_name(*id),
        Value::Raised(b) => format!("^({})", to_str_like(env, b)),
        Value::Ctor { name, args } => {
            if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                // print as tuple literal
                format!(
                    "({})",
                    args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                )
            } else if args.is_empty() {
                name.clone()
            } else {
                format!(
                    "{}({})",
                    name,
                    args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                )
            }
        }
        Value::List(xs) => {
            format!("[{}]", xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", "))
        }
        Value::Tuple(xs) => {
            format!("({})", xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", "))
        }
        Value::Record(map) => {
            let inner = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", inner)
        }
        Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
        // Thunk should have been forced above, but double-check for safety
        _ => "<thunk>".into(),
    }
}

pub fn char_literal_string(c: i32) -> String {
    let ch = char::from_u32(c as u32).unwrap_or('\u{FFFD}');
    let mut tmp = String::new();
    tmp.push(ch);
    format!("'{}'", tmp.escape_default())
}

pub fn bool_ctor(b: bool) -> Value {
    if b {
        Value::Ctor { name: "True".into(), args: vec![] }
    } else {
        Value::Ctor { name: "False".into(), args: vec![] }
    }
}

pub fn option_value(value: Option<Value>) -> Value {
    match value {
        Some(v) => Value::Ctor { name: "Some".into(), args: vec![v] },
        None => Value::Ctor { name: "None".into(), args: vec![] },
    }
}

pub fn result_ok(value: Value) -> Value {
    Value::Ctor { name: "Ok".into(), args: vec![value] }
}

pub fn result_err(value: Value) -> Value {
    Value::Ctor { name: "Err".into(), args: vec![value] }
}

pub fn as_bool(env: &Env, v: &Value) -> Result<bool, EvalError> {
    let v = force_value(env, v)?;
    match &v {
        Value::Ctor { name, args } if args.is_empty() && name == "True" => Ok(true),
        Value::Ctor { name, args } if args.is_empty() && name == "False" => Ok(false),
        Value::Symbol(id) => {
            let s = env.symbol_name(*id);
            if s == "True" {
                Ok(true)
            } else if s == "False" {
                Ok(false)
            } else {
                Err(EvalError::TypeError)
            }
        }
        _ => Err(EvalError::TypeError),
    }
}

pub fn eff_guard(env: &Env) -> Result<(), EvalError> {
    if env.strict_effects && !env.in_effect_context {
        Err(EvalError::EffectNotAllowed)
    } else {
        Ok(())
    }
}
