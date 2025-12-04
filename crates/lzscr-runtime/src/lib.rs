use std::sync::Arc;

// ===== Runtime core types (Env, Value, Errors, Thunks) =====

mod effects;
mod error;
mod eval;
mod helpers;
mod thunk;
mod value;
pub use effects::*;
pub use error::EvalError;
pub use eval::{apply_value, eval, force_value, match_pattern, v_equal};
pub use helpers::*;
pub use thunk::{ThunkKind, ThunkState};
pub use value::{RtStr, Value};
#[derive(Debug, Clone, Default)]
pub struct Env {
    pub vars: std::collections::HashMap<String, Value>,
    pub strict_effects: bool,
    pub in_effect_context: bool,
    pub ctor_arity: std::collections::HashMap<String, usize>,
    pub sym_intern: std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, u32>>>,
    pub sym_rev: std::rc::Rc<std::cell::RefCell<Vec<String>>>,
    pub str_intern:
        std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, Arc<Vec<u8>>>>>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern_symbol<S: AsRef<str>>(&self, s: S) -> u32 {
        let key = s.as_ref().to_string();
        // fast path
        if let Some(&id) = self.sym_intern.borrow().get(&key) {
            return id;
        }
        // assign new id
        let id = {
            let mut rev = self.sym_rev.borrow_mut();
            let id = rev.len() as u32;
            rev.push(key.clone());
            id
        };
        self.sym_intern.borrow_mut().insert(key, id);
        id
    }

    pub fn symbol_name(&self, id: u32) -> String {
        self.sym_rev.borrow().get(id as usize).cloned().unwrap_or_else(|| format!("#{}", id))
    }

    pub fn intern_string<S: Into<String>>(&self, s: S) -> RtStr {
        let s = s.into();
        if let Some(buf) = self.str_intern.borrow().get(&s).cloned() {
            return RtStr { data: buf, start: 0, len: s.len() };
        }
        let bytes = s.as_bytes().to_vec();
        let arc = Arc::new(bytes);
        self.str_intern.borrow_mut().insert(s.clone(), arc.clone());
        RtStr { data: arc, start: 0, len: s.len() }
    }

    pub fn declare_ctor_arity<S: Into<String>>(&mut self, name: S, arity: usize) {
        self.ctor_arity.insert(name.into(), arity);
    }

    pub fn with_builtins() -> Self {
        let mut e = Env::new();

        // Arithmetic and comparisons
        e.vars.insert(
            "add".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "sub".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "mul".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "div".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(_), Value::Int(b)) if *b == 0 => Err(EvalError::TypeError),
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fadd".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fsub".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fmul".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fdiv".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(_), Value::Float(b)) if *b == 0.0 => Err(EvalError::TypeError),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "eq".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| {
                    let res = v_equal(env, &args[0], &args[1]);
                    Ok(if res {
                        Value::Ctor { name: "True".into(), args: vec![] }
                    } else {
                        Value::Ctor { name: "False".into(), args: vec![] }
                    })
                },
            },
        );
        e.vars.insert(
            "lt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(bool_ctor(a < b)),
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a < b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "le".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(bool_ctor(a <= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a <= b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "gt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(bool_ctor(a > b)),
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a > b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "ge".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(bool_ctor(a >= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a >= b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "ne".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| {
                    let res = !v_equal(env, &args[0], &args[1]);
                    Ok(bool_ctor(res))
                },
            },
        );
        e.vars.insert(
            "flt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a < b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fle".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a <= b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fgt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a > b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fge".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(bool_ctor(a >= b)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // seq/chain/bind (control evaluation order)
        e.vars.insert(
            "seq".into(),
            Value::Native { arity: 2, args: vec![], f: |_env, args| Ok(args[1].clone()) },
        );
        e.vars.insert(
            "chain".into(),
            Value::Native { arity: 2, args: vec![], f: |_env, args| Ok(args[1].clone()) },
        );
        e.vars.insert(
            "bind".into(),
            Value::Native { arity: 2, args: vec![], f: |_env, args| Ok(args[1].clone()) },
        );

        // effects gateway
        e.vars.insert(
            "effects".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| {
                    let sym = match &args[0] {
                        Value::Symbol(id) => env.symbol_name(*id),
                        _ => return Err(EvalError::TypeError),
                    };
                    let eff_value = match sym.as_str() {
                        ".print" => Value::Native { arity: 1, args: vec![], f: eff_print },
                        ".println" => Value::Native { arity: 1, args: vec![], f: eff_println },
                        ".fs" => {
                            let mut fs_fields: std::collections::BTreeMap<String, Value> =
                                std::collections::BTreeMap::new();
                            fs_fields.insert(
                                "read_text".into(),
                                Value::Native { arity: 1, args: vec![], f: eff_fs_read_text },
                            );
                            fs_fields.insert(
                                "write_text".into(),
                                Value::Native { arity: 2, args: vec![], f: eff_fs_write_text },
                            );
                            fs_fields.insert(
                                "append_text".into(),
                                Value::Native { arity: 2, args: vec![], f: eff_fs_append_text },
                            );
                            fs_fields.insert(
                                "list_dir".into(),
                                Value::Native { arity: 1, args: vec![], f: eff_fs_list_dir },
                            );
                            fs_fields.insert(
                                "remove_file".into(),
                                Value::Native { arity: 1, args: vec![], f: eff_fs_remove_file },
                            );
                            fs_fields.insert(
                                "create_dir".into(),
                                Value::Native { arity: 1, args: vec![], f: eff_fs_create_dir },
                            );
                            fs_fields.insert(
                                "metadata".into(),
                                Value::Native { arity: 1, args: vec![], f: eff_fs_metadata },
                            );
                            Value::Record(fs_fields)
                        }
                        _ => return Err(EvalError::UnknownEffect(sym)),
                    };
                    Ok(eff_value)
                },
            },
        );

        // Builtins namespaces
        use std::collections::BTreeMap;
        let mut string_ns: BTreeMap<String, Value> = BTreeMap::new();
        string_ns.insert(
            "len".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Str(s) => Ok(Value::Int(s.char_count() as i64)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        string_ns.insert(
            "concat".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Str(a), Value::Str(b)) => {
                        Ok(Value::Str(env.intern_string(format!("{}{}", a, b))))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        string_ns.insert(
            "slice".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1], &args[2]) {
                    (Value::Str(s), Value::Int(st), Value::Int(le)) => {
                        let stc = *st as usize;
                        let lec = *le as usize;
                        if let Some(sl) = s.slice_chars(stc, lec) {
                            Ok(Value::Str(sl))
                        } else {
                            Err(EvalError::TypeError)
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        string_ns.insert(
            "char_at".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Str(s), Value::Int(idx)) => {
                        let s_utf = match std::str::from_utf8(s.as_bytes()) {
                            Ok(v) => v,
                            Err(_) => {
                                return Ok(Value::Ctor { name: "None".into(), args: vec![] })
                            }
                        };
                        for (i, ch) in s_utf.chars().enumerate() {
                            if (i as i64) == *idx {
                                return Ok(Value::Ctor {
                                    name: "Some".into(),
                                    args: vec![Value::Char(ch as i32)],
                                });
                            }
                        }
                        Ok(Value::Ctor { name: "None".into(), args: vec![] })
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        let mut math_ns: BTreeMap<String, Value> = BTreeMap::new();
        for name in [
            "add", "sub", "mul", "div", "fadd", "fsub", "fmul", "fdiv", "eq", "lt", "le", "gt",
            "ge",
        ] {
            if let Some(v) = e.vars.get(name).cloned() {
                math_ns.insert(name.to_string(), v);
            }
        }

        let mut char_ns: BTreeMap<String, Value> = BTreeMap::new();
        fn bool_val(b: bool) -> Value {
            bool_ctor(b)
        }
        char_ns.insert(
            "is_alpha".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Char(c) => {
                        let ch = char::from_u32(*c as u32).unwrap_or('\u{0}');
                        Ok(bool_val(ch.is_alphabetic()))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        char_ns.insert(
            "is_digit".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Char(c) => {
                        let ch = char::from_u32(*c as u32).unwrap_or('\u{0}');
                        Ok(bool_val(ch.is_ascii_digit()))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        char_ns.insert(
            "is_alnum".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Char(c) => {
                        let ch = char::from_u32(*c as u32).unwrap_or('\u{0}');
                        Ok(bool_val(ch.is_ascii_alphanumeric()))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        char_ns.insert(
            "is_space".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Char(c) => {
                        let ch = char::from_u32(*c as u32).unwrap_or('\u{0}');
                        Ok(bool_val(ch.is_whitespace()))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        char_ns.insert(
            "between".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1], &args[2]) {
                    (Value::Char(c), Value::Char(lo), Value::Char(hi)) => {
                        let code = *c as u32;
                        let lo_c = *lo as u32;
                        let hi_c = *hi as u32;
                        Ok(bool_val(code >= lo_c && code <= hi_c))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        let mut builtins: BTreeMap<String, Value> = BTreeMap::new();
        builtins.insert("string".into(), Value::Record(string_ns));
        builtins.insert("math".into(), Value::Record(math_ns));
        builtins.insert("char".into(), Value::Record(char_ns));

        // scan namespace
        let mut scan_ns: BTreeMap<String, Value> = BTreeMap::new();
        fn get_scan(v: &Value) -> Option<(&RtStr, usize)> {
            if let Value::Record(m) = v {
                let s = m.get("s");
                let i = m.get("i");
                if let (Some(Value::Str(rs)), Some(Value::Int(ix))) = (s, i) {
                    return Some((rs, *ix as usize));
                }
            }
            None
        }
        fn make_scan(s: RtStr, i: usize) -> Value {
            let mut m = BTreeMap::new();
            m.insert("s".into(), Value::Str(s));
            m.insert("i".into(), Value::Int(i as i64));
            Value::Record(m)
        }
        scan_ns.insert(
            "new".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Str(s) => {
                        let mut m = BTreeMap::new();
                        m.insert("s".into(), Value::Str(s.clone()));
                        m.insert("i".into(), Value::Int(0));
                        Ok(Value::Record(m))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "eof".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    v if get_scan(v).is_some() => {
                        let (s, i) = get_scan(v).unwrap();
                        let at_end = i >= s.char_count();
                        Ok(bool_ctor(at_end))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "pos".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    v if get_scan(v).is_some() => Ok(Value::Int(get_scan(v).unwrap().1 as i64)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "set_pos".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (v, Value::Int(p)) if get_scan(v).is_some() => {
                        let (s, _i) = get_scan(v).unwrap();
                        let total = s.char_count();
                        let np = (*p).clamp(0, total as i64) as usize;
                        Ok(make_scan(s.clone(), np))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "peek".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    v if get_scan(v).is_some() => {
                        let (s, i) = get_scan(v).unwrap();
                        let utf = std::str::from_utf8(s.as_bytes()).unwrap_or_default();
                        let ch = utf.chars().nth(i);
                        match ch {
                            Some(c) => Ok(Value::Ctor {
                                name: "Some".into(),
                                args: vec![Value::Char(c as i32)],
                            }),
                            None => Ok(Value::Ctor { name: "None".into(), args: vec![] }),
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "next".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    v if get_scan(v).is_some() => {
                        let (s, i) = get_scan(v).unwrap();
                        let utf = std::str::from_utf8(s.as_bytes()).unwrap_or_default();
                        let mut it = utf.chars();
                        let c = it.nth(i);
                        match c {
                            Some(ch) => {
                                let ns = make_scan(s.clone(), i + 1);
                                let pair = Value::Ctor {
                                    name: ",".into(),
                                    args: vec![Value::Char(ch as i32), ns],
                                };
                                Ok(Value::Ctor { name: "Some".into(), args: vec![pair] })
                            }
                            None => Ok(Value::Ctor { name: "None".into(), args: vec![] }),
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "take_if".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (pred, v) if get_scan(v).is_some() => {
                        let (s, i) = get_scan(v).unwrap();
                        let utf = std::str::from_utf8(s.as_bytes()).unwrap_or_default();
                        if let Some(ch) = utf.chars().nth(i) {
                            let res = apply_value(env, pred.clone(), Value::Char(ch as i32))?;
                            if as_bool(env, &res)? {
                                let ns = make_scan(s.clone(), i + 1);
                                let pair = Value::Ctor {
                                    name: ",".into(),
                                    args: vec![Value::Char(ch as i32), ns],
                                };
                                return Ok(Value::Ctor { name: "Some".into(), args: vec![pair] });
                            }
                        }
                        Ok(Value::Ctor { name: "None".into(), args: vec![] })
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "take_while".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (pred, v) if get_scan(v).is_some() => {
                        let (s, mut i) = get_scan(v).unwrap();
                        let utf = match std::str::from_utf8(s.as_bytes()) {
                            Ok(u) => u.to_string(),
                            Err(_) => String::new(),
                        };
                        let mut out = String::new();
                        for (ci, ch) in utf.chars().enumerate().skip(i) {
                            let res = apply_value(env, pred.clone(), Value::Char(ch as i32))?;
                            if as_bool(env, &res)? {
                                out.push(ch);
                                i = ci + 1;
                            } else {
                                break;
                            }
                        }
                        let ns = make_scan(s.clone(), i);
                        let pair = Value::Ctor {
                            name: ",".into(),
                            args: vec![Value::Str(env.intern_string(out)), ns],
                        };
                        Ok(pair)
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "take_while1".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (pred, v) if get_scan(v).is_some() => {
                        let (s, i0) = get_scan(v).unwrap();
                        let utf = match std::str::from_utf8(s.as_bytes()) {
                            Ok(u) => u.to_string(),
                            Err(_) => String::new(),
                        };
                        let mut out = String::new();
                        let mut i = i0;
                        for (ci, ch) in utf.chars().enumerate().skip(i0) {
                            let res = apply_value(env, pred.clone(), Value::Char(ch as i32))?;
                            if as_bool(env, &res)? {
                                out.push(ch);
                                i = ci + 1;
                            } else {
                                break;
                            }
                        }
                        if out.is_empty() {
                            Ok(Value::Ctor { name: "None".into(), args: vec![] })
                        } else {
                            let ns = make_scan(s.clone(), i);
                            let pair = Value::Ctor {
                                name: ",".into(),
                                args: vec![Value::Str(env.intern_string(out)), ns],
                            };
                            Ok(Value::Ctor { name: "Some".into(), args: vec![pair] })
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        scan_ns.insert(
            "slice_span".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1], &args[2]) {
                    (v, Value::Int(a), Value::Int(b)) if get_scan(v).is_some() => {
                        let (s, _i) = get_scan(v).unwrap();
                        let a = *a as usize;
                        let b = *b as usize;
                        if b >= a {
                            if let Some(part) = s.slice_chars(a, b - a) {
                                Ok(Value::Str(part))
                            } else {
                                Err(EvalError::TypeError)
                            }
                        } else {
                            Err(EvalError::TypeError)
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        builtins.insert("scan".into(), Value::Record(scan_ns));

        let mut uni_ns: BTreeMap<String, Value> = BTreeMap::new();
        uni_ns.insert(
            "of_int".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Int(n) => Ok(Value::Char(*n as i32)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        uni_ns.insert(
            "to_int".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Char(c) => Ok(Value::Int(*c as i64)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        builtins.insert("unicode".into(), Value::Record(uni_ns));
        e.vars.insert("Builtins".into(), Value::Record(builtins));

        // Tuple/Record helpers
        e.vars.insert(
            "Tuple".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Unit => Ok(Value::Tuple(vec![])),
                    v => Ok(Value::Tuple(vec![v.clone()])),
                },
            },
        );
        e.declare_ctor_arity("Tuple", 1);
        e.vars.insert(
            "Record".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| match &args[0] {
                    Value::Unit => Ok(Value::Record(std::collections::BTreeMap::new())),
                    Value::Tuple(kvs) => {
                        let mut map: std::collections::BTreeMap<String, Value> =
                            std::collections::BTreeMap::new();
                        for kv in kvs {
                            match kv {
                                Value::Tuple(xs) if xs.len() == 2 => {
                                    if let Value::Str(k) = &xs[0] {
                                        map.insert(k.to_string(), xs[1].clone());
                                    } else {
                                        return Err(EvalError::TypeError);
                                    }
                                }
                                _ => return Err(EvalError::TypeError),
                            }
                        }
                        Ok(Value::Record(map))
                    }
                    Value::Ctor { name, args }
                        if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') =>
                    {
                        let mut map: std::collections::BTreeMap<String, Value> =
                            std::collections::BTreeMap::new();
                        for kv in args {
                            match kv {
                                Value::Tuple(xs) if xs.len() == 2 => {
                                    if let Value::Str(k) = &xs[0] {
                                        map.insert(k.to_string(), xs[1].clone());
                                    } else {
                                        return Err(EvalError::TypeError);
                                    }
                                }
                                _ => return Err(EvalError::TypeError),
                            }
                        }
                        Ok(Value::Record(map))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.declare_ctor_arity("Record", 1);
        e.vars.insert(
            "KV".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Str(k), v) => Ok(Value::Tuple(vec![Value::Str(k.clone()), v.clone()])),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.declare_ctor_arity("KV", 2);

        // Logic
        e.vars.insert(
            "and".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (true, true) => Ok(bool_ctor(true)),
                    _ => Ok(bool_ctor(false)),
                },
            },
        );
        e.vars.insert(
            "or".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (false, false) => Ok(bool_ctor(false)),
                    _ => Ok(bool_ctor(true)),
                },
            },
        );
        e.vars.insert(
            "not".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| Ok(bool_ctor(!as_bool(env, &args[0])?)),
            },
        );

        // if
        e.vars.insert(
            "if".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |env, args| {
                    let cond = as_bool(env, &args[0])?;
                    let branch = if cond { &args[1] } else { &args[2] };
                    match branch.clone() {
                        Value::Native { arity: 0, f, args } if args.is_empty() => f(env, &[]),
                        Value::Closure { param, body, mut env } => {
                            if let Some(bind) = match_pattern(&env, &param, &Value::Unit) {
                                for (k, v) in bind {
                                    env.vars.insert(k, v);
                                }
                                eval(&env, &body)
                            } else {
                                Ok(Value::Raised(Box::new(Value::Unit)))
                            }
                        }
                        other => Ok(other),
                    }
                },
            },
        );

        // Alt-lambda helper: try left branch, fall back to right on pattern mismatch
        e.vars.insert(
            "alt".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |env, args| {
                    let left = args[0].clone();
                    let right = args[1].clone();
                    let input = args[2].clone();
                    match apply_value(env, left, input.clone())? {
                        Value::Raised(payload) => {
                            if v_equal(env, payload.as_ref(), &input) {
                                apply_value(env, right, input)
                            } else {
                                Ok(Value::Raised(payload))
                            }
                        }
                        other => Ok(other),
                    }
                },
            },
        );

        // to_str, cons
        e.vars.insert(
            "to_str".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| {
                    let s = to_str_like(env, &args[0]);
                    Ok(Value::Str(env.intern_string(s)))
                },
            },
        );
        e.vars.insert(
            "cons".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (head, Value::List(tail)) => {
                        let mut out = Vec::with_capacity(tail.len() + 1);
                        out.push(head.clone());
                        out.extend(tail.clone());
                        Ok(Value::List(out))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // Finish building environment
        e
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lzscr_ast::ast::{Expr, ExprKind, Pattern, PatternKind};
    use lzscr_ast::span::Span;

    fn ref_expr(name: &str) -> Expr {
        Expr::new(ExprKind::Ref(name.into()), Span::new(0, 0))
    }

    #[test]
    fn string_len_and_concat_only() {
        let env = Env::with_builtins();
        // Access Builtins.string namespace
        let string_ns = match env.vars.get("Builtins").cloned().unwrap() {
            Value::Record(m) => m.get("string").cloned().unwrap(),
            _ => panic!("Builtins missing"),
        };
        let (len_f, concat_f) = match string_ns.clone() {
            Value::Record(m) => (m.get("len").cloned().unwrap(), m.get("concat").cloned().unwrap()),
            _ => panic!("string ns not a record"),
        };

        let s = Value::Str(env.intern_string("hé😺llo"));
        // len = 6 (h é 😺 l l o)
        let v = apply_value(&env, len_f.clone(), s.clone()).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 6),
            _ => panic!(),
        }

        // concat("ab","c") = "abc"
        let v = apply_value(&env, concat_f.clone(), Value::Str(env.intern_string("ab")))
            .and_then(|f| apply_value(&env, f, Value::Str(env.intern_string("c"))))
            .unwrap();
        match v {
            Value::Str(rs) => assert_eq!(rs.to_string(), "abc"),
            _ => panic!(),
        }
    }
    fn int_expr(n: i64) -> Expr {
        Expr::new(ExprKind::Int(n), Span::new(0, 0))
    }
    fn str_expr(s: &str) -> Expr {
        Expr::new(ExprKind::Str(s.into()), Span::new(0, 0))
    }
    fn sym_expr(s: &str) -> Expr {
        Expr::new(ExprKind::Symbol(s.into()), Span::new(0, 0))
    }

    #[test]
    fn ctor_equality() {
        // (~eq (Foo 1 2) (Foo 1 2)) => True
        // Build ((~eq (Foo 1 2)) (Foo 1 2))
        let eq_ref = ref_expr("eq");
        // Foo 1 2
        let foo_sym = sym_expr("Foo");
        let foo1 = Expr::new(
            ExprKind::Apply { func: Box::new(foo_sym.clone()), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let foo12 = Expr::new(
            ExprKind::Apply { func: Box::new(foo1), arg: Box::new(int_expr(2)) },
            Span::new(0, 0),
        );

        let left_app = Expr::new(
            ExprKind::Apply { func: Box::new(eq_ref), arg: Box::new(foo12.clone()) },
            Span::new(0, 0),
        );
        let whole = Expr::new(
            ExprKind::Apply { func: Box::new(left_app), arg: Box::new(foo12) },
            Span::new(0, 0),
        );

        let env = Env::with_builtins();
        let v = eval(&env, &whole).unwrap();
        match v {
            Value::Ctor { name, .. } if name == "True" => {}
            _ => panic!("expected Bool true"),
        }

        // Different tag
        let bar_sym = sym_expr("Bar");
        let bar1 = Expr::new(
            ExprKind::Apply { func: Box::new(bar_sym), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let left_app = Expr::new(
            ExprKind::Apply { func: Box::new(ref_expr("eq")), arg: Box::new(bar1) },
            Span::new(0, 0),
        );
        let whole = Expr::new(
            ExprKind::Apply { func: Box::new(left_app), arg: Box::new(sym_expr("Foo")) },
            Span::new(0, 0),
        );
        let env2 = Env::with_builtins();
        let v = eval(&env2, &whole).unwrap();
        match v {
            Value::Ctor { name, .. } if name == "False" => {}
            _ => panic!("expected Bool false"),
        }
    }

    #[test]
    fn bare_constructor_application_succeeds() {
        // Foo 1 2 => Ctor("Foo", [1, 2])
        let foo = sym_expr("Foo");
        let foo1 = Expr::new(
            ExprKind::Apply { func: Box::new(foo), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let foo12 = Expr::new(
            ExprKind::Apply { func: Box::new(foo1), arg: Box::new(int_expr(2)) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &foo12).expect("constructor application should succeed");
        match v {
            Value::Ctor { name, args } => {
                assert_eq!(name, "Foo");
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected constructor value, got {:?}", other),
        }
    }

    #[test]
    fn symbol_application_errors() {
        // (.println 1) should be rejected because symbols are not functions
        let sym = Expr::new(ExprKind::Symbol(".println".into()), Span::new(0, 0));
        let appl = Expr::new(
            ExprKind::Apply { func: Box::new(sym), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let err = eval(&env, &appl).expect_err("symbol application must fail");
        match err {
            EvalError::Traced { kind, .. } => match *kind {
                EvalError::NotApplicable(msg) => {
                    assert!(msg.contains("symbol"), "unexpected message: {}", msg)
                }
                other => panic!("expected NotApplicable, got {:?}", other),
            },
            other => panic!("expected traced error, got {:?}", other),
        }
    }

    #[test]
    fn eval_let_group_basic() {
        // ( ~x = 1; ~x; ~y = 2; ) => 1
        let x_pat = Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) };
        let y_pat = Pattern { kind: PatternKind::Var("y".into()), span: Span::new(0, 0) };
        let e = Expr::new(
            ExprKind::LetGroup {
                type_decls: vec![],
                bindings: vec![
                    (x_pat, Expr::new(ExprKind::Int(1), Span::new(0, 0))),
                    (y_pat, Expr::new(ExprKind::Int(2), Span::new(0, 0))),
                ],
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &e).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 1),
            other => panic!("expected Int(1), got {:?}", other),
        }
    }

    #[test]
    fn eval_add() {
        // (~add 1 2)
        let add_ref = ref_expr("add");
        let one = int_expr(1);
        let two = int_expr(2);
        let appl1 = Expr::new(
            ExprKind::Apply { func: Box::new(add_ref), arg: Box::new(one) },
            Span::new(0, 0),
        );
        let appl2 = Expr::new(
            ExprKind::Apply { func: Box::new(appl1), arg: Box::new(two) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &appl2).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 3),
            _ => panic!("expected Int 3"),
        }
    }

    #[test]
    fn eval_lambda_identity() {
        // ((\x -> x) 42)
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(
            ExprKind::Lambda {
                param: Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) },
                body: Box::new(body),
            },
            Span::new(0, 0),
        );
        let forty_two = int_expr(42);
        let appl = Expr::new(
            ExprKind::Apply { func: Box::new(lam), arg: Box::new(forty_two) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &appl).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("expected Int 42"),
        }
    }

    #[test]
    fn eval_to_str() {
        // (~to_str 5)
        let to_str = ref_expr("to_str");
        let five = int_expr(5);
        let appl = Expr::new(
            ExprKind::Apply { func: Box::new(to_str), arg: Box::new(five) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &appl).unwrap();
        match v {
            Value::Str(s) => assert_eq!(s, "5"),
            _ => panic!("expected Str '5'"),
        }
    }

    #[test]
    fn eval_lambda_pattern_ctor_match() {
        // ((\(Foo x y) -> ~x) (Foo 1 2)) == 1
        let pat = Pattern {
            kind: PatternKind::Ctor {
                name: "Foo".into(),
                args: vec![
                    Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) },
                    Pattern { kind: PatternKind::Var("y".into()), span: Span::new(0, 0) },
                ],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(ExprKind::Lambda { param: pat, body: Box::new(body) }, Span::new(0, 0));
        // Foo 1 2
        let foo = Expr::new(ExprKind::Symbol("Foo".into()), Span::new(0, 0));
        let a1 = Expr::new(
            ExprKind::Apply { func: Box::new(foo), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let foo12 = Expr::new(
            ExprKind::Apply { func: Box::new(a1), arg: Box::new(int_expr(2)) },
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply { func: Box::new(lam), arg: Box::new(foo12) },
            Span::new(0, 0),
        );
        let v = eval(&Env::with_builtins(), &appl).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 1),
            _ => panic!("expected Int 1"),
        }
    }

    #[test]
    fn eval_lambda_pattern_mismatch_raises() {
        // ((\(Foo x) -> ~x) (Bar 1)) => ^(Bar(1))
        let pat = Pattern {
            kind: PatternKind::Ctor {
                name: "Foo".into(),
                args: vec![Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) }],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(ExprKind::Lambda { param: pat, body: Box::new(body) }, Span::new(0, 0));
        let bar = Expr::new(ExprKind::Symbol("Bar".into()), Span::new(0, 0));
        let bar1 = Expr::new(
            ExprKind::Apply { func: Box::new(bar), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply { func: Box::new(lam), arg: Box::new(bar1) },
            Span::new(0, 0),
        );
        let v = eval(&Env::with_builtins(), &appl).unwrap();
        match v {
            Value::Raised(b) => match *b {
                Value::Ctor { name, args } => {
                    assert_eq!(name, "Bar");
                    assert_eq!(args.len(), 1);
                }
                other => panic!("unexpected payload: {:?}", other),
            },
            _ => panic!("expected Raised"),
        }
    }

    #[test]
    fn effects_non_strict_allowed() {
        // ((~effects .println) "x")
        let eff = Expr::new(
            ExprKind::Apply {
                func: Box::new(ref_expr("effects")),
                arg: Box::new(sym_expr(".println")),
            },
            Span::new(0, 0),
        );
        let call = Expr::new(
            ExprKind::Apply { func: Box::new(eff), arg: Box::new(str_expr("x")) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let v = eval(&env, &call).unwrap();
        matches!(v, Value::Unit);
    }

    #[test]
    fn lambda_list_pattern_matches() {
        // (\[ ~x, ~y ] -> ~x) [10, 20] == 10
        let lam = Expr::new(
            ExprKind::Lambda {
                param: Pattern {
                    kind: PatternKind::List(vec![
                        Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) },
                        Pattern { kind: PatternKind::Var("y".into()), span: Span::new(0, 0) },
                    ]),
                    span: Span::new(0, 0),
                },
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let arg = Expr::new(ExprKind::List(vec![int_expr(10), int_expr(20)]), Span::new(0, 0));
        let appl =
            Expr::new(ExprKind::Apply { func: Box::new(lam), arg: Box::new(arg) }, Span::new(0, 0));
        let v = eval(&Env::with_builtins(), &appl).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 10),
            _ => panic!("expected Int 10"),
        }
    }

    #[test]
    fn lambda_cons_pattern_matches() {
        // (\( ~h : ~t ) -> ~h) [7,8,9] == 7
        let lam = Expr::new(
            ExprKind::Lambda {
                param: Pattern {
                    kind: PatternKind::Cons(
                        Box::new(Pattern {
                            kind: PatternKind::Var("h".into()),
                            span: Span::new(0, 0),
                        }),
                        Box::new(Pattern {
                            kind: PatternKind::Var("t".into()),
                            span: Span::new(0, 0),
                        }),
                    ),
                    span: Span::new(0, 0),
                },
                body: Box::new(Expr::new(ExprKind::Ref("h".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let arg =
            Expr::new(ExprKind::List(vec![int_expr(7), int_expr(8), int_expr(9)]), Span::new(0, 0));
        let appl =
            Expr::new(ExprKind::Apply { func: Box::new(lam), arg: Box::new(arg) }, Span::new(0, 0));
        let v = eval(&Env::with_builtins(), &appl).unwrap();
        match v {
            Value::Int(n) => assert_eq!(n, 7),
            _ => panic!("expected Int 7"),
        }
    }

    #[test]
    fn effects_strict_blocked() {
        let eff = Expr::new(
            ExprKind::Apply {
                func: Box::new(ref_expr("effects")),
                arg: Box::new(sym_expr(".println")),
            },
            Span::new(0, 0),
        );
        let call = Expr::new(
            ExprKind::Apply { func: Box::new(eff), arg: Box::new(str_expr("x")) },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let mut env_strict = env.clone();
        env_strict.strict_effects = true;
        let err = eval(&env_strict, &call).unwrap_err();
        match err {
            EvalError::EffectNotAllowed => {}
            EvalError::Traced { kind, .. } if matches!(*kind, EvalError::EffectNotAllowed) => {}
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn effects_strict_allowed_via_seq() {
        // (~seq () ((~effects .println) "x"))
        let seq_ref = ref_expr("seq");
        let unit = Expr::new(ExprKind::Unit, Span::new(0, 0));
        let eff = Expr::new(
            ExprKind::Apply {
                func: Box::new(ref_expr("effects")),
                arg: Box::new(sym_expr(".println")),
            },
            Span::new(0, 0),
        );
        let call = Expr::new(
            ExprKind::Apply { func: Box::new(eff), arg: Box::new(str_expr("x")) },
            Span::new(0, 0),
        );
        let first = Expr::new(
            ExprKind::Apply { func: Box::new(seq_ref), arg: Box::new(unit) },
            Span::new(0, 0),
        );
        let expr = Expr::new(
            ExprKind::Apply { func: Box::new(first), arg: Box::new(call) },
            Span::new(0, 0),
        );
        let mut env = Env::with_builtins();
        env.strict_effects = true;
        let v = eval(&env, &expr).unwrap();
        matches!(v, Value::Unit);
    }

    #[test]
    fn eval_list_literal() {
        let env = Env::with_builtins();
        let e =
            Expr::new(ExprKind::List(vec![int_expr(1), int_expr(2), int_expr(3)]), Span::new(0, 0));
        let v = eval(&env, &e).unwrap();
        match v {
            Value::List(xs) => assert_eq!(xs.len(), 3),
            _ => panic!("expected List"),
        }
    }

    #[test]
    fn eval_cons_builds_list() {
        // ((~cons 1) [2,3]) => [1,2,3]
        let env = Env::with_builtins();
        let cons_ref = ref_expr("cons");
        let app1 = Expr::new(
            ExprKind::Apply { func: Box::new(cons_ref), arg: Box::new(int_expr(1)) },
            Span::new(0, 0),
        );
        let list = Expr::new(ExprKind::List(vec![int_expr(2), int_expr(3)]), Span::new(0, 0));
        let whole = Expr::new(
            ExprKind::Apply { func: Box::new(app1), arg: Box::new(list) },
            Span::new(0, 0),
        );
        let v = eval(&env, &whole).unwrap();
        match v {
            Value::List(xs) => {
                assert_eq!(xs.len(), 3);
                match (&xs[0], &xs[1], &xs[2]) {
                    (Value::Int(1), Value::Int(2), Value::Int(3)) => {}
                    _ => panic!("unexpected list contents: {:?}", xs),
                }
            }
            _ => panic!("expected List"),
        }
    }
}
