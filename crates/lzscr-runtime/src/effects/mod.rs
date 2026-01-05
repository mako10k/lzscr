use crate::force_value;
use crate::helpers::{
    bool_ctor, char_literal_string, eff_guard, option_value, result_err, result_ok, to_str_like,
};
use crate::{Env, EvalError, Value};
use std::io::{Read as _, Seek as _, Write as _};
use std::sync::Arc;

#[derive(Debug)]
pub enum FsHandle {
    File(std::fs::File),
    Stdin,
    Stdout,
    Stderr,
}
pub fn eff_print(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    let v0 = force_value(env, &args[0])?;
    match &v0 {
        Value::Str(s) => {
            print!("{}", s);
            Ok(Value::Unit)
        }
        other => match other.clone() {
            Value::Unit => {
                print!("()");
                Ok(Value::Unit)
            }
            Value::Int(n) => {
                print!("{}", n);
                Ok(Value::Unit)
            }
            Value::Float(f) => {
                print!("{}", f);
                Ok(Value::Unit)
            }
            Value::Char(c) => {
                print!("{}", char_literal_string(c));
                Ok(Value::Unit)
            }
            Value::Symbol(id) => {
                print!("{}", env.symbol_name(id));
                Ok(Value::Unit)
            }
            Value::Raised(b) => {
                print!("^({})", to_str_like(env, &b));
                Ok(Value::Unit)
            }
            Value::Ctor { name, args } => {
                if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                    print!(
                        "({})",
                        args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                    );
                } else if args.is_empty() {
                    print!("{}", name);
                } else {
                    print!(
                        "{}({})",
                        name,
                        args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                    );
                }
                Ok(Value::Unit)
            }
            Value::List(xs) => {
                print!(
                    "[{}]",
                    xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Tuple(xs) => {
                print!(
                    "({})",
                    xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Record(map) => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                print!("{{{}}}", inner);
                Ok(Value::Unit)
            }
            Value::ModeMap { fields: map, default } => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                match default {
                    Some(d) if inner.is_empty() => {
                        print!(".{{; {}}}", to_str_like(env, d.as_ref()))
                    }
                    Some(d) => print!(".{{{}; {}}}", inner, to_str_like(env, d.as_ref())),
                    None => print!(".{{{}}}", inner),
                }
                Ok(Value::Unit)
            }
            Value::Native { .. } | Value::Closure { .. } => {
                print!("<fun>");
                Ok(Value::Unit)
            }
            Value::Thunk { .. } => unreachable!(),
            Value::Str(_) => unreachable!(),
        },
    }
}

pub fn eff_println(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    let v0 = force_value(env, &args[0])?;
    match &v0 {
        Value::Str(s) => {
            println!("{}", s);
            Ok(Value::Unit)
        }
        other => match other.clone() {
            Value::Unit => {
                println!("()");
                Ok(Value::Unit)
            }
            Value::Int(n) => {
                println!("{}", n);
                Ok(Value::Unit)
            }
            Value::Float(f) => {
                println!("{}", f);
                Ok(Value::Unit)
            }
            Value::Char(c) => {
                println!("{}", char_literal_string(c));
                Ok(Value::Unit)
            }
            Value::Symbol(id) => {
                println!("{}", env.symbol_name(id));
                Ok(Value::Unit)
            }
            Value::Raised(b) => {
                println!("^({})", to_str_like(env, &b));
                Ok(Value::Unit)
            }
            Value::Ctor { name, args } => {
                if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                    println!(
                        "({})",
                        args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                    );
                } else if args.is_empty() {
                    println!("{}", name);
                } else {
                    println!(
                        "{}({})",
                        name,
                        args.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                    );
                }
                Ok(Value::Unit)
            }
            Value::List(xs) => {
                println!(
                    "[{}]",
                    xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Tuple(xs) => {
                println!(
                    "({})",
                    xs.iter().map(|x| to_str_like(env, x)).collect::<Vec<_>>().join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Record(map) => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("{{{}}}", inner);
                Ok(Value::Unit)
            }
            Value::ModeMap { fields: map, default } => {
                let inner = map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                match default {
                    Some(d) if inner.is_empty() => {
                        println!(".{{; {}}}", to_str_like(env, d.as_ref()))
                    }
                    Some(d) => println!(".{{{}; {}}}", inner, to_str_like(env, d.as_ref())),
                    None => println!(".{{{}}}", inner),
                }
                Ok(Value::Unit)
            }
            Value::Native { .. } | Value::Closure { .. } => {
                println!("<fun>");
                Ok(Value::Unit)
            }
            Value::Thunk { .. } => unreachable!(),
            Value::Str(_) => unreachable!(),
        },
    }
}

pub fn eff_fs_read_text(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::read_to_string(&path) {
        Ok(contents) => Ok(result_ok(Value::Str(env.intern_string(contents)))),
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

fn handle_from_value(env: &Env, v: &Value) -> Result<usize, Value> {
    match force_value(env, v) {
        Ok(Value::Int(n)) if n >= 0 => Ok(n as usize),
        _ => Err(result_err(Value::Str(env.intern_string("invalid handle")))),
    }
}

fn size_from_value(env: &Env, v: &Value) -> Result<usize, Value> {
    match force_value(env, v) {
        Ok(Value::Int(n)) if n >= 0 => Ok(n as usize),
        _ => Err(result_err(Value::Str(env.intern_string("invalid size")))),
    }
}

fn pos_from_value(env: &Env, v: &Value) -> Result<u64, Value> {
    match force_value(env, v) {
        Ok(Value::Int(n)) if n >= 0 => match u64::try_from(n) {
            Ok(p) => Ok(p),
            Err(_) => Err(result_err(Value::Str(env.intern_string("invalid position")))),
        },
        _ => Err(result_err(Value::Str(env.intern_string("invalid position")))),
    }
}

pub fn eff_fs_open(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => String::from_utf8_lossy(s.as_bytes()).to_string(),
        _ => return Err(EvalError::TypeError),
    };

    let file = std::fs::OpenOptions::new().read(true).write(true).create(true).open(&path);
    match file {
        Ok(f) => {
            let mut table = env.file_handles.borrow_mut();
            // reuse empty slot if possible
            let mut idx: Option<usize> = None;
            for (i, slot) in table.iter().enumerate() {
                if slot.is_none() {
                    idx = Some(i);
                    break;
                }
            }
            let i = idx.unwrap_or_else(|| {
                table.push(None);
                table.len() - 1
            });
            table[i] = Some(FsHandle::File(f));
            Ok(result_ok(Value::Int(i as i64)))
        }
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_read(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 2 {
        return Err(EvalError::TypeError);
    }

    let handle = match handle_from_value(env, &args[0]) {
        Ok(h) => h,
        Err(v) => return Ok(v),
    };
    let size = match size_from_value(env, &args[1]) {
        Ok(n) => n,
        Err(v) => return Ok(v),
    };

    let mut table = env.file_handles.borrow_mut();
    let Some(Some(h)) = table.get_mut(handle) else {
        return Ok(result_err(Value::Str(env.intern_string("invalid handle"))));
    };

    let mut buf = vec![0u8; size];
    match h {
        FsHandle::File(file) => match file.read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                let data = Arc::new(buf);
                Ok(result_ok(Value::Str(crate::RtStr { data, start: 0, len: n })))
            }
            Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
        },
        FsHandle::Stdin => {
            drop(table);
            match std::io::stdin().lock().read(&mut buf) {
                Ok(n) => {
                    buf.truncate(n);
                    let data = Arc::new(buf);
                    Ok(result_ok(Value::Str(crate::RtStr { data, start: 0, len: n })))
                }
                Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
            }
        }
        FsHandle::Stdout | FsHandle::Stderr => Ok(result_err(Value::Str(
            env.intern_string("handle is not readable"),
        ))),
    }
}

pub fn eff_fs_write(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 2 {
        return Err(EvalError::TypeError);
    }

    let handle = match handle_from_value(env, &args[0]) {
        Ok(h) => h,
        Err(v) => return Ok(v),
    };
    let payload_val = force_value(env, &args[1])?;
    let payload = match payload_val {
        Value::Str(s) => s,
        _ => return Err(EvalError::TypeError),
    };

    let mut table = env.file_handles.borrow_mut();
    let Some(Some(h)) = table.get_mut(handle) else {
        return Ok(result_err(Value::Str(env.intern_string("invalid handle"))));
    };

    match h {
        FsHandle::File(file) => match file.write_all(payload.as_bytes()) {
            Ok(()) => Ok(result_ok(Value::Int(payload.len as i64))),
            Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
        },
        FsHandle::Stdout => {
            drop(table);
            match std::io::stdout().lock().write_all(payload.as_bytes()) {
                Ok(()) => Ok(result_ok(Value::Int(payload.len as i64))),
                Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
            }
        }
        FsHandle::Stderr => {
            drop(table);
            match std::io::stderr().lock().write_all(payload.as_bytes()) {
                Ok(()) => Ok(result_ok(Value::Int(payload.len as i64))),
                Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
            }
        }
        FsHandle::Stdin => Ok(result_err(Value::Str(
            env.intern_string("handle is not writable"),
        ))),
    }
}

pub fn eff_fs_close(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let handle = match handle_from_value(env, &args[0]) {
        Ok(h) => h,
        Err(v) => return Ok(v),
    };

    let mut table = env.file_handles.borrow_mut();
    let Some(slot) = table.get_mut(handle) else {
        return Ok(result_err(Value::Str(env.intern_string("invalid handle"))));
    };
    match slot {
        Some(FsHandle::Stdin) | Some(FsHandle::Stdout) | Some(FsHandle::Stderr) => Ok(result_err(
            Value::Str(env.intern_string("cannot close stdio")),
        )),
        Some(FsHandle::File(_)) => {
            slot.take();
            Ok(result_ok(Value::Unit))
        }
        None => Ok(result_err(Value::Str(env.intern_string("invalid handle")))),
    }
}

pub fn eff_fs_seek(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 2 {
        return Err(EvalError::TypeError);
    }

    let handle = match handle_from_value(env, &args[0]) {
        Ok(h) => h,
        Err(v) => return Ok(v),
    };
    let pos = match pos_from_value(env, &args[1]) {
        Ok(p) => p,
        Err(v) => return Ok(v),
    };

    let mut table = env.file_handles.borrow_mut();
    let Some(Some(h)) = table.get_mut(handle) else {
        return Ok(result_err(Value::Str(env.intern_string("invalid handle"))));
    };

    let FsHandle::File(file) = h else {
        return Ok(result_err(Value::Str(env.intern_string("handle is not seekable"))));
    };

    match file.seek(std::io::SeekFrom::Start(pos)) {
        Ok(new_pos) => {
            if new_pos > i64::MAX as u64 {
                Ok(result_err(Value::Str(env.intern_string("position too large"))))
            } else {
                Ok(result_ok(Value::Int(new_pos as i64)))
            }
        }
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_stdin(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    Ok(result_ok(Value::Int(0)))
}

pub fn eff_fs_stdout(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    Ok(result_ok(Value::Int(1)))
}

pub fn eff_fs_stderr(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    Ok(result_ok(Value::Int(2)))
}

pub fn eff_fs_write_text(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 2 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let contents_val = force_value(env, &args[1])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    let contents = match contents_val {
        Value::Str(s) => s,
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::write(&path, contents.as_bytes()) {
        Ok(()) => Ok(result_ok(Value::Unit)),
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_append_text(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 2 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let contents_val = force_value(env, &args[1])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    let contents = match contents_val {
        Value::Str(s) => s,
        _ => return Err(EvalError::TypeError),
    };
    let res =
        std::fs::OpenOptions::new().create(true).append(true).open(&path).and_then(|mut file| {
            use std::io::Write as _;
            file.write_all(contents.as_bytes())
        });
    match res {
        Ok(()) => Ok(result_ok(Value::Unit)),
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_list_dir(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::read_dir(&path) {
        Ok(entries) => {
            let mut out = Vec::new();
            for entry in entries {
                match entry {
                    Ok(dir_entry) => {
                        let name = dir_entry.file_name();
                        let name = name.to_string_lossy().to_string();
                        out.push(Value::Str(env.intern_string(name)));
                    }
                    Err(err) => {
                        return Ok(result_err(Value::Str(env.intern_string(err.to_string()))));
                    }
                }
            }
            Ok(result_ok(Value::List(out)))
        }
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_remove_file(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::remove_file(&path) {
        Ok(()) => Ok(result_ok(Value::Unit)),
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_create_dir(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::create_dir_all(&path) {
        Ok(()) => Ok(result_ok(Value::Unit)),
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}

pub fn eff_fs_metadata(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    if args.len() != 1 {
        return Err(EvalError::TypeError);
    }
    let path_val = force_value(env, &args[0])?;
    let path = match path_val {
        Value::Str(s) => s.to_string(),
        _ => return Err(EvalError::TypeError),
    };
    match std::fs::metadata(&path) {
        Ok(meta) => {
            let mut fields = std::collections::BTreeMap::new();
            fields.insert("is_dir".into(), bool_ctor(meta.is_dir()));
            fields.insert("is_file".into(), bool_ctor(meta.is_file()));
            fields.insert("readonly".into(), bool_ctor(meta.permissions().readonly()));
            let modified_ms = meta
                .modified()
                .ok()
                .and_then(|ts| ts.duration_since(std::time::UNIX_EPOCH).ok())
                .map(|dur| {
                    let millis = dur.as_millis();
                    let clamped = if millis > i64::MAX as u128 { i64::MAX } else { millis as i64 };
                    Value::Int(clamped)
                });
            fields.insert("modified_ms".into(), option_value(modified_ms));
            let len_u64 = meta.len();
            let size_clamped = if len_u64 > i64::MAX as u64 { i64::MAX } else { len_u64 as i64 };
            fields.insert("size".into(), Value::Int(size_clamped));
            Ok(result_ok(Value::Record(fields)))
        }
        Err(err) => Ok(result_err(Value::Str(env.intern_string(err.to_string())))),
    }
}
