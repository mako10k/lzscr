use lzscr_ast::ast::*;
use std::sync::Arc;

// Runtime string: shared UTF-8 buffer with slice (start,len)
#[derive(Debug, Clone)]
pub struct RtStr {
    pub data: Arc<Vec<u8>>, // whole buffer
    pub start: usize,       // byte offset
    pub len: usize,         // byte length
}

impl RtStr {
    pub fn as_bytes(&self) -> &[u8] {
        &self.data[self.start..self.start + self.len]
    }
    // Removed inherent to_string to avoid shadowing Display
    pub fn eq_str(&self, s: &str) -> bool {
        self.as_bytes() == s.as_bytes()
    }
    pub fn char_count(&self) -> usize {
        std::str::from_utf8(self.as_bytes())
            .map(|s| s.chars().count())
            .unwrap_or_else(|_| String::from_utf8_lossy(self.as_bytes()).chars().count())
    }
    fn char_boundaries(s: &str) -> Vec<usize> {
        // boundaries[ci] = byte index at char index ci; includes trailing s.len()
        let mut b: Vec<usize> = s.char_indices().map(|(i, _)| i).collect();
        b.push(s.len());
        b
    }
    pub fn slice_chars(&self, start_chars: usize, len_chars: usize) -> Option<RtStr> {
        let s = std::str::from_utf8(self.as_bytes()).ok()?;
        let boundaries = RtStr::char_boundaries(s);
        let total_chars = boundaries.len() - 1;
        if start_chars > total_chars {
            return None;
        }
        if start_chars + len_chars > total_chars {
            return None;
        }
        let begin_b = boundaries[start_chars];
        let end_b = boundaries[start_chars + len_chars];
        Some(RtStr {
            data: self.data.clone(),
            start: self.start + begin_b,
            len: end_b.saturating_sub(begin_b),
        })
    }
}

impl std::fmt::Display for RtStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::str::from_utf8(self.as_bytes()) {
            Ok(s) => f.write_str(s),
            Err(_) => f.write_str(&String::from_utf8_lossy(self.as_bytes())),
        }
    }
}

impl PartialEq for RtStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}
impl Eq for RtStr {}
impl PartialEq<String> for RtStr {
    fn eq(&self, other: &String) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}
impl PartialEq<&str> for RtStr {
    fn eq(&self, other: &&str) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("not a function")]
    NotFunc,
    #[error("unbound ref: {0}")]
    Unbound(String),
    #[error("type error")]
    TypeError,
    #[error("effect not allowed outside effect context")]
    EffectNotAllowed,
    #[error("unknown effect: {0}")]
    UnknownEffect(String),
    #[error("raised: {0}")]
    Raised(String),
    #[error("runtime error with trace")]
    Traced { kind: Box<EvalError>, spans: Vec<lzscr_ast::span::Span> },
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    // Sliceable runtime string (UTF-8 bytes; may contain NUL). Semantics: list of Char(codepoint).
    Str(RtStr),
    // Codepoint character (integer). Conversions with Int via builtins.
    Char(i32),
    // Simple immutable containers (PoC)
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Record(std::collections::BTreeMap<String, Value>),
    // Constructor value: tag name with payload args (order-significant)
    Ctor { name: String, args: Vec<Value> },
    Native { arity: usize, f: fn(&Env, &[Value]) -> Result<Value, EvalError>, args: Vec<Value> },
    Closure { param: Pattern, body: Expr, env: Env },
    Symbol(u32),
    // Exception payload (internal). Represent ^(Expr) after evaluation as a value that propagates.
    Raised(Box<Value>),
    // Lazy thunk for recursive/non-function let bindings
    Thunk { state: std::rc::Rc<std::cell::RefCell<ThunkState>>, kind: ThunkKind },
}

#[derive(Debug, Clone)]
pub enum ThunkKind {
    Expr { expr: Expr, env: std::rc::Rc<std::cell::RefCell<Env>> },
    Project { src: Box<Value>, pattern: Pattern, var: String },
}

#[derive(Debug, Clone)]
pub enum ThunkState {
    Unevaluated,
    Evaluating,
    Evaluated(Box<Value>),
}

#[derive(Debug, Clone, Default)]
pub struct Env {
    pub vars: std::collections::HashMap<String, Value>,
    pub strict_effects: bool,
    pub in_effect_context: bool,
    pub ctor_arity: std::collections::HashMap<String, usize>,
    // symbol interning
    pub sym_intern: std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, u32>>>,
    pub sym_rev: std::rc::Rc<std::cell::RefCell<Vec<String>>>,
    // string interning: map full string to shared byte buffer
    pub str_intern:
        std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, Arc<Vec<u8>>>>>,
}

// Default is derived

impl Env {
    pub fn new() -> Self {
        Self {
            vars: std::collections::HashMap::new(),
            strict_effects: false,
            in_effect_context: false,
            ctor_arity: std::collections::HashMap::new(),
            sym_intern: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
            sym_rev: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
            str_intern: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        }
    }

    pub fn intern_symbol<S: AsRef<str>>(&self, s: S) -> u32 {
        let key = s.as_ref();
        if let Some(&id) = self.sym_intern.borrow().get(key) {
            return id;
        }
        let mut rev = self.sym_rev.borrow_mut();
        let id = rev.len() as u32;
        rev.push(key.to_string());
        self.sym_intern.borrow_mut().insert(key.to_string(), id);
        id
    }

    pub fn symbol_name(&self, id: u32) -> String {
        self.sym_rev.borrow().get(id as usize).cloned().unwrap_or("<sym?>".to_string())
    }

    pub fn intern_string<S: AsRef<str>>(&self, s: S) -> RtStr {
        let key = s.as_ref();
        if let Some(buf) = self.str_intern.borrow().get(key) {
            return RtStr { data: buf.clone(), start: 0, len: buf.len() };
        }
        let bytes = key.as_bytes().to_vec();
        let arc = Arc::new(bytes);
        self.str_intern.borrow_mut().insert(key.to_string(), arc.clone());
        RtStr { data: arc, start: 0, len: key.len() }
    }

    pub fn declare_ctor_arity(&mut self, name: &str, arity: usize) {
        // Accept registration names with or without the leading dot (and try both when looking up)
        let n = name.to_string();
        self.ctor_arity.insert(n.clone(), arity);
        if n.starts_with('.') {
            self.ctor_arity.insert(n.trim_start_matches('.').to_string(), arity);
        } else {
            self.ctor_arity.insert(format!(".{n}"), arity);
        }
    }

    pub fn with_builtins() -> Self {
        let mut e = Env::new();

    // Bool via constructors .True / .False (no legacy ~true/~false refs)
    e.declare_ctor_arity(".True", 0);
    e.declare_ctor_arity(".False", 0);
    // Pre-intern commonly used symbols
    let _ = e.intern_symbol(".True");
    let _ = e.intern_symbol(".False");
        let _ = e.intern_symbol(".print");
        let _ = e.intern_symbol(".println");
        let _ = e.intern_symbol(".,");

        // Bool constructor: (.true|.false) -> Bool
        e.vars.insert(
            "Bool".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| match &args[0] {
                    Value::Symbol(id) if env.symbol_name(*id) == ".True" => Ok(Value::Bool(true)),
                    Value::Symbol(id) if env.symbol_name(*id) == ".False" => Ok(Value::Bool(false)),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.declare_ctor_arity("Bool", 1);

        // to_str : a -> Str
        e.vars.insert(
            "to_str".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| {
                    let v = &args[0];
                    Ok(Value::Str(env.intern_string(to_str_like(env, v))))
                },
            },
        );

        // add : (Int|Float) -> (Int|Float) -> same
        e.vars.insert(
            "cons".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (h, Value::List(tl)) => {
                        let mut v = Vec::with_capacity(tl.len() + 1);
                        v.push(h.clone());
                        v.extend_from_slice(tl);
                        Ok(Value::List(v))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // alt : (a -> r) -> (a -> r) -> a -> r
        e.vars.insert(
            "alt".into(),
            Value::Native {
                arity: 3,
                args: vec![],
                f: |env, args| {
                    let lf = args[0].clone();
                    let rf = args[1].clone();
                    let av = args[2].clone();
                    let res = apply_value(env, lf, av.clone())?;
                    match res {
                        Value::Raised(payload) => {
                            if v_equal(env, &payload, &av) {
                                apply_value(env, rf, av)
                            } else {
                                Ok(Value::Raised(payload))
                            }
                        }
                        other => Ok(other),
                    }
                },
            },
        );

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

        // sub : (Int|Float) -> (Int|Float) -> same
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

        // mul : (Int|Float) -> (Int|Float) -> same
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

        // div : Int -> Int -> Int (division by 0 is an error)
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

        // fadd : Float -> Float -> Float
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
        // fsub : Float -> Float -> Float
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
        // fmul : Float -> Float -> Float
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
        // fdiv : Float -> Float -> Float (division by 0.0 is an error)
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

        // eq : Int|Float|Bool|Str|Unit|Symbol -> same -> Symbol("True"|"False")
        e.vars.insert(
            "eq".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| {
                    let res = v_equal(env, &args[0], &args[1]);
                    Ok(Value::Bool(res))
                },
            },
        );

        // lt : Int|Float -> Int|Float -> Symbol("True"|"False")
        e.vars.insert(
            "lt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(Value::Bool(a < b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a < b))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // le : Int|Float -> Int|Float -> Symbol("True"|"False")
        e.vars.insert(
            "le".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(Value::Bool(a <= b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a <= b))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // gt : Int|Float -> Int|Float -> Symbol("True"|"False")
        e.vars.insert(
            "gt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(Value::Bool(a > b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a > b))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // ge : Int|Float -> Int|Float -> Symbol("True"|"False")
        e.vars.insert(
            "ge".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(Value::Bool(a >= b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a >= b))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // ne : a -> a -> Bool (structural inequality)
        e.vars.insert(
            "ne".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| {
                    let res = !v_equal(env, &args[0], &args[1]);
                    Ok(Value::Bool(res))
                },
            },
        );

        // Float-only comparisons: flt/fle/fgt/fge
        e.vars.insert(
            "flt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a < b))
                    }
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
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a <= b))
                    }
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
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a > b))
                    }
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
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Bool(a >= b))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // seq/chain/bind are special forms to control evaluation order; not needed as Natives but registered for name resolution
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

        // effects: .sym -> effect function (guarded by strict-effects)
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
                    let f: fn(&Env, &[Value]) -> Result<Value, EvalError> = match sym.as_str() {
                        ".print" => eff_print,
                        ".println" => eff_println,
                        _ => return Err(EvalError::UnknownEffect(sym)),
                    };
                    Ok(Value::Native { arity: 1, args: vec![], f })
                },
            },
        );
        // ---------------- Builtins record (namespaced access, prefer stdlib delegation) ----------------
        use std::collections::BTreeMap;

        // string namespace
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
        // slice by char indices: slice(str, start_chars, len_chars) -> Str
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
        // char_at(str, index_chars) -> .Some(Char) | .None
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
                                return Ok(Value::Ctor { name: ".None".into(), args: vec![] })
                            }
                        };
                        for (i, ch) in s_utf.chars().enumerate() {
                            if (i as i64) == *idx {
                                return Ok(Value::Ctor {
                                    name: ".Some".into(),
                                    args: vec![Value::Char(ch as i32)],
                                });
                            }
                        }
                        Ok(Value::Ctor { name: ".None".into(), args: vec![] })
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // keep builtins minimal: only len/concat for strings for now

        // math namespace (reuse existing natives where possible)
        let mut math_ns: BTreeMap<String, Value> = BTreeMap::new();
        for name in [
            "add", "sub", "mul", "div", // int
            "fadd", "fsub", "fmul", "fdiv", // float
            "eq", "lt", "le", "gt", "ge",
        ] {
            if let Some(v) = e.vars.get(name).cloned() {
                math_ns.insert(name.to_string(), v);
            }
        }

    // char classification namespace
    let mut char_ns: BTreeMap<String, Value> = BTreeMap::new();
    fn bool_val(b: bool) -> Value { Value::Bool(b) }
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
                    (Value::Char(c), Value::Int(lo), Value::Int(hi)) => {
                        let code = *c as i64;
                        Ok(bool_val(code >= *lo && code <= *hi))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        let mut builtins: BTreeMap<String, Value> = BTreeMap::new();
        builtins.insert("string".into(), Value::Record(string_ns));
        builtins.insert("math".into(), Value::Record(math_ns));
        builtins.insert("char".into(), Value::Record(char_ns));

        // scan namespace: functional scanner over UTF-8 string (char-index based)
        let mut scan_ns: BTreeMap<String, Value> = BTreeMap::new();
        // Helpers to extract scan record fields
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
        // new : Str -> Scan
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
    // eof : Scan -> Bool
        scan_ns.insert(
            "eof".into(),
            Value::Native {
                arity: 1,
                args: vec![],
        f: |_env, args| match &args[0] {
                    v if get_scan(v).is_some() => {
                        let (s, i) = get_scan(v).unwrap();
                        let at_end = i >= s.char_count();
            Ok(Value::Bool(at_end))
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // pos : Scan -> Int
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
        // set_pos : Scan -> Int -> Scan
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
        // peek : Scan -> .Some(Char) | .None
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
                                name: ".Some".into(),
                                args: vec![Value::Char(c as i32)],
                            }),
                            None => Ok(Value::Ctor { name: ".None".into(), args: vec![] }),
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // next : Scan -> .Some((Char, Scan)) | .None
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
                                // 2-tuple tag is '.,'
                                let pair = Value::Ctor {
                                    name: ".,".into(),
                                    args: vec![Value::Char(ch as i32), ns],
                                };
                                Ok(Value::Ctor { name: ".Some".into(), args: vec![pair] })
                            }
                            None => Ok(Value::Ctor { name: ".None".into(), args: vec![] }),
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // take_if : (Char -> Bool) -> Scan -> .Some((Char, Scan)) | .None
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
                                    name: ".,".into(),
                                    args: vec![Value::Char(ch as i32), ns],
                                };
                                return Ok(Value::Ctor { name: ".Some".into(), args: vec![pair] });
                            }
                        }
                        Ok(Value::Ctor { name: ".None".into(), args: vec![] })
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // take_while : (Char -> Bool) -> Scan -> (Str, Scan)
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
                            name: ".,".into(),
                            args: vec![Value::Str(env.intern_string(out)), ns],
                        };
                        Ok(pair)
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // take_while1 : (Char -> Bool) -> Scan -> .Some((Str, Scan)) | .None
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
                            Ok(Value::Ctor { name: ".None".into(), args: vec![] })
                        } else {
                            let ns = make_scan(s.clone(), i);
                            let pair = Value::Ctor {
                                name: ".,".into(),
                                args: vec![Value::Str(env.intern_string(out)), ns],
                            };
                            Ok(Value::Ctor { name: ".Some".into(), args: vec![pair] })
                        }
                    }
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        // slice_span : Scan -> Int -> Int -> Str
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
        // unicode/codepoint namespace (Char conversions)
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

        // Tuple/Record constructors used by parser sugar
        // Tuple sugar is now handled via special symbol '.,' directly; keep 'Tuple' for compatibility if needed.
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

    // logical ops: and/or/not. Accept Bool (or legacy Symbol True/False) and return Bool.
        e.vars.insert(
            "and".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (true, true) => Ok(Value::Bool(true)),
                    _ => Ok(Value::Bool(false)),
                },
            },
        );
        e.vars.insert(
            "or".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (false, false) => Ok(Value::Bool(false)),
                    _ => Ok(Value::Bool(true)),
                },
            },
        );
        e.vars.insert(
            "not".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| Ok(Value::Bool(!as_bool(env, &args[0])?)),
            },
        );

        // if : cond then else
        // cond: Bool or Symbol("True"|"False")
        // then/else: either a raw value, a Closure, or a Native with arity=0. Call closures with Unit; return others as-is.
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
                            // Pass Unit to closure via pattern binding
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

        e
    }
}

fn to_str_like(env: &Env, v: &Value) -> String {
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
        Value::Bool(b) => b.to_string(),
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

fn char_literal_string(c: i32) -> String {
    let ch = char::from_u32(c as u32).unwrap_or('\u{FFFD}');
    let mut tmp = String::new();
    tmp.push(ch);
    format!("'{}'", tmp.escape_default())
}

fn as_bool(env: &Env, v: &Value) -> Result<bool, EvalError> {
    let v = force_value(env, v)?;
    match &v {
        Value::Bool(b) => Ok(*b),
        _ => Err(EvalError::TypeError),
    }
}

fn eff_guard(env: &Env) -> Result<(), EvalError> {
    if env.strict_effects && !env.in_effect_context {
        Err(EvalError::EffectNotAllowed)
    } else {
        Ok(())
    }
}

fn eff_print(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
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
            Value::Bool(b) => {
                print!("{}", b);
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
            Value::Native { .. } | Value::Closure { .. } => {
                print!("<fun>");
                Ok(Value::Unit)
            }
            Value::Thunk { .. } => unreachable!(),
            Value::Str(_) => unreachable!(),
        },
    }
}

fn eff_println(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
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
            Value::Bool(b) => {
                println!("{}", b);
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
            Value::Native { .. } | Value::Closure { .. } => {
                println!("<fun>");
                Ok(Value::Unit)
            }
            Value::Thunk { .. } => unreachable!(),
            Value::Str(_) => unreachable!(),
        },
    }
}

fn v_equal(_env: &Env, a: &Value, b: &Value) -> bool {
    // Note: do not force thunks here (equality is assumed to be used after evaluation on typical paths)
    match (a, b) {
        (Value::Unit, Value::Unit) => true,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Ctor { name: na, args: aa }, Value::Ctor { name: nb, args: ab }) => {
            na == nb
                && aa.len() == ab.len()
                && aa.iter().zip(ab.iter()).all(|(x, y)| v_equal(_env, x, y))
        }
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| v_equal(_env, x, y))
        }
        (Value::Tuple(xs), Value::Tuple(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| v_equal(_env, x, y))
        }
        (Value::Record(xm), Value::Record(ym)) => {
            if xm.len() != ym.len() {
                return false;
            }
            xm.iter().zip(ym.iter()).all(|((kx, vx), (ky, vy))| kx == ky && v_equal(_env, vx, vy))
        }
        // Functions are not comparable structurally in this simple eq
        (Value::Native { .. }, Value::Native { .. }) => false,
        (Value::Closure { .. }, Value::Closure { .. }) => false,
        _ => false,
    }
}

fn match_pattern(
    env: &Env,
    p: &Pattern,
    v: &Value,
) -> Option<std::collections::HashMap<String, Value>> {
    let v = force_value(env, v).ok()?;
    use std::collections::HashMap;
    fn merge(
        mut a: HashMap<String, Value>,
        b: HashMap<String, Value>,
    ) -> Option<HashMap<String, Value>> {
        for (k, v) in b {
            if a.contains_key(&k) {
                return None;
            }
            a.insert(k, v);
        }
        Some(a)
    }
    match &p.kind {
        PatternKind::Wildcard => Some(std::collections::HashMap::new()),
        PatternKind::TypeBind { pat, .. } => match_pattern(env, pat, &v),
        PatternKind::Var(n) => {
            let mut m = std::collections::HashMap::new();
            m.insert(n.clone(), v.clone());
            Some(m)
        }
        PatternKind::Unit => match v {
            Value::Unit => Some(std::collections::HashMap::new()),
            _ => None,
        },
        PatternKind::Tuple(ps) => match v {
            Value::Tuple(xs) if xs.len() == ps.len() => {
                let mut acc = std::collections::HashMap::new();
                for (pi, xi) in ps.iter().zip(xs.iter()) {
                    let bi = match_pattern(env, pi, xi)?;
                    acc = merge(acc, bi)?;
                }
                Some(acc)
            }
            _ => None,
        },
        PatternKind::Ctor { name, args } => match &v {
            Value::Ctor { name: n2, args: vs } if n2 == name && vs.len() == args.len() => {
                let mut acc = std::collections::HashMap::new();
                for (pi, xi) in args.iter().zip(vs.iter()) {
                    let bi = match_pattern(env, pi, xi)?;
                    acc = merge(acc, bi)?;
                }
                Some(acc)
            }
            _ => None,
        },
        PatternKind::Symbol(s) => match &v {
            Value::Symbol(id) if env.symbol_name(*id) == s.clone() => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Int(n) => match &v {
            Value::Int(m) if *m == *n => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Float(f) => match &v {
            Value::Float(g) if *g == *f => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Str(st) => match &v {
            Value::Str(s2) if s2.eq_str(st) => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Char(cc) => match &v {
            Value::Char(c2) if *c2 == *cc => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Bool(b) => match &v {
            Value::Bool(c) if *c == *b => Some(HashMap::new()),
            _ => None,
        },
        PatternKind::Record(fields) => match &v {
            Value::Record(map) => {
                // all fields in pattern must exist in value
                let mut acc = HashMap::new();
                for (k, pv) in fields {
                    let vv = map.get(k)?;
                    let bi = match_pattern(env, pv, vv)?;
                    acc = merge(acc, bi)?;
                }
                Some(acc)
            }
            _ => None,
        },
        PatternKind::List(ps) => match &v {
            Value::List(xs) if xs.len() == ps.len() => {
                let mut acc = HashMap::new();
                for (pi, xi) in ps.iter().zip(xs.iter()) {
                    let bi = match_pattern(env, pi, xi)?;
                    acc = merge(acc, bi)?;
                }
                Some(acc)
            }
            _ => None,
        },
        PatternKind::Cons(h, t) => match &v {
            Value::List(xs) if !xs.is_empty() => {
                let mut acc = HashMap::new();
                let bi = match_pattern(env, h, &xs[0])?;
                acc = merge(acc, bi)?;
                let tail = Value::List(xs[1..].to_vec());
                let bj = match_pattern(env, t, &tail)?;
                acc = merge(acc, bj)?;
                Some(acc)
            }
            _ => None,
        },
        PatternKind::As(a, b) => {
            let m1 = match_pattern(env, a, &v)?;
            let m2 = match_pattern(env, b, &v)?;
            let mut acc = HashMap::new();
            for (k, v) in m1 {
                if acc.contains_key(&k) {
                    return None;
                }
                acc.insert(k, v);
            }
            for (k, v) in m2 {
                if acc.contains_key(&k) {
                    return None;
                }
                acc.insert(k, v);
            }
            Some(acc)
        }
    }
}

fn force_value(env: &Env, v: &Value) -> Result<Value, EvalError> {
    match v {
        Value::Thunk { state, kind } => {
            let mut st = state.borrow_mut();
            match &*st {
                ThunkState::Evaluated(val) => return Ok((**val).clone()),
                ThunkState::Evaluating => return Err(EvalError::TypeError), // recursive value
                ThunkState::Unevaluated => {
                    *st = ThunkState::Evaluating;
                }
            }
            drop(st);
            // compute
            let result = match kind {
                ThunkKind::Expr { expr, env: tenv } => {
                    // Evaluate using the shared environment (which may contain recursive bindings)
                    let borrowed = tenv.borrow();
                    eval(&borrowed, expr)?
                }
                ThunkKind::Project { src, pattern, var } => {
                    let base = force_value(env, src)?;
                    let mm = match_pattern(env, pattern, &base).ok_or(EvalError::TypeError)?;
                    mm.get(var).cloned().ok_or(EvalError::TypeError)?
                }
            };
            let mut st2 = state.borrow_mut();
            *st2 = ThunkState::Evaluated(Box::new(result.clone()));
            Ok(result)
        }
        other => Ok(other.clone()),
    }
}

fn apply_value(env: &Env, fval: Value, aval: Value) -> Result<Value, EvalError> {
    // Propagate exceptions (if either side is Raised, return it)
    if let Value::Raised(_) = fval {
        return Ok(fval);
    }
    if let Value::Raised(_) = aval {
        return Ok(aval);
    }
    match fval {
        Value::Record(mut map) => {
            // field access: ({...} .key) only
            let key_opt = match &aval {
                Value::Symbol(id) => {
                    let mut s = env.symbol_name(*id);
                    if let Some(stripped) = s.strip_prefix('.') {
                        s = stripped.to_string();
                    }
                    Some(s)
                }
                _ => None,
            };
            if let Some(k) = key_opt {
                if let Some(v) = map.remove(&k) {
                    Ok(v)
                } else {
                    Err(EvalError::TypeError)
                }
            } else {
                Err(EvalError::TypeError)
            }
        }
        Value::Symbol(id) => {
            let name = env.symbol_name(id);
            // Special internal tuple pack constructor '.,', '.,,', ... accepts any arity; collect progressively.
            if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                Ok(Value::Ctor { name, args: vec![aval] })
            } else {
                if let Some(&k) =
                    env.ctor_arity.get(&name).or_else(|| env.ctor_arity.get(&format!(".{name}")))
                {
                    if k == 0 {
                        return Err(EvalError::TypeError);
                    }
                }
                Ok(Value::Ctor { name, args: vec![aval] })
            }
        }
        Value::Ctor { name, mut args } => {
            args.push(aval);
            if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                // Never enforce arity for tuple-like tags
                Ok(Value::Ctor { name, args })
            } else {
                if let Some(&k) =
                    env.ctor_arity.get(&name).or_else(|| env.ctor_arity.get(&format!(".{name}")))
                {
                    if args.len() > k {
                        return Err(EvalError::TypeError);
                    }
                }
                Ok(Value::Ctor { name, args })
            }
        }
        Value::Native { arity, f, mut args } => {
            args.push(aval);
            if args.len() < arity {
                Ok(Value::Native { arity, f, args })
            } else if args.len() == arity {
                f(env, &args)
            } else {
                let (first_args, rest) = args.split_at(arity);
                let mut res = f(env, first_args)?;
                for v in rest.iter().cloned() {
                    res = apply_value(env, res, v)?;
                }
                Ok(res)
            }
        }
        Value::Closure { param, body, mut env } => {
            if let Some(bind) = match_pattern(&env, &param, &aval) {
                for (k, v) in bind {
                    env.vars.insert(k, v);
                }
                eval(&env, &body)
            } else {
                Ok(Value::Raised(Box::new(aval)))
            }
        }
        _ => Err(EvalError::NotFunc),
    }
}

pub fn eval(env: &Env, e: &Expr) -> Result<Value, EvalError> {
    fn print_type_expr(t: &TypeExpr) -> String {
        match t {
            TypeExpr::Unit => "Unit".into(),
            TypeExpr::Int => "Int".into(),
            TypeExpr::Float => "Float".into(),
            TypeExpr::Bool => "Bool".into(),
            TypeExpr::Str => "Str".into(),
            TypeExpr::Char => "Char".into(),
            TypeExpr::Var(a) => format!("%{}", a),
            TypeExpr::Hole(Some(a)) => format!("?{}", a),
            TypeExpr::Hole(opt) => {
                if let Some(a) = opt {
                    format!("?{}", a)
                } else {
                    "?".into()
                }
            }
            TypeExpr::List(x) => format!("[{}]", print_type_expr(x)),
            TypeExpr::Tuple(xs) => {
                let inner = xs.iter().map(print_type_expr).collect::<Vec<_>>().join(", ");
                format!("({})", inner)
            }
            TypeExpr::Record(fields) => {
                let inner = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_type_expr(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            TypeExpr::Fun(a, b) => format!("{} -> {}", print_type_expr(a), print_type_expr(b)),
            TypeExpr::Ctor { tag, args } => {
                if args.is_empty() {
                    tag.clone()
                } else {
                    format!(
                        "{} {}",
                        tag,
                        args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                    )
                }
            }
        }
    }
    match &e.kind {
        ExprKind::Annot { ty: _, expr } => eval(env, expr),
        ExprKind::TypeVal(ty) => Ok(Value::Str(env.intern_string(print_type_expr(ty)))),
        ExprKind::Unit => Ok(Value::Unit),
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Str(s) => Ok(Value::Str(env.intern_string(s))),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Record(fields) => {
            // Eagerly evaluate field expressions (could later be lazy if desired)
            let mut map = std::collections::BTreeMap::new();
            for (k, v) in fields {
                let val = eval(env, v)?;
                map.insert(k.clone(), val);
            }
            Ok(Value::Record(map))
        }
        ExprKind::LetGroup { bindings, body, .. } => {
            // Support recursion for non-functions via lazy bindings
            // Use a shared environment so RHS thunks see recursive names.
            let shared_env = std::rc::Rc::new(std::cell::RefCell::new(env.clone()));
            // Prepare: create a "whole-value thunk" per binding capturing the shared env
            let mut whole_thunks: Vec<(Pattern, Value)> = Vec::new();
            for (p, ex) in bindings.iter() {
                let state = std::rc::Rc::new(std::cell::RefCell::new(ThunkState::Unevaluated));
                let whole = Value::Thunk {
                    state: state.clone(),
                    kind: ThunkKind::Expr { expr: ex.clone(), env: shared_env.clone() },
                };
                whole_thunks.push((p.clone(), whole));
            }
            // Register variable name -> derived thunk into the shared environment
            fn collect_vars(p: &Pattern, out: &mut Vec<String>) {
                match &p.kind {
                    PatternKind::Wildcard
                    | PatternKind::Unit
                    | PatternKind::Symbol(_)
                    | PatternKind::Int(_)
                    | PatternKind::Float(_)
                    | PatternKind::Str(_)
                    | PatternKind::Char(_)
                    | PatternKind::Bool(_) => {}
                    PatternKind::TypeBind { pat, .. } => {
                        collect_vars(pat, out);
                    }
                    PatternKind::Var(n) => out.push(n.clone()),
                    PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                        for x in xs {
                            collect_vars(x, out);
                        }
                    }
                    PatternKind::Record(fs) => {
                        for (_, v) in fs {
                            collect_vars(v, out);
                        }
                    }
                    PatternKind::Ctor { args, .. } => {
                        for x in args {
                            collect_vars(x, out);
                        }
                    }
                    PatternKind::Cons(h, t) => {
                        collect_vars(h, out);
                        collect_vars(t, out);
                    }
                    PatternKind::As(a, b) => {
                        collect_vars(a, out);
                        collect_vars(b, out);
                    }
                }
            }
            for (p, whole) in whole_thunks.iter() {
                let mut names = Vec::new();
                collect_vars(p, &mut names);
                for n in names {
                    let state = std::rc::Rc::new(std::cell::RefCell::new(ThunkState::Unevaluated));
                    let derived = Value::Thunk {
                        state,
                        kind: ThunkKind::Project {
                            src: Box::new(whole.clone()),
                            pattern: p.clone(),
                            var: n.clone(),
                        },
                    };
                    shared_env.borrow_mut().vars.insert(n, derived);
                }
            }
            // Evaluate the body (forcing thunks as needed) using the shared environment
            let borrowed = shared_env.borrow();
            eval(&borrowed, body)
        }
        ExprKind::List(xs) => {
            let mut out = Vec::with_capacity(xs.len());
            for ex in xs {
                let v = eval(env, ex)?;
                if let Value::Raised(_) = v {
                    return Ok(v);
                }
                out.push(v);
            }
            Ok(Value::List(out))
        }
        ExprKind::Raise(inner) => {
            // Evaluate payload and wrap into Raised value
            let v = eval(env, inner)?;
            Ok(Value::Raised(Box::new(v)))
        }
        ExprKind::Ref(n) => {
            let val = env.vars.get(n).cloned().ok_or_else(|| EvalError::Unbound(n.clone()))?;
            force_value(env, &val)
        }
        ExprKind::Symbol(s) => Ok(Value::Symbol(env.intern_symbol(s))),
        ExprKind::Lambda { param, body } => {
            Ok(Value::Closure { param: param.clone(), body: *body.clone(), env: env.clone() })
        }
        ExprKind::Apply { func, arg } => {
            // Special forms controlling effect-context
            if let ExprKind::Apply { func: seq_ref_expr, arg: a_expr } = &func.kind {
                if let ExprKind::Ref(seq_name) = &seq_ref_expr.kind {
                    if seq_name == "seq" {
                        let _ = eval(env, a_expr)?; // evaluate first argument
                        let mut env2 = env.clone();
                        env2.in_effect_context = true;
                        return eval(&env2, arg);
                    }
                    if seq_name == "chain" {
                        // (~chain a b): evaluate a for effects, then evaluate b in effect-context
                        let _ = eval(env, a_expr)?;
                        let mut env2 = env.clone();
                        env2.in_effect_context = true;
                        return eval(&env2, arg);
                    }
                    if seq_name == "bind" {
                        // (~bind e k): run e, then apply k to its value under effect-context
                        let v = eval(env, a_expr)?;
                        if let Value::Raised(_) = v {
                            return Ok(v);
                        }
                        let mut env2 = env.clone();
                        env2.in_effect_context = true;
                        let k = eval(&env2, arg)?;
                        if let Value::Raised(_) = k {
                            return Ok(k);
                        }
                        return apply_value(&env2, k, v);
                    }
                }
            }

            let f = match eval(env, func) {
                Ok(v) => v,
                Err(err) => {
                    return Err(match err {
                        EvalError::Traced { kind, mut spans } => {
                            spans.push(func.span);
                            EvalError::Traced { kind, spans }
                        }
                        other => {
                            EvalError::Traced { kind: Box::new(other), spans: vec![func.span] }
                        }
                    })
                }
            };
            // If func evaluates to a Raised, propagate as-is (attempting to apply a raised value raises the whole)
            if let Value::Raised(_) = f {
                return Ok(f);
            }
            let a = match eval(env, arg) {
                Ok(v) => v,
                Err(err) => {
                    return Err(match err {
                        EvalError::Traced { kind, mut spans } => {
                            spans.push(arg.span);
                            EvalError::Traced { kind, spans }
                        }
                        other => EvalError::Traced { kind: Box::new(other), spans: vec![arg.span] },
                    })
                }
            };
            if let Value::Raised(_) = a {
                return Ok(a);
            }
            match f {
                Value::Record(mut map) => {
                    let key_opt = match &a {
                        Value::Symbol(id) => {
                            let mut s = env.symbol_name(*id);
                            if let Some(stripped) = s.strip_prefix('.') {
                                s = stripped.to_string();
                            }
                            Some(s)
                        }
                        _ => None,
                    };
                    if let Some(k) = key_opt {
                        if let Some(v) = map.remove(&k) {
                            Ok(v)
                        } else {
                            Err(EvalError::Traced {
                                kind: Box::new(EvalError::TypeError),
                                spans: vec![e.span],
                            })
                        }
                    } else {
                        Err(EvalError::TypeError)
                    }
                }
                // Constructor variable: build constructor value accumulating payload args
                Value::Symbol(id) => {
                    let name = env.symbol_name(id);
                    // At the first application we cannot check max arity yet; if declared arity is 0, error
                    if let Some(&k) = env
                        .ctor_arity
                        .get(&name)
                        .or_else(|| env.ctor_arity.get(&format!(".{name}")))
                    {
                        if k == 0 {
                            return Err(EvalError::Traced {
                                kind: Box::new(EvalError::TypeError),
                                spans: vec![e.span],
                            });
                        }
                    }
                    Ok(Value::Ctor { name, args: vec![a] })
                }
                Value::Ctor { name, mut args } => {
                    args.push(a);
                    if let Some(&k) = env
                        .ctor_arity
                        .get(&name)
                        .or_else(|| env.ctor_arity.get(&format!(".{name}")))
                    {
                        if args.len() > k {
                            return Err(EvalError::TypeError);
                        }
                        // If just satisfied, return as value; if insufficient, keep a partial value
                    }
                    Ok(Value::Ctor { name, args })
                }
                Value::Native { arity, f, mut args } => {
                    args.push(a);
                    if args.len() < arity {
                        Ok(Value::Native { arity, f, args })
                    } else if args.len() == arity {
                        match f(env, &args) {
                            Ok(v) => Ok(v),
                            Err(err) => Err(match err {
                                EvalError::Traced { .. } => err,
                                other => {
                                    EvalError::Traced { kind: Box::new(other), spans: vec![e.span] }
                                }
                            }),
                        }
                    } else {
                        // over-application: apply result to remaining args
                        let (first_args, rest) = args.split_at(arity);
                        let mut res = match f(env, first_args) {
                            Ok(v) => v,
                            Err(err) => {
                                return Err(match err {
                                    EvalError::Traced { .. } => err,
                                    other => EvalError::Traced {
                                        kind: Box::new(other),
                                        spans: vec![e.span],
                                    },
                                })
                            }
                        };
                        for v in rest.iter().cloned() {
                            res = match res {
                                Value::Native { arity, f, mut args } => {
                                    args.push(v);
                                    if args.len() == arity {
                                        match f(env, &args) {
                                            Ok(x) => x,
                                            Err(err) => {
                                                return Err(match err {
                                                    EvalError::Traced { .. } => err,
                                                    other => EvalError::Traced {
                                                        kind: Box::new(other),
                                                        spans: vec![e.span],
                                                    },
                                                })
                                            }
                                        }
                                    } else {
                                        Value::Native { arity, f, args }
                                    }
                                }
                                Value::Closure { param, body, mut env } => {
                                    if let Some(bind) = match_pattern(&env, &param, &v) {
                                        for (k, vv) in bind {
                                            env.vars.insert(k, vv);
                                        }
                                        match eval(&env, &body) {
                                            Ok(x) => x,
                                            Err(err) => {
                                                return Err(match err {
                                                    EvalError::Traced { kind, mut spans } => {
                                                        spans.push(body.span);
                                                        EvalError::Traced { kind, spans }
                                                    }
                                                    other => EvalError::Traced {
                                                        kind: Box::new(other),
                                                        spans: vec![body.span],
                                                    },
                                                })
                                            }
                                        }
                                    } else {
                                        Value::Raised(Box::new(v))
                                    }
                                }
                                _ => return Err(EvalError::NotFunc),
                            };
                        }
                        Ok(res)
                    }
                }
                Value::Closure { param, body, mut env } => {
                    if let Some(bind) = match_pattern(&env, &param, &a) {
                        for (k, v) in bind {
                            env.vars.insert(k, v);
                        }
                        match eval(&env, &body) {
                            Ok(v) => Ok(v),
                            Err(err) => Err(match err {
                                EvalError::Traced { kind, mut spans } => {
                                    spans.push(body.span);
                                    EvalError::Traced { kind, spans }
                                }
                                other => EvalError::Traced {
                                    kind: Box::new(other),
                                    spans: vec![body.span],
                                },
                            }),
                        }
                    } else {
                        Ok(Value::Raised(Box::new(a)))
                    }
                }
                _ => Err(EvalError::NotFunc),
            }
        }
        ExprKind::Block(inner) => eval(env, inner),
        ExprKind::AltLambda { left, right } => {
            // Desugar to closure: \x -> (~alt left right x)
            let param =
                Pattern::new(PatternKind::Var("x".into()), lzscr_ast::span::Span::new(0, 0));
            let alt_ref = Expr::new(ExprKind::Ref("alt".into()), lzscr_ast::span::Span::new(0, 0));
            let app1 = Expr::new(
                ExprKind::Apply { func: Box::new(alt_ref), arg: left.clone() },
                lzscr_ast::span::Span::new(0, 0),
            );
            let app2 = Expr::new(
                ExprKind::Apply { func: Box::new(app1), arg: right.clone() },
                lzscr_ast::span::Span::new(0, 0),
            );
            let x_ref = Expr::new(ExprKind::Ref("x".into()), lzscr_ast::span::Span::new(0, 0));
            let body = Expr::new(
                ExprKind::Apply { func: Box::new(app2), arg: Box::new(x_ref) },
                lzscr_ast::span::Span::new(0, 0),
            );
            Ok(Value::Closure { param, body, env: env.clone() })
        }
        ExprKind::OrElse { left, right } => {
            // Spec: discard the result of LHS and return the result of RHS
            // Discard LHS even if it is Raised; if RHS is Raised, the whole is Raised
            let _ = eval(env, left)?;
            eval(env, right)
        }
        ExprKind::Catch { left, right } => {
            let lv = eval(env, left)?;
            match lv {
                Value::Raised(payload) => {
                    let rv = eval(env, right)?;
                    apply_value(env, rv, *payload)
                }
                other => Ok(other),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
            Value::Symbol(id) => assert_eq!(env.symbol_name(id), "True"),
            _ => panic!("expected Symbol True"),
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
            Value::Symbol(id) => assert_eq!(env2.symbol_name(id), "False"),
            _ => panic!("expected Symbol False"),
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
        // ((\(.Foo x y) -> ~x) (.Foo 1 2)) == 1
        let pat = Pattern {
            kind: PatternKind::Ctor {
                name: ".Foo".into(),
                args: vec![
                    Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) },
                    Pattern { kind: PatternKind::Var("y".into()), span: Span::new(0, 0) },
                ],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(ExprKind::Lambda { param: pat, body: Box::new(body) }, Span::new(0, 0));
        // .Foo 1 2
        let foo = Expr::new(ExprKind::Symbol(".Foo".into()), Span::new(0, 0));
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
        // ((\(.Foo x) -> ~x) (.Bar 1)) => ^(.Bar(1))
        let pat = Pattern {
            kind: PatternKind::Ctor {
                name: ".Foo".into(),
                args: vec![Pattern { kind: PatternKind::Var("x".into()), span: Span::new(0, 0) }],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(ExprKind::Lambda { param: pat, body: Box::new(body) }, Span::new(0, 0));
        let bar = Expr::new(ExprKind::Symbol(".Bar".into()), Span::new(0, 0));
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
                    assert_eq!(name, ".Bar");
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
