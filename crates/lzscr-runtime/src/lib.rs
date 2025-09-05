use lzscr_ast::ast::*;

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
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    // Simple immutable containers (PoC)
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Record(std::collections::BTreeMap<String, Value>),
    // Constructor value: tag name with payload args (order-significant)
    Ctor {
        name: String,
        args: Vec<Value>,
    },
    Native {
        arity: usize,
        f: fn(&Env, &[Value]) -> Result<Value, EvalError>,
        args: Vec<Value>,
    },
    Closure {
        param: Pattern,
        body: Expr,
        env: Env,
    },
    Symbol(u32),
    // Exception payload (internal). Represent ^(Expr) after evaluation as a value that propagates.
    Raised(Box<Value>),
    // Lazy thunk for recursive/non-function let bindings
    Thunk {
        state: std::rc::Rc<std::cell::RefCell<ThunkState>>,
        kind: ThunkKind,
    },
}

#[derive(Debug, Clone)]
pub enum ThunkKind {
    Expr {
        expr: Expr,
        env: Env,
    },
    Project {
        src: Box<Value>,
        pattern: Pattern,
        var: String,
    },
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
        self.sym_rev
            .borrow()
            .get(id as usize)
            .cloned()
            .unwrap_or("<sym?>".to_string())
    }

    pub fn declare_ctor_arity(&mut self, name: &str, arity: usize) {
        // 登録名はドット有無の両方を受け付ける（検索時も両方試す）
        let n = name.to_string();
        self.ctor_arity.insert(n.clone(), arity);
        if n.starts_with('.') {
            self.ctor_arity
                .insert(n.trim_start_matches('.').to_string(), arity);
        } else {
            self.ctor_arity.insert(format!(".{n}"), arity);
        }
    }

    pub fn with_builtins() -> Self {
        let mut e = Env::new();

        // Inject ~true / ~false as references
        e.vars.insert("true".into(), Value::Bool(true));
        e.vars.insert("false".into(), Value::Bool(false));
        e.declare_ctor_arity(".true", 0);
        e.declare_ctor_arity(".false", 0);
        // Pre-intern commonly used symbols
        let _ = e.intern_symbol("True");
        let _ = e.intern_symbol("False");
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
                    Value::Symbol(id) if env.symbol_name(*id) == ".true" => Ok(Value::Bool(true)),
                    Value::Symbol(id) if env.symbol_name(*id) == ".false" => Ok(Value::Bool(false)),
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
                    Ok(Value::Str(to_str_like(env, v)))
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

        // div : Int -> Int -> Int (0 除算はエラー)
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
        // fdiv : Float -> Float -> Float (0.0 除算はエラー)
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
                    Ok(if res { sym_true(env) } else { sym_false(env) })
                },
            },
        );

        // lt : Int|Float -> Int|Float -> Symbol("True"|"False")
        e.vars.insert(
            "lt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(if a < b { sym_true(env) } else { sym_false(env) })
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(if a < b { sym_true(env) } else { sym_false(env) })
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
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(if a <= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
                    (Value::Float(a), Value::Float(b)) => Ok(if a <= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
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
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        Ok(if a > b { sym_true(env) } else { sym_false(env) })
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(if a > b { sym_true(env) } else { sym_false(env) })
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
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(if a >= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
                    (Value::Float(a), Value::Float(b)) => Ok(if a >= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // ne : a -> a -> Bool (構造的不等価)
        e.vars.insert(
            "ne".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| {
                    let res = !v_equal(env, &args[0], &args[1]);
                    Ok(if res { sym_true(env) } else { sym_false(env) })
                },
            },
        );

        // Float-only comparisons: flt/fle/fgt/fge
        e.vars.insert(
            "flt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(if a < b { sym_true(env) } else { sym_false(env) })
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
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(if a <= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
                    _ => Err(EvalError::TypeError),
                },
            },
        );
        e.vars.insert(
            "fgt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(if a > b { sym_true(env) } else { sym_false(env) })
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
                f: |env, args| match (&args[0], &args[1]) {
                    (Value::Float(a), Value::Float(b)) => Ok(if a >= b {
                        sym_true(env)
                    } else {
                        sym_false(env)
                    }),
                    _ => Err(EvalError::TypeError),
                },
            },
        );

        // seq は特別扱いで評価順を制御するため、Native としては不要だが参照解決のために登録はしておく
        e.vars.insert(
            "seq".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| Ok(args[1].clone()),
            },
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
                    Ok(Value::Native {
                        arity: 1,
                        args: vec![],
                        f,
                    })
                },
            },
        );

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
                        let mut map = std::collections::BTreeMap::new();
                        for kv in kvs {
                            match kv {
                                Value::Tuple(xs) if xs.len() == 2 => {
                                    if let Value::Str(k) = &xs[0] {
                                        map.insert(k.clone(), xs[1].clone());
                                    } else {
                                        return Err(EvalError::TypeError);
                                    }
                                }
                                _ => return Err(EvalError::TypeError),
                            }
                        }
                        Ok(Value::Record(map))
                    }
                    Value::Ctor { name, args } if name == ".," => {
                        let mut map = std::collections::BTreeMap::new();
                        for kv in args {
                            match kv {
                                Value::Tuple(xs) if xs.len() == 2 => {
                                    if let Value::Str(k) = &xs[0] {
                                        map.insert(k.clone(), xs[1].clone());
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

        // logical ops: and/or/not. Accept Bool or Symbol("True"|"False"). Return Symbol("True"|"False").
        e.vars.insert(
            "and".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (true, true) => Ok(sym_true(env)),
                    _ => Ok(sym_false(env)),
                },
            },
        );
        e.vars.insert(
            "or".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |env, args| match (as_bool(env, &args[0])?, as_bool(env, &args[1])?) {
                    (false, false) => Ok(sym_false(env)),
                    _ => Ok(sym_true(env)),
                },
            },
        );
        e.vars.insert(
            "not".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |env, args| {
                    Ok(if as_bool(env, &args[0])? {
                        sym_false(env)
                    } else {
                        sym_true(env)
                    })
                },
            },
        );

        // if : cond then else
        // cond: Bool or Symbol("True"|"False")
        // then/else: 値そのもの、Closure、または arity=0 の Native。Closure は Unit を渡して呼び出し、その他はそのまま返す。
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
                        Value::Closure {
                            param,
                            body,
                            mut env,
                        } => {
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
    // サンクはここで可能なら強制してから表示する
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
        Value::Str(s) => s.clone(),
        Value::Symbol(id) => env.symbol_name(*id),
        Value::Raised(b) => format!("^({})", to_str_like(env, b)),
        Value::Ctor { name, args } => {
            if name == ".," {
                // print as tuple literal
                format!(
                    "({})",
                    args.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            } else if args.is_empty() {
                name.clone()
            } else {
                format!(
                    "{}({})",
                    name,
                    args.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        Value::List(xs) => format!(
            "[{}]",
            xs.iter()
                .map(|x| to_str_like(env, x))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Tuple(xs) => format!(
            "({})",
            xs.iter()
                .map(|x| to_str_like(env, x))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Record(map) => {
            let inner = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, to_str_like(env, v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", inner)
        }
        Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
        // Thunk は上で force 済みのはずだが、安全のため
        _ => "<thunk>".into(),
    }
}

fn sym_true(env: &Env) -> Value {
    Value::Symbol(env.intern_symbol("True"))
}
fn sym_false(env: &Env) -> Value {
    Value::Symbol(env.intern_symbol("False"))
}

fn as_bool(env: &Env, v: &Value) -> Result<bool, EvalError> {
    let v = force_value(env, v)?;
    match &v {
        Value::Bool(b) => Ok(*b),
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
            Value::Symbol(id) => {
                print!("{}", env.symbol_name(id));
                Ok(Value::Unit)
            }
            Value::Raised(b) => {
                print!("^({})", to_str_like(env, &b));
                Ok(Value::Unit)
            }
            Value::Ctor { name, args } => {
                if name == ".," {
                    print!(
                        "({})",
                        args.iter()
                            .map(|x| to_str_like(env, x))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                } else if args.is_empty() {
                    print!("{}", name);
                } else {
                    print!(
                        "{}({})",
                        name,
                        args.iter()
                            .map(|x| to_str_like(env, x))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                Ok(Value::Unit)
            }
            Value::List(xs) => {
                print!(
                    "[{}]",
                    xs.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Tuple(xs) => {
                print!(
                    "({})",
                    xs.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
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
            Value::Symbol(id) => {
                println!("{}", env.symbol_name(id));
                Ok(Value::Unit)
            }
            Value::Raised(b) => {
                println!("^({})", to_str_like(env, &b));
                Ok(Value::Unit)
            }
            Value::Ctor { name, args } => {
                if name == ".," {
                    println!(
                        "({})",
                        args.iter()
                            .map(|x| to_str_like(env, x))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                } else if args.is_empty() {
                    println!("{}", name);
                } else {
                    println!(
                        "{}({})",
                        name,
                        args.iter()
                            .map(|x| to_str_like(env, x))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                Ok(Value::Unit)
            }
            Value::List(xs) => {
                println!(
                    "[{}]",
                    xs.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                Ok(Value::Unit)
            }
            Value::Tuple(xs) => {
                println!(
                    "({})",
                    xs.iter()
                        .map(|x| to_str_like(env, x))
                        .collect::<Vec<_>>()
                        .join(", ")
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
    // 注意: ここではサンクを強制しない（等価性は実質的に評価後に比較されるパスで使われる想定）
    match (a, b) {
        (Value::Unit, Value::Unit) => true,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
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
            xm.iter()
                .zip(ym.iter())
                .all(|((kx, vx), (ky, vy))| kx == ky && v_equal(_env, vx, vy))
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
            Value::Str(s2) if s2 == st => Some(HashMap::new()),
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
                ThunkKind::Expr { expr, env: tenv } => eval(tenv, expr)?,
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
    // 例外伝播（どちらかが Raised ならそのまま）
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
            // Special internal tuple pack constructor '.,' accepts any arity; materialize to Tuple progressively.
            if name == ".," {
                // First application: (., a) => Ctor { name='.,', args=[a] }
                // Subsequent applications collect and eventually will be printed as Tuple-like.
                Ok(Value::Ctor {
                    name,
                    args: vec![aval],
                })
            } else {
                if let Some(&k) = env
                    .ctor_arity
                    .get(&name)
                    .or_else(|| env.ctor_arity.get(&format!(".{name}")))
                {
                    if k == 0 {
                        return Err(EvalError::TypeError);
                    }
                }
                Ok(Value::Ctor {
                    name,
                    args: vec![aval],
                })
            }
        }
        Value::Ctor { name, mut args } => {
            args.push(aval);
            if name == ".," {
                // Never enforce arity for '.,'
                Ok(Value::Ctor { name, args })
            } else {
                if let Some(&k) = env
                    .ctor_arity
                    .get(&name)
                    .or_else(|| env.ctor_arity.get(&format!(".{name}")))
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
        Value::Closure {
            param,
            body,
            mut env,
        } => {
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
    match &e.kind {
        ExprKind::Unit => Ok(Value::Unit),
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::LetGroup { bindings, body } => {
            // 遅延束縛により非関数の再帰もサポート
            let mut env2 = env.clone();
            // 準備: 各バインディングごとに「全体値のサンク」を作る
            let mut whole_thunks: Vec<(Pattern, Value)> = Vec::new();
            for (p, ex) in bindings.iter() {
                let state = std::rc::Rc::new(std::cell::RefCell::new(ThunkState::Unevaluated));
                let whole = Value::Thunk {
                    state: state.clone(),
                    kind: ThunkKind::Expr {
                        expr: ex.clone(),
                        env: env2.clone(),
                    },
                };
                whole_thunks.push((p.clone(), whole));
            }
            // 変数名→派生サンクを環境に登録
            fn collect_vars(p: &Pattern, out: &mut Vec<String>) {
                match &p.kind {
                    PatternKind::Wildcard
                    | PatternKind::Unit
                    | PatternKind::Symbol(_)
                    | PatternKind::Int(_)
                    | PatternKind::Float(_)
                    | PatternKind::Str(_)
                    | PatternKind::Bool(_) => {}
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
                    env2.vars.insert(n, derived);
                }
            }
            // 本体を評価（必要に応じてサンクが強制される）
            eval(&env2, body)
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
            let val = env
                .vars
                .get(n)
                .cloned()
                .ok_or_else(|| EvalError::Unbound(n.clone()))?;
            force_value(env, &val)
        }
        ExprKind::Symbol(s) => Ok(Value::Symbol(env.intern_symbol(s))),
        ExprKind::Lambda { param, body } => Ok(Value::Closure {
            param: param.clone(),
            body: *body.clone(),
            env: env.clone(),
        }),
        ExprKind::Apply { func, arg } => {
            // Special form: (~seq a b) controls effect-context for b
            if let ExprKind::Apply {
                func: seq_ref_expr,
                arg: a_expr,
            } = &func.kind
            {
                if let ExprKind::Ref(seq_name) = &seq_ref_expr.kind {
                    if seq_name == "seq" {
                        let _ = eval(env, a_expr)?; // evaluate first argument
                        let mut env2 = env.clone();
                        env2.in_effect_context = true;
                        return eval(&env2, arg);
                    }
                }
            }

            let f = eval(env, func)?;
            // If func evaluates to a Raised, propagate as-is (attempting to apply a raised value raises the whole)
            if let Value::Raised(_) = f {
                return Ok(f);
            }
            let a = eval(env, arg)?;
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
                            Err(EvalError::TypeError)
                        }
                    } else {
                        Err(EvalError::TypeError)
                    }
                }
                // Constructor variable: build constructor value accumulating payload args
                Value::Symbol(id) => {
                    let name = env.symbol_name(id);
                    // 1 引数目適用時点では最大 arity チェックはできないが、0 アリティならエラー
                    if let Some(&k) = env
                        .ctor_arity
                        .get(&name)
                        .or_else(|| env.ctor_arity.get(&format!(".{name}")))
                    {
                        if k == 0 {
                            return Err(EvalError::TypeError);
                        }
                    }
                    Ok(Value::Ctor {
                        name,
                        args: vec![a],
                    })
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
                        // ちょうど満たした場合はそのまま返す（値）。不足時は未完成値。
                    }
                    Ok(Value::Ctor { name, args })
                }
                Value::Native { arity, f, mut args } => {
                    args.push(a);
                    if args.len() < arity {
                        Ok(Value::Native { arity, f, args })
                    } else if args.len() == arity {
                        f(env, &args)
                    } else {
                        // over-application: apply result to remaining args
                        let (first_args, rest) = args.split_at(arity);
                        let mut res = f(env, first_args)?;
                        for v in rest.iter().cloned() {
                            res = match res {
                                Value::Native { arity, f, mut args } => {
                                    args.push(v);
                                    if args.len() == arity {
                                        f(env, &args)?
                                    } else {
                                        Value::Native { arity, f, args }
                                    }
                                }
                                Value::Closure {
                                    param,
                                    body,
                                    mut env,
                                } => {
                                    if let Some(bind) = match_pattern(&env, &param, &v) {
                                        for (k, vv) in bind {
                                            env.vars.insert(k, vv);
                                        }
                                        eval(&env, &body)?
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
                Value::Closure {
                    param,
                    body,
                    mut env,
                } => {
                    if let Some(bind) = match_pattern(&env, &param, &a) {
                        for (k, v) in bind {
                            env.vars.insert(k, v);
                        }
                        eval(&env, &body)
                    } else {
                        Ok(Value::Raised(Box::new(a)))
                    }
                }
                _ => Err(EvalError::NotFunc),
            }
        }
        ExprKind::Block(inner) => eval(env, inner),
        ExprKind::OrElse { left, right } => {
            // 指定仕様: LHS の評価結果は捨て、RHS の評価結果を返す。
            // LHS が Raised でも捨てる。RHS が Raised なら全体が Raised。
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
            ExprKind::Apply {
                func: Box::new(foo_sym.clone()),
                arg: Box::new(int_expr(1)),
            },
            Span::new(0, 0),
        );
        let foo12 = Expr::new(
            ExprKind::Apply {
                func: Box::new(foo1),
                arg: Box::new(int_expr(2)),
            },
            Span::new(0, 0),
        );

        let left_app = Expr::new(
            ExprKind::Apply {
                func: Box::new(eq_ref),
                arg: Box::new(foo12.clone()),
            },
            Span::new(0, 0),
        );
        let whole = Expr::new(
            ExprKind::Apply {
                func: Box::new(left_app),
                arg: Box::new(foo12),
            },
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
            ExprKind::Apply {
                func: Box::new(bar_sym),
                arg: Box::new(int_expr(1)),
            },
            Span::new(0, 0),
        );
        let left_app = Expr::new(
            ExprKind::Apply {
                func: Box::new(ref_expr("eq")),
                arg: Box::new(bar1),
            },
            Span::new(0, 0),
        );
        let whole = Expr::new(
            ExprKind::Apply {
                func: Box::new(left_app),
                arg: Box::new(sym_expr("Foo")),
            },
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
        let x_pat = Pattern {
            kind: PatternKind::Var("x".into()),
            span: Span::new(0, 0),
        };
        let y_pat = Pattern {
            kind: PatternKind::Var("y".into()),
            span: Span::new(0, 0),
        };
        let e = Expr::new(
            ExprKind::LetGroup {
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
            ExprKind::Apply {
                func: Box::new(add_ref),
                arg: Box::new(one),
            },
            Span::new(0, 0),
        );
        let appl2 = Expr::new(
            ExprKind::Apply {
                func: Box::new(appl1),
                arg: Box::new(two),
            },
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
                param: Pattern {
                    kind: PatternKind::Var("x".into()),
                    span: Span::new(0, 0),
                },
                body: Box::new(body),
            },
            Span::new(0, 0),
        );
        let forty_two = int_expr(42);
        let appl = Expr::new(
            ExprKind::Apply {
                func: Box::new(lam),
                arg: Box::new(forty_two),
            },
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
            ExprKind::Apply {
                func: Box::new(to_str),
                arg: Box::new(five),
            },
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
                    Pattern {
                        kind: PatternKind::Var("x".into()),
                        span: Span::new(0, 0),
                    },
                    Pattern {
                        kind: PatternKind::Var("y".into()),
                        span: Span::new(0, 0),
                    },
                ],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(
            ExprKind::Lambda {
                param: pat,
                body: Box::new(body),
            },
            Span::new(0, 0),
        );
        // Foo 1 2
        let foo = Expr::new(ExprKind::Symbol("Foo".into()), Span::new(0, 0));
        let a1 = Expr::new(
            ExprKind::Apply {
                func: Box::new(foo),
                arg: Box::new(int_expr(1)),
            },
            Span::new(0, 0),
        );
        let foo12 = Expr::new(
            ExprKind::Apply {
                func: Box::new(a1),
                arg: Box::new(int_expr(2)),
            },
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply {
                func: Box::new(lam),
                arg: Box::new(foo12),
            },
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
                args: vec![Pattern {
                    kind: PatternKind::Var("x".into()),
                    span: Span::new(0, 0),
                }],
            },
            span: Span::new(0, 0),
        };
        let body = Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0));
        let lam = Expr::new(
            ExprKind::Lambda {
                param: pat,
                body: Box::new(body),
            },
            Span::new(0, 0),
        );
        let bar = Expr::new(ExprKind::Symbol("Bar".into()), Span::new(0, 0));
        let bar1 = Expr::new(
            ExprKind::Apply {
                func: Box::new(bar),
                arg: Box::new(int_expr(1)),
            },
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply {
                func: Box::new(lam),
                arg: Box::new(bar1),
            },
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
            ExprKind::Apply {
                func: Box::new(eff),
                arg: Box::new(str_expr("x")),
            },
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
                        Pattern {
                            kind: PatternKind::Var("x".into()),
                            span: Span::new(0, 0),
                        },
                        Pattern {
                            kind: PatternKind::Var("y".into()),
                            span: Span::new(0, 0),
                        },
                    ]),
                    span: Span::new(0, 0),
                },
                body: Box::new(Expr::new(ExprKind::Ref("x".into()), Span::new(0, 0))),
            },
            Span::new(0, 0),
        );
        let arg = Expr::new(
            ExprKind::List(vec![int_expr(10), int_expr(20)]),
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply {
                func: Box::new(lam),
                arg: Box::new(arg),
            },
            Span::new(0, 0),
        );
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
        let arg = Expr::new(
            ExprKind::List(vec![int_expr(7), int_expr(8), int_expr(9)]),
            Span::new(0, 0),
        );
        let appl = Expr::new(
            ExprKind::Apply {
                func: Box::new(lam),
                arg: Box::new(arg),
            },
            Span::new(0, 0),
        );
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
            ExprKind::Apply {
                func: Box::new(eff),
                arg: Box::new(str_expr("x")),
            },
            Span::new(0, 0),
        );
        let env = Env::with_builtins();
        let mut env_strict = env.clone();
        env_strict.strict_effects = true;
        let err = eval(&env_strict, &call).unwrap_err();
        match err {
            EvalError::EffectNotAllowed => {}
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
            ExprKind::Apply {
                func: Box::new(eff),
                arg: Box::new(str_expr("x")),
            },
            Span::new(0, 0),
        );
        let first = Expr::new(
            ExprKind::Apply {
                func: Box::new(seq_ref),
                arg: Box::new(unit),
            },
            Span::new(0, 0),
        );
        let expr = Expr::new(
            ExprKind::Apply {
                func: Box::new(first),
                arg: Box::new(call),
            },
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
        let e = Expr::new(
            ExprKind::List(vec![int_expr(1), int_expr(2), int_expr(3)]),
            Span::new(0, 0),
        );
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
            ExprKind::Apply {
                func: Box::new(cons_ref),
                arg: Box::new(int_expr(1)),
            },
            Span::new(0, 0),
        );
        let list = Expr::new(
            ExprKind::List(vec![int_expr(2), int_expr(3)]),
            Span::new(0, 0),
        );
        let whole = Expr::new(
            ExprKind::Apply {
                func: Box::new(app1),
                arg: Box::new(list),
            },
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
