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
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Str(String),
    Native {
        arity: usize,
        f: fn(&Env, &[Value]) -> Result<Value, EvalError>,
        args: Vec<Value>,
    },
    Closure {
        param: String,
        body: Expr,
        env: Env,
    },
    Symbol(String),
}

#[derive(Debug, Clone, Default)]
pub struct Env {
    pub vars: std::collections::HashMap<String, Value>,
    pub strict_effects: bool,
    pub in_effect_context: bool,
}

// Default is derived

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_builtins() -> Self {
        let mut e = Env::new();

        // to_str : a -> Str
        e.vars.insert(
            "to_str".into(),
            Value::Native {
                arity: 1,
                args: vec![],
                f: |_env, args| {
                    let v = &args[0];
                    Ok(Value::Str(match v.clone() {
                        Value::Unit => "()".into(),
                        Value::Int(n) => n.to_string(),
                        Value::Str(s) => s,
                        Value::Symbol(s) => s,
                        Value::Native { .. } | Value::Closure { .. } => "<fun>".into(),
                    }))
                },
            },
        );

        // add : Int -> Int -> Int
        e.vars.insert(
            "add".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| {
                    if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
                        Ok(Value::Int(a + b))
                    } else {
                        Err(EvalError::TypeError)
                    }
                },
            },
        );

        // sub : Int -> Int -> Int
        e.vars.insert(
            "sub".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| {
                    if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
                        Ok(Value::Int(a - b))
                    } else {
                        Err(EvalError::TypeError)
                    }
                },
            },
        );

        // eq : Int|Str|Unit|Symbol -> same -> Symbol("True"|"False")
        e.vars.insert(
            "eq".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| {
                    let res = match (&args[0], &args[1]) {
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Str(a), Value::Str(b)) => a == b,
                        (Value::Unit, Value::Unit) => true,
                        (Value::Symbol(a), Value::Symbol(b)) => a == b,
                        _ => false,
                    };
                    Ok(Value::Symbol(if res {
                        "True".into()
                    } else {
                        "False".into()
                    }))
                },
            },
        );

        // lt : Int -> Int -> Symbol("True"|"False")
        e.vars.insert(
            "lt".into(),
            Value::Native {
                arity: 2,
                args: vec![],
                f: |_env, args| {
                    if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
                        Ok(Value::Symbol(if a < b {
                            "True".into()
                        } else {
                            "False".into()
                        }))
                    } else {
                        Err(EvalError::TypeError)
                    }
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
                f: |_env, args| {
                    let sym = match &args[0] {
                        Value::Symbol(s) => s.clone(),
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

        e
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
    match &args[0] {
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
            Value::Symbol(sym) => {
                print!("{}", sym);
                Ok(Value::Unit)
            }
            Value::Native { .. } | Value::Closure { .. } => {
                print!("<fun>");
                Ok(Value::Unit)
            }
            Value::Str(_) => unreachable!(),
        },
    }
}

fn eff_println(env: &Env, args: &[Value]) -> Result<Value, EvalError> {
    eff_guard(env)?;
    match &args[0] {
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
            Value::Symbol(sym) => {
                println!("{}", sym);
                Ok(Value::Unit)
            }
            Value::Native { .. } | Value::Closure { .. } => {
                println!("<fun>");
                Ok(Value::Unit)
            }
            Value::Str(_) => unreachable!(),
        },
    }
}

pub fn eval(env: &Env, e: &Expr) -> Result<Value, EvalError> {
    match &e.kind {
        ExprKind::Unit => Ok(Value::Unit),
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Ref(n) => env
            .vars
            .get(n)
            .cloned()
            .ok_or_else(|| EvalError::Unbound(n.clone())),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
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
            let a = eval(env, arg)?;
            match f {
                // Constructor variable: accumulate args as an unapplied function-like value
                Value::Symbol(_name) => {
                    let args = vec![a];
                    Ok(Value::Native { arity: usize::MAX, f: |_env, _args| Err(EvalError::NotFunc), args })
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
                                    env.vars.insert(param, v);
                                    eval(&env, &body)?
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
                    env.vars.insert(param, a);
                    eval(&env, &body)
                }
                _ => Err(EvalError::NotFunc),
            }
        }
        ExprKind::Block(inner) => eval(env, inner),
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
                param: "x".into(),
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
}
