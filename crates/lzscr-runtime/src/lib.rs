use lzscr_ast::ast::*;

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("not a function")] NotFunc,
    #[error("unbound ref: {0}")] Unbound(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Str(String),
    Native { arity: usize, f: fn(&[Value]) -> Result<Value, EvalError>, args: Vec<Value> },
    Closure { param: String, body: Expr, env: Env },
    Symbol(String),
}

#[derive(Debug, Clone, Default)]
pub struct Env(std::collections::HashMap<String, Value>);

impl Env {
    pub fn new() -> Self { Self::default() }
    pub fn with_builtins() -> Self {
        let mut e = Env::new();
        e.0.insert("to_str".into(), Value::Native { arity: 1, args: vec![], f: |args| {
            let v = &args[0];
            Ok(Value::Str(match v.clone() {
                Value::Unit => "()".into(),
                Value::Int(n) => n.to_string(),
                Value::Str(s) => s,
                Value::Symbol(s) => s,
                Value::Native{..} | Value::Closure{..} => "<fun>".into(),
            }))
        }});
        e.0.insert("add".into(), Value::Native { arity: 2, args: vec![], f: |args| {
            if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) { Ok(Value::Int(a + b)) } else { Err(EvalError::NotFunc) }
        }});
        e
    }
}

pub fn eval(env: &Env, e: &Expr) -> Result<Value, EvalError> {
    match &e.kind {
        ExprKind::Unit => Ok(Value::Unit),
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Ref(n) => env.0.get(n).cloned().ok_or_else(|| EvalError::Unbound(n.clone())),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::Lambda { param, body } => Ok(Value::Closure { param: param.clone(), body: *body.clone(), env: env.clone() }),
        ExprKind::Apply { func, arg } => {
            let f = eval(env, func)?;
            let a = eval(env, arg)?;
            match f {
                // 裸のシンボルはコンストラクタ変数: 最初の適用で (s a) という「構成中の値」になる。
                // ここでは簡易に Native(arity=usize::MAX) に積む形でモデル化し、完全化はユーザ定義や型解決後に扱う前提。
                Value::Symbol(_name) => {
                    // コンストラクタ変数の適用はシンボルと引数列のペアをシンボル値で表現するのが素直だが、
                    // ここでは暫定で args を保持する Native で表現し、さらなる適用を可能にする。
                    let mut args = Vec::new();
                    args.push(a);
                    Ok(Value::Native { arity: usize::MAX, f: |_args| Err(EvalError::NotFunc), args })
                }
                Value::Native { arity, f, mut args } => {
                    args.push(a);
                    if args.len() < arity { Ok(Value::Native { arity, f, args }) }
                    else if args.len() == arity { f(&args) }
                    else {
                        // over-application: apply result to remaining args
                        let (first_args, rest) = args.split_at(arity);
                        let mut res = f(first_args)?;
                        for v in rest.iter().cloned() {
                            res = match res {
                                Value::Native { arity, f, mut args } => {
                                    args.push(v);
                                    if args.len() == arity { f(&args)? } else { Value::Native { arity, f, args } }
                                }
                                Value::Closure { param, body, mut env } => {
                                    env.0.insert(param, v);
                                    eval(&env, &body)?
                                }
                                _ => return Err(EvalError::NotFunc),
                            };
                        }
                        Ok(res)
                    }
                }
                Value::Closure { param, body, mut env } => {
                    env.0.insert(param, a);
                    eval(&env, &body)
                }
                _ => Err(EvalError::NotFunc)
            }
        }
        ExprKind::Block(inner) => eval(env, inner),
    }
}
