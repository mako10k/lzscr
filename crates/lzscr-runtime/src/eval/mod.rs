//! Evaluation functions for lzscr runtime.

use crate::{Env, EvalError, ThunkKind, ThunkState, Value};
use lzscr_ast::ast::{Expr, ExprKind, Pattern, PatternKind, TypeExpr};

pub fn v_equal(_env: &Env, a: &Value, b: &Value) -> bool {
    // Note: do not force thunks here (equality is assumed to be used after evaluation on typical paths)
    match (a, b) {
        (Value::Unit, Value::Unit) => true,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
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

pub fn match_pattern(
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
        PatternKind::Record(fields) => match &v {
            Value::Record(map) => {
                // all fields in pattern must exist in value
                let mut acc = HashMap::new();
                for f in fields {
                    let vv = map.get(&f.name)?;
                    let bi = match_pattern(env, &f.pattern, vv)?;
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

pub fn force_value(env: &Env, v: &Value) -> Result<Value, EvalError> {
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

pub fn apply_value(env: &Env, fval: Value, aval: Value) -> Result<Value, EvalError> {
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
                // Symbols have arity 0 and cannot be applied
                Err(EvalError::NotApplicable(format!("symbol '{}' (symbols have arity 0)", name)))
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
                        return Err(EvalError::NotApplicable(format!(
                            "constructor '{}' (expected {} args, got {})",
                            name,
                            k,
                            args.len()
                        )));
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
        v => {
            let type_desc = match v {
                Value::Unit => "unit value ()",
                Value::Int(_) => "integer",
                Value::Float(_) => "float",
                Value::Str(_) => "string",
                Value::Char(_) => "character",
                Value::List(_) => "list",
                Value::Raised(_) => "raised exception",
                _ => "non-function value",
            };
            Err(EvalError::NotApplicable(type_desc.to_string()))
        }
    }
}

pub fn eval(env: &Env, e: &Expr) -> Result<Value, EvalError> {
    fn print_type_expr(t: &TypeExpr) -> String {
        fn dotted(name: &str) -> String {
            let mut s = String::from(".");
            s.push_str(name);
            s
        }
        match t {
            TypeExpr::Unit => dotted("Unit"),
            TypeExpr::Int => dotted("Int"),
            TypeExpr::Float => dotted("Float"),
            TypeExpr::Bool => dotted("Bool"),
            TypeExpr::Str => dotted("Str"),
            TypeExpr::Char => dotted("Char"),
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
                    .map(|f| format!("{}: {}", f.name, print_type_expr(&f.type_expr)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", inner)
            }
            TypeExpr::Fun(a, b) => format!("{} -> {}", print_type_expr(a), print_type_expr(b)),
            TypeExpr::Ctor { tag, args } => {
                let head = dotted(tag);
                if args.is_empty() {
                    head
                } else {
                    format!(
                        "{} {}",
                        head,
                        args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                    )
                }
            }
            TypeExpr::Sum(alts) => {
                let parts = alts
                    .iter()
                    .map(|(tag, args)| {
                        let head = dotted(tag);
                        if args.is_empty() {
                            head
                        } else {
                            format!(
                                "{} {}",
                                head,
                                args.iter().map(print_type_expr).collect::<Vec<_>>().join(" ")
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("%{{{}}}", parts)
            }
        }
    }
    match &e.kind {
        ExprKind::Annot { ty: _, expr } => eval(env, expr),
        ExprKind::TypeVal(ty) => {
            let inner = print_type_expr(ty);
            Ok(Value::Str(env.intern_string(format!("%{{{}}}", inner))))
        }
        ExprKind::Unit => Ok(Value::Unit),
        ExprKind::Int(n) => Ok(Value::Int(*n)),
        ExprKind::Str(s) => Ok(Value::Str(env.intern_string(s))),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Record(fields) => {
            // Eagerly evaluate field expressions (could later be lazy if desired)
            let mut map = std::collections::BTreeMap::new();
            for f in fields {
                let val = eval(env, &f.value)?;
                map.insert(f.name.clone(), val);
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
                    | PatternKind::Char(_) => {}
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
                        for f in fs {
                            collect_vars(&f.pattern, out);
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
        ExprKind::Symbol(s) => {
            fn is_tuple_tag(name: &str) -> bool {
                name.starts_with('.') && name.chars().skip(1).all(|c| c == ',')
            }
            if s.starts_with('.') && !is_tuple_tag(s) {
                Ok(Value::Symbol(env.intern_symbol(s)))
            } else {
                Ok(Value::Ctor { name: s.clone(), args: vec![] })
            }
        }
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
                // Symbol: has arity 0, cannot be applied (except tuple constructors)
                Value::Symbol(id) => {
                    let name = env.symbol_name(id);
                    // Special internal tuple pack constructor '.,', '.,,', ... accepts any arity
                    if name.starts_with('.') && name.chars().skip(1).all(|c| c == ',') {
                        Ok(Value::Ctor { name, args: vec![a] })
                    } else {
                        // Symbols have arity 0 and cannot be applied
                        Err(EvalError::Traced {
                            kind: Box::new(EvalError::NotApplicable(format!(
                                "symbol '{}' (symbols have arity 0)",
                                name
                            ))),
                            spans: vec![e.span],
                        })
                    }
                }
                Value::Ctor { name, mut args } => {
                    args.push(a);
                    if let Some(&k) = env
                        .ctor_arity
                        .get(&name)
                        .or_else(|| env.ctor_arity.get(&format!(".{name}")))
                    {
                        if args.len() > k {
                            return Err(EvalError::Traced {
                                kind: Box::new(EvalError::NotApplicable(format!(
                                    "constructor '{}' (expected {} args, got {})",
                                    name,
                                    k,
                                    args.len()
                                ))),
                                spans: vec![e.span],
                            });
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
                                v => {
                                    let type_desc = match v {
                                        Value::Unit => "unit value ()",
                                        Value::Int(_) => "integer",
                                        Value::Float(_) => "float",
                                        Value::Str(_) => "string",
                                        Value::Char(_) => "character",
                                        Value::List(_) => "list",
                                        Value::Raised(_) => "raised exception",
                                        _ => "non-function value",
                                    };
                                    return Err(EvalError::Traced {
                                        kind: Box::new(EvalError::NotApplicable(
                                            type_desc.to_string(),
                                        )),
                                        spans: vec![e.span],
                                    });
                                }
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
                v => {
                    let type_desc = match v {
                        Value::Unit => "unit value ()",
                        Value::Int(_) => "integer",
                        Value::Float(_) => "float",
                        Value::Str(_) => "string",
                        Value::Char(_) => "character",
                        Value::List(_) => "list",
                        Value::Raised(_) => "raised exception",
                        _ => "non-function value",
                    };
                    Err(EvalError::Traced {
                        kind: Box::new(EvalError::NotApplicable(type_desc.to_string())),
                        spans: vec![e.span],
                    })
                }
            }
        }
        ExprKind::Block(inner) => eval(env, inner),
        ExprKind::ModeMap(fields) => {
            // Evaluate each field expression and collect into a record-like structure
            // Store as Value::Record for consistency with record evaluation
            let mut result_fields: std::collections::BTreeMap<String, Value> =
                std::collections::BTreeMap::new();
            for f in fields {
                let v = eval(env, &f.value)?;
                if let Value::Raised(_) = v {
                    return Ok(v);
                }
                result_fields.insert(f.name.clone(), v);
            }
            Ok(Value::Record(result_fields))
        }
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
