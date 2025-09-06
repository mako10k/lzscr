use ahash::AHashMap;
use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct DupFinding {
    pub span: lzscr_ast::span::Span,
    pub count: usize,
    pub size: usize,
    pub repr: String,
}

#[derive(Debug, Clone)]
pub struct AnalyzeOptions {
    pub min_size: usize,
    pub min_count: usize,
}

impl Default for AnalyzeOptions {
    fn default() -> Self {
        Self {
            min_size: 3,
            min_count: 2,
        }
    }
}

pub fn analyze_duplicates(expr: &Expr, opt: AnalyzeOptions) -> Vec<DupFinding> {
    let mut table: AHashMap<String, (usize, lzscr_ast::span::Span, usize)> = AHashMap::new();
    fn walk(e: &Expr, tbl: &mut AHashMap<String, (usize, lzscr_ast::span::Span, usize)>) -> usize {
        let size = match &e.kind {
            ExprKind::Unit
            | ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Str(_)
            | ExprKind::Ref(_)
            | ExprKind::Symbol(_)
            | ExprKind::TypeVal(_) => 1,
            ExprKind::Annot { expr, .. } => 1 + walk(expr, tbl),
            ExprKind::Raise(inner) => 1 + walk(inner, tbl),
            ExprKind::OrElse { left, right } => 1 + walk(left, tbl) + walk(right, tbl),
            ExprKind::AltLambda { left, right } => 1 + walk(left, tbl) + walk(right, tbl),
            ExprKind::Catch { left, right } => 1 + walk(left, tbl) + walk(right, tbl),
            ExprKind::Lambda { body, .. } => 1 + walk(body, tbl),
            ExprKind::Apply { func, arg } => 1 + walk(func, tbl) + walk(arg, tbl),
            ExprKind::Block(inner) => 1 + walk(inner, tbl),
            ExprKind::LetGroup { bindings, body } => {
                let mut acc = 1 + walk(body, tbl);
                for (_p, ex) in bindings {
                    acc += walk(ex, tbl);
                }
                acc
            }
            ExprKind::List(xs) => 1 + xs.iter().map(|x| walk(x, tbl)).sum::<usize>(),
        };
        // very simple structural repr (not fully unique but works for heuristic)
        let repr = match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("i:{n}"),
            ExprKind::Float(f) => format!("f:{}", f),
            ExprKind::Str(s) => format!("s:\"{}\"", s),
            ExprKind::Ref(n) => format!("r:{n}"),
            ExprKind::Symbol(s) => format!("y:{s}"),
            ExprKind::TypeVal(_) => "typeval".into(),
            ExprKind::Annot { expr, .. } => format!("annot[{}]", expr.span.len),
            ExprKind::Raise(inner) => format!("raise[{}]", inner.span.len),
            ExprKind::OrElse { left, right } => {
                format!("orelse({},{})", left.span.len, right.span.len)
            }
            ExprKind::AltLambda { left, right } => {
                format!("altlam({},{})", left.span.len, right.span.len)
            }
            ExprKind::Catch { left, right } => {
                format!("catch({},{})", left.span.len, right.span.len)
            }
            ExprKind::Lambda { param, body } => {
                fn pp(p: &Pattern) -> String {
                    match &p.kind {
                        PatternKind::Wildcard => "_".into(),
                        PatternKind::Var(n) => format!("~{}", n),
                        PatternKind::Unit => "()".into(),
                        PatternKind::Tuple(xs) => {
                            format!(
                                "({})",
                                xs.iter().map(pp).collect::<Vec<_>>().join(", ")
                            )
                        }
                        PatternKind::List(xs) => {
                            format!(
                                "[{}]",
                                xs.iter().map(pp).collect::<Vec<_>>().join(", ")
                            )
                        }
                        PatternKind::Record(fields) => {
                            let inner = fields
                                .iter()
                                .map(|(k, v)| format!("{}: {}", k, pp(v)))
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("{{{}}}", inner)
                        }
                        PatternKind::Ctor { name, args } => {
                            if args.is_empty() {
                                name.clone()
                            } else {
                                format!(
                                    "{} {}",
                                    name,
                                    args.iter().map(pp).collect::<Vec<_>>().join(" ")
                                )
                            }
                        }
                        PatternKind::Cons(h, t) => format!("{} : {}", pp(h), pp(t)),
                        PatternKind::Symbol(s) => s.clone(), // 現状パーサで禁止方向に寄せたが互換のため残置
                        PatternKind::Int(n) => format!("{}", n),
                        PatternKind::Float(f) => format!("{}", f),
                        PatternKind::Str(s) => format!("\"{}\"", s.escape_default()),
                        PatternKind::Bool(b) => format!("{}", b),
                        PatternKind::As(a, b) => format!("{} @ {}", pp(a), pp(b)),
                    }
                }
                format!("lam {} -> [{}]", pp(param), body.span.len)
            }
            ExprKind::Apply { func, arg } => format!("ap({},{})", func.span.len, arg.span.len),
            ExprKind::Block(inner) => format!("blk({})", inner.span.len),
            ExprKind::LetGroup { bindings, body } => {
                let bcount = bindings.len();
                format!("letg[{};{}]", bcount, body.span.len)
            }
            ExprKind::List(xs) => format!("list({})", xs.len()),
        };
        let entry = tbl.entry(repr).or_insert((0, e.span, size));
        entry.0 += 1;
        entry.1 = e.span; // last span seen (heuristic)
        entry.2 = size; // overwrite size (same repr should be same size)
        size
    }
    let _ = walk(expr, &mut table);
    let mut out = Vec::new();
    for (repr, (count, span, size)) in table {
        if size >= opt.min_size && count >= opt.min_count {
            out.push(DupFinding {
                span,
                count,
                size,
                repr,
            });
        }
    }
    out.sort_by_key(|d| (usize::MAX - d.size, usize::MAX - d.count));
    out
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct UnboundRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct Shadowing {
    pub name: String,
    pub lambda_span: Span,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct UnusedParam {
    pub name: String,
    pub lambda_span: Span,
}

pub fn default_allowlist() -> HashSet<String> {
    // Builtins available via ~name (keep in sync with runtime)
    [
        "to_str", "add", "sub", "mul", "div", "fadd", "fsub", "fmul", "fdiv", "lt", "le", "gt",
        "ge", "eq", "ne", "flt", "fle", "fgt", "fge", "and", "or", "not", "if", "seq", "effects",
        "Tuple", "Record", "KV", "Bool", "cons",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct CtorArityIssue {
    pub name: String,
    pub expected: usize,
    pub got: usize,
    pub span: Span,
    pub kind: String, // "over" | "zero-arity-applied"
}

pub fn analyze_ctor_arity(
    expr: &Expr,
    arities: &std::collections::HashMap<String, usize>,
) -> Vec<CtorArityIssue> {
    fn head_and_args(e: &Expr) -> Option<(&Expr, Vec<&Expr>)> {
        let mut args = Vec::new();
        let mut cur = e;
        while let ExprKind::Apply { func, arg } = &cur.kind {
            args.push(arg.as_ref());
            cur = func.as_ref();
        }
        args.reverse();
        Some((cur, args))
    }
    fn check(
        e: &Expr,
        arities: &std::collections::HashMap<String, usize>,
        out: &mut Vec<CtorArityIssue>,
    ) {
        // Only check value-level constructor applications
        if let Some((head, args)) = head_and_args(e) {
            if let ExprKind::Symbol(name) = &head.kind {
                if let Some(exp) = arities.get(name) {
                    let got = args.len();
                    if *exp == 0 && got > 0 {
                        out.push(CtorArityIssue {
                            name: name.clone(),
                            expected: 0,
                            got,
                            span: e.span,
                            kind: "zero-arity-applied".into(),
                        });
            } else if got > *exp {
                        out.push(CtorArityIssue {
                            name: name.clone(),
                expected: *exp,
                            got,
                            span: e.span,
                            kind: "over".into(),
                        });
                    }
                }
            }
        }
        match &e.kind {
            ExprKind::Lambda { body, .. } => check(body, arities, out),
            ExprKind::Apply { func, arg } => {
                check(func, arities, out);
                check(arg, arities, out);
            }
            ExprKind::LetGroup { bindings, body } => {
                for (_p, ex) in bindings {
                    check(ex, arities, out);
                }
                check(body, arities, out);
            }
            ExprKind::Block(inner) => check(inner, arities, out),
            ExprKind::Annot { expr, .. } => check(expr, arities, out),
            ExprKind::TypeVal(_) => {}
            _ => {}
        }
    }
    let mut out = Vec::new();
    check(expr, arities, &mut out);
    out
}

pub fn analyze_unbound_refs(expr: &Expr, allowlist: &HashSet<String>) -> Vec<UnboundRef> {
    let mut out = Vec::new();
    fn walk(
        e: &Expr,
        scopes: &mut Vec<HashSet<String>>, 
        allow: &HashSet<String>,
        out: &mut Vec<UnboundRef>,
    ) {
        match &e.kind {
            ExprKind::Unit
            | ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Str(_)
            | ExprKind::Symbol(_)
            | ExprKind::TypeVal(_) => {}
            ExprKind::Annot { expr, .. } => {
                walk(expr, scopes, allow, out);
            }
            ExprKind::List(xs) => {
                for x in xs {
                    walk(x, scopes, allow, out);
                }
            }
            ExprKind::LetGroup { bindings, body } => {
                // let-group 全体のスコープを 1 つ作成（全束縛名を可視化）
                fn binds(p: &Pattern, acc: &mut HashSet<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Bool(_) => {}
                        PatternKind::Var(n) => {
                            acc.insert(n.clone());
                        }
                        PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                            for x in xs {
                                binds(x, acc);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for (_, v) in fs {
                                binds(v, acc);
                            }
                        }
                        PatternKind::Ctor { args, .. } => {
                            for x in args {
                                binds(x, acc);
                            }
                        }
                        PatternKind::Cons(h, t) => {
                            binds(h, acc);
                            binds(t, acc);
                        }
                        PatternKind::As(a, b) => {
                            binds(a, acc);
                            binds(b, acc);
                        }
                    }
                }
                let mut set = scopes.last().cloned().unwrap_or_default();
                for (p, _) in bindings.iter() {
                    binds(p, &mut set);
                }
                scopes.push(set);
                // 式と各束縛式を解析
                walk(body, scopes, allow, out);
                for (_p, ex) in bindings.iter() {
                    walk(ex, scopes, allow, out);
                }
                scopes.pop();
            }
            ExprKind::Raise(inner) => walk(inner, scopes, allow, out),
            ExprKind::OrElse { left, right } => {
                walk(left, scopes, allow, out);
                walk(right, scopes, allow, out);
            }
            ExprKind::AltLambda { left, right } => {
                walk(left, scopes, allow, out);
                walk(right, scopes, allow, out);
            }
            ExprKind::Catch { left, right } => {
                walk(left, scopes, allow, out);
                walk(right, scopes, allow, out);
            }
            ExprKind::Ref(n) => {
                let mut bound = allow.contains(n);
                if !bound {
                    for s in scopes.iter().rev() {
                        if s.contains(n) {
                            bound = true;
                            break;
                        }
                    }
                }
                if !bound {
                    out.push(UnboundRef {
                        name: n.clone(),
                        span: e.span,
                    });
                }
            }
            ExprKind::Lambda { param, body } => {
                fn binds(p: &Pattern, acc: &mut HashSet<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Bool(_) => {}
                        PatternKind::Var(n) => {
                            acc.insert(n.clone());
                        }
                        PatternKind::Tuple(xs) => {
                            for x in xs {
                                binds(x, acc);
                            }
                        }
                        PatternKind::List(xs) => {
                            for x in xs {
                                binds(x, acc);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for (_, v) in fs {
                                binds(v, acc);
                            }
                        }
                        PatternKind::Ctor { args, .. } => {
                            for x in args {
                                binds(x, acc);
                            }
                        }
                        PatternKind::Cons(h, t) => {
                            binds(h, acc);
                            binds(t, acc);
                        }
                        PatternKind::As(a, b) => {
                            binds(a, acc);
                            binds(b, acc);
                        }
                    }
                }
                let mut set = HashSet::new();
                binds(param, &mut set);
                scopes.push(set);
                walk(body, scopes, allow, out);
                scopes.pop();
            }
            ExprKind::Apply { func, arg } => {
                walk(func, scopes, allow, out);
                walk(arg, scopes, allow, out);
            }
            ExprKind::Block(inner) => walk(inner, scopes, allow, out),
        }
    }
    let mut scopes: Vec<HashSet<String>> = Vec::new();
    walk(expr, &mut scopes, allowlist, &mut out);
    out
}

pub fn analyze_shadowing(expr: &Expr) -> Vec<Shadowing> {
    let mut out = Vec::new();
    fn walk(e: &Expr, scopes: &mut Vec<HashSet<String>>, out: &mut Vec<Shadowing>) {
        match &e.kind {
            ExprKind::Lambda { param, body } => {
                fn pat_idents(p: &Pattern, out: &mut Vec<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Bool(_) => {}
                        PatternKind::Var(n) => out.push(n.clone()),
                        PatternKind::Tuple(xs) => {
                            for x in xs {
                                pat_idents(x, out);
                            }
                        }
                        PatternKind::List(xs) => {
                            for x in xs {
                                pat_idents(x, out);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for (_, v) in fs {
                                pat_idents(v, out);
                            }
                        }
                        PatternKind::Ctor { args, .. } => {
                            for x in args {
                                pat_idents(x, out);
                            }
                        }
                        PatternKind::Cons(h, t) => {
                            pat_idents(h, out);
                            pat_idents(t, out);
                        }
                        PatternKind::As(a, b) => {
                            pat_idents(a, out);
                            pat_idents(b, out);
                        }
                    }
                }
                let mut ids = Vec::new();
                pat_idents(param, &mut ids);
                let is_shadow = scopes.iter().any(|s| ids.iter().any(|n| s.contains(n)));
                if is_shadow {
                    out.push(Shadowing {
                        name: ids.first().cloned().unwrap_or_else(|| "_".into()),
                        lambda_span: e.span,
                    });
                }
                let mut top = scopes.last().cloned().unwrap_or_default();
                for n in ids.iter() {
                    top.insert(n.clone());
                }
                scopes.push(top);
                walk(body, scopes, out);
                scopes.pop();
            }
            ExprKind::Apply { func, arg } => {
                walk(func, scopes, out);
                walk(arg, scopes, out);
            }
            ExprKind::LetGroup { bindings, body } => {
                fn pat_idents(p: &Pattern, outn: &mut Vec<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Bool(_) => {}
                        PatternKind::Var(n) => outn.push(n.clone()),
                        PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                            for x in xs {
                                pat_idents(x, outn);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for (_, v) in fs {
                                pat_idents(v, outn);
                            }
                        }
                        PatternKind::Ctor { args, .. } => {
                            for x in args {
                                pat_idents(x, outn);
                            }
                        }
                        PatternKind::Cons(h, t) => {
                            pat_idents(h, outn);
                            pat_idents(t, outn);
                        }
                        PatternKind::As(a, b) => {
                            pat_idents(a, outn);
                            pat_idents(b, outn);
                        }
                    }
                }
                let mut ids = Vec::new();
                for (p, _) in bindings.iter() {
                    pat_idents(p, &mut ids);
                }
                let is_shadow = scopes.iter().any(|s| ids.iter().any(|n| s.contains(n)));
                if is_shadow {
                    out.push(Shadowing {
                        name: ids.first().cloned().unwrap_or_else(|| "_".into()),
                        lambda_span: e.span,
                    });
                }
                let mut top = scopes.last().cloned().unwrap_or_default();
                for n in ids.iter() {
                    top.insert(n.clone());
                }
                scopes.push(top);
                walk(body, scopes, out);
                for (_p, ex) in bindings.iter() {
                    walk(ex, scopes, out);
                }
                scopes.pop();
            }
            ExprKind::List(xs) => {
                for x in xs {
                    walk(x, scopes, out);
                }
            }
            ExprKind::Raise(inner) => walk(inner, scopes, out),
            ExprKind::OrElse { left, right } => {
                walk(left, scopes, out);
                walk(right, scopes, out);
            }
            ExprKind::Catch { left, right } => {
                walk(left, scopes, out);
                walk(right, scopes, out);
            }
            ExprKind::Block(inner) => walk(inner, scopes, out),
            ExprKind::Annot { expr, .. } => walk(expr, scopes, out),
            ExprKind::TypeVal(_) => {}
            _ => {}
        }
    }
    let mut scopes: Vec<HashSet<String>> = Vec::new();
    walk(expr, &mut scopes, &mut out);
    out
}

pub fn analyze_unused_params(expr: &Expr) -> Vec<UnusedParam> {
    let mut out = Vec::new();
    fn used_in(e: &Expr, target: &str) -> bool {
        match &e.kind {
            ExprKind::Ref(n) => n == target,
            ExprKind::Lambda { param, body } => {
                used_in(body, target) && !binds_param(param, target)
            }
            ExprKind::Apply { func, arg } => used_in(func, target) || used_in(arg, target),
            ExprKind::List(xs) => xs.iter().any(|x| used_in(x, target)),
            ExprKind::Raise(inner) => used_in(inner, target),
            ExprKind::OrElse { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::AltLambda { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::Catch { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::Block(inner) => used_in(inner, target),
            ExprKind::Annot { expr, .. } => used_in(expr, target),
            ExprKind::TypeVal(_) => false,
            _ => false,
        }
    }
    fn binds_param(p: &Pattern, name: &str) -> bool {
        match &p.kind {
            PatternKind::Wildcard
            | PatternKind::Unit
            | PatternKind::Symbol(_)
            | PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Str(_)
            | PatternKind::Bool(_) => false,
            PatternKind::Var(n) => n == name,
            PatternKind::Tuple(xs) => xs.iter().any(|x| binds_param(x, name)),
            PatternKind::List(xs) => xs.iter().any(|x| binds_param(x, name)),
            PatternKind::Record(fs) => fs.iter().any(|(_, v)| binds_param(v, name)),
            PatternKind::Ctor { args, .. } => args.iter().any(|x| binds_param(x, name)),
            PatternKind::Cons(h, t) => binds_param(h, name) || binds_param(t, name),
            PatternKind::As(a, b) => binds_param(a, name) || binds_param(b, name),
        }
    }
    fn walk(e: &Expr, out: &mut Vec<UnusedParam>) {
        match &e.kind {
            ExprKind::Lambda { param, body } => {
                // collect bound names
                fn collect(p: &Pattern, outn: &mut Vec<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Bool(_) => {}
                        PatternKind::Var(n) => outn.push(n.clone()),
                        PatternKind::Tuple(xs) => {
                            for x in xs {
                                collect(x, outn);
                            }
                        }
                        PatternKind::List(xs) => {
                            for x in xs {
                                collect(x, outn);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for (_, v) in fs {
                                collect(v, outn);
                            }
                        }
                        PatternKind::Ctor { args, .. } => {
                            for x in args {
                                collect(x, outn);
                            }
                        }
                        PatternKind::Cons(h, t) => {
                            collect(h, outn);
                            collect(t, outn);
                        }
                        PatternKind::As(a, b) => {
                            collect(a, outn);
                            collect(b, outn);
                        }
                    }
                }
                let mut names = Vec::new();
                collect(param, &mut names);
                for n in names.iter() {
                    if !used_in(body, n) {
                        out.push(UnusedParam { name: n.clone(), lambda_span: e.span });
                    }
                }
                walk(body, out);
            }
            ExprKind::Annot { expr, .. } => walk(expr, out),
            ExprKind::Apply { func, arg } => {
                walk(func, out);
                walk(arg, out);
            }
            ExprKind::List(xs) => {
                for x in xs {
                    walk(x, out);
                }
            }
            ExprKind::Raise(inner) => walk(inner, out),
            ExprKind::OrElse { left, right } => {
                walk(left, out);
                walk(right, out);
            }
            ExprKind::AltLambda { left, right } => {
                walk(left, out);
                walk(right, out);
            }
            ExprKind::Catch { left, right } => {
                walk(left, out);
                walk(right, out);
            }
            ExprKind::Block(inner) => walk(inner, out),
            ExprKind::TypeVal(_) => {}
            _ => {}
        }
    }
    walk(expr, &mut out);
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use lzscr_parser::parse_expr;

    #[test]
    fn detects_simple_duplicate_apply() {
        // (~add 1 2) (~add 1 2)
        let e = parse_expr("((~add 1 2) (~add 1 2))").unwrap();
        let d = analyze_duplicates(
            &e,
            AnalyzeOptions {
                min_size: 3,
                min_count: 2,
            },
        );
        assert!(d.iter().any(|f| f.count >= 2 && f.size >= 3));
    }
}
