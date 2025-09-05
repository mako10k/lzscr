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
            | ExprKind::Symbol(_) => 1,
            ExprKind::Raise(inner) => 1 + walk(inner, tbl),
            ExprKind::OrElse { left, right } => 1 + walk(left, tbl) + walk(right, tbl),
            ExprKind::Catch { left, right } => 1 + walk(left, tbl) + walk(right, tbl),
            ExprKind::Lambda { body, .. } => 1 + walk(body, tbl),
            ExprKind::Apply { func, arg } => 1 + walk(func, tbl) + walk(arg, tbl),
            ExprKind::Block(inner) => 1 + walk(inner, tbl),
        };
        // very simple structural repr (not fully unique but works for heuristic)
        let repr = match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("i:{n}"),
            ExprKind::Float(f) => format!("f:{}", f),
            ExprKind::Str(s) => format!("s:\"{}\"", s),
            ExprKind::Ref(n) => format!("r:{n}"),
            ExprKind::Symbol(s) => format!("y:{s}"),
            ExprKind::Raise(inner) => format!("raise[{}]", inner.span.len),
            ExprKind::OrElse { left, right } => format!("orelse({},{})", left.span.len, right.span.len),
            ExprKind::Catch { left, right } => format!("catch({},{})", left.span.len, right.span.len),
            ExprKind::Lambda { param, body } => {
        fn pp(p: &Pattern) -> String {
                    match &p.kind {
            PatternKind::Wildcard => "_".into(),
            PatternKind::Var(n) => format!("~{}", n),
            PatternKind::Unit => "()".into(),
            PatternKind::Tuple(xs) => format!("({})", xs.iter().map(pp).collect::<Vec<_>>().join(", ")),
            PatternKind::Ctor { name, args } => if args.is_empty() { name.clone() } else { format!("{} {}", name, args.iter().map(pp).collect::<Vec<_>>().join(" ")) },
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
        "to_str",
        // arithmetic
        "add", "sub", "mul", "div",
        "fadd", "fsub", "fmul", "fdiv",
        // compare/equality
        "lt", "le", "gt", "ge", "eq", "ne",
        "flt", "fle", "fgt", "fge",
        // logic/branch
        "and", "or", "not", "if",
        // seq/effects
        "seq", "effects",
        // tuple/record sugar
        "Tuple", "Record", "KV",
        // Bool ctor
        "Bool",
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
    fn head_and_args<'a>(e: &'a Expr) -> Option<(&'a Expr, Vec<&'a Expr>)> {
        let mut args = Vec::new();
        let mut cur = e;
        while let ExprKind::Apply { func, arg } = &cur.kind {
            args.push(arg.as_ref());
            cur = func.as_ref();
        }
        args.reverse();
        Some((cur, args))
    }
    fn check(e: &Expr, arities: &std::collections::HashMap<String, usize>, out: &mut Vec<CtorArityIssue>) {
        if let Some((head, args)) = head_and_args(e) {
            if let ExprKind::Symbol(name) = &head.kind {
                let key_a = name.clone();
                let key_b = if name.starts_with('.') { name.clone() } else { format!(".{name}") };
                if let Some(&exp) = arities.get(&key_a).or_else(|| arities.get(&key_b)) {
                    let got = args.len();
                    if exp == 0 && got > 0 {
                        out.push(CtorArityIssue { name: name.clone(), expected: exp, got, span: e.span, kind: "zero-arity-applied".into() });
                    } else if got > exp {
                        out.push(CtorArityIssue { name: name.clone(), expected: exp, got, span: e.span, kind: "over".into() });
                    }
                }
            }
        }
        match &e.kind {
            ExprKind::Lambda { body, .. } => check(body, arities, out),
            ExprKind::Apply { func, arg } => { check(func, arities, out); check(arg, arities, out); }
            ExprKind::Block(inner) => check(inner, arities, out),
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
            ExprKind::Unit | ExprKind::Int(_) | ExprKind::Float(_) | ExprKind::Str(_) | ExprKind::Symbol(_) => {}
            ExprKind::Raise(inner) => walk(inner, scopes, allow, out),
            ExprKind::OrElse { left, right } => { walk(left, scopes, allow, out); walk(right, scopes, allow, out); }
            ExprKind::Catch { left, right } => { walk(left, scopes, allow, out); walk(right, scopes, allow, out); }
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
            PatternKind::Wildcard | PatternKind::Unit | PatternKind::Symbol(_) | PatternKind::Int(_) | PatternKind::Float(_) | PatternKind::Str(_) | PatternKind::Bool(_) => {}
            PatternKind::Var(n) => { acc.insert(n.clone()); }
            PatternKind::Tuple(xs) => { for x in xs { binds(x, acc); } }
            PatternKind::Ctor { args, .. } => { for x in args { binds(x, acc); } }
            PatternKind::As(a, b) => { binds(a, acc); binds(b, acc); }
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
            PatternKind::Wildcard | PatternKind::Unit | PatternKind::Symbol(_) | PatternKind::Int(_) | PatternKind::Float(_) | PatternKind::Str(_) | PatternKind::Bool(_) => {}
            PatternKind::Var(n) => out.push(n.clone()),
            PatternKind::Tuple(xs) => { for x in xs { pat_idents(x, out); } }
            PatternKind::Ctor { args, .. } => { for x in args { pat_idents(x, out); } }
            PatternKind::As(a, b) => { pat_idents(a, out); pat_idents(b, out); }
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
                for n in ids.iter() { top.insert(n.clone()); }
                scopes.push(top);
                walk(body, scopes, out);
                scopes.pop();
            }
            ExprKind::Apply { func, arg } => {
                walk(func, scopes, out);
                walk(arg, scopes, out);
            }
            ExprKind::Raise(inner) => walk(inner, scopes, out),
            ExprKind::OrElse { left, right } => { walk(left, scopes, out); walk(right, scopes, out); }
            ExprKind::Catch { left, right } => { walk(left, scopes, out); walk(right, scopes, out); }
            ExprKind::Block(inner) => walk(inner, scopes, out),
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
            ExprKind::Lambda { param, body } => used_in(body, target) && !binds_param(param, target),
            ExprKind::Apply { func, arg } => used_in(func, target) || used_in(arg, target),
            ExprKind::Raise(inner) => used_in(inner, target),
            ExprKind::OrElse { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::Catch { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::Block(inner) => used_in(inner, target),
            _ => false,
        }
    }
    fn binds_param(p: &Pattern, name: &str) -> bool {
        match &p.kind {
            PatternKind::Wildcard | PatternKind::Unit | PatternKind::Symbol(_) | PatternKind::Int(_) | PatternKind::Float(_) | PatternKind::Str(_) | PatternKind::Bool(_) => false,
            PatternKind::Var(n) => n == name,
            PatternKind::Tuple(xs) => xs.iter().any(|x| binds_param(x, name)),
            PatternKind::Ctor { args, .. } => args.iter().any(|x| binds_param(x, name)),
            PatternKind::As(a, b) => binds_param(a, name) || binds_param(b, name),
        }
    }
    fn walk(e: &Expr, out: &mut Vec<UnusedParam>) {
    match &e.kind {
            ExprKind::Lambda { param, body } => {
                // collect bound names
        fn collect(p: &Pattern, outn: &mut Vec<String>) {
                    match &p.kind {
            PatternKind::Wildcard | PatternKind::Unit | PatternKind::Symbol(_) | PatternKind::Int(_) | PatternKind::Float(_) | PatternKind::Str(_) | PatternKind::Bool(_) => {}
            PatternKind::Var(n) => outn.push(n.clone()),
            PatternKind::Tuple(xs) => { for x in xs { collect(x, outn); } }
            PatternKind::Ctor { args, .. } => { for x in args { collect(x, outn); } }
            PatternKind::As(a, b) => { collect(a, outn); collect(b, outn); }
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
            ExprKind::Apply { func, arg } => {
                walk(func, out);
                walk(arg, out);
            }
            ExprKind::Raise(inner) => walk(inner, out),
            ExprKind::OrElse { left, right } => { walk(left, out); walk(right, out); }
            ExprKind::Catch { left, right } => { walk(left, out); walk(right, out); }
            ExprKind::Block(inner) => walk(inner, out),
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
