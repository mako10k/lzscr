use ahash::{AHashMap, AHasher};
use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use std::collections::HashSet;
use std::hash::Hasher;

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
        Self { min_size: 3, min_count: 2 }
    }
}

pub fn analyze_duplicates(expr: &Expr, opt: AnalyzeOptions) -> Vec<DupFinding> {
    // Structural hasher for Expr (pattern shapes summarized; strings not fully embedded)
    fn hash_pattern_shape(p: &Pattern, h: &mut AHasher) {
        use PatternKind::*;
        match &p.kind {
            Wildcard => h.write_u8(1),
            Unit => h.write_u8(2),
            Symbol(_) => h.write_u8(3),
            Int(_) => h.write_u8(4),
            Float(_) => h.write_u8(5),
            Str(s) => {
                h.write_u8(6);
                h.write_usize(s.len());
            }
            Char(c) => {
                h.write_u8(7);
                h.write_i32(*c);
            }
            TypeBind { pat, .. } => {
                h.write_u8(8);
                hash_pattern_shape(pat, h);
            }
            Var(_) => {
                h.write_u8(9);
            }
            Tuple(xs) => {
                h.write_u8(10);
                h.write_usize(xs.len());
                for x in xs {
                    hash_pattern_shape(x, h);
                }
            }
            List(xs) => {
                h.write_u8(11);
                h.write_usize(xs.len());
                for x in xs {
                    hash_pattern_shape(x, h);
                }
            }
            Record(fs) => {
                h.write_u8(12);
                h.write_usize(fs.len());
                for f in fs {
                    h.write(f.name.as_bytes());
                    hash_pattern_shape(&f.pattern, h);
                }
            }
            Ctor { name, args } => {
                h.write_u8(13);
                h.write(name.as_bytes());
                h.write_usize(args.len());
                for a in args {
                    hash_pattern_shape(a, h);
                }
            }
            Cons(hd, tl) => {
                h.write_u8(14);
                hash_pattern_shape(hd, h);
                hash_pattern_shape(tl, h);
            }
            As(a, b) => {
                h.write_u8(15);
                hash_pattern_shape(a, h);
                hash_pattern_shape(b, h);
            }
        }
    }
    fn hash_expr(e: &Expr, size_out: &mut usize) -> u64 {
        use ExprKind::*;
        let mut hasher = AHasher::default();
        fn go(e: &Expr, h: &mut AHasher, sz: &mut usize) {
            *sz += 1;
            match &e.kind {
                Unit => {
                    h.write_u8(1);
                }
                Int(n) => {
                    h.write_u8(2);
                    h.write_i64(*n);
                }
                Float(fv) => {
                    h.write_u8(3);
                    h.write_u64(fv.to_bits());
                }
                Str(s) => {
                    h.write_u8(4);
                    h.write_usize(s.len());
                }
                Char(c) => {
                    h.write_u8(5);
                    h.write_i32(*c);
                }
                Ref(n) => {
                    h.write_u8(6);
                    h.write(n.as_bytes());
                }
                Symbol(s) => {
                    h.write_u8(7);
                    h.write(s.as_bytes());
                }
                TypeVal(_) => {
                    h.write_u8(8);
                }
                Annot { expr, .. } => {
                    h.write_u8(9);
                    go(expr, h, sz);
                }
                Raise(inner) => {
                    h.write_u8(10);
                    go(inner, h, sz);
                }
                OrElse { left, right } => {
                    h.write_u8(11);
                    go(left, h, sz);
                    go(right, h, sz);
                }
                AltLambda { left, right } => {
                    h.write_u8(12);
                    go(left, h, sz);
                    go(right, h, sz);
                }
                Catch { left, right } => {
                    h.write_u8(13);
                    go(left, h, sz);
                    go(right, h, sz);
                }
                Lambda { param, body } => {
                    h.write_u8(14);
                    hash_pattern_shape(param, h);
                    go(body, h, sz);
                }
                Apply { func, arg } => {
                    h.write_u8(15);
                    go(func, h, sz);
                    go(arg, h, sz);
                }
                Block(inner) => {
                    h.write_u8(16);
                    go(inner, h, sz);
                }
                List(xs) => {
                    h.write_u8(17);
                    h.write_usize(xs.len());
                    for x in xs {
                        go(x, h, sz);
                    }
                }
                LetGroup { bindings, body, .. } => {
                    h.write_u8(18);
                    h.write_usize(bindings.len());
                    for (p, ex) in bindings {
                        hash_pattern_shape(p, h);
                        go(ex, h, sz);
                    }
                    go(body, h, sz);
                }
                Record(fs) => {
                    h.write_u8(19);
                    h.write_usize(fs.len());
                    for f in fs {
                        h.write(f.name.as_bytes());
                        go(&f.value, h, sz);
                    }
                }
            }
        }
        go(e, &mut hasher, size_out);
        hasher.finish()
    }
    fn summary_repr(e: &Expr) -> String {
        use ExprKind::*;
        match &e.kind {
            Unit => "()".to_string(),
            Int(n) => format!("i:{n}"),
            Float(f) => format!("f:{}", f),
            Str(s) => format!("s(len={})", s.len()),
            Char(c) => format!("c:{}", c),
            Ref(n) => format!("r:{n}"),
            Symbol(s) => format!("y:{s}"),
            TypeVal(_) => "typeval".into(),
            Annot { expr, .. } => format!("annot[{}]", expr.span.len),
            Raise(inner) => format!("raise[{}]", inner.span.len),
            OrElse { left, right } => format!("orelse({},{})", left.span.len, right.span.len),
            AltLambda { left, right } => format!("altlam({},{})", left.span.len, right.span.len),
            Catch { left, right } => format!("catch({},{})", left.span.len, right.span.len),
            Lambda { body, .. } => format!("lam -> [{}]", body.span.len),
            Apply { func, arg } => format!("ap({},{})", func.span.len, arg.span.len),
            Block(inner) => format!("blk({})", inner.span.len),
            LetGroup { bindings, body, .. } => {
                format!("letg[{};{}]", bindings.len(), body.span.len)
            }
            List(xs) => format!("list({})", xs.len()),
            Record(fs) => format!("rec({})", fs.len()),
        }
    }

    // Walk the AST, hash each sub-expression, and aggregate counts/sizes
    #[derive(Clone)]
    struct Entry {
        count: usize,
        span: Span,
        size: usize,
        repr: String,
    }

    fn should_recurse(e: &Expr) -> bool {
        !matches!(&e.kind, ExprKind::TypeVal(_))
    }

    fn collect(e: &Expr, opt: &AnalyzeOptions, map: &mut AHashMap<u64, Entry>) {
        // hash the current node
        let mut size = 0usize;
        let h = hash_expr(e, &mut size);
        if size >= opt.min_size {
            let ent = map.entry(h).or_insert_with(|| Entry {
                count: 0,
                span: e.span,
                size,
                repr: summary_repr(e),
            });
            ent.count += 1;
            // keep the largest size/earliest span as a more informative exemplar
            if size > ent.size {
                ent.size = size;
                ent.span = e.span;
            }
        }
        // recurse into children to index all sub-expressions
        if !should_recurse(e) {
            return;
        }
        match &e.kind {
            ExprKind::Annot { expr, .. } => collect(expr, opt, map),
            ExprKind::Raise(inner) => collect(inner, opt, map),
            ExprKind::OrElse { left, right }
            | ExprKind::AltLambda { left, right }
            | ExprKind::Catch { left, right } => {
                collect(left, opt, map);
                collect(right, opt, map);
            }
            ExprKind::Lambda { body, .. } => {
                collect(body, opt, map);
            }
            ExprKind::Apply { func, arg } => {
                collect(func, opt, map);
                collect(arg, opt, map);
            }
            ExprKind::Block(inner) => collect(inner, opt, map),
            ExprKind::List(xs) => {
                for x in xs {
                    collect(x, opt, map);
                }
            }
            ExprKind::Record(fs) => {
                for f in fs {
                    collect(&f.value, opt, map);
                }
            }
            ExprKind::LetGroup { bindings, body, .. } => {
                for (_p, ex) in bindings {
                    collect(ex, opt, map);
                }
                collect(body, opt, map);
            }
            _ => {}
        }
    }

    let mut map: AHashMap<u64, Entry> = AHashMap::new();
    collect(expr, &opt, &mut map);
    let mut out: Vec<DupFinding> = map
        .into_iter()
        .filter_map(|(_h, e)| {
            if e.count >= opt.min_count && e.size >= opt.min_size {
                Some(DupFinding { span: e.span, count: e.count, size: e.size, repr: e.repr })
            } else {
                None
            }
        })
        .collect();
    // sort by count desc, then size desc, then span start
    out.sort_by(|a, b| {
        b.count.cmp(&a.count).then(b.size.cmp(&a.size)).then(a.span.offset.cmp(&b.span.offset))
    });
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

#[derive(Debug, Clone, serde::Serialize)]
pub struct UnusedLet {
    pub name: String,
    pub binding_span: Span,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct LetCollision {
    pub name: String,
    pub group_span: Span,
}

pub fn default_allowlist() -> HashSet<String> {
    // Builtins available via ~name (keep in sync with runtime)
    [
        "to_str", "add", "sub", "mul", "div", "fadd", "fsub", "fmul", "fdiv", "lt", "le", "gt",
        "ge", "eq", "ne", "flt", "fle", "fgt", "fge", "and", "or", "not", "if", "seq", "chain",
        "bind", "effects", "Tuple", "Record", "KV", "Bool", "cons",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
    // \%{ %a } ~x -> 1  => x is unused
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct CtorArityIssue {
    pub name: String,
    pub expected: usize,
    pub got: usize,
    pub span: Span,
    // \\~x -> (\\%{ %a } ~x -> ~x) ~x  => inner lambda's x shadows the outer one
    pub kind: String, // kinds: over | zero-arity-applied
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
            ExprKind::LetGroup { bindings, body, .. } => {
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
            | ExprKind::Char(_)
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
            ExprKind::Record(fs) => {
                for f in fs {
                    walk(&f.value, scopes, allow, out);
                }
            }
            ExprKind::LetGroup { bindings, body, .. } => {
                // Create one scope for the whole let-group (all binding names visible)
                fn binds(p: &Pattern, acc: &mut HashSet<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Char(_) => {}
                        PatternKind::TypeBind { pat, .. } => {
                            binds(pat, acc);
                        }
                        PatternKind::Var(n) => {
                            acc.insert(n.clone());
                        }
                        PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                            for x in xs {
                                binds(x, acc);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for f in fs {
                                binds(&f.pattern, acc);
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
                // Analyze body and each bound expression
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
                    out.push(UnboundRef { name: n.clone(), span: e.span });
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
                        | PatternKind::Char(_) => {}
                        PatternKind::TypeBind { pat, .. } => {
                            binds(pat, acc);
                        }
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
                            for f in fs {
                                binds(&f.pattern, acc);
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
                        | PatternKind::Char(_) => {}
                        PatternKind::TypeBind { pat, .. } => {
                            pat_idents(pat, out);
                        }
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
                            for f in fs {
                                pat_idents(&f.pattern, out);
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
                        name: ids.first().cloned().unwrap_or_else(|| "_".to_string()),
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
            ExprKind::LetGroup { bindings, body, .. } => {
                fn pat_idents(p: &Pattern, outn: &mut Vec<String>) {
                    match &p.kind {
                        PatternKind::Wildcard
                        | PatternKind::Unit
                        | PatternKind::Symbol(_)
                        | PatternKind::Int(_)
                        | PatternKind::Float(_)
                        | PatternKind::Str(_)
                        | PatternKind::Char(_) => {}
                        PatternKind::TypeBind { pat, .. } => {
                            pat_idents(pat, outn);
                        }
                        PatternKind::Var(n) => outn.push(n.clone()),
                        PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                            for x in xs {
                                pat_idents(x, outn);
                            }
                        }
                        PatternKind::Record(fs) => {
                            for f in fs {
                                pat_idents(&f.pattern, outn);
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
                        name: ids.first().cloned().unwrap_or_else(|| "_".to_string()),
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
            ExprKind::Record(fs) => fs.iter().any(|f| used_in(&f.value, target)),
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
            | PatternKind::Char(_) => false,
            PatternKind::TypeBind { pat, .. } => binds_param(pat, name),
            PatternKind::Var(n) => n == name,
            PatternKind::Tuple(xs) => xs.iter().any(|x| binds_param(x, name)),
            PatternKind::List(xs) => xs.iter().any(|x| binds_param(x, name)),
            PatternKind::Record(fs) => fs.iter().any(|f| binds_param(&f.pattern, name)),
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
                        | PatternKind::Char(_) => {}
                        PatternKind::TypeBind { pat, .. } => {
                            collect(pat, outn);
                        }
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
                            for f in fs {
                                collect(&f.pattern, outn);
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
            ExprKind::Record(fs) => {
                for f in fs {
                    walk(&f.value, out);
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

pub fn analyze_unused_let_bindings(expr: &Expr) -> Vec<UnusedLet> {
    let mut out = Vec::new();

    fn binds_name(p: &Pattern, name: &str) -> bool {
        match &p.kind {
            PatternKind::Var(n) => n == name,
            PatternKind::TypeBind { pat, .. } => binds_name(pat, name),
            PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                xs.iter().any(|x| binds_name(x, name))
            }
            PatternKind::Record(fs) => fs.iter().any(|f| binds_name(&f.pattern, name)),
            PatternKind::Ctor { args, .. } => args.iter().any(|a| binds_name(a, name)),
            PatternKind::Cons(h, t) => binds_name(h, name) || binds_name(t, name),
            PatternKind::As(a, b) => binds_name(a, name) || binds_name(b, name),
            _ => false,
        }
    }

    fn used_in(e: &Expr, target: &str) -> bool {
        match &e.kind {
            ExprKind::Ref(n) => n == target,
            ExprKind::Lambda { param, body } => {
                // Shadowing by lambda param prevents counting inner uses
                !binds_name(param, target) && used_in(body, target)
            }
            ExprKind::Apply { func, arg } => used_in(func, target) || used_in(arg, target),
            ExprKind::Annot { expr, .. } => used_in(expr, target),
            ExprKind::Raise(inner) => used_in(inner, target),
            ExprKind::OrElse { left, right }
            | ExprKind::AltLambda { left, right }
            | ExprKind::Catch { left, right } => used_in(left, target) || used_in(right, target),
            ExprKind::Block(inner) => used_in(inner, target),
            ExprKind::List(xs) => xs.iter().any(|x| used_in(x, target)),
            ExprKind::Record(fs) => fs.iter().any(|f| used_in(&f.value, target)),
            ExprKind::LetGroup { bindings, body, .. } => {
                let in_body = used_in(body, target);
                let in_bindings = bindings.iter().any(|(_p, ex)| used_in(ex, target));
                in_body || in_bindings
            }
            ExprKind::TypeVal(_) => false,
            _ => false,
        }
    }

    fn collect_vars(p: &Pattern, outn: &mut Vec<(String, Span)>) {
        match &p.kind {
            PatternKind::Var(n) => outn.push((n.clone(), p.span)),
            PatternKind::TypeBind { pat, .. } => collect_vars(pat, outn),
            PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                for x in xs {
                    collect_vars(x, outn);
                }
            }
            PatternKind::Record(fs) => {
                for f in fs {
                    collect_vars(&f.pattern, outn);
                }
            }
            PatternKind::Ctor { args, .. } => {
                for a in args {
                    collect_vars(a, outn);
                }
            }
            PatternKind::Cons(h, t) => {
                collect_vars(h, outn);
                collect_vars(t, outn);
            }
            PatternKind::As(a, b) => {
                collect_vars(a, outn);
                collect_vars(b, outn);
            }
            _ => {}
        }
    }

    fn walk(e: &Expr, out: &mut Vec<UnusedLet>) {
        match &e.kind {
            ExprKind::LetGroup { bindings, body, .. } => {
                // gather all bound names in this group
                let mut names = Vec::new();
                for (p, _ex) in bindings.iter() {
                    collect_vars(p, &mut names);
                }
                // check usage across body and all binding expressions
                for (n, sp) in names {
                    let mut used = used_in(body, &n);
                    if !used {
                        for (_p, ex) in bindings.iter() {
                            if used_in(ex, &n) {
                                used = true;
                                break;
                            }
                        }
                    }
                    if !used {
                        out.push(UnusedLet { name: n, binding_span: sp });
                    }
                }
                // recurse
                for (_p, ex) in bindings.iter() {
                    walk(ex, out);
                }
                walk(body, out);
            }
            ExprKind::Annot { expr, .. } => walk(expr, out),
            ExprKind::Lambda { body, .. } => walk(body, out),
            ExprKind::Apply { func, arg } => {
                walk(func, out);
                walk(arg, out);
            }
            ExprKind::List(xs) => {
                for x in xs {
                    walk(x, out);
                }
            }
            ExprKind::Record(fs) => {
                for f in fs {
                    walk(&f.value, out);
                }
            }
            ExprKind::Raise(inner) => walk(inner, out),
            ExprKind::OrElse { left, right }
            | ExprKind::AltLambda { left, right }
            | ExprKind::Catch { left, right } => {
                walk(left, out);
                walk(right, out);
            }
            ExprKind::Block(inner) => walk(inner, out),
            _ => {}
        }
    }

    walk(expr, &mut out);
    out
}

pub fn analyze_let_collisions(expr: &Expr) -> Vec<LetCollision> {
    let mut out = Vec::new();

    fn collect_vars(p: &Pattern, outn: &mut Vec<String>) {
        match &p.kind {
            PatternKind::Var(n) => outn.push(n.clone()),
            PatternKind::TypeBind { pat, .. } => collect_vars(pat, outn),
            PatternKind::Tuple(xs) | PatternKind::List(xs) => {
                for x in xs {
                    collect_vars(x, outn);
                }
            }
            PatternKind::Record(fs) => {
                for f in fs {
                    collect_vars(&f.pattern, outn);
                }
            }
            PatternKind::Ctor { args, .. } => {
                for a in args {
                    collect_vars(a, outn);
                }
            }
            PatternKind::Cons(h, t) => {
                collect_vars(h, outn);
                collect_vars(t, outn);
            }
            PatternKind::As(a, b) => {
                collect_vars(a, outn);
                collect_vars(b, outn);
            }
            _ => {}
        }
    }

    fn walk(e: &Expr, out: &mut Vec<LetCollision>) {
        match &e.kind {
            ExprKind::LetGroup { bindings, body, .. } => {
                let mut seen = std::collections::HashMap::<String, usize>::new();
                let mut duped = std::collections::HashSet::<String>::new();
                for (p, _ex) in bindings.iter() {
                    let mut names = Vec::new();
                    collect_vars(p, &mut names);
                    for n in names {
                        let c = seen.entry(n.clone()).or_insert(0);
                        *c += 1;
                        if *c > 1 {
                            duped.insert(n);
                        }
                    }
                }
                for n in duped {
                    out.push(LetCollision { name: n, group_span: e.span });
                }
                for (_p, ex) in bindings.iter() {
                    walk(ex, out);
                }
                walk(body, out);
            }
            ExprKind::Annot { expr, .. } => walk(expr, out),
            ExprKind::Lambda { body, .. } => walk(body, out),
            ExprKind::Apply { func, arg } => {
                walk(func, out);
                walk(arg, out);
            }
            ExprKind::List(xs) => {
                for x in xs {
                    walk(x, out);
                }
            }
            ExprKind::Record(fs) => {
                for f in fs {
                    walk(&f.value, out);
                }
            }
            ExprKind::Raise(inner) => walk(inner, out),
            ExprKind::OrElse { left, right }
            | ExprKind::AltLambda { left, right }
            | ExprKind::Catch { left, right } => {
                walk(left, out);
                walk(right, out);
            }
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
        let d = analyze_duplicates(&e, AnalyzeOptions { min_size: 3, min_count: 2 });
        assert!(d.iter().any(|f| f.count >= 2 && f.size >= 3));
    }

    #[test]
    fn unused_param_with_typebind_pattern() {
        // \%{ %a } ~x -> 1  => x is unused
        let e = parse_expr("(\\%{ %a } ~x -> 1)").unwrap();
        let issues = analyze_unused_params(&e);
        assert!(issues.iter().any(|u| u.name == "x"));
    }

    #[test]
    fn shadowing_with_typebind_param() {
        // \\~x -> (\\%{ %a } ~x -> ~x) ~x  => inner lambda's x shadows the outer one
        let e = parse_expr("(\\~x -> (\\%{ %a } ~x -> ~x) ~x)").unwrap();
        let issues = analyze_shadowing(&e);
        assert!(issues.iter().any(|s| s.name == "x"));
    }
}
