use ahash::AHashMap;
use lzscr_ast::ast::*;
use lzscr_ast::span::Span;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
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
            | ExprKind::Str(_)
            | ExprKind::Ref(_)
            | ExprKind::Symbol(_) => 1,
            ExprKind::Lambda { body, .. } => 1 + walk(body, tbl),
            ExprKind::Apply { func, arg } => 1 + walk(func, tbl) + walk(arg, tbl),
            ExprKind::Block(inner) => 1 + walk(inner, tbl),
        };
        // very simple structural repr (not fully unique but works for heuristic)
        let repr = match &e.kind {
            ExprKind::Unit => "()".to_string(),
            ExprKind::Int(n) => format!("i:{n}"),
            ExprKind::Str(s) => format!("s:\"{}\"", s),
            ExprKind::Ref(n) => format!("r:{n}"),
            ExprKind::Symbol(s) => format!("y:{s}"),
            ExprKind::Lambda { param, body } => format!("lam {param} -> [{}]", body.span.len),
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

#[derive(Debug, Clone)]
pub struct UnboundRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Shadowing {
    pub name: String,
    pub lambda_span: Span,
}

#[derive(Debug, Clone)]
pub struct UnusedParam {
    pub name: String,
    pub lambda_span: Span,
}

pub fn default_allowlist() -> HashSet<String> {
    // Builtins available via ~name
    ["to_str", "add", "sub", "eq", "lt", "seq", "effects"]
        .iter()
        .map(|s| s.to_string())
        .collect()
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
            ExprKind::Unit | ExprKind::Int(_) | ExprKind::Str(_) | ExprKind::Symbol(_) => {}
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
                scopes.push(HashSet::from([param.clone()]));
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
                let is_shadow = scopes.iter().any(|s| s.contains(param));
                if is_shadow {
                    out.push(Shadowing {
                        name: param.clone(),
                        lambda_span: e.span,
                    });
                }
                let mut top = scopes.last().cloned().unwrap_or_default();
                top.insert(param.clone());
                scopes.push(top);
                walk(body, scopes, out);
                scopes.pop();
            }
            ExprKind::Apply { func, arg } => {
                walk(func, scopes, out);
                walk(arg, scopes, out);
            }
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
            ExprKind::Lambda { param, body } => {
                if param == target {
                    false
                } else {
                    used_in(body, target)
                }
            }
            ExprKind::Apply { func, arg } => used_in(func, target) || used_in(arg, target),
            ExprKind::Block(inner) => used_in(inner, target),
            _ => false,
        }
    }
    fn walk(e: &Expr, out: &mut Vec<UnusedParam>) {
        match &e.kind {
            ExprKind::Lambda { param, body } => {
                if !used_in(body, param) {
                    out.push(UnusedParam {
                        name: param.clone(),
                        lambda_span: e.span,
                    });
                }
                walk(body, out);
            }
            ExprKind::Apply { func, arg } => {
                walk(func, out);
                walk(arg, out);
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
