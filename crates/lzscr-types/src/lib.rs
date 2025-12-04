//! lzscr-types: HM type inference, unification, schemes, diagnostics (occurs/mismatch).
//!
//! Status:
//! - Stable: unify/instantiate/generalize, occurs-check, basic pretty print
//! - Improving: dual-caret errors (MismatchBoth/RecordField…), occurs normalization
//!
//! Notes:
//! - apply(): single-pass structural substitution（安価）
//! - zonk(): 置換連鎖を固定点まで展開（最終出力/表示前に使用）
//! - ftv(): 自由型変数の収集（必要に応じて apply/zonk 後に）
//!
//! Source of truth: docs/ROADMAP.md
//!
//! Implements a minimal rank-1 HM inference over lzscr AST, including AltLambda
//! typing with Ctor-limited union (SumCtor) for lambda chains.
//!
//! # Module Structure (Refactored 2025-12-02)
//!
//! This crate has been split into multiple focused modules:
//! - `types`: Core type representations (`Type`, `TvId`)
//! - `error`: Type errors and suggestion helpers (`TypeError`, `edit_distance`)
//! - `builtins`: Built-in type constructors (bool, option, result, fs effects)
//! - `scheme`: Type schemes, substitutions, environments (Scheme, Subst, TypeEnv)
//! - `typeexpr`: Type expression conversion and typedef management
//! - `unification`: Type unification algorithm (Robinson's algorithm with extensions)
//! - Other modules to be extracted: inference, display

mod builtins;
mod diagnostic;
mod display;
mod error;
mod inference;
mod scheme;
mod typeexpr;
mod types;
mod unification;

// Re-export core types
pub use builtins::{
    bool_sum_type, effect_signature, fs_effects_record_type, fs_metadata_record_type, option_type,
    result_list_str_type, result_metadata_type, result_str_str_type, result_unit_str_type,
};
pub use diagnostic::{DiagnosticSpan, DualSpan, SpanContext, SpanOrigin};
use display::{normalize_type_and_map, pp_type, user_pretty_type, user_pretty_type_and_map};
pub use error::{
    find_similar_names, format_field_path, suggest_fixes_for_mismatch,
    suggest_fixes_for_record_field, TypeError,
};
use inference::{infer_expr, DebugConfig, InferCtx};
pub use scheme::{
    generalize, instantiate, normalize_tuples, zonk_type, Scheme, Subst, TvGen, TypeEnv, TypesApply,
};
pub use typeexpr::{
    build_typedefs_frame, build_typename_frame, conv_typeexpr_fresh, validate_typedecls_positive,
    TypeDefsFrame, TypeNameDef, TypeNameDefsFrame,
};
pub use types::{TvId, Type};

use lzscr_ast::ast::*;
use std::collections::{BTreeMap, HashMap};

// ---------- Inference ----------

// ---------- Pretty Printing ----------
// pp_type: low-level, stable, developer-oriented printer.
//   - Raw type variables shown as %tN (internal ids) for debugging / legacy tests.
//   - No %{ } wrapper, no variable renaming, no cycle guard beyond recursion.
// user_pretty_type (below): user-facing normalized printer.
//   - Performs zonk beforehand at call sites.
//   - Deterministic renaming %a, %b, ... and wraps in "%{ ... }" for visual distinction.
//   - Keeps mapping (via user_pretty_type_and_map) for consistent occurs diagnostics.
// Guidelines:
//   * Use pp_type inside logs / debug output / legacy mode.
//   * Use user_pretty_type for CLI surface and error messages exposed to end users (pretty mode).
//   * Do not mix both in one diagnostic line to avoid confusing two naming domains.
// (Display functions moved to display/ module)

// ---------- Public API ----------

pub mod api {
    use super::*;
    use lzscr_parser::parse_expr;
    /// オプション指定で推論結果文字列のフォーマットを切り替えるための設定。
    #[derive(Debug, Clone, Copy, Default)]
    pub struct InferOptions {
        /// true ならユーザ向け pretty 表示 (%{ ... } + 型変数 %a,%b など)。
        /// false ならレガシー表示 (テスト互換: %tN 生の変数, ラッパ無し)。
        pub pretty: bool,
    }

    /// 下位互換 (レガシー) API: 既存テストが期待するフォーマット (pretty=false)。
    pub fn infer_program(src: &str) -> Result<String, String> {
        infer_program_with_opts(src, InferOptions { pretty: false })
    }

    /// 新 API: フォーマット指定付き。将来はこちらを安定化させる。
    pub fn infer_program_with_opts(src: &str, opts: InferOptions) -> Result<String, String> {
        let ast = parse_expr(src).map_err(|e| format!("parse error: {e}"))?;
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        match infer_expr(&mut ctx, &ast, false) {
            Ok((t, s)) => {
                let zonked = zonk_type(&t.apply(&s), &s);
                if opts.pretty {
                    Ok(user_pretty_type(&zonked))
                } else {
                    Ok(pp_type(&zonked))
                }
            }
            Err(e) => Err(format!("{e}")),
        }
    }

    // Prefer this from tools that already have an AST with precise spans (e.g., CLI after ~require expansion).
    #[allow(clippy::result_large_err)]
    pub fn infer_ast(ast: &Expr) -> Result<String, super::TypeError> {
        infer_ast_with_opts(ast, InferOptions { pretty: true })
    }

    /// オプション付き AST 推論。`infer_ast` は pretty=true の糖衣。
    #[allow(clippy::result_large_err)]
    pub fn infer_ast_with_opts(ast: &Expr, opts: InferOptions) -> Result<String, super::TypeError> {
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: None,
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let zonked = zonk_type(&t.apply(&s), &s);
        if opts.pretty {
            Ok(user_pretty_type(&zonked))
        } else {
            Ok(pp_type(&zonked))
        }
    }

    // Debug variant returning (type, logs)
    #[allow(clippy::result_large_err)]
    pub fn infer_ast_debug(
        ast: &Expr,
        level: usize,
        max_depth: usize,
    ) -> Result<(String, Vec<String>), super::TypeError> {
        let dbg = DebugConfig {
            level,
            max_depth,
            logs: Vec::new(),
            log_unify: true,
            log_env: false,
            log_schemes: true,
        };
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        // Log initial environment keys only if env logging enabled
        if let Some(dbg) = &ctx.debug {
            if dbg.borrow().log_env {
                let mut keys: Vec<_> = ctx.env.0.keys().cloned().collect();
                keys.sort();
                let preview: Vec<_> = keys.into_iter().take(64).collect();
                dbg.borrow_mut().log(0, 1, format!("initial env keys={:?}", preview));
            }
        }
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let ty = user_pretty_type(&t.apply(&s));
        let logs = ctx.debug.as_ref().unwrap().borrow().logs.clone();
        Ok((ty, logs))
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_ast_debug_with(
        ast: &Expr,
        level: usize,
        max_depth: usize,
        log_unify: bool,
        log_env: bool,
        log_schemes: bool,
    ) -> Result<(String, Vec<String>), super::TypeError> {
        let dbg =
            DebugConfig { level, max_depth, logs: Vec::new(), log_unify, log_env, log_schemes };
        let mut ctx = InferCtx {
            tv: TvGen::new(),
            env: prelude_env(),
            tyvars: vec![],
            typedefs: vec![],
            typedef_types: vec![],
            debug: Some(std::rc::Rc::new(std::cell::RefCell::new(dbg))),
            depth: 0,
            tv_origins: HashMap::new(),
            span_stack: Vec::new(),
        };
        let (t, s) = infer_expr(&mut ctx, ast, false)?;
        let ty = user_pretty_type(&t.apply(&s));
        let logs = ctx.debug.as_ref().unwrap().borrow().logs.clone();
        Ok((ty, logs))
    }

    fn prelude_env() -> TypeEnv {
        let mut env = TypeEnv::new();
        // Minimal builtins required by tests; most tests use pure lambdas.
        // Add 'alt' for completeness: forall a r. (a->r)->(a->r)->a->r
        let a = TvId(1000);
        let r = TvId(1001);
        let alt_ty = Type::fun(
            Type::fun(Type::Var(a), Type::Var(r)),
            Type::fun(Type::fun(Type::Var(a), Type::Var(r)), Type::fun(Type::Var(a), Type::Var(r))),
        );
        env.insert("alt".into(), Scheme { vars: vec![a, r], ty: alt_ty });
        // add : Int -> Int -> Int
        let add_ty = Type::fun(Type::Int, Type::fun(Type::Int, Type::Int));
        env.insert("add".into(), Scheme { vars: vec![], ty: add_ty });
        // Boolean ops (approximate): and/or : Bool -> Bool -> Bool ; not : Bool -> Bool
        // and/or : Bool -> Bool -> Bool
        let bool_t = bool_sum_type();
        env.insert(
            "and".into(),
            Scheme {
                vars: vec![],
                ty: Type::fun(bool_t.clone(), Type::fun(bool_t.clone(), bool_t.clone())),
            },
        );
        env.insert(
            "or".into(),
            Scheme {
                vars: vec![],
                ty: Type::fun(bool_t.clone(), Type::fun(bool_t.clone(), bool_t.clone())),
            },
        );
        env.insert(
            "not".into(),
            Scheme { vars: vec![], ty: Type::fun(bool_t.clone(), bool_t.clone()) },
        );
        // boolean values
        // seq : forall a b. a -> b -> b
        let a2 = TvId(1002);
        let b2 = TvId(1003);
        let seq_ty = Type::fun(Type::Var(a2), Type::fun(Type::Var(b2), Type::Var(b2)));
        env.insert("seq".into(), Scheme { vars: vec![a2, b2], ty: seq_ty });
        // chain : forall a b. a -> b -> b
        let a4 = TvId(1006);
        let b4 = TvId(1007);
        let chain_ty = Type::fun(Type::Var(a4), Type::fun(Type::Var(b4), Type::Var(b4)));
        env.insert("chain".into(), Scheme { vars: vec![a4, b4], ty: chain_ty });
        // bind : forall x r. x -> (x -> r) -> r
        let x5 = TvId(1008);
        let r5 = TvId(1009);
        let bind_ty = Type::fun(
            Type::Var(x5),
            Type::fun(Type::fun(Type::Var(x5), Type::Var(r5)), Type::Var(r5)),
        );
        env.insert("bind".into(), Scheme { vars: vec![x5, r5], ty: bind_ty });
        // effects : forall s a. s -> a -> Unit  (approximate; first arg is an effect symbol)
        let s3 = TvId(1004);
        let a3 = TvId(1005);
        let eff_ty = Type::fun(Type::Var(s3), Type::fun(Type::Var(a3), Type::Unit));
        env.insert("effects".into(), Scheme { vars: vec![s3, a3], ty: eff_ty });

        // Arithmetic and comparison commonly used in prelude
        env.insert(
            "add".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert(
            "sub".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert(
            "mul".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)) },
        );
        env.insert("eq".into(), {
            let a = TvId(1010);
            Scheme {
                vars: vec![a],
                ty: Type::fun(Type::Var(a), Type::fun(Type::Var(a), bool_t.clone())),
            }
        });
        env.insert(
            "lt".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "le".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "gt".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );
        env.insert(
            "ge".into(),
            Scheme { vars: vec![], ty: Type::fun(Type::Int, Type::fun(Type::Int, bool_t.clone())) },
        );

        // cons : forall a. a -> List a -> List a
        let a_cons = TvId(1012);
        let cons_ty = Type::fun(
            Type::Var(a_cons),
            Type::fun(
                Type::List(Box::new(Type::Var(a_cons))),
                Type::List(Box::new(Type::Var(a_cons))),
            ),
        );
        env.insert("cons".into(), Scheme { vars: vec![a_cons], ty: cons_ty });

        // Common list functions (if stdlib not already providing before top-level expression inference)
        // append : forall a. [a] -> [a] -> [a]
        if !env.0.contains_key("append") {
            let a_app = TvId(2000);
            let app_ty = Type::fun(
                Type::List(Box::new(Type::Var(a_app))),
                Type::fun(
                    Type::List(Box::new(Type::Var(a_app))),
                    Type::List(Box::new(Type::Var(a_app))),
                ),
            );
            env.insert("append".into(), Scheme { vars: vec![a_app], ty: app_ty });
        }
        // length : forall a. [a] -> Int
        if !env.0.contains_key("length") {
            let a_len = TvId(2001);
            let len_ty = Type::fun(Type::List(Box::new(Type::Var(a_len))), Type::Int);
            env.insert("length".into(), Scheme { vars: vec![a_len], ty: len_ty });
        }
        // map : forall a b. (a -> b) -> [a] -> [b]
        if !env.0.contains_key("map") {
            let a_map = TvId(2002);
            let b_map = TvId(2003);
            let map_ty = Type::fun(
                Type::fun(Type::Var(a_map), Type::Var(b_map)),
                Type::fun(
                    Type::List(Box::new(Type::Var(a_map))),
                    Type::List(Box::new(Type::Var(b_map))),
                ),
            );
            env.insert("map".into(), Scheme { vars: vec![a_map, b_map], ty: map_ty });
        }
        // reverse : forall a. [a] -> [a]
        if !env.0.contains_key("reverse") {
            let a_rev = TvId(2004);
            let rev_ty = Type::fun(
                Type::List(Box::new(Type::Var(a_rev))),
                Type::List(Box::new(Type::Var(a_rev))),
            );
            env.insert("reverse".into(), Scheme { vars: vec![a_rev], ty: rev_ty });
        }

        // to_str : forall a. a -> Str
        let a_ts = TvId(1013);
        let to_str_ty = Type::fun(Type::Var(a_ts), Type::Str);
        env.insert("to_str".into(), Scheme { vars: vec![a_ts], ty: to_str_ty });

        // if : forall a. Bool -> a -> a -> a   (Bool in union form)
        let a_if = TvId(1011);
        let if_ty = Type::fun(
            bool_t.clone(),
            Type::fun(Type::Var(a_if), Type::fun(Type::Var(a_if), Type::Var(a_if))),
        );
        env.insert("if".into(), Scheme { vars: vec![a_if], ty: if_ty });

        // Builtins record with nested namespaces used by stdlib/prelude.lzscr
        fn record(fields: Vec<(&str, Type)>) -> Type {
            let mut m = BTreeMap::new();
            for (k, v) in fields {
                m.insert(k.to_string(), v);
            }
            Type::Record(m.into_iter().map(|(k, v)| (k, (v, None))).collect())
        }
        // String namespace
        let string_ns = record(vec![
            ("len", Type::fun(Type::Str, Type::Int)),
            ("concat", Type::fun(Type::Str, Type::fun(Type::Str, Type::Str))),
            ("slice", Type::fun(Type::Str, Type::fun(Type::Int, Type::fun(Type::Int, Type::Str)))),
            // char_at : Str -> Int -> (Some Char | None)
            (
                "char_at",
                Type::fun(
                    Type::Str,
                    Type::fun(
                        Type::Int,
                        Type::SumCtor(vec![
                            ("Some".into(), vec![Type::Char]),
                            ("None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
        ]);
        // Math namespace (minimal for now)
        let math_ns = record(vec![("abs", Type::fun(Type::Int, Type::Int))]);
        // Char namespace
        let char_ns = record(vec![
            ("is_digit", Type::fun(Type::Char, bool_t.clone())),
            ("is_alpha", Type::fun(Type::Char, bool_t.clone())),
            ("is_alnum", Type::fun(Type::Char, bool_t.clone())),
            ("is_space", Type::fun(Type::Char, bool_t.clone())),
            // between : Char -> Char -> Char -> Bool-like (code point range using chars)
            (
                "between",
                Type::fun(Type::Char, Type::fun(Type::Char, Type::fun(Type::Char, bool_t.clone()))),
            ),
        ]);
        // Unicode namespace
        let unicode_ns = record(vec![
            ("to_int", Type::fun(Type::Char, Type::Int)),
            ("of_int", Type::fun(Type::Int, Type::Char)),
        ]);
        // Scan namespace
        let scan_state = record(vec![
            ("dummy", Type::Unit), // placeholder field; state is opaque
        ]);
        let scan_ns = record(vec![
            ("new", Type::fun(Type::Str, scan_state.clone())),
            // eof returns a symbol union used in AltLambda patterns: True | False
            (
                "eof",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![("True".into(), vec![]), ("False".into(), vec![])]),
                ),
            ),
            ("pos", Type::fun(scan_state.clone(), Type::Int)),
            ("set_pos", Type::fun(scan_state.clone(), Type::fun(Type::Int, scan_state.clone()))),
            // peek : ScanState -> (Some Char | None)
            (
                "peek",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![("Some".into(), vec![Type::Char]), ("None".into(), vec![])]),
                ),
            ),
            // next : ScanState -> (Some (Char, Scan) | None)
            (
                "next",
                Type::fun(
                    scan_state.clone(),
                    Type::SumCtor(vec![
                        (
                            "Some".into(),
                            vec![Type::Ctor {
                                // Todo: Should be tuple constructor helper?
                                tag: ",".into(),
                                payload: vec![Type::Char, scan_state.clone()],
                            }],
                        ),
                        ("None".into(), vec![]),
                    ]),
                ),
            ),
            // take_if : (Char -> Bool-like) -> ScanState -> (Some (., Char Scan) | None)
            (
                "take_if",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::SumCtor(vec![
                            (
                                "Some".into(),
                                vec![Type::Ctor {
                                    tag: ",".into(),
                                    payload: vec![Type::Char, scan_state.clone()],
                                }],
                            ),
                            ("None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
            // take_while : (Char -> Bool-like) -> ScanState -> (., Str Scan)
            (
                "take_while",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::Ctor {
                            tag: ",".into(),
                            payload: vec![Type::Str, scan_state.clone()],
                        },
                    ),
                ),
            ),
            // take_while1 : (Char -> Bool-like) -> ScanState -> (Some (., Str Scan) | None)
            (
                "take_while1",
                Type::fun(
                    Type::fun(Type::Char, bool_t.clone()),
                    Type::fun(
                        scan_state.clone(),
                        Type::SumCtor(vec![
                            (
                                "Some".into(),
                                vec![Type::Ctor {
                                    tag: ",".into(),
                                    payload: vec![Type::Str, scan_state.clone()],
                                }],
                            ),
                            ("None".into(), vec![]),
                        ]),
                    ),
                ),
            ),
            (
                "slice_span",
                Type::fun(
                    scan_state.clone(),
                    Type::fun(Type::Int, Type::fun(Type::Int, Type::Str)),
                ),
            ),
        ]);
        let builtins_ty = record(vec![
            ("string", string_ns),
            ("math", math_ns),
            ("char", char_ns),
            ("unicode", unicode_ns),
            ("scan", scan_ns),
        ]);
        env.insert("Builtins".into(), Scheme { vars: vec![], ty: builtins_ty });
        env
    }
}

#[cfg(test)]
mod tests {
    use super::api::infer_program;

    #[test]
    fn infer_char_literal() {
        let t = infer_program("'x'").unwrap();
        assert_eq!(t, ".Char");
    }
}
