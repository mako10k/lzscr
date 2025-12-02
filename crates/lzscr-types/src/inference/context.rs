//! Inference context and helper functions for type inference.
//!
//! This module provides:
//! - `InferCtx`: The main context structure for type inference
//! - `DebugConfig`: Optional debugging configuration
//! - Helper functions for type variable management

use lzscr_ast::ast::Pattern;
use lzscr_ast::span::Span;
use std::collections::HashMap;

use crate::display::pp_type;
use crate::scheme::{Scheme, TvGen, TypeEnv};
use crate::typeexpr::{TypeDefsFrame, TypeNameDefsFrame};
use crate::types::{TvId, Type};

/// Inference context holding type variables, environment, and typedefs.
pub(crate) struct InferCtx {
    pub(crate) tv: TvGen,
    pub(crate) env: TypeEnv,
    pub(crate) tyvars: Vec<HashMap<String, TvId>>, // stack of frames; lookup is from last to first
    pub(crate) typedefs: Vec<TypeDefsFrame>,       // stack of ctor templates from % declarations
    pub(crate) typedef_types: Vec<TypeNameDefsFrame>, // stack of named type defs (μ-types)
    pub(crate) debug: Option<std::rc::Rc<std::cell::RefCell<DebugConfig>>>, // optional debugging
    pub(crate) depth: usize,                       // current AST depth for logging
    pub(crate) tv_origins: HashMap<TvId, Span>,    // origin span for each generated type variable
    pub(crate) span_stack: Vec<Span>,              // expression span context stack
}

/// Debug configuration for inference logging.
#[derive(Clone)]
pub(crate) struct DebugConfig {
    pub(crate) level: usize,
    pub(crate) max_depth: usize,
    pub(crate) logs: Vec<String>,
    pub(crate) log_unify: bool,
    pub(crate) log_env: bool,
    pub(crate) log_schemes: bool,
}

impl DebugConfig {
    pub(crate) fn log(&mut self, depth: usize, lvl: usize, msg: impl Into<String>) {
        if lvl <= self.level && depth <= self.max_depth {
            self.logs.push(format!("[d{depth} l{lvl}] {}", msg.into()));
        }
    }

    pub(crate) fn dump_env(&mut self, depth: usize, env: &TypeEnv) {
        if self.log_env && depth <= self.max_depth {
            let mut entries: Vec<_> = env.0.iter().collect();
            entries.sort_by(|a, b| a.0.cmp(b.0));
            for (k, sc) in entries.into_iter().take(64) {
                self.logs.push(format!("[d{depth} env] {} : {}", k, pp_type(&sc.ty)));
            }
        }
    }

    pub(crate) fn log_scheme(&mut self, depth: usize, name: &str, sc: &Scheme) {
        if self.log_schemes && depth <= self.max_depth {
            self.logs.push(format!("[d{depth} scheme] {} :: {}", name, pp_type(&sc.ty)));
        }
    }
}

/// Look up a type variable by name in the context's tyvar stack.
pub(crate) fn lookup_tyvar(ctx: &InferCtx, name: &str) -> Option<TvId> {
    for frame in ctx.tyvars.iter().rev() {
        if let Some(id) = frame.get(name) {
            return Some(*id);
        }
    }
    None
}

impl InferCtx {
    /// Push an expression span onto the context stack.
    pub(crate) fn push_span(&mut self, sp: Span) {
        self.span_stack.push(sp);
    }

    /// Pop an expression span from the context stack.
    pub(crate) fn pop_span(&mut self) {
        self.span_stack.pop();
    }

    /// Get the current expression span.
    pub(crate) fn current_span(&self) -> Option<Span> {
        self.span_stack.last().copied()
    }

    /// Generate a fresh type variable with current span tracking.
    pub(crate) fn fresh_tv(&mut self) -> Type {
        let t = self.tv.fresh();
        if let Type::Var(id) = t {
            if let Some(sp) = self.current_span() {
                self.tv_origins.insert(id, sp);
            }
        }
        t
    }
}

/// Push type variable bindings from a pattern's TypeBind wrappers.
/// Returns the innermost pattern and the number of frames pushed.
pub(crate) fn push_tyvars_from_pattern<'a>(
    ctx: &mut InferCtx,
    p: &'a Pattern,
) -> (&'a Pattern, usize) {
    let mut cur = p;
    let mut pushed = 0usize;
    while let lzscr_ast::ast::PatternKind::TypeBind { tvars, pat } = &cur.kind {
        let mut frame = HashMap::new();
        for nm in tvars {
            // '' (empty) represents anonymous '?', we still allocate a distinct id but not bound to a name for lookup
            if nm.is_empty() {
                continue;
            }
            let Type::Var(id) = ctx.tv.fresh() else { unreachable!() }; // keep raw fresh: type bind generics shouldn't inherit expr span
            frame.insert(nm.clone(), id);
        }
        ctx.tyvars.push(frame);
        pushed += 1;
        cur = pat;
    }
    (cur, pushed)
}

/// Pop n type variable frames from the context.
pub(crate) fn pop_tyvars(ctx: &mut InferCtx, n: usize) {
    for _ in 0..n {
        let _ = ctx.tyvars.pop();
    }
}
