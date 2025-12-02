# lzscr-types Refactoring: Detailed Analysis and Strategy

**Date**: 2025-12-02
**Branch**: refactor/phase1-module-splitting
**Current file size**: 3438 lines (must split to <500 lines per module)

## 1. Current Code Structure Analysis

### Public API Surface (must preserve)

```rust
// Core types
pub struct TvId(pub u32);
pub enum Type { ... }  // 15 variants
pub struct Scheme { pub vars: Vec<TvId>, pub ty: Type }
pub enum TypeError { ... }  // 10+ variants

// Hidden but used by other crates
pub struct Subst(HashMap<TvId, Type>);  // via pub(crate) methods
pub struct TypeEnv(HashMap<String, Scheme>);  // via pub(crate) methods

// Main entry points (used by lzscr-cli, lzscr-analyzer)
pub fn infer_ast(ast: &Ast) -> Result<(Type, DebugInfo), TypeError>
pub fn infer_ast_with_opts(ast: &Ast, opts: InferOptions) -> Result<(Type, DebugInfo), TypeError>

// Display functions (used by lzscr-cli)
fn user_pretty_type(t: &Type) -> String  // should be pub
fn pp_type(t: &Type) -> String  // legacy display
```

### Internal Function Dependencies (line ranges)

**Layer 1: Core type constructors (61-119)**
- `bool_sum_type()`, `option_type()`, `result_*_type()`, `fs_*_type()`
- No dependencies (only construct Type variants)
- **Target module**: `builtins.rs`

**Layer 2: Effect signatures (119-125)**
- `effect_signature(sym: &str) -> Option<Type>`
- Depends on: Layer 1 functions
- **Target module**: `builtins.rs`

**Layer 3: Subst operations (133-189)**
- `Subst::new()`, `singleton()`, `compose()`
- Depends on: Type (apply trait)
- **Target module**: `scheme.rs` (with Type trait implementations)

**Layer 4: Type traversals (190-394)**
- `zonk_type()`, `normalize_tuples()`, Type trait impls (apply, ftv, etc.)
- Depends on: Type, Subst
- **Target module**: Split between `types.rs` (traits) and `scheme.rs` (traversals)

**Layer 5: Type declarations (395-456)**
- `build_typedefs_frame()`, `typedefs_lookup_ctor()`, etc.
- Depends on: Type, AST
- **Target module**: `typeexpr.rs`

**Layer 6: TypeError and helpers (457-590)**
- `TypeError` enum (10 variants with rich span data)
- `edit_distance()`, `find_similar_names()`
- Depends on: Type, Span
- **Target module**: `error.rs`

**Layer 7: Unification (591-1077)**
- `occurs()`, `unify()`, `unify_slices()`, `ctx_unify()`, `bind()`
- Depends on: Type, Subst, TypeError, TypeContext
- **Target module**: `unification.rs`

**Layer 8: Type environment (1078-1116)**
- `TypeEnv`, `instantiate()`, `generalize()`
- Depends on: Type, Scheme, Subst
- **Target module**: `scheme.rs`

**Layer 9: Pattern inference (1117-1310)**
- `infer_pattern()`
- Depends on: TypeEnv, unify, fresh_tv
- **Target module**: `inference/pattern.rs`

**Layer 10: Type variable scopes (1311-1370)**
- `lookup_tyvar()`, `push_tyvars_from_pattern()`, `pop_tyvars()`
- Depends on: InferCtx
- **Target module**: `inference/context.rs`

**Layer 11: TypeExpr conversion (1371-1398)**
- `conv_typeexpr_fresh()`
- Depends on: Type, TypeExpr, TvGen
- **Target module**: `typeexpr.rs`

**Layer 12: Expression inference (1399-2361) ⚠️ MASSIVE**
- `infer_expr()` - 963 lines!
- Contains nested helper functions and giant match arms
- Depends on: ALL previous layers
- **Target module**: Split into `inference/expr_*.rs` (see breakdown below)

**Layer 13: Display functions (2362-3438)**
- `short_expr_kind()`, `pp_type()`, `pp_type_with_renaming()`, `user_pretty_type()`, etc.
- Depends on: Type only
- **Target module**: `display/` submodules

### infer_expr() Breakdown (1399-2361, 963 lines)

**Structure analysis:**
```rust
fn infer_expr(ctx: &mut InferCtx, e: &Expr, allow_effects: bool) 
    -> Result<(Type, Subst), TypeError> 
{
    // Nested helper functions (1414-1538): ~124 lines
    - find_ctor_symbol_span_in_expr()
    - find_ctor_symbol_span_in_pattern()
    - conv_typeexpr()  // local version with holes

    // Match on e.kind (1539-2355): ~816 lines
    match &e.kind {
        ExprKind::Unit | Int | Float | Str | Char => // 5 lines
        ExprKind::Ref(n) => // 25 lines (includes UnboundRef error with suggestions)
        
        ExprKind::Lambda { param, body } => // 16 lines
        ExprKind::Apply { func, arg } => // 170 lines (handles if, seq, effects)
            - Special case: effects (eff_ref) // 11 lines
            - Special case: if-then-else // 35 lines
            - Special case: seq (strict effects) // 55 lines
            - Special case: effects in Apply // 35 lines
            - General application // 34 lines
        
        ExprKind::LetGroup { ... } => // 239 lines ⚠️ HUGE
            - Type declarations validation // 50 lines
            - LetGroup pattern binding // 120 lines
            - Wildcard pattern handling // 30 lines
            - Body inference // 39 lines
        
        ExprKind::AltLambda { left, right } => // 247 lines ⚠️ HUGE
            - Flatten chain // 40 lines
            - Process each branch // 80 lines
            - Build SumCtor // 70 lines
            - Unify return types // 57 lines
        
        ExprKind::Block(b) => // 3 lines
        ExprKind::Tuple(es) => // 12 lines
        ExprKind::List(es) => // 17 lines
        ExprKind::Record(fs) => // 20 lines
        ExprKind::Member { obj, field } => // 35 lines
        ExprKind::Symbol(s) => // 40 lines (constructor inference)
        ExprKind::TypeAnnotation { ty, expr } => // 25 lines
        ExprKind::TypeVal(te) => // 8 lines
    }
}
```

**Split strategy for infer_expr:**
1. **inference/expr.rs** (main dispatcher, ~150 lines)
   - Entry point `infer_expr()`
   - Trivial cases: Unit, Int, Float, Str, Char, Block, Tuple, List
   - Delegates complex cases to specialized modules

2. **inference/expr_ref.rs** (~50 lines)
   - `ExprKind::Ref` handling
   - UnboundRef error with Levenshtein suggestions

3. **inference/expr_lambda.rs** (~100 lines)
   - `ExprKind::Lambda` (simple lambda)
   - Helper: pattern type inference integration

4. **inference/expr_apply.rs** (~200 lines)
   - `ExprKind::Apply` general case
   - Special forms: if-then-else, seq, effects
   - Function application unification

5. **inference/expr_letgroup.rs** (~250 lines)
   - `ExprKind::LetGroup` complete logic
   - Type declarations
   - Pattern bindings and generalization

6. **inference/expr_altlambda.rs** (~270 lines)
   - `ExprKind::AltLambda` complete logic
   - Branch flattening
   - SumCtor construction
   - Mixed branch error detection

7. **inference/expr_misc.rs** (~100 lines)
   - `ExprKind::Record`, `Member`, `Symbol`
   - `ExprKind::TypeAnnotation`, `TypeVal`

8. **inference/context.rs** (~150 lines)
   - `InferCtx` struct
   - Type variable scope management
   - Fresh type variable generation

9. **inference/pattern.rs** (~200 lines)
   - `infer_pattern()` (currently at 1117-1310)
   - Pattern type variable binding

## 2. Proposed Module Structure (Final)

```
crates/lzscr-types/src/
├── lib.rs (150 lines)
│   ├── Re-exports: Type, TvId, Scheme, TypeError, TypeEnv
│   ├── Re-exports: infer_ast, infer_ast_with_opts
│   ├── Re-exports: user_pretty_type
│   └── Module declarations
│
├── types.rs (200 lines)
│   ├── pub struct TvId
│   ├── pub enum Type (all 15 variants)
│   ├── impl Type::fun()
│   ├── Display traits
│   └── Basic trait implementations (PartialEq, Clone, etc.)
│
├── scheme.rs (200 lines)
│   ├── pub struct Scheme
│   ├── pub struct Subst
│   ├── pub struct TypeEnv
│   ├── Subst operations (new, singleton, compose)
│   ├── Type traversal traits (Apply, Ftv)
│   ├── zonk_type(), normalize_tuples()
│   ├── instantiate(), generalize()
│   └── TypeEnv methods
│
├── error.rs (250 lines)
│   ├── pub enum TypeError (10+ variants)
│   ├── edit_distance()
│   ├── find_similar_names()
│   ├── format_field_path()
│   └── Error helper functions
│
├── builtins.rs (150 lines)
│   ├── bool_sum_type()
│   ├── option_type()
│   ├── result_*_type() (5 functions)
│   ├── fs_*_type() (2 functions)
│   └── effect_signature()
│
├── unification.rs (350 lines)
│   ├── occurs()
│   ├── unify() (main function, ~170 lines)
│   ├── unify_slices()
│   ├── ctx_unify()
│   ├── bind()
│   ├── check_positive_occurrence()
│   └── validate_typedecls_positive()
│
├── typeexpr.rs (200 lines)
│   ├── conv_typeexpr_fresh()
│   ├── conv_typeexpr_with_subst()
│   ├── build_typedefs_frame()
│   ├── typedefs_lookup_ctor()
│   ├── build_typename_frame()
│   ├── typedefs_lookup_typename()
│   └── instantiate_named_sum()
│
├── inference/
│   ├── mod.rs (100 lines)
│   │   ├── pub fn infer_ast()
│   │   ├── pub fn infer_ast_with_opts()
│   │   ├── InferOptions struct
│   │   └── DebugInfo struct
│   │
│   ├── context.rs (150 lines)
│   │   ├── pub(crate) struct InferCtx
│   │   ├── TvGen (type variable generator)
│   │   ├── lookup_tyvar()
│   │   ├── push_tyvars_from_pattern()
│   │   └── pop_tyvars()
│   │
│   ├── pattern.rs (200 lines)
│   │   └── pub(crate) fn infer_pattern()
│   │
│   ├── expr.rs (150 lines)
│   │   ├── pub(crate) fn infer_expr() (dispatcher)
│   │   ├── Handles: Unit, Int, Float, Str, Char
│   │   ├── Handles: Block, Tuple, List
│   │   └── Delegates to specialized modules
│   │
│   ├── expr_ref.rs (50 lines)
│   │   └── Handle ExprKind::Ref with suggestions
│   │
│   ├── expr_lambda.rs (100 lines)
│   │   └── Handle ExprKind::Lambda
│   │
│   ├── expr_apply.rs (200 lines)
│   │   ├── Handle ExprKind::Apply
│   │   ├── Special: if-then-else
│   │   ├── Special: seq (strict effects)
│   │   └── Special: effects
│   │
│   ├── expr_letgroup.rs (250 lines)
│   │   └── Handle ExprKind::LetGroup
│   │
│   ├── expr_altlambda.rs (270 lines)
│   │   └── Handle ExprKind::AltLambda
│   │
│   └── expr_misc.rs (100 lines)
│       ├── Handle ExprKind::Record
│       ├── Handle ExprKind::Member
│       ├── Handle ExprKind::Symbol
│       ├── Handle ExprKind::TypeAnnotation
│       └── Handle ExprKind::TypeVal
│
└── display/
    ├── mod.rs (50 lines)
    │   └── Re-exports public display functions
    │
    ├── pretty.rs (250 lines)
    │   ├── pub fn user_pretty_type()
    │   ├── user_pretty_type_and_map()
    │   └── pp_type_with_renaming()
    │
    ├── legacy.rs (150 lines)
    │   ├── pp_type()
    │   └── pp_atom_legacy()
    │
    └── normalize.rs (150 lines)
        ├── normalize_type_and_map()
        └── Variable renaming logic

Total: ~3300 lines (slight reduction from refactoring)
```

## 3. Migration Order (Minimizes Disruption)

### Phase 1: Foundation (Days 1-2)

**Step 1.1**: Extract `types.rs`
- Move: TvId, Type enum, impl Type::fun(), Display traits
- Update: lib.rs with `mod types; pub use types::*;`
- Test: `cargo test -p lzscr-types`

**Step 1.2**: Extract `error.rs`
- Move: TypeError enum, edit_distance, find_similar_names
- Import: `use crate::types::*;`
- Test: `cargo test -p lzscr-types`

**Step 1.3**: Extract `builtins.rs`
- Move: All *_type() constructors, effect_signature
- Import: `use crate::types::*;`
- Test: `cargo test -p lzscr-types`

### Phase 2: Type System Core (Days 3-4)

**Step 2.1**: Extract `scheme.rs`
- Move: Scheme, Subst, TypeEnv structs
- Move: Type trait impls (Apply, Ftv), zonk_type, normalize_tuples
- Move: instantiate, generalize
- Import: `use crate::types::*; use crate::error::*;`
- Test: `cargo test -p lzscr-types`

**Step 2.2**: Extract `typeexpr.rs`
- Move: All conv_typeexpr_*, typedefs_* functions
- Import: `use crate::{types::*, scheme::*, error::*};`
- Test: `cargo test -p lzscr-types`

**Step 2.3**: Extract `unification.rs`
- Move: occurs, unify, unify_slices, ctx_unify, bind
- Import: `use crate::{types::*, scheme::*, error::*, typeexpr::*};`
- Test: `cargo test -p lzscr-types`

### Phase 3: Display Layer (Day 5)

**Step 3.1**: Create `display/` module structure
- Create: `display/mod.rs`, `normalize.rs`, `pretty.rs`, `legacy.rs`
- Move display functions to appropriate files
- Import: `use crate::types::*;`
- Test: `cargo test -p lzscr-types`

### Phase 4: Inference Core (Days 6-10) ⚠️ COMPLEX

**Step 4.1**: Create `inference/` skeleton
- Create: `inference/mod.rs`, `context.rs`
- Move: InferCtx, TvGen, lookup_tyvar, push/pop_tyvars
- Keep stub `infer_ast()` in mod.rs that calls lib.rs temporarily
- Test: `cargo test -p lzscr-types`

**Step 4.2**: Extract `inference/pattern.rs`
- Move: infer_pattern function
- Import: `use crate::inference::context::*;`
- Test: `cargo test -p lzscr-types`

**Step 4.3**: Extract simple expression handlers
- Create: `inference/expr_ref.rs`, `expr_lambda.rs`, `expr_misc.rs`
- Move corresponding match arms from infer_expr
- Test after each file

**Step 4.4**: Extract complex expression handlers
- Create: `inference/expr_apply.rs` (200 lines)
- Move Apply match arm + nested helper functions
- Test: `cargo test -p lzscr-types`

**Step 4.5**: Extract LetGroup handler
- Create: `inference/expr_letgroup.rs` (250 lines)
- Move LetGroup match arm
- Test: `cargo test -p lzscr-types`

**Step 4.6**: Extract AltLambda handler
- Create: `inference/expr_altlambda.rs` (270 lines)
- Move AltLambda match arm
- Test: `cargo test -p lzscr-types`

**Step 4.7**: Finalize `inference/expr.rs` dispatcher
- Keep only: infer_expr entry point (150 lines)
- Simple cases: Unit, Int, Float, Str, Char, Block, Tuple, List
- Delegate to: expr_ref::*, expr_lambda::*, expr_apply::*, etc.
- Test: `cargo test -p lzscr-types`

**Step 4.8**: Move `infer_ast()` to `inference/mod.rs`
- Move main entry points from lib.rs
- Update lib.rs to re-export: `pub use inference::{infer_ast, infer_ast_with_opts};`
- Test: `cargo test -p lzscr-types`

### Phase 5: Finalization (Day 11)

**Step 5.1**: Clean up `lib.rs`
- Keep only: module declarations, re-exports
- Target: <150 lines
- Documentation at module level

**Step 5.2**: Full integration testing
- Run: `cargo test` (all workspace)
- Run: `cargo test -p lzscr-cli` (integration tests)
- Run: `cargo clippy --all-targets -- -D warnings`
- Run: `./scripts/check-file-sizes.sh`

**Step 5.3**: Documentation update
- Add module-level docs to each new file
- Update docs/refactoring-lzscr-types.md with "Completed" status
- Update CONTRIBUTING.md with new structure

## 4. Testing Strategy per Step

After each file extraction:
```bash
# Format check
cargo fmt --all

# Lint check
cargo clippy -p lzscr-types -- -D warnings

# Unit tests
cargo test -p lzscr-types

# Integration tests (after major milestones)
cargo test -p lzscr-cli
cargo test -p lzscr-analyzer

# File size check
./scripts/check-file-sizes.sh
```

## 5. Commit Strategy

One commit per extraction step:
```
refactor(types): Extract types.rs from lib.rs

- Move TvId, Type enum to types.rs
- Add module declaration and re-exports to lib.rs
- All tests pass, no functionality change

Refs: docs/refactoring-lzscr-types.md Step 1.1
```

## 6. Risk Mitigation

**Circular dependencies**: 
- Solution: Strict ordering (types → error → scheme → unification → inference)
- Validate: `cargo check` after each step

**Test failures**:
- Solution: Run tests after EACH file extraction
- Rollback: `git revert` if tests fail

**API breakage**:
- Solution: Preserve all `pub` exports in lib.rs
- Validate: Compile lzscr-cli and lzscr-analyzer

**merge conflicts**:
- Solution: Work on dedicated branch, merge frequently from main
- Validate: `git merge main` every 2-3 days

## 7. Success Criteria

- [✓] All files under 500 lines
- [✓] No function over 200 lines
- [✓] All tests pass
- [✓] No clippy warnings
- [✓] Coverage remains above 70%
- [✓] lzscr-cli and lzscr-analyzer compile without changes

## 8. Next Steps

1. Start with Phase 1, Step 1.1: Extract types.rs
2. Commit and test after each step
3. Document any deviations from plan
4. Update this file with actual progress

---

**Status**: Ready to begin implementation
**Estimated total effort**: 40-50 hours over 11 days
**Current step**: Phase 1, Step 1.1 (types.rs extraction)
