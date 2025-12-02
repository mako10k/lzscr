# lzscr-types Refactoring Plan

**Current status**: Single file with 3438 lines
**Target**: Split into multiple focused modules under 500 lines each
**Timeline**: Phase 1 priority (2025-12-02 ~ 2025-12-15)

## Current Structure Analysis

### Major Sections (line ranges approximate)

1. **Core Type Definitions** (1-450)
   - `TvId`, `Type` enum
   - `Scheme`, `Subst`
   - Helper functions: `bool_sum_type()`, `option_type()`, etc.
   - Effect signatures
   - Type declarations frame builders

2. **Error Types** (457-590)
   - `TypeError` enum (all variants)
   - Helper functions: `edit_distance()`, `find_similar_names()`, `format_field_path()`

3. **Unification Core** (591-1075)
   - `occurs()` check
   - `unify()` main function
   - `unify_slices()`
   - `ctx_unify()` with context
   - `bind()` variable binding
   - Type declaration validation

4. **Type Environment & Instantiation** (1078-1370)
   - `TypeEnv` struct
   - `instantiate()`, `generalize()`
   - Pattern type inference: `infer_pattern()`
   - Type variable scope management

5. **Expression Type Inference** (1371-2361)
   - `conv_typeexpr_fresh()` 
   - `infer_expr()` (massive function, 900+ lines)
   - Expression kind utilities

6. **Type Display** (2362-2954+)
   - `pp_type()`, `pp_type_with_renaming()`
   - `user_pretty_type()`
   - Variable normalization and mapping
   - Legacy display functions

### Function Complexity Hotspots

**Large functions requiring refactoring:**
- `infer_expr()`: ~900 lines (must split)
- `unify()`: ~170 lines (consider splitting)
- `pp_type_with_renaming()`: ~100 lines (acceptable)
- `user_pretty_type_and_map()`: ~80 lines (acceptable)

## Proposed Module Structure

```
crates/lzscr-types/src/
├── lib.rs                    # Public API, re-exports (150 lines target)
├── types.rs                  # Core Type/TvId definitions (200 lines)
├── scheme.rs                 # Scheme, Subst, TypeEnv (150 lines)
├── error.rs                  # TypeError and helpers (200 lines)
├── unification.rs            # unify, occurs, bind (300 lines)
├── inference/                # Split inference module
│   ├── mod.rs                # Public inference API (100 lines)
│   ├── pattern.rs            # infer_pattern (200 lines)
│   ├── expr.rs               # infer_expr core (400 lines)
│   ├── expr_lit.rs           # Literal inference (100 lines)
│   ├── expr_lambda.rs        # Lambda/AltLambda (200 lines)
│   ├── expr_letgroup.rs      # LetGroup/LetRec (200 lines)
│   └── context.rs            # InferCtx, type var scopes (150 lines)
├── typeexpr.rs               # TypeExpr conversion (150 lines)
├── display/                  # Type display module
│   ├── mod.rs                # Public display API (50 lines)
│   ├── pretty.rs             # user_pretty_type (200 lines)
│   ├── legacy.rs             # pp_type_legacy (150 lines)
│   └── normalize.rs          # Variable renaming logic (150 lines)
└── builtins.rs               # effect_signature, builtin types (150 lines)
```

**Total estimated**: ~3000 lines (some reduction from refactoring)

## Migration Strategy

### Step 1: Extract Non-dependent Modules (Day 1-2)

**Priority: Low coupling, high cohesion**

1. **types.rs**: Extract type definitions
   - `TvId`, `Type` enum
   - Simple helper constructors (`bool_sum_type`, etc.)
   - No dependencies on other modules

2. **error.rs**: Extract error types
   - `TypeError` enum
   - `edit_distance()`, `find_similar_names()`
   - Only depends on `types.rs`

3. **builtins.rs**: Extract builtin signatures
   - `effect_signature()`
   - Builtin type constructors
   - Only depends on `types.rs`

### Step 2: Extract Display Layer (Day 3-4)

4. **display/normalize.rs**: Variable renaming
   - `normalize_type_and_map()`
   - `user_pretty_type_and_map()`

5. **display/pretty.rs**: Pretty printing
   - `user_pretty_type()`
   - `pp_type_with_renaming()`

6. **display/legacy.rs**: Legacy formatting
   - `pp_type_legacy()`
   - `pp_atom_legacy()`

7. **display/mod.rs**: Public API wrapper

### Step 3: Extract Core Inference (Day 5-7)

8. **scheme.rs**: Polymorphism support
   - `Scheme`, `Subst`
   - `TypeEnv`
   - `instantiate()`, `generalize()`

9. **unification.rs**: Unification algorithm
   - `unify()` main function
   - `unify_slices()`
   - `occurs()`, `bind()`
   - `ctx_unify()`

10. **typeexpr.rs**: Type expression conversion
    - `conv_typeexpr_fresh()`
    - `conv_typeexpr_with_subst()`
    - Type declaration validation

### Step 4: Split Expression Inference (Day 8-10)

**Challenge: `infer_expr()` is 900+ lines**

11. **inference/context.rs**: Context management
    - `InferCtx` struct
    - Type variable scope push/pop
    - `lookup_tyvar()`

12. **inference/pattern.rs**: Pattern inference
    - `infer_pattern()`
    - Pattern type variable binding

13. **inference/expr_lit.rs**: Literal expressions
    - `ExprKind::Unit`, `Int`, `Float`, `Str`, `Char`
    - Simple cases from `infer_expr()`

14. **inference/expr_lambda.rs**: Lambda expressions
    - `ExprKind::Lam`, `AltLambda`
    - SumCtor construction logic

15. **inference/expr_letgroup.rs**: Let bindings
    - `ExprKind::LetGroup`, `LetRec`
    - Generalization logic

16. **inference/expr.rs**: Main dispatcher
    - `infer_expr()` main function (reduced to ~200 lines)
    - Delegates to specialized modules
    - `App`, `If`, `Match`, etc.

17. **inference/mod.rs**: Public API
    - `infer_ast()`, `infer_ast_with_opts()`
    - `InferOptions`
    - Re-exports

### Step 5: Update lib.rs (Day 11)

18. **lib.rs**: Minimal public API
    - Re-export public types
    - Re-export public functions
    - Module declarations
    - Integration tests stay in `tests/`

## Implementation Details

### Dependencies Between Modules

```
types.rs (no deps)
  ↑
  ├── error.rs (types)
  ├── builtins.rs (types)
  ├── scheme.rs (types)
  │     ↑
  │     └── unification.rs (types, scheme, error)
  │           ↑
  │           └── typeexpr.rs (types, unification)
  │                 ↑
  │                 └── inference/* (all above)
  │
  └── display/* (types)
```

### API Stability

**Public API (must preserve):**
- `infer_ast()` → `inference::infer_ast()`
- `infer_ast_with_opts()` → `inference::infer_ast_with_opts()`
- `Type`, `TvId`, `Scheme`, `TypeError` → re-exported from `lib.rs`
- `user_pretty_type()` → `display::user_pretty_type()`

**Internal API (can change):**
- All helper functions
- `InferCtx` internals
- Subst operations

### Testing Strategy

**During refactoring:**
- Run `cargo test -p lzscr-types` after each module extraction
- Ensure no test failures
- Verify no clippy warnings introduced

**Final validation:**
- Run full workspace tests: `cargo test`
- Run CLI integration tests: `cargo test -p lzscr-cli`
- Coverage check: `cargo llvm-cov`

### Migration Checklist

For each module extraction:

- [ ] Create new file under `src/`
- [ ] Copy relevant code from `lib.rs`
- [ ] Add module declaration to `lib.rs` or parent `mod.rs`
- [ ] Update imports in extracted code
- [ ] Update imports in remaining `lib.rs` code
- [ ] Run `cargo fmt --all`
- [ ] Run `cargo clippy -p lzscr-types -- -D warnings`
- [ ] Run `cargo test -p lzscr-types`
- [ ] Commit with descriptive message

## Risk Mitigation

### Potential Issues

1. **Circular dependencies**: Carefully order extractions
2. **Test breakage**: Tests in `tests/` import from crate root
3. **API surface changes**: Preserve all public exports
4. **Merge conflicts**: Do on a clean branch, merge frequently

### Rollback Plan

- Each module extraction is one commit
- If issues arise, can revert specific commits
- Keep feature branch until all tests pass

## Success Criteria

- [ ] All files under 500 lines (except `lib.rs` which re-exports)
- [ ] No function over 200 lines
- [ ] All tests pass
- [ ] No clippy warnings
- [ ] Coverage remains above 70%
- [ ] Documentation updated (rustdoc)

## Follow-up Tasks

After completion:
- Update `docs/coding-standards.md` with new structure
- Create ADR documenting the refactoring
- Update CONTRIBUTING.md with module map
- Consider similar refactoring for other large files

---

**Owner**: Assigned to refactoring phase 1
**Status**: Planning complete, ready to implement
**Estimated effort**: 10-12 hours over 11 days
