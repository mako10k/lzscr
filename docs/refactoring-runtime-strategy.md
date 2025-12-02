# lzscr-runtime Refactoring Strategy

**Date**: 2025-12-02
**Target**: `crates/lzscr-runtime/src/lib.rs` (2685 lines)
**Goal**: Split into focused modules (target: lib.rs < 500 lines)

## Current Structure Analysis

### Core Types (lines 1-100)
- `Env`: Runtime environment
- `EvalError`: Error types
- `ThunkState`, `ThunkKind`: Lazy evaluation
- `Value`: Runtime values
- `RtStr`: Runtime string with slicing

### Helper Functions (lines 1098-1535)
- String conversion helpers (`to_str_like`, `char_literal_string`)
- Value constructors (`bool_ctor`, `option_value`, `result_ok/err`)
- Boolean extraction (`as_bool`)

### Effects (lines 1201-1535)
- `eff_guard`: Effect permission check
- `eff_print`, `eff_println`: IO operations
- `eff_fs_*`: File system operations (read, write, append, list, remove, create, metadata)

### Core Evaluation (lines 1536-2255)
- `v_equal`: Value equality
- `match_pattern`: Pattern matching
- `force_value`: Thunk forcing
- `apply_value`: Function application
- `eval`: Main evaluation function (large!)

### Tests (lines 2256+)

## Proposed Module Structure

```
crates/lzscr-runtime/src/
├── lib.rs (< 500 lines)          # Public API, re-exports, Env
├── value.rs                       # Value, RtStr, value constructors
├── error.rs                       # EvalError and Display
├── thunk.rs                       # ThunkState, ThunkKind, force_value
├── effects/
│   ├── mod.rs                     # Effect guard, public interface
│   ├── io.rs                      # print, println
│   └── fs.rs                      # File system operations
├── eval/
│   ├── mod.rs                     # Main eval function entry
│   ├── pattern.rs                 # match_pattern, v_equal
│   └── apply.rs                   # apply_value
└── helpers.rs                     # String helpers, conversions
```

## Extraction Phases

### Phase 1: Foundation (error.rs, value.rs)
- Extract `EvalError` → `error.rs`
- Extract `Value`, `RtStr` → `value.rs`
- Update: lib.rs with `mod error; mod value;`
- Test: `cargo test -p lzscr-runtime`

### Phase 2: Thunks (thunk.rs)
- Extract `ThunkState`, `ThunkKind`, `force_value` → `thunk.rs`
- Import: `use crate::{error::*, value::*};`
- Test: `cargo test -p lzscr-runtime`

### Phase 3: Effects (effects/)
- Create `effects/mod.rs`, `effects/io.rs`, `effects/fs.rs`
- Move effect functions to appropriate files
- Test: `cargo test -p lzscr-runtime`

### Phase 4: Evaluation (eval/)
- Create `eval/mod.rs`, `eval/pattern.rs`, `eval/apply.rs`
- Extract pattern matching and application logic
- Keep main `eval` function in eval/mod.rs initially
- Test: `cargo test -p lzscr-runtime`

### Phase 5: Helpers (helpers.rs)
- Extract string conversion utilities
- Test: `cargo test -p lzscr-runtime`

### Phase 6: Final Cleanup
- Verify lib.rs is < 500 lines
- Run full test suite
- Check clippy warnings

## Success Criteria

- ✅ lib.rs reduced to < 500 lines (from 2685)
- ✅ All tests passing
- ✅ 0 clippy warnings
- ✅ Clear module boundaries
- ✅ No breaking changes to public API

## Extraction Method

Use `nl` and `sed` commands for precise line extraction:
1. Identify exact line ranges with `nl -ba`
2. Extract with `sed -n 'START,ENDp'`
3. Delete with `sed -i 'START,ENDd'`
4. Verify with `wc -l` and test compilation
