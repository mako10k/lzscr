# Step 7: PatternRecordField Implementation

## Objective

Update `PatternKind::Record` to use `PatternRecordField` struct with field name span tracking, following the same pattern as `ExprRecordField` (completed in Step 1-6).

## Implementation Steps

### 1. Add PatternRecordField Struct

**File**: `crates/lzscr-ast/src/lib.rs`

**Location**: Immediately after `ExprRecordField`

```rust
/// Record field in a pattern with field name span tracking.
/// Phase 5: Diagnostics Improvement - enables precise error reporting for pattern record fields.
#[derive(Debug, Clone, PartialEq)]
pub struct PatternRecordField {
    pub name: String,
    pub name_span: Span,
    pub pattern: Pattern,
}

impl PatternRecordField {
    pub fn new(name: String, name_span: Span, pattern: Pattern) -> Self {
        Self { name, name_span, pattern }
    }
}
```

**Note**: Must include `#[derive(PartialEq)]` - required for pattern matching

### 2. Update PatternKind::Record Definition

**File**: `crates/lzscr-ast/src/lib.rs`

**Change**:
```rust
// Before:
Record(Vec<(String, Pattern)>),

// After:
Record(Vec<PatternRecordField>),
```

### 3. Update Parser

**File**: `crates/lzscr-parser/src/lib.rs`

**Location**: Pattern record parsing (~line 2000)

**Change**: Capture field name span before consuming token

```rust
let key_span = ktok.span;  // Add this line
fields.push(PatternRecordField::new(key, key_span, pattern));  // Update this line
```

### 4. Update Affected Crates

#### lzscr-analyzer/src/lib.rs

**Functions to update**:
- `hash_pattern_shape()` (~line 30-80)
  - Change: `for (k, v) in fs` → `for f in fs`
  - Access: `f.name`, `&f.pattern`
  
- `collect_vars()` (multiple locations: ~line 520, 750, 1050)
  - Change: `for (_k, v) in fs` → `for f in fs`
  - Access: `&f.pattern`
  
- `pat_idents()` (multiple locations: ~line 650, 780)
  - Same pattern as `collect_vars()`
  
- `binds_param()` (~line 900)
  - Change: `fs.iter().any(|(_, v)| binds_param(v, name))` → `fs.iter().any(|f| binds_param(&f.pattern, name))`
  
- `binds_name()` (~line 1100)
  - Same pattern as `binds_param()`

#### lzscr-cli/src/main.rs

**Function**: `rebase_pattern_with_minus()` (~line 1983)

```rust
Record(fields) => {
    let mut new = Vec::with_capacity(fields.len());
    for f in fields.iter() {
        new.push(PatternRecordField::new(
            f.name.clone(),
            f.name_span,
            map_pattern(&f.pattern)
        ));
    }
    Record(new)
}
```

#### lzscr-types/src/inference/pattern.rs

**Pattern type inference logic**

Update pattern matching for `PatternKind::Record`:
- Store field name spans in type information
- Use `f.name_span` for diagnostic purposes

### 5. Build and Test

```bash
# Iterative compilation
cargo build

# Fix errors as they appear
# Repeat until clean build

# Run full test suite
cargo test

# Verify all 113 tests pass
```

### 6. Commit

```bash
git add -A
git commit -m "feat(diagnostics): Phase 5 Step 7 - Add field name spans to PatternKind::Record

- Added PatternRecordField struct with name, name_span, pattern fields
- Updated PatternKind::Record to use Vec<PatternRecordField>
- Parser captures pattern field name spans
- Updated lzscr-analyzer, lzscr-cli, lzscr-types
- All tests passing
- Enables precise error reporting for pattern record fields"
```

## Expected Outcomes

- **Pattern match errors** now point to field names precisely
- **Consistency** with `ExprRecordField` structure
- **Scope**: ~25-30 locations updated (less than Expr)
- **Tests**: All 113 tests continue passing

## Key Differences from ExprRecordField

1. **Field type**: `pattern: Pattern` instead of `value: Expr`
2. **Derives**: Must include `PartialEq` (used in pattern matching)
3. **Usage locations**: Fewer than Expr (patterns less common)
4. **Type inference**: Store span in pattern type context

## Common Pitfalls

- ❌ Forgetting `#[derive(PartialEq)]` on `PatternRecordField`
- ❌ Mixing up `ExprKind::Record` and `PatternKind::Record` during fixes
- ❌ Using `f.value` instead of `f.pattern` (copy-paste error)
- ❌ Not capturing `key_span` before token consumption in parser

## Success Criteria

- ✅ All crates compile without errors
- ✅ All 113 tests pass
- ✅ `PatternRecordField` struct defined with correct derives
- ✅ Parser captures field name spans
- ✅ All pattern record iterations updated to new format
