# Step 9: Error Display Improvements

## Objective

Leverage field name spans in TypeError display to provide precise error reporting for record fields. This is the final step that makes all the previous infrastructure work visible to users.

## Implementation Steps

### 1. Analyze Current Error Display

**File**: `crates/lzscr-types/src/error.rs`

**Function**: `display_type_error_diagnostic()`

Review how Type::Record errors are currently displayed and identify where field name spans should be used.

### 2. Update Record Error Reporting

**Key improvements**:

- **Missing field errors**: Point to the field name in the pattern/expression, not the entire record
- **Type mismatch in fields**: Use field name span instead of value span
- **Extra field errors**: Highlight the unexpected field name precisely

**Example transformations**:

```rust
// Before: Error points to entire record literal
{ name: "Alice", age: "30" }
^^^^^^^^^^^^^^^^^^^^^^^^^^ Type error: Expected Int, found Str

// After: Error points to field name
{ name: "Alice", age: "30" }
                 ^^^ Type error in field 'age': Expected Int, found Str
```

### 3. Locate Error Display Code

**Search for**:
- `Type::Record` in error display functions
- Current span usage in record-related errors
- Functions that format type error messages

**Files to check**:
- `crates/lzscr-types/src/error.rs`
- `crates/lzscr-cli/src/main.rs` (error display utilities)

### 4. Update Span Selection Logic

When displaying errors for Type::Record:

```rust
// Use the stored field name span from type information
match &expected_type {
    Type::Record(fields) => {
        // fields is BTreeMap<String, (Type, Option<Span>)>
        if let Some((_, Some(field_span))) = fields.get(&field_name) {
            // Use field_span for error location
            display_error_at_span(field_span, &message);
        }
    }
}
```

### 5. Test Error Messages

Create test cases that demonstrate improved error reporting:

**Test file**: `crates/lzscr-types/tests/record_field_errors.rs` (or similar)

**Test scenarios**:
1. Missing required field
2. Type mismatch in field value
3. Extra unexpected field
4. Field name typo (suggestion)

### 6. Update Golden Tests

If golden test files exist for error messages:

**Location**: `goldens/` or `crates/lzscr-types/tests/goldens/`

Update expected error output to show:
- Field name highlighted (not entire record)
- Precise column/line pointing to field
- Clear "in field 'name'" message

### 7. Documentation

**Optional but recommended**:

Update error message documentation to showcase improved diagnostics:
- Before/after examples
- Screenshots or text examples in the PR description (or a short note under `docs/` if needed)

## Expected Outcomes

### User-Visible Improvements

**Before (Step 1-8)**:
```
Type error at line 5, column 10:
{ name: "Alice", age: "30" }
^^^^^^^^^^^^^^^^^^^^^^^^^^ Expected Int, found Str
```

**After (Step 9)**:
```
Type error at line 5, column 22:
{ name: "Alice", age: "30" }
                 ^^^ Field 'age': Expected Int, found Str
```

### Technical Achievements

- ✅ Field name spans from Step 1-8 are now utilized
- ✅ Error messages point to exact field locations
- ✅ Improved developer experience with precise diagnostics
- ✅ Phase 5 complete (100%)

## Common Patterns to Update

### Pattern 1: Field Name in Error Message

```rust
// Add field name to error message
format!("Field '{}': {}", field_name, type_error_message)
```

### Pattern 2: Span Selection Priority

```rust
// Priority: field_span > value_span > entire_record_span
let error_span = field_span
    .or(value_span)
    .unwrap_or(record_span);
```

### Pattern 3: Diagnostic Context

```rust
// Add context about which field caused the error
Diagnostic {
    message: format!("Type mismatch in field '{}'", field_name),
    span: field_name_span,  // Use field name span
    notes: vec!["Expected Int, found Str"],
}
```

## Validation

### Manual Testing

```bash
# Test with intentional type errors
echo '{ name: "Alice", age: "30" }' | cargo run --bin lzscr -- eval -

# Verify error points to 'age', not entire record
```

### Automated Testing

```bash
# Run type error tests
cargo test --package lzscr-types test_record_field_errors

# Run all tests to ensure no regressions
cargo test
```

## Success Criteria

- ✅ Record field type errors point to field name, not value
- ✅ Missing field errors point to expected location in pattern/type
- ✅ All 113 tests continue passing
- ✅ Error messages are more helpful (verified manually)
- ✅ No performance regression in type checking

## Notes

- This step is primarily about **presentation**, not type system changes
- The infrastructure (field name spans) is already in place from Steps 1-8
- Focus on user-facing error messages and diagnostics
- May require iterative refinement based on actual error scenarios encountered
