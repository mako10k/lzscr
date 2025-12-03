# Rust Coding Patterns

## AST Refactoring Patterns

### Record Field Structures

When adding span tracking to AST record fields, follow this consistent pattern:

**Structure Definition:**
```rust
/// Record field in [context] with field name span tracking.
/// Phase 5: Diagnostics Improvement - enables precise error reporting.
#[derive(Debug, Clone, PartialEq)]
pub struct [Type]RecordField {
    pub name: String,
    pub name_span: Span,
    pub [field_name]: [FieldType],
}

impl [Type]RecordField {
    pub fn new(name: String, name_span: Span, [field_name]: [FieldType]) -> Self {
        Self { name, name_span, [field_name] }
    }
}
```

**Iteration Pattern:**
```rust
// Before: Vec<(String, T)>
for (k, v) in fields { ... }

// After: Vec<TRecordField>
for f in fields {
    // Access: f.name, f.name_span, &f.[field_name]
}
```

### Parser Span Capture

Always capture field name spans during parsing:

```rust
let key_span = ktok.span;  // Capture before consumption
fields.push([Type]RecordField::new(key, key_span, value));
```

### Type Inference with Spans

Use field name spans for diagnostics, not value spans:

```rust
// Prefer field name span for error reporting
map.insert(f.name.clone(), (type_value, Some(f.name_span)));
```

## Testing Requirements

After AST changes:
1. Run `cargo build` - fix all compilation errors
2. Run `cargo test` - ensure all 113+ tests pass
3. Commit with descriptive message including:
   - What changed
   - Why (Phase/Step reference)
   - Impact (tests passing, lines affected)
