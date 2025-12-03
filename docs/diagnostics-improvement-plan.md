# Diagnostics Improvement Plan

**Date**: 2025-12-02  
**Status**: Planning Phase  
**Goal**: Standardize and enhance error diagnostics following ROADMAP priorities

## Current State Analysis

### Existing Dual-Span Support
The codebase already has partial dual-span diagnostic implementation:

1. **Type Errors with Dual Spans** (in `lzscr-types/src/error.rs`):
   - `MismatchBoth`: expected vs actual type spans
   - `RecordFieldMismatchBoth`: field mismatch with dual spans
   - `Occurs`: infinite type detection (var location vs occurrence)
   - `AnnotMismatch`: annotation vs expression spans
   - `AltLambdaArityMismatch`: expected vs actual arity spans

2. **CLI Display Logic** (in `lzscr-cli/src/main.rs`):
   - `format_span_caret()`: single-span caret rendering
   - `SourceRegistry::format_span_block()`: span-to-source mapping with module support
   - Dual-span rendering for specific error types (lines 1214-1249)

### Gaps and Inconsistencies

1. **Incomplete Dual-Span Coverage**:
   - Some error variants still use single-span (`Mismatch`, `RecordFieldMismatch`)
   - Not all dual-span errors have consistent display logic
   
2. **Span Adjustment Heuristics**:
   - 1-char span heuristic (shift to first non-comment token) scattered across code
   - No centralized span normalization logic

3. **Error Message Style**:
   - Mix of English and technical jargon
   - Missing fix-it hints for many error types
   - No consistent "cause vs effect" labeling

4. **Occurs Check Display**:
   - Currently uses raw type variable IDs
   - No origin span tracking for generated type variables
   - No normalized display (%a, %b convention not fully implemented)

## Improvement Phases

### Phase 1: Standardize Dual-Span Infrastructure (Week 1)

**Goal**: Create consistent dual-span error types and display utilities

**Tasks**:
1. Create `DiagnosticSpan` struct to encapsulate span metadata
   ```rust
   struct DiagnosticSpan {
       offset: usize,
       len: usize,
       label: Option<String>,  // e.g., "expected here", "actual here"
       context: SpanContext,    // primary/secondary/note
   }
   ```

2. Add `format_dual_span_caret()` function
   - Renders two carets with distinct labels
   - Handles spans in different sources/modules
   - Consistent color/formatting (if terminal supports)

3. Deprecate single-span error variants (migration path):
   - Keep for backward compat but emit deprecation warnings
   - Add dual-span versions for all remaining errors

**Acceptance Criteria**:
- All type errors support dual-span reporting
- Single function for dual-span rendering
- Golden tests for dual-span display format

### Phase 2: Normalize Span Adjustment (Week 2) ✅ COMPLETED

**Goal**: Centralize span normalization heuristics

**Status**: ✅ Infrastructure complete (2025-12-03)

**Completed Tasks**:
1. ✅ `normalize_span()` function already exists in `SourceRegistry`
   - Handles 1-char span expansion to meaningful tokens
   - Leading '{' shifted inward to first token
   - Comment/whitespace adjustment implemented

2. ✅ Span metadata tracking added:
   - Added `SpanOrigin` enum (Source vs Generated)
   - Added `origin` field to `DiagnosticSpan`
   - `DiagnosticSpan::generated()` constructor for compiler-generated spans
   - Type variable origin tracking via `InferCtx::tv_origins`
   - Expression span stack via `InferCtx::span_stack`

3. ✅ Documentation updated:
   - Added normalization heuristics to `diagnostic.rs` module docs
   - Documented `SpanOrigin` usage patterns
   - Exported `SpanOrigin` from `lzscr-types`

**Acceptance Criteria**:
- ✅ Single source of truth for span normalization (SourceRegistry)
- ✅ Span metadata tracking infrastructure in place
- ✅ Documentation of normalization rules complete

**Notes**:
- Actual normalization logic remains in CLI's SourceRegistry (requires source text access)
- Type system now tracks span origins for future improvements
- Ready for Phase 3 (Occurs Check improvements)

### Phase 3: Enhance Occurs Check Display (Week 3) ✅ COMPLETED

**Goal**: Improve infinite type error messages

**Status**: ✅ Enhanced occurs check display (2025-12-03)

**Completed Tasks**:
1. ✅ Type variable origin tracking (already implemented):
   - `InferCtx::tv_origins` tracks generation spans
   - `InferCtx::fresh_tv()` automatically records origins
   - Occurs check errors enriched with span information

2. ✅ Normalized type variable display (already implemented):
   - `normalize_type_and_map()` and `user_pretty_type_and_map()` functions
   - Consistent %a, %b, %c naming in error messages
   - `var_pretty` and `pretty` fields in Occurs error

3. ✅ Improved occurs error message:
   - Added `TypeError::occurs_explanation()` method
   - Detailed explanation of infinite type issue
   - Suggests common causes (missing annotations, recursive patterns)
   - `display_type_error_diagnostic()` shows enhanced explanation

**Acceptance Criteria**:
- ✅ Type variables use %a, %b notation (via user_pretty_type_and_map)
- ✅ Origin spans tracked via tv_origins HashMap
- ✅ Clear multi-line explanation with examples

**Example Output**:
```
type error: cannot construct infinite type: type variable %a occurs within its own definition %{List %a}
  type variable defined here:
    <span>
  occurs inside here:
    <span>

The type variable %a would occur within its own definition.
The inferred type would be: %a = List %a
This creates an infinite type (recursive definition without a fixpoint).

Possible causes:
- Missing type annotation on recursive function
- Self-referential data structure without explicit type
- Incorrect recursive call pattern
```

### Phase 4: Add Fix-It Hints (Week 4) ✅ COMPLETED

**Goal**: Provide actionable suggestions for common errors

**Implementation Summary**:
- Added `TypeError::fix_hints()` method covering all error types
- Integrated auto-hint generation in `display_type_error_diagnostic()`
- Consolidated existing hint logic (`suggest_fixes_for_*`)
- Provides actionable suggestions for: type mismatches, unbound refs, effect errors, occurs check failures, record field issues, alt lambda arity mismatches, annotation conflicts

**Tasks**:
1. Add `hint` field to diagnostic display:
   - Suggest explicit type annotations for ambiguous inference
   - Suggest seq/chain for effect errors
   - Suggest similar names for unbound refs (already implemented)

2. Create hint templates:
   ```rust
   enum FixItHint {
       AddTypeAnnotation { expr_span: Span, suggested_type: Type },
       UseEffectWrapper { wrapper: &'static str },  // "seq", "chain"
       RenameVariable { from: String, to: Vec<String> },
       AddImport { module: String },
   }
   ```

3. Integrate hints into error display:
   - Show after main error message
   - Format consistently: "  hint: <suggestion>"
   - Include code examples where helpful

**Acceptance Criteria**:
- All error types have appropriate hints
- Hints are clear and actionable
- User testing confirms hints are helpful

### Phase 5: Records Field Spans (Week 5)

**Goal**: Precise span tracking for record field names and values

**Tasks**:
1. Enhance AST to track field name spans:
   ```rust
   struct RecordField {
       name: String,
       name_span: Span,
       value: Expr,
       value_span: Span,  // redundant with value.span but explicit
   }
   ```

2. Update parser to capture field name spans

3. Use field spans in record mismatch errors:
   - Point to specific field name for missing/extra fields
   - Point to field value for type mismatches
   - Show both field name and value spans for context

**Acceptance Criteria**:
- Field name spans available in AST
- Record errors use field-specific spans
- Golden tests for record error display

### Phase 6: Error Message Style Guide (Ongoing)

**Goal**: Consistent, clear, concise error messages

**Guidelines**:
1. **Structure**: `<error-type>: <brief description>`
2. **Language**: Simple English, avoid jargon
3. **Spans**: Show cause (expected) before effect (actual)
4. **Labels**: Use "expected here" / "actual here" / "defined here"
5. **Hints**: Start with "hint:", provide concrete actions
6. **Examples**: Include code snippets for complex suggestions

**Tasks**:
- Audit all error messages for consistency
- Create message templates for common patterns
- Document style guide in `docs/coding-standards.md`

## Success Metrics

1. **Coverage**: All error types support dual-span reporting
2. **Consistency**: All errors follow style guide
3. **Clarity**: User testing shows improved error understanding
4. **Maintainability**: Span handling centralized, easy to extend

## Dependencies

- Parser enhancements (for field spans)
- Terminal color support (optional, for better visual distinction)
- Golden test framework expansion (for diagnostic tests)

## Timeline

- **Phase 1**: Week 1 (Jan 8-12, 2025)
- **Phase 2**: Week 2 (Jan 15-19, 2025)  
- **Phase 3**: Week 3 (Jan 22-26, 2025)
- **Phase 4**: Week 4 (Jan 29 - Feb 2, 2025)
- **Phase 5**: Week 5 (Feb 5-9, 2025)
- **Phase 6**: Ongoing (reviews every 2 weeks)

## Open Questions

1. Should we add color/ANSI support for terminal output?
2. How to handle very long spans (e.g., entire function bodies)?
3. Should hints be suppressible via CLI flag?
4. Need JSON output format for IDE integration?
