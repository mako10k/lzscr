# Phase 5: Record Field Spans

## Overview

**Goal**: Add precise span tracking to record field names for better error diagnostics

**Status**: 33% Complete (Step 1-6 done, Steps 7-9 remaining)

## Completed Work

### ✅ Step 1-6: ExprKind::Record (Commit: 526024f)

- Added `ExprRecordField` struct with `name`, `name_span`, `value` fields
- Updated `ExprKind::Record` from `Vec<(String, Expr)>` to `Vec<ExprRecordField>`
- Parser captures field name spans during record literal parsing
- Type checker uses field name span for diagnostics (key improvement)
- Updated 7 crates: ast, parser, types, runtime, coreir, analyzer, cli
- All 113 tests passing

**Key Achievement**: Type errors on record fields now point to field names, not values

## Current Work

### 🔄 Step 7: PatternKind::Record

**Objective**: Apply same transformation to pattern records

**Changes Required**:
1. Add `PatternRecordField` struct (similar to `ExprRecordField`)
2. Update `PatternKind::Record(Vec<(String, Pattern)>)` → `Vec<PatternRecordField>`
3. Parser: capture pattern field name spans
4. Update affected crates:
   - lzscr-analyzer: `hash_pattern_shape()`, `collect_vars()`, `pat_idents()`, `binds_param()`, `binds_name()`
   - lzscr-cli: `rebase_pattern_with_minus()`
   - lzscr-types: pattern type inference

**Estimated Scope**: ~25-30 locations (fewer than Expr)

## Remaining Work

### ⏳ Step 8: TypeExpr::Record

- Add `TypeExprRecordField` struct
- Update type expression records
- Estimated: 30-45 minutes (smallest scope)

### ⏳ Step 9: Error Display Improvements

- Update `display_type_error_diagnostic()` to use field name spans
- Add golden tests for record field errors
- Example: "Missing field 'age'" points precisely at field name

## Progress Tracking

- Phase 5: 33% → 66% (after Step 7) → 100% (after Step 9)
- Next Phase: Phase 6 (Error Message Style Guide)
