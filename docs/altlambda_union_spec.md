# AltLambda and Restricted Union (SumCtor) Display Specification (Draft)

Last Updated: 2025-09-09 (post legacy/pretty type output integration)

This document is a draft specification summarizing the generation, normalization, and display rules for the restricted union type (SumCtor) of argument patterns and return values in AltLambda syntax. The implementation is reflected in `crates/lzscr-types`, and the CLI exposes formatted type output modes (`--types pretty|legacy|json`).

Status notes (2025-09-09):
* Added dual formatting paths: user pretty (stable variable naming with `%{...}` wrapper) vs legacy raw (`%tN` variables) now selectable via `--types`.
* API additions: `infer_ast_with_opts(ast, InferOptions { pretty: bool })` introduced; `infer_ast` delegates to pretty=true.
* CLI non-debug path avoids recomputation unless `legacy` requested; debug path re-runs inference only when switching to legacy for minimally invasive change.
* SumCtor invariants defensively re-validated in pretty printer; construction sites enforce sorting + dedup + collapse.
* Future: unify runtime/IR lowering semantics for AltLambda after Core IR branch merging.

## 1. AltLambda Overview

```
(\(p1) -> e1) | (\(p2) -> e2)
```

Two lambdas are chained with `|` (right-associative). At application time pattern matching is attempted left → right. If the left matches the right is not tried. Even if the left raises during its evaluation the right is not evaluated (to be clarified in a future IR).

## 2. Parameter Type Inference Rules

1. If both branch patterns are Ctor / Symbol forms (e.g. `.Foo`, `.Bar`, `.Foo ~x ~y`), collect the set of tags as union candidates.
2. If the tag set is non-empty:
    - Single variant: collapse to a plain `Ctor { tag, payload }` (do NOT wrap in `SumCtor`.
    - Two or more variants: build `SumCtor([...])`, sorting tags lexicographically.
3. If both branches are list structure patterns only (`List` / `Cons`), heuristically specialize to `List(%tN)` (keep legacy behavior).
4. Other structural patterns (tuple, record, etc.) are unified to an ordinary single type (fresh var, etc.) without forming a union.

## 3. Return Type Inference Rules

1. Infer each branch body; collect the result types.
2. If all are Ctor / existing SumCtor and no non-Ctor type appears:
    - Single variant → collapse to `Ctor`.
    - Two or more → form `SumCtor` (sorted).
3. If any branch yields a non-Ctor type:
    - Unify the non-Ctor types; that unified result becomes the final return type.
    - Ignore Ctor-style candidates in the return union (MVP policy to avoid mixed hybrid forms).
4. Exception: If all fail and a fresh is required (theoretically not expected) return a fresh variable.

## 4. Invariants of `Type::SumCtor`

Whenever constructing / returning a SumCtor internally:

| Condition | Content |
|----------|---------|
| Variant sort | Ascending lexicographic order by tag name |
| No duplicates | Each tag appears at most once |
| 2+ variants | Single variant cases are collapsed to `Ctor` |

`pp_type` defensively re-sorts (in case future internal ops disturb ordering).

## 5. Display & Formatting Modes

Two user-visible type formatting modes now exist:

1. Pretty (default): human-friendly with stabilized variable names (e.g. `%{ a -> List a }`). Wrapper `%{` `}` distinguishes it from legacy/raw.
2. Legacy: direct printer `pp_type` output (e.g. `%t0 -> List %t0`) preserving internal variable IDs and raw constructor union formatting.
3. JSON: wraps the pretty (or legacy, not yet both) string inside `{ "ty": "..." }` (currently uses the string produced by the pretty path unless `--types legacy`).

Unless explicitly stated, examples below show legacy formatting on the right. Pretty formatting will rename `%tN` variables to sequential `%a, %b` (implementation detail) and wrap with `%{ ... }`.

### 5.1 Legacy Shape Table (pp_type)

| Shape | Example |
|-------|---------|
| Single constructor | `.Foo Int` / `.Bar` / `.Baz(Int, Int)` (payload > 1) |
| Union (2+) | `(.Bar | .Foo Int | .Qux(Int, Str))` (tag order) |
| Payload 0 | `.Tag` |
| Payload 1 | `.Tag Ty` |
| Payload 2+ | `.Tag(T1, T2, ...)` |

### 5.2 Pretty Formatting

Pretty formatting (`user_pretty_type`) is layered atop `pp_type` post-zonk & normalization:
* Renames type variables deterministically in left-to-right appearance order.
* Wraps final string in `%{ ... }` to distinguish from legacy when diffing or piping.
* Preserves SumCtor formatting (only variable names differ).

Rationale: Maintains backwards-compatible raw output (legacy) for tooling that scraped `%tN` while providing a more stable, low-noise rendering for humans.

## 6. Examples

| AltLambda Expr | Inferred Type (legacy style) | Pretty Style (illustrative) |
|----------------|------------------------------|-----------------------------|
| `\(.A) -> 1 | \(.B) -> 1` | `(.A | .B) -> Int` | `%{ (.A | .B) -> Int }` |
| `\(.A x) -> x | \(.B y z) -> y` | `(.A %t0 | .B(%t1, %t2)) -> %t0` | `%{ (.A %a | .B(%b, %c)) -> %a }` |
| `\(.A) -> .B | \(.C) -> .D` | `(.A | .C) -> (.B | .D)` | `%{ (.A | .C) -> (.B | .D) }` |
| `\(.A) -> .B | \(.C) -> 1` | `(.A | .C) -> %t0` | `%{ (.A | .C) -> %a }` |
| `\(.A) -> .B 1 | \(.C) -> .D 2 3` | `(.A | .C) -> (.B Int | .D(Int, Int))` | `%{ (.A | .C) -> (.B Int | .D(Int, Int)) }` |
| `\(.A x) -> .A x | \(.A y) -> .A y` | Error (duplicate tag) | Error |
| `\(.A x) -> 1 | \y -> 2` | Error (Ctor + non-Ctor pattern mix) | Error |

## 7. Error Conditions (MVP)

| Condition | Error Kind |
|-----------|------------|
| Duplicate tag (`.Foo` vs `.Foo`, differing arity included) | `DuplicateCtorTag` |
| One branch Ctor-only & the other is variable / non-Ctor (Wildcard allowed) | `MixedAltBranches` |

## 8. Future Extensions (Notes)
- More than 2 AltLambda branches: currently apply the 2-branch inference logic recursively (right-associative). Consider left-fold optimization for efficiency.
- Partial-order unions (subset tolerance) / gradual extension: current strategy = "exact match or error".
- Unifying SumCtor with named ADTs: may later elevate to a lightweight nominal ADT / subtyping-like form.
- Finer-grained policy for allowing "mixed" on return side (feature flag).

## 9. Implementation & Tooling Integration

Core entry points:
* Construction: `infer_expr` inside `ExprKind::AltLambda` branch (builds & normalizes SumCtor candidates).
* Normalization & raw display: `pp_type` in `Type::SumCtor` arm (defensive resort/dedup/collapse).
* Pretty display: `user_pretty_type` (variable renaming + `%{}` wrapper) used when `InferOptions.pretty` is true.
* API: `infer_ast_with_opts(ast, InferOptions { pretty })` (new); `infer_ast(ast)` == pretty path.

CLI wiring (`lzscr-cli`):
* Flag `--types` now accepts `pretty|legacy|json`.
* Non-debug path: if `legacy`, calls `infer_ast_with_opts(... pretty=false)`, else uses `infer_ast` (pretty).
* Debug path: obtains pretty result + logs, then (if `legacy`) performs a second lightweight inference for legacy printing (chosen for simplicity over refactoring debug API).

Analysis & CI context (recent additions):
* `cargo deny` configuration upgraded (v2 schema) and wildcard internal deps version-pinned.
* Coverage generation integrated via `cargo llvm-cov` (produces `lcov.info`).
* Unused dependency scanning (`cargo udeps`) pending nightly toolchain gating.
* No behavioral change to SumCtor; only display surface extended.

## 10. Test Strategy Ideas (Pending)
Recommended tests to add:
1. Single variant collapse clarity: `\(.A) -> 1 | \(.A) -> 2` → duplicate error (no silent collapse).
2. Multi-payload formatting: `.Foo(Int, Int)` prints consistently in union context.
3. Return-only union: `\x -> .A | \x -> .B` (ensure only return side unions; param stays generic).
4. Pretty vs legacy variable stability: same AST yields structurally identical unions; only variable names & wrapper differ.
5. JSON mode: `--types json` wraps pretty (or legacy when combined) output in stable object shape.

Planned follow-up: add a golden-test harness capturing pretty + legacy outputs side-by-side to detect accidental printer drift.

---
This is a draft; for the implementation-conformant spec, prefer `docs/spec/*`.
