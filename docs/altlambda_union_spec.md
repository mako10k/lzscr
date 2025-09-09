# AltLambda and Restricted Union (SumCtor) Display Specification (Draft)

Last Updated: 2025-09-09

This document is a draft specification summarizing the generation, normalization, and display rules for the restricted union type (SumCtor) of argument patterns and return values in AltLambda syntax. The implementation is already reflected in `crates/lzscr-types`.

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

## 5. Display Format (pp_type)

| Shape | Example |
|-------|---------|
| Single constructor | `.Foo Int` / `.Bar` / `.Baz(Int, Int)` (payload > 1) |
| Union (2+) | `(.Bar | .Foo Int | .Qux(Int, Str))` (tag order) |
| Payload 0 | `.Tag` |
| Payload 1 | `.Tag Ty` |
| Payload 2+ | `.Tag(T1, T2, ...)` |

User-facing pretty (`%{ ... }`) also uses the same `pp_type` for consistency.

## 6. Examples

| AltLambda Expr | Inferred Type (legacy style) |
|----------------|------------------------------|
| `\(.A) -> 1 | \(.B) -> 1` | `(.A | .B) -> Int` |
| `\(.A x) -> x | \(.B y z) -> y` | `(.A %t0 | .B(%t1, %t2)) -> %t0` |
| `\(.A) -> .B | \(.C) -> .D` | `(.A | .C) -> (.B | .D)` |
| `\(.A) -> .B | \(.C) -> 1` | `(.A | .C) -> %t0` (`.B` + `Int` mixed → prefer unifying non-Ctor) |
| `\(.A) -> .B 1 | \(.C) -> .D 2 3` | `(.A | .C) -> (.B Int | .D(Int, Int))` |
| `\(.A x) -> .A x | \(.A y) -> .A y` | Error (duplicate tag) |
| `\(.A x) -> 1 | \y -> 2` | Error (Ctor + non-Ctor pattern mix) |

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

## 9. Implementation Locations
- Construction: `infer_expr` inside `ExprKind::AltLambda` branch
- Normalization / display: `pp_type` in `Type::SumCtor` arm
- Invariant comments: at `enum Type` declaration site

## 10. Test Strategy Ideas (Unimplemented)
- Single variant collapse clarity: `\(.A) -> 1 | \(.A) -> 2` → duplicate error (ensure no unintended collapse)
- Multi-payload formatting: verify `.Foo(Int, Int)` output
- Return-only union: `\x -> .A | \x -> .B` (parameter non-Ctor, return union). Currently parameter side does not unionize; specification decision pending (future).

---
After feedback this draft will be merged into `docs/lzscr.md`.
