# Type Tag (Named Type) Specification Proposal

This document organizes the specification of "tags (naming types)" in lzscr. Here, a tag means naming an entire type expression (including Sums), not the names of each variant.

## Goals

- Value-level constructors (e.g., `A`, `B`) are treated as their own types (e.g., `Ctor 'A`, `Ctor 'B`).
- By naming a type (tagging), we can express constraints when treating values as that type (e.g., label identity).
- In typing, the mere occurrence of a constructor does not immediately place it into a Named type; it retains a state of "may belong to that Named type".

## Definition (Notation)

- A named type declaration in type values/type declarations has the form:
    - `%{.Tag} = %{type}`
    - Here `type` is not limited to Sum (e.g., `(A T | B U)`) but may be any type expression.
    - `.Tag` is the type's name (the tag) and is distinct from the value-level `.sym` — it is a type-level identifier.

## Behavior (Interpretation during inference)

- When the value-level `A` appears:
    - It is merely a single-constructor type `Ctor 'A` (possibly with payload).
    - It does not immediately resolve to the Named type `Tag`.

- When a `Tag` is required by context (e.g., via annotation or function signature):
    - The `type` associated with `Tag` is enforced; for a Sum this means "must be one of `A` or `B`".
    - Additionally, check label identity (the set of variant names defined by `Tag`) to ensure no undefined labels are present.

## Validation Specification (Errors and Check Details)

### Entry point for conformance checks
- When context demands the Named type `Tag`, validate conformance against `Tag`'s right-hand `type`.
- If `type` is a Sum: check the accepted set of variants and payload-type consistency.
- If `type` is not a Sum (e.g., `List Int`, `{a:Int}`, `A -> B`): validate structural conformance via normal type compatibility.

### Label identity (for Sum)
- Compare the label set Ldef from the definition's `type` with the label set Luse of the used type.
    - Closed Sum (no OpenTail): require `Luse ⊆ Ldef`. If `Luse \ Ldef` is non-empty, error.
    - Open Sum (with OpenTail): unknown labels in `Luse` are allowed, but payload types for defined labels must match.
- Payload concordance: for each label `ℓ ∈ Ldef`, require matching payload arity and component types.

### Representative errors
- `UndefinedLabel`: an undefined label is used (for closed Sum).
    - Example message: `label 'C' is not allowed in Tag (expected one of: A, B)`
- `PayloadArityMismatch`: number of payloads for a label does not match the definition.
    - Example: `label 'A' expects 1 payload(s), but got 2`
- `PayloadTypeMismatch`: payload type mismatch (consider type variable instantiation if applicable).
    - Example: `label 'B' payload[0] expected Int, got Str`
- `NonSumMismatch`: `Tag` expects a Sum but the target is not a Sum (or vice versa).
    - Example: `expected Tag (sum type), but got constructor 'A'`

### When validation triggers
- Validate whenever a Named type appears: type annotation `%{ .Tag } expr`, function signatures requiring `Tag`, LetGroup type declarations, etc.
- Do not validate in value-only constructor contexts (`A` is treated as `Ctor 'A`).

## Examples

```
# Named Sum example
%{.ResultAB} = %{ (A Str | B Int) }

# Value-level constructor usage
let x = A "ok"
# x is simply type Ctor 'A (Str). It is not immediately .ResultAB.

# When required by function annotation
%{ .ResultAB -> Int } (\~v -> case v of ...)
# Here v is treated as .ResultAB, so variants other than A/B are disallowed and label identity is checked.
```

## Implementation Notes (Current State and Deltas)

- AST unification policy: disregard backward-compatibility; unify tag types into a single `Tag::Name(String)` (the distinction from value-level `.sym` is pushed to syntax).
- Parser builds `Sum` or other type expressions inside `%{...}` by parsing constructor lists, functions, etc.
- In type transforms, represent Named types as `Type::Named { name, args }` and instantiate to Sum etc. for validation as needed.
- Important: value constructors and type tags are separate domains. The value-level `A` occurrence does not decide `Tag`; `Tag`-required contexts perform conformance checks.

## Potential Future Extensions

- Generalize tags: allow naming of arbitrary composite types (records, functions, tuples), not only Sums.
- Provide explicit type-level label-identity constraints and improved diagnostics in the type system.
- Unify display: clarify guidelines distinguishing Named type notation (`.Tag`) from value constructor notation (e.g., `.A`).

---

(This section is Copilot's self-review)

### Self-review and Addenda

- Terminology clarification:
    - Emphasized that "tag" means a type name (Named Type) and is distinct from Sum variant names (`A`, `B`).
    - Reiterated domain separation between value-level constructors and type tags to avoid confusion.

- Strengthened inference description:
    - Clarified that values remain in a "may belong" state until a `Tag` is required and validation runs, with concrete examples.

- Implementation alignment:
    - Standardize tag type as `Tag::Name` so built-in vs. ad-hoc distinctions are not held in the tag representation; validation relies on the Named type's definition.
    - Use `Type::Named` during conversion to expand to Sum and perform label-identity checks.

- TODO suggestions:
    - Summarize rules for Named type usage sites (annotations, type values, LetGroup declarations) in a table.
    - Add more error examples for label identity issues (undefined or surplus labels) to improve developer diagnostics.
