# Type System (ModeMap and `.select`)

Status: PoC extension. Aligns runtime selection with static expectations.

## Overview

- Goal: Define typing rules for ModeMap values and the `.select` combinator, and formalize the sugar `.Ident ModedValue ≡ .select .Ident ModedValue`.
- Scope: Value-level typing. Not introducing new keywords; `.select` is a combinator, symbols stay atomic.

## Types and Notation

- Base types: `.Unit`, `.Int`, `.Float`, `.Bool`, `.Str`, `.List T`, `.Tuple T1 .. Tn`, record types `{ k: T, ... }`.
- Function types: `T -> U` (curried functions).
- Type variables: `%a, %b, ...` (universally quantified where appropriate).
- Symbol names: `.Ident` (mode labels).

## ModeMap Type

A ModeMap at value level uses Record syntax; its static type is a mapping from mode labels to arm types, optionally with a default arm type.

- Form:
  - Value: `.{ M1: v1, M2: v2, ...; vDefault? }`
  - Type: `ModeMap { M1: T1, M2: T2, ...; Default?: TDef }`
- Encoding:
  - Syntactically a Record; semantically "ModeMap" marks that member keys are symbolic labels used for selection.
  - If the default arm is omitted, `Default?` is absent.

Well-formedness:
- All arm expressions `vi` must type-check, yielding `Ti`.
- If present, the default arm must type-check, yielding `TDef`.
- Keys `Mi` must be distinct symbolic labels.

## `.select` Typing

Signature (conceptual): `.select : .Symbol -> ModeMap -> a`

Typing rule:

- Given `.select .M E`:
  - Infer `E : ModeMap { ...; Default?: TDef }`.
  - If there exists an arm labeled `M` with type `TM`, then `(.select .M E) : TM`.
  - Else, if `Default? = TDef` exists, then `(.select .M E) : TDef`.
  - Else, type error (no arm and no default).

Sugar:
- `.Ident E` is elaborated to `.select .Ident E` prior to typing (or treated identically by the typechecker).

## Examples

- Simple selection:
  - Value: `.{ Int: 1, Str: "1" }`
  - Type: `ModeMap { .Int: .Int, .Str: .Str }`
  - `(.select .Int ...) : .Int`

- With default arm:
  - Value: `.{ Pure: x, Str: y; z }`
  - Type: `ModeMap { .Pure: TP, .Str: TS; Default?: TD }`
  - `.select .Bool` yields type `TD` (falls back to default).

- Sugar:
  - `.Int (.{ Int: 1, Str: "1" })` ≡ `.select .Int (.{ Int: 1, Str: "1" })` with type `.Int`.

## Interactions and Constraints

- Annotations: `%{Ty} expr` must unify `expr` with `Ty`. For selection, `%{TM} (.select .M E)` succeeds iff the chosen arm type is `TM`.
- Polymorphism: Mode labels are nominal symbols; there is no implicit subtyping between arm types. Use explicit unions or sum types at the type level if needed.
- Records vs ModeMap: Ordinary records `{ a: T }` are not ModeMaps. Selection by `.select` applies only to ModeMap-typed values.
- Errors: When `E` lacks `M` and has no default, typing fails (aligns with runtime error on selection).

## Future Work (non-normative)

- Generalized mode contexts: Allow ambient `current_mode` to inform typing via conditional types; currently not in scope.
- ModeMap in pattern types: Extend to pattern-level type bindings (e.g., `%{'a} .select .M E`).
- IR lowering: Consider an explicit IR op for `.select`.
