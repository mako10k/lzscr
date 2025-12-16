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
  - If the default arm is omitted in source, the typechecker assigns `Unit` as the default arm.

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

## ModeKind and implicit ModeMap views

- Rationale: In code the typechecker treats many values as implicitly viewable as a ModeMap for the purposes
  of selection. For example a record whose fields are themselves ModeMaps can be "transposed" by selecting
  the same mode from each field. To track how many implicit view/wrap steps were applied we record a
  small `ModeKind` annotation on `ModeMap` types.

- Representation: `ModeMap { ...; Default }` carries a `ModeKind` that is either `Explicit` (created from
  a `.{} ` literal in source) or `Implicit(n)` where `n: u8` counts implicit-view applications (saturated).

- Implicit wrapping rules:
  - Any non-`ModeMap` value may be treated as an implicit ModeMap with a single default arm equal to the
    entire value; the typechecker wraps such values into `ModeMap(..., Default = value, ModeKind::Implicit(1))`.
  - When projecting a field `M` from a `Record` whose fields are ModeMaps, the typechecker forms a result
    `Record` whose per-field types are the corresponding arm (or default) types; implicit wrappers increment
    the `Implicit` counter for bookkeeping.
  - For algebraic constructors and tuples/lists, selection transposes over payload/element positions: selecting
    `.M` from `Ctor tag p1 p2 ...` yields `Ctor tag p1' p2' ...` where each `pi'` is the projection of `.M`
    from `pi` (wrapping non-ModeMap `pi` as necessary).

- Semantics of unresolved arms: An unresolved arm (a type variable) is treated as "not matching any explicit
  keys" and therefore the default arm is used for selection; implicit wrapping ensures a default is always
  available for selection contexts introduced by view application.

This design guarantees that selection (`.Ident`) always yields a well-formed type (possibly involving fresh
type variables) and that chains of implicit views are traceable through the `Implicit(n)` counter. It also
makes it straightforward to present diagnostics about how many implicit projections were used when resolving
a selection.

## Future Work (non-normative)

- Generalized mode contexts: Allow ambient `current_mode` to inform typing via conditional types; currently not in scope.
- ModeMap in pattern types: Extend to pattern-level type bindings (e.g., `%{'a} .select .M E`).
- IR lowering: Consider an explicit IR op for `.select`.
