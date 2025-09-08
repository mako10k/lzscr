# lzscr type definitions (mu-types / isorecursive) design notes

This document records the plan to add user type definitions (% declarations) and introduce mu-types (isorecursive) in the internal representation. We first agree on the spec and then implement in stages.

## Goals
- Remove list-specific guessing and support general recursive ADTs soundly.
- Allow named types (List/Option, etc.) to be used in polymorphic annotations.
- Stay compatible with the existing HM inference (termination and error messages should improve).

## Approach (Plan C)
- Users declare types with `%`.
- Detect “shape-identical self references” in RHS and convert them to mu-types internally (isorecursive).
- Unification unfolds μ one step only when needed.
- Known families (List/Option/Bool, etc.) remain printable and annotatable by name while being convertible to/from their internal structure.

## Syntax (draft)
- Top-level type declarations
  - `%TypeName %a %b = %{ .Tag TYPE | .Tag TYPE | ... }`  // sum
  - Future: `%TypeName %a ... = %record{ field: TYPE, ... }`  // record
- Examples
  - `%Option %a = %{ .Some %a | .None }`
  - `%List %a = %{ .Nil | .Cons %a (%List %a) }`

Inside type expressions:
- `%a` are type variables.
- `.Tag` are constructor labels (may share the label space with value-level).
- `%Name %args...` applies a named type.

## Self-reference -> μ conversion (shape rule)
- Target: occurrences of the LHS head name in the RHS that are shape-identical to the LHS head application.
- “Shape-identical” means:
  - Exactly the same list of type variables in the same order (only alpha-renaming allowed).
  - Example: in `%List %a` the RHS `%List %a` is shape-identical; `%List (%Foo %a)` or partial applications are not.
- Steps:
  1) Introduce a fresh bound type variable `%T`.
  2) Replace all shape-identical self references `%Head %params` in RHS with `%T`.
  3) Represent as `μ %T . Body` (Body may be sum/record/tuple etc.).
- Example (List):
  - Input: `%List %a = %{ .Nil | .Cons %a (%List %a) }`
  - Internal: `μ %T . sum { .Nil | .Cons %a %T }`

Option doesn’t need μ:
- Input: `%Option %a = %{ .Some %a | .None }`
- Internal: `sum { .Some %a | .None }`

## Scope
- LHS args (`%a`, etc.): only valid within that declaration’s RHS.
- Self binder `%T`: introduced by the internal conversion (not referenceable externally).
- LHS head name (`%List`, etc.): bound in the type namespace, mutually visible within the group, exported outside.

## Mutually recursive groups
- Treat a consecutive `%`-declaration sequence as one group and process in two passes.
  - Pass 1: register head names and arities.
  - Pass 2: perform the “shape self-ref -> %T -> μ” transform in each RHS. References to other types remain Named.
- Mutual recursion is allowed (A can reference B and vice versa). Only self references are replaced by `%T`.

## Soundness checks
- Positivity: in `μ %T . Body`, `%T` must appear only in positive positions.
  - Allowed: element positions of sum/record/list/tuple (covariant).
  - Disallowed: negative positions such as function argument types.
- Shape constraint: self-references must be shape-identical.
- Unfold termination: μ-unfolding is on-demand and only one step. Use a visited set/fuel to avoid divergence.

## Unification strategy (isorecursive)
- `unify(μ %T . A, B)`:
  - When needed, unfold `A[%T := μ %T . A]` exactly once and compare with B (and vice versa).
- occurs check:
  - Performed for regular type variables. `%T` is a binder and excluded.
  - For var vs μ, peel μ by one step first, then decide.

## Compatibility with existing code (migration)
- Normalization of known families:
  - Prefer displaying/annotating as `%List %a` / `%Option %a`.
  - Internally stay convertible to existing structures (List type / SumCtor) during migration.
- Keep Prelude/existing code compatible. Allow type names in annotations to improve readability.

## Implementation slices (phased)
1. Parser
  - Add `%` type declarations (head name, type variable list, RHS type expr).
  - Add `Mu(T, Body)` and `Named(name, args)` to type expressions.
2. Type definition environment
  - `TypeDefEnv`: name → (params, body).
  - Two-pass registration + “shape self-ref → μ” transform.
3. Type representation/unification
  - One-step μ-unfold unification, positivity check, visited management.
  - On-demand unfolding for Named (compare only required parts, avoid full expansion).
4. Display/errors
  - Show known families by name; if shapes match, bias to Named.
  - Clear diagnostics with spans for occurs/positivity failures.
5. Bridging
  - Interoperate with existing List/Option/Bool. Initially keep internal reps side-by-side; consider unifying later.

## Open items (to agree)
- How to group (file/blank-lines/block)
- Label space (.Tag) sharing policy (likely fine as-is)
- Equality for Named: operate on structural equality after full expansion (no nominal equality)
- Strictness of positivity check (start by forbidding negative positions in function types only)

## Reference snippets
- Option:
  - Source: `%Option %a = %{ .Some %a | .None }`
  - Internal: `sum { .Some %a | .None }`
- List:
  - Source: `%List %a = %{ .Nil | .Cons %a (%List %a) }`
  - Internal: `μ %T . sum { .Nil | .Cons %a %T }`

---
If we proceed with this spec, first add the parser and typedef environment (phases 1-2). After implementation, verify annotations and unification using representative functions like `~map` and `~length`.

  ## Integration into Let (type-decl grouping and scope)

  Integrate type declarations (%) into let-expressions. Extend grammar as follows:

  - Old: `letExpr ::= bind* body bind*` (at least one bind before/after)
  - New: `letExpr ::= (bind | typedeclare)* body (bind | typedeclare)*`

  Here `typedeclare` refers to `%TypeName %a ... = %{ ... }` in this doc.

  Scope/resolution rules:
  - Treat a consecutive sequence of `(bind | typedeclare)` inside a let block as one group, and process value and type namespaces as mutually recursive.
  - Two-pass (type namespace):
    1) Pass 1: register `%TypeName` and arity (shadowable).
    2) Pass 2: transform RHS via “shape self-ref → %T → μ” and register into `TypeDefEnv`.
  - Values (`bind`) can reference `typedeclare` within the same let-group.
  - `typedeclare` can reference outer type names (static scope).
  - LHS type variables (`%a`, etc.) are valid only within the decl RHS. The self binder `%T` is internal-only.

  Example (mutual reference inside let):
  ```
  let
    %List %a = %{ .Nil | .Cons %a (%List %a) }
    ~length = \(~xs) ->
      \[] -> 0
    | \[ ~_ : ~t ] -> 1 + (~length ~t)
  in
    ~length [1,2,3]
  ```
  In this example, `%List` is bound within the let-group and can be referenced from `~length`. `%List` is exported outside the group as well.
