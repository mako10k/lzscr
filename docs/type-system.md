## lzscr type system (current: HM rank-1 + annotations/type values/pattern type vars)
Disclaimer: Snapshot as of 2025-09-06. Any "future" references are exploratory (WIP) and not commitments.

This document describes the implemented design as of 2025-09-06. It centers on Hindley–Milner (rank-1) inference, with type annotations (`%{...}`), first-class type values, and pattern-level type variable binders.

### 1) Overview and scope

- Inference: rank-1 HM (with polymorphic let). LetGroup generalizes; LetRec may require annotations in some cases due to current limits.
- Additional syntax: type annotation `%{Type} expr`, type value `%{Type}`, type variables `%a` (leading `%`), holes `?name`/`?`, and pattern type binders `%{ %a, ?x } pat`.
- Effects/Kinds are not integrated yet (future work).

### 2) Kinds of types (Type)

- Primitives: `Unit | Int | Float | Bool | Str`
- Variables: `Var(α)` (unification variables)
- Function: `Fun(T1, T2)` (notation `T1 -> T2`)
- Structures: `List(T) | Tuple(T1,..,Tn) | Record({k1:T1,..})` (Records are closed)
- Constructors: `Ctor<'Tag, Payload>` (`'Tag` is a bare symbol name; `Payload` is a tuple type, etc.)
- (Internal) Union: argument type of AltLambda branches is collected into a finite sum `SumCtor([('Tag, [T...]), ...])` (no external syntax).

Note: The current type pretty-print depends on implementation shortcuts. Records require an exact key-set match.

### 3) Type expressions (TypeExpr) and syntax

Type expressions used in annotations and type values include:

- Literals: `Unit, Int, Float, Bool, Str`
- Structures: `List T`, `Tuple(T1, ..., Tn)`, `Record{ a: T, b: U }`, `T1 -> T2`
- Constructors: `.Foo T1 ... Tn` (constructor tags are value-level symbols prefixed with a dot)
- Type variables: `%a` (leading `%`. For backward compat we also accept `'a`, but `%a` is recommended)
- Holes: `?x` (shared by name within one annotation) / `?` (fresh each occurrence)

Interpretation rules (conv_typeexpr):
- `%a` resolves within the current scope (see "pattern type variable binder" below). Unresolved is an error.
- `?x` is a shared unification var within the same annotation, `?` is fresh each time.
- `.Foo ...` converts to `Ctor<'Foo, Payload>`. When given 0 args, `Payload=Unit`.

### 4) Type annotations and type values

- Annotation: `%{ Type } expr`
  - During inference: unify the inferred type of `expr` with `Type`.
  - At runtime: identity (annotation does not affect the value).
- Type value (TypeVal): `%{ Type }`
  - Currently represented as `Str` (string form of the type). Will be upgraded to a dedicated type/value later.

Examples:
```
%{ List Int } [1,2,3]          # OK
%{ List ?a } [1,2]             # a resolves to Int
%{ %a -> %a } (\~x -> ~x)      # id annotation
```

### 5) Pattern-level type variable binder (TypeBind)

- Syntax: `%{ %a, %b, ?x, ... } pat`
  - Immediately before the pattern, extends the scope with type variable/hole names that annotations and nested type expressions can reference.
  - Applies to lambda parameters and LetGroup LHS patterns. These names are pushed during RHS inference and annotation interpretation.
- This affects types only. Runtime/IR/printers treat it transparently, equivalent to `pat`.

Examples:
```
(\%{ %a } ~x -> %{ %a } ~x)        # share the same %a (id)
let %{ %a, ?k } (~f, ~v) = (~id, 1) in ...
```

### 6) AltLambda (pattern-branching lambda)

- Syntax: `(\pat1 -> e1) | (\pat2 -> e2) | ...`
- Rules:
  - Base: each branch has type `a -> r` and they unify to the same `a` and `r`.
  - If any branch uses constructor patterns, all branches must be constructor patterns (or a final wildcard/variable only).
  - The argument type is aggregated as `SumCtor` (finite sum). Duplicate tags or shape mismatches are errors.
  - The default branch (`_` etc.) does not add new cases (it accepts the existing sum).

Example:
```
(\(.Foo ~x) -> ~x) | (\(.Bar ~y ~z) -> ~z)   # a is SumCtor([.Foo(α), .Bar(β,γ)])
```

### 7) Exceptions/OrElse typing (excerpt)

- `^(e)`: payload `e: γ`. The expression type is treated as arbitrary `ρ` (bottom reached).
- `x ^| h`: requires `x: ρ` and `h: γ -> ρ`.
- `e1 || e2`: unify to the same type `ρ`.

### 8) Constructors and arity

- A bare member symbol `.Foo` acts as a "constructor function". Its principal type is `∀a1..an. a1 -> .. -> an -> Ctor<'Foo,(a1,..,an)>`.
- Arity is enforced against the parser/CLI-provided map (`--ctor-arity`). The surface notation follows the .Member style.

### 9) Major builtin types

- `add/sub/mul/div : Int -> Int -> Int`
- `fadd/fsub/fmul/fdiv : Float -> Float -> Float`
- `eq/ne : ∀a. a -> a -> Bool`
- `lt/le/gt/ge : Int -> Int -> Bool`
- `flt/fle/fgt/fge : Float -> Float -> Bool`
- `cons : ∀a. a -> List a -> List a`
- `to_str : ∀a. a -> Str`
- `alt : ∀a r. (a->r) -> (a->r) -> a -> r`

### 10) CLI integration

- Pipeline: `parse → analyzer → typecheck → eval`
- `--no-typecheck` can disable inference.
- Output mode: `--types pretty|json` (current behavior; subject to change).

### 11) Limitations and notes

- Full LetRec support is not complete; self/mutual recursion may require annotations.
- Annotations can be useful when composing List and AltLambda in complex ways.
- Type values `%{Type}` are currently treated as `Str` (provisional behavior).
- Pattern type variable binders extend only the type scope; runtime semantics do not change.

### 12) Examples

```
# Annotations and holes (shared/fresh)
%{ List ?a } [1,2]            # a=Int
%{ List ? } []                # fresh var (decided at use sites)

# Pattern type variable binder (lambda)
(\%{ %a } ~x -> %{ %a } ~x)   # share the same %a

# AltLambda (finite sum of ctor cases)
let f = (\(.A ~x) -> ~x) | (\(.B ~y ~z) -> ~z) in ~f
```

Note: Analyzer/Runtime/CoreIR treat `PatternKind::TypeBind` transparently (printing/execution behave the same as without it).

