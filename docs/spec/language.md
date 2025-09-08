lzscr Language Specification (Consolidated, as of 2025-09)

This document summarizes the currently implemented specification of lzscr, organized by layers (lexer → parser → semantics → file format → APIs). See the sibling files in this folder for details (tokenizer.md, syntax.md, semantics.md, modules.md, ast.md, ir.md).

- Character encoding: UTF-8
- File extension: .lzscr (MIME: text/vnd.lzscr; charset=utf-8)

1. Lexical level (tokens)

Lexing is implemented with logos (crates/lzscr-lexer). Representative tokens and rules:

- Whitespace/newlines: `[ \t\r\n]+` are skipped
- Comments:
  - Line: `#[^\n]*` (until end of line)
  - Block: `{- ... -}` (nestable)
- Brackets/separators: `{ } ( ) [ ] , ; :`
- Symbols/operators:
  - Reference: `~`
  - Effect prefix: `!`
  - Lambda: `\\`
  - Assignment/equal: `=` (note: `==` is a different token)
  - Arrow: `->`
  - Cons (in patterns, right-associative): `:`
  - As-pattern: `@`
  - Caret (exception): `^`
  - Pipe: `|`, logical or `||`
  - Arithmetic: `+ - * /`
  - Float operators: `.+ .- .* ./`
  - Comparisons: `< <= > >= == !=` and float variants `. < .<= .> .>=`
- Literals:
  - Int: `[0-9]+` → Int(i64)
  - Float: `([0-9]+\.[0-9]*|\.[0-9]+)` → Float(f64)
  - Str: `"([^"\\]|\\.)*"` (simple escapes)
  - Char: `'(?:[^'\\]|\\u\{[0-9a-fA-F]+\}|\\.)'` → Unicode code point (i32)
- Identifiers:
  - `Ident`: `[a-zA-Z_][a-zA-Z0-9_]*`
  - `Member`: `.name` / `.,` (value-level symbol/tag; `.,` is internal)
  - `TyVar`: `%a` etc. (placeholder for type vars/holes syntax)

Notes:
- Comments are preserved in the PRE-AST layer but dropped in parsing.
- `Member` is used as a value-level symbol (tags/namespace lookup).

2. Parsing (expressions, patterns, sugar)

The implementation uses a procedural parser with a PRE-AST front (chumsky as a helper). Major constructs:

- Expressions (Expr):
  - Literals: Unit `()`, Int, Float, Str, Char
  - Reference: `~ident` (statically bound name)
  - Symbol value: `.name` (tag/namespace key)
  - Lambda: `\pat -> expr`
    - Multi-parameter sugar: `\Pat1 Pat2 ... PatN -> Expr` ≡ `\Pat1 -> (\Pat2 -> ... (\PatN -> Expr) ...)`
  - Application: `(f x)` (left-associative; usually prefix-application style without parens)
  - Block: `{ expr }`
  - List/tuple/record (syntactic sugar): implemented (see syntax.md)
  - Let group: a parenthesized group with one or more bindings around a body
    - Concrete source form: `( [Pat = Expr;]* Body [; [Pat = Expr;]*] )`
    - Parse rule: if there is at least one binding in total (leading + trailing ≥ 1), the whole becomes a LetGroup; otherwise it is parsed as a plain grouped expression `(Body)`.
    - Top-level convenience: the CLI wraps file input with parentheses. Therefore, “a sequence of top-level `~x = e;` followed by a final expression” becomes a LetGroup automatically.
  - Let-binding parameter chain sugar (function definition): `~name Pat1 ... PatN = Expr` ≡ `~name = \Pat1 -> ( ... (\PatN -> Expr) ...)`
    - Restriction: within this parameter chain (LambdaLHSParamChain), duplicate binder names across `Pat1..PatN` are forbidden. The explicit nested-lambda form does not have this restriction.
  - Effect sugar: `!name` → `(~effects .name)`, `!{ ... }` → `chain/bind` chaining (do-notation)
  - Exceptions: `^(expr)` (raise) and caret handler `expr ^| handler`
  - Alternative lambda composition: `lam1 | lam2` (try right when the left does not match)

- Patterns:
  - Variable: `~x` (patterns also use the `~` prefix)
  - Wildcard: `_`
  - Basics: `()` / Int / Float / Str / Char / Bool (`true` / `false`)
  - Constructors: `Ctor p1 p2 ...` or `.Ctor p1 ...` (zero-arity must be `Ctor()` / `.Ctor()`)
  - Tuples/lists: `(p1, p2, ...)`, `[p1, p2, ...]`, cons `p : ps` (right-associative)
  - Records: `{ k: p, ... }` (keys are identifiers)
  - Record field lambda sugar (definition): `field Pat1 ... PatN: Expr` ≡ `field: \Pat1 -> ( ... (\PatN -> Expr) ...)`

Notes on precedence and patterns:
- When a parameter chain may conflict with a pattern application, the parameter-chain (LambdaLHSParamChain) takes precedence. If you intend a pattern application at the outermost level of a parameter position, wrap it in parentheses like `(Func Arg1 Arg2)`.
- The “duplicate-binder prohibition” applies only to LambdaLHSParamChain (let LHS and record field sugars). It does not apply to explicit nested lambdas.
  - As-pattern: `p1 @ p2`
  - Type bind: `%{ %a, ?x } p` (also accepts `'a` form; used by typechecker/validator)

- Infix operators and precedence (excerpt): `* /` > `+ -` > comparisons `< <= > >=` > equality `== !=` (see syntax.md for details).

## do-notation (sequential blocks)

Purpose: sugar to write effects and bindings in a readable way. Inside `!{ ... }`, write statements in order.

- Grammar (sketch)
  - `Block ::= !{ Stmt* Last }`
  - `Stmt  ::= Expr ; | Pat <- Expr ;`
  - `Last  ::= Expr` (trailing `;` is optional)
  - `Pat` is any pattern (`~x`, `_`, tuples/lists/records/constructors, ...). Pattern variables must use the `~name` form.

- Desugaring rules (fold right-to-left)
  - Terminal: `E` → `(~bind E (\\x -> x))`
  - Sequencing: `S; ACC` (where `S` is an expression) → `(~chain S ACC)`
  - Binding: `P <- E; ACC` → `(~bind E (\\P -> ACC))`

- Effect context
  - The right-hand sides of `chain/bind` (the `ACC` or the body of `\\... -> ...`) are evaluated in an effect-context (allowed even with strict-effects enabled).

- Failure/exception propagation
  - If `Expr` yields a caret exception (`Raised`), it propagates as-is.
  - If `P <- E` fails to match the value of `E`, a `Raised(E)` is produced and propagates (you can use `lam1 | lam2` or `^|` to recover).

- Scope
  - Bindings introduced by `P <- E` (e.g., `~x`) are visible to subsequent statements and the final expression.

- Tips
  - To discard a result, prefer `S;` or `_ <- S;` (the former desugars to `(~chain S ...)`, the latter to `(~bind S (\\_ -> ...))`).

- Examples
  - Simple sequence:
    `!{ !println "a"; !println "b"; 42 }`
    → `(~chain (!println "a") (~chain (!println "b") (~bind 42 (\\x -> x))))`
  - Pattern binding (success):
    `!{ (.Some ~x) <- (.Some 10); !println (~to_str ~x); () }`
    → `(~bind (.Some 10) (\\(.Some ~x) -> (~chain (!println (~to_str ~x)) (~bind () (\\y -> y)))))`
  - Pattern binding (fallback on failure):
    `((\\(.Some ~x) -> x) | (\\.None -> 0)) ((!{ v <- f(); v }))`

Note: Always use `~` on pattern variables (write `~x`, not `x`).

3. Semantics (evaluation, effects, exceptions)

- Evaluator (crates/lzscr-runtime):
  - Values: Unit / Int / Float / Bool / Str (UTF-8 string) / Char / List / Tuple / Record / Ctor / Native / Closure / Symbol / Raised / Thunk
  - Env: variable map, strict-effects flag, effect-context flag, constructor arity table, symbol/string interning
  - Application: Native functions are curried and execute on saturation; Closures pattern-match arguments then evaluate the body.
  - Special forms: `seq/chain/bind` control order and effect-context (including pattern-binding execution).

- Effect discipline (strict-effects):
  - Effectful functions (e.g., `(~effects .println)`) can run only in an effect-context when strict-effects is enabled.
  - The effect-context is established on the right-hand side of `seq/chain/bind`.

- Caret exceptions:
  - `^(e)` builds a `Raised` value carrying `e` and propagates it.
  - `x ^| handler` tries `handler` only when `x` failed via caret.
  - `lam1 | lam2` applies the right lambda only when the left failed to match (desugars to the `~alt` builtin).

- Pattern binding:
  - On success, binds into pre-allocated slots; on failure, produces a caret-style error that propagates.

- Representative builtins (Bool-like results are represented as `Symbol("True"|"False")`):
  - `to_str : a -> Str` (rendering)
  - `add/sub/mul/div : Int -> Int -> Int` (divide by zero is an error)
  - `fadd/fsub/fmul/fdiv : Float -> Float -> Float`
  - `eq/ne/lt/le/gt/ge : same -> same -> Bool-like`
  - logical ops: `and/or : Bool-like -> Bool-like -> Bool-like`, `not : Bool-like -> Bool-like`
  - conditional: `if : Bool-like -> a -> a -> a`
  - `seq : a -> b -> b`, `chain : m -> (Unit->k) -> k`, `bind : m -> (x->k) -> k`
  - `effects .print/.println : Str|basic -> Unit`
  - Namespaces `Builtins.string/char/math/scan/unicode` (see API below)

See semantics.md for more.

4. File format (modules and layout)

- Encoding is UTF-8; extension is `.lzscr`.
- Recommended module layout:
  - Put private defs at the top as `~name = expr;` and return a public record as the final expression:
    `~helper = ...; { publicFn: \x -> ...; publicConst: 42 };`
- Module resolution `~require`:
  - Syntax: `(~require .seg1 .seg2 ... .segN)` (arguments must be `.name` symbols)
  - Path: searches `seg1/seg2/.../segN.lzscr` (current → `--stdlib-dir` → `--module-path`)
  - Inlined into the AST before evaluation (hygienic). See modules.md.

- Formatter/CLI:
  - Use `lzscr-cli --format-code` (options: `--fmt-indent`, `--fmt-width`).

5. Key APIs (builtins / stdlib)

Type hints below are informal (to be aligned with the HM type inference). Bool note as above.

- Core (top-level)
  - `to_str : a -> Str`
  - `add, sub, mul, div : Int -> Int -> Int`
  - `fadd, fsub, fmul, fdiv : Float -> Float -> Float`
  - `eq, ne, lt, le, gt, ge : t -> t -> Bool-like` (returns `Symbol("True"|"False")`)
  - `and, or : Bool-like -> Bool-like -> Bool-like`, `not : Bool-like -> Bool-like`
  - `if : Bool-like -> a -> a -> a`
  - `seq : a -> b -> b`, `chain : m -> (Unit -> k) -> k`, `bind : m -> (x -> k) -> k`
  - `effects .print : Str|basic -> Unit`, `effects .println : Str|basic -> Unit`

- Builtins.string (`~Builtins .string`)
  - `len : Str -> Int` (character count)
  - `concat : Str -> Str -> Str`
  - `slice : Str -> Int -> Int -> Str` (index in characters)
  - `char_at : Str -> Int -> .Some(Char) | .None`

- Builtins.char (`~Builtins .char`)
  - `is_alpha : Char -> Bool-like`, `is_digit : Char -> Bool-like`, `is_alnum : Char -> Bool-like`, `is_space : Char -> Bool-like`
  - `between : Char -> Int -> Int -> Bool-like` (code point range)

- Builtins.math (`~Builtins .math`)
  - Re-exports arithmetic/comparison in a namespace

- Builtins.scan (`~Builtins .scan`)
  - `new : Str -> Scan` (`Scan = { s: Str, i: Int }`)
  - `eof : Scan -> Bool-like`, `pos : Scan -> Int`, `set_pos : Scan -> Int -> Scan`
  - `peek : Scan -> .Some(Char) | .None`
  - `next : Scan -> .Some((Char, Scan)) | .None`
  - `take_if : (Char -> Bool-like) -> Scan -> .Some((Char, Scan)) | .None`
  - `take_while : (Char -> Bool-like) -> Scan -> (Str, Scan)`
  - `take_while1 : (Char -> Bool-like) -> Scan -> .Some((Str, Scan)) | .None`
  - `slice_span : Scan -> Int -> Int -> Str`

- Builtins.unicode (`~Builtins .unicode`)
  - `of_int : Int -> Char`, `to_int : Char -> Int`

- stdlib/prelude (`stdlib/prelude.lzscr`, typically auto-loaded)
  - Composition/identity: `id`, `compose`, `flip`, `const`, `compose2`, `pipe`
  - Bool wrappers: `bool_and, bool_or, bool_not`
  - List: `length, head_opt, tail_opt, foldl, foldr, map, append, reverse, any, all`
  - Option: `option_is_some, option_is_none, option_map, option_bind, option_unwrap_or`
  - Result: `result_is_ok, result_is_err, result_map, result_map_err, result_bind, result_unwrap_or`
  - Delegations to String/Char/Math/Unicode/Scan: export `string/math/char/unicode/scan` fields and helpers

Notes:
- Zero-arity constructors must be written as `Ctor()` / `.Ctor()` (bare `Ctor` is parsed as a variable name).
- Provide sugar where `true()` / `false()` expand to `~true` / `~false`.

---

6. Type system (current implementation: HM rank-1 + annotations/type values/pattern type binders)

This section documents the implemented type system as of 2025-09-06. It is Hindley–Milner-like (rank-1) with polymorphic let, plus type annotations (`%{...}`), type values, and pattern-level type variable binding. Effects/kinds are not yet integrated.

6.1 Overview and scope

- Inference: HM rank-1 with polymorphic let. LetGroup performs generalization; LetRec may require annotations in some cases due to current limitations.
- Extra syntax: type annotation `%{Type} expr`, type value `%{Type}`, type variables `%a` (leading `%`), holes `?name` / `?`, and pattern-level type variable binding `%{ %a, ?x } pat`.
- Effects/kinds: not integrated yet (future work).

6.2 Types

- Primitives: `Unit | Int | Float | Bool | Str`
- Variables: `Var(α)` (unification variables)
- Function: `Fun(T1, T2)` (notation: `T1 -> T2`)
- Structures: `List(T) | Tuple(T1,..,Tn) | Record({k1:T1,..})` (records are closed; key sets must match)
- Constructors: `Ctor<'Tag, Payload>` (where `'Tag` is a bare symbol name; `Payload` is usually a tuple type)
- (Internal) Union: finite sum `SumCtor([('Tag, [T...]), ...])` derived from AltLambda chains; no external surface syntax.

Note: current printed forms are shorthand aligned with implementation. Records require exact key set equality.

6.3 Type expressions (TypeExpr) and syntax

Used in type annotations and type values:

- Literals: `Unit, Int, Float, Bool, Str`
- Structures: `List T`, `Tuple(T1, ..., Tn)`, `Record{ a: T, b: U }`, `T1 -> T2`
- Constructors: `Foo T1 ... Tn` (where `Foo` is a bare symbol name)
- Type variables: `%a` (leading `%`; `'a` is accepted for compatibility but `%a` is recommended)
- Holes: `?x` (shared within the same annotation), `?` (fresh variable each time)

Interpretation rules (conv_typeexpr):
- `%a` resolves within the current scope (see “Pattern type variable binding” below). Unresolved names are errors.
- `?x` is a shared type variable within the same annotation; `?` introduces a fresh variable each occurrence.
- `Foo ...` converts to `Ctor<'Foo, Payload>`. With zero args, `Payload = Unit`.

6.4 Type annotations and type values

- Type annotation: `%{ Type } expr`
  - During inference, unify the inferred type of `expr` with `Type`.
  - At runtime, this is a no-op (annotations don’t affect values).
- Type value (TypeVal): `%{ Type }`
  - For now, this is represented as `Str` (human-readable type string). A dedicated type/value is planned in the future.

Examples:

```
%{ List Int } [1,2,3]          # OK
%{ List ?a } [1,2]             # a resolves to Int
%{ %a -> %a } (\~x -> ~x)      # annotation for id
```

6.5 Pattern-level type variable binding (TypeBind)

- Syntax: `%{ %a, %b, ?x, ... } pat`
  - Introduces a scope for named type variables/holes that can be referenced by annotations immediately surrounding the pattern and within the pattern’s subtree.
  - Applicable on lambda parameter patterns and LetGroup LHS patterns. The scope lives during the right-hand side inference/annotation interpretation.
- Type-system only; transparent to runtime/IR/printers. Semantically equivalent to `pat` alone when executing.

Examples:

```
(\%{ %a } ~x -> %{ %a } ~x)        # share the same %a (id)
let %{ %a, ?k } (~f, ~v) = (~id, 1) in ...
```

6.6 AltLambda typing (pattern-branching lambdas)

- Syntax: `(\pat1 -> e1) | (\pat2 -> e2) | ...`
- Rules:
  - Basic: each branch types as `a -> r` and must unify to the same `a` and `r`.
  - If any branch uses a constructor pattern, all branches must be constructor patterns (or a final `_`/variable-only catch-all).
  - The argument type is aggregated as a finite sum `SumCtor([.Foo(α), .Bar(β,γ), ...])`. Duplicate tags or mismatched arities/types are errors.
  - A default branch (`_`, etc.) does not extend the sum; it accepts the existing sum.

Example:

```
(\(.Foo ~x) -> ~x) | (\(.Bar ~y ~z) -> ~z)   # argument is SumCtor([.Foo(α), .Bar(β,γ)])
```

6.7 Typing for caret/OrElse (excerpt)

- `^(e)`: payload `e: γ`. The expression type is treated as arbitrary `ρ` (bottom/does not return).
- `x ^| h`: requires `x: ρ` and `h: γ -> ρ`.
- `e1 || e2`: both sides unify to the same `ρ`.

6.8 Constructors and arity

- A bare symbol `Foo` denotes a constructor function with principal type `∀a1..an. a1 -> .. -> an -> Ctor<'Foo,(a1,..,an)>`.
- Arity is checked against the parser/CLI setting (`--ctor-arity`). Zero-arity constructors should be written explicitly when required by the chosen surface syntax.

6.9 Main builtin types

- `add/sub/mul/div : Int -> Int -> Int`
- `eq/ne : ∀a. a -> a -> Bool-like` (Symbol("True"|"False"))
- `lt/le/gt/ge : Int|Float -> Int|Float -> Bool-like` (Symbol("True"|"False"))
- `flt/fle/fgt/fge : Float -> Float -> Bool-like`
- `cons : ∀a. a -> List a -> List a`
- `to_str : ∀a. a -> Str`
- `alt : ∀a r. (a->r) -> (a->r) -> a -> r`

6.10 CLI integration

- Pipeline: `parse → analyzer → typecheck → eval`
- `--no-typecheck` disables inference (temporary escape hatch for current limitations).
- Future: richer output (pretty-printed types/JSON) may be added.

6.11 Limitations and notes

- LetRec is not fully supported; self- and mutual-recursive definitions may require annotations.
- Complex combinations of List and AltLambda may benefit from explicit annotations.
- Type value `%{Type}` is currently represented as `Str` (temporary).
- Pattern type binders only extend type scope; they do not change runtime semantics.

6.12 Samples

```
# Annotation with shared/fresh holes
%{ List ?a } [1,2]            # a = Int
%{ List ? } []                # fresh variable, decided by later use

# Pattern type binders (lambda)
(\%{ %a } ~x -> %{ %a } ~x)   # share the same %a

# AltLambda (finite sum of Ctors)
let f = (\(A ~x) -> ~x) | (\(B ~y ~z) -> ~z) in ~f
```

Appendix: the Analyzer/Runtime/CoreIR treat `PatternKind::TypeBind` transparently (printing/execution behave as before).

---
This consolidated spec will be updated as the PoC evolves. Changes will also be reflected in the per-topic documents (syntax/semantics/modules, etc.).