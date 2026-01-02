# Core IR (PoC)

Location: `crates/lzscr-coreir`

- Types `Ty`:
  - `Unit | Int | Float | Bool | Str | Fun(Box<Ty>, Box<Ty>) | Dyn`
- Operators `Op` and `Term`:
  - `Unit | Int(i64) | Float(f64) | Bool(bool) | Str(String) | Char(i32)`
  - `Ref(String) | Symbol(String)`
  - `List { items: Vec<Term> }`
  - `Record { fields: Vec<RecordFieldTerm> }` (tracks `name_span`)
  - `ModeMap { fields: Vec<RecordFieldTerm> }` (tracks `name_span`)
  - `Lam { param, body } | App { func, arg }`
  - `Seq { first, second } | Chain { first, second } | Bind { value, cont }`
  - `Raise { payload } | Catch { left, right } | OrElse { left, right }` (exceptions/control)
  - `Alt { left, right }` (AltLambda)
- Module `Module { body: Term }`

Lowering `lower_expr_to_core(&Expr) -> Term`:
- Convert AST `(~seq a b)` into `Seq { first=a, second=b }`.
- Convert AST `(~chain a b)` into `Chain { first=a, second=b }`.
- Convert AST `(~bind e k)` into `Bind { value=e, cont=k }`.
- Convert AST `^(e)` / `(a ^| h)` / `(a || b)` into `Raise` / `Catch` / `OrElse`.
- Convert AST `(l | r)` (AltLambda) into `Alt { left=l, right=r }`.
- Convert AST `[a, b, c]` into `List { items: [a, b, c] }`.
- Convert AST `{ k1: e1, k2: e2 }` into `Record { fields: ... }` and preserve each field's `name_span`.
- Convert AST `.{ M1: e1, M2: e2 }` into `ModeMap { fields: ... }` and preserve each field's `name_span`.
- Everything else maps shape-wise.

Evaluator conventions (PoC)

`crates/lzscr-coreir` includes a minimal evaluator for debugging (`--eval-coreir`).
It is not the language runtime, but it tries to follow the same *value-level* conventions where possible.

- `Op::Bool(true/false)` evaluates to constructor values `True` / `False`.
- `Op::Symbol(s)` evaluates using a small heuristic:
  - `"[]"` becomes an empty list value.
  - Symbols that start with `.` and are **not** tuple tags are treated as atomic symbols (e.g. `.Int`, `.Pure`).
  - Everything else is treated as a constructor value (curried via application).
- Tuple tags are encoded as `.` followed by one or more commas, e.g. `.,` / `.,,`.

These conventions exist to keep CoreIR dumps and the PoC evaluator usable while the IR is still evolving.

Textual form:
- `print_term(&Term)` prints a human-readable form (future: dedicated text/binary format).

CLI support:
- `lzscr-cli -e "..." --dump-coreir` prints a textual dump
- `lzscr-cli -e "..." --dump-coreir-json` prints a JSON dump

Example:
```
$ lzscr-cli -e "(~seq 1 (~add 2 3))" --dump-coreir
(~seq 1 ((~add 2) 3))
```
