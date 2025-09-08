# Core IR (PoC)

Location: `crates/lzscr-coreir`

- Types `Ty`:
  - `Unit | Int | Float | Bool | Str | Fun(Box<Ty>, Box<Ty>) | Dyn`
- Operators `Op` and `Term`:
  - `Unit | Int(i64) | Str(String)`
  - `Ref(String) | Symbol(String)`
  - `Lam { param, body } | App { func, arg } | Seq { first, second }`
- Module `Module { body: Term }`

Lowering `lower_expr_to_core(&Expr) -> Term`:
- Convert AST `(~seq a b)` into `Seq { first=a, second=b }`.
 - `(~chain a b)`, `(~bind e k)` are currently handled as AST special forms by the evaluator (may become IR later).
- Everything else maps shape-wise.

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
