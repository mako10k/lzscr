# AST (current)

Location: `crates/lzscr-ast`

- `Span { offset, len }` (serde enabled)
- `Expr { kind: ExprKind, span: Span }`
- `ExprKind`:
  - `Unit | Int(i64) | Str(String) | Ref(String) | Symbol(String)`
  - `Lambda { param: String, body: Box<Expr> }`
  - `Apply { func: Box<Expr>, arg: Box<Expr> }`
  - `Block(Box<Expr>)`

Notes:
- Lexer/Parser use `logos`. Tokens add `Bang(!)` and `Member(.sym)`. `!sym` desugars to `(~effects .sym)`.
- Strings support simple escapes.
