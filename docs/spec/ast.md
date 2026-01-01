# AST (current)

Location: `crates/lzscr-ast`

- `Span { offset, len }` (serde enabled)
- `Expr { kind: ExprKind, span: Span }`
- `ExprKind`:
  - Literals: `Unit | Int(i64) | Float(f64) | Str(String) | Char(i32)`
  - Names: `Ref(String)` (e.g. `~x`) / `Symbol(String)` (e.g. `.println`)
  - Structural: `List(Vec<Expr>) | Record(Vec<ExprRecordField>) | ModeMap(Vec<ExprRecordField>)`
  - Functions: `Lambda { param: Pattern, body: Box<Expr> } | Apply { func, arg }`
  - Types: `Annot { ty: TypeExpr, expr } | TypeVal(TypeExpr)`
  - Groups/Control: `LetGroup { ... } | Raise | AltLambda | OrElse | Catch`
- `Pattern { kind: PatternKind, span: Span }` (used by `Lambda` params and let bindings)
- Records in Expr/Pattern/TypeExpr track field-name spans via `ExprRecordField` / `PatternRecordField` / `TypeExprRecordField`.

Notes:
- Lexer/Parser use `logos`. Tokens add `Bang(!)` and `Member(.sym)`. `!sym` desugars to `(~effects .sym)`.
- String/char literal escapes are implemented in `crates/lzscr-lexer`.
