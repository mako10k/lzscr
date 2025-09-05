# AST（現状）

定義: `crates/lzscr-ast`

- `Span { offset, len }`（serde対応）
- `Expr { kind: ExprKind, span: Span }`
- `ExprKind`:
  - `Unit | Int(i64) | Str(String) | Ref(String) | Symbol(String)`
  - `Lambda { param: String, body: Box<Expr> }`
  - `Apply { func: Box<Expr>, arg: Box<Expr> }`
  - `Block(Box<Expr>)`

備考:
- レキサ/パーサは `logos` ベース。トークンに `Bang(!)` と `Member(.sym)` を追加、`!sym` を `(~effects .sym)` にデシュガ。
- 文字列は簡易エスケープをサポート。
