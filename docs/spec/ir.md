# Core IR（PoC）

定義: `crates/lzscr-coreir`

- 型 `Ty`:
  - `Unit | Int | Str | Fun(Box<Ty>, Box<Ty>) | Dyn`
- 項 `Op` と `Term`:
  - `Unit | Int(i64) | Str(String)`
  - `Ref(String) | Symbol(String)`
  - `Lam { param, body } | App { func, arg } | Seq { first, second }`
- モジュール `Module { body: Term }`

ロワリング `lower_expr_to_core(&Expr) -> Term`:
- ASTの `(~seq a b)` を `Seq { first=a, second=b }` へ変換。
- それ以外は同形にマッピング。

テキスト化:
- `print_term(&Term)` により、人間可読の簡易表現を出力（将来的に専用テキスト/バイナリ形式を定義）。

CLI サポート:
- `lzscr-cli -e "..." --dump-coreir` でテキスト出力
- `lzscr-cli -e "..." --dump-coreir-json` で JSON 出力

例:
```
$ lzscr-cli -e "(~seq 1 (~add 2 3))" --dump-coreir
(~seq 1 ((~add 2) 3))
```
