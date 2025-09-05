# lzscr Analyzer

AST ベースの簡易静的解析。重複検出・未束縛参照・シャドーイング・未使用パラメータを報告します。

- 実装: `crates/lzscr-analyzer`
- CLI: `lzscr-cli -e "..." --analyze [--format json] [--dup-min-size N] [--dup-min-count M]`

## 出力

- text: 各種 findings を1行ずつ
- json: 以下のスキーマ（簡略）
  - `duplicates: [{ size, count, span:{offset,len}, repr }]`
  - `unbound_refs: [{ name, span }]`
  - `shadowing: [{ name, lambda_span }]`
  - `unused_params: [{ name, lambda_span }]`

## 参考

- 既定の許可リスト: `default_allowlist()`
- 最小サイズや出現回数の閾値は PoC として簡易に設定可能。
