# 言語概要（現状）

- 目的: 遅延評価ベースの式言語のPoC実装。
- 実行経路: いまはAST直実行（評価器）に加え、Core IRを導入（AST→IRロワリング、テキスト化）。
- 実装済みサブセット:
  - 値: Unit, Int, Str, Symbol（裸/コンストラクタ変数候補）, Lambda, Native（ビルトイン）, Closure
  - 式: Unit, Int, Str, Ref(~name), Symbol, Lambda(\x -> e), Apply(e1 e2), Block({ e })
  - ビルトイン: to_str, add, sub, eq, lt, seq, effects(.print/.println)
  - strict-effects: 有効時、効果は(~seq a b)の第2引数文脈でのみ許可。
  - 糖衣: !sym → (~effects .sym), .name はシンボル（Symbol）として扱う。
- 解析: 重複検出/未束縛参照/シャドウ/未使用引数（CLI --analyze）
- ツール: CLI（-e評価、--strict-effects、--analyze、--format json）。CIでfmt/clippy/test/audit/deny/coverage。
