# lzscr (work-in-progress)

最小サブセットを持つ LazyScript 再実装の骨組みです。
- ints, strings, `~ref`, 裸シンボル, ラムダ, 関数適用, ブロック。
- 簡易ランタイム評価器といくつかのビルトインを含みます（`to_str`, `add`, `sub`, `eq`, `lt`, `seq`, `print`, `println`）。

試す:

```
cargo run -p lzscr-cli -- -e '(~add 1 2)'
# => 3

cargo run -p lzscr-cli -- -e '(~to_str 42)'
# => 42

cargo run -p lzscr-cli -- -e '(~println "hi")'
# 標準出力: hi\n, 最終値: () を表示

cargo run -p lzscr-cli -- -e '\x -> x'
# => <fun>
```

メモ:
- 裸シンボルやコンストラクタ変数は現段階では表示と未飽和適用の保持に留まります（型/IR 実装後に意味付けを拡張）。
- strict-effects は実装済みです。`--strict-effects` 有効時は、効果は `(~seq a b)` の第2引数（effect 文脈）でのみ許可されます。
	- 効果 API は `(~effects .sym)` から取得します。糖衣構文 `!sym` は `(~effects .sym)` へ展開されます（例: `!println`, `!print`）。

効果の例:

```
# 非 strict（デフォルト）: 効果はどこでも可
cargo run -p lzscr-cli -- -e '!println "hello"'

# strict-effects: seq の第2引数でのみ効果可
cargo run -p lzscr-cli -- -e '(~seq () (!println "hello"))' -s
```

静的解析（言語レベル AST）:
- 1行式の解析を行うには `--analyze` を付けます。重複検出の閾値は `--dup-min-size`（ノード数）と `--dup-min-count`（出現回数）。

```
cargo run -p lzscr-cli -- -e '(~add (~add 1 2) (~add 1 2))' --analyze --dup-min-size 3 --dup-min-count 2
```

開発者向け（Rust 側の静的解析）:
- フォーマットチェック: `cargo fmt --all -- --check`
- Lint（Clippy）: `cargo clippy --all-targets -- -D warnings`
- テスト: `cargo test`
GitHub Actions で fmt/clippy/test を自動実行します（`.github/workflows/ci.yml`）。
