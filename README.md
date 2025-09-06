# lzscr (work-in-progress)

最小サブセットを持つ LazyScript 再実装の骨組みです。
- Int/Float/Bool/Str、`~ref`、裸シンボル（Ctor）、ラムダ、関数適用、ブロック。
- シュガー: タプル `(a,b,...)`、レコード `{k:v,...}`、`true()`/`false()`。
- 中値演算子: `+ - * /`（Int）、`.+ .- .* ./`（Float）、比較 `< <= > >=` と `== !=`（Float 比較は `. < .<= .> .>=`）。
- 簡易ランタイム評価器とビルトイン（抜粋）: `to_str`, `add/sub/mul/div`, `fadd/fsub/fmul/fdiv`, `lt/le/gt/ge`, `flt/fle/fgt/fge`, `eq/ne`, `and/or/not`, `if`, `seq`, `effects.print/println`, `Tuple/Record/KV`, `Bool`。

試す:

```
cargo run -p lzscr-cli -- -e '1 + 2 * 3'
# => 7

cargo run -p lzscr-cli -- -e '1 == 1'
# => true

cargo run -p lzscr-cli -- -e '1 .+ 2.5'
# => 3.5

cargo run -p lzscr-cli -- -e '(~println "hi")'
# 標準出力: hi\n, 最終値: () を表示

cargo run -p lzscr-cli -- -e '\x -> x'
# => <fun>

cargo run -p lzscr-cli -- -e '(1, 2, 3)'
# => (1, 2, 3)

cargo run -p lzscr-cli -- -e '{a: 1, b: 2}'
# => {"a": 1, "b": 2} の順序相当（表示は環境依存）

cargo run -p lzscr-cli -- -e 'if true() 10 20'
# => 10

## ファイルからの実行 (--file)

1ファイルの式/バインディング群を評価できます。CLI はファイル内容を括弧で囲み、先頭や末尾の `~x = ...;` 形式のバインディングを let グループとして扱います。

```
echo '~x = 1; ~add ~x 2;' > /tmp/a.lzs
cargo run -p lzscr-cli -- --file /tmp/a.lzs
# => 3
```

## 型チェック (Hindley–Milner 推論)

実行前に簡易型推論を行います。失敗時はエラー終了します。出力は pretty か json を選べます。

```
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --types pretty
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --types json

# 型チェックをスキップ
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --no-typecheck
```
```

メモ:
- 裸シンボル（`.Foo` など）は Ctor（名前付きコンテナ）。`--ctor-arity` でアリティ宣言すると、過剰適用や 0 引数誤用を実行時で検出します。
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

- Ctor アリティの静的検査（`--ctor-arity` を併用）

```
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --ctor-arity 'Foo=1'
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --format json --ctor-arity 'Foo=1'
```

- 実行時のアリティ検査（評価時に過剰適用などをエラーにします）

```
cargo run -p lzscr-cli -- -e '.Bar 1 2' --ctor-arity 'Bar=1'
```

Core IR ダンプ:

```
cargo run -p lzscr-cli -- -e '1 + 2' --dump-coreir
cargo run -p lzscr-cli -- -e '1 + 2' --dump-coreir-json
```

開発者向け（Rust 側の静的解析）:
- フォーマットチェック: `cargo fmt --all -- --check`
- Lint（Clippy）: `cargo clippy --all-targets -- -D warnings`
- テスト: `cargo test`
GitHub Actions で fmt/clippy/test を自動実行します（`.github/workflows/ci.yml`）。

## コードフォーマット

- 本リポジトリは `rustfmt.toml` により Rust のフォーマットを統一しています。
- VS Code: 保存時に自動整形されます（`.vscode/settings.json`）。
- 手動実行:

```bash
cargo fmt --all
```
