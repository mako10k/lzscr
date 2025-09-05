# lzscr CLI

Rust製の PoC CLI。式の評価・静的解析・Core IR ダンプを提供。

- バイナリ名: `lzscr-cli`

## 使い方

- 評価
  - `lzscr-cli -e "(~add 1 2)"`
  - 厳格効果モード: `-s` または `--strict-effects`
- 静的解析
  - `lzscr-cli -e "..." --analyze [--format json] [--dup-min-size N] [--dup-min-count M]`
- Core IR ダンプ
  - テキスト: `lzscr-cli -e "..." --dump-coreir`
  - JSON: `lzscr-cli -e "..." --dump-coreir-json`

優先順位: `--dump-coreir(-json)` > `--analyze` > 実行。

## 例

- `(~seq () (!println "x"))` を IR テキストで出力:
  - `lzscr-cli -e "(~seq () (!println \"x\"))" --dump-coreir`
- 解析を JSON で:
  - `lzscr-cli -e "(\\x -> x) 1" --analyze --format json`
