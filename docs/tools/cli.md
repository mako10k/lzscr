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

## モジュール解決（~require）

- `(~require .seg1 .seg2 ... .segN)` は実行前に解決され、`seg1/seg2/.../segN.lzscr` を読み込んで式に展開されます。
- 引数はすべて `.name` 形式の Ctor 記号のみ許可。ファイル未発見、パース/型検査失敗、循環参照は静的エラー。
- 検索パスの優先順:
  1. カレントディレクトリ
  2. `--stdlib-dir <PATH>`
  3. `--module-path <P1:..:PN>`（コロン区切り）

### 関連オプション
- `--stdlib-dir <PATH>`: 標準ライブラリ探索ディレクトリ。
- `--module-path <P1:..:PN>`: 追加の探索パス。
