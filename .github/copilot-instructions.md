# Copilot Instructions (lzscr)

このファイルは lzscr リポジトリでAIコーディングエージェントが即作業できるようにするための、実態ベースの短いガイドです。

## 全体像（クレート境界）
- Rust workspace（[Cargo.toml](../Cargo.toml)）配下に複数クレート。入口は主に `lzscr-cli`。
- 大枠の流れ: `lzscr-lexer` → `lzscr-parser`（AST構築）→ `lzscr-analyzer`（静的解析）→ `lzscr-types`（HM型推論）→ `lzscr-coreir`（IR）→ `lzscr-runtime`（評価/組み込み/effects）。
- フォーマッタ: `lzscr-format`（ASTベース）。VS Code拡張は `extensions/lzscr-vscode`。

## 仕様・ドキュメントの優先順位
- 仕様の一次情報: `docs/spec/*`（統合は `docs/spec/language.md`）。
- READMEとROADMAPが矛盾する場合は `docs/ROADMAP.md` を優先。
- 将来の計画/優先度は `docs/ROADMAP.md` を参照。

## よく使う開発コマンド
- CLI評価: `cargo run -p lzscr-cli -- -e '1 + 2 * 3'`
- ファイル実行: `cargo run -p lzscr-cli -- --file path/to/code.lzscr`
- 解析/表示: `--analyze`, `--types pretty`, `--dump-coreir`
- フォーマット: `--format-code`（幅/インデントは `--fmt-width`, `--fmt-indent`）

## テスト/品質ゲート（CIも同様）
- 最小セット: `cargo fmt --all --check` / `cargo clippy --all-targets -- -D warnings` / `cargo test` / `cargo build --workspace --all-features`
- ゴールデン（トークナイザ）: `cargo test -p lzscr-cli --tests tokenize_golden`（`goldens/*.golden`）
- CI概観は `.github/workflows/`（coverageは tarpaulin + cobertura.xml）。

## このリポジトリ固有の実装パターン
- Span（診断）を落とさない: 特にレコードのフィールド名 `name_span` は AST→Parser→Types→エラー表示まで伝播する設計が多い。
- 反復の書き方: フィールド群は `(k, v)` ではなく「フィールド構造体を回して `f.name` / `f.name_span`」の形に寄せる。
- コメント/ドキュメントは可能な限り日本語。

## VS Code 拡張（フォーマット連携）
- `extensions/lzscr-vscode` は `lzscr-cli --format-code` を呼ぶ。開発/パッケージ: `npm install` → `npm run watch` / `npm run package`。

