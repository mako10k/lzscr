````markdown
# AGENT

## プロジェクト概要
- **リポジトリ**: `lzscr` (Rust 製の LazyScript 実装), マルチクレート構成。
- **主なクレート**: `lzscr-ast`, `lzscr-lexer`, `lzscr-parser`, `lzscr-preast`, `lzscr-types`, `lzscr-analyzer`, `lzscr-coreir`, `lzscr-runtime`, `lzscr-format`, `lzscr-cli`.
- **利用言語**: Rust。コメントやドキュメントは可能な限り日本語で記述。
- **公式ドキュメント**: プロジェクトのロードマップは `docs/ROADMAP.md` を参照し、齟齬がある場合は ROADMAP を優先。

## 現在のフェーズ
- **Phase 5: Record Field Spans** を進行中 (目標: レコードフィールド名の Span 追跡によるエラーダイアグノスティクス改善)。
- 最新達成状況: Expr/Pattern/TypeExpr 各レコードフィールドで `name_span` を保持し、型エラー表示まで反映済み。
- **直近タスク**
  - **Step 7 – PatternRecordField**: `PatternKind::Record` を `Vec<PatternRecordField>` に変更。AST/Parser/Analyzer/CLI/Types で `name_span` を伝播。
  - **Step 9 – Error Display Improvements**: 型エラー表示 (`crates/lzscr-types/src/error.rs` 他) でフィールド名の Span を優先して使用し、フィールド毎に精緻なメッセージを出す。欠落フィールド／型不一致／余剰フィールドのケースをテストで保証。

## コードパターンと実装ガイド
- **Record Field Structs**: `ExprRecordField`, `PatternRecordField`, `TypeExprRecordField` は `name`, `name_span`, 値本体（`value`/`pattern`/`ty`）を持つ構造体を導入し、`#[derive(Debug, Clone, PartialEq)]` を付ける (Pattern 用のみ `PartialEq` 必須)。
- **Parser での Span 捕捉**: フィールド名トークンを消費する前に `key_span = ktok.span;` を取得し、`RecordField::new(key, key_span, value)` へ渡す。
- **イテレーション**: 旧来の `for (k, v) in fields` パターンは `for f in fields { f.name ... }` へ書き換える。型推論やアナライザーでは `f.name_span` をダイアグノスティクスへ渡す。
- **型エラー表示**: `Type::Record` の `BTreeMap<String, (Type, Option<Span>)>` に保持した `field_span` を最優先で使用し、`Field '<name>': ...` の形式で注記を追加する。Span が無い場合は従来の値 Span → レコード全体 Span の順でフォールバック。

## ワークフロー
1. `feature/<name>` ブランチで作業 (緊急時は `hotfix/<issue>`)。`main` は保護ブランチ。
2. 実装順序の推奨: AST → Parser → 依存クレート (types/analyzer/runtime/cli など) → ビルド/テスト。
3. 変更ごとに `cargo build` でコンパイルエラーを潰し、完了時に `cargo test` を全実行。
4. コミットは Conventional Commits。例: `feat(diagnostics): Phase 5 Step 7 - add PatternRecordField`。
5. PR 作成前チェック: `cargo fmt --all -- --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`, `cargo build --workspace --all-features`。VS Code Extension を触った場合は `extensions/lzscr-vscode` のビルドも通す。

## テストと品質ゲート
- **必須テスト**: リポジトリ全体で 113+ テストが存在。新機能やリグレッション修正ではテストを追加し、成功ケース/失敗ケースの両方をカバー。
- **必須コマンド**
  - `cargo fmt --all`
  - `cargo clippy --all-targets -- -D warnings`
  - `cargo test`
  - `cargo build --workspace --all-features`
- **任意チェック**: `cargo audit`, `cargo deny`, `cargo tarpaulin`, `cargo udeps` などは余裕があれば実行。

## CLI / 開発補助コマンド
- 評価: `cargo run -p lzscr-cli -- -e '...'`
- ファイル実行: `cargo run -p lzscr-cli -- --file path/to/file.lzscr`
- ゴールデンテスト (トークナイザ): `cargo test -p lzscr-cli --tests tokenize_golden`
- 型ダンプ/解析: `cargo run -p lzscr-cli -- -e 'expr' --types pretty`, `--dump-coreir`, `--analyze` などを活用。

## エラーダイアグノスティクス改善の要点
- フィールド名 Span の伝播が最重要。Span を失わないために AST → Parser → Type 構造体 → エラー表示まで通貫して `Option<Span>` を保持する。
- Diagnostics では `field_span` > `value_span` > `record_span` の順に優先。適切なメッセージ (例: `Field 'age': Expected Int, found Str`) を出力してゴールデンやテストで検証。
- テストケース例: 必須フィールド欠落、型不一致、余剰フィールド、フィールド名 Typo。必要に応じて `goldens/*.golden` や `crates/lzscr-types/tests` に追加。

## 参考スニペット
```rust
// PatternRecordField の例
#[derive(Debug, Clone, PartialEq)]
pub struct PatternRecordField {
    pub name: String,
    pub name_span: Span,
    pub pattern: Pattern,
}

impl PatternRecordField {
    pub fn new(name: String, name_span: Span, pattern: Pattern) -> Self {
        Self { name, name_span, pattern }
    }
}

// パターンの再ベース処理 (lzscr-cli)
PatternKind::Record(fields) => {
    let mut new_fields = Vec::with_capacity(fields.len());
    for f in fields {
        new_fields.push(PatternRecordField::new(
            f.name.clone(),
            f.name_span,
            map_pattern(&f.pattern),
        ));
    }
    PatternKind::Record(new_fields)
}
```

## ドキュメントとコミュニケーション
- 公開 API の doc コメントには説明・引数・エラーケース・例を含める。
- TODO や lint 抑制には必ず理由を書く (`#[allow(...)] // reason`)。
- Issue/PR/コミット本文は日本語で詳細を記述。PR 作成時は `gh` CLI を推奨 (`gh pr create ...`→`gh pr checks`→`gh pr merge` 流れ)。

## リリース／インシデント対応メモ
- **リリース**: SemVer。`gh release create vX.Y.Z --notes-file CHANGELOG.md` 後に成果物 (`target/release/lzscr-cli` や VSIX) を添付。
- **ホットフィックス**: `hotfix/<issue>` ブランチで修正し、最優先レビュー→`gh pr merge --squash --delete-branch`。
- **ロールバック**: `git revert <commit>` + 専用 PR。必要に応じてホットフィックス手順へ。

このドキュメントを Copilot/Codium/LLM エージェントのシステムプロンプトとして与えることで、リポジトリの文脈と現行タスク、品質基準に沿った提案やコード生成を行える。

````
