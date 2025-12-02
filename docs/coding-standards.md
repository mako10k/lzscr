# lzscr Coding Standards and Refactoring Guidelines

Last updated: 2025-12-02

本文書はlzscrプロジェクトにおけるコード品質基準とリファクタリング方針を定めるものです。

## 1. ファイルサイズと複雑度の制限

### 1.1 行数制限

| ファイル種別 | 推奨上限 | 警告閾値 | 必須分割閾値 |
|------------|---------|---------|------------|
| Rustソースファイル (.rs) | 500行 | 1000行 | 2000行 |
| LazyScriptファイル (.lzscr) | 200行 | 300行 | 500行 |
| テストファイル (*_test.rs, tests/*.rs) | 800行 | 1200行 | 2000行 |
| ドキュメント (.md) | 800行 | 1500行 | 制限なし |

**現状 (2025-12-02):**
- 🔴 `lzscr-types/src/lib.rs`: 3438行 (必須分割)
- 🔴 `lzscr-runtime/src/lib.rs`: 2685行 (必須分割)
- 🔴 `lzscr-parser/src/lib.rs`: 2178行 (必須分割)
- 🔴 `lzscr-cli/src/main.rs`: 2060行 (必須分割)
- 🟡 `lzscr-analyzer/src/lib.rs`: 1150行 (警告)
- 🟡 `stdlib/pure/lex.lzscr`: 383行 (警告)

### 1.2 関数サイズ

- **推奨**: 50行以内
- **警告**: 100行
- **必須分割**: 200行

大きな関数は以下の方針で分割:
- ヘルパー関数の抽出
- ステートマシンのステート分離
- エラー処理ロジックの独立化

### 1.3 Cyclomatic Complexity (循環的複雑度)

Clippyの`cognitive_complexity`を使用:
- **推奨**: 10以下
- **警告**: 15
- **必須リファクタ**: 25

## 2. Clippy Lint設定

### 2.1 clippy.toml設定 (計画)

```toml
# clippy.toml
cognitive-complexity-threshold = 15
type-complexity-threshold = 250
too-many-arguments-threshold = 7
single-char-binding-names-threshold = 4
```

### 2.2 有効化するLintカテゴリ

**常時有効 (CI enforced):**
- `clippy::all`
- `clippy::correctness` (デフォルトでdeny)
- `clippy::suspicious`
- `clippy::complexity`
- `clippy::perf`

**段階的導入 (警告のみ):**
- `clippy::pedantic` (一部除外あり)
- `clippy::nursery` (実験的、任意)

**現在許可している例外:**
- `clippy::result_large_err`: TypeErrorは意図的にリッチなスパン情報を保持
- `clippy::dead_code`: 開発中の実験的API

### 2.3 許可すべきでないパターン

以下は原則として修正必須:
- `clippy::cognitive_complexity` 25以上
- `clippy::too_many_arguments` 8個以上
- `clippy::manual_*` (手動実装の自動化可能パターン)
- `clippy::unwrap_used` / `clippy::expect_used` (テスト以外)

## 3. モジュール分割戦略

### 3.1 Rust Crateの分割方針

**原則: 単一責任の原則 (SRP)**

大きな`lib.rs`は以下の観点で分割:

1. **機能ドメイン**: 型推論 / unification / 表示 / エラー処理
2. **データ構造**: 型定義 / AST / IR
3. **アルゴリズム**: パーサコンビネータ / evaluator / builtin関数

**推奨ディレクトリ構造 (例: lzscr-types):**

```
crates/lzscr-types/src/
├── lib.rs           # Public API と re-exports
├── types.rs         # Type enum, 基本型定義
├── inference.rs     # 型推論エントリポイント
├── unification.rs   # unify_*, zonk, apply_subs
├── type_display.rs  # pp_type, user_pretty_type
├── error.rs         # TypeError enum
├── hints.rs         # Fix-it hints生成
├── context.rs       # TypeContext, 環境管理
└── tests/           # 統合テスト
```

### 3.2 LazyScriptファイルの分割方針

**stdlib/pure/lex.lzscr (383行) の分割例:**

```
stdlib/pure/
├── lex.lzscr        # メインエクスポート (50行程度)
├── lex/
│   ├── token.lzscr      # Token型定義、emit関数
│   ├── scanner.lzscr    # Scanラッパー、基本操作
│   ├── char_class.lzscr # is_digit, is_alpha等
│   ├── number.lzscr     # take_number_token (radix対応)
│   ├── string.lzscr     # take_string, take_char_lit
│   ├── comment.lzscr    # skip_ws_and_comments分離
│   └── keywords.lzscr   # keyword matching
```

**分割基準:**
- 1ファイル200行以下を目標
- 関連する関数をまとめる (凝集度)
- 循環依存を避ける

## 4. コードクローン検出と排除

### 4.1 検出ツール

**現在未導入 (将来検討):**
- `cargo-clone` (Rust用クローン検出)
- 手動レビュー時の識別

### 4.2 よくあるクローンパターン

**lzscr-cliの3系統エラー表示:**
- `print_error_debug`, `print_error_legacy`, `print_error_pretty`
- 現状: 各100-200行で構造が類似
- 対策: 共通ヘルパー抽出、trait化検討

**lzscr-typesのunify_*系関数:**
- `unify`, `unify_with_spans`, `unify_ctor`, etc.
- 対策: 統一インターフェース設計 (ROADMAP B-5)

### 4.3 クローン許容基準

以下は許容:
- テストケースの類似構造 (Given-When-Then)
- 意図的な対称性 (left/right, debug/release)
- パフォーマンス最適化のための展開

## 5. ドキュメント要件

### 5.1 必須ドキュメント

**各crateに必要:**
- `README.md`: 目的、主要API、例
- Rustdoc: public関数・型に`///`コメント
- 複雑なアルゴリズムには設計ノート

**プロジェクトルート:**
- `CONTRIBUTING.md`: 本文書へのリンク
- `docs/architecture.md`: クレート間依存関係

### 5.2 コメントガイドライン

**Good:**
```rust
/// Unifies two types with dual-span error reporting.
/// 
/// # Errors
/// Returns `TypeError::Mismatch` when types are incompatible.
fn unify_with_spans(ty1: &Type, sp1: Span, ty2: &Type, sp2: Span) -> Result<()>
```

**Avoid:**
```rust
// unify types
fn unify(a: &Type, b: &Type) -> Result<()>
```

## 6. テストカバレッジ基準

### 6.1 目標カバレッジ

- **全体**: 70%以上 (現状: CI統合済み)
- **重要モジュール**: 80%以上
  - lzscr-types (型推論コア)
  - lzscr-parser (構文解析)
  - lzscr-runtime (評価器)

### 6.2 テスト戦略

**ユニットテスト:**
- 各関数の正常系・異常系
- エッジケース (空リスト、ネスト深度)

**統合テスト:**
- CLI golden tests (goldens/*.golden)
- stdlib smoke tests

**プロパティベーステスト:**
- 型推論の健全性 (将来導入検討)

## 7. リファクタリング優先順位

### Phase 1: 緊急対応 (2025-12-02 ~ 2025-12-15)

1. **lzscr-types/lib.rs分割** (3438行 → 500行×7ファイル)
2. **lzscr-runtime/lib.rs分割** (2685行 → 500行×5ファイル)
3. **clippy.toml導入** (complexity閾値設定)

### Phase 2: 中期改善 (2025-12-16 ~ 2026-01-15)

4. **lzscr-parser/lib.rs分割**
5. **lzscr-cli/main.rs分割**
6. **stdlib/pure/lex.lzscr分割**
7. **CI lint強化** (complexity/size警告)

### Phase 3: 長期保守 (2026-01-16 ~)

8. コードクローン検出自動化
9. 各crateのREADME充実
10. Architecture Decision Records (ADR) 導入

## 8. CI/CD統合

### 8.1 既存チェック (継続)

- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo llvm-cov` (カバレッジ)

### 8.2 追加予定チェック

- **Line count警告**: 1000行超えファイルを検出
- **Complexity警告**: cognitive_complexity 15超えを報告
- **Dead code検出**: 未使用関数の洗い出し (cargo-udeps)

### 8.3 チェック失敗基準

**CI失敗 (PR merge阻止):**
- Clippy warnings (current)
- Test failures
- カバレッジ5%以上低下

**CI警告 (merge可だがレビュー必須):**
- ファイル行数閾値超過
- Complexity高 (20以上)

## 9. 移行計画

### 9.1 既存コードへの適用

**新規コード:**
- 本基準を即座に適用

**既存コード:**
- 段階的リファクタリング (Phase 1-3)
- 修正時に周辺コードも改善 (Boy Scout Rule)

### 9.2 例外申請プロセス

基準を満たせない正当な理由がある場合:
1. 該当コードに`// REASON: <説明>`コメント
2. `#[allow(clippy::...)]`で抑制 (最小範囲)
3. docs/exceptions.mdに記録

## 10. レビューチェックリスト

PRレビュー時の確認項目:

- [ ] 新規ファイルは500行以内か?
- [ ] 関数は100行以内か?
- [ ] Clippy警告は適切に対処されたか?
- [ ] 複雑な処理にコメントがあるか?
- [ ] テストカバレッジが低下していないか?
- [ ] 重複コードは抽出されたか?

## 参考文献

- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- [Clippy Lint List](https://rust-lang.github.io/rust-clippy/master/)
- [Google Engineering Practices](https://google.github.io/eng-practices/)

---

本文書は継続的に更新されます。変更提案はissue/PRで受け付けます。
