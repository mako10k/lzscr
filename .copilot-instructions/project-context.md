# Project Context

## Project: lzscr - Functional Programming Language

**Language**: Rust  
**Type**: Programming language implementation (compiler/interpreter)  
**Architecture**: Multi-crate workspace

## Crate Structure

### Core AST & Parsing
- **lzscr-ast**: Abstract syntax tree definitions
- **lzscr-lexer**: Tokenization
- **lzscr-parser**: Parser (AST construction)
- **lzscr-preast**: Pre-AST intermediate representation

### Type System & Analysis
- **lzscr-types**: Type inference and checking
- **lzscr-analyzer**: Static analysis (unused vars, shadowing, duplicates)

### Compilation & Runtime
- **lzscr-coreir**: Core intermediate representation
- **lzscr-runtime**: Expression evaluation
- **lzscr-format**: Code formatting

### User Interface
- **lzscr-cli**: Command-line interface

## Current Focus: Phase 5 - Record Field Spans

**Branch**: `feature/diagnostics-phase5-record-field-spans` (if created) or `main`

**Goal**: Improve error diagnostics by tracking precise spans for record field names

**Progress**: Step 7 of 9 (66% when complete)

## Recent Commits

- `526024f`: Phase 5 Step 1-6 - ExprKind::Record field spans (completed)

## Key Files for Current Work

- `crates/lzscr-ast/src/lib.rs` - AST definitions
- `crates/lzscr-parser/src/lib.rs` - Parsing logic
- `crates/lzscr-analyzer/src/lib.rs` - Static analysis
- `crates/lzscr-types/src/inference/*.rs` - Type inference
- `crates/lzscr-cli/src/main.rs` - CLI implementation

## Testing

- **Test Suite**: 113 tests across all crates
- **Command**: `cargo test`
- **Requirement**: All tests must pass before commit

## Workflow

1. Make AST changes in `lzscr-ast`
2. Update parser in `lzscr-parser`
3. Fix compilation errors in dependent crates
4. Run `cargo test` to validate
5. Commit with descriptive message

## Team Preferences

- **Language**: Japanese for comments and documentation
- **Commit Style**: Conventional commits with detailed body
- **Testing**: Test-driven with 100% pass requirement

---

## 🚀 Quick Reference (High Priority)

### Language Syntax Cheat Sheet

#### Core Constructs
```lzscr
# Variables & References
~varname                    # Reference (statically bound)

# Functions
\~x -> ~x + 1               # Lambda function
\~x ~y -> ~x * ~y           # Multi-argument lambda

# Blocks
{ expr }                    # Block expression

# Records
{ age : 25, name : "Alice" }      # Record literal
~person .age                       # Field access

# Lists
[1, 2, 3]                   # List literal
~head : ~tail               # Cons operator (right-associative)
[]                          # Empty list

# Tuples
(1, "hello", True)       # Tuple

# Constructors
.Some(value)                # Constructor with value
.None()                     # Zero-arity constructor
.Ok(42)                     # Result type constructor
.Err("error")               # Error constructor

# Pattern Matching (in lambdas)
\(Some ~x) -> ~x           # Pattern in lambda
\[] -> 0                    # Empty list pattern
| \(~h : ~t) -> ~h          # Alternative pattern (cons)

# Type Annotations
%{ Int }                    # Type value
%{ { age : Int } } { age : 25 }   # Annotated record
%{ %a -> [%a] }             # Polymorphic type
%{ ?x -> [?x] }             # Type with hole

# Effects
!println("hello")           # Effect sugar: (~effects .println)
!{ !println("a"); !println("b"); }  # Do-notation (chain/bind)

# Exceptions
^(error_value)              # Raise exception
~expr ^| \~err -> handle    # Catch exception
```

#### Common Patterns
```lzscr
# Conditional (pattern matching on Bool)
(\~cond -> (\True -> then_val | \False -> else_val) ~cond)

# Let-like binding (via lambda)
(\~x -> body_using_x) value

# Sequence effects
~seq () (!println "first")
~seq (!println "first") (!println "second")

# Chain effects
~chain (!read_input) (\~input -> !println ~input)
```

### Toolchain Quick Start

#### Build & Run
```bash
# Build all crates
cargo build

# Build CLI only (faster)
cargo build --bin lzscr-cli

# Run tests
cargo test                  # All tests
cargo test --package lzscr-types  # Specific crate

# Format check
cargo fmt --all -- --check

# Linting
cargo clippy --all-targets -- -D warnings
```

#### CLI Usage
```bash
# Evaluate expression
cargo run --bin lzscr-cli -- -e '{ age : 25 }'

# Run file
cargo run --bin lzscr-cli -- -f script.lzscr

# With standard library
cargo run --bin lzscr-cli -- -f script.lzscr --stdlib-dir ./stdlib

# Type checking only
cargo run --bin lzscr-cli -- -e 'expr' --types pretty

# Format code
cargo run --bin lzscr-cli -- -f input.lzscr --format-code

# Dump Core IR
cargo run --bin lzscr-cli -- -f input.lzscr --dump-coreir
```

#### Development Workflow
```bash
# 1. Create feature branch
git checkout -b feature/my-feature

# 2. Make changes
# Edit files in crates/lzscr-*/

# 3. Test continuously
cargo test

# 4. Format & lint
cargo fmt --all
cargo clippy --all-targets -- -D warnings

# 5. Commit (conventional style)
git commit -m "feat(crate): description

- Detail 1
- Detail 2
- Tests passing"

# 6. Push and create PR
git push origin feature/my-feature
```

#### Common Development Tasks
```bash
# Add dependency to specific crate
cd crates/lzscr-cli
cargo add serde --features derive

# Check compilation without running tests
cargo check --all-targets

# Run specific test
cargo test --package lzscr-types test_record_inference

# Watch mode (with cargo-watch)
cargo watch -x test

# Coverage (with cargo-tarpaulin)
cargo tarpaulin --out Html
```

#### Debugging Tips
```bash
# Type inference debug
cargo run --bin lzscr-cli -- -e 'expr' --type-debug 2

# Verbose error output
RUST_BACKTRACE=1 cargo run --bin lzscr-cli -- -e 'expr'

# Check specific file for parsing
cargo run --bin lzscr-cli -- -f test.lzscr --analyze
```


---

## 📋 Project Rules and Standards

### Git Workflow (詳細はメモリに保存済み)

**ブランチ戦略**:
- `main`: プロダクション対応コード (保護ブランチ)
- `feature/<name>`: 機能開発ブランチ
- `hotfix/<issue>`: 緊急修正ブランチ

**PRプロセス** (必須: `gh` CLI使用):
```bash
# PR作成前チェック
cargo test
cargo fmt --all -- --check
cargo clippy --all-targets -- -D warnings

# PR作成 (推奨方法)
gh pr create \
  --title "feat(scope): description" \
  --body-file /tmp/pr_description.md \
  --base main

# CI確認
gh pr checks

# マージ (承認後)
gh pr merge --squash --delete-branch
```

**コミット規約** (Conventional Commits):
- `feat(scope)`: 新機能
- `fix(scope)`: バグ修正
- `docs(scope)`: ドキュメント
- `style(scope)`: フォーマット
- `refactor(scope)`: リファクタリング
- `test(scope)`: テスト追加
- `chore(scope)`: メンテナンス

### 品質ゲート (必須)

**マージ前の必須チェック**:
1. ✅ 全テスト合格 (113+)
2. ✅ `cargo fmt --all -- --check`
3. ✅ `cargo clippy --all-targets -- -D warnings`
4. ✅ `cargo build --workspace --all-features`
5. ✅ VS Code extension build (該当時)

**オプションチェック** (continue-on-error):
- Security audit (cargo-audit)
- License check (cargo-deny)
- Coverage (tarpaulin)
- Unused deps (cargo-udeps)

### 開発標準

**コードスタイル**:
- Rust標準スタイル (rustfmt)
- 最大行長: 100文字
- 説明的な変数名 (単一文字は避ける)

**テスト要件**:
- 新機能には必ずテストを追加
- テストカバレッジ: 現状維持または改善
- 成功ケースとエラーケースの両方をテスト

**ドキュメント**:
- 全publicAPIにdocコメント
- 例を含む
- パニック、エラー、安全性を文書化

### CI/CD

**必須チェック** (must pass):
- build: Rust build & test
- vsix: VS Code extension packaging

**CIトリガー**:
- mainへのpush
- mainへのPR
- 手動実行
- 週次スケジュール (月曜3:00 UTC)

**CI監視コマンド**:
```bash
gh pr checks          # PRのCI状態
gh run watch          # リアルタイム監視
gh run list           # 実行履歴
gh run rerun          # 失敗したチェックの再実行
```

### リリースプロセス

**バージョニング**: SemVer (MAJOR.MINOR.PATCH)

**リリース手順**:
```bash
# リリース作成
gh release create v0.1.0 \
  --title "v0.1.0: Phase 5 Complete" \
  --notes-file CHANGELOG.md

# 成果物アップロード
gh release upload v0.1.0 target/release/lzscr-cli
```

### 緊急対応

**ホットフィックス**:
1. mainからhotfixブランチ作成
2. 修正とテスト
3. `gh pr create --title "hotfix: description"`
4. Fast-track review
5. `gh pr merge --squash --delete-branch`

**ロールバック**:
1. 問題のあるコミットを特定
2. `git revert <commit-hash>`
3. `gh pr create --title "revert: description"`
4. マージ後にホットフィックス

### GitHub CLI セットアップ

**インストール**:
```bash
# macOS
brew install gh

# Linux (Debian/Ubuntu)
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | \
  sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | \
  sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
sudo apt update
sudo apt install gh

# Windows
winget install GitHub.cli
```

**認証**:
```bash
gh auth login
gh auth status
```

**設定**:
```bash
gh config set editor vim
gh config set git_protocol ssh
```

---

## 📚 詳細ドキュメント

上記ルールの詳細版は以下に保存されています (MCPメモリシステム):

1. **Git Workflow Rules**: Git戦略、PR管理、CI/CD、マージ戦略の詳細
2. **Development Standards**: コーディング規約、テスト標準、エラーハンドリング、パフォーマンス考慮事項
3. **Quality Gates**: 品質チェック、リリース基準、インシデント対応、品質モニタリング

これらはメモリシステムに保存され、全ての開発セッションで参照可能です。
