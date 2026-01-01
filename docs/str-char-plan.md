# Str/Char 設計変更計画（WIP）

この文書は、lzscr の `Str` / `Char` 周りを「実装と仕様を揃えつつ段階的に」再設計するための計画メモです。
現時点の一次情報（現在実装）は `docs/spec/*` と実装（`crates/*`）です。

## 現状（2026-01 時点）

- `Char` は **Unicode code point を `i32`** で表すリテラル/値として実装済み
  - lexer: `crates/lzscr-lexer` の `Tok::Char(i32)`
  - AST: `ExprKind::Char(i32)` / `PatternKind::Char(i32)`
  - runtime: `Value::Char(i32)`
- `Str` は runtime 側で **共有 UTF-8 バッファ + (start,len) スライス**（`RtStr`）
  - `Env::intern_string` により intern され、同一文字列の共有が起きる
  - `slice_chars` 等により部分文字列を（バイトスライスとして）保持可能
- エスケープ解釈（lexer）は現在以下
  - `\\n \\r \\t \\0 \\\\ \\\" \\\' \\u{HEX+}`
  - それ以外の `\\X` は **エラー**

## ゴール（要望）

- Str/Char のエスケープ仕様を統一する
- Unicode codepoint を一般的に扱えるリテラル体系を整える
- Str セマンティクスを再検討する
  - 分割不可（= 文字列の合成/部分文字列を効率的に扱いたい）
  - Char リストとの相互変換（遅延評価の余地）
  - Symbol 相互変換
  - intern と GC（参照が無ければ回収）
  - rope 等のデータ構造の検討

## フェーズ分け（提案）

### Phase 0: 仕様と実装の整合（完了）

- `docs/spec/semantics.md` の Char 記述を現状実装に合わせる
- `docs/spec/syntax.md` のリテラル/エスケープ記述を現状実装に合わせる
- `docs/spec/ast.md` を現行 AST に更新する

### Phase 1: エスケープ仕様の明文化と統一（実装前提の最小変更）

- 目標: `Str` と `Char` のエスケープ解釈を **完全に同一の表** で扱えるようにする
- 方針案:
  - 現行互換を優先する（既存コードが壊れない）
  - `\u{HEX+}` の範囲チェック（0..=0x10FFFF、サロゲート除外など）を明確化
  - 未知のエスケープはエラー（決定済み）

### Phase 2: codepoint リテラル体系の拡張

- 目標: 実装/仕様として「何が書けて、どう解釈されるか」を揃える
- 例（案）:
  - `\u{...}` は維持
  - `\xHH` のような形式を導入するかは、互換/利便性/仕様の簡潔さで判断

### Phase 3: Str セマンティクスの再設計（大きな変更）

- 目標: Str を「不可分・効率的な合成/部分参照」を中心に再定義する
- 検討項目（案）:
  - `Str` を rope / concat tree / slice などの表現に寄せる
  - `to_chars : Str -> List Char` / `from_chars : List Char -> Str` のコストモデル
  - `Symbol <-> Str` 変換と intern の境界（同一化の範囲、寿命）
  - `intern` と GC をどう実現するか（現状は Env 内 HashMap + Arc 共有）

## 次の一手（実装開始の入口）

- Phase 1 の仕様決め（特に「未知のエスケープの扱い」と codepoint の妥当性チェック）を先に確定する
- その後、lexer の `parse_string` / `parse_char` を共有関数化して、統一表で実装する
