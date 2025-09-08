## ロードマップ（実装タスク）
 - [x] stdlib ディレクトリ作成: `stdlib/prelude.lzscr`（雛形作成・Builtins 委譲を開始）
 - [ ] Rust ランタイムに最小ビルトインを追加（str_len/str_concat/str_slice/str_cmp/str_codepoint_at/str_from_codepoint、cp_* 判定、算術/比較の公開整理）
 - [ ] M0 API 実装: list/string/unicode(codepoint)/option/result の最小セット（lzscr 実装）
 - [ ] M0 検証: lzscr で簡易トークナイザ（識別子/整数/記号/空白）を実装して CLI から実行
 - [ ] M1 API 実装: string の `join/split/find/starts_with/ends_with` と list の拡張、math 最小セット
使用例:
```bash
 cargo run -p lzscr-cli -- -e '~add 1 2'                 # stdlib 有効（デフォルト）
 cargo run -p lzscr-cli -- -e '~add 1 2' --no-stdlib     # stdlib を無効化
 cargo run -p lzscr-cli -- --file prog.lzscr --stdlib-dir ./stdlib
```

補足: prelude は `stdlib/prelude.lzscr` に配置済み。CLI はデフォルトでこれを先頭に読み込み、ユーザコードを後置した let グループとして結合します（--no-stdlib で無効化可能）。
# lzscr 標準ライブラリ計画（ドラフト）

この文書は、lzscr の標準ライブラリ（以下、stdlib）を段階的に整備するための計画と設計方針をまとめたものです。初期は CLI による事前読込（プリロード）で提供し、将来的にモジュール/インポート構文を導入して本格運用に移行します。

## 目的と非目標
- 目的
  - ふだんのプログラミングで必要な基本関数群を提供（コレクション操作、Option/Result、文字列、数値、レコード操作、IO ラッパ等）。
  - 既存のビルトイン（ランタイム）を安全・一貫した API として露出。
  - 言語の HM 型推論と相性のよい API 形状を採用（関数合成・高階関数を活用）。
  - 中期目標: lzscr のパーサ/実行環境を lzscr 自身で書ける自己ホスト（Self-host）を視野に、必要最小限のプリミティブ/stdlib を整備。
- 非目標（初期段階）
  - 並行/非同期、ファイルシステム等の重い IO（将来の別提案で拡張）。
  - パッケージマネージャや外部依存の解決機構（将来検討）。

### 自己ホスト（Self-host）方針と最小要件
自己ホストに向けた最小要件を以下にまとめます。これらは Rust ランタイムの安定したビルトインとして提供し、その上に lzscr 実装で積み上げます。

- 必須プリミティブ（Rust 側のビルトインとして露出）
  - 算術/比較: `(+,-,*,/,%,==,!=,<,<=,>,>=)`（Int/Float 対応）
  - 等価/ハッシュ（当面は基本型に限定）
  - 文字列: `str_len : Str -> Int`, `str_concat : Str -> Str -> Str`,
    `str_slice : Str -> Int -> Int -> Str`（半開区間、境界はクランプ）, `str_cmp : Str -> Str -> Int`
  - 文字列/コードポイント（最初は UTF-8 の手堅いサブセットで可）:
    `str_codepoint_at : Str -> Int -> Option Int`（コードポイント Int を返す）, `str_from_codepoint : Int -> Str`（1 コードポイントの文字列）
  - 文字クラス判定（コードポイント Int に対して）: `cp_is_alpha`, `cp_is_digit`, `cp_is_alnum`, `cp_is_space`, `cp_is_newline`, `cp_is_lower`, `cp_is_upper`
  - デバッグ/表示: `to_str : a -> Str`（既存）, `println : Str -> Unit`（M3 にも関連）
- stdlib（lzscr 実装）で提供する基礎
  - list: `length/map/filter/foldl/foldr/append/reverse/take/drop/range`
  - string: 上記ビルトインを包む安全 API と追加関数 `join/split`（簡易）, `starts_with/ends_with`, `find`
  - unicode/codepoint: コードポイント Int に対する文字クラス API の薄いラップと補助 `cp_is_lower/cp_is_upper`
  - option/result: 既存案のとおり
- パーサ/字句解析に必要な土台
  - `Span = { start: Int, end: Int }` レコードとユーティリティ
  - `Token` バリアント: `.Ident | .IntLit | .StrLit | .Symbol | ...`（段階的に）
  - `StringCursor` 相当の操作を `str_len/str_char_at/str_slice` で代替（最初は O(n) でも可）。

## フェーズ計画（マイルストーン）
0. M0: 自己ホスト準備（最小ビルトインと stdlib の土台）
   - Rust ランタイムに最小ビルトインを追加・公開（上記「必須プリミティブ」）。
   - `stdlib/` に `prelude.lzscr` を用意し、list/string/char/option/result の最小関数を lzscr で実装。
   - 文字列/文字 API を用いた簡易トークナイザの PoC を lzscr で作成（記号/識別子/数値/空白の区別）。
   - CLI から PoC を呼び出す統合テストを追加。
1. M1: ブートストラップ stdlib（プリロード方式）
   - 配布形態: リポジトリ内の `stdlib/` に .lzscr ソースとして配置。
   - CLI でデフォルト有効（`--no-stdlib` で無効化、`--stdlib-dir` で差し替え）。
   - 提供モジュール（単一ファイルでも可）:
     - prelude: よく使う関数を再エクスポート
     - option, result, list, string, math（最小限）
   - テスト: CLI 経由でのゴールデン/性質テスト。
2. M2: レコード/タプル/マップ操作ユーティリティ
   - record: get/set/update/merge、keys/values、レンズ風補助（軽量）。
   - tuple: fst/snd/…、zip/unzip ユーティリティ。
3. M3: IO/effects ラッパ
   - strict-effects と両立する安全 API（effect 文脈へ誘導）。
   - println/print、debug 系（to_str 経由）。
4. M4: フォーマッタ・型と整合した API 整理
   - 命名規約/名前空間の見直し、`prelude` の露出最適化（衝突回避）。
5. M5: モジュール/インポート構文の導入
   - 言語に `import`/`from ... import ...` などの簡易構文を追加。
   - モジュール解決: `stdlib/` とユーザープロジェクトを同一ルールで探索。
6. M6: バージョニング/互換性指針
   - stdlib セマンティックバージョニング（言語バージョンと連動）。
   - 破壊的変更はメジャーアップ時のみ。`prelude` は特に保守的に。

## 構成と API 概要（初期案）

- prelude（自動オープン想定／プリロード時は全体が可視）
  - 関数合成 `(~compose f g x) = f (g x)`、恒等 `(~id x) = x`
  - 比較/ブール: `and/or/not` の薄いラップ（短絡挙動は保持）
  - Option/Result の代表関数の再エクスポート
  - Builtins 委譲方針: ランタイムのビルトインは `~Builtins` レコードに名前空間で集約し、ユーザコードは原則 `prelude` 経由で参照（例: `~Str = ~Builtins .string;`）
- option
  - 構築子: `.Some x`, `.None`
  - `is_some`, `is_none`, `map`, `and_then`, `unwrap_or`
- result
  - 構築子: `.Ok x`, `.Err e`
  - `map`, `map_err`, `and_then`, `unwrap_or`
- list
  - `nil`（`[]` 相当）と `cons`（`(::)` 相当）；構築は糖衣 `[a,b,c]`
  - `length`, `map`, `filter`, `foldl`, `foldr`, `append`, `reverse`, `take`, `drop`, `range`
- string
  - `len`, `concat`, `slice`, `cmp`, `join`, `split`（簡易）, `starts_with`, `ends_with`, `find`, `to_int`, `from_int`
- char
  - `of_int`, `to_int`, `is_alpha`, `is_digit`, `is_alnum`, `is_space`, `is_lower`, `is_upper`, `is_newline`
- math（数値）
  - `abs`, `min`, `max`, `clamp`, `floor`, `ceil`, `pow`（必要に応じ Int/Float 版）
- record（M2 以降）
  - `get`, `set`, `update`, `merge`, `keys`, `values`
- io（M3 以降）
  - `println`, `print`, `debug` など（strict-effects と相性の良い形に）

> 注意: ランタイムのビルトイン（`to_str`, 算術、比較、`effects.*` 等）を土台に lzscr で関数を実装します。Ctor アリティ検査を活用し、安全性の高い API を目指します。

## ローダ設計（M1: プリロード方式）
- CLI 起動時に stdlib を読み込み、ユーザプログラムの前に let グループとして合成。
- 提案する CLI フラグ（実装容易順）:
  - `--no-stdlib`（デフォルト読み込みを無効化）
  - `--stdlib-dir <PATH>`（`prelude.lzscr` を起点に読み込む）
- 依存解決（M1）
  - 初期は単一ファイル `prelude.lzscr` に集約し、内部で関数定義のみ（import は未導入）。

使用例:

```bash
cargo run -p lzscr-cli -- -e '~add 1 2'                 # stdlib 有効（デフォルト）
cargo run -p lzscr-cli -- -e '~add 1 2' --no-stdlib     # stdlib を無効化
cargo run -p lzscr-cli -- --file prog.lzscr --stdlib-dir ./stdlib
```

## 命名規約とスタイル
- snake_case（関数/変数）、コンストラクタは .Member 形式（例: `.Some`, `.None`）
- `prelude` に入れるのは曖昧さが少なく、使用頻度が高いものに限定
- フォーマッタで lzscr ソースを整形（CLI `--format-code`、VS Code 連携）

## 型の方針
- Option/Result: 多相型を前提とした関数群（例: `map : (a -> b) -> Option a -> Option b`）
- List 操作は多相（`map : (a -> b) -> [a] -> [b]`）
- 可能な範囲で注釈/モジュール毎の型シグネチャをドキュメント化

## テスト戦略
- 単体: 各関数に最小限の性質テスト（例: `length (append xs ys) == length xs + length ys`）
- 結合: `prelude` 経由での代表的なコード片が期待どおり評価されることを CLI テストで確認
- フォーマット互換: stdlib 自身が `--format-code` で安定整形され、挙動が変わらないこと
 - 自己ホスト準備: 文字列スライスや文字クラスの恒等性/境界条件（UTF-8 での境界クランプ）に関する性質テスト、簡易トークナイザ PoC の受入テスト

## ロードマップ（実装タスク）
- [x] stdlib ディレクトリ作成: `stdlib/prelude.lzscr`（雛形作成・Builtins 委譲を開始）
- [x] ランタイムに `~Builtins` レコード導入（namespaces: string/math）し、prelude から委譲
- [ ] Rust ランタイムに最小ビルトインを追加（str_len/str_concat/str_slice/str_cmp/str_codepoint_at/str_from_codepoint、cp_* 判定、算術/比較の公開整理）
- [ ] CLI: `--no-stdlib` / `--stdlib-dir` フラグ追加、プリロード実装
- [ ] M0 API 実装: list/string/unicode(codepoint)/option/result の最小セット（lzscr 実装）
- [ ] M0 検証: lzscr で簡易トークナイザ（識別子/整数/記号/空白）を実装して CLI から実行
- [ ] M1 API 実装: string の `join/split/find/starts_with/ends_with` と list の拡張、math 最小セット
- [ ] テスト: CLI から stdlib 機能を検証する統合テスト + 文字列/文字性質テスト
- [ ] ドキュメント: モジュール毎の README、API 一覧と使用例
- [ ] M2 以降のモジュール拡充（record/tuple/io 等）
- [ ] import 構文の提案と実装（パーサ/ランタイム/CLI 連携）
- [ ] バージョニングと互換性ポリシー策定

## サンプル（雛形イメージ）
```lzscr
~id x = x;
~compose f g x = f (g x);

# Option
~is_some o = match o with { .Some _ -> true(); .None -> false() };
~map_option f o = match o with { .Some x -> .Some (f x); .None -> .None };

# List（簡易例）
~length xs = match xs with {
  .[] -> 0;
  .,( _ , tail ) -> 1 + (~length tail);
};

# 簡易トークナイザ PoC（非常に簡略化の方針メモ）
# - 入力を空白で分割し、各トークンについて「全て数字なら .IntLit、そうでなければ .Ident」を返す。
# - 実装には list/string/codepoint API（`string.split`, `str_codepoint_at`, `cp_is_digit`, `list.map` など）を用いる。
# - まずは方針と型を決める：
#   Token = .IntLit Str | .Ident Str;
#   tokenize : Str -> [Token]
# - 具体的な実装は M0 の完了後に stdlib 上で作成し、CLI 統合テストで検証する。
```

---
このドラフトに沿ってまず M1（プリロード方式の最小 stdlib）から着手し、順次モジュールを拡充していきます。