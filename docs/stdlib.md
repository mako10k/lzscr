# lzscr 標準ライブラリ計画（ドラフト）

この文書は、lzscr の標準ライブラリ（以下、stdlib）を段階的に整備するための計画と設計方針をまとめたものです。初期は CLI による事前読込（プリロード）で提供し、将来的にモジュール/インポート構文を導入して本格運用に移行します。

## 目的と非目標
- 目的
  - ふだんのプログラミングで必要な基本関数群を提供（コレクション操作、Option/Result、文字列、数値、レコード操作、IO ラッパ等）。
  - 既存のビルトイン（ランタイム）を安全・一貫した API として露出。
  - 言語の HM 型推論と相性のよい API 形状を採用（関数合成・高階関数を活用）。
- 非目標（初期段階）
  - 並行/非同期、ファイルシステム等の重い IO（将来の別提案で拡張）。
  - パッケージマネージャや外部依存の解決機構（将来検討）。

## フェーズ計画（マイルストーン）
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
  - `len`, `concat`, `join`, `split`（簡易）, `to_int`, `from_int`
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

## 命名規約とスタイル
- snake_case（関数/変数）、UpperCamelCase（Ctor）
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

## ロードマップ（実装タスク）
- [ ] stdlib ディレクトリ作成: `stdlib/prelude.lzscr`（空の雛形）
- [ ] CLI: `--no-stdlib` / `--stdlib-dir` フラグ追加、プリロード実装
- [ ] M1 API 実装: option/result/list/string/math（最小セット）
- [ ] テスト: CLI から stdlib 機能を検証する統合テスト
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
```

---
このドラフトに沿ってまず M1（プリロード方式の最小 stdlib）から着手し、順次モジュールを拡充していきます。