# トークナイザ/スキャナ DSL（ドラフト）

目的: 自己ホストの tokenizer/parser 構築に向けて、文字列スキャンと文字分類の標準土台を用意します。

## ランタイム Builtins 追加（実装済み）

- `Builtins.char` 名前空間
  - `is_alpha : Char -> Bool`
  - `is_digit : Char -> Bool` (ASCII)
  - `is_alnum : Char -> Bool` (ASCII)
  - `is_space : Char -> Bool` (Unicode 空白)
  - `between : Char -> Int -> Int -> Bool`（コードポイント範囲）
- `Builtins.string` に追加
  - `slice : Str -> Int -> Int -> Str`（文字インデックス指定）
  - `char_at : Str -> Int -> Option(Char)`
- `Builtins.scan` 名前空間（文字インデックスベースの関数型スキャナ）
  - `new : Str -> Scan`（`Scan` は `{ s: Str, i: Int }` レコード）
  - `eof : Scan -> Bool`
  - `pos : Scan -> Int`, `set_pos : Scan -> Int -> Scan`
  - `peek : Scan -> Option(Char)`
  - `next : Scan -> Option((Char, Scan))`
  - `take_if : (Char -> Bool) -> Scan -> Option((Char, Scan))`
  - `take_while : (Char -> Bool) -> Scan -> (Str, Scan)`
  - `take_while1 : (Char -> Bool) -> Scan -> Option((Str, Scan))`

戻りの `Bool` は内部的に `Symbol("True"|"False")` を用い、`Builtins.and/or/not` 互換としています。

## stdlib: `stdlib/lex.lzscr`

- 文字述語の合成と、スキャン基礎関数の再エクスポート。
- 簡易トークンプリミティブ（`take_ident`, `take_number`, `skip_spaces`）。

使用例:

```lzscr
~Lex = ~require .stdlib .lex;
~S = ~Lex .scan .new "let x = 42";
~pair = ~Lex .token .take_ident ~S; # => .Some ("let", S1) など
```

## 今後

- 高階コンビネータ（`many`, `sep_by`, `choice`）の追加。
- エラーレポート用に位置情報型を導入（行・列計算）。
- 文字クラスの拡充（Unicode カテゴリ）。
