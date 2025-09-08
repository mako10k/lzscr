# lzscr type definitions (mu-types / isorecursive) design notes

This document records the plan to add user type definitions (% declarations) and introduce mu-types (isorecursive) in the internal representation. We first agree on the spec and then implement in stages.

## Goals
- Remove list-specific guessing and support general recursive ADTs soundly.
- Allow named types (List/Option, etc.) to be used in polymorphic annotations.
- Stay compatible with the existing HM inference (termination and error messages should improve).

## Approach (Plan C)
- Users declare types with `%`.
- Detect “shape-identical self references” in RHS and convert them to mu-types internally (isorecursive).
- Unification unfolds μ one step only when needed.
- Known families (List/Option/Bool, etc.) remain printable and annotatable by name while being convertible to/from their internal structure.

## Syntax (draft)
- Top-level type declarations
  - `%TypeName %a %b = %{ .Tag TYPE | .Tag TYPE | ... }`  // sum
  - Future: `%TypeName %a ... = %record{ field: TYPE, ... }`  // record
- Examples
  - `%Option %a = %{ .Some %a | .None }`
  - `%List %a = %{ .Nil | .Cons %a (%List %a) }`

Inside type expressions:
- `%a` are type variables.
- `.Tag` are constructor labels (may share the label space with value-level).
- `%Name %args...` applies a named type.

## Self-reference -> μ conversion (shape rule)
- Target: occurrences of the LHS head name in the RHS that are shape-identical to the LHS head application.
- “Shape-identical” means:
  - Exactly the same list of type variables in the same order (only alpha-renaming allowed).
  - Example: in `%List %a` the RHS `%List %a` is shape-identical; `%List (%Foo %a)` or partial applications are not.
- Steps:
  1) Introduce a fresh bound type variable `%T`.
  2) Replace all shape-identical self references `%Head %params` in RHS with `%T`.
  3) Represent as `μ %T . Body` (Body may be sum/record/tuple etc.).
- Example (List):
  - Input: `%List %a = %{ .Nil | .Cons %a (%List %a) }`
  - Internal: `μ %T . sum { .Nil | .Cons %a %T }`

Option doesn’t need μ:
- Input: `%Option %a = %{ .Some %a | .None }`
- Internal: `sum { .Some %a | .None }`

## Scope
- LHS args (`%a`, etc.): only valid within that declaration’s RHS.
- Self binder `%T`: introduced by the internal conversion (not referenceable externally).
- LHS head name (`%List`, etc.): bound in the type namespace, mutually visible within the group, exported outside.

## Mutually recursive groups
- Treat a consecutive `%`-declaration sequence as one group and process in two passes.
  - Pass 1: register head names and arities.
  - Pass 2: perform the “shape self-ref -> %T -> μ” transform in each RHS. References to other types remain Named.
- Mutual recursion is allowed (A can reference B and vice versa). Only self references are replaced by `%T`.

## Soundness checks
- Positivity: in `μ %T . Body`, `%T` must appear only in positive positions.
  - Allowed: element positions of sum/record/list/tuple (covariant).
  - Disallowed: negative positions such as function argument types.
- 同形制約: 自己参照は“同形”でなければならない。
- 展開停止性: μ 展開は on-demand で 1 段。Visited セット/燃料で発散防止。

## ユニファイ戦略（isorecursive）
- `unify(μ %T . A, B)`:
  - 必要に応じて `A[%T := μ %T . A]` を 1 回だけ展開して B と比較（逆も同様）。
- occurs check:
  - 通常の型変数に対して実施。`%T` は束縛子なので対象外。
  - 変数 vs μ の場合は、μを 1 段剥がしてから判定する優先度を採用。

## 既存との整合（移行）
- 既知ファミリの正規化:
  - 表示/注釈では `%List %a` / `%Option %a` を優先表示。
  - 内部は既存の構造（List 型/ SumCtor）と相互変換可能にする（移行期）。
- Prelude/既存コードは互換維持。型注釈で型名を使えるようにして読みやすさ向上。

## 実装スライス（段階導入）
1. パーサ
   - `%` 型宣言を追加（ヘッド名、型変数列、RHS 型式）。
   - 型式に `Mu(T, Body)` と `Named(name, args)` を追加。
2. 型定義環境
   - `TypeDefEnv`: name → (params, body)。
   - 2 パス登録＋“同形自己参照→μ” 変換。
3. 型表現/ユニファイ
   - μ の 1 段展開ユニファイ、positivity チェック、visited 管理。
   - Named の on-demand 展開（完全展開せず比較必要箇所のみ）。
4. 表示/エラー
   - 既知ファミリは型名で表示。構造一致時に Named へ寄せる。
   - occurs/positivity 失敗にはスパン付きの分かりやすい診断。
5. 橋渡し
   - 既存 List/Option/Bool と相互運用。最初は内部表現を併存、後に統一を検討。

## オープン事項（要合意）
- グルーピングの決め方（ファイル/空行/ブロック）
- ラベル空間（.Tag）の共有方針（現状どおりでよい見込み）
- Named の等価性：完全展開の構造同値ベースで運用（名義同値は導入しない）
- positivity チェックの厳密度（関数型の負位置のみ禁止で開始）

## 参考スニペット
- Option:
  - ソース: `%Option %a = %{ .Some %a | .None }`
  - 内部: `sum { .Some %a | .None }`
- List:
  - ソース: `%List %a = %{ .Nil | .Cons %a (%List %a) }`
  - 内部: `μ %T . sum { .Nil | .Cons %a %T }`

---
この仕様で進める場合、まずはパーサと型定義環境の導入（段階 1-2）から着手します。実装後は、`~map`, `~length` などの代表関数で型注釈とユニファイ挙動を検証します。

  ## Let 式への統合（型宣言グルーピングとスコープ）

  Let 式に型宣言（%）を組み込む。構文は次のように拡張する。

  - 旧: `letExpr ::= bind* body bind*`（ただし前後の bind は 1 つ以上）
  - 新: `letExpr ::= (bind | typedeclare)* body (bind | typedeclare)*`

  ここで `typedeclare` は本ドキュメントの `%TypeName %a ... = %{ ... }` を指す。

  スコープ/解決規則:
  - Let ブロック内の連続した `(bind | typedeclare)` 列を 1 グループとみなし、値名空間と型名空間をそれぞれ相互再帰として処理する。
  - 2 パス処理（型名空間）:
    1) パス1: `%TypeName` とアリティの登録（シャドウ可能）。
    2) パス2: RHS を“同形自己参照→%T→μ”変換して `TypeDefEnv` に確定登録。
  - 値の `bind` から `typedeclare` を参照可能（同一 Let グループ内であれば可）。
  - `typedeclare` から外側の型名も参照可能（通常の静的スコープ）。
  - LHS の型変数（%a 等）は宣言 RHS 内にだけ有効。自己束縛 `%T` は内部のみ。

  例（Let 内での相互参照）:
  ```
  let
    %List %a = %{ .Nil | .Cons %a (%List %a) }
    ~length = \(~xs) ->
      \[] -> 0
    | \[ ~_ : ~t ] -> 1 + (~length ~t)
  in
    ~length [1,2,3]
  ```
  この例では、`%List` は Let グループ内に束縛され、`~length` から参照できる。`%List` はグループの外へもエクスポートされる。
