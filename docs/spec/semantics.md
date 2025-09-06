# セマンティクス（現状）

- 評価器: `crates/lzscr-runtime`
  - 値: Unit/Int/Float/Bool/Str/Symbol/List/Tuple/Record/Native/Closure
  - 環境Env: `vars: HashMap<String, Value>`, `strict_effects: bool`, `in_effect_context: bool`
  - 適用:
    - Native: 逐次カリー化、arity到達で実行。剰余は過剰適用処理。
    - Closure: `param` に値束縛して `body` を評価。
    - Symbol: 未飽和関数様として保持（PoC段階）。
  - 特別形:
    - `(~seq a b)` は `a` を評価後、`b` を effect-context で評価。
    - `(~chain a b)` は `a` を評価後、`b` を effect-context で評価し、その値を返す。
    - `(~bind e k)` は `e` を評価して値 `v` を得て、`k` を effect-context で評価したのち `k v` を適用して返す。
  - 効果: `(~effects .sym)` で効果関数を取得。
    - `.print`/`.println` を実装。
    - strict-effects時、effect-context外では `EffectNotAllowed`。
- ビルトイン（例）:
  - `to_str : a -> Str`
  - `add, sub : Int -> Int -> Int`
  - `eq : a -> a -> Symbol("True"|"False")`（Int/Float/Bool/Str/Unit/Symbolに対応）
  - `lt : Int|Float -> Int|Float -> Symbol("True"|"False")`
  - `seq : a -> b -> b`（実装では参照と特別形）
  - `effects .println : Str -> Unit`（effect-contextでのみ許可）
補足:
- Bool は一時的に `~true`/`~false` を環境に注入（将来はリテラル導入）。
- Float はリテラルをサポート（例: 1.0, .5, 10.）。
- Char は現時点で Int と同一カテゴリ（0..=255 を意図）。専用リテラルは未実装。
- List/Tuple/Record は不変値としてランタイムの表示や to_str に対応（構文は未定）。
