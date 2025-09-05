# セマンティクス（現状）

- 評価器: `crates/lzscr-runtime`
  - 値: Unit/Int/Str/Symbol/Native/Closure
  - 環境Env: `vars: HashMap<String, Value>`, `strict_effects: bool`, `in_effect_context: bool`
  - 適用:
    - Native: 逐次カリー化、arity到達で実行。剰余は過剰適用処理。
    - Closure: `param` に値束縛して `body` を評価。
    - Symbol: 未飽和関数様として保持（PoC段階）。
  - 特別形: `(~seq a b)` は `a` を評価後、`b` を effect-context で評価。
  - 効果: `(~effects .sym)` で効果関数を取得。
    - `.print`/`.println` を実装。
    - strict-effects時、effect-context外では `EffectNotAllowed`。
- ビルトイン（例）:
  - `to_str : a -> Str`
  - `add, sub : Int -> Int -> Int`
  - `eq : a -> a -> Symbol("True"|"False")`
  - `lt : Int -> Int -> Symbol("True"|"False")`
  - `seq : a -> b -> b`（実装では参照と特別形）
  - `effects .println : Str -> Unit`（effect-contextでのみ許可）
