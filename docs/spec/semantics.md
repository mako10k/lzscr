# Semantics (current)
Disclaimer: This document describes the current PoC behavior. Items labeled as planned or provisional are WIP and may change.

- Evaluator: `crates/lzscr-runtime`
  - Values: Unit/Int/Float/Bool/Str/Char/Symbol/List/Tuple/Record/Ctor/Native/Closure
  - Env: `vars: HashMap<String, Value>`, `strict_effects: bool`, `in_effect_context: bool`
  - Application:
    - Native: curried; execute when arity is satisfied; extra args are applied to the result
    - Closure: bind value to `param` via pattern and evaluate `body`
    - Ctor: bare identifier constructors support ordinary application; arity checks happen at runtime via the constructor table
    - Symbol: is a pure atom. 原則として適用は不可だが、ModeMap に対する糖衣構文（`.Ident ModedValue` ≡ `.select .Ident ModedValue`）に限り選択として扱う
  - Special forms:
    - `(~seq a b)`: evaluate `a`, then evaluate `b` in effect-context
    - `(~chain a b)`: evaluate `a`, then evaluate `b` in effect-context and return its value
    - `(~bind e k)`: evaluate `e` to value `v`, evaluate `k` in effect-context, then apply `k v`
    - `.select .M e`: evaluate `e` and, if it is a ModeMap (Record 拡張), select the arm labeled `.M`; それが無ければデフォルト腕があればそれを返す。いずれも無ければランタイムエラー
  - Effects: `(~effects .sym)` retrieves an effect function
    - Implements `.print`/`.println`
    - With strict-effects, effects outside the effect-context raise `EffectNotAllowed`
- Builtins (examples):
  - `to_str : a -> Str`
  - `add, sub : Int -> Int -> Int`
  - `eq : a -> a -> Bool` (supports Int/Float/Bool/Str/Unit/Symbol)
  - `lt : Int|Float -> Int|Float -> Bool`
  - `seq : a -> b -> b` (implemented via ref + special form)
  - `effects .println : Str -> Unit` (only in effect-context)
Notes:
- Bool represented by constructors `True` / `False` (no implicit variable aliases)
- ModeMap literal supports an explicit default arm: `.{ M1: v1, M2: v2, ...; vDefault }`. `.select` により選択する。`current_mode` が将来的に導入される場合は、明示の `.M` が無いときのフォールバックに用いる。
 - 型レベルとの整合: ModeMap の型は「モード名→腕の型」の写像と、任意のデフォルト腕型から構成される。`.select .M e` の型は、`e` が ModeMap 型で `M` に対応する腕が存在する場合はその腕の型、無い場合はデフォルト腕型（存在すれば）になる。いずれも無ければ型エラー相当（実行時エラーに一致）。
- Float supports literals (e.g., 1.0, .5, 10.)
- Char is a distinct literal/value represented as a Unicode scalar value code point (`i32`)
- List/Tuple/Record are immutable values with runtime display and to_str support
