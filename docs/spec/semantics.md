# Semantics (current)
Disclaimer: This document describes the current PoC behavior. Items labeled as planned or provisional are WIP and may change.

- Evaluator: `crates/lzscr-runtime`
  - Values: Unit/Int/Float/Bool/Str/Symbol/List/Tuple/Record/Native/Closure
  - Env: `vars: HashMap<String, Value>`, `strict_effects: bool`, `in_effect_context: bool`
  - Application:
    - Native: curried; execute when arity is satisfied; extra args are applied to the result
    - Closure: bind value to `param` via pattern and evaluate `body`
    - Symbol: behaves like an unapplied constructor/function-like value (PoC semantics)
  - Special forms:
    - `(~seq a b)`: evaluate `a`, then evaluate `b` in effect-context
    - `(~chain a b)`: evaluate `a`, then evaluate `b` in effect-context and return its value
    - `(~bind e k)`: evaluate `e` to value `v`, evaluate `k` in effect-context, then apply `k v`
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
- Bool represented by constructors `.True` / `.False` (no implicit variable aliases)
- Float supports literals (e.g., 1.0, .5, 10.)
- Char is currently treated as Int (intended 0..=0x10FFFF); dedicated literal not implemented
- List/Tuple/Record are immutable values with runtime display and to_str support
