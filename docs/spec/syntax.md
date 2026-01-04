# Syntax (current)

## Syntax guide (2025-12-03)

### Literals and basic forms

- **Unit**: `()`
- **Int**: `42`, `0x2A` (hex), `0o52` (octal), `0b101010` (binary)
- **Float**: `1.0`, `.5`, `3.`, `1.5e10`, `2e-3`
- **Str**: `"hello"` (supports escape sequences: `\n`, `\r`, `\t`, `\0`, `\\`, `\"`, `\'`, `\u{HEX+}`; unknown escapes are errors)
- **Char**: `'a'`, `'\n'`, `'\u{3042}'` (unknown escapes are errors)
- **Ref**: `~name` (variable reference, e.g., `~add`, `~x`)
- **Named ctor**: bare identifier such as `Foo`, `Some`, `Tree`
  - They represent value-level tags whose arity is enforced only by how many arguments you apply: `Some 1`, `Tree left right`.
  - The name never captures a type; it is only a tag compared at runtime.
  - Zero arity uses just the bare name (`None`). Built-in ctors such as `Int`, `Bool`, or `Unit` are plain zero-arity Named ctors; a literal like `6` still has type `.Int`, while the expression `Int` refers to the ctor value itself.
- **Symbol**: `.name` (atomic symbol, e.g., `.println`, `.tag`)
  - Symbols are interned runtime atoms. They are never constructors and cannot be applied.
- **Type ctor literal**: `%{TypeExpr}` produces a first-class type value (see “Constructors vs Symbols and Types”). Type expressions use dotted tags for built-in primitives and structural heads (e.g., `.Int`, `.Bool`, `.List`, `.Tuple`), while user-defined constructors stay bare (`Foo`, `Some T`, `Tree Left Right`). Tuples spell their head as `.Tuple T1 ... Tn`, while record types stay braced such as `{ name: .Str, age: .Int }`. Bracket sugar `[T]` is accepted as `.List T`, and `(T1, T2, ..., Tn)` is sugar for `.Tuple T1 T2 ... Tn`.
- **Lambda**: `\x -> expr` or `\~x -> expr` (parameter patterns supported)
- **Block**: `{ expr }` (groups expression)
- **Apply**: `(f x)` (left-associative function application)
- **List literal**: `[1, 2, 3]` or `[]`
- **Record literal / ModeMap**: `{a: 1, b: 2}` or `{}` (keys must be identifiers)
  - ModeMap は Record 構文を用いた「モード選択マップ」です。
  - 拡張構文: `.{ ModeA: exprA, ModeB: exprB; exprDefault }`
    - `ModeA`, `ModeB` はドット付き識別子に対応するフィールド（例: `.Int`, `.Str`）。
    - セミコロン以降の `exprDefault` はデフォルト腕で、指定モードが存在しない場合に選択されます。
    - デフォルト腕省略時に未定義モードを選ぶと実行時エラーになります。
  - 構文上は Record と同一ですが、`.select` と糖衣構文により特別な選択動作を持ちます。
- **Tuple** (via sugar): `(a, b, c)` parsed as tuple pattern/expression
- **Raise**: `^(expr)` (raises an exception/control flow signal)

### Sugar and special forms

- **Effects**: `!println` → `(~effects .println)`
- **Type annotation**: `%{Ty} expr` (unifies the inferred type of `expr` with `Ty`; failure is a type error just like an ordinary annotation)
- **Type value / Type ctor**: `%{Ty}` (first-class type literal; `%{.Type}` is the canonical “type of types” and every `%{...}` lives in that universe)
- **Type declaration**: `%TypeName = %{ Ctor1 T1 | Ctor2 T2 }` (sum type definition)
- **Pattern type binding**: `%{'a, ?x} pat` (bind type variables in pattern scope)

### do-notation and sequencing

- **`!{ ... }` block**: Sugar for effect sequencing
  - Grammar: `stmt ::= pat <- expr ; | expr ;`
  - A block has zero or more statements (expressions with trailing `;`) and one final expression
  - Expansion rules:
    - Final `E` becomes `(~bind E (\x -> x))` (return value)
    - `expr; ACC` becomes `(~chain expr ACC)`
    - `pat <- expr; ACC` becomes `(~bind expr (\pat -> ACC))`

### Patterns

- **Wildcard**: `_` (matches anything, binds nothing)
- **Variable**: `~x` (binds matched value to `x`)
- **Unit**: `()`
- **Tuple**: `(~a, ~b)` (matches tuple, binds components)
- **List**: `[~a, ~b]` or `[]` (matches exact list structure)
- **Cons**: `~h : ~t` (matches head and tail of list)
- **Constructor**: `Some ~x` or `None` (matches Named ctors with their payloads; ctor names are bare identifiers only)
- **Record**: `{a: ~x, b: ~y}` (matches record with specific fields)
- **Literals**: `42`, `"hello"`, `'a'`, `True` (match exact values)
- **Type binding**: `%{'a} ~x` (bind type variable in pattern scope)

### Let-groups and bindings

- **Let-group**: `(~x = 1; ~y = 2; ~x + ~y)` or with file-level syntax
  - Bindings can be mutually/recursively referential within the group
  - Type declarations (`%Type = ...`) can appear anywhere in the group
  - Final expression is the result of the group

### Booleans and constructors

- **Booleans**: `True`, `False` constructors (no `~true` / `~false` aliases)
- **Constructor values**: `Foo 1 2` creates `Ctor("Foo", [1, 2])`
- **Option**: `Some x`, `None`
- **Result**: `Ok x`, `Err e`

### Constructors, type ctors, and symbols

- **Named ctors** are **always** bare identifiers. They never borrow the `.` syntax, and applying one just uses normal function application. Curried application keeps the ctor partially applied until its arity is satisfied. Tuple syntax `(a, b, c, ...)` is sugar for dedicated Named ctors (`.,`, `.,,`, …) rather than symbols.
- **Type ctors** are every `%{TypeExpr}` form. `%{Ty} expr` simply unifies the inferred type of `expr` with `Ty`, and `%{Ty}` yields a first-class type value whose own type is `%{.Type}`. Type expressions keep the dot prefix for primitives and structural heads (`.Int`, `.Bool`, `.List`, `.Tuple`) so they never conflict with value-level Named ctors, while user-defined constructor names remain bare (e.g., `Foo`, `Some %a`).
- `%{.Type}` is the canonical “type of types.” Future work may admit richer `%{Expr}` type lifting, but today every `%{...}` lives in `%{.Type}` and annotations fail if unification with `%{.Type}`-inhabiting expressions cannot succeed.
- **Symbols (`.foo`)** are atomic interned values. Applying a symbolは原則としてランタイムエラーですが、ModeMap に対しては特別扱いされます（下記糖衣構文）。
  - 糖衣構文: `.Ident ModedValue` は `.select .Ident ModedValue` と等価に解釈されます。
  - `ModedValue` は上記 ModeMap 構文で書かれた値、または同等の選択可能値です。
- The type of the Named ctor literal `Foo` is `%{(Foo .. | ...)}` (the literal `..` marks unspecified arity; `...` indicates the remaining ctors in the sum type). `%{Foo ..}` expands to `%{(Foo | %a -> Foo %a | %a -> %b -> Foo %a %b | ...)}`, and `%{Foo T1 T2 ..}` fixes the first payload positions before continuing the curry.
- Notation reminder: `..` is a concrete lexical token that actually appears in type expressions (e.g., `%{Foo ..}`), whereas `...` is merely prose ellipsis for “and the other alternatives.” Do not interchange the two when writing code.

### Infix operators (left-assoc; Pratt precedence)

Precedence (high→low):
- 20: `*`, `/`, `.*`, `./`
- 10: `+`, `-`, `.+`, `.-`
- 5: `<`, `<=`, `>`, `>=`, `.<`, `.<=`, `.>`, `.>=`
- 4: `==`, `!=`

Desugaring (to function application):

- Int ops
  - `a + b` → `(~add a) b`
  - `a - b` → `(~sub a) b`
  - `a * b` → `(~mul a) b`
  - `a / b` → `(~div a) b` (error on 0)
  - `-a` → `(~sub 0) a`
- Float ops (separate symbols; mixed types are errors)
  - `a .+ b` → `(~fadd a) b`
  - `a .- b` → `(~fsub a) b`
  - `a .* b` → `(~fmul a) b`
  - `a ./ b` → `(~fdiv a) b` (error on 0.0)
- Comparisons/equality
  - `a < b`  → `(~lt a) b`, `a <= b` → `(~le a) b`, `a > b` → `(~gt a) b`, `a >= b` → `(~ge a) b`
  - `a .< b` → `(~flt a) b`, `a .<= b` → `(~fle a) b`, `a .> b` → `(~fgt a) b`, `a .>= b` → `(~fge a) b`
  - `a == b` → `(~eq a) b`, `a != b` → `(~ne a) b`

Return value is a Bool (`True` | `False`).

### Reserved tokens and symbols

- **Ref/Effect**: `~`, `!`
- **Symbol/Member**: `.`
- **Type**: `%`
- **Grouping**: `(`, `)`, `{`, `}`, `[`, `]`
- **Separators**: `,`, `;`
- **Operators**: `->`, `<-`, `|`, `||`, `^|`, `^(`, `:`, `@`
- **Lambda**: `\`
- **Assignment**: `=`
- **Arithmetic**: `+`, `-`, `*`, `/`, `.+`, `.-`, `.*`, `./`
- **Continuation**: `..` (for partial ctor application / last sum ctor marker as unknown rest)


### Examples

```lzscr
# Arithmetic and comparison
1 + 2 * 3            # => 7
1.5 .+ 2.0 .* 2.0    # => 5.5
8 / 2                # => 4
8.0 ./ 2.0           # => 4.0
1 < 2                # => True
1.5 .<= 2.0          # => True

# Constructors and pattern matching
(Some 42) == (Some 42)  # => True
(\(Some ~x) -> ~x | \_ -> 0) (Some 10)  # => 10

# Lists and records
[1, 2, 3]            # list literal
{a: 1, b: 2}         # record literal
(\(~h : ~t) -> ~h) [1, 2, 3]  # => 1

# Effects
!println "ok"                      # effect call
(~seq () (!println "ok"))          # explicit sequencing
!{ ~x <- !read_line; !println ~x } # do-notation

# Exception handling
^("error message")                 # raise exception
~try_action || ^("fallback")       # catch with ||
~try_action ^| ~handle_error       # catch with ^|

# Lambdas and application
(\~x -> ~x) 10       # => 10
(\~x ~y -> ~x + ~y) 1 2  # => 3

# Type annotations
%{.Int -> .Int} (\~x -> ~x + 1)   # annotated lambda
%{.List .Int} [1, 2, 3]             # annotated list
```
