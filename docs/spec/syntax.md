# Syntax (current)

## Syntax guide (2025-12-03)

### Literals and basic forms

- **Unit**: `()`
- **Int**: `42`, `0x2A` (hex), `0o52` (octal), `0b101010` (binary)
- **Float**: `1.0`, `.5`, `3.`, `1.5e10`, `2e-3`
- **Str**: `"hello"` (supports escape sequences: `\n`, `\t`, `\\`, `\"`, `\xHH`, `\u{HHHH}`)
- **Char**: `'a'`, `'\n'`, `'\x41'`, `'\u{3042}'`
- **Ref**: `~name` (variable reference, e.g., `~add`, `~x`)
- **Ctor (data constructor)**: bare identifier such as `Foo`, `Some`, `Tree`
  - Zero arity uses just the bare name (`None`).
  - Applying a constructor uses ordinary function application syntax: `Some 1`, `Tree left right`.
- **Symbol**: `.name` (atomic symbol, e.g., `.println`, `.tag`)
  - Symbols are interned runtime atoms. They are never constructors and cannot be applied.
- **Lambda**: `\x -> expr` or `\~x -> expr` (parameter patterns supported)
- **Block**: `{ expr }` (groups expression)
- **Apply**: `(f x)` (left-associative function application)
- **List literal**: `[1, 2, 3]` or `[]`
- **Record literal**: `{a: 1, b: 2}` or `{}` (keys must be identifiers)
- **Tuple** (via sugar): `(a, b, c)` parsed as tuple pattern/expression
- **Raise**: `^(expr)` (raises an exception/control flow signal)

### Sugar and special forms

- **Effects**: `!println` → `(~effects .println)`
- **Type annotation**: `%{Int} expr` (annotate expression with type)
- **Type value**: `%{Int -> Bool}` (first-class type value)
- **Type declaration**: `%TypeName = %{ .Ctor1 T1 | .Ctor2 T2 }` (sum type definition)
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
- **Constructor**: `Some ~x` or `None` (matches constructor with args; constructor names are bare idents)
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

### Constructors vs Symbols

- Constructors are **always** bare identifiers. The surface syntax never treats `.name` as a constructor alias.
- Symbols (`.foo`) are atomic interned values. Applying a symbol is a runtime error and symbols are never implicitly converted into constructors.
- Tuple syntax `(a, b, c, ...)` is sugar for dedicated tuple constructors; it does not desugar through symbols either.
- Constructor literals behave like curried functions until all arguments are supplied.
- Constructor type of expr `Foo` is `%{(Foo .. | ...)}` (the literal `..` marks undefined arity; `...` is explanatory ellipsis for other constructors in the sum).
- Constructor type `%{Foo ..}` means `%{(Foo | %a -> Foo %a | %a -> %b -> Foo %a %b | ...)}` (all arities up to some max).
- Constructor type `%{Foo T1 T2 ..}` means `%{(Foo T1 T2 | %a -> Foo T1 T2 %a | %a -> %b -> Foo T1 T2 %a %b | ...)}` (fixed first args, then curried).

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
(\(Some ~x) -> ~x | \None -> 0) (Some 10)  # => 10

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
%{Int -> Int} (\~x -> ~x + 1)  # annotated lambda
%{[Int]} [1, 2, 3]             # annotated list
```
