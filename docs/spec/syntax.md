# Syntax (current)

- Literals:
  - Unit: `()`
  - Int: `42`
  - Str: `"hi"` (supports escapes)
- Identifiers:
  - Reference: `~name`
  - Symbol value (member-style): `.println` parses as a Symbol
- Lambda: `\x -> expr`
- Application: `(f x)` (left-associative)
- Block: `{ expr }`
- Sugar:
  - `!sym` → `(~effects .sym)`
- Reserved/tokens: `~`, `!`, `.ident`, `(`, `)`, `{`, `}`, `,`, `->`, `\\`

例:
## Syntax guide (2025-09-05)

### Literals/basics

- Unit: `()`
- Int: `42`
- Float: `1.0`, `.5`, `3.`
- Str: `"hi"` (supports escapes)
- Ref: `~name` (e.g., `~add`, `~true`)
- Symbol value: `.name` (e.g., `.println`, `.Foo`) — constructors are .Member-only
- Lambda: `\x -> expr`
- Block: `{ expr }`
- Apply: `(f x)` (left-associative)

### Sugar

- Effects: `!println` → `(~effects .println)`

## do-notation and sequencing

- `!{ ... }` is sugar for sequencing.
  - Grammar: `stmt ::= pat <- expr ; | expr ;`
  - A block has zero or more statements (any expression with trailing `;`) and one final expression.
  - Expansion rules:
    - Final `E` becomes `(~bind E (\x -> x))` (return value)
    - `expr; ACC` becomes `(~chain expr ACC)`
    - `pat <- expr; ACC` becomes `(~bind expr (\pat -> ACC))`
- Booleans: `true()` → `~true`, `false()` → `~false`
- Constructor value: `.Foo 1 2` → `Ctor(".Foo", [1,2])`

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
  - `a / b` → `(~div a) b`（0 でエラー）
- Float ops (separate symbols; mixed types are errors)
  - `a .+ b` → `(~fadd a) b`
  - `a .- b` → `(~fsub a) b`
  - `a .* b` → `(~fmul a) b`
  - `a ./ b` → `(~fdiv a) b`（0.0 でエラー）
- Comparisons/equality
  - `a < b`  → `(~lt a) b`、`a <= b` → `(~le a) b`、`a > b` → `(~gt a) b`、`a >= b` → `(~ge a) b`
  - `a .< b` → `(~flt a) b`、`a .<= b` → `(~fle a) b`、`a .> b` → `(~fgt a) b`、`a .>= b` → `(~fge a) b`
  - `a == b` → `(~eq a) b`、`a != b` → `(~ne a) b`

Return value is currently `Symbol("True"|"False")`.

### Examples

```
1 + 2 * 3            # => 7
1.5 .+ 2.0 .* 2.0    # => 5.5
8 / 2                # => 4
8.0 ./ 2.0           # => 4.0
(.Foo 1) == (.Foo 1) # => True
!println "ok"
(~seq () (!println "ok"))
(\x -> x) 10
```
