# Tokenizer/Scanner DSL (draft)

Goal: prepare a standard foundation for string scanning and character classification to build a self-hosted tokenizer/parser.

## Runtime builtins (implemented)

- `Builtins.char` namespace
  - `is_alpha : Char -> Bool`
  - `is_digit : Char -> Bool` (ASCII)
  - `is_alnum : Char -> Bool` (ASCII)
  - `is_space : Char -> Bool` (Unicode whitespace)
  - `between : Char -> Int -> Int -> Bool` (code-point range)
- additions under `Builtins.string`
  - `slice : Str -> Int -> Int -> Str` (character index)
  - `char_at : Str -> Int -> Option(Char)`
- `Builtins.scan` namespace (functional scanner on character indices)
  - `new : Str -> Scan` (`Scan` is a record `{ s: Str, i: Int }`)
  - `eof : Scan -> Bool`
  - `pos : Scan -> Int`, `set_pos : Scan -> Int -> Scan`
  - `peek : Scan -> Option(Char)`
  - `next : Scan -> Option((Char, Scan))`
  - `take_if : (Char -> Bool) -> Scan -> Option((Char, Scan))`
  - `take_while : (Char -> Bool) -> Scan -> (Str, Scan)`
  - `take_while1 : (Char -> Bool) -> Scan -> Option((Str, Scan))`

Returned `Bool` internally uses `Symbol("True"|"False")`, compatible with `Builtins.and/or/not`.

## stdlib: `stdlib/lex.lzscr`

- Compose character predicates and re-export scanning primitives.
- Simple token primitives (`take_ident`, `take_number`, `skip_spaces`).

Example:

```lzscr
~Lex = ~require .stdlib .lex;
~S = ~Lex .scan .new "let x = 42";
~pair = ~Lex .token .take_ident ~S; # => .Some ("let", S1), etc.
```

## Next

- Add higher-order combinators (`many`, `sep_by`, `choice`).
- Introduce a location type for error reporting (line/column computation).
- Expand character classes (Unicode categories).
