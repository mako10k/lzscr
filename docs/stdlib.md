## Roadmap (implementation tasks)

- [x] Create stdlib directory: `stdlib/prelude.lzscr` (skeleton, start delegating to Builtins)
- [ ] Add minimal Rust runtime builtins (str_len/str_concat/str_slice/str_cmp/str_codepoint_at/str_from_codepoint, cp_* predicates, arithmetic/comparison exposure)
- [ ] Implement M0 APIs in lzscr: list/string/unicode(codepoint)/option/result
- [ ] Validate M0: implement a tiny tokenizer in lzscr and run it via CLI
- [ ] Implement M1 APIs: string `join/split/find/starts_with/ends_with`, list extensions, minimal math

Usage:
```bash
cargo run -p lzscr-cli -- -e '~add 1 2'                 # stdlib enabled (default)
cargo run -p lzscr-cli -- -e '~add 1 2' --no-stdlib     # disable stdlib
cargo run -p lzscr-cli -- --file prog.lzscr --stdlib-dir ./stdlib
```

Note: `prelude.lzscr` lives under `stdlib/`. The CLI prepends it by default and then appends the user code as one LetGroup (disable with `--no-stdlib`).

# lzscr standard library plan (draft)

This document outlines the phased plan and design principles for the lzscr standard library (stdlib). Initially, it is provided by preloading via the CLI; later, module/import syntax will be added for normal usage.

## Goals and non-goals
- Goals
  - Provide the basic functions needed for everyday programming (collections, Option/Result, strings, numbers, records, IO wrappers).
  - Expose runtime builtins as a safe, consistent API surface.
  - Favor HM-friendly, composable higher-order APIs.
  - Mid-term: prepare minimal primitives/stdlib to move toward self-hosting (parser/evaluator in lzscr itself).
- Non-goals (initial)
  - Heavy IO (files/async/concurrency)
  - External dependency/package resolution

### Self-host direction and minimal requirements

Rust runtime builtins to expose:
- Arithmetic/comparison: `+ - * / % == != < <= > >=` (Int/Float variants)
- Equality/hash (initially for basic types)
- Strings: `str_len : Str -> Int`, `str_concat : Str -> Str -> Str`,
  `str_slice : Str -> Int -> Int -> Str` (half-open, clamped), `str_cmp : Str -> Str -> Int`
- Codepoints: `str_codepoint_at : Str -> Int -> Option Int`, `str_from_codepoint : Int -> Str`
- Char classes (on Int codepoints): `cp_is_alpha`, `cp_is_digit`, `cp_is_alnum`, `cp_is_space`, `cp_is_newline`, `cp_is_lower`, `cp_is_upper`
- Debug/display: `to_str : a -> Str`, `println : Str -> Unit`

Foundation in stdlib (implemented in lzscr):
- list: `length/map/filter/foldl/foldr/append/reverse/take/drop/range`
- string: safe wrappers for the above + `join/split` (simple), `starts_with/ends_with`, `find`
- unicode/codepoint: thin wrappers + helpers
- option/result: as proposed below

Parser/lexer utilities:
- `Span = { start: Int, end: Int }`
- `Token = .Ident | .IntLit | .StrLit | .Symbol | ...` (phased)
- `StringCursor`-like operations can be built with `str_len/str_slice/str_codepoint_at`

## Phases (milestones)
0. M0: self-host preparation (minimal builtins + stdlib base)
   - Add minimal builtins in Rust (above)
   - Implement minimal list/string/char/option/result in `stdlib/prelude.lzscr`
   - Build a tiny tokenizer PoC using these APIs
   - Add CLI integration tests
1. M1: bootstrap stdlib (preloaded)
   - Place .lzscr sources in `stdlib/`
   - Default enable via CLI (`--no-stdlib` to disable, `--stdlib-dir` to override)
   - Provide modules (can be a single file): prelude, option, result, list, string, math (minimal)
   - Tests via CLI
2. M2: record/tuple/map utilities
   - record: get/set/update/merge, keys/values, small lens-like helpers
   - tuple: fst/snd/..., zip/unzip
3. M3: IO/effects wrappers
   - Safe APIs compatible with strict-effects; print/println/debug
4. M4: formatter/type-aligned API grooming
   - Naming/namespace review; conservative prelude exposure
5. M5: module/import syntax
   - `import` and friends; consistent module resolution
6. M6: versioning/compat policy
   - Semantic versioning tied to language; breaking changes only on majors

## Structure and API (initial)

- prelude (auto-open when preloaded)
  - Compose `(~compose f g x) = f (g x)`, identity `(~id x) = x`
  - Bool/logical wrappers: `and/or/not` with short-circuit behavior preserved
  - Re-export of Option/Result helpers
  - Delegation: collect runtime builtins under `~Builtins` record namespaces; user code references via prelude (e.g., `~Str = ~Builtins .string`)
- option
  - Constructors: `.Some x`, `.None`
  - `is_some`, `is_none`, `map`, `and_then`, `unwrap_or`
- result
  - Constructors: `.Ok x`, `.Err e`
  - `map`, `map_err`, `and_then`, `unwrap_or`
- list
  - `nil` (`[]`) and `cons` (pair tag `.,`); literal sugar `[a,b,c]`
  - `length`, `map`, `filter`, `foldl`, `foldr`, `append`, `reverse`, `take`, `drop`, `range`
- string
  - `len`, `concat`, `slice`, `cmp`, `join`, `split` (simple), `starts_with`, `ends_with`, `find`, `to_int`, `from_int`
- char
  - `of_int`, `to_int`, `is_alpha`, `is_digit`, `is_alnum`, `is_space`, `is_lower`, `is_upper`, `is_newline`
- math
  - `abs`, `min`, `max`, `clamp`, `floor`, `ceil`, `pow` (Int/Float variants as needed)
- record (M2+)
  - `get`, `set`, `update`, `merge`, `keys`, `values`
- io (M3+)
  - `println`, `print`, `debug` (strict-effects compatible)

Note: We build stdlib functions on top of the runtime builtins (to_str, arithmetic/comparison, effects.*). Constructor arity checks help improve safety.

## Loading design (M1: preload)
- On CLI start, load stdlib and merge it before user program as a LetGroup.
- CLI flags:
  - `--no-stdlib` to disable
  - `--stdlib-dir <PATH>` to override
- Dependency resolution (M1):
  - Start with a single `prelude.lzscr` containing function definitions only; no imports yet.

Usage:

```bash
cargo run -p lzscr-cli -- -e '~add 1 2'
cargo run -p lzscr-cli -- -e '~add 1 2' --no-stdlib
cargo run -p lzscr-cli -- --file prog.lzscr --stdlib-dir ./stdlib
```

## Naming and style
- snake_case for functions/variables; constructors are .Member-only (e.g., `.Some`, `.None`)
- Keep prelude minimal and unambiguous
- Use the formatter (`--format-code`) and the VS Code extension when available

## Types
- Option/Result and List APIs are polymorphic; document signatures as we stabilize

## Testing
- Unit: minimal properties per function (e.g., `length (append xs ys) == length xs + length ys`)
- Integration: CLI-based golden tests for representative usage via prelude
- Formatting: stdlib should be stable under `--format-code`
- Self-host prep: string slice/char-class invariants and tokenizer PoC acceptance tests

## Roadmap (task checklist)
- [x] Create stdlib directory: `stdlib/prelude.lzscr` (skeleton and Builtins delegation)
- [x] Introduce `~Builtins` namespaces in runtime (string/math) and delegate from prelude
- [ ] Minimal builtins in Rust (string ops, cp_* predicates, arithmetic/comparison exposure)
- [ ] CLI: `--no-stdlib` / `--stdlib-dir` flags and preload
- [ ] M0 APIs in lzscr: list/string/unicode(option/result)
- [ ] M0 validation: tokenizer PoC invoked from CLI
- [ ] M1 APIs: string/list/math enrichments
- [ ] Tests: CLI integration + string/char property tests
- [ ] Docs: per-module READMEs, API index and examples
- [ ] M2+ module expansion (record/tuple/io)
- [ ] Import syntax proposal/impl
- [ ] Versioning/compat policy

## Samples (skeleton image)
```lzscr
~id x = x;
~compose f g x = f (g x);

# Option
~is_some o = match o with { .Some _ -> true(); .None -> false() };
~map_option f o = match o with { .Some x -> .Some (f x); .None -> .None };

# List (simple example)
~length xs = match xs with {
  .[] -> 0;
  .,( _ , tail ) -> 1 + (~length tail);
};

# Tiny tokenizer PoC (sketch)
# - Split by whitespace; if all digits -> .IntLit, else .Ident
# - Use list/string/codepoint APIs: string.split, str_codepoint_at, cp_is_digit, list.map, etc.
# - First decide shapes and types:
#   Token = .IntLit Str | .Ident Str;
#   tokenize : Str -> [Token]
# - Implement after M0 on stdlib; validate via CLI tests
```

---
Following this draft, we'll start with M1 (minimal preloaded stdlib) and then expand modules step by step.