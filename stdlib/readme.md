# lzscr Standard Library (Work in Progress)

This directory hosts early, pure-LazyScript implementations of common functional helpers.
The prelude (`prelude.lzscr`) is automatically prepended (unless `--no-stdlib`) and exposes
core namespaces from the runtime `Builtins` plus foundational list/string/option/result utilities.

## Files

- `prelude.lzscr`: Core base + list/string/scan helpers. Now lazily loads `option.lzscr`, `result.lzscr`, and `list.lzscr` via `~require` and exposes backward-compatible deprecated aliases (e.g. `~is_some`, `~map_option`, `~map_result`).
- `option.lzscr`: Stand-alone Option helper module (functional ops).
- `result.lzscr`: Stand-alone Result helper module (map/and_then/or_else, etc.).
- `list.lzscr`: Expanded list algorithms (any/all/sum/product/zip/etc.).
- `lex.lzscr`: Lexer-oriented character + scanning helpers (used by tooling examples).

## Loading Additional Modules

Use the surface form `(~require .stdlib .option)` once module resolution roots include the project `stdlib` directory. Example CLI invocation:

```
cargo run -p lzscr-cli -- -e '( (~require .option) .is_some (.Some 1) )'
```

The CLI search order is:
1. Current directory
2. `--stdlib-dir` (default `./stdlib`)
3. Extra paths supplied via `--module-path` (colon-separated)

## Conventions

- All exported values are inside a final record expression at the bottom of each file.
- Internal helpers prefixed with `~` remain local; only record fields form the public API.
- Pattern-matching uses AltLambda (`|`) for simple branching; nested let/blocks are kept minimal for readability.

## Roadmap (Short Term)

- Add `either.lzscr` (higher-order combinators over Result)
- Add `string_extra.lzscr` (case-insensitive compare, trim functions)
- Add `parse.lzscr` (small parser combinator core) building on `lex.lzscr`
- Add property-based golden tests for equivalence between prelude-provided Option helpers and module versions.
- Performance pass: replace naive recursion with tail-call-friendly constructs once optimizer/IR exists.

## Testing Ideas

Until a dedicated test harness is wired, quick manual checks:

```
# Option
cargo run -p lzscr-cli -- -e '( (~require .option) .map (\~x -> (~x + 1)) (.Some 41) )'

# Result
cargo run -p lzscr-cli -- -e '( (~require .result) .map (\~x -> (~x * 2)) (.Ok 5) )'

# List zip
cargo run -p lzscr-cli -- -e '( (~require .list) .zip [1,2,3] [4,5] )'
```

## Notes

This is an early iteration; APIs may evolve. Keep modules small and orthogonal. Avoid premature optimization; favor clarity while language semantics are still shifting.
