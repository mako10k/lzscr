# lzscr Standard Library (Work in Progress)

This directory hosts early, pure-LazyScript implementations of common functional helpers.
The prelude (`prelude.lzscr`) is automatically prepended (unless `--no-stdlib`) and exposes
core namespaces from the runtime `Builtins` plus foundational list/string/option/result utilities.

## Files

- `prelude.lzscr`: Core base + list/string/scan helpers. Now lazily loads the pure submodules and exposes backward-compatible deprecated aliases (e.g. `~is_some`, `~map_option`, `~map_result`).
- `pure/option.lzscr`: Stand-alone Option helper module (functional ops).
- `pure/result.lzscr`: Stand-alone Result helper module (map/and_then/or_else, etc.).
- `pure/list.lzscr`: Expanded list algorithms (any/all/sum/product/zip/etc.).
- `pure/lex.lzscr`: Lexer-oriented character + scanning helpers (used by tooling examples).
- `effect/io.lzscr`: Wrappers over builtin `!print`/`!println` (logging helpers, gated behind `--stdlib-mode=allow-effects`).
- `effect/log.lzscr`: Structured logging helpers layered on top of `effect/io`, including tap utilities, field builders, and `key=value` / JSON field emitters.
- `effect/fs.lzscr`: Filesystem helpers that wrap runtime `!fs.*` effects and expose ergonomic `Result`-first APIs (e.g. `read_text_*`, `write_text_*`, `append_text_*`, `list_dir_*`, `remove_file_*`).

## Purity Classification (initial pass)

| Module | Status | Notes | Follow-up |
| --- | --- | --- | --- |
| `prelude.lzscr` | Mixed / compat | Re-exports helpers and still bundles deprecated aliases; mixes pure namespaces with effectful entry-points. | Split into thin pure prelude + compat shim once purity enforcement lands. |
| `pure/option.lzscr` | Pure | Functional combinators over `Option`; no IO or mutation. | Monitor for dependency on effect modules as new features land. |
| `pure/result.lzscr` | Pure | Mirrors `option` style—map/and_then/or_else without side effects. | Same as above. |
| `pure/list.lzscr` | Pure | Collection helpers over in-memory lists only. | Keep dependency graph flowing into effect tree only (if ever needed). |
| `pure/lex.lzscr` | Pure (tooling) | Helper predicates for characters; current usage is deterministic and effect-free. | Re-evaluate classification if lexer helpers start performing IO. |
| `effect/io.lzscr` | Effect | Thin wrappers around builtin `!print`/`!println` plus logging helpers. | Layer additional effect modules on top (fs/net) once IO core is stable. |
| `effect/log.lzscr` | Effect | Level-tagged logging helpers with tap utilities, field builders, scoped field combinators, and `key=value` / JSON emitters; depends on `effect/io`. | Extend with richer structured emitters (JSON, spans) once runtime protocols exist. |
| `effect/fs.lzscr` | Effect | Filesystem helpers wrapping `!fs.read_text` / `!fs.write_text` / `!fs.append_text` / `!fs.list_dir` / `!fs.remove_file`, returning `Result` interfaces so callers decide when to surface errors or fallbacks. | Next: expose directory metadata and create/remove dir effects when runtime hooks land. |

Run `python scripts/check_stdlib_classification.py` to ensure every `.lzscr` file listed under `stdlib/` has an up-to-date entry in this table before sending a PR.

## Loading Additional Modules

Use the surface form `(~require .pure .option)` when running inside this repository (the CLI already adds `./stdlib` to the search path). End-users can always spell the fully-qualified path `(~require .stdlib .pure .option)` if they want to be explicit. Example CLI invocation:

```
cargo run -p lzscr-cli -- -e '( (~require .pure .option) .is_some (.Some 1) )'
```

> Effect namespaces (e.g. `(~require .effect ...)`) are gated. Run tooling with `--stdlib-mode=allow-effects` to opt in; the default `pure` mode rejects those imports to keep builds deterministic by default.

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
cargo run -p lzscr-cli -- -e '( (~require .pure .option) .map (\~x -> (~x + 1)) (.Some 41) )'

# Result
cargo run -p lzscr-cli -- -e '( (~require .pure .result) .map (\~x -> (~x * 2)) (.Ok 5) )'

# List zip
cargo run -p lzscr-cli -- -e '( (~require .pure .list) .zip [1,2,3] [4,5] )'
```

## Notes

This is an early iteration; APIs may evolve. Keep modules small and orthogonal. Avoid premature optimization; favor clarity while language semantics are still shifting.
