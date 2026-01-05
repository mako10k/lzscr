# lzscr Standard Library (Work in Progress)

This directory hosts early, pure-LazyScript implementations of common functional helpers.
The prelude (`prelude.lzscr`) is automatically prepended (unless `--no-stdlib`) and exposes
core namespaces from the runtime `Builtins` plus foundational list/string/option/result utilities.

## Files

- `prelude.lzscr`: Core base + list/string/scan helpers. It now focuses on pure helpers and delegates deprecated aliases to the compat shim.
- `compat/prelude_aliases.lzscr`: Effectful shim that re-exports the legacy prelude helper names (`~is_some`, `~map_option`, etc.) and logs warnings whenever they are used.
- `pure/option.lzscr`: Stand-alone Option helper module (functional ops).
- `pure/result.lzscr`: Stand-alone Result helper module (map/and_then/or_else, etc.).
- `pure/list.lzscr`: Expanded list algorithms (any/all/sum/product/zip/etc.).
- `pure/lex.lzscr`: Lexer-oriented character + scanning helpers (used by tooling examples).
- `effect/io.lzscr`: Wrappers over builtin `!print`/`!println` (logging helpers, gated behind `--stdlib-mode=allow-effects`).
- `effect/log.lzscr`: Structured logging helpers layered on top of `effect/io`, including tap utilities, field builders, and `key=value` / JSON field emitters.
- `effect/fs.lzscr`: Filesystem helpers that wrap runtime `!fs.*` effects and expose ergonomic `Result`-first APIs (e.g. `read_text_*`, `write_text_*`, `append_text_*`, `list_dir_*`, `remove_file_*`, `create_dir_*`, `metadata_*`).

## Purity Classification (initial pass)

| Module                         | Status                  | Notes                                                                                                                                                                                                                                                                                                                                     | Follow-up                                                                                                  |
| ------------------------------ | ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| `prelude.lzscr`                | Mixed / compat          | Re-exports helpers and still bundles deprecated aliases; mixes pure namespaces with effectful entry-points.                                                                                                                                                                                                                               | Split into thin pure prelude + compat shim once purity enforcement lands.                                  |
| `core/bool.lzscr`              | Pure                    | Boolean combinators (`not`/`and`/`or`/`if`) implemented via pattern matches only.                                                                                                                                                                                                                                                         | Sync with language-level bool semantics whenever tagged unions change.                                     |
| `core/list.lzscr`              | Pure                    | Deterministic folds, zips, and builders over in-memory lists.                                                                                                                                                                                                                                                                             | Revisit for tail-call or iterator fusion once optimizer work begins.                                       |
| `core/option.lzscr`            | Pure                    | Basic Option predicates and combinators; used by both prelude and compat shim.                                                                                                                                                                                                                                                            | None yet.                                                                                                  |
| `core/result.lzscr`            | Pure                    | Result combinators plus Option interop, effect-free.                                                                                                                                                                                                                                                                                      | None yet.                                                                                                  |
| `core/primitives.lzscr`        | Pure                    | Identity/compose/flip/apply helpers for higher-order patterns.                                                                                                                                                                                                                                                                            | None yet.                                                                                                  |
| `core/string.lzscr`            | Pure (runtime builtins) | Wraps deterministic string/char/scan builtins; no IO or mutation.                                                                                                                                                                                                                                                                         | Audit if runtime string APIs gain side effects.                                                            |
| `pure/option.lzscr`            | Pure                    | Functional combinators over `Option`; no IO or mutation.                                                                                                                                                                                                                                                                                  | Monitor for dependency on effect modules as new features land.                                             |
| `pure/result.lzscr`            | Pure                    | Mirrors `option` style—map/and_then/or_else without side effects.                                                                                                                                                                                                                                                                         | Same as above.                                                                                             |
| `pure/list.lzscr`              | Pure                    | Collection helpers over in-memory lists only.                                                                                                                                                                                                                                                                                             | Keep dependency graph flowing into effect tree only (if ever needed).                                      |
| `pure/lex.lzscr`               | Pure (tooling)          | Helper predicates for characters; current usage is deterministic and effect-free.                                                                                                                                                                                                                                                         | Re-evaluate classification if lexer helpers start performing IO.                                           |
| `pure/json.lzscr`              | Pure                    | Minimal JSON AST + parser/printer (Int-only numbers; limited escapes).                                                                                                                                                                                                                                                                    | Expand number/string escape coverage as needed.                                                            |
| `pure/jsonrpc.lzscr`           | Pure                    | JSON-RPC 2.0 helpers built atop `pure/json` (message builders + classifier).                                                                                                                                                                                                                                                              | Add stricter validation and streaming framing once IO/bytes APIs exist.                                    |
| `pure/data/map.lzscr`          | Pure                    | Persistent BST-backed map structure; operates only on in-memory data.                                                                                                                                                                                                                                                                     | Consider adding balancing/rotation utilities once performance becomes an issue.                            |
| `pure/data/set.lzscr`          | Pure                    | BST-backed set utilities layered on `tree.lzscr`.                                                                                                                                                                                                                                                                                         | Same balancing story as `pure/data/map.lzscr`.                                                             |
| `pure/data/tree.lzscr`         | Pure                    | Fundamental tree operations (insert/find/fold) with no side effects.                                                                                                                                                                                                                                                                      | Potential future: add self-balancing variants.                                                             |
| `pure/functional/iter.lzscr`   | Pure                    | Lazy/infinite iterator combinators constructed via pure thunks.                                                                                                                                                                                                                                                                           | Add safeguards when we gain lazy destructors or resource tracking.                                         |
| `pure/math/basic.lzscr`        | Pure                    | Deterministic arithmetic helpers (`abs`/`clamp`/`pow`/`gcd`/`lcm`).                                                                                                                                                                                                                                                                       | Expand alongside richer math namespace once numeric tower stabilizes.                                      |
| `effect/io.lzscr`              | Effect                  | Thin wrappers around builtin `!print`/`!println` plus logging helpers.                                                                                                                                                                                                                                                                    | Layer additional effect modules on top (fs/net) once IO core is stable.                                    |
| `effect/log.lzscr`             | Effect                  | Level-tagged logging helpers with tap utilities, field builders, scoped field combinators, and `key=value` / JSON emitters; depends on `effect/io`.                                                                                                                                                                                       | Extend with richer structured emitters (JSON, spans) once runtime protocols exist.                         |
| `effect/fs.lzscr`              | Effect                  | Filesystem helpers wrapping `!fs.read_text` / `!fs.write_text` / `!fs.append_text` / `!fs.list_dir` / `!fs.remove_file` / `!fs.create_dir` / `!fs.metadata`, returning `Result` interfaces so callers decide exactly how to surface errors. Metadata now reports `size`, `is_dir`, `is_file`, `readonly`, and `modified_ms` (Option Int). | Future: extend metadata further once platform-specific signals (e.g. perms bits) are consistently exposed. |
| `compat/prelude_aliases.lzscr` | Effect                  | Deprecated helper aliases that log warnings and delegate to `pure/option.lzscr` / `pure/result.lzscr`.                                                                                                                                                                                                                                    | Eventually drop entirely once downstream crates migrate.                                                   |
| `pure/tuple.lzscr`             | Pure                    | Minimal 2-tuple helpers: constructor `tuple2` and accessors `fst` / `snd` implemented as record-based pairs.                                                                                                                                                                                                                              | None                                                                                                       |

Run `python scripts/check_stdlib_classification.py` to ensure every `.lzscr` file listed under `stdlib/` has an up-to-date entry in this table before sending a PR.

## Loading Additional Modules

Use the surface form `(~require .pure .option)` when running inside this repository (the CLI already adds `./stdlib` to the search path). End-users can always spell the fully-qualified path `(~require .stdlib .pure .option)` if they want to be explicit. Example CLI invocation:

```
cargo run -p lzscr-cli -- -e '( (~require .pure .option) .is_some (Some 1) )'
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
cargo run -p lzscr-cli -- -e '( (~require .pure .option) .map (\~x -> (~x + 1)) (Some 41) )'

# Result
cargo run -p lzscr-cli -- -e '( (~require .pure .result) .map (\~x -> (~x * 2)) (Ok 5) )'

# List zip
cargo run -p lzscr-cli -- -e '( (~require .pure .list) .zip [1,2,3] [4,5] )'
```

## Notes

This is an early iteration; APIs may evolve. Keep modules small and orthogonal. Avoid premature optimization; favor clarity while language semantics are still shifting.
