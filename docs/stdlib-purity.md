# Standard Library Purity Goals

## Background
The current `stdlib` mixes pure utilities and effectful APIs in the same layer, making it hard
to tell which symbols can perform side effects just by reading the code. Clearly separating the
pure and effectful worlds—visually and at the type level—is critical for safe refactors and
optimizations.

## Core Goals
1. **Recognize purity from naming alone**  
   - Pure bindings stick to the `~foo` style.  
   - Effectful bindings always begin with `!foo`, keeping the `!` visible to every caller.
2. **Physically split modules by behavior**  
   - Place every pure module under `stdlib/pure/`.  
   - Place effectful modules (the ones exporting `!` names) under `stdlib/effect/`.  
   - The default prelude re-exports only pure modules; effectful modules are opt-in via explicit `~require` calls.
3. **Encode the boundary in types**  
   - Pure APIs return plain values (or `Pure a`).  
   - Effectful APIs return `Effect r a` (working name) so pure code cannot run them directly.  
   - The CLI / REPL interprets `Effect` values to actually interact with the outside world.
4. **Enforce via load paths and flags**  
   - The CLI exposes `--stdlib-mode={pure,allow-effects}` (default `pure`) to gate access to effect namespaces.  
   - In `pure` mode, importing from `.effect` fails immediately with a hint to rerun using `--stdlib-mode=allow-effects`.
5. **Guard the rules with CI / lint**  
   - Fail the build if `pure` modules export `!` names or return `Effect`.  
   - Fail if `effect` modules export public symbols without the `!` prefix.  
   - Keep the dependency graph one-way: `pure -> effect` is allowed, the reverse is rejected.
6. **Provide a compatibility layer for gradual adoption**  
   - Move legacy prelude aliases into `stdlib/compat/` with deprecation warnings.  
   - Instrument compat calls (logging or lint) so we can track and eventually remove them.

## Next Steps
- After reviewing these goals, draft the migration plan (work breakdown, tooling, CI enforcement).
- Keep the `stdlib/readme.md` classification table current and fail CI when a stdlib module lacks a declared purity level. The helper script `scripts/check_stdlib_classification.py` enforces the coverage locally—add it to CI once the split begins.
- Keep the `stdlib/readme.md` classification table current and fail CI when a stdlib module lacks a declared purity level. The helper script `scripts/check_stdlib_classification.py` enforces the coverage locally (run via `python scripts/check_stdlib_classification.py` or `cargo test -p lzscr-cli --test stdlib_classification`).
- Stand up the `stdlib/effect/` tree (starting with IO wrappers) and migrate remaining effect helpers there (e.g. the new `effect/log` module built atop IO, now with tap, field/JSON logging, and scoped-field combinators so pure call sites can observe values without re-threading logging code).
- Keep extending the `effect/fs` namespace that wraps the runtime filesystem effects (`!fs.read_text`, `!fs.write_text`, `!fs.append_text`, `!fs.list_dir`, `!fs.remove_file`, etc.) so every helper funnels results through `Result`; next focus areas are directory metadata/create/remove-dir helpers once runtime hooks land.
