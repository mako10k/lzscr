# ctor-bare-id Implementation Plan

## Context
- Branch: `ctor-bare-id`
- Objective: align language surface syntax, runtime, stdlib, tooling, and docs so constructors are always bare identifiers and symbols remain non-applicable atoms.
- Baseline: runtime + docs already updated and committed ("Enforce bare constructor semantics"). Parser, stdlib, tooling, and tests still expect dotted constructors.

### Decision (2025-12-04): Named vs Type ctors
- Bare identifiers are **Named ctors**. They carry only a runtime tag and arity information—no implicit type binding.
- Every `%{...}` expression is a **Type ctor**. `%{Ty}` lives in the `%{.Type}` universe, and `%{Ty} expr` is purely a unification check between the inferred type of `expr` and `Ty`.
- `%{.Type}` acts as the canonical “type of types” until a richer `%{Expr}` lifting story exists. Future work may generalize annotations to `%{Expr}`, but that is explicitly deferred.
- Symbols stay unapplied atoms and are never promoted into either ctor class.

## Milestones
1. **Parser emits bare constructors**
   - Update `crates/lzscr-parser` so every constructor literal (including tuples) becomes a `Ctor` AST node with bare identifier names.
   - Remove fallback paths that auto-prefix `.` or treat `.Foo` as constructor aliases.
   - Ensure tuple sugar keeps using internal tuple ctor names (e.g., `.,,`). [← Rejected]
     - Application not allowed for any symbol, including tuple ctor names.
     - Tuple ctor treat as bare Named ctors (e.g., `,`).  
2. **Runtime + tests harden behavior**
   - Add parser/runtime regression tests verifying: applying any symbol errors, tuple ctors remain callable, constructor arity errors surface.
   - Target suites: `crates/lzscr-runtime/tests`, `crates/lzscr-parser/tests`, plus high-level programs in `tests/`.
3. **Stdlib migration**
   - Sweep `stdlib/*.lzscr` for dotted constructor usage (e.g., `.Some`, `.True`) and convert to bare ctors or helper lambdas.
   - Provide shims only where necessary to avoid breaking existing call sites.
4. **Developer tooling updates**
   - Lexer/tokenizer: ensure `..` token exists where needed and docs stay synchronized.
   - VS Code extension: syntax highlighting and snippets differentiate constructors vs symbols; no dotted ctor suggestions.
5. **CI + packaging**
   - Run `cargo fmt --check`, `cargo clippy`, `cargo test` for workspace.
   - Build VSIX via `npm run package` inside `extensions/lzscr-vscode` (task provided).

## Work Breakdown
| Step | Area | Key Tasks | Notes |
| --- | --- | --- | --- |
| P1 | Parser | Adjust grammar/AST builders, update tuple handling, add golden tests | coordinate with `docs/spec/syntax.md` |
| P2 | Runtime | Add targeted unit tests + error wording assertions | confirm `NotApplicable` messages |
| P3 | Stdlib | Replace dotted ctor usage, ensure modules compile | consider helper aliases where ergonomic |
| P4 | Tooling | Lexer token updates, VSIX grammar/snippets | re-run `npm run package` |
| P5 | CI | fmt/clippy/test + VSIX artifact | capture results in PR description |

## Testing Strategy
- Parser golden files and existing integration tests for tuple/ctor syntax.
- Runtime unit tests covering symbol application failure, ctor arity mismatches, tuple constructors.
- Stdlib smoke tests via `tests/` programs and targeted CLI runs.
- VSIX grammar snapshots (if available) plus manual syntax check in editor.

## Risks / Mitigations
- **Parser regressions**: rely on golden tests and AST dumps.
- **Stdlib churn**: stage changes file-by-file to keep diffs reviewable.
- **Tooling drift**: regenerate VSIX grammar artifacts after lexer/token changes.
- **CI duration**: leverage existing VS Code tasks for fmt/clippy/test and VSIX packaging.

## Next Action
Start with milestone P1 (parser) now that the plan is recorded. Document follow-up decisions back into this file as milestones progress.
