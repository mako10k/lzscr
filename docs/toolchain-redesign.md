# lzscr Toolchain Milestones

**Date**: 2025-12-02  
**Scope**: End-to-end tooling for the lzscr compiler/runtime pipeline (scripts → compile → CoreIR → opt → exec → LLVM IR), including formatting and linting.

## Goals
- Single, repeatable flow for: script immediate execution, compilation, CoreIR transform/dump, CoreIR execution, optimization passes, formatting, linting, and LLVM IR emission.
- Shared commands for local dev and CI (no drift); artifacts stored predictably.
- Fast defaults; heavy analyses (coverage/udeps) remain opt-in.

## Milestones
- **M1: Command Surface + Docs**
  - Define canonical commands (CLI/xtask) for each stage: `run`, `build`, `coreir-dump`, `coreir-run`, `coreir-opt`, `fmt`, `lint`, `llvmir`.
  - Document the flow and artifacts (paths, formats) in `docs/tools/`.
  - Add quickstart examples for scripts and files.
- **M2: Core Pipeline Wiring**
  - Implement CLI or `xtask` wrappers that call existing crates with consistent flags:
    - Script immediate exec: `cargo run -p lzscr-cli -- -e '<expr>'`.
    - Compile to binary (debug/release) with explicit targets.
    - CoreIR transform/dump: `--dump-coreir` + JSON variant.
    - CoreIR execution hook (if evaluator exists) or placeholder that fails with TODO.
    - CoreIR optimization pass entry (stub allowed initially).
    - LLVM IR emission stub (`--dump-llvmir`), gated until backend lands.
  - Normalize exit codes; ensure all commands are non-interactive and cache-friendly.
- **M3: Linting & Formatting**
  - Rust: `cargo fmt`, `cargo clippy --all-targets --all-features -- -D warnings`.
  - lzscr source: formatter wiring (`--format-code`), plus file glob support.
  - Add style gates to CI using the same commands.
- **M4: CoreIR/LLVM Artifacts & Tests**
  - Golden tests for CoreIR dumps; store under `goldens/coreir/*.golden`.
  - Smoke tests for CoreIR execution (once implemented).
  - Snapshot tests for LLVM IR output (behind feature flag).
  - Introduce `--update-golden` path for all snapshot tests.
- **M5: Optimization + Bench Hooks**
  - Add CoreIR optimization passes toggles (`--coreir-opt level=N`).
  - Micro-bench harness for runtime and CoreIR evaluator; optional in CI, documented for perf runs.
- **M6: CI Integration**
  - CI jobs call the same commands (`xtask` or direct CLI): fmt/lint/test/coreir-golden/coreir-run (once ready)/llvmir-snap (feature-gated).
  - Upload artifacts: `cobertura.xml` (coverage), `goldens` diffs on failure, `coreir/llvmir` snapshots on main.
  - Nightly: `cargo +nightly udeps`, `cargo audit`, optional `tarpaulin`.
- **M7: Release Track**
  - Release bundles include: CLI binaries per target, stdlib bundle, CoreIR/LLVM IR samples, VSIX.
  - Add checksum/signing step and a publish guide.

## Command Matrix (target)
- `run`: Evaluate expression/file (strict-effects toggle).
- `build`: Compile CLI + runtime (debug/release; target support).
- `coreir-dump`: Parse → CoreIR lowering → emit text/JSON.
- `coreir-run`: Execute CoreIR (no lowering) – stub until executor lands.
- `coreir-opt`: Apply optimization passes and emit optimized CoreIR.
- `llvmir`: Emit LLVM IR (stub until backend lands).
- `fmt`: Rust fmt + lzscr formatter over specified files.
- `lint`: clippy + deny (optionally pedantic).
- `test`: nextest (or `cargo test`) + golden tests + size checks.

## Binary Command Naming (proposed)
- Binary prefix: `lzscr-<verb>` for standalone tools; keep `lzscr` as the CLI front door.
- Recommended commands:
  - `lzscr` (existing): top-level evaluator/driver; `lzscr run`, `lzscr coreir-dump`, etc. remain as subcommands.
  - `lzscr-coreir-dump`: lowered CoreIR emitter (text/JSON) for scripting/CI.
  - `lzscr-coreir-run`: CoreIR executor (when available).
  - `lzscr-coreir-opt`: CoreIR optimizer (passes configurable via flags).
  - `lzscr-llvmir`: LLVM IR emitter (feature-gated while backend is experimental).
  - `lzscr-fmt`: formatter for .lzscr files (batch + stdin).
- Subcommand aliases inside `lzscr` should mirror the standalone names (`lzscr coreir-dump` → `lzscr-coreir-dump` behavior) to avoid drift.
- Installation layout:
  - Cargo install: all binaries emitted via one crate or `xtask install` that copies/symlinks names into `$CARGO_HOME/bin`.
  - VS Code extension: calls into `lzscr` subcommands; avoid depending on the standalone binaries to reduce surface area.
- Flag style:
  - Use long-form kebab flags (`--strict-effects`, `--dump-json`); short flags only for common ops (`-e`, `-f`).
  - Prefer `--input <file>` / `--output <file>` over positional when ambiguity exists.
  - Common `--format {text,json}` where dual-output is supported (CoreIR/LLVM).

## Open Items
- Confirm naming/location: `cargo xtask <cmd>` vs direct CLI flags.
- Decide when CoreIR executor/optimizer is expected to land; stub behavior acceptable until then.
- Determine default feature gating for LLVM IR emission to keep stable builds green.
