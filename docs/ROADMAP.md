# LazyScript (lzscr) Roadmap

Last updated: 2025-09-22

This document is the single source of truth for the roadmap. Inline code comments and README excerpts must follow this document. If there is any discrepancy, this document wins.

## Current scope and assumptions
- Implemented areas: lexer/parser/AST/runtime (selected built-ins)/type inference (HM rank-1)/Core IR lowering
- Stability:
  - Stable: parser, basic evaluation, core type inference skeleton, CLI execution/dumps
  - Experimental: Core IR evaluation, dual-caret diagnostics, `--strict-effects`
- Compatibility policy: during 0.x, internal APIs may change. CLI flags may change with prior notice.

## Short-term priorities
- Diagnostics (ongoing)
  - Dual-caret: standardize two-point highlighting (expected vs actual, cause vs site)
  - Records: precise spans for field names and value positions
  - Occurs: track origin spans of generated type variables; normalize display (`%a`, `%b`, ...)
  - Message style: English, concise; add fix-it hints where applicable
- Type system
  - Clarify when to apply `zonk` (finalization) vs lightweight `apply`; stabilize final outputs
  - De-duplicate unify logic via safe helper extraction (no behavior changes)
- Core IR
  - Harden lowering (fix the representation of exceptions/alt/catch)
  - Improve small-case evaluator correctness (PoC)

## Mid-term
- Type and IR integration for exceptions/alternative lambdas (AltLambda/Catch)
- Name resolution and upvalue analysis (expand static analysis)
- Effect typing plan (integrate strict-effects policy into types)

## Long-term
- ADT declarations and constructor arity checks; explore kinds/typeclasses extensions
- Formatter stabilization and minimal VS Code extension release
- Runtime performance work (future LLVM/optimization remains research)

## Display/diagnostics policy
- Prefer dual spans (cause and effect) when available
- Keep message text concise; show line/column via caret headers
- Heuristically shift ambiguous 1-char spans (like leading `{`) inward to the meaningful token
