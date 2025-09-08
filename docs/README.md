# lzscr documentation index (2025-09-05)

This folder contains documentation for the lzscr PoC: language specs and tools, for both users and contributors.

- Specs (/spec)
  - language.md: integrated spec (lexing/parsing/semantics/file format/key APIs)
  - overview.md: language overview and current state
  - syntax.md: tokens/grammar, sugar, and examples (current minimal subset)
  - semantics.md: evaluation rules, values, built-ins, strict-effects behavior
  - ast.md: AST structures (types/fields and examples)
  - ir.md: Core IR (types/terms/modules, AST→IR lowering, text/binary plans)
- Tools (/tools)
  - cli.md: CLI usage and key options
  - analyzer.md: static analysis (duplicates/unbound/shadowed/unused) and JSON output

Notes:
- docs/lzscr.md is an early design note (includes future ideas). For the up-to-date, implementation-conformant spec, see this /spec set instead.
