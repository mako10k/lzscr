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

Other docs (maintainers/contributors):
- coding-standards.md: coding standards, refactoring guidelines
- coreir-guide.md: CoreIR developer guide
- type-system.md: implemented type system snapshot
- rca_instructions.md: RCA tool workflow (for repo incidents)

Project policies and contributor guides live at the repo root:
- ../AGENT.md
- ../CONTRIBUTING.md
- ../SECURITY.md

Notes:
- Roadmap/source of truth for prioritization: `ROADMAP.md`.
- parser-letgroup-regression.md captures the November 2025 parser RCA (cause, fix, repro commands).
