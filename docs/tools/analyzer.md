# lzscr Analyzer

A simple AST-based static analyzer. It reports duplicate code, unbound references, shadowing, unused parameters, unused let bindings, and duplicate bindings within a let group.

- Implementation: `crates/lzscr-analyzer`
- CLI: `lzscr-cli -e "..." --analyze [--format json] [--dup-min-size N] [--dup-min-count M]`

## Output

- text: one finding per line
- json: schema (brief)
  - `duplicates: [{ size, count, span:{offset,len}, repr }]`
  - `unbound_refs: [{ name, span }]`
  - `shadowing: [{ name, lambda_span }]`
  - `unused_params: [{ name, lambda_span }]`
  - `unused_let: [{ name, binding_span }]`
  - `let_collisions: [{ name, group_span }]`

## Notes

- Default allowlist: `default_allowlist()`
- Minimum size and occurrence thresholds are configurable; current defaults are simple PoC values.
