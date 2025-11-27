# Parser let-group regression (2025-11-27)

## Summary
Nested let-groups that mix real bindings with semicolon-terminated effect statements were being downgraded to plain tuples. After the parser finished parsing the body of a parenthesized group it would only continue collecting trailing items if it could see another `=` sign. As soon as the next form was an expression like `~helper 1;` (no `=`), the tuple fallback triggered and we surfaced `expected , or )`.

## Fix
When at least one binding/type declaration has already been parsed in the current parenthesized block, the trailing section now treats bare expressions as wildcard bindings. This keeps the execution order identical while allowing `;`-separated statements to stay inside the `LetGroup`. The change lives in `crates/lzscr-parser/src/lib.rs` inside the `Tok::LParen` arm.

## Reproducing the original failure
From the workspace root:

```
cargo run -p lzscr-cli -- --no-stdlib -f tests/min_repro.lzscr
cargo run -p lzscr-cli -- --no-stdlib -f tests/hyp_nested.lzscr
```

Before the fix both commands stopped at `expected , or )`. After the fix they evaluate successfully and print `%{Int} 1`.

## Regression coverage
`cargo test -p lzscr-parser --lib tests::letgroup_trailing_effect_statements`
`cargo test -p lzscr-cli --test cli regression_hyp_nested_executes`
`cargo test -p lzscr-cli --test cli regression_min_repro_executes`
`cargo test -p lzscr-cli --test cli prelude_basic_smoke`

The last test also exercises the stdlib/prelude glue via `tests/prelude_basic.lzscr`, ensuring the CLI path that wraps files continues to work.
