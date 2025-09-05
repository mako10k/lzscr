# lzscr (work-in-progress)

Minimal skeleton to parse a subset of the language:
- ints, strings, ~ref, bare symbols, lambdas, application, blocks.

Try:

```
cargo run -p lzscr-cli -- -e '\x -> (~print "hi") x'
```

This only pretty-prints the AST; evaluation and full grammar will come next.
