# lzscr CLI

Rust-based PoC CLI. Provides expression evaluation, static analysis, and Core IR dumps.

- Binary name: `lzscr-cli`

## Usage

- Evaluate (from a one-line program)
  - `lzscr-cli -e "(~add 1 2)"`
  - Strict effects mode: `-s` or `--strict-effects`
- Evaluate (from file)
  - `lzscr-cli --file path/to/prog.lzscr`
  - Note: file input is wrapped in parentheses internally to allow top-level let groups.
- Static analysis
  - `lzscr-cli -e "..." --analyze [--format json] [--dup-min-size N] [--dup-min-count M]`
- Core IR dump
  - Text: `lzscr-cli -e "..." --dump-coreir`
  - JSON: `lzscr-cli -e "..." --dump-coreir-json`
- Core IR evaluation
  - `lzscr-cli -e "..." --eval-coreir`
- LLVM IR dump (PoC)
  - `lzscr-cli -e "..." --dump-llvmir`
- Build native executable (PoC)
  - `lzscr-cli -e "..." --build-exe /path/to/out`
  - Requires an external toolchain: `clang` (recommended) or `llc` + `cc`.

Mode precedence: `--dump-coreir-json | --dump-coreir | --eval-coreir | --dump-llvmir | --build-exe` > `--analyze` > evaluation.

## Examples

- Output IR text for `(~seq () (!println "x"))`:
  - `lzscr-cli -e "(~seq () (!println \"x\"))" --dump-coreir`

- Evaluate do-notation example:
  - `lzscr-cli -e "!{ _ <- !println \"a\"; !println \"b\"; 1 + 2 }" -s`

- Use chain/bind directly:
  - `lzscr-cli -e "(~chain (!println \"a\") (~bind 1 (\\x -> x)))" -s`
- Get analysis in JSON:
  - `lzscr-cli -e "(\\x -> x) 1" --analyze --format json`

- Format source (pretty-print) without executing:
  - `lzscr-cli --file path/to/prog.lzscr --format-code [--fmt-indent 2] [--fmt-width 100]`

- Print type info as JSON (typecheck enabled by default):
  - `lzscr-cli -e "\\x -> x" --types json`

- Declare constructor arities for analysis/runtime:
  - `lzscr-cli -e "(.Foo 1 2)" --analyze --ctor-arity .Foo=2,.Bar=0`

## Module resolution (~require)

- `(~require .seg1 .seg2 ... .segN)` is resolved before execution; it reads `seg1/seg2/.../segN.lzscr` and expands it into the expression.
- All arguments must be constructor-like symbols in `.name` form. Missing files, parse/typecheck failures, and cyclic references are static errors.
- Search path precedence:
  1. Current directory
  2. `--stdlib-dir <PATH>`
  3. `--module-path <P1:..:PN>` (colon-separated)

### Related options
- `--stdlib-dir <PATH>`: directory to search for the standard library.
- `--module-path <P1:..:PN>`: additional search paths.
- `--no-stdlib`: disable preloading the stdlib prelude (by default, `stdlib/prelude.lzscr` is prepended).

## Builtin aliasing (~builtin)

- `(~builtin .a .b .c)` is recognized and rewritten to a reference named `builtin_a_b_c`.
- If no segments are provided, it becomes a reference named `builtin`.

## Typechecking

- Enabled by default before evaluation.
- Output mode: `--types pretty|json` (default: `pretty`).
- Disable typecheck: `--no-typecheck`.

## Analyzer options (summary)

- `--format text|json` (default: `text`).
- Duplicate detection thresholds: `--dup-min-size <N>` (default 3), `--dup-min-count <M>` (default 2).
- Skip duplicate detection: `--no-dup`.
- Trace analyzer timings/findings to stderr: `--analyze-trace`.
- Constructor arities: `--ctor-arity "Name=Arity,Other=0"` (comma-separated).

For details on analyzer output schema, see `docs/tools/analyzer.md`.

## Running source from stdin

Use `scripts/lzscr-run-stdin.sh` to evaluate scratch programs without manually
creating temporary files. The script copies stdin into a temporary `.lzscr`
file, runs `lzscr-cli --file` with the repo stdlib, forwards any additional
flags, and then removes the file (use `--keep-temp` to inspect it).

```
cat <<'LZ' | scripts/lzscr-run-stdin.sh -- --fmt-indent 2
(!println "hi")
LZ
```

Options:

- `-s|--stdlib-dir <path>`: override the stdlib search root (defaults to
  `stdlib/` relative to the repo).
- `-k|--keep-temp`: prevent cleanup so you can re-run the generated file.

`LZSCR_CLI_BIN=/path/to/lzscr-cli scripts/lzscr-run-stdin.sh ...` forces a
specific binary; otherwise the script prefers `target/debug/lzscr-cli` and
falls back to `cargo run -q -p lzscr-cli`.
