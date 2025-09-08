# lzscr CLI

Rust-based PoC CLI. Provides expression evaluation, static analysis, and Core IR dumps.

- Binary name: `lzscr-cli`

## Usage

- Evaluate
  - `lzscr-cli -e "(~add 1 2)"`
  - Strict effects mode: `-s` or `--strict-effects`
- Static analysis
  - `lzscr-cli -e "..." --analyze [--format json] [--dup-min-size N] [--dup-min-count M]`
- Core IR dump
  - Text: `lzscr-cli -e "..." --dump-coreir`
  - JSON: `lzscr-cli -e "..." --dump-coreir-json`

Precedence: `--dump-coreir(-json)` > `--analyze` > evaluation.

## Examples

- Output IR text for `(~seq () (!println "x"))`:
  - `lzscr-cli -e "(~seq () (!println \"x\"))" --dump-coreir`

- Evaluate do-notation example:
  - `lzscr-cli -e "!{ _ <- !println \"a\"; !println \"b\"; 1 + 2 }" -s`

- Use chain/bind directly:
  - `lzscr-cli -e "(~chain (!println \"a\") (~bind 1 (\\x -> x)))" -s`
- Get analysis in JSON:
  - `lzscr-cli -e "(\\x -> x) 1" --analyze --format json`

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
