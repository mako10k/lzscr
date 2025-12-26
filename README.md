# lzscr (work in progress)

An experimental re-implementation skeleton of LazyScript with a minimal, pragmatic subset.

Roadmap/source of truth: see `docs/ROADMAP.md`. If README and ROADMAP conflict, prefer ROADMAP.
- Values and core syntax: Int/Float/Bool/Str, `~ref`, bare symbols (constructors), lambdas, application, blocks.
- Sugar: tuples `(a,b,...)`, records `{k:v,...}`.
- Infix operators: `+ - * /` (Int), `.+ .- .* ./` (Float), comparisons `< <= > >=` and `== !=` (Float comparisons use `. < .<= .> .>=`).
- Simple runtime evaluator with selected built-ins: `to_str`, `add/sub/mul/div`, `fadd/fsub/fmul/fdiv`, `lt/le/gt/ge`, `flt/fle/fgt/fge`, `eq/ne`, `and/or/not`, `if`, `seq`, `effects.print/println`, `Tuple/Record/KV`, `Bool`.

Try it:

```
cargo run -p lzscr-cli -- -e '1 + 2 * 3'
# => 7

cargo run -p lzscr-cli -- -e '1 == 1'
# => true

cargo run -p lzscr-cli -- -e '1 .+ 2.5'
# => 3.5

cargo run -p lzscr-cli -- -e '(~println "hi")'
# prints: hi\n, final value: ()

cargo run -p lzscr-cli -- -e '\x -> x'
# => <fun>

cargo run -p lzscr-cli -- -e '(1, 2, 3)'
# => (1, 2, 3)

cargo run -p lzscr-cli -- -e '{a: 1, b: 2}'
# => equivalent to {"a": 1, "b": 2} (rendering order may vary)
```

## Quick Start (CLI Examples)

```bash
# Simple evaluation
cargo run -p lzscr-cli -- -e '1 + 2 * 3'

# Type-checked evaluation
cargo run -p lzscr-cli -- -e '(~x -> ~x) 1' --types pretty

# Floating-point arithmetic
cargo run -p lzscr-cli -- -e '1 .+ 2.5'

# Effects (stdout)
cargo run -p lzscr-cli -- -e '(!println "hello")'

# Lambda
cargo run -p lzscr-cli -- -e '\x -> x'

# Tuples / Records
cargo run -p lzscr-cli -- -e '(1, 2, 3)'
cargo run -p lzscr-cli -- -e '{a: 1, b: 2}'
```

## File Execution

File extension is `.lzscr`. The CLI wraps the entire file in parentheses and treats top-level `~name = ...;` as let-group bindings.

```bash
echo '~x = 1; ~add ~x 2;' > /tmp/a.lzscr
cargo run -p lzscr-cli -- --file /tmp/a.lzscr
# => 3
```

## Installing the Binary

```bash
cargo install --path crates/lzscr-cli
```

After installation, verify with `lzscr-cli --help` or `lzscr-cli --version`.

## Type Inference and Runtime Checks

- Hindley–Milner style type inference runs before execution and halts on errors (skip with `--no-typecheck` if needed).
- Constructor arity is configurable via `--ctor-arity` for over-application detection.

## Effect Management (strict-effects)

- By default, side effects are permitted anywhere.
- Enable `--strict-effects` to restrict side effects to controlled forms (`~seq`, `~chain`, `~bind`, etc.).
- Do-notation sugar (`!{ ... }`) expands to `~chain`/`~bind` internally.

## Static Analysis and Golden Tests

- Use `--analyze` for AST-level analysis (duplicate detection, arity checks, etc.).
- Tokenizer golden tests use `goldens/*.golden`:

```bash
cargo test -p lzscr-cli --tests tokenize_golden
```

## Formatting (formatter / pretty-printer)

- Experimental AST-based formatter included. May emit expanded intermediate forms.
- Examples:

```bash
# Format one-liner
cargo run -p lzscr-cli -- -e '1 + 2 * 3' --format-code

# Format file in-place
cargo run -p lzscr-cli -- --file /path/to/code.lzscr --format-code --fmt-indent 4 --fmt-width 120
```

## VS Code Extension (extensions/lzscr-vscode)

- Includes syntax highlighting and formatter integration scaffolding. Build and install as VSIX:

```bash
cd extensions/lzscr-vscode
npm ci
npm run package
# Outputs lzscr-vscode-*.vsix
```

Configuration: `lzscr.formatterPath` (default: `lzscr-cli`), `lzscr.indent`, `lzscr.maxWidth`.

## Contributing and Development Workflow

See [CONTRIBUTING.md](CONTRIBUTING.md) and [docs/coding-standards.md](docs/coding-standards.md) for detailed guidelines. Quick start:

```bash
# Clone and build
git clone https://github.com/mako10k/lzscr.git
cd lzscr
cargo build

# Pre-commit checks
cargo fmt --all --check
cargo clippy --all-targets -- -D warnings
cargo test
./scripts/check-file-sizes.sh
```

## Additional Resources

- CI runs formatting, linting, tests, and coverage.
- Design details and language spec available in `docs/` (language spec in `docs/spec`).

## Contributing

