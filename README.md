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

# Booleans use constructor form: True / False
cargo run -p lzscr-cli -- -e 'if True 10 20'
# => 10

## File extension / MIME / running from file (--file)

Official extension: `.lzscr`. MIME type: `text/vnd.lzscr; charset=utf-8` (a text language).
You can evaluate an expression or a group of bindings in a single file. The CLI wraps file contents in parentheses and treats leading/trailing `~x = ...;` entries as a let-group.

```
echo '~x = 1; ~add ~x 2;' > /tmp/a.lzscr
cargo run -p lzscr-cli -- --file /tmp/a.lzscr
# => 3
```

Until dedicated editor support is installed, treat it as plain text. (VS Code associates `*.lzscr` with Plain Text via `.vscode/settings.json` in this repo.)

Shebang example (when the binary is installed as `lzscr`):

```
#!/usr/bin/env lzscr
~add 1 2;
```

## ゴールデンテスト (tokenizer)

CLI 経由で self-hosted tokenizer を検証するゴールデンテストを追加しました。

実行:

```bash
cargo test -p lzscr-cli --tests tokenize_golden
```

期待値ファイルは `goldens/*.golden` にあります。仕様変更で出力が変わった場合は、差分を確認し、意図した変更であれば golden を更新してください。

## Type checking (Hindley–Milner inference)

The CLI performs simple type inference ahead of evaluation. If it fails, execution aborts. You can print types as pretty or JSON.

```
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --types pretty
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --types json

# Skip type checking
cargo run -p lzscr-cli -- -e '(\~x -> ~x) 1' --no-typecheck
```

Notes:
- Bare symbols like `.Foo` are constructors (named containers). With `--ctor-arity` you can declare arity so the runtime catches over-application and 0-arg misuse.
- strict-effects is implemented. With `--strict-effects` enabled, effects are only allowed inside these special forms:
	- `(~seq a b)`: evaluate `a` in pure context, then `b` in effect context
	- `(~chain a b)`: run `a` then `b` (both effect context), return `b`
	- `(~bind e k)`: run `e`, pass the value to continuation `k`, then run `k`
	- Effect APIs are obtained via `(~effects .sym)`. The sugar `!sym` desugars to `(~effects .sym)` (e.g., `!println`, `!print`).

Effect examples:

```
# non-strict (default): effects are allowed anywhere
cargo run -p lzscr-cli -- -e '!println "hello"'

# strict-effects: only the 2nd arg of seq can perform effects
cargo run -p lzscr-cli -- -e '(~seq () (!println "hello"))' -s

# do-notation (sugar)

`!{ ... }` desugars to a chain of `~chain`/`~bind`. You can write multiple statements and a single final expression.

```
!{
	_ <- !println "start";   # drop the result
	x <- !{ !println "work"; 1 + 2 };  # bind
	!println "done";         # intermediate statement
	x + 4                    # final expression (returned)
}
```

Which conceptually corresponds to:

```
(~bind (!println "start") (\_ ->
 (~bind (!{ !println "work"; 1 + 2 }) (\x ->
	(~chain (!println "done") (x + 4)))))
```

Static analysis (language-level AST):
- Use `--analyze` to analyze a single expression. Duplicate detection thresholds: `--dup-min-size` (node count) and `--dup-min-count` (occurrences).

```
cargo run -p lzscr-cli -- -e '(~add (~add 1 2) (~add 1 2))' --analyze --dup-min-size 3 --dup-min-count 2
```

- Static arity checks for constructors (with `--ctor-arity`)

```
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --ctor-arity 'Foo=1'
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --format json --ctor-arity 'Foo=1'
```

- Runtime arity checks (report over-application, etc. at evaluation time)

```
cargo run -p lzscr-cli -- -e '.Bar 1 2' --ctor-arity 'Bar=1'
```

Core IR dump:

```
cargo run -p lzscr-cli -- -e '1 + 2' --dump-coreir
cargo run -p lzscr-cli -- -e '1 + 2' --dump-coreir-json
```

## Contributing

We welcome contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development workflow and setup
- Coding standards and style guidelines
- Testing requirements and coverage expectations
- Pull request process

### Quick Start for Contributors

```bash
# Clone and build
git clone https://github.com/mako10k/lzscr.git
cd lzscr
cargo build

# Before committing, ensure these pass:
cargo fmt --all --check        # Format check
cargo clippy --all-targets -- -D warnings  # Lint check
cargo test                     # All tests
./scripts/check-file-sizes.sh  # File size compliance
```

**Code Quality Standards:**
- Rust files: Keep under 1000 lines (recommended: 500 lines)
- LazyScript files: Keep under 300 lines (recommended: 200 lines)
- Function complexity: cognitive_complexity under 15
- Test coverage: Maintain above 70% overall

See [docs/coding-standards.md](docs/coding-standards.md) for detailed guidelines.

GitHub Actions CI runs 7 checks automatically: fmt, clippy, test, coverage, security audit, docs, and dependency checks.

## Code formatting

- The repository uses `rustfmt.toml` to enforce Rust code style.
- VS Code: format-on-save is enabled via `.vscode/settings.json`.
- Manual run:

```bash
cargo fmt --all
```

### Formatting lzscr source (experimental)

An experimental pretty-printer for lzscr (via AST) is included.

```bash
# one-liners
cargo run -p lzscr-cli -- -e '1 + 2 * 3' --format-code

# files (.lzscr)
cargo run -p lzscr-cli -- --file /path/to/code.lzscr --format-code

# options (width and indent)
cargo run -p lzscr-cli -- --file /path/to/code.lzscr --format-code --fmt-indent 4 --fmt-width 120
```

Note: for now, the output reflects desugared forms (e.g., infix operators rendered as function applications). Human-friendly formatting rules may evolve.

## Building and installing the VS Code extension (VSIX)

This repo contains a VS Code extension scaffold under `extensions/lzscr-vscode` that provides highlighting and formatting for `.lzscr` files.

Steps:

```bash
# install deps and build the VSIX package
cd extensions/lzscr-vscode
npm ci
npm run package
# lzscr-vscode-*.vsix will be created in the current directory
```

In VS Code, use “Extensions > … > Install from VSIX…” and pick the generated VSIX.

Extension settings:
- lzscr.formatterPath: CLI executable path (default: lzscr-cli)
- lzscr.indent: indent width (default: 2)
- lzscr.maxWidth: max line width hint (default: 100)

These are forwarded to the CLI as `--fmt-indent` / `--fmt-width`.

## Standard library (roadmap)

We plan to build a small stdlib in stages. See `docs/stdlib.md` for the plan, modules, and roadmap. Initially it will be preloaded by the CLI; later we’ll introduce import syntax.
