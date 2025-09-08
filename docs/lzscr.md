## lzscr: Language overview and development plan

This document consolidates the practical overview, design choices, and roadmap for the lzscr implementation (a reboot inspired by GitHub: mako10k/lazyscript), updated to match the current .Member-only constructor policy and the English-only docs policy.

### 1) Language highlights (from the original lazyScript, adapted)

- Goal: A small, lazy, expression-oriented scripting language. Top-level is an expression; the CLI wraps file contents as a LetGroup to allow definitions before the final expression.
- Runtime model (representative):
	- Lazy evaluation via thunks. Core value kinds: lambda/closure, application, reference, int, str, char, tuple/record, list, ctor (algebraic-like), symbol, native, choice, raised (caret-exception), thunk.
	- Reference: `~name` resolves to a statically-bound slot at runtime evaluation.
	- Lambda: `\x -> expr`. Blocks `{ expr }` are expressions.
	- Lists/tuples/records: have sugars and desugars, consistent with the parser and formatter.
	- Constructors: values use .Member-only tags, like `.Some`, `.None`, `.Ok`, `.Err`. Zero-arity forms are written as `.Tag()`. Bare `Tag` is a variable; `.Tag` is a constructor tag/symbol.
- Syntax sugar (current root module is `Builtins` via prelude wiring):
	- Effect sugar: `!name` → `(~effects .name)`
	- Do-notation: `!{ ... }` → desugars to `chain/bind` sequencing
	- Exceptions: `^(Expr)` raises; `Expr ^| lam` catches caret-style exceptions
- Execution semantics:
	- File mode: by default looks for `main` (configurable with `--entry`). `-e` evaluates a one-liner and prints the final value.
	- strict-effects: effectful builtins (println/print/def/require, etc.) are restricted to effect-contexts formed by `seq/chain/bind`.
- Builtins/prelude:
	- Core: `to_str, print, println, seq, add, sub, lt, eq, chain, bind, return`, etc.
	- Prelude can be swapped (cdylib) and provides a `Builtins` root record/namespace when available.
	- Dynamic loading: `~require`, `!def` as runtime-level features under effect discipline.
- Intermediate Representation (IR):
	- A Core IR exists for lowering/diagnostics; an IR evaluator is planned; LLVM IR is future work.
- Tooling:
	- Formatter and a VS Code extension (highlight/format/diagnostics) are planned; rustfmt/clippy/tests are enforced.

### 2) Implementation choices

- Implementation language: Rust (stable toolchain).

Reasons:
- Safety/stability with ownership and types; no undefined behavior; no GC required.
- Good lexing/parsing/diagnostics ecosystem (`logos`, `chumsky`, `miette`/`ariadne`).
- Plugin/FFI via `libloading` and a simple C-ABI for prelude providers.
- Future LLVM via `inkwell` is feasible.
- Cargo makes distribution and development fast and portable.

Other options considered (brief):
- C(+Flex/Bison): easy to port from original, but worse safety/devex.
- Zig: nice single binary/FFI, but parser/diagnostic ecosystem is less mature for this use.
- OCaml: great for language work, but distribution/FFI/editor tooling trade-offs.

### 3) Workspace architecture (Cargo workspace)

Suggested layout (current repository follows this):
- crates/
	- lzscr-lexer: tokenization (logos)
	- lzscr-ast: AST and spans, printing/formatting helpers
	- lzscr-parser: syntax parsing (chumsky) + sugars (`!`, `!{}`)
	- lzscr-runtime: evaluator, env/effects, strict-effects checking
	- lzscr-coreir: Core IR definitions, lowering, minimal type checks
	- lzscr-cli: CLI binary (`-e`, `--strict-effects`, `--entry`, etc.)
	- Optional: lzscr-llvmir (inkwell), lzscr-compiler, lzscr-format, lzscr-prelude

Design notes:
- Memory/ownership: arena/slotmap for cycles; avoid `Rc<RefCell<_>>` cycles when possible.
- Effect rules: runtime enforces effect-context for effectful calls.
- Plugin ABI: `#[no_mangle] extern "C" fn ls_prelude_register(env: *mut Env) -> i32` expected; loaded via `libloading`.

Additional deltas from original:
- List syntax implemented: `[a, b, ...]`, `[]`, and right-associative `h : t`.
	- Desugar: `h : t` → `((~cons h) t)`, `[a,b,c]` → `(~cons a (~cons b (~cons c [])))`.
	- `cons : a -> List a -> List a` is a builtin.

### 1.1) Terminology

- Namespace (compile-time) vs Module (runtime record). Value-level member access uses symbols like `.name`.
- Environment means the runtime evaluation environment (frames and closure captures).

CLI flag `--sugar-namespace` remains for compatibility, but docs refer to the prelude root as a module/record.

### 4) Dev environment (recommended)

- Rust stable (rustup), rustfmt, clippy
- VS Code + rust-analyzer (format-on-save, show clippy warnings)
- CI: GitHub Actions (build + test + clippy with cache)
- Optional: LLVM 15+ and inkwell
- Tests: `cargo test` across crates (parser golden tests, runtime expectations, property tests with `proptest`)

File type/MIME:
- Extension: `.lzscr`
- MIME: `text/vnd.lzscr; charset=utf-8`
- Editor association: initially plaintext; a dedicated VS Code extension is planned.

#### 4.1) CLI/ENV compatibility (initial + future)

- Initial flags: `-e/--eval`, `-s/--strict-effects`, `--entry`, `-p/--prelude-so`, `-n/--sugar-namespace`
- Env vars: `LAZYSCRIPT_PRELUDE_SO`, `LAZYSCRIPT_SUGAR_NS`, `LAZYSCRIPT_INIT`
- Future: `-i/--dump-coreir`, `-c/--eval-coreir`, `-t/--typecheck`, `--no-kind-warn`, `--kind-error`, `--init`, `--trace-*`, `-d/--debug`, `-v/--version`

### 5) Near-term milestones

1. Workspace skeleton (crates, minimal CLI)
2. Lexer/parser/desugar for comments/numbers/strings/refs/lambdas/blocks/cons
3. Minimal evaluator and core pure builtins (`to_str`, `add/sub/eq/lt`)
4. Effects API (`println/print/def/require`) and strict-effects verification
5. Prelude cdylib design and dynamic loading (`--prelude-so`)
6. Formatter/editor wiring
7. Core IR and minimal type checks; future LLVM groundwork

Type primitives (current): Unit / Int / Float / Bool / Str / Char
- Chars: `'a'`, `'\n'`, `'\\'`, `'\''`, `'\u{1F600}'` are supported (one Unicode scalar)
- Display: CLI/print/to_str show single-quoted char forms

Open items:
- Char literal error coverage (invalid `\u{}`, multi-scalar, empty)
- CLI/runtime: char utilities docs (`char_to_int`/`int_to_char`)
- Core IR evaluator: exceptions/effects/letrec (future IR pass)
- Documentation: grammar and escape table for Char

### 6) Name resolution strategy (resolve early)

Resolve `~name` references at compile-time (IR build) where possible, so the evaluator avoids dynamic name lookups.

Two-phase approach:
- Phase 1: collect and assign slots (locals/modules), analyze upvalues
- Phase 2: lower to IR with pattern-binding code that writes into pre-allocated slots

Kinds of references:
- Local binds: slots or de Bruijn-like indices
- Closure upvalues: upvalue tables to parent frames
- Module (top-level): fixed module slots (mutual recursion allowed)
- Dynamic module member access remains deferred for plugin/import compatibility
- Dynamic env operations (`!require`, `!def`) are always dynamic (explicit nodes)

Patterns (LHS):
- Reserve slots for binders (`~x`, etc.) during resolution; at runtime, matching writes values into those slots
- RHS references can be resolved to those slots; detect use-before-initialization paths statically where applicable
- On mismatch, produce a caret-style `Raised` value; can be caught by `^|` or alt lambdas

IR sketch:
- VarRef(kind, slot, upvalue_path), PatBind opcode, ModuleRef + DynMember(".env"/name)

Static errors:
- Unbound references, duplicate pattern binders, use-before-initialization in values

Exceptions integration:
- `^(e)` → Raise(e); `x ^| h` → try-catch chain; caret patterns allocate slots like normal patterns

Compatibility:
- `!name`/`!{}` sugars desugar before resolution; prelude symbols appear as module+member accesses

### 7) Bash history expansion tip

When using `-e '!{ ... }'`, turn off history expansion: `set +H`, or use single quotes/heredocs.
Example: `( set +H; ./target/debug/lzscr -e '!{ !println ("ok"); };' )`

### 8) Future scope (selected; WIP)
The following items are exploratory and not guaranteed. They require separate design/approval before implementation.

- Core IR dump/eval, typecheck, kind warnings/errors, execution tracing

## Current roadmap (as of 2025-09)

Ordered roughly by priority. Items will be detailed and adjusted as implementation proceeds.

1. Core IR for `~chain/~bind/~raise/~catch/~alt` (lowering + executor)
2. Static name resolution and upvalue analysis (slotting)
3. HM rank-1 inference; future kind/effect integration with strict-effects
4. Exceptions and alt-lambda typing; IR integration
5. Constructor variables/ADT arity checking and declarations (later)
6. Stdlib build-out (Option/List/Result/String)
7. Char/String literal improvements; Unicode utilities
8. CLI diagnostics (`-i/-c/-t/--trace-*`)
9. Analyzer/linter rewire on IR
10. VS Code minimal extension
11. Formatter stabilization
12. Perf/memory optimizations
13. Documentation updates (specs and tutorials)

### 9) Type system (MVP sketch)

Goal: catch type/effect errors early to improve safety and diagnostics.

Minimal policy (current state may be ahead; see specs for details):
- Separate kind (Pure/IO) from types; enforce effect-context for IO
- HM rank-1 on IR: `Int/Str/Unit/Tuple/List/Func`
- References instantiate schemes; lambdas introduce fresh type vars
- Applications unify; tuples/lists compose
- Patterns constrain by RHS type and bind variable types
- Exceptions: `^(e)` introduces non-returning paths; caret handlers align return types

Representative builtin kinds:
- `to_str : forall a. a -> Str` (Pure)
- `add/sub : Int -> Int -> Int` (Pure)
- `eq : forall a. a -> a -> Bool` (Pure)
- `lt : Int -> Int -> Bool` (Pure)
- `print/println : Str -> Unit` (IO)
- `seq : a -> b -> b` (Pure; second arg may be IO under context check)
- `chain/bind` are context-driven (checked by the runtime and, future, by kinds)

Note: We represent booleans as `.true`/`.false` tags via a `Bool` constructor in the surface language; `true()`/`false()` sugar expands to `~true`/`~false` for convenience.

### 10) Constructors and patterns (.Member-only)

Policy: Constructors at the surface level are .Member-only.
- Values: `.Tag arg1 arg2 ...`; zero-arity must be `.Tag()`
- Patterns: `\(.Tag ~x ~y) -> ...`; zero-arity: `.Tag()`
- Bare uppercase identifiers have no special meaning; they are just variables.

Sugars and operators (excerpt):
- Arithmetic: `+ - * /` (Int), float: `.+ .- .* ./`
- Comparisons: `< <= > >=`, equality `== !=`; float variants `. < .<= .> .>=`
- OrElse is `||` (parser/formatter use `||`)
- Alternative lambda composition `|`: only between lambdas; lower precedence than `->` and `||`; right-associative
	- Semantics: on application, try left; if its pattern fails, try right; if left matches and later raises, right is not evaluated
	- Desugar: `\x -> (~alt left right x)` with builtin `alt : (a->r) -> (a->r) -> a -> r`

Tuples/records sugars:
- `(a,b,...)` → `(.Tuple a b ...)`
- `{k:v,...}` → `(.Record (.KV "k" v) ...)` (keys are identifiers)
- Field access is function-call style with a symbol: `{a:1, b:2} .a`

CLI example:

```
# Analyze with an expected constructor arity map
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --ctor-arity 'Foo=1'

# Same, JSON output
cargo run -p lzscr-cli -- -e '.Foo 1 2' --analyze --format json --ctor-arity 'Foo=1'

# Runtime error example (over-application)
cargo run -p lzscr-cli -- -e '.Bar 1 2' --ctor-arity 'Bar=1'
```

- Current provisional behavior:
- Parser: `Ident` at value position is a `Symbol` (constructor-like tag) and `.Member` is the canonical constructor/symbol form; `~Ident` is a `Ref`.
- Evaluator: a `Symbol`/member tag acts like a constructor token that accumulates arguments; materialization is finalized after type/resolution work. For now, `.S()`, `.S a`, `.S a b`, etc. render as partially constructed `<fun>` values (to be finalized with type/IR work).
- Builtins: resolved via references, so `~` is required. Example: `(~to_str (~add 1 2))`.

