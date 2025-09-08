# Module spec (draft; WIP)

Treat a module as a record value and define a pre-resolution mechanism via `~require` (static expansion before execution). Goals: avoid namespace collisions and pave the way to self-hosting (an lzscr implementation of parser/runtime).

## Basic model
- A module is just a record value.
- A module file (`.lzscr`) is recommended to contain private `~` bindings at the top level and a final record expression exposing the public API:

```lzscr
~helper = ...;
~internal_state = ...;

{  # this record is the public surface of the module
  publicFn: \x -> ...;
  publicConst: 42;
};

~more_privates = ...;   # not exported because it's not the final expression
```

The final record expression becomes the public API. Other final expressions are allowed, but consumers of `~require` typically expect a record for field access.

## `~require` (pre-resolution)
`~require` is a special builtin resolved before execution; it inlines the module's AST at the call site.

- Syntax (readable pseudo BNF):
  - `(~require .seg1 .seg2 ... .segN)`
  - Arguments must be one or more ctor labels (dot-prefixed identifiers)
- Example:
  - `~Json = ~require .std .data .json;`
  - Usage: `~Json .parse "..."` (field access on the record)

### Resolution rules
1. Precondition: all args must be ctors (`.name`) or it's a static error.
2. Path resolution: map to relative path `seg1/seg2/.../segN.lzscr` and search in order:
  - workspace current directory (user project)
  - directory provided via CLI `--stdlib-dir`
  - future: extra `--module-path` (colon-separated list)
3. File existence: static error if not found.
4. Parse/typecheck: parse the module file (typecheck unless `--no-typecheck`); static error on failure.
5. Expansion: inline the module AST at the `~require` call using an anonymous let-group:
  - Concretely, replace with `let { <top-level ~ bindings> } in <final-record>`.
  - Private identifiers remain scoped to the expansion site (hygienic).

### Error conditions
- Non-ctor args or indeterminate values: error
- File not found: error
- Parse/typecheck failure: error
- Cycles (A requires B, B requires A): error; include reference chain in message

### Implementation notes (CLI/frontend)
- Perform expansion in a preprocessing pass walking the AST.
- Cache: if the same path is required multiple times, read/parse once and reuse the AST (still expand at each site).
- Debug: when `--dump-coreir(-json)` is used, output IR after expansion (IR differs with/without expansion).
- Spans: include both call-site and definition-site spans in error messages if possible.

## Typing and evaluation semantics
- After expansion, normal type inference/evaluation applies to the resulting expression (a record).
- `~require` does not remain at runtime and has no effects (pure syntax).
- The final expression can be non-record, but records are recommended for consumers expecting field access.

## Example
Module file `std/data/json.lzscr`:

```lzscr
~ws = ...;         # private
~digits = ...;     # private

{ parse: \s -> ...; from_int: \n -> ... };
```

Caller:

```lzscr
~Json = ~require .std .data .json;
~Json .parse "{\"x\": 1}";
```

## Limitations and future work (WIP)
The items below are exploratory and not committed for implementation yet. Treat them as proposals subject to change.
- No variable arguments or dynamic paths (purity)
- Parameterized modules (functors), aliases, and re-exports are future work
- A future `import` syntax (`from M import f, g`) may be specified separately; keep `~require` as a low-level mechanism
