# Language overview (current)
Note: This overview reflects the current implementation. Any "future" or "planned" mentions are exploratory (WIP) and are not commitments.

Roadmap/source of truth for prioritization: see ../ROADMAP.md.

- Goal: proof-of-concept of a lazy-evaluated, expression-based language.
- Execution paths: direct AST eval (interpreter) plus a Core IR pipeline (AST→IR lowering, textual dump).
- Implemented subset:
  - Values: Unit, Int, Float, Bool, Str, Symbol (atomic `.name` values), Ctor, List, Tuple, Record, Lambda, Native (builtins), Closure
  - Expressions: Unit, Int, Str, Ref(~name), Symbol, Lambda(\x -> e), Apply(e1 e2), Block({ e })
  - Builtins: to_str, add, sub, eq, lt, seq, effects(.print/.println)
  - strict-effects: when enabled, effects are allowed only in the second argument of (~seq a b)
  - Sugar: !sym → (~effects .sym); constructors use bare identifiers, symbols remain `.name` atoms
  - Sequential sugar: !{ … } → chain/bind chaining
- Analysis: duplicate detection/unbound/shadowing/unused params (CLI --analyze)
- Tools: CLI (-e eval, --strict-effects, --analyze, --format json, --dump-coreir, --dump-coreir-json). CI runs fmt/clippy/test/audit/deny/coverage.
