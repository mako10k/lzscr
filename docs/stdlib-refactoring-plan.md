# Standard Library Refactoring Plan

**Date**: 2025-12-03  
**Status**: Planning  
**Target Completion**: 2026-Q1

## Executive Summary

Refactor the lzscr standard library to establish clear separation between effects/pure code, create hierarchical module structure, and introduce namespace aliasing mechanisms to reduce boilerplate.

## Current Problems

### Structural Issues
- `stdlib/prelude.lzscr` mixes pure functions with Builtins re-exports (300+ lines)
- Primitive operations (Builtins delegation) and complex operations (recursive implementations) at same level
- Frequent namespace aliasing (`~Str`, `~Math`) required in every file
- Good `pure/` vs `effect/` separation exists but needs further layering

### Namespace Redundancy
- `prelude.lzscr` re-imports `~Option`, `~Result` via `~require`
- Yet `prelude` itself redefines Option/Result helper functions
- User code must repeatedly write `~Str = ~Builtins .string`

### Maintenance Burden
- Adding new functionality unclear where to place
- Cross-module dependencies implicit
- No clear primitive vs. derived function boundary

## Proposed New Structure

```
stdlib/
├── core/                      # NEW: Primitive layer
│   ├── builtins.lzscr         # Builtins namespace re-exports
│   ├── types.lzscr            # Type aliases (%Bool, %Option, %Result)
│   └── ops.lzscr              # Basic operator overloads/wrappers
│
├── pure/                      # Pure function layer (existing + extended)
│   ├── primitives/            # NEW: Primitive data type operations
│   │   ├── list.lzscr         # MOVED from stdlib/pure/list.lzscr
│   │   ├── string.lzscr       # NEW: extracted from prelude
│   │   ├── int.lzscr          # NEW: integer conversion/operations
│   │   └── char.lzscr         # NEW: character predicates (from lex)
│   │
│   ├── data/                  # NEW: Algebraic data type operations
│   │   ├── option.lzscr       # EXISTING
│   │   ├── result.lzscr       # EXISTING
│   │   ├── either.lzscr       # TODO
│   │   └── tuple.lzscr        # NEW: tuple helpers
│   │
│   ├── algorithms/            # NEW: Higher-order algorithms
│   │   ├── sort.lzscr         # TODO: sorting/comparison
│   │   ├── search.lzscr       # TODO: searching/filtering
│   │   └── fold.lzscr         # TODO: advanced folds
│   │
│   └── parsers/               # NEW: Parser combinators
│       ├── lex.lzscr          # MOVED from stdlib/pure/lex.lzscr
│       └── parse.lzscr        # TODO: self-hosted parser
│
├── effect/                    # Effect layer (existing + extended)
│   ├── primitives/            # NEW: Basic effects
│   │   ├── io.lzscr           # EXISTING
│   │   └── log.lzscr          # EXISTING
│   │
│   ├── system/                # NEW: System operations
│   │   ├── fs.lzscr           # EXISTING
│   │   ├── env.lzscr          # TODO: environment variables
│   │   └── process.lzscr      # TODO: process operations
│   │
│   └── advanced/              # NEW: Advanced effects
│       ├── async.lzscr        # TODO: asynchronous operations
│       └── stream.lzscr       # TODO: streaming IO
│
├── compat/                    # Backward compatibility (existing)
│   └── prelude_aliases.lzscr  # EXISTING
│
└── prelude.lzscr              # REFACTORED: thin import layer
```

## New prelude.lzscr Design (Thin Import Layer)

```lzscr
# lzscr stdlib prelude v2 (thin import layer)
# Recommended entry point for all modules.
# Selective imports possible based on project style.

# === Core layer: Builtins re-exports ===
~Core = (~require .core .builtins);
~Str = (~Core .Str);
~Math = (~Core .Math);
~Char = (~Core .Char);
~Scan = (~Core .Scan);
~Unicode = (~Core .Unicode);

# === Pure layer: Primitive operations ===
~String = (~require .pure .primitives .string);
~List = (~require .pure .primitives .list);
~Int = (~require .pure .primitives .int);

# === Pure layer: Algebraic data types ===
~Option = (~require .pure .data .option);
~Result = (~require .pure .data .result);

# === Basic functions (direct definition) ===
~id ~x = ~x;
~compose ~f ~g ~x = ~f (~g ~x);

# === Backward-compatible aliases ===
# List operations (delegate to List module)
~length = (~List .length);
~append = (~List .append);
~reverse = (~List .reverse);
~map = (~List .map);
~filter = (~List .filter);
~foldl = (~List .foldl);
~foldr = (~List .foldr);

# String operations (delegate to String module)
~str_len = (~String .len);
~str_concat = (~String .concat);
~str_slice = (~String .slice);
~starts_with = (~String .starts_with);
~ends_with = (~String .ends_with);
~find = (~String .find);
~split_char = (~String .split_char);

# Option/Result (re-export key functions)
~is_some = (~Option .is_some);
~map_option = (~Option .map);
~unwrap_or = (~Option .unwrap_or);
~is_ok = (~Result .is_ok);
~map_result = (~Result .map);

# === Type definitions (from core/types.lzscr) ===
%Bool = (~Core .%Bool);
%Option = (~Core .%Option);
%Result = (~Core .%Result);

# === Effect layer NOT imported in prelude ===
# Reason: preserve purity, introduce effects consciously where needed
```

## Namespace Abbreviation Mechanisms

### Proposal A: Package-Local Import Syntax (NEW FEATURE)

```lzscr
# Current: verbose re-aliasing in each file
~Str = ~Builtins .string;
~Math = ~Builtins .math;

# Proposal: local expansion at file top
(~use ~Builtins [.string .math .char])
# After this, ~string is synonymous with ~Builtins .string

# Or shorthand notation
(~use ~Builtins .string as ~Str)
# Reference as ~Str
```

**Benefits:**
- Explicit file-level dependencies
- Reduced boilerplate
- Zero-cost if expanded at parse/typecheck time

**Implementation Complexity:** Medium (parser extension + desugaring)

### Proposal B: Project Configuration File (`.lzscr.toml`)

```toml
# .lzscr.toml
[auto_imports]
"~Str" = "~Builtins.string"
"~Math" = "~Builtins.math"
"~Char" = "~Builtins.char"
"~Scan" = "~Builtins.scan"

[stdlib]
mode = "pure"  # or "allow-effects"
search_paths = ["./stdlib", "./vendor"]
```

**Benefits:**
- Project-wide consistency
- Separation from CI/build config

**Drawbacks:**
- Increased implicitness (requires IDE support)

**Implementation Complexity:** Low (load config at CLI startup)

### Proposal C: Prelude Extension (REALISTIC SHORT-TERM)

```lzscr
# stdlib/core/builtins.lzscr (NEW)
# Flat re-export of Builtins namespace
~Str = ~Builtins .string;
~Math = ~Builtins .math;
~Char = ~Builtins .char;
~Scan = ~Builtins .scan;
~Unicode = ~Builtins .unicode;

{
  Str: ~Str,
  Math: ~Math,
  Char: ~Char,
  Scan: ~Scan,
  Unicode: ~Unicode,
  # Type aliases
  "%Bool": %{ True | False },
  "%Option": %forall %a . %{ Some %a | None },
  "%Result": %forall %a %e . %{ Ok %a | Err %e }
}
```

User code:
```lzscr
# Traditional
~Str = ~Builtins .string;
~custom_len ~s = (~Str .len ~s);

# After proposal
~Core = (~require .core .builtins);
~custom_len ~s = ((~Core .Str) .len ~s);

# Or if prelude auto-imports
~custom_len ~s = (~Str .len ~s);
```

## Implementation Roadmap

### Phase 1: Establish Core Layer (1-2 weeks)
**Priority**: HIGH  
**Risk**: LOW

- [ ] Create `stdlib/core/builtins.lzscr`
- [ ] Create `stdlib/core/types.lzscr` (type aliases)
- [ ] Rewrite `stdlib/prelude.lzscr` to use core layer
- [ ] Verify existing tests pass

**Success Criteria:**
- All existing tests pass
- No user-facing API changes
- Core layer exports verified in REPL

### Phase 2: Hierarchize Pure Layer (2-3 weeks)
**Priority**: HIGH  
**Risk**: MEDIUM

- [ ] Extract `pure/primitives/string.lzscr` from prelude
- [ ] Move `stdlib/pure/list.lzscr` → `pure/primitives/list.lzscr`
- [ ] Create `pure/primitives/int.lzscr` (`str_to_int`, etc.)
- [ ] Organize `pure/data/` (option, result existing)
- [ ] Move `stdlib/pure/lex.lzscr` → `pure/parsers/lex.lzscr`
- [ ] All modules reference core layer

**Success Criteria:**
- Prelude delegates to specialized modules
- No circular dependencies
- Golden tests updated

### Phase 3: Hierarchize Effect Layer (1-2 weeks)
**Priority**: MEDIUM  
**Risk**: LOW

- [ ] Move to `effect/primitives/`
- [ ] Move `stdlib/effect/fs.lzscr` → `effect/system/fs.lzscr`
- [ ] Create `effect/system/env.lzscr` (environment variables)
- [ ] Unify Result types (all effects return Result)

**Success Criteria:**
- Effect stdlib tests pass
- Clear pure/effect boundary
- Documentation reflects new structure

### Phase 4: Release Prelude v2 (1 week)
**Priority**: HIGH  
**Risk**: HIGH (breaking change)

- [ ] Rewrite prelude.lzscr as thin import layer
- [ ] Add backward-compatible aliases
- [ ] Update documentation (readme.md, stdlib.md)
- [ ] Create migration guide

**Success Criteria:**
- Migration guide published
- Deprecation warnings for old paths
- User code migration script available

### Phase 5: Namespace Abbreviation Mechanism (Immediate if Proposal C, 2-4 weeks for A/B)
**Priority**: MEDIUM  
**Risk**: LOW (C) / MEDIUM (A, B)

- Proposal C (short-term): Complete in Phase 1
- Proposal A (`~use` syntax): Parser extension + desugar implementation
- Proposal B (`.lzscr.toml`): CLI config loading implementation

**Success Criteria:**
- Boilerplate reduced by >50%
- IDE autocomplete works
- Examples updated

## Recommended Namespace Practices

### Pattern 1: Module-Local Expansion (RECOMMENDED)
```lzscr
# stdlib/pure/primitives/string.lzscr
~Core = (~require .core .builtins);
~Str = (~Core .Str);  # Abbreviate within file

~starts_with ~s ~pre = (
  ~n = (~Str .len ~pre);
  ~m = (~Str .len ~s);
  # ...
);
```

### Pattern 2: Record Field Access (VERBOSE BUT EXPLICIT)
```lzscr
~Core = (~require .core .builtins);
~starts_with ~s ~pre = (
  ~n = ((~Core .Str) .len ~pre);
  ~m = ((~Core .Str) .len ~s);
  # ...
);
```

### Pattern 3: Via Prelude (GLOBAL NAMESPACE POLLUTION)
```lzscr
# Assuming prelude defines ~Str
~starts_with ~s ~pre = (
  ~n = (~Str .len ~pre);
  # ...
);
```

## Minimizing Breaking Changes

### Migration Path:
1. Implement new structure as `stdlib_v2/` in parallel
2. Mark existing `stdlib/` as `DEPRECATED`
3. Gradual phase-out over 3 release cycles (~6 months)
4. `--stdlib-version=v1|v2` flag for selection

### Automated Migration Script:
```bash
# scripts/migrate_stdlib.sh
# - (~require .pure .option) → (~require .pure .data .option)
# - (~require .pure .lex) → (~require .pure .parsers .lex)
sed -i 's/(~require \.pure \.option)/(~require .pure .data .option)/g' **/*.lzscr
sed -i 's/(~require \.pure \.lex)/(~require .pure .parsers .lex)/g' **/*.lzscr
```

### Deprecation Timeline:
- **2025-Q4**: stdlib_v2 alpha release
- **2026-Q1**: stdlib_v2 beta, v1 deprecation warnings
- **2026-Q2**: stdlib_v2 stable, v1 removed from default
- **2026-Q3**: v1 fully removed

## Risks and Mitigations

### Risk 1: Breaking User Code
**Mitigation:**
- Maintain v1 compatibility for 6 months
- Provide auto-migration script
- Clear deprecation warnings

### Risk 2: Performance Regression
**Mitigation:**
- Benchmark prelude loading time
- Lazy module loading if possible
- Profile-guided optimization

### Risk 3: Circular Dependencies
**Mitigation:**
- Strict layering: core → pure/primitives → pure/data → pure/algorithms
- Dependency graph validation in CI

### Risk 4: IDE/Tooling Break
**Mitigation:**
- Update VS Code extension simultaneously
- Test language server with new structure
- Document module resolution changes

## Success Metrics

- [ ] Prelude loading time < 100ms (current baseline)
- [ ] Test suite passes 100%
- [ ] User code migration script tested on 5+ real projects
- [ ] Documentation coverage >90%
- [ ] Zero regression in type inference performance
- [ ] Community feedback positive (GitHub issues/discussions)

## Dependencies

- **Tooling**: lzscr-cli, lzscr-vscode
- **Documentation**: stdlib.md, ROADMAP.md
- **Tests**: All golden tests, stdlib classification tests

## Questions to Resolve

1. Should `core/` layer be user-facing or internal only?
2. How to handle third-party libraries following different conventions?
3. Should we enforce namespace naming conventions (e.g., `~Str` vs `~String`)?
4. Is `~use` syntax desirable or too much magic?
5. Timeline for self-hosted parser affecting `pure/parsers/` structure?

## References

- Current stdlib structure: `stdlib/` directory
- Module system: `docs/spec/modules.md`
- Purity annotations: `docs/stdlib-purity.md`
- Type system: `docs/type-system.md`

---

**Next Steps**: Review this plan, get feedback, then begin Phase 1 implementation.
