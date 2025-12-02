# effect/io Module Specification

## Overview
The `stdlib/effect/io.lzscr` module provides wrappers around the runtime `!print` and `!println` effects, plus convenience helpers for rendering values in effect contexts.

## Module Path
- **Namespace**: `.effect .io`
- **Requires**: `--stdlib-mode=allow-effects` to import

## Exported API

### `~print`
```lzscr
~print : Str -> Effect Unit
```
Prints a string to stdout without a trailing newline. Wraps the builtin `!print` effect.

**Example**:
```lzscr
(~Io = (~require .effect .io); (~Io .print "hello"))
```

---

### `~println`
```lzscr
~println : Str -> Effect Unit
```
Prints a string to stdout followed by a newline. Wraps the builtin `!println` effect.

**Example**:
```lzscr
(~Io = (~require .effect .io); (~Io .println "hello world"))
```

---

### `~show`
```lzscr
~show : a -> Effect Unit
```
Converts any value to its string representation via `~to_str` and prints it with a trailing newline.

**Example**:
```lzscr
(~Io = (~require .effect .io); (~Io .show 42))
# Output: 42
```

---

### `~log`
```lzscr
~log : Str -> a -> Effect Unit
```
Prints a labeled value by concatenating the label string with the value's string representation, followed by a newline.

**Example**:
```lzscr
(~Io = (~require .effect .io); (~Io .log "count: " 10))
# Output: count: 10
```

---

### `~say_all`
```lzscr
~say_all : [Str] -> Effect Unit
```
Prints each string in the list on its own line, in order.

**Example**:
```lzscr
(~Io = (~require .effect .io); (~Io .say_all ["one", "two", "three"]))
# Output:
# one
# two
# three
```

---

## Implementation Notes
- All helpers return `Effect Unit` and must be sequenced using `~chain` or similar effect combinators.
- The module depends on `~Builtins .string` for string concatenation utilities.
- Pattern matching on list constructors (`[]` / `( ~head : ~tail )`) drives the recursive `~say_all` implementation.

## Related Modules
- `effect/log`: Structured logging with levels, fields, and JSON output
- `effect/fs`: Filesystem operations returning Result types

## Testing
- Type inference tests: `crates/lzscr-types/tests/effects.rs`
- Integration tests: `crates/lzscr-cli/tests/cli.rs`
