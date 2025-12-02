# effect/log Module Specification

## Overview
The `stdlib/effect/log.lzscr` module provides structured logging utilities with level tags, field builders, and JSON output formatters. It builds on `effect/io` to emit decorated log messages.

## Module Path
- **Namespace**: `.effect .log`
- **Requires**: `--stdlib-mode=allow-effects` to import

## Exported API

### Basic Logging

#### `~emit`
```lzscr
~emit : Str -> Str -> Effect Unit
```
Writes a plain message with a level prefix (`[LEVEL] message`).

**Example**:
```lzscr
(~Log = (~require .effect .log); (~Log .emit "INFO" "startup complete"))
# Output: [INFO] startup complete
```

---

#### Level-Specific Loggers
```lzscr
~trace, ~debug, ~info, ~warn, ~error, ~fatal : Str -> a -> Effect Unit
```
Each logger emits `[LEVEL] label: value` where `value` is rendered via `~to_str`.

**Example**:
```lzscr
(~Log = (~require .effect .log); (~Log .info "count" 42))
# Output: [INFO] count: 42
```

---

### Tap Logging (Observe and Return)

#### `~tap_with_level`
```lzscr
~tap_with_level : Str -> Str -> a -> Effect a
```
Logs the value at the given level and label, then returns the value unchanged.

**Example**:
```lzscr
(~Log = (~require .effect .log); (~chain (~Log .tap_with_level "DEBUG" "x" 10) (\~result -> ~result)))
# Output: [DEBUG] x: 10
# Returns: 10
```

---

#### Level-Specific Tap Helpers
```lzscr
~tap_trace, ~tap_debug, ~tap_info, ~tap_warn, ~tap_error, ~tap_fatal : Str -> a -> Effect a
```
Convenience wrappers around `~tap_with_level` for each log level.

---

### Structured Field Logging

#### `~field`
```lzscr
~field : Str -> a -> { key: Str, value: a }
```
Constructs a key-value field record.

**Example**:
```lzscr
(~Log = (~require .effect .log); (~Log .field "session" 123))
# Returns: { key: "session", value: 123 }
```

---

#### `~log_fields`
```lzscr
~log_fields : Str -> Str -> [{ key: Str, value: a }] -> Effect Unit
```
Emits fields as `[LEVEL] label: key1=value1 key2=value2 ...`.

**Example**:
```lzscr
(~Log = (~require .effect .log); 
 (~Log .log_fields "INFO" "session" [
   (~Log .field "user" "alice"),
   (~Log .field "count" 5)
 ]))
# Output: [INFO] session: user=alice count=5
```

---

#### Level-Specific Field Loggers
```lzscr
~trace_fields, ~debug_fields, ~info_fields, ~warn_fields, ~error_fields, ~fatal_fields : Str -> [{ key: Str, value: a }] -> Effect Unit
```

---

### JSON Field Logging

#### `~log_fields_json`
```lzscr
~log_fields_json : Str -> Str -> [{ key: Str, value: a }] -> Effect Unit
```
Emits fields as JSON: `[LEVEL] label: {"key1": value1, "key2": value2}`.

**Example**:
```lzscr
(~Log = (~require .effect .log);
 (~Log .log_fields_json "INFO" "metrics" [
   (~Log .field "latency" 42),
   (~Log .field "errors" 0)
 ]))
# Output: [INFO] metrics: {"latency": 42, "errors": 0}
```

---

#### Level-Specific JSON Loggers
```lzscr
~trace_fields_json, ~debug_fields_json, ~info_fields_json, ~warn_fields_json, ~error_fields_json, ~fatal_fields_json : Str -> [{ key: Str, value: a }] -> Effect Unit
```

---

### Scoped Field Combinators

#### `~with_fields_logger`
```lzscr
~with_fields_logger : (Str -> [field] -> Effect Unit) -> [field] -> (Str -> [field] -> Effect Unit)
```
Creates a logger that always prepends the given base fields to any additional fields passed in.

**Example**:
```lzscr
(~Log = (~require .effect .log);
 ~Scoped = ((~Log .with_fields_logger) (~Log .info_fields) [
   (~Log .field "session" 999)
 ]);
 (~Scoped "request" [
   (~Log .field "path" "/api")
 ]))
# Output: [INFO] request: session=999 path=/api
```

---

#### `~with_fields_json_logger`
Alias for `~with_fields_logger`, works identically with JSON formatters.

---

### Utility Functions

#### `~append_fields`
```lzscr
~append_fields : [field] -> [field] -> [field]
```
Concatenates two field lists.

---

#### `~fields_from_pairs`
```lzscr
~fields_from_pairs : [{ key: Str, value: a }] -> [{ key: Str, value: a }]
```
Identity mapping for field construction from pairs (reserved for future transformations).

---

## Implementation Notes
- All loggers return `Effect Unit` and must be sequenced with `~chain`.
- Fields are rendered recursively; JSON output uses `~to_str` for values.
- Level tags are uppercase: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL`.

## Related Modules
- `effect/io`: Base print/println effects
- `effect/fs`: Filesystem operations with Result types

## Testing
- Type inference tests: `crates/lzscr-types/tests/effects.rs`
- Integration tests: `crates/lzscr-cli/tests/cli.rs`
