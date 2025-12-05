# effect/fs Module Specification

## Overview
The `stdlib/effect/fs.lzscr` module wraps the runtime filesystem effects (`!fs.*`) so callers work with ergonomic `Result` types. Each operation returns `(Ok value | Err msg)` and provides convenience helpers for error handling.

## Module Path
- **Namespace**: `.effect .fs`
- **Requires**: `--stdlib-mode=allow-effects` to import

## Exported API

Every helper returns a `Result` so callers decide how to handle failures. Compose your own defaults with pattern matching, e.g.

```lzscr
(~Fs = (~require .effect .fs);
	((\(Ok ~contents) -> ~contents)
	 | (\(Err ~msg) -> (~panic (~Str .concat "fs error: " ~msg))))
	(~Fs .read_text_result "/tmp/input"))
```

### File Reading

#### `~read_text_result`
```lzscr
~read_text_result : Str -> Effect (Ok Str | Err Str)
```
Reads entire file as a string, returning `(Ok contents)` on success or `(Err msg)` on failure.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .read_text_result "/tmp/file.txt"))
```

---


### File Writing

#### `~write_text_result`
```lzscr
~write_text_result : Str -> Str -> Effect (Ok Unit | Err Str)
```
Writes text to file (creating or truncating), returning `(Ok ())` or `(Err msg)`.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .write_text_result "/tmp/out.txt" "data"))
```

---


### File Appending

#### `~append_text_result`
```lzscr
~append_text_result : Str -> Str -> Effect (Ok Unit | Err Str)
```
Appends text to file (creating if needed), returning `(Ok ())` or `(Err msg)`.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .append_text_result "/tmp/log.txt" "entry\n"))
```

---


### Directory Listing

#### `~list_dir_result`
```lzscr
~list_dir_result : Str -> Effect (Ok [Str] | Err Str)
```
Lists directory entries (filenames only), returning `(Ok entries)` or `(Err msg)`.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .list_dir_result "/tmp"))
```

---


### File Deletion

#### `~remove_file_result`
```lzscr
~remove_file_result : Str -> Effect (Ok Unit | Err Str)
```
Deletes file, returning `(Ok ())` or `(Err msg)`.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .remove_file_result "/tmp/old.txt"))
```

---


### Directory Creation

#### `~create_dir_result`
```lzscr
~create_dir_result : Str -> Effect (Ok Unit | Err Str)
```
Creates directory (including parent directories), returning `(Ok ())` or `(Err msg)`.

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .create_dir_result "/tmp/nested/dir"))
```

---


### File Metadata

#### `~metadata_result`
```lzscr
~metadata_result : Str -> Effect (Ok { size: Int, is_dir: Bool, is_file: Bool, readonly: Bool, modified_ms: (Some Int | None) } | Err Str)
```
Fetches file metadata record:
- `size`: File size in bytes (clamped to `i64::MAX`)
- `is_dir`: True if directory
- `is_file`: True if regular file
- `readonly`: True if read-only
- `modified_ms`: Last modified time as epoch milliseconds wrapped in `(Some Int | None)` (None when platform doesn't support timestamps)

**Example**:
```lzscr
(~Fs = (~require .effect .fs); (~Fs .metadata_result "/tmp/file.txt"))
```

---


## Implementation Notes
- All operations return `Effect` and must be sequenced with `~chain`.
- Underlying runtime effects: `!fs.read_text`, `!fs.write_text`, `!fs.append_text`, `!fs.list_dir`, `!fs.remove_file`, `!fs.create_dir`, `!fs.metadata`.
- Error messages are strings returned by the Rust `std::io::Error` implementation.
- File sizes > `i64::MAX` are clamped to `i64::MAX` in the metadata record.

## Related Modules
- `effect/io`: Basic print/println effects
- `effect/log`: Structured logging with levels and fields

## Testing
- Type inference tests: `crates/lzscr-types/tests/effects.rs`
- Integration tests: `crates/lzscr-cli/tests/cli.rs`
