# lzscr VS Code Extension

Provides syntax highlighting and automatic formatting for `.lzscr` files.

## Features
- Language id `lzscr` for `*.lzscr`
- Syntax highlighting with TextMate grammar
- Format Document command using `lzscr-cli --format-code`
- Automatic space insertion for lzscr syntax (`.` `~` `!` symbols)
- Configurable indentation and line width

## Requirements
- `lzscr-cli` must be available in PATH (build from this repository with `cargo build -p lzscr-cli`)

## Settings
- `lzscr.formatterPath`: Path to lzscr CLI (default: `lzscr-cli`)
- `lzscr.maxWidth`: Maximum line width for formatting (default: 100)
- `lzscr.indent`: Indent size in spaces (default: 2)

## Usage
1. Build `lzscr-cli`: `cargo build -p lzscr-cli --release`
2. Ensure `lzscr-cli` is in PATH or configure `lzscr.formatterPath`
3. Open a `.lzscr` file and run "Format Document" (Shift+Alt+F)

## Development
- Install dependencies:
```sh
npm install
```
- Build/watch:
```sh
npm run watch
```
- Package VSIX:
```sh
npm run package
```

Then install the VSIX in VS Code via `Extensions` > `…` > `Install from VSIX…`.

## Changelog

### 0.0.5 (2025-12-03)
- Add syntax highlighting for `^(` raise operator
- Add type annotation syntax: `%{...}` and `%TypeName`
- Add reserved token highlighting: `@`, `$`
- Add hex/binary number support
- Add char literal escape sequences
- Keyword additions: `handle`, `resume`
- Complete grammar alignment with syntax.md spec

### 0.0.4 (2025-12-03)
- Add syntax highlighting for Float operators (`.+`, `.-`, `.*`, `./`)
- Add syntax highlighting for Float comparison operators (`.<`, `.<=`, `.>`, `.>=`)
- Add syntax highlighting for `%` type symbol
- Add syntax highlighting for `<-` bind operator
- Distinguish constructors (`True`, `False`, `Some`) from methods
- Improved TextMate grammar alignment with current language spec

### 0.0.3 (2025-12-03)
- Fix space insertion rules for lzscr syntax
- Proper handling of `.` `~` `!` symbol spacing
- Updated formatter to respect token binding rules

### 0.0.2
- Initial formatter integration
- Basic syntax highlighting

### 0.0.1
- Initial release