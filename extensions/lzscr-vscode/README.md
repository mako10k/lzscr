# lzscr VS Code Extension

Provides basic syntax highlighting and formatting for `.lzscr` files by invoking `lzscr-cli --format-code`.

## Features
- Language id `lzscr` for `*.lzscr`
- Syntax highlighting (basic)
- Format Document using `lzscr-cli`

## Requirements
- `lzscr-cli` must be available in PATH (built from this repository)

## Settings
- `lzscr.formatterPath`: path to the CLI (default: `lzscr-cli`)
- `lzscr.maxWidth`: hint for formatter max width (not yet used by CLI)

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