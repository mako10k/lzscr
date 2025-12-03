# Project Context

## Project: lzscr - Functional Programming Language

**Language**: Rust  
**Type**: Programming language implementation (compiler/interpreter)  
**Architecture**: Multi-crate workspace

## Crate Structure

### Core AST & Parsing
- **lzscr-ast**: Abstract syntax tree definitions
- **lzscr-lexer**: Tokenization
- **lzscr-parser**: Parser (AST construction)
- **lzscr-preast**: Pre-AST intermediate representation

### Type System & Analysis
- **lzscr-types**: Type inference and checking
- **lzscr-analyzer**: Static analysis (unused vars, shadowing, duplicates)

### Compilation & Runtime
- **lzscr-coreir**: Core intermediate representation
- **lzscr-runtime**: Expression evaluation
- **lzscr-format**: Code formatting

### User Interface
- **lzscr-cli**: Command-line interface

## Current Focus: Phase 5 - Record Field Spans

**Branch**: `feature/diagnostics-phase5-record-field-spans` (if created) or `main`

**Goal**: Improve error diagnostics by tracking precise spans for record field names

**Progress**: Step 7 of 9 (66% when complete)

## Recent Commits

- `526024f`: Phase 5 Step 1-6 - ExprKind::Record field spans (completed)

## Key Files for Current Work

- `crates/lzscr-ast/src/lib.rs` - AST definitions
- `crates/lzscr-parser/src/lib.rs` - Parsing logic
- `crates/lzscr-analyzer/src/lib.rs` - Static analysis
- `crates/lzscr-types/src/inference/*.rs` - Type inference
- `crates/lzscr-cli/src/main.rs` - CLI implementation

## Testing

- **Test Suite**: 113 tests across all crates
- **Command**: `cargo test`
- **Requirement**: All tests must pass before commit

## Workflow

1. Make AST changes in `lzscr-ast`
2. Update parser in `lzscr-parser`
3. Fix compilation errors in dependent crates
4. Run `cargo test` to validate
5. Commit with descriptive message

## Team Preferences

- **Language**: Japanese for comments and documentation
- **Commit Style**: Conventional commits with detailed body
- **Testing**: Test-driven with 100% pass requirement
