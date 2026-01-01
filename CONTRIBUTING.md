# Contributing to lzscr

Thank you for your interest in contributing to lzscr! This document provides guidelines and instructions for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Pull Request Process](#pull-request-process)

## Code of Conduct

This project follows standard open-source community guidelines. Be respectful, constructive, and collaborative.

## Getting Started

### Prerequisites

- Rust 1.75+ (see `rust-toolchain.toml`)
- Git
- Familiarity with Rust and functional programming concepts

### Setup

```bash
# Clone the repository
git clone https://github.com/mako10k/lzscr.git
cd lzscr

# Build all crates
cargo build

# Run tests
cargo test

# Run clippy
cargo clippy --all-targets -- -D warnings

# Format code
cargo fmt --all
```

## Development Workflow

1. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make incremental changes**
   - Keep commits atomic and focused
   - Write descriptive commit messages
   - Run `cargo test` and `cargo clippy` frequently

3. **Before pushing**
   ```bash
   cargo fmt --all --check
   cargo clippy --all-targets -- -D warnings
   cargo test
   ```

4. **Push and create PR**
   ```bash
   git push origin feature/your-feature-name
   ```

## Coding Standards

**READ FIRST:** [Coding Standards and Refactoring Guidelines](docs/coding-standards.md)

### Quick Summary

- **File size limits**: 
  - Rust: 500 lines (recommended), 1000 (warning), 2000 (must split)
  - LazyScript: 200 lines (recommended), 300 (warning), 500 (must split)
  
- **Function complexity**:
  - Keep functions under 50 lines (recommended)
  - Split functions over 100 lines
  - Cognitive complexity under 15 (clippy threshold)

- **Code style**:
  - Follow `rustfmt.toml` settings (enforced by CI)
  - Use descriptive variable names (no `foo`, `bar`, `baz`)
  - Add rustdoc comments to public APIs

### Clippy Lints

We enforce `clippy::all` and additional lints via `clippy.toml`. Common exceptions:

- `#[allow(clippy::result_large_err)]` for `TypeError` (intentionally carries rich span data)
- `#[allow(dead_code)]` for experimental APIs (document reason)

**Never silence lints without a comment explaining why.**

### Module Organization

Large files (1000+ lines) should be split into multiple modules. See [coding-standards.md](docs/coding-standards.md#3-module-splitting-strategy) for detailed guidelines.

Example structure for `lzscr-types`:
```
src/
├── lib.rs          # Public API
├── types.rs        # Type definitions
├── inference.rs    # Type inference
├── unification.rs  # Unification algorithm
├── error.rs        # Error types
└── tests/          # Integration tests
```

## Testing

### Test Coverage

- **Minimum**: 70% overall coverage (enforced by CI)
- **Critical modules**: 80%+ coverage
  - `lzscr-types` (type inference)
  - `lzscr-parser` (parsing)
  - `lzscr-runtime` (evaluation)

### Test Types

1. **Unit tests**: In-module `#[cfg(test)]` blocks
2. **Integration tests**: `tests/` directory
3. **Golden tests**: `goldens/*.golden` for CLI output regression

### Running Tests

```bash
# All tests
cargo test

# Specific crate
cargo test -p lzscr-types

# With coverage
cargo llvm-cov --lcov --output-path lcov.info
```

### Writing Tests

**Good test structure:**
```rust
#[test]
fn unify_int_with_int_succeeds() {
    // Arrange
    let ty1 = Type::Int;
    let ty2 = Type::Int;
    
    // Act
    let result = unify(&ty1, &ty2);
    
    // Assert
    assert!(result.is_ok());
}
```

## Documentation

### Rustdoc

All public APIs must have doc comments:

```rust
/// Unifies two types with dual-span error reporting.
/// 
/// # Arguments
/// * `ty1` - First type to unify
/// * `sp1` - Span of first type
/// * `ty2` - Second type to unify
/// * `sp2` - Span of second type
/// 
/// # Errors
/// Returns `TypeError::Mismatch` when types are incompatible.
/// 
/// # Examples
/// ```
/// let result = unify_with_spans(&Type::Int, span1, &Type::Int, span2);
/// assert!(result.is_ok());
/// ```
pub fn unify_with_spans(
    ty1: &Type, sp1: Span,
    ty2: &Type, sp2: Span
) -> Result<(), TypeError> {
    // ...
}
```

### Comments

- **Why, not what**: Explain design decisions, not obvious code
- **TODOs**: Use `TODO:` with issue number if tracked
- **Complex algorithms**: Add brief explanation or link to paper/spec

## Pull Request Process

### Before Submitting

- [ ] Code follows [coding standards](docs/coding-standards.md)
- [ ] All tests pass (`cargo test`)
- [ ] Clippy passes (`cargo clippy --all-targets -- -D warnings`)
- [ ] Code is formatted (`cargo fmt --all`)
- [ ] Documentation is updated (if API changed)
- [ ] Tests are added for new functionality
- [ ] Coverage does not decrease by more than 5%

### PR Description Template

```markdown
## Summary
Brief description of changes

## Motivation
Why is this change needed?

## Changes
- Bullet list of changes
- Group related changes

## Testing
- How was this tested?
- What test cases were added?

## Checklist
- [ ] Tests pass
- [ ] Clippy clean
- [ ] Documentation updated
- [ ] No significant coverage decrease

## Related Issues
Fixes #123
Related to #456
```

### Review Process

1. **Automated checks**: CI must pass (7 checks: fmt, clippy, test, coverage, security, docs, deps)
2. **Code review**: At least one approval required
3. **Discussion**: Address reviewer feedback
4. **Merge**: Use "Rebase and merge" to maintain linear history

### After Merge

- Delete feature branch (automated for remote, manual for local)
- Monitor CI on `main` branch
- Address any post-merge issues promptly

## Specific Contribution Areas

### Type System

- Files: `crates/lzscr-types/src/`
- Focus: Type inference, unification, error reporting
- See: `docs/type-system.md`

### Parser

- Files: `crates/lzscr-parser/src/`
- Focus: Syntax parsing, AST construction
- See: `docs/spec/syntax.md`

### Runtime

- Files: `crates/lzscr-runtime/src/`
- Focus: Evaluation, builtins, effects
- See: `docs/spec/semantics.md`

### Standard Library

- Files: `stdlib/*.lzscr`
- Focus: Pure LazyScript implementations
- See: `stdlib/readme.md`

### CLI

- Files: `crates/lzscr-cli/src/`
- Focus: User interface, error display, flags
- See: `docs/tools/cli.md`

## Common Tasks

### Adding a New Error Type

1. Add variant to `TypeError` enum in `lzscr-types/src/lib.rs`
2. Implement display logic in error formatting functions
3. Add fix-it hint if applicable (see PR #32)
4. Add test cases for error triggering and display
5. Update documentation

### Adding a New Builtin

1. Add implementation in `lzscr-runtime/src/lib.rs` (builtins section)
2. Register in `mk_initial_env()` or `Builtins` record
3. Add type signature in stdlib prelude (if exposed)
4. Add tests (unit + integration)
5. Document in `stdlib/readme.md`

### Adding a New Syntax Feature

1. Update lexer (`lzscr-lexer`) if new tokens needed
2. Update parser (`lzscr-parser`) with new grammar rules
3. Update AST (`lzscr-ast`) with new node types
4. Update type checker (`lzscr-types`) with inference rules
5. Update evaluator (`lzscr-runtime`) with evaluation rules
6. Add comprehensive tests at each layer
7. Update language specification in `docs/spec/`

## Getting Help

- **Questions**: Open a GitHub Discussion
- **Bugs**: Open a GitHub Issue with reproduction steps
- **Feature requests**: Open a GitHub Issue with use case
- **Security**: See `SECURITY.md`

## Resources

- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- [Clippy Lint List](https://rust-lang.github.io/rust-clippy/master/)
- [Rustdoc Book](https://doc.rust-lang.org/rustdoc/)
- [lzscr Language Spec](docs/spec/overview.md)
- [lzscr ROADMAP](docs/ROADMAP.md)

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to lzscr! 🎉
