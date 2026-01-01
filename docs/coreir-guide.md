# CoreIR Developer Guide

Last Updated: 2025-12-26

This guide provides practical information for working with the Core IR (Intermediate Representation) module.

## Overview

CoreIR is a small, lambda-calculus-based intermediate representation used for:
- Lowering high-level AST expressions to a simpler form
- Proof-of-concept evaluation independent of the main runtime
- Experimentation with alternative evaluation strategies

**Status**: Proof of Concept (PoC) - prioritizes correctness and clarity over performance.

## Core IR Structure

### Term Types

The IR consists of `Term` objects wrapping an `Op` enum:

```rust
pub enum Op {
    // Literals
    Unit,
    Int(i64),
    Float(f64),
    Str(String),
    Char(i32),
    Bool(bool),
    
    // References and symbols
    Ref(String),      // Variable reference (e.g., "x", "add")
    Symbol(String),   // Bare symbol for special forms
    
    // Lambda calculus core
    Lam { param: String, body: Box<Term> },
    App { func: Box<Term>, arg: Box<Term> },
    
    // Control flow
    Seq { first: Box<Term>, second: Box<Term> },     // Sequence: eval first, then second
    Chain { first: Box<Term>, second: Box<Term> },   // Chain: both in effect context
    Bind { value: Box<Term>, cont: Box<Term> },      // Monadic bind
    
    // Let bindings
    LetRec { bindings: Vec<(String, Term)>, body: Box<Term> },  // Recursive let
}
```

### Convenience Constructors

Helper methods make term construction cleaner:

```rust
// Instead of:
Term::new(Op::Int(5))

// Use:
Term::int(5)

// Full list of helpers:
Term::int(n: i64) -> Term
Term::float(f: f64) -> Term
Term::string(s: impl Into<String>) -> Term
Term::var(name: impl Into<String>) -> Term
Term::lambda(param: impl Into<String>, body: Term) -> Term
Term::app(func: Term, arg: Term) -> Term
Term::letrec(bindings: Vec<(String, Term)>, body: Term) -> Term
```

## Lowering from AST

The `lower_expr_to_core` function transforms AST expressions to CoreIR terms:

### Key Transformations

**Let Groups** → **LetRec**:
```
AST: (~x = 1; ~y = 2; ~x + ~y)
CoreIR: (letrec { ~x = 1; ~y = 2; } (~add ~x ~y))
```

**Sequence** → **Seq**:
```
AST: (~seq a b)
CoreIR: Seq { first: a, second: b }
```

**Chain** → **Chain**:
```
AST: (~chain a b)
CoreIR: Chain { first: a, second: b }
```

**Alternative Lambdas** → **Alt Application**:
```
AST: (\~x -> e1) | (\~y -> e2)
CoreIR: \~z -> ((~alt (\~x -> e1)) (\~y -> e2)) ~z
```

## Evaluation

### PoC Evaluator

The `eval_term` function provides a small-step evaluator:

```rust
pub fn eval_term(t: &Term) -> Result<IrValue, IrEvalError>
```

**Supported**:
- ✅ Integer and float arithmetic (add, sub, mul, div)
- ✅ Lambda abstraction and application
- ✅ Recursive let bindings (LetRec)
- ✅ Sequence operations (Seq, Chain, Bind)
- ✅ Builtin functions (limited set)

**Not Supported**:
- ❌ Full runtime standard library
- ❌ Advanced pattern matching in evaluator
- ❌ Thunks and lazy evaluation (uses eager evaluation)
- ❌ Exception handling in evaluator

### Environment Model

The evaluator maintains an environment `HashMap<String, IrValue>`:

```rust
// Variable binding (without ~ prefix)
env.insert("x", IrValue::Int(5));

// Reference lookup (without ~ prefix)
env.get("x") // Returns Some(IrValue::Int(5))
```

**Important**: Variable names in the environment don't include the `~` prefix. The prefix is used in lambda parameters and references in the IR, but stripped during environment operations.

## Writing Tests

### Basic Test Pattern

```rust
#[test]
fn test_my_feature() {
    // Build IR using convenience constructors
    let expr = Term::letrec(
        vec![("x".into(), Term::int(5))],
        Term::app(
            Term::app(Term::var("add"), Term::var("x")),
            Term::int(3)
        )
    );
    
    // Evaluate
    let result = eval_term(&expr).expect("evaluation failed");
    
    // Verify
    assert_eq!(print_ir_value(&result), "8");
}
```

### Testing LetRec

When testing LetRec, remember:
1. Binding names don't include `~` prefix
2. References to bindings don't include `~` prefix
3. Lambda parameters include `~` prefix in IR, but it's stripped in environment

```rust
// Correct:
let bindings = vec![("x".into(), Term::int(1))];
let body = Term::var("x");  // Reference without ~

// Incorrect:
let bindings = vec![("~x".into(), Term::int(1))];  // Wrong! No ~ in binding name
```

## Common Patterns

### Building Function Application Chains

```rust
// For: (~add (~mul 2 3) 1)
let expr = Term::app(
    Term::app(
        Term::var("add"),
        Term::app(
            Term::app(Term::var("mul"), Term::int(2)),
            Term::int(3)
        )
    ),
    Term::int(1)
);
```

### Nested LetRec

```rust
// For: (letrec { ~x = (letrec { ~y = 2; } ~y); } ~x)
let inner = Term::letrec(
    vec![("y".into(), Term::int(2))],
    Term::var("y")
);
let outer = Term::letrec(
    vec![("x".into(), inner)],
    Term::var("x")
);
```

## Debugging Tips

### Pretty Printing

Use `print_term` to see the IR structure:

```rust
let term = Term::int(42);
println!("{}", print_term(&term));  // Output: 42
```

### Evaluation Errors

Common `IrEvalError` variants:
- `Unbound(String)` - Variable not found in environment
- `NotFunction(IrValue)` - Trying to apply a non-function
- `Arity(String)` - Builtin called with wrong number of arguments
- `UnsupportedParam(String)` - Lambda parameter pattern not supported

## Performance Notes

The PoC evaluator uses:
- **Eager evaluation** - Arguments evaluated before function application
- **Cloning** - Liberal use of `.clone()` for simplicity
- **No optimization** - Direct interpretation without compilation

Production-grade performance work is explicitly deferred to future phases.

## Future Work

Planned improvements (see ROADMAP.md):
1. Harden exception/alt/catch representation
2. Extend evaluator coverage for more language constructs
3. Add thunk support for true lazy evaluation
4. Optimize evaluation strategy (future research)

## References

- Main implementation: `crates/lzscr-coreir/src/lib.rs`
- Tests: Lines 548+ in same file
- Lowering: `lower_expr_to_core` function
- Evaluation: `eval_term` and `eval_term_with_env` functions
- ROADMAP: `docs/ROADMAP.md` (lines 23-25)

