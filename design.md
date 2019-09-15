# Disco Language

* High-level (not a systems programming language)
* Compiled
* Statically-typed (nominal type system)
* Strict evaluation
* Garbage collected

## Pipeline

1. Disco Source Code
2. AST (parsing)
3. Declarations (name resolution)
4. IR (type inference & checking)
5. C code (code generation)

## Primitive Types

* `bool` - boolean (`true` or `false`)
* `int` - integer (future: dynamically-sized?)
* `real` - floating-point number
* `bstr` - byte string
* `[T]` - list (dynamically-sized)
* `set<T>` - hash set
* `map<K, V>` - hash map
* `(T, U, V)` - tuple (zero or more types, statically-sized)
  * `()` - unit type

Literals:

* integers: `1`, `2`, `-4`, `34950`
* real numbers: `1`, `3.14`, `-1e-3` (notice that integers can be used)
* byte string: `b"cool stuff"` (notice the `b` prefix for forward compat)
* list: `[a, b, c]` or `[x; len]`
* hash map: `map!{b"key1": val1, b"key2": val2}`
* hash set: `set!{val1, val2}`
* tuple: `()`, `(val1, val2)`, `(val1, val2, val3)`
  * member access: `t.0`, `t.1 = val`

## Standard Library

Rust dynamic library linked with the compiled code.

## Features/Syntax

* Variable declaration: `let name: type = value;`
  * Variable shadowing
  * Assignment operator: `name = value;`
* Function declaration: `fn name(arg1: ty1, arg2: ty2) -> ty { body }`
  * Entry point: `fn main() { body }`
  * Multiple declarations with the same fully-qualified name are not allowed
  * Declaration after use: no forward declarations necessary
* Function calls: `name(val1, val2)`
* Boolean Operators:
  * Not: `!a`
  * And: `a && b`
  * Or: `a || b`
* Numeric Operators:
  * Unary negative: `-a`
  * Unary positive: `+a`
  * Addition: `a + b`
  * Subtraction: `a - b`
  * Multiplication: `a * b`
  * Division: `a / b`
  * Remainder: `a % b`
  * Bitwise operators
* Other Operators:
  * Field/Method Access: `val.field`, `val.method()`
  * Index: `a[b]`, `a[b] = c`
  * Range: `..`, `..=` (not overloadable)
* Blocks: `{ stmt1; stmt2; expr }`
* Pattern matching
* Loops:
  * `for pat in expr { body }`
  * `while (expr) { body }`
  * `while let pat = expr { body }`
* Conditions:
  * `if (expr) { body }`
  * `if (expr) { body } else { body2 }`
  * `if (expr) { body } else if (expr2) { body2 } else { body3 }`
* Comments:
  * Single-line: `// This is a comment!`
  * Multi-line: `/* This comment can be /* nested */ and span multiple lines */`

## Future Features

* Pull-based compiler (query-response architecture)
* structs, enums, and methods
* Module system
  * Item privacy (`pub`)
* Panic / Stack unwinding
* Trait system
* Generators
* Concurrency
* Async/Await

## Syntax Examples

### Minimal Program

```
fn main() {}
```

Error case:

```
// Error: no main function
```

### Variable & Integer Literal

```
fn main() {
    // No type inference yet
    let x: int = 3;
}
```

Error case:

```
fn main() {
    // Error: mismatched types
    let x: int = true;
}
```

### Non-trivial Program

```
fn main() {
    let x: int = 3;

    for i in (x..0).step_by(-1) {
        println!("{}", i);
    }
}
```
