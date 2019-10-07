# dino

Programming language / compiler experiments.

## Building & Running

You need to have the `dino-runtime` and `dino-std` libraries built before
building the compiler. To build each of them, use the commands:

```sh
cargo build --manifest-path dino-runtime/Cargo.toml
cargo build --manifest-path dino-std/Cargo.toml
```

Once those are built, you can use the following command to build and run the
compiler:

```sh
cargo run -- path/to/program.dino
```

When working on the runtime, it can be useful to use the following command to
make sure everything is always kept up-to-date:

```sh
cargo build --manifest-path dino-runtime/Cargo.toml && \
    cargo build --manifest-path dino-std/Cargo.toml && \
    cargo run -- path/to/program.dino
```

## Running Tests

To run the tests, use:

```sh
cargo test
```

You can set `TESTCOMPILE=overwrite` to overwrite the `stdout` and `stderr`
files. This should only be used when the compiler's output is modified or if
a test's output is modified.

```sh
TESTCOMPILE=overwrite cargo test
```
