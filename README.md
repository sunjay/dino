# dino

Programming language / compiler experiments.

## Building & Running

You need to have the `dino-runtime` library built before building the compiler.
To build the runtime, use the command:

```sh
cargo build --manifest-path dino-runtime/Cargo.toml
```

Once that is built, you can use the following command to build and run the
compiler:

```sh
cargo run -- path/to/program.dino
```

When working on the runtime, it is recommended to use the following command in
order to make sure everything is always kept up-to-date:

```sh
cargo build -p dino-runtime && cargo run -- path/to/program.dino
```
