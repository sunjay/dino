# disco

Programming language / compiler experiments.

## Building & Running

You need to have the `disco-runtime` library built before building the compiler.
You can build both using the command:

```sh
cargo build --all
```

This usually takes care of building everything in the right order
(`disco-runtime` before `disco`). If it doesn't, you can explicitly build
the runtime using the command:

```sh
cargo build -p disco-runtime
```

Once that is built, you can use the following command to build and run the
compiler:

```sh
cargo run -- path/to/program.disco
```

When working on the runtime, it is recommended to use the following command in
order to make sure everything is always kept up-to-date:

```sh
cargo build -p disco-runtime && cargo run -- path/to/program.disco
```
