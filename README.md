# dino

Programming language / compiler experiments.

## Dependencies

This project uses the [Boehm-Demers-Weiser conservative garbage collector][gc].
To build this project, you will need to clone and build that library.

The commands below should be run in the root directory of this project (the
location of the main `Cargo.toml` file).

```bash
$ git clone git://github.com/ivmai/bdwgc.git
$ cd bdwgc
$ ./autogen.sh
$ ./configure --enable-static
$ make -j
```

Notice that `--enable-static` is used to ensure that `libgc.a` is produced by
the build.

[gc]: https://www.hboehm.info/gc

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
