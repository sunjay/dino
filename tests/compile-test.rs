use std::io;
use std::fs;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

#[test]
fn compile_fail() -> io::Result<()> {
    // Pass the environment variable TESTCOMPILE=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTCOMPILE")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("tests/compile-fail");
    //TODO: Parallelize with rayon + remove duplication in this file
    for entry in tests_dir.read_dir()? {
        let entry = entry?;
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() == Some(OsStr::new("stderr")) {
            continue;
        }

        match compile(&entry_path) {
            Ok(exec_path) => {
                // Remove the generated executable
                fs::remove_file(&exec_path)?;

                panic!("Compile should have failed for '{}'", entry.path().display());
            },
            Err(stderr) => {
                // Check the stderr output against what's expected
                let stderr_file = entry_path.with_extension("stderr");

                if overwrite_expected_output {
                    fs::write(&stderr_file, &stderr)
                        .unwrap_or_else(|err| panic!("Failed to write expected output to '{}': {}", stderr_file.display(), err));
                    continue;
                }

                let expected_stderr = fs::read_to_string(&stderr_file)
                    .unwrap_or_else(|err| panic!("Failed to open '{}': {}", stderr_file.display(), err));

                if stderr != expected_stderr {
                    panic!("Error for '{}' did not match '{}'", entry_path.display(), stderr_file.display());
                }
            },
        }
    }

    Ok(())
}

#[test]
fn run_pass() -> io::Result<()> {
    // Pass the environment variable TESTCOMPILE=overwrite to overwrite the stderr files
    let overwrite_expected_output = env::var("TESTCOMPILE")
        .map(|val| val == "overwrite")
        .unwrap_or(false);

    let tests_dir = Path::new("tests/run-pass");
    //TODO: Parallelize with rayon + remove duplication in this file
    for entry in tests_dir.read_dir()? {
        let entry = entry?;
        let entry_path = entry.path();
        if entry_path.is_dir() || entry_path.extension() == Some(OsStr::new("stdout")) {
            continue;
        }

        match compile(&entry_path) {
            Ok(exec_path) => {
                // Test running the program
                let output = Command::new(exec_path).output()
                    .unwrap_or_else(|err| panic!("Failed to run program generated for '{}': {}", entry_path.display(), err));

                // Check the output of the generated program against what's expected
                let stdout_file = entry_path.with_extension("stdout");

                if overwrite_expected_output {
                    fs::write(&stdout_file, &output.stdout)
                        .unwrap_or_else(|err| panic!("Failed to write expected output to '{}': {}", stdout_file.display(), err));
                    continue;
                }

                let expected_stdout = fs::read_to_string(&stdout_file)
                    .unwrap_or_else(|err| panic!("Failed to open '{}': {}", stdout_file.display(), err));

                if output.stdout != expected_stdout.as_bytes() {
                    panic!("Output for '{}' did not match '{}'", entry_path.display(), stdout_file.display());
                }

                // Remove the generated executable
                //TODO: Use the tempfile crate to ensure that this file is actually reliably
                // deleted, even in scenarios when there is a panic
                fs::remove_file(&exec_path)?;
            },
            Err(_) => panic!("Compile failed for '{}'", entry_path.display()),
        }
    }

    Ok(())
}

/// Compiles a single file, returning the path to its executable if the compile succeeded and the
/// compiler error message if the compile failed.
fn compile(source_path: &Path) -> Result<&'static Path, String> {
    // The path to compile to
    let exec_path = "./a.out";

    //TODO: Don't hard code this path
    //TODO: Guarantee that this path will exist & be up to date before this call
    let output = Command::new("./target/debug/dinoc")
        .arg(source_path)
        .arg("-o")
        .arg(exec_path)
        .output()
        .unwrap_or_else(|err| panic!("Failed to run dinoc: {}", err));

    // Check if compile failed
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr)
            .unwrap_or_else(|err| panic!("Compiler stderr for '{}' was not valid UTF-8: {}", exec_path, err)));
    }

    Ok(Path::new(exec_path))
}
