use assert_cmd::Command;
use std::path::PathBuf;

fn run_test<A>(asserter: A)
where
    A: FnOnce(&mut Command),
{
    let nix_script_bin = PathBuf::from(env!("CARGO_BIN_EXE_nix-script-haskell"))
        .parent()
        .unwrap()
        .join("nix-script");

    // `cargo test` should have automatically created this binary. If not,
    // we'd better bail early!
    assert!(nix_script_bin.exists());

    let mut command = Command::cargo_bin(env!("CARGO_BIN_EXE_nix-script-haskell")).unwrap();

    command.arg("--nix-script-bin").arg(nix_script_bin);

    asserter(&mut command);
}

#[test]
fn hello_world() {
    run_test(|cmd| {
        cmd.arg("sample-scripts/hello-world.hs")
            .assert()
            //
            .success()
            .stdout("Hello, World!\n");
    });
}

#[test]
fn ghc_flags() {
    run_test(|cmd| {
        cmd.arg("sample-scripts/ghc-flags.hs")
            .assert()
            //
            .success()
            .stdout("Success! Bound threads are supported\n");
    });
}

#[test]
fn no_extension() {
    run_test(|cmd| {
        cmd.arg("sample-scripts/no-extension")
            .assert()
            //
            .success()
            .stdout("Hello, World!\n");
    });
}

#[test]
fn relude() {
    run_test(|cmd| {
        cmd.arg("sample-scripts/relude.hs")
            .assert()
            //
            .success()
            .stdout("Hello, World!\n");
    });
}
