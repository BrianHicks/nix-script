use assert_cmd::Command;

fn bin() -> Command {
    Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap()
}

#[test]
fn hello_world_py() {
    let assert = bin().arg("sample-scripts/hello-world.py").assert();

    assert.success().stdout("Hello, World!\n");
}

#[test]
fn hello_world_hs() {
    let assert = bin().arg("sample-scripts/hello-world.hs").assert();

    assert.success().stdout("Hello, World!\n");
}

#[test]
fn jq_sh() {
    let assert = bin().arg("sample-scripts/jq.sh").assert();

    assert.success().stdout("Hello, World!\n");
}
