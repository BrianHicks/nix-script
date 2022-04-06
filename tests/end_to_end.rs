use assert_cmd::Command;

fn bin() -> Command {
    Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap()
}

mod sample_scripts {
    use super::*;

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

    #[test]
    fn forwards_success_code() {
        let assert = bin().arg("tests/exit-with-code.sh").arg("0").assert();

        assert.success();
    }

    #[test]
    fn forwards_error_code() {
        let assert = bin().arg("tests/exit-with-code.sh").arg("1").assert();

        assert.code(1);
    }

    #[test]
    fn forwards_custom_code() {
        let assert = bin().arg("tests/exit-with-code.sh").arg("32").assert();

        assert.code(32);
    }

    #[test]
    fn forwards_stdin() {
        let assert = bin()
            .arg("tests/echo.sh")
            .write_stdin("Hello, World!")
            .assert();

        assert.success().stdout("Hello, World!");
    }
}
