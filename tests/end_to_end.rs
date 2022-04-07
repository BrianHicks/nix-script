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
}

mod io_behavior {
    use super::*;
    use std::os::unix::fs::symlink;
    use std::path::PathBuf;
    use tempfile::tempdir;

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

    #[test]
    fn gc_safety() {
        let temp = tempdir().unwrap();

        // run once to set up the cache
        bin()
            .env("NIX_SCRIPT_CACHE", temp.path().display().to_string())
            .arg("tests/exit-with-code.sh")
            .arg("0")
            .assert()
            .success();

        // mess with the symlink to make it point to an invalid destination. Note
        // that we can't use the more ergonomic `DirEntry.path()` here because
        // it traverses symlinks.
        let mut cache_entries = std::fs::read_dir(temp.path()).unwrap();
        let filename = cache_entries.next().unwrap().unwrap().file_name();
        let link = temp.path().join(filename);
        std::fs::remove_file(&link).unwrap();
        symlink(PathBuf::from("garbage"), &link).unwrap();

        // run the command again to make sure we handle the newly-bad link
        bin()
            .env("NIX_SCRIPT_CACHE", temp.path().display().to_string())
            .arg("tests/exit-with-code.sh")
            .arg("0")
            .assert()
            .success();
    }

    #[test]
    fn include_runtime_file() {
        bin()
            .arg("tests/with_runtime_file/script.sh")
            .assert()
            .success()
            .stdout("Hello, World!\n");
    }
}
