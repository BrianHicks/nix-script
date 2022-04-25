use anyhow::{Context, Result};
use clap::Parser;
use directives::Directives;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};

/// Does the same thing as nix-script, but specializes some options for
/// scripts written in Haskell.
#[derive(Debug, Parser)]
#[clap(version, trailing_var_arg = true)]
struct Opts {
    /// Launch a ghcid session watching the script
    #[clap(long, conflicts_with("shell"))]
    ghcid: bool,

    /// Enter a shell with all script dependencies
    #[clap(long, conflicts_with("ghcid"))]
    shell: bool,

    /// In shell mode, run this command instead of a shell.
    #[clap(long, requires("shell"))]
    run: Option<String>,

    /// In shell mode, run a "pure" shell (that is, one that isolates the
    /// shell a little more from what you have in your environment.)
    #[clap(long, requires("shell"))]
    pure: bool,

    /// The script and args to pass to nix-script
    #[clap(min_values = 1, required = true)]
    script_and_args: Vec<String>,
}

impl Opts {
    fn run(&self) -> Result<ExitStatus> {
        let (script, args) = self
            .parse_script_and_args()
            .context("could not parse script and args")?;

        let directives = Directives::from_file("#!", &script)
            .context("could not parse directives from script")?;

        let mut command = Command::new("nix-script");

        let build_command = format!(
            "mv $SRC $SRC.hs; ghc {} -o $OUT $SRC.hs",
            directives
                .raw
                .get("ghcFlags")
                .map(|ps| ps.join(" "))
                .unwrap_or_else(|| String::from(" "))
        );
        log::debug!("build command is `{}`", build_command);
        command.arg("--build-command").arg(build_command);

        let compiler = format!(
            "haskellPackages.ghcWithPackages (ps: with ps; [ {} ])",
            directives
                .raw
                .get("haskellPackages")
                .map(|ps| ps.join(" "))
                .unwrap_or(String::default())
        );
        log::debug!("compiler is `{}`", &compiler);
        command.arg("--build-input").arg(compiler);

        if self.shell {
            log::debug!("entering shell mode");
            command.arg("--shell");
        } else if self.ghcid {
            log::debug!("entering ghcid mode");
            command
                .arg("--shell")
                .arg("--runtime-input")
                .arg("ghcid")
                .arg("--run")
                .arg(format!("ghcid {}", script.display()));
        }

        command.arg(script);
        command.args(args);

        let mut child = command
            .spawn()
            .context("could not call nix-script. Is it on the PATH?")?;

        child.wait().context("could not run the script")
    }

    fn parse_script_and_args(&self) -> Result<(PathBuf, Vec<String>)> {
        log::trace!("parsing script and args");
        let mut script_and_args = self.script_and_args.iter();

        let script = PathBuf::from(script_and_args.next().context("I need at least a script name to run, but didn't get one. This represents an internal error, and you should open a bug!")?);

        Ok((script, self.script_and_args[1..].to_vec()))
    }
}

fn main() {
    env_logger::Builder::from_env("NIX_SCRIPT_LOG").init();

    let opts = Opts::parse();
    log::trace!("opts: {:?}", opts);

    match opts.run().map(|status| status.code()) {
        Ok(Some(code)) => std::process::exit(code),
        Ok(None) => {
            log::warn!("we didn't receive an exit code; was the script killed with a signal?");
            std::process::exit(1)
        }
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1)
        }
    }
}
