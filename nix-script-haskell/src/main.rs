use anyhow::Result;
use clap::Parser;
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
        anyhow::bail!("no")
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
