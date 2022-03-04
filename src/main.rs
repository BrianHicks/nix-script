mod directive;

use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[clap(version, trailing_var_arg = true)]
struct Opts {
    /// The script to run
    script: PathBuf,

    /// Args to pass on to the script
    script_args: Vec<String>,
}

impl Opts {
    fn run(&self) -> Result<()> {
        println!("{:#?}", self);
        Ok(())
    }
}

fn main() {
    let opts = Opts::parse();

    if let Err(err) = opts.run() {
        eprintln!("{:?}", err);
        std::process::exit(1)
    }
}
