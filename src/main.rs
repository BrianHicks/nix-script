mod derivation;
mod directives;
mod expr;

use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[clap(version, trailing_var_arg = true)]
struct Opts {
    /// What indicator do directives start with in the source file?
    #[clap(long, default_value = "#!")]
    indicator: String,

    /// How should we build this script? (Will override any `#!build` line
    /// present in the script.)
    #[clap(long("build"))]
    build_command: Option<String>,

    #[clap(long("interpreter"))]
    interpreter: Option<String>,

    /// The script to run, plus any arguments. Any positional arguments after
    /// the script name will be passed on to the script.
    // Note: it'd be better to have a "script" and "args" field separately,
    // but there's a parsing issue in Clap (not a bug, but maybe a bug?) that
    // prevents passing args starting in -- after the script if we do that. See
    // https://github.com/clap-rs/clap/issues/1538
    #[clap(min_values = 1)]
    script_and_args: Vec<String>,
}

impl Opts {
    fn run(&self) -> Result<()> {
        let script = PathBuf::from(self.script_and_args.get(0).context("we already validated that we had at least the script in script_and_args, but couldn't read it. Please file a bug!")?);

        let source = fs::read_to_string(&script).context("could not read script")?;

        let directives = Directives::parse(&self.indicator, &source)
            .context("could not construct a directive parser")?;

        let build_command = if let Some(from_opts) = &self.build_command {
            from_opts
        } else if let Some(from_directives) = directives.build_command {
            from_directives
        } else {
            anyhow::bail!("Need a build command, either by specifying a `build` directive or passing the `--build` option.")
        };

        let mut derivation =
            Derivation::new(&script, build_command).context("could not create a Nix derivation")?;
        derivation.add_build_inputs(directives.build_inputs);
        derivation.add_runtime_inputs(directives.runtime_inputs);

        if let Some(from_opts) = &self.interpreter {
            derivation
                .set_interpreter(from_opts)
                .context("could not set interpreter from command-line flags")?
        } else if let Some(from_directives) = directives.interpreter {
            derivation
                .set_interpreter(from_directives)
                .context("could not set interpreter from file directives")?
        };

        println!("{}", derivation);

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
