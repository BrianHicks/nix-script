mod derivation;
mod directives;
mod expr;

use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};

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

    /// Instead of executing the script, parse directives from the file and
    /// print them as JSON to stdout
    #[clap(long("parse"), conflicts_with("export"))]
    parse: bool,

    /// Instead of executing the script, print the derivation we'd build
    /// to stdout
    #[clap(long("export"), conflicts_with("parse"))]
    export: bool,

    /// Use this folder as the root for any building we do. You can use this
    /// to bring other files into scope in your build. If there is a `default.nix`
    /// file in the specified root, we will use that instead of generating our own.
    #[clap(long)]
    root: Option<PathBuf>,

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
        let (script, _args) = self
            .parse_script_and_args()
            .context("could not parse script and args")?;

        let source = fs::read_to_string(&script).context("could not read script")?;

        let directives = Directives::parse(&self.indicator, &source)
            .context("could not construct a directive parser")?;

        if self.parse {
            println!(
                "{}",
                serde_json::to_string(&directives).context("could not serialize directives")?
            );
            std::process::exit(0);
        }

        let (root, target) = self
            .isolate_script(&script)
            .context("could not get an isolated build root for script")?;

        log::error!("{}, {}", root.display(), target.display());

        let derivation = self
            .derivation(&script, directives)
            .context("could not generate derivation")?;

        if self.export {
            println!("{}", derivation);
            std::process::exit(0);
        }

        Ok(())
    }

    fn parse_script_and_args(&self) -> Result<(PathBuf, Vec<String>)> {
        log::trace!("parsing script and args");
        let mut script_and_args = self.script_and_args.iter();

        let script = PathBuf::from(script_and_args.next().context("I need at least a script name to run, but didn't get one. Please pass that as the first positional argument and try again!")?);

        Ok((script, self.script_and_args[1..].to_vec()))
    }

    fn isolate_script(&self, script: &Path) -> Result<(PathBuf, PathBuf)> {
        if let Some(root) = &self.root {
            let from_root = script.strip_prefix(root).context("could not find a path from the provided root to the script file (root must contain script)")?;
            log::debug!(
                "calculated script path from root `{}` as `{}`",
                root.display(),
                from_root.display()
            );

            Ok((root.to_owned(), from_root.to_owned()))
        } else {
            let target_name = script
                .file_name()
                .context("could not get file name from script name")?;

            let tempdir = tempfile::Builder::new()
                .prefix("nix-script-")
                .tempdir_in("todo")
                .context("could not create temporary directory")?
                .into_path();

            std::fs::copy(script, tempdir.join(target_name))
                .context("could not copy script to temporary directory")?;

            Ok((tempdir, target_name.into()))
        }
    }

    fn derivation(&self, script: &Path, directives: Directives) -> Result<Derivation> {
        let build_command = if let Some(from_opts) = &self.build_command {
            log::debug!("using build command from opts");
            from_opts
        } else if let Some(from_directives) = directives.build_command {
            log::debug!("using build command from directives");
            from_directives
        } else {
            anyhow::bail!("Need a build command, either by specifying a `build` directive or passing the `--build` option.")
        };

        let mut derivation =
            Derivation::new(script, build_command).context("could not create a Nix derivation")?;

        log::trace!("adding build inputs");
        derivation.add_build_inputs(directives.build_inputs);

        log::trace!("adding runtime inputs");
        derivation.add_runtime_inputs(directives.runtime_inputs);

        if let Some(from_opts) = &self.interpreter {
            log::debug!("using interpreter from opts");
            derivation
                .set_interpreter(from_opts)
                .context("could not set interpreter from command-line flags")?
        } else if let Some(from_directives) = directives.interpreter {
            log::debug!("using interpreter from directives");
            derivation
                .set_interpreter(from_directives)
                .context("could not set interpreter from file directives")?
        } else {
            log::trace!("not using an interpreter")
        };

        Ok(derivation)
    }
}

fn main() {
    env_logger::Builder::from_env("NIX_SCRIPT_LOG").init();

    let opts = Opts::parse();
    log::trace!("opts: {:?}", opts);

    if let Err(err) = opts.run() {
        eprintln!("{:?}", err);
        std::process::exit(1)
    }
}
