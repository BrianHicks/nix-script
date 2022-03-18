mod clean_path;
mod derivation;
mod directives;
mod expr;

use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use clap::Parser;
use clean_path::clean_path;
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

    /// Where should we cache files?
    #[clap(long("cache-directory"), env("NIX_SCRIPT_CACHE"))]
    cache_directory: Option<PathBuf>,

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
        // First things first: what are we running? Where does it live? What
        // are its arguments?
        let (mut script, _args) = self
            .parse_script_and_args()
            .context("could not parse script and args")?;
        script = clean_path(&script).context("could not clean path to script")?;

        let source = fs::read_to_string(&script).context("could not read script")?;

        let directives = Directives::parse(&self.indicator, &source)
            .context("could not construct a directive parser")?;

        // First place we might bail early: if a script just wants to parse
        // directives using our parser, we dump JSON and quit instead of running.
        if self.parse {
            println!(
                "{}",
                serde_json::to_string(&directives).context("could not serialize directives")?
            );
            return Ok(());
        }

        // We check here instead of inside isolate_script or similar so we
        // can get an early bail that doesn't create trash in the system's
        // temporary directories.
        if self.export && self.root.is_none() {
            anyhow::bail!(
                "I don't have a root to refer to while exporting, so I can't isolate the script and dependencies. Specify a --root and try this again!"
            )
        }

        // TODO: create hash, check cache. If we've got a hit, proceed to the
        // last TODO in here.
        let cache_directory = self
            .get_cache_directory()
            .context("couldn't get cache directory")?;
        log::debug!(
            "using `{}` as the cache directory",
            cache_directory.display()
        );

        // TODO: this goes in the big `if` eventually
        let (root, target) = self
            .isolate_script(&script)
            .context("could not get an isolated build root for script")?;

        let derivation = self
            .derivation(&root, &target, directives)
            .context("could not generate derivation")?;

        // Second place we can bail early: if someone wants the generated
        // derivation to do IFD or similar
        if self.export {
            println!("{}", derivation);
            return Ok(());
        }

        // TODO: figure out which `default.nix` we want
        // TODO: run `nix-build` and get the store path

        // TODO: run the executable with the given args

        std::fs::write(cache_directory.join("default.nix"), derivation.to_string())
            .context("could not write default.nix")?;
        Ok(())
    }

    fn parse_script_and_args(&self) -> Result<(PathBuf, Vec<String>)> {
        log::trace!("parsing script and args");
        let mut script_and_args = self.script_and_args.iter();

        let script = PathBuf::from(script_and_args.next().context("I need at least a script name to run, but didn't get one. Please pass that as the first positional argument and try again!")?);

        Ok((script, self.script_and_args[1..].to_vec()))
    }

    fn isolate_script(&self, script: &Path) -> Result<(PathBuf, PathBuf)> {
        if let Some(raw_root) = &self.root {
            let root = clean_path(raw_root).context("could not clean path to root")?;

            let from_root = script.strip_prefix(&root).context("could not find a path from the provided root to the script file (root must contain script)")?;
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
                .tempdir_in(
                    self.get_cache_directory()
                        .context("could not get the cache directory")?,
                )
                .context("could not create temporary directory")?
                .into_path();

            std::fs::copy(script, tempdir.join(target_name))
                .context("could not copy script to temporary directory")?;

            Ok((tempdir, target_name.into()))
        }
    }

    fn derivation(&self, root: &Path, script: &Path, directives: Directives) -> Result<Derivation> {
        let build_command = if let Some(from_opts) = &self.build_command {
            log::debug!("using build command from opts");
            from_opts
        } else if let Some(from_directives) = directives.build_command {
            log::debug!("using build command from directives");
            from_directives
        } else {
            anyhow::bail!("Need a build command, either by specifying a `build` directive or passing the `--build` option.")
        };

        let mut derivation = Derivation::new(root, script, build_command)
            .context("could not create a Nix derivation")?;

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

    fn get_cache_directory(&self) -> Result<PathBuf> {
        let target = match &self.cache_directory {
            Some(explicit) => explicit.to_owned(),
            None => {
                let dirs = directories::ProjectDirs::from("zone", "bytes", "nix-script").context(
                    "couldn't load HOME (set --cache-directory explicitly to get around this.)",
                )?;

                dirs.cache_dir().to_owned()
            }
        };

        if !target.exists() {
            std::fs::create_dir_all(&target).context("could not create cache directory")?;
        }

        Ok(target)
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
