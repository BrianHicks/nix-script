#[warn(clippy::cargo)]
mod builder;
mod clean_path;
mod derivation;

use crate::builder::Builder;
use anyhow::{Context, Result};
use clap::Parser;
use clean_path::clean_path;
use directives::Directives;
use std::fs;
use std::io::ErrorKind;
use std::os::unix::fs::symlink;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};

// TODO: options for the rest of the directives
#[derive(Debug, Parser)]
#[clap(version, trailing_var_arg = true)]
struct Opts {
    /// What indicator do directives start with in the source file?
    #[clap(long, default_value = "#!")]
    indicator: String,

    /// How should we build this script? (Will override any `#!build` line
    /// present in the script.)
    #[clap(long)]
    build_command: Option<String>,

    /// Add build inputs to those specified by the source directives.
    #[clap(long("build-input"))]
    build_inputs: Vec<String>,

    #[clap(long("interpreter"))]
    interpreter: Option<String>,

    /// Add runtime inputs to those specified by the source directives.
    #[clap(long("runtime-input"))]
    runtime_inputs: Vec<String>,

    /// Instead of executing the script, parse directives from the file and
    /// print them as JSON to stdout
    #[clap(long("parse"), conflicts_with_all(&["export", "shell"]))]
    parse: bool,

    /// Instead of executing the script, print the derivation we'd build
    /// to stdout
    #[clap(long("export"), conflicts_with_all(&["parse", "shell"]))]
    export: bool,

    /// Enter a shell with the build-time and runtime inputs available.
    #[clap(long, conflicts_with_all(&["parse", "export"]))]
    shell: bool,

    /// In shell mode, run this command instead of a shell.
    #[clap(long, requires("shell"))]
    run: Option<String>,

    /// In shell mode, run a "pure" shell (that is, one that isolates the
    /// shell a little more from what you have in your environment.)
    #[clap(long, requires("shell"))]
    pure: bool,

    /// Use this folder as the root for any building we do. You can use this
    /// to bring other files into scope in your build. If there is a `default.nix`
    /// file in the specified root, we will use that instead of generating our own.
    #[clap(long)]
    build_root: Option<PathBuf>,

    /// Include files for use at runtime (relative to the build root)
    #[clap(long)]
    runtime_files: Vec<PathBuf>,

    /// Where should we cache files?
    #[clap(long("cache-directory"), env("NIX_SCRIPT_CACHE"))]
    cache_directory: Option<PathBuf>,

    /// The script to run (required), plus any arguments (optional). Any positional
    /// arguments after the script name will be passed on to the script.
    // Note: it'd be better to have a "script" and "args" field separately,
    // but there's a parsing issue in Clap (not a bug, but maybe a bug?) that
    // prevents passing args starting in -- after the script if we do that. See
    // https://github.com/clap-rs/clap/issues/1538
    #[clap(min_values = 1, required = true)]
    script_and_args: Vec<String>,
}

impl Opts {
    fn run(&self) -> Result<ExitStatus> {
        // First things first: what are we running? Where does it live? What
        // are its arguments?
        let (mut script, args) = self
            .parse_script_and_args()
            .context("could not parse script and args")?;
        script = clean_path(&script).context("could not clean path to script")?;

        if self.shell && !args.is_empty() {
            log::warn!("you specified both `--shell` and script args. I'm going to ignore the args! Use `--run` if you want to run something in the shell immediately.");
        }

        let script_name = script
            .file_name()
            .context("script did not have a file name")?
            .to_str()
            .context("filename was not valid UTF-8")?;

        // Parse our directives, but don't combine them with command-line arguments yet!
        let mut directives = Directives::from_file(&self.indicator, &script)
            .context("could not parse directives from script")?;

        let mut build_root = self.build_root.to_owned();
        if build_root.is_none() {
            if let Some(from_directives) = &directives.build_root {
                let out = script
                    .parent()
                    .map(Path::to_path_buf)
                    .unwrap_or_else(|| PathBuf::from("."));

                out.join(from_directives)
                    .canonicalize()
                    .context("could not canonicalize final path to build root")?;

                log::debug!("path to root from script directive: {}", out.display());

                build_root = Some(out);
            }
        };
        if build_root.is_none()
            && (!self.runtime_files.is_empty() || !directives.runtime_files.is_empty())
        {
            log::warn!("Requested runtime files without specifying a build root. I'm assuming it's the parent directory of the script for now, but you should set it explicitly!");
            build_root = Some(
                script
                    .parent()
                    .map(|p| p.to_owned())
                    .unwrap_or_else(|| PathBuf::from(".")),
            );
        }

        let mut builder = if let Some(build_root) = &build_root {
            Builder::from_directory(build_root, &script)
                .context("could not initialize source in directory")?
        } else {
            Builder::from_script(&script)
        };

        // First place we might bail early: if a script just wants to parse
        // directives using our parser, we dump JSON and quit instead of running.
        if self.parse {
            println!(
                "{}",
                serde_json::to_string(&directives).context("could not serialize directives")?
            );
            return Ok(ExitStatus::from_raw(0));
        }

        // we don't merge command-line and script directives until now because
        // we shouldn't provide them in the output of `--parse` without showing
        // where each option came from. For now, we're assuming that people who
        // write wrapper scripts know what they want to pass into `nix-script`.
        directives.maybe_override_build_command(&self.build_command);
        directives
            .merge_build_inputs(&self.build_inputs)
            .context("could not add build inputs provided on the command line")?;
        directives.maybe_override_interpreter(&self.interpreter);
        directives
            .merge_runtime_inputs(&self.runtime_inputs)
            .context("could not add runtime inputs provided on the command line")?;
        directives.merge_runtime_files(&self.runtime_files);

        // Second place we might bail early: if we're requesting a shell instead
        // of building and running the script.
        if self.shell {
            return self.run_shell(script, &directives);
        }

        // Third place we can bail early: if someone wants the generated
        // derivation to do IFD or similar
        if self.export {
            // We check here instead of inside while isolating the script or
            // similar so we can get an early bail that doesn't create trash
            // in the system's temporary directories.
            if build_root.is_none() {
                anyhow::bail!(
                    "I don't have a root to refer to while exporting, so I can't isolate the script and dependencies. Specify a --build-root and try this again!"
                )
            }

            println!(
                "{}",
                builder
                    .derivation(&directives, true)
                    .context("could not create a Nix derivation from the script")?
            );
            return Ok(ExitStatus::from_raw(0));
        }

        let cache_directory = self
            .get_cache_directory()
            .context("couldn't get cache directory")?;
        log::debug!(
            "using `{}` as the cache directory",
            cache_directory.display()
        );

        // create hash, check cache
        let hash = builder
            .hash(&directives)
            .context("could not calculate cache location for the script's compiled version")?;

        let target = cache_directory.join(format!("{}-{}", hash, script_name));
        log::trace!("cache target: {}", target.display());

        // before we perform the build, we need to check if the symlink target
        // has gone stale. This can happen when you run `nix-collect-garbage`,
        // since we don't pin the resulting derivations. We have to do things
        // in a slightly less ergonomic way in order to not follow symlinks.
        if fs::symlink_metadata(&target).is_ok() {
            let link_target = fs::read_link(&target).context("failed to read existing symlink")?;

            if !link_target.exists() {
                log::info!("removing stale (garbage-collected?) symlink");
                fs::remove_file(&target).context("could not remove stale symlink")?;
            }
        }

        if !target.exists() {
            log::debug!("hashed path does not exist; building");

            let out_path = builder
                .build(&cache_directory, &hash, &directives)
                .context("could not build derivation from script")?;

            if let Err(err) = symlink(&out_path, &target) {
                match err.kind() {
                    ErrorKind::AlreadyExists => {
                        // we could hypothetically detect if the link is
                        // pointing to the right location, but the Nix paths
                        // change for minor reasons that don't matter for
                        // script execution. Instead, we just warn here and
                        // trust our cache key to do the right thing. If we
                        // get a collision, we do!
                        log::warn!("detected a parallel write to the cache");
                    }
                    _ => return Err(err).context("could not create symlink in cache"),
                }
            }
        } else {
            log::debug!("hashed path exists; skipping build");
        }

        let mut child = Command::new(target.join("bin").join(script_name))
            .args(args)
            .spawn()
            .context("could not start the script")?;

        child.wait().context("could not run the script")
    }

    fn parse_script_and_args(&self) -> Result<(PathBuf, Vec<String>)> {
        log::trace!("parsing script and args");
        let mut script_and_args = self.script_and_args.iter();

        let script = PathBuf::from(script_and_args.next().context("I need at least a script name to run, but didn't get one. This represents an internal error, and you should open a bug!")?);

        Ok((script, self.script_and_args[1..].to_vec()))
    }

    fn get_cache_directory(&self) -> Result<PathBuf> {
        let mut target = match &self.cache_directory {
            Some(explicit) => explicit.to_owned(),
            None => {
                let dirs = directories::ProjectDirs::from("zone", "bytes", "nix-script").context(
                    "couldn't load HOME (set --cache-directory explicitly to get around this.)",
                )?;

                dirs.cache_dir().to_owned()
            }
        };

        if target.is_relative() {
            target = std::env::current_dir().context("could not get the current directory to figure out an absolute path to the cache")?.join(target)
        }

        if !target.exists() {
            log::trace!("creating cache directory");
            std::fs::create_dir_all(&target).context("could not create cache directory")?;
        }

        Ok(target)
    }

    fn run_shell(&self, script_file: PathBuf, directives: &Directives) -> Result<ExitStatus> {
        log::debug!("entering shell mode");

        let mut command = Command::new("nix-shell");

        log::trace!("setting SCRIPT_FILE to `{}`", script_file.display());
        command.env("SCRIPT_FILE", script_file);

        if self.pure {
            log::trace!("setting shell to pure mode");
            command.arg("--pure");
        }

        for input in &directives.build_inputs {
            log::trace!("adding build input `{}` to packages", input);
            command.arg("-p").arg(input.to_string());
        }

        for input in &directives.runtime_inputs {
            log::trace!("adding runtime input `{}` to packages", input);
            command.arg("-p").arg(input.to_string());
        }

        if let Some(run) = &self.run {
            log::trace!("running `{}`", run);
            command.arg("--run").arg(run);
        }

        command
            .spawn()
            .context("could not start nix-shell")?
            .wait()
            .context("could not start the shell")
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
