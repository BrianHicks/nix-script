use crate::clean_path::clean_path;
use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use once_cell::unsync::OnceCell;
use path_absolutize::Absolutize;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug)]
pub struct Builder {
    source: Source,
}

impl Builder {
    pub fn from_script(script: &Path) -> Self {
        log::trace!("constructing Source from script");

        Self {
            source: Source::Script {
                script: script.to_owned(),
                tempdir: OnceCell::new(),
            },
        }
    }

    pub fn from_directory(raw_root: &Path, raw_script: &Path) -> Result<Self> {
        log::trace!("constructing Source from directory");
        let root = clean_path(raw_root).context("could not clean path to root")?;

        let script = raw_script.strip_prefix(&root)
            .context("could not find a path from the provided root to the script file (root must contain script)")?
            .to_owned();

        log::debug!(
            "calculated script path from root `{}` as `{}`",
            root.display(),
            script.display()
        );

        Ok(Self {
            source: Source::Directory {
                script,
                absolute_root: root
                    .absolutize()
                    .context("could not find absolute path to root")?
                    .to_path_buf(),
                root,
                tempdir: OnceCell::new(),
            },
        })
    }

    pub fn directives(&self, indicator: &str) -> Result<Directives> {
        let source = self.source.read()?;
        Directives::parse(indicator, &source).context("could not construct a directive parser")
    }

    pub fn derivation(&self, directives: &Directives) -> Result<Derivation> {
        let build_command = match &directives.build_command {
            Some(bc) => bc,
            None => anyhow::bail!("Need a build command, either by specifying a `build` directive or passing the `--build` option.")
        };

        let mut derivation = Derivation::new(
            self.source
                .root()
                .context("could not get the root directory for the derivation")?,
            self.source
                .script()
                .context("could not get the script name for the derivation")?,
            build_command,
        )
        .context("could not create a Nix derivation")?;

        log::trace!("adding build inputs");
        derivation.add_build_inputs(directives.build_inputs.clone());

        log::trace!("adding runtime inputs");
        derivation.add_runtime_inputs(directives.runtime_inputs.clone());

        if let Some(interpreter) = &directives.interpreter {
            log::debug!("using interpreter from directives");
            derivation
                .set_interpreter(&interpreter)
                .context("could not set interpreter from file directives")?
        } else {
            log::trace!("not using an interpreter")
        };

        Ok(derivation)
    }

    pub fn build(&mut self, cache_root: &Path, directives: &Directives) -> Result<PathBuf> {
        log::trace!("building");

        self.source
            .isolate(cache_root)
            .context("could not isolate source in order to build")?;
        // TODO: make sure `default.nix` is in the right place

        let (build_path, write_default_nix) = self
            .source
            .derivation_path(cache_root)
            .context("could not determine where to run the build")?;

        if write_default_nix {
            let derivation = self
                .derivation(directives)
                .context("could not prepare derivation to build")?;

            log::debug!("writing derivation to {}", build_path.display());
            fs::write(build_path.join("default.nix"), derivation.to_string())
                .context("could not write derivation contents")?;
        }

        log::info!("building in {}", build_path.display());
        let mut output = Command::new("nix-build")
            .arg(build_path)
            .arg("--no-out-link") // TODO: it might be good to explicitly set `--out-link` to somewhere in the cache!
            .output()
            .context("failed to build")?;

        match output.status.code() {
            Some(0) => {}
            Some(other) => anyhow::bail!("nix-build exited with code {}", other),
            None => anyhow::bail!("nix-build was terminated by a signal"),
        }

        // trim newline from the end of the output
        match output.stdout.pop() {
            Some(0x0A) => {}
            Some(other) => {
                log::debug!("stdout: {:x?} ending in {:x}", output.stdout, other);
                anyhow::bail!("stdout didn't end in a single newline, but {:x}", other)
            }
            None => anyhow::bail!("nix-build's stdout was empty. Was there an error building?"),
        };

        Ok(PathBuf::from(std::str::from_utf8(&output.stdout).context(
            "could not convert nix-build's path output to a string",
        )?))
    }
}

#[derive(Debug)]
enum Source {
    Script {
        tempdir: OnceCell<PathBuf>,
        script: PathBuf,
    },
    Directory {
        script: PathBuf,

        root: PathBuf,
        absolute_root: PathBuf,

        // only created if we need a place to put `default.nix`
        tempdir: OnceCell<PathBuf>,
    },
}

impl Source {
    fn read(&self) -> Result<String> {
        log::trace!("reading script source");
        match self {
            Self::Script { script, .. } => fs::read_to_string(&script)
                .with_context(|| format!("could not read {}", script.display())),
            Self::Directory { root, script, .. } => fs::read_to_string(root.join(script))
                .with_context(|| format!("could not read {}", script.display())),
        }
    }

    fn root(&self) -> Result<&Path> {
        match self {
            Self::Script { tempdir, .. } => Ok(&tempdir
                .get()
                .context("the temporary directory has not been created yet")?),
            Self::Directory {
                tempdir,
                root,
                absolute_root,
                ..
            } => {
                if tempdir.get().is_some() {
                    Ok(absolute_root)
                } else {
                    Ok(root)
                }
            }
        }
    }

    fn script(&self) -> Result<&Path> {
        match self {
            Self::Script { script, .. } => script
                .file_name()
                .with_context(|| {
                    format!("script path ({}) did not have a filename", script.display())
                })
                .map(|p| p.as_ref()),
            Self::Directory { script, .. } => Ok(script),
        }
    }

    fn isolate(&mut self, cache_root: &Path) -> Result<()> {
        match self {
            Self::Script { script, tempdir } => {
                let target =
                    tempdir.get_or_try_init(|| Self::make_temporary_directory(cache_root))?;

                log::trace!(
                    "copying build script into temporary build directory at {}",
                    target.display()
                );

                let script_dest = target.join(
                    script
                        .file_name()
                        .context("the script path did not have a file name")?,
                );

                fs::copy(script, &script_dest)
                    .context("could not copy source to temporary build directory")?;

                Ok(())
            }

            // We don't need to do anything to isolate if we're working with
            // a directory since we build in place.
            Self::Directory { .. } => Ok(()),
        }
    }

    fn derivation_path(&self, cache_root: &Path) -> Result<(&PathBuf, bool)> {
        match self {
            Self::Script { tempdir, .. } =>
                tempdir.get()
                    .context("I'm trying to build a script but have not created a temporary directory. This is an internal error and you should report it!")
                    .map(|temp| (temp, true)),
            Self::Directory { root, tempdir, .. } => if root.join("default.nix").exists() {
                Ok((root, false))
            } else {
                let target = tempdir
                    .get_or_try_init(|| Self::make_temporary_directory(cache_root))
                    .context("could not create a place to write default.nix away from the source root")?;

                Ok((target, true))
            },
        }
    }

    fn make_temporary_directory(cache_root: &Path) -> Result<PathBuf> {
        Ok(tempfile::Builder::new()
            .prefix("nix-script-")
            .tempdir_in(cache_root)
            .context("could not create temporary directory")?
            .into_path())
    }
}

impl Drop for Source {
    fn drop(&mut self) {
        let tempdir = match self {
            Self::Script { tempdir, .. } => tempdir,
            Self::Directory { tempdir, .. } => tempdir,
        };

        if let Some(created) = tempdir.get() {
            log::trace!("attempting to remove temporary directory");
            if let Err(err) = fs::remove_dir_all(&created) {
                log::warn!(
                    "Got an error while removing the temporary directory at {}: {}",
                    created.display(),
                    err
                )
            }
        }
    }
}
