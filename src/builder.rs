use crate::clean_path::clean_path;
use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use once_cell::unsync::OnceCell;
use path_absolutize::Absolutize;
use seahash::SeaHasher;
use std::fs;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::ErrorKind;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use walkdir::WalkDir;

#[derive(Debug)]
pub struct Builder {
    source: Source,
}

lazy_static::lazy_static! {
    static ref CURRENT: PathBuf = PathBuf::from("./.");
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

    pub fn derivation(&self, directives: &Directives, for_export: bool) -> Result<Derivation> {
        let build_command = match &directives.build_command {
            Some(bc) => bc,
            None => anyhow::bail!("Need a build command, either by specifying a `build` directive or passing the `--build` option.")
        };

        let root = if for_export {
            &*CURRENT
        } else {
            self.source
                .root()
                .context("could not get the root directory for the derivation")?
        };

        let mut derivation = Derivation::new(
            root,
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
                .set_interpreter(interpreter)
                .context("could not set interpreter from file directives")?
        } else {
            log::trace!("not using an interpreter")
        };

        Ok(derivation)
    }

    pub fn hash(&self, directives: &Directives) -> Result<String> {
        let mut hasher = SeaHasher::new();

        // TODO: should we use the derivation here instead? It seems like this
        // should be equivalent (that is, it should change when the derivation
        // does.) The cost is not huge if we have to change it, though... just
        // a few rebuilds. It's probably fine?
        directives.hash(&mut hasher);
        log::trace!("hashed directives, hash is now {:x}", hasher.finish());

        self.source
            .hash(&mut hasher)
            .context("could not hash source")?;
        log::trace!("hashed source, hash is now {:x}", hasher.finish());

        Ok(format!("{:x}", hasher.finish()))
    }

    pub fn build(&mut self, cache_root: &Path, directives: &Directives) -> Result<PathBuf> {
        log::trace!("building");

        self.source
            .isolate(cache_root)
            .context("could not isolate source in order to build")?;

        let build_path = self
            .source
            .derivation_path(cache_root)
            .context("could not determine where to run the build")?;

        if !self.source.has_default_nix() {
            let derivation = self
                .derivation(directives, false)
                .context("could not prepare derivation to build")?;

            log::debug!("writing derivation to {}", build_path.display());
            fs::write(build_path.join("default.nix"), derivation.to_string())
                .context("could not write derivation contents")?;
        }

        log::info!("building");
        let mut output = Command::new("nix-build")
            .arg(build_path)
            .arg("--no-out-link") // TODO: it might be good to explicitly set `--out-link` to somewhere in the cache!
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .output()
            .map_err(|err| match err.kind() {
                ErrorKind::NotFound => {
                    anyhow::anyhow!("I couldn't call nix-build because I couldn't find the nix-build binary. Is Nix installed?")
                }
                _ => anyhow::anyhow!("{}", err),
            })
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
        tempdir: OnceCell<TempDir>,
        script: PathBuf,
    },
    Directory {
        script: PathBuf,

        root: PathBuf,
        absolute_root: PathBuf,

        // only created if we need a place to put `default.nix`
        tempdir: OnceCell<TempDir>,
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
            Self::Script {
                tempdir, script, ..
            } => Ok(tempdir
                .get()
                .map(|tempdir| tempdir.build.as_ref())
                .or_else(|| script.parent())
                .context("can't find a path to the root for this script. This is probably a bug and you should report it!")?),
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
                let target = tempdir.get_or_try_init(|| TempDir::new_in(cache_root))?;

                log::trace!(
                    "copying build script into temporary build directory at {}",
                    target.build.display()
                );

                let script_dest = target.build.join(
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

    fn derivation_path(&self, cache_root: &Path) -> Result<&PathBuf> {
        match self {
            Self::Script { tempdir, .. } =>
                tempdir.get()
                    .context("I'm trying to build a script but have not created a temporary directory. This is an internal error and you should report it!")
                    .map(|temp| &temp.build),
            Self::Directory { root, tempdir, .. } => if root.join("default.nix").exists() {
                Ok(root)
            } else {
                let target = tempdir
                    .get_or_try_init(|| TempDir::new_in(cache_root))
                    .context("could not create a place to write default.nix away from the source root")?;

                Ok(&target.build)
            },
        }
    }

    fn has_default_nix(&self) -> bool {
        match self {
            Self::Script { .. } => false,
            Self::Directory { root, .. } => root.join("default.nix").exists(),
        }
    }

    fn hash<H: Hasher>(&self, hasher: &mut H) -> Result<()> {
        match self {
            Self::Script { script, .. } => {
                log::debug!("hashing {}", script.display());
                hasher.write(
                    fs::read_to_string(script)
                        .context("could not read script contents")?
                        .as_ref(),
                )
            }
            Self::Directory { root, .. } => {
                for path_res in WalkDir::new(root)
                    .min_depth(1)
                    .follow_links(true)
                    .sort_by_file_name()
                {
                    let path = path_res.context("could not read directory entry")?;
                    if path.file_type().is_dir() {
                        continue;
                    }

                    log::debug!("hashing {}", path.path().display());
                    hasher.write(path.file_name().as_bytes());
                    hasher.write(
                        fs::read_to_string(path.path())
                            .with_context(|| {
                                format!("could not read {} in script source", path.path().display())
                            })?
                            .as_ref(),
                    );
                }
            }
        };

        Ok(())
    }
}

/// When you run a build, Nix uses the directory name as part of the calculation
/// for the final path in the store. That means that if we have random temporary
/// directory names like `nix-script-a4beff` we'll bust the cache every time. We
/// can get around this by building in a subdirectory of the temporary directory,
/// but that makes it more difficult to keep track of the real root. This data
/// structure solves that problem by explicitly tracking both and dropping the
/// temporary directory when the owner goes out of scope.
#[derive(Debug)]
struct TempDir {
    root: PathBuf,
    build: PathBuf,
}

impl TempDir {
    fn new_in(root: &Path) -> Result<Self> {
        log::trace!("creating temporary directory");

        let root = tempfile::Builder::new()
            .prefix("nix-script-")
            .tempdir_in(root)
            .context("could not create temporary directory")?
            .into_path();

        let build = root.join("nix-script");

        fs::create_dir(&build)
            .context("could not create build directory in temporary directory")?;

        Ok(TempDir { root, build })
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        log::trace!("attempting to remove temporary directory");

        if let Err(err) = fs::remove_dir_all(&self.root) {
            log::warn!(
                "Got an error while removing the temporary directory at {}: {}",
                self.root.display(),
                err
            )
        }
    }
}
