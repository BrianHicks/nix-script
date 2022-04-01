use crate::clean_path::clean_path;
use crate::derivation::Derivation;
use crate::directives::Directives;
use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

pub struct Builder {
    source: Source,
}

impl Builder {
    pub fn from_script(script: &Path, cache_root: &Path) -> Result<Self> {
        log::trace!("constructing Source from script");

        // TODO: don't isolate until we need to; we may just want to parse the
        // directives or export a derivation

        // TODO: clean this dir up in `Drop`
        let tempdir = tempfile::Builder::new()
            .prefix("nix-script-")
            .tempdir_in(cache_root)
            .context("could not create temporary directory for build")?
            .into_path();

        let script_dest = tempdir.join(
            script
                .file_name()
                .context("the script path did not have a file name")?,
        );

        fs::copy(script, &script_dest)
            .context("could not copy source to temporary build directory")?;

        Ok(Self {
            source: Source::Script {
                script: script_dest,
                tempdir,
            },
        })
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
            source: Source::Directory { root, script },
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

        let mut derivation =
            Derivation::new(self.source.root(), self.source.script(), build_command)
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
}

enum Source {
    Script { tempdir: PathBuf, script: PathBuf },
    Directory { root: PathBuf, script: PathBuf },
}

impl Source {
    fn read(&self) -> Result<String> {
        log::trace!("reading script source");
        match self {
            Self::Script { script, .. } => fs::read_to_string(&script)
                .with_context(|| format!("could not read {}", script.display())),
            Self::Directory { root, script } => fs::read_to_string(root.join(script))
                .with_context(|| format!("could not read {}", script.display())),
        }
    }

    fn root(&self) -> &Path {
        match self {
            Self::Script { tempdir, .. } => tempdir,
            Self::Directory { root, .. } => root,
        }
    }

    fn script(&self) -> &Path {
        match self {
            Self::Script { script, .. } => script,
            Self::Directory { script, .. } => script,
        }
    }
}
