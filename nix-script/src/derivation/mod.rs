mod inputs;

use crate::clean_path::clean_path;
use anyhow::{Context, Result};
use directives::expr::Expr;
use inputs::Inputs;
use std::collections::BTreeSet;
use std::fmt::{self, Display};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Derivation {
    inputs: Inputs,

    name: String,
    src: PathBuf,
    root: PathBuf,

    build_command: String,

    build_inputs: BTreeSet<Expr>,

    interpreter: Option<(String, Option<String>)>,
    runtime_inputs: BTreeSet<Expr>,
    runtime_files: BTreeSet<PathBuf>,
}

impl Derivation {
    pub fn new(
        root: &Path,
        src: &Path,
        build_command: &str,
        nixpkgs_options: Option<&Expr>,
    ) -> Result<Self> {
        log::trace!(
            "creating a new derivation with root of {} and src of {}",
            root.display(),
            src.display()
        );

        let final_nixpkgs_options =
            match nixpkgs_options {
                // Argh! I don't love this clone but it seems to be the least
                // unreasonable way around the fact that we don't own the
                // expressions we're being passed.
                Some(options) => options.clone(),
                None => ("{ }").parse().context("hardcoded empty attrset did not parse successfully. This is a bug and should be reported.")?
            };

        Ok(Self {
            inputs: Inputs::from(vec![
                (
                    "pkgs".into(),
                    Some(format!("import <nixpkgs> {final_nixpkgs_options}")),
                ),
                ("makeWrapper".into(), Some("pkgs.makeWrapper".into())),
            ]),
            name: src
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_owned())
                .context("could not determine derivation name from input path")?,
            src: src.to_owned(),
            root: clean_path(root).context("could not determine path to source for derivation")?,
            build_command: build_command.to_owned(),
            build_inputs: BTreeSet::new(),
            interpreter: None,
            runtime_inputs: BTreeSet::new(),
            runtime_files: BTreeSet::new(),
        })
    }

    pub fn add_build_inputs(&mut self, build_inputs: Vec<Expr>) {
        for build_input in build_inputs {
            if build_input.is_extractable() {
                log::trace!("extracting build input `{}`", build_input);
                self.inputs
                    .insert(build_input.to_string(), Some(format!("pkgs.{build_input}")));
            }
            self.build_inputs.insert(build_input);
        }
    }

    pub fn set_interpreter(&mut self, interpreter: &str) -> Result<()> {
        let trimmed = interpreter.trim();
        let mut words = trimmed.split(' ');

        let command = words
            .next()
            .context("need at least a command in the interpreter, but got a blank string")?;

        let args = trimmed[command.len()..].trim();

        self.interpreter = Some((
            command.to_owned(),
            if args.is_empty() {
                None
            } else {
                Some(args.to_owned())
            },
        ));

        Ok(())
    }

    pub fn add_runtime_inputs(&mut self, runtime_inputs: Vec<Expr>) {
        for runtime_input in runtime_inputs {
            if runtime_input.is_extractable() {
                log::trace!("extracting build input `{}`", runtime_input);
                self.inputs.insert(
                    runtime_input.to_string(),
                    Some(format!("pkgs.{runtime_input}")),
                );
            }
            self.runtime_inputs.insert(runtime_input);
        }
    }

    pub fn add_runtime_files(&mut self, runtime_files: Vec<PathBuf>) {
        for runtime_file in runtime_files {
            self.runtime_files.insert(runtime_file);
        }
    }
}

impl Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:\npkgs.stdenv.mkDerivation {{\n  name = \"{}\";\n  src = {};\n\n",
            self.inputs,
            self.name,
            self.root.display(),
        )?;

        if !self.build_inputs.is_empty() {
            write!(f, "  buildInputs = with pkgs; ")?;
            fmt_list(f, &self.build_inputs)?;
            writeln!(f, ";")?;
        }

        // build phase
        write!(
            f,
            "  buildPhase = ''\n    SRC={}\n\n    mkdir bin\n    OUT=bin/{}\n\n",
            self.src.display(),
            self.name,
        )?;
        if self.build_command.is_empty() {
            write!(f, "    echo build command is not set\n    exit 1\n")?;
        } else {
            writeln!(f, "    {}", self.build_command)?;
        }
        write!(f, "  '';\n\n")?;

        // install phase
        if !self.runtime_inputs.is_empty() {
            write!(f, "  nativeBuildInputs = with pkgs; ")?;
            fmt_list(f, &self.runtime_inputs)?;
            writeln!(f, ";")?;
        }

        write!(
            f,
            "  installPhase = ''\n    mkdir -p $out\n    mv bin $out/bin"
        )?;

        if !self.runtime_files.is_empty() {
            let target: PathBuf = ["$out", "usr", "share", &self.name].iter().collect();
            write!(f, "\n\n    mkdir -p {}", target.display())?;
            for file in &self.runtime_files {
                write!(f, "\n    mv {} {}", &file.display(), target.display())?;
            }
        }

        write!(
            f,
            "\n\n    source ${{makeWrapper}}/nix-support/setup-hook\n    "
        )?;

        if let Some((command, maybe_args)) = &self.interpreter {
            write!(
                f,
                "mv $out/bin/{} $out/bin/.{}\n    makeWrapper $(command -v {}) $out/bin/{} \\\n",
                self.name, self.name, command, self.name
            )?;

            if let Some(args) = maybe_args {
                write!(
                    f,
                    "        --add-flags \"{} $out/bin/.{}\" ",
                    args, self.name
                )?
            } else {
                write!(f, "        --add-flags \"$out/bin/.{}\"", self.name)?;
            }
        } else {
            write!(
                f,
                "wrapProgram $out/bin/{} --argv0 {}",
                self.name, self.name
            )?
        }

        if !self.runtime_files.is_empty() {
            write!(
                f,
                " \\\n        --set RUNTIME_FILES_ROOT $out/usr/share/{}",
                self.name
            )?;
        }

        write!(f, " \\\n        --set SCRIPT_FILE {}", self.name)?;

        if !self.runtime_inputs.is_empty() {
            write!(
                f,
                " \\\n        --prefix PATH : ${{with pkgs; lib.makeBinPath "
            )?;
            fmt_list(f, &self.runtime_inputs)?;
            write!(f, "}}")?;
        }

        write!(f, "\n  '';\n")?;

        write!(f, "}}")
    }
}

fn fmt_list(f: &mut fmt::Formatter<'_>, inputs: &BTreeSet<Expr>) -> Result<(), fmt::Error> {
    write!(f, "[")?;
    for input in inputs {
        if input.needs_parens_in_list() {
            write!(f, " ({input})")?;
        } else {
            write!(f, " {input}")?;
        }
    }
    write!(f, " ]")
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn assert_no_errors(src: &str) {
        let empty: Vec<rnix::parser::ParseError> = Vec::new();
        println!("{}", src);
        assert_eq!(empty, rnix::Root::parse(src).errors())
    }

    mod to_string {
        use super::*;
        use std::path::PathBuf;

        #[test]
        fn empty() {
            let root = PathBuf::from("/");
            let path: PathBuf = ["path", "to", "my", "cool-script"].iter().collect();
            let derivation = Derivation::new(&root, &path, "mv $SRC $DEST", None).unwrap();

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_build_inputs() {
            let root = PathBuf::from("/");
            let path = PathBuf::from("X");
            let mut derivation = Derivation::new(&root, &path, "mv $SRC $DEST", None).unwrap();
            derivation.add_build_inputs(vec![("jq").parse().unwrap(), ("bash").parse().unwrap()]);

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_runtime_inputs() {
            let root = PathBuf::from("/");
            let path = PathBuf::from("X");
            let mut derivation = Derivation::new(&root, &path, "mv $SRC $DEST", None).unwrap();
            derivation.add_runtime_inputs(vec![("jq").parse().unwrap()]);

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_interpreter() {
            let root = PathBuf::from("/");
            let path = PathBuf::from("X");
            let mut derivation = Derivation::new(&root, &path, "mv $SRC $DEST", None).unwrap();
            derivation.set_interpreter("bash").unwrap();

            assert_no_errors(&derivation.to_string());
        }
    }
}
