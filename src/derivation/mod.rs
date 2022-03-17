mod inputs;

use crate::expr::Expr;
use anyhow::{Context, Result};
use inputs::Inputs;
use std::collections::BTreeSet;
use std::fmt::{self, Display};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Derivation {
    inputs: Inputs,

    name: String,
    src: PathBuf,

    build_command: String,

    build_inputs: BTreeSet<Expr>,

    interpreter: Option<(String, Option<String>)>,
    runtime_inputs: BTreeSet<Expr>,
}

impl Derivation {
    pub fn new(src: &Path, build_command: &str) -> Result<Self> {
        Ok(Self {
            inputs: Inputs::from(vec![("pkgs".into(), Some("import <nixpkgs> { }".into()))]),
            name: src
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_owned())
                .context("could not determine derivation name from input path")?,
            src: match src.parent() {
                Some(path) => {
                    if path.is_relative() {
                        anyhow::bail!("I need an absolute path as source")
                    } else if path.parent() == None {
                        // no parent means we're at the root and we need to
                        // format the path just a little differently so Nix
                        // will be fine with it.
                        PathBuf::from("/.")
                    } else {
                        path.to_path_buf()
                    }
                }
                None => anyhow::bail!(
                    "could not determine an absolute path from the given source directory"
                ),
            },
            build_command: build_command.to_owned(),
            build_inputs: BTreeSet::new(),
            interpreter: None,
            runtime_inputs: BTreeSet::new(),
        })
    }

    pub fn add_build_inputs(&mut self, build_inputs: Vec<Expr>) {
        for build_input in build_inputs {
            if build_input.is_extractable() {
                self.inputs.insert(
                    build_input.to_string(),
                    Some(format!("pkgs.{}", build_input)),
                );
            }
            self.build_inputs.insert(build_input);
        }
    }

    pub fn set_interpreter(&mut self, interpreter: &str) -> Result<()> {
        let trimmed = interpreter.trim();
        let mut words = trimmed.split(" ");

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
        self.depend_on_make_wrapper();

        Ok(())
    }

    pub fn add_runtime_inputs(&mut self, runtime_inputs: Vec<Expr>) {
        self.depend_on_make_wrapper();
        for runtime_input in runtime_inputs {
            if runtime_input.is_extractable() {
                self.inputs.insert(
                    runtime_input.to_string(),
                    Some(format!("pkgs.{}", runtime_input)),
                );
            }
            self.runtime_inputs.insert(runtime_input);
        }
    }

    fn depend_on_make_wrapper(&mut self) {
        self.inputs.insert(
            "makeWrapper".to_string(),
            Some("pkgs.makeWrapper".to_string()),
        );
    }
}

impl Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:\npkgs.stdenv.mkDerivation {{\n  name = \"{}\";\n  src = {};\n\n",
            self.inputs,
            self.name,
            self.src.display(),
        )?;

        if self.build_inputs.len() > 0 {
            write!(f, "  buildInputs = with pkgs; [")?;

            for input in &self.build_inputs {
                if input.needs_parens_in_list() {
                    write!(f, " ({})", input)?;
                } else {
                    write!(f, " {}", input)?;
                }
            }

            write!(f, " ];\n")?;
        }

        // build phase
        write!(
            f,
            "  buildPhase = ''\n    SRC={}\n\n    mkdir bin\n    OUT=bin/{}\n\n",
            self.name, self.name,
        )?;
        if self.build_command.is_empty() {
            write!(f, "    echo build command is not set\n    exit 1\n")?;
        } else {
            write!(f, "    {}\n", self.build_command)?;
        }
        write!(f, "  '';\n\n")?;

        // install phase
        write!(
            f,
            "  installPhase = ''\n    mkdir -p $out\n    mv bin $out/bin"
        )?;

        if self.interpreter.is_some() || !self.runtime_inputs.is_empty() {
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
                    "makeWrapper $out/bin/{} --argv0 {}",
                    self.name, self.name
                )?
            }

            if !self.runtime_inputs.is_empty() {
                write!(f, " \\\n        --prefix PATH : ${{pkgs.lib.makeBinPath [ ")?;

                for input in &self.runtime_inputs {
                    if input.needs_parens_in_list() {
                        write!(f, "({}) ", input)?;
                    } else {
                        write!(f, "{} ", input)?;
                    }
                }

                write!(f, "]}}")?;
            }
        }
        write!(f, "\n  '';\n")?;

        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn assert_no_errors(src: &str) {
        let empty: Vec<rnix::parser::ParseError> = Vec::new();
        println!("{}", src);
        assert_eq!(empty, rnix::parse(src).errors())
    }

    mod to_string {
        use super::*;
        use crate::expr::Expr;
        use std::path::PathBuf;

        #[test]
        fn empty() {
            let path: PathBuf = ["/", "path", "to", "my", "cool-script"].iter().collect();
            let derivation = Derivation::new(&path, "mv $SRC $DEST".into()).unwrap();

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_build_inputs() {
            let path = PathBuf::from("/X");
            let mut derivation = Derivation::new(&path, "mv $SRC $DEST".into()).unwrap();
            derivation.add_build_inputs(vec![
                Expr::parse("jq").unwrap(),
                Expr::parse("bash").unwrap(),
            ]);

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_runtime_inputs() {
            let path = PathBuf::from("/X");
            let mut derivation = Derivation::new(&path, "mv $SRC $DEST".into()).unwrap();
            derivation.add_runtime_inputs(vec![Expr::parse("jq").unwrap()]);

            assert_no_errors(&derivation.to_string());
        }

        #[test]
        fn with_interpreter() {
            let path = PathBuf::from("/X");
            let mut derivation = Derivation::new(&path, "mv $SRC $DEST".into()).unwrap();
            derivation.set_interpreter("bash").unwrap();

            assert_no_errors(&derivation.to_string());
        }
    }
}
