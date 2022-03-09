mod inputs;

use crate::expr::Expr;
use anyhow::{Context, Result};
use inputs::Inputs;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::path::Path;

#[derive(Debug)]
pub struct Derivation<'path> {
    inputs: Inputs,

    name: &'path str,
    src: &'path Path,

    build_inputs: HashSet<Expr>,
    runtime_inputs: HashSet<Expr>,
}

impl<'path> Derivation<'path> {
    pub fn new(src: &'path Path) -> Result<Self> {
        Ok(Self {
            inputs: Inputs::from(vec![("pkgs".into(), Some("import <nixpkgs> { }".into()))]),
            name: src
                .file_name()
                .and_then(|name| name.to_str())
                .context("could not determine derivation name from input path")?,
            src: src,
            build_inputs: HashSet::new(),
            runtime_inputs: HashSet::new(),
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

    pub fn add_runtime_inputs(&mut self, runtime_inputs: Vec<Expr>) {
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
}
impl Display for Derivation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:\n{{\n  name = \"{}\";\n  src = {};\n}}",
            self.inputs,
            self.name,
            self.src.display(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn assert_no_errors(src: &str) {
        let empty: Vec<rnix::parser::ParseError> = Vec::new();
        assert_eq!(empty, rnix::parse(src).errors())
    }

    mod to_string {
        use super::*;
        use std::path::PathBuf;

        #[test]
        fn empty() {
            let path: PathBuf = [".", "path", "to", "my", "cool-script"].iter().collect();

            assert_eq!(
                String::from(
                    "{ pkgs ? import <nixpkgs> { } }:\n{\n  name = \"cool-script\";\n  src = ./path/to/my/cool-script;\n}"
                ),
                Derivation::new(&path).unwrap().to_string(),
            )
        }
    }
}
