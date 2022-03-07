mod input;
mod inputs;

use anyhow::{Context, Result};
use input::Input;
use inputs::Inputs;
use std::fmt::{self, Display};
use std::path::Path;

#[derive(Debug)]
pub struct Derivation<'path> {
    inputs: Inputs,

    name: &'path str,
    src: &'path Path,
}

impl<'path> Derivation<'path> {
    pub fn new(src: &'path Path) -> Result<Self> {
        Ok(Self {
            inputs: Inputs::from(vec![Input::new(
                "pkgs".into(),
                Some("import <nixpkgs> { }".into()),
            )]),
            name: src
                .file_name()
                .and_then(|name| name.to_str())
                .context("could not determine derivation name from input path")?,
            src: src,
        })
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
