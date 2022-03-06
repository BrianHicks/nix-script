mod input;
mod inputs;
use std::fmt::{self, Display};
use std::path::Path;

use input::Input;
use inputs::Inputs;

#[derive(Debug)]
pub struct Derivation<'path> {
    inputs: Inputs,
    src: &'path Path,
}

impl<'path> Derivation<'path> {
    pub fn new(src: &'path Path) -> Self {
        Self {
            inputs: Inputs::new(vec![Input::new(
                "pkgs".into(),
                Some("import <nixpkgs> { }".into()),
            )]),
            src,
        }
    }
}
impl Display for Derivation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:\n{{\n  name = \"{}\";\n  src = {};\n}}",
            self.inputs,
            self.src
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("UNKNOWN"),
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
                Derivation::new(&path).to_string(),
            )
        }
    }
}
