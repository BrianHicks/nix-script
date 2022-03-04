mod input;
mod inputs;
use std::fmt::{self, Display};

use input::Input;
use inputs::Inputs;

#[derive(Debug)]
pub struct Derivation {
    inputs: Inputs,
    name: String,
    src: String,
}

impl Derivation {
    pub fn new(name: String, src: String) -> Self {
        Self {
            inputs: Inputs::new(vec![Input::new(
                "pkgs".into(),
                Some("import <nixpkgs> { }".into()),
            )]),
            name,
            src,
        }
    }
}
impl Display for Derivation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:\n{{\n  name = \"{}\";\n  src = {};\n}}",
            self.inputs.to_string(),
            self.name,
            self.src,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod to_string {
        use super::*;

        #[test]
        fn empty() {
            assert_eq!(
                String::from(
                    "{ pkgs ? import <nixpkgs> { } }:\n{\n  name = \"cool-script\";\n  src = ./.;\n}"
                ),
                Derivation::new("cool-script".into(), "./.".into()).to_string(),
            )
        }
    }
}
