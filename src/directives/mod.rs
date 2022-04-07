mod parser;

use crate::expr::Expr;
use anyhow::{Context, Result};
use core::hash::{Hash, Hasher};
use std::collections::HashMap;

#[derive(Debug, serde::Serialize)]
pub struct Directives {
    pub build_command: Option<String>,
    pub build_inputs: Vec<Expr>,
    pub interpreter: Option<String>,
    pub runtime_inputs: Vec<Expr>,
}

impl Directives {
    pub fn parse(indicator: &str, source: &str) -> Result<Self> {
        let parser = parser::Parser::new(indicator).context("could not construct a parser")?;
        let fields = parser.parse(source);

        Self::from_directives(fields)
    }

    fn from_directives(fields: HashMap<&str, Vec<&str>>) -> Result<Self> {
        // Build (once)
        let build_command = match fields.get("build") {
            Some(value) => {
                if value.len() != 1 {
                    anyhow::bail!("I got multiple build directives, and I don't know which to use. Remove all but one and try again!");
                }

                Some(value[0].to_owned())
            }
            None => None,
        };

        // buildInputs (many)
        let build_inputs = match fields.get("buildInputs") {
            None => Vec::new(),
            Some(lines) => {
                Expr::parse_as_list(&lines.join(" ")).context("could not parse build inputs")?
            }
        };

        // interpreter (once)
        let interpreter = match fields.get("interpreter") {
            Some(value) => {
                if value.len() != 1 {
                    anyhow::bail!("I got multiple interpreter directives, and I don't known which to use. Remove all but one and try agagin!");
                }

                Some(value[0].to_owned())
            }
            None => None,
        };

        // runtimeInputs (many)
        let runtime_inputs = match fields.get("runtimeInputs") {
            None => Vec::new(),
            Some(lines) => {
                Expr::parse_as_list(&lines.join(" ")).context("could not parse runtime inputs")?
            }
        };

        Ok(Directives {
            build_command,
            build_inputs,
            interpreter,
            runtime_inputs,
        })
    }

    pub fn maybe_override_build_command(&mut self, maybe_new: &Option<String>) {
        if maybe_new.is_some() {
            self.build_command = maybe_new.to_owned()
        }
    }

    pub fn maybe_override_interpreter(&mut self, maybe_new: &Option<String>) {
        if maybe_new.is_some() {
            self.interpreter = maybe_new.to_owned()
        }
    }
}

impl Hash for Directives {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        if let Some(build_command) = &self.build_command {
            hasher.write(build_command.as_ref())
        }

        for input in &self.build_inputs {
            input.hash(hasher)
        }

        if let Some(interpreter) = &self.interpreter {
            hasher.write(interpreter.as_ref())
        }

        for input in &self.runtime_inputs {
            input.hash(hasher)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod hash {
        use super::*;

        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        fn assert_have_different_hashes<H: Hash>(l: H, r: H) {
            let mut l_hasher = DefaultHasher::new();
            let mut r_hasher = DefaultHasher::new();

            l.hash(&mut l_hasher);
            r.hash(&mut r_hasher);

            println!("l: {}, r: {}", l_hasher.finish(), r_hasher.finish());
            assert!(l_hasher.finish() != r_hasher.finish())
        }

        #[test]
        fn build_command_changes_hash() {
            assert_have_different_hashes(
                Directives::from_directives(HashMap::from([("build", vec!["a"])])).unwrap(),
                Directives::from_directives(HashMap::from([("build", vec!["b"])])).unwrap(),
            )
        }

        #[test]
        fn build_inputs_changes_hash() {
            assert_have_different_hashes(
                Directives::from_directives(HashMap::from([("buildInputs", vec!["a"])])).unwrap(),
                Directives::from_directives(HashMap::from([("buildInputs", vec!["b"])])).unwrap(),
            )
        }

        #[test]
        fn interpreter_changes_hash() {
            assert_have_different_hashes(
                Directives::from_directives(HashMap::from([("interpreter", vec!["a"])])).unwrap(),
                Directives::from_directives(HashMap::from([("interpreter", vec!["b"])])).unwrap(),
            )
        }

        #[test]
        fn runtime_inputs_changes_hash() {
            assert_have_different_hashes(
                Directives::from_directives(HashMap::from([("runtimeInputs", vec!["a"])])).unwrap(),
                Directives::from_directives(HashMap::from([("runtimeInputs", vec!["b"])])).unwrap(),
            )
        }
    }
}
