mod parser;

use crate::expr::Expr;
use anyhow::{Context, Result};
use std::collections::HashMap;

#[derive(Debug, serde::Serialize, Hash)]
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
