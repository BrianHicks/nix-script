mod parser;

use crate::expr::Expr;
use anyhow::{Context, Result};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Directives<'src> {
    build: Option<&'src str>,
    build_inputs: Vec<Expr>,
    runtime_inputs: Vec<Expr>,
}

impl<'src> Directives<'src> {
    pub fn parse(indicator: &str, source: &'src str) -> Result<Self> {
        let parser = parser::Parser::new(indicator).context("could not construct a parser")?;
        let fields = parser.parse(source);

        Self::from_directives(fields)
    }

    fn from_directives(fields: HashMap<&'src str, Vec<&'src str>>) -> Result<Self> {
        // Build (once)
        let build = match fields.get("build") {
            Some(value) => {
                if value.len() != 1 {
                    anyhow::bail!("I got multiple build directives, and I don't know which to use. Remove all but one and try again!");
                }

                Some(value[0])
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

        // runtimeInputs (many)
        let runtime_inputs = match fields.get("runtimeInputs") {
            None => Vec::new(),
            Some(lines) => {
                Expr::parse_as_list(&lines.join(" ")).context("could not parse runtime inputs")?
            }
        };

        Ok(Directives {
            build,
            build_inputs,
            runtime_inputs: vec![],
        })
    }
}
