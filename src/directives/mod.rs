mod parser;

use anyhow::{Context, Result};
use rnix::types::Root;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Directives<'src> {
    build: Option<&'src str>,
}

#[derive(Debug)]
pub struct NixExpr<'src> {
    raw: &'src str,
    parsed: Root,
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

        Ok(Directives { build })
    }
}
