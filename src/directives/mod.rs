mod parser;

use anyhow::{Context, Result};
use rnix::types::Root;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Directives<'src> {
    build: Option<&'src str>,
    buildInputs: Vec<NixExpr<'src>>,
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
        let buildInputs = fields
            .get("buildInputs")
            .map(|arr| arr.iter().map(|line| NixExpr::parse(line)).collect())
            .unwrap_or_else(|| Vec::new());

        Ok(Directives { build, buildInputs })
    }
}

#[derive(Debug)]
pub struct NixExpr<'src> {
    raw: &'src str,
    parsed: Root,
}

impl<'src> NixExpr<'src> {
    pub fn parse(source: &'src str) -> Self {
        NixExpr {
            raw: source,
            parsed: rnix::parse(source).root(),
        }
    }
}
