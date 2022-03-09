mod parser;

use anyhow::{Context, Result};
use rnix::types::Root;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Directives<'src> {
    build: Option<&'src str>,
    build_inputs: Option<NixExpr>,
    runtime_inputs: Option<NixExpr>,
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
        let build_inputs = fields
            .get("buildInputs")
            .map(|lines| NixExpr::as_list_from_lines(lines));

        // runtimeInputs (many)
        let runtime_inputs = fields
            .get("runtimeInputs")
            .map(|lines| NixExpr::as_list_from_lines(lines));

        Ok(Directives {
            build,
            build_inputs,
            runtime_inputs,
        })
    }
}

#[derive(Debug)]
pub struct NixExpr {
    raw: String,
    parsed: Root,
}

impl<'src> NixExpr {
    pub fn parse(source: &str) -> Self {
        NixExpr {
            raw: source.to_string(),
            parsed: rnix::parse(source).root(),
        }
    }

    pub fn as_list_from_lines(sources: &Vec<&str>) -> Self {
        Self::parse(&format!("[{}]", sources.join(" ")))
    }
}
