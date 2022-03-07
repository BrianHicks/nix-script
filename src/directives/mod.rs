mod parser;

use anyhow::{Context, Result};
use rnix::types::Root;

#[derive(Debug)]
pub struct Directives<'src> {
    build: NixExpr<'src>,
}

#[derive(Debug)]
pub struct NixExpr<'src> {
    raw: &'src str,
    parsed: Root,
}

impl<'src> Directives<'src> {
    pub fn parse(indicator: &str, source: &'src str) -> Result<Self> {
        let parser = parser::Parser::new(indicator).context("could not construct a parser")?;
        let _fields = parser.parse(source);

        anyhow::bail!("todo")
    }
}
