mod parser;

use anyhow::Result;
use rnix::types::Root;

#[derive(Debug)]
pub struct Directives<'src> {
    build_inputs: NixExpr<'src>,
}

#[derive(Debug)]
pub struct NixExpr<'src> {
    raw: &'src str,
    parsed: Root,
}

impl<'src> Directives<'src> {
    pub fn parse(indicator: &str, source: &'src str) -> Result<Self> {
        anyhow::bail!("todo")
    }
}
