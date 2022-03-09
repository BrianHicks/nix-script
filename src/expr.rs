use anyhow::{Context, Result};
use rnix::types::{List, TypedNode, Wrapper};
use rnix::{SyntaxKind, SyntaxNode};

#[derive(Debug)]
pub struct Expr {
    raw: String,
    parsed: SyntaxNode,
}

impl Expr {
    pub fn parse(source: &str) -> Result<Self> {
        Ok(Self {
            raw: source.to_string(),
            parsed: rnix::parse(source)
                .as_result()
                .context("failed to parse the source")?
                .root()
                .inner()
                .context("root node did not have an inner node")?,
        })
    }

    pub fn parse_as_list(source: &str) -> Result<Vec<Self>> {
        let root = rnix::parse(&format!("[{}]", source))
            .as_result()
            .context("failed to parse the source when wrapping as a list")?
            .root();

        Ok(
            List::cast(root.inner().context("root did not have an inner node")?)
                .context("could not parse this list as a list")?
                .items()
                .map(|node| Expr {
                    raw: node.to_string(),
                    parsed: node,
                })
                .collect(),
        )
    }

    pub fn is_extractable(&self) -> bool {
        match self.parsed.kind() {
            SyntaxKind::NODE_IDENT => true,
            _ => false,
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod equality {
        use super::*;

        #[test]
        fn equal_if_raw_is_equal() {
            assert_eq!(Expr::parse("a").unwrap(), Expr::parse("a").unwrap())
        }

        #[test]
        fn unequal_if_raw_is_unequal() {
            assert!(Expr::parse("a").unwrap() != Expr::parse("b").unwrap())
        }
    }

    mod parse {
        use super::*;

        #[test]
        fn accepts_valid() {
            assert!(Expr::parse("a").is_ok())
        }

        #[test]
        fn rejects_invalid() {
            assert!(Expr::parse("[").is_err())
        }
    }

    mod parse_as_list {
        use super::*;

        #[test]
        fn single_item() {
            let parsed = Expr::parse_as_list("a").unwrap();

            assert_eq!(1, parsed.len());
            assert_eq!("a", parsed[0].raw);
        }

        #[test]
        fn multiple_items() {
            let parsed = Expr::parse_as_list("a b").unwrap();

            assert_eq!(2, parsed.len());
            assert_eq!("a", parsed[0].raw);
            assert_eq!("b", parsed[1].raw);
        }
    }

    mod is_extractable {
        use super::*;

        #[test]
        fn ident_yes() {
            let parsed = Expr::parse("a").unwrap();
            assert!(parsed.is_extractable());
        }

        #[test]
        fn call_no() {
            let parsed = Expr::parse("haskellPackages.ghcWithPackages (ps: [ ps.text ])").unwrap();
            assert!(!parsed.is_extractable());
        }
    }
}
