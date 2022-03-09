use anyhow::{Context, Result};
use rnix::types::{List, TypedNode, Wrapper};
use rnix::{SyntaxKind, SyntaxNode};
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Expr {
    raw: String,
    parsed: SyntaxNode,
}

impl Expr {
    pub fn parse(source: &str) -> Result<Self> {
        Ok(Self::from_node(
            rnix::parse(source)
                .as_result()
                .context("failed to parse the source")?
                .root()
                .inner()
                .context("root node did not have an inner node")?,
        ))
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
                .map(|node| Self::from_node(node))
                .collect(),
        )
    }

    fn from_node(node: SyntaxNode) -> Expr {
        let mut out = node;

        loop {
            if out.kind() == SyntaxKind::NODE_PAREN {
                if let Some(inner) = out.children().next() {
                    out = inner;
                    continue;
                }
            }

            break;
        }

        Self {
            raw: out.to_string(),
            parsed: out,
        }
    }

    pub fn is_extractable(&self) -> bool {
        match self.parsed.kind() {
            SyntaxKind::NODE_IDENT => true,
            _ => false,
        }
    }

    pub fn needs_parens_in_list(&self) -> bool {
        // We're explicit that we don't need tokens most of the time (instead
        // of being explicit when we *do* need them) since it's always safe to
        // add more parentheses but not always safe to leave them off.
        match self.parsed.kind() {
            SyntaxKind::NODE_IDENT => false,
            _ => true,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.raw)
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

        #[test]
        fn unwraps_root() {
            assert_eq!(
                SyntaxKind::NODE_IDENT,
                Expr::parse("a").unwrap().parsed.kind()
            )
        }

        #[test]
        fn unwraps_parens() {
            assert_eq!(
                SyntaxKind::NODE_IDENT,
                Expr::parse("(a)").unwrap().parsed.kind()
            )
        }

        #[test]
        fn unwraps_all_parens() {
            assert_eq!(
                SyntaxKind::NODE_IDENT,
                Expr::parse("((a))").unwrap().parsed.kind()
            )
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
        fn apply_no() {
            let parsed = Expr::parse("haskellPackages.ghcWithPackages (ps: [ ps.text ])").unwrap();
            assert!(!parsed.is_extractable());
        }
    }

    mod needs_parens_in_list {
        use super::*;

        #[test]
        fn ident_no() {
            let parsed = Expr::parse("a").unwrap();
            assert!(!parsed.needs_parens_in_list());
        }

        #[test]
        fn apply_yes() {
            let parsed = Expr::parse("haskellPackages.ghcWithPackages (ps: [ ps.text ])").unwrap();
            assert!(parsed.needs_parens_in_list());
        }
    }

    mod display {
        use super::*;

        #[test]
        fn same_as_node() {
            let parsed = Expr::parse("a b c").unwrap();
            assert_eq!(parsed.to_string(), parsed.parsed.to_string());
        }
    }
}
