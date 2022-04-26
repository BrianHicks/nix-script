use anyhow::{Context, Result};
use core::hash::{Hash, Hasher};
use rnix::types::{List, TypedNode, Wrapper};
use rnix::{SyntaxKind, SyntaxNode};
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::str::FromStr;

#[derive(Debug, Eq, serde::Serialize, Clone)]
pub struct Expr {
    raw: String,
    #[serde(skip)]
    parsed: SyntaxNode,
}

impl Expr {
    pub fn parse_as_list(source: &str) -> Result<Vec<Self>> {
        let root = rnix::parse(&format!("[{}]", source))
            .as_result()
            .context("failed to parse the source when wrapping as a list")?
            .root();

        Ok(
            List::cast(root.inner().context("root did not have an inner node")?)
                .context("could not parse this list as a list")?
                .items()
                .map(Self::from_node)
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

    pub fn kind(&self) -> SyntaxKind {
        self.parsed.kind()
    }

    pub fn is_extractable(&self) -> bool {
        matches!(self.kind(), SyntaxKind::NODE_IDENT)
    }

    pub fn needs_parens_in_list(&self) -> bool {
        // We're explicit that we don't need tokens most of the time (instead
        // of being explicit when we *do* need them) since it's always safe to
        // add more parentheses but not always safe to leave them off.
        !matches!(self.kind(), SyntaxKind::NODE_IDENT)
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

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.raw.partial_cmp(&other.raw)
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write(self.raw.as_ref())
    }
}

impl FromStr for Expr {
    type Err = anyhow::Error;

    fn from_str(source: &str) -> Result<Self> {
        Ok(Self::from_node(
            rnix::parse(source)
                .as_result()
                .context("failed to parse Nix expression")?
                .root()
                .inner()
                .context("root node did not have an inner node")?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod equality {
        use super::*;

        #[test]
        fn equal_if_raw_is_equal() {
            assert_eq!(Expr::from_str("a").unwrap(), Expr::from_str("a").unwrap())
        }

        #[test]
        fn unequal_if_raw_is_unequal() {
            assert!(Expr::from_str("a").unwrap() != Expr::from_str("b").unwrap())
        }
    }

    mod parse {
        use super::*;

        #[test]
        fn accepts_valid() {
            assert!(Expr::from_str("a").is_ok())
        }

        #[test]
        fn rejects_invalid() {
            assert!(Expr::from_str("[").is_err())
        }

        #[test]
        fn unwraps_root() {
            assert_eq!(SyntaxKind::NODE_IDENT, Expr::from_str("a").unwrap().kind())
        }

        #[test]
        fn unwraps_parens() {
            assert_eq!(
                SyntaxKind::NODE_IDENT,
                Expr::from_str("(a)").unwrap().kind()
            )
        }

        #[test]
        fn unwraps_all_parens() {
            assert_eq!(
                SyntaxKind::NODE_IDENT,
                Expr::from_str("((a))").unwrap().kind()
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
            let parsed = Expr::from_str("a").unwrap();
            assert!(parsed.is_extractable());
        }

        #[test]
        fn apply_no() {
            let parsed =
                Expr::from_str("haskellPackages.ghcWithPackages (ps: [ ps.text ])").unwrap();
            assert!(!parsed.is_extractable());
        }
    }

    mod needs_parens_in_list {
        use super::*;

        #[test]
        fn ident_no() {
            let parsed = Expr::from_str("a").unwrap();
            assert!(!parsed.needs_parens_in_list());
        }

        #[test]
        fn apply_yes() {
            let parsed =
                Expr::from_str("haskellPackages.ghcWithPackages (ps: [ ps.text ])").unwrap();
            assert!(parsed.needs_parens_in_list());
        }
    }

    mod display {
        use super::*;

        #[test]
        fn same_as_node() {
            let parsed = Expr::from_str("a b c").unwrap();
            assert_eq!(parsed.to_string(), parsed.parsed.to_string());
        }
    }
}
