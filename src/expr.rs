use rnix::types::Root;

#[derive(Debug)]
pub struct Expr {
    raw: String,
    parsed: Root,
}

impl Expr {
    pub fn parse(source: &str) -> Self {
        Self {
            raw: source.to_string(),
            parsed: rnix::parse(source).root(),
        }
    }

    pub fn parse_as_list(source: &str) -> Vec<Self> {
        let ast = rnix::parse(source);
        println!("{:#?}", ast.root());

        vec![]
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
            assert_eq!(Expr::parse("a"), Expr::parse("a"))
        }

        #[test]
        fn unequal_if_raw_is_unequal() {
            assert!(Expr::parse("a") != Expr::parse("b"))
        }
    }

    mod parse_as_list {
        use super::*;

        #[test]
        fn single_item() {
            let out: Vec<Expr> = Vec::new();
            assert_eq!(out, Expr::parse_as_list("a"))
        }
    }
}
