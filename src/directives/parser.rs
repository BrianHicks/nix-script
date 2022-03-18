use anyhow::Result;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Parser {
    indicator: String,
}

impl Parser {
    pub fn new(indicator: &str) -> Result<Self> {
        if indicator.is_empty() {
            anyhow::bail!("a blank indicator is not allowed")
        }

        Ok(Parser {
            indicator: indicator.to_string(),
        })
    }

    pub fn parse<'a>(&self, source: &'a str) -> HashMap<&'a str, Vec<&'a str>> {
        let mut out = HashMap::new();

        for line in source.lines() {
            if !line.starts_with(&self.indicator) {
                continue;
            }

            let without_indicator = line[self.indicator.len()..].trim_start();
            let mut words = without_indicator.split(' ');

            if let Some(key) = words.next() {
                let value = without_indicator[key.len()..].trim_start();

                if value.is_empty() {
                    log::warn!("skipping directive \"{}\" because value was empty", key);
                    continue;
                }

                let entry = out.entry(key).or_insert_with(|| Vec::new());
                entry.push(value);
            }
        }

        return out;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod new {
        use super::*;

        #[test]
        fn blank_indicator_is_not_allowed() {
            assert_eq!(
                "a blank indicator is not allowed",
                Parser::new("").unwrap_err().to_string(),
            )
        }
    }

    mod parse {
        use super::*;

        #[test]
        fn blank_is_blank() {
            let directives = Parser::new("#!").unwrap().parse("");

            assert!(directives.is_empty());
        }

        #[test]
        fn ignores_non_shebangs() {
            let directives = Parser::new("#!").unwrap().parse("nope");

            assert!(directives.is_empty());
        }

        #[test]
        fn matches_shebangs() {
            let directives = Parser::new("#!").unwrap().parse("#!buildInputs jq");

            assert_eq!(Some(&vec!["jq"]), directives.get("buildInputs"));
        }

        #[test]
        fn matches_comment_chars() {
            let directives = Parser::new("//").unwrap().parse("// buildInputs jq");

            assert_eq!(Some(&vec!["jq"]), directives.get("buildInputs"));
        }

        #[test]
        fn removes_empty_directives() {
            let directives = Parser::new("#!").unwrap().parse("#!buildInputs");

            assert_eq!(None, directives.get("buildInputs"));
        }

        #[test]
        fn combines_multiple_lines() {
            let directives = Parser::new("#!")
                .unwrap()
                .parse("#!buildInputs a\n#!buildInputs b");

            assert_eq!(Some(&vec!["a", "b"]), directives.get("buildInputs"));
        }
    }
}
