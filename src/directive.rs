use std::collections::HashMap;

pub struct Parser {
    indicator: String,
}

impl Parser {
    pub fn new(indicator: &str) -> Self {
        Parser {
            indicator: indicator.to_string(),
        }
    }

    pub fn parse<'a>(&self, source: &'a str) -> HashMap<&'a str, &'a str> {
        source
            .lines()
            .flat_map(|line| {
                if line.starts_with(&self.indicator) {
                    let without_indicator = line[self.indicator.len()..].trim_start();
                    let mut words = without_indicator.split(' ');

                    words
                        .next()
                        .map(|first| (first, without_indicator[first.len()..].trim_start()))
                } else {
                    None
                }
            })
            .filter(|(_, v)| !v.is_empty())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod from_string {
        use super::*;

        #[test]
        fn blank_is_blank() {
            let directives = Parser::new("#!").parse("");

            assert!(directives.is_empty());
        }

        #[test]
        fn ignores_non_shebangs() {
            let directives = Parser::new("#!").parse("nope");

            assert!(directives.is_empty());
        }

        #[test]
        fn matches_shebangs() {
            let directives = Parser::new("#!").parse("#!buildInputs jq");

            assert_eq!(Some(&"jq"), directives.get("buildInputs"));
        }

        #[test]
        fn matches_comment_chars() {
            let directives = Parser::new("//").parse("// buildInputs jq");

            assert_eq!(Some(&"jq"), directives.get("buildInputs"));
        }

        #[test]
        fn removes_empty_directives() {
            let directives = Parser::new("#!").parse("#!buildInputs");

            assert_eq!(None, directives.get("buildInputs"));
        }
    }
}
