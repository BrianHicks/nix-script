use super::input::Input;
use std::convert::From;
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Inputs(Vec<Input>);

impl Inputs {
    pub fn push(&mut self, input: Input) {
        self.0.push(input)
    }
}

impl From<Vec<Input>> for Inputs {
    fn from(inputs: Vec<Input>) -> Self {
        Inputs(inputs)
    }
}

impl Display for Inputs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{{ {} }}",
            self.0
                .iter()
                .map(|input| input.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::derivation::tests::assert_no_errors;

    mod to_string {
        use super::*;

        #[test]
        fn with_one() {
            assert_eq!(
                String::from("{ pkgs }"),
                Inputs::from(vec![Input::new("pkgs".into(), None)]).to_string(),
            )
        }

        #[test]
        fn with_many() {
            assert_eq!(
                String::from("{ pkgs, jq ? pkgs.jq }"),
                Inputs::from(vec![
                    Input::new("pkgs".into(), None),
                    Input::new("jq".into(), Some("pkgs.jq".into()))
                ])
                .to_string(),
            )
        }

        #[test]
        fn with_one_valid() {
            assert_no_errors(&format!(
                "{}: 1",
                Inputs::from(vec![Input::new("pkgs".into(), None)])
            ))
        }

        #[test]
        fn with_many_valid() {
            assert_no_errors(&format!(
                "{}: 1",
                Inputs::from(vec![
                    Input::new("pkgs".into(), None),
                    Input::new("jq".into(), Some("pkgs.jq".into()))
                ])
            ))
        }
    }
}
