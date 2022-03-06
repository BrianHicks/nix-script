use super::input::Input;
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Inputs(Vec<Input>);

impl Inputs {
    pub fn new(inputs: Vec<Input>) -> Self {
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

    mod to_string {
        use super::*;

        #[test]
        fn with_one() {
            assert_eq!(
                String::from("{ pkgs }"),
                Inputs::new(vec![Input::new("pkgs".into(), None)]).to_string(),
            )
        }

        #[test]
        fn with_many() {
            assert_eq!(
                String::from("{ pkgs, jq ? pkgs.jq }"),
                Inputs::new(vec![
                    Input::new("pkgs".into(), None),
                    Input::new("jq".into(), Some("pkgs.jq".into()))
                ])
                .to_string(),
            )
        }

        #[test]
        fn with_one_valid() {
            crate::derivation::tests::assert_no_errors(&format!(
                "{}: 1",
                Inputs::new(vec![Input::new("pkgs".into(), None)])
            ))
        }

        #[test]
        fn with_many_valid() {
            crate::derivation::tests::assert_no_errors(&format!(
                "{}: 1",
                Inputs::new(vec![
                    Input::new("pkgs".into(), None),
                    Input::new("jq".into(), Some("pkgs.jq".into()))
                ])
            ))
        }
    }
}
