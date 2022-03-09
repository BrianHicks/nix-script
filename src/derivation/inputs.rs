use std::collections::HashMap;
use std::convert::From;
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Inputs(HashMap<String, Option<String>>);

impl Inputs {
    pub fn new() -> Self {
        Inputs(HashMap::new())
    }

    pub fn insert(&mut self, name: String, default: Option<String>) {
        self.0.insert(name, default);
    }
}

impl From<Vec<(String, Option<String>)>> for Inputs {
    fn from(inputs: Vec<(String, Option<String>)>) -> Self {
        let mut out = Inputs::new();

        for (name, default) in inputs {
            out.insert(name, default);
        }

        out
    }
}

impl Display for Inputs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{{ ")?;

        let mut done_with_first = false;

        for (key, default) in &self.0 {
            if done_with_first {
                write!(f, ", ")?;
            } else {
                done_with_first = true;
            }

            write!(f, "{}", key)?;
            if let Some(value) = default {
                write!(f, " ? {}", value)?;
            }
        }

        write!(f, " }}")
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
                Inputs::from(vec![("pkgs".into(), None)]).to_string(),
            )
        }

        #[test]
        fn with_many() {
            assert_eq!(
                String::from("{ pkgs, jq ? pkgs.jq }"),
                Inputs::from(vec![
                    ("pkgs".into(), None),
                    ("jq".into(), Some("pkgs.jq".into()))
                ])
                .to_string(),
            )
        }

        #[test]
        fn with_one_valid() {
            assert_no_errors(&format!("{}: 1", Inputs::from(vec![("pkgs".into(), None)])))
        }

        #[test]
        fn with_many_valid() {
            assert_no_errors(&format!(
                "{}: 1",
                Inputs::from(vec![
                    ("pkgs".into(), None),
                    ("jq".into(), Some("pkgs.jq".into()))
                ])
            ))
        }
    }

    mod insert {
        use super::*;

        #[test]
        fn adds_when_not_present() {
            let mut inputs = Inputs::new();
            inputs.insert("pkgs".into(), None);
            assert_eq!("{ pkgs }", inputs.to_string());
        }

        #[test]
        fn override_when_present() {
            let mut inputs = Inputs::new();
            inputs.insert("pkgs".into(), None);
            inputs.insert("pkgs".into(), Some("default".into()));
            assert_eq!("{ pkgs ? default }", inputs.to_string())
        }
    }
}
