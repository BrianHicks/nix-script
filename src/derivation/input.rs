use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Input {
    name: String,
    default: Option<String>,
}

impl Input {
    pub fn new(name: String, default: Option<String>) -> Self {
        Self { name, default }
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)?;

        if let Some(default) = &self.default {
            write!(f, " ? {}", default)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod to_string {
        use super::*;

        #[test]
        fn no_default() {
            assert_eq!(
                String::from("pkgs"),
                Input::new("pkgs".into(), None).to_string(),
            )
        }

        #[test]
        fn with_default() {
            assert_eq!(
                String::from("pkgs ? import <nixpkgs> { }"),
                Input::new("pkgs".into(), Some("import <nixpkgs> { }".into())).to_string(),
            )
        }
    }
}
