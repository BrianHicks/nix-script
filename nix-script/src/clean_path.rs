use anyhow::Result;
use std::path::{Component, Path, PathBuf};

lazy_static::lazy_static! {
    static ref ROOT: PathBuf = PathBuf::from("/");
}

pub fn clean_path(path: &Path) -> Result<PathBuf> {
    if path.is_relative() {
        let components: Vec<Component> = path.components().collect();

        match components.get(0) {
            Some(Component::CurDir) => {
                if components.len() == 1 {
                    Ok(PathBuf::from("./."))
                } else {
                    Ok(path.to_owned())
                }
            }
            Some(_) => Ok(PathBuf::from(".").join(path)),
            None => anyhow::bail!("couldn't generate a Nix-safe version of a blank path"),
        }
    } else if path == *ROOT {
        Ok(PathBuf::from("/."))
    } else {
        Ok(path.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(
            String::from("couldn't generate a Nix-safe version of a blank path"),
            clean_path(&PathBuf::new()).unwrap_err().to_string()
        )
    }

    #[test]
    fn relative() {
        assert_eq!(
            String::from("./foo"),
            clean_path(&PathBuf::from("foo"))
                .unwrap()
                .display()
                .to_string()
        )
    }

    #[test]
    fn relative_starting_with_current_directory() {
        assert_eq!(
            String::from("./foo"),
            clean_path(&PathBuf::from("./foo"))
                .unwrap()
                .display()
                .to_string()
        )
    }

    #[test]
    fn current_directory() {
        assert_eq!(
            String::from("./."),
            clean_path(&PathBuf::from("."))
                .unwrap()
                .display()
                .to_string()
        )
    }

    #[test]
    fn absolute() {
        assert_eq!(
            String::from("/foo"),
            clean_path(&PathBuf::from("/foo"))
                .unwrap()
                .display()
                .to_string()
        )
    }

    #[test]
    fn only_root() {
        assert_eq!(
            String::from("/."),
            clean_path(&PathBuf::from("/"))
                .unwrap()
                .display()
                .to_string()
        )
    }
}
