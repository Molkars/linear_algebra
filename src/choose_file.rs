use std::fmt::{Display, Formatter};
use std::fs::read_dir;
use std::io;
use std::path::{Path, PathBuf};

use inquire::Select;

pub fn choose_file(
    start: impl AsRef<Path>,
    filter: impl Fn(&String) -> bool,
) -> io::Result<PathBuf> {
    let mut dir = start.as_ref().to_path_buf();

    loop {
        let items = items(&dir, &filter)?;
        let result = Select::new("Select file/dir", items)
            .prompt().unwrap();
        match result {
            Item::File(name) => {
                let path = dir.join(name);
                return Ok(path);
            }
            Item::Dir(name) => dir = dir.join(name),
        }
    }
}

enum Item {
    Dir(String),
    File(String),
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Dir(name) => write!(f, "{}", name),
            Item::File(name) => write!(f, "{}", name),
        }
    }
}

fn items(
    buf: &PathBuf,
    filter: &impl Fn(&String) -> bool,
) -> io::Result<Vec<Item>> {
    std::fs::read_dir(buf)
        .and_then(|read_dir| {
            read_dir
                .into_iter()
                .map(|item| {
                    let item = item?;
                    let meta = item.metadata()?;
                    Ok((item, meta))
                })
                .map(|val| {
                    val.map(|(item, meta)| {
                        let name = item.file_name().to_string_lossy().to_string();
                        if meta.is_dir() {
                            Some(Item::Dir(name))
                        } else {
                            let ext = item.path().extension().map(|s| s.to_string_lossy().to_string());
                            if ext.filter(|v| filter(v)).is_some() {
                                Some(Item::File(name))
                            } else {
                                None
                            }
                        }
                    })
                })
                .filter_map(extract)
                .collect::<io::Result<Vec<_>>>()
        })
}

fn extract<T, E>(v: Result<Option<T>, E>) -> Option<Result<T, E>> {
    match v {
        Ok(opt) => opt.map(Result::Ok),
        Err(e) => Some(Err(e))
    }
}