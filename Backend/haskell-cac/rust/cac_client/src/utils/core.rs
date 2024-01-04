use std::fmt;

pub trait MapError<T> {
    fn map_err_to_string(self) -> Result<T, String>;
}

impl<T, E> MapError<T> for Result<T, E>
where
    E: fmt::Display,
{
    fn map_err_to_string(self) -> Result<T, String> {
        self.map_err(|e| e.to_string())
    }
}
