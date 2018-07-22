#![feature(try_from)]
#![allow(unknown_lints)]
#![warn(clippy)]

macro_rules! error {
  ($($arg : tt)*) => {
    Err(MalError(format!($($arg)*)))
  };
}

pub mod ast;
pub mod env;
pub mod reader;

pub type MalResult<T> = Result<T, MalError>;

#[derive(Debug, Eq, PartialEq)]
pub struct MalError(String);

use std::fmt;

impl From<std::io::Error> for MalError {
  fn from(err: std::io::Error) -> Self {
    MalError(err.to_string())
  }
}

impl From<String> for MalError {
  fn from(err: String) -> Self {
    MalError(err)
  }
}

impl fmt::Display for MalError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}
