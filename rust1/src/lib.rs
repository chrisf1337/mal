#![feature(try_from)]
#![allow(unknown_lints)]
#![warn(clippy)]

pub mod ast;
use ast::Value;

macro_rules! error {
  ($($arg : tt)*) => {
    Err(MalError(Value::Str(format!($($arg)*))))
  };
}

extern crate rustyline;

pub mod env;
pub mod reader;

pub type MalResult<T> = Result<T, MalError>;

#[derive(Debug, Eq, PartialEq)]
pub struct MalError(ast::Value);

use std::fmt;

impl From<std::io::Error> for MalError {
  fn from(err: std::io::Error) -> Self {
    MalError(Value::Str(err.to_string()))
  }
}

impl From<String> for MalError {
  fn from(err: String) -> Self {
    MalError(Value::Str(err))
  }
}

impl From<rustyline::error::ReadlineError> for MalError {
  fn from(err: rustyline::error::ReadlineError) -> Self {
    MalError(Value::Str(err.to_string()))
  }
}

impl fmt::Display for MalError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}
