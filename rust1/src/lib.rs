#![feature(try_from)]
#![allow(unknown_lints)]
#![warn(clippy)]

pub mod ast;
pub mod env;
pub mod reader;

pub type MalResult<T> = Result<T, String>;
