extern crate mal;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use mal::ast::Ast;
use mal::reader::{Reader, ReaderResult};

pub fn read(input: String) -> ReaderResult<Ast> {
    Reader::read_str(&input)
}

pub fn eval(ast: Ast) -> Ast {
    ast
}

pub fn print(ast: Ast) -> String {
    ast.string(true)
}

pub fn rep(input: String) -> ReaderResult<String> {
    Ok(print(eval(read(input)?)))
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history");
    }
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(ref line) if line.is_empty() => (),
            Ok(line) => match rep(line) {
                Ok(s) => println!("{}", s),
                Err(err) => println!("{}", err),
            },
            Err(ReadlineError::Interrupted) => {
                println!("C-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("C-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
