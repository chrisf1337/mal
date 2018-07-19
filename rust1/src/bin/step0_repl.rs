extern crate mal;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn read(input: String) -> String {
    input
}

pub fn eval(input: String) -> String {
    input
}

pub fn print(input: String) -> String {
    input
}

pub fn rep(input: String) -> String {
    print(eval(read(input)))
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history");
    }
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => println!("{}", rep(line)),
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
