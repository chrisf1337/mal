extern crate mal;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use mal::ast::{Ast, Value};
use mal::env::EvalEnv;
use mal::reader::{Reader, ReaderResult};
use mal::MalResult;

pub fn read(input: String) -> ReaderResult<Ast> {
    Reader::read_str(&input)
}

pub fn eval(ast: Ast, env: &mut EvalEnv) -> MalResult<Value> {
    let value = Value::from(ast);
    env.eval(value)
}

pub fn print(ast: Value) -> String {
    ast.string(false)
}

pub fn rep(input: String, env: &mut EvalEnv) -> MalResult<String> {
    Ok(print(eval(read(input)?, env)?))
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history");
    }
    let mut eval_env = EvalEnv::default();
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(ref line) if line.is_empty() => (),
            Ok(line) => match rep(line, &mut eval_env) {
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
    rl.save_history("history.txt").unwrap();
}
