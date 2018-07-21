extern crate mal;
extern crate rustyline;

use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use rustyline::hint::Hinter;
use rustyline::{Cmd, CompletionType, Config, EditMode, Editor, KeyPress};

use mal::ast::{Ast, Value};
use mal::env;
use mal::env::EvalEnv;
use mal::reader::{Reader, ReaderResult};
use mal::MalResult;

pub fn read(input: String) -> ReaderResult<Ast> {
    Reader::read_str(&input)
}

pub fn eval(ast: Ast, env: EvalEnv) -> MalResult<Value> {
    let value = Value::from(ast);
    env::eval(env, value)
}

pub fn print(ast: Value) -> String {
    ast.string(true)
}

pub fn rep(input: String, env: EvalEnv) -> MalResult<String> {
    Ok(print(eval(read(input)?, env.clone())?))
}

struct Hints {}

impl Hinter for Hints {
    fn hint(&self, _line: &str, _pos: usize) -> Option<String> {
        None
    }
}

fn main() {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let c = FilenameCompleter::new();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some((c, Hints {})));
    rl.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);
    if rl.load_history("history.txt").is_err() {
        println!("No previous history");
    }
    let eval_env = EvalEnv::default();
    rep(
        "(def! not (fn* (a) (if a false true)))".to_owned(),
        eval_env.clone(),
    ).expect("failed to define not");
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(ref line) if line.is_empty() => (),
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                match rep(line, eval_env.clone()) {
                    Ok(s) => println!("{}", s),
                    Err(err) => println!("{}", err),
                }
            }
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
