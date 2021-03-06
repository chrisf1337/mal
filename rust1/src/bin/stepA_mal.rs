extern crate mal;
extern crate rustyline;

use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use rustyline::hint::Hinter;
use rustyline::{Cmd, CompletionType, Config, EditMode, Editor, KeyPress};

use mal::ast::{Ast, Atom, Value};
use mal::env;
use mal::env::EvalEnv;
use mal::reader::{Reader, ReaderResult};
use mal::MalResult;

pub fn read(input: String) -> ReaderResult<Ast> {
  Reader::read_str(&input)
}

pub fn eval(eval_env: EvalEnv, ast: Ast) -> MalResult<Value> {
  let value = Value::from(ast);
  env::eval(eval_env, value, false)
}

pub fn print(ast: Value) -> String {
  ast.string(true)
}

pub fn rep(eval_env: EvalEnv, input: String) -> MalResult<String> {
  Ok(print(eval(eval_env.clone(), read(input)?)?))
}

struct Hints {}

impl Hinter for Hints {
  fn hint(&self, _line: &str, _pos: usize) -> Option<String> {
    None
  }
}

fn main() -> MalResult<()> {
  let mut args: Vec<String> = ::std::env::args().collect();
  args.remove(0);
  let mut eval_env = EvalEnv::default();
  rep(
    eval_env.clone(),
    "(def! not (fn* (a) (if a false true)))".to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))"#.to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(defmacro! cond
      (fn* (& xs)
        (if (> (count xs) 0)
            (list 'if (first xs)
                      (if (> (count xs) 1)
                          (nth xs 1)
                          (throw "odd number of forms to cond"))
                          (cons 'cond (rest (rest xs)))))))"#.to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(defmacro! or
      (fn* (& xs)
        (if (empty? xs)
            nil
            (if (= 1 (count xs))
                (first xs)
                `(let* (or_FIXME ~(first xs))
                       (if or_FIXME or_FIXME (or ~@(rest xs))))))))"#.to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(def! *gensym-counter* (atom 0))"#.to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(def! gensym (fn* [] (symbol (str "G__" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"#.to_owned(),
  )?;
  rep(
    eval_env.clone(),
    r#"(defmacro! or
        (fn* (& xs)
             (if (empty? xs)
                 nil
                 (if (= 1 (count xs))
                     (first xs)
                     (let* (condvar (gensym))
                       `(let* (~condvar ~(first xs))
                         (if ~condvar ~condvar (or ~@(rest xs)))))))))"#.to_owned(),
  )?;

  if args.is_empty() {
    rep(
      eval_env.clone(),
      r#"(println (str "Mal [" *host-language* "]"))"#.to_owned(),
    )?;
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

    loop {
      let readline = rl.readline("user> ");
      match readline {
        Ok(ref line) if line.is_empty() => (),
        Ok(line) => {
          rl.add_history_entry(line.as_ref());
          match rep(eval_env.clone(), line) {
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
    rl.save_history("history.txt")?;
  } else {
    eval_env.set(
      Atom::Symbol("*ARGV*".to_owned()),
      Value::new_list(
        (&args[1..])
          .to_vec()
          .into_iter()
          .map(|arg| Value::Str(arg))
          .collect(),
      ),
    );
    rep(
      eval_env.clone(),
      format!("(load-file \"{}\")", args[0].clone()),
    )?;
  }
  Ok(())
}
