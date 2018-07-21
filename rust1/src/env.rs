use ast::{Atom, Value};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use MalResult;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Env {
  env: HashMap<Atom, Value>,
  outer: Option<EvalEnv>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct EvalEnv(Rc<RefCell<Env>>);

impl EvalEnv {
  fn new(env: HashMap<Atom, Value>, outer: Option<EvalEnv>, binds: Vec<(Atom, Value)>) -> Self {
    let mut eval_env = EvalEnv(Rc::new(RefCell::new(Env { env, outer })));
    for (var, expr) in binds {
      eval_env.set(var, expr);
    }
    eval_env
  }

  fn borrow(&self) -> Ref<Env> {
    self.0.borrow()
  }

  fn borrow_mut(&mut self) -> RefMut<Env> {
    self.0.borrow_mut()
  }

  fn set(&mut self, var: Atom, value: Value) {
    let mut env = self.borrow_mut();
    env.env.insert(var, value);
  }

  fn find(&self, symbol: &Atom) -> Option<EvalEnv> {
    let env = self.borrow();
    if env.env.contains_key(symbol) {
      Some(self.clone())
    } else {
      match env.outer {
        Some(ref outer) => outer.find(symbol),
        None => None,
      }
    }
  }

  fn get(&self, symbol: &Atom) -> Option<Value> {
    match self.find(symbol) {
      Some(env) => {
        let env = env.borrow();
        Some(env.env[symbol].clone())
      }
      None => None,
    }
  }

  fn new_child(&self) -> EvalEnv {
    EvalEnv::new(HashMap::new(), Some(self.clone()), vec![])
  }

  fn new_child_with_binds(&self, binds: Vec<(Atom, Value)>) -> EvalEnv {
    EvalEnv::new(HashMap::new(), Some(self.clone()), binds)
  }
}

pub fn eval(mut eval_env: EvalEnv, mut value: Value) -> MalResult<Value> {
  loop {
    match value {
      Value::List(list) => if list.is_empty() {
        return Ok(Value::List(list));
      } else {
        {
          let args = &list[1..];
          match list[0] {
            Value::Symbol(ref sym) if sym == "def!" => {
              return eval_def(eval_env.clone(), &list[1..])
            }
            Value::Symbol(ref sym) if sym == "let*" => {
              if args.len() != 2 {
                return Err(format!("let* takes 2 args but was given {}", args.len()));
              }
              let mut new_env = eval_env.new_child();
              let bindings = &args[0];
              match bindings {
                Value::List(list) | Value::Vector(list) => if list.len() % 2 != 0 {
                  return Err(format!(
                    "bindings list must have an even number of elements but has {}",
                    list.len()
                  ));
                } else {
                  for chunk in list.chunks(2) {
                    let var = match &chunk[0] {
                      v @ Value::Symbol(_) => v,
                      var => return Err(format!("var {} in binding list must be a symbol", var)),
                    };
                    let val = eval(new_env.clone(), chunk[1].clone())?;
                    new_env.set(Atom::from(var.clone()), val);
                  }
                },
                _ => return Err("bindings must be a list".to_owned()),
              }
              eval_env = new_env;
              value = args[1].clone();
              continue;
            }
            Value::Symbol(ref sym) if sym == "do" => {
              if args.is_empty() {
                return Ok(Value::Nil);
              }
              for expr in &args[..args.len() - 1] {
                eval(eval_env.clone(), expr.clone())?;
              }
              value = args[args.len() - 1].clone();
              continue;
            }
            Value::Symbol(ref sym) if sym == "if" => {
              if args.len() < 2 {
                return Err(format!(
                  "if must have either 2 or 3 args but was given {}",
                  args.len()
                ));
              }
              let cond = &args[0];
              let then_expr = &args[1];
              let else_expr = if args.len() == 3 {
                Some(&args[2])
              } else {
                None
              };
              match eval(eval_env.clone(), cond.clone())? {
                Value::Nil | Value::False => match else_expr {
                  Some(else_expr) => value = else_expr.clone(),
                  _ => return Ok(Value::Nil),
                },
                _ => value = then_expr.clone(),
              }
              continue;
            }
            Value::Symbol(ref sym) if sym == "fn*" => return eval_fn(eval_env.clone(), &list[1..]),
            _ => (),
          }
        }
        let values = eval_ast(eval_env.clone(), Value::List(list))?;
        match values {
          Value::List(values) => {
            let func = &values[0];
            let args = &values[1..];
            match func {
              Value::CoreFunction { func, .. } => return apply_core_func(*func, args),
              Value::Function { params, body, env } => {
                let params = params.clone();
                let binds: Vec<(Atom, Value)> = match params
                  .iter()
                  .position(|p| p == &Atom::Symbol("&".to_owned()))
                {
                  Some(pos) if pos != params.len() - 1 => {
                    if pos > args.len() {
                      return Err(format!(
                        "{} takes at least {} args but was given {}",
                        func,
                        pos,
                        args.len()
                      ));
                    }
                    let before_params = params[..pos].to_vec();
                    let variadic_param = &params[pos + 1];
                    let before_args = args[..pos].to_vec();
                    let mut variadic_args = vec![Value::Symbol("list".to_string())];
                    variadic_args.extend_from_slice(&args[pos..]);
                    let mut binds: Vec<(Atom, Value)> = before_params
                      .into_iter()
                      .zip(before_args.into_iter())
                      .collect();
                    binds.push((variadic_param.clone(), Value::List(variadic_args)));
                    binds
                  }
                  _ => {
                    if args.len() < params.len() {
                      return Err(format!(
                        "{} takes {} args but was given {}",
                        func,
                        params.len(),
                        args.len()
                      ));
                    }
                    params.into_iter().zip(args.to_vec().into_iter()).collect()
                  }
                };

                eval_env = env.new_child_with_binds(binds.clone());
                value = Value::subst_binds(*(body.clone()), &binds);
                continue;
              }
              _ => return Err(format!("{} is not a function and cannot be applied", func)),
            }
          }
          _ => unreachable!(),
        }
      },
      value => return eval_ast(eval_env.clone(), value),
    }
  }
}

fn eval_ast(eval_env: EvalEnv, value: Value) -> MalResult<Value> {
  match value {
    Value::Symbol(sym) => match eval_env.get(&Atom::Symbol(sym.clone())) {
      Some(val) => Ok(val.clone()),
      None => Err(format!("not in env: {}", sym)),
    },
    Value::List(list) => Ok(Value::List(
      list
        .into_iter()
        .map(|l| eval(eval_env.clone(), l))
        .collect::<MalResult<Vec<Value>>>()?,
    )),
    Value::Vector(list) => Ok(Value::Vector(
      list
        .into_iter()
        .map(|l| eval(eval_env.clone(), l))
        .collect::<MalResult<Vec<Value>>>()?,
    )),
    Value::Hashmap(mut hashmap) => {
      for v in hashmap.values_mut() {
        *v = eval(eval_env.clone(), v.clone())?;
      }
      Ok(Value::Hashmap(hashmap))
    }
    _ => Ok(value),
  }
}

fn eval_def(mut eval_env: EvalEnv, args: &[Value]) -> MalResult<Value> {
  if args.len() != 2 {
    return Err(format!("def! takes 2 args but was given {}", args.len()));
  }
  let var = match args[0] {
    Value::Symbol(_) => args[0].clone(),
    ref v => return Err(format!("var {} in def! must be a symbol", v)),
  };
  let val = eval(eval_env.clone(), args[1].clone())?;
  eval_env.set(Atom::from(var), val.clone());
  Ok(val)
}

fn eval_fn(eval_env: EvalEnv, args: &[Value]) -> MalResult<Value> {
  if args.len() != 2 {
    return Err(format!("fn* must have 2 args but was given {}", args.len()));
  }
  let params: Vec<Atom> = match args[0] {
    Value::List(ref list) => list
      .iter()
      .map(|l| match l {
        Value::Symbol(_) => Ok(Atom::from(l.clone())),
        _ => Err(format!("param {} is not a symbol", l)),
      })
      .collect::<MalResult<Vec<Atom>>>()?,
    Value::Vector(ref vector) => vector
      .iter()
      .map(|v| match v {
        Value::Symbol(_) => Ok(Atom::from(v.clone())),
        _ => Err(format!("param {} is not a symbol", v)),
      })
      .collect::<MalResult<Vec<Atom>>>()?,
    _ => {
      return Err(format!(
        "params list must be either a vector or list, got {}",
        args[0]
      ))
    }
  };
  Ok(Value::Function {
    params,
    body: Box::new(args[1].clone()),
    env: eval_env,
  })
}

fn apply_core_func(func: fn(Vec<Value>) -> MalResult<Value>, args: &[Value]) -> MalResult<Value> {
  func(args.to_vec())
}

impl Default for EvalEnv {
  fn default() -> Self {
    let binds = vec![
      (
        Atom::Symbol("+".to_owned()),
        Value::CoreFunction {
          name: "+",
          func: |args| {
            let mut sum = 0;
            for arg in args {
              match arg {
                Value::Int(i) => sum += i,
                _ => return Err(format!("cannot apply + to {}", arg)),
              }
            }
            Ok(Value::Int(sum))
          },
        },
      ),
      (
        Atom::Symbol("-".to_owned()),
        Value::CoreFunction {
          name: "-",
          func: |args| {
            if args.is_empty() {
              return Ok(Value::Int(0));
            }
            let mut diff = match args[0] {
              Value::Int(i) => i,
              _ => return Err(format!("cannot apply - to {}", args[0])),
            };
            for arg in &args[1..] {
              match arg {
                Value::Int(i) => diff -= i,
                _ => return Err(format!("cannot apply - to {}", arg)),
              }
            }
            Ok(Value::Int(diff))
          },
        },
      ),
      (
        Atom::Symbol("*".to_owned()),
        Value::CoreFunction {
          name: "*",
          func: |args| {
            let mut prod = 1;
            for arg in args {
              match arg {
                Value::Int(i) => prod *= i,
                _ => return Err(format!("cannot apply + to {}", arg)),
              }
            }
            Ok(Value::Int(prod))
          },
        },
      ),
      (
        Atom::Symbol("/".to_owned()),
        Value::CoreFunction {
          name: "/",
          func: |args| {
            if args.is_empty() {
              return Ok(Value::Int(1));
            }
            let mut quot = match args[0] {
              Value::Int(i) => i,
              _ => return Err(format!("cannot apply / to {}", args[0])),
            };
            for arg in &args[1..] {
              match arg {
                Value::Int(i) => quot /= i,
                _ => return Err(format!("cannot apply / to {}", arg)),
              }
            }
            Ok(Value::Int(quot))
          },
        },
      ),
      (
        Atom::Symbol("list".to_owned()),
        Value::CoreFunction {
          name: "list",
          func: |args| Ok(Value::List(args)),
        },
      ),
      (
        Atom::Symbol("list?".to_owned()),
        Value::CoreFunction {
          name: "list?",
          func: |args| {
            if args.is_empty() {
              Err("cannot apply list? to 0 args".to_owned())
            } else {
              Ok(match args[0] {
                Value::List(_) => Value::True,
                _ => Value::False,
              })
            }
          },
        },
      ),
      (
        Atom::Symbol("empty?".to_owned()),
        Value::CoreFunction {
          name: "empty?",
          func: |args| {
            if args.is_empty() {
              Err("cannot apply empty? to 0 args".to_owned())
            } else {
              Ok(match args[0] {
                Value::List(ref list) | Value::Vector(ref list) => Value::from(list.is_empty()),
                _ => Value::False,
              })
            }
          },
        },
      ),
      (
        Atom::Symbol("count".to_owned()),
        Value::CoreFunction {
          name: "count",
          func: |args| {
            if args.is_empty() {
              Err("cannot call count with 0 args".to_owned())
            } else {
              match args[0] {
                Value::List(ref list) | Value::Vector(ref list) => {
                  Ok(Value::Int(list.len() as isize))
                }
                Value::Nil => Ok(Value::Int(0)),
                _ => Err("cannot apply count to non-list".to_owned()),
              }
            }
          },
        },
      ),
      (
        Atom::Symbol("=".to_owned()),
        Value::CoreFunction {
          name: "=",
          func: |args| {
            if args.len() < 2 {
              return Err("cannot apply = to less than 2 args".to_owned());
            }
            Ok(Value::from(args[0].list_eq(&args[1])))
          },
        },
      ),
      (
        Atom::Symbol(">".to_owned()),
        Value::CoreFunction {
          name: ">",
          func: |args| {
            if args.len() < 2 {
              return Err("cannot apply > to less than 2 args".to_owned());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a > b)),
              _ => Err(format!(
                "cannot apply > to non-ints: {} {}",
                args[0], args[1]
              )),
            }
          },
        },
      ),
      (
        Atom::Symbol("<".to_owned()),
        Value::CoreFunction {
          name: "<",
          func: |args| {
            if args.len() < 2 {
              return Err("cannot apply < to less than 2 args".to_owned());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a < b)),
              _ => Err(format!(
                "cannot apply < to non-ints: {} {}",
                args[0], args[1]
              )),
            }
          },
        },
      ),
      (
        Atom::Symbol(">=".to_owned()),
        Value::CoreFunction {
          name: ">=",
          func: |args| {
            if args.len() < 2 {
              return Err("cannot apply >= to less than 2 args".to_owned());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a >= b)),
              _ => Err(format!(
                "cannot apply >= to non-ints: {} {}",
                args[0], args[1]
              )),
            }
          },
        },
      ),
      (
        Atom::Symbol("<=".to_owned()),
        Value::CoreFunction {
          name: "<=",
          func: |args| {
            if args.len() < 2 {
              return Err("cannot apply <= to less than 2 args".to_owned());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a <= b)),
              _ => Err(format!(
                "cannot apply <= to non-ints: {} {}",
                args[0], args[1]
              )),
            }
          },
        },
      ),
      (
        Atom::Symbol("pr-str".to_owned()),
        Value::CoreFunction {
          name: "pr-str",
          func: |args| {
            Ok(Value::Str(
              args
                .into_iter()
                .map(|a| a.string(true))
                .collect::<Vec<String>>()
                .join(" "),
            ))
          },
        },
      ),
      (
        Atom::Symbol("str".to_owned()),
        Value::CoreFunction {
          name: "str",
          func: |args| {
            Ok(Value::Str(
              args
                .into_iter()
                .map(|a| a.string(false))
                .collect::<Vec<String>>()
                .join(""),
            ))
          },
        },
      ),
      (
        Atom::Symbol("prn".to_owned()),
        Value::CoreFunction {
          name: "prn",
          func: |args| {
            println!(
              "{}",
              args
                .into_iter()
                .map(|a| a.string(true))
                .collect::<Vec<String>>()
                .join(" ")
            );
            Ok(Value::Nil)
          },
        },
      ),
      (
        Atom::Symbol("println".to_owned()),
        Value::CoreFunction {
          name: "println",
          func: |args| {
            println!(
              "{}",
              args
                .into_iter()
                .map(|a| a.string(false))
                .collect::<Vec<String>>()
                .join(" ")
            );
            Ok(Value::Nil)
          },
        },
      ),
    ];
    EvalEnv::new(HashMap::new(), None, binds)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_eval1() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("+").to_owned(),
        Value::Int(1),
        Value::Int(2),
      ])),
      Ok(Value::Int(3))
    );
  }

  #[test]
  fn test_eval2() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("+").to_owned(),
        Value::Int(1),
        Value::List(vec![
          Value::Symbol("*").to_owned(),
          Value::Int(2),
          Value::Int(3),
        ]),
      ])),
      Ok(Value::Int(7))
    );
  }

  #[test]
  fn test_def() {
    let mut eval_env = EvalEnv::default();
    assert!(
      eval_env
        .eval(Value::List(vec![
          Value::Symbol("def!").to_owned(),
          Value::Symbol("a").to_owned(),
          Value::Int(6),
        ]))
        .is_ok()
    );
    assert_eq!(
      eval_env.eval(Value::Symbol("a")).to_owned(),
      Ok(Value::Int(6))
    );
    assert!(
      eval_env
        .eval(Value::List(vec![
          Value::Symbol("def!").to_owned(),
          Value::Symbol("b").to_owned(),
          Value::List(vec![
            Value::Symbol("+").to_owned(),
            Value::Symbol("a").to_owned(),
            Value::Int(2),
          ]),
        ]))
        .is_ok()
    );
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("+").to_owned(),
        Value::Symbol("a").to_owned(),
        Value::Symbol("b").to_owned(),
      ])),
      Ok(Value::Int(14))
    );
  }

  #[test]
  fn test_let() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("let*").to_owned(),
        Value::List(vec![
          Value::Symbol("c").to_owned(),
          Value::Int(2),
          Value::Symbol("d").to_owned(),
          Value::List(vec![
            Value::Symbol("+").to_owned(),
            Value::Symbol("c").to_owned(),
            Value::Int(2),
          ]),
        ]),
        Value::List(vec![
          Value::Symbol("+").to_owned(),
          Value::Symbol("c").to_owned(),
          Value::Symbol("d").to_owned(),
        ]),
      ])),
      Ok(Value::Int(6))
    );
  }

  #[test]
  fn test_def_fail() {
    let mut eval_env = EvalEnv::default();
    assert!(
      eval_env
        .eval(Value::List(vec![
          Value::Symbol("def!").to_owned(),
          Value::Keyword("a").to_owned(),
          Value::Int(6),
        ]))
        .is_err()
    );
  }

  #[test]
  fn test_let_fail() {
    let mut eval_env = EvalEnv::default();
    assert!(
      eval_env
        .eval(Value::List(vec![
          Value::Symbol("let*").to_owned(),
          Value::List(vec![Value::Keyword("c".to_owned()), Value::Int(2)]),
          Value::List(vec![
            Value::Symbol("+").to_owned(),
            Value::Symbol("c").to_owned(),
            Value::Int(1),
          ]),
        ]))
        .is_err()
    );
  }

  #[test]
  fn test_do1() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![Value::Symbol("do".to_owned())])),
      Ok(Value::Nil)
    );
  }

  #[test]
  fn test_do2() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("do").to_owned(),
        Value::Int(1),
      ])),
      Ok(Value::Int(1))
    );
  }

  #[test]
  fn test_if1() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::Symbol("if").to_owned(),
        Value::List(vec![
          Value::Symbol("if").to_owned(),
          Value::Nil,
          Value::False,
          Value::True,
        ]),
        Value::Int(1),
        Value::Int(2),
      ])),
      Ok(Value::Int(1))
    );
  }

  #[test]
  fn test_fn1() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::List(vec![
          Value::Symbol("fn*").to_owned(),
          Value::List(vec![Value::Symbol("a".to_owned())]),
          Value::Symbol("a").to_owned(),
        ]),
        Value::Int(7),
      ])),
      Ok(Value::Int(7))
    );
  }

  #[test]
  fn test_fn2() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::List(vec![
          Value::Symbol("fn*".to_owned()),
          Value::List(vec![
            Value::Symbol("a".to_owned()),
            Value::Symbol("b".to_owned()),
          ]),
          Value::List(vec![
            Value::Symbol("+".to_owned()),
            Value::Symbol("a".to_owned()),
            Value::Symbol("b".to_owned()),
          ]),
        ]),
        Value::Int(2),
        Value::Int(3),
      ])),
      Ok(Value::Int(5))
    );
  }

  #[test]
  fn test_fn_fail() {
    let mut eval_env = EvalEnv::default();
    assert!(
      eval_env
        .eval(Value::List(vec![
          Value::List(vec![
            Value::Symbol("fn*").to_owned(),
            Value::List(vec![
              Value::Symbol("a").to_owned(),
              Value::Symbol("b").to_owned(),
            ]),
            Value::List(vec![
              Value::Symbol("+").to_owned(),
              Value::Symbol("a").to_owned(),
              Value::Symbol("b").to_owned(),
            ]),
          ]),
          Value::Int(2),
        ]))
        .is_err()
    );
  }

  #[test]
  fn test_nested_fn() {
    let mut eval_env = EvalEnv::default();
    assert_eq!(
      eval_env.eval(Value::List(vec![
        Value::List(vec![
          Value::List(vec![
            Value::Symbol("fn*").to_owned(),
            Value::List(vec![Value::Symbol("a".to_owned())]),
            Value::List(vec![
              Value::Symbol("fn*").to_owned(),
              Value::List(vec![Value::Symbol("b".to_owned())]),
              Value::List(vec![
                Value::Symbol("+".to_owned()),
                Value::Symbol("a".to_owned()),
                Value::Symbol("b".to_owned()),
              ]),
            ]),
          ]),
          Value::Int(5),
        ]),
        Value::Int(7),
      ])),
      Ok(Value::Int(12))
    )
  }
}
