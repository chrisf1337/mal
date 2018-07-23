use ast::{Atom, Value};
use reader::Reader;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use {MalError, MalResult};

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

  pub fn set(&mut self, var: Atom, value: Value) {
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

  fn root(&self) -> EvalEnv {
    let eval_env = self.borrow();
    match eval_env.outer {
      Some(ref outer) => outer.root(),
      None => self.clone(),
    }
  }

  fn new_child(&self) -> EvalEnv {
    EvalEnv::new(HashMap::new(), Some(self.clone()), vec![])
  }

  fn new_child_with_binds(&self, binds: Vec<(Atom, Value)>) -> EvalEnv {
    EvalEnv::new(HashMap::new(), Some(self.clone()), binds)
  }
}

pub fn eval(mut eval_env: EvalEnv, mut value: Value, is_macro: bool) -> MalResult<Value> {
  loop {
    match value.clone() {
      Value::List(list) => if list.is_empty() {
        return Ok(Value::List(list));
      } else {
        value = macro_expand(&eval_env, value)?;
        match value {
          Value::List(list) => {
            {
              let args = &list[1..];
              if let Value::Symbol(ref sym) = list[0] {
                match sym.as_ref() {
                  "def!" => return eval_def(eval_env.clone(), &list[1..], false),
                  "defmacro!" => return eval_def(eval_env.clone(), &list[1..], true),
                  "let*" => {
                    if args.len() != 2 {
                      return error!("let* takes 2 args but was given {}", args.len());
                    }
                    let mut new_env = eval_env.new_child();
                    let bindings = &args[0];
                    match bindings {
                      Value::List(list) | Value::Vector(list) => if list.len() % 2 != 0 {
                        return error!(
                          "bindings list must have an even number of elements but has {}",
                          list.len()
                        );
                      } else {
                        for chunk in list.chunks(2) {
                          let var = match &chunk[0] {
                            v @ Value::Symbol(_) => v,
                            var => return error!("var {} in binding list must be a symbol", var),
                          };
                          let val = eval(new_env.clone(), chunk[1].clone(), is_macro)?;
                          new_env.set(Atom::from(var.clone()), val);
                        }
                      },
                      _ => return error!("bindings must be a list"),
                    }
                    eval_env = new_env;
                    value = args[1].clone();
                    continue;
                  }
                  "do" => {
                    if args.is_empty() {
                      return Ok(Value::Nil);
                    }
                    for expr in &args[..args.len() - 1] {
                      eval(eval_env.clone(), expr.clone(), is_macro)?;
                    }
                    value = args[args.len() - 1].clone();
                    continue;
                  }
                  "if" => {
                    if !(args.len() == 2 || args.len() == 3) {
                      return error!("if takes either 2 or 3 args but was given {}", args.len());
                    }
                    let cond = &args[0];
                    let then_expr = &args[1];
                    let else_expr = if args.len() == 3 {
                      Some(&args[2])
                    } else {
                      None
                    };
                    match eval(eval_env.clone(), cond.clone(), is_macro)? {
                      Value::Nil | Value::False => match else_expr {
                        Some(else_expr) => value = else_expr.clone(),
                        _ => return Ok(Value::Nil),
                      },
                      _ => value = then_expr.clone(),
                    }
                    continue;
                  }
                  "fn*" => return eval_fn(eval_env.clone(), args, is_macro),
                  "quote" => {
                    return if args.len() != 1 {
                      error!("quote takes 1 arg but was given {}", args.len())
                    } else {
                      Ok(args[0].clone())
                    }
                  }
                  "quasiquote" => {
                    value = eval_quasiquote(args[0].clone())?;
                    continue;
                  }
                  "macroexpand" => return macro_expand(&eval_env, args[0].clone()),
                  _ => (),
                }
              }
            }
            let values = eval_ast(&eval_env, Value::List(list), is_macro)?;
            match values {
              Value::List(values) => {
                let func = &values[0];
                let args = &values[1..];
                match func {
                  Value::CoreFunction { func, .. } => return func(eval_env.clone(), args.to_vec()),
                  Value::Function {
                    params, body, env, ..
                  } => {
                    let params = params.clone();
                    let binds: Vec<(Atom, Value)> = match params
                      .iter()
                      .position(|p| p == &Atom::Symbol("&".to_owned()))
                    {
                      Some(pos) if pos != params.len() - 1 => {
                        if pos > args.len() {
                          return error!(
                            "{} takes at least {} arg{} but was given {}",
                            func,
                            pos,
                            if params.len() == 1 { "" } else { "s" },
                            args.len()
                          );
                        }
                        let before_params = params[..pos].to_vec();
                        let variadic_param = &params[pos + 1];
                        let before_args = args[..pos].to_vec();
                        let mut variadic_args = args[pos..].to_vec();
                        let mut binds: Vec<(Atom, Value)> = before_params
                          .into_iter()
                          .zip(before_args.into_iter())
                          .collect();
                        binds.push((variadic_param.clone(), Value::List(variadic_args)));
                        binds
                      }
                      _ => {
                        if args.len() != params.len() {
                          return error!(
                            "{} takes {} arg{} but was given {}",
                            func,
                            params.len(),
                            if params.len() == 1 { "" } else { "s" },
                            args.len()
                          );
                        }
                        params.into_iter().zip(args.to_vec().into_iter()).collect()
                      }
                    };
                    eval_env = env.new_child_with_binds(binds.clone());
                    value = *(body.clone());
                    continue;
                  }
                  _ => return error!("{} is not a function and cannot be applied", func),
                }
              }
              _ => unreachable!(),
            }
          }
          _ => return eval_ast(&eval_env, value, is_macro),
        }
      },
      value => return eval_ast(&eval_env, value, is_macro),
    }
  }
}

fn eval_ast(eval_env: &EvalEnv, value: Value, is_macro: bool) -> MalResult<Value> {
  match value {
    Value::Symbol(sym) => match eval_env.get(&Atom::Symbol(sym.clone())) {
      Some(val) => Ok(val.clone()),
      None => error!("not in env: {}", sym),
    },
    Value::List(list) => Ok(Value::List(
      list
        .into_iter()
        .map(|l| eval(eval_env.clone(), l, is_macro))
        .collect::<MalResult<Vec<Value>>>()?,
    )),
    Value::Vector(list) => Ok(Value::Vector(
      list
        .into_iter()
        .map(|l| eval(eval_env.clone(), l, is_macro))
        .collect::<MalResult<Vec<Value>>>()?,
    )),
    Value::Hashmap(mut hashmap) => {
      for v in hashmap.values_mut() {
        *v = eval(eval_env.clone(), v.clone(), is_macro)?;
      }
      Ok(Value::Hashmap(hashmap))
    }
    _ => Ok(value),
  }
}

fn eval_def(mut eval_env: EvalEnv, args: &[Value], is_macro: bool) -> MalResult<Value> {
  if args.len() != 2 {
    return error!("def! takes 2 args but was given {}", args.len());
  }
  let var = match args[0] {
    Value::Symbol(_) => args[0].clone(),
    ref v => return error!("var {} in def! must be a symbol", v),
  };
  let val = eval(eval_env.clone(), args[1].clone(), is_macro)?;
  eval_env.set(Atom::from(var), val.clone());
  Ok(val)
}

fn eval_fn(eval_env: EvalEnv, args: &[Value], is_macro: bool) -> MalResult<Value> {
  if args.len() != 2 {
    return error!("fn* must have 2 args but was given {}", args.len());
  }
  let params: Vec<Atom> = match args[0] {
    Value::List(ref list) => list
      .iter()
      .map(|l| match l {
        Value::Symbol(_) => Ok(Atom::from(l.clone())),
        _ => error!("param {} is not a symbol", l),
      })
      .collect::<MalResult<Vec<Atom>>>()?,
    Value::Vector(ref vector) => vector
      .iter()
      .map(|v| match v {
        Value::Symbol(_) => Ok(Atom::from(v.clone())),
        _ => error!("param {} is not a symbol", v),
      })
      .collect::<MalResult<Vec<Atom>>>()?,
    _ => {
      return error!(
        "params list must be either a vector or list, got {}",
        args[0]
      );
    }
  };
  Ok(Value::Function {
    params,
    body: Box::new(args[1].clone()),
    env: eval_env,
    is_macro,
  })
}

fn apply_fn(
  func: &Value,
  params: &[Atom],
  body: &Value,
  env: &EvalEnv,
  args: &[Value],
) -> MalResult<Value> {
  let binds: Vec<(Atom, Value)> = match params
    .iter()
    .position(|p| p == &Atom::Symbol("&".to_owned()))
  {
    Some(pos) if pos != params.len() - 1 => {
      if pos > args.len() {
        return error!(
          "{} takes at least {} arg{} but was given {}",
          func,
          pos,
          if params.len() == 1 { "" } else { "s" },
          args.len()
        );
      }
      let before_params = params[..pos].to_vec();
      let variadic_param = &params[pos + 1];
      let before_args = args[..pos].to_vec();
      let mut variadic_args = args[pos..].to_vec();
      let mut binds: Vec<(Atom, Value)> = before_params
        .into_iter()
        .zip(before_args.into_iter())
        .collect();
      binds.push((variadic_param.clone(), Value::List(variadic_args)));
      binds
    }
    _ => {
      if args.len() != params.len() {
        return error!(
          "{} takes {} arg{} but was given {}",
          func,
          params.len(),
          if params.len() == 1 { "" } else { "s" },
          args.len()
        );
      }
      params
        .to_vec()
        .into_iter()
        .zip(args.to_vec().into_iter())
        .collect()
    }
  };
  eval(env.new_child_with_binds(binds.clone()), body.clone(), false)
}

fn eval_quasiquote(ast: Value) -> MalResult<Value> {
  if !ast.is_pair() {
    Ok(Value::List(vec![Value::Symbol("quote".to_owned()), ast]))
  } else {
    match &ast {
      Value::List(list) | Value::Vector(list) => {
        match &list[0] {
          Value::Symbol(ref sym) if sym == "unquote" => {
            if list.len() != 2 {
              return error!("unquote takes 1 arg but was given {}", list.len() - 1);
            } else {
              return Ok(list[1].clone());
            }
          }
          ref head if head.is_pair() => match head {
            Value::List(inner_list) | Value::Vector(inner_list) => match &inner_list[0] {
              Value::Symbol(ref sym) if sym == "splice-unquote" => {
                if inner_list.len() != 2 {
                  return error!(
                    "splice-unquote takes 1 arg but was given {}",
                    inner_list.len() - 1
                  );
                } else {
                  return Ok(Value::List(vec![
                    Value::Symbol("concat".to_owned()),
                    inner_list[1].clone(),
                    eval_quasiquote(Value::List(list[1..].to_vec()))?,
                  ]));
                }
              }
              _ => (),
            },
            _ => (),
          },
          _ => (),
        }
        Ok(Value::List(vec![
          Value::Symbol("cons".to_owned()),
          eval_quasiquote(list[0].clone())?,
          eval_quasiquote(Value::List(list[1..].to_vec()))?,
        ]))
      }
      _ => unreachable!(),
    }
  }
}

fn is_macro_call(env: &EvalEnv, ast: &Value) -> bool {
  match ast {
    Value::List(list) => match list.get(0) {
      Some(sym @ Value::Symbol(_)) => match env.get(&Atom::from(sym.clone())) {
        Some(Value::Function { is_macro, .. }) => is_macro,
        _ => false,
      },
      _ => false,
    },
    _ => false,
  }
}

fn macro_expand(env: &EvalEnv, mut ast: Value) -> MalResult<Value> {
  while is_macro_call(env, &ast) {
    match ast {
      Value::List(list) => match env.get(&Atom::from(list[0].clone())) {
        Some(Value::Function {
          params,
          body,
          env,
          is_macro,
        }) => {
          // this super sucks
          let func = Value::Function {
            params: params.clone(),
            body: body.clone(),
            env: env.clone(),
            is_macro,
          };
          ast = apply_fn(&func, &params, &body, &env, &list[1..])?;
        }
        _ => unreachable!(),
      },

      _ => unreachable!(),
    }
  }
  Ok(ast)
}

impl Default for EvalEnv {
  fn default() -> Self {
    let binds = vec![
      (
        Atom::Symbol("+".to_owned()),
        Value::CoreFunction {
          name: "+",
          func: |_, args| {
            let mut sum = 0;
            for arg in args {
              match arg {
                Value::Int(i) => sum += i,
                _ => return error!("cannot apply + to {}", arg),
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
          func: |_, args| {
            if args.is_empty() {
              return Ok(Value::Int(0));
            }
            let mut diff = match args[0] {
              Value::Int(i) => i,
              _ => return error!("cannot apply - to {}", args[0]),
            };
            for arg in &args[1..] {
              match arg {
                Value::Int(i) => diff -= i,
                _ => return error!("cannot apply - to {}", arg),
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
          func: |_, args| {
            let mut prod = 1;
            for arg in args {
              match arg {
                Value::Int(i) => prod *= i,
                _ => return error!("cannot apply + to {}", arg),
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
          func: |_, args| {
            if args.is_empty() {
              return Ok(Value::Int(1));
            }
            let mut quot = match args[0] {
              Value::Int(i) => i,
              _ => return error!("cannot apply / to {}", args[0]),
            };
            for arg in &args[1..] {
              match arg {
                Value::Int(i) => quot /= i,
                _ => return error!("cannot apply / to {}", arg),
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
          func: |_, args| Ok(Value::List(args)),
        },
      ),
      (
        Atom::Symbol("list?".to_owned()),
        Value::CoreFunction {
          name: "list?",
          func: |_, args| {
            if args.is_empty() {
              error!("list? requires at least 1 arg but was given {}", args.len())
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
          func: |_, args| {
            if args.is_empty() {
              error!("empty requires at least 1 arg but was given {}", args.len())
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
          func: |_, args| {
            if args.is_empty() {
              error!("count requires at least 1 arg but was given {}", args.len())
            } else {
              match args[0] {
                Value::List(ref list) | Value::Vector(ref list) => {
                  Ok(Value::Int(list.len() as isize))
                }
                Value::Nil => Ok(Value::Int(0)),
                _ => error!("cannot apply count to non-list"),
              }
            }
          },
        },
      ),
      (
        Atom::Symbol("=".to_owned()),
        Value::CoreFunction {
          name: "=",
          func: |_, args| {
            if args.len() < 2 {
              return error!("= requires at least 2 args but was given {}", args.len());
            }
            let first = &args[0];
            Ok(Value::from(args[1..].iter().all(|x| first.list_eq(x))))
          },
        },
      ),
      (
        Atom::Symbol(">".to_owned()),
        Value::CoreFunction {
          name: ">",
          func: |_, args| {
            if args.len() != 2 {
              return error!("> requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a > b)),
              _ => error!("cannot apply > to non-ints: {} {}", args[0], args[1]),
            }
          },
        },
      ),
      (
        Atom::Symbol("<".to_owned()),
        Value::CoreFunction {
          name: "<",
          func: |_, args| {
            if args.len() != 2 {
              return error!("< requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a < b)),
              _ => error!("cannot apply < to non-ints: {} {}", args[0], args[1]),
            }
          },
        },
      ),
      (
        Atom::Symbol(">=".to_owned()),
        Value::CoreFunction {
          name: ">=",
          func: |_, args| {
            if args.len() != 2 {
              return error!(">= requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a >= b)),
              _ => error!("cannot apply >= to non-ints: {} {}", args[0], args[1]),
            }
          },
        },
      ),
      (
        Atom::Symbol("<=".to_owned()),
        Value::CoreFunction {
          name: "<=",
          func: |_, args| {
            if args.len() != 2 {
              return error!("<= requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::Int(a), Value::Int(b)) => Ok(Value::from(a <= b)),
              _ => error!("cannot apply <= to non-ints: {} {}", args[0], args[1]),
            }
          },
        },
      ),
      (
        Atom::Symbol("pr-str".to_owned()),
        Value::CoreFunction {
          name: "pr-str",
          func: |_, args| {
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
          func: |_, args| {
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
          func: |_, args| {
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
          func: |_, args| {
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
      (
        Atom::Symbol("read-string".to_owned()),
        Value::CoreFunction {
          name: "read-string",
          func: |_, args| {
            if args.len() != 1 {
              return error!("read-string requires 1 arg but was given {}", args.len());
            }
            match args[0] {
              Value::Str(ref string) => Ok(Value::from(Reader::read_str(string)?)),
              _ => error!("cannot apply read-string to non-string: {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("slurp".to_owned()),
        Value::CoreFunction {
          name: "slurp",
          func: |_, args| {
            if args.len() != 1 {
              return error!("slurp requires 1 arg but was given {}", args.len());
            }
            match args[0] {
              Value::Str(ref filename) => {
                let mut f = File::open(filename)?;
                let mut contents = String::new();
                f.read_to_string(&mut contents)?;
                Ok(Value::Str(contents))
              }
              _ => error!("cannot apply slurp to non-string: {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("eval".to_owned()),
        Value::CoreFunction {
          name: "eval",
          func: |eval_env, args| {
            if args.len() != 1 {
              return error!("eval requires 1 arg but was given {}", args.len());
            }
            eval(eval_env.root(), args[0].clone(), false)
          },
        },
      ),
      (
        Atom::Symbol("atom".to_owned()),
        Value::CoreFunction {
          name: "atom",
          func: |_, args| {
            if args.len() != 1 {
              return error!("atom requires 1 arg but was given {}", args.len());
            }
            Ok(Value::Atom(Rc::new(RefCell::new(args[0].clone()))))
          },
        },
      ),
      (
        Atom::Symbol("atom?".to_owned()),
        Value::CoreFunction {
          name: "atom?",
          func: |_, args| {
            if args.len() != 1 {
              return error!("atom? requires 1 arg but was given {}", args.len());
            }
            Ok(match args[0] {
              Value::Atom(_) => Value::True,
              _ => Value::False,
            })
          },
        },
      ),
      (
        Atom::Symbol("deref".to_owned()),
        Value::CoreFunction {
          name: "deref",
          func: |_, args| {
            if args.len() != 1 {
              return error!("deref requires 1 arg but was given {}", args.len());
            }
            match args[0] {
              Value::Atom(ref value) => Ok(value.borrow().clone()),
              _ => error!("cannot deref non-atom: {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("reset!".to_owned()),
        Value::CoreFunction {
          name: "reset!",
          func: |_, args| {
            if args.len() != 2 {
              return error!("reset! requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::Atom(ref value), _) => {
                value.replace(args[1].clone());
                Ok(args[1].clone())
              }
              _ => error!("cannot reset! non-atom: {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("swap!".to_owned()),
        Value::CoreFunction {
          name: "swap!",
          func: |eval_env, args| {
            if args.len() < 2 {
              return error!("swap requires at least 2 args but was given {}", args.len());
            }
            let atom = &args[0];
            let func = &args[1];
            let args = &args[2..];
            match (atom, func) {
              (
                Value::Atom(ref value),
                Value::Function {
                  params, body, env, ..
                },
              ) => {
                let mut func_args = vec![value.borrow().clone()];
                func_args.extend_from_slice(args);
                let new_value = apply_fn(func, params, body, &env, &func_args)?;
                *(value.borrow_mut()) = new_value.clone();
                Ok(new_value)
              }
              (Value::Atom(ref value), Value::CoreFunction { func, .. }) => {
                let mut func_args = vec![value.borrow().clone()];
                func_args.extend_from_slice(args);
                let new_value = func(eval_env, func_args)?;
                *(value.borrow_mut()) = new_value.clone();
                Ok(new_value)
              }
              (Value::Atom(_), _) => error!("{} is not a function", args[1]),
              (_, _) => error!("cannot swap! non-atom: {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("cons".to_owned()),
        Value::CoreFunction {
          name: "cons",
          func: |_, args| {
            if args.len() != 2 {
              return error!("cons requires 2 args but was given {}", args.len());
            }
            let head = &args[0];
            match args[1] {
              Value::List(ref list) | Value::Vector(ref list) => {
                let mut new_list = list.clone();
                new_list.insert(0, head.clone());
                Ok(Value::List(new_list))
              }
              _ => error!("cannot apply cons to non-list: {}", args[1]),
            }
          },
        },
      ),
      (
        Atom::Symbol("concat".to_owned()),
        Value::CoreFunction {
          name: "concat",
          func: |_, args| {
            let mut new_list = vec![];
            for list in args {
              match list {
                Value::List(ref l) | Value::Vector(ref l) => new_list.extend_from_slice(l),
                _ => return error!("cannot apply concat to non-list: {}", list),
              }
            }
            Ok(Value::List(new_list))
          },
        },
      ),
      (
        Atom::Symbol("nth".to_owned()),
        Value::CoreFunction {
          name: "nth",
          func: |_, args| {
            if args.len() != 2 {
              return error!("nth requires 2 args but was given {}", args.len());
            }
            match (&args[0], &args[1]) {
              (Value::List(ref list), Value::Int(i)) | (Value::Vector(ref list), Value::Int(i)) => {
                if *i < 0 || *i as usize >= list.len() {
                  error!("index {} is ouside the bounds of {}", i, args[0])
                } else {
                  Ok(list[*i as usize].clone())
                }
              }
              _ => error!(
                "nth requires a list and an int but was given {} and {}",
                args[0], args[1]
              ),
            }
          },
        },
      ),
      (
        Atom::Symbol("first".to_owned()),
        Value::CoreFunction {
          name: "first",
          func: |_, args| {
            if args.len() != 1 {
              return error!("first requires 1 arg but was given {}", args.len());
            }
            match args[0] {
              Value::List(ref list) | Value::Vector(ref list) => {
                if list.is_empty() {
                  Ok(Value::Nil)
                } else {
                  Ok(list[0].clone())
                }
              }
              Value::Nil => Ok(Value::Nil),
              _ => error!("first requires a list or nil but was given {}", args[0]),
            }
          },
        },
      ),
      (
        Atom::Symbol("rest".to_owned()),
        Value::CoreFunction {
          name: "rest",
          func: |_, args| {
            if args.len() != 1 {
              return error!("rest requires 1 arg but was given {}", args.len());
            }
            match args[0] {
              Value::List(ref list) | Value::Vector(ref list) => {
                if list.is_empty() {
                  Ok(Value::List(vec![]))
                } else {
                  Ok(Value::List(list[1..].to_vec()))
                }
              }
              Value::Nil => Ok(Value::List(vec![])),
              _ => error!("rest requires a list or nil but was given {}", args[0]),
            }
          },
        },
      ),
      (Atom::Symbol("*ARGV*".to_owned()), Value::List(vec![])),
    ];
    EvalEnv::new(HashMap::new(), None, binds)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_eval1() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("+".to_owned()),
          Value::Int(1),
          Value::Int(2),
        ]),
        false
      ),
      Ok(Value::Int(3))
    );
  }

  #[test]
  fn test_eval2() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("+".to_owned()),
          Value::Int(1),
          Value::List(vec![
            Value::Symbol("*".to_owned()),
            Value::Int(2),
            Value::Int(3),
          ]),
        ]),
        false
      ),
      Ok(Value::Int(7))
    );
  }

  #[test]
  fn test_def() {
    let eval_env = EvalEnv::default();
    assert!(
      eval(
        eval_env.clone(),
        Value::List(vec![
          Value::Symbol("def!".to_owned()),
          Value::Symbol("a".to_owned()),
          Value::Int(6),
        ]),
        false,
      ).is_ok()
    );
    assert_eq!(
      eval(eval_env.clone(), Value::Symbol("a".to_owned()), false),
      Ok(Value::Int(6))
    );
    assert!(
      eval(
        eval_env.clone(),
        Value::List(vec![
          Value::Symbol("def!".to_owned()),
          Value::Symbol("b".to_owned()),
          Value::List(vec![
            Value::Symbol("+".to_owned()),
            Value::Symbol("a".to_owned()),
            Value::Int(2),
          ]),
        ]),
        false,
      ).is_ok()
    );
    assert_eq!(
      eval(
        eval_env.clone(),
        Value::List(vec![
          Value::Symbol("+".to_owned()),
          Value::Symbol("a".to_owned()),
          Value::Symbol("b".to_owned()),
        ]),
        false
      ),
      Ok(Value::Int(14))
    );
  }

  #[test]
  fn test_let() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("let*".to_owned()),
          Value::List(vec![
            Value::Symbol("c".to_owned()),
            Value::Int(2),
            Value::Symbol("d".to_owned()),
            Value::List(vec![
              Value::Symbol("+".to_owned()),
              Value::Symbol("c".to_owned()),
              Value::Int(2),
            ]),
          ]),
          Value::List(vec![
            Value::Symbol("+".to_owned()),
            Value::Symbol("c".to_owned()),
            Value::Symbol("d".to_owned()),
          ]),
        ]),
        false
      ),
      Ok(Value::Int(6))
    );
  }

  #[test]
  fn test_def_fail() {
    let eval_env = EvalEnv::default();
    assert!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("def!".to_owned()),
          Value::Keyword("a".to_owned()),
          Value::Int(6),
        ]),
        false,
      ).is_err()
    );
  }

  #[test]
  fn test_let_fail() {
    let eval_env = EvalEnv::default();
    assert!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("let*".to_owned()),
          Value::List(vec![Value::Keyword("c".to_owned()), Value::Int(2)]),
          Value::List(vec![
            Value::Symbol("+".to_owned()),
            Value::Symbol("c".to_owned()),
            Value::Int(1),
          ]),
        ]),
        false,
      ).is_err()
    );
  }

  #[test]
  fn test_do1() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![Value::Symbol("do".to_owned())]),
        false
      ),
      Ok(Value::Nil)
    );
  }

  #[test]
  fn test_do2() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![Value::Symbol("do".to_owned()), Value::Int(1)]),
        false
      ),
      Ok(Value::Int(1))
    );
  }

  #[test]
  fn test_if1() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::Symbol("if".to_owned()),
          Value::List(vec![
            Value::Symbol("if".to_owned()),
            Value::Nil,
            Value::False,
            Value::True,
          ]),
          Value::Int(1),
          Value::Int(2),
        ]),
        false
      ),
      Ok(Value::Int(1))
    );
  }

  #[test]
  fn test_fn1() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::List(vec![
            Value::Symbol("fn*".to_owned()),
            Value::List(vec![Value::Symbol("a".to_owned())]),
            Value::Symbol("a".to_owned()),
          ]),
          Value::Int(7),
        ]),
        false
      ),
      Ok(Value::Int(7))
    );
  }

  #[test]
  fn test_fn2() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
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
        ]),
        false
      ),
      Ok(Value::Int(5))
    );
  }

  #[test]
  fn test_fn_fail() {
    let eval_env = EvalEnv::default();
    assert!(
      eval(
        eval_env,
        Value::List(vec![
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
        ]),
        false,
      ).is_err()
    );
  }

  #[test]
  fn test_nested_fn() {
    let eval_env = EvalEnv::default();
    assert_eq!(
      eval(
        eval_env,
        Value::List(vec![
          Value::List(vec![
            Value::List(vec![
              Value::Symbol("fn*".to_owned()),
              Value::List(vec![Value::Symbol("a".to_owned())]),
              Value::List(vec![
                Value::Symbol("fn*".to_owned()),
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
        ]),
        false
      ),
      Ok(Value::Int(12))
    )
  }
}
