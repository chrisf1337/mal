use ast::{Atom, Value};
use std::collections::HashMap;
use MalResult;

#[derive(Debug)]
pub struct EvalEnv<'a> {
    env: HashMap<Atom, Value>,
    outer: Option<&'a EvalEnv<'a>>,
}

impl<'a> EvalEnv<'a> {
    fn new(env: HashMap<Atom, Value>, outer: Option<&'a EvalEnv<'a>>) -> Self {
        EvalEnv { env, outer }
    }

    fn set(&mut self, symbol: Value, value: Value) {
        self.env.insert(Atom::from(symbol), value);
    }

    fn find<'b>(&'b self, symbol: &Value) -> Option<&'b EvalEnv> {
        if self.env.contains_key(&Atom::from(symbol.clone())) {
            Some(self)
        } else {
            match self.outer {
                Some(ref outer) => outer.find(symbol),
                None => None,
            }
        }
    }

    fn get(&self, symbol: &Value) -> Option<Value> {
        match self.find(symbol) {
            Some(env) => Some(env.env[&Atom::from(symbol.clone())].clone()),
            None => None,
        }
    }

    pub fn eval(&mut self, value: Value) -> MalResult<Value> {
        match value {
            Value::List(list) => if list.is_empty() {
                Ok(Value::List(list))
            } else {
                match list[0] {
                    Value::Symbol(ref sym) if sym == "def!" => return self.eval_def(&list[1..]),
                    Value::Symbol(ref sym) if sym == "let*" => return self.eval_let(&list[1..]),
                    _ => (),
                }
                let values = self.eval_ast(Value::List(list))?;
                match values {
                    Value::List(values) => {
                        let func = &values[0];
                        let args = &values[1..];
                        match func {
                            Value::Function { arity, func } => self.apply_func(*arity, *func, args),
                            _ => Err(format!("{} is not a function and cannot be applied", func)),
                        }
                    }
                    _ => unreachable!(),
                }
            },
            value => self.eval_ast(value),
        }
    }

    fn eval_ast(&mut self, value: Value) -> MalResult<Value> {
        match value {
            Value::Symbol(sym) => match self.env.get(&Atom::Symbol(sym.clone())) {
                Some(val) => Ok(val.clone()),
                None => Err(format!("not in env: {}", sym)),
            },
            Value::List(list) => Ok(Value::List(
                list.into_iter()
                    .map(|l| self.eval(l))
                    .collect::<MalResult<Vec<Value>>>()?,
            )),
            Value::Vector(list) => Ok(Value::Vector(
                list.into_iter()
                    .map(|l| self.eval(l))
                    .collect::<MalResult<Vec<Value>>>()?,
            )),
            Value::Hashmap(mut hashmap) => {
                for v in hashmap.values_mut() {
                    *v = self.eval(v.clone())?;
                }
                Ok(Value::Hashmap(hashmap))
            }
            _ => Ok(value),
        }
    }

    fn new_child(&self) -> EvalEnv {
        EvalEnv {
            env: self.env.clone(),
            outer: Some(self),
        }
    }

    fn eval_let(&mut self, args: &[Value]) -> MalResult<Value> {
        if args.len() != 2 {
            return Err(format!("let* takes 2 args but was given {}", args.len()));
        }
        let mut new_env = self.new_child();
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
                    let val = new_env.eval(chunk[1].clone())?;
                    new_env.set(var.clone(), val);
                }
            },
            _ => return Err(format!("bindings must be a list")),
        }
        let expr = &args[1];
        new_env.eval(expr.clone())
    }

    fn eval_def(&mut self, args: &[Value]) -> MalResult<Value> {
        if args.len() != 2 {
            return Err(format!("def! takes 2 args but was given {}", args.len()));
        }
        let val = self.eval(args[1].clone())?;
        self.set(args[0].clone(), val.clone());
        Ok(val)
    }

    fn apply_func(
        &self,
        arity: usize,
        func: fn(Vec<Value>) -> MalResult<Value>,
        args: &[Value],
    ) -> MalResult<Value> {
        if args.len() != arity {
            return Err(format!(
                "function {:?} has arity {} was applied on {} args",
                func,
                arity,
                args.len()
            ));
        }
        func(args.to_vec())
    }
}

impl<'a> Default for EvalEnv<'a> {
    fn default() -> Self {
        let mut env = EvalEnv {
            env: HashMap::new(),
            outer: None,
        };
        env.set(
            Value::Symbol(String::from("+")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Value::Symbol(String::from("-")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Value::Symbol(String::from("*")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Value::Symbol(String::from("/")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env
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
                Value::Symbol(String::from("+")),
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
                Value::Symbol(String::from("+")),
                Value::Int(1),
                Value::List(vec![
                    Value::Symbol(String::from("*")),
                    Value::Int(2),
                    Value::Int(3),
                ]),
            ])),
            Ok(Value::Int(7))
        );
    }

    #[test]
    fn test_def_let() {
        let mut eval_env = EvalEnv::default();
        assert!(
            eval_env
                .eval(Value::List(vec![
                    Value::Symbol(String::from("def!")),
                    Value::Symbol(String::from("a")),
                    Value::Int(6),
                ]))
                .is_ok()
        );
        assert_eq!(
            eval_env.eval(Value::Symbol(String::from("a"))),
            Ok(Value::Int(6))
        );
        assert!(
            eval_env
                .eval(Value::List(vec![
                    Value::Symbol(String::from("def!")),
                    Value::Symbol(String::from("b")),
                    Value::List(vec![
                        Value::Symbol(String::from("+")),
                        Value::Symbol(String::from("a")),
                        Value::Int(2),
                    ]),
                ]))
                .is_ok()
        );
        assert_eq!(
            eval_env.eval(Value::List(vec![
                Value::Symbol(String::from("+")),
                Value::Symbol(String::from("a")),
                Value::Symbol(String::from("b")),
            ])),
            Ok(Value::Int(14))
        );
    }

    #[test]
    fn test_let() {
        let mut eval_env = EvalEnv::default();
        assert_eq!(
            eval_env.eval(Value::List(vec![
                Value::Symbol(String::from("let*")),
                Value::List(vec![
                    Value::Symbol(String::from("c")),
                    Value::Int(2),
                    Value::Symbol(String::from("d")),
                    Value::List(vec![
                        Value::Symbol(String::from("+")),
                        Value::Symbol(String::from("c")),
                        Value::Int(2),
                    ]),
                ]),
                Value::List(vec![
                    Value::Symbol(String::from("+")),
                    Value::Symbol(String::from("c")),
                    Value::Symbol(String::from("d")),
                ]),
            ])),
            Ok(Value::Int(6))
        );
    }
}
