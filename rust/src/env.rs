use ast::{Atom, Value};
use std::collections::HashMap;
use MalResult;

pub struct EvalEnv {
    env: HashMap<Atom, Value>,
}

impl EvalEnv {
    pub fn new(env: HashMap<Atom, Value>) -> Self {
        EvalEnv { env }
    }

    pub fn eval(&self, value: Value) -> MalResult<Value> {
        match value {
            Value::List(list) => if list.is_empty() {
                Ok(Value::List(list))
            } else {
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

    pub fn apply_func(
        &self,
        arity: usize,
        func: fn(Vec<Value>) -> Result<Value, String>,
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

    pub fn eval_ast(&self, value: Value) -> MalResult<Value> {
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
            _ => Ok(value),
        }
    }
}

impl Default for EvalEnv {
    fn default() -> Self {
        let mut env = HashMap::new();
        env.insert(
            Atom::Symbol(String::from("+")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.insert(
            Atom::Symbol(String::from("-")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.insert(
            Atom::Symbol(String::from("*")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.insert(
            Atom::Symbol(String::from("/")),
            Value::Function {
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        EvalEnv { env }
    }
}
