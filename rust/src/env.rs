use ast::{Atom, Value};
use std::collections::HashMap;
use MalResult;

trait BoxClone: Fn(Vec<Value>) -> MalResult<Value> + Clone {}
impl<T> BoxClone for T where T: Fn(Vec<Value>) -> MalResult<Value> + Clone {}

#[derive(Debug)]
pub struct EvalEnv<'a> {
    env: HashMap<Atom, Value>,
    outer: Option<&'a EvalEnv<'a>>,
}

impl<'a> EvalEnv<'a> {
    fn new(
        env: HashMap<Atom, Value>,
        outer: Option<&'a EvalEnv<'a>>,
        binds: Vec<(Atom, Value)>,
    ) -> Self {
        let mut eval_env = EvalEnv { env, outer };
        for (var, expr) in binds {
            eval_env.set(var, expr);
        }
        eval_env
    }

    fn set(&mut self, var: Atom, value: Value) {
        self.env.insert(var, value);
    }

    fn find<'b>(&'b self, symbol: &Atom) -> Option<&'b EvalEnv> {
        if self.env.contains_key(symbol) {
            Some(self)
        } else {
            match self.outer {
                Some(ref outer) => outer.find(symbol),
                None => None,
            }
        }
    }

    fn get(&self, symbol: &Atom) -> Option<Value> {
        match self.find(symbol) {
            Some(env) => Some(env.env[symbol].clone()),
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
                    Value::Symbol(ref sym) if sym == "do" => return self.eval_do(&list[1..]),
                    Value::Symbol(ref sym) if sym == "if" => return self.eval_if(&list[1..]),
                    Value::Symbol(ref sym) if sym == "fn*" => return self.eval_fn(&list[1..]),
                    _ => (),
                }
                let values = self.eval_ast(Value::List(list))?;
                match values {
                    Value::List(values) => {
                        let func = &values[0];
                        let args = &values[1..];
                        match func {
                            Value::CoreFunction { arity, func, .. } => {
                                self.apply_core_func(*arity, *func, args)
                            }
                            Value::Function { params, body } => {
                                self.apply_func(params.clone(), body.clone(), args)
                            }
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
        EvalEnv::new(self.env.clone(), Some(self), vec![])
    }

    fn new_child_with_binds(&self, binds: Vec<(Atom, Value)>) -> EvalEnv {
        EvalEnv::new(self.env.clone(), Some(self), binds)
    }

    fn eval_do(&mut self, args: &[Value]) -> MalResult<Value> {
        if args.len() == 0 {
            return Ok(Value::Nil);
        }
        for expr in &args[..args.len() - 1] {
            self.eval_ast(expr.clone())?;
        }
        self.eval_ast(args[args.len() - 1].clone())
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
                    new_env.set(Atom::from(var.clone()), val);
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
        let var = match args[0] {
            Value::Symbol(_) => args[0].clone(),
            ref v => return Err(format!("var {} in def! must be a symbol", v)),
        };
        let val = self.eval(args[1].clone())?;
        self.set(Atom::from(var), val.clone());
        Ok(val)
    }

    fn eval_if(&mut self, args: &[Value]) -> MalResult<Value> {
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
        match self.eval(cond.clone())? {
            Value::Nil | Value::False => match else_expr {
                Some(else_expr) => self.eval(else_expr.clone()),
                _ => Ok(Value::Nil),
            },
            _ => self.eval(then_expr.clone()),
        }
    }

    fn eval_fn(&mut self, args: &[Value]) -> MalResult<Value> {
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
        })
    }

    fn apply_func(
        &mut self,
        params: Vec<Atom>,
        body: Box<Value>,
        args: &[Value],
    ) -> MalResult<Value> {
        if args.len() != params.len() {
            return Err(format!(
                "function has arity {}, but {} arguments were provided",
                params.len(),
                args.len()
            ));
        }
        let binds = params.into_iter().zip(args.to_vec().into_iter()).collect();
        let mut eval_env = self.new_child_with_binds(binds);
        eval_env.eval(*body)
    }

    fn apply_core_func(
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
        let mut env = EvalEnv::new(HashMap::new(), None, vec![]);
        env.set(
            Atom::Symbol(String::from("+")),
            Value::CoreFunction {
                name: "+",
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Atom::Symbol(String::from("-")),
            Value::CoreFunction {
                name: "-",
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Atom::Symbol(String::from("*")),
            Value::CoreFunction {
                name: "*",
                arity: 2,
                func: |args| match (args[0].clone(), args[1].clone()) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (a, b) => Err(format!("cannot add {} and {}", a, b)),
                },
            },
        );
        env.set(
            Atom::Symbol(String::from("/")),
            Value::CoreFunction {
                name: "/",
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
    fn test_def() {
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

    #[test]
    fn test_def_fail() {
        let mut eval_env = EvalEnv::default();
        assert!(
            eval_env
                .eval(Value::List(vec![
                    Value::Symbol(String::from("def!")),
                    Value::Keyword(String::from("a")),
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
                    Value::Symbol(String::from("let*")),
                    Value::List(vec![Value::Keyword(String::from("c")), Value::Int(2)]),
                    Value::List(vec![
                        Value::Symbol(String::from("+")),
                        Value::Symbol(String::from("c")),
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
            eval_env.eval(Value::List(vec![Value::Symbol(String::from("do"))])),
            Ok(Value::Nil)
        );
    }

    #[test]
    fn test_do2() {
        let mut eval_env = EvalEnv::default();
        assert_eq!(
            eval_env.eval(Value::List(vec![
                Value::Symbol(String::from("do")),
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
                Value::Symbol(String::from("if")),
                Value::List(vec![
                    Value::Symbol(String::from("if")),
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
                    Value::Symbol(String::from("fn*")),
                    Value::List(vec![Value::Symbol(String::from("a"))]),
                    Value::Symbol(String::from("a")),
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
                    Value::Symbol(String::from("fn*")),
                    Value::List(vec![
                        Value::Symbol(String::from("a")),
                        Value::Symbol(String::from("b")),
                    ]),
                    Value::List(vec![
                        Value::Symbol(String::from("+")),
                        Value::Symbol(String::from("a")),
                        Value::Symbol(String::from("b")),
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
                        Value::Symbol(String::from("fn*")),
                        Value::List(vec![
                            Value::Symbol(String::from("a")),
                            Value::Symbol(String::from("b")),
                        ]),
                        Value::List(vec![
                            Value::Symbol(String::from("+")),
                            Value::Symbol(String::from("a")),
                            Value::Symbol(String::from("b")),
                        ]),
                    ]),
                    Value::Int(2),
                ]))
                .is_err()
        );
    }
}
