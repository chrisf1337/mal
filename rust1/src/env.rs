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

    fn borrow_mut(&self) -> RefMut<Env> {
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
                            Value::CoreFunction { func, .. } => self.apply_core_func(*func, args),
                            Value::Function { params, body, env } => {
                                self.apply_func(params.clone(), body.clone(), args, env.clone())
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
            Value::Symbol(sym) => match self.get(&Atom::Symbol(sym.clone())) {
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
        EvalEnv::new(HashMap::new(), Some(self.clone()), vec![])
    }

    fn new_child_with_binds(&self, binds: Vec<(Atom, Value)>) -> EvalEnv {
        EvalEnv::new(HashMap::new(), Some(self.clone()), binds)
    }

    fn eval_do(&mut self, args: &[Value]) -> MalResult<Value> {
        if args.is_empty() {
            return Ok(Value::Nil);
        }
        for expr in &args[..args.len() - 1] {
            self.eval(expr.clone())?;
        }
        self.eval(args[args.len() - 1].clone())
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
            _ => return Err(String::from("bindings must be a list")),
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
            env: self.new_child(),
        })
    }

    fn apply_func(
        &mut self,
        params: Vec<Atom>,
        body: Box<Value>,
        args: &[Value],
        mut env: EvalEnv,
    ) -> MalResult<Value> {
        // TODO: varargs
        let binds: Vec<(Atom, Value)> = params.into_iter().zip(args.to_vec().into_iter()).collect();
        for (var, expr) in binds {
            env.set(var, expr);
        }
        env.eval(*body)
    }

    fn apply_core_func(
        &self,
        func: fn(Vec<Value>) -> MalResult<Value>,
        args: &[Value],
    ) -> MalResult<Value> {
        func(args.to_vec())
    }
}

impl Default for EvalEnv {
    fn default() -> Self {
        let binds = vec![
            (
                Atom::Symbol(String::from("+")),
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
                Atom::Symbol(String::from("-")),
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
                Atom::Symbol(String::from("*")),
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
                Atom::Symbol(String::from("/")),
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
                Atom::Symbol(String::from("list")),
                Value::CoreFunction {
                    name: "list",
                    func: |args| Ok(Value::List(args)),
                },
            ),
            (
                Atom::Symbol(String::from("list?")),
                Value::CoreFunction {
                    name: "list?",
                    func: |args| {
                        if args.is_empty() {
                            Err(String::from("cannot call list? with 0 args"))
                        } else {
                            match args[0] {
                                Value::List(_) => Ok(Value::True),
                                _ => Ok(Value::False),
                            }
                        }
                    },
                },
            ),
            (
                Atom::Symbol(String::from("empty?")),
                Value::CoreFunction {
                    name: "empty?",
                    func: |args| {
                        if args.is_empty() {
                            Err(String::from("cannot call empty? with 0 args"))
                        } else {
                            match args[0] {
                                Value::List(ref list) => if list.is_empty() {
                                    Ok(Value::True)
                                } else {
                                    Ok(Value::False)
                                },
                                Value::Vector(ref vector) => if vector.is_empty() {
                                    Ok(Value::True)
                                } else {
                                    Ok(Value::False)
                                },
                                _ => Ok(Value::False),
                            }
                        }
                    },
                },
            ),
            (
                Atom::Symbol(String::from("prn")),
                Value::CoreFunction {
                    name: "prn",
                    func: |args| {
                        println!(
                            "{}",
                            args.into_iter()
                                .map(|a| a.string(true))
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

    #[test]
    fn test_nested_fn() {
        let mut eval_env = EvalEnv::default();
        assert_eq!(
            eval_env.eval(Value::List(vec![
                Value::List(vec![
                    Value::List(vec![
                        Value::Symbol(String::from("fn*")),
                        Value::List(vec![Value::Symbol(String::from("a"))]),
                        Value::List(vec![
                            Value::Symbol(String::from("fn*")),
                            Value::List(vec![Value::Symbol(String::from("b"))]),
                            Value::List(vec![
                                Value::Symbol(String::from("+")),
                                Value::Symbol(String::from("a")),
                                Value::Symbol(String::from("b")),
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
