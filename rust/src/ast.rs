use std::collections::HashMap;
use std::fmt;
use MalResult;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Ast {
    List(Vec<Ast>),
    Vector(Vec<Ast>),
    Hashmap(Vec<(Ast, Ast)>),
    Str(String),
    Keyword(String),
    Symbol(String),
    Int(isize),
    True,
    False,
    Nil,
    Quote(Box<Ast>),
    Quasiquote(Box<Ast>),
    Unquote(Box<Ast>),
    SpliceUnquote(Box<Ast>),
    Deref(Box<Ast>),              // atom
    WithMeta(Box<Ast>, Box<Ast>), // val, meta
}

impl Ast {
    pub fn string(&self, readable: bool) -> String {
        match self {
            Ast::List(list) => {
                let list_str = list
                    .into_iter()
                    .map(|l| l.string(readable))
                    .collect::<Vec<String>>()
                    .join(" ");
                format!("({})", list_str)
            }
            Ast::Vector(vec) => {
                let vec_str = vec
                    .into_iter()
                    .map(|l| l.string(readable))
                    .collect::<Vec<String>>()
                    .join(" ");
                format!("[{}]", vec_str)
            }
            Ast::Hashmap(hm) => {
                let kv_pair_str: String = hm
                    .into_iter()
                    .map(|(k, v)| vec![k.string(readable), v.string(readable)])
                    .collect::<Vec<Vec<String>>>()
                    .concat()
                    .join(" ");
                format!("{{{}}}", kv_pair_str)
            }
            Ast::Str(s) => if readable {
                format!("\"{}\"", s)
            } else {
                format!("\"{}\"", s.replace("\"", r#"\""#))
            },
            Ast::Keyword(kw) => format!(":{}", kw),
            Ast::Symbol(sym) => sym.clone(),
            Ast::Int(i) => i.to_string(),
            Ast::True => String::from("true"),
            Ast::False => String::from("false"),
            Ast::Nil => String::from("nil"),
            Ast::Quote(ast) => format!("(quote {})", ast),
            Ast::Quasiquote(ast) => format!("(quasiquote {})", ast),
            Ast::Unquote(ast) => format!("(unquote {})", ast),
            Ast::SpliceUnquote(ast) => format!("(splice-unquote {})", ast),
            Ast::Deref(atom) => format!("(deref {})", atom),
            Ast::WithMeta(val, meta) => format!("(with-meta {} {})", val, meta),
        }
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string(true))
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Atom {
    Str(String),
    Keyword(String),
    Symbol(String),
    Int(isize),
    True,
    False,
    Nil,
}

impl From<Ast> for Atom {
    fn from(ast: Ast) -> Self {
        match ast {
            Ast::Str(s) => Atom::Str(s),
            Ast::Keyword(kw) => Atom::Keyword(kw),
            Ast::Symbol(sym) => Atom::Symbol(sym),
            Ast::Int(i) => Atom::Int(i),
            Ast::True => Atom::True,
            Ast::False => Atom::False,
            Ast::Nil => Atom::Nil,
            _ => unreachable!("cannot convert to atom: {:?}", ast),
        }
    }
}

impl From<Value> for Atom {
    fn from(value: Value) -> Self {
        match value {
            Value::Str(s) => Atom::Str(s),
            Value::Keyword(kw) => Atom::Keyword(kw),
            Value::Symbol(sym) => Atom::Symbol(sym),
            Value::Int(i) => Atom::Int(i),
            Value::True => Atom::True,
            Value::False => Atom::False,
            Value::Nil => Atom::Nil,
            value => unreachable!("cannot convert to atom: {:?}", value),
        }
    }
}

impl Atom {
    pub fn string(&self, readable: bool) -> String {
        match self {
            Atom::Str(s) => if readable {
                format!("\"{}\"", s)
            } else {
                format!("\"{}\"", s.replace("\"", r#"\""#))
            },
            Atom::Keyword(kw) => format!(":{}", kw),
            Atom::Symbol(sym) => sym.clone(),
            Atom::Int(i) => i.to_string(),
            Atom::True => String::from("true"),
            Atom::False => String::from("false"),
            Atom::Nil => String::from("nil"),
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string(true))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    List(Vec<Value>),
    Vector(Vec<Value>),
    Hashmap(HashMap<Atom, Value>),
    Str(String),
    Keyword(String),
    Symbol(String),
    Int(isize),
    True,
    False,
    Nil,
    Function {
        arity: usize,
        func: fn(Vec<Value>) -> MalResult<Value>,
    },
}

impl Value {
    pub fn string(&self, readable: bool) -> String {
        match self {
            Value::List(list) => {
                let list_str = list
                    .into_iter()
                    .map(|l| l.string(readable))
                    .collect::<Vec<String>>()
                    .join(" ");
                format!("({})", list_str)
            }
            Value::Vector(vec) => {
                let vec_str = vec
                    .into_iter()
                    .map(|l| l.string(readable))
                    .collect::<Vec<String>>()
                    .join(" ");
                format!("[{}]", vec_str)
            }
            Value::Hashmap(hm) => {
                let kv_pair_str: String = hm
                    .into_iter()
                    .map(|(k, v)| vec![k.string(readable), v.string(readable)])
                    .collect::<Vec<Vec<String>>>()
                    .concat()
                    .join(" ");
                format!("{{{}}}", kv_pair_str)
            }
            Value::Str(s) => if readable {
                format!("\"{}\"", s)
            } else {
                format!("\"{}\"", s.replace("\"", r#"\""#))
            },
            Value::Keyword(kw) => format!(":{}", kw),
            Value::Symbol(sym) => sym.clone(),
            Value::Int(i) => i.to_string(),
            Value::True => String::from("true"),
            Value::False => String::from("false"),
            Value::Nil => String::from("nil"),
            Value::Function { arity, func } => format!("func/{} {:?}", arity, func),
        }
    }
}

impl From<Ast> for Value {
    fn from(ast: Ast) -> Self {
        match ast {
            Ast::List(list) => {
                Value::List(list.into_iter().map(Value::from).collect::<Vec<Value>>())
            }
            Ast::Vector(vec) => {
                Value::Vector(vec.into_iter().map(Value::from).collect::<Vec<Value>>())
            }
            Ast::Hashmap(hm) => {
                let mut hashmap = HashMap::new();
                for (k, v) in hm {
                    hashmap.insert(Atom::from(k), Value::from(v));
                }
                Value::Hashmap(hashmap)
            }
            Ast::Str(s) => Value::Str(s),
            Ast::Keyword(kw) => Value::Keyword(kw),
            Ast::Symbol(sym) => Value::Symbol(sym),
            Ast::Int(i) => Value::Int(i),
            Ast::True => Value::True,
            Ast::False => Value::False,
            Ast::Nil => Value::Nil,
            _ => unimplemented!(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string(true))
    }
}
