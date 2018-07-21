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

fn escape(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let mut out = vec![];
    for c in chars {
        match c {
            '\n' => out.extend_from_slice(&['\\', 'n']),
            '"' => out.extend_from_slice(&['\\', '"']),
            '\\' => out.extend_from_slice(&['\\', '\\']),
            c => out.push(c),
        }
    }
    out.into_iter().collect()
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
                format!("\"{}\"", escape(s))
            } else {
                s.clone()
            },
            Ast::Keyword(kw) => format!(":{}", kw),
            Ast::Symbol(sym) => sym.clone(),
            Ast::Int(i) => i.to_string(),
            Ast::True => "true".to_owned(),
            Ast::False => "false".to_owned(),
            Ast::Nil => "nil".to_owned(),
            Ast::Quote(ast) => format!("(quote {})", ast),
            Ast::Quasiquote(ast) => format!("(quasiquote {})", ast),
            Ast::Unquote(ast) => format!("(unquote {})", ast),
            Ast::SpliceUnquote(ast) => format!("(splice-unquote {})", ast),
            Ast::Deref(atom) => format!("(deref {})", atom),
            Ast::WithMeta(val, meta) => format!("(with-meta {} {})", val, meta),
        }
    }

    pub fn is_atom(&self) -> bool {
        match self {
            Ast::Str(_)
            | Ast::Keyword(_)
            | Ast::Symbol(_)
            | Ast::Int(_)
            | Ast::True
            | Ast::False
            | Ast::Nil => true,
            _ => false,
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
                format!("\"{}\"", escape(s))
            } else {
                s.clone()
            },
            Atom::Keyword(kw) => format!(":{}", kw),
            Atom::Symbol(sym) => sym.clone(),
            Atom::Int(i) => i.to_string(),
            Atom::True => "true".to_owned(),
            Atom::False => "false".to_owned(),
            Atom::Nil => "nil".to_owned(),
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
    CoreFunction {
        name: &'static str,
        func: fn(Vec<Value>) -> MalResult<Value>,
    },
    Function {
        params: Vec<Atom>, // must be Symbols
        body: Box<Value>,
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
                format!("\"{}\"", escape(s))
            } else {
                s.clone()
            },
            Value::Keyword(kw) => format!(":{}", kw),
            Value::Symbol(sym) => sym.clone(),
            Value::Int(i) => i.to_string(),
            Value::True => "true".to_owned(),
            Value::False => "false".to_owned(),
            Value::Nil => "nil".to_owned(),
            Value::CoreFunction { name, func } => format!("core func {} {:?}", name, func),
            Value::Function { params, body, .. } => format!(
                "func ({}) {}",
                params
                    .iter()
                    .map(|p| p.string(true))
                    .collect::<Vec<String>>()
                    .join(" "),
                body
            ),
        }
    }

    pub fn is_atom(&self) -> bool {
        match self {
            Value::Str(_)
            | Value::Keyword(_)
            | Value::Symbol(_)
            | Value::Int(_)
            | Value::True
            | Value::False
            | Value::Nil => true,
            _ => false,
        }
    }

    pub fn subst(val: Value, from: &Atom, to: &Value) -> Value {
        match val {
            Value::Str(_)
            | Value::Symbol(_)
            | Value::Keyword(_)
            | Value::Int(_)
            | Value::True
            | Value::False
            | Value::Nil => if val == Value::from(from.clone()) {
                to.clone()
            } else {
                val
            },
            Value::List(list) => Value::List(
                list.into_iter()
                    .map(|l| Value::subst(l, from, to))
                    .collect(),
            ),
            Value::Vector(list) => Value::List(
                list.into_iter()
                    .map(|l| Value::subst(l, from, to))
                    .collect(),
            ),
            Value::Hashmap(mut hashmap) => {
                for v in hashmap.values_mut() {
                    *v = Value::subst(v.clone(), from, to);
                }
                Value::Hashmap(hashmap)
            }
            Value::Function { params, body } => Value::Function {
                params,
                body: Box::new(Value::subst(*body, from, to)),
            },
            _ => val,
        }
    }

    pub fn subst_binds(mut val: Value, binds: &[(Atom, Value)]) -> Value {
        for (var, expr) in binds {
            val = Value::subst(val, var, expr);
        }
        val
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

impl From<Atom> for Value {
    fn from(atom: Atom) -> Self {
        match atom {
            Atom::Str(s) => Value::Str(s),
            Atom::Keyword(kw) => Value::Keyword(kw),
            Atom::Symbol(sym) => Value::Symbol(sym),
            Atom::Int(i) => Value::Int(i),
            Atom::True => Value::True,
            Atom::False => Value::False,
            Atom::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string(true))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string_escaping() {
        assert_eq!(Value::Str(r#"ab"c"#)).string(true.to_owned(), r#""ab\"c""#);
    }
}