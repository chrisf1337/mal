use std::fmt;
use std::num::ParseIntError;

pub type Result<T> = ::std::result::Result<T, String>;

pub struct Reader {
    tokens: Vec<Token>,
    pos: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Ast {
    List(Vec<Ast>),
    Str(String),
    Symbol(String),
    Int(isize),
    True,
    False,
    Nil,
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::List(list) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    list.into_iter()
                        .map(|l| l.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;
                write!(f, ")")
            }
            Ast::Str(s) => write!(f, "\"{}\"", s.replace("\"", r#"\""#)),
            Ast::Symbol(sym) => write!(f, "{}", sym),
            Ast::Int(i) => write!(f, "{}", i),
            Ast::True => write!(f, "true"),
            Ast::False => write!(f, "false"),
            Ast::Nil => write!(f, "nil"),
        }
    }
}

impl Reader {
    pub fn read_str(input: &str) -> Result<Ast> {
        Reader::new(input)?.read_form()
    }

    fn new(input: &str) -> Result<Self> {
        Ok(Reader {
            tokens: Tokenizer::tokenize(input)?,
            pos: 0,
        })
    }

    fn peek(&self) -> Option<Token> {
        if self.pos == self.tokens.len() {
            None
        } else {
            Some(self.tokens[self.pos].clone())
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.pos == self.tokens.len() {
            None
        } else {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        }
    }

    fn read_form(&mut self) -> Result<Ast> {
        match self.peek() {
            Some(Token::LParen) => {
                let _ = self.next();
                Ok(self.read_list()?)
            }
            _ => Ok(self.read_atom()?),
        }
    }

    fn read_list(&mut self) -> Result<Ast> {
        let mut list = vec![];
        loop {
            match self.peek() {
                Some(Token::RParen) => break,
                Some(_) => list.push(self.read_form()?),
                None => return Err(String::from("expected ')', got EOF")),
            }
        }
        let _ = self.next();
        Ok(Ast::List(list))
    }

    fn read_atom(&mut self) -> Result<Ast> {
        match self.next() {
            Some(Token::Str(s)) => Ok(Ast::Str(s)),
            Some(Token::Symbol(sym)) => Ok(Ast::Symbol(sym)),
            Some(Token::Int(i)) => Ok(Ast::Int(i)),
            Some(Token::True) => Ok(Ast::True),
            Some(Token::False) => Ok(Ast::False),
            Some(Token::Nil) => Ok(Ast::Nil),
            Some(tok) => Err(format!("called read_atom() on non-atom: {:?}", tok)),
            None => Err(String::from("no remaining tokens")),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Tokenizer {
    input: Vec<char>,
    pos: usize,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Int(isize),
    Str(String),
    True,
    False,
    Nil,
}

impl Tokenizer {
    fn tokenize(input: &str) -> Result<Vec<Token>> {
        Tokenizer::new(input).tokenize_all()
    }

    fn new(input: &str) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn tokenize_all(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                '(' => {
                    tokens.push(Token::LParen);
                    self.pos += 1;
                }
                ')' => {
                    tokens.push(Token::RParen);
                    self.pos += 1;
                }
                '"' => {
                    self.pos += 1;
                    tokens.push(self.tokenize_str()?);
                }
                ch if ch.is_numeric() => tokens.push(self.tokenize_int()?),
                ch if ch.is_whitespace() || ch == ',' => self.pos += 1,
                _ => {
                    let symbol = self.tokenize_symbol()?;
                    tokens.push(match symbol {
                        Token::Symbol(ref sym) if sym == "true" => Token::True,
                        Token::Symbol(ref sym) if sym == "false" => Token::False,
                        Token::Symbol(ref sym) if sym == "nil" => Token::Nil,
                        sym @ Token::Symbol(_) => sym,
                        _ => unreachable!(),
                    });
                }
            }
        }
        Ok(tokens)
    }

    /// Called when tokenize() encounters a " char
    fn tokenize_str(&mut self) -> Result<Token> {
        let mut str_chars = vec![];
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                '"' => {
                    self.pos += 1;
                    return Ok(Token::Str(str_chars.into_iter().collect()));
                }
                '\\' => {
                    self.pos += 1;
                    if self.pos == self.input.len() {
                        return Err(String::from("EOF while processing escape char"));
                    }
                    str_chars.push(self.input[self.pos]);
                }
                c => str_chars.push(c),
            }
            self.pos += 1;
        }
        Err(String::from("EOF while processing string"))
    }

    fn tokenize_int(&mut self) -> Result<Token> {
        let mut int_chars = vec![];
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                c if c.is_numeric() => int_chars.push(c),
                _ => {
                    if int_chars.is_empty() {
                        return Err(format!("no int at pos {}", self.pos));
                    } else {
                        return Ok(Token::Int(
                            int_chars
                                .into_iter()
                                .collect::<String>()
                                .parse()
                                .map_err(|e: ParseIntError| format!("{}", e))?,
                        ));
                    }
                }
            }
            self.pos += 1;
        }
        Ok(Token::Int(
            int_chars
                .into_iter()
                .collect::<String>()
                .parse()
                .map_err(|e: ParseIntError| format!("{}", e))?,
        ))
    }

    fn tokenize_symbol(&mut self) -> Result<Token> {
        let mut symbol_chars = vec![];
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                c if c == '(' || c == ')' || c == '"' || c.is_whitespace() => {
                    if symbol_chars.is_empty() {
                        return Err(format!("no symbol at pos {}", self.pos));
                    } else {
                        return Ok(Token::Symbol(symbol_chars.into_iter().collect()));
                    }
                }
                c => symbol_chars.push(c),
            }
            self.pos += 1;
        }
        Ok(Token::Symbol(symbol_chars.into_iter().collect()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_str() {
        assert_eq!(
            Tokenizer::tokenize("\"string\""),
            Ok(vec![Token::Str(String::from("string"))])
        );
    }

    #[test]
    fn test_tokenize_escape_str() {
        assert_eq!(
            Tokenizer::tokenize(r#""str\"ing""#),
            Ok(vec![Token::Str(String::from("str\"ing"))])
        );
    }

    #[test]
    fn test_tokenize_int1() {
        assert_eq!(Tokenizer::tokenize("123"), Ok(vec![Token::Int(123)]));
    }

    #[test]
    fn test_tokenize_int2() {
        assert_eq!(Tokenizer::tokenize("123"), Ok(vec![Token::Int(123)]));
    }

    #[test]
    fn test_tokenize_int3() {
        assert_eq!(
            Tokenizer::tokenize("123 456"),
            Ok(vec![Token::Int(123), Token::Int(456)])
        );
    }

    #[test]
    fn test_tokenize_int4() {
        assert_eq!(
            Tokenizer::tokenize("123abc"),
            Ok(vec![Token::Int(123), Token::Symbol(String::from("abc"))])
        );
    }

    #[test]
    fn test_tokenize_symbol1() {
        assert_eq!(
            Tokenizer::tokenize("a1b2-c3"),
            Ok(vec![Token::Symbol(String::from("a1b2-c3"))])
        );
    }

    #[test]
    fn test_tokenize_keyword() {
        assert_eq!(
            Tokenizer::tokenize("true false nil"),
            Ok(vec![Token::True, Token::False, Token::Nil])
        );
    }

    #[test]
    fn test_tokenize() {
        assert_eq!(
            Tokenizer::tokenize("(+ 1 3)"),
            Ok(vec![
                Token::LParen,
                Token::Symbol(String::from("+")),
                Token::Int(1),
                Token::Int(3),
                Token::RParen,
            ])
        );
    }

    #[test]
    fn test_reader1() {
        assert_eq!(
            Reader::read_str("(+ 1 (* 2 3))"),
            Ok(Ast::List(vec![
                Ast::Symbol(String::from("+")),
                Ast::Int(1),
                Ast::List(vec![
                    Ast::Symbol(String::from("*")),
                    Ast::Int(2),
                    Ast::Int(3),
                ]),
            ]))
        )
    }

    #[test]
    fn test_reader2() {
        assert_eq!(
            Reader::read_str("(def f (x y) (+ x y))"),
            Ok(Ast::List(vec![
                Ast::Symbol(String::from("def")),
                Ast::Symbol(String::from("f")),
                Ast::List(vec![
                    Ast::Symbol(String::from("x")),
                    Ast::Symbol(String::from("y")),
                ]),
                Ast::List(vec![
                    Ast::Symbol(String::from("+")),
                    Ast::Symbol(String::from("x")),
                    Ast::Symbol(String::from("y")),
                ]),
            ]))
        )
    }

    #[test]
    fn test_reader_fail1() {
        assert_eq!(
            Reader::read_str("(1"),
            Err(String::from("expected ')', got EOF"))
        )
    }

    #[test]
    fn test_reader_fail2() {
        assert_eq!(
            Reader::read_str(r#"("st"#),
            Err(String::from("EOF while processing string"))
        )
    }

    #[test]
    fn test_reader_fail3() {
        assert_eq!(
            Reader::read_str(r#"("st\"#),
            Err(String::from("EOF while processing escape char"))
        )
    }
}
