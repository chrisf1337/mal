use std::fmt;
use std::num::ParseIntError;

pub type Result<T> = ::std::result::Result<T, String>;

pub struct Reader {
    tokens: Vec<Token>,
    pos: usize,
}

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
            Some(Token::LBracket) => {
                let _ = self.next();
                Ok(self.read_vector()?)
            }
            Some(Token::LBrace) => {
                let pos = self.pos;
                let _ = self.next();
                Ok(self.read_hashmap(pos)?)
            }
            Some(Token::Quote) => {
                let _ = self.next();
                Ok(Ast::Quote(Box::new(self.read_form()?)))
            }
            Some(Token::Quasiquote) => {
                let _ = self.next();
                Ok(Ast::Quasiquote(Box::new(self.read_form()?)))
            }
            Some(Token::Unquote) => {
                let _ = self.next();
                Ok(Ast::Unquote(Box::new(self.read_form()?)))
            }
            Some(Token::SpliceUnquote) => {
                let _ = self.next();
                Ok(Ast::SpliceUnquote(Box::new(self.read_form()?)))
            }
            Some(Token::Deref) => {
                let _ = self.next();
                Ok(Ast::Deref(Box::new(self.read_atom()?)))
            }
            Some(Token::WithMeta) => {
                let _ = self.next();
                Ok(self.read_with_meta()?)
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

    fn read_vector(&mut self) -> Result<Ast> {
        let mut vector = vec![];
        loop {
            match self.peek() {
                Some(Token::RBracket) => break,
                Some(_) => vector.push(self.read_form()?),
                None => return Err(String::from("expected ']', got EOF")),
            }
        }
        let _ = self.next();
        Ok(Ast::Vector(vector))
    }

    fn read_hashmap(&mut self, start_pos: usize) -> Result<Ast> {
        let mut kv_tokens = vec![];
        loop {
            match self.peek() {
                Some(Token::RBrace) => break,
                Some(_) => kv_tokens.push(self.read_form()?),
                None => return Err(String::from("expected '}', got EOF")),
            }
        }
        let _ = self.next();
        if kv_tokens.len() % 2 != 0 {
            Err(format!("hashmap at pos {} is missing a value", start_pos))
        } else {
            let mut kv_pairs = vec![];
            for chunk in kv_tokens.chunks(2) {
                kv_pairs.push((chunk[0].clone(), chunk[1].clone()))
            }
            Ok(Ast::Hashmap(kv_pairs))
        }
    }

    fn read_atom(&mut self) -> Result<Ast> {
        match self.next() {
            Some(Token::Str(s)) => Ok(Ast::Str(s)),
            Some(Token::Keyword(kw)) => Ok(Ast::Keyword(kw)),
            Some(Token::Symbol(sym)) => Ok(Ast::Symbol(sym)),
            Some(Token::Int(i)) => Ok(Ast::Int(i)),
            Some(Token::True) => Ok(Ast::True),
            Some(Token::False) => Ok(Ast::False),
            Some(Token::Nil) => Ok(Ast::Nil),
            Some(tok) => Err(format!("called read_atom() on non-atom: {:?}", tok)),
            None => Err(String::from("no remaining tokens")),
        }
    }

    fn read_with_meta(&mut self) -> Result<Ast> {
        match self.read_form() {
            Ok(meta) => match self.read_form() {
                Ok(val) => Ok(Ast::WithMeta(Box::new(val), Box::new(meta))),
                Err(err) => Err(format!("error while reading val form: {}", err)),
            },
            Err(err) => Err(format!("error while reading meta form: {}", err)),
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
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Symbol(String),
    Keyword(String),
    Int(isize),
    Str(String),
    True,
    False,
    Nil,
    Quote,
    Quasiquote,
    Unquote,
    SpliceUnquote,
    WithMeta,
    Deref,
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
        'outer: while self.pos < self.input.len() {
            match self.input[self.pos] {
                ';' => {
                    while self.input[self.pos] != '\n' {
                        self.pos += 1;
                        if self.pos == self.input.len() {
                            break 'outer;
                        }
                    }
                    self.pos += 1;
                }
                '(' => {
                    self.pos += 1;
                    tokens.push(Token::LParen);
                }
                ')' => {
                    self.pos += 1;
                    tokens.push(Token::RParen);
                }
                '[' => {
                    self.pos += 1;
                    tokens.push(Token::LBracket);
                }
                ']' => {
                    self.pos += 1;
                    tokens.push(Token::RBracket);
                }
                '{' => {
                    self.pos += 1;
                    tokens.push(Token::LBrace);
                }
                '}' => {
                    self.pos += 1;
                    tokens.push(Token::RBrace);
                }
                '"' => {
                    self.pos += 1; // ORDER IS IMPORTANT HERE!
                    tokens.push(self.tokenize_str()?);
                }
                ':' => {
                    self.pos += 1; // ORDER IS IMPORTANT HERE!
                    tokens.push(self.tokenize_keyword()?);
                }
                '\'' => {
                    self.pos += 1;
                    tokens.push(Token::Quote);
                }
                '`' => {
                    self.pos += 1;
                    tokens.push(Token::Quasiquote);
                }
                '~' if self.pos + 1 < self.input.len() && self.input[self.pos + 1] == '@' => {
                    self.pos += 2;
                    tokens.push(Token::SpliceUnquote);
                }
                '~' => {
                    self.pos += 1;
                    tokens.push(Token::Unquote);
                }
                '^' => {
                    self.pos += 1;
                    tokens.push(Token::WithMeta);
                }
                '@' => {
                    self.pos += 1;
                    tokens.push(Token::Deref);
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

    fn tokenize_keyword(&mut self) -> Result<Token> {
        let mut keyword_chars = vec![];
        'outer: while self.pos < self.input.len() {
            match self.input[self.pos] {
                ';' => {
                    while self.input[self.pos] != '\n' {
                        self.pos += 1;
                        if self.pos == self.input.len() {
                            break 'outer;
                        }
                    }
                    self.pos += 1;
                }
                c if c == '(' || c == ')' || c == '"' || c == ':' || c.is_whitespace() => {
                    if keyword_chars.is_empty() {
                        return Err(format!("no symbol at pos {}", self.pos));
                    } else {
                        return Ok(Token::Keyword(keyword_chars.into_iter().collect()));
                    }
                }
                c => keyword_chars.push(c),
            }
            self.pos += 1;
        }
        Ok(Token::Keyword(keyword_chars.into_iter().collect()))
    }

    /// Called when tokenize() encounters a " char
    fn tokenize_str(&mut self) -> Result<Token> {
        let mut str_chars = vec![];
        'outer: while self.pos < self.input.len() {
            match self.input[self.pos] {
                ';' => {
                    while self.input[self.pos] != '\n' {
                        self.pos += 1;
                        if self.pos == self.input.len() {
                            break 'outer;
                        }
                    }
                    self.pos += 1;
                }
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
        'outer: while self.pos < self.input.len() {
            match self.input[self.pos] {
                ';' => {
                    while self.input[self.pos] != '\n' {
                        self.pos += 1;
                        if self.pos == self.input.len() {
                            break 'outer;
                        }
                    }
                    self.pos += 1;
                }
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
        'outer: while self.pos < self.input.len() {
            match self.input[self.pos] {
                ';' => {
                    while self.input[self.pos] != '\n' {
                        self.pos += 1;
                        if self.pos == self.input.len() {
                            break 'outer;
                        }
                    }
                    self.pos += 1;
                }
                c if c == '(' || c == ')' || c == '"' || c == ':' || c.is_whitespace() => {
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
    fn test_tokenize_quote() {
        assert_eq!(
            Tokenizer::tokenize("'1"),
            Ok(vec![Token::Quote, Token::Int(1)])
        );
    }

    #[test]
    fn test_tokenize_unquote() {
        assert_eq!(
            Tokenizer::tokenize("~1"),
            Ok(vec![Token::Unquote, Token::Int(1)])
        );
    }

    #[test]
    fn test_tokenize_splice_unquote() {
        assert_eq!(
            Tokenizer::tokenize("~@1"),
            Ok(vec![Token::SpliceUnquote, Token::Int(1)])
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
    fn test_tokenize_with_comments() {
        assert_eq!(
            Tokenizer::tokenize("(+ 1 ; 3)"),
            Ok(vec![
                Token::LParen,
                Token::Symbol(String::from("+")),
                Token::Int(1),
            ])
        );
    }

    #[test]
    fn test_tokenize_no_whitespace() {
        assert_eq!(
            Tokenizer::tokenize(r#":kw1:kw2"str""#),
            Ok(vec![
                Token::Keyword(String::from("kw1")),
                Token::Keyword(String::from("kw2")),
                Token::Str(String::from("str")),
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
    fn test_reader_vector() {
        assert_eq!(
            Reader::read_str("[1 2 3]"),
            Ok(Ast::Vector(vec![Ast::Int(1), Ast::Int(2), Ast::Int(3)]))
        )
    }

    #[test]
    fn test_reader_hashmap() {
        assert_eq!(
            Reader::read_str("{:a 1 :b 2}"),
            Ok(Ast::Hashmap(vec![
                (Ast::Keyword(String::from("a")), Ast::Int(1)),
                (Ast::Keyword(String::from("b")), Ast::Int(2)),
            ]))
        );
    }

    #[test]
    fn test_reader_hashmap_nested() {
        assert_eq!(
            Reader::read_str("{:a {:b 1}}"),
            Ok(Ast::Hashmap(vec![(
                Ast::Keyword(String::from("a")),
                Ast::Hashmap(vec![(Ast::Keyword(String::from("b")), Ast::Int(1))]),
            )]))
        )
    }

    #[test]
    fn test_reader_with_comments() {
        assert_eq!(
            Reader::read_str("(def f (x y) ; (+ x y)) \n (+ x y))"),
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
    fn test_reader_quote1() {
        assert_eq!(
            Reader::read_str("'1"),
            Ok(Ast::Quote(Box::new(Ast::Int(1))))
        );
    }

    #[test]
    fn test_reader_quote2() {
        assert_eq!(
            Reader::read_str("'(1 2 3)"),
            Ok(Ast::Quote(Box::new(Ast::List(vec![
                Ast::Int(1),
                Ast::Int(2),
                Ast::Int(3),
            ]))))
        );
    }

    #[test]
    fn test_reader_deref() {
        assert_eq!(
            Reader::read_str("@a"),
            Ok(Ast::Deref(Box::new(Ast::Symbol(String::from("a")))))
        );
    }

    #[test]
    fn test_reader_with_meta() {
        assert_eq!(
            Reader::read_str(r#"^{"a" 1} [1 2 3]"#),
            Ok(Ast::WithMeta(
                Box::new(Ast::Vector(vec![Ast::Int(1), Ast::Int(2), Ast::Int(3)])),
                Box::new(Ast::Hashmap(vec![(
                    Ast::Str(String::from("a")),
                    Ast::Int(1),
                )]))
            ))
        );
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

    #[test]
    fn test_reader_with_meta_fail() {
        assert_eq!(
            Reader::read_str("^1"),
            Err(String::from(
                "error while reading val form: no remaining tokens"
            ))
        );
    }

    #[test]
    fn test_reader_no_whitespace() {
        assert_eq!(
            Reader::read_str(r#"(sym:kw1:kw2"str")"#),
            Ok(Ast::List(vec![
                Ast::Symbol(String::from("sym")),
                Ast::Keyword(String::from("kw1")),
                Ast::Keyword(String::from("kw2")),
                Ast::Str(String::from("str")),
            ]))
        );
    }
}
