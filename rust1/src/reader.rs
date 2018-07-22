use ast::Ast;
use std::num::ParseIntError;

pub type ReaderResult<T> = Result<T, String>;

pub struct Reader {
  tokens: Vec<Token>,
  pos: usize,
}

impl Reader {
  pub fn read_str(input: &str) -> ReaderResult<Ast> {
    Reader::new(input)?.read_form()
  }

  fn new(input: &str) -> ReaderResult<Self> {
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

  fn read_form(&mut self) -> ReaderResult<Ast> {
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
        let _ = self.next();
        Ok(self.read_hashmap()?)
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

  fn read_list(&mut self) -> ReaderResult<Ast> {
    let mut list = vec![];
    loop {
      match self.peek() {
        Some(Token::RParen) => break,
        Some(_) => list.push(self.read_form()?),
        None => return Err("expected ')', got EOF".to_owned()),
      }
    }
    let _ = self.next();
    Ok(Ast::List(list))
  }

  fn read_vector(&mut self) -> ReaderResult<Ast> {
    let mut vector = vec![];
    loop {
      match self.peek() {
        Some(Token::RBracket) => break,
        Some(_) => vector.push(self.read_form()?),
        None => return Err("expected ']', got EOF".to_owned()),
      }
    }
    let _ = self.next();
    Ok(Ast::Vector(vector))
  }

  fn read_hashmap(&mut self) -> ReaderResult<Ast> {
    let mut kv_tokens = vec![];
    loop {
      match self.peek() {
        Some(Token::RBrace) => break,
        Some(_) => kv_tokens.push(self.read_form()?),
        None => return Err("expected '}', got EOF".to_owned()),
      }
    }
    let _ = self.next();
    if kv_tokens.len() % 2 != 0 {
      Err("hashmap is missing a value".to_owned())
    } else {
      let mut kv_pairs = vec![];
      for chunk in kv_tokens.chunks(2) {
        if !chunk[0].is_atom() {
          return Err(format!("key {} is not an atom", chunk[0]));
        }
        kv_pairs.push((chunk[0].clone(), chunk[1].clone()))
      }
      Ok(Ast::Hashmap(kv_pairs))
    }
  }

  fn read_atom(&mut self) -> ReaderResult<Ast> {
    match self.next() {
      Some(Token::Str(s)) => Ok(Ast::Str(s)),
      Some(Token::Keyword(kw)) => Ok(Ast::Keyword(kw)),
      Some(Token::Symbol(sym)) => Ok(Ast::Symbol(sym)),
      Some(Token::Int(i)) => Ok(Ast::Int(i)),
      Some(Token::True) => Ok(Ast::True),
      Some(Token::False) => Ok(Ast::False),
      Some(Token::Nil) => Ok(Ast::Nil),
      Some(tok) => Err(format!("called read_atom() on non-atom: {:?}", tok)),
      None => Err("no remaining tokens".to_owned()),
    }
  }

  fn read_with_meta(&mut self) -> ReaderResult<Ast> {
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

fn is_delimiter(ch: char) -> bool {
  match ch {
    '(' | ')' | '"' | ':' | '[' | ']' | '{' | '}' | ',' => true,
    _ => ch.is_whitespace(),
  }
}

impl Tokenizer {
  fn tokenize(input: &str) -> ReaderResult<Vec<Token>> {
    Tokenizer::new(input).tokenize_all()
  }

  fn new(input: &str) -> Self {
    Tokenizer {
      input: input.chars().collect(),
      pos: 0,
    }
  }

  fn tokenize_all(&mut self) -> ReaderResult<Vec<Token>> {
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
        '-' => {
          let pos = self.pos;
          match self.tokenize_int() {
            Ok(i) => tokens.push(i),
            Err(_) => {
              self.pos = pos;
              tokens.push(self.tokenize_symbol()?)
            }
          }
        }
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

  fn tokenize_keyword(&mut self) -> ReaderResult<Token> {
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
        c if is_delimiter(c) => {
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
  fn tokenize_str(&mut self) -> ReaderResult<Token> {
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
            return Err("EOF while processing escape char".to_owned());
          }
          str_chars.push(match self.input[self.pos] {
            'n' => '\n',
            c => c,
          });
        }
        c => str_chars.push(c),
      }
      self.pos += 1;
    }
    Err("EOF while processing string".to_owned())
  }

  fn tokenize_int(&mut self) -> ReaderResult<Token> {
    let mut int_chars = vec![];
    if self.input[self.pos] == '-' {
      int_chars.push('-');
      self.pos += 1;
    }
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

  fn tokenize_symbol(&mut self) -> ReaderResult<Token> {
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
        c if is_delimiter(c) => {
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
      Ok(vec![Token::Str("string".to_owned())])
    );
  }

  #[test]
  fn test_tokenize_escape_str1() {
    assert_eq!(
      Tokenizer::tokenize(r#""str\"ing""#),
      Ok(vec![Token::Str("str\"ing".to_owned())])
    );
  }

  #[test]
  fn test_tokenize_escape_str2() {
    assert_eq!(
      Tokenizer::tokenize(r#""str\ning""#),
      Ok(vec![Token::Str("str\ning".to_owned())])
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
      Ok(vec![Token::Int(123), Token::Symbol("abc".to_owned())])
    );
  }

  #[test]
  fn test_tokenize_negative_int1() {
    assert_eq!(Tokenizer::tokenize("-1"), Ok(vec![Token::Int(-1)]));
  }

  #[test]
  fn test_tokenize_negative_int2() {
    assert_eq!(
      Tokenizer::tokenize("-"),
      Ok(vec![Token::Symbol("-".to_owned())])
    );
  }

  #[test]
  fn test_tokenize_symbol1() {
    assert_eq!(
      Tokenizer::tokenize("a1b2-c3"),
      Ok(vec![Token::Symbol("a1b2-c3".to_owned())])
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
        Token::Symbol("+".to_owned()),
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
        Token::Symbol("+".to_owned()),
        Token::Int(1),
      ])
    );
  }

  #[test]
  fn test_tokenize_no_whitespace() {
    assert_eq!(
      Tokenizer::tokenize(r#":kw1:kw2"str""#),
      Ok(vec![
        Token::Keyword("kw1".to_owned()),
        Token::Keyword("kw2".to_owned()),
        Token::Str("str".to_owned()),
      ])
    );
  }

  #[test]
  fn test_tokenize_delimiters() {
    assert_eq!(
      Tokenizer::tokenize("(fn* [a] nil)"),
      Ok(vec![
        Token::LParen,
        Token::Symbol("fn*".to_owned()),
        Token::LBracket,
        Token::Symbol("a".to_owned()),
        Token::RBracket,
        Token::Nil,
        Token::RParen,
      ])
    );
  }

  #[test]
  fn test_reader1() {
    assert_eq!(
      Reader::read_str("(+ 1 (* 2 3))"),
      Ok(Ast::List(vec![
        Ast::Symbol("+".to_owned()),
        Ast::Int(1),
        Ast::List(vec![Ast::Symbol("*".to_owned()), Ast::Int(2), Ast::Int(3)]),
      ]))
    )
  }

  #[test]
  fn test_reader2() {
    assert_eq!(
      Reader::read_str("(def f (x y) (+ x y))"),
      Ok(Ast::List(vec![
        Ast::Symbol("def".to_owned()),
        Ast::Symbol("f".to_owned()),
        Ast::List(vec![
          Ast::Symbol("x".to_owned()),
          Ast::Symbol("y".to_owned()),
        ]),
        Ast::List(vec![
          Ast::Symbol("+".to_owned()),
          Ast::Symbol("x".to_owned()),
          Ast::Symbol("y".to_owned()),
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
        (Ast::Keyword("a".to_owned()), Ast::Int(1)),
        (Ast::Keyword("b".to_owned()), Ast::Int(2)),
      ]))
    );
  }

  #[test]
  fn test_reader_hashmap_nested() {
    assert_eq!(
      Reader::read_str("{:a {:b 1}}"),
      Ok(Ast::Hashmap(vec![(
        Ast::Keyword("a".to_owned()),
        Ast::Hashmap(vec![(Ast::Keyword("b".to_owned()), Ast::Int(1))]),
      )]))
    );
  }

  #[test]
  fn test_reader_with_comments() {
    assert_eq!(
      Reader::read_str("(def f (x y) ; (+ x y)) \n (+ x y))"),
      Ok(Ast::List(vec![
        Ast::Symbol("def".to_owned()),
        Ast::Symbol("f".to_owned()),
        Ast::List(vec![
          Ast::Symbol("x".to_owned()),
          Ast::Symbol("y".to_owned()),
        ]),
        Ast::List(vec![
          Ast::Symbol("+".to_owned()),
          Ast::Symbol("x".to_owned()),
          Ast::Symbol("y".to_owned()),
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
      Ok(Ast::Deref(Box::new(Ast::Symbol("a".to_owned()))))
    );
  }

  #[test]
  fn test_reader_with_meta() {
    assert_eq!(
      Reader::read_str(r#"^{"a" 1} [1 2 3]"#),
      Ok(Ast::WithMeta(
        Box::new(Ast::Vector(vec![Ast::Int(1), Ast::Int(2), Ast::Int(3)])),
        Box::new(Ast::Hashmap(vec![(Ast::Str("a".to_owned()), Ast::Int(1))]))
      ))
    );
  }

  #[test]
  fn test_reader_fail1() {
    assert_eq!(
      Reader::read_str("(1"),
      Err("expected ')', got EOF".to_owned())
    )
  }

  #[test]
  fn test_reader_fail2() {
    assert_eq!(
      Reader::read_str(r#"("st"#),
      Err("EOF while processing string".to_owned())
    )
  }

  #[test]
  fn test_reader_fail3() {
    assert_eq!(
      Reader::read_str(r#"("st\"#),
      Err("EOF while processing escape char".to_owned())
    )
  }

  #[test]
  fn test_reader_fail_hashmap_nonatom_key() {
    assert!(Reader::read_str("{(1 2 3) 1}").is_err());
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
        Ast::Symbol("sym".to_owned()),
        Ast::Keyword("kw1".to_owned()),
        Ast::Keyword("kw2".to_owned()),
        Ast::Str("str".to_owned()),
      ]))
    );
  }
}
