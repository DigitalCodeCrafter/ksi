use std::{collections::HashMap, io::Write};

use ksi::{evaluation::{Dimension, Quantity, eval_expression}, lexer::Lexer, parser::Parser};

fn main() {
    let mut env = HashMap::new();
    add_units(&mut env);

    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        match std::io::stdout().flush() {
            Err(e) => break eprintln!("Error: {e}"),
            _ => {},
        }
        match std::io::stdin().read_line(&mut line) {
            Err(e) => break eprintln!("Error: {e}"),
            _ => {},
        }

        if line.trim().is_empty() { continue; }
        if line.trim() == "exit" { break; }

        let token_stream = Lexer::new(line.chars());
        
        let expr = match Parser::new(token_stream).parse_expr(0) {
            Ok(ast) => ast,
            Err(e) => { eprintln!("Error: {e}"); continue; }
        };

        match eval_expression(&expr, &mut env) {
            Err(e) => eprintln!("Error: {e}"),
            Ok(o) if !o.is_empty() => println!("{o}"),
            _ => {},
        }
    }
}

fn add_units(env: &mut HashMap<String, Quantity>) {
    env.insert("m".to_string(), Quantity { value: 1.0, dim: Dimension { l: 1, t: 0, m: 0 } });
    env.insert("s".to_string(), Quantity { value: 1.0, dim: Dimension { l: 0, t: 1, m: 0 } });
    env.insert("kg".to_string(), Quantity { value: 1.0, dim: Dimension { l: 0, t: 0, m: 1 } });
}

mod ksi {
    pub mod lexer {
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Literal(f64),

    Add,
    Sub,
    Mul,
    Div,
    Assign,

    LParen,
    RParen,

    EOF,
}

// Stream based lexer

pub struct Lexer<I: Iterator<Item = char>> {
    src: Peekable<I>,
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Err(_) => None,
            Ok(Token::EOF) => None,
            Ok(t) => Some(t)
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(src: I) -> Self {
        Self { src: src.peekable() }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();

        let Some(c) = self.src.peek() else { return Ok(Token::EOF); };

        match c {
            '0'..='9' => self.parse_lit(),
            'a'..='z' | 'A'..='Z' | '_' => self.parse_ident(),
            _ => self.parse_symbol(),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.src.peek() {
            if !c.is_whitespace() { break; }
            self.src.next();
        }
    }

    fn parse_lit(&mut self) -> Result<Token, String> {
        let mut numstr = String::new();
        let mut has_dot = false;
        let mut has_e = false;

        while let Some(c) = self.src.peek() {
            if !c.is_ascii_digit() && (*c != '.' || has_dot) && (*c != 'e' || has_e) { break; }
            if *c == '.' {
                has_dot = true;
                numstr.push(self.src.next().unwrap());

                let Some(c) = self.src.peek() else { break; };
                if !c.is_ascii_digit() { break; }
            }
            else if *c == 'e' {
                has_e = true;
                numstr.push(self.src.next().unwrap());

                let Some(c) = self.src.peek() else { break; };
                if !c.is_ascii_digit() && *c != '+' && *c != '-' { break; }
            }
            numstr.push(self.src.next().unwrap());
        }

        match numstr.parse() {
            Ok(f) => Ok(Token::Literal(f)),
            Err(e) => Err(format!("Invalid float literal: {e}"))
        }
    }

    fn parse_ident(&mut self) -> Result<Token, String> {
        let mut ident = String::new();

        while let Some(c) = self.src.peek() {
            if !c.is_alphanumeric() && *c != '_' { break; }
            ident.push(self.src.next().unwrap());
        }

        Ok(Token::Ident(ident))
    }

    fn parse_symbol(&mut self) -> Result<Token, String> {
        let Some(c) = self.src.next() else { return Ok(Token::EOF); };

        match c {
            '+' => Ok(Token::Add),
            '-' => Ok(Token::Sub),
            '*' => Ok(Token::Mul),
            '/' => Ok(Token::Div),
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '=' => Ok(Token::Assign),
            _ => Err(format!("Invalid Char '{}'", c)),
        }
    }
}

    }
    pub mod parser {
use std::iter::Peekable;
use super::lexer::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Token, Box<Expression>, Box<Expression>),
    Var(String),
    Lit(f64),
}

pub struct Parser<I: Iterator<Item = Token>> {
    lexer: Peekable<I>
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(lexer: I) -> Self {
        Self { lexer: lexer.peekable() }
    }

    pub fn parse_expr(&mut self, min_bp: u8) -> Result<Expression, String> {
        let mut lhs = match self.lexer.next() {
            None => return Err("Unexpected end of token stream".to_string()),
            Some(Token::LParen) => {
                let lhs = self.parse_expr(0)?;
                if !matches!(self.lexer.next(), Some(Token::RParen)) {
                    return Err("Expected ')'".to_string());
                }
                lhs
            }
            Some(Token::Literal(l)) => Expression::Lit(l),
            Some(Token::Ident(i)) => Expression::Var(i),
            Some(t) => return Err(format!("Unexpected token: {:?}", t)),
        };

        loop {
            let op = match self.lexer.peek() {
                None => break,
                Some(Token::EOF) => break,
                Some(Token::RParen) => break,
                Some(t) => t,
            };

            let (l_bp, r_bp) = Self::infix_bp(op)?;

            if l_bp < min_bp { break; }
            let op = self.lexer.next().unwrap();

            let rhs = self.parse_expr(r_bp)?;

            lhs = Expression::Binary(op, Box::new(lhs), Box::new(rhs))
        }

        Ok(lhs)
    }

    fn infix_bp(op: &Token) -> Result<(u8, u8), String> {
        match op {
            Token::Assign => Ok((0, 1)),
            Token::Add => Ok((2, 3)),
            Token::Sub => Ok((2, 3)),
            Token::Mul => Ok((4, 5)),
            Token::Div => Ok((4, 5)),
            t => Err(format!("{:?} is not an operator", t))
        }
    }
}

    }
    pub mod evaluation {
use std::{collections::HashMap, fmt::Display};

use super::{lexer::Token, parser::Expression};

#[derive(Debug, Clone)]
pub struct Quantity {
    pub value: f64,
    pub dim: Dimension,
}
impl Display for Quantity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.value, self.dim)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dimension {
    pub l: i8,
    pub t: i8,
    pub m: i8,
}
impl Display for Dimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dims = Vec::with_capacity(3);
        if self.l != 0 { if self.l != 1 { dims.push(format!("m^{}", self.l)) } else { dims.push("m".to_string()) }}
        if self.t != 0 { if self.t != 1 { dims.push(format!("s^{}", self.l)) } else { dims.push("s".to_string()) }}
        if self.m != 0 { if self.m != 1 { dims.push(format!("kg^{}", self.l)) } else { dims.push("kg".to_string()) }}
        if dims.is_empty() {
            write!(f, "[1]")
        } else {
            write!(f, "{}", dims.join(" "))
        }
    }
}


pub fn eval_node(node: &Expression, env: &HashMap<String, Quantity>) -> Result<Quantity, String> {
    match node {
        Expression::Lit(f) => Ok(Quantity { value: *f, dim: Dimension { l: 0, t: 0, m: 0 } }),
        Expression::Var(name) => match env.get(name) {
            Some(var) => Ok(var.clone()),
            None => Err(format!("Unknown variable or unit: {name}")),
        }
        Expression::Binary(op, lhs  , rhs) => {
            let left = eval_node(lhs, env)?;
            let right = eval_node(rhs, env)?;

            match op {
                Token::Add => {
                    if left.dim != right.dim { return Err(format!("Cannot add {} + {}", left.dim, right.dim)); }
                    Ok(Quantity { value: left.value + right.value, dim: left.dim })
                }
                Token::Sub => {
                    if left.dim != right.dim { return Err(format!("Cannot subtract {} - {}", left.dim, right.dim)); }
                    Ok(Quantity { value: left.value - right.value, dim: left.dim })
                }
                Token::Mul => {
                    let new_dim = Dimension {
                        l: left.dim.l + right.dim.l,
                        t: left.dim.t + right.dim.t,
                        m: left.dim.m + right.dim.m,
                    };
                    Ok(Quantity { value: left.value * right.value, dim: new_dim })
                }
                Token::Div => {
                    let new_dim = Dimension {
                        l: left.dim.l - right.dim.l,
                        t: left.dim.t - right.dim.t,
                        m: left.dim.m - right.dim.m,
                    };
                    Ok(Quantity { value: left.value / right.value, dim: new_dim })
                }
                _ => Err(format!("Invalid operator: {:?}", op))
            }
        }
    }
}

pub fn eval_expression(node: &Expression, env: &mut HashMap<String, Quantity>) -> Result<String, String> {
    match node {
        Expression::Binary(Token::Assign, var, expr) => {
            let Expression::Var(ref name) = **var else { return Err("Expected Variable on left hand side of '='".to_string()); };

            let value = eval_node(expr, env)?;

            env.insert(name.clone(), value);
            Ok("".to_string())
        }
        _ => {
            let value = eval_node(node, env)?;
            env.insert("_".to_string(), value.clone());
            Ok(format!("{}", value))
        }
    }
}

    }
}