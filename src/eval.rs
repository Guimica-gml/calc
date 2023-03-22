use std::collections::HashMap;
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::process;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol {
    Plus,
    Dash,
    Asterisk,
    Slash,
    Caret,
    ParenOpen,
    ParenClose,
}

impl Symbol {
    fn from(text: char) -> Option<Self> {
        match text {
            '+' => Some(Symbol::Plus),
            '-' => Some(Symbol::Dash),
            '*' => Some(Symbol::Asterisk),
            '^' => Some(Symbol::Caret),
            '/' => Some(Symbol::Slash),
            '(' => Some(Symbol::ParenOpen),
            ')' => Some(Symbol::ParenClose),
            _ => None,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Plus => write!(f, "+"),
            Symbol::Dash => write!(f, "-"),
            Symbol::Asterisk => write!(f, "*"),
            Symbol::Caret => write!(f, "^"),
            Symbol::Slash => write!(f, "/"),
            Symbol::ParenOpen => write!(f, "("),
            Symbol::ParenClose => write!(f, ")"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TokenKind {
    Symbol(Symbol),
    Number,
}

#[derive(Debug, Clone)]
pub struct Token {
    column: u32,
    text: String,
    kind: TokenKind,
}

pub struct Lexer {
    content: String,
    column: u32,
}

impl Lexer {
    pub fn new(equation: String) -> Self {
        Lexer {
            content: equation,
            column: 1,
        }
    }

    fn chop_char(&mut self) -> char {
        self.column += 1;
        self.content.remove(0)
    }

    fn peek_char(&mut self) -> Option<char> {
        self.content.chars().next()
    }

    fn trim_left(&mut self) {
        while let Some(ch) = self.peek_char() {
            if !ch.is_whitespace() {
                break;
            }
            self.chop_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_left();

        let column = self.column;
        let ch = self.peek_char()?;

        if let Some(symbol) = Symbol::from(ch) {
            return Some(Token {
                text: self.chop_char().to_string(),
                kind: TokenKind::Symbol(symbol),
                column
            });
        }

        if ch.is_numeric() {
            let mut text = String::new();

            while let Some(ch) = self.peek_char() {
                if !ch.is_numeric() {
                    break;
                }
                text.push(self.chop_char());
            }

            return Some(Token {
                text,
                kind: TokenKind::Number,
                column,
            });
        }

        // TODO: handle this without closing the program
        eprintln!("Error at column {}: Invalid token: `{}`", column, ch);
        process::exit(1);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pot,
}

impl BinaryOpKind {
    fn prec(&self) -> u32 {
        match self {
            BinaryOpKind::Add => 0,
            BinaryOpKind::Sub => 0,
            BinaryOpKind::Mul => 1,
            BinaryOpKind::Div => 1,
            BinaryOpKind::Pot => 2,
        }
    }
}

impl Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOpKind::Add => write!(f, "+"),
            BinaryOpKind::Sub => write!(f, "-"),
            BinaryOpKind::Mul => write!(f, "*"),
            BinaryOpKind::Div => write!(f, "/"),
            BinaryOpKind::Pot => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Pos,
    Neg,
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOpKind::Pos => write!(f, "+"),
            UnaryOpKind::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinaryOp {
        op: BinaryOpKind,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOpKind,
        right: Box<Expr>,
    },
    Number(f64),
    ParenBlock(Box<Expr>),
}

impl Expr {
    pub fn eval(&self) -> f64 {
        match self {
            Expr::BinaryOp { op, left, right } => match op {
                BinaryOpKind::Add => left.eval() + right.eval(),
                BinaryOpKind::Sub => left.eval() - right.eval(),
                BinaryOpKind::Mul => left.eval() * right.eval(),
                BinaryOpKind::Div => left.eval() / right.eval(),
                BinaryOpKind::Pot => f64::powf(left.eval(), right.eval()),
            },
            Expr::UnaryOp { op, right } => match op {
                UnaryOpKind::Pos => 0.0 + right.eval(),
                UnaryOpKind::Neg => 0.0 - right.eval(),
            },
            Expr::Number(number) => *number,
            Expr::ParenBlock(content) => content.eval(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::BinaryOp { op, left, right } => write!(f, "({} {} {})", left, op, right),
            Expr::UnaryOp { op, right } => write!(f, "({}{})", op, right),
            Expr::Number(number) => write!(f, "{}", number),
            Expr::ParenBlock(content) => write!(f, "({})", content),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEnd,
    UnexpectedToken(u32, String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedEnd => write!(f, "Error: unexpected end of expression"),
            ParseError::UnexpectedToken(loc, text) => write!(f, "Error at column {}: unexpected token `{}`", loc, text),
        }
    }
}

fn parse_primary(lexer: &mut Peekable<Lexer>) -> Result<Expr, ParseError> {
    use TokenKind::*;
    use self::Symbol::*;

    let token = match lexer.next() {
        Some(t) => t,
        None => return Err(ParseError::UnexpectedEnd),
    };

    match token.kind {
        Symbol(Plus) => Ok(Expr::UnaryOp { op: UnaryOpKind::Pos, right: Box::from(parse_primary(lexer)?) }),
        Symbol(Dash) => Ok(Expr::UnaryOp { op: UnaryOpKind::Neg, right: Box::from(parse_primary(lexer)?) }),
        Number => Ok(Expr::Number(token.text.parse().unwrap())),
        Symbol(ParenOpen) => {
            let expr = parse_expr_impl(lexer, None, true)?;
            match lexer.next() {
                Some(t) if t.kind != Symbol(ParenClose) => Err(ParseError::UnexpectedEnd),
                None => Err(ParseError::UnexpectedEnd),
                _ => Ok(Expr::ParenBlock(Box::from(expr)))
            }
        }
        _ => Err(ParseError::UnexpectedToken(token.column, token.text)),
    }
}

fn parse_expr_impl(lexer: &mut Peekable<Lexer>, lh_expr: Option<Expr>, expect_paren_close: bool) -> Result<Expr, ParseError> {
    use TokenKind::*;
    use self::Symbol::*;

    let symbol_to_op = HashMap::<TokenKind, BinaryOpKind>::from([
        (Symbol(Plus), BinaryOpKind::Add),
        (Symbol(Dash), BinaryOpKind::Sub),
        (Symbol(Asterisk), BinaryOpKind::Mul),
        (Symbol(Slash), BinaryOpKind::Div),
        (Symbol(Caret), BinaryOpKind::Pot),
    ]);

    let mut lh_expr = if let Some(expr) = lh_expr { expr } else { parse_primary(lexer)? };
    while let Some(t) = lexer.peek() {
        if symbol_to_op.contains_key(&t.kind) {
            let t = lexer.next().unwrap();
            let op = symbol_to_op[&t.kind];
            let rh_expr = parse_primary(lexer)?;

            match lexer.peek() {
                Some(t) if symbol_to_op.contains_key(&t.kind) => {
                    let next_op = symbol_to_op[&t.kind];
                    if op.prec() >= next_op.prec() {
                        lh_expr = Expr::BinaryOp { op, left: Box::from(lh_expr), right: Box::from(rh_expr) };
                    } else {
                        lexer.next().unwrap();
                        let rh_expr2 = Expr::BinaryOp { op: next_op, left: Box::from(rh_expr), right: Box::from(parse_primary(lexer)?) };
                        lh_expr = Expr::BinaryOp { op, left: Box::from(lh_expr), right: Box::from(rh_expr2) };
                    }
                }
                _ => {
                    lh_expr = Expr::BinaryOp { op, left: Box::from(lh_expr), right: Box::from(rh_expr) };
                }
            }
        } else {
            if t.kind == Symbol(ParenClose) && expect_paren_close {
                break;
            }
            return Err(ParseError::UnexpectedToken(t.column, t.text.clone()));
        }
    }

    Ok(lh_expr)
}

pub fn parse_expr(lexer: &mut Peekable<Lexer>) -> Result<Expr, ParseError> {
    parse_expr_impl(lexer, None, false)
}

pub fn eval(equation: String) -> Result<f64, ParseError> {
    Ok(parse_expr(&mut Lexer::new(equation).peekable())?.eval())
}
