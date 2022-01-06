use std::fmt::Display;

use crate::parser::Precedence;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Ident(IdentToken),
    Int(i32),
    String(String),
    Assign,
    Plus,
    Minus,
    Bang,      // !
    Asterrisk, // *
    Slash,     // /
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace, // {
    RBrace,
    LBlock, // [
    RBlock, // ]
    Eq,     // ==
    NotEq,  // !=
    LT,     // <
    GT,     // >
    LTEq,   // <=
    GTEq,   // >=
    // 예약어
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn is_infix_op(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Minus
                | Token::Asterrisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::LT
                | Token::GT
                | Token::LTEq
                | Token::GTEq
                | Token::Function
                | Token::LParen
                | Token::LBlock,
        )
    }

    pub fn is_prefix_op(&self) -> bool {
        matches!(self, Token::Minus | Token::Bang)
    }

    pub fn precedence(&self) -> Option<Precedence> {
        match self {
            Token::Eq | Token::NotEq => Some(Precedence::Equals),
            Token::LT | Token::GT | Token::LTEq | Token::GTEq => Some(Precedence::LessGreater),
            Token::Plus | Token::Minus => Some(Precedence::Sum),
            Token::Asterrisk | Token::Slash => Some(Precedence::Product),
            Token::Function | Token::LParen => Some(Precedence::Call),
            Token::LBlock => Some(Precedence::ArrayIndex),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, "illegal"),
            Token::EOF => write!(f, "eof"),
            Token::Ident(s) => write!(f, "{}", s.0),
            Token::Int(i) => write!(f, "{}", i),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterrisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBlock => write!(f, "["),
            Token::RBlock => write!(f, "]"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::LTEq => write!(f, "<="),
            Token::GTEq => write!(f, ">="),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct IdentToken(pub String);

impl Display for IdentToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct NumToken(pub i32);

impl Display for NumToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
