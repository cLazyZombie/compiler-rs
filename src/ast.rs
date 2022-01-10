#![allow(clippy::new_without_default)]

use std::fmt::Display;

use crate::token::{IdentToken, Token};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn add(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }

    pub fn statement_count(&self) -> usize {
        self.statements.len()
    }

    pub fn get_statement(&self, idx: usize) -> Option<&Statement> {
        self.statements.get(idx)
    }

    pub fn take_statement(self) -> Vec<Statement> {
        self.statements
    }
}

#[derive(Debug)]
pub enum Node<'a> {
    Program(&'a Program),
    Stmt(&'a Statement),
    Expr(&'a Expr),
}

impl<'a> From<&'a Program> for Node<'a> {
    fn from(program: &'a Program) -> Self {
        Node::Program(program)
    }
}

impl<'a> From<&'a Statement> for Node<'a> {
    fn from(stmt: &'a Statement) -> Self {
        Node::Stmt(stmt)
    }
}

impl<'a> From<&'a Expr> for Node<'a> {
    fn from(expr: &'a Expr) -> Self {
        Node::Expr(expr)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExprStatement(ExprStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(let_stmt) => write!(f, "{}", let_stmt.to_string()),
            Statement::ReturnStatement(return_stmt) => write!(f, "{}", return_stmt.to_string()),
            Statement::ExprStatement(expr_stmt) => {
                write!(f, "{}", expr_stmt.to_string())
            }
            Statement::BlockStatement(block_stmt) => write!(f, "{}", block_stmt.to_string()),
        }
    }
}
#[derive(Debug, Clone)]
pub struct LetStatement {
    pub ident: IdentToken,
    pub expr: Expr,
}

impl LetStatement {
    pub fn new(ident: IdentToken, expr: Expr) -> Self {
        Self { ident, expr }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.ident, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub expr: Expr,
}

impl ReturnStatement {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprStatement {
    pub expr: Expr,
}

impl ExprStatement {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl Display for ExprStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        writeln!(f, "}}")
    }
}

impl From<BlockStatement> for Statement {
    fn from(block_stmt: BlockStatement) -> Self {
        Statement::BlockStatement(block_stmt)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(IdentExpr),
    Number(NumberExpr),
    String(StringExpr),
    Bool(BoolExpr),
    Array(ArrayExpr),
    ArrayIndex(ArrayIndexExpr),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    If(IfExpr),
    Function(FuncExpr),
    Call(CallExpr),
}

impl Expr {
    /// single expr - ideitifier나 int 같은 그 자체로 expression이 될수 있는 것들
    pub fn new_single_expr(token: Token) -> Option<Expr> {
        match token {
            Token::Ident(ident_tok) => Some(Expr::Identifier(IdentExpr { ident: ident_tok })),
            Token::Int(value) => Some(Expr::Number(NumberExpr { value })),
            Token::True => Some(Expr::Bool(BoolExpr { value: true })),
            Token::False => Some(Expr::Bool(BoolExpr { value: false })),
            Token::String(s) => Some(Expr::String(StringExpr { value: s })),
            _ => None,
        }
    }

    pub fn new_prefix_expr(op: Token, exp: Expr) -> Expr {
        Expr::Prefix(PrefixExpr {
            op,
            exp: Box::new(exp),
        })
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(identifier) => write!(f, "{}", identifier),
            Expr::Number(number) => write!(f, "{}", number),
            Expr::String(string_expr) => string_expr.fmt(f),
            Expr::Bool(boolean) => write!(f, "{}", boolean),
            Expr::Prefix(prefix) => {
                write!(f, "{}{}", prefix.op, prefix.exp)
            }
            Expr::Infix(infix) => write!(f, "({} {} {})", infix.left, infix.op, infix.right),
            Expr::If(if_expr) => write!(f, "{}", if_expr),
            Expr::Function(fn_expr) => write!(f, "{}", fn_expr),
            Expr::Call(call_expr) => write!(f, "{}", call_expr),
            Expr::Array(array_expr) => array_expr.fmt(f),
            Expr::ArrayIndex(array_index_expr) => array_index_expr.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub op: Token,
    pub exp: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IdentExpr {
    pub ident: IdentToken,
}

impl Display for IdentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Debug, Clone)]
pub struct NumberExpr {
    pub value: i32,
}

impl Display for NumberExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StringExpr {
    pub value: String,
}

impl Display for StringExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct BoolExpr {
    pub value: bool,
}

impl Display for BoolExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

impl Display for ArrayExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for element in &self.array {
            element.fmt(f)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone)]
pub struct ArrayIndexExpr {
    pub ident: IdentToken,
    pub index_expr: Box<Expr>,
}

impl Display for ArrayIndexExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)?;
        write!(f, "[")?;
        self.index_expr.fmt(f)?;
        write!(f, "]")
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl InfixExpr {
    pub fn new(left: Expr, op: Token, right: Expr) -> Self {
        Self {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub consequence_statement: Box<Statement>, // should be BlockStatement
    pub alternative_statement: Option<Box<Statement>>, // should be BlockStatement
                                               // pub consequence_statement: BlockStatement,
                                               // pub alternative_statement: Option<BlockStatement>,
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.consequence_statement)?;
        if let Some(alternative) = &self.alternative_statement {
            write!(f, "else {}", alternative)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FuncExpr {
    pub args: Vec<IdentToken>,
    pub body: Box<Statement>,
}

impl Display for FuncExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn (")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        self.body.fmt(f)
        // write!(f, "{}", self.block_statement)
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.func)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}
