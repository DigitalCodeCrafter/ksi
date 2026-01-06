use crate::common::{Span, diagnostics::ErrorGuaranteed};

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedAst<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind<'a> {
    Let { name: &'a str, value: Expr<'a> },
    Expr(Expr<'a>),
    Empty,
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    Literal(Literal),
    Identifier { name: &'a str },
    UnaryOp { op: UnaryOp, expr: Box<Expr<'a>> },
    BinaryOp { op: BinaryOp, left: Box<Expr<'a>>, right: Box<Expr<'a>> },
    Block { stmts: Vec<Stmt<'a>>, tail_expr: Option<Box<Expr<'a>>> },
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number { value: f64, unit: Option<Unit> },
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Unit {

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Assign,
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    // And,
    // Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    // Assign,
    Neg,
}
