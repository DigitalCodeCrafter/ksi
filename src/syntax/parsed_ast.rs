use crate::common::Span;

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
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    Number { value: f64, unit: Option<Unit> },
    Identifier { name: &'a str },
    BinaryOp { op: BinaryOp, left: Box<Expr<'a>>, right: Box<Expr<'a>> },
    Block { stmts: Vec<Stmt<'a>>, tail_expr: Option<Box<Expr<'a>>> },
    Error,
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
}
