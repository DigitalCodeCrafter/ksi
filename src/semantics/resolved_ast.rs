use crate::common::{Span, diagnostics::ErrorGuaranteed};
pub use crate::syntax::{Unit, BinaryOp, UnaryOp, Literal};
use crate::semantics::resolver::SymbolId;

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedAst {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Let { sym: SymbolId, value: Expr },
    Expr(Expr),
    Empty,
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Identifier { sym: SymbolId },
    UnaryOp { op: UnaryOp, expr: Box<Expr> },
    BinaryOp { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Block { stmts: Vec<Stmt>, tail_expr: Option<Box<Expr>> },
    Error(ErrorGuaranteed),
}
