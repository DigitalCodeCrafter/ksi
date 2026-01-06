use crate::common::{Span, diagnostics::ErrorGuaranteed};
pub use crate::semantics::resolved_ast::{Unit, BinaryOp, UnaryOp, Literal};
use crate::semantics::{
    resolver::SymbolId,
    typechecker::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAst {
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
    pub ty: Type,
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
