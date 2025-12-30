use crate::common::Span;
pub use crate::semantics::resolved_ast::{Unit, BinaryOp};
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
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Number { value: f64, unit: Option<Unit> },
    Identifier { sym: SymbolId },
    BinaryOp { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Block { stmts: Vec<Stmt>, tail_expr: Option<Box<Expr>> },
    Error,
}
