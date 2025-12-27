use crate::Span;
pub use crate::parsed_ast::Unit;
pub use crate::parsed_ast::BinaryOp;
use crate::resolver::SymbolId;

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
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Number { value: f64, unit: Option<Unit> },
    Identifier { sym: SymbolId },
    BinaryOp { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Error,
}
