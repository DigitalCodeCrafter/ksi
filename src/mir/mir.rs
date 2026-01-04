use crate::{common::Span, mir::{BlockId, LocalId}, semantics::Type};
pub use crate::semantics::typed_ast::BinaryOp;

#[derive(Debug, PartialEq)]
pub struct Body {
    pub blocks: Vec<Block>,
    pub locals: Vec<Local>,
}

#[derive(Debug, PartialEq)]
pub struct Local {
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub terminator: Terminator,
}

#[derive(Debug, PartialEq)]
pub enum Terminator {
    Goto(BlockId),
    Return(Operand),
    Unreachable,
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    Assign(Place, RValue),
    Debug(DebugInfo),
}

#[derive(Debug, PartialEq)]
pub enum RValue {
    Use(Operand),
    Binary(BinaryOp, Operand, Operand),
    Poison,
}

#[derive(Debug, PartialEq)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Const(Const),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    pub local: LocalId,
    pub projection: Vec<Projection>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Projection {
    
}

#[derive(Debug, PartialEq)]
pub enum Const {
    Number(f64),
    Unit,
}

#[derive(Debug, PartialEq)]
pub struct DebugInfo {
    pub span: Span,
    pub kind: DebugKind,
}

#[derive(Debug, PartialEq)]
pub enum DebugKind {
    EnterExpr,
    ExitExpr,
    DeclareLocal {
        local: LocalId,
        name: String,
    }
}
