pub use crate::semantics::typed_ast::BinaryOp;
use crate::semantics::Type;
use crate::ir::{
    lowerer::{BlockId, LocalId, TempId}
};

pub struct ProgramIR {
    pub functions: Vec<FunctionIR>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionIR {
    pub locals: Vec<Local>,
    pub temps: Vec<Temp>,
    pub blocks: Vec<Block>,
}

#[derive(Debug, PartialEq)]
pub struct Temp {
    pub ty: Type
}

#[derive(Debug, PartialEq)]
pub struct Local {
    pub ty: Type
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub terminator: Terminator,
}

#[derive(Debug, PartialEq)]
pub enum Terminator {
    Goto(BlockId),
    Return(Value),
    Unreachable,
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    LoadConst { dst: TempId, value: Const },
    Binary { dst: TempId, op: BinaryOp, lhs: Value, rhs: Value },
    Store { place: Place, value: Value },
    Load { dst: TempId, place: Place },
    Poision { dst: Option<TempId> }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Const(Const),
    Temp(TempId),
}

#[derive(Debug, PartialEq)]
pub enum Place {
    Local(LocalId)
}

#[derive(Debug, PartialEq)]
pub enum Const {
    Number(f64),
    Unit,
}
