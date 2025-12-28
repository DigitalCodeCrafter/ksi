use crate::ir::ir::{BinaryOp, Block, FunctionIR, Instr, Place, Terminator, Value};

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Temp(t) => write!(f, "t{}", t.index()),
            Value::ConstNumber(n) => write!(f, "const {}", n),
        }
    }
}

impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Place::Local(l) => write!(f, "l{}", l.index()),
        }
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::LoadConst { dst, value } => {
                write!(f, "t{} = const {}", dst.index(), value)
            }
            Instr::Binary { dst, op, lhs, rhs } => {
                let op_str = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                };
                write!(f, "t{} = {} {} {}", dst.index(), lhs, op_str, rhs)
            }
            Instr::Store { place, value } => {
                write!(f, "store {}, {}", place, value)
            }
            Instr::Load { dst, place } => {
                write!(f, "t{} = load {}", dst.index(), place)
            }
            Instr::Poision { dst } => {
                if let Some(dst) = dst {
                    write!(f, "poison t{}", dst.index())
                } else {
                    write!(f,"posion")
                }
            }
        }
    }
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Goto(b) => write!(f, "goto block{}", b.index()),
            Terminator::Return(v) => write!(f, "return {}", v),
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &self.instrs {
            write!(f, "  {}\n", instr)?;
        }
        write!(f, "  {}\n", self.terminator)
    }
}

impl std::fmt::Display for FunctionIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "locals:\n")?;
        for (i, local) in self.locals.iter().enumerate() {
            write!(f, "  l{}: {:?}\n", i, local.ty)?;
        }
        for (i, block) in self.blocks.iter().enumerate() {
            write!(f, "block{}:\n{}",i,block)?;
        }
        Ok(())
    }
}
