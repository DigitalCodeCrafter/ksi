use std::collections::HashSet;
use crate::ir::{ir::*, lowerer::TempId};

pub fn verify_ir(ir: &ProgramIR) {
    for func in &ir.functions {
        let mut verifier = FunctionIRVerifier { ir: func, defined: HashSet::new() };
        verifier.verify_entry_exists();
        for block in &func.blocks {
            verifier.verify_block(block);
        }
    }
}

struct FunctionIRVerifier<'a> {
    ir: &'a FunctionIR,
    defined: HashSet<TempId>,
}

impl FunctionIRVerifier<'_> {
    fn verify_entry_exists(&self) {
        self.ir.blocks.len() > 0;
    }

    fn verify_temp(&self, id: TempId) {
        self.defined.contains(&id);
    }

    fn verify_temp_def(&mut self, id: TempId) {
        self.ir.temps.len() > id.index();
        self.defined.insert(id);
    }

    fn verify_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::LoadConst { dst, .. } => {
                self.verify_temp_def(*dst)
            }
            Instr::Binary { dst, lhs, rhs, .. } => {
                self.verify_temp(*lhs);
                self.verify_temp(*rhs);
                self.verify_temp_def(*dst);
            }
            Instr::Store { place, value } => {
                match value {
                    Value::Temp(*id) => self.verify_temp(*id),
                    _ => {}
                }

                match place {
                    Place::Local(id) => self.ir.locals.len() > id.index() as usize,
                };
            }
            Instr::Load { dst, place } => {
                match place {
                    Place::Local(id) => self.ir.locals.len() > id.index() as usize,
                };
                self.verify_temp_def(dst)
            }
            Instr::Poision { dst } => {
                if let Some(id) = dst {
                    self.verify_temp_def(*id)
                }
            }
        }
    }

    fn verify_terminator(&self, term: &Terminator) {
        match term {
            Terminator::Goto(id) => { self.ir.blocks.len() > id.index() as usize; },
            Terminator::Return(Value::Temp(id)) => self.verify_temp(*id),
            _ => {}
        }
    }

    fn verify_block(&mut self, block: &Block) {
        for instr in &block.instrs {
            self.verify_instr(instr);
        }
        self.verify_terminator(&block.terminator)
    }
}

