use crate::mir::{BlockId, lowerer::LocalId, mir::*};

pub fn verify_ir(ir: &Body) -> Result<(), ()> {
    let mut verifier = FunctionIRVerifier { ir, errors: Vec::new() };
    verifier.verify_entry_exists();
    for block in &ir.blocks {
        verifier.verify_block(block);
    }
    verifier.throw()
}

struct FunctionIRVerifier<'a> {
    ir: &'a Body,
    errors: Vec<String>,
}

impl FunctionIRVerifier<'_> {
    fn throw(self) -> Result<(), ()> {
        if self.errors.is_empty() { return Ok(()); }
        for e in self.errors {
            eprintln!("{}", e);
        }
        Err(())
    }

    fn verify_entry_exists(&mut self) {
        if self.ir.blocks.len() <= 0 {
            self.errors.push("Missing entry block".to_string());
        }
    }

    fn verify_local(&mut self, id: LocalId) {
        if self.ir.locals.len() <= id.index() as usize {
            self.errors.push(format!("Missing metadata for local {:?}", id));
        }
    }

    fn verify_block_id(&mut self, id: BlockId) {
        if self.ir.blocks.len() <= id.index() as usize {
            self.errors.push(format!("Block {:?} does not exist (max: {})", id, self.ir.blocks.len() - 1));
        }
    }

    fn verify_place(&mut self, p: &Place) {
        self.verify_local(p.local);
    }

    fn verify_operand(&mut self, op: &Operand) {
        match op {
            Operand::Move(p) => self.verify_place(p),
            Operand::Copy(p) => self.verify_place(p),
            Operand::Const(_) => {},
        }
    }

    fn verify_rval(&mut self, rval: &RValue) {
        match rval {
            RValue::Use(op) => self.verify_operand(op),

            RValue::Binary(_, lhs, rhs) => {
                self.verify_operand(lhs);
                self.verify_operand(rhs);
            }

            RValue::Poison => {},
        }
    }

    fn verify_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::Assign(place, rval) => {
                self.verify_place(place);
                self.verify_rval(rval);
            }
            Instr::Debug(info) => {
                if let DebugKind::DeclareLocal { local, .. } = info.kind {
                    self.verify_local(local);
                }
            }
        }
    }

    fn verify_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Goto(id) => self.verify_block_id(*id),
            Terminator::Return(op) => self.verify_operand(op),
            Terminator::Unreachable => {}
        }
    }

    fn verify_block(&mut self, block: &Block) {
        for instr in &block.instrs {
            self.verify_instr(instr);
        }
        self.verify_terminator(&block.terminator)
    }
}

