use std::collections::HashSet;
use crate::mir::{Block, DebugKind, Instr, LocalId, Operand, Place, RValue, Terminator, passes::Pass};


pub struct DeadLocalElim;

impl Pass for DeadLocalElim {
    fn name(&self) -> &'static str { "dead-local-elimination" }

    fn run(&mut self, body: &mut crate::mir::Body) {
        let mut used = HashSet::new();
        for block in &mut body.blocks {
            find_live_in_block(&mut used, block);
        }
        
        let mut remap = vec![None; body.locals.len()];
        let mut new_locals = Vec::with_capacity(used.len());
        for (old_idx, local) in body.locals.drain(..).enumerate() {
            let old_id = LocalId(old_idx as u32);
            if used.contains(&old_id) {
                let new_idx = new_locals.len();
                new_locals.push(local);
                remap[old_idx] = Some(LocalId(new_idx as u32));
            }
        }
        body.locals = new_locals;

        for block in &mut body.blocks {
            remap_in_block(&remap, block);
        } 
    }
}

fn find_live_in_block(used: &mut HashSet<LocalId>, block: &Block) {
    for instr in &block.instrs {
        match instr {
            Instr::Assign(_, rval) => {
                find_live_in_rval(used, rval);
            }
            Instr::Debug(info) => {
                if let DebugKind::DeclareLocal { local, .. } = info.kind {
                    used.insert(local);
                }
            }
        }
    }
    find_live_in_term(used, &block.terminator);
}

fn find_live_in_rval(used: &mut HashSet<LocalId>, rval: &RValue) {
    match rval {
        RValue::Use(op) => find_live_in_op(used, op),
        RValue::Unary(_, op) => find_live_in_op(used, op),
        RValue::Binary(_, lhs, rhs) => {
            find_live_in_op(used, lhs);
            find_live_in_op(used, rhs);
        }
        RValue::Poison => {},
    }
}

fn find_live_in_term(used: &mut HashSet<LocalId>, term: &Terminator) {
    match term {
        Terminator::Goto(_) => {},
        Terminator::Return(op) => find_live_in_op(used, op),
        Terminator::Branch(op, _, _) => find_live_in_op(used, op),
        Terminator::Unreachable => {},
    }
}

fn find_live_in_op(used: &mut HashSet<LocalId>, op: &Operand) {
    match op {
        Operand::Copy(place) |
        Operand::Move(place) => {
            used.insert(place.local);
        }
        Operand::Const(_) => {},
    }
}

fn remap_in_block(remap: &[Option<LocalId>], block: &mut Block) {
    block.instrs.retain_mut(|instr| {
        match instr {
            Instr::Assign(place, rval) => {
                remap_in_rval(remap, rval);
                remap_place(remap, place)
            }
            Instr::Debug(info) => {
                if let DebugKind::DeclareLocal { local, .. } = &mut info.kind {
                    if let Some(new) = remap[local.index() as usize] {
                        *local = new;
                    }
                }
                true
            }
        }
    });
    remap_in_term(remap, &mut block.terminator);
}

fn remap_in_rval(remap: &[Option<LocalId>], rval: &mut RValue) {
    match rval {
        RValue::Use(op) => remap_in_op(remap, op),
        RValue::Unary(_, op) => remap_in_op(remap, op),
        RValue::Binary(_, lhs, rhs) => {
            remap_in_op(remap, lhs);
            remap_in_op(remap, rhs);
        }
        RValue::Poison => {},
    }
}

fn remap_in_term(remap: &[Option<LocalId>], term: &mut Terminator) {
    match term {
        Terminator::Goto(_) => {},
        Terminator::Return(op) => remap_in_op(remap, op),
        Terminator::Branch(op, _, _) => remap_in_op(remap, op),
        Terminator::Unreachable => {},
    }
}

fn remap_in_op(remap: &[Option<LocalId>], op: &mut Operand) {
    match op {
        Operand::Copy(place) |
        Operand::Move(place) => {
            remap_place(remap, place);
        }
        Operand::Const(_) => {},
    }
}

fn remap_place(remap: &[Option<LocalId>], place: &mut Place) -> bool {
    if let Some(new) = remap[place.local.index() as usize] {
        place.local = new;
        return true;
    }
    false
}
