use std::collections::HashMap;

use crate::mir::{Block, Instr, LocalId, Operand, Place, RValue, Terminator, passes::Pass};


pub struct CopyPropagation;
impl Pass for CopyPropagation {
    fn name(&self) -> &'static str { "copy-propagation" }

    fn run(&mut self, body: &mut crate::mir::Body) {
        let mut copy_map = HashMap::new();
        for block in &mut body.blocks {
            copy_map.clear();
            propagate_in_block(&mut copy_map, block);
        }
    }
}

fn propagate_in_block(copy_map: &mut HashMap<LocalId, LocalId>, block: &mut Block) {
    for instr in &mut block.instrs {
        prop_in_instr(copy_map, instr);
    }
    prop_in_terminator(copy_map, &mut block.terminator);
}

fn prop_in_instr(copy_map: &mut HashMap<LocalId, LocalId>, instr: &mut Instr) {
    match instr {
        Instr::Assign(place, RValue::Use(Operand::Copy(src))) => {
            copy_map.insert(place.local, src.local);
        }

        Instr::Assign(place, rval) => {
            prop_in_rval(copy_map, rval);
            copy_map.remove(&place.local);
        }

        Instr::Debug(_) => {},
    }
}

fn prop_in_rval(copy_map: &HashMap<LocalId, LocalId>, rval: &mut RValue) {
    match rval {
        RValue::Use(op) => prop_in_operand(copy_map, op),
        RValue::Binary(_, lhs, rhs) => {
            prop_in_operand(copy_map, lhs);
            prop_in_operand(copy_map, rhs);
        }
        RValue::Poison => {},
    }
}

fn prop_in_operand(copy_map: &HashMap<LocalId, LocalId>, op: &mut Operand) {
    let local = match op {
        Operand::Copy(place) |
        Operand::Move(place) => &place.local,
        _ => return,
    };

    if let Some(src) = copy_map.get(local) {
        *op = Operand::Copy(Place { local: *src, projection: Vec::new() });
    }
}

fn prop_in_terminator(copy_map: &HashMap<LocalId, LocalId>, term: &mut Terminator) {
    match term {
        Terminator::Goto(_) => {},
        Terminator::Return(op) => prop_in_operand(copy_map, op),
        Terminator::Unreachable => {},
    }
}
