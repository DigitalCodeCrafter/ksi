use std::collections::HashMap;

use crate::mir::{Block, Const, Instr, LocalId, Operand, RValue, Terminator, passes::Pass};


pub struct ConstPropagation;

impl Pass for ConstPropagation {
    fn name(&self) -> &'static str { "constant-propagation" }

    fn run(&mut self, body: &mut crate::mir::Body) {
        let mut const_map = HashMap::new();
        for block in &mut body.blocks {
            const_map.clear();
            propagate_in_block(&mut const_map, block);
        }
    }
}

fn propagate_in_block(const_map: &mut HashMap<LocalId, Const>, block: &mut Block) {
    for instr in &mut block.instrs {
        prop_in_instr(const_map, instr);
    }
    prop_in_terminator(const_map, &mut block.terminator);
}

fn prop_in_instr(const_map: &mut HashMap<LocalId, Const>, instr: &mut Instr) {
    match instr {
        Instr::Assign(place, RValue::Use(Operand::Const(constant))) => {
            const_map.insert(place.local, constant.clone());
        }

        Instr::Assign(place, rval) => {
            prop_in_rval(const_map, rval);
            const_map.remove(&place.local);
        }

        Instr::Debug(_) => {},
    }
}

fn prop_in_rval(const_map: &HashMap<LocalId, Const>, rval: &mut RValue) {
    match rval {
        RValue::Use(op) => prop_in_operand(const_map, op),
        RValue::Unary(_, op) => prop_in_operand(const_map, op),
        RValue::Binary(_, lhs, rhs) => {
            prop_in_operand(const_map, lhs);
            prop_in_operand(const_map, rhs);
        }
        RValue::Poison => {},
    }
}

fn prop_in_operand(const_map: &HashMap<LocalId, Const>, op: &mut Operand) {
    let local = match op {
        Operand::Copy(place) |
        Operand::Move(place) => &place.local,
        _ => return,
    };

    if let Some(constant) = const_map.get(local) {
        *op = Operand::Const(constant.clone());
    }
}

fn prop_in_terminator(const_map: &HashMap<LocalId, Const>, term: &mut Terminator) {
    match term {
        Terminator::Goto(_) => {},
        Terminator::Return(op) => prop_in_operand(const_map, op),
        Terminator::Unreachable => {},
    }
}


