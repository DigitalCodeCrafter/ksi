use crate::mir::{BinaryOp, Block, Body, Const, Instr, Operand, RValue, Terminator, UnaryOp, verify_body};


pub fn interpret_program<'a>(body: &'a Body) -> Const {
    let mut env = vec![Const::Unit; body.locals.len()];
    verify_body(body).expect("Invald MIR");

    let mut next_block = 0;
    loop {
        interpret_block(&body.blocks[next_block], &mut env);
        match &body.blocks[next_block].terminator {
            Terminator::Goto(bid) => next_block = bid.index() as usize,
            Terminator::Branch(op, then_block, else_block) => {
                let val = eval_operand(op, &env);
                match val {
                    Const::Bool(true) => next_block = then_block.index() as usize,
                    Const::Bool(false) => next_block = else_block.index() as usize,
                    _ => panic!("Invalid MIR"),
                }
            }
            Terminator::Return(op) => break eval_operand(op, &env),
            Terminator::Unreachable => panic!("Invalid MIR"),
        }
    }
}

fn interpret_block(block: &Block, env: &mut [Const]) {
    for instr in &block.instrs {
        match instr {
            Instr::Assign(place, rval) => {
                env[place.local.index() as usize] = eval_rval(rval, env);
            }
            Instr::Debug(_) => {},
        }
    }
}

fn eval_rval(rval: &RValue, env: &[Const]) -> Const {
    match rval {
        RValue::Use(op) => eval_operand(op, env),
        RValue::Unary(op, val) => {
            let val = eval_operand(val, env);
            match (op, val) {
                (UnaryOp::Neg, Const::Number(n)) => Const::Number(-n),
                _ => panic!("Invalid MIR")
            }
        }
        RValue::Binary(op, lhs, rhs) => {
            let left = eval_operand(lhs, env);
            let right = eval_operand(rhs, env);
            match (op, left, right) {
                (BinaryOp::Add, Const::Number(l), Const::Number(r)) => Const::Number(l + r),
                (BinaryOp::Sub, Const::Number(l), Const::Number(r)) => Const::Number(l - r),
                (BinaryOp::Div, Const::Number(l), Const::Number(r)) => Const::Number(l / r),
                (BinaryOp::Mul, Const::Number(l), Const::Number(r)) => Const::Number(l * r),
                _ => panic!("Invalid MIR")
            }
        }
        RValue::Poison => panic!("Poisoned value"),
    }
}

fn eval_operand(op: &Operand, env: &[Const]) -> Const {
    match op {
        Operand::Const(c) => c.clone(),
        Operand::Copy(place) |
        Operand::Move(place) => env[place.local.index() as usize].clone(),
    }
}
