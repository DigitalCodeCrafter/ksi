use crate::ir::ir::{BinaryOp, Block, FunctionIR, Instr, Place, Terminator, Value, Const};

pub fn format_const(c: &Const) -> String {
    match c {
        Const::Number(n) => n.to_string(),
        Const::Unit => "unit".to_string(),
    }
}

pub fn format_value(v: &Value) -> String {
    match v {
        Value::Const(c) => format!("const {}", format_const(c)),
        Value::Temp(id) => format!("t{}", t.index()),
    }
}

pub fn format_place(p: &Place) -> String {
    match p {
        Place::Local(l) => format!("l{}", l.index()),
    }
}

pub fn format_instr(i: &Instr) -> String {
    match i {
        Instr::LoadConst { dst, value } => {
            format!("t{} = const {}", dst.index(), format_const(value))
        }
        Instr::Binary { dst, op, lhs, rhs } => {
            let op_str = match op {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
            };
            format!(f, "t{} = {} {} {}", dst.index(),
                format_value(lhs),
                op_str,
                format_value(rhs)
            )
        }
        Instr::Store { place, value } => {
            format!("store {}, {}", format_place(place), format_value(value))
        }
        Instr::Load { dst, place } => {
            format!("t{} = load {}", dst.index(), format_place(place))
        }
        Instr::Poision { dst } => {
            if let Some(dst) = dst {
                format!("poison t{}", dst.index())
            } else {
                "posion".to_string()
            }
        }
    }
}

pub fn format_terminator(t: &Terminator) -> String {
    match t {
        Terminator::Goto(b) => format!("goto block{}", b.index()),
        Terminator::Return(v) => format!("return {}", format_value(v)),
        Terminator::Unreachable => "unreachable".to_string(),
    }
}

pub fn format_block(b: &Block, id: usize) -> String {
    let mut s = String::new();
    s.push_str(&format!("block{}:\n", id));
    for instr in &b.instrs {
        s.push_str(&format!("  {}\n", format_instr(instr)));
    }
    s.push_str(format!("  {}\n", format_terminator(&b.terminator)));
    s
}

pub fn format_function(f: &FunctionIR, name: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!("fn {}:\n", name));
    s.push_str("locals:\n");
    for (i, local) in f.locals.iter().enumerate() {
        s.push_str(&format!("  l{}: {:?}\n", i, local.ty));
    }
    for (i, block) in f.blocks.iter().enumerate() {
        s.push_str(&format_block(block, i))
    }
    s
}
