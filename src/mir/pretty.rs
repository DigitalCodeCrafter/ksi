use crate::mir::mir::*;

pub fn format_const(c: &Const) -> String {
    match c {
        Const::Number(n) => n.to_string(),
        Const::Bool(b) => b.to_string(),
        Const::Unit => "unit".to_string(),
    }
}

pub fn format_operand(o: &Operand) -> String {
    match o {
        Operand::Copy(p) => format!("copy {}", format_place(p)),
        Operand::Move(p) => format!("{}", format_place(p)),
        Operand::Const(c) => format!("const {}", format_const(c)),
    }
}

pub fn format_place(p: &Place) -> String {
    let s = format!("l{}", p.local.index());
    s
}

pub fn format_rval(rv: &RValue) -> String {
    match rv {
        RValue::Use(op) => format_operand(op),

        RValue::Binary(op, lhs, rhs) => {
            let op_str = match op {
                BinaryOp::Add   => "+",
                BinaryOp::Sub   => "-",
                BinaryOp::Div   => "/",
                BinaryOp::Mul   => "*",
                BinaryOp::Gt    => ">",
                BinaryOp::Ge    => ">=",
                BinaryOp::Lt    => "<",
                BinaryOp::Le    => "<=",
                BinaryOp::Eq    => "==",
                BinaryOp::Ne    => "!=",
            };
            format!("{} {} {}",
                format_operand(lhs),
                op_str,
                format_operand(rhs)
            )
        }
        RValue::Unary(op, val) => {
            let op_str = match op {
                UnaryOp::Neg    => "- "
            };
            format!("{}{}",
                op_str,
                format_operand(val)
            )
        }
        RValue::Poison => "posion".to_string()
    }
}

pub fn format_debug_info(info: &DebugInfo) -> String {
    let note = match &info.kind {
        DebugKind::EnterScope => "enter scope".to_string(),
        DebugKind::ExitScope => "exit scope".to_string(),
        DebugKind::DeclareLocal { local, name } => format!("{} := l{}", name, local.index()),
    };
    format!("[{}..{}] debug: {}", info.span.start, info.span.end, note)
}

pub fn format_instr(i: &Instr) -> String {
    match i {
        Instr::Assign(place, rvalue) => format!("{} = {}", format_place(place), format_rval(rvalue)),
        Instr::Debug(info) => format_debug_info(info),
    }
}

pub fn format_terminator(t: &Terminator) -> String {
    match t {
        Terminator::Goto(b) => format!("goto block{}", b.index()),
        Terminator::Branch(op, then_block, else_block) => format!("branch {} [then -> block{}, else -> block{}]", format_operand(op), then_block.index(), else_block.index()),
        Terminator::Return(op) => format!("return {}", format_operand(op)),
        Terminator::Unreachable => "unreachable".to_string(),
    }
}

pub fn format_block(b: &Block, id: usize) -> String {
    let mut s = String::new();
    s.push_str(&format!("block{}:\n", id));
    for instr in &b.instrs {
        s.push_str(&format!("  {}\n", format_instr(instr)));
    }
    s.push_str(&format!("  {}\n", format_terminator(&b.terminator)));
    s
}

pub fn format_body(b: &Body, name: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!("fn {}:\n", name));
    s.push_str("locals:\n");
    for (i, local) in b.locals.iter().enumerate() {
        s.push_str(&format!("  l{}: {:?}\n", i, local.ty));
    }
    for (i, block) in b.blocks.iter().enumerate() {
        s.push_str(&format_block(block, i))
    }
    s
}
