use crate::mir::*;

// TODO: REFACTOR THIS 100%

pub struct KosEmitter {
    indent: usize,
    out: String,
}
impl KosEmitter {
    pub fn emit_program(prog: &Body) -> String {
        let mut emitter = KosEmitter {
            indent: 0,
            out: String::new(),
        };

        emitter.emit_function("main", prog);

        
        emitter.emit("main().\n");

        emitter.out
    }

    fn emit_const(&mut self, c: &Const) {
        match c {
            Const::Number(n) => self.out.push_str(&n.to_string()),
            Const::Bool(true) => self.out.push_str("true"),
            Const::Bool(false) => self.out.push_str("false"),
            Const::Unit => self.out.push_str("0"),
        }
    }

    fn emit_operand(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => self.emit_const(c),
            Operand::Move(place) | Operand::Copy(place) => self.emit_place(place),
        }
    }

    fn emit_place(&mut self, p: &Place) {
        self.emit(&format!("l{}", p.local.index()));
    }

    fn emit_rval(&mut self, rval: &RValue) {
        match rval {
            RValue::Use(op) => self.emit_operand(op),

            RValue::Unary(op, val) => {
                let op_str = match op {
                    UnaryOp::Neg => "-",
                };
                self.emit(op_str);
                self.emit_operand(val);
            }

            RValue::Binary(op, lhs, rhs) => {
                let op_str = match op {
                    BinaryOp::Add => " + ",
                    BinaryOp::Sub => " - ",
                    BinaryOp::Mul => " * ",
                    BinaryOp::Div => " / ",
                    BinaryOp::Gt => " > ",
                    BinaryOp::Ge => " >= ",
                    BinaryOp::Lt => " < ",
                    BinaryOp::Le => " <= ",
                    BinaryOp::Eq => " == ",
                    BinaryOp::Ne => " <> ",
                };
                self.emit_operand(lhs);
                self.emit(op_str);
                self.emit_operand(rhs);
            }

            RValue::Poison => {
                todo!("TOOD: Some good way to handle this.")
            }
        }
    }

    fn emit_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::Assign(place, rval) => {
                self.emit_indent();
                self.emit("set ");
                self.emit_place(place);
                self.emit(" to ");
                self.emit_rval(rval);
                self.emit(".\n");
            }
            Instr::Debug(_) => {},
        }
    }

    fn emit_terminator(&mut self, term: &Terminator) {
        self.emit_indent();
        match term {
            Terminator::Goto(_) => todo!("Some good way to handle this too."),

            Terminator::Branch(_, _, _) => todo!("Some good way to handle this... "),

            Terminator::Return(op) => {
                self.emit("return ");
                self.emit_operand(op);
                self.emit(".\n");
            }
            Terminator::Unreachable => {
                self.emit("print \"UNREACHABLE has been reached\".\n");
                self.emit("wait until false.\n");
            }
        }
    }

    fn emit_block(&mut self, id: usize, block: &Block) {
        self.emit_indent();
        self.emit("// block");
        self.emit(&id.to_string());
        self.emit(":\n");

        for instr in &block.instrs {
            self.emit_instr(instr);
        }
        self.emit_terminator(&block.terminator);
    }

    fn emit_function(&mut self, name: &str, func: &Body) {
        self.emit_indent();
        self.emit("function ");
        self.emit(name);
        self.emit(" {\n");

        self.increase_indent();

        for (lidx, _) in func.locals.iter().enumerate() {
            self.emit_indent();
            self.emit("local l");
            self.emit(&lidx.to_string());
            self.emit(" is 0.\n");
        }

        for (bidx, block) in func.blocks.iter().enumerate() {
            self.emit_block(bidx, block);
        }

        self.decrease_indent();
        self.emit("}\n\n");
    }

    fn emit_indent(&mut self) {
        self.out.extend(std::iter::repeat_n(' ', self.indent));
    }

    fn emit(&mut self, str: &str) {
        self.out.push_str(str);
    }

    fn increase_indent(&mut self) {
        self.indent += 4;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 4.min(self.indent);
    }
}
