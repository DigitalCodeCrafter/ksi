use std::collections::HashMap;
use crate::ir::*;

// TODO: REFACTOR THIS 100%

pub struct KosEmitter<'a> {
    indent: usize,
    out: String,
    temp_exprs: HashMap<TempId, &'a Instr>
}
impl<'e> KosEmitter<'e> {
    pub fn emit_program(prog: &ProgramIR) -> String {
        let mut emitter = KosEmitter {
            indent: 0,
            out: String::new(),
            temp_exprs: HashMap::new(),
        };

        for (fidx, func) in prog.functions.iter().enumerate() {
            emitter.emit_function(&format!("f{}", fidx), func);
        }

        if !prog.functions.is_empty() {
            emitter.emitln("f0().");
        }

        emitter.out
    }

    fn emit_local(&mut self, id: LocalId) {
        self.out.push_str(&format!("l{}", id.index()))
    }

    fn emit_const(&mut self, c: &Const) {
        match c {
            Const::Number(n) => self.out.push_str(&n.to_string()),
            Const::Unit => self.out.push_str("0"),
        }
    }

    fn emit_value(&mut self, v: &Value) {
        match v {
            Value::Const(c) => self.emit_const(c),
            Value::Temp(id) => {
                let instr = *self.temp_exprs.get(id).unwrap();
                self.emit_instr(instr);
            },
        }
    }

    fn emit_place(&mut self, p: &Place) {
        match p {
            Place::Local(id) => self.emit_local(*id),
        }
    }

    fn emit_instr(&mut self, instr: &'e Instr) {
        match instr {
            Instr::LoadConst { dst, value } => {
                if self.temp_exprs.insert(*dst, instr).is_some() {
                    self.emit_const(value);
                    // self.temp_exprs.remove(dst);
                }
            }
            Instr::Binary { dst, op, lhs, rhs } => {
                if self.temp_exprs.insert(*dst, instr).is_some() {
                    let op_str = match op {
                        BinaryOp::Add => "+",
                        BinaryOp::Sub => "-",
                        BinaryOp::Mul => "*",
                        BinaryOp::Div => "/",
                    };
                    self.emit_value(lhs);
                    self.emit(" ");
                    self.emit(op_str);
                    self.emit(" ");
                    self.emit_value(rhs);
                    // self.temp_exprs.remove(dst);
                }
            }
            Instr::Store { place, value } => {
                self.emit_indent();
                self.emit("set ");
                self.emit_place(place);
                self.emit(" to ");
                self.emit_value(value);
                self.emitln(".");
            }
            Instr::Load { dst, place} => {
                if self.temp_exprs.insert(*dst, instr).is_some() {
                    self.emit_place(place);
                    // self.temp_exprs.remove(dst);
                }
            }
            Instr::Poision { .. } => {
                todo!("TOOD: Some good way to handle this.")
            }
        }
    }

    fn emit_terminator(&mut self, term: &Terminator) {
        self.emit_indent();
        match term {
            Terminator::Goto(_) => todo!("Some good way to handle this too."),
            Terminator::Return(v) => {
                self.emit("return ");
                self.emit_value(v);
                self.emitln(".");
            }
            Terminator::Unreachable => {
                self.emitln("print \"UNREACHABLE has been reached\".");
                self.emitln("shutdown.");
            }
        }
    }

    fn emit_block(&mut self, id: usize, block: &'e Block) {
        self.emit_indent();
        self.emit("// block");
        self.emit(&id.to_string());
        self.emitln(":");

        for instr in &block.instrs {
            self.emit_instr(instr);
        }
        self.emit_terminator(&block.terminator);
    }

    fn emit_function(&mut self, name: &str, func: &'e FunctionIR) {
        self.emit_indent();
        self.emit("function ");
        self.emit(name);
        self.emitln(" {");

        self.increase_indent();

        for (lidx, _) in func.locals.iter().enumerate() {
            self.emit_indent();
            self.emit("local l");
            self.emit(&lidx.to_string());
            self.emitln(" is 0.");
        }

        for (bidx, block) in func.blocks.iter().enumerate() {
            self.emit_block(bidx, block);
        }

        self.decrease_indent();
        self.emitln("}\n");
    }

    fn emit_indent(&mut self) {
        self.out.extend(std::iter::repeat_n(' ', self.indent));
    }

    fn emit(&mut self, str: &str) {
        self.out.push_str(str);
    }

    fn emitln(&mut self, str: &str) {
        self.out.push_str(str);
        self.out.push('\n');
    }

    fn increase_indent(&mut self) {
        self.indent += 4;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 4.min(self.indent);
    }
}
