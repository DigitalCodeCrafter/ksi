mod kerboscript;


use crate::common::diagnostics::DiagnosticSink;
use crate::ir::ProgramIR;

pub fn emit(program_ir: &ProgramIR, _diagnostics: &mut impl DiagnosticSink) -> String {
    kerboscript::KosEmitter::emit_program(&program_ir)
}
