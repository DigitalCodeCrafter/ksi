mod kerboscript;


use crate::common::diagnostics::DiagnosticSink;
use crate::ir::ProgramIR;

pub fn generate(program_ir: &ProgramIR, diagnostics: &mut impl DiagnosticSink) -> String {
    kerboscript::KosEmitter::emit_program(&program_ir)
}
