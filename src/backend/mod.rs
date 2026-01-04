mod kerboscript;


use crate::common::diagnostics::DiagnosticSink;
use crate::mir::Body;

pub fn emit(program_ir: &Body, _diagnostics: &mut impl DiagnosticSink) -> String {
    kerboscript::KosEmitter::emit_program(&program_ir)
}
