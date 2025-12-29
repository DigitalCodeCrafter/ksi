use ksi::{backend, common::diagnostics::sinks::Diagnostics, ir, semantics, syntax};


fn main() {
    let file_path = std::env::args().nth(1).expect("Missing file path");

    let content = std::fs::read_to_string(file_path).unwrap();

    let mut diagnostics = Diagnostics::empty();
    let parsed_ast = syntax::parse(&content, &mut diagnostics);
    let (typed_ast, symbols) = semantics::analyze(parsed_ast, &mut diagnostics);
    let prog_ir = ir::lower(typed_ast, &symbols, &mut diagnostics);

    if diagnostics.has_error() {
        eprintln!("{:#?}", diagnostics.diagnostics);
        std::process::exit(1);
    }

    let out = backend::emit(&prog_ir, &mut diagnostics);

    if diagnostics.has_error() {
        eprintln!("{:#?}", diagnostics.diagnostics);
    }

    println!("{}", out)
}

