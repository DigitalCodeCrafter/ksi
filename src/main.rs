use ksi::{codegen::generate, common::diagnostics::Diagnostics, ir::lower, semantics::analyze, syntax::parse};

fn main() {
    let file_path = std::env::args().nth(1).expect("Missing file path");

    let content = std::fs::read_to_string(file_path).unwrap();

    let mut diagnostics = Diagnostics::empty();
    let parsed_ast = parse(&content, &mut diagnostics);
    let (typed_ast, symbols) = analyze(parsed_ast, &mut diagnostics);
    let ir = lower(typed_ast, &symbols, &mut diagnostics);
    let out = generate(&ir, &mut diagnostics);

    println!("{}", out)
}

