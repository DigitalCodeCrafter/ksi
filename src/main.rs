use ksi::{codegen::KosEmitter, lexer::TokenStream, lowerer::FunctionIRBuilder, parser::Parser, resolver::Resolver, typechecker::TypeChecker};

fn main() {
    let file_path = std::env::args().nth(1).expect("Missing file path");

    let content = std::fs::read_to_string(file_path).unwrap();

    let tokens = TokenStream::new(&content);
    let ast = Parser::new(tokens).parse_program();

    let mut resolver = Resolver::new();

    let resolved_ast = resolver.resolve_program(ast);
    let mut symbols = resolver.into_table();

    let typed_ast = TypeChecker::new(&mut symbols).type_check(resolved_ast);

    let mut builder = FunctionIRBuilder::new(&symbols);
    builder.lower_program(typed_ast);
    let ir = ksi::ir::ProgramIR { functions: vec![builder.finish()] };

    println!("{}", KosEmitter::emit_program(&ir))
}
