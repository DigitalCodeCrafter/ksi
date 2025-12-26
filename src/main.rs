fn main() {
    let file_path = std::env::args().nth(1).expect("Missing file path");

    let content = std::fs::read_to_string(file_path).unwrap();

    let (vars, errors) = ksi::interpreter::interpret_program(&content);

    println!("{:?}", vars);
    eprintln!("{:?}", errors);
}
