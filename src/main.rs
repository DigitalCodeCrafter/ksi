use ksi::{backend, common::diagnostics::{Diagnostic, Severity, sinks::Diagnostics}, mir::{self, passes::{ConstPropagation, CopyPropagation, DeadLocalElim, Pass, run_passes}, pretty}, semantics, syntax};


fn main() -> Result<(), ()> {
    let file_path = std::env::args().nth(1).expect("Missing file path");

    let content = std::fs::read_to_string(file_path).unwrap();
    let line_starts = line_indices(&content);

    let mut diagnostics = Diagnostics::empty();
    let parsed_ast = syntax::parse(&content, &mut diagnostics);
    let (typed_ast, symbols) = semantics::analyze(parsed_ast, &mut diagnostics);
    if diagnostics.has_error() {
        render_all(&content, &line_starts, &diagnostics.diagnostics);
        return Err(());
    }

    let mut prog_ir = mir::lower(typed_ast, &symbols, &mut diagnostics);

    let mut passes: Vec<Box<dyn Pass>> = vec![
        Box::new(CopyPropagation),
        Box::new(ConstPropagation),
        Box::new(DeadLocalElim)
    ];

    run_passes(&mut prog_ir, &mut passes);

    let out = backend::emit(&prog_ir, &mut diagnostics);

    render_all(&content, &line_starts, &diagnostics.diagnostics);

    let pretty_ir = pretty::format_body(&prog_ir, "main");
    for line in pretty_ir.lines() {
        println!("// {}", line);
    }

    println!("\n\n{}", out);
    Ok(()) 
}

fn line_indices(src: &str) -> Vec<usize> {
    let mut line_starts = vec![0];
    for (i, b) in src.bytes().enumerate() {
        if b == b'\n' {
            line_starts.push(i + 1);
        }
    }
    line_starts
}

fn line_col(src: &str, line_starts: &[usize], pos: usize) -> (usize, usize) {
    let line = match line_starts.binary_search(&pos) {
        Ok(i) => i,
        Err(i) => i - 1,
    };

    let col = src[line_starts[line]..pos].chars().count();
    (line + 1, col + 1)
}

fn get_line<'a>(src: &'a str, line_idx: usize, line_starts: &[usize]) -> &'a str {
    let start = line_starts[line_idx];
    let end = if line_idx + 1 < line_starts.len() {
        line_starts[line_idx + 1] - 1
    } else {
        src.len()
    };
    &src[start..end]
}

fn print_marker(line: &str, col: usize, marker: char, message: Option<&str>) {
    let mut pos = 0;
    for c in line.chars().take(col - 1) {
        if c == '\t' { pos += 4 }
        else { pos += 1 }
    }
    let padding = " ".repeat(pos);
    if let Some(msg) = message {
        print!("{padding}{} {}\n", marker, msg);
    } else {
        print!("{padding}{}\n", marker)
    }
}

fn render_diagnostic(diag: &Diagnostic, line_starts: &[usize], src: &str) {
    let severity = match diag.severity {
        Severity::Note => "note",
        Severity::Warning => "warning",
        Severity::Error => "error"
    };
    print!("{}: {}\n", severity, diag.message);

    if let Some(span) = diag.span {
        let loc = line_col(src, line_starts, span.start);
        print!("  --> input:{}:{}\n", loc.0, loc.1);

        let source_line = get_line(src, loc.0 - 1, line_starts);
        print!("   |\n{:>2} | {}\n", loc.0, source_line);
    }

    for label in &diag.labels {
        let loc = line_col(src, line_starts, label.span.start);
        let source_line = get_line(src, loc.0 - 1, line_starts);
        let marker = if label.is_primary { '^' } else { '-' };
        print!("     ");
        print_marker(source_line, loc.1, marker, label.message.as_deref());
    }

    for note in &diag.notes {
        print!("   = {}\n", note);
    }

    println!();
}

fn render_all(src: &str, line_starts: &[usize], diags: &[Diagnostic]) {
    let mut error_count = 0;
    for diag in diags {
        if diag.severity == Severity::Error { error_count += 1 }
        render_diagnostic(diag, line_starts, src);
    }
    if error_count > 0 {
        println!("error: compilation failed due to {} previous errors", error_count)
    }
}
