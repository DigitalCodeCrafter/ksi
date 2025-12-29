use crate::common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
    pub is_primary: bool,
}
impl Label {
    pub fn primary(span: Span) -> Self {
        Self { span, message: None, is_primary: true }
    }

    pub fn secondary(span: Span, msg: impl Into<String>) -> Self {
        Self { span, message: Some(msg.into()), is_primary: false }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}
impl Diagnostic {
    pub fn error(msg: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: msg.into(),
            span: None,
            labels: Vec::new(),
            notes: Vec::new()
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn note(mut self, msg: impl Into<String>) -> Self {
        self.notes.push(msg.into());
        self
    }
}

pub trait DiagnosticSink {
    fn emit(&mut self, diagnostic: Diagnostic);
}

pub mod sinks {
    use super::{Diagnostic, DiagnosticSink, Severity};
    pub struct Diagnostics {
        pub diagnostics: Vec<Diagnostic>,
    }
    impl DiagnosticSink for Diagnostics {
        fn emit(&mut self, diagnostic: Diagnostic) {
            self.diagnostics.push(diagnostic);
        }
    }
    impl Diagnostics {
        pub fn empty() -> Self {
            Self { diagnostics: Vec::new() }
        }
        
        pub fn has_error(&self) -> bool {
            self.diagnostics.iter().any(|d| d.severity == Severity::Error)
        }
    }

    pub struct IgnoreErrors;
    impl DiagnosticSink for IgnoreErrors {
        fn emit(&mut self, _: Diagnostic) {}
    }

    pub struct AssertErrors;
    impl DiagnosticSink for AssertErrors {
        fn emit(&mut self, diagnostic: Diagnostic) {
            if diagnostic.severity == Severity::Error {
                panic!("[AssertErrors] Error: {:?}", diagnostic)
            }
        }
    }
}