mod mir;
mod lowerer;
pub mod pretty;
mod verifier;
pub mod passes;

pub use mir::*;
pub use lowerer::{BlockId, LocalId};
pub use lowerer::lower;
pub use verifier::verify_body;
