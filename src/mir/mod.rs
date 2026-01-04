mod mir;
mod lowerer;
mod verifier;
pub mod pretty;

pub use mir::*;
pub use lowerer::{BlockId, LocalId};
pub use lowerer::lower;
pub use verifier::verify_ir;
