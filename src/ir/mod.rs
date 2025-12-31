mod ir;
mod lowerer;
pub mod pretty;

pub use ir::*;
pub use lowerer::{BlockId, TempId, LocalId};
pub use lowerer::lower;
