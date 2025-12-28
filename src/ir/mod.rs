mod ir;
mod lowerer;
mod pretty;

pub use ir::*;
pub use lowerer::{BlockId, TempId, LocalId};
pub use lowerer::lower;
