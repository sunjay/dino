#![no_std]

mod dint;
mod dreal;

pub use dint::*;
pub use dreal::*;

// Needed to define #[panic_handler]
#[allow(unused_imports)]
use panic_halt;

// A list of C functions that are being imported
extern {
    pub fn printf(format: *const u8, ...) -> i32;
}
