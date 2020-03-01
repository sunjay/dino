#![no_std]

mod unique;
mod runtime;
mod outptr;

mod dunit;
mod dbool;
mod dint;
mod dreal;
mod dcomplex;
mod dbstr;

pub use dunit::*;
pub use dbool::*;
pub use dint::*;
pub use dreal::*;
pub use dcomplex::*;
pub use dbstr::*;

// Needed to define #[panic_handler]
#[allow(unused_imports)]
use panic_halt;

// Needed for everything to link correctly
// See: https://github.com/rust-lang/rust/blob/152527f443c51517bb867befa93809ce5b9b1cd1/src/libpanic_abort/lib.rs#L65-L90
#[no_mangle]
pub extern fn rust_eh_personality() {}

// A list of C functions/items that are being imported
extern {
    pub static mut stdin: *mut libc::FILE;
    pub fn printf(format: *const u8, ...) -> i32;
}
