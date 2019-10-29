use lazy_static::lazy_static;

use crate::unique::Unique;
use crate::alloc::{alloc_no_ptr, alloc_static};
use crate::dunit::DUnit;

lazy_static! {
    static ref ZERO: Unique<DReal> = unsafe { alloc_static(DReal(0.0)) };
}

/// The dino real number type
///
/// A 64-bit floating point number
#[repr(transparent)]
pub struct DReal(f64);

impl DReal {
    fn new(value: f64) -> Unique<Self> {
        unsafe {
            alloc_no_ptr(DReal(value))
        }
    }

    pub fn zero() -> Unique<Self> {
        *ZERO
    }
}

/// Creates a new DReal from an integer literal
#[no_mangle]
pub extern fn __dino__DReal_from_int_literal(value: i64) -> Unique<DReal> {
    DReal::new(value as f64)
}

/// Creates a new DReal from a real number literal
#[no_mangle]
pub extern fn __dino__DReal_from_real_literal(value: f64) -> Unique<DReal> {
    DReal::new(value)
}

#[no_mangle]
pub extern fn add_real(x: &DReal, y: &DReal) -> Unique<DReal> {
    DReal::new(x.0 + y.0)
}

#[no_mangle]
pub extern fn sub_real(x: &DReal, y: &DReal) -> Unique<DReal> {
    DReal::new(x.0 - y.0)
}

#[no_mangle]
pub extern fn print_real(x: &DReal) -> Unique<DUnit> {
    unsafe {
        super::printf(b"%g\n\0" as *const u8, x.0);
    }

    DUnit::new()
}
