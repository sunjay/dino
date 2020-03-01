use core::ops::{Add, Sub};

use lazy_static::lazy_static;

use crate::unique::Unique;
use crate::outptr::OutPtr;
use crate::runtime::{alloc_no_ptr, alloc_static};
use crate::dunit::DUnit;

lazy_static! {
    static ref ZERO: Unique<DReal> = alloc_static(DReal(0.0));
}

/// The dino real number type
///
/// A 64-bit floating point number
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct DReal(f64);

impl DReal {
    fn new(value: f64) -> Unique<Self> {
        alloc_no_ptr(DReal(value))
    }

    pub fn zero() -> Unique<Self> {
        *ZERO
    }

    pub fn value(self) -> f64 {
        self.0
    }
}

impl From<i64> for Unique<DReal> {
    fn from(value: i64) -> Self {
        DReal::new(value as f64)
    }
}

impl From<f64> for Unique<DReal> {
    fn from(value: f64) -> Self {
        DReal::new(value)
    }
}

impl<'a> Add for &'a DReal {
    type Output = Unique<DReal>;

    fn add(self, other: Self) -> Unique<DReal> {
        DReal::new(self.0 + other.0)
    }
}

impl<'a> Sub for &'a DReal {
    type Output = Unique<DReal>;

    fn sub(self, other: Self) -> Unique<DReal> {
        DReal::new(self.0 - other.0)
    }
}

/// Creates a new DReal from an integer literal
#[no_mangle]
pub extern fn __dino__DReal_from_int_literal(value: i64, mut out: OutPtr<DReal>) {
    out.write(value.into());
}

/// Creates a new DReal from a real number literal
#[no_mangle]
pub extern fn __dino__DReal_from_real_literal(value: f64, mut out: OutPtr<DReal>) {
    out.write(DReal::new(value));
}

#[no_mangle]
pub extern fn add_real(x: &DReal, y: &DReal, mut out: OutPtr<DReal>) {
    out.write(x + y);
}

#[no_mangle]
pub extern fn sub_real(x: &DReal, y: &DReal, mut out: OutPtr<DReal>) {
    out.write(x - y);
}

#[no_mangle]
pub extern fn print_real(x: &DReal, mut out: OutPtr<DUnit>) {
    unsafe {
        super::printf(b"%g\n\0" as *const u8, x.0);
    }

    out.write(DUnit::new());
}
