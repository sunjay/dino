use crate::dunit::DUnit;

/// The dino real number type
///
/// A 64-bit floating point number
#[repr(transparent)]
pub struct DReal(f64);

impl From<f64> for DReal {
    fn from(x: f64) -> Self {
        DReal(x)
    }
}

impl DReal {
    pub fn zero() -> Self {
        DReal(0.0)
    }
}

/// Creates a new DReal from an integer literal
#[no_mangle]
pub extern fn __dino__DReal_from_int_literal(value: i64) -> DReal {
    DReal(value as f64)
}

/// Creates a new DReal from a real number literal
#[no_mangle]
pub extern fn __dino__DReal_from_real_literal(value: f64) -> DReal {
    DReal(value)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn add_real(x: DReal, y: DReal) -> DReal {
    DReal(x.0 + y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn sub_real(x: DReal, y: DReal) -> DReal {
    DReal(x.0 - y.0)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_real(x: DReal) -> DUnit {
    unsafe {
        super::printf(b"%g\n\0" as *const u8, x.0);
    }

    DUnit::default()
}
