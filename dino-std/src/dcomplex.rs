use crate::unique::Unique;
use crate::alloc::alloc_struct;
use crate::dunit::DUnit;
use crate::dreal::{self, DReal};

/// The dino complex number type
///
/// Two 64-bit floating point numbers
#[repr(C)]
pub struct DComplex {
    real: Unique<DReal>,
    imag: Unique<DReal>,
}

/// Creates a new DComplex from an integer literal
#[no_mangle]
pub extern fn __dino__DComplex_from_int_literal(value: i64) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::__dino__DReal_from_int_literal(value),
        imag: DReal::zero(),
    }) }
}

/// Creates a new DComplex from a real number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_real_literal(value: f64) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::__dino__DReal_from_real_literal(value),
        imag: DReal::zero(),
    }) }
}

/// Creates a new DComplex from a complex number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_complex_literal(value: f64) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: DReal::zero(),
        imag: dreal::__dino__DReal_from_real_literal(value),
    }) }
}

#[no_mangle]
pub extern fn add_complex(x: &DComplex, y: &DComplex) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::add_real(x.real.as_ref(), y.real.as_ref()),
        imag: dreal::add_real(x.imag.as_ref(), y.imag.as_ref()),
    }) }
}

#[no_mangle]
pub extern fn add_real_complex(x: &DReal, y: &DComplex) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::add_real(x, y.real.as_ref()),
        imag: y.imag,
    }) }
}

#[no_mangle]
pub extern fn add_complex_real(x: &DComplex, y: &DReal) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::add_real(x.real.as_ref(), y),
        imag: x.imag,
    }) }
}

#[no_mangle]
pub extern fn sub_complex(x: &DComplex, y: &DComplex) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::sub_real(x.real.as_ref(), y.real.as_ref()),
        imag: dreal::sub_real(x.imag.as_ref(), y.imag.as_ref()),
    }) }
}

#[no_mangle]
pub extern fn sub_real_complex(x: &DReal, y: &DComplex) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::sub_real(x, y.real.as_ref()),
        imag: y.imag,
    }) }
}

#[no_mangle]
pub extern fn sub_complex_real(x: &DComplex, y: &DReal) -> Unique<DComplex> {
    unsafe { alloc_struct(DComplex {
        real: dreal::sub_real(x.real.as_ref(), y),
        imag: x.imag,
    }) }
}

#[no_mangle]
pub unsafe extern fn print_complex(x: &DComplex) -> Unique<DUnit> {
    let real = x.real.as_ref().value();
    let imag = x.imag.as_ref().value();
    super::printf(b"%g + %gi\n\0" as *const u8, real, imag);

    DUnit::new()
}
