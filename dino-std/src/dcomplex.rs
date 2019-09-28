use crate::dreal::{self, DReal};

/// The dino complex number type
///
/// Two 64-bit floating point numbers
#[repr(C)]
pub struct DComplex {
    real: DReal,
    imag: DReal,
}

/// Creates a new DComplex from an integer literal
#[no_mangle]
pub extern fn __dino__DComplex_from_int_literal(value: i64) -> DComplex {
    DComplex {
        real: dreal::__dino__DReal_from_int_literal(value),
        imag: DReal::zero(),
    }
}

/// Creates a new DComplex from a real number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_real_literal(value: f64) -> DComplex {
    DComplex {
        real: dreal::__dino__DReal_from_real_literal(value),
        imag: DReal::zero(),
    }
}

/// Creates a new DComplex from a complex number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_complex_literal(value: f64) -> DComplex {
    DComplex {
        real: DReal::zero(),
        imag: dreal::__dino__DReal_from_real_literal(value),
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn add_complex(x: DComplex, y: DComplex) -> DComplex {
    DComplex {
        real: dreal::add_real(x.real, y.real),
        imag: dreal::add_real(x.imag, y.imag),
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn add_real_complex(x: DReal, y: DComplex) -> DComplex {
    DComplex {
        real: dreal::add_real(x, y.real),
        imag: y.imag,
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn add_complex_real(x: DComplex, y: DReal) -> DComplex {
    DComplex {
        real: dreal::add_real(x.real, y),
        imag: x.imag,
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn sub_complex(x: DComplex, y: DComplex) -> DComplex {
    DComplex {
        real: dreal::sub_real(x.real, y.real),
        imag: dreal::sub_real(x.imag, y.imag),
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn sub_real_complex(x: DReal, y: DComplex) -> DComplex {
    DComplex {
        real: dreal::sub_real(x, y.real),
        imag: y.imag,
    }
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn sub_complex_real(x: DComplex, y: DReal) -> DComplex {
    DComplex {
        real: dreal::sub_real(x.real, y),
        imag: x.imag,
    }
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_complex(x: DComplex) {
    unsafe {
        super::printf(b"%g + %gi\n\0" as *const u8, x.real, x.imag);
    }
}
