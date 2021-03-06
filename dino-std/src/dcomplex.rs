use crate::unique::Unique;
use crate::outptr::OutPtr;
use crate::runtime::alloc_struct;
use crate::dunit::DUnit;
use crate::dreal::DReal;

/// The dino complex number type
///
/// Two 64-bit floating point numbers
#[repr(C)]
pub struct DComplex {
    real: Unique<DReal>,
    imag: Unique<DReal>,
}

impl DComplex {
    pub fn real(&self) -> &DReal {
        // This is safe assuming that the memory is well aligned
        unsafe {
            self.real.as_ref()
        }
    }

    pub fn imag(&self) -> &DReal {
        // This is safe assuming that the memory is well aligned
        unsafe {
            self.imag.as_ref()
        }
    }
}

/// Creates a new DComplex from an integer literal
#[no_mangle]
pub extern fn __dino__DComplex_from_int_literal(value: i64, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: value.into(),
        imag: DReal::zero(),
    }));
}

/// Creates a new DComplex from a real number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_real_literal(value: f64, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: value.into(),
        imag: DReal::zero(),
    }));
}

/// Creates a new DComplex from a complex number literal
#[no_mangle]
pub extern fn __dino__DComplex_from_complex_literal(value: f64, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: DReal::zero(),
        imag: value.into(),
    }));
}

#[no_mangle]
pub extern fn add_complex(x: &DComplex, y: &DComplex, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x.real() + y.real(),
        imag: x.imag() + y.imag(),
    }));
}

#[no_mangle]
pub extern fn add_real_complex(x: &DReal, y: &DComplex, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x + y.real(),
        imag: y.imag,
    }));
}

#[no_mangle]
pub extern fn add_complex_real(x: &DComplex, y: &DReal, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x.real() + y,
        imag: x.imag,
    }));
}

#[no_mangle]
pub extern fn sub_complex(x: &DComplex, y: &DComplex, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x.real() - y.real(),
        imag: x.imag() - y.imag(),
    }));
}

#[no_mangle]
pub extern fn sub_real_complex(x: &DReal, y: &DComplex, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x - y.real(),
        imag: y.imag,
    }));
}

#[no_mangle]
pub extern fn sub_complex_real(x: &DComplex, y: &DReal, mut out: OutPtr<DComplex>) {
    out.write(alloc_struct(DComplex {
        real: x.real() - y,
        imag: x.imag,
    }));
}

#[no_mangle]
pub extern fn print_complex(x: &DComplex, mut out: OutPtr<DUnit>) {
    let real = x.real().value();
    let imag = x.imag().value();

    unsafe {
        super::printf(b"%g + %gi\n\0" as *const u8, real, imag);
    }

    out.write(DUnit::new());
}
