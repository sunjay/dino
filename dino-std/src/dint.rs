use crate::unique::Unique;
use crate::runtime::alloc_no_ptr;
use crate::dunit::DUnit;
use crate::dbool::DBool;

/// The dino integer type
///
/// An integer type that is (at least) 64-bits wide.
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DInt(i64);

impl DInt {
    pub fn new(value: i64) -> Unique<Self> {
        alloc_no_ptr(DInt(value))
    }

    pub fn value(self) -> i64 {
        self.0
    }

    pub fn map(self, f: impl FnOnce(i64) -> i64) -> Self {
        DInt(f(self.0))
    }
}

/// Creates a new DInt from an integer literal
#[no_mangle]
pub extern fn __dino__DInt_from_int_literal(value: i64) -> Unique<DInt> {
    DInt::new(value)
}

#[no_mangle]
pub extern fn int__eq(x: &DInt, y: &DInt) -> Unique<DBool> {
    DBool::new(x.0 == y.0)
}

#[no_mangle]
pub extern fn int__gt(x: &DInt, y: &DInt) -> Unique<DBool> {
    DBool::new(x.0 > y.0)
}

#[no_mangle]
pub extern fn int__gte(x: &DInt, y: &DInt) -> Unique<DBool> {
    DBool::new(x.0 >= y.0)
}

#[no_mangle]
pub extern fn int__lt(x: &DInt, y: &DInt) -> Unique<DBool> {
    DBool::new(x.0 < y.0)
}

#[no_mangle]
pub extern fn int__lte(x: &DInt, y: &DInt) -> Unique<DBool> {
    DBool::new(x.0 <= y.0)
}

#[no_mangle]
pub extern fn int__add(x: &DInt, y: &DInt) -> Unique<DInt> {
    DInt::new(x.0 + y.0)
}

#[no_mangle]
pub extern fn int__sub(x: &DInt, y: &DInt) -> Unique<DInt> {
    DInt::new(x.0 - y.0)
}

#[no_mangle]
pub extern fn int__mul(x: &DInt, y: &DInt) -> Unique<DInt> {
    DInt::new(x.0 * y.0)
}

#[no_mangle]
pub extern fn int__div(x: &DInt, y: &DInt) -> Unique<DInt> {
    DInt::new(x.0 / y.0)
}

#[no_mangle]
pub extern fn int__rem(x: &DInt, y: &DInt) -> Unique<DInt> {
    DInt::new(x.0 % y.0)
}

#[no_mangle]
pub extern fn int__neg(x: &DInt) -> Unique<DInt> {
    DInt::new(-x.0)
}

#[no_mangle]
pub extern fn print_int(x: &DInt) -> Unique<DUnit> {
    unsafe {
        super::printf(b"%lld\n\0" as *const u8, x.0);
    }

    DUnit::new()
}
