use crate::dunit::DUnit;
use crate::dbool::DBool;

/// The dino integer type
///
/// An integer type that is (at least) 64-bits wide.
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DInt(i64);

impl DInt {
    pub fn map(self, f: impl FnOnce(i64) -> i64) -> Self {
        DInt(f(self.0))
    }
}

impl From<i64> for DInt {
    fn from(x: i64) -> Self {
        DInt(x)
    }
}

impl From<DInt> for i64 {
    fn from(x: DInt) -> Self {
        x.0
    }
}

/// Creates a new DInt from an integer literal
#[no_mangle]
pub extern fn __dino__DInt_from_int_literal(value: i64) -> DInt {
    DInt(value)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__eq(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 == y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__gt(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 > y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__gte(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 >= y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__lt(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 < y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__lte(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 <= y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__add(x: DInt, y: DInt) -> DInt {
    DInt(x.0 + y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__sub(x: DInt, y: DInt) -> DInt {
    DInt(x.0 - y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__mul(x: DInt, y: DInt) -> DInt {
    DInt(x.0 * y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__div(x: DInt, y: DInt) -> DInt {
    DInt(x.0 / y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__rem(x: DInt, y: DInt) -> DInt {
    DInt(x.0 % y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int__neg(x: DInt) -> DInt {
    DInt(-x.0)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_int(x: DInt) -> DUnit {
    unsafe {
        super::printf(b"%lld\n\0" as *const u8, x.0);
    }

    DUnit::default()
}
