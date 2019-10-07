use crate::dunit::DUnit;
use crate::dbool::DBool;

/// The dino integer type
///
/// An integer type that is (at least) 64-bits wide.
#[repr(transparent)]
pub struct DInt(i64);

impl From<i64> for DInt {
    fn from(x: i64) -> Self {
        DInt(x)
    }
}

/// Creates a new DInt from an integer literal
#[no_mangle]
pub extern fn __dino__DInt_from_int_literal(value: i64) -> DInt {
    DInt(value)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int_eq(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 == y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int_gt(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 > y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int_gte(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 >= y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int_lt(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 < y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn int_lte(x: DInt, y: DInt) -> DBool {
    DBool::from(x.0 <= y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn add_int(x: DInt, y: DInt) -> DInt {
    DInt(x.0 + y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn sub_int(x: DInt, y: DInt) -> DInt {
    DInt(x.0 - y.0)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_int(x: DInt) -> DUnit {
    unsafe {
        super::printf(b"%lld\n\0" as *const u8, x.0);
    }

    DUnit::default()
}
