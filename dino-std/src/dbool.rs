use lazy_static::lazy_static;

use crate::unique::Unique;
use crate::alloc::alloc_static;
use crate::dunit::DUnit;

// Avoid re-allocating the same values over and over again by reusing the two possible values of
// this type
lazy_static! {
    static ref TRUE: Unique<DBool> = unsafe { alloc_static(DBool(true)) };
    static ref FALSE: Unique<DBool> = unsafe { alloc_static(DBool(false)) };
}

/// The dino boolean type
#[repr(transparent)]
pub struct DBool(bool);

impl DBool {
    pub fn new(x: bool) -> Unique<Self> {
        if x { *TRUE } else { *FALSE }
    }
}

/// Creates a new DBool from a boolean literal
#[no_mangle]
pub extern fn __dino__DBool_from_bool_literal(value: bool) -> Unique<DBool> {
    DBool::new(value)
}

/// Creates a C bool from a DBool
#[no_mangle]
pub extern fn __dino__DBool_coerce_bool(x: &DBool) -> bool {
    x.0
}

#[no_mangle]
pub extern fn bool_eq(x: &DBool, y: &DBool) -> Unique<DBool> {
    DBool::new(x.0 == y.0)
}

#[no_mangle]
pub extern fn bool_and(x: &DBool, y: &DBool) -> Unique<DBool> {
    DBool::new(x.0 && y.0)
}

#[no_mangle]
pub extern fn bool_or(x: &DBool, y: &DBool) -> Unique<DBool> {
    DBool::new(x.0 || y.0)
}

#[no_mangle]
pub extern fn bool_not(x: &DBool) -> Unique<DBool> {
    DBool::new(!x.0)
}

#[no_mangle]
pub extern fn print_bool(x: &DBool) -> Unique<DUnit> {
    if x.0 {
        unsafe { super::printf(b"true\n\0" as *const u8); }
    } else {
        unsafe { super::printf(b"false\n\0" as *const u8); }
    }

    DUnit::new()
}
