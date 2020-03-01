use lazy_static::lazy_static;

use crate::unique::Unique;
use crate::outptr::OutPtr;
use crate::runtime::alloc_static;
use crate::dunit::DUnit;

// Avoid re-allocating the same values over and over again by reusing the two possible values of
// this type
lazy_static! {
    static ref TRUE: Unique<DBool> = alloc_static(DBool(true));
    static ref FALSE: Unique<DBool> = alloc_static(DBool(false));
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
pub extern fn __dino__DBool_from_bool_literal(value: bool, mut out: OutPtr<DBool>) {
    out.write(DBool::new(value));
}

/// Creates a C bool from a DBool
#[no_mangle]
pub extern fn __dino__DBool_coerce_bool(x: &DBool, out: &mut bool) {
    *out = x.0
}

#[no_mangle]
pub extern fn bool_eq(x: &DBool, y: &DBool, mut out: OutPtr<DBool>) {
    out.write(DBool::new(x.0 == y.0));
}

#[no_mangle]
pub extern fn bool_and(x: &DBool, y: &DBool, mut out: OutPtr<DBool>) {
    out.write(DBool::new(x.0 && y.0));
}

#[no_mangle]
pub extern fn bool_or(x: &DBool, y: &DBool, mut out: OutPtr<DBool>) {
    out.write(DBool::new(x.0 || y.0));
}

#[no_mangle]
pub extern fn bool_not(x: &DBool, mut out: OutPtr<DBool>) {
    out.write(DBool::new(!x.0));
}

#[no_mangle]
pub extern fn print_bool(x: &DBool, mut out: OutPtr<DUnit>) {
    if x.0 {
        unsafe { super::printf(b"true\n\0" as *const u8); }
    } else {
        unsafe { super::printf(b"false\n\0" as *const u8); }
    }

    out.write(DUnit::new());
}
