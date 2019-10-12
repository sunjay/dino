use crate::dunit::DUnit;

/// The dino boolean type
#[repr(transparent)]
pub struct DBool(bool);

impl From<bool> for DBool {
    fn from(x: bool) -> Self {
        DBool(x)
    }
}

/// Creates a new DBool from a boolean literal
#[no_mangle]
pub extern fn __dino__DBool_from_bool_literal(value: bool) -> DBool {
    DBool(value)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bool_eq(x: DBool, y: DBool) -> DBool {
    DBool::from(x.0 == y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bool_and(x: DBool, y: DBool) -> DBool {
    DBool::from(x.0 && y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bool_or(x: DBool, y: DBool) -> DBool {
    DBool::from(x.0 || y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bool_not(x: DBool) -> DBool {
    DBool::from(!x.0)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_bool(x: DBool) -> DUnit {
    if x.0 {
        unsafe { super::printf(b"true\n\0" as *const u8); }
    } else {
        unsafe { super::printf(b"false\n\0" as *const u8); }
    }

    DUnit::default()
}
