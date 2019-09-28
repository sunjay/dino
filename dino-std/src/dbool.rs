/// The dino boolean type
#[repr(transparent)]
pub struct DBool(bool);

/// Creates a new DBool from a boolean literal
#[no_mangle]
pub extern fn __dino__DBool_from_bool_literal(value: bool) -> DBool {
    DBool(value)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_bool(x: DBool) {
    if x.0 {
        unsafe { super::printf(b"true\n\0" as *const u8); }
    } else {
        unsafe { super::printf(b"false\n\0" as *const u8); }
    }
}
