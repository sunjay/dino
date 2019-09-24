/// The disco integer type
#[repr(transparent)]
pub struct DInt(i64);

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
pub extern fn print_int(x: DInt) {
    unsafe {
        super::printf(b"%ll" as *const u8, x.0);
    }
}
