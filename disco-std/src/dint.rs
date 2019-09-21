/// The disco integer type
#[repr(transparent)]
pub struct DInt(i64);

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn __disco__DInt__operator_add(x: DInt, y: DInt) -> DInt {
    DInt(x.0 + y.0)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn __disco__DInt__operator_sub(x: DInt, y: DInt) -> DInt {
    DInt(x.0 - y.0)
}
