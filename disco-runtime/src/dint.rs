/// The disco integer type
#[repr(transparent)]
pub struct DInt(i64);

#[no_mangle]
pub extern fn __disco__DInt__operator_add(x: *const DInt, y: *const DInt) -> DInt {
    let x = unsafe { (*x).0 };
    let y = unsafe { (*y).0 };

    DInt(x + y)
}
