use crate::dbool::DBool;

/// The dino unit type
#[repr(C)]
#[derive(Default)]
pub struct DUnit {
    // Zero-sized types are not allowed in C
    __unused: u8,
}

/// Creates a new DUnit
#[no_mangle]
pub extern fn __dino__DUnit_from_unit_literal() -> DUnit {
    DUnit::default()
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn unit_eq(_x: DUnit, _y: DUnit) -> DBool {
    // Unit is always equal to itself
    DBool::from(true)
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_unit(_x: DUnit) -> DUnit {
    unsafe {
        super::printf(b"()\n\0" as *const u8);
    }

    DUnit::default()
}
