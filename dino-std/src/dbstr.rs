use core::ptr;

use libc::{malloc, getline};

use crate::dunit::DUnit;
use crate::dint::DInt;

/// The dino byte string type
#[repr(C)]
pub struct DBStr {
    data: *mut u8,
    length: usize,
}

impl Default for DBStr {
    fn default() -> Self {
        Self {data: ptr::null_mut(), length: 0}
    }
}

impl DBStr {
    /// Copies the data from the given pointer and returns a new DBStr
    pub fn copy_ptr(input_data: *const u8, length: usize) -> Self {
        if length == 0 {
            return DBStr::default();
        }

        //TODO: Free allocated memory
        let data = unsafe { malloc(length) } as *mut u8;
        unsafe { ptr::copy(input_data, data, length) }
        Self {data, length}
    }
}

/// Creates a new DBStr from a byte string literal
#[no_mangle]
pub extern fn __dino__DBStr_from_bstr_literal(data: *const u8, length: usize) -> DBStr {
    DBStr::copy_ptr(data, length)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_len(s: DBStr) -> DInt {
    DInt::from(s.length as i64)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_concat(s1: DBStr, s2: DBStr) -> DBStr {
    let length = s1.length + s2.length;
    if length == 0 {
        return DBStr::default();
    }

    //TODO: Free allocated memory
    let data = unsafe { malloc(length) } as *mut u8;
    unsafe { ptr::copy(s1.data, data, s1.length) }
    unsafe { ptr::copy(s2.data, data.add(s1.length), s2.length) }
    DBStr {data, length}
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_slice(s: DBStr, start: DInt, end: DInt) -> DBStr {
    //TODO: Bounds checking
    let start = i64::from(start) as usize;
    let end = i64::from(end) as usize;
    // Offset the pointer to the start
    let data_ptr = unsafe { s.data.add(start) };
    // Copy from the start to the character just before the end
    DBStr::copy_ptr(data_ptr, end - start)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_get(s: DBStr, index: DInt) -> DBStr {
    bstr_slice(s, index, index.map(|x| x + 1))
}

//TODO: This parameter will eventually be a pointer (since the value is meant to be borrowed).
#[no_mangle]
pub extern fn print_bstr(s: DBStr) -> DUnit {
    // https://stackoverflow.com/questions/2239519/is-there-a-way-to-specify-how-many-characters-of-a-string-to-print-out-using-pri
    unsafe { super::printf(b"%*.*s\n\0" as *const u8, s.length, s.length, s.data); }

    DUnit::default()
}

#[no_mangle]
pub extern fn read_line_bstr() -> DBStr {
    //TODO: Free memory allocated by getline
    // See: http://man7.org/linux/man-pages/man3/getline.3.html
    let mut data = ptr::null_mut();
    let mut length = 0;
    //TODO: lock stdin
    if unsafe { getline(&mut data as *mut _, &mut length as *mut _, super::stdin) } == -1 {
        //TODO: Error handling
    }
    return DBStr {data: data as *mut u8, length}
}
