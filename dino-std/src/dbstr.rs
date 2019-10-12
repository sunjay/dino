use core::ptr;
use core::slice;
use core::cmp::Ordering;

use libc::{malloc, getline};

use crate::dbool::DBool;
use crate::dunit::DUnit;
use crate::dint::DInt;

/// The dino byte string type
#[repr(C)]
#[derive(Ord, Eq)]
pub struct DBStr {
    data: *mut u8,
    length: usize,
}

impl Default for DBStr {
    fn default() -> Self {
        Self {data: ptr::null_mut(), length: 0}
    }
}

impl PartialEq for DBStr {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for DBStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let s1 = unsafe { slice::from_raw_parts(self.data, self.length) };
        let s2 = unsafe { slice::from_raw_parts(other.data, other.length) };
        s1.partial_cmp(s2)
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
pub extern fn bstr_eq(s1: DBStr, s2: DBStr) -> DBool {
    DBool::from(s1 == s2)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_gt(s1: DBStr, s2: DBStr) -> DBool {
    DBool::from(s1 > s2)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_gte(s1: DBStr, s2: DBStr) -> DBool {
    DBool::from(s1 >= s2)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_lt(s1: DBStr, s2: DBStr) -> DBool {
    DBool::from(s1 < s2)
}

//TODO: These parameters will eventually be pointers (since the values are meant to be borrowed).
#[no_mangle]
pub extern fn bstr_lte(s1: DBStr, s2: DBStr) -> DBool {
    DBool::from(s1 <= s2)
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
    // Note that the allocated buffer length can often be much bigger than the actual length of
    // characters on the line
    let mut buffer_len = 0usize;
    //TODO: lock stdin
    let length = unsafe { getline(&mut data as *mut _, &mut buffer_len as *mut _, super::stdin) };
    if length == -1 {
        //TODO: Error handling
    }

    // Ignore the trailing newline. Note that because the newline is always read, `length` is
    // guaranteed to be greater than or equal to 1 before this line.
    let length = (length - 1) as usize;
    //TODO: We should truncate the data buffer to save memory if buffer_len >> length

    return DBStr {data: data as *mut u8, length}
}
