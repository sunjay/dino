use core::ptr;
use core::slice;
use core::cmp::Ordering;

use libc::c_char;

use crate::unique::Unique;
use crate::outptr::OutPtr;
use crate::runtime::{alloc_struct, __dino__alloc_value};
use crate::dbool::DBool;
use crate::dunit::DUnit;
use crate::dint::DInt;

/// The dino byte string type
#[repr(C)]
pub struct DBStr {
    data: Unique<c_char>,
    length: usize,
}

impl DBStr {
    pub fn new() -> Unique<Self> {
        alloc_struct(DBStr {
            data: Unique::empty(),
            length: 0,
        })
    }
}

impl PartialEq for DBStr {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for DBStr {}

impl PartialOrd for DBStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DBStr {
    fn cmp(&self, other: &Self) -> Ordering {
        //TODO: Guard against null/dangling data pointers
        let s1 = unsafe { slice::from_raw_parts(self.data.as_ptr(), self.length) };
        let s2 = unsafe { slice::from_raw_parts(other.data.as_ptr(), other.length) };
        s1.cmp(s2)
    }
}

impl DBStr {
    /// Copies the data from the given pointer and returns a new DBStr
    ///
    /// # Safety
    ///
    /// Safe as long as the pointer passed in is valid and the length is correct
    pub unsafe fn copy_ptr(input_data: *const c_char, length: usize) -> Unique<Self> {
        if length == 0 {
            return DBStr::new();
        }

        let data = {
            let data = __dino__alloc_value(length) as *mut c_char;
            //TODO: Check if returned ptr is NULL
            ptr::copy(input_data, data, length);
            Unique::new_unchecked(data)
        };

        alloc_struct(Self {data, length})
    }
}

/// Creates a new DBStr from a byte string literal
///
/// # Safety
///
/// Safe as long as the pointer passed in is valid and the length is correct
#[no_mangle]
pub unsafe extern fn __dino__DBStr_from_bstr_literal(data: *const c_char, length: u64, mut out: OutPtr<DBStr>) {
    out.write(DBStr::copy_ptr(data, length as usize));
}

#[no_mangle]
pub extern fn bstr_len(s: &DBStr, mut out: OutPtr<DInt>) {
    out.write(DInt::new(s.length as i64));
}

#[no_mangle]
pub extern fn bstr_eq(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBool>) {
    out.write(DBool::new(s1 == s2));
}

#[no_mangle]
pub extern fn bstr_gt(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBool>) {
    out.write(DBool::new(s1 > s2));
}

#[no_mangle]
pub extern fn bstr_gte(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBool>) {
    out.write(DBool::new(s1 >= s2));
}

#[no_mangle]
pub extern fn bstr_lt(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBool>) {
    out.write(DBool::new(s1 < s2));
}

#[no_mangle]
pub extern fn bstr_lte(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBool>) {
    out.write(DBool::new(s1 <= s2));
}

#[no_mangle]
pub extern fn bstr_concat(s1: &DBStr, s2: &DBStr, mut out: OutPtr<DBStr>) {
    let length = s1.length + s2.length;
    if length == 0 {
        return out.write(DBStr::new());
    }

    let data = unsafe {
        let data = __dino__alloc_value(length) as *mut c_char;
        //TODO: Check if returned ptr is NULL
        ptr::copy(s1.data.as_ptr(), data, s1.length);
        ptr::copy(s2.data.as_ptr(), data.add(s1.length), s2.length);
        Unique::new_unchecked(data)
    };

    out.write(alloc_struct(DBStr {data, length}));
}

#[no_mangle]
pub extern fn bstr_slice(s: &DBStr, start: &DInt, end: &DInt, mut out: OutPtr<DBStr>) {
    //TODO: Bounds checking
    let start = start.value() as usize;
    let end = end.value() as usize;
    // Offset the pointer to the start
    let data_ptr = unsafe { s.data.as_ptr().add(start) };
    // Copy from the start to the character just before the end
    // Safe as long as the pointer passed in is valid and the length is correct
    out.write(unsafe { DBStr::copy_ptr(data_ptr, end - start) });
}

#[no_mangle]
pub extern fn bstr_get(s: &DBStr, index: &DInt, out: OutPtr<DBStr>) {
    bstr_slice(s, index, &index.map(|x| x + 1), out);
}

#[no_mangle]
pub extern fn print_bstr(s: &DBStr, mut out: OutPtr<DUnit>) {
    // https://stackoverflow.com/questions/2239519/is-there-a-way-to-specify-how-many-characters-of-a-string-to-print-out-using-pri
    unsafe { super::printf(b"%*.*s\n\0" as *const u8, s.length, s.length, s.data); }

    out.write(DUnit::new());
}

#[no_mangle]
pub extern fn read_line_bstr(mut out: OutPtr<DBStr>) {
    // See: http://man7.org/linux/man-pages/man3/getline.3.html
    let mut data = ptr::null_mut();
    // Note that the allocated buffer length can often be much bigger than the actual length of
    // characters on the line
    let mut buffer_len = 0usize;
    //TODO: lock stdin
    let length = unsafe { libc::getline(
        &mut data as *mut _,
        &mut buffer_len as *mut _,
        super::stdin,
    ) };

    if length == -1 {
        //TODO: Error handling
        unsafe { libc::exit(0); }
    }

    // Ignore the trailing newline. Note that because the newline is always read, `length` is
    // guaranteed to be greater than or equal to 1 before this line.
    let length = (length - 1) as usize;

    //TODO: Free memory allocated by getline
    //TODO: We should make sure the data buffer has size min(length, buffer_len) to save memory
    // Safe as long as the pointer passed in is valid and the length is correct
    out.write(unsafe {
        DBStr::copy_ptr(data as *mut c_char, length)
    });
}
