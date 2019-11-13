use core::ptr;
use core::slice;
use core::cmp::Ordering;

use libc::c_void;

use crate::unique::Unique;
use crate::alloc::{alloc_struct, gc_malloc_no_ptr};
use crate::dbool::DBool;
use crate::dunit::DUnit;
use crate::dint::DInt;

/// The dino list type
///
/// Every list is a list of pointers
#[repr(C)]
pub struct DList {
    data: Unique<Unique<c_void>>,
    length: usize,
}

impl DList {
    pub fn new() -> Unique<DList> {
        unsafe { alloc_struct(DList {
            data: Unique::empty(),
            length: 0,
        }) }
    }
}

impl PartialEq for DList {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for DList {}

impl PartialOrd for DList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DList {
    fn cmp(&self, other: &Self) -> Ordering {
        //TODO: Guard against null/dangling data pointers
        let s1 = unsafe { slice::from_raw_parts(self.data.as_ptr(), self.length) };
        let s2 = unsafe { slice::from_raw_parts(other.data.as_ptr(), other.length) };
        s1.cmp(s2)
    }
}

impl DList {
    /// Copies the data from the given pointer and returns a new DList
    pub fn copy_ptr(input_data: *const u8, length: usize) -> Unique<Self> {
        if length == 0 {
            return DList::new();
        }

        let data = unsafe { gc_malloc_no_ptr(length) } as *mut u8;
        unsafe { ptr::copy(input_data, data, length) }
        Self {data, length}
    }
}

/// Creates a new DList from a byte string literal
#[no_mangle]
pub extern fn __dino__DList_from_bstr_literal(data: *const u8, length: usize) -> Unique<DList> {
    DList::copy_ptr(data, length)
}

#[no_mangle]
pub extern fn bstr_len(s: &DList) -> Unique<DInt> {
    DInt::new(s.length as i64)
}

#[no_mangle]
pub extern fn bstr_eq(s1: &DList, s2: &DList) -> Unique<DBool> {
    DBool::new(s1 == s2)
}

#[no_mangle]
pub extern fn bstr_gt(s1: &DList, s2: &DList) -> Unique<DBool> {
    DBool::new(s1 > s2)
}

#[no_mangle]
pub extern fn bstr_gte(s1: &DList, s2: &DList) -> Unique<DBool> {
    DBool::new(s1 >= s2)
}

#[no_mangle]
pub extern fn bstr_lt(s1: &DList, s2: &DList) -> Unique<DBool> {
    DBool::new(s1 < s2)
}

#[no_mangle]
pub extern fn bstr_lte(s1: &DList, s2: &DList) -> Unique<DBool> {
    DBool::new(s1 <= s2)
}

#[no_mangle]
pub extern fn bstr_concat(s1: &DList, s2: &DList) -> Unique<DList> {
    let length = s1.length + s2.length;
    if length == 0 {
        return DList::default();
    }

    let data = unsafe { gc_malloc_no_ptr(length) } as *mut u8;
    unsafe { ptr::copy(s1.data, data, s1.length) }
    unsafe { ptr::copy(s2.data, data.add(s1.length), s2.length) }
    unsafe { alloc_struct(DList {data, length}) }
}

#[no_mangle]
pub extern fn bstr_slice(s: &DList, start: &DInt, end: &DInt) -> Unique<DList> {
    //TODO: Bounds checking
    let start = start.value();
    let end = end.value();
    // Offset the pointer to the start
    let data_ptr = unsafe { s.data.add(start) };
    // Copy from the start to the character just before the end
    DList::copy_ptr(data_ptr, end - start)
}

#[no_mangle]
pub extern fn bstr_get(s: &DList, index: &DInt) -> Unique<DList> {
    bstr_slice(s, index, index.map(|x| x + 1))
}

#[no_mangle]
pub extern fn print_bstr(s: &DList) -> Unique<DUnit> {
    // https://stackoverflow.com/questions/2239519/is-there-a-way-to-specify-how-many-characters-of-a-string-to-print-out-using-pri
    unsafe { super::printf(b"%*.*s\n\0" as *const u8, s.length, s.length, s.data); }

    DUnit::new()
}

// #[no_mangle]
// pub extern fn read_line_bstr() -> Unique<DList> {
//     //TODO: Free memory allocated by getline
//     // See: http://man7.org/linux/man-pages/man3/getline.3.html
//     let mut data = ptr::null_mut();
//     // Note that the allocated buffer length can often be much bigger than the actual length of
//     // characters on the line
//     let mut buffer_len = 0usize;
//     //TODO: lock stdin
//     let length = unsafe { getline(&mut data as *mut _, &mut buffer_len as *mut _, super::stdin) };
//     if length == -1 {
//         //TODO: Error handling
//         unsafe { libc::exit(0); }
//     }
//
//     // Ignore the trailing newline. Note that because the newline is always read, `length` is
//     // guaranteed to be greater than or equal to 1 before this line.
//     let length = (length - 1) as usize;
//     //TODO: We should truncate the data buffer to save memory if buffer_len >> length
//
//     DList {data: data as *mut u8, length}
// }
