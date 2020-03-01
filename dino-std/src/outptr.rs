use crate::unique::Unique;

/// Represents a pointer to a pointer of T.
///
/// # Safety
///
/// For this type to be used safely, you have to ensure that Rust's aliasing rules are not being
/// broken. That is, the pointer stored here should not alias any other reference. A potentially
/// safer version of this struct would use `&mut *mut T`, but that was deemed unergonomic, so we decided to go
/// with this (the cons outweighed the pros).
///
/// In addition to this, the safety conditions on [`ptr::write`] must also be met.
///
/// [`ptr::write`]: https://doc.rust-lang.org/std/ptr/fn.write.html
#[repr(transparent)]
pub struct OutPtr<T: ?Sized> {
    ptr: *mut *mut T,
}

impl<T: ?Sized> OutPtr<T> {
    /// Sets the pointer pointed to by the out pointer to the given pointer.
    pub fn write(&mut self, mut value: Unique<T>) {
        // This is safe as long as the conditions documented on the struct are met
        unsafe {
            self.ptr.write(value.as_mut());
        }
    }
}
