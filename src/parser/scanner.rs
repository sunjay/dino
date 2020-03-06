pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
        }
    }
}
