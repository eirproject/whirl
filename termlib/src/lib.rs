use libc::c_void;

static TAG_PRIMARY_MASK: u64 = 0b111;
static TAG_PRIMARY_SMALL: u64 = 0b001;

struct Term(u64);

/// Ints
impl Term {

    pub fn new_smallint(num: i64) -> Term {
        Term(((num as u64) << 3) | TAG_PRIMARY_SMALL)
    }

    pub fn get_smallint(&self) -> Option<i64> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_SMALL {
            Some((self.0 >> 3) as i64)
        } else {
            None
        }
    }

}

/// Tuples
impl Term {

    pub fn new_tuple(loc: &mut (*mut c_void)) -> Term {
    }

}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
