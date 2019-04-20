const TAG_PRIMARY_MASK: u64 = 0b111;
const TAG_PRIMARY_BOXED: u64 = 0b000;
const TAG_PRIMARY_SMALLINT: u64 = 0b001;
const TAG_PRIMARY_BIGINT: u64 = 0b010;
const TAG_PRIMARY_ATOM: u64 = 0b011;
const TAG_PRIMARY_TUPLE: u64 = 0b100;
const TAG_PRIMARY_FUN: u64 = 0b101;
const TAG_PRIMARY_PID: u64 = 0b111;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Term(pub u64);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TermTag {
    Boxed = TAG_PRIMARY_BOXED as isize,
    SmallInt = TAG_PRIMARY_SMALLINT as isize,
    Atom = TAG_PRIMARY_ATOM as isize,
    Tuple = TAG_PRIMARY_TUPLE as isize,
    Fun = TAG_PRIMARY_FUN as isize,
}

/// Ints
impl Term {

    pub fn term_tag(&self) -> TermTag {
        match (self.0 & 0b111) {
            TAG_PRIMARY_BOXED => TermTag::Boxed,
            TAG_PRIMARY_SMALLINT => TermTag::SmallInt,
            TAG_PRIMARY_ATOM => TermTag::Atom,
            TAG_PRIMARY_TUPLE => TermTag::Tuple,
            TAG_PRIMARY_FUN => TermTag::Fun,
            _ => unreachable!(),
        }
    }

    pub fn new_smallint(num: i64) -> Term {
        Term(((num as u64) << 3) | TAG_PRIMARY_SMALLINT)
    }

    pub fn get_smallint(&self) -> Option<i64> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_SMALLINT {
            Some((self.0 >> 3) as i64)
        } else {
            None
        }
    }

    pub fn new_atom_idx(idx: u32) -> Term {
        Term(((idx as u64) << 3) | TAG_PRIMARY_ATOM)
    }

    pub fn new_tuple_header(num: u32) -> Term {
        Term(((num as u64) << 3) | TAG_PRIMARY_TUPLE)
    }
    pub fn get_tuple_header(self) -> Option<u32> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_TUPLE {
            Some((self.0 >> 3) as u32)
        } else {
            None
        }
    }

    pub fn new_boxed(ptr: *const u64) -> Term {
        assert!((unsafe { std::mem::transmute::<_, u32>(ptr) } & 0b111) == 0);
        Term((ptr as u64) | TAG_PRIMARY_BOXED)
    }
    pub fn get_boxed(&self) -> Option<*const u64> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_BOXED {
            Some(unsafe { std::mem::transmute::<_, *const u64>((self.0 & (!0b111)) as u32) })
        } else {
            None
        }
    }

    pub fn new_fun(env_len: u32, fun: extern "C" fn()) -> Term {
        assert!(env_len < 2000);
        let fun_num = unsafe { std::mem::transmute::<_, u32>(fun) };

        let term_num =
            TAG_PRIMARY_FUN
            | ((env_len as u64) << 3)
            | ((fun_num as u64) << 32);

        Term(term_num)
    }
    pub fn get_fun(self) -> Option<(u32, extern "C" fn())> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_FUN {
            let fun_num = (self.0 >> 32) as u32;
            let fun = unsafe { std::mem::transmute::<_, extern "C" fn()>(fun_num) };

            let env_len = ((self.0 >> 3) & (2^29)) as u32;

            Some((env_len, fun))
        } else {
            None
        }
    }

    pub fn new_pid(num: usize) -> Term {
        Term(((num as u64) << 3) | TAG_PRIMARY_PID)
    }
    pub fn pid_get(self) -> Option<usize> {
        if self.0 & TAG_PRIMARY_MASK == TAG_PRIMARY_PID {
            Some((self.0 >> 3) as usize)
        } else {
            None
        }
    }

}

fn format_term(term_ptr: *const Term, buf: &mut String) {
    let term = unsafe { *term_ptr };
    match term.term_tag() {
        TermTag::Tuple => {
            buf.push('{');
            buf.push('}');
        },
        _ => unimplemented!(),
    }
}
