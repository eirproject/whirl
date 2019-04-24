use std::collections::{ HashMap, VecDeque };
use crate::{ Term, TermTag };

#[derive(Debug)]
pub struct TermHeap {
    // DO NOT grow
    mem: Vec<u64>,
    heap_top: usize,
    build_remaining: usize,
}
impl TermHeap {

    pub fn new(initial_size: usize) -> TermHeap {
        TermHeap {
            mem: vec![0; initial_size],
            heap_top: 0,
            build_remaining: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.mem.len() - self.heap_top
    }

    pub fn alloc(&mut self, num: usize) -> *mut u64 {
        let elem = &self.mem[self.heap_top];
        self.heap_top += num;
        (elem as *const u64) as *mut u64
    }

    fn push_val(&mut self, val: u64) -> *const u64 {
        self.mem[self.heap_top] = val;
        let ptr = unsafe { self.mem.as_ptr().offset(self.heap_top as isize) };
        self.heap_top += 1;
        ptr
    }

    pub fn tuple(&mut self, arity: u32) -> Term {
        assert!(self.build_remaining == 0);
        self.build_remaining = arity as usize;
        Term::new_boxed(self.push_val(Term::new_tuple_header(arity).0))
    }

    pub fn fun(&mut self, env_len: u32, fun_ptr: extern "C" fn()) -> Term {
        if env_len == 0 {
            Term::new_fun(env_len, fun_ptr)
        } else {
            assert!(self.build_remaining == 0);
            self.build_remaining = env_len as usize;
            Term::new_boxed(self.push_val(Term::new_fun(env_len, fun_ptr).0))
        }
    }

    pub fn atomic(&mut self, term: Term) {
        assert!(term.is_atomic());
        self.build_remaining -= 1;
        self.push_val(term.0);
    }

    pub fn clear(&mut self) {
        self.heap_top = 0;
    }

}

pub struct Bitmap {
    data: Vec<u64>,
}
impl Bitmap {
    fn new() -> Self {
        Bitmap {
            data: Vec::new(),
        }
    }
    fn clear(&mut self) {
        self.data.clear();
    }
    fn set(&mut self, num: usize, dat: bool) {
        let idx = num / 64;
        let bit = num % 64;
        let required = (num + 63) / 64;
        if required > self.data.len() {
            self.data.reserve(required - self.data.len());
            for _ in (self.data.len())..required {
                self.data.push(0);
            }
        }
        if dat {
            self.data[idx] |= 1 << bit;
        } else {
            self.data[idx] &= !(1 << bit);
        }
    }
    fn get(&mut self, num: usize) -> bool {
        let idx = num / 64;
        if let Some(dat) = self.data.get(idx) {
            let bit = num % 64;
            dat & (1 << bit) != 0
        } else {
            false
        }
    }
}

pub struct TermHeapCopy {
    buf: Vec<Term>,
}
impl TermHeapCopy {

    pub fn new() -> Self {
        TermHeapCopy {
            buf: Vec::new(),
        }
    }

    pub fn copy(&mut self, to_heap: &mut TermHeap, term: Term) -> Term {
        self.copy_inner(to_heap, &term, true)
    }

    // TODO: Probably want to do this off-stack
    fn copy_inner(&mut self, to_heap: &mut TermHeap, term_ptr: *const Term, atomic: bool) -> Term {
        let term = unsafe { *term_ptr };
        match term.term_tag() {
            TermTag::Fun => {
                let (env_len, fun_ptr) = term.get_fun().unwrap();
                if env_len == 0 {
                    term
                } else {
                    assert!(!atomic);

                    // TODO: Remove use of buffer

                    self.buf.clear();
                    for n in 0..env_len {
                        let orig = unsafe { term_ptr.offset(n as isize + 1) };
                        let copied = self.copy_inner(to_heap, orig, true);
                        self.buf.push(copied);
                    }

                    let ret = to_heap.fun(env_len, fun_ptr);
                    for term in self.buf.iter() {
                        to_heap.atomic(*term);
                    }
                    ret
                }
            }
            TermTag::Tuple => {
                let arity = term.get_tuple_header().unwrap();
                assert!(!atomic);

                self.buf.clear();
                for n in 0..arity {
                    let orig = unsafe { term_ptr.offset(n as isize + 1) };
                    let copied = self.copy_inner(to_heap, orig, true);
                    self.buf.push(copied);
                }

                let ret = to_heap.tuple(arity);
                for term in self.buf.iter() {
                    to_heap.atomic(*term);
                }
                ret
            }
            TermTag::Boxed => {
                let ptr = term.get_boxed().unwrap();
                self.copy_inner(to_heap, ptr as *const Term, false)
            }
            _ => term,
        }
    }

    pub fn required_size(&self, term: Term) -> usize {
        self.required_size_inner(&term)
    }

    fn required_size_inner(&self, term_ptr: *const Term) -> usize {
        let term = unsafe { *term_ptr };
        match term.term_tag() {
            TermTag::Atom => 0,
            TermTag::Pid => 0,
            TermTag::SmallInt => 0,
            TermTag::Boxed => {
                let ptr = term.get_boxed().unwrap();
                self.required_size_inner(ptr as *const Term)
            }
            TermTag::Tuple => {
                let arity = term.get_tuple_header().unwrap();
                let mut len = arity as usize + 1;
                for n in 0..arity {
                    len += self.required_size_inner(unsafe { term_ptr.offset(n as isize + 1) });
                }
                len
            }
            TermTag::Fun => {
                let (env_len, _fun_ptr) = term.get_fun().unwrap();
                if env_len == 0 {
                    0
                } else {
                    let mut len  = env_len as usize + 1;
                    for n in 0..env_len {
                        len += self.required_size_inner(unsafe { term_ptr.offset(n as isize + 1) });
                    }
                    len
                }
            }
        }
    }

}

/// Pretty simple invented-on-the-spot GC algorithm.
/// There are probably better ways of doing things, but
/// this is simple and good enough.
///
/// 1. Generate `action_list`. This is a topologically sorted
///    list that starts at the roots and goes down through all
///    live terms. Only includes composite terms.
/// 2. Naively copy every term in action list from leaves up to
///    roots. Store the new offset of the term in `mappings`.
/// 3. Grow original heap if needed.
/// 4. Copy from `gc_buf` back to heap, applying new offsets to
///    all boxed terms that reference other terms in the heap.
/// 5. The user can then call the `map` function to get the
///    new and copied versions of the roots.
pub struct TermHeapGC {
    mappings: HashMap<*const Term, isize>,
    heap_start: *const u64,
    heap_end: *const u64,
    new_heap_start: *const u64,
    gc_buf: Vec<u64>,
    action_list: Vec<*const Term>,
    to_translate: Vec<usize>,
}
impl TermHeapGC {

    pub fn new() -> Self {
        TermHeapGC {
            mappings: HashMap::new(),
            heap_start: std::ptr::null(),
            heap_end: std::ptr::null(),
            new_heap_start: std::ptr::null(),
            gc_buf: Vec::new(),
            action_list: Vec::new(),
            to_translate: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.mappings.clear();
        self.heap_start = std::ptr::null();
        self.heap_end = std::ptr::null();
        self.new_heap_start = std::ptr::null();
        self.gc_buf.clear();
        self.action_list.clear();
        self.to_translate.clear();
    }

    pub fn init_for_heap(&mut self, heap: &TermHeap) {
        self.clear();
        self.heap_start = heap.mem.as_ptr();
        let heap_word_len = heap.mem.len();
        self.heap_end = unsafe { self.heap_start.offset(heap_word_len as isize * 8) };
    }

    pub fn seed(&mut self, term: Term) {
        self.action_list_add(term);
    }

    fn in_heap(&self, term: Term) -> bool {
        if let Some(ptr) = term.get_boxed() {
            if ptr >= self.heap_start && ptr < self.heap_end {
                return true;
            }
        }
        false
    }

    fn action_list_add_ptr(&mut self, ptr: *const Term) {
        if !self.mappings.contains_key(&ptr) {
            self.mappings.insert(ptr, -1);
            self.action_list.push(ptr);
        }
    }
    fn action_list_add(&mut self, term: Term) {
        if self.in_heap(term) {
            let ptr = term.get_boxed().unwrap();
            self.action_list_add_ptr(ptr as *const Term);
        }
    }

    pub fn do_gc(&mut self, heap: &mut TermHeap, required_free: usize) {
        console_log!("GC!!");

        // Construct complete action list
        let mut pos = 0;
        while pos < self.action_list.len() {
            let term_ptr = self.action_list[pos];
            let term = unsafe { *term_ptr };
            match term.term_tag() {
                TermTag::Tuple => {
                    let len = term.get_tuple_header().unwrap();
                    for item_num in 0..len {
                        let item_ptr = unsafe { term_ptr.offset(item_num as isize + 1) };
                        let item = unsafe { *item_ptr };
                        self.action_list_add(item);
                    }
                },
                TermTag::Fun => {
                    let (len, _fun_ptr) = term.get_fun().unwrap();
                    for item_num in 0..len {
                        let item_ptr = unsafe { term_ptr.offset(item_num as isize + 1) };
                        let item = unsafe { *item_ptr };
                        self.action_list_add(item);
                    }
                },
                _ => {
                    self.action_list_add(term);
                },
            }
            pos += 1;
        }

        self.gc_buf.clear();
        self.gc_buf.reserve(heap.mem.len());

        // Traverse action list backwards and copy to gc buf
        for term_ptr_idx in (0..(self.action_list.len())).rev() {
            let term_ptr = self.action_list[term_ptr_idx];
            let term = unsafe { *term_ptr };

            let new_offset = self.gc_buf.len();

            match term.term_tag() {
                TermTag::Tuple => {
                    let len = term.get_tuple_header().unwrap();
                    self.gc_buf.push(term.0);
                    for item_num in 0..len {
                        let item_ptr = unsafe { term_ptr.offset(item_num as isize + 1) };
                        let item = unsafe { *item_ptr };
                        self.push_term_gc_buf(item);
                    }
                },
                TermTag::Fun => {
                    let (len, _fun_ptr) = term.get_fun().unwrap();
                    self.gc_buf.push(term.0);
                    for item_num in 0..len {
                        let item_ptr = unsafe { term_ptr.offset(item_num as isize + 1) };
                        let item = unsafe { *item_ptr };
                        self.push_term_gc_buf(item);
                    }
                },
                _ => {
                    self.push_term_gc_buf(term);
                },
            }

            self.mappings.insert(term_ptr, new_offset as isize);
        }

        // If the heap is 3/4th full after GC, grow heap
        if self.gc_buf.len() > (heap.mem.len() / 4 * 3) {
            let grow = (heap.mem.len() / 2) + required_free;
            heap.mem.reserve(grow);
            for _ in 0..grow {
                heap.mem.push(0);
            }
        }

        // Clear heap
        heap.heap_top = 0;
        self.new_heap_start = heap.mem.as_ptr();

        // Copy from GC buf to new heap
        let mut to_translate_idx = 0;
        for item in self.gc_buf.iter() {
            let mut item = *item;
            if self.to_translate.get(to_translate_idx).cloned() == Some(heap.heap_top) {
                to_translate_idx += 1;

                let ptr = Term(item).get_boxed().unwrap();
                let offset = self.mappings[&(ptr as *const Term)];
                assert!(offset >= 0);

                let new_ptr = unsafe { self.new_heap_start.offset(offset) };
                item = Term::new_boxed(new_ptr).0;
            }
            heap.mem[heap.heap_top] = item;
            heap.heap_top += 1;
        }

    }

    fn push_term_gc_buf(&mut self, term: Term) {
        let mapped = if let Some(ptr) = term.get_boxed() {
            let offset = self.mappings[&(ptr as *const Term)];
            assert!(offset >= 0);
            self.to_translate.push(self.gc_buf.len());
            Term::new_boxed(ptr)
        } else {
            term
        };
        self.gc_buf.push(mapped.0);
    }

    pub fn map(&self, term: Term) -> Term {
        assert!(self.new_heap_start != std::ptr::null());
        if self.in_heap(term) {
            let ptr = term.get_boxed().unwrap();

            let offset = self.mappings[&(ptr as *const Term)];
            assert!(offset >= 0);

            let new_ptr = unsafe { self.new_heap_start.offset(offset) };
            Term::new_boxed(new_ptr as *const u64)
        } else {
            term
        }
    }

}

#[cfg(test)]
mod tests {
    use crate::Term;
    use super::{ TermHeap, TermHeapGC, TermHeapCopy };

    #[test]
    #[should_panic]
    fn invalid_term_build_panic() {
        let mut heap = TermHeap::new(100);
        heap.tuple(1);
        heap.tuple(1);
    }

    #[test]
    fn basic_term_building() {
        let mut heap = TermHeap::new(100);
        let heap_base = heap.mem.as_ptr();

        let tup1 = heap.tuple(2);
        assert!(tup1 == Term::new_boxed(heap_base));
        assert!(tup1.get_boxed().unwrap() == heap_base);

        heap.atomic(Term::new_pid(0));
        heap.atomic(Term::new_pid(0));

        let tup2 = heap.tuple(0);
        unsafe {
            assert!(tup2 == Term::new_boxed(heap_base.offset(3)));
            assert!(tup2.get_boxed().unwrap() == heap_base.offset(3));
        }

        let tup3 = heap.tuple(0);

        assert!(unsafe { *tup1.get_boxed().unwrap() } == Term::new_tuple_header(2).0);
        assert!(unsafe { *tup1.get_boxed().unwrap().offset(1) } == Term::new_pid(0).0);
        assert!(unsafe { *tup1.get_boxed().unwrap().offset(2) } == Term::new_pid(0).0);

        assert!(unsafe { *tup2.get_boxed().unwrap() } == Term::new_tuple_header(0).0);
        assert!(unsafe { *tup3.get_boxed().unwrap() } == Term::new_tuple_header(0).0);

        let mut copy = TermHeapCopy::new();
        copy.copy(&mut heap, tup1);

    }

    #[test]
    fn basic_gc() {
        let mut heap = TermHeap::new(100);

        let tup1 = heap.tuple(2);
        heap.atomic(Term::new_pid(5));
        heap.atomic(Term::new_pid(10));

        let tup2 = heap.tuple(2);
        heap.atomic(Term::new_pid(15));
        heap.atomic(tup1);

        let tup3 = heap.tuple(2);
        heap.atomic(tup1);
        heap.atomic(Term::new_pid(20));

        let tup4 = heap.tuple(1);
        heap.atomic(Term::new_pid(25));

        let mut gc = TermHeapGC::new();
        gc.init_for_heap(&heap);
        gc.seed(tup2);
        gc.seed(tup3);
        gc.do_gc(&mut heap, 0);

        let tup1 = gc.map(tup1);
        let tup2 = gc.map(tup2);
        let tup3 = gc.map(tup3);

        assert!(tup1 != tup2);
        assert!(tup1 != tup3);
        assert!(tup2 != tup3);

        assert!(unsafe { *tup1.get_boxed().unwrap() } == Term::new_tuple_header(2).0);
        assert!(unsafe { *tup2.get_boxed().unwrap() } == Term::new_tuple_header(2).0);
        assert!(unsafe { *tup3.get_boxed().unwrap() } == Term::new_tuple_header(2).0);

        assert!(unsafe { *tup1.get_boxed().unwrap().offset(1) } == Term::new_pid(5).0);
        assert!(unsafe { *tup1.get_boxed().unwrap().offset(2) } == Term::new_pid(10).0);

        assert!(unsafe { *tup2.get_boxed().unwrap().offset(1) } == Term::new_pid(15).0);
        assert!(unsafe { *tup2.get_boxed().unwrap().offset(2) } == tup1.0);

        assert!(unsafe { *tup3.get_boxed().unwrap().offset(1) } == tup1.0);
        assert!(unsafe { *tup3.get_boxed().unwrap().offset(2) } == Term::new_pid(20).0);

        // Term has been GCed, panic!
        let res = std::panic::catch_unwind(|| gc.map(tup4));
        assert!(res.is_err());

    }

}
