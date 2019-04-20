use std::collections::HashMap;

use wasm_bindgen::prelude::*;

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen(module = "/js/jslib.js")]
extern "C" {
    fn wrap_catch_bail(f: &mut FnMut()) -> bool;
    fn throw_bail();
}

extern "C" {
    #[no_mangle]
    fn whirlc_module_init_testing();
}

extern "C" fn bail_ok_cont(proc_env: *const u8, env: Term, ret: Term) {
    unsafe { BAIL_REASON = BailReason::ReturnOk(ret) };
    throw_bail();
}
extern "C" fn bail_err_cont(proc_env: *const u8, env: Term, ret: Term) {
    unsafe { BAIL_REASON = BailReason::ReturnErr(ret) };
    throw_bail();
}

#[wasm_bindgen]
pub fn init() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log!("Init!");

    let vm = VM::new();
    unsafe {
        VM_INST = Some(vm);
    }

    unsafe { whirlc_module_init_testing() };

    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let ident = FunctionIdent {
        module: "testing".to_string(),
        name: "hello_world".to_string(),
        arity: 1,
    };
    let fun_ptr = vm.funs[&ident];
    let transmuted = unsafe { std::mem::transmute::<_, extern "C" fn(*const u8, u64, Term, Term, Term)>(fun_ptr) };

    vm.pcbs.push(Some(PCB::new()));
    unsafe { CURR_PROC = 0 };

    let test: u8 = 0;
    let hi_atom = vm.atoms.get_atom("hi");
    console_log!("Hi atom: {:?}", hi_atom);

    let cont_ok_ptr = bail_ok_cont as extern "C" fn(*const u8, Term, Term);
    let bail_cont_ok_term = Term::new_fun(0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_ok_ptr) });

    let ret = wrap_catch_bail(&mut || {
        // NOTE: Be EXTREMELY careful about allocations here.
        // They will leak if you don't free them/store them
        // and a bail is thrown!
        transmuted(&test, 0, bail_cont_ok_term, bail_cont_ok_term, hi_atom);
        unreachable!();
    });
    assert!(!ret);

    let mut term_fmt = String::new();
    console_log!("Bail: {:?}", unsafe { &BAIL_REASON });

    console_log!("{:?}", vm.atoms.idx_to_name);
    

}

#[wasm_bindgen]
pub fn testing(name: &str) -> String {
    println!("woo!");
    name.to_string()
}

#[derive(Debug)]
enum BailReason {
    None,
    ReturnOk(Term),
    ReturnErr(Term),
}

static mut VM_INST: Option<VM> = None;
static mut CURR_PROC: usize = 0;
static mut BAIL_REASON: BailReason = BailReason::None;

struct VM {
    atoms: AtomTable,
    pcbs: Vec<Option<PCB>>,

    // TODO
    funs: HashMap<FunctionIdent, extern "C" fn()>,
}
impl VM {
    fn new() -> Self {
        VM {
            atoms: AtomTable::new(),
            pcbs: Vec::new(),
            funs: HashMap::new(),
        }
    }
}

struct PCB {
    heap: ProcessHeap,
}
impl PCB {
    fn new() -> Self {
        PCB {
            heap: ProcessHeap::new(),
        }
    }
}

struct ProcessHeap {
    // DO NOT grow
    mem: Vec<u64>,
    heap_top: usize,
}
impl ProcessHeap {

    fn new() -> ProcessHeap {
        ProcessHeap {
            mem: vec![0; 1048576], // 1mb heap
            heap_top: 0,
        }
    }

    fn alloc(&mut self, num: usize) -> *mut u64 {
        let elem = &self.mem[self.heap_top];
        self.heap_top += num;
        (elem as *const u64) as *mut u64
    }

}

struct AtomTable {
    idx_to_name: Vec<String>,
    name_to_idx: HashMap<String, usize>,
}
impl AtomTable {

    fn new() -> Self {
        AtomTable {
            idx_to_name: Vec::new(),
            name_to_idx: HashMap::new(),
        }
    }
    
    fn get_atom(&mut self, name: &str) -> Term {
        if let Some(idx) = self.name_to_idx.get(name) {
            Term::new_atom_idx(*idx as u32)
        } else {
            let idx = self.idx_to_name.len();
            self.idx_to_name.push(name.to_string());
            self.name_to_idx.insert(name.to_string(), idx);
            Term::new_atom_idx(idx as u32)
        }
    }

}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FunctionIdent {
    module: String,
    name: String,
    arity: u32,
}

const TAG_PRIMARY_MASK: u64 = 0b111;
const TAG_PRIMARY_BOXED: u64 = 0b000;
const TAG_PRIMARY_SMALLINT: u64 = 0b001;
const TAG_PRIMARY_BIGINT: u64 = 0b010;
const TAG_PRIMARY_ATOM: u64 = 0b011;
const TAG_PRIMARY_TUPLE: u64 = 0b100;
const TAG_PRIMARY_FUN: u64 = 0b101;

#[derive(Debug, Copy, Clone)]
pub struct Term(u64);

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

#[no_mangle]
pub extern "C" fn whrt_new_smallint(num: i64) -> Term { Term::new_smallint(num) }

#[no_mangle]
pub extern "C" fn whirlrt_term_eq(proc_env: *const u8, lhs: Term, rhs: Term) -> bool {
    console_log!("Eq: {:?} ({:?}) {:?} ({:?})", lhs, lhs.term_tag(), rhs, rhs.term_tag());
    match (lhs.term_tag(), rhs.term_tag()) {
        (TermTag::Atom, TermTag::Atom) => lhs.0 == rhs.0,
        _ => unimplemented!(),
    }
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_tuple(proc_env: *const u8, len: u32, terms: *const Term) -> Term {
    let terms_slice = unsafe { std::slice::from_raw_parts(terms, len as usize) };
    console_log!("MakeTuple {:?}", terms_slice);

    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let pcb = vm.pcbs[unsafe { CURR_PROC }].as_mut().unwrap();

    let alloc_ptr = pcb.heap.alloc(1 + terms_slice.len());
    unsafe { *alloc_ptr = Term::new_tuple_header(terms_slice.len() as u32).0 };

    for (idx, term) in terms_slice.iter().enumerate() {
        unsafe { *alloc_ptr.offset((idx + 1) as isize) = term.0 };
    }

    Term::new_boxed(alloc_ptr)
}

#[no_mangle]
pub extern "C" fn whirlrt_call_cont(proc_env: *const u8, cont: Term, ret: Term) {
    assert!(cont.0 != 0);
    let fun;
    match cont.term_tag() {
        TermTag::Fun => {
            let (env_size, fun_ptr) = cont.get_fun().unwrap();
            assert!(env_size == 0);
            fun = fun_ptr;
        },
        TermTag::Boxed => {
            let term_ptr = cont.get_boxed().unwrap();
            let base_term = Term(unsafe { *term_ptr });
            assert!(base_term.term_tag() == TermTag::Fun);

            let (_env_size, fun_ptr) = cont.get_fun().unwrap();
            fun = fun_ptr;
        },
        _ => unreachable!(),
    }
    let fun_ptr_casted = unsafe { std::mem::transmute::<_, extern "C" fn(*const u8, Term, Term)>(fun) };
    fun_ptr_casted(proc_env, cont, ret);
}

#[no_mangle]
pub extern "C" fn whirlrt_term_make_atom_from_string(len: u32, data: *const u8) -> Term {
    let name_slice = unsafe { std::slice::from_raw_parts(data, len as usize) };
    let name = std::str::from_utf8(name_slice).unwrap();
    let vm = unsafe { VM_INST.as_mut() }.unwrap();
    let term = vm.atoms.get_atom(name);
    console_log!("Register atom: {} -> {:?}", name, term);
    term
}

#[no_mangle]
pub extern "C" fn whirlrt_module_register_function(module_len: u32, module_data: *const u8, name_len: u32, name_data: *const u8, arity: u32, fun_ptr: extern "C" fn()) {
    let module_slice = unsafe { std::slice::from_raw_parts(module_data, module_len as usize) };
    let module = std::str::from_utf8(module_slice).unwrap();
    let name_slice = unsafe { std::slice::from_raw_parts(name_data, name_len as usize) };
    let name = std::str::from_utf8(name_slice).unwrap();

    console_log!("Register function: {}:{}/{} -> {}", module, name, arity, unsafe { std::mem::transmute::<_, u32>(fun_ptr) });

    let ident = FunctionIdent {
        module: module.to_string(),
        name: name.to_string(),
        arity: arity,
    };

    let vm = unsafe { VM_INST.as_mut() }.unwrap();

    vm.funs.insert(ident, fun_ptr);
}







