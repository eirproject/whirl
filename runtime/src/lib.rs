#[macro_export]
macro_rules! console_log {
    ($($t:tt)*) => (crate::log(&format_args!($($t)*).to_string()))
}

use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Weak;
use std::cell::RefCell;

use wasm_bindgen::prelude::*;

mod term;
use term::{ Term, TermTag };

mod bifs;

mod codegen_api;

mod js_api;

mod fun_n_launchpad;

mod heap;
pub use heap::{ TermHeap, TermHeapCopy, TermHeapGC };

mod scheduler;

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
    fn whirlc_module_initall();
}


#[wasm_bindgen]
pub fn init() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log!("Init!");

    let vm = VM::new();
    unsafe { VM_INST = Some(vm) };

    unsafe { GC_INST = Some(TermHeapGC::new()) };
    unsafe { COPY_INST = Some(TermHeapCopy::new()) };

    // Call whirlc generated initialization module.
    // Will initialize constants in modules and register module
    // functions with the VM state.
    unsafe { whirlc_module_initall() };

    // Print atom table after module init for debug
    //console_log!("{:?}", vm.atoms.idx_to_name);
}

#[wasm_bindgen]
pub fn run_some() {
    scheduler::run_reductions(5000);
}

#[derive(Debug)]
pub struct Mailbox {
    messages: VecDeque<Term>,
    heap: TermHeap,
    cursor: Option<usize>,
}
impl Mailbox {

    pub fn new() -> Self {
        Mailbox {
            messages: VecDeque::new(),
            heap: TermHeap::new(512),
            cursor: None,
        }
    }

    pub fn put(&mut self, term: Term) {
        let copy = VM::get_copy();
        let size = copy.required_size(term);
        if self.heap.capacity() < size {
            let gc = VM::get_gc();
            gc.init_for_heap(&self.heap);
            for message in self.messages.iter() {
                gc.seed(*message);
            }
            gc.do_gc(&mut self.heap, size);
            for message in self.messages.iter_mut() {
                *message = gc.map(*message);
            }
        }
        let copied = copy.copy(&mut self.heap, term);
        self.messages.push_back(copied);
    }

    pub fn start_receive(&mut self) {
        assert!(self.cursor == None);
        self.cursor = Some(0);
    }
    pub fn peek_receive(&mut self) -> Option<Term> {
        let idx = self.cursor.unwrap();
        let ret = self.messages.get(idx).cloned();
        if ret.is_some() {
            self.cursor = Some(idx + 1);
        }
        ret
    }
    pub fn finish_receive(&mut self) {
        let cursor = self.cursor.unwrap();
        self.messages.remove(cursor-1);
        self.cursor = None;
    }
    pub fn cancel_receive(&mut self) {
        self.cursor = None;
    }

    pub fn peek(&mut self) -> Option<Term> {
        self.messages.get(0).cloned()
    }
    pub fn pop(&mut self) -> Option<Term> {
        self.messages.pop_front()
    }

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
    Yield,
}

static mut VM_INST: Option<VM> = None;
static mut GC_INST: Option<TermHeapGC> = None;
static mut COPY_INST: Option<TermHeapCopy> = None;

//static mut RUN_CURR_PID_NUM: usize = 0;

#[derive(Clone)]
enum PidTarget {
    Process(usize),
    JsProcess(Weak<RefCell<Mailbox>>),
}

pub struct VM {
    atoms: AtomTable,
    pids: Vec<PidTarget>,

    // Current state
    proc_curr: Option<usize>,
    proc_bail_reason: BailReason,
    proc_reds: usize,
    proc_reds_limit: usize,

    // Scheduler state
    sched_curr_proc_num: usize,

    pcbs: Vec<Option<PCB>>,
    funs: HashMap<(Term, Term, u32), extern "C" fn()>,
}
impl VM {
    fn new() -> Self {
        VM {
            atoms: AtomTable::new(),
            pids: Vec::new(),

            proc_curr: None,
            proc_bail_reason: BailReason::None,
            proc_reds: 0,
            proc_reds_limit: 100,

            sched_curr_proc_num: 0,

            pcbs: Vec::new(),
            funs: HashMap::new(),
        }
    }

    // TODO: Find a better way? This is done for convenience.
    // Hacky at the moment, improve at refinement stage.
    pub fn get_instance() -> &'static mut VM {
        unsafe { VM_INST.as_mut() }.unwrap()
    }

    pub fn get_gc() -> &'static mut TermHeapGC {
        unsafe { GC_INST.as_mut() }.unwrap()
    }

    pub fn get_copy() -> &'static mut TermHeapCopy {
        unsafe { COPY_INST.as_mut() }.unwrap()
    }

}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ProcessState {
    Run,
    Receive,
}

struct PCB {
    state: ProcessState,

    heap: TermHeap,

    mailbox: Mailbox,

    // Continuation
    cont: Option<Term>,
    err_cont: Option<Term>,
    cont_args: Vec<Term>,
}
impl PCB {
    fn new() -> Self {
        PCB {
            state: ProcessState::Run,
            heap: TermHeap::new(512),
            mailbox: Mailbox::new(),
            cont: None,
            err_cont: None,
            cont_args: Vec::new(),
        }
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



//#[no_mangle]
//pub extern "C" fn whrt_new_smallint(num: i64) -> Term { Term::new_smallint(num) }




















