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

    vm.pcbs.push(Some(PCB::new()));
    unsafe { CURR_PROC = 0 };

    //let cont_ok_ptr = bail_ok_cont as extern "C" fn(*const u8, Term, Term);
    //let bail_cont_ok_term = Term::new_fun(0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_ok_ptr) });

    //let cont_err_ptr = bail_err_cont as extern "C" fn(*const u8, Term, Term);
    //let bail_cont_err_term = Term::new_fun(0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_err_ptr) });

    //{
    //    let fun_ptr = vm.funs[&(vm.atoms.get_atom("testing"),
    //                            vm.atoms.get_atom("hello_world"), 1)];
    //    let transmuted = unsafe { std::mem::transmute::<_, extern "C" fn(*const u8, u64, Term, Term, Term)>(fun_ptr) };

    //    let test: u8 = 0;
    //    let hi_atom = vm.atoms.get_atom("hi");
    //    console_log!("Hi atom: {:?}", hi_atom);

    //    let ret = wrap_catch_bail(&mut || {
    //        // NOTE: Be EXTREMELY careful about allocations here.
    //        // They will leak if you don't free them/store them
    //        // and a bail is thrown!
    //        transmuted(&test, 0, bail_cont_ok_term, bail_cont_err_term, hi_atom);
    //        unreachable!();
    //    });
    //    assert!(!ret);

    //    let mut term_fmt = String::new();
    //    console_log!("Bail: {:?}", unsafe { &BAIL_REASON });
    //}

    //{
    //    let fun_ptr = vm.funs[&(vm.atoms.get_atom("testing"),
    //                            vm.atoms.get_atom("fib"), 1)];
    //    let transmuted = unsafe { std::mem::transmute::<_, extern "C" fn(*const u8, u64, Term, Term, Term)>(fun_ptr) };

    //    let test: u8 = 0;
    //    let five_num = Term::new_smallint(5);

    //    let ret = wrap_catch_bail(&mut || {
    //        // NOTE: Be EXTREMELY careful about allocations here.
    //        // They will leak if you don't free them/store them
    //        // and a bail is thrown!
    //        transmuted(&test, 0, bail_cont_ok_term, bail_cont_err_term, five_num);
    //        unreachable!();
    //    });
    //    assert!(!ret);

    //    let mut term_fmt = String::new();
    //    console_log!("Bail: {:?}", unsafe { &BAIL_REASON });
    //}

    console_log!("{:?}", vm.atoms.idx_to_name);

}

#[wasm_bindgen]
pub fn run_some() {
    let vm = unsafe { VM_INST.as_mut() }.unwrap();

    let mut count = 10;

    while count > 0 {
        let pid = &vm.pids[unsafe { RUN_CURR_PID_NUM }];

        match pid {
            PidTarget::JsProcess(_) => (),
            PidTarget::Process(pcb_num) => {
                let pcb = vm.pcbs[*pcb_num].as_mut().unwrap();
                if let Some(cont_fun) = pcb.cont {

                    let (_cnt, fun_ptr) = cont_fun.get_fun().unwrap();

                    let mut args = vec![cont_fun];
                    args.extend(pcb.cont_args.iter().cloned());

                    let launchpad = fun_n_launchpad::FUN_LAUNCHPADS[
                        args.len()];

                    let ret = wrap_catch_bail(&mut || {
                        let dummy: u8 = 0;
                        launchpad(&dummy, fun_ptr, &args);
                        unreachable!();
                    });
                    assert!(!ret);

                    match unsafe { &BAIL_REASON } {
                        BailReason::None => unreachable!(),
                        BailReason::ReturnOk(_) => {
                            console_log!("ReturnOk");
                            pcb.cont = None;
                        }
                        BailReason::ReturnErr(_) => {
                            console_log!("ReturnOk");
                            pcb.cont = None;
                        }
                        BailReason::Yield => {
                            console_log!("yield!");
                        }
                    }
                }

            },
        }

        count -= 1;
        unsafe {
            RUN_CURR_PID_NUM += 1;
            if RUN_CURR_PID_NUM >= vm.pids.len() { RUN_CURR_PID_NUM = 0; }
        }
    }
}

pub struct Mailbox {
    messages: VecDeque<Term>,
}
impl Mailbox {

    pub fn new() -> Self {
        Mailbox {
            messages: VecDeque::new(),
        }
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
static mut CURR_PROC: usize = 0;
static mut BAIL_REASON: BailReason = BailReason::None;

static mut PROC_CURRENT_REDUCTIONS: usize = 0;
static mut PROC_REDUCTION_LIMIT: usize = 100;

static mut RUN_CURR_PID_NUM: usize = 0;

enum PidTarget {
    Process(usize),
    JsProcess(Weak<RefCell<(Mailbox, ProcessHeap)>>),
}

struct VM {
    atoms: AtomTable,
    pids: Vec<PidTarget>,

    pcbs: Vec<Option<PCB>>,

    // TODO
    funs: HashMap<(Term, Term, u32), extern "C" fn()>,
}
impl VM {
    fn new() -> Self {
        VM {
            atoms: AtomTable::new(),
            pids: Vec::new(),
            pcbs: Vec::new(),
            funs: HashMap::new(),
        }
    }
}

struct PCB {
    heap: ProcessHeap,

    // Continuation
    cont: Option<Term>,
    cont_args: Vec<Term>,
}
impl PCB {
    fn new() -> Self {
        PCB {
            heap: ProcessHeap::new(),
            cont: None,
            cont_args: Vec::new(),
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



//#[no_mangle]
//pub extern "C" fn whrt_new_smallint(num: i64) -> Term { Term::new_smallint(num) }




















