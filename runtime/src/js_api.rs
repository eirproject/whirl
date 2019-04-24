use wasm_bindgen::prelude::*;

use std::rc::{ Rc, Weak };
use std::cell::RefCell;

use crate::VM;
use super::TermHeap;
use super::Term;
use super::PCB;
use super::PidTarget;
use super::Mailbox;

#[wasm_bindgen]
pub struct JsProcess {
    pid: Term,
    mailbox: Rc<RefCell<Mailbox>>,
}

#[wasm_bindgen]
impl JsProcess {

    pub fn new() -> Self {
        let mailbox = Rc::new(RefCell::new(Mailbox::new()));

        let vm = VM::get_instance();
        let pid_num = vm.pids.len();
        vm.pids.push(PidTarget::JsProcess(Rc::downgrade(&mailbox)));

        JsProcess {
            pid: Term::new_pid(pid_num),
            mailbox: mailbox,
        }
    }

    pub fn spawn(&mut self, heap: &mut JsTermHeap, fun: u32, args: &[u32]) -> u32 {
        // TODO: Copy to target heap

        let vm = VM::get_instance();

        let (free_args, _fun_ptr) = heap.exported[fun as usize].get_fun().unwrap();
        assert!(free_args == 0); // spawning with closures not implemented yet

        let pcb_num = vm.pcbs.len();
        let mut pcb = PCB::new();

        let cont_ok_ptr = crate::codegen_api::bail_ok_cont
            as extern "C" fn(*const u8, Term, Term);
        let bail_cont_ok_term = Term::new_fun(
            0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_ok_ptr) });

        let cont_err_ptr = crate::codegen_api::bail_err_cont
            as extern "C" fn(*const u8, Term, Term);
        let bail_cont_err_term = Term::new_fun(
            0, unsafe { std::mem::transmute::<_, extern "C" fn()>(cont_err_ptr) });

        pcb.cont = Some(heap.exported[fun as usize]);
        pcb.cont_args.push(bail_cont_ok_term);
        pcb.cont_args.push(bail_cont_err_term);
        for arg in args.iter() {
            pcb.cont_args.push(heap.exported[*arg as usize]);
        }

        vm.pcbs.push(Some(pcb));

        let pid_num = vm.pids.len();
        vm.pids.push(PidTarget::Process(pcb_num));

        let pid_term = Term::new_pid(pid_num);

        let exp_num = heap.exported.len();
        heap.exported.push(pid_term);
        exp_num as u32
    }

    pub fn send(&mut self, heap: &mut JsTermHeap, pid: u32, message: u32) {
        let vm = VM::get_instance();
        let pid = heap.exported[pid as usize];
        let message = heap.exported[message as usize];
        match pid.pid_get() {
            Some(pid_num) => {
                match &vm.pids[pid_num] {
                    PidTarget::JsProcess(weak) => {
                        if let Some(rc) = weak.upgrade() {
                            let mut refer = rc.borrow_mut();
                            refer.put(message);
                        }
                    },
                    PidTarget::Process(pcb_idx) => {
                        let vm = VM::get_instance();
                        let pcb = vm.pcbs[*pcb_idx].as_mut().unwrap();
                        pcb.mailbox.put(message);
                    },
                }
            },
            None => (),
        }
    }

    pub fn get_pid(&self, heap: &mut JsTermHeap) -> u32 {
        let num = heap.exported.len();
        heap.exported.push(self.pid);
        num as u32
    }

    pub fn poll_receive(&self, heap: &mut JsTermHeap) -> Option<u32> {
        // TODO copy to heap
        let mut refer = self.mailbox.borrow_mut();
        if let Some(term) = refer.pop() {
            let copy = VM::get_copy();
            let size = copy.required_size(term);
            if size > heap.heap.capacity() {
                let gc = VM::get_gc();
                gc.init_for_heap(&heap.heap);
                for term in heap.exported.iter() {
                    gc.seed(*term);
                }
                gc.do_gc(&mut heap.heap, size);
                for term in heap.exported.iter_mut() {
                    *term = gc.map(*term);
                }
            }
            let idx = heap.exported.len();
            heap.exported.push(copy.copy(&mut heap.heap, term));
            Some(idx as u32)
        } else {
            None
        }
    }

}

#[wasm_bindgen]
pub struct JsTermHeap {
    heap: TermHeap,
    exported: Vec<Term>,
}

#[wasm_bindgen]
impl JsTermHeap {

    pub fn alloc(size: u32) -> Self {
        JsTermHeap {
            heap: TermHeap::new(size as usize),
            exported: Vec::new(),
        }
    }

    pub fn make_tuple(&mut self, terms: &[u32]) -> u32 {
        let tup = self.heap.tuple(terms.len() as u32);
        for term in terms.iter() {
            self.heap.atomic(self.exported[*term as usize]);
        }

        let pos = self.exported.len();
        self.exported.push(tup);
        pos as u32
    }

    pub fn make_atom(&mut self, name: &str) -> u32 {
        console_log!("MakeAtom {:?}", name);
        let vm = VM::get_instance();
        let term = vm.atoms.get_atom(name);
        let pos = self.exported.len();
        self.exported.push(term);
        pos as u32
    }

    pub fn make_int(&mut self, num: i64) -> u32 {
        let pos = self.exported.len();
        self.exported.push(Term::new_smallint(num));
        pos as u32
    }
    pub fn get_int(&self, term: u32) -> Option<i64> {
        let term = self.exported[term as usize];
        if let Some(num) = term.get_smallint() {
            Some(num)
        } else {
            None
        }
    }

    pub fn capture_fun(&mut self, module: u32, name: u32, arity: u32) -> u32 {
        let module_term = self.exported[module as usize];
        let name_term = self.exported[name as usize];

        console_log!("CaptureFun {:?} {:?} {:?}", module_term, name_term, arity);

        let vm = VM::get_instance();
        let fun = vm.funs[&(module_term, name_term, arity)];

        let pos = self.exported.len();
        self.exported.push(Term::new_fun(0, fun));
        pos as u32
    }

    pub fn clear(&mut self) {
        self.exported.clear();
        self.heap.clear();
    }

}
